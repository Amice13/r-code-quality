set.seed(123)
library(gtsummary) # v1.4.1
library(jtools) #v2.1.3
library(modelsummary) #v0.7.0
library(sandwich) #v3.0-1
library(stargazer) #v5.2.2
library(scales) #v1.1.1
library(tidyverse) #v1.3.1
library(vtable) #v1.3.1


# Function 1
boot_function <- function(variable, reps){
  empty_vector <- c()
  for(i in 1:reps){
    n <- length(variable)
    sample.boot.cont <- sample(variable, size = n, replace=TRUE)
    empty_vector[i] <-  mean(sample.boot.cont, na.rm = T) 
  }
  print(sd(empty_vector)) 
} 

### Load data
oia <- read.csv("oia_apsr_final_replication.csv")


## Remove Rs that failed attention checks
oia %>%
  filter(informed_consent == 1 & attention_check1 == 3 & attention_check2 == 1) %>%
  filter(!is.na(attention_check1)) -> oia

###### Descriptives of entire survey
oia %>%
  mutate_all(na_if,"") %>%
  mutate(income = case_when(
    hhi == 1 | hhi == 2 ~ "20k or less",
    hhi >= 3 & hhi <= 10 ~ "20 - 59k",
    hhi >= 11 & hhi <= 18 ~ "60 - 99k",
    hhi >= 19 ~ "100k or more",
    hhi == -3105 ~ "NA"),
    region = factor(case_when(
      region == 1 ~ "Northeast",
      region == 2 ~ "Midwest",
      region == 3 ~ "South",
      region == 4 ~ "West")),
    gender = factor(case_when(gender == 1 ~ "Male",
                              gender == 2 ~ "Women")),
    age = factor(case_when(
      age >= 18 & age <= 29 ~ "18 - 29",
      age >= 30 & age <= 41 ~ "30 - 41",
      age >= 42 & age <= 54 ~ "42 - 54",
      age >= 55 ~ "55 or more",
      TRUE ~ "other")),
    educ = factor(case_when(
      educ == 1 ~ "Less than HS", 
      educ == 2 ~ "HS",
      educ == 3 ~ "Some college",
      educ == 4 ~ "AA",
      educ == 5 ~ "BA",
      educ == 6 ~ "Postgraduate",
      TRUE ~ "other")),
    ethnicity = factor(case_when(
      ethnicity_1 == 1 ~ "White",
      ethnicity_2 == 1 ~ "Hispanic",
      ethnicity_3 == 1 ~ "Black",
      ethnicity_4 == 1 ~ "Asian",
      ethnicity_5 == 1 ~ "Native American",
      ethnicity_6 == 1 ~ "Other",
      TRUE ~ "other"))) %>%
  mutate(pid_3level = case_when(pid_7level == 1 ~ 1,
                                pid_7level == 2 ~ 1,
                                pid_7level == 3 ~ 1,
                                pid_7level == 4 ~ 2,
                                pid_7level == 5 ~ 3,
                                pid_7level == 6 ~ 3,
                                pid_7level == 7 ~ 3),
         polid_3level = case_when(pol_ideology == 1 ~ 1,
                                  pol_ideology == 2 ~ 1,
                                  pol_ideology == 3 ~ 1,
                                  pol_ideology == 4 ~ 2,
                                  pol_ideology == 5 ~ 3,
                                  pol_ideology == 6 ~ 3,
                                  pol_ideology == 7 ~ 3)) %>%
  rename(age_qual = age.1) %>%
  mutate(income = relevel(factor(income), 
                          ref = "20 - 59k"),
         age = relevel(factor(age),
                       ref = "30 - 41"),
         educ = relevel(factor(educ),
                        ref = "HS"),
         treatment_s2_gen = relevel(factor(treatment_s2_gen),
                                    ref = "Control"),
         gender = relevel(factor(gender),
                          ref = "Male"),
         ethnicity = relevel(factor(ethnicity),
                             ref = "White") ) -> oia


#### subset by Party ID
# democrats
dems <- subset(oia, pid_3level == 1)
# independents
indep <- subset(oia, pid_3level == 2)
# republicans
reps <- subset(oia, pid_3level == 3)

#### subset by Pol ID
# liberals
liberal <- subset(oia, polid_3level == 1)
# moderates
moderate <- subset(oia, polid_3level == 2)
# conservatives
conserv <- subset(oia, polid_3level == 3)


################################################
############ Figure 1 ##########################
################################################

oia %>%
  group_by(treatment_s1) %>%
  summarise(n = n(),
            response = mean(response_s1, na.rm = T),
            se = sd(response_s1)/sqrt(n())) %>% 
  mutate(pid_3level = 0) %>% # Denote the pooled sample
  filter(!is.na(treatment_s1)) -> fig1_all

oia %>%
  group_by(treatment_s1, pid_3level) %>%
  summarise(n = n(),
            response = mean(response_s1, na.rm = T),
            se = sd(response_s1)/sqrt(n())) %>%
  filter(!is.na(treatment_s1),
         !is.na(pid_3level)) -> fig1_pid

# Merge pooled + by Pid
fig1_all %>%
  full_join(fig1_pid, by = NULL) -> fig1


fig1 %>%
  mutate(pid_3level = factor(pid_3level,
                             levels = c(0,
                                        1,
                                        2,
                                        3),
                             labels = c("Pooled",
                                        "Democrats",
                                        "Independent",
                                        "Republicans"))) %>%
  ggplot(aes(x = treatment_s1, y = response,
             ymin = response - (2*se),
             ymax = response + (2*se),
             color = as.factor(pid_3level),
             shape = as.factor(pid_3level))) +
  geom_pointrange(position = position_dodge2(w = .5)) +
  scale_color_manual(name = "",
                     labels = c(
                       "Pooled\nn=2077",
                       "Democrats\nn=806", 
                       "Independents\nn=638", 
                       "Republicans\nn=620"),
                     values = c("black",
                                "blue",
                                "purple",
                                "red")) +
  scale_shape_discrete(name = "",
                       labels = c("Pooled\nn=2077", 
                                  "Democrats\nn=806", 
                                  "Independents\nn=638",
                                  "Republicans\nn=620")) +
  scale_y_continuous(labels = scales::percent_format(accuracy = 1)) +
  scale_x_discrete(labels = c("Cooperation\nImmigration\nEnforcement", "Immigration \nOffice")) +
  theme_bw() +
  labs(x = "",
       y = "Mean Favorability") +
  theme(legend.position = "top", 
        legend.text=element_text(size=13.5),
        axis.text.y = element_text(size=rel(1.3)),
        axis.text.x = element_text(size=rel(1.45)) ) 
ggsave("fig_1_final.jpeg")


###############################################            
############## Figure 2 #######################
###############################################

oia %>%
  mutate(ethnicity2 = case_when(ethnicity == "White" ~ "White",
                                NA ~ "NA",
                                TRUE ~ "Non-White")) %>%
  group_by(treatment_s1, ethnicity2) %>%
  summarise(n = n(),
            response = mean(response_s1, na.rm = T),
            se = sd(response_s1)/sqrt(n())) %>%
  filter(!is.na(treatment_s1)) %>%
  ggplot(aes(x = treatment_s1, 
             y = response, 
             shape = as.factor(ethnicity2),
             color = as.factor(ethnicity2),
             ymin = response - (2*se),
             ymax = response + (2*se))) +
  geom_pointrange(position = position_dodge2(w = .5)) +
  scale_color_manual(name = "",
                     labels = c("Non-White\nn=555", "White\nn=1522"),
                     values = c("navy", "light blue")) +
  scale_shape_discrete(name = "",
                       labels = c("Non-White\nn=555", "White\nn=1522")) +
  scale_y_continuous(labels = scales::percent_format(accuracy = 1),
                     breaks = seq(.55,.85, by = .10)) +
  scale_x_discrete(labels = c("Cooperation\nImmigration\nEnforcement", "Immigration \nOffice")) +
  theme_bw() +
  labs(x = "",
       y = "Mean Favorability") +
  theme(legend.position = "top",
        legend.text=element_text(size=13.5),
        axis.text.y = element_text(size=rel(1.3)),
        axis.text.x = element_text(size=rel(1.45)) )
ggsave("fig_2_final.jpeg")



###############################################            
############## Figure 3 #######################
###############################################

oia %>%
  mutate(ethnicity2 = case_when(ethnicity == "White" ~ "White",
                                NA ~ "NA",
                                TRUE ~ "Non-White")) %>%
  group_by(treatment_s1, ethnicity2) %>%
  summarise(n = n(),
            response = mean(response_s1, na.rm = T),
            se = sd(response_s1)/sqrt(n())) %>% 
  mutate(pid_3level = 0)  %>%
  filter(!is.na(pid_3level),
         !is.na(treatment_s1)) -> fig3_all

oia %>%
  mutate(ethnicity2 = case_when(ethnicity == "White" ~ "White",
                                NA ~ "NA",
                                TRUE ~ "Non-White")) %>%
  group_by(treatment_s1, pid_3level, ethnicity2) %>%
  summarise(n = n(),
            response = mean(response_s1, na.rm = T),
            se = sd(response_s1)/sqrt(n())) %>%
  filter(!is.na(pid_3level),
         !is.na(treatment_s1)) -> fig3_pid_ethnicity

# Merge pooled + by Pid
fig3_all %>%
  full_join(fig3_pid_ethnicity, by = NULL) -> fig3

fig3 %>%
  ggplot(aes(x = treatment_s1, 
             y = response, 
             shape = as.factor(pid_3level),
             color = as.factor(pid_3level),
             ymin = response - (2*se),
             ymax = response + (2*se))) +
  geom_pointrange(position = position_dodge2(w = .5)) +
  facet_wrap(ethnicity2 ~.,
             labeller = as_labeller(c(
               `Non-White` = "Non-White\nn=555",
               `White` = "White\nn=1522"))) + 
  scale_color_manual(name = "",
                     labels = c("Pooled",
                                "Democrats",
                                "Independents",
                                "Republicans"),
                     values = c("black",
                                "blue", 
                                "purple",
                                "red")) +
  scale_shape_discrete(name = "",
                       labels = c("Pooled",
                                  "Democrats",
                                  "Independents",
                                  "Republicans")) +
  scale_y_continuous(labels = scales::percent_format(accuracy = 1)) +
  scale_x_discrete(labels = c("Cooperation\nImmigration\nEnforcement", "Immigration \nOffice")) +
  theme_bw() +
  labs(x = "",
       y = "Mean Favorability") +
  theme(legend.position = "top",
        legend.text = element_text(size=13.5),
        axis.text.y = element_text(size=rel(1.3)),
        axis.text.x = element_text(size=rel(1.45)),
        strip.text = element_text(size = 13))
ggsave("fig_3_final.jpeg")


###############################################            
############## Figure 4 #######################
###############################################

# Pooled
oia %>%
  group_by(treatment_s2_gen) %>%
  summarise(n = n(),
            response = mean(response_s2_gen, na.rm = T),
            se = sd(response_s2_gen)/sqrt(n())) %>% 
  mutate(pid_3level = 0) %>% 
  filter(!is.na(pid_3level),
         !is.na(treatment_s2_gen)) -> fig4_all

# By party ID
oia %>%
  group_by(treatment_s2_gen, pid_3level) %>%
  summarise(n = n(),
            response = mean(response_s2_gen, na.rm = T),
            se = sd(response_s2_gen)/sqrt(n())) %>%
  filter(!is.na(pid_3level),
         !is.na(treatment_s2_gen)) -> fig4_pid

# Merge pooled + by Pid
fig4_all %>%
  full_join(fig4_pid, by = NULL) -> fig4


fig4 %>%
  mutate(treatment_s2_gen = factor(treatment_s2_gen,
                                   levels = c("Control",
                                              "Access Undoc",
                                              "Doc Only")),
         pid_3level = factor(pid_3level, 
                             levels = c(0,
                                        1,
                                        2,
                                        3),
                             labels = c("Pooled",
                                        "Democrats",
                                        "Independents",
                                        "Republicans"))) %>%
  ggplot(aes(x = treatment_s2_gen, y = response, 
             shape = as.factor(pid_3level),
             color = as.factor(pid_3level),
             ymin = response - (2*se),
             ymax = response + (2*se))) +
  geom_pointrange(position = position_dodge2(w = .5)) +
  scale_color_manual(name = "",
                     labels = c("Pooled\nn=2057",
                                "Democrats\nn=805",
                                "Independents\nn=634",
                                "Republicans\nn=618"),
                     values = c("black",
                                "blue", 
                                "purple",
                                "red")) +
  scale_shape_discrete(name = "",
                       labels = c("Pooled\nn=2057",
                                  "Democrats\nn=805", 
                                  "Independents\nn=634",
                                  "Republicans\nn=618")) +
  theme_bw() +
  labs(x = "",
       y = "Mean Support") +
  scale_y_continuous(labels = scales::percent_format(accuracy = 1)) +
  scale_x_discrete(labels = c("Control", "Access \nOpen to All",
                              "Access \nDocumented Only")) +
  theme_bw() +
  theme(legend.position = "top",
        legend.text=element_text(size=13.5),
        axis.text.y = element_text(size=rel(1.3)),
        axis.text.x = element_text(size=rel(1.55)))
ggsave("fig_4_final.jpeg")


###############################################            
############## Figure 5 #######################
###############################################

# Pooled data
oia %>%
  mutate(ethnicity2 = case_when(ethnicity == "White" ~ "White",
                                NA ~ "NA",
                                TRUE ~ "Non-White"),
         treatment_s2_gen = factor(treatment_s2_gen,
                                   levels = c("Control",
                                              "Access Undoc",
                                              "Doc Only"))) %>%
  group_by(treatment_s2_gen, ethnicity2) %>%
  summarise(n = n(),
            response = mean(response_s2_gen, na.rm = T),
            se = sd(response_s2_gen)/sqrt(n())) %>%
  mutate(pid_3level = 0) %>%
  filter(!is.na(pid_3level),
         !is.na(treatment_s2_gen)) -> fig5_ethnicity

# By partisanship and ethnoracial 
oia %>%
  mutate(ethnicity2 = case_when(ethnicity == "White" ~ "White",
                                TRUE ~ "Non-White"),
         treatment_s2_gen = factor(treatment_s2_gen,
                                   levels = c("Control",
                                              "Access Undoc",
                                              "Doc Only"))) %>%
  group_by(treatment_s2_gen, pid_3level, ethnicity2) %>%
  summarise(n = n(),
            response = mean(response_s2_gen, na.rm = T),
            se = sd(response_s2_gen)/sqrt(n())) %>%
  filter(!is.na(pid_3level),
         !is.na(treatment_s2_gen)) -> fig5_pid_ethnicity


# merge  
fig5_ethnicity %>%
  full_join(fig5_pid_ethnicity, by = NULL) -> fig5



### Plot
fig5 %>%
  ggplot(aes(x = treatment_s2_gen, 
             y = response, 
             shape = as.factor(pid_3level),
             color = as.factor(pid_3level),
             ymin = response - (2*se),
             ymax = response + (2*se))) +
  geom_pointrange(position = position_dodge2(w = .5)) +
  facet_wrap(ethnicity2 ~.,
             labeller = as_labeller(c(
               `Non-White` = "Non-White\nn=549",
               `White` = "White\nn=1508"))) + 
  scale_color_manual(name = "",
                     labels = c("Pooled",
                                "Democrats",
                                "Independents",
                                "Republicans"),
                     values = c("black",
                                "blue", 
                                "purple",
                                "red")) +
  scale_shape_discrete(name = "",
                       labels = c("Pooled",
                                  "Democrats",
                                  "Independents",
                                  "Republicans")) +
  scale_y_continuous(labels = scales::percent_format(accuracy = 1)) +
  scale_x_discrete(labels = c("Control", "Access \nOpen to All",
                              "Access \nDocumented \nOnly")) +
  theme_bw() +
  labs(x = "",
       y = "Mean Support") +
  theme(legend.position = "top",
        legend.text=element_text(size=13.5),
        axis.text.y = element_text(size=rel(1.3)),
        axis.text.x = element_text(size=rel(1.55)),
        strip.text = element_text(size = 13))
ggsave("fig_5_final.jpeg")


