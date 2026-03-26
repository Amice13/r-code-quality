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


###### Set Reference Values
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
                                  pol_ideology == 7 ~ 3),
         favorability = case_when(undoc_immigrants == 0 ~ "low favorability",
                                  undoc_immigrants == 1 ~ "low favorability",
                                  undoc_immigrants == 3 ~ "high favorability",
                                  undoc_immigrants == 4 ~ "high favorability"),
         welfare_level = case_when(welfare == 1 ~ "low support",
                                   welfare == 4 ~ "high support")) %>%
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


################################################
############### Figure A5 ######################
################################################
# Included in Supplementary Materials #1

# Favorability Undoc
oia %>%
  group_by(treatment_s1, favorability) %>%
  summarise(outcome = mean(response_s1, na.rm = T),
            se = sd(response_s1)/sqrt(n())) %>%
  filter(!is.na(treatment_s1),
         !is.na(favorability)) -> favorability

# Welfare preferences
oia %>% 
  group_by(treatment_s1, welfare_level) %>%
  summarise(outcome = mean(response_s1, na.rm = T),
            se = sd(response_s1)/sqrt(n())) %>%
  filter(!is.na(treatment_s1),
         !is.na(welfare_level)) -> welfare_level

# combine them
combined <- rbind(favorability, welfare_level)

# Get column
which( colnames(combined)=="favorability" )
which( colnames(combined)=="welfare_level" )

# Make plot
combined %>%
  unite("variable", c(2, 5), remove = F, sep = "",
        na.rm = T) %>% 
  mutate(variable = factor(variable,
                           levels = c("low favorability", 
                                      "high favorability",
                                      "low support",
                                      "high support"),
                           labels = c("Low Favorability of Undocumented",
                                      "High Favorability of Undocumented",
                                      "Decreased" = "Supports Welfare Decrease",
                                      "Increased" = "Supports Welfare Increase"))) %>%
  ggplot(aes(x = treatment_s1, y = outcome) )  + 
  geom_errorbar(aes(ymin = outcome - (2*se),
                    ymax = outcome + (2*se) ),
                position = position_dodge(w = 0.75),
                stat = "identity",
                size = 0.5, width = 0.5) +
  geom_point() + 
  facet_wrap(.~variable,
             scales = "free") +
  xlab("") +
  ylab("Mean Favorability") +
  scale_y_continuous(labels = scales::percent_format(accuracy = 1)) +
  scale_x_discrete(labels = c("Cooperation\nImmigration\nEnforcement", "Immigration\nOffice")) +
  theme_bw() +
  theme(strip.text = element_text(size = 13),
        axis.text.x = element_text( size = 12),
        axis.text.y = element_text( size = 12))
ggsave("fig_a5.jpeg")


################################################
############### Figure A6 ######################
################################################
# Included in Supplementary Materials #1

# Favorability Undoc
oia %>% 
  group_by(treatment_s2_gen, favorability) %>%
  summarise(outcome = mean(response_s2_gen, na.rm = T),
            se = sd(response_s2_gen)/sqrt(n())) %>%
  filter(!is.na(treatment_s2_gen),
         !is.na(favorability)) -> favorability_s2

# Welfare preferences
oia %>%
  group_by(treatment_s2_gen, welfare_level) %>%
  summarise(outcome = mean(response_s2_gen, na.rm = T),
            se = sd(response_s2_gen)/sqrt(n())) %>%
  filter(!is.na(treatment_s2_gen),
         !is.na(welfare_level)) -> welfare_level_s2

# combine them
combined_s2 <- rbind(favorability_s2, welfare_level_s2)

which( colnames(combined_s2)=="favorability" )
which( colnames(combined_s2)=="welfare_level" )

# Plot results
combined_s2 %>%
  unite("variable", c(2,5), remove = FALSE, sep = "", na.rm=T) %>%
  mutate(variable = factor(variable,
                           levels = c("low favorability", 
                                      "high favorability",
                                      "low support",
                                      "high support"),
                           labels = c("Low Favorability of Undocumented",
                                      "High Favorability of Undocumented",
                                      "Decreased" = "Supports Welfare Decrease",
                                      "Increased" = "Supports Welfare Increase"))) %>%
  ggplot(aes(x = treatment_s2_gen, y = outcome) )  + 
  geom_errorbar(aes(ymin = outcome - (2*se),
                    ymax = outcome + (2*se) ),
                position = position_dodge(w = 0.75),
                stat = "identity",
                size = 0.5, width = 0.5) +
  geom_point() + 
  facet_wrap(.~variable,
             scales = "free") +
  xlab("") +
  ylab("Mean support") +
  scale_y_continuous(labels = scales::percent_format(accuracy = 1)) +
  scale_x_discrete(labels = c("Control",
                              "Open\nto all", "Access\nDocumented Only")) +
  theme_bw() +
  theme(strip.text = element_text(size = 13),
        axis.text.x = element_text( size = 12),
        axis.text.y = element_text( size = 12))
ggsave("fig_a6.jpeg")

################################################
######## Tabular results for Figure A5 #########
################################################
# Included in Supplementary Materials #2 as Table 8

# subset welfare low
welfare_low <- subset(oia, welfare_level == "low support")

# subset welfare high
welfare_high <- subset(oia, welfare_level == "high support")

# subset undoc favorability low
undoc_low <- subset(oia, favorability == "low favorability")

# subset undoc favorability high
undoc_high <- subset(oia, favorability == "high favorability")

#### 
# Reg among those preferring lower welfare funding
reg_welfare_low_s1 <- lm(response_s1 ~ treatment_s1, welfare_low)

# Reg among those preferring higher welfare funding
reg_welfare_high_s1 <- lm(response_s1 ~ treatment_s1, welfare_high)

# Reg among those viewing undocumented immigrants unfavorably
reg_favorability_low_s1 <- lm(response_s1 ~ treatment_s1, undoc_low)

# Reg among those viewing undocumented immigrants favorably
reg_favorability_high_s1 <- lm(response_s1 ~ treatment_s1, undoc_high)

## Table
modelsummary(list("Welfare Low" = reg_welfare_low_s1,
                  "Welfare High" = reg_welfare_high_s1,
                  "Undoc Fav Low" = reg_favorability_low_s1,
                  "Undoc Fav High" = reg_favorability_high_s1),
             stars = T,
             vcov = "HC1",
             out = "default")

################################################
######## Tabular results for Figure A6 #########
################################################
# Included in Supplementary Materials #2 as Table 9

#### 
# Reg among those preferring lower welfare funding
reg_welfare_low_s2 <- lm(response_s2_gen ~ treatment_s2_gen, welfare_low)

# Reg among those preferring higher welfare funding
reg_welfare_high_s2 <- lm(response_s2_gen ~ treatment_s2_gen, welfare_high)

# Reg among those viewing undocumented immigrants unfavorably
reg_favorability_low_s2 <- lm(response_s2_gen ~ treatment_s2_gen, undoc_low)

# Reg among those viewing undocumented immigrants favorably
reg_favorability_high_s2 <- lm(response_s2_gen ~ treatment_s2_gen, undoc_high)

## Table
modelsummary(list("Welfare Low" = reg_welfare_low_s2,
                  "Welfare High" = reg_welfare_high_s2,
                  "Undoc Fav Low" = reg_favorability_low_s2,
                  "Undoc Fav High" = reg_favorability_high_s2),
             stars = T,
             vcov = "HC1",
             out = "default")




