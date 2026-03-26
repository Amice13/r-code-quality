################################################################################
#### Title: Progressive Ideology and Support for Punitive Crime Policy
#### Author: Isabel Laterzo
#### Year: 2023
#### Journal: Comparative Political Studies
#### Portion of Analysis: Appendix 11 - Additional Experimental Results
################################################################################


################################## SET UP ######################################

rm(list=ls()) #clear global environ

# uncomment below and set working directory using setwd() to directory which contains
# all data files
#setwd()


#packages
library(tidyverse)
library(naniar)
library(cregg)
library(ggplot2)
library(scales)


#read in data and filter
data <- readRDS("laterzo_cps_2023c.RDS")

#fix levels name
levels(data$crime)[levels(data$crime)=="Tough on Crime"] <- "Punitive"

############################### CONJOINT SET UP ################################

#clear out nas on relevant conjoint variables
data <- data %>%
  drop_na(sex, abort, tax, lgbtq, environ, crime)

#safety variable
#4 insecure, 1 secure --> 1 = unsafe, 0 = safe
data$safety_di <- factor(ifelse(data$safety_neighb == 4, "1", 
                                ifelse(data$safety_neighb == 3, "1", 
                                       ifelse(data$safety_neighb == 2, "0", 
                                              ifelse(data$safety_neighb == 1, "0", NA)))))

#country labels
data$country <- as.factor(ifelse(data$country == "brazil", "Brazil",
                                 "Argentina"))

##create data frames for prog vs. conserv on social and econ issues based on
## median value
prog_ideo <- data %>% filter(ideo >= 0) #same as not including zero
cons_ideo <- data %>% filter(ideo < 0) #same as not including zero

#variable for prog/conserv based on median index value
data$ideo_di <- factor(ifelse(data$ideo >= 0, "Prog", "Conserv"))

#subset again with ideology as well
brazil <- data %>% filter(country == "Brazil")
brazil_prog <- prog_ideo %>% filter(country == "Brazil")
brazil_cons <- cons_ideo %>% filter(country == "Brazil")


arg <- data %>% filter(country == "Argentina")
arg_prog <- prog_ideo %>% filter(country == "Argentina")
arg_cons <- cons_ideo %>% filter(country == "Argentina")


#sample size for each

#all brazil
brazil_n <- brazil %>%
  select(id) %>%
  unique()
nrow(brazil_n) #1235

#all brazil progressives
brazil_p_n <- brazil_prog %>%
  select(id) %>%
  unique()
nrow(brazil_p_n) #471

brazil_p_vic <- brazil_prog %>%
  filter(vic == 1) %>%
  select(id) %>%
  unique()
nrow(brazil_p_vic) #92

brazil_p_nonvic <- brazil_prog %>%
  filter(vic == 0) %>%
  select(id) %>%
  unique()
nrow(brazil_p_nonvic) #372

brazil_c_n <- brazil_cons %>%
  select(id) %>%
  unique()
nrow(brazil_c_n) #764

arg_n <- arg %>%
  select(id) %>%
  unique()
nrow(arg_n) #1161

arg_p_n <- arg_prog %>%
  select(id) %>%
  unique()
nrow(arg_p_n) #794

arg_p_vic <- arg_prog %>%
  filter(vic == 1) %>%
  select(id) %>%
  unique()
nrow(arg_p_vic) #114

arg_p_nonvic <- arg_prog %>%
  filter(vic == 0) %>%
  select(id) %>%
  unique()
nrow(arg_p_nonvic) #675

arg_c_n <- arg_cons %>%
  select(id) %>%
  unique()
nrow(arg_c_n) #367




##### BASELINE MODEL: All respondents pooled ------------------
base_model <- cj(data,
                 chosen ~ sex + abort + tax + lgbtq + environ + crime,
                 id = ~id,
                 estimate = "mm",
                 alpha = .05,
                 h0 = 0.5) #null hyp value at 0.5 prob

# Figure A11.1: Candidate Attribute Preferences Across Pooled Sample 

#jpeg("app_figA11-1.jpeg", res=600, width=8000, height=3000)
ggplot(base_model, aes(x = level, y = estimate)) +
  geom_hline(yintercept = 0.5, colour = gray(1/2), lty = 2) + 
  geom_point(aes(x = level, 
                 y = estimate, color = feature)) +
  geom_linerange(aes(x = level, 
                     ymin = lower,
                     ymax = upper,
                     color = feature),
                 lwd = 1) + 
  coord_flip() +
  scale_colour_discrete(name="Attribute",
                        labels = c("Sex", "Abortion", "Taxes",
                                   "LGBTQ+ Rights", "Environment", "Crime")) +
  labs(x = "Attribute Levels",
       y = "Marginal Mean") +
  theme_bw() +
  theme(text = element_text(size=16))
#dev.off()


########################### Pooled Models ######################################

##### BASELINE MODEL: Divided by ideology ------------------
ideo_model <- cj(data,
                 chosen ~ sex + abort + tax + lgbtq + environ + crime,
                 id = ~id,
                 estimate = "mm",
                 by = ~ideo_di,
                 alpha = .05,
                 h0 = 0.5)

# Figure A11.2: Candidate Attribute Preferences Across Divided by Ideology

#jpeg("app_figA11-2.jpeg", res=600, width=8000, height=3000)
ggplot(ideo_model, aes(x = level, y = estimate, linetype = BY, color = feature)) +
  geom_hline(yintercept = 0.5, colour = gray(1/2), lty = 2) + 
  geom_point(position=position_dodge(width = 0.5)) +
  geom_linerange(aes(x = level, 
                     ymin = lower,
                     ymax = upper),
                 position=position_dodge(0.5),
                 lwd = 0.75) +
  coord_flip() +
  scale_colour_discrete(name="Attribute",
                        labels = c("Sex", "Abortion", "Taxes",
                                   "LGBTQ+ Rights", "Environment", "Crime")) +
  scale_linetype_discrete(name="Ideology",
                          labels = c("Conservative", 'Progressive')) +
  labs(x = "Attribute Levels",
       y = "Marginal Mean") +
  theme_bw() +
  theme(text = element_text(size=16))
#dev.off()


#base pooled model by country
base_model <- cj(data,
                 chosen ~ sex + abort + tax + lgbtq + environ + crime,
                 id = ~id,
                 estimate = "mm",
                 by = ~country,
                 alpha = .05,
                 h0 = 0.5) #null hyp value at 0.5 prob

# Figure A11.4: Preferences for Candidate Attributes in Brazil and Argentina by Ideology


#jpeg("app_figA11-3.jpeg.jpeg", res=600, width=8000, height=4000)
ggplot(base_model, aes(x = level, y = estimate)) +
  geom_hline(yintercept = 0.5, colour = gray(1/2), lty = 2) + 
  facet_wrap(~country) +
  geom_point(aes(x = level, 
                 y = estimate, color = feature)) +
  geom_linerange(aes(x = level, 
                     ymin = lower,
                     ymax = upper,
                     color = feature),
                 lwd = 1) + 
  coord_flip() +
  scale_colour_discrete(name="Attribute",
                        labels = c("Sex", "Abortion", "Taxes",
                                   "LGBTQ+ Rights", "Environment", "Crime")) +
  labs(x = "Attribute Levels",
       y = "Marginal Mean") +
  theme_bw() +
  theme(text = element_text(size=16))
#dev.off()


## brazil models

#ideology
ideo_model_b <- cj(brazil,
                 chosen ~ sex + abort + tax + lgbtq + environ + crime,
                 id = ~id,
                 estimate = "mm",
                 by = ~ideo_di,
                 alpha = .05,
                 h0 = 0.5)


## argentina models

#ideology 
ideo_model_a <- cj(arg,
                   chosen ~ sex + abort + tax + lgbtq + environ + crime,
                   id = ~id,
                   estimate = "mm",
                   by = ~ideo_di,
                   alpha = .05,
                   h0 = 0.5)


# combine
ideo_model_b$country <- "Brazil"
ideo_model_a$country <- "Argentina"


ideo_model_b <- ideo_model_b %>% select(BY, feature, level, estimate, lower,
                                        upper, country)
ideo_model_a <- ideo_model_a %>% select(BY, feature, level, estimate, lower,
                                        upper, country)


combo <- rbind(ideo_model_b, ideo_model_a)


#Figure A11.4: Preferences for Candidate Attributes in Brazil and Argentina by Ideology 

#jpeg("app_figA11-4.jpeg.jpeg", res=600, width=8000, height=4000)
ggplot(combo, aes(x = level, y = estimate, linetype = BY, color = feature)) +
  geom_hline(yintercept = 0.5, colour = gray(1/2), lty = 2) + 
  geom_point(position=position_dodge(width = 0.5)) +
  geom_linerange(aes(x = level, 
                     ymin = lower,
                     ymax = upper),
                 position=position_dodge(0.5),
                 lwd = 0.75) + 
  coord_flip() +
  facet_wrap(~country) +
  scale_colour_discrete(name="Attribute",
                        labels = c("Sex", "Abortion", "Taxes",
                                   "LGBTQ+ Rights", "Environment", "Crime")) +
  scale_linetype_discrete(name="Ideology",
                          labels = c("Conservative", 'Progressive')) +
  labs(x = "Attribute Levels",
       y = "Marginal Mean") +
  theme_bw() +
  theme(text = element_text(size=16))
#dev.off()


########################### VICTIMIZATION MODELS ###############################

### brazil

#pooled
vic_b <- cj(brazil,
               chosen ~ crime,
               id = ~id,
               estimate = "mm",
               by = ~vic,
               alpha = .05)

#progressive econ
vic_prog_b <- cj(brazil_prog,
                        chosen ~ crime,
                        id = ~id,
                        estimate = "mm",
                        by = ~vic,
                        alpha = .05)
#progressive social
vic_cons_b <- cj(brazil_cons,
                          chosen ~ crime,
                          id = ~id,
                          estimate = "mm",
                          by = ~vic,
                          alpha = .05)

### argentina

#pooled
vic_a <- cj(arg,
            chosen ~ crime,
            id = ~id,
            estimate = "mm",
            by = ~vic,
            alpha = .05)

#progressive econ
vic_prog_a <- cj(arg_prog,
                     chosen ~ crime,
                     id = ~id,
                     estimate = "mm",
                     by = ~vic,
                     alpha = .05)

#progressive social
vic_cons_a <- cj(arg_cons,
                       chosen ~ crime,
                       id = ~id,
                       estimate = "mm",
                       by = ~vic,
                       alpha = .05)


# combine
vic_b$sample <- "Pooled"
vic_a$sample <- "Pooled"

vic_prog_b$sample <- "Progressive"
vic_prog_a$sample <- "Progressive"

vic_cons_b$sample <- "Conservative"
vic_cons_a$sample <- "Conservative"

vic_b$country <- "Brazil"
vic_prog_b$country <- "Brazil"
vic_cons_b$country <- "Brazil"

vic_a$country <- "Argentina"
vic_prog_a$country <- "Argentina"
vic_cons_a$country <- "Argentina"

vic_b <- vic_b %>% select(BY, feature, level, estimate, lower, upper,
                          sample, country)
vic_a <- vic_a %>% select(BY, feature, level, estimate, lower, upper,
                          sample, country)

vic_prog_b <- vic_prog_b %>% select(BY, feature, level, estimate, lower,
                                        upper, sample, country)
vic_prog_a <- vic_prog_a %>% select(BY, feature, level, estimate, lower,
                                            upper, sample, country)

vic_cons_a <- vic_cons_a %>% select(BY, feature, level, estimate, lower,
                                            upper, sample, country)
vic_cons_b <- vic_cons_b %>% select(BY, feature, level, estimate, lower,
                                                upper, sample, country)

combo2 <- rbind(vic_b, vic_a, vic_prog_b, vic_prog_a, vic_cons_b, vic_cons_a)

# Figure A11.5: Preferences for Candidate Crime Policy in Brazil and Argentina by Victimization
#jpeg("app_figA11-5.jpeg", res=600, width=8000, height=4000)
ggplot(combo2, aes(x = level, y = estimate, color = BY)) +
  geom_hline(yintercept = 0.5, colour = gray(1/2), lty = 2) + 
  geom_point(position=position_dodge(width = 0.5)) +
  geom_linerange(aes(x = level, 
                     ymin = lower,
                     ymax = upper),
                 position=position_dodge(0.5),
                 lwd = 0.75) + 
  coord_flip() +
  facet_wrap(~country + sample) +
  scale_y_continuous(labels = label_number(accuracy = 0.10),
                     breaks = c(0.45, 0.5, 0.55)) +
  scale_colour_brewer(palette = "Set1",
                      name="Victimization",
                      labels =c("Non-Victim", "Victim")) +
  labs(x = "Crime Policy",
       y = "Marginal Mean") +
  theme_bw() +
  theme(text = element_text(size=16))
#dev.off()


######################### COMMUNITY SAFETY MODELS ###############################

### brazil

#pooled
safe_b <- cj(brazil,
            chosen ~ crime,
            id = ~id,
            estimate = "mm",
            by = ~safety_di,
            alpha = .05)

#progressive econ
safe_prog_b <- cj(brazil_prog,
                     chosen ~ crime,
                     id = ~id,
                     estimate = "mm",
                     by = ~safety_di,
                     alpha = .05)
#progressive social
safe_cons_b <- cj(brazil_cons,
                       chosen ~ crime,
                       id = ~id,
                       estimate = "mm",
                       by = ~safety_di,
                       alpha = .05)
### argentina

#pooled
safe_a <- cj(arg,
            chosen ~ crime,
            id = ~id,
            estimate = "mm",
            by = ~safety_di,
            alpha = .05)

#progressive econ
safe_prog_a <- cj(arg_prog,
                     chosen ~ crime,
                     id = ~id,
                     estimate = "mm",
                     by = ~safety_di,
                     alpha = .05)

#progressive social
safe_cons_a <- cj(arg_cons,
                       chosen ~ crime,
                       id = ~id,
                       estimate = "mm",
                       by = ~safety_di,
                       alpha = .05)

# combine
safe_b$sample <- "Pooled"
safe_a$sample <- "Pooled"

safe_prog_b$sample <- "Progressive"
safe_prog_a$sample <- "Progressive"

safe_cons_b$sample <- "Conservative"
safe_cons_a$sample <- "Conservative"

safe_b$country <- "Brazil"
safe_prog_b$country <- "Brazil"
safe_cons_b$country <- "Brazil"

safe_a$country <- "Argentina"
safe_prog_a$country <- "Argentina"
safe_cons_a$country <- "Argentina"

safe_b <- safe_b %>% select(BY, feature, level, estimate, lower, upper,
                          sample, country)
safe_a <- safe_a %>% select(BY, feature, level, estimate, lower, upper,
                          sample, country)

safe_prog_b <- safe_prog_b %>% select(BY, feature, level, estimate, lower,
                                            upper, sample, country)
safe_prog_a <- safe_prog_a %>% select(BY, feature, level, estimate, lower,
                                            upper, sample, country)

safe_cons_a <- safe_cons_a %>% select(BY, feature, level, estimate, lower,
                                                upper, sample, country)
safe_cons_b <- safe_cons_b %>% select(BY, feature, level, estimate, lower,
                                                upper, sample, country)

combo3 <- rbind(safe_b, safe_a, safe_prog_b, safe_prog_a, safe_cons_b,
                safe_cons_a)

# Figure A11.6: Preferences for Candidate Crime Policy in Brazil and Argentina by Neighborhood Security
#jpeg("app_figA11-6.jpeg", res=600, width=8000, height=4000)
ggplot(combo3, aes(x = level, y = estimate, color = BY)) +
  geom_hline(yintercept = 0.5, colour = gray(1/2), lty = 2) + 
  geom_point(position=position_dodge(width = 0.5)) +
  geom_linerange(aes(x = level, 
                     ymin = lower,
                     ymax = upper),
                 position=position_dodge(0.5),
                 lwd = 0.75) + 
  coord_flip() +
  facet_wrap(~country + sample) +
  scale_y_continuous(labels = label_number(accuracy = 0.10),
                     breaks = c(0.45, 0.5, 0.55)) +
  scale_colour_brewer(palette = "Set1",
                      name="Neighb. Safety",
                      labels =c("Secure", "Insecure")) +
  labs(x = "Crime Policy",
       y = "Marginal Mean") +
  theme_bw() +
  theme(text = element_text(size=16))
#dev.off()


######################### SOCIAL ASSIST MODELS ###############################

### brazil

#pooled
assist_b <- cj(brazil,
             chosen ~ crime,
             id = ~id,
             estimate = "mm",
             by = ~assist_effect,
             alpha = .05)

#progressive econ
assist_prog_b <- cj(brazil_prog,
                      chosen ~ crime,
                      id = ~id,
                      estimate = "mm",
                      by = ~assist_effect,
                      alpha = .05)
#progressive social
assist_cons_b <- cj(brazil_cons,
                        chosen ~ crime,
                        id = ~id,
                        estimate = "mm",
                        by = ~assist_effect,
                        alpha = .05)
### argentina

#pooled
assist_a <- cj(arg,
             chosen ~ crime,
             id = ~id,
             estimate = "mm",
             by = ~assist_effect,
             alpha = .05)

#progressive econ
assist_prog_a <- cj(arg_prog,
                      chosen ~ crime,
                      id = ~id,
                      estimate = "mm",
                      by = ~assist_effect,
                      alpha = .05)

#progressive social
assist_cons_a <- cj(arg_cons,
                        chosen ~ crime,
                        id = ~id,
                        estimate = "mm",
                        by = ~assist_effect,
                        alpha = .05)

# combine
assist_b$sample <- "Pooled"
assist_a$sample <- "Pooled"

assist_prog_b$sample <- "Progressive"
assist_prog_a$sample <- "Progressive"

assist_cons_b$sample <- "Conservative"
assist_cons_a$sample <- "Conservative"

assist_b$country <- "Brazil"
assist_prog_b$country <- "Brazil"
assist_cons_b$country <- "Brazil"

assist_a$country <- "Argentina"
assist_prog_a$country <- "Argentina"
assist_cons_a$country <- "Argentina"

assist_b <- assist_b %>% select(BY, feature, level, estimate, lower, upper,
                            sample, country)
assist_a <- assist_a %>% select(BY, feature, level, estimate, lower, upper,
                            sample, country)

assist_prog_b <- assist_prog_b %>% select(BY, feature, level, estimate, lower,
                                              upper, sample, country)
assist_prog_a <- assist_prog_a %>% select(BY, feature, level, estimate, lower,
                                              upper, sample, country)

assist_cons_b <- assist_cons_b %>% select(BY, feature, level, estimate, lower,
                                                      upper, sample, country)
assist_cons_a <- assist_cons_a %>% select(BY, feature, level, estimate, lower,
                                                  upper, sample, country)

combo4 <- rbind(assist_b, assist_a, assist_prog_b, assist_prog_a,
                assist_cons_b, assist_cons_a)

# Figure A11.7: Preferences for Candidate Crime Policy in Brazil and Argentina by Social Assistance Effectiveness
#jpeg("app_figA11-7.jpeg", res=600, width=8000, height=4000)
ggplot(combo4, aes(x = level, y = estimate, color = BY)) +
  geom_hline(yintercept = 0.5, colour = gray(1/2), lty = 2) + 
  geom_point(position=position_dodge(width = 0.5)) +
  geom_linerange(aes(x = level, 
                     ymin = lower,
                     ymax = upper),
                 position=position_dodge(0.5),
                 lwd = 0.75) + 
  coord_flip() +
  facet_wrap(~country + sample) +
  scale_y_continuous(labels = label_number(accuracy = 0.10),
                     breaks = c(0.45, 0.5, 0.55)) +
  scale_colour_brewer(palette = "Set1",
                      name="Soc. Assist. Effective?",
                      labels =c("No", "Yes")) +
  labs(x = "Crime Policy",
       y = "Marginal Mean") +
  theme_bw() +
  theme(text = element_text(size=16))
#dev.off()


######################### CRIME:GANGS MODELS ###############################

### brazil

#pooled
gang_b <- cj(brazil,
               chosen ~ crime,
               id = ~id,
               estimate = "mm",
               by = ~crime_gang,
               alpha = .05)

#progressive econ
gang_prog_b <- cj(brazil_prog,
                        chosen ~ crime,
                        id = ~id,
                        estimate = "mm",
                        by = ~crime_gang,
                        alpha = .05)
#progressive social
gang_cons_b <- cj(brazil_cons,
                          chosen ~ crime,
                          id = ~id,
                          estimate = "mm",
                          by = ~crime_gang,
                          alpha = .05)
### argentina

#pooled
gang_a <- cj(arg,
               chosen ~ crime,
               id = ~id,
               estimate = "mm",
               by = ~crime_gang,
               alpha = .05)

#progressive econ
gang_prog_a <- cj(arg_prog,
                        chosen ~ crime,
                        id = ~id,
                        estimate = "mm",
                        by = ~crime_gang,
                        alpha = .05)

#progressive social
gang_cons_a <- cj(arg_cons,
                          chosen ~ crime,
                          id = ~id,
                          estimate = "mm",
                          by = ~crime_gang,
                          alpha = .05)

# combine
gang_b$sample <- "Pooled"
gang_a$sample <- "Pooled"

gang_prog_b$sample <- "Progressive"
gang_prog_a$sample <- "Progressive"

gang_cons_b$sample <- "Conservative"
gang_cons_a$sample <- "Conservative"

gang_b$country <- "Brazil"
gang_prog_b$country <- "Brazil"
gang_cons_b$country <- "Brazil"

gang_a$country <- "Argentina"
gang_prog_a$country <- "Argentina"
gang_cons_a$country <- "Argentina"

gang_b <- gang_b %>% select(BY, feature, level, estimate, lower, upper,
                                sample, country)
gang_a <- gang_a %>% select(BY, feature, level, estimate, lower, upper,
                                sample, country)

gang_prog_b <- gang_prog_b %>% select(BY, feature, level, estimate, lower,
                                                  upper, sample, country)
gang_cons_b <- gang_cons_b %>% select(BY, feature, level, estimate, lower,
                                                  upper, sample, country)

gang_prog_a <- gang_prog_a %>% select(BY, feature, level, estimate, lower,
                                                      upper, sample, country)
gang_cons_a <- gang_cons_a %>% select(BY, feature, level, estimate, lower,
                                                      upper, sample, country)

combo5 <- rbind(gang_b, gang_a, gang_prog_b, gang_prog_a, gang_cons_b,
                gang_cons_a)

# Figure A11.8: Preferences for Candidate Crime Policy in Brazil and Argentina by Perceived Source of Crime (Gangs)
#jpeg("app_figA11-8.jpeg", res=600, width=8000, height=4000)
ggplot(combo5, aes(x = level, y = estimate, color = BY)) +
  geom_hline(yintercept = 0.5, colour = gray(1/2), lty = 2) + 
  geom_point(position=position_dodge(width = 0.5)) +
  geom_linerange(aes(x = level, 
                     ymin = lower,
                     ymax = upper),
                 position=position_dodge(0.5),
                 lwd = 0.75) + 
  coord_flip() +
  facet_wrap(~country + sample) +
  scale_y_continuous(labels = label_number(accuracy = 0.10),
                     breaks = c(0.45, 0.5, 0.55)) +
  scale_colour_brewer(palette = "Set1",
                      name="Crime by Gangs?",
                      labels =c("No", "Yes")) +
  labs(x = "Crime Policy",
       y = "Marginal Mean") +
  theme_bw() +
  theme(text = element_text(size=16))
#dev.off()


