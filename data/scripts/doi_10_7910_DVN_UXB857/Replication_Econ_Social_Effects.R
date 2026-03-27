library(gtsummary) # v1.4.1
library(jtools) #v2.1.3
library(modelsummary) #v0.7.0
library(sandwich) #v3.0-1
library(stargazer) #v5.2.2
library(scales) #v1.1.1
library(tidyverse) #v1.3.1
library(vtable) #v1.3.1

### Load data
oia <- read.csv("oia_apsr_final_replication.csv")

## Remove Rs that failed attention checks
oia %>%
  filter(informed_consent == 1 & attention_check1 == 3 & attention_check2 == 1) %>%
  filter(!is.na(attention_check1)) -> oia


#### Set reference labels 
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
         treatment_s2_econ = relevel(factor(treatment_s2_econ),
                                            ref = "Control"),
         treatment_s2_soc = relevel(factor(treatment_s2_soc),
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
############### Table A18 ######################
################################################

# Pooled
reg1_rr_friend <- lm(response_s1_friend ~ treatment_s1_friend, oia)

# Democrats
reg1_dems_rr_friend <- lm(response_s1_friend ~ treatment_s1_friend,
                          dems) 

# Independents
reg1_indep_rr_friend <- lm(response_s1_friend ~ treatment_s1_friend,
                           indep) 

# Republicans
reg1_reps_rr_friend <- lm(response_s1_friend ~ treatment_s1_friend,
                          reps) 


modelsummary(list("Pooled" = reg1_rr_friend,
                  "Democrats" = reg1_dems_rr_friend,
                  "Independents" = reg1_indep_rr_friend,
                  "Republicans" = reg1_reps_rr_friend),
             stars = T,
             vcov = "HC1",
             coef_map = c('(Intercept)' = 'Intercept',
                          'treatment_s1_friendImm Office' = 'OIA'),
             out = "default")

################################################
############### Table A19 ######################
################################################

# Pooled adjusted
reg1_rr_friend_adj <- lm(response_s1_friend ~ treatment_s1_friend 
                         + age + gender + educ + income + ethnicity + region,
                         data = oia)

# Democrats adjusted
reg1_dems_rr_friend_adj <- lm(response_s1_friend ~ treatment_s1_friend + age + gender 
                              + educ + income + ethnicity + region,
                              dems) 

# Independents adjusted
reg1_indep_rr_friend_adj <- lm(response_s1_friend ~ treatment_s1_friend + age + gender 
                               + educ + income + ethnicity + region,
                               indep) 

# Republicans adjusted
reg1_reps_rr_friend_adj <- lm(response_s1_friend ~ treatment_s1_friend + age + gender 
                              + educ + income + ethnicity + region,
                              reps) 

## Table
modelsummary(list("Pooled" = reg1_rr_friend_adj,
                  "Democrats" = reg1_dems_rr_friend_adj,
                  "Independents" = reg1_indep_rr_friend_adj,
                  "Republicans" = reg1_reps_rr_friend_adj),
             stars = T,
             vcov = "HC1",
             coef_map = c('(Intercept)' = 'Intercept', # Remove for SM2
                          'treatment_s1_friendImm Office' = 'OIA'), # Remove for SM2
             out = "default")



################################################
############### Economic #######################
############# Consequences #####################
################################################

################################################
############### Table A20 ######################
################################################


# Pooled
reg2_rr_econ <- lm(response_s2_econ ~ treatment_s2_econ, oia)

# Democrats
reg2_dems_rr_econ <- lm(response_s2_econ ~ treatment_s2_econ, dems)

# Independents
reg2_indep_rr_econ <- lm(response_s2_econ ~ treatment_s2_econ, indep)

# Republicans
reg2_reps_rr_econ <- lm(response_s2_econ ~ treatment_s2_econ, reps)


## Table
modelsummary(list("Pooled" = reg2_rr_econ,
                  "Democrats" = reg2_dems_rr_econ,
                  "Independents" = reg2_indep_rr_econ,
                  "Republicans" = reg2_reps_rr_econ),
             stars = T,
             vcov = "HC1",
             coef_map = c('(Intercept)' = 'Intercept',
                          'treatment_s2_econAccess Undoc' = 'Access Open to All',
                          'treatment_s2_econDoc Only' = 'Documented Only'),
             out = "default")

################################################
############### Table A21 ######################
################################################


# Pooled
reg2_rr_econ_adj <- lm(response_s2_econ ~ treatment_s2_econ 
                       + age + gender + educ + income + ethnicity + region, oia)


# Dems 
reg2_dems_rr_econ_adj <- lm(response_s2_econ ~ treatment_s2_econ 
                            + age + gender + educ + income + ethnicity + region, dems) 

# Indep 
reg2_indep_rr_econ_adj <- lm(response_s2_econ ~ treatment_s2_econ 
                             + age + gender + educ + income + ethnicity + region, indep) 

# Rep 
reg2_reps_rr_econ_adj = lm(response_s2_econ ~ treatment_s2_econ 
                           + age + gender + educ + income + ethnicity + region, reps)

## Table
modelsummary(list("Pooled" = reg2_rr_econ_adj,
                  "Democrats" = reg2_dems_rr_econ_adj,
                  "Independents" = reg2_indep_rr_econ_adj,
                  "Republicans" = reg2_reps_rr_econ_adj),
             stars = T,
             vcov = "HC1",
             coef_map = c('(Intercept)' = 'Intercept', # Remove for SM2 
                          'treatment_s2_econAccess Undoc' = 'Access Open to All', # Remove for SM2
                          'treatment_s2_econDoc Only' = 'Documented Only'), # Remove for SM2
             out = "default")


################################################
################ Social ########################
############# Consequences #####################
################################################

################################################
############### Table A22 ######################
################################################

# Pooled
reg2_rr_soc <- lm(response_s2_soc ~ treatment_s2_soc, oia)

# Dems 
reg2_dems_rr_soc <- lm(response_s2_soc ~ treatment_s2_soc, dems) 

# Indep
reg2_indep_rr_soc <- lm(response_s2_soc ~ treatment_s2_soc, indep) 

# Rep
reg2_reps_rr_soc = lm(response_s2_soc ~ treatment_s2_soc, reps)

## Table
modelsummary(list("Pooled" = reg2_rr_soc,
                  "Democrats" = reg2_dems_rr_soc,
                  "Independents" = reg2_indep_rr_soc,
                  "Republicans" = reg2_reps_rr_soc),
             stars = T,
             vcov = "HC1",
             coef_map = c('(Intercept)' = 'Intercept',
                          'treatment_s2_socAccess Undoc' = 'Access Open to All',
                          'treatment_s2_socDoc Only' = 'Documented Only'),
             out = "default")



################################################
############### Table A23 ######################
################################################

reg2_rr_soc_adj <- lm(response_s2_soc ~ treatment_s2_soc 
                      + age + gender + educ + income + ethnicity + region, oia)

# Dems
reg2_dems_rr_soc_adj <- lm(response_s2_soc ~ treatment_s2_soc 
                           + age + gender + educ + income + ethnicity + region, dems) 

# Indep
reg2_indep_rr_soc_adj <- lm(response_s2_soc ~ treatment_s2_soc 
                            + age + gender + educ + income + ethnicity + region, indep) 

# Rep
reg2_reps_rr_soc_adj = lm(response_s2_soc ~ treatment_s2_soc 
                          + age + gender + educ + income + ethnicity + region, reps)


## Table
modelsummary(list("Pooled" = reg2_rr_soc_adj,
                  "Democrats" = reg2_dems_rr_soc_adj,
                  "Independents" = reg2_indep_rr_soc_adj,
                  "Republicans" = reg2_reps_rr_soc_adj),
             stars = T,
             vcov = "HC1",
             coef_map = c('(Intercept)' = 'Intercept', # Remove for SM2
                          'treatment_s2_socAccess Undoc' = 'Access Open to All', # Remove for SM2
                          'treatment_s2_socDoc Only' = 'Documented Only'), # Remove for SM2
             out = "default")

