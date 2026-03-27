set.seed(123)
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
################# Table A6 #####################
################################################

# Pooled
reg1 <- lm(response_s1 ~ treatment_s1, oia)

# Democrats
reg1_dems <- lm(response_s1 ~ treatment_s1, dems)

# Independents
reg1_indep <- lm(response_s1 ~ treatment_s1, indep)

# Republicans
reg1_reps <- lm(response_s1 ~ treatment_s1, reps)

#### Create table
modelsummary(list("Pooled" = reg1,
                  "Democrats" = reg1_dems,
                  "Independents" = reg1_indep,
                  "Republicans" = reg1_reps),
             stars = T,
             coef_map = c('(Intercept)' = 'Intercept',
                          'treatment_s1Imm Office' = 'OIA'),
             vcov = "HC1")


################################################
################ Table A7 ######################
################################################

# Pooled + controls
reg1_rr <- lm(response_s1 ~ treatment_s1 + age + gender + educ 
              + income + ethnicity + region, oia)
# Democrats
reg1_dems_rr <- lm(response_s1 ~ treatment_s1 + age + gender + educ 
                   + income + ethnicity + region, dems)

# Independents
reg1_indep_rr <- lm(response_s1 ~ treatment_s1 + age + gender + educ 
                    + income + ethnicity + region, indep) 

# Republicans
reg1_reps_rr = lm(response_s1 ~ treatment_s1 + age + gender + educ 
                  + income + ethnicity + region, reps)

# Create table
modelsummary(list("Pooled Adjusted" = reg1_rr,
                  "Democrats Adjusted" = reg1_dems_rr,
                  "Independents Adjusted" = reg1_indep_rr,
                  "Republicans Adjusted" = reg1_reps_rr),
             stars = T,
             coef_map = c('(Intercept)' = 'Intercept',
                          'treatment_s1Imm Office' = 'OIA'), #remove this for SM2
             vcov = "HC1")

################################################
################ Table A8 ###################### 
################################################

# Pooled
reg1 <- lm(response_s1 ~ treatment_s1, oia)

# Liberals
reg1_lib <- lm(response_s1 ~ treatment_s1, liberal)

# Moderates
reg1_mod <- lm(response_s1 ~ treatment_s1, moderate)

# Conservatives
reg1_con <- lm(response_s1 ~ treatment_s1, conserv)

####
modelsummary(list("Pooled" = reg1,
                  "Liberal" = reg1_lib,
                  "Moderate" = reg1_mod,
                  "Conservative" = reg1_con),
             stars = T,
             coef_map = c('(Intercept)' = 'Intercept', #remove this for SM2
                          'treatment_s1Imm Office' = 'OIA'), #remove this for SM2
             vcov = "HC1")


################################################
################ Table A9 ###################### 
################################################

# Pooled 
reg1_rr <- lm(response_s1 ~ treatment_s1 + age + gender + educ 
              + income + ethnicity + region, oia)

# Liberals
reg1_lib_rr <- lm(response_s1 ~ treatment_s1 + age + gender + educ 
                  + income + ethnicity + region, liberal)

# Moderates
reg1_mod_rr <- lm(response_s1 ~ treatment_s1 + age + gender + educ 
                  + income + ethnicity + region, moderate) 


# Conservatives
reg1_con_rr = lm(response_s1 ~ treatment_s1 + age + gender + educ 
                 + income + ethnicity + region, conserv)



modelsummary(list("Pooled Adjusted" = reg1_rr,
                  "Liberals Adjusted" = reg1_lib_rr,
                  "Moderates Adjusted" = reg1_mod_rr,
                  "Conservatives Adjusted" = reg1_con_rr),
             stars = T,
             vcov = "HC1",
             coef_map = c('(Intercept)' = 'Intercept', #remove this for SM2
                          'treatment_s1Imm Office' = 'OIA'), #remove this for SM2
             out = "default")

################################################
############### Table A10 ###################### 
################################################

# Subset respondents
oia %>%
  mutate(White = factor(case_when(ethnicity == "White" ~ "White",
                                  NA ~ "NA",
                                  TRUE ~ "Non-White")) ) -> oia

# Pooled unadjusted
reg1_ethn <- lm(response_s1 ~ treatment_s1*White, oia)

# Pooled adjusted with controls
reg1_ethn_adj <- lm(response_s1 ~ treatment_s1*White + age + gender + educ 
                    + income + region, oia)

# Table
modelsummary(list(Unadjusted = reg1_ethn,
                  Adjusted_eth = reg1_ethn_adj),
             stars = T,
             vcov = "HC1",
             coef_map = c('(Intercept)' = 'Intercept', #remove this for SM2
                          'treatment_s1Imm Office' = 'OIA', #remove this for SM2
                          'WhiteWhite' = "White", #remove this for SM2
                          'treatment_s1Imm Office:WhiteWhite' = 'OIA x White'), #remove this for SM2
             out = "default")


################################################
############### Table A11 ###################### 
################################################

# Subset respondents
regs_nonwhite <- subset(oia, White == "Non-White")
regs_white <- subset(oia, White == "White")

# Non-White respondents only
regs1_nonwhite_pid <- lm(response_s1 ~ treatment_s1*as.factor(pid_3level), regs_nonwhite)

# White respondents only
regs1_white_pid <- lm(response_s1 ~ treatment_s1*as.factor(pid_3level), regs_white)

# Table
modelsummary(list(NonWhite = regs1_nonwhite_pid,
                  White = regs1_white_pid),
             stars = T,
             vcov = "HC1",
             coef_map = c('(Intercept)' = 'Intercept', # Remove for SM2
                          'treatment_s1Imm Office' = 'OIA', # Remove for SM2
                          'as.factor(pid_3level)2' = 'Independents', # Remove for SM2
                          'as.factor(pid_3level)3' = "Republicans", # Remove for SM2
                          'treatment_s1Imm Office:as.factor(pid_3level)2' = 'OIA x Independents', # Remove for SM2
                          'treatment_s1Imm Office:as.factor(pid_3level)3' = 'OIA x Republicans'), # Remove for SM2
             output = "default")

################################################
############### Table A12 ###################### 
################################################

# Non-White respondents with controls
regs1_nonwhite_pid_adj <- lm(response_s1 ~ treatment_s1*as.factor(pid_3level)
                             + age + gender + educ + income + region, regs_nonwhite)

# White respondents with controls
regs1_white_pid_adj <- lm(response_s1 ~ treatment_s1*as.factor(pid_3level)
                          + age + gender + educ + income + region, regs_white)

# Table
modelsummary(list(NonWhite = regs1_nonwhite_pid_adj,
                  White = regs1_white_pid_adj),
             stars = T,
             vcov = "HC1",
             coef_map = c('(Intercept)' = 'Intercept', # Remove for SM2
                          'treatment_s1Imm Office' = 'OIA', # Remove for SM2
                          'as.factor(pid_3level)2' = 'Independents', # Remove for SM2
                          'as.factor(pid_3level)3' = "Republicans", # Remove for SM2
                          'treatment_s1Imm Office:as.factor(pid_3level)2' = 'OIA x Independents', # Remove for SM2
                          'treatment_s1Imm Office:as.factor(pid_3level)3' = 'OIA x Republicans'), # Remove for SM2
             output = "default")


################################################
############## Survey Exp #2 ###################
################################################


################################################
############ Table A13 #########################
################################################

# Pooled survey
reg2 <- lm(response_s2_gen ~ treatment_s2_gen, oia)

# Democrats
reg2_dems <- lm(response_s2_gen ~ treatment_s2_gen, dems)

# Independents
reg2_indep <- lm(response_s2_gen ~ treatment_s2_gen, indep)

# Republicans
reg2_reps <- lm(response_s2_gen ~ treatment_s2_gen, reps)

# Table
modelsummary(list("Pooled" = reg2,
                  "Democrats" = reg2_dems,
                  "Independets" = reg2_indep,
                  "Republicans" = reg2_reps),
             stars = T,
             vcov = "HC1",
             coef_map = c('(Intercept)' = "Intercept",
                          'treatment_s2_genAccess Undoc' = 'Access Open to All',
                          'treatment_s2_genDoc Only' = "Documented Only"),
             out = "default")

#################################################
############ Table A14 ##########################
#################################################

# Pooled with controls
reg2_rr <- lm(response_s2_gen ~ treatment_s2_gen + age + gender + educ 
              + income + ethnicity + region, oia)

# Democrats with controls
reg2_dems_rr <- lm(response_s2_gen ~ treatment_s2_gen + age + gender + educ 
                   + income + ethnicity + region, dems)

# Independents with controls
reg2_indep_rr <- lm(response_s2_gen ~ treatment_s2_gen + age + gender + educ 
                    + income + ethnicity + region, indep) 

# Republicans with controls
reg2_reps_rr = lm(response_s2_gen ~ treatment_s2_gen + age + gender + educ
                  + income + ethnicity + region, reps)

# Table
modelsummary(list("Pooled" = reg2_rr,
                  "Democrats" = reg2_dems_rr,
                  "Independets" = reg2_indep_rr,
                  "Republicans" = reg2_reps_rr),
             stars = T,
             vcov = "HC1",
             coef_map = c('(Intercept)' = "Intercept", # Remove for SM2
                          'treatment_s2_genAccess Undoc' = 'Access Open to All', # Remove for SM2
                          'treatment_s2_genDoc Only' = "Documented Only"), # Remove for SM2
             out = "default")


#################################################
############ Table A15 ##########################
#################################################

# Non-white respondents
regs2_nonwhite_pid_rr <- lm(response_s2_gen ~ treatment_s2_gen*as.factor(pid_3level), regs_nonwhite)

# White respondents
regs2_white_pid_rr <- lm(response_s2_gen ~ treatment_s2_gen*as.factor(pid_3level), regs_white)

# Table
modelsummary(list(NonWhite = regs2_nonwhite_pid_rr,
                  White = regs2_white_pid_rr),
             stars = T,
             vcov = "HC1",
             coef_map = c('(Intercept)' = "Intercept", # Remove for SM2
                          'treatment_s2_genAccess Undoc' = 'Access Open to All', # Remove for SM2
                          'treatment_s2_genDoc Only' = "Documented Only", # Remove for SM2
                          'as.factor(pid_3level)2' = 'Independents', # Remove for SM2
                          'as.factor(pid_3level)3' = "Republicans", # Remove for SM2
                          'treatment_s2_genAccess Undoc:as.factor(pid_3level)2' = 'Access Open to All × Independents', # Remove for SM2
                          'treatment_s2_genDoc Only:as.factor(pid_3level)2' = 'Documented Only × Independents', # Remove for SM2
                          'treatment_s2_genAccess Undoc:as.factor(pid_3level)3' = 'Access Open to All × Republicans', # Remove for SM2
                          'treatment_s2_genDoc Only:as.factor(pid_3level)3' = 'Documented Only x Republicans'), # Remove for SM2
             output = "default")



#################################################
############ Table A16 ##########################
#################################################

# Non-White respondents adjusted
regs2_nonwhite_pid_adj_rr <- lm(response_s2_gen ~ treatment_s2_gen*as.factor(pid_3level)
                                + age + gender + educ + income + region, regs_nonwhite)

# White respondents adjusted
regs2_white_pid_adj_rr <- lm(response_s2_gen ~ treatment_s2_gen*as.factor(pid_3level)
                             + age + gender + educ + income + region, regs_white)

# Table
modelsummary(list(NonWhite = regs2_nonwhite_pid_adj_rr,
                  White = regs2_white_pid_adj_rr),
             stars = T,
             vcov = "HC1",
             coef_map = c('(Intercept)' = "Intercept", # Remove for SM2
                          'treatment_s2_genAccess Undoc' = 'Access Open to All', # Remove for SM2
                          'treatment_s2_genDoc Only' = "Documented Only", # Remove for SM2
                          'as.factor(pid_3level)2' = 'Independents', # Remove for SM2
                          'as.factor(pid_3level)3' = "Republicans", # Remove for SM2
                          'treatment_s2_genAccess Undoc:as.factor(pid_3level)2' = 'Access Open to All × Independents', # Remove for SM2
                          'treatment_s2_genDoc Only:as.factor(pid_3level)2' = 'Documented Only × Independents', # Remove for SM2
                          'treatment_s2_genAccess Undoc:as.factor(pid_3level)3' = 'Access Open to All × Republicans', # Remove for SM2
                          'treatment_s2_genDoc Only:as.factor(pid_3level)3' = 'Documented Only x Republicans'), # Remove for SM2
             output = "default")


#################################################
############ Figure A1 ##########################
#################################################

plot_summs(reg1_rr, reg1_dems_rr, reg1_indep_rr, reg1_reps_rr,
           ci_level = .95,
           inner_ci_level = .90,
           coefs = c("Intercept" = "(Intercept)",
                     "Immigration\nOffice" = "treatment_s1Imm Office"),
           model.names = c("Pooled",
                           "Democrats",
                           "Independents",
                           "Republicans"),
           colors = c("Black",
                      "Blue",
                      "Purple",
                      "Red"),
           robust = c("HC1", "HC1", "HC1", "HC1"),
           legend.title = "") +
  theme(legend.position = "top",
        legend.text=element_text(size=10),
        axis.text.y = element_text(size=rel(1.2)),
        axis.text.x = element_text(size=rel(1.25))) +
  xlab("Estimate")
ggsave("fig_a1.jpeg")



#################################################
############ Figure A2 ##########################
#################################################

plot_summs(reg1_rr, reg1_lib_rr, reg1_mod_rr, reg1_con_rr,
           ci_level = .95,
           inner_ci_level = .90,
           coefs = c("Intercept" = "(Intercept)",
                     "Immigration\nOffice" = "treatment_s1Imm Office"),
           model.names = c("Pooled",
                           "Liberals",
                           "Moderate",
                           "Conservative"),
           colors = c("Black",
                      "Blue",
                      "Purple",
                      "Red"),
           robust = c("HC1", "HC1", "HC1", "HC1"),
           legend.title = "") +
  theme(legend.position = "top",
        legend.text=element_text(size=10),
        axis.text.y = element_text(size=rel(1.2)),
        axis.text.x = element_text(size=rel(1.25))) +
  xlab("Estimate")
ggsave("fig_a2.jpeg")



#################################################
############ Figure A3 ##########################
#################################################

# Plot 
plot_summs(reg2_rr, reg2_dems_rr, reg2_indep_rr, reg2_reps_rr,
           ci_level = .95,
           inner_ci_level = .90,
           coefs = c("Intercept" = "(Intercept)",
                     "Open to all" = "treatment_s2_genAccess Undoc",
                     "Access\nDocumented\nOnly" = "treatment_s2_genDoc Only"),
           model.names = c("Pooled",
                           "Democrats",
                           "Independents",
                           "Republicans"),
           colors = c("Black",
                      "Blue",
                      "Purple",
                      "Red"),
           robust = c("HC1", "HC1", "HC1", "HC1"),
           legend.title = "") +
  theme(legend.position = "top",
        legend.text=element_text(size=10),
        axis.text.y = element_text(size=rel(1.2)),
        axis.text.x = element_text(size=rel(1.25))) +
  xlab("Estimate")
ggsave("fig_a3.jpeg")



#################################################
############ Figure A4 ##########################
#################################################

# Pooled 
reg2 <- lm(response_s2_gen ~ treatment_s2_gen + age + gender + educ
           + income + ethnicity + region, oia)

# Liberals adjusted 
reg2_lib <- lm(response_s2_gen ~ treatment_s2_gen + age + gender + educ
               + income + ethnicity + region, liberal)

# Moderates
reg2_mod <- lm(response_s2_gen ~ treatment_s2_gen + age + gender + educ
               + income + ethnicity + region, moderate)

# Conservatives
reg2_con <- lm(response_s2_gen ~ treatment_s2_gen + age + gender + educ
               + income + ethnicity + region, conserv)

# Plot results
plot_summs(reg2, reg2_lib, reg2_mod, reg2_con,
           ci_level = .95,
           inner_ci_level = .90,
           coefs = c("Intercept" = "(Intercept)",
                     "Open to all" = "treatment_s2_genAccess Undoc",
                     "Access\nDocumented\nOnly" = "treatment_s2_genDoc Only"),
           model.names = c("Pooled",
                           "Liberals",
                           "Moderate",
                           "Conservative"),
           colors = c("Black",
                      "Blue",
                      "Purple",
                      "Red"),
           robust = c("HC1", "HC1", "HC1", "HC1"),
           legend.title = "") +
  theme(legend.position = "top",
        legend.text=element_text(size=10),
        axis.text.y = element_text(size=rel(1.2)),
        axis.text.x = element_text(size=rel(1.25))) +
  xlab("Estimate")
ggsave("fig_a4.jpeg")

#################################################    
############ Tabular results of Fig A4 ##########
#################################################    
### Included as Table 7 in SM2 

# Table
modelsummary(list("Pooled" = reg2,
                  "Liberals" = reg2_lib,
                  "Moderates" = reg2_mod,
                  "Conservatives" = reg2_con),
             stars = T,
             vcov = "HC1",
             out = "default")





