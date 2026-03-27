################################################################################
#### Title: Progressive Ideology and Support for Punitive Crime Policy
#### Author: Isabel Laterzo
#### Year: 2023
#### Journal: Comparative Political Studies
#### Portion of Analysis: Main Analysis for AmericasBarometer AND
#### Appendices 14, 15, and 17
################################################################################

# Clean Environment
rm(list = ls())

#packages - install if not previously installed on device
library(tidyverse)
library(stargazer)
library(nnet) #multinom

# uncomment below and set working directory using setwd() to directory which contains
# all data files resulting from cleaning completed in 2a_lapop_datacleaning.R
#setwd()

#read in data

data <- read.csv("lapop_2014_clean.csv") %>%
  select(-X)

#convert to factors
data[,c("vic","party_yn", "partyid", "prv_vs_pun",
        "vic_house", "country", "female",
        "etid")] <- sapply(data[,c("vic","party_yn", "partyid", "prv_vs_pun",
                                   "vic_house", "country", "female",
                                   "etid")], as.character)

#variable for if self or household has been vic
data$close_vic <- ifelse(data$vic == 1, 1,
                         ifelse(data$vic_house == 1, 1,
                                0))

#ethnicity variable
data$etid_new <- ifelse(data$etid == 1, "White",
                        ifelse(data$etid == 3, "Indigenous",
                               ifelse(data$etid == 4, "Black", "Other")))

data$etid_new <- factor(data$etid_new, ordered = F)
data$etid_new <- relevel(data$etid_new, ref = "White")

#variable for age
data$age <- 2014 - data$birth_year

#create crime_problem variable where everyone who says crime related issue
# is number one problem == 1, else == 0

data$crime_prob <- ifelse(#data$problem == 30 | #armed conflict
  data$problem == 05 | #crime
    data$problem == 11 | #drug addiction/consumption
    data$problem == 12 | #drug trafficking
    data$problem == 14 | #gangs
    data$problem == 31 | #kidnappings
    data$problem == 27 | #security (lack of)
    data$problem == 57,  #violence
  1,
  0)



#variable for community safety - flip scale so 4 is very safe, 1 is very unsafe
data$comm_safe <- ifelse(data$comm_safe == 1, 4,
                         ifelse(data$comm_safe == 2, 3,
                                ifelse(data$comm_safe == 3, 2,
                                       ifelse(data$comm_safe == 4, 1,
                                              NA))))

#variable for punitive preferences
data$punish <- ifelse(data$prv_vs_pun == 1, 0, #prevent = 0
                      ifelse(data$prv_vs_pun == 2, 1, #prevent + punish = 1
                             ifelse(data$prv_vs_pun == 3, 1, #punish = 1
                                    NA)))

#label left and right
data$left <- ifelse(data$left_right <= 5, 1,
                    ifelse(data$left_right > 5, 0,
                           NA))

#isolaate argentina and brazil data separately
brazil_data <- data %>% filter(country == "Brazil")
arg_data <- data %>% filter(country == "Argentina")


#isolate left and right
left_data <- data %>% filter(left == 1)
right_data <- data %>% filter(left == 0)


# countries isolated by left and right
brazil_left <- data %>%
  filter(country == "Brazil") %>%
  filter(left == 1)

brazil_right <- data %>%
  filter(country == "Brazil") %>%
  filter(left == 0)

arg_left <- data %>%
  filter(country == "Argentina") %>%
  filter(left == 1)

arg_right <- data %>%
  filter(country == "Argentina") %>%
  filter(left == 0)


## Model 1: Victimization, econ effectiveness, gangs

# stand alone
mod1_vic <- glm(punish ~ vic + left_right +  female + age + ed +
                  etid_new + country,
                  data = data,
             family = "binomial")

mod1_safe <- glm(punish ~ comm_safe + left_right +  female + age + ed +
                   etid_new + country,
                data = data,
                family = "binomial")

mod1_econ <- glm(punish ~ admin_econ + left_right +  female + age + ed +
                   etid_new + country,
                 data = data,
                 family = "binomial")

mod1_gangs <- glm(punish ~ neighb_gangs + left_right +  female + age + ed +
                   etid_new + country,
                 data = data,
                 family = "binomial")

mod1_all <- glm(punish ~ vic + comm_safe + admin_econ + neighb_gangs +
                  left_right +  female + age + ed +
                    etid_new +  country,
                  data = data,
                  family = "binomial")

## MAIN TEXT: TABLE 3
stargazer(mod1_vic, mod1_safe, mod1_econ, mod1_gangs, mod1_all,
          covariate.labels = c("Victim", "Community Safety",
                               "Admin Effective Econ",
                               "Neighb. Gangs", "Right Wing",
                               "Female", "Age", "Education",
                               "Race/Eth: Black",
                               "\\hspace{1cm}Indigenous",
                               "\\hspace{1cm}Other",
                               "Brazil",
                               "Constant"),
          no.space = TRUE,
          font.size = "tiny",
          dep.var.labels.include = FALSE)



# left models
mod1_left_vic <- glm(punish ~ vic +  female + age + ed +
                  etid_new + country,
                data = left_data,
                family = "binomial")

mod1_left_safe <- glm(punish ~ comm_safe +  female + age + ed +
                   etid_new + country,
                 data = left_data,
                 family = "binomial")

mod1_left_econ <- glm(punish ~ admin_econ +  female + age + ed +
                   etid_new + country,
                 data = left_data,
                 family = "binomial")

mod1_left_gangs <- glm(punish ~ neighb_gangs +  female + age + ed +
                    etid_new + country,
                  data = left_data,
                  family = "binomial")

mod1_left_all <- glm(punish ~ vic + comm_safe + admin_econ + neighb_gangs +
                   female + age + ed +
                  etid_new +  country,
                data = left_data,
                family = "binomial")


# right models
mod1_right_vic <- glm(punish ~ vic +  female + age + ed +
                       etid_new + country,
                     data = right_data,
                     family = "binomial")

mod1_right_safe <- glm(punish ~ comm_safe +  female + age + ed +
                        etid_new + country,
                      data = right_data,
                      family = "binomial")

mod1_right_econ <- glm(punish ~ admin_econ + female + age + ed +
                        etid_new + country,
                      data = right_data,
                      family = "binomial")

mod1_right_gangs <- glm(punish ~ neighb_gangs + female + age + ed +
                         etid_new + country,
                       data = right_data,
                       family = "binomial")

mod1_right_all <- glm(punish ~ vic + comm_safe + admin_econ + neighb_gangs +
                       female + age + ed +
                       etid_new +  country,
                     data = right_data,
                     family = "binomial")

#### Appendix Table A14.1: Logistic Regression AmericasBarometer Models Segmented by Ideology
### Please note: In newer versions of R, there are some issues with creating stargazer
### tables for many models. For this reason, they are broken up here

# models 1-4
stargazer(mod1_left_vic, mod1_left_safe, mod1_left_econ, mod1_left_gangs,
          covariate.labels = c("Victim", "Community Safety",
                               "Admin Effective Econ",
                               "Neighb. Gangs",
                               "Female", "Age", "Education",
                               "Race/Eth: Black",
                               "\\hspace{1cm}Indigenous",
                               "\\hspace{1cm}Other",
                               "Brazil",
                               "Constant"),
          no.space = TRUE,
          font.size = "tiny",
          dep.var.labels.include = FALSE)

#models 6-9
stargazer(mod1_right_vic, mod1_right_safe, mod1_right_econ, mod1_right_gangs, 
          covariate.labels = c("Victim", "Community Safety",
                     "Admin Effective Econ",
                     "Neighb. Gangs",
                     "Female", "Age", "Education",
                     "Race/Eth: Black",
                     "\\hspace{1cm}Indigenous",
                     "\\hspace{1cm}Other",
                     "Brazil",
                     "Constant"),
          no.space = TRUE,
          font.size = "tiny",
          dep.var.labels.include = FALSE)

# models 5 and 10
stargazer(mod1_left_all, mod1_right_all,
          covariate.labels = c("Victim", "Community Safety",
                               "Admin Effective Econ",
                               "Neighb. Gangs",
                               "Female", "Age", "Education",
                               "Race/Eth: Black",
                               "\\hspace{1cm}Indigenous",
                               "\\hspace{1cm}Other",
                               "Brazil",
                               "Constant"),
          no.space = TRUE,
          font.size = "tiny",
          dep.var.labels.include = FALSE)


# Brazil
mod1b_vic <- glm(punish ~ vic + left_right +  female + age + ed +
                   etid_new,
                data = brazil_data,
                family = "binomial")

mod1b_safe <- glm(punish ~ comm_safe + left_right +  female + age + ed +
                   etid_new,
                 data = brazil_data,
                 family = "binomial")

mod1b_econ <- glm(punish ~ admin_econ + left_right +  female + age + ed +
                   etid_new,
                 data = brazil_data,
                 family = "binomial")

mod1b_gangs <- glm(punish ~ neighb_gangs + left_right +  female + age + ed +
                    etid_new,
                  data = brazil_data,
                  family = "binomial")

mod1b_all <- glm(punish ~ vic + comm_safe + admin_econ + neighb_gangs +
                  left_right +  female + age + ed +
                  etid_new,
                data = brazil_data,
                family = "binomial")


# Argentina
mod1a_vic <- glm(punish ~ vic + left_right +  female + age + ed +
                   etid_new,
                 data = arg_data,
                 family = "binomial")

mod1a_safe <- glm(punish ~ comm_safe + left_right +  female + age + ed +
                    etid_new,
                  data = arg_data,
                  family = "binomial")

mod1a_econ <- glm(punish ~ admin_econ + left_right +  female + age + ed +
                    etid_new,
                  data = arg_data,
                  family = "binomial")

mod1a_gangs <- glm(punish ~ neighb_gangs + left_right +  female + age + ed +
                     etid_new,
                   data = arg_data,
                   family = "binomial")

mod1a_all <- glm(punish ~ vic + comm_safe + admin_econ + neighb_gangs +
                  left_right +  female + age + ed +
                  etid_new,
                data = arg_data,
                family = "binomial")



# Appendix Table A14.2 : Logistic Regression AmericasBarometer Models Segmented by Country

# Argentina
stargazer(mod1a_vic, mod1a_safe, mod1a_econ, mod1a_gangs, mod1a_all,
  covariate.labels = c("Victim", "Community Safety",
                       "Admin Effective Econ",
                       "Neighb. Gangs", "Right-Wing", "Female",
                       "Age", "Edu", "Race/Eth: Black",
                       "\\hspace{1cm}Indigenous",
                       "\\hspace{1cm}Other",
                       "Constant"),
  no.space = TRUE,
  font.size = "tiny",
  title = "Country Models",
  dep.var.labels.include = FALSE)



# Brazil
stargazer(mod1b_vic, mod1b_safe, mod1b_econ, mod1b_gangs, mod1b_all,
          covariate.labels = c("Victim", "Community Safety",
                               "Admin Effective Econ",
                               "Neighb. Gangs", "Right-Wing", "Female",
                               "Age", "Edu", "Race/Eth: Black",
                               "\\hspace{1cm}Indigenous",
                               "\\hspace{1cm}Other",
                               "Constant"),
          no.space = TRUE,
          font.size = "tiny",
          title = "Country Models",
          dep.var.labels.include = FALSE)




### Models broken down by country AND ideology


# Argentina -- Left
mod1aleft_vic <- glm(punish ~ vic + female + age + ed +
                   etid_new,
                 data = arg_left,
                 family = "binomial")

mod1aleft_safe <- glm(punish ~ comm_safe + female + age + ed +
                    etid_new,
                  data = arg_left,
                  family = "binomial")

mod1aleft_econ <- glm(punish ~ admin_econ + female + age + ed +
                    etid_new,
                  data = arg_left,
                  family = "binomial")

mod1aleft_gangs <- glm(punish ~ neighb_gangs + female + age + ed +
                     etid_new,
                   data = arg_left,
                   family = "binomial")

mod1aleft_all <- glm(punish ~ vic + comm_safe + admin_econ + neighb_gangs +
                   female + age + ed +
                   etid_new,
                 data = arg_left,
                 family = "binomial")



# Argentina -- Right
mod1aright_vic <- glm(punish ~ vic + female + age + ed +
                       etid_new,
                     data = arg_right,
                     family = "binomial")

mod1aright_safe <- glm(punish ~ comm_safe + female + age + ed +
                        etid_new,
                      data = arg_right,
                      family = "binomial")

mod1aright_econ <- glm(punish ~ admin_econ + female + age + ed +
                        etid_new,
                      data = arg_right,
                      family = "binomial")

mod1aright_gangs <- glm(punish ~ neighb_gangs + female + age + ed +
                         etid_new,
                       data = arg_right,
                       family = "binomial")

mod1aright_all <- glm(punish ~ vic + comm_safe + admin_econ + neighb_gangs +
                      female + age + ed +
                       etid_new,
                     data = arg_right,
                     family = "binomial")


# Appendix Table A14.3 : Logistic Regression AmericasBarometer Models: Argentina Segmented by Ideology

# Argentina Left

# models 1-4
stargazer(mod1aleft_vic, mod1aleft_safe, mod1aleft_econ, mod1aleft_gangs,
  covariate.labels = c("Victim", "Community Safety",
                       "Admin Effective Econ",
                       "Neighb. Gangs","Female",
                       "Age", "Edu", "Race/Eth: Black",
                       "\\hspace{1cm}Indigenous",
                       "\\hspace{1cm}Other",
                       "Constant"),
  no.space = TRUE,
  font.size = "tiny",
  title = "Argentina Ideology Models: Left",
  dep.var.labels.include = FALSE)

# model 5
stargazer(mod1aleft_all,
          covariate.labels = c("Victim", "Community Safety",
                                            "Admin Effective Econ",
                                            "Neighb. Gangs","Female",
                                            "Age", "Edu", "Race/Eth: Black",
                                            "\\hspace{1cm}Indigenous",
                                            "\\hspace{1cm}Other",
                                            "Constant"),
          no.space = TRUE,
          font.size = "tiny",
          title = "Argentina Ideology Models: Left",
          dep.var.labels.include = FALSE)         

# Argentina Right

#models 6-9
stargazer(mod1aright_vic, mod1aright_safe, mod1aright_econ, mod1aright_gangs,
          covariate.labels = c("Victim", "Community Safety",
                               "Admin Effective Econ",
                               "Neighb. Gangs","Female",
                               "Age", "Edu", "Race/Eth: Black",
                               "\\hspace{1cm}Indigenous",
                               "\\hspace{1cm}Other",
                               "Constant"),
          no.space = TRUE,
          font.size = "tiny",
          title = "Argentina Ideology Models: Right",
          dep.var.labels.include = FALSE)

# model 10
stargazer(mod1aright_all,
          covariate.labels = c("Victim", "Community Safety",
                               "Admin Effective Econ",
                               "Neighb. Gangs","Female",
                               "Age", "Edu", "Race/Eth: Black",
                               "\\hspace{1cm}Indigenous",
                               "\\hspace{1cm}Other",
                               "Constant"),
          no.space = TRUE,
          font.size = "tiny",
          title = "Argentina Ideology Models: Right",
          dep.var.labels.include = FALSE)  



# Brazil -- Left
mod1bleft_vic <- glm(punish ~ vic + female + age + ed +
                       etid_new,
                     data = brazil_left,
                     family = "binomial")

mod1bleft_safe <- glm(punish ~ comm_safe + female + age + ed +
                        etid_new,
                      data = brazil_left,
                      family = "binomial")

mod1bleft_econ <- glm(punish ~ admin_econ + female + age + ed +
                        etid_new,
                      data = brazil_left,
                      family = "binomial")

mod1bleft_gangs <- glm(punish ~ neighb_gangs + female + age + ed +
                         etid_new,
                       data = brazil_left,
                       family = "binomial")

mod1bleft_all <- glm(punish ~ vic + comm_safe + admin_econ + neighb_gangs +
                       female + age + ed +
                       etid_new,
                     data = brazil_left,
                     family = "binomial")



# Brazil -- Right
mod1bright_vic <- glm(punish ~ vic + female + age + ed +
                        etid_new,
                      data = brazil_right,
                      family = "binomial")

mod1bright_safe <- glm(punish ~ comm_safe + female + age + ed +
                         etid_new,
                       data = brazil_right,
                       family = "binomial")

mod1bright_econ <- glm(punish ~ admin_econ + female + age + ed +
                         etid_new,
                       data = brazil_right,
                       family = "binomial")

mod1bright_gangs <- glm(punish ~ neighb_gangs + female + age + ed +
                          etid_new,
                        data = brazil_right,
                        family = "binomial")

mod1bright_all <- glm(punish ~ vic + comm_safe + admin_econ + neighb_gangs +
                        female + age + ed +
                        etid_new,
                      data = brazil_right,
                      family = "binomial")

# Appendix Table A14.4 : Logistic Regression AmericasBarometer Models: Argentina Segmented by Ideology

# Brazil Left

# models 1-4
stargazer(mod1bleft_vic, mod1bleft_safe, mod1bleft_econ, mod1bleft_gangs,
  covariate.labels = c("Victim", "Community Safety",
                       "Admin Effective Econ",
                       "Neighb. Gangs", "Female",
                       "Age", "Edu", "Race/Eth: Black",
                       "\\hspace{1cm}Indigenous",
                       "\\hspace{1cm}Other",
                       "Constant"),
  no.space = TRUE,
  font.size = "tiny",
  title = "Brazil Ideology Models: Left",
  dep.var.labels.include = FALSE)

# model 5
stargazer(mod1bleft_all,
          covariate.labels = c("Victim", "Community Safety",
                               "Admin Effective Econ",
                               "Neighb. Gangs", "Female",
                               "Age", "Edu", "Race/Eth: Black",
                               "\\hspace{1cm}Indigenous",
                               "\\hspace{1cm}Other",
                               "Constant"),
          no.space = TRUE,
          font.size = "tiny",
          title = "Brazil Ideology Models: Left",
          dep.var.labels.include = FALSE)

# Brazil Right

# models 6-9
stargazer(mod1bright_vic, mod1bright_safe, mod1bright_econ, mod1bright_gangs,
          covariate.labels = c("Victim", "Community Safety",
                               "Admin Effective Econ",
                               "Neighb. Gangs", "Female",
                               "Age", "Edu", "Race/Eth: Black",
                               "\\hspace{1cm}Indigenous",
                               "\\hspace{1cm}Other",
                               "Constant"),
          no.space = TRUE,
          font.size = "tiny",
          title = "Brazil Ideology Models: Right",
          dep.var.labels.include = FALSE)

# model 10
stargazer(mod1bright_all,
          covariate.labels = c("Victim", "Community Safety",
                               "Admin Effective Econ",
                               "Neighb. Gangs", "Female",
                               "Age", "Edu", "Race/Eth: Black",
                               "\\hspace{1cm}Indigenous",
                               "\\hspace{1cm}Other",
                               "Constant"),
          no.space = TRUE,
          font.size = "tiny",
          title = "Brazil Ideology Models: Right",
          dep.var.labels.include = FALSE)




################ Multinomial Logistic Regression Models #########################


### Pooled
mod1_vic_multi <- multinom(prv_vs_pun ~ vic + left_right +  female + age + ed + 
                             etid_new + country,
                           data = data)

mod1_safe_multi <- multinom(prv_vs_pun ~ comm_safe + left_right +  female + age + ed +
                   etid_new + country,
                 data = data)

mod1_econ_multi <- multinom(prv_vs_pun ~ admin_econ + left_right +  female + age + ed +
                   etid_new + country,
                 data = data)

mod1_gangs_multi <- multinom(prv_vs_pun ~ neighb_gangs + left_right +  female + age + ed +
                    etid_new + country,
                  data = data)

mod1_all_multi <- multinom(prv_vs_pun ~ vic + comm_safe + admin_econ + neighb_gangs +
                  left_right +  female + age + ed +
                  etid_new +  country,
                data = data)


# Appendix Table A14.5 : Multinomial Logistic Regression AmericasBarometer Models 

# models 1-8
stargazer(mod1_vic_multi, mod1_safe_multi, mod1_econ_multi, mod1_gangs_multi,
          covariate.labels = c("Victim", "Comm. Safety",
                               "Admin Effective Econ",
                               "Neighb. Gangs", "Right-Wing",
                               "Female", "Age", "Edu",
                               "Race/Eth: Black",
                               "\\hspace{1cm}Indigenous",
                               "\\hspace{1cm}Other",
                               "Brazil", "Constant"),
          column.labels = c("Punish", "Both","Punish", "Both", 
                            "Punish", "Both","Punish", "Both"),
          no.space = TRUE,
          font.size = "tiny")

# models 9 - 10
stargazer(mod1_all_multi, 
          covariate.labels = c("Victim", "Comm. Safety",
                                               "Admin Effective Econ",
                                               "Neighb. Gangs", "Right-Wing",
                                               "Female", "Age", "Edu",
                                               "Race/Eth: Black",
                                               "\\hspace{1cm}Indigenous",
                                               "\\hspace{1cm}Other",
                                               "Brazil", "Constant"),
          column.labels = c("Punish", "Both"),
          no.space = TRUE,
          font.size = "tiny")

################################## Appendix 15 ################################
########################### Left-Right Validity ###############################

mod <- lm(left_right ~ abort + gay_mar + ed + female + country,
          data = data)

stargazer(mod,
          covariate.labels = c("Support Abortion",
                               "Support Same-Sex Marriage",
                               "Education",
                               "Female",
                               "Brazil"),
          no.space = TRUE,
          font.size = "tiny",
          dep.var.labels = "Left-Right ID")



################################## Appendix 16 ################################
###################### Additional Victimization Models ##########################

###### Logistic Regression Models #####
## 1) Victim of violent crime
mod1_vic_logit <- glm(punish ~ vic_vio + left_right + female + age + ed +
                    etid_new + country,
                  data = data,
                  family = "binomial")

## 2) Household victim
mod2_vic_logit <- glm(punish ~ vic_house + left_right + female + age + ed +
                    etid_new + country,
                  data = data,
                  family = "binomial")

# Appendix Table A16.1 : Logistic Regression AmericasBarometer Models
# Alternative Measures of Victimization
stargazer(mod1_vic_logit, mod2_vic_logit,
          covariate.labels = c("Victim: Violent", "Household Victim",
                               "Right-Wing",
                               "Female", "Age", "Edu",
                               "Race/Eth: Black",
                               "\\hspace{1cm}Indigenous",
                               "\\hspace{1cm}Other",
                               "Income",
                               "Brazil", "Constant"),
          no.space = TRUE,
          font.size = "tiny",
          model.numbers = FALSE,
          title = "Victimization Logit Models")


# Appendix Table A16.2 : Multinomial Logistic Regression AmericasBarometer Models
# Alternative Measures of Victimization

## 1) Victim of violent crime
mod1_vic_multi <- multinom(prv_vs_pun ~ vic_vio + left_right + female + age + ed +
                         etid_new + income + country,
                       data = data)


## 2) Household victim
mod2_vic_multi <- multinom(prv_vs_pun ~ vic_house + left_right + female + age + ed +
                         etid_new + income + country,
                       data = data)

# models 
stargazer(mod1_vic_multi, mod2_vic_multi,
          covariate.labels = c("Victim: Violent", "Household Victim",
                               "Right-Wing",
                               "Female", "Age", "Edu",
                               "Race/Eth: Black",
                               "\\hspace{1cm}Indigenous",
                               "\\hspace{1cm}Other",
                               "Income",
                               "Brazil", "Constant"),
          column.labels = c("Punish", "Both","Punish", "Both"),
          no.space = TRUE,
          font.size = "tiny",
          model.numbers = FALSE,
          title = "Victimization Multinomial Models")



################################## Appendix 17 ################################
############ Differences between Argentina & Brazil LAPOP Samples #############


# Examine descriptions between countries

brazil <- data %>% filter(country == "Brazil")

arg <- data %>% filter(country == "Argentina")



# #1 police response - seems to be a difference

table(brazil$time_police) #police seem to respond far slower
summary(brazil$time_police)

table(arg$time_police) #police seem to respond far faster
summary(arg$time_police)



# #2 main problem - relatively similar in terms of crime being main problem
table(brazil$crime_prob) 
summary(brazil$crime_prob)
table(brazil$crime_prob)[2]/nrow(brazil) 


table(arg$crime_prob) 
summary(arg$crime_prob)
table(arg$crime_prob)[2]/nrow(arg) #higher in Argentina

# #3 satisfaction with police performance
table(brazil$pol_satisf) #police seem to respond far slower
summary(brazil$pol_satisf)

table(arg$pol_satisf) #police seem to respond far faster
summary(arg$pol_satisf)



