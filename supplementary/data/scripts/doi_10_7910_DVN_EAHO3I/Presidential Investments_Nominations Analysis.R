#####################################
##### PRESIDENTIAL INVESTMENTS  #####
##### REPLICATION: 11/11/2022   #####
##### R Version: 4.0.2          #####
#####################################

#######################
##### 1.0 SET UP  #####
#######################

##### 1.1 Clearing the R Environment
rm(list = ls())

##### 1.2 Setting Working Directory
#### INSTRUCTION: You will need to change the working directory to the location of the data.
setwd("F:\\Research\\Presidential Investment\\_DATA")

##### 1.3 Loading Packages
#### INSTRUCTION: Install any missing packages
#install.packages("tidyverse")
library(tidyverse)
#install.packages("stargazer")
library(stargazer)
#install.packages("stringr")
library(stringr)
#install.packages("sandwich")
library(sandwich)
#install.packages("AER")
library(AER)
#install.packages("sjPlot")
library(sjPlot)
#install.packages("readxl")
library(readxl)
#install.packages("magrittr")
library(magrittr)
#install.packages("survminer")
library(survminer)
#install.packages("survival")
library(survival)
#install.packages("haven")
library(haven)
#install.packages("mediation")
library(mediation)

##############################
##### 2.0 DATA CLEANING  #####
##############################

##### 2.1 Loading Appointments Data
appointee_data <- read_dta("plum_complete_00_20.dta") %>%
                  filter(dep != "CONGRESS") %>%
                  filter(dep != "JUDICIAL")


####################################
##### 3.0 DESCRIPTIVE ANALYSIS #####
##### TABLE 1                  #####
####################################

##### 3.1 Descriptive Statistics About the Number of Positions
nrow(appointee_data[appointee_data$year == 2000,])
nrow(appointee_data[appointee_data$year == 2008,])
nrow(appointee_data[appointee_data$year == 2016,])
nrow(appointee_data[appointee_data$year == 2020,])


##### 3.2 Vacancies
nrow(appointee_data[appointee_data$vacant == 1,])
nrow(appointee_data[appointee_data$year == 2000 & appointee_data$vacant == 1,])
nrow(appointee_data[appointee_data$year == 2008 & appointee_data$vacant == 1,])
nrow(appointee_data[appointee_data$year == 2016 & appointee_data$vacant == 1,])
nrow(appointee_data[appointee_data$year == 2020 & appointee_data$vacant == 1,])

##### 3.3 Nominations Raw
nrow(appointee_data[appointee_data$vacant == 1 & appointee_data$Nominee == 1,])
nrow(appointee_data[appointee_data$year == 2000 & appointee_data$vacant == 1 & appointee_data$Nominee == 1,])
nrow(appointee_data[appointee_data$year == 2008 & appointee_data$vacant == 1 & appointee_data$Nominee == 1,])
nrow(appointee_data[appointee_data$year == 2016 & appointee_data$vacant == 1 & appointee_data$Nominee == 1,])
nrow(appointee_data[appointee_data$year == 2020 & appointee_data$vacant == 1 & appointee_data$Nominee == 1,])

##### 3.4 Nominations Percent
nrow(appointee_data[appointee_data$vacant == 1 & appointee_data$Nominee == 1,])/nrow(appointee_data[appointee_data$vacant == 1,])
nrow(appointee_data[appointee_data$year == 2000 & appointee_data$vacant == 1 & appointee_data$Nominee == 1,])/nrow(appointee_data[appointee_data$year == 2000 & appointee_data$vacant == 1,])
nrow(appointee_data[appointee_data$year == 2008 & appointee_data$vacant == 1 & appointee_data$Nominee == 1,])/nrow(appointee_data[appointee_data$year == 2008 & appointee_data$vacant == 1,])
nrow(appointee_data[appointee_data$year == 2016 & appointee_data$vacant == 1 & appointee_data$Nominee == 1,])/nrow(appointee_data[appointee_data$year == 2016 & appointee_data$vacant == 1,])
nrow(appointee_data[appointee_data$year == 2020 & appointee_data$vacant == 1 & appointee_data$Nominee == 1,])/nrow(appointee_data[appointee_data$year == 2020 & appointee_data$vacant == 1,])

##### 3.5 Days to Nomination
mean(appointee_data$Days[ appointee_data$vacant == 1], na.rm  = T)
sd(appointee_data$Days[appointee_data$vacant == 1], na.rm  = T)
mean(appointee_data$Days[appointee_data$year == 2000 & appointee_data$vacant == 1], na.rm  = T)
mean(appointee_data$Days[appointee_data$year == 2008 & appointee_data$vacant == 1], na.rm  = T)
mean(appointee_data$Days[appointee_data$year == 2016 & appointee_data$vacant == 1], na.rm  = T)
mean(appointee_data$Days[appointee_data$year == 2020 & appointee_data$vacant == 1], na.rm  = T)
max(appointee_data$Days, na.rm = T)

##### 3.6 Day 1 Announcements
sum(appointee_data$Nominee[appointee_data$Days == 0 & appointee_data$vacant == 1], na.rm = T)/nrow(appointee_data)
1-mean(appointee_data$Nominee[appointee_data$vacant == 1 & appointee_data$year != 2020], na.rm =T)
1-mean(appointee_data$Nominee[appointee_data$vacant == 1 & appointee_data$year == 2020], na.rm =T)

##### 3.7 Policy Positions versus Management Positions
mean(appointee_data$specpol, na.rm = T)
mean(appointee_data$keymgt, na.rm = T)

##### 3.8 Policy positions
mean(appointee_data$Bpriority[appointee_data$vacant ==1], na.rm = T)
mean(appointee_data$Bpriority[appointee_data$vacant ==1 & appointee_data$year == 2000], na.rm = T)
mean(appointee_data$Bpriority[appointee_data$vacant ==1 & appointee_data$year == 2008], na.rm = T)
mean(appointee_data$Bpriority[appointee_data$vacant ==1 & appointee_data$year == 2016], na.rm = T)
mean(appointee_data$Bpriority[appointee_data$vacant ==1 & appointee_data$year == 2020], na.rm = T)


##### 3.9 Ideology
appointee_data <- appointee_data %>%
                  mutate(adjideo = adjideo+2) %>% #Adjusting Ideo so it ranges form 0 to 2.
                  mutate(pideo = Bpriority*adjideo)


mean(appointee_data$adjideo[appointee_data$vacant == 1], na.rm = T)
sd(appointee_data$adjideo[appointee_data$vacant == 1], na.rm = T)

min(appointee_data$adjideo[appointee_data$vacant == 1], na.rm = T)
max(appointee_data$adjideo[appointee_data$vacant == 1], na.rm = T)

##### 3.10 Workforce Skills
mean(appointee_data$workforce_skill[appointee_data$vacant == 1], na.rm = T)
sd(appointee_data$workforce_skill[appointee_data$vacant == 1], na.rm = T)
min(appointee_data$workforce_skill[appointee_data$vacant == 1], na.rm = T)
max(appointee_data$workforce_skill[appointee_data$vacant == 1], na.rm = T)

##### 3.11 Agency Structure
mean(appointee_data$eop[appointee_data$vacant == 1], na.rm = T)
mean(appointee_data$cabinet[appointee_data$vacant == 1], na.rm = T)
mean(appointee_data$commission[appointee_data$vacant == 1], na.rm = T)
mean(appointee_data$indadmin[appointee_data$vacant == 1], na.rm = T)
mean(appointee_data$other[appointee_data$vacant == 1], na.rm = T)

table(appointee_data$afe)

##### 3.12 Grant Giving Agency
mean(appointee_data$ggiver[appointee_data$vacant == 1], na.rm = T)

##### 3.13 Descriptive Statistics About Pay Level
mean(appointee_data$plevel[appointee_data$vacant == 1], na.rm = T)
sd(appointee_data$plevel[appointee_data$vacant == 1], na.rm = T)

##### 3.14  Part Time
mean(appointee_data$PartTime[appointee_data$vacant == 1], na.rm = T)
sd(appointee_data$PartTime[appointee_data$vacant == 1], na.rm = T)

##### 3.6 Descriptive Statistics for Specific Positions
mean(appointee_data$ambassador[appointee_data$vacant == 1], na.rm = T)
mean(appointee_data$IG[appointee_data$vacant == 1], na.rm = T)
mean(appointee_data$USMarshal[appointee_data$vacant == 1], na.rm = T)
mean(appointee_data$USAttorney[appointee_data$vacant == 1], na.rm = T)


#########################################
##### 4.0 ANALYSIS - All Presidents #####
##### TABLE 2                       #####
#########################################

##### 4.1 Model 1: No Strata
model1 <- coxph(Surv(Days, Nominee) ~  specpol+keymgt+Bpriority+adjideo+pideo+ambassador+USMarshal+USAttorney+IG+plevel+PartTime+eop+commission+ggiver+y1+y2+y3,
                data = appointee_data[appointee_data$vacant ==1,],
                cluster = appointee_data$dep[appointee_data$vacant ==1],
                robust =  T)

model1_robustse <- as.data.frame(summary(model1)[[7]]) %>% dplyr::select('robust se') %>% rename(rse = 'robust se')

model1_wald_mgtpol <- linearHypothesis(model1, 
                                        c("specpol = keymgt"),
                                        white.adjust =  T)


model1_wald_prio <- linearHypothesis(model1, 
                                       c("Bpriority = 0", "pideo = 0"),
                                       white.adjust =  T)


##### 4.2 Model 2: Strata w/o Agency Characteristics
model2 <- coxph(Surv(Days, Nominee) ~  specpol+keymgt+Bpriority+adjideo+pideo+ambassador+USMarshal+USAttorney+IG+plevel+PartTime+ggiver+y1+y2+y3+strata(committee1)+strata(afe),
                data = appointee_data[appointee_data$vacant ==1,],
                cluster = appointee_data$dep[appointee_data$vacant ==1],
                robust =  T)

model2_nobush <- coxph(Surv(Days, Nominee) ~  specpol+keymgt+Bpriority+adjideo+pideo+ambassador+USMarshal+USAttorney+IG+plevel+PartTime+ggiver+y1+y2+y3+y4+strata(committee1)+strata(afe),
                data = appointee_data[appointee_data$vacant ==1 &appointee_data$y1 == 0 ,],
                cluster = appointee_data$dep[appointee_data$vacant ==1 &appointee_data$y1 == 0],
                robust =  T)

model2_robustse <- as.data.frame(summary(model2)[[7]]) %>% dplyr::select('robust se') %>% rename(rse = 'robust se')

model2_wald_mgtpol <- linearHypothesis(model2, 
                                       c("specpol = keymgt"),
                                       white.adjust =  T)

##### 4.3 Model 3: Strata w/ Agency Characteristics
model3 <- coxph(Surv(Days, Nominee) ~  specpol+keymgt+Bpriority+adjideo+pideo+workforce_skill+ambassador+USMarshal+USAttorney+IG+plevel+PartTime+eop+commission+ggiver+y3+strata(committee1),
                data = appointee_data[appointee_data$vacant ==1 & appointee_data$Concurrent == 0,],
                cluster = appointee_data$dep[appointee_data$vacant ==1 & appointee_data$Concurrent == 0],
                robust =  T)

model3_robustse <- as.data.frame(summary(model3)[[7]]) %>% dplyr::select('robust se') %>% rename(rse = 'robust se')

model3_wald_mgtpol <- linearHypothesis(model3, 
                                       c("specpol = keymgt"),
                                       white.adjust =  T)

##### 4.3 Model 4: Grants
model4 <- coxph(Surv(Days, Nominee) ~  specpol+keymgt+Bpriority+adjideo+pideo+workforce_skill+ambassador+USMarshal+USAttorney+IG+plevel+PartTime+ggiver+y3+strata(committee1)+strata(afe),
                data = appointee_data[appointee_data$vacant ==1 & appointee_data$Concurrent == 0,],
                cluster = appointee_data$dep[appointee_data$vacant ==1 & appointee_data$Concurrent == 0],
                robust =  T)

model4_robustse <- as.data.frame(summary(model4)[[7]]) %>% dplyr::select('robust se') %>% rename(rse = 'robust se')


model4_wald_mgtpol <- linearHypothesis(model4, 
                                       c("specpol = keymgt"),
                                       white.adjust =  T)

star_labels <- c("Policy Position (0,1)", "Management Position(0,1)", "Presidential Priority (0,1)", "Agency Ideology (0.00,3.87)", "Presidential Priority*Agency Ideology", "Agency Skills (-2.53,2.21)","EOP (0,1)", "Commission (0,1)",  "Pay Level (0-5)", "Part Time (0,1)", "Grant Giver (0,1)", "Ambassador (0,1)", "Inspector General (0,1)", "US Attorney (0,1)", "US Marshal (0,1)", "Bush (0,1)", "Obama (0,1)", "Trump (0,1)")
star_order <- paste("^",c("specpol","keymgt","Bpriority","adjideo","pideo","workforce_skill","eop","commission", "plevel","PartTime","ggiver", "ambassador", "IG","USAttorney", "USMarshal","y1","y2","y3"), "$", sep = "")
#### 4.4 Output of Stargazer
stargazer(model1, model2,  model3, model4,
          digits = 2,
          type = "html",
          star.cutoffs = c(0.1, 0.05),
          title = "Estimated Days to First Nomination: Bush, Obama, Trump, and Biden Administrations",
          #omit = c("ambassador", "USAttorney", "USMarshal",  "Log(scale)"),
          covariate.labels = star_labels,
          order = star_order,
          single.row = TRUE,
          digits.extra =  0, 
          no.space = FALSE,
          se = c(model1_robustse, model2_robustse, model3_robustse, model4_robustse),
          intercept.bottom =  TRUE,
          add.lines = list(c("Estimator", "Cox", "Cox", "Cox", "Cox"),
                           c("Position-Type Controls", "Yes", "Yes", "Yes", "Yes"),
                           c("Department Level Stratified", "No", "Yes", "No", "Yes"),
                           c("Committee Stratified", "No", "Yes", "Yes", "Yes")),
          notes = "Note: *significant at the 0.05 level ; +significant at the 0.10 level in two-tailed tests.  All estimates use type HC0 standard errors clustered at the department level. Position-type controls include indicators for ambassadors, U.S. marshals, and U.S. attorneys.",
          out = "Tab1_Nominees_cox_full.doc")

##### 4.5 Hazard Ratios
exp(coef(model2)[1])
exp(coef(model2)[2])
exp(coef(model3)[3])
exp(coef(model4)[3])
exp(coef(model2_nobush)[3])
exp(coef(model2)[4]*4)
exp(coef(model4)[6]*-2.56)
exp(coef(model4)[6]*2.21)


##### 4.6 Model 5: Log Grants
model3_loggrants <- coxph(Surv(Days, Nominee)~  specpol+keymgt+Bpriority+adjideo+pideo+workforce_skill+ambassador+USMarshal+USAttorney+IG+plevel+PartTime+eop+commission+lngrant+y3+strata(committee1),
                          data = appointee_data[appointee_data$year != 2000,],
                          cluster = appointee_data$dep_fe[appointee_data$year != 2000],
                          robust =  T)

##### 4.7 Model 6: Log Grants+Skills
model4_loggrants <- coxph(Surv(Days, Nominee)~ specpol+keymgt+Bpriority+adjideo+pideo+workforce_skill+ambassador+USMarshal+USAttorney+IG+plevel+PartTime+lngrant+y3+strata(committee1)+strata(afe),
                          data = appointee_data[appointee_data$year == 2016,],
                          cluster = appointee_data$dep_fe[appointee_data$year == 2016],
                          robust =  T)


############################
##### 5.0 TOBIT MODELS #####
##### TABLE B2         #####
############################


##### 5.1 Model 1: No Strata
model6 <- survreg(Surv(Days, Nominee) ~  specpol+keymgt+Bpriority+adjideo+pideo+ambassador+USMarshal+USAttorney+IG+plevel+PartTime+eop+commission+ggiver+y1+y2+y3,
                  data = appointee_data[appointee_data$vacant ==1,],
                  dist = "gaussian")
model6_clust <- vcovCL(model6, cluster = appointee_data$dep_fe, type = "HC1")
model6_clust_est <- coeftest(model6, model6_clust)
model6_adj <- update(model6, . ~ 1)
model6_pseudor <- 1 - as.vector(logLik(model6)/logLik(model6_adj))

##### 5.2 Model 2: Strata w/o Agency Characteristics
model7 <- survreg(Surv(Days, Nominee) ~  specpol+keymgt+Bpriority+adjideo+pideo+ambassador+USMarshal+USAttorney+IG+plevel+PartTime+ggiver+y1+y2+y3+committee1+afe,
                  data = appointee_data[appointee_data$vacant ==1,],
                  dist = "gaussian")
model7_clust <- vcovCL(model7, cluster = appointee_data$dep_fe, type = "HC1")
model7_clust_est <- coeftest(model7, model7_clust)
model7_adj <- update(model7, . ~ 1)
model7_pseudor <- 1 - as.vector(logLik(model7)/logLik(model7_adj))

##### 5.3 Model 3: Strata w/ Agency Characteristics
model8 <- survreg(Surv(Days, Nominee) ~  specpol+keymgt+Bpriority+adjideo+pideo+workforce_skill+ambassador+USMarshal+USAttorney+IG+plevel+PartTime+eop+commission+ggiver+y3+committee1,
                  data = appointee_data[appointee_data$vacant ==1,],
                  dist = "gaussian")
model8_clust <- vcovCL(model8, cluster = appointee_data$dep_fe, type = "HC1")
model8_clust_est <- coeftest(model8, model8_clust)
model8_adj <- update(model8, . ~ 1)
model8_pseudor <- 1 - as.vector(logLik(model8)/logLik(model8_adj))


##### 5.4 Model 4: Grants
model9 <- survreg(Surv(Days, Nominee) ~  specpol+keymgt+Bpriority+adjideo+pideo+workforce_skill+ambassador+USMarshal+USAttorney+IG+plevel+PartTime+ggiver+y3+committee1+afe,
                  data = appointee_data[appointee_data$vacant ==1,],
                  dist = "gaussian")
model9_clust <- vcovCL(model9, cluster = appointee_data$dep_fe, type = "HC1")
model9_clust_est <- coeftest(model9, model9_clust)
model9_adj <- update(model9, . ~ 1)
model9_pseudor <- 1 - as.vector(logLik(model9)/logLik(model9_adj))

star_labels <- c("Policy Position (0,1)", "Management Position(0,1)", "Presidential Priority (0,1)", "Agency Ideology (0.00,3.87)", "Presidential Priority*Agency Ideology", "Agency Skills (-2.53,2.21)","EOP (0,1)", "Commission (0,1)",  "Pay Level (0-5)", "Part Time (0,1)", "Grant Giver (0,1)", "Ambassador (0,1)", "Inspector General (0,1)", "US Attorney (0,1)", "US Marshal (0,1)", "Bush (0,1)", "Obama (0,1)", "Trump (0,1)")
star_order <- paste("^",c("specpol","keymgt","Bpriority","adjideo","pideo","workforce_skill","eop","commission", "plevel","PartTime","ggiver", "ambassador", "IG","USAttorney", "USMarshal","y1","y2","y3"), "$", sep = "")

#### 5.5 Output of Stargazer
stargazer(model6_clust_est, model7_clust_est,  model8_clust_est, model9_clust_est,
          digits = 2,
          type = "html",
          star.cutoffs = c(0.1, 0.05),
          title = "Estimated Days to First Nomination: Bush, Obama, Trump, and Biden Administrations",
          omit = c("committee1", "afe"),
          covariate.labels = star_labels,
          order = star_order,
          single.row = TRUE,
          digits.extra =  0, 
          no.space = FALSE,
          intercept.bottom =  TRUE,
          add.lines = list(c("Estimator", "Tobit", "Tobit", "Tobit", "Tobit"),
                           c("Department Fixed Effects", "No", "Yes", "No", "No"),
                           c("Committee Fixed", "No", "Yes", "Yes", "Yes"),
                           c("N", nobs(model6), nobs(model7), nobs(model8), nobs(model9)),
                           c("Psuedo-R2", round(model6_pseudor,2), round(model7_pseudor,2), round(model8_pseudor,2), round(model9_pseudor,2))),
          notes = "Note: *significant at the 0.05 level ; +significant at the 0.10 level in two-tailed tests.  All estimates use type HC0 standard errors clustered at the department level. Position-type controls include indicators for ambassadors, U.S. marshals, and U.S. attorneys. Goodness-of-fit for tobit models reported as McFadden's Pseudo-R2 (McFadden 1974).",
          out = "AppendixB_Tobits_full.doc")




###################################
##### 6.0 MEDIATION ANALYSIS ######
##### TABLE F4               ######
###################################

set.seed(123)

##### 6.1 Correlations Between Key Variables
cor(appointee_data$Bpriority[appointee_data$vacant == 1], appointee_data$adjideo[appointee_data$vacant == 1], use = "complete.obs")
cor(appointee_data$Bpriority[appointee_data$vacant == 1], appointee_data$workforce_skill[appointee_data$vacant == 1], use = "complete.obs")


##### 6.2 Priority Function
priority_function <- glm(Bpriority ~ adjideo, 
                         data = appointee_data[appointee_data$vacant ==1 & appointee_data$Concurrent == 0 & appointee_data$afe != "DVA",],
                         family = binomial(link = "probit"))



model_med <- lm(Days ~  specpol+keymgt+Bpriority+adjideo+pideo+ambassador+USMarshal+USAttorney+IG+plevel+PartTime+commission+ggiver+y1+y2+y3+committee1+afe,
                data = appointee_data[appointee_data$vacant ==1 & appointee_data$Concurrent == 0 &  appointee_data$afe != "DVA",])


##### 6.3 Mediation 
med_ideo <- mediate(model.m=priority_function, model.y=model_med, treat='adjideo', mediator = 'Bpriority', data=appointee_data[appointee_data$vacant ==1 & appointee_data$Concurrent == 0 & appointee_data$afe != "DVA",], dropobs = T, cluster = appointee_data$dep[appointee_data$vacant ==1 & appointee_data$Concurrent == 0 & appointee_data$afe != "DVA"])

#### 6.3.1 This is the Ideology Half of Table F4
summary(med_ideo)


##### 6.4 Priority Function
priority_function <- glm(Bpriority ~ workforce_skill, data = appointee_data[appointee_data$vacant ==1 & appointee_data$Concurrent == 0 & appointee_data$afe != "DVA",],
                         family = binomial(link = "probit"))



model_med <- lm(Days ~  specpol+keymgt+Bpriority+adjideo+pideo+workforce_skill+ambassador+USMarshal+USAttorney+IG+plevel+PartTime+commission+ggiver+y3+committee1+afe,
                data = appointee_data[appointee_data$vacant ==1 & appointee_data$Concurrent == 0 &  appointee_data$afe != "DVA",])


##### 6.5 Mediation 
med_skills <- mediate(model.m=priority_function, model.y=model_med, treat='workforce_skill', mediator = 'Bpriority', data=appointee_data[appointee_data$vacant ==1 & appointee_data$Concurrent == 0 & appointee_data$afe != "DVA",], dropobs = T, cluster = appointee_data$dep[appointee_data$vacant ==1 & appointee_data$Concurrent == 0 & appointee_data$afe != "DVA"])

summary(med_skills)



###########################################
##### 7.0 RULEMAKING ROBUSTNESS CHECK #####
##### TABLES F1 AND F2                #####
###########################################

#### 7.1.1 Loading Data
rulemaking <- read.csv("Rulemaking_Major_Rules.csv") %>%
  dplyr::select(-X)

#### 7.1.2 Merging
appointee_data <- appointee_data %>%
  filter(year != 2000) 


appointee_data <-   left_join(appointee_data, rulemaking) %>%
  mutate(MAJOR_RULES_IND = ifelse(is.na(MAJOR_RULES_IND), 0, MAJOR_RULES_IND ),
         MAJOR_RULES_COUNT = ifelse(is.na(MAJOR_RULES_COUNT), 0, MAJOR_RULES_COUNT)) %>% 
  mutate(obama_rules_count = MAJOR_RULES_COUNT*y2,
         trump_rules_count = MAJOR_RULES_COUNT*y3,
         obama_rules_ind = MAJOR_RULES_IND*y2,
         trump_rules_ind = MAJOR_RULES_IND*y3) 



##### 7.2 Model 1: No Strata
model10 <- coxph(Surv(Days, Nominee) ~  specpol+keymgt+Bpriority+adjideo+pideo+ambassador+USMarshal+USAttorney+IG+plevel+PartTime+eop+commission+ggiver+y2+y3+MAJOR_RULES_COUNT+obama_rules_count+trump_rules_count,
                 data = appointee_data[appointee_data$vacant ==1,],
                 cluster = appointee_data$dep[appointee_data$vacant ==1],
                 robust =  T)

model10_robustse <- as.data.frame(summary(model10)[[7]]) %>% dplyr::select('robust se') %>% rename(rse = 'robust se')


##### 7.3 Model 2: Strata w/o Agency Characteristics
model11 <- coxph(Surv(Days, Nominee) ~  specpol+keymgt+Bpriority+adjideo+pideo+ambassador+USMarshal+USAttorney+IG+plevel+PartTime+ggiver+y2+y3+MAJOR_RULES_COUNT+obama_rules_count+trump_rules_count+strata(committee1)+strata(afe),
                 data = appointee_data[appointee_data$vacant ==1,],
                 cluster = appointee_data$dep[appointee_data$vacant ==1],
                 robust =  T)

model11_robustse <- as.data.frame(summary(model11)[[7]]) %>% dplyr::select('robust se') %>% rename(rse = 'robust se')


##### 7.4 Model 3: Strata w/ Agency Characteristics
model12 <- coxph(Surv(Days, Nominee) ~  specpol+keymgt+Bpriority+adjideo+pideo+workforce_skill+ambassador+USMarshal+USAttorney+IG+plevel+PartTime+eop+commission+ggiver+y3+MAJOR_RULES_COUNT+trump_rules_count+strata(committee1),
                 data = appointee_data[appointee_data$vacant ==1 & appointee_data$Concurrent == 0,],
                 cluster = appointee_data$dep[appointee_data$vacant ==1 & appointee_data$Concurrent == 0],
                 robust =  T)

model12_robustse <- as.data.frame(summary(model12)[[7]]) %>% dplyr::select('robust se') %>% rename(rse = 'robust se')


##### 7.5 Model 4: Grants
model13 <- coxph(Surv(Days, Nominee) ~  specpol+keymgt+Bpriority+adjideo+pideo+workforce_skill+ambassador+USMarshal+USAttorney+IG+plevel+PartTime+ggiver+y3+MAJOR_RULES_COUNT+trump_rules_count+strata(committee1)+strata(afe),
                 data = appointee_data[appointee_data$vacant ==1 & appointee_data$Concurrent == 0,],
                 cluster = appointee_data$dep[appointee_data$vacant ==1 & appointee_data$Concurrent == 0],
                 robust =  T)

model13_robustse <- as.data.frame(summary(model13)[[7]]) %>% dplyr::select('robust se') %>% rename(rse = 'robust se')



star_labels <- c("Policy Position (0,1)", "Management Position(0,1)", "Presidential Priority (0,1)", "Agency Ideology (0.00,3.87)", "Presidential Priority*Agency Ideology", "Agency Skills (-2.53,2.21)","EOP (0,1)", "Commission (0,1)",  "Pay Level (0-5)", "Part Time (0,1)", "Grant Giver (0,1)", "Ambassador (0,1)", "Inspector General (0,1)", "US Attorney (0,1)", "US Marshal (0,1)", "Obama (0,1)", "Trump (0,1)", "Rulemaking Count", "Obama*Rulemaking Count", "Trump*Rulemaking Count")
star_order <- paste("^",c("specpol","keymgt","Bpriority","adjideo","pideo","workforce_skill","eop","commission", "plevel","PartTime","ggiver", "ambassador", "IG","USAttorney", "USMarshal","y2","y3","MAJOR_RULES_COUNT","obama_rules_count", "trump_rules_count"), "$", sep = "")

#### 7.6 Output of Stargazer
stargazer(model10, model11,  model12, model13,
          digits = 2,
          type = "html",
          star.cutoffs = c(0.1, 0.05),
          title = "Estimated Days to First Nomination: Bush, Obama, Trump, and Biden Administrations",
          #omit = c("ambassador", "USAttorney", "USMarshal",  "Log(scale)"),
          covariate.labels = star_labels,
          order = star_order,
          se = c(model10_robustse, model11_robustse, model12_robustse, model13_robustse),
          single.row = TRUE,
          digits.extra =  0, 
          no.space = FALSE,
          intercept.bottom =  TRUE,
          add.lines = list(c("Estimator", "Cox", "Cox", "Cox", "Cox"),
                           c("Position-Type Controls", "Yes", "Yes", "Yes", "Yes"),
                           c("Department Level Stratified", "No", "Yes", "No", "No"),
                           c("Committee Stratified", "No", "Yes", "Yes", "Yes")),
          notes = "Note: *significant at the 0.05 level ; +significant at the 0.10 level in two-tailed tests.  All estimates use type HC0 standard errors clustered at the department level. Position-type controls include indicators for ambassadors, U.S. marshals, and U.S. attorneys.",
          out = "Appendix_Rulemaking_Count.doc")



##### 7.7 Model 1: No Strata
model14 <- coxph(Surv(Days, Nominee) ~  specpol+keymgt+Bpriority+adjideo+pideo+ambassador+USMarshal+USAttorney+IG+plevel+PartTime+eop+commission+ggiver+y2+y3+MAJOR_RULES_IND+obama_rules_ind+trump_rules_ind,
                 data = appointee_data[appointee_data$vacant ==1,],
                 cluster = appointee_data$dep[appointee_data$vacant ==1],
                 robust =  T)

model14_robustse <- as.data.frame(summary(model14)[[7]]) %>% dplyr::select('robust se') %>% rename(rse = 'robust se')


##### 7.8 Model 2: Strata w/o Agency Characteristics
model15 <- coxph(Surv(Days, Nominee) ~  specpol+keymgt+Bpriority+adjideo+pideo+ambassador+USMarshal+USAttorney+IG+plevel+PartTime+ggiver+y2+y3++MAJOR_RULES_IND+obama_rules_ind+trump_rules_ind+strata(committee1)+strata(afe),
                 data = appointee_data[appointee_data$vacant ==1,],
                 cluster = appointee_data$dep[appointee_data$vacant ==1],
                 robust =  T)

model15_robustse <- as.data.frame(summary(model15)[[7]]) %>% dplyr::select('robust se') %>% rename(rse = 'robust se')


##### 7.9 Model 3: Strata w/ Agency Characteristics
model16 <- coxph(Surv(Days, Nominee) ~  specpol+keymgt+Bpriority+adjideo+pideo+workforce_skill+ambassador+USMarshal+USAttorney+IG+plevel+PartTime+eop+commission+ggiver+y3+MAJOR_RULES_IND+trump_rules_ind+strata(committee1),
                 data = appointee_data[appointee_data$vacant ==1 & appointee_data$Concurrent == 0,],
                 cluster = appointee_data$dep[appointee_data$vacant ==1 & appointee_data$Concurrent == 0],
                 robust =  T)

model16_robustse <- as.data.frame(summary(model16)[[7]]) %>% dplyr::select('robust se') %>% rename(rse = 'robust se')



##### 7.10 Model 4: Grants
model17 <- coxph(Surv(Days, Nominee) ~  specpol+keymgt+Bpriority+adjideo+pideo+workforce_skill+ambassador+USMarshal+USAttorney+IG+plevel+PartTime+ggiver+y3+MAJOR_RULES_IND+trump_rules_ind+strata(committee1)+strata(afe),
                 data = appointee_data[appointee_data$vacant ==1 & appointee_data$Concurrent == 0,],
                 cluster = appointee_data$dep[appointee_data$vacant ==1 & appointee_data$Concurrent == 0],
                 robust =  T)

model17_robustse <- as.data.frame(summary(model17)[[7]]) %>% dplyr::select('robust se') %>% rename(rse = 'robust se')



star_labels <- c("Policy Position (0,1)", "Management Position(0,1)", "Presidential Priority (0,1)", "Agency Ideology (0.00,3.87)", "Presidential Priority*Agency Ideology", "Agency Skills (-2.53,2.21)","EOP (0,1)", "Commission (0,1)",  "Pay Level (0-5)", "Part Time (0,1)", "Grant Giver (0,1)", "Ambassador (0,1)", "Inspector General (0,1)", "US Attorney (0,1)", "US Marshal (0,1)", "Obama (0,1)", "Trump (0,1)", "Rulemaking Indicator", "Obama*Rulemaking Indicator", "Trump*Rulemaking Indicator")
star_order <- paste("^",c("specpol","keymgt","Bpriority","adjideo","pideo","workforce_skill","eop","commission", "plevel","PartTime","ggiver", "ambassador", "IG","USAttorney", "USMarshal","y2","y3","MAJOR_RULES_IND", "obama_rules_ind", "trump_rules_ind"), "$", sep = "")

#### 7.11 Output of Stargazer
stargazer(model14, model15,  model16, model17,
          digits = 2,
          type = "html",
          star.cutoffs = c(0.1, 0.05),
          title = "Estimated Days to First Nomination: Bush, Obama, Trump, and Biden Administrations",
          #omit = c("ambassador", "USAttorney", "USMarshal",  "Log(scale)"),
          covariate.labels = star_labels,
          se = c(model14_robustse, model15_robustse, model16_robustse, model17_robustse),
          order = star_order,
          single.row = TRUE,
          digits.extra =  0, 
          no.space = FALSE,
          intercept.bottom =  TRUE,
          add.lines = list(c("Estimator", "Cox", "Cox", "Cox", "Cox"),
                           c("Position-Type Controls", "Yes", "Yes", "Yes", "Yes"),
                           c("Department Level Stratified", "No", "Yes", "No", "No"),
                           c("Committee Stratified", "No", "Yes", "Yes", "Yes")),
          notes = "Note: *significant at the 0.05 level ; +significant at the 0.10 level in two-tailed tests.  All estimates use type HC0 standard errors clustered at the department level. Position-type controls include indicators for ambassadors, U.S. marshals, and U.S. attorneys.",
          out = "Appendix_Rulemaking_Ind.doc")


