#This file fits the main models, reported in the main text of the paper 
rm(list=ls())
library(rstanarm)
library(tidyverse)
library(bayesplot)

options(mc.cores = 4)

setwd("~/Documents/Projects/Non-RAND Projects/Police Shootings/Data/")
df <- readRDS(file = "shooting_data_yearly.rds")

#Pooled
M1 <- stan_glm(n_shootings ~ 1, 
                 offset = log(population_LEMAS), 
                 data = df, 
                 family = poisson(link="log"), 
                 seed = 4641)
print(M1)
summary(M1)
#launch_shinystan(M1, ppd = FALSE)
  
#Random intercept
M2 <- stan_glmer(n_shootings ~ 1 + (1 | oricode), 
                   offset = log(population_LEMAS), 
                   data = df, 
                   family = poisson(link="log"), 
                   seed = 4641)
print(M2)
summary(M2)
#launch_shinystan(M2, ppd = FALSE)
  
#RI + socio-demographics
M3 <- stan_glmer(n_shootings ~ year + blacknh_pct + hispanic_pct + less18_pct + 
                     fborn_pct + ssi_pct + femalehh_pct + hhinc_median + lesshs_pct + 
                     moreba_pct + unemp_rate + fampov_rate + samehouse1yr_pct + hhsize +
                     vacancy_rate + owner_occ_pct + (1 | oricode), 
                   offset = log(population_LEMAS), 
                   data = df, QR=T,
                   family = poisson(link="log"), 
                   seed = 4641) 
print(M3)
summary(M3)
#launch_shinystan(M3, ppd = FALSE)
  
#RI + demands for services (crime/calls)
M4 <- stan_glmer(n_shootings ~ year + blacknh_pct + hispanic_pct + less18_pct + 
                     fborn_pct + ssi_pct + femalehh_pct + hhinc_median + lesshs_pct + 
                     moreba_pct + unemp_rate + fampov_rate + samehouse1yr_pct + hhsize +
                     vacancy_rate + owner_occ_pct  +
                     actual_murder + actual_index_violent  + gun_prev +
                     actual_assault_with_a_gun + actual_index_property + 
                     calls911_dispatched + officers_rate + (1 | oricode),
                   offset = log(population_LEMAS), 
                   data = df, QR=T,
                   family = poisson(link="log"), 
                   seed = 4641) 
  
print(M4)
summary(M4)
#launch_shinystan(M4, ppd = FALSE)
  
#RI+ LEMAS org characteristics
M5 <- stan_glmer(n_shootings ~ year + blacknh_pct + hispanic_pct + less18_pct + 
                     fborn_pct + ssi_pct + femalehh_pct + hhinc_median + lesshs_pct + 
                     moreba_pct + unemp_rate + fampov_rate + samehouse1yr_pct + hhsize +
                     vacancy_rate + owner_occ_pct  +
                     actual_murder + actual_index_violent  +  gun_prev +
                     actual_assault_with_a_gun + actual_index_property + 
                     calls911_dispatched + officers_rate +
                     investigate_UoFinjury + investigate_UoFdeath + investigate_otherdeath +  investigate_firegun + ccrb  +  
                     computer_perfsystem + equip_bluntprojectile +  equip_taser + union_authorized + policy_vehpursuit + 
                     document_displaygun  +  document_dischargegun  + officers_male_pct +  white_overrep + educ_requirement + 
                     cameras_numBWC + trainhrs_academy  +  trainhrs_fieldrecruits   + trainhrs_inservofficers  +  check_total +             
                     cp_total +  policy_total + (1|oricode),
                   offset = log(population_LEMAS), 
                   family = poisson(link = "log"),
                   data = df, 
                   QR = T,
                   seed = 4641)
  
print(M5)
summary(M5)
#launch_shinystan(M5, ppd = FALSE)
  
save(df, M1, M2, M3, M4, M5, file = "Bayesian_models_main.RData")


