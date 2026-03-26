#This is a sensitivity check that puts the random intercept in at the end instead of as M2
rm(list=ls())
library(rstanarm)
library(tidyverse)
library(bayesplot)

options(mc.cores = 4)

setwd("~/Documents/Projects/Non-RAND Projects/Police Shootings")
df <- readRDS(file = "Data/shooting_data_yearly.rds") 

#FIT MODELS----
#Pooled
M1 <- stan_glm(n_shootings ~ 1, 
               offset = log(population_LEMAS), 
               data = df, 
               family = poisson(link="log"), 
               seed = 4641)
print(M1)
summary(M1)

# + socio-demographics
M2 <- stan_glm(n_shootings ~ year + blacknh_pct + hispanic_pct + less18_pct + 
                   fborn_pct + ssi_pct + femalehh_pct + hhinc_median + lesshs_pct + 
                   moreba_pct + unemp_rate + fampov_rate + samehouse1yr_pct + hhsize +
                   vacancy_rate + owner_occ_pct, 
                 offset = log(population_LEMAS), 
                 data = df, QR=T,
                 family = poisson(link="log"), 
                 seed = 4641) 
print(M2)
summary(M2)

#+ demands for services (crime/calls)
M3 <- stan_glm(n_shootings ~ year + blacknh_pct + hispanic_pct + less18_pct + 
                   fborn_pct + ssi_pct + femalehh_pct + hhinc_median + lesshs_pct + 
                   moreba_pct + unemp_rate + fampov_rate + samehouse1yr_pct + hhsize +
                   vacancy_rate + owner_occ_pct  +
                   actual_murder + actual_index_violent  + gun_prev +
                   actual_assault_with_a_gun + actual_index_property + 
                   calls911_dispatched + officers_rate,
                 offset = log(population_LEMAS), 
                 data = df, QR=T,
                 family = poisson(link="log"), 
                 seed = 4641) 

print(M3)
summary(M3)

#+ LEMAS org characteristics
M4 <- stan_glm(n_shootings ~ year + blacknh_pct + hispanic_pct + less18_pct + 
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
                   cp_total +  policy_total,
                 offset = log(population_LEMAS), 
                 family = poisson(link="log"), 
                 data = df, 
                 QR = T,
                 seed = 4641)

print(M4)
summary(M4)

#+RI
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
                 family = poisson(link="log"), 
                 data = df, 
                 QR = T,
                 seed = 4641)

print(M5)
summary(M5)

#ANALYSES----
library(haven)
library(sf)
library(ggpubr)
library(gt)

#0. Check model diagnostics----
#r-hat, neff, mixing of low autocorrelation chains
#launch_shinystan(M1, ppd = FALSE)
#launch_shinystan(M2, ppd = FALSE)
#launch_shinystan(M3, ppd = FALSE)
#launch_shinystan(M4, ppd = FALSE)
#launch_shinystan(M5, ppd = FALSE)

#1. Results Section 1----
##1.b) PPC Table----
tab_output <- NULL
for(i in 1:5){
  mod <- paste0("M",i)
  yrep1 <- posterior_predict(get(mod)) 
  
  ### Y max
  t <- apply(yrep1, 1, max)
  obs <- max(df$n_shootings)
  range_x <- range(c(t,obs))
  # hist(t, xlim = range_x); abline(v=obs,col="blue",lwd=4)
  y_max <- mean(t >= obs)
  
  ### % 0s
  t <- apply(yrep1, 1, function(f){sum(f==0)})
  obs <- sum(df$n_shootings==0)
  range_x <- range(c(t,obs))
  #hist(t, xlim = range_x, breaks=50); abline(v=obs,col="blue",lwd=4)
  prop_zero <- mean(t >= obs)
  
  #### Variance
  t <- apply(yrep1, 1, var)
  obs <- var(df$n_shootings)
  range_x <- range(c(t,obs))
  #hist(t, xlim=range_x, breaks=50); abline(v=obs,col="blue",lwd=4)
  var_mod <- mean(t >= obs)
  
  temp <- data.frame(y_max, prop_zero, var_mod, Model = i)
  tab_output <- rbind(tab_output, temp)
}

l <- round(tab_output,2) %>% gt()
gtsave(l, "Output/Model Checks/Reverse Order/PPCs.html")

##1.c) LOOIC----
loo1 <- loo(M1)
loo2 <- loo(M2)
loo3 <- loo(M3)
loo4 <- loo(M4)
loo5 <- loo(M5)
loo_compare(loo1, loo2)
loo_compare(loo2, loo3)
loo_compare(loo3, loo4)
loo_compare(loo4, loo5)
loo1; loo2; loo3; loo4; loo5


plot.dat <- cbind(loo1$estimates[,1], loo2$estimates[,1], loo3$estimates[,1], loo4$estimates[,1], loo5$estimates[,1])
colnames(plot.dat) <- c("M1", "M2", "M3", "M4", "M5")    

plot.dat <- plot.dat %>% as.data.frame() %>% gt()
gtsave(plot.dat, "Output/Model Checks/Reverse Order/LOOIC.html")
