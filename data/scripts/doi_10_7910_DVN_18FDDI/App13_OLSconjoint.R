################################################################################
#### Title: Progressive Ideology and Support for Punitive Crime Policy
#### Author: Isabel Laterzo
#### Year: 2023
#### Journal: Comparative Political Studies
#### Portion of Analysis: Appendix 13 - OLS Models
################################################################################

################################# SET UP #######################################

rm(list=ls()) #clear global environ


# uncomment below and set working directory using setwd() to directory which contains
# all data files
#setwd()


#packages
library(tidyverse)
library(ggplot2)
library(stargazer)


#read in data and filter
data <- readRDS("laterzo_cps_2023c.rds")
levels(data$crime)[levels(data$crime)=="Tough on Crime"] <- "Punitive"

############################### CONJOINT SET UP ################################

#safety variable
#4 insecure, 1 secure --> 1 = unsafe, 0 = safe
data$safety_di <- factor(ifelse(data$safety_neighb == 4, "1", 
                                ifelse(data$safety_neighb == 3, "1", 
                                       ifelse(data$safety_neighb == 2, "0", 
                                              ifelse(data$safety_neighb == 1, "0", NA)))))


#fix country names
data$country <- as.factor(ifelse(data$country == "brazil", "Brazil",
                                 "Argentina"))


##create data frames for prog vs. conserv on social and econ issues based on
## median value
prog_ideo <- data %>% filter(ideo >= 0) #same as not including zero
cons_ideo <- data %>% filter(ideo < 0) #same as not including zero


######################## Base OLS model ########################################

base_mod <- lm(chosen ~ crime + sex + abort + tax + lgbtq + environ + country + id,
               data = data)

base_prog <- lm(chosen ~ crime + sex + abort + tax + lgbtq + environ + country + id,
                  data = prog_ideo)

base_cons <- lm(chosen ~ crime + sex + abort + tax + lgbtq + environ + country + id,
                data = cons_ideo)

# Appendix Table A13.1: OLS Results for Pooled Mode
stargazer(base_mod, base_prog, base_cons,
          column.labels = c("Pooled", "Progressives", "Conservatives"),
          omit = "id",
          covariate.labels = c("Crime: Punitive",
                               "Sex: Male",
                               "Abortion: Support",
                               "Taxes: Reduce",
                               "Same-Sex Marriage: Support",
                               "Environ: Low Concern",
                               "Country: Brazil",
                               "Constant"),
          no.space = TRUE,
          font.size = "tiny",
          dep.var.labels.include = FALSE)

######################## Victimization Model ###################################
  
#victims vs. non-victims subset
vic_pool <- data %>% filter(vic == 1)
nonvic_pool <- data %>% filter(vic == 0)
vic_prog <- prog_ideo %>% filter(vic == 1)
vic_cons <- cons_ideo %>% filter(vic == 1)
nonvic_prog <- prog_ideo %>% filter(vic == 0)
nonvic_cons <- cons_ideo %>% filter(vic == 0)

#pooled models
pool_vic <- lm(chosen ~ crime + sex + abort + tax + lgbtq + environ + country + id,
               data = vic_pool)

pool_nonvic <- lm(chosen ~ crime + sex + abort + tax + lgbtq + environ + country + id,
               data = nonvic_pool)

#progressive models
prog_vic <- lm(chosen ~ crime + sex + abort + tax + lgbtq + environ + country + id,
               data = vic_prog)
  
prog_nonvic <- lm(chosen ~ crime + sex + abort + tax + lgbtq + environ + country + id,
                 data = nonvic_prog)

#econ models
cons_vic <- lm(chosen ~ crime + sex + abort + tax + lgbtq + environ + country + id,
                 data = vic_cons)

cons_nonvic <- lm(chosen ~ crime + sex + abort + tax + lgbtq + environ + country + id,
                    data = nonvic_cons)

# Appendix Table A13.2: OLS Results for Victims
stargazer(pool_vic, prog_vic, cons_vic,
          column.labels = c("Pooled", "Progressives", "Conservatives"),
          title = "Victims",
          omit = "id",
          covariate.labels = c("Crime: Punitive",
                               "Sex: Male",
                               "Abortion: Support",
                               "Taxes: Reduce",
                               "Same-Sex Marriage: Support",
                               "Environ: Low Concern",
                               "Country: Brazil",
                               "Constant"),
          no.space = TRUE,
          font.size = "tiny",
          dep.var.labels.include = FALSE) 

# Appendix Table A13.3: OLS Results for Non-Victims
stargazer(pool_nonvic, prog_nonvic, cons_nonvic,
          column.labels = c("Pooled", "Progressives", "Conservatives"),
          title = "Non-Victims",
          omit = "id",
          covariate.labels = c("Crime: Punitive",
                               "Sex: Male",
                               "Abortion: Support",
                               "Taxes: Reduce",
                               "Same-Sex Marriage: Support",
                               "Environ: Low Concern",
                               "Country: Brazil",
                               "Constant"),
          no.space = TRUE,
          font.size = "tiny",
          dep.var.labels.include = FALSE)  

  

######################## Safety Models ###################################

#safe vs. unsafe subset
safe_pool <- data %>% filter(safety_di == 0)
unsafe_pool <- data %>% filter(safety_di == 1)
safe_prog <- prog_ideo %>% filter(safety_di == 0)
safe_cons <- cons_ideo %>% filter(safety_di == 0)
unsafe_prog <- prog_ideo %>% filter(safety_di == 1)
unsafe_cons <- cons_ideo %>% filter(safety_di == 1)


#pooled models
pool_safe <- lm(chosen ~ crime + sex + abort + tax + lgbtq + environ + country + id,
               data = safe_pool)

pool_unsafe <- lm(chosen ~ crime + sex + abort + tax + lgbtq + environ + country + id,
                  data = unsafe_pool)

#social models
prog_safe <- lm(chosen ~ crime + sex + abort + tax + lgbtq + environ + country + id,
                 data = safe_prog)

prog_unsafe <- lm(chosen ~ crime + sex + abort + tax + lgbtq + environ + country + id,
                    data = unsafe_prog)

#econ models
cons_safe <- lm(chosen ~ crime + sex + abort + tax + lgbtq + environ + country + id,
               data = safe_cons)

cons_unsafe <- lm(chosen ~ crime + sex + abort + tax + lgbtq + environ + country + id,
                  data = unsafe_cons)



# Appendix Table A13.4: OLS Results for Residents of “Secure” Neighborhoods
stargazer(pool_safe, prog_safe, cons_safe,
          column.labels = c("Pooled", "Progressives", "Conservatives"),
          title = "Residents of ``Secure`` Neighborhoods",
          omit = "id",
          covariate.labels = c("Crime: Punitive",
                               "Sex: Male",
                               "Abortion: Support",
                               "Taxes: Reduce",
                               "Same-Sex Marriage: Support",
                               "Environ: Low Concern",
                               "Country: Brazil",
                               "Constant"),
          no.space = TRUE,
          font.size = "tiny",
          dep.var.labels.include = FALSE) 

# Table A13.5: OLS Results for Residents of “Insecure” Neighborhoods
stargazer(pool_unsafe, prog_unsafe, cons_unsafe,
          column.labels = c("Pooled", "Progressives", "Conservatives"),
          title = "Residents of ``Insecure`` Neighborhoods",
          omit = "id",
          covariate.labels = c("Crime: Punitive",
                               "Sex: Male",
                               "Abortion: Support",
                               "Taxes: Reduce",
                               "Same-Sex Marriage: Support",
                               "Environ: Low Concern",
                               "Country: Brazil",
                               "Constant"),
          no.space = TRUE,
          font.size = "tiny",
          dep.var.labels.include = FALSE)  



######################### CRIME DRIVEN BY GANGS ###############################

#crime driven by gangs vs. not
gang_pool <- data %>% filter(crime_gang == 1)
nongang_pool <- data %>% filter(crime_gang == 0)
gang_prog <- prog_ideo %>% filter(crime_gang == 1)
gang_cons <- cons_ideo %>% filter(crime_gang == 1)
nongang_prog <- prog_ideo %>% filter(crime_gang == 0)
nongang_cons <- cons_ideo %>% filter(crime_gang == 0)


#pooled models
pool_gang <- lm(chosen ~ crime + sex + abort + tax + lgbtq + environ + country + id,
                data = gang_pool)

pool_nongang <- lm(chosen ~ crime + sex + abort + tax + lgbtq + environ + country + id,
                  data = nongang_pool)

#progressive models
prog_gang <- lm(chosen ~ crime + sex + abort + tax + lgbtq + environ + country + id,
                  data = gang_prog)

prog_nongang <- lm(chosen ~ crime + sex + abort + tax + lgbtq + environ + country + id,
                    data = nongang_prog)

#conservative models
cons_gang <- lm(chosen ~ crime + sex + abort + tax + lgbtq + environ + country + id,
                data = gang_cons)

cons_nongang <- lm(chosen ~ crime + sex + abort + tax + lgbtq + environ + country + id,
                  data = nongang_cons)


# Appendix Table A13.8: OLS Results for Gang-Driven Crime
stargazer(pool_gang, prog_gang, cons_gang,
          column.labels = c("Pooled", "Progressives", "Conservatives"),
          title = "Crime is Driven by Gangs",
          omit = "id",
          covariate.labels = c("Crime: Punitive",
                               "Sex: Male",
                               "Abortion: Support",
                               "Taxes: Reduce",
                               "Same-Sex Marriage: Support",
                               "Environ: Low Concern",
                               "Country: Brazil",
                               "Constant"),
          no.space = TRUE,
          font.size = "tiny",
          dep.var.labels.include = FALSE) 

# Appendix Table A13.9: OLS Results for Non-Gang-Driven Crime
stargazer(pool_nongang, prog_nongang, cons_nongang,
          column.labels = c("Pooled", "Progressives", "Conservatives"),
          title = "Crime is Not Driven by Gangs",
          omit = "id",
          covariate.labels = c("Crime: Punitive",
                               "Sex: Male",
                               "Abortion: Support",
                               "Taxes: Reduce",
                               "Same-Sex Marriage: Support",
                               "Environ: Low Concern",
                               "Country: Brazil",
                               "Constant"),
          no.space = TRUE,
          font.size = "tiny",
          dep.var.labels.include = FALSE)  




######################### MODELS: PERCEPTIONS OF SOCIAL ASSISTANCE #############

#social assistance is effective vs. not
effect_pool <- data %>% filter(assist_effect == 1)
noteffect_pool <- data %>% filter(assist_effect == 0)
effect_prog <- prog_ideo %>% filter(assist_effect == 1)
effect_cons <- cons_ideo %>% filter(assist_effect == 1)
noteffect_prog <- prog_ideo %>% filter(assist_effect == 0)
noteffect_cons <- cons_ideo %>% filter(assist_effect == 0)


#pooled models
pool_effect <- lm(chosen ~ crime + sex + abort + tax + lgbtq + environ + country + id,
                data = effect_pool)

pool_noteffect <- lm(chosen ~ crime + sex + abort + tax + lgbtq + environ + country + id,
                   data = noteffect_pool)

#progressive models
prog_effect <- lm(chosen ~ crime + sex + abort + tax + lgbtq + environ + country + id,
                  data = effect_prog)

prog_noteffect <- lm(chosen ~ crime + sex + abort + tax + lgbtq + environ + country + id,
                     data = noteffect_prog)

#conservative models
cons_effect <- lm(chosen ~ crime + sex + abort + tax + lgbtq + environ + country + id,
                data = effect_cons)

cons_noteffect <- lm(chosen ~ crime + sex + abort + tax + lgbtq + environ + country + id,
                   data = noteffect_cons)

# Appendix Table A13.6: OLS Results for Respondents who Believe Social Assistance is Effective
stargazer(pool_effect, prog_effect, cons_effect,
          column.labels = c("Pooled", "Progressives", "Conservatives"),
          title = "Social Assistance is Effective",
          omit = "id",
          covariate.labels = c("Crime: Punitive",
                               "Sex: Male",
                               "Abortion: Support",
                               "Taxes: Reduce",
                               "Same-Sex Marriage: Support",
                               "Environ: Low Concern",
                               "Country: Brazil",
                               "Constant"),
          no.space = TRUE,
          font.size = "tiny",
          dep.var.labels.include = FALSE) 

# Appendix Table A13.7: OLS Results for Respondents who Believe Social Assistance is Not Effective
stargazer(pool_noteffect, prog_noteffect, cons_noteffect,
          column.labels = c("Pooled", "Progressives", "Conservatives"),
          title = "Social Assistance is Not Effective",
          omit = "id",
          covariate.labels = c("Crime: Punitive",
                               "Sex: Male",
                               "Abortion: Support",
                               "Taxes: Reduce",
                               "Same-Sex Marriage: Support",
                               "Environ: Low Concern",
                               "Country: Brazil",
                               "Constant"),
          no.space = TRUE,
          font.size = "tiny",
          dep.var.labels.include = FALSE)  



