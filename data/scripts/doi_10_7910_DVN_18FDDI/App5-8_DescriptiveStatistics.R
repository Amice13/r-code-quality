################################################################################
#### Title: Progressive Ideology and Support for Punitive Crime Policy
#### Author: Isabel Laterzo
#### Year: 2023
#### Journal: Comparative Political Studies
#### Portion of Analysis: Appendices 5-8
################################################################################


############################### SET UP #########################################

rm(list=ls()) #clear global environ


# uncomment below and set working directory using setwd() to directory which contains
# all data files
#setwd()


#packages
library(tidyverse)

#read in data and filter
arg <- readRDS("arg_clean.rds") %>% dplyr::select(id, age, edu,  ideo_01b,
                                                  ideo_01c, ideo_01d,
                                                  ideo_05b, ideo_05c, ideo_05d,
                                                  ideo_05e, vic, crime_gang,
                                                  safety_neighb, assist_effect,
                                                  social_pol, toilet, female, state,
                                                  NSE_Score)

arg$country <- "argentina" #country variable
arg$edu <- as.numeric(arg$edu) #make education numeric

#houses with 2 or more toilets
arg$toilet2 <- ifelse(arg$toilet >= 2, 1, ifelse(arg$toilet < 2, 0, arg$toilet))


brazil <- readRDS("brazil_clean.rds")  %>% dplyr::select(id, age, edu,  ideo_01b,
                                                 ideo_01c, ideo_01d,
                                                  ideo_05b, ideo_05c, ideo_05d,
                                                 ideo_05e, vic, crime_gang,
                                                 safety_neighb, assist_effect,
                                                 social_pol, toilet, female, state,
                                                 NSE_Score)
brazil$country <- "brazil" #country variable
brazil$edu <- as.numeric(brazil$edu) #make education numeric

#houses with 2 or more toilets
brazil$toilet2 <- ifelse(brazil$toilet >= 2, 1, ifelse(brazil$toilet < 2, 0, brazil$toilet))

#combine
braz_noNSE <- brazil %>% select(!NSE_Score)
arg_noNSE <- arg %>% select(!NSE_Score)
data <- rbind(arg_noNSE, braz_noNSE) %>% unique()




#################### Appendix 5 -- Comparing to Quotas  ########################

# 1) Table A5.1: Argentina 
arg_complete <- unique(arg)

#age categories
arg_complete$age_cat <- ifelse(arg_complete$age <= 25, "18-25",
                                ifelse(arg_complete$age > 25 &
                                         arg_complete$age <= 40, "26-40",
                                       ifelse(arg_complete$age > 26 &
                                                arg_complete$age <= 60, "41-60",
                                              "61 and over")))
age <- arg_complete %>%
  group_by(age_cat) %>%
  summarise(cnt = n()) %>%
  mutate(freq = round(cnt / sum(cnt), 3))
age

#gender (1 = female)
gender <- arg_complete %>%
  group_by(female) %>%
  summarise(cnt = n()) %>%
  mutate(freq = round(cnt / sum(cnt), 3))
gender

# NSE_recoded
arg_complete$nse <- as.numeric(as.character(arg_complete$NSE_Score))
arg_complete$nse_group[arg_complete$nse > 0 & arg_complete$nse <= 3] <- "1. Low"
arg_complete$nse_group[arg_complete$nse == 4 | arg_complete$nse == 5] <- "2. Med"
arg_complete$nse_group[arg_complete$nse == 6 | arg_complete$nse == 7] <- "3. High"

socioecon <- arg_complete %>%
  group_by(nse_group) %>%
  summarise(cnt = n()) %>%
  mutate(freq = round(cnt / sum(cnt), 3))
socioecon


#region
arg_complete$region <- ifelse(arg_complete$state == "1" |
                      arg_complete$state == "25", 1,
                    ifelse(arg_complete$state == "2" |
                             arg_complete$state == "11" |
                             arg_complete$state == "21" |
                             arg_complete$state == "6" |
                             arg_complete$state == "8", 2,
                           ifelse(arg_complete$state == "13" |
                                    arg_complete$state == "18" |
                                    arg_complete$state == "19", 3,
                                  ifelse(arg_complete$state == "7" |
                                           arg_complete$state == "9" |
                                           arg_complete$state == "4" |
                                           arg_complete$state == "14", 4,
                                         ifelse(arg_complete$state == "3" |
                                                  arg_complete$state == "24" |
                                                  arg_complete$state == "10" |
                                                  arg_complete$state == "12" |
                                                  arg_complete$state == "17" |
                                                  arg_complete$state == "22", 5,
                                                ifelse(arg_complete$state == "15" |
                                                         arg_complete$state == "20" |
                                                         arg_complete$state == "16" |
                                                         arg_complete$state == "23" |
                                                         arg_complete$state == "5", 6, NA))))))

region <- arg_complete %>%
  group_by(region) %>%
  summarise(cnt = n()) %>%
  mutate(freq = round(cnt / sum(cnt), 3))
region 




# 2) Table A5.2: Brazil
braz_complete <- unique(brazil)

#age categories
braz_complete$age_cat <- ifelse(braz_complete$age <= 25, "18-25",
                                ifelse(braz_complete$age > 25 &
                                         braz_complete$age <= 40, "26-40",
                                       ifelse(braz_complete$age > 26 &
                                                braz_complete$age <= 60, "41-60",
                                              "61 and over")))
age <- braz_complete %>%
    group_by(age_cat) %>%
    summarise(cnt = n()) %>%
    mutate(freq = round(cnt / sum(cnt), 3))
age

#gender
gender <- braz_complete %>%
  group_by(female) %>%
  summarise(cnt = n()) %>%
  mutate(freq = round(cnt / sum(cnt), 3))
gender #where female = 1

#region
braz_complete$region <- ifelse(braz_complete$state == "7" |
                      braz_complete$state == "9" |
                      braz_complete$state == "11" |
                      braz_complete$state == "12", 1, 
                    ifelse(braz_complete$state == "2" |
                             braz_complete$state == "5" |
                             braz_complete$state == "6" |
                             braz_complete$state == "10" |
                             braz_complete$state == "15" |
                             braz_complete$state == "17" |
                             braz_complete$state == "18" |
                             braz_complete$state == "20" |
                             braz_complete$state == "26", 2,
                           ifelse(braz_complete$state == "1" |
                                    braz_complete$state == "3" |
                                    braz_complete$state == "4" |
                                    braz_complete$state == "14" |
                                    braz_complete$state == "22" |
                                    braz_complete$state == "23" |
                                    braz_complete$state == "37", 3,
                                  ifelse(braz_complete$state == "8" |
                                           braz_complete$state == "13" |
                                           braz_complete$state == "19" |
                                           braz_complete$state == "25", 4,
                                         ifelse(braz_complete$state == "16" |
                                                  braz_complete$state == "21" |
                                                  braz_complete$state == "24", 5, NA)))))

region <- braz_complete %>%
  group_by(region) %>%
  summarise(cnt = n()) %>%
  mutate(freq = round(cnt / sum(cnt), 3))
region

# NSE_recoded
braz_complete$NSE_Score <- as.numeric(as.character(braz_complete$NSE_Score))
braz_complete$nse_group[braz_complete$NSE_Score > 0 & braz_complete$NSE_Score <= 22] <- "1. Low"
braz_complete$nse_group[braz_complete$NSE_Score >= 23 & braz_complete$NSE_Score <= 37] <- "2. Med"
braz_complete$nse_group[braz_complete$NSE_Score >= 38] <- "3. High"

socioecon <- braz_complete %>%
  group_by(nse_group) %>%
  summarise(cnt = n()) %>%
  mutate(freq = round(cnt / sum(cnt), 3))
socioecon


#################### Appendix 6: Basic Descriptives ###############################

# Table A6.1

# 1) Argentina

mean(arg_complete$age, na.rm = T) #mean age 40.33
mean(arg_complete$edu, na.rm = T) #mean edu 5.96
table(arg_complete$female)/nrow(arg_complete) #female 51.20
table(arg_complete$toilet2)/nrow(arg_complete) #toilet 22.49
table(arg_complete$vic)/nrow(arg_complete) #victim 14.47
mean(as.numeric(arg_complete$safety_neighb), na.rm = T) #mean neighb safety 2.42
table(arg_complete$assist_effect)/nrow(arg_complete) #social pol 18.97
mean(as.numeric(arg_complete$crime_gang), na.rm = T) #mean neighb safety 1.82

# 2) Brazil

mean(braz_complete$edu, na.rm = T) #mean edu 7.26
table(braz_complete$female)/nrow(braz_complete) #female 53.18
table(braz_complete$toilet2)/nrow(braz_complete) #toilet 25.85
table(braz_complete$vic)/nrow(braz_complete) #victim 19.50
mean(as.numeric(braz_complete$safety_neighb), na.rm = T) #mean neighb safety 2.48
table(braz_complete$assist_effect)/nrow(braz_complete) #social pol 40.25
mean(as.numeric(braz_complete$crime_gang), na.rm = T) #mean neighb safety 1.83





#############  Appendix 7: Subsample comparison for non-response bias ##########


## Table A7.1: Brazil

# 1) ideology non rep removed
braz_nrm <- brazil %>% drop_na(ideo_01b, ideo_01c, ideo_01d, ideo_05b, ideo_05c,
                            ideo_05d, ideo_05e) %>%
  unique()
nrow(braz_nrm) #1235 sample size

mean(braz_nrm$age, na.rm = T) # age 35.71
mean(braz_nrm$edu, na.rm = T) #edu 7.27
table(braz_nrm$female)/nrow(braz_nrm) #female 53%
table(braz_nrm$toilet2)/nrow(braz_nrm) #toilets 26%

# 2) vic non rep removed
braz_vic <- brazil %>% drop_na(vic) %>% unique()
nrow(braz_vic) #1327 sample size

mean(braz_vic$age, na.rm = T) # age 35.64
mean(braz_vic$edu, na.rm = T) # edu 7.27
table(braz_vic$female)/nrow(braz_vic) #female 53%
table(braz_vic$toilet2)/nrow(braz_vic) #toilet 26%


# 3) community security non response removed
braz_sec <- brazil %>% drop_na(safety_neighb) %>% unique()
nrow(braz_sec) #1293

mean(braz_sec$age, na.rm = T) #age 35.63
mean(braz_sec$edu, na.rm = T) # edu 7.26
table(braz_sec$female)/nrow(braz_sec) #female 53%
table(braz_sec$toilet2)/nrow(braz_sec) #toilet 26%

# 4) crime gang non response removed
braz_gang <- brazil %>% drop_na(crime_gang) %>% unique()
nrow(braz_gang) #1227

mean(braz_gang$age, na.rm = T) #age 35.64
mean(braz_gang$edu, na.rm = T) # edu 7.26
table(braz_gang$female)/nrow(braz_gang) #female 51%
table(braz_gang$toilet2)/nrow(braz_gang) #toilet 27%

# 5) social assistance effectiveness non response removed
braz_assist <- brazil %>% drop_na(assist_effect) %>% unique()
nrow(braz_assist) #1197

mean(braz_assist$age, na.rm = T) #age 35.72
mean(braz_assist$edu, na.rm = T) # edu 7.20
table(braz_assist$female)/nrow(braz_assist) #female 53%
table(braz_assist$toilet2)/nrow(braz_assist) #toilet 26%

## Table A7.2: Argentina

# 1) ideology non response removed
arg_nrm <- arg %>% drop_na(ideo_01b, ideo_01c, ideo_01d, ideo_05b, ideo_05c,
                           ideo_05d, ideo_05e) %>%
  unique()
nrow(arg_nrm) #1164

mean(arg_nrm$age, na.rm = T) #mean age = 40.83
mean(arg_nrm$edu, na.rm = T) #mean edu = 6.02
table(arg_nrm$female)/nrow(arg_nrm) # % female = 48%
table(arg_nrm$toilet2)/nrow(arg_nrm) # % 2 or more toilets = 34%

# 2) vic non response removed
arg_vic <- arg %>% drop_na(vic) %>% unique()
nrow(arg_vic) #1324

mean(arg_vic$age, na.rm = T) #mean age = 40.40
mean(arg_vic$edu, na.rm = T) #mean edu = 5.96
table(arg_vic$female)/nrow(arg_vic) # % female = 51%
table(arg_vic$toilet2)/nrow(arg_vic) # % 2 or more toilets = 23%

# 3) community security non response removed
arg_sec <- arg %>% drop_na(safety_neighb) %>% unique()
nrow(arg_sec) #1300

mean(arg_sec$age, na.rm = T) #age 40.23
mean(arg_sec$edu, na.rm = T) # edu 5.95
table(arg_sec$female)/nrow(arg_sec) #female 51%
table(arg_sec$toilet2)/nrow(arg_sec) #toilet 22%

# 4) crime gang non response removed
arg_gang <- arg %>% drop_na(crime_gang) %>% unique()
nrow(arg_gang) #1275

mean(arg_gang$age, na.rm = T) #age 40.12
mean(arg_gang$edu, na.rm = T) # edu 5.99
table(arg_gang$female)/nrow(arg_gang) #female 51%
table(arg_gang$toilet2)/nrow(arg_gang) #toilet 23%

# 5) social assistance effectiveness non response removed
arg_assist <- arg %>% drop_na(assist_effect) %>% unique()
nrow(arg_assist) #1219

mean(arg_assist$age, na.rm = T) #age 40.60
mean(arg_assist$edu, na.rm = T) # edu 6.00
table(arg_assist$female)/nrow(arg_assist) #female 51%
table(arg_assist$toilet2)/nrow(arg_assist) #toilet 23%




############ Appendix 8: Social policy and perceptions of social policy ######## 

# where assist_effect == 0 means "not effective"
# and social_pol == 0 means "not beneficiary of social assistance"

# rename variables for clarity
data$assist_effect_fac <- ifelse(data$assist_effect == 1, "Effective",
                                 ifelse(data$assist_effect == 0, "Ineffective",
                                        NA))

data$social_pol_fac <- ifelse(data$social_pol == 1, "Beneficiary",
                                 ifelse(data$social_pol == 0, "Not Beneficiary",
                                        NA))

# Table A8.1

table(data$assist_effect_fac, data$social_pol_fac)


