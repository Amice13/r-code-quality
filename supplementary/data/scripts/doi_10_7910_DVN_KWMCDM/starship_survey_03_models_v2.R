####################################################
# Steven R. Gehrke
# Northern Arizona University
# 21-0405: Starship Robots
# Task: PAR-D Survey Results Modeling (New Outcome)
# Created: 06.27.2022
# Updated: 06.27.2022
####################################################

### NOTES
# https://stats.oarc.ucla.edu/r/dae/ordinal-logistic-regression/

####################
### SET-UP

# Load libraries
# install.packages(c("tidyverse","MASS"))
packages <- c("tidyverse","MASS")
lapply(packages, require, character.only=TRUE); rm(packages)

# Set directory
dat_dir <- "H:/_projects/21-0405_SidewalkDeliveryRobots/"

# Import cleaned data set
dat <- read_csv(paste(dat_dir, "dat_pard_clean.csv", sep="_dat/_survey/_tabular/"))

####################
### DEPENDENT VARIABLE REVIEW + CREATION

# Remove records without DV response
dat <- dat[!is.na(dat$q17_ped_3s)&!is.na(dat$q18_ped_1s)&!is.na(dat$q19_bike_3s)&!is.na(dat$q20_bike_1s),]

# Outcome: Pedestrian Comfort Categories -- FOUR CATEGORIES
dat$ped_3s_4 <- NA
dat$ped_3s_4 <- ifelse(dat$q17_ped_3s=="Very uncomfortable", 0,
                       ifelse(dat$q17_ped_3s=="Uncomfortable", 0,
                              ifelse(dat$q17_ped_3s=="Neutral", 1,
                                     ifelse(dat$q17_ped_3s=="Comfortable", 2,
                                            ifelse(dat$q17_ped_3s=="Very comfortable", 3, NA)))))
table(dat$ped_3s_4); round(prop.table(table(dat$ped_3s_4)),2)
summary(dat$ped_3s_4); sd(dat$ped_3s_4)
dat$ped_1s_4 <- NA
dat$ped_1s_4 <- ifelse(dat$q18_ped_1s=="Very uncomfortable", 0,
                       ifelse(dat$q18_ped_1s=="Uncomfortable", 0,
                              ifelse(dat$q18_ped_1s=="Neutral", 1,
                                     ifelse(dat$q18_ped_1s=="Comfortable", 2,
                                            ifelse(dat$q18_ped_1s=="Very comfortable", 3, NA)))))
table(dat$ped_1s_4); round(prop.table(table(dat$ped_1s_4)),2)
summary(dat$ped_1s_4); sd(dat$ped_1s_4)

# Outcome: Bicyclist Comfort Categories -- THREE CATEGORIES
dat$bike_3s_3 <- NA
dat$bike_3s_3 <- ifelse(dat$q19_bike_3s=="Very uncomfortable", 0,
                        ifelse(dat$q19_bike_3s=="Uncomfortable", 0,
                               ifelse(dat$q19_bike_3s=="Neutral", 1,
                                      ifelse(dat$q19_bike_3s=="Comfortable", 2,
                                             ifelse(dat$q19_bike_3s=="Very comfortable", 2, NA)))))
table(dat$bike_3s_3); round(prop.table(table(dat$bike_3s_3)),2)
summary(dat$bike_3s_3); sd(dat$bike_3s_3)
dat$bike_1s_3 <- NA
dat$bike_1s_3 <- ifelse(dat$q20_bike_1s=="Very uncomfortable", 0,
                        ifelse(dat$q20_bike_1s=="Uncomfortable", 0,
                               ifelse(dat$q20_bike_1s=="Neutral", 1,
                                      ifelse(dat$q20_bike_1s=="Comfortable", 2,
                                             ifelse(dat$q20_bike_1s=="Very comfortable", 2, NA)))))
table(dat$bike_1s_3); round(prop.table(table(dat$bike_1s_3)),2)
summary(dat$bike_1s_3); sd(dat$bike_1s_3)

####################
### INDEPENDENT VARIABLE REVIEW + CREATION

## Respondent Characteristics

# On-campus residence
table(dat$q01_oncampus); round(prop.table(table(dat$q01_oncampus)),2)
summary(dat$q01_oncampus); sd(dat$q01_oncampus)

# Gender: Male
dat$q04_male[is.na(dat$q04_male)] <- 0
table(dat$q04_male); round(prop.table(table(dat$q04_male)),2)
summary(dat$q04_male); sd(dat$q04_male)

# Race/Ethnicity: White or Latino
dat$q05_rac_whi[is.na(dat$q05_rac_whi)] <- 0
table(dat$q05_rac_whi); round(prop.table(table(dat$q05_rac_whi)),2)
summary(dat$q05_rac_whi); sd(dat$q05_rac_whi)
dat$q05_rac_lat[is.na(dat$q05_rac_lat)] <- 0
table(dat$q05_rac_lat); round(prop.table(table(dat$q05_rac_lat)),2)
summary(dat$q05_rac_lat); sd(dat$q05_rac_lat)

# Work Status: Full-time Student
dat$q07_stu_full[is.na(dat$q07_stu_full)] <- 0
table(dat$q07_stu_full); round(prop.table(table(dat$q07_stu_full)),2)
summary(dat$q07_stu_full); sd(dat$q07_stu_full)

# Travel Mode at Campus: Walk, Bike, or Car
dat$q11_mode_at_walk[is.na(dat$q11_mode_at_walk)] <- 0
table(dat$q11_mode_at_walk); round(prop.table(table(dat$q11_mode_at_walk)),2)
summary(dat$q11_mode_at_walk); sd(dat$q11_mode_at_walk)
dat$q11_mode_at_bike[is.na(dat$q11_mode_at_bike)] <- 0
table(dat$q11_mode_at_bike); round(prop.table(table(dat$q11_mode_at_bike)),2)
summary(dat$q11_mode_at_bike); sd(dat$q11_mode_at_bike)
dat$q11_mode_at_car[is.na(dat$q11_mode_at_car)] <- 0
table(dat$q11_mode_at_car); round(prop.table(table(dat$q11_mode_at_car)),2)
summary(dat$q11_mode_at_car); sd(dat$q11_mode_at_car)

## SADR Experiences

# Starship Utilization: Never, Less than Monthly, or Monthly
dat$sadr_use <- NA
table(dat$q12_sadr_use)
dat$sadr_use <- ifelse(dat$q12_sadr_use=="Never", 0,
                       ifelse(dat$q12_sadr_use=="Very rarely (one time per year or less)"|
                                dat$q12_sadr_use=="Rarely (one time per month or less)", 1,
                              ifelse(dat$q12_sadr_use=="Occasionally (two or three times per month)"|
                                       dat$q12_sadr_use=="Frequently (one time per week)"|
                                       dat$q12_sadr_use=="Very frequently (two or three times per week)"|
                                       dat$q12_sadr_use=="Always (one or more times per day)", 2, NA)))
table(dat$sadr_use); round(prop.table(table(dat$sadr_use)),2)
dat$sadr_use_0 <- NA; dat$sadr_use_0 <- ifelse(dat$sadr_use==0, 1, 0)
dat$sadr_use_1 <- NA; dat$sadr_use_1 <- ifelse(dat$sadr_use==1, 1, 0)
dat$sadr_use_2 <- NA; dat$sadr_use_2 <- ifelse(dat$sadr_use==2, 1, 0)

# Altered Path
dat$sadr_path <- NA
table(dat$q13_sadr_path)
dat$sadr_path <- ifelse(dat$q13_sadr_path=="Yes", 1, 0)
dat$sadr_path[is.na(dat$sadr_path)] <- 0
table(dat$sadr_path); round(prop.table(table(dat$sadr_path)),2)
summary(dat$sadr_path); sd(dat$sadr_path)

# SADR Shared Path Comfort: Pedestrian
dat$sadr_ped <- NA
table(dat$q14_sadr_ped)
dat$sadr_ped <- ifelse(dat$q14_sadr_ped=="Very uncomfortable"|dat$q14_sadr_ped=="Uncomfortable", 1,
                       ifelse(dat$q14_sadr_ped=="Neutral"|dat$q14_sadr_ped=="Don't know", 2,
                              ifelse(dat$q14_sadr_ped=="Comfortable"|dat$q14_sadr_ped=="Very comfortable", 3, NA)))
table(dat$sadr_ped); round(prop.table(table(dat$sadr_ped)),2)
dat$sadr_ped_1 <- NA; dat$sadr_ped_1 <- ifelse(dat$sadr_ped==1, 1, 0)
dat$sadr_ped_2 <- NA; dat$sadr_ped_2 <- ifelse(dat$sadr_ped==2, 1, 0)
dat$sadr_ped_3 <- NA; dat$sadr_ped_3 <- ifelse(dat$sadr_ped==3, 1, 0)

# SADR Shared Path Comfort: Bicyclist
dat$sadr_bike <- NA
table(dat$q15_sadr_bike)
dat$sadr_bike <- ifelse(dat$q15_sadr_bike=="Very uncomfortable"|dat$q15_sadr_bike=="Uncomfortable", 1,
                       ifelse(dat$q15_sadr_bike=="Neutral"|dat$q15_sadr_bike=="Don't know or don't ride a bicycle", 2,
                              ifelse(dat$q15_sadr_bike=="Comfortable"|dat$q15_sadr_bike=="Very comfortable", 3, NA)))
table(dat$sadr_bike); round(prop.table(table(dat$sadr_bike)),2)
dat$sadr_bike_1 <- NA; dat$sadr_bike_1 <- ifelse(dat$sadr_bike==1, 1, 0)
dat$sadr_bike_2 <- NA; dat$sadr_bike_2 <- ifelse(dat$sadr_bike==2, 1, 0)
dat$sadr_bike_3 <- NA; dat$sadr_bike_3 <- ifelse(dat$sadr_bike==3, 1, 0)

# SADR Adoption: Future Intention
dat$sadr_future <- NA
table(dat$q16a_future)
dat$sadr_future <- ifelse(dat$q16a_future=="1 (Least Agreement)", 1,
                          ifelse(dat$q16a_future=="2", 2,
                                 ifelse(dat$q16a_future=="3", 3,
                                        ifelse(dat$q16a_future=="4", 4,
                                               ifelse(dat$q16a_future=="5 (Most Agreement)", 5, NA)))))
table(dat$sadr_future); round(prop.table(table(dat$sadr_future)),2)

# SADR Paths: Ped/Bike
dat$sadr_pedbike <- NA
table(dat$q16b_pedbike)
dat$sadr_pedbike <- ifelse(dat$q16b_pedbike=="1 (Least Agreement)", 1,
                          ifelse(dat$q16b_pedbike=="2", 2,
                                 ifelse(dat$q16b_pedbike=="3", 3,
                                        ifelse(dat$q16b_pedbike=="4", 4,
                                               ifelse(dat$q16b_pedbike=="5 (Most Agreement)", 5, NA)))))
table(dat$sadr_pedbike); round(prop.table(table(dat$sadr_pedbike)),2)

# SADR Paths: Driver
dat$sadr_driver <- NA
table(dat$q16c_driver)
dat$sadr_driver <- ifelse(dat$q16c_driver=="1 (Least Agreement)", 1,
                           ifelse(dat$q16c_driver=="2", 2,
                                  ifelse(dat$q16c_driver=="3", 3,
                                         ifelse(dat$q16c_driver=="4", 4,
                                                ifelse(dat$q16c_driver=="5 (Most Agreement)", 5, NA)))))
table(dat$sadr_driver); round(prop.table(table(dat$sadr_driver)),2)

####################
### ORDERED LOGIT MODELS

# Clean data set for modeling
colnames(dat)
dat2 <- dat[,c("id_resp","ped_3s_4","ped_1s_4","bike_3s_3","bike_1s_3",
               "q01_oncampus","q04_male","q05_rac_whi","q05_rac_lat","q07_stu_full","q11_mode_at_car","q11_mode_at_bike","q11_mode_at_walk",
               "sadr_use_0","sadr_use_1","sadr_use_2","sadr_path","sadr_ped_1","sadr_ped_2","sadr_ped_3","sadr_bike_1","sadr_bike_2","sadr_bike_3",
               "sadr_future","sadr_pedbike","sadr_driver")]
head(dat2)
write_csv(dat2, paste(dat_dir, "dat_pard_model_v2.csv" , sep="_dat/_video/_tabular/"))

## OUTCOME 1: PEDESTRIAN, 3 SEC -- FOUR CATEGORIES

# Examine correlation of independent variables with PET ordinal measure
colnames(dat2)
cor_out <- as.data.frame(round(cor(dat2[,c(2,6:26,3,4,5)], use="complete.obs", method="spearman"), 2))
cor_out$var <- row.names(cor_out)
cor_out <- cor_out[,c(26,1)]
cor_out$abs_ped3s_cat <- NA; cor_out$abs_ped3s_cat <- abs(cor_out$ped_3s_4)
write_csv(cor_out, paste(dat_dir, "pard_ped3s_vars-spearman_v2.csv" , sep="_analysis/_outputs/"))

# Examine p-values of largest associations (p<0.10)
cor.test(x=dat2$sadr_ped_3, y=dat2$ped_3s_4, method="spearman")
cor.test(x=dat2$sadr_ped_2, y=dat2$ped_3s_4, method="spearman") # exclude from models
cor.test(x=dat2$sadr_future, y=dat2$ped_3s_4, method="spearman")
cor.test(x=dat2$sadr_pedbike, y=dat2$ped_3s_4, method="spearman")
cor.test(x=dat2$sadr_ped_1, y=dat2$ped_3s_4, method="spearman")
cor.test(x=dat2$sadr_bike_3, y=dat2$ped_3s_4, method="spearman")
cor.test(x=dat2$sadr_bike_1, y=dat2$ped_3s_4, method="spearman")
cor.test(x=dat2$sadr_path, y=dat2$ped_3s_4, method="spearman")
cor.test(x=dat2$sadr_use_2, y=dat2$ped_3s_4, method="spearman")

# MODEL 0: Constants only
dat2$ped_3s_4 <- as.factor(dat2$ped_3s_4)
m0_ped3 <- polr(ped_3s_4 ~ 1, data=dat2, Hess=TRUE)
logLik(m0_ped3)

# MODEL 1: Base model with correlated independent variables
m1 <- polr(ped_3s_4 ~ sadr_ped_3 + sadr_future + sadr_pedbike + sadr_ped_1 + sadr_bike_3 + sadr_bike_1 + sadr_path + sadr_use_2,
           data=dat2, Hess=TRUE)
summary(m1); confint(m1, level=0.90)

# MODEL 2: Remove non-significant predictors
m2 <- polr(ped_3s_4 ~ sadr_ped_3 + sadr_future + sadr_pedbike + sadr_ped_1, data=dat2, Hess=TRUE)
summary(m2); confint(m2, level=0.90)

# MODEL 3: Iterative addition of predictors with marginal significance (p<0.10)
m3 <- polr(ped_3s_4 ~ sadr_ped_3 + sadr_future + sadr_pedbike + sadr_ped_1, data=dat2, Hess=TRUE)
summary(m3); confint(m3, level=0.90)
# Note: 0 additional significant variables found

# FINAL MODEL
m1_ped3 <- polr(ped_3s_4 ~ sadr_ped_3 + sadr_future + sadr_pedbike + sadr_ped_1, data=dat2, Hess=TRUE)
summary(m1_ped3); confint(m1_ped3, level=0.90)
logLik(m1_ped3)
rm(cor_out,m1,m2,m3)

## OUTCOME 2: PEDESTRIAN, 1 SEC -- FOUR CATEGORIES

# Convert previous outcome format back to numeric
dat2$ped_3s_4 <- as.numeric(dat2$ped_3s_4)

# Examine correlation of independent variables with PET ordinal measure
colnames(dat2)
cor_out <- as.data.frame(round(cor(dat2[,c(3,6:26,2,4,5)], use="complete.obs", method="spearman"), 2))
cor_out$var <- row.names(cor_out)
cor_out <- cor_out[,c(26,1)]
cor_out$abs_ped1s_cat <- NA; cor_out$abs_ped1s_cat <- abs(cor_out$ped_1s_4)
write_csv(cor_out, paste(dat_dir, "pard_ped1s_vars-spearman_v2.csv" , sep="_analysis/_outputs/"))

# Examine p-values of largest associations (p<0.10)
cor.test(x=dat2$sadr_ped_3, y=dat2$ped_1s_4, method="spearman")
cor.test(x=dat2$sadr_pedbike, y=dat2$ped_1s_4, method="spearman")
cor.test(x=dat2$sadr_ped_2, y=dat2$ped_1s_4, method="spearman") # exclude from models
cor.test(x=dat2$sadr_ped_1, y=dat2$ped_1s_4, method="spearman")
cor.test(x=dat2$sadr_future, y=dat2$ped_1s_4, method="spearman")
cor.test(x=dat2$sadr_bike_3, y=dat2$ped_1s_4, method="spearman")
cor.test(x=dat2$sadr_path, y=dat2$ped_1s_4, method="spearman")
cor.test(x=dat2$sadr_bike_1, y=dat2$ped_1s_4, method="spearman")
cor.test(x=dat2$sadr_use_2, y=dat2$ped_1s_4, method="spearman")
cor.test(x=dat2$sadr_use_1, y=dat2$ped_1s_4, method="spearman")
cor.test(x=dat2$q04_male, y=dat2$ped_1s_4, method="spearman") # exclude from models

# MODEL 0: Constants only
dat2$ped_1s_4 <- as.factor(dat2$ped_1s_4)
m0_ped1 <- polr(ped_1s_4 ~ 1, data=dat2, Hess=TRUE)
logLik(m0_ped1)

# MODEL 1: Base model with correlated independent variables
m1 <- polr(ped_1s_4 ~ sadr_ped_3 + sadr_pedbike + sadr_ped_1 + sadr_future + sadr_bike_3 + sadr_path + sadr_bike_1 + sadr_use_2 + sadr_use_1,
           data=dat2, Hess=TRUE)
summary(m1); confint(m1, level=0.90)

# MODEL 2: Remove non-significant predictors
m2 <- polr(ped_1s_4 ~ sadr_ped_3 + sadr_pedbike + sadr_ped_1 + sadr_future + sadr_path, data=dat2, Hess=TRUE)
summary(m2); confint(m2, level=0.90)

# MODEL 3: Iterative addition of predictors with marginal significance (p<0.10)
m3 <- polr(ped_1s_4 ~ sadr_ped_3 + sadr_pedbike + sadr_ped_1 + sadr_future + sadr_path + sadr_bike_3 + sadr_use_2, data=dat2, Hess=TRUE)
summary(m3); confint(m3, level=0.90)
# Note: 2 additional significant variables found: sadr_bike_3, sadr_use_2

# FINAL MODEL
m1_ped1 <- polr(ped_1s_4 ~ sadr_ped_3 + sadr_pedbike + sadr_ped_1 + sadr_future + sadr_path + sadr_bike_3 + sadr_use_2, data=dat2, Hess=TRUE)
summary(m1_ped1); confint(m1_ped1, level=0.90)
logLik(m1_ped1)
rm(cor_out,m1,m2,m3)

## OUTCOME 3: BICYCLIST, 3 SEC -- THREE CATEGORIES

# Convert previous outcome format back to numeric
dat2$ped_1s_4 <- as.numeric(dat2$ped_1s_4)

# Examine correlation of independent variables with PET ordinal measure
colnames(dat2)
cor_out <- as.data.frame(round(cor(dat2[,c(4,6:26,2,3,5)], use="complete.obs", method="spearman"), 2))
cor_out$var <- row.names(cor_out)
cor_out <- cor_out[,c(26,1)]
cor_out$abs_bike3s_cat <- NA; cor_out$abs_bike3s_cat <- abs(cor_out$bike_3s_3)
write_csv(cor_out, paste(dat_dir, "pard_bike3s_vars-spearman_v2.csv" , sep="_analysis/_outputs/"))

# Examine p-values of largest associations (p<0.10)
cor.test(x=dat2$sadr_ped_3, y=dat2$bike_3s_3, method="spearman")
cor.test(x=dat2$sadr_bike_3, y=dat2$bike_3s_3, method="spearman")
cor.test(x=dat2$sadr_bike_1, y=dat2$bike_3s_3, method="spearman")
cor.test(x=dat2$sadr_pedbike, y=dat2$bike_3s_3, method="spearman")
cor.test(x=dat2$sadr_driver, y=dat2$bike_3s_3, method="spearman")
cor.test(x=dat2$sadr_ped_1, y=dat2$bike_3s_3, method="spearman")
cor.test(x=dat2$sadr_future, y=dat2$bike_3s_3, method="spearman")
cor.test(x=dat2$sadr_ped_2, y=dat2$bike_3s_3, method="spearman") # exclude from models
cor.test(x=dat2$sadr_path, y=dat2$bike_3s_3, method="spearman")
cor.test(x=dat2$sadr_use_2, y=dat2$bike_3s_3, method="spearman")

# MODEL 0: Constants only
dat2$bike_3s_3 <- as.factor(dat2$bike_3s_3)
m0_bike3 <- polr(bike_3s_3 ~ 1, data=dat2, Hess=TRUE)
logLik(m0_bike3)

# MODEL 1: Base model with correlated independent variables
m1 <- polr(bike_3s_3 ~ sadr_ped_3 + sadr_bike_3 + sadr_bike_1 + sadr_pedbike + sadr_driver + sadr_ped_1 + sadr_future + sadr_path + sadr_use_2, data=dat2, Hess=TRUE)
summary(m1); confint(m1, level=0.90)

# MODEL 2: Remove non-significant predictors
m2 <- polr(bike_3s_3 ~ sadr_ped_3 + sadr_bike_3 + sadr_pedbike + sadr_bike_1 + sadr_driver + sadr_ped_1 + sadr_future + sadr_path, data=dat2, Hess=TRUE)
summary(m2); confint(m2, level=0.90)

# MODEL 3: Iterative addition of predictors with marginal significance (p<0.10)
m3 <- polr(bike_3s_3 ~ sadr_ped_3 + sadr_bike_3 + sadr_pedbike + sadr_bike_1 + sadr_driver + sadr_ped_1 + sadr_future + sadr_path, data=dat2, Hess=TRUE)
summary(m3); confint(m3, level=0.90)
# Note: 0 additional significant variables found

# FINAL MODEL
m1_bike3 <- polr(bike_3s_3 ~ sadr_ped_3 + sadr_bike_3 + sadr_pedbike + sadr_bike_1 + sadr_driver + sadr_ped_1 + sadr_future + sadr_path, data=dat2, Hess=TRUE)
summary(m1_bike3); confint(m1_bike3, level=0.90)
logLik(m1_bike3)
rm(cor_out,m1,m2,m3)

## OUTCOME 4: BICYCLIST, 1 SEC -- THREE CATEGORIES

# Convert previous outcome format back to numeric
dat2$bike_3s_3 <- as.numeric(dat2$bike_3s_3)

# Examine correlation of independent variables with PET ordinal measure
colnames(dat2)
cor_out <- as.data.frame(round(cor(dat2[,c(5,6:26,2,3,4)], use="complete.obs", method="spearman"), 2))
cor_out$var <- row.names(cor_out)
cor_out <- cor_out[,c(26,1)]
cor_out$abs_bike1s_cat <- NA; cor_out$abs_bike1s_cat <- abs(cor_out$bike_1s_3)
write_csv(cor_out, paste(dat_dir, "pard_bike1s_vars-spearman_v2.csv" , sep="_analysis/_outputs/"))

# Examine p-values of largest associations (p<0.10)
cor.test(x=dat2$sadr_bike_3, y=dat2$bike_1s_3, method="spearman")
cor.test(x=dat2$sadr_pedbike, y=dat2$bike_1s_3, method="spearman")
cor.test(x=dat2$sadr_bike_1, y=dat2$bike_1s_3, method="spearman")
cor.test(x=dat2$sadr_path, y=dat2$bike_1s_3, method="spearman")
cor.test(x=dat2$sadr_ped_3, y=dat2$bike_1s_3, method="spearman")
cor.test(x=dat2$sadr_driver, y=dat2$bike_1s_3, method="spearman")
cor.test(x=dat2$sadr_ped_2, y=dat2$bike_1s_3, method="spearman") # exclude from model
cor.test(x=dat2$sadr_future, y=dat2$bike_1s_3, method="spearman")
cor.test(x=dat2$q05_rac_whi, y=dat2$bike_1s_3, method="spearman") # exclude from model
cor.test(x=dat2$sadr_use_1, y=dat2$bike_1s_3, method="spearman")
cor.test(x=dat2$sadr_use_2, y=dat2$bike_1s_3, method="spearman")
cor.test(x=dat2$sadr_ped_1, y=dat2$bike_1s_3, method="spearman")
cor.test(x=dat2$q11_mode_at_car, y=dat2$bike_1s_3, method="spearman")
cor.test(x=dat2$q07_stu_full, y=dat2$bike_1s_3, method="spearman")
cor.test(x=dat2$q04_male, y=dat2$bike_1s_3, method="spearman") # exclude from model
cor.test(x=dat2$q11_mode_at_walk, y=dat2$bike_1s_3, method="spearman")
cor.test(x=dat2$q05_rac_lat, y=dat2$bike_1s_3, method="spearman") # exclude from model
cor.test(x=dat2$q11_mode_at_bike, y=dat2$bike_1s_3, method="spearman")

# MODEL 0: Constants only
dat2$bike_1s_3 <- as.factor(dat2$bike_1s_3)
m0_bike1 <- polr(bike_1s_3 ~ 1, data=dat2, Hess=TRUE)
logLik(m0_bike1)

# MODEL 1: Base model with correlated independent variables
m1 <- polr(bike_1s_3 ~ sadr_bike_3 + sadr_pedbike + sadr_bike_1 + sadr_path + sadr_ped_3 + sadr_driver + sadr_future + sadr_use_1 + sadr_use_2 + sadr_ped_1 + q11_mode_at_car + q07_stu_full +
             q11_mode_at_walk + q11_mode_at_bike, data=dat2, Hess=TRUE)
summary(m1); confint(m1, level=0.90)

# MODEL 2: Remove non-significant predictors
m2 <- polr(bike_1s_3 ~ sadr_bike_3 + sadr_pedbike + sadr_bike_1 + sadr_path + sadr_driver + sadr_use_1 + sadr_ped_1 + q11_mode_at_car + q11_mode_at_bike, data=dat2, Hess=TRUE)
summary(m2); confint(m2, level=0.90)

# MODEL 3: Iterative addition of predictors with marginal significance (p<0.10)
m3 <- polr(bike_1s_3 ~ sadr_bike_3 + sadr_pedbike + sadr_bike_1 + sadr_path + sadr_driver + sadr_use_1 + sadr_ped_1 + q11_mode_at_car + q11_mode_at_bike, data=dat2, Hess=TRUE)
summary(m3); confint(m3, level=0.90)
# Note: 0 additional significant variables found

# FINAL MODEL
m1_bike1 <- polr(bike_1s_3 ~ sadr_bike_3 + sadr_pedbike + sadr_bike_1 + sadr_path + sadr_driver + sadr_use_1 + sadr_ped_1 + q11_mode_at_car + q11_mode_at_bike, data=dat2, Hess=TRUE)
summary(m1_bike1); confint(m1_bike1, level=0.90)
logLik(m1_bike1)
rm(cor_out,m1,m2,m3)

####################
### ORDERED LOGIT MODELS: FINAL RESULTS (5 CATEGORIES)

# Pedestrian Models
summary(m1_ped3); confint(m1_ped3, level=0.90)
logLik(m1_ped3); logLik(m0_ped3)
summary(m1_ped1); confint(m1_ped1, level=0.90)
logLik(m1_ped1); logLik(m0_ped1)

# Bicyclist Models
summary(m1_bike3); confint(m1_bike3, level=0.90)
logLik(m1_bike3); logLik(m0_bike3)
summary(m1_bike1); confint(m1_bike1, level=0.90)
logLik(m1_bike1); logLik(m0_bike1)

####################
### END OF SCRIPT
