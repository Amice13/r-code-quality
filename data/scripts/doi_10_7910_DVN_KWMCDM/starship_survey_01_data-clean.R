####################################################
# Steven R. Gehrke
# Northern Arizona University
# 21-0405: Starship Robots
# Task: PAR-D Survey Data Cleaning
# Created: 05.03.2022
# Updated: 05.09.2022
####################################################

####################
### SET-UP

# Load libraries
# install.packages(c("tidyverse"))
packages <- c("tidyverse")
lapply(packages, require, character.only=TRUE); rm(packages)

# Set directory
dat_dir <- "H:/_projects/21-0405_SidewalkDeliveryRobots/"

# Import raw survey data
dat <- read_csv(paste(dat_dir, "dat_pard_raw.csv", sep="_dat/_survey/_tabular/"))
colnames(dat)
head(dat)

####################
### DATA CLEANING

# Remove extra column headers
dat <- dat[-c(1:2),]
head(dat)

# Rename remaining column headers
colnames(dat)
colnames(dat) <- c("date_start","date_end","status","ip_add","progress","dur_sec","finished","date_rec",
                   "id_resp","name_last","name_first","email","ref_ext","loc_lat","loc_long","distrib_ch","user_lang","consent",
                   "q01_house","q02_zip","q03_age","q04_gend","q04_gend_o","q05_race","q05_race_o","q06_income","q07_status","q07_status_o",
                   "q08_edu","q09_edu_deg","q10_mode_to","q10_mode_to_o","q11_mode_at","q11_mode_at_o","q12_sadr_use","q13_sadr_path",
                   "q14_sadr_ped","q15_sadr_bike","q16a_future","q16b_pedbike","q16c_driver",
                   "q17_ped_3s","q18_ped_1s","q19_bike_3s","q20_bike_1s")

# Filter 1: Completed survey
table(dat$finished)
dat <- dat[dat$finished==c("True"),]
table(dat$progress)

# Filter 2: Survey date
dat$date_survey <- substr(dat$date_rec, start=1, stop=10)
table(dat$date_survey)

# Reduce and reorganize column headers
colnames(dat)
dat2 <- dat[,c(9,46,5:6,19:45)]

####################
### DATA CONVERSION (SECTION 1)

# Convert survey time from text to numeric
str(dat2)
dat2$dur_sec <- as.numeric(dat2$dur_sec)

# Q01: On-campus housing
dat2$q01_oncampus <- ifelse(dat2$q01_house==c("On-campus housing"),1,0)

# Q02: ZIP code
dat2$q02_zip[is.na(dat2$q02_zip)] <- c("86011")

# Q03: Age
table(dat2$q03_age)
dat2$q03_age1 <- ifelse(dat2$q03_age==c("18 - 24 years"),1,0)
dat2$q03_age2 <- ifelse(dat2$q03_age==c("25 - 34 years"),1,0)
dat2$q03_age3 <- ifelse(dat2$q03_age==c("35 - 44 years"),1,0) # Note: No responses for 45-64 or 65 and over

# Q04: Gender
table(dat2$q04_gend)
dat2$q04_male <- ifelse(dat2$q04_gend==c("Male"),1,
                        ifelse(dat2$q04_gend==c("Prefer not to answer"),NA,0))

# Q05: Race/ethnicity
table(dat2$q05_race)
dat2$q05_rac_whi <- ifelse(dat2$q05_race==c("White/Caucasian"),1,
                           ifelse(dat2$q05_race==c("White/Caucasian,Prefer not to answer"),1,
                                  ifelse(dat2$q05_race==c("White/Caucasian,Self-describe (please specify)"),1,
                                         ifelse(dat2$q05_race==c("Self-describe (please specify)"),1,
                                                ifelse(dat2$q05_race==c("Prefer not to answer"),NA,0)))))
dat2$q05_rac_lat <- ifelse(dat2$q05_race==c("Latino/Hispanic"),1,
                           ifelse(dat2$q05_race==c("Prefer not to answer"),NA,0))
dat2$q05_rac_baa <- ifelse(dat2$q05_race==c("Black/African American"),1,
                           ifelse(dat2$q05_race==c("Prefer not to answer"),NA,0))
dat2$q05_rac_asian <- ifelse(dat2$q05_race==c("Asian"),1,
                           ifelse(dat2$q05_race==c("Prefer not to answer"),NA,0))
dat2$q05_rac_aian <- ifelse(dat2$q05_race==c("American Indian or Alaska Native"),1,
                            ifelse(dat2$q05_race==c("American Indian or Alaska Native,Prefer not to answer"),1,
                                   ifelse(dat2$q05_race==c("Prefer not to answer"),NA,0)))
dat2$q05_rac_nhpi <- ifelse(dat2$q05_race==c("Native Hawaiian or other Pacific Islander"),1,
                            ifelse(dat2$q05_race==c("Prefer not to answer"),NA,0))
dat2$q05_rac_multi <- ifelse(dat2$q05_race==c("Asian,American Indian or Alaska Native,Native Hawaiian or other Pacific Islander"),1,
                             ifelse(dat2$q05_race==c("Asian,Native Hawaiian or other Pacific Islander"),1,
                                    ifelse(dat2$q05_race==c("Black/African American,Asian"),1,
                                           ifelse(dat2$q05_race==c("Latino/Hispanic,American Indian or Alaska Native"),1,
                                                  ifelse(dat2$q05_race==c("Latino/Hispanic,Asian"),1,
                                                         ifelse(dat2$q05_race==c("White/Caucasian,American Indian or Alaska Native"),1,
                                                                ifelse(dat2$q05_race==c("White/Caucasian,Asian"),1,
                                                                       ifelse(dat2$q05_race==c("White/Caucasian,Asian,Native Hawaiian or other Pacific Islander"),1,
                                                                              ifelse(dat2$q05_race==c("White/Caucasian,Asian,Self-describe (please specify)"),1,
                                                                                     ifelse(dat2$q05_race==c("White/Caucasian,Black/African American"),1,
                                                                                            ifelse(dat2$q05_race==c("White/Caucasian,Latino/Hispanic"),1,
                                                                                                   ifelse(dat2$q05_race==c("White/Caucasian,Latino/Hispanic,Asian"),1,
                                                                                                          ifelse(dat2$q05_race==c("White/Caucasian,Latino/Hispanic,Black/African American,Asian,American Indian or Alaska Native,Native Hawaiian or other Pacific Islander"),1,
                                                                                                                 ifelse(dat2$q05_race==c("White/Caucasian,Native Hawaiian or other Pacific Islander"),1,
                                                                                                                        ifelse(dat2$q05_race==c("White/Caucasian,Native Hawaiian or other Pacific Islander"),1,
                                                                                                                               ifelse(dat2$q05_race==c("Prefer not to answer"),NA,
                                                                                                                                      0))))))))))))))))

# Q06: Personal income
table(dat2$q06_income)
dat2$q06_inc1 <- ifelse(dat2$q06_income==c("Under $15,000"),1,
                        ifelse(dat2$q06_income==c("Prefer not to answer"),NA,0))
dat2$q06_inc2 <- ifelse(dat2$q06_income==c("$15,000 - $34,999"),1,
                        ifelse(dat2$q06_income==c("Prefer not to answer"),NA,0))
dat2$q06_inc3 <- ifelse(dat2$q06_income==c("$35,000 - $49,999"),1,
                        ifelse(dat2$q06_income==c("Prefer not to answer"),NA,0))
dat2$q06_inc4 <- ifelse(dat2$q06_income==c("$50,000 - $74,999"),1,
                        ifelse(dat2$q06_income==c("$100,000 or more"),1,
                               ifelse(dat2$q06_income==c("Prefer not to answer"),NA,0))) # Note: No responses for $75,000-$99,999

# Q07: Work status
table(dat2$q07_status)
dat2$q07_work_full <- ifelse(dat2$q07_status==c("Full-time work (35 hours or more per week)"),1,
                             ifelse(dat2$q07_status==c("Full-time work (35 hours or more per week),Full-time student"),1,
                                    ifelse(dat2$q07_status==c("Full-time work (35 hours or more per week),Part-time work (1-34 hours of work per week)"),1,
                                           ifelse(dat2$q07_status==c("Full-time work (35 hours or more per week),Part-time work (1-34 hours of work per week)"),1,
                                                  0))))
dat2$q07_work_part <- ifelse(dat2$q07_status==c("Part-time work (1-34 hours of work per week)"),1,
                             ifelse(dat2$q07_status==c("Part-time work (1-34 hours of work per week),Full-time student"),1,
                                    ifelse(dat2$q07_status==c("Part-time work (1-34 hours of work per week),Part-time student"),1,
                                                  0)))
dat2$q07_stu_full <- ifelse(dat2$q07_status==c("Full-time student"),1,
                             ifelse(dat2$q07_status==c("Full-time student,Other (please specify)"),1,
                                    ifelse(dat2$q07_status==c("Full-time student,Part-time student"),1,
                                           ifelse(dat2$q07_status==c("Full-time student,Unemployed and looking for work"),1,
                                                  ifelse(dat2$q07_status==c("Full-time student,Unemployed and NOT looking for work"),1,
                                                         ifelse(dat2$q07_status==c("Full-time work (35 hours or more per week),Full-time student"),1,
                                                                ifelse(dat2$q07_status==c("Part-time work (1-34 hours of work per week),Full-time student"),1,
                                                                       0)))))))
dat2$q07_stu_part <- ifelse(dat2$q07_status==c("Part-time student"),1,
                            ifelse(dat2$q07_status==c("Part-time work (1-34 hours of work per week),Part-time student"),1,
                                   0))

# Q08 & Q09: Educational status
dat2$q08_educ <- dat2$q09_edu_deg
dat2$q08_educ[dat2$q08_edu==c("Freshman")] <- c("Associate degree or some college")
dat2$q08_educ[dat2$q08_edu==c("Sophomore")] <- c("Associate degree or some college")
dat2$q08_educ[dat2$q08_edu==c("Junior")] <- c("Associate degree or some college")
dat2$q08_educ[dat2$q08_edu==c("Senior")] <- c("Associate degree or some college")
dat2$q08_educ[dat2$q08_edu==c("Master's student")] <- c("Bachelor's degree")
table(dat2$q08_educ)
dat2$q08_educ <- recode(dat2$q08_educ, "Associate degree or some college"=2,"Bachelor's degree"=3,
                        "Graduate degree (Masters, PhD)"=4,"High school degree or equivalent"=1)
table(dat2$q08_educ)
dat2$q08_educ1 <- ifelse(dat2$q08_educ==1,1,0)
dat2$q08_educ2 <- ifelse(dat2$q08_educ==2,1,0)
dat2$q08_educ3 <- ifelse(dat2$q08_educ==3,1,0)
dat2$q08_educ4 <- ifelse(dat2$q08_educ==4,1,0)

# Q10: Mode to NAU
table(dat2$q10_mode_to); table(dat2$q10_mode_to_o)
dat2$q10_mode_to_car[dat2$q10_mode_to=="Car"] <- 1
dat2$q10_mode_to_car[dat2$q10_mode_to=="Bicycle"|dat2$q10_mode_to=="Bus"|dat2$q10_mode_to=="Other (please specify)"|dat2$q10_mode_to=="Walk"] <- 0
table(dat2$q10_mode_to_car)
dat2$q10_mode_to_bike[dat2$q10_mode_to=="Bicycle"] <- 1
dat2$q10_mode_to_bike[dat2$q10_mode_to=="Car"|dat2$q10_mode_to=="Bus"|dat2$q10_mode_to=="Other (please specify)"|dat2$q10_mode_to=="Walk"] <- 0
table(dat2$q10_mode_to_bike)
dat2$q10_mode_to_walk[dat2$q10_mode_to=="Walk"] <- 1
dat2$q10_mode_to_walk[dat2$q10_mode_to=="Car"|dat2$q10_mode_to=="Bus"|dat2$q10_mode_to=="Other (please specify)"|dat2$q10_mode_to=="Bicycle"] <- 0
table(dat2$q10_mode_to_walk)

# Q11: Mode around NAU
table(dat2$q11_mode_at); table(dat2$q11_mode_at_o)
dat2$q11_mode_at_car <- ifelse(dat2$q11_mode_at==c("Car"),1,
                             ifelse(dat2$q11_mode_at==c("Car,Bicycle,Walk"),1,
                                    ifelse(dat2$q11_mode_at==c("Car,Bus"),1,
                                           ifelse(dat2$q11_mode_at==c("Car,Bus,Bicycle,Walk"),1,
                                                  ifelse(dat2$q11_mode_at==c("Car,Bus,Bicycle,Walk,Other (please specify)"),1,
                                                         ifelse(dat2$q11_mode_at==c("Car,Bus,Walk"),1,
                                                                ifelse(dat2$q11_mode_at==c("Car,Bus,Walk,Other (please specify)"),1,
                                                                       ifelse(dat2$q11_mode_at==c("Car,Other (please specify)"),1,
                                                                              ifelse(dat2$q11_mode_at==c("Car,Walk"),1,
                                                                                     0)))))))))
dat2$q11_mode_at_bike <- ifelse(dat2$q11_mode_at==c("Bicycle"),1,
                               ifelse(dat2$q11_mode_at==c("Bicycle,Walk"),1,
                                      ifelse(dat2$q11_mode_at==c("Bicycle,Walk,Other (please specify)"),1,
                                             ifelse(dat2$q11_mode_at==c("Bus,Bicycle"),1,
                                                    ifelse(dat2$q11_mode_at==c("Bus,Bicycle,Walk"),1,
                                                           ifelse(dat2$q11_mode_at==c("Car,Bicycle,Walk"),1,
                                                                  ifelse(dat2$q11_mode_at==c("Car,Bus,Bicycle,Walk"),1,
                                                                         ifelse(dat2$q11_mode_at==c("Car,Bus,Bicycle,Walk,Other (please specify)"),1,
                                                                                0))))))))
dat2$q11_mode_at_walk <- ifelse(dat2$q11_mode_at==c("Bicycle,Walk"),1,
                                ifelse(dat2$q11_mode_at==c("Bicycle,Walk,Other (please specify)"),1,
                                       ifelse(dat2$q11_mode_at==c("Bus,Bicycle,Walk"),1,
                                              ifelse(dat2$q11_mode_at==c("Bus,Walk"),1,
                                                     ifelse(dat2$q11_mode_at==c("Bus,Walk,Other (please specify)"),1,
                                                            ifelse(dat2$q11_mode_at==c("Car,Bicycle,Walk"),1,
                                                                   ifelse(dat2$q11_mode_at==c("Car,Bus,Bicycle,Walk"),1,
                                                                          ifelse(dat2$q11_mode_at==c("Car,Bus,Bicycle,Walk,Other (please specify)"),1,
                                                                                 ifelse(dat2$q11_mode_at==c("Car,Bus,Walk"),1,
                                                                                        ifelse(dat2$q11_mode_at==c("Car,Bus,Walk,Other (please specify)"),1,
                                                                                               ifelse(dat2$q11_mode_at==c("Car,Walk"),1,
                                                                                                      ifelse(dat2$q11_mode_at==c("Walk"),1,
                                                                                                             ifelse(dat2$q11_mode_at==c("Walk,Other (please specify)"),1,
                                                                                                                    0)))))))))))))

####################
### DATA CONVERSION (SECTIONS 2 & 3)

# Q12: Starship (SADR) use
table(dat2$q12_sadr_use)
dat2$q12_sadr_use <- recode(dat2$q12_sadr_use, "Never"=1,"Very rarely (one time per year or less)"=2,
                            "Rarely (one time per month or less)"=3,"Occasionally (two or three times per month)"=4,
                            "Frequently (one time per week)"=5,"Very frequently (two or three times per week)"=6,
                            "Always (one or more times per day)"=7)
table(dat2$q12_sadr_use)

# Q13: Altered intended path
table(dat2$q13_sadr_path)
dat2$q13_sadr_path <- recode(dat2$q13_sadr_path, "Yes"=1,"No"=0,"Don't know"=99)
table(dat2$q13_sadr_path)

# Q14: Sharing path / walk
table(dat2$q14_sadr_ped)
dat2$q14_sadr_ped <- recode(dat2$q14_sadr_ped, "Very uncomfortable"=1,"Uncomfortable"=2,"Neutral"=3,"Comfortable"=4,"Very comfortable"=5,
                            "Don't know"=99)
table(dat2$q14_sadr_ped)

# Q15: Sharing path / bike
table(dat2$q15_sadr_bike)
dat2$q15_sadr_bike <- recode(dat2$q15_sadr_bike, "Very uncomfortable"=1,"Uncomfortable"=2,"Neutral"=3,"Comfortable"=4,"Very comfortable"=5,
                             "Don't know or don't ride a bicycle"=99)
table(dat2$q15_sadr_bike)

# Q16a: SADR future use
table(dat2$q16a_future)
dat2$q16a_future <- recode(dat2$q16a_future, "1 (Least Agreement)"=1,"2"=2,"3"=3,"4"=4,"5 (Most Agreement)"=5)
table(dat2$q16a_future)

# Q16b: SADR use on ped/bike facilities
table(dat2$q16b_pedbike)
dat2$q16b_pedbike <- recode(dat2$q16b_pedbike, "1 (Least Agreement)"=1,"2"=2,"3"=3,"4"=4,"5 (Most Agreement)"=5)
table(dat2$q16b_pedbike)

# Q16c: SADR use on roadways
table(dat2$q16c_driver)
dat2$q16c_driver <- recode(dat2$q16c_driver, "1 (Least Agreement)"=1,"2"=2,"3"=3,"4"=4,"5 (Most Agreement)"=5)
table(dat2$q16c_driver)

# Q17: Video/ped/3 secs
table(dat2$q17_ped_3s)
dat2$q17_ped_3s <- recode(dat2$q17_ped_3s, "Very uncomfortable"=1,"Uncomfortable"=2,"Neutral"=3,"Comfortable"=4,"Very comfortable"=5)
table(dat2$q17_ped_3s)

# Q18: Video/ped/1 secs
table(dat2$q18_ped_1s)
dat2$q18_ped_1s <- recode(dat2$q18_ped_1s, "Very uncomfortable"=1,"Uncomfortable"=2,"Neutral"=3,"Comfortable"=4,"Very comfortable"=5)
table(dat2$q18_ped_1s)

# Q19: Video/bike/3 secs
table(dat2$q19_bike_3s)
dat2$q19_bike_3s <- recode(dat2$q19_bike_3s, "Very uncomfortable"=1,"Uncomfortable"=2,"Neutral"=3,"Comfortable"=4,"Very comfortable"=5)
table(dat2$q19_bike_3s)

# Q20: Video/bike/1 secs
table(dat2$q20_bike_1s)
dat2$q20_bike_1s <- recode(dat2$q20_bike_1s, "Very uncomfortable"=1,"Uncomfortable"=2,"Neutral"=3,"Comfortable"=4,"Very comfortable"=5)
table(dat2$q20_bike_1s)

####################
### EXPORT DATA SET

# Reorganize new data frame
colnames(dat2)
dat3 <- dat2[,c("id_resp","date_survey","progress","dur_sec","q01_oncampus","q03_age1","q03_age2","q03_age3","q04_male",
                "q05_rac_whi","q05_rac_lat","q05_rac_baa","q05_rac_asian","q05_rac_aian","q05_rac_nhpi","q05_rac_multi",
                "q06_inc1","q06_inc2","q06_inc3","q06_inc4","q07_work_full","q07_work_part","q07_stu_full","q07_stu_part",
                "q08_educ","q08_educ1","q08_educ2","q08_educ3","q08_educ4","q10_mode_to_car","q10_mode_to_bike","q10_mode_to_walk",
                "q11_mode_at_car","q11_mode_at_bike","q11_mode_at_walk","q12_sadr_use","q13_sadr_path","q14_sadr_ped","q15_sadr_bike",
                "q16a_future","q16b_pedbike","q16c_driver","q17_ped_3s","q18_ped_1s","q19_bike_3s","q20_bike_1s")]

# Export data set
write_csv(dat3, paste(dat_dir, "dat_pard_clean.csv", sep="_dat/_survey/_tabular/"))

####################
### END OF SCRIPT
