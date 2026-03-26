
rm(list=ls())
setwd("W:/02192835/incoming/maya_hisachon_chiild")

library(stargazer)
library(lubridate)
library(ggplot2)
library(reshape2)
library(dplyr)
library(scales)
library(pastecs)
library(Hmisc)
library(MASS)
library(plyr)
library(foreign)
library(rio)
library(data.table)
library(Matching)
library(MatchIt)
library(rmarkdown)
library(dplyr)
library(textclean)
library(substr)
library(lubridate)

list.files()

cda_seker_new <- read.csv("W:/02192835/incoming/maya_hisachon_chiild/cda_seker_newvlookup.csv", header=TRUE)
##make dates dates

cda_seker_new$father_tar_idkun_022017_date <- 
  as.Date(cda_seker_new$father_tar_idkun_022017,
          format="%d/%m/%Y")


cda_seker_new$choose_date_date <- 
  as.Date(cda_seker_new$choose_date,
          format="%d/%m/%Y")


cda_seker_new$mother_tar_idkun_022017_date <- 
  as.Date(cda_seker_new$mother_tar_idkun_022017,
          format="%d/%m/%Y")

#######################
#################################################################

cda_seker_new$zman_father_feb1 <- as.POSIXct(cda_seker_new$father_zman_idkun_022017,format="%d/%m/%Y-%H:%M:%S")
cda_seker_new$zman_mother_feb1 <- as.POSIXct(cda_seker_new$mother_zman_idkun_022017,format="%d/%m/%Y-%H:%M:%S")
cda_seker_new$zman_idkun_022017 <- with (cda_seker_new, pmin(zman_mother_feb1,zman_father_feb1, na.rm=TRUE))
cda_seker_new$zman_idkun_022017[is.na(cda_seker_new$zman_idkun_022017)] <- "1899-12-30"


#############

cda_seker_new$zman_idkun_022017_afternoon <- ifelse(cda_seker_new$zman_idkun_022017>"1899-12-30 00:00:00" & cda_seker_new$zman_idkun_022017<"2017-02-06 16:00:00" ,1,0 )
cda_seker_new$zman_idkun_022017_afternoon <- ifelse(cda_seker_new$zman_idkun_022017>"2017-02-07 14:00:00" & cda_seker_new$zman_idkun_022017<"2017-02-07 16:00:00" ,1,cda_seker_new$zman_idkun_022017_afternoon) 
cda_seker_new$zman_idkun_022017_evening <- ifelse(cda_seker_new$zman_idkun_022017>"2017-02-06 16:00:00" & cda_seker_new$zman_idkun_022017<"2017-02-06 17:30:00" ,1,0 )
cda_seker_new$zman_idkun_022017_evening <- ifelse(cda_seker_new$zman_idkun_022017>"2017-02-07 16:00:00" & cda_seker_new$zman_idkun_022017<"2017-02-07 17:30:00" ,1,cda_seker_new$zman_idkun_022017_evening) 
cda_seker_new$zman_idkun_022017_morning <- ifelse(cda_seker_new$zman_idkun_022017>"2017-02-06 00:00:00" & cda_seker_new$zman_idkun_022017<"2017-02-06 10:00:00" ,1,0 )
cda_seker_new$zman_idkun_022017_morning <- ifelse(cda_seker_new$zman_idkun_022017>"2017-02-07 00:00:00" & cda_seker_new$zman_idkun_022017<"2017-02-07 10:00:00" ,1,cda_seker_new$zman_idkun_022017_morning) 

              ##make variables for receiving sms
cda_seker_new$tar_idkun_022017 <- with (cda_seker_new, pmin(mother_tar_idkun_022017_date,father_tar_idkun_022017_date, na.rm=TRUE))
cda_seker_new$tar_idkun_022017_yes=ifelse(cda_seker_new$tar_idkun_022017>0,1,0)
cda_seker_new$tar_idkun_022017_yes[is.na(cda_seker_new$tar_idkun_022017_yes)] <- 0




#number of days between choice and SMS
cda_seker_new$choice_days_022017=cda_seker_new$choose_date_date-cda_seker_new$tar_idkun_022017
cda_seker_new$choice_days_022017_c <- as.integer(cda_seker_new$choice_days_022017)
##make 0/1 variable for who took action following sms - Y variables
cda_seker_new$choice_3days_022017 <- ifelse(cda_seker_new$choice_days_022017>0 & cda_seker_new$choice_days_022017<4,1,0)
cda_seker_new$choice_3days_022017[is.na(cda_seker_new$choice_3days_022017)] <- 0
cda_seker_new$choice_7days_022017 <- ifelse(cda_seker_new$choice_days_022017>0 & cda_seker_new$choice_days_022017<8,1,0)
cda_seker_new$choice_7days_022017[is.na(cda_seker_new$choice_7days_022017)] <- 0

cda_seker_new$sms_022017 <- cda_seker_new$tar_idkun_022017_yes
 cda_seker_new$choose_new <- cda_seker_new$choose_date_date>0
 cda_seker_new$choose_new[is.na(cda_seker_new$choose_new)] <- 0
 cda_seker_new$feb_1<- cda_seker_new$choose_date_date=="2017-02-06" |cda_seker_new$choose_date_date=="2017-02-07"
 cda_seker_new$feb_2<- cda_seker_new$choose_date_date=="2017-02-06" |cda_seker_new$choose_date_date=="2017-02-07"|
   cda_seker_new$choose_date_date=="2017-02-08"
 cda_seker_new$feb_1[is.na(cda_seker_new$feb_1)] <- 0
 cda_seker_new$feb_2[is.na(cda_seker_new$feb_2)] <- 0
 
 summary( cda_seker_new$feb_1)
 summary( cda_seker_new$feb_2)
 
 cda_seker_new$child_with_disability1 <- cda_seker_new$child_with_disability
 cda_seker_new$child_with_disability1[is.na(cda_seker_new$child_with_disability)] <- 0
 
 
cda_seker_new$feb_19 <- cda_seker_new$choose_date_date=="2017-02-06" |cda_seker_new$choose_date_date=="2017-02-07"|
  cda_seker_new$choose_date_date=="2017-02-08"|cda_seker_new$choose_date_date=="2017-02-09"|cda_seker_new$choose_date_date=="2017-02-10"|
  cda_seker_new$choose_date_date=="2017-02-11"|cda_seker_new$choose_date_date=="2017-02-12"|cda_seker_new$choose_date_date=="2017-02-13"|  
  cda_seker_new$choose_date_date=="2017-02-14"|cda_seker_new$choose_date_date=="2017-02-15"|cda_seker_new$choose_date_date=="2017-02-16"|
  cda_seker_new$choose_date_date=="2017-02-17"|cda_seker_new$choose_date_date=="2017-02-18"|cda_seker_new$choose_date_date=="2017-02-19"
cda_seker_new$feb_10 <- cda_seker_new$choose_date_date=="2017-02-06" |cda_seker_new$choose_date_date=="2017-02-07"|
  cda_seker_new$choose_date_date=="2017-02-08"|cda_seker_new$choose_date_date=="2017-02-09"|cda_seker_new$choose_date_date=="2017-02-10"
 
cda_seker_new$feb_19[is.na(cda_seker_new$feb_19)] <- 0
cda_seker_new$feb_10[is.na(cda_seker_new$feb_10)] <- 0

cda_seker_new$feb_19 <- as.integer(cda_seker_new$feb_19)
cda_seker_new$feb_10 <- as.integer(cda_seker_new$feb_10)

#############################



cda_seker_new$feb_50_20 <- cda_seker_new$choose_date_date=="2017-02-06"&  cda_seker_new$ex_50_nis==1 |cda_seker_new$choose_date_date=="2017-02-07"&  cda_seker_new$ex_50_nis==1|
  cda_seker_new$choose_date_date=="2017-02-08"&  cda_seker_new$ex_50_nis==1|cda_seker_new$choose_date_date=="2017-02-09"&  cda_seker_new$ex_50_nis==1|cda_seker_new$choose_date_date=="2017-02-10"&  cda_seker_new$ex_50_nis==1|
  cda_seker_new$choose_date_date=="2017-02-11"&  cda_seker_new$ex_50_nis==1|cda_seker_new$choose_date_date=="2017-02-12"&  cda_seker_new$ex_50_nis==1|cda_seker_new$choose_date_date=="2017-02-13"&  cda_seker_new$ex_50_nis==1|  
  cda_seker_new$choose_date_date=="2017-02-14"&  cda_seker_new$ex_50_nis==1|cda_seker_new$choose_date_date=="2017-02-15"&  cda_seker_new$ex_50_nis==1|cda_seker_new$choose_date_date=="2017-02-16"&  cda_seker_new$ex_50_nis==1|
  cda_seker_new$choose_date_date=="2017-02-17"&  cda_seker_new$ex_50_nis==1|cda_seker_new$choose_date_date=="2017-02-18"&  cda_seker_new$ex_50_nis==1|cda_seker_new$choose_date_date=="2017-02-19"&  cda_seker_new$ex_50_nis==1|
  cda_seker_new$choose_date_date=="2017-02-20"&  cda_seker_new$ex_50_nis==1
  cda_seker_new$choose_date_date=="2017-02-08"&  cda_seker_new$ex_50_nis==1|cda_seker_new$choose_date_date=="2017-02-09"&  cda_seker_new$ex_50_nis==1|cda_seker_new$choose_date_date=="2017-02-10"&  cda_seker_new$ex_50_nis==1

cda_seker_new$feb_50_20[is.na(cda_seker_new$feb_50_20)] <- 0

cda_seker_new$feb_50_20 <- as.integer(cda_seker_new$feb_50_20)

###############################################
cda_seker_new$tar_idkun_022017_1 <-cda_seker_new$tar_idkun_022017
cda_seker_new$tar_idkun_022017_1[is.na(cda_seker_new$tar_idkun_022017_1)] <- "1899-12-30"
cda_seker_new$choose_date_date_1 <-cda_seker_new$choose_date_date
cda_seker_new$choose_date_date_1[is.na(cda_seker_new$choose_date_date_1)] <-  "2020-12-30"
cda_seker_new <- cda_seker_new[!(cda_seker_new$tar_idkun_022017_1=="2017-02-07"  & cda_seker_new$choose_date_date=="2017-02-06"),]
cda_seker_new$sms_feb <- cda_seker_new$tar_idkun_022017=="2017-02-06"|cda_seker_new$tar_idkun_022017=="2017-02-07"
cda_seker_new$sms_feb[is.na(cda_seker_new$sms_feb)] <- 0
cda_seker_new$sms_feb_father <- cda_seker_new$father_tar_idkun_022017_date=="2017-02-06"|cda_seker_new$father_tar_idkun_022017_date=="2017-02-07"
cda_seker_new$sms_feb_mother <- cda_seker_new$mother_tar_idkun_022017_date=="2017-02-06"|cda_seker_new$mother_tar_idkun_022017_date=="2017-02-07"
cda_seker_new$seker_sms <- ifelse(cda_seker_new$sms_feb_father==1 & cda_seker_new$participant==2 ,1,0)
cda_seker_new$seker_sms <- ifelse(cda_seker_new$sms_feb_mother==1 & cda_seker_new$participant==1 ,1,cda_seker_new$seker_sms)
cda_seker_new$seker_sms <- ifelse(is.na(cda_seker_new$seker_sms),0,cda_seker_new$seker_sms)

######################data base partition
cda_seker_new_before_patition <- cda_seker_new
save(cda_seker_new_before_patition ,file="cda_seker_new_before_patition")
load("cda_seker_new_before_patition")

cda_seker_big<- cda_seker_new_before_patition
cda_seker_big$k <- cda_seker_big$choose_date_date<"2017-02-06"
cda_seker_big$k[is.na(cda_seker_big$k)] <- 0
cda_seker_big <- cda_seker_big[cda_seker_big$k==0,]


my_vars1 <- c("cover_expenses_easily","choose_date_date_1","zman_idkun_022017_afternoon","zman_idkun_022017_evening","zman_idkun_022017_morning",
             "zman_idkun_052017_afternoon","zman_idkun_052017_lunch","zman_idkun_052017_morning",
             "zman_idkun_052017_lunch_sms2","zman_idkun_052017_morning_sms2",
             "zman_idkun_052017","zman_idkun_022017","investment_fund", "ex_50_nis","child_male","num_family","num_child_in_family","kod_choice","device", "mother_work", "mother_wage","mother_academic", "father_work", "father_wage", "father_academic",  "parents_num_children", "parents_hardi", "parents_arab", "parents_married", "parents_avg_age", "socio_econ_cluster", "periphery_cluster", "rural", "gill_child", "sms_022017", "choose_new", "feb_19", "feb_10", "sms_feb",
             "financial_knowledge", "finance_lit_basic",
             "feb_50_1","feb_50_2","feb_50_10","feb_50_20",
             "money_set_aside", "private_saving_for_child","seker_sms","feb_1","feb_2",
             "trust_nii","trust_government","program_will_exist_10y","program_will_exist_25y","decrease_child_private_saving")


cda_seker_new_match_feb_big <- cda_seker_big
cda_seker_new_match_feb_big <- cda_seker_new_match_feb_big[!is.na(cda_seker_new_match_feb_big$financial_knowledge),]
cda_seker_new_match_feb_big <- cda_seker_new_match_feb_big[!is.na(cda_seker_new_match_feb_big$finance_lit_basic),]
cda_seker_new_match_feb_big <- cda_seker_new_match_feb_big[,my_vars1]
cda_seker_new_match_feb_big <- na.omit(cda_seker_new_match_feb_big)
save(cda_seker_new_match_feb_big,file="cda_seker_new_match_feb_big")
