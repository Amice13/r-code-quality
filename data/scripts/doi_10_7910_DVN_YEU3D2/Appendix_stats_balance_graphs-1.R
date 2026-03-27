#########on all sample
gc()
rm(list=ls())

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
library(Hmisc)
library(rmarkdown)
library(dplyr)
library(lubridate)
library(weights)
library(cobalt)
library(survival)

setwd("w:/incoming/maya_hisachon_chiild")

load("total_before_prartition") ###raw data
load("feb_nearest_main_t_data") ####matched data


total_child <- total_before_prartition[total_before_prartition$gill_child<16,]
total_before_prartition <- total_child[total_child$num_child_in_family==1,]
# 
total_before_prartition$ex_50_nis_19 <- ifelse(total_before_prartition$ex_50_nis==1 & total_before_prartition$feb_19==1,1,0)
total_before_prartition$choose_clean_20 <-ifelse(total_before_prartition$choose_new==1 & total_before_prartition$feb_19==0 & total_before_prartition$choose_date_date1>"2017-02-06"  ,1,0 )

# #digital
total_before_prartition$k_digital <-ifelse(total_before_prartition$device%in%c("DevicePhone","DeviceComputer","DeviceComputer","DeviceTV","DeviceTablet"),1,0)
total_before_prartition$device_d_unknown <- ifelse(total_before_prartition$device=="" | total_before_prartition$device=="DeviceUnknown",1,0)

total_before_prartition$choose_invest <- ifelse(total_before_prartition$kod_choice%in%c(120,121,130,131,110,111,140,141,210,211,220,230,231),1,0)
total_before_prartition$choose_risk <- ifelse(total_before_prartition$kod_choice%in%c(120,121,130,131),1,0)

feb_nearest_main_t_data$choose_invest <- ifelse(feb_nearest_main_t_data$kod_choice%in%c(120,121,130,131,110,111,140,141,210,211,220,230,231),1,0)
feb_nearest_main_t_data$choose_risk <- ifelse(feb_nearest_main_t_data$kod_choice%in%c(120,121,130,131),1,0)
feb_nearest_main_t_data$investment_fund_risk_19 <- ifelse(feb_nearest_main_t_data$kod_choice%in%c(120,121,131,130) & feb_nearest_main_t_data$feb_19==1,1,0)
feb_nearest_main_t_data$choose_invest_19 <- ifelse(feb_nearest_main_t_data$choose_invest==1 & feb_nearest_main_t_data$feb_19==1,1,0)


feb_nearest_main_t_data$choose_bank <- ifelse(feb_nearest_main_t_data$kod_choice%in%c(210,211,220,230,231),1,0)
feb_nearest_main_t_data$choose_bank_19 <- ifelse(feb_nearest_main_t_data$choose_bank==1 & feb_nearest_main_t_data$feb_19==1,1,0)


feb_nearest_main_t_data$choose_clean <- feb_nearest_main_t_data$choose_new
feb_nearest_main_t_data$choose_clean <- ifelse(feb_nearest_main_t_data$kod_choice%in%c(310,410),0,feb_nearest_main_t_data$choose_clean)
feb_nearest_main_t_data$kosher <- ifelse(feb_nearest_main_t_data$kod_choice%in%c(140,141),1,0)
feb_nearest_main_t_data$choose_clean_19 <- ifelse(feb_nearest_main_t_data$kod_choice%in%c(310,410) & feb_nearest_main_t_data$feb_19==1,0,feb_nearest_main_t_data$choose_clean)
feb_nearest_main_t_data$kosher_19 <- ifelse(feb_nearest_main_t_data$kod_choice%in%c(140,141)& feb_nearest_main_t_data$feb_19==1,1,0)

total_before_prartition$choose_clean <- total_before_prartition$choose_new
total_before_prartition$choose_clean <- ifelse(total_before_prartition$kod_choice%in%c(310,410),0,total_before_prartition$choose_clean)
total_before_prartition$kosher <- ifelse(total_before_prartition$kod_choice%in%c(140,141),1,0)
total_before_prartition$choose_clean_19 <- ifelse(total_before_prartition$kod_choice%in%c(310,410) & total_before_prartition$feb_19==1,0,total_before_prartition$choose_clean)
total_before_prartition$kosher_19 <- ifelse(total_before_prartition$kod_choice%in%c(140,141)& total_before_prartition$feb_19==1,1,0)


total_before_prartition$choose_invest <- ifelse(total_before_prartition$kod_choice%in%c(120,121,130,131,110,111,140,141,210,211,220,230,231),1,0)
total_before_prartition$choose_risk <- ifelse(total_before_prartition$kod_choice%in%c(120,121,130,131),1,0)
total_before_prartition$investment_fund_risk_19 <- ifelse(total_before_prartition$kod_choice%in%c(120,121,131,130) & total_before_prartition$feb_19==1,1,0)
total_before_prartition$choose_invest_19 <- ifelse(total_before_prartition$choose_invest==1 & total_before_prartition$feb_19==1,1,0)


total_before_prartition$income= 1000*(total_before_prartition$father_wage+total_before_prartition$mother_wage)
feb_nearest_main_t_data$income= 1000*(feb_nearest_main_t_data$father_wage+feb_nearest_main_t_data$mother_wage)


###phone
feb_nearest_main_t_data$phone <-ifelse(feb_nearest_main_t_data$device=="DevicePhone",1,0)
feb_nearest_main_t_data$phone_19 <-ifelse(feb_nearest_main_t_data$phone==1 & feb_nearest_main_t_data$feb_19==1,1,0)

total_before_prartition$phone <-ifelse(total_before_prartition$device=="DevicePhone",1,0)
total_before_prartition$phone_19 <-ifelse(total_before_prartition$phone==1 & total_before_prartition$feb_19==1,1,0)


# ##graphs - made chioce by dates
Sys.setlocale(category="LC_ALL",locale="english")
hist(total_before_prartition$choose_date_date, breaks = "days",col="blue",labels=FALSE, main="Histogram of all choices
     Blue all choices, Green for those who received an SMS",xlab="Choice Date") 
total_before_prartition$sms_feb_1 <- ifelse(total_before_prartition$sms_feb==1 & total_before_prartition$choose_date_date>"2017-02-05",1,0) 
hist(total_before_prartition$choose_date_date[total_before_prartition$sms_feb_1>0  ], breaks = "days",col=rgb(0,1,0,0.5), add=T, axes=FALSE)
#graphs no legend 
Sys.setlocale(category="LC_ALL",locale="english")
hist(total_before_prartition$choose_date_date, breaks = "days",col="blue",labels=FALSE,main="",xlab="Choice Date") 
total_before_prartition$sms_feb_1 <- ifelse(total_before_prartition$sms_feb==1 & total_before_prartition$choose_date_date>"2017-02-05",1,0) 
hist(total_before_prartition$choose_date_date[total_before_prartition$sms_feb_1>0  ], breaks = "days",col=rgb(0,1,0,0.5), add=T, axes=FALSE)
legend("top",      c("All choices","Choices of text message receivers"),fill=c("blue","green"))


##ultra-orthodox population/arab
hist(total_before_prartition$choose_date_date, breaks = "days",col="blue",labels=FALSE,main="",xlab="Choice Date")
hist(total_before_prartition$choose_date_date[total_before_prartition$parents_arab>0  ], breaks = "days",col=rgb(0,1,0,0.5), add=T, axes=FALSE)
legend("top",      c("All choices","Arabs"),fill=c("blue","green"))
hist(total_before_prartition$choose_date_date, breaks = "days",col="blue",labels=FALSE,main="",xlab="Choice Date")
hist(total_before_prartition$choose_date_date[total_before_prartition$parents_hardi>0  ], breaks = "days",col=rgb(0,1,0,0.5), add=T, axes=FALSE)
legend("top",      c("All choices","Ultra-Orthodox Jews"),fill=c("blue","green"))



total_before_prartition_digital_checks <- total_before_prartition[choose_date_date>"2017-02-01"]
total_before_prartition_digital_checks$phone <-ifelse(total_before_prartition_digital_checks$device=="DevicePhone",1,0)
hist(total_before_prartition_digital_checks$choose_date_date[total_before_prartition_digital_checks$phone>0], breaks = "days",col="blue",labels=FALSE, main="",xlab="Choice Date",angle="45") 
total_before_prartition_digital_checks$phone_hist_1 <- ifelse(total_before_prartition_digital_checks$sms_feb_1==1 & total_before_prartition_digital_checks$phone==1,1,0) 
hist(total_before_prartition_digital_checks$choose_date_date[total_before_prartition_digital_checks$phone_hist_1>0  ], breaks = "days",col=rgb(0,1,0,0.5), add=T, axes=FALSE)
legend("top",      c("All choices","Choices of text message receivers"),fill=c("blue","green"))


total_before_prartition_digital_checks$computer <-ifelse(total_before_prartition_digital_checks$device=="DeviceComputer",1,0)
hist(total_before_prartition_digital_checks$choose_date_date[total_before_prartition_digital_checks$computer>0], breaks = "days",col="blue",labels=FALSE, main="",xlab="Choice Date",angle="45") 
total_before_prartition_digital_checks$coputer_hist_1 <- ifelse(total_before_prartition_digital_checks$sms_feb_1==1 & total_before_prartition_digital_checks$computer==1,1,0) 
hist(total_before_prartition_digital_checks$choose_date_date[total_before_prartition_digital_checks$coputer_hist_1>0  ], breaks = "days",col=rgb(0,1,0,0.5), add=T, axes=FALSE)
legend("top",      c("All choices","Choices of text message receivers"),fill=c("blue","green"))


total_before_prartition_digital_checks$digital <-ifelse(total_before_prartition_digital_checks$device%in%c("DevicePhone","DeviceComputer","DeviceComputer","DeviceTV","DeviceTablet"),1,0)
hist(total_before_prartition_digital_checks$choose_date_date[total_before_prartition_digital_checks$digital>0], breaks = "days",col="blue",labels=FALSE, main="",xlab="Choice Date",angle="45") 
total_before_prartition_digital_checks$digital_hist_1 <- ifelse(total_before_prartition_digital_checks$sms_feb_1==1 & total_before_prartition_digital_checks$digital==1,1,0) 
hist(total_before_prartition_digital_checks$choose_date_date[total_before_prartition_digital_checks$digital_hist_1>0  ], breaks = "days",col=rgb(0,1,0,0.5), add=T, axes=FALSE)
legend("top",      c("All choices","Choices of text message receivers"),fill=c("blue","green"))


total_before_prartition_digital_checks$no_digital <-ifelse(total_before_prartition_digital_checks$device%in%c("DeviceUnknown"),1,0)
hist(total_before_prartition_digital_checks$choose_date_date[total_before_prartition_digital_checks$no_digital>0], breaks = "days",col="blue",labels=FALSE, main="",xlab="Choice Date",angle="45") 
total_before_prartition_digital_checks$no_digital_hist_1 <- ifelse(total_before_prartition_digital_checks$sms_feb_1==1 & total_before_prartition_digital_checks$no_digital==1,1,0) 
hist(total_before_prartition_digital_checks$choose_date_date[total_before_prartition_digital_checks$no_digital_hist_1>0  ], breaks = "days",col=rgb(0,1,0,0.5), add=T, axes=FALSE)
legend("top",      c("All choices","Choices of text message receivers"),fill=c("blue","green"))


 
# ##################################################################
###Appendix tables
stargazer(feb_nearest_main_t_data,weights="weights",type="html",out="table_matched_data.htm")
stargazer(total_before_prartition,type="html",out="table_before_partition_data.htm")
stargazer(total_match_feb,type="html",out="table_before_match_matchdata.htm")


total_prat_feb_sms <- subset(total_before_prartition, total_before_prartition$sms_feb==1)
stargazer(total_prat_feb_sms,type="html",out="total_prat_feb_sms.htm")
total_prat_feb_nosms <- subset(total_before_prartition, total_before_prartition$sms_feb==0)
stargazer(total_prat_feb_nosms,type="html",out="total_prat_feb_nosms.htm")


total_match_feb_sms <- subset(feb_nearest_main_t_data, feb_nearest_main_t_data$sms_feb==1)
stargazer(total_match_feb_sms,weights="weights",type="html",out="total_match_feb_sms.htm")
total_match_feb_nosms <- subset(feb_nearest_main_t_data, feb_nearest_main_t_data$sms_feb==0)
stargazer(total_match_feb_nosms,weights="weights",type="html",out="total_match_feb_nosms.htm")


###
total_prat_before_feb_choose <- subset(total_before_prartition, total_before_prartition$choose_date_date1<"2017-02-01" &  total_before_prartition$choose_date_date1>"2016-01-01")
stargazer(total_prat_before_feb_choose,type="html",out="total_prat_before_feb_choose.htm")
total_prat_before_feb_arab_choose <- subset(total_prat_before_feb_choose, total_prat_before_feb_choose$parents_arab==1)
stargazer(total_prat_before_feb_arab_choose,type="html",out="total_prat_before_feb_choose_arab.htm")
total_prat_before_feb_choose_noarab_choose <- subset(total_prat_before_feb_choose, total_prat_before_feb_choose$parents_arab==0)
stargazer(total_prat_before_feb_choose_noarab_choose,type="html",out="total_prat_before_feb_choose_noarab.htm")

total_prat_before_feb <- subset(total_before_prartition, total_before_prartition$choose_date_date1<"2017-02-01")
stargazer(total_prat_before_feb,type="html",out="total_prat_before_feb")
total_prat_before_feb_arab <- subset(total_prat_before_feb, total_prat_before_feb$parents_arab==1)
stargazer(total_prat_before_feb_arab,type="html",out="total_prat_before_feb_arab.htm")
total_prat_before_feb_noarab <- subset(total_prat_before_feb, total_prat_before_feb$parents_arab==0)
stargazer(total_prat_before_feb_noarab,type="html",out="total_prat_before_feb_noarab.htm")

total_prat_before_feb_hardi <- subset(total_prat_before_feb, total_prat_before_feb$parents_hardi==1)
stargazer(total_prat_before_feb_hardi,type="html",out="total_prat_before_feb_hardi.htm")
total_prat_before_feb_nohardi <- subset(total_prat_before_feb, total_prat_before_feb$parents_hardi==0)
stargazer(total_prat_before_feb_nohardi,type="html",out="total_prat_before_feb_nohardi.htm")

total_prat_before_feb_nohardi_noarab <- subset(total_prat_before_feb, total_prat_before_feb$parents_hardi==0 &total_prat_before_feb$parents_arab==0)
stargazer(total_prat_before_feb_nohardi_noarab,type="html",out="total_prat_before_feb_nohardi_noarab.htm")

#####


feb_nearest_main_t_data_sms <- subset(feb_nearest_main_t_data, feb_nearest_main_t_data$sms_feb==1)
stargazer(feb_nearest_main_t_data_sms,type="html",weights="weights",out="feb_nearest_main_t_data_sms")
feb_nearest_main_t_data_nosms <- subset(feb_nearest_main_t_data,weights="weights", feb_nearest_main_t_data$sms_feb==0)
stargazer(feb_nearest_main_t_data_nosms,type="html",out="feb_nearest_main_t_data_nosms")


feb_nearest_main_t_data_arab <- subset(feb_nearest_main_t_data, feb_nearest_main_t_data$parents_arab==1)
stargazer(feb_nearest_main_t_data_arab,type="html",weights="weights",out="feb_nearest_main_t_data_arab")
feb_nearest_main_t_data_hardi <- subset(feb_nearest_main_t_data, feb_nearest_main_t_data$parents_hardi==1)
stargazer(feb_nearest_main_t_data_hardi,type="html",weights="weights",out="feb_nearest_main_t_data_hardi")

feb_nearest_main_t_data_arab_sms <- subset(feb_nearest_main_t_data_arab, feb_nearest_main_t_data_arab$sms_feb==1)
stargazer(feb_nearest_main_t_data_arab_sms,type="html",weights="weights",out="feb_nearest_main_t_data_arab_sms")
feb_nearest_main_t_data_arab_nosms <- subset(feb_nearest_main_t_data_arab,weights="weights", feb_nearest_main_t_data_arab$sms_feb==0)
stargazer(feb_nearest_main_t_data_arab_nosms,type="html",out="feb_nearest_main_t_data_arab_nosms")


load("cda_seker_new_match_feb_big")

cda_seker_new_match_feb_big_sms <- subset(cda_seker_new_match_feb_big, cda_seker_new_match_feb_big$sms_feb==1)
stargazer(cda_seker_new_match_feb_big_sms,type="html",weights="weights",out="cda_seker_new_match_feb_big_data_sms")
cda_seker_new_match_feb_big_nosms <- subset(cda_seker_new_match_feb_big,weights="weights", cda_seker_new_match_feb_big$sms_feb==0)
stargazer(cda_seker_new_match_feb_big_nosms,type="html",out="cda_seker_new_match_feb_big_nosms")


cda_seker_new_match_feb_big_arab <- subset(cda_seker_new_match_feb_big, cda_seker_new_match_feb_big$parents_arab==1)
stargazer(cda_seker_new_match_feb_big_arab,type="html",weights="weights",out="cda_seker_new_match_feb_big_arab")
cda_seker_new_match_feb_big_hardi <- subset(cda_seker_new_match_feb_big, cda_seker_new_match_feb_big$parents_hardi==1)
stargazer(cda_seker_new_match_feb_big_data_hardi,type="html",weights="weights",out="cda_seker_new_match_feb_big_hardi")

cda_seker_new_match_feb_big_arab_sms <- subset(cda_seker_new_match_feb_big_arab, feb_nearest_main_t_data_arab$sms_feb==1)
stargazer(cda_seker_new_match_feb_big_arab_sms,type="html",weights="weights",out="cda_seker_new_match_feb_big_arab_sms")
cda_seker_new_match_feb_big_arab_nosms <- subset(cda_seker_new_match_feb_big_arab,weights="weights", cda_seker_new_match_feb_big_arab$sms_feb==0)
stargazer(cda_seker_new_match_feb_big_arab_nosms,type="html",out="cda_seker_new_match_feb_big_arab_nosms")



cda_seker_new_match_feb_big_hardi_sms <- subset(cda_seker_new_match_feb_big_hardi, cda_seker_new_match_feb_big_hardi$sms_feb==1)
stargazer(cda_seker_new_match_feb_big_hardi_sms,type="html",weights="weights",out="cda_seker_new_match_feb_big_hardi_Sms")
cda_seker_new_match_feb_big_hardi_nosms <- subset(cda_seker_new_match_feb_big_hardi,weights="weights", cda_seker_new_match_feb_big_hardi$sms_feb==0)
stargazer(cda_seker_new_match_feb_big_hardi_nosms,type="html",out="cda_seker_new_match_feb_big_hardi_nosms")

###################

feb_nearest_main_t_data_sms <- subset(feb_nearest_main_t_data, feb_nearest_main_t_data$sms_feb==1)
stargazer(feb_nearest_main_t_data_sms,type="html",weights="weights",out="feb_nearest_main_t_data_sms")
feb_nearest_main_t_data_nosms <- subset(feb_nearest_main_t_data,weights="weights", feb_nearest_main_t_data$sms_feb==0)
stargazer(feb_nearest_main_t_data_nosms,type="html",out="feb_nearest_main_t_data_nosms")


feb_nearest_main_t_data_arab <- subset(feb_nearest_main_t_data, feb_nearest_main_t_data$parents_arab==1)
stargazer(feb_nearest_main_t_data_arab,type="html",weights="weights",out="feb_nearest_main_t_data_arab")
feb_nearest_main_t_data_hardi <- subset(feb_nearest_main_t_data, feb_nearest_main_t_data$parents_hardi==1)
stargazer(feb_nearest_main_t_data_hardi,type="html",weights="weights",out="feb_nearest_main_t_data_hardi")

feb_nearest_main_t_data_arab_sms <- subset(feb_nearest_main_t_data_arab, feb_nearest_main_t_data_arab$sms_feb==1)
stargazer(feb_nearest_main_t_data_arab_sms,type="html",weights="weights",out="feb_nearest_main_t_data_arab_sms")
feb_nearest_main_t_data_arab_nosms <- subset(feb_nearest_main_t_data_arab,weights="weights", feb_nearest_main_t_data_arab$sms_feb==0)
stargazer(feb_nearest_main_t_data_arab_nosms,type="html",out="feb_nearest_main_t_data_arab_nosms")




#####
stargazer(a_total_match_feb_main_data,weights="weights",out="a_stargazer_main_match_feb_data.htm")
a_total_match_feb_main_data <- subset(a_total_match_feb_main_data, a_total_match_feb_data$sms_feb==1)
stargazer(a_total_match_feb_main_data_sms,type="html",weights="weights",out="a_stargazer_main_match_feb_data_sms.htm")
a_total_match_feb_data_nosms <- subset(a_total_match_feb_main_data, a_total_match_feb_main_data$sms_feb==0)
stargazer(a_total_match_feb_main_data_nosms,type="html",weights="weights",out="a_stargazer_main_match_feb_data_nosms.htm")

stargazer(j_total_match_feb_main_data,weights="weights",out="j_stargazer_main_match_feb_data.htm")
j_total_match_feb_main_data <- subset(j_total_match_feb_main_data, j_total_match_feb_data$sms_feb==1)
stargazer(j_total_match_feb_main_datj_sms,type="html",weights="weights",out="j_stargazer_main_match_feb_datj_sms.htm")
j_total_match_feb_datj_nosms <- subset(j_total_match_feb_main_data, j_total_match_feb_main_data$sms_feb==0)
stargazer(j_total_match_feb_main_datj_nosms,type="html",weights="weights",out="j_stargazer_main_match_feb_datj_nosms.htm")

stargazer(h_total_match_feb_main_data,weights="weights",out="h_stargazer_main_match_feb_data.htm")
h_total_match_feb_main_data <- subset(h_total_match_feb_main_data, h_total_match_feb_data$sms_feb==1)
stargazer(h_total_match_feb_main_dath_sms,type="html",weights="weights",out="h_stargazer_main_match_feb_dath_sms.htm")
h_total_match_feb_dath_nosms <- subset(h_total_match_feb_main_data, h_total_match_feb_main_data$sms_feb==0)
stargazer(h_total_match_feb_main_dath_nosms,type="html",weights="weights",out="h_stargazer_main_match_feb_dath_nosms.htm")


stargazer(feb_nearest_main_t_data,weights="weights",out="feb_nearest_main_t_data.htm")
feb_nearest_main_t_data <- subset(feb_nearest_main_t_data, feb_nearest_main_t_data$sms_feb==1)
stargazer(feb_nearest_main_t_data_sms,type="html",weights="weights",out="feb_nearest_main_t_data_sms.htm")
feb_nearest_main_t_data_nosms <- subset(feb_nearest_main_t_data, feb_nearest_main_t_data$sms_feb==0)
stargazer(feb_nearest_main_t_data_nosms,type="html",weights="weights",out="feb_nearest_main_t_data_nosms.htm")

##correlation matrix

total_before_prartition_forstat_arab = total_before_prartition[total_before_prartition$parents_arab==1]
total_before_prartition_forstat_hardi = total_before_prartition[total_before_prartition$parents_hardi==1]

total_before_prartition_forstat <- subset(total_before_prartition,select=c(sms_feb,socio_econ_cluster,rural,periphery_cluster,
                                                         parents_num_children,child_male,
                                                         child_with_disability1,parents_avg_age,income,
                                                         father_wage,mother_work,mother_wage,
                                                         parents_hardi,parents_arab,parents_married,
                                                         gill_child,ex_50_nis_19,ex_50_nis,feb_19,feb_10,feb_1,phone))
check <- rcorr(as.matrix(total_before_prartition_forstat))
print(round(check$r,2))
print(round(check$P,2))


total_before_prartition_forstat_arab <- subset(total_before_prartition_forstat_arab,select=c(sms_feb,socio_econ_cluster,rural,periphery_cluster,
                                                                           parents_num_children,child_male,
                                                                           child_with_disability1,parents_avg_age,income,
                                                                           father_wage,mother_work,mother_wage,
                                                                           parents_hardi,parents_arab,parents_married,
                                                                           gill_child,ex_50_nis_19,ex_50_nis,feb_19,feb_10,feb_1,phone))
check <- rcorr(as.matrix(total_before_prartition_forstat_arab))
print(round(check$r,2))
print(round(check$P,2))



total_before_prartition_forstat_hardi <- subset(total_before_prartition_forstat_hardi,select=c(sms_feb,socio_econ_cluster,rural,periphery_cluster,
                                                         parents_num_children,child_male,
                                                         child_with_disability1,parents_avg_age,income,
                                                         father_wage,mother_work,mother_wage,
                                                         parents_hardi,parents_arab,parents_married,
                                                         gill_child,ex_50_nis,feb_19,feb_10,feb_1,phone))
check <- rcorr(as.matrix(total_before_prartition_forstat_hardi))
print(round(check$r,2))
print(round(check$P,2))

load("cda_seker_new_match_feb_big")
cda_seker_new_match_feb_big$income <- cda_seker_new_match_feb_big$mother_wage+cda_seker_new_match_feb_big$father_wage
cda_seker_new_match_feb_biga_forstat <- subset(cda_seker_new_match_feb_big,select=c(sms_feb,
                                                         parents_num_children,child_male,
                                                         parents_avg_age,income,
                                                         father_wage,mother_work,mother_wage,
                                                         parents_hardi,parents_arab,parents_married,
                                                         gill_child,financial_knowledge,finance_lit_basic,
                                                         money_set_aside,private_saving_for_child, trust_nii,trust_government, program_will_exist_10y,
                                                         program_will_exist_25y, money_set_aside,decrease_child_private_saving))
check2 <- rcorr(as.matrix(cda_seker_new_match_feb_biga_forstat))
print(round(check2$r,2))
print(round(check2$P,2))

load("feb_big_seker_data_1")
feb_big_seker_data_1$income <- feb_big_seker_data_1$mother_wage+feb_big_seker_data_1$father_wage
feb_big_seker_data_1_forstat <- subset(feb_big_seker_data_1,select=c(sms_feb,
                                                                                    parents_num_children,child_male,
                                                                                    parents_avg_age,income,
                                                                                    father_wage,mother_work,mother_wage,
                                                                                    parents_hardi,parents_arab,parents_married,
                                                                                    gill_child,financial_knowledge,finance_lit_basic,
                                                                                    money_set_aside,private_saving_for_child, trust_nii,trust_government, program_will_exist_10y,
                                                                                    program_will_exist_25y, money_set_aside,decrease_child_private_saving))

check2 <- rcorr(as.matrix(feb_big_seker_data_1_forstat))
print(round(check2$r,2))
print(round(check2$P,2))




load("feb_nearest_main_t_data")
feb_nearest_main_t_data$treat=ifelse(feb_nearest_main_t_data$weights==1,1,0)
summary(feb_nearest_main_t_data$treat)

feb_nearest_main_t_data1 <- subset(feb_nearest_main_t_data,select=c(sms_feb,socio_econ_cluster,rural,periphery_cluster,
                                                  parents_num_children,child_male,
                                                  child_with_disability1,parents_avg_age,
                                                  father_wage,mother_work,mother_wage,mother_academic,
                                                  parents_hardi,parents_arab,parents_married,
                                                  gill_child,weights,treat))


outm <-bal.tab(feb_nearest_main_t_data1,treat = feb_nearest_main_t_data1$treat,weights = feb_nearest_main_t_data1$weights,disp=c("means","sds"), stats=c("mean.diffs","variance.ration"),abs=TRUE, var.names=v,m.threshold=0.1,v.threshold = 2, binary="std")
outm
v <-data.frame(old=c("mother_academic","parents_num_children","parents_arab","parents_hardi","gill_child","mother_wage","father_wage",
"parents_married"),
               new=c("Mother academic","Number of children","Arab","Ultra-Orthodox jews","Age of child","Mother wage",
                  "Father wage","Parents Married"))

##love plot could not run on NII computers
data_treat<- subset(feb_nearest_main_t_data,feb_nearest_main_t_data$weights==1)
data_cont<- subset(feb_nearest_main_t_data,feb_nearest_main_t_data$weights!=1)

wtd.var(data_treat$parents_num_children,weights=data_treat$weights)
wtd.var(data_cont$parents_num_children,weights=data_cont$weights)

wtd.var(data_treat$child_male,weights=data_treat$weights)
wtd.var(data_cont$child_male,weights=data_cont$weights)

wtd.var(data_treat$child_with_disability1,weights=data_treat$weights)
wtd.var(data_cont$child_with_disability1,weights=data_cont$weights)

wtd.var(data_treat$father_wage,weights=data_treat$weights)
wtd.var(data_cont$father_wage,weights=data_cont$weights)

wtd.var(data_treat$mother_wage,weights=data_treat$weights)
wtd.var(data_cont$mother_wage,weights=data_cont$weights)

wtd.var(data_treat$parents_hardi,weights=data_treat$weights)
wtd.var(data_cont$parents_hardi,weights=data_cont$weights)

wtd.var(data_treat$parents_arab,weights=data_treat$weights)
wtd.var(data_cont$parents_arab,weights=data_cont$weights)

wtd.var(data_treat$parents_married,weights=data_treat$weights)
wtd.var(data_cont$parents_married,weights=data_cont$weights)

wtd.var(data_treat$gill_child,weights=data_treat$weights)
wtd.var(data_cont$gill_child,weights=data_cont$weights)

wtd.mean(data_treat$parents_num_children,weights=data_treat$weights)
wtd.mean(data_cont$parents_num_children,weights=data_cont$weights)

wtd.mean(data_treat$child_male,weights=data_treat$weights)
wtd.mean(data_cont$child_male,weights=data_cont$weights)

wtd.mean(data_treat$child_with_disability1,weights=data_treat$weights)
wtd.mean(data_cont$child_with_disability1,weights=data_cont$weights)

wtd.mean(data_treat$father_wage,weights=data_treat$weights)
wtd.mean(data_cont$father_wage,weights=data_cont$weights)

wtd.mean(data_treat$mother_wage,weights=data_treat$weights)
wtd.mean(data_cont$mother_wage,weights=data_cont$weights)

wtd.mean(data_treat$parents_hardi,weights=data_treat$weights)
wtd.mean(data_cont$parents_hardi,weights=data_cont$weights)

wtd.mean(data_treat$parents_arab,weights=data_treat$weights)
wtd.mean(data_cont$parents_arab,weights=data_cont$weights)

wtd.mean(data_treat$parents_married,weights=data_treat$weights)
wtd.mean(data_cont$parents_married,weights=data_cont$weights)

wtd.mean(data_treat$gill_child,weights=data_treat$weights)
wtd.mean(data_cont$gill_child,weights=data_cont$weights)


#####Survey graphs
# 
# ##graphs
table(cda_seker_before_patition$finance_lit_basic)
load("cda_seker_before_patition")
cda_seker_before_patition$financial_knowledge1 <-ifelse(cda_seker_before_patition$financial_knowledge%in%c(0,1),1,0)
cda_seker_before_patition$finance_lit_basic1 <-ifelse(cda_seker_before_patition$finance_lit_basic%in%c(0,1),1,0)
cda_seker_before_patition$financial_knowledge2 <-ifelse(cda_seker_before_patition$financial_knowledge%in%c(4,5),1,0)
cda_seker_before_patition$finance_lit_basic2 <-ifelse(cda_seker_before_patition$finance_lit_basic%in%c(3),1,0)


#graphs no legend 
Sys.setlocale(category="LC_ALL",locale="english")
hist(cda_seker_before_patition$choose_date_date, breaks = "days",col="blue",labels=FALSE,main="",xlab="Choice Date") 
cda_seker_before_patition$sms_feb_1 <- ifelse(cda_seker_before_patition$sms_feb==1 & cda_seker_before_patition$choose_date_date>"2017-02-05",1,0) 
hist(cda_seker_before_patition$choose_date_date[cda_seker_before_patition$sms_feb_1>0  ], breaks = "days",col=rgb(0,1,0,0.5), add=T, axes=FALSE) 
legend("top",      c("All choices","Choices of SMS receivers"),fill=c("blue","green"))


#
hist(cda_seker_before_patition$choose_date_date[cda_seker_before_patition$financial_knowledge1==1 ], breaks = "days",col="blue",labels=FALSE,main="",xlab="Choice Date") 
hist(cda_seker_before_patition$choose_date_date[cda_seker_before_patition$financial_knowledge1==0  ], breaks = "days",col=rgb(0,1,0,0.5), add=T, axes=FALSE)
legend("top",      c("Low subjective financial literacy","Other"),fill=c("blue","green"))

#
hist(cda_seker_before_patition$choose_date_date[cda_seker_before_patition$finance_lit_basic1==1 ], breaks = "days",col="blue",labels=FALSE,main="",xlab="Choice Date") 
hist(cda_seker_before_patition$choose_date_date[cda_seker_before_patition$finance_lit_basic1==0  ], breaks = "days",col=rgb(0,1,0,0.5), add=T, axes=FALSE)
legend("top",      c("Low objective financial literacy","Other"),fill=c("blue","green"))

#
hist(cda_seker_before_patition$choose_date_date[cda_seker_before_patition$financial_knowledge2==1 ], breaks = "days",col="blue",labels=FALSE,main="",xlab="Choice Date") 
hist(cda_seker_before_patition$choose_date_date[cda_seker_before_patition$financial_knowledge2==0  ], breaks = "days",col=rgb(0,1,0,0.5), add=T, axes=FALSE)
legend("top",      c("High subjective financial literacy","Other"),fill=c("blue","green"))

#
hist(cda_seker_before_patition$choose_date_date[cda_seker_before_patition$finance_lit_basic2==1 ], breaks = "days",col="blue",labels=FALSE,main="",xlab="Choice Date") 
hist(cda_seker_before_patition$choose_date_date[cda_seker_before_patition$finance_lit_basic2==0  ], breaks = "days",col=rgb(0,1,0,0.5), add=T, axes=FALSE)
legend("top",      c("High objective financial literacy","Other"),fill=c("blue","green"))

#
hist(cda_seker_before_patition$choose_date_date[cda_seker_before_patition$financial_knowledge2==1 ], breaks = "days",col="blue",labels=FALSE,main="",xlab="Choice Date") 
hist(cda_seker_before_patition$choose_date_date[cda_seker_before_patition$financial_knowledge1==1  ], breaks = "days",col=rgb(0,1,0,0.5), add=T, axes=FALSE)
legend("top",      c("High subjective financial literacy","Low subjective financial literacy"),fill=c("blue","green"))

#
hist(cda_seker_before_patition$choose_date_date[cda_seker_before_patition$finance_lit_basic2==1 ], breaks = "days",col="blue",labels=FALSE,main="",xlab="Choice Date") 
hist(cda_seker_before_patition$choose_date_date[cda_seker_before_patition$finance_lit_basic1==1  ], breaks = "days",col=rgb(0,1,0,0.5), add=T, axes=FALSE)
legend("top",      c("High objective financial literacy","Low objective financial literacy"),fill=c("blue","green"))

table(cda_seker_before_patition$choose_date_date,cda_seker_before_patition$financial_knowledge2==0)
stargazer(cda_seker_before_patition,out="seker_beforematching_stat.htm")
stargazer(cda_seker_before_patition[financial_knowledge2==1],out="seker_beforematching_stat_high_conf.htm")
stargazer(cda_seker_before_patition[financial_knowledge1==1],out="seker_beforematching_stat_low_conf.htm")
stargazer(cda_seker_before_patition[finance_lit_basic2==1],out="seker_beforematching_stat_high_lit.htm")
stargazer(cda_seker_before_patition[finance_lit_basic1==1],out="seker_beforematching_stat_low_lit.htm")
stargazer(cda_seker_before_patition[choose_new==1],out="seker_beforematching_choose.htm")
stargazer(cda_seker_before_patition[choose_new==0],out="seker_beforematching_nochoose.htm")

cda_seker_before_patition_arab=subset(cda_seker_before_patition,cda_seker_before_patition$parents_arab==1)
cda_seker_before_patition_noarab=subset(cda_seker_before_patition,cda_seker_before_patition$parents_arab==0)
cda_seker_before_patition_hardi=subset(cda_seker_before_patition,cda_seker_before_patition$parents_hardi==1)
cda_seker_before_patition_nohardi=subset(cda_seker_before_patition,cda_seker_before_patition$parents_hardi==0)
cda_seker_before_patition_nohardi_noarab=subset(cda_seker_before_patition,cda_seker_before_patition$parents_hardi==0 & cda_seker_before_patition$parents_arab==0 )

t.test(cda_seker_before_patition_arab$financial_knowledge,cda_seker_before_patition_noarab$financial_knowledge)
t.test(cda_seker_before_patition_arab$financial_knowledge,cda_seker_before_patition_nohardi_noarab$financial_knowledge)
t.test(cda_seker_before_patition_hardi$financial_knowledge,cda_seker_before_patition_nohardi$financial_knowledge)
t.test(cda_seker_before_patition_hardi$financial_knowledge,cda_seker_before_patition_nohardi_noarab$financial_knowledge)


t.test(cda_seker_before_patition_arab$finance_lit_basic,cda_seker_before_patition_noarab$finance_lit_basic)
t.test(cda_seker_before_patition_arab$finance_lit_basic,cda_seker_before_patition_nohardi_noarab$finance_lit_basic)
t.test(cda_seker_before_patition_hardi$finance_lit_basic,cda_seker_before_patition_nohardi$finance_lit_basic)
t.test(cda_seker_before_patition_hardi$finance_lit_basic,cda_seker_before_patition_nohardi_noarab$finance_lit_basic)

stargazer(cda_seker_before_patition_arab,out="cda_seker_before_patition_arab.htm")
stargazer(cda_seker_before_patition_noarab,out="cda_seker_before_patition_noarab.htm")
stargazer(cda_seker_before_patition_hardi,out="cda_seker_before_patition_hardi.htm")
stargazer(cda_seker_before_patition_nohardi,out="cda_seker_before_patition_nohardi.htm")
stargazer(cda_seker_before_patition_nohardi_noarab,out="cda_seker_before_patition_nohardi_noarab.htm")



prob_reg0=lm(financial_knowledge~parents_hardi +parents_arab+parents_num_children +mother_wage+father_wage+
                mother_academic+father_academic+parents_married+gill_child,
              data= cda_seker_before_patition)


stargazer(prob_reg0, title="financial literacy survey data",align=TRUE,
          type="html",
          order=c("parents_hardi","parents_arab","parents_num_children","gill_child","mother_wage","father_wage",
                  "mother_academic","father_academic","father_academic",
                  "parents_married"),
          covariate.labels = c("Parents Ultra-Orthodox","Parents Arab","Number of children", "Age of child", "Mother's wage","Father's wage",
                               "Mother academic", "Father academic", "Parents Married" ),
          out=" reg_subjective_fin_no_partition.htm",
          no.space=TRUE, digits=2)



prob_reg0=lm(finance_lit_basic~parents_hardi +parents_arab+parents_num_children +mother_wage+father_wage+
               mother_academic+father_academic+parents_married+gill_child,
             data= cda_seker_before_patition)


stargazer(prob_reg0, title="objective financial literacy survey data",align=TRUE,
          type="html",
          order=c("parents_hardi","parents_arab","parents_num_children","gill_child","mother_wage","father_wage",
                  "mother_academic","father_academic","father_academic",
                  "parents_married"),
          covariate.labels = c("Parents Ultra-Orthodox","Parents Arab","Number of children", "Age of child", "Mother's wage","Father's wage",
                               "Mother academic", "Father academic", "Parents Married" ),
          out=" reg_objective_fin_no_partition.htm",
          no.space=TRUE, digits=2)


#####
load("total_before_prartition_before_feb_arab_data")
prob_reg0=glm(choose_new~ parents_arab+parents_num_children +mother_wage+father_wage+
                mother_academic+father_academic+parents_married+gill_child,
              data= total_before_prartition_before_feb_arab_data,family=binomial(link="logit"),weights=weights)
nulllogit <- glm(choose_new~1,family=binomial(link="logit"),weights=weights, data= total_before_prartition_before_feb_arab_data)
mcfadden0 <- round(as.numeric(1-logLik(prob_reg0)/logLik(nulllogit)),digits=2)


stargazer(prob_reg0, title="Choose before February",align=TRUE,
          type="html",
          order=c("parents_arab","parents_num_children","gill_child","mother_wage","father_wage",
                  "mother_academic","father_academic","father_academic",
                  "parents_married"),
          covariate.labels = c("Parents Arab","Number of children", "Age of child", "Mother's wage","Father's wage",
                               "Mother academic", "Father academic", "Parents Married" ),
          out=" before_feb_arab.htm",
          no.space=TRUE, digits=2, add.lines=list(c("Mcfadden Pseudo R square",mcfadden0)))

####\
load("total_before_prartition_before_feb_hardi_data")
prob_reg0=glm(choose_new~ parents_hardi+parents_num_children +mother_wage+father_wage+
                mother_academic+father_academic+parents_married+gill_child,
              data= total_before_prartition_before_feb_hardi_data,family=binomial(link="logit"),weights=weights)
nulllogit <- glm(choose_new~1,family=binomial(link="logit"),weights=weights, data= total_before_prartition_before_feb_hardi_data)
mcfadden0 <- round(as.numeric(1-logLik(prob_reg0)/logLik(nulllogit)),digits=2)


stargazer(prob_reg0, title="Choose before February",align=TRUE,
          type="html",
          order=c("parents_hardi","parents_num_children","gill_child","mother_wage","father_wage",
                  "mother_academic","father_academic","father_academic",
                  "parents_married"),
          covariate.labels = c("Parents Ultra-Orthodox jews","Number of children", "Age of child", "Mother's wage","Father's wage",
                               "Mother academic", "Father academic", "Parents Married" ),
          out=" before_feb_hardi.htm",
          no.space=TRUE, digits=2, add.lines=list(c("Mcfadden Pseudo R square",mcfadden0)))


prob_reg0=glm(choose_new~ parents_hardi,
              data= total_before_prartition_before_feb_hardi_data,family=binomial(link="logit"),weights=weights)
nulllogit <- glm(choose_new~1,family=binomial(link="logit"),weights=weights, data= total_before_prartition_before_feb_hardi_data)
mcfadden0 <- round(as.numeric(1-logLik(prob_reg0)/logLik(nulllogit)),digits=2)


stargazer(prob_reg0, title="Choose before February",align=TRUE,
          type="html",
          order=c("parents_hardi"),
          covariate.labels = c("Parents Ultra-Orthodox jews" ),
          out=" before_feb_hardi.htm",
          no.space=TRUE, digits=2, add.lines=list(c("Mcfadden Pseudo R square",mcfadden0)))

prob_reg0=glm(choose_new~ parents_arab,
              data= total_before_prartition_before_feb_arab_data,family=binomial(link="logit"),weights=weights)
nulllogit <- glm(choose_new~1,family=binomial(link="logit"),weights=weights, data= total_before_prartition_before_feb_arab_data)
mcfadden0 <- round(as.numeric(1-logLik(prob_reg0)/logLik(nulllogit)),digits=2)


stargazer(prob_reg0, title="Choose before February",align=TRUE,
          type="html",
          order=c("parents_arab"),
          covariate.labels = c("Parents Arab"),
          out=" before_feb_arab.htm",
          no.space=TRUE, digits=2, add.lines=list(c("Mcfadden Pseudo R square",mcfadden0)))


########################

load("a_total_feb_main")
load("h_total_feb_main")
load("j_total_feb_main")
stargazer(a_total_feb_main,weights="weights",out="a_stargazer_a_total_feb_main.htm")
a_total_match_feb_main_data_sms <- subset(a_total_feb_main, a_total_feb_main$sms_feb==1)
stargazer(a_total_match_feb_main_data_sms,type="html",weights="weights",out="a_stargazer_a_total_match_feb_main_data_sms.htm")
a_total_match_feb_data_nosms <- subset(a_total_match_feb_main_data, a_total_match_feb_main_data$sms_feb==0)
stargazer(a_total_match_feb_main_data_nosms,type="html",weights="weights",out="a_stargazer_main_match_feb_data_nosms.htm")

stargazer(j_total_match_feb_main_data,weights="weights",out="j_stargazer_main_match_feb_data.htm")
j_total_match_feb_main_data <- subset(j_total_match_feb_main_data, j_total_match_feb_data$sms_feb==1)
stargazer(j_total_match_feb_main_data_sms,type="html",weights="weights",out="j_stargazer_main_match_feb_data_sms.htm")
j_total_match_feb_data_nosms <- subset(j_total_match_feb_main_data, j_total_match_feb_main_data$sms_feb==0)
stargazer(j_total_match_feb_main_data_nosms,type="html",weights="weights",out="j_stargazer_main_match_feb_data_nosms.htm")

stargazer(h_total_match_feb_main_data,weights="weights",out="h_stargazer_main_match_feb_data.htm")
h_total_match_feb_main_data <- subset(h_total_match_feb_main_data, h_total_match_feb_data$sms_feb==1)
stargazer(h_total_match_feb_main_dath_sms,type="html",weights="weights",out="h_stargazer_main_match_feb_dath_sms.htm")
h_total_match_feb_dath_nosms <- subset(h_total_match_feb_main_data, h_total_match_feb_main_data$sms_feb==0)
stargazer(h_total_match_feb_main_dath_nosms,type="html",weights="weights",out="h_stargazer_main_match_feb_dath_nosms.htm")


stargazer(feb_nearest_main_t_data,weights="weights",out="feb_nearest_main_t_data.htm")
feb_nearest_main_t_data <- subset(feb_nearest_main_t_data, feb_nearest_main_t_data$sms_feb==1)
stargazer(feb_nearest_main_t_data_sms,type="html",weights="weights",out="feb_nearest_main_t_data_sms")
feb_nearest_main_t_data_nosms <- subset(feb_nearest_main_t_data, feb_nearest_main_t_data$sms_feb==0)
stargazer(feb_nearest_main_t_data_nosms,type="html",weights="weights",out="feb_nearest_main_t_data_nosms")


############################

#############balance checks

load("feb_nearest_main_t_data")
load("feb_nearest_main_t_match")


summary(feb_nearest_main_t)
plot(feb_nearest_main_t,type="hist", standardize = TRUE)
v<- summary(feb_nearest_main_t, standardize = TRUE)
plot (v, var.orger="unmatched")

################

load("a_total_match_feb_main_match")
plot(a_total_match_feb_main,type="hist", standardize = TRUE)
v<- summary(a_total_match_feb_main, standardize = TRUE)
plot (v, var.orger="unmatched")
summary(a_total_match_feb_main)

load("h_total_match_feb_main_match")
plot(h_total_match_feb_main,type="hist", standardize = TRUE)
v<- summary(h_total_match_feb_main, standardize = TRUE)
plot (v, var.orger="unmatched")
summary(h_total_match_feb_main)


load("j_total_match_feb_main_match")
plot(j_total_match_feb_main,type="hist", standardize = TRUE)
v<- summary(j_total_match_feb_main, standardize = TRUE)
plot (v, var.orger="unmatched")
summary(j_total_match_feb_main)



#############################
#robust specifications

load("feb_nearest_noreplace_t_match")
load("a_total_match_feb_noreplace_match")
load("h_total_match_feb_noreplace_match")
load("j_total_match_feb_noreplace_match")
plot(feb_nearest_noreplace_t,type="hist", standardize = TRUE)
v<- summary(feb_nearest_noreplace_t, standardize = TRUE)
plot (v)
summary(feb_nearest_noreplace_t)


plot(a_total_match_feb_noreplace,type="hist", standardize = TRUE)
v<- summary(a_total_match_feb_noreplace, standardize = TRUE)
plot (v)
summary(a_total_match_feb_noreplace)

plot(h_total_match_feb_noreplace,type="hist", standardize = TRUE)
v<- summary(h_total_match_feb_noreplace, standardize = TRUE)
plot (v)
summary(h_total_match_feb_noreplace)
plot(j_total_match_feb_noreplace,type="hist", standardize = TRUE)
v<- summary(j_total_match_feb_noreplace, standardize = TRUE)
plot (v)
summary(j_total_match_feb_noreplace)


load("feb_nearest_caliper_t_match")
 load("a_total_match_feb_caliper_match")
 load("h_total_match_feb_caliper_match")
 load("j_total_match_feb_caliper_match")
 plot(feb_nearest_caliper_t,type="hist", standardize = TRUE)
 v<- summary(feb_nearest_caliper_t, standardize = TRUE)
 plot (v)
 summary(feb_nearest_caliper_t)
 
 
 plot(a_total_match_feb_caliper,type="hist", standardize = TRUE)
 v<- summary(a_total_match_feb_caliper, standardize = TRUE)
 plot (v)
 summary(a_total_match_feb_caliper)
 
 
 plot(h_total_match_feb_caliper,type="hist", standardize = TRUE)
 v<- summary(h_total_match_feb_caliper, standardize = TRUE)
 plot (v)
 summary(h_total_match_feb_caliper)
 
 
 
 plot(j_total_match_feb_caliper,type="hist", standardize = TRUE)
 v<- summary(j_total_match_feb_caliper, standardize = TRUE)
  plot (v)
  summary(j_total_match_feb_caliper)
 
 
  load("feb_nearest_ratio_t_match")
load("a_total_match_feb_ratio_match")
 load("h_total_match_feb_ratio_match")
 load("j_total_match_feb_ratio_match")
 load("feb_nearest_lamas_t_match")
 load("a_total_match_feb_lamas_match")
 load("h_total_match_feb_lamas_match")
 load("j_total_match_feb_lamas_match")
 plot(feb_nearest_ratio_t,type="hist", standardize = TRUE)
 v<- summary(feb_nearest_ratio_t, standardize = TRUE)
 plot (v)
 summary(feb_nearest_ratio_t)
 
  plot(a_total_match_feb_ratio,type="hist", standardize = TRUE)
  v<- summary(a_total_match_feb_ratio, standardize = TRUE)
  plot (v)
  summary(a_total_match_feb_ratio)
  
 plot(h_total_match_feb_ratio,type="hist", standardize = TRUE)
 v<- summary(h_total_match_feb_ratio, standardize = TRUE)
 plot (v)
 summary(h_total_match_feb_ratio)
 
 plot(j_total_match_feb_ratio,type="hist", standardize = TRUE)
 v<- summary(j_total_match_feb_ratio, standardize = TRUE)
 plot (v)
 summary(j_total_match_feb_ratio)
 
  plot(feb_nearest_lamas_t,type="hist", standardize = TRUE)
  v<- summary(feb_nearest_lamas_t, standardize = TRUE)
  plot (v)
  summary(feb_nearest_lamas_t)
  
   plot(a_total_match_feb_lamas,type="hist", standardize = TRUE)
   v<- summary(a_total_match_feb_lamas, standardize = TRUE)
   plot (v)
   summary(a_total_match_feb_lamas)
   
 plot(h_total_match_feb_lamas,type="hist", standardize = TRUE)
 v<- summary(h_total_match_feb_lamas, standardize = TRUE)
 plot (v)
 summary(h_total_match_feb_lamas)
 
 plot(j_total_match_feb_lamas,type="hist", standardize = TRUE)
 v<- summary(j_total_match_feb_lamas, standardize = TRUE)
 plot (v)
 summary(j_total_match_feb_lamas)
 
 ##seker


load("feb_big_seker_1_match")
plot(feb_big_seker_1,type="hist", standardize = TRUE)
v<- summary(feb_big_seker_1, standardize = TRUE)
plot (v)
summary(feb_big_seker_1)



#