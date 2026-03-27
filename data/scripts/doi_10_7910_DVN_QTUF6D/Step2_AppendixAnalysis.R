#######
#######
####### Replication files for Do Women Officers Police Differently? Evidence from Traffic Stops
####### This file runs most of the supplemental regressions shown in the appendix.
####### Last Updated: Jan. 2021
#######
#######

# Opening up those libraries:
library(dplyr)
library(ggplot2)
library(texreg)
library(readr)
library(pscl)
library(arm)

# Setting the working directory:
setwd("~/Desktop/PinkPolicing/AJPS_ReplicationFiles")

#
# Appendix: Alternative Specifications
#

# Clearing the workspace.
rm(list = ls())

# Loading in the Data
load("Data/FloridaSmall.RData")
load("Data/FL_Aggregated.RData")

# FE for Officer 
fl.search = lmer(search_occur~factor(race_gender)+
                   subject_age+out_of_state+
                   investigatory+
                   factor(of_gender)+factor(of_race)+
                   officer_years_of_service+officer_age+
                   factor(hour_of_day)+factor(month)+factor(year)+
                   factor(county_name)+(1|officer_id_hash),
                 data=fl.sm,  
                 subset=fl.sm$county_include==1&fl.sm$officer_exclude==0)
save(fl.search,file="Data/FLSearch_OLS_FE.RData")
fl.contra = lmer(contra~factor(race_gender)+
                   subject_age+out_of_state+
                   investigatory+
                   factor(of_gender)+factor(of_race)+
                   officer_years_of_service+officer_age+
                   factor(hour_of_day)+factor(month)+factor(year)+factor(county_name)+
                   (1|officer_id_hash),
                 data=fl.sm,  
                 subset=fl.sm$county_include==1&
                   fl.sm$search_occur==1&
                   fl.sm$officer_exclude==0)
save(fl.contra,file="Data/FlContra_OLS_FE.RData")
contra.search.rate.reg = lmer(contra.search.rate ~ factor(of_gender) + factor(of_exper) + 
                                factor(of_age) +factor(of_race) +
                                factor(race_gender) + factor(driver_age)+ 
                                investigatory + out_of_state +
                                factor(year)+factor(tod)+
                                (1|officer_id),
                              data=fl.ag.officers,
                              subset=fl.ag.officers$search_occur>0)
save(contra.search.rate.reg,file="Data/FlSearchRate_OLS_FE.RData")
contra.stop.rate.reg = lmer(contra.stop.rate ~ factor(of_gender) + factor(of_exper) + 
                              factor(of_age) + factor(of_race) +
                              factor(race_gender) + factor(driver_age)+ 
                              investigatory + out_of_state +
                              factor(year)+factor(tod)+(1|officer_id),
                            data=fl.ag.officers)
save(contra.stop.rate.reg,file="Data/FlStopRate_OLS_FE.RData")

# Logistc Regressions
rm(list = ls())

load("Data/NorthCarolina.RData")
load("Data/FloridaSmall.RData")

fl.search = glm(search_occur~factor(race_gender)+
                  subject_age+out_of_state+
                  investigatory+
                  factor(of_gender)+factor(of_race)+
                  officer_years_of_service+officer_age+
                  factor(hour_of_day)+factor(month)+factor(year)+
                  factor(county_name),
                data=fl.sm,family="binomial",
                subset=fl.sm$county_include==1&fl.sm$officer_exclude==0)
save(fl.search,file="Data/FLSearch_Logit.RData")
nc.search = glm(search~factor(race_gender)+subject_age+
                  investigatory+
                  factor(of_race)+
                  factor(of_gender)+Officer_Years_of_Service+
                  factor(month)+factor(year)+
                  factor(CMPD_Division),
                family="binomial",
                data=nc)
save(nc.search,file="Data/NCSearch_Logit.RData")
fl.contra = glm(contra~factor(race_gender)+
                  subject_age+out_of_state+
                  investigatory+
                  factor(of_gender)+factor(of_race)+
                  officer_years_of_service+officer_age+
                  factor(hour_of_day)+factor(month)+factor(year)+
                  factor(county_name),
                data=fl.sm, family = "binomial",
                subset=fl.sm$county_include==1&
                  fl.sm$search_occur==1&
                  fl.sm$officer_exclude==0)
save(fl.contra,file="Data/FlContra_Logit.RData")


#
# Appendix: Interaction Models
#

rm(list = ls())

load("Data/NorthCarolina.RData")
load("Data/FloridaSmall.RData")
load("Data/FloridaLarge.RData")
load("Data/FL_Aggregated.RData")


# Experience
fl.search.exper = lm(search_occur~factor(race_gender)+
                       subject_age+out_of_state+
                       investigatory+factor(of_race)+
                       factor(of_gender)*officer_years_of_service+officer_age+
                       factor(hour_of_day)+factor(month)+factor(year)+
                       factor(county_name),
                     data=fl.sm,  
                     subset=fl.sm$county_include==1&fl.sm$officer_exclude==0)
save(fl.search.exper,file="Data/FLSearch_Exper_OLS.RData")
nc.search.exper = lm(search~factor(race_gender)+subject_age+
                       investigatory+factor(of_race)+
                       factor(of_gender)*Officer_Years_of_Service+
                       factor(month)+factor(year)+
                       factor(CMPD_Division),
                     data=nc)
save(nc.search.exper,file="Data/NCSearch_Exper_OLS.RData")
fl.contra.exper = lm(contra~factor(race_gender)+
                       subject_age+out_of_state+
                       investigatory+factor(of_gender)*officer_years_of_service+
                       factor(of_race)+officer_age+
                       factor(hour_of_day)+factor(month)+factor(year)+
                       factor(county_name),
                     data=fl.sm,  
                     subset=fl.sm$county_include==1&
                       fl.sm$search_occur==1&
                       fl.sm$officer_exclude==0)
save(fl.contra.exper,file="Data/FlContra_Exper_OLS.RData")
contra.search.rate.exper = lm(contra.search.rate ~ factor(of_gender)*factor(of_exper) + 
                                investigatory+factor(of_age) +factor(of_race) +
                                factor(race_gender) + factor(driver_age)+ 
                                out_of_state +
                                factor(year),
                              data=fl.ag.officers,
                              subset=fl.ag.officers$search_occur>0)
save(contra.search.rate.exper,file="Data/FlSearchRate_Exper_OLS.RData")
contra.stop.rate.exper = lm(contra.stop.rate ~ factor(of_gender)*factor(of_exper) + 
                              investigatory+
                              factor(of_age) +factor(of_race) +
                              factor(race_gender) + factor(driver_age)+ 
                              out_of_state +
                              factor(year),
                            data=fl.ag.officers)
save(contra.stop.rate.exper,file="Data/FlStopRate_Exper_OLS.RData")

# Prop Female
fl$male.officer = ifelse(fl$of_gender==1,0,1)
fl.ag = aggregate(fl$officer_id_hash,
                  by=list(fl$of_gender,fl$county_name,fl$year),
                  function(x){length(unique(x))})
fl.ag.m = fl.ag[fl.ag$Group.1==0,]
fl.ag.f = fl.ag[fl.ag$Group.1==1,]
colnames(fl.ag.m)=c("male","county_name","year","male.count")
colnames(fl.ag.f)=c("female","county_name","year","female.count")
fl.ag = merge(fl.ag.m,fl.ag.f,all=T)
fl.ag$male.count[is.na(fl.ag$male.count)] = 0
fl.ag$female.count[is.na(fl.ag$female.count)] = 0
fl.ag$female.prop = fl.ag$female.count/(fl.ag$female.count+fl.ag$male.count)
summary(fl.ag$female.prop)
fl.sm = merge(fl.sm,fl.ag)
fl.search.prop = lm(search_occur~factor(race_gender)+
                      subject_age+out_of_state+
                      investigatory+factor(of_race)+
                      factor(of_gender)*female.prop+officer_years_of_service+officer_age+
                      factor(hour_of_day)+factor(month)+factor(year)+
                      factor(county_name),
                    data=fl.sm,  
                    subset=fl.sm$county_include==1&fl.sm$officer_exclude==0)
save(fl.search.prop,file="Data/FLSearch_Prop_OLS.RData")
fl.contra.prop = lm(contra~factor(race_gender)+
                      subject_age+out_of_state+
                      investigatory+factor(of_gender)*female.prop+
                      officer_years_of_service+
                      factor(of_race)+officer_age+
                      factor(hour_of_day)+factor(month)+factor(year)+
                      factor(county_name),
                    data=fl.sm,  
                    subset=fl.sm$county_include==1&
                      fl.sm$search_occur==1&
                      fl.sm$officer_exclude==0)
save(fl.contra.prop,file="Data/FlContra_Prop_OLS.RData")

# Stop Type
fl.search.st = lm(search_occur~factor(race_gender)+
                    subject_age+out_of_state+
                    factor(of_gender)+factor(of_race)+
                    officer_years_of_service+officer_age+
                    factor(hour_of_day)+factor(month)+factor(year)+
                    factor(county_name),
                  data=fl.sm,  
                  subset=fl.sm$county_include==1&fl.sm$officer_exclude==0&
                    fl.sm$investigatory==1)
save(fl.search.st,file="Data/FLSearch_StopType_OLS.RData")
nc.search.st = lm(search~factor(race_gender)+subject_age+
                    factor(of_gender)+
                    factor(of_race)+Officer_Years_of_Service+
                    factor(month)+factor(year)+
                    factor(CMPD_Division),
                  data=nc,
                  subset = nc$investigatory==1)
save(nc.search.st,file="Data/NCSearch_StopType_OLS.RData")
fl.contra.st = lm(contra~factor(race_gender)+
                    subject_age+out_of_state+
                    factor(of_gender)+
                    factor(of_race)+
                    officer_years_of_service+officer_age+
                    factor(hour_of_day)+factor(month)+factor(year)+
                    factor(county_name),
                  data=fl.sm,  
                  subset=fl.sm$county_include==1&
                    fl.sm$search_occur==1&
                    fl.sm$officer_exclude==0&
                    fl.sm$investigatory==1)
save(fl.contra.st,file="Data/FlContra_StopType_OLS.RData")
contra.search.rate.st = lm(contra.search.rate ~ factor(of_gender)+ 
                             factor(of_exper) + 
                             factor(of_age) +factor(of_race) +
                             factor(race_gender) + factor(driver_age)+ 
                             out_of_state +
                             factor(year),
                           data=fl.ag.officers,
                           subset=fl.ag.officers$search_occur>0&
                             fl.ag.officers$investigatory==1)
save(contra.search.rate.st,file="Data/FlSearchRate_StopType_OLS.RData")
contra.stop.rate.st = lm(contra.stop.rate ~ factor(of_gender)+ 
                           factor(of_exper) + 
                           factor(of_age) +factor(of_race) +
                           factor(race_gender) + factor(driver_age)+ 
                           out_of_state +
                           factor(year),
                         data=fl.ag.officers,
                         subset=fl.ag.officers$investigatory==1)
save(contra.stop.rate.st,file="Data/FlStopRate_StopType_OLS.RData")

# Driver Characteristics
fl.sm$subject_female = ifelse(fl.sm$subject_sex=="female",1,0)
fl.sm$subject_race2 = ifelse(fl.sm$subject_race=="white",0,
                             ifelse(fl.sm$subject_race=="black",1,2))
fl.search.inter = lm(search_occur~factor(of_gender)*factor(subject_female)+
                       factor(of_race)*factor(subject_race2)+
                       subject_age+out_of_state+investigatory+
                       officer_years_of_service+officer_age+
                       factor(hour_of_day)+factor(month)+factor(year)+
                       factor(county_name),
                     data=fl.sm,  
                     subset=fl.sm$county_include==1&
                       fl.sm$officer_exclude==0&
                       as.numeric(fl.sm$of_race)<3)
save(fl.search.inter,file="Data/FLInter_Search.RData")
fl.contra.inter = lm(contra~factor(of_gender)*factor(subject_female)+
                       factor(of_race)*factor(subject_race2)+
                       subject_age+out_of_state+investigatory+
                       officer_years_of_service+officer_age+
                       factor(hour_of_day)+factor(month)+factor(year)+
                       factor(county_name),
                     data=fl.sm,  
                     subset=fl.sm$search_occur==1&
                       fl.sm$county_include==1&
                       fl.sm$officer_exclude==0&
                       as.numeric(fl.sm$of_race)<3)
save(fl.contra.inter,file="Data/FLInter_Contra.RData")
fl.ag.officers$subject_female = ifelse(fl.ag.officers$race_gender%in%c(1,3,5),1,0)
fl.ag.officers$subject_race2 = ifelse(fl.ag.officers$race_gender%in%c(0,1),0,
                                      ifelse(fl.ag.officers$race_gender%in%c(2,3),1,2))
contra.search.rate.inter = lm(contra.search.rate ~ factor(of_gender)*factor(subject_female) + 
                                factor(of_race) * factor(subject_race2)+
                                factor(of_exper) + factor(of_age) +
                                factor(race_gender) + factor(driver_age)+ 
                                investigatory + out_of_state +
                                factor(year),
                              data=fl.ag.officers,
                              subset=fl.ag.officers$search_occur>0)
save(contra.search.rate.inter,file="Data/FlSearchRate_Inter_OLS.RData")
contra.stop.rate.inter = lm(contra.stop.rate ~ factor(of_gender)*factor(subject_female) + 
                              factor(of_race) * factor(subject_race2)+
                              factor(of_exper) + factor(of_age) +
                              factor(race_gender) + factor(driver_age)+ 
                              investigatory + out_of_state +
                              factor(year),
                            data=fl.ag.officers)
save(contra.stop.rate.inter,file="Data/FlStopRate_Inter_OLS.RData")

nc$of_race = ifelse(nc$Officer_Race=="White",0,
                    ifelse(nc$Officer_Race=="Black/African American",1,
                           ifelse(nc$Officer_Race=="Hispanic/Latino",2,NA)))
nc$subject_female = ifelse(nc$Driver_Gender=="Female",1,0)
nc$subject_race2 = ifelse(nc$Driver_Race=="White"&
                            nc$Driver_Ethnicity=="Non-Hispanic",0,
                          ifelse(nc$Driver_Race=="Black"&
                                   nc$Driver_Ethnicity=="Non-Hispanic",1,
                                 ifelse(nc$Driver_Ethnicity=="Hispanic",2,NA)))
nc.search.inter = lm(search~factor(of_gender)*factor(subject_female)+
                       factor(of_race)*factor(subject_race2)+
                       subject_age+investigatory+
                       Officer_Years_of_Service+
                       factor(month)+factor(year)+
                       factor(CMPD_Division),
                     data=nc)
save(nc.search.inter,file = "Data/NCInter_Search.RData")