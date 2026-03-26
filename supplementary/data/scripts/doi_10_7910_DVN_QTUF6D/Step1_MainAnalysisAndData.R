#######
#######
####### Replication files for Do Women Officers Police Differently? Evidence from Traffic Stops
####### This file cleans the raw data and runs the analysis for the body of the paper. 
####### Last Updated: Jan. 2021
#######
#######


###
### 1. Setting up the space. 
###

# Setting the working directory:
setwd("~/Desktop/PinkPolicing/AJPS_ReplicationFiles")

# Installing the needed libraries:
#install.packages("pscl",dependencies = T)
#install.packages("ggplot2",dependencies = T)
#install.packages("texreg",dependencies = T)
#install.packages("readr",dependencies = T)
#install.packages("arm",dependencies = T)
#install.packages("dplyr",dependencies = T)

# Opening up those libraries:
library(dplyr)
library(ggplot2)
library(texreg)
library(readr)
library(pscl)
library(arm)

# Loading the raw data:
nc_new = read_csv("Data/Officer_Traffic_Stops_Update.csv")
nc_old = read_csv("Data/Officer_Traffic_Stops_Original.csv")
nc = bind_rows(nc_new,nc_old)
fl = read_csv("Data/fl_statewide_2019_08_13.csv")


###
### 2. Producing the data sets for each table. 
###

# Cleaning the NC Data
nc$driver_re = as.numeric(ifelse(nc$Driver_Race=="White"&
                                   nc$Driver_Ethnicity=="Non-Hispanic","0",
                                 ifelse(nc$Driver_Race=="Black"&
                                          nc$Driver_Ethnicity=="Non-Hispanic","1",
                                        ifelse(nc$Driver_Ethnicity=="Hispanic","2",NA))))
nc$of_rg = ifelse(nc$Officer_Race=="White",
                  ifelse(nc$Officer_Gender=="Male","0","1"),
                  ifelse(nc$Officer_Race=="Black/African American",
                         ifelse(nc$Officer_Gender=="Male","2","3"),NA))
nc$of_race = ifelse(nc$Officer_Race=="White",0,
                    ifelse(nc$Officer_Race=="Black/African American",1,NA))
nc$of_gender = ifelse(nc$Officer_Gender=="Male","0","1")
nc$investigatory = ifelse(grepl("Impaired|Speeding|Light|Movement",
                                as.character(nc$Reason_for_Stop)),0,1)
nc$investigatory = ifelse(grepl("Check",as.character(nc$Reason_for_Stop)),
                          NA,nc$investigatory)
nc$race_gender = ifelse(nc$driver_re=="0",
                        ifelse(nc$Driver_Gender=="Male","0","1"),
                        ifelse(nc$driver_re=="1",
                               ifelse(nc$Driver_Gender=="Male","2","3"),NA))
nc$search = ifelse(nc$Was_a_Search_Conducted=="Yes",1,0)

nc$subject_sex = tolower(nc$Driver_Gender)
nc$subject_age = nc$Driver_Age
nc$officer_sex = tolower(nc$Officer_Gender)
nc$month = apply(as.matrix(as.character(nc$Month_of_Stop)),1,
                 function(x){strsplit(x,"/",fixed=T)[[1]][2]})
nc$year = apply(as.matrix(as.character(nc$Month_of_Stop)),1,
                function(x){strsplit(x,"/",fixed=T)[[1]][1]})

nc$arrest = ifelse(nc$Result_of_Stop=="Arrest",1,0)
save(nc,file="Data/NorthCarolina.RData")

# Cleaning the FL data.
violations_list = strsplit(paste(fl$reason_for_stop,collapse = "|"),"|",fixed = T)
violations_list_small = unique(violations_list[[1]])[2:71]
violations_indicator = violations_list_small[c(1,2,5,6,7,9,10,14,19,
                                               20,23,40,45)]
fl$investigatory = ifelse(is.na(fl$violation),NA,
                          ifelse(fl$violation %in% violations_indicator, 0, 1))
fl$contraband_found = ifelse(grepl("contraband",
                                   tolower(fl$violation)),1,0)
fl$race_gender = ifelse(fl$subject_race=="white",
                        ifelse(fl$subject_sex=="male",0,1),
                        ifelse(fl$subject_race=="black",
                               ifelse(fl$subject_sex=="male",2,3),
                               ifelse(fl$subject_race=="hispanic",
                                      ifelse(fl$subject_sex=="male",4,5),NA)))
fl$of_rg = ifelse(fl$officer_race=="white",
                  ifelse(fl$officer_sex=="male",0,1),
                  ifelse(fl$officer_race=="black",
                         ifelse(fl$officer_sex=="male",2,3),
                         ifelse(fl$officer_race=="hispanic",
                                ifelse(fl$officer_sex=="male",4,5),NA)))
fl$of_race = ifelse(fl$officer_race=="white",0,
                    ifelse(fl$officer_race=="black",1,
                           ifelse(fl$officer_race=="hispanic",2,
                                  ifelse(fl$officer_race=="asian/pacific islander",3,
                                         ifelse(fl$officer_race=="other",4,NA)))))
fl$of_gender = ifelse(fl$officer_sex=="male",0,1)
fl$out_of_state = ifelse(fl$vehicle_registration_state=="FL",0,1)
fl$hour_of_day = apply(as.matrix(as.character(fl$time)),1,
                       function(x)(strsplit(x,":",fixed = T)[[1]][1]))
fl$month = apply(as.matrix(as.character(fl$date)),1,
                 function(x)(paste(strsplit(x,"-",fixed = T)[[1]][2],
                                   collapse = "_")))
fl$year = apply(as.matrix(as.character(fl$date)),1,
                function(x)(paste(strsplit(x,"-",fixed = T)[[1]][1],
                                  collapse = "_")))
fl = subset(fl,fl$year!="2016"&fl$year!="2017"&fl$year!="2018") #Narrows down to complete years that don't report extreme misingness on key outcome. 
fl.officers = names(table(fl$officer_id_hash))[table(fl$officer_id_hash)>1000]
fl$officers_include = ifelse(fl$officer_id_hash%in%fl.officers,1,0)
fl.counties = names(table(fl$county_name))[table(fl$county_name)>1000]
fl$county_include = ifelse(fl$county_name%in%fl.counties,1,0)
fl.ag.id = aggregate(fl$of_gender,
                     list(fl$officer_id_hash,fl$year,fl$county_name),
                     mean)
fl.ag.id$officer = ifelse(!is.na(fl.ag.id$x),1,0)
fl.ag.gender = aggregate(fl.ag.id[,c("x","officer")],
                         list(fl.ag.id$Group.2,fl.ag.id$Group.3),
                         sum,na.rm=T)
fl.ag.gender$prop.female = fl.ag.gender$x/fl.ag.gender$officer
colnames(fl.ag.gender) = c("year","county_name","count.female","tot.officer","prop.female")
fl = merge(fl,fl.ag.gender,by=c("year","county_name"),all.x=T)
fl$officer_exclude = ifelse(fl$officer_years_of_service<0|fl$officer_years_of_service>40,1,0)
fl.ag.id2 = aggregate(fl$of_gender,
                      list(fl$officer_id_hash),
                      mean)
fl$search_occur = ifelse(fl$search_conducted == 0, 0, 
                         ifelse(fl$search_basis != "other",1,NA))
fl$contra = ifelse(is.na(fl$search_occur),0,
                   ifelse(fl$search_occur==1,fl$contraband_found,0))

complete = complete.cases(fl[,c("search_occur","race_gender","subject_age",
                                "out_of_state","investigatory","of_gender",
                                "of_race","officer_years_of_service","officer_age",
                                "hour_of_day","month","year","county_name")])
fl.sm = fl[complete,]
complete2 = complete.cases(fl[,c("search_occur","of_gender")])
table(complete)
table(complete2)

fl.missingness = apply(fl[,c("search_occur","race_gender","subject_age",
                             "out_of_state","investigatory","of_gender",
                             "of_race","officer_years_of_service","officer_age",
                             "county_name")],
                       2,
                       FUN = function(x){table(is.na(x))})
save(fl,file="Data/FloridaLarge.RData")
save(fl.sm,file="Data/FloridaSmall.RData")

fl$stops = ifelse(!is.na(fl$search_occur),1,0)
fl$contra.ttest = ifelse(fl$search_occur==1,fl$contra,NA)
prop.test(table(fl$of_gender,fl$contra.ttest))
fl$of_exper = ifelse(fl$officer_years_of_service>=
                       mean(fl$officer_years_of_service,na.rm=T),1,0)
fl$of_age = ifelse(fl$officer_age<30,1,
                   ifelse(fl$officer_age>64,3,2))
fl$driver_age = ifelse(fl$subject_age<30,1,
                       ifelse(fl$subject_age>64,3,2))
fl$hour_of_day2 = as.numeric(fl$hour_of_day)
fl$tod = ifelse(fl$hour_of_day2<3,1,
                ifelse(fl$hour_of_day2<6,2,
                       ifelse(fl$hour_of_day2<9,3,
                              ifelse(fl$hour_of_day2<12,4,
                                     ifelse(fl$hour_of_day2<15,5,
                                            ifelse(fl$hour_of_day2<18,6,
                                                   ifelse(fl$hour_of_day2<21,7,8)))))))

fl.ag.officers = aggregate(fl[,c("stops","search_occur","contra")],
                           by=list(fl$officer_id_hash,
                                   fl$of_race,fl$of_gender,
                                   fl$of_exper,fl$of_age,
                                   fl$race_gender,fl$driver_age,
                                   fl$out_of_state,fl$investigatory,
                                   fl$year,fl$tod),
                           sum,na.rm=T)
colnames(fl.ag.officers) = c("officer_id","of_race","of_gender","of_exper",
                             "of_age","race_gender","driver_age",
                             "out_of_state","investigatory","year",
                             "tod","stops","search_occur","contra")
fl.ag.officers$contra.search.rate = (fl.ag.officers$contra/fl.ag.officers$search_occur)*10
fl.ag.officers$contra.stop.rate = (fl.ag.officers$contra/fl.ag.officers$stops)*100
save(fl.ag.officers,file="Data/FL_Aggregated.RData")

# Data for Figure 1
search.df = data.frame("Department" = c("CPD","CPD","FHP","FHP"),
                       "Gender" = c("Male","Female","Male","Female"),
                       "Rate" = c(prop.table(table(nc$of_gender,nc$search),1)[,2],
                                  prop.table(table(fl$of_gender[fl.sm$county_include==1&
                                                                  fl.sm$officer_exclude==0],
                                                   fl$search_occur[fl.sm$county_include==1&
                                                                     fl.sm$officer_exclude==0]),1)[,2]))
save(search.df,file="Data/Fig1_Data.RData")

###
### 3. Regressions
###

#
# For the Main Text:
#

# Search Regressions
fl.search.sm = lm(search_occur~factor(of_gender),data=fl)
save(fl.search.sm, file="Data/FLSearch_Sm_OLS.RData")
fl.search = lm(search_occur~factor(race_gender)+
                 subject_age+out_of_state+
                 investigatory+
                 factor(of_gender)+factor(of_race)+
                 officer_years_of_service+officer_age+
                 factor(hour_of_day)+factor(month)+factor(year)+
                 factor(county_name),
               data=fl.sm,  
               subset=fl.sm$county_include==1&fl.sm$officer_exclude==0)
save(fl.search,file="Data/FLSearch_OLS.RData")
nc.search.sm = lm(search~factor(of_gender),data = nc)
save(nc.search.sm,file="Data/NCSearch_Sm_OLS.RData")
nc.search = lm(search~factor(race_gender)+subject_age+
                 investigatory+
                 factor(of_race)+
                 factor(of_gender)+Officer_Years_of_Service+
                 factor(month)+factor(year)+
                 factor(CMPD_Division),
               data=nc)
save(nc.search,file="Data/NCSearch_OLS.RData")

# Contraband Regressions
fl.contra = lm(contra~factor(race_gender)+
                 subject_age+out_of_state+
                 investigatory+
                 factor(of_gender)+factor(of_race)+
                 officer_years_of_service+officer_age+
                 factor(hour_of_day)+factor(month)+factor(year)+
                 factor(county_name),
               data=fl.sm,  
               subset=fl.sm$county_include==1&
                 fl.sm$search_occur==1&
                 fl.sm$officer_exclude==0)
save(fl.contra,file="Data/FlContra_OLS.RData")
contra.search.rate.reg = lm(contra.search.rate ~ factor(of_gender) + factor(of_exper) + 
                              factor(of_age) +factor(of_race) +
                              factor(race_gender) + factor(driver_age)+ 
                              investigatory + out_of_state +
                              factor(year)+factor(tod),
                            data=fl.ag.officers,
                            subset=fl.ag.officers$search_occur>0)
save(contra.search.rate.reg,file="Data/FlSearchRate_OLS.RData")
contra.stop.rate.reg = lm(contra.stop.rate ~ factor(of_gender) + factor(of_exper) + 
                            factor(of_age) + factor(of_race) +
                            factor(race_gender) + factor(driver_age)+ 
                            investigatory + out_of_state +
                            factor(year)+factor(tod),
                          data=fl.ag.officers)
save(contra.stop.rate.reg,file="Data/FlStopRate_OLS.RData")