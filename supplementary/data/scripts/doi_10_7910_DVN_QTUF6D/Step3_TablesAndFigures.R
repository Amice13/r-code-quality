#######
#######
####### Replication files for Do Women Officers Police Differently? Evidence from Traffic Stops
####### This file produces the tables and figures seen in the paper and appendix.
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

# Opening up those libraries:
library(ggplot2)
library(texreg)
library(readr)
library(pscl)
library(arm)

###
### 2. Body of the Paper
###

# Clearing the workspace + reading in data bit by bit to produce each table and figure. 
rm(list = ls())

# Loading in the Data
load("Data/NorthCarolina.RData")
load("Data/FloridaLarge.RData")
load("Data/FloridaSmall.RData")
cmpd.employee = read_csv("Data/CMPD_Employee_Demographics.csv")

# Number of stops and searches by sex:
dim(fl)
dim(nc)

table(fl$search_occur)
table(nc$search)

prop.table(table(fl$search_occur))
prop.table(table(nc$search))

table(fl$of_gender)
table(nc$of_gender)

table(fl$of_gender,fl$search_occur)
table(nc$of_gender,nc$search)

prop.table(table(fl$of_gender,fl$search_occur),1)
prop.table(table(nc$of_gender,nc$search),1)

table(fl$of_gender,fl$contra)

# Number of officers by sex in FL
length(unique(fl$officer_id_hash))
length(unique(fl$officer_id_hash[fl$of_gender==0]))
length(unique(fl$officer_id_hash[fl$of_gender==1]))

length(unique(fl$officer_id_hash[fl$officer_exclude==0]))
length(unique(fl$officer_id_hash[fl$of_gender==0&fl$officer_exclude==0]))
length(unique(fl$officer_id_hash[fl$of_gender==1&fl$officer_exclude==0]))

table(cmpd.employee$JOB_TITLE[cmpd.employee$JOB_TITLE=="Police Officer"])
sum(table(cmpd.employee$Gender[cmpd.employee$JOB_TITLE=="Police Officer"]))

table(fl$year)
(table(fl$of_gender)/c(length(unique(fl$officer_id_hash[fl$of_gender==0&fl$officer_exclude==0])),length(unique(fl$officer_id_hash[fl$of_gender==1&fl$officer_exclude==0]))))/6

avg.stops = aggregate(fl$year,by=list(fl$officer_id_hash,fl$year,fl$of_gender),length)
summary(avg.stops)
mean(avg.stops$x)
median(avg.stops$x[avg.stops$Group.3==0])
median(avg.stops$x[avg.stops$Group.3==1])

prop.table(table(fl$investigatory[fl$of_gender==0]))
prop.table(table(fl$investigatory[fl$of_gender==1]))

table(nc$of_gender[nc$year==2019])[2:1]/table(cmpd.employee$Gender[cmpd.employee$JOB_TITLE=="Police Officer"])

# Excluding Cases:
dim(nc)
dim(nc)-dim(nc[!is.na(nc$search),])
dim(fl)
dim(fl)-dim(fl[!is.na(fl$search_occur),])
(dim(fl[!is.na(fl$search_occur),])-dim(fl.sm))+table(fl.sm$officer_exclude)[2]
table(fl.sm$county_include)

# Table 1
tab1 = data.frame("Department"=c("Charlotte PD (NC)",
                                 "Male Officers","Female Officers",
                                 "Florida Highwar Patrol",
                                 "Male Officers","Female Officers"),
                  "Type"=c("Municipal","","","Statewide","",""),
                  "Years"=c("2016-2017","","",
                            "2010-2015","",""),
                  "Stops"=c(dim(nc)[1],table(nc$of_gender),
                            dim(fl[!is.na(fl$search_occur),])[1],
                            table(fl$of_gender[!is.na(fl$search_occur)])),
                  "Searches"=c(table(nc$search)[2],table(nc$of_gender,nc$search)[,2],
                               table(fl$search_occur)[2],
                               table(fl$of_gender,fl$search_occur)[,2]),
                  "Search Rate"=c(table(nc$search)[2]/dim(nc)[1],
                                  table(nc$of_gender,nc$search)[,2]/table(nc$of_gender),
                                  table(fl$search_occur)[2]/dim(fl[!is.na(fl$search_occur),])[1],
                                  table(fl$of_gender,fl$search_occur)[,2]/
                                    table(fl$of_gender[!is.na(fl$search_occur)])))
tab1 = rbind(tab1,
             c("Total","","",
               sum(tab1[c(1,4),4]),sum(tab1[c(1,4),5]),
               sum(tab1[c(1,4),5])/sum(tab1[c(1,4),4])))
tab1

# Figure 1
load("Data/Fig1_Data.RData")
png("Figures/Fig1_PredProb.png",
    750,519)
ggplot(data = search.df, aes(x=Department,y=Rate,fill=Gender)) + 
  geom_bar(stat="identity", position=position_dodge()) + 
  ylab("Search Rate") +
  theme_bw(base_size=15)+
  theme(legend.position = "bottom") +
  labs(fill="Officer Sex")+
  scale_fill_grey(start = 0.25, end = .75) 
dev.off()

prop.test(table(fl$of_gender,fl$search_occur))
prop.test(table(nc$of_gender,nc$search))

# Table 2
load("Data/FLSearch_Sm_OLS.RData")
load("Data/FLSearch_OLS.RData")
load("Data/NCSearch_Sm_OLS.RData")
load("Data/NCSearch_OLS.RData")
screenreg(list(nc.search,fl.search),
          stars=c(0.01,0.05),
          custom.coef.map = list("(Intercept)"="(Intercept)",
                                 "factor(of_gender)1"="Female Officer",
                                 "factor(of_race)1"="Black Officer",
                                 "factor(race_gender)1"="White Female",
                                 "factor(race_gender)2"="Black Male",
                                 "factor(race_gender)3"="Black Female",
                                 "factor(race_gender)4"="Latino Male",
                                 "factor(race_gender)5"="Latina Female",
                                 "investigatory" = "Investigatory Stop Purpose"),
          custom.model.names = c("(1) NC Search",
                                 "(2) FL Search"),
          digits=4)

# Figure 2
fl.of.pred = predict(fl.search,
                     newdata = data.frame("of_gender"=c(0,1),"race_gender"=0,
                                            "subject_age"=35,"out_of_state"=0,
                                            "investigatory"=1,
                                          "officer_years_of_service"=6,
                                          "of_race"=0,"officer_age"=39,
                                          "hour_of_day"=15,
                                          "month"="05","year"=2013,
                                          "county_name"="Orange County"),
                     type="response",se.fit=T)
nc.of.pred = predict(nc.search,
                     newdata = data.frame("of_gender"=c(0,1),
                                          "race_gender"=0,
                                          "subject_age"=36,
                                          "investigatory"=1,
                                          "Officer_Years_of_Service"=10.25,
                                          "of_race"=0,"month"="01",
                                          "year"=2019,"CMPD_Division"="South Division"),
                     type="response",se.fit=T)



pred.df = data.frame("Department" = c("Charlotte Police Department",
                                      "Charlotte Police Department",
                                      "Florida Highway Patrol",
                                      "Florida Highway Patrol"),
                     "Gender" = c("Male","Female","Male","Female"),
                     "Predict" = c(nc.of.pred$fit,
                                   fl.of.pred$fit),
                     "Lower"=c(nc.of.pred$fit-1.96*nc.of.pred$se.fit,
                               fl.of.pred$fit-1.96*fl.of.pred$se.fit),
                     "Upper"=c(nc.of.pred$fit+1.96*nc.of.pred$se.fit,
                               fl.of.pred$fit+1.96*fl.of.pred$se.fit))

png("Figures/Fig2_PredProb.png",
    900,514)
ggplot(data = pred.df, aes(x=Gender,y=Predict)) + 
  geom_point(size=4) +  
  geom_errorbar(aes(ymin = Lower, ymax = Upper),
                width=.2,size = 0.75,                   
                position=position_dodge(.9)) + 
  ylab("Expected Probbility of a Search") +
  xlab("Officer Sex") +
  theme_bw(base_size=15) +facet_wrap(~Department) 
dev.off()

pred.df$Predict[1]/pred.df$Predict[2]
pred.df$Predict[3]/pred.df$Predict[4]

# Table 3
tab3 = data.frame("Officer Gender"=c("Male","Female"),
                  "Searches"=table(fl$of_gender[!is.na(fl$search_occur)],
                                   fl$search_occur[!is.na(fl$search_occur)])[,2],
                  "Contraband"=table(fl$of_gender[!is.na(fl$search_occur)],
                                     fl$contra[!is.na(fl$search_occur)])[,2],
                  "Contraband Hit Rate"=table(fl$of_gender[!is.na(fl$search_occur)],
                                              fl$contra[!is.na(fl$search_occur)])[,2]/
                    table(fl$of_gender[!is.na(fl$search_occur)],
                          fl$search_occur[!is.na(fl$search_occur)])[,2],
                  "Difference"=c((table(fl$of_gender[!is.na(fl$search_occur)],
                                        fl$contra[!is.na(fl$search_occur)])[,2]/
                                    table(fl$of_gender[!is.na(fl$search_occur)],
                                          fl$search_occur[!is.na(fl$search_occur)])[,2])[1]-
                                   (table(fl$of_gender[!is.na(fl$search_occur)],
                                          fl$contra[!is.na(fl$search_occur)])[,2]/
                                      table(fl$of_gender[!is.na(fl$search_occur)],
                                            fl$search_occur[!is.na(fl$search_occur)])[,2])[2],NA))
tab3
prop.test(table(fl$of_gender[fl$search_occur==1],
                fl$contra[fl$search_occur==1]))

# Table 4
load("Data/FlContra_OLS.RData")
load("Data/FlSearchRate_OLS.RData")
load("Data/FlStopRate_OLS.RData")
screenreg(list(fl.contra,contra.search.rate.reg,contra.stop.rate.reg),
          stars=c(0.01,0.05),
          custom.coef.map = list("(Intercept)"="(Intercept)",
                                 "factor(of_gender)1"="Female Officer",
                                 "factor(of_race)1"="Black Officer",
                                 "factor(race_gender)1"="White Female",
                                 "factor(race_gender)2"="Black Male",
                                 "factor(race_gender)3"="Black Female",
                                 "factor(race_gender)4"="Latino Male",
                                 "factor(race_gender)5"="Latina Female",
                                 "investigatory" = "Investigatory Stop Purpose"),
          custom.model.names = c("(1) Contra|Search",
                                 "(2) Hit Rate, per 10 Searches",
                                 "(3) Hit Rate, per 100 Stops"),
          digits=4)

###
### 3. Appendix A: Full Regression Results
###

screenreg(list(nc.search,fl.search,
               fl.contra,contra.search.rate.reg,contra.stop.rate.reg),
          stars=c(0.01,0.05),
          custom.coef.map = list("(Intercept)"="(Intercept)",
                                 "factor(of_gender)1"="Female Officer",
                                 "factor(of_race)1"="Black Officer",
                                 "officer_age"="Officer Age",
                                 "factor(of_age)2"="Officer Age: 30-64",
                                 "factor(of_age)3"="Officer Age: 65+",
                                 "officer_years_of_service"="Officer Years of Service",
                                 "Officer_Years_of_Service"="Officer Years of Service",
                                 "factor(of_exper)1"="Experienced Officer",
                                 "factor(race_gender)1"="White Female",
                                 "factor(race_gender)2"="Black Male",
                                 "factor(race_gender)3"="Black Female",
                                 "factor(race_gender)4"="Latino Male",
                                 "factor(race_gender)5"="Latina Female",
                                 "subject_age"="Driver Age",
                                 "factor(driver_age)2"="Driver Age: 30-64",
                                 "factor(driver_age)3"="Driver Age: 65+",
                                 "investigatory" = "Investigatory Stop Purpose",
                                 "out_of_state"="Out of State"),
          custom.model.names = c("(1)","(2)",
                                 "(3)","(4)","(5)"),
          digits=3)

###
### 4. Appendix B: Alternative Test of Differences in Search and Contraband Hit Rates
###

# Florida
fl$stop = 1
fl$of_exper = ifelse(fl$officer_years_of_service>=
                       mean(fl$officer_years_of_service,na.rm=T),1,0)
fl$of_age = ifelse(fl$officer_age<30,1,
                   ifelse(fl$officer_age>64,3,2))
fl$driver_age = ifelse(fl$subject_age<30,1,
                       ifelse(fl$subject_age>64,3,2))
fl$hour_of_day=as.numeric(fl$hour_of_day)
fl$tod = ifelse(fl$hour_of_day<3,1,
                ifelse(fl$hour_of_day<6,2,
                       ifelse(fl$hour_of_day<9,3,
                              ifelse(fl$hour_of_day<12,4,
                                     ifelse(fl$hour_of_day<15,5,
                                            ifelse(fl$hour_of_day<18,6,
                                                   ifelse(fl$hour_of_day<21,7,8)))))))

fl.ag = aggregate(fl[!is.na(fl$search_occur),c("stop","search_occur","contra")], 
                  by = list(fl$tod[!is.na(fl$search_occur)],
                            fl$officer_race[!is.na(fl$search_occur)],
                            fl$officer_sex[!is.na(fl$search_occur)],
                            fl$of_exper[!is.na(fl$search_occur)],
                            fl$race_gender[!is.na(fl$search_occur)],
                            fl$driver_age[!is.na(fl$search_occur)],
                            fl$out_of_state[!is.na(fl$search_occur)],
                            fl$investigatory[!is.na(fl$search_occur)]),
                  sum,na.rm=T)
colnames(fl.ag) = c("tod",
                    "of_race","of_sex","of_exper","driver_rg",
                    "driver_age","out_of_state","invest",
                    "stop","search","contraband")
fl.ag.female = fl.ag[fl.ag$of_sex=="female",]
colnames(fl.ag.female)[c(3,9:11)] = c("female","stop.f",
                                       "search.f","contra.f")
fl.ag.male = fl.ag[fl.ag$of_sex=="male",]
colnames(fl.ag.male)[c(3,9:11)] = c("male","stop.m",
                                     "search.m","contra.m")

fl.matches = merge(fl.ag.female,fl.ag.male)
min.stops = 9
table(fl.matches$stop.f>min.stops&
        fl.matches$stop.m>min.stops)
min.searches = 0
table(fl.matches$search.f>min.searches&
        fl.matches$search.m>min.searches)
table(fl.matches$search.f>min.searches&
        fl.matches$search.m>min.searches&
        fl.matches$stop.f>min.stops&
        fl.matches$stop.m>min.stops)

# North Carolina
nc$stop = 1
nc$search = ifelse(nc$Was_a_Search_Conducted=="Yes",1,0)
nc$driver_age = ifelse(nc$Driver_Age<30,1,
                       ifelse(nc$Driver_Age>65,3,2))
nc$of_exper = ifelse(nc$Officer_Years_of_Service>=mean(nc$Officer_Years_of_Service),
                     1,0)
nc.ag = aggregate(nc[,c("search","stop")], 
                  by = list(nc$CMPD_Division,
                            nc$Officer_Gender,nc$Officer_Race,
                            nc$of_exper,
                            nc$race_gender,nc$driver_age,
                            nc$investigatory,
                            nc$year),
                  sum)
nc.ag.female = nc.ag[nc.ag$Group.2=="Female",]
colnames(nc.ag.female) = c("division","female","race","of_exper",
                           "driver.rg","driver_age","investigatory",
                           "year",
                           "searches.f","stops.f")
nc.ag.male = nc.ag[nc.ag$Group.2=="Male",]
colnames(nc.ag.male) = c("division","male","race","of_exper",
                         "driver.rg","driver_age","investigatory",
                         "year",
                         "searches.m","stops.m")


# Searches
fl.matches$sr.f = fl.matches$search.f/fl.matches$stop.f
fl.matches$sr.m = fl.matches$search.m/fl.matches$stop.m
fl.matches$cr.f = fl.matches$contra.f/fl.matches$search.f
fl.matches$cr.m = fl.matches$contra.m/fl.matches$search.m
t.test(fl.matches$sr.f[fl.matches$stop.f>min.stops&
                         fl.matches$stop.m>min.stops],
       fl.matches$sr.m[fl.matches$stop.f>min.stops&
                         fl.matches$stop.m>min.stops],
       paired = T)
length(fl.matches$sr.f[fl.matches$stop.f>min.stops&
                         fl.matches$stop.m>min.stops])
mean(fl.matches$sr.f[fl.matches$stop.f>min.stops&
                       fl.matches$stop.m>min.stops])
mean(fl.matches$sr.m[fl.matches$stop.f>min.stops&
                       fl.matches$stop.m>min.stops])

nc.matches = merge(nc.ag.female,nc.ag.male)
min.stops = 9
nc.matches$sr.f = nc.matches$searches.f/nc.matches$stops.f
nc.matches$sr.m = nc.matches$searches.m/nc.matches$stops.m
t.test(nc.matches$sr.f[nc.matches$stops.f>min.stops&
                         nc.matches$stops.m>min.stops],
       nc.matches$sr.m[nc.matches$stops.f>min.stops&
                         nc.matches$stops.m>min.stops],
       paired = T)

length(nc.matches$sr.f[nc.matches$stops.f>min.stops&
                         nc.matches$stops.m>min.stops])
mean(nc.matches$sr.f[nc.matches$stops.f>min.stops&
                       nc.matches$stops.m>min.stops])
mean(nc.matches$sr.m[nc.matches$stops.f>min.stops&
                       nc.matches$stops.m>min.stops],)

# Contraband
t.test(fl.matches$cr.f[fl.matches$search.f>min.searches&
                         fl.matches$search.m>min.searches&
                         fl.matches$stop.f>min.stops&
                         fl.matches$stop.m>min.stops],
       fl.matches$cr.m[fl.matches$search.f>min.searches&
                         fl.matches$search.m>min.searches&
                         fl.matches$stop.f>min.stops&
                         fl.matches$stop.m>min.stops],
       paired = T)
length(fl.matches$cr.f[fl.matches$search.f>min.searches&
                         fl.matches$search.m>min.searches&
                         fl.matches$stop.f>min.stops&
                         fl.matches$stop.m>min.stops])
mean(fl.matches$cr.f[fl.matches$search.f>min.searches&
                       fl.matches$search.m>min.searches&
                       fl.matches$stop.f>min.stops&
                       fl.matches$stop.m>min.stops])
mean(fl.matches$cr.m[fl.matches$search.f>min.searches&
                       fl.matches$search.m>min.searches&
                       fl.matches$stop.f>min.stops&
                       fl.matches$stop.m>min.stops])

###
### 5. Appendix C: Logistic Regrssion Models
###

rm(list = ls())

load("Data/FlContra_Logit.RData")
load("Data/FLSearch_Logit.RData")
load("Data/NCSearch_Logit.RData")

texreg(list(nc.search,fl.search,fl.contra),
          stars=c(0.01,0.05),
          custom.coef.map = list("(Intercept)"="(Intercept)",
                                 "factor(of_gender)1"="Female Officer",
                                 "factor(of_race)1"="Black Officer",
                                 "factor(of_race)2"="Latinx Officer",
                                 "factor(of_race)3"="Asain/Pacific Islander Officer",
                                 "factor(of_race)4"="Other Race Officer",
                                 "officer_age"="Officer Age",
                                 "officer_years_of_service"="Officer Years of Service",
                                 "Officer_Years_of_Service"="Officer Years of Service",
                                 "factor(race_gender)1"="White Female",
                                 "factor(race_gender)2"="Black Male",
                                 "factor(race_gender)3"="Black Female",
                                 "factor(race_gender)4"="Latino Male",
                                 "factor(race_gender)5"="Latina Female",
                                 "subject_age"="Driver Age",
                                 "investigatory" = "Investigatory Stop Purpose",
                                 "out_of_state"="Out of State"),
          custom.model.names = c("(1) NC Search",
                                 "(2) FL Search",
                                 "(3) FL Contra|Search"),
          digits=4)

###
### 6. Appendix C: Fixed Effects 
###

rm(list = ls())

load("Data/FLSearch_OLS_FE.RData")
load("Data/FlContra_OLS_FE.RData")
load("Data/FlSearchRate_OLS_FE.RData")
load("Data/FlStopRate_OLS_FE.RData")

texreg(list(fl.search,
               fl.contra,
               contra.search.rate.reg,
               contra.stop.rate.reg),
          stars=c(0.01,0.05),
          custom.coef.map = list("(Intercept)"="(Intercept)",
                                 "factor(of_gender)1"="Female Officer",
                                 "factor(of_race)1"="Black Officer",
                                 "officer_age"="Officer Age",
                                 "factor(of_age)2"="Officer Age: 30-64",
                                 "factor(of_age)3"="Officer Age: 65+",
                                 "officer_years_of_service"="Officer Years of Service",
                                 "Officer_Years_of_Service"="Officer Years of Service",
                                 "factor(of_exper)1"="Experienced Officer",
                                 "factor(race_gender)1"="White Female",
                                 "factor(race_gender)2"="Black Male",
                                 "factor(race_gender)3"="Black Female",
                                 "factor(race_gender)4"="Latino Male",
                                 "factor(race_gender)5"="Latina Female",
                                 "subject_age"="Driver Age",
                                 "factor(driver_age)2"="Driver Age: 30-64",
                                 "factor(driver_age)3"="Driver Age: 65+",
                                 "investigatory" = "Investigatory Stop Purpose",
                                 "out_of_state"="Out of State"),
          custom.model.names = c("(1) Search",
                                 "(2) Contra|Search",
                                 "(3) Hit Rate, per 10 Searches",
                                 "(4) Hit Rate, per 100 Stops"),
          digits=4)

###
### 7. Appendix D: Interaction Models
###

rm(list = ls())

# Table 1. Officer Experience
load("Data/FLSearch_Exper_OLS.RData")
load("Data/NCSearch_Exper_OLS.RData")
load("Data/FlContra_Exper_OLS.RData")
load("Data/FlSearchRate_Exper_OLS.RData")
load("Data/FlStopRate_Exper_OLS.RData")

texreg(list(nc.search.exper,fl.search.exper,fl.contra.exper,
            contra.search.rate.exper,contra.stop.rate.exper),
       stars=c(0.05,0.01),
       custom.coef.map = list("factor(of_gender)1"="Female Officer",
                              "officer_years_of_service"="Officer Years of Service",
                              "Officer_Years_of_Service"="Officer Years of Service",
                              "factor(of_exper)1"="Experienced Officer",
                              "factor(of_gender)1:officer_years_of_service"="Female Officer * Exper.",
                              "factor(of_gender)1:Officer_Years_of_Service"="Female Officer * Exper.",
                              "factor(of_gender)1:factor(of_exper)1"="Female Officer * Exper."),
       digits = 3)

# Table 2. Prop Female
load("Data/FLSearch_Prop_OLS.RData")
load("Data/FlContra_Prop_OLS.RData")

texreg(list(fl.search.prop,fl.contra.prop),
       stars=c(0.05,0.01),
       custom.coef.map = list("factor(of_gender)1"="Female Officer",
                              "female.prop"="Female Proportion of Proximate Force",
                              "factor(of_gender)1:female.prop"="Female Officer * Female Prop."),
       digits = 3)

# Table 3. Stop Type
load("Data/FLSearch_StopType_OLS.RData")
load("Data/NCSearch_StopType_OLS.RData")
load("Data/FlContra_StopType_OLS.RData")
load("Data/FlSearchRate_StopType_OLS.RData")
load("Data/FlStopRate_StopType_OLS.RData")

texreg(list(nc.search.st,fl.search.st,fl.contra.st,
            contra.search.rate.st,contra.stop.rate.st),
       stars=c(0.05,0.01),
       custom.coef.map = list("(Intercept)"="(Intercept)",
                              "factor(of_gender)1"="Female Officer",
                              "factor(of_race)1"="Black Officer",
                              "officer_age"="Officer Age",
                              "factor(of_age)2"="Officer Age: 30-64",
                              "factor(of_age)3"="Officer Age: 65+",
                              "officer_years_of_service"="Officer Years of Service",
                              "Officer_Years_of_Service"="Officer Years of Service",
                              "factor(of_exper)1"="Experienced Officer",
                              "factor(race_gender)1"="White Female",
                              "factor(race_gender)2"="Black Male",
                              "factor(race_gender)3"="Black Female",
                              "factor(race_gender)4"="Latino Male",
                              "factor(race_gender)5"="Latina Female",
                              "subject_age"="Driver Age",
                              "factor(driver_age)2"="Driver Age: 30-64",
                              "factor(driver_age)3"="Driver Age: 65+",
                              "investigatory" = "Investigatory Stop Purpose",
                              "out_of_state"="Out of State"),
       digits = 3)

# Table 4. Driver Characteristics
load("Data/FLInter_Search.RData")
load("Data/FLInter_Contra.RData")
load("Data/FLStopRate_Inter_OLS.RData")
load("Data/FLSearchRate_Inter_OLS.RData")
load("Data/NCInter_Search.RData")

texreg(list(nc.search.inter,fl.search.inter,fl.contra.inter,
            contra.search.rate.inter,contra.stop.rate.inter),
          stars=c(0.01,0.05),
          custom.coef.map = list("factor(of_gender)1"="Female Officer",
                                 "factor(subject_female)1"="Female Driver",
                                 "factor(of_race)1"="Black Officer",
                                 "factor(of_race)2"="Latinx Officer",
                                 "factor(subject_race2)1"="Black Driver",
                                 "factor(subject_race2)2"="Latinx Driver",
                                 "factor(of_gender)1:factor(subject_female)1"="Female Officer*Driver",
                                 "factor(of_race)1:factor(subject_race2)1"="Black Officer*Driver",
                                 "factor(of_race)2:factor(subject_race2)1"="Latinx Officer*Black Driver",
                                 "factor(of_race)1:factor(subject_race2)2"="Black Officer*Latinx Driver",
                                 "factor(of_race)2:factor(subject_race2)2"="Latinx Officer* Driver"),digits=3)

###
### 8. Appendix E: A Conservative Test with the Charlotte Police Department
###

load("Data/NorthCarolina.RData")

table(nc$year)

nc.search16 = lm(search~factor(race_gender)+subject_age+
                   investigatory+
                   factor(of_race)+
                   factor(of_gender)+Officer_Years_of_Service+
                   factor(month)+
                   factor(CMPD_Division),
                 data=nc,subset=nc$year==2016)
nc.search17 = lm(search~factor(race_gender)+subject_age+
                   investigatory+
                   factor(of_race)+
                   factor(of_gender)+Officer_Years_of_Service+
                   factor(month)+
                   factor(CMPD_Division),
                 data=nc,subset=nc$year==2017)
nc.search19 = lm(search~factor(race_gender)+subject_age+
                   investigatory+
                   factor(of_race)+
                   factor(of_gender)+Officer_Years_of_Service+
                   factor(month)+
                   factor(CMPD_Division),
                 data=nc,subset=nc$year==2019)
nc.search20 = lm(search~factor(race_gender)+subject_age+
                   investigatory+
                   factor(of_race)+
                   factor(of_gender)+Officer_Years_of_Service+
                   factor(month)+
                   factor(CMPD_Division),
                 data=nc,subset=nc$year==2020)
texreg(list(nc.search16,nc.search17,nc.search19,nc.search20),
       omit.coef = "Division*|month*",
       custom.coef.map = list("(Intercept)"="(Intercept)",
                              "factor(of_gender)1"="Female Officer",
                              "factor(of_race)1"="Black Officer",
                              "Officer_Years_of_Service"="Officer Years of Service",
                              "investigatory"="Investigatory Stop",
                              "factor(race_gender)1"="White Female",
                              "factor(race_gender)2"="Black Male",
                              "factor(race_gender)3"="Black Female",
                              "subject_age"="Driver Age"),
       stars=c(0.01,0.05))