# Load Necessary Packages
library(dplyr)
library(car)
library(lmtest)
library(sandwich)
library(fabricatr)
library(sjPlot)
library(lme4)
library(stargazer)
library(margins)
library(multcomp)
library(estimatr)
library(cregg)
library(domir)

### Import Clean Data ###
# SET YOUR WORKING DIRECTORY
PV<-read.csv("Double-Profile Experiment.csv")

# Get Profile pair IDs
seven <- data.frame(seven=c(1:19341,1:19341))
seven <- arrange(seven,seven)
PV$profile <- seven$seven

### Setting Reference Categories ###
PV$profile_target<-factor(PV$profile_target, c("Counter-protester", "Government", "Police", 
                                               "Small business"))
PV$profile_severity<-factor(PV$profile_severity, c("Low", "Medium", "High"))
PV$profile_race<-factor(PV$profile_race, c("White", "Black", "Hispanic", "Asian", "Middle Eastern",
                                           "American Indian"))
PV$profile_marital<-factor(PV$profile_marital, c("Single", "Cohabitating with partner", "Married", 
                                                 "Divorced", "Widowed"))
PV$profile_marital_d <- factor(relevel(PV$profile_marital,ref="Divorced"))
PV$profile_age<-factor(PV$profile_age, c("Mid twenties", "Mid thirties", "Mid forties", 
                                         "Mid fifties"))

PV$Race.Ethnicity<-factor(PV$Race.Ethnicity, levels= c("White", "Black", "Asian", "Native American",
                                                       "Native Hawaiian or Pacific Islander","Other"))
PV$Gender<-factor(PV$Gender, levels= c("Male", "Female"))
PV$hispanic<-factor(PV$hispanic, levels= c("No", "Yes"))
PV$native<-factor(PV$native, levels= c("No", "Yes"))
PV$interest<-factor(PV$interest, levels= c("Not at all interested", "Not very interested", "Fairly interested",
                                           "Very interested"))
PV$education<-factor(PV$education, levels= c("High School Grad", "Less than High School", "Some College",
                                             "Associates", "Bachelors", "Post-Grad"))
PV$employment<-factor(PV$employment, levels= c("Full time employee", "Homemaker", "Part time employee",
                                               "Unemployed seeking work", "Sick/Disabled", 
                                               "Temporary Sick/Disabled", "Student", "Retired", "None"))
PV$PID_Congruent <- NA
PV$PID_Congruent[(PV$Party7<4 & PV$profile_party=="Democrat") | (PV$Party7>4 & PV$profile_party=="Republican")] <- "Congruent"
PV$PID_Congruent[PV$profile_party=="Independent"] <- "Independent"
PV$PID_Congruent[(PV$Party7<4 & PV$profile_party=="Republican") | (PV$Party7>4 & PV$profile_party=="Democrat")] <- "Incongruent"
PV$PID_Congruent <- factor(PV$PID_Congruent,levels=c("Congruent","Independent","Incongruent"))
PV$polariz <- ifelse(PV$Party7<4,PV$Dems_ft-PV$Repubs_ft,ifelse(PV$Party7>4,PV$Repubs_ft-PV$Dems_ft,NA))
PV$race <- ifelse(PV$hispanic=="Yes","Hispanic",PV$Race.Ethnicity)
PV$race[PV$race=="1"] <- "White"
PV$race[PV$race=="2"] <- "Black"
PV$race[PV$race=="3"] <- "Asian"
PV$race[PV$race=="4"] <- "Native American"
PV$race[PV$race=="5"] <- NA
PV$race[PV$race=="6"] <- NA
PV$race <- factor(PV$race,levels=c("White","Black","Hispanic","Asian","Native American"))
PV$profile_raceingroup <- ifelse(is.na(PV$race)==T | is.na(PV$profile_race)==T,NA,"Racial Outgroup")
PV$profile_raceingroup[PV$race=="White" & PV$profile_race=="White"] <- "Racial Ingroup"
PV$profile_raceingroup[PV$race=="Black" & PV$profile_race=="Black"] <- "Racial Ingroup"
PV$profile_raceingroup[PV$race=="Hispanic" & PV$profile_race=="Hispanic"] <- "Racial Ingroup"
PV$profile_raceingroup[PV$race=="Asian" & PV$profile_race=="Asian"] <- "Racial Ingroup"
PV$profile_raceingroup[PV$race=="Native American" & PV$profile_race=="American Indian"] <- "Racial Ingroup"
PV$profile_raceingroup <- factor(PV$profile_raceingroup)
PV$profile_raceadvantage <- factor(ifelse(PV$profile_race=="White","White","POC"),levels=c("White","POC"))
PV$profile_act1 <- PV$profile_act
PV$profile_act <- factor(PV$profile_act)
PV$act <- factor(ifelse(as.numeric(PV$profile_act)==1,NA,ifelse(as.numeric(PV$profile_act)==10,"Threatened Counter-Protester",ifelse(as.numeric(PV$profile_act)==8,"Punched Counter-Protester",ifelse(as.numeric(PV$profile_act)==12,"Injured Counter-Protester",ifelse(as.numeric(PV$profile_act)==11,"Threatened Police",ifelse(as.numeric(PV$profile_act)==9,"Punched Police",ifelse(as.numeric(PV$profile_act)==13,"Injured Police",ifelse(as.numeric(PV$profile_act)==6,"Graffitied Government Building",ifelse(as.numeric(PV$profile_act)==4,"Damaged Government Building",ifelse(as.numeric(PV$profile_act)==2,"Burned Down Government Building",ifelse(as.numeric(PV$profile_act)==7,"Graffitied Small Business",ifelse(as.numeric(PV$profile_act)==5,"Damaged Small Business","Burned Down Small Business")))))))))))),levels=c("Threatened Counter-Protester","Punched Counter-Protester","Injured Counter-Protester","Threatened Police","Punched Police","Injured Police","Graffitied Government Building","Damaged Government Building","Burned Down Government Building","Graffitied Small Business","Damaged Small Business","Burned Down Small Business"))
PV$Party3_1 <- factor(ifelse(PV$Party3=="Independent",NA,PV$Party3))
PV$profile_residence_group <- factor(PV$profile_residence_group)
PV$profile_occupation_group <- factor(PV$profile_occupation_group)
PV$profile_gender <- factor(PV$profile_gender)
PV$profile_children <- factor(PV$profile_children)
PV$r_rural <- factor(ifelse(as.numeric(as.factor(PV$geo_id_current))==1,NA,ifelse(as.numeric(as.factor(PV$geo_id_current))==3 | as.numeric(as.factor(PV$geo_id_current))==5,"Rural","Not Rural")))
PV$arrest <- 1-PV$decision

# Formulas
form <- arrest ~ profile_target + profile_severity + PID_Congruent + profile_residence_group + profile_occupation_group + profile_age + profile_raceingroup + profile_raceadvantage + profile_gender + profile_marital + profile_children
form_a <- arrest ~ profile_target + profile_severity + PID_Congruent + profile_residence_group + profile_occupation_group + profile_age + profile_raceingroup + profile_raceadvantage + profile_gender + profile_marital_d + profile_children

# Figure A4
cj.leniency.mm <- mm(PV,form,id=~ResponseId,h0=0.5)
f2.leniency <- plot(cj.leniency.mm)
