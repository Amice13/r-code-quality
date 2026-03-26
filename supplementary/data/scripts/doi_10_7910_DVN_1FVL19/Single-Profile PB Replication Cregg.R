# Load Necessary Packages
library(cregg)
library(ggplot2)
library(sjPlot)
library(estimatr)
library(plotrix)
library(dplyr)
library(ggpubr)
library(stargazer)
library(egg)
library(multcomp)
library(rmcorr)
library(lme4)
library(domir)

# Load Necessary Packages
# SET YOUR WORKING DIRECTORY
data <- read.csv("Single-Profile Experiment.csv",header=T,sep=",")

# Code Data
## Outcome
data$sentence_ordinal <- ((data$choice_num+1)*12)-11
data$sentence_ordinal_factor <- as.factor(data$sentence_ordinal)
data$sentence_jail <- ifelse(data$sentence_ordinal_factor=="1" | data$sentence_ordinal_factor=="2",0,1)
data$sentence_years <- ifelse(data$sentence_ordinal_factor=="1",0,ifelse(data$sentence_ordinal_factor=="2",0,ifelse(data$sentence_ordinal_factor=="3",1/365,ifelse(data$sentence_ordinal_factor=="4",4/365,ifelse(data$sentence_ordinal_factor=="5",2/12,ifelse(data$sentence_ordinal_factor=="6",1/3,ifelse(data$sentence_ordinal_factor=="7",7/12,ifelse(data$sentence_ordinal_factor=="8",2,ifelse(data$sentence_ordinal_factor=="9",6,ifelse(data$sentence_ordinal_factor=="10",11,ifelse(data$sentence_ordinal_factor=="11",16,ifelse(data$sentence_ordinal_factor=="12",20,30))))))))))))
## Target
data$target <- factor(data$profile_target,levels=c("Counter-protester","Government","Police","Small business"))
## Severity
data$severity <- factor(data$profile_severity,levels=c("Low","Medium","High"))
## General Act
data$act <- factor(data$profile_act,levels=c(" Threatened to injure a counter-protester ", " Punched a counter-protester "," Used a weapon to severely injure a counter protester "," Threatened to injure a police officer "," Punched a police officer "," Used a weapon to severely injure a police officer "," Graffitied a nearby government building "," Caused significant damage to a nearby government building "," Burned down a nearby government building "," Graffitied a nearby small business "," Caused significant damage to a nearby small business "," Burned down a nearby small business "))
## Age
data$p_age <- factor(data$profile_age,levels=c("Mid twenties","Mid thirties","Mid forties","Mid fifties"))
## Children
data$children <- factor(data$profile_children,levels=c("None","One","Two"))
## Gender
data$p_gender <- factor(data$profile_gender,levels=c("Man","Woman"))
## Race
data$p_race <- factor(data$profile_race,levels=c("White","Black","Hispanic","Asian","Middle Eastern"))
## Marital
data$marital <- factor(data$profile_marital,levels=c("Single","Cohabitating with partner","Married","Divorced","Widowed"))
data$marital_d <- relevel(data$marital,ref="Divorced")
## Occupation Group
data$occupation_group <- factor(data$profile_occupation_group,levels=c("Blue Collar","White Collar"))
## Residence Group
data$residence_group <- factor(data$profile_residence_group,levels=c("Urban","Rural","Out of State"))
## Party Congruency
data$PID_congruency <- factor(ifelse(data$Party3=="Democrat" & data$profile_party=="Democrat","Congruent",ifelse(data$Party3=="Republican" & data$profile_party=="Republican","Congruent",ifelse(data$Party3=="Democrat" & data$profile_party=="Independent","Independent",ifelse(data$Party3=="Republican" & data$profile_party=="Independent","Independent",ifelse(data$Party3=="Democrat" & data$profile_party=="Republican","Incongruent",ifelse(data$Party3=="Republican" & data$profile_party=="Democrat","Incongruent",NA)))))),levels=c("Congruent","Independent","Incongruent"))
## Respondent Race
data$race <- factor(ifelse(data$hispanic=="Yes","Hispanic",data$Race.Ethnicity),levels=c("White","Black","Hispanic","Asian","Native American"))
## Racial Ingroup Status
data$p_raceingroup <- ifelse(is.na(data$race)==F,"Racial Outgroup",NA)
data$p_raceingroup[data$race=="White" & data$p_race=="White"] <- "Racial Ingroup"
data$p_raceingroup[data$race=="Black" & data$p_race=="Black"] <- "Racial Ingroup"
data$p_raceingroup[data$race=="Hispanic" & data$p_race=="Hispanic"] <- "Racial Ingroup"
data$p_raceingroup[data$race=="Asian" & data$p_race=="Asian"] <- "Racial Ingroup"
data$p_raceingroup <- factor(data$p_raceingroup)
## Racial Subordinate status
data$p_racesubordinate <- factor(ifelse(data$p_race=="White","White","POC"),levels=c("White","POC"))
## Party
data$r_party <- factor(ifelse(data$Party3=="Independent",NA,data$Party3))
## Rurality
data$r_rural <- factor(ifelse(as.numeric(as.factor(data$placeidentity))==1,NA,ifelse(as.numeric(as.factor(data$placeidentity))==3,"Rural","Not Rural")))

# Formulas
form1 <- sentence_jail ~ target + severity + PID_congruency + residence_group + occupation_group + p_age + p_raceingroup + p_racesubordinate + p_gender + marital + children  # General, Jail
form9 <- sentence_years ~ target + severity + PID_congruency + residence_group + occupation_group + p_age + p_raceingroup + p_racesubordinate + p_gender + marital + children  # General, Years
form10 <- motivation_num ~ target + severity + PID_congruency + residence_group + occupation_group + p_age + p_raceingroup + p_racesubordinate + p_gender + marital + children  # General, Motive

# Figure A4
cj.jail.mm <- mm(data,form1,id=~ResponseId,h0=mean(data$sentence_jail,na.rm=T))
cj.years.mm <- mm(data,form9,id=~ResponseId,h0=mean(data$sentence_years,na.rm=T))
f1.jail <- plot(cj.jail.mm)
f1.years <- plot(cj.years.mm)

# Figure A5
cj.motive.mm <- mm(data,form10,id=~ResponseId,h0=mean(data$motivation_num,na.rm=T))
cj.motive.amce <- amce(data,form10,id=~ResponseId)
f1.motive <- plot(cj.motive.mm)
f2.motive <- plot(cj.motive.amce)