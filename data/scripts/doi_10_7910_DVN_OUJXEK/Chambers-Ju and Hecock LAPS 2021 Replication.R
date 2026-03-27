#################################################
## "Union Affiliation and Political Activation" 
## Christopher Chambers-Ju and Doug Hecock      
## Sept 1, 2021                              
#################################################

# install libraries
install.packages("ggplot2", "dplyr", 
                 "stargazer", "plyr", "rtf", 
                 "ggpubr", "mfx", "ggpubr")

# load libraries
library(foreign)
library(ggplot2)
library(plyr)
library(dplyr)
library(stargazer)
library(rtf)
library(ggpubr)

# Importing data
# file.choose()
dta<-read.csv("/Users/chambersjuc/Documents/LAPS Turnout and Unions Survey Data 2020/CCJ_RDH_LAPS_2021_data.csv")
attach(dta)

########################
## Dependent Variables #
########################

## Motivation to Vote for Presidency, Mayor, Senate, Lower House, City Council, Local Council, as dummies

dta$Motivate_Pres<-recode(p70m2_1, "Bastante" = 1, "No mucho" = 0, 
                        "Muy poco" = 0, "No sabe" = 0, "No responde"=0)
dta$Motivate_May<-recode(p70m2_4, "Bastante" = 1, "No mucho" = 0, 
                         "Muy poco" = 0,"No sabe" = 0, "No responde"=0)
dta$Motivate_Sen<-recode(p70m2_3, "Bastante" = 1, "No mucho" = 0, 
                         "Muy poco" = 0, "No sabe" = 0, "No responde"=0)
dta$Motivate_House<-recode(p70m2_2, "Bastante" = 1, "No mucho" = 0, 
                          "Muy poco" = 0, "No sabe" = 0, "No responde"=0)
dta$Motivate_Cou<-recode(p70m2_5, "Bastante" = 1, "No mucho" = 0, "Muy poco" = 0, 
                          "No sabe" = 0, "No responde"=0)
dta$Motivate_Local<-recode(p70m2_6, "Bastante" = 1, "No mucho" = 0, "Muy poco" = 0, 
                           "No sabe" = 0, "No responde"=0)


#############################################################
## Explanatory Variable: union membership #
#############################################################
dta$union_member<-factor(mapvalues(p77m2_3, 
                                   from = c("Si","No","No responde"), to = c(1,0,0)))

sum(length(which(is.na(dta$union_member))))

##############################################
## Figure 1. Proportion Motivated to Vote 
##############################################

Motivate_percent_full_df<-data.frame(Motivate_percent = c(Motivate_Pres_Bog = .4124,
                                                     Motivate_Pres_t = mean(dta$Motivate_Pres, na.rm=T),
                                                     Motivate_Pres_u = mean(dta$Motivate_Pres[dta$union_member == 1], na.rm=T),
                                                     Motivate_May_Bog = .2967,
                                                     Motivate_May_t = mean(dta$Motivate_May, na.rm=T),
                                                     Motivate_May_u = mean(dta$Motivate_May[dta$union_member == 1], na.rm=T),
                                                     Motivate_Sen_Bog = .1226,
                                                     Motivate_Sen_t = mean(dta$Motivate_Sen, na.rm=T),
                                                     Motivate_Sen_u = mean(dta$Motivate_Sen[dta$union_member == 1], na.rm=T),
                                                     Motivate_House_Bog = .113,
                                                     Motivate_House_t = mean(dta$Motivate_House, na.rm=T),
                                                     Motivate_House_u = mean(dta$Motivate_House[dta$union_member == 1], na.rm=T),
                                                     Motivate_Cou_Bog = .1295,
                                                     Motivate_Cou_t = mean(dta$Motivate_Cou, na.rm=T),
                                                     Motivate_Cou_u = mean(dta$Motivate_Cou[dta$union_member == 1], na.rm=T),
                                                     Motivate_Local_Bog = .1417,
                                                     Motivate_Local_t = mean(dta$Motivate_Local, na.rm=T),
                                                     Motivate_Local_u = mean(dta$Motivate_Local[dta$union_member == 1], na.rm=T)),
                                Office= c("President", "President", "President", "Mayor", "Mayor","Mayor",
                                          "Senate","Senate","Senate",
                                          "Lower House","Lower House","Lower House",
                                          "City Council", "City Council","City Council",
                                          "Local Council", "Local Council","Local Council"),
                                Type = rep(c("Bogota","Full Teacher Sample","Unionized Teachers"),6))

Motivate_percent_full_df$Office <- factor(Motivate_percent_full_df$Office, 
                                          ordered =T, levels = c("President", "Mayor", 
                                          "Senate","Lower House", 
                                          "City Council", "Local Council"))
                                                                                                  
Motivate_percent_full_df

ggplot(Motivate_percent_full_df, aes(x=Office, y=Motivate_percent, fill=Type)) +
  geom_bar(stat="identity", position="dodge", width=0.5) +
  xlab(NULL)+ylab("Proportion Motivated to Vote") + 
  theme(axis.text.x = element_text(angle = 90)) 

tiff(file="Figure 1.tiff",
     width=6, height=4, units="in", res=100)

##############
## Controls ##
##############

## Dummy for "belongs to a party or political group"

dta$party_member<-recode(p77m2_4, "Si" = 1, "No" =0, "No responde" = 0)
dta$party_member_NA<-factor(mapvalues(p77m2_4, 
                                    from = c("Si","No","No responde"), to = c(1,0,NA)))

## Economic outlook

Econ.sit<-recode(p16, "Igual" = "Same", "Mejor" = "Better", "Peor"="Worse")
dta$Economic_outlook<-factor(Econ.sit, levels=c("Worse","Same","Better"), ordered=T)

## Dummy for Urban

dta$Urban <-mapvalues(p4_sector,  from = c("Rural","Urbano","No responde"), to = c(0,1,NA))

## Income

dta$income<-mapvalues(p19, from = c("Hasta 993000","994000-1986000",
                                "1987000-2979000","2980000-3972000",
                                "3973000-4966000","4967000-5958000",
                                "5959000-6951000","6952000-7944000",
                                "7945000-9930000","9931000-11916000",
                                "mas de 11917000","No Responde"),
                                to = c(993000,994000, 1987000,2980000,3973000,
                                       4967000,5959000,6952000,7945000,9931000,
                                       11917000,NA))
dta$income<-as.numeric(dta$income)

## Left ideology

dta$Left<-recode(dta$p69m2, 
                 "Izquierda" =1,
                 "Centro izquierda" =1, 
                 "Centro" = 0,
                 "Centro derecha"=0, 
                 "Corrientes no tradicionales (anarquistas, ecologistas, libertad sexual, feministas, etc)"=0,
                 "Derecha"=0, 
                 "No tiene preferencias políticas"=0,
                 "No responde"=0) ## I included "No responde"

## Dummy for woman

dta$Woman<-recode(p1.x, "Hombre" = 0, "Mujer" =1)

## Age

dta$Age<-Edad

############

## Table 1. Logit Regression of Union Member on Motivation to Vote
## Demographic Controls 

############

r1.1<-glm(Motivate_Pres ~ Woman + Age + Urban + Economic_outlook + income + union_member, data = dta, family=binomial(link="logit"))
r1.2<- glm(Motivate_May ~ Woman + Age + Urban + Economic_outlook + income + union_member, data = dta, family=binomial(link="logit"))
r1.3<- glm(Motivate_Sen ~ Woman + Age + Urban + Economic_outlook + income + union_member, data = dta, family=binomial(link="logit"))
r1.4<- glm(Motivate_House ~ Woman + Age + Urban + Economic_outlook + income + union_member, data = dta, family=binomial(link="logit"))
r1.5<- glm(Motivate_Cou ~ Woman + Age + Urban + Economic_outlook + income + union_member, data = dta, family=binomial(link="logit"))
r1.6 <- glm(Motivate_Local ~ Woman + Age + Urban + Economic_outlook + income + union_member, data = dta, family=binomial(link="logit"))

stargazer(r1.1, r1.2, r1.3, r1.4, r1.5, r1.6, type="html", out="logit.htm")

odds1<-round(cbind(coef(r1.1),exp(coef(r1.1)),coef(r1.2), exp(coef(r1.2)),coef(r1.3), exp(coef(r1.3)),coef(r1.4),exp(coef(r1.4)),coef(r1.5), exp(coef(r1.5)),coef(r1.6),exp(coef(r1.6))),2)

odds1

############

## Table 2. Logit Regression of Union Member on Motivation to Vote
## All Controls

############

r2.1<- glm(Motivate_Pres ~ Woman + Age + Urban + Economic_outlook + income + party_member + union_member + Left, data = dta, family=binomial(link="logit"))
r2.2<- glm(Motivate_May ~ Woman + Age + Urban + Economic_outlook + income + party_member + union_member + Left, data = dta, family=binomial(link="logit"))
r2.3<- glm(Motivate_Sen ~ Woman + Age + Urban + Economic_outlook + income + party_member + union_member + Left, data = dta, family=binomial(link="logit"))
r2.4<- glm(Motivate_House ~ Woman + Age + Urban + Economic_outlook + income + party_member + union_member + Left, data = dta, family=binomial(link="logit"))
r2.5<- glm(Motivate_Cou ~ Woman + Age + Urban + Economic_outlook + income +party_member + union_member + Left, data = dta, family=binomial(link="logit"))
r2.6 <- glm(Motivate_Local ~ Woman + Age + Urban + Economic_outlook + income + party_member + union_member + Left, data = dta, family=binomial(link="logit"))

stargazer(r2.1, r2.2, r2.3, r2.4, r2.5, r2.6, type="html", out="logit.htm")

odds2<-round(cbind(coef(r2.1),exp(coef(r2.1)),coef(r2.2), exp(coef(r2.2)),coef(r2.3), exp(coef(r2.3)),coef(r2.4),exp(coef(r2.4)),coef(r2.5), exp(coef(r2.5)),coef(r2.6),exp(coef(r2.6))),2)

odds2

######################################
## Figure 2. Predicted Probabilities for Motivation to Vote

## Predicted Probability Model 1.1 President
######################################
calculate_mode <- function(x) {
  uniqx <- unique(na.omit(x))
  uniqx[which.max(tabulate(match(x, uniqx)))]
}

calculate_mode(dta$Woman)

new_dta1.1<-with(dta, 
               data.frame(Woman = calculate_mode(Woman), 
                          Age = mean(Age, na.rm=T),
                          Urban = calculate_mode(Urban),
                          Economic_outlook = calculate_mode(Economic_outlook),
                          income = mean(income, na.rm=T),
                          union_member = factor(c(0,1))))

new_dta1.1<-cbind(new_dta1.1,predict(r1.1, newdata = new_dta1.1, type = "response", se.fit=T))

new_dta1.1

names(new_dta1.1)[names(new_dta1.1)=="fit"] = "prob"
names(new_dta1.1)[names(new_dta1.1)=="se.fit"] = "se.prob"

new_dta1.1$ll = new_dta1.1$prob - 1.96*new_dta1.1$se.prob
new_dta1.1$ul = new_dta1.1$prob + 1.96*new_dta1.1$se.prob

new_dta1.1$Affiliation<-c("Non-member","Union Member")
new_dta1.1$office<-c("President","President")

######################################
## Predicted Probability Model 1.2 Mayor  ##
######################################

new_dta1.2<-with(dta, 
                 data.frame(Woman = calculate_mode(Woman), 
                            Age = mean(Age, na.rm=T),
                            Urban = calculate_mode(Urban),
                            Economic_outlook = calculate_mode(Economic_outlook),
                            income = mean(income, na.rm=T),
                            union_member = factor(c(0,1))))

new_dta1.2<-cbind(new_dta1.2,predict(r1.2, newdata = new_dta1.2, type = "response", se.fit=T))

names(new_dta1.2)[names(new_dta1.2)=="fit"] = "prob"
names(new_dta1.2)[names(new_dta1.2)=="se.fit"] = "se.prob"

new_dta1.2$ll = new_dta1.2$prob - 1.96*new_dta1.2$se.prob
new_dta1.2$ul = new_dta1.2$prob + 1.96*new_dta1.2$se.prob

new_dta1.2$Affiliation<-c("Non-member","Union Member")
new_dta1.2$office<-c("Mayor","Mayor")


######################################
## Predicted Probability Model 1.2 Senate  ##
######################################

new_dta1.3<-with(dta, 
                 data.frame(Woman = calculate_mode(Woman), 
                            Age = mean(Age, na.rm=T),
                            Urban = calculate_mode(Urban),
                            Economic_outlook = calculate_mode(Economic_outlook),
                            income = mean(income, na.rm=T),
                            union_member = factor(c(0,1))))

new_dta1.3<-cbind(new_dta1.3,predict(r1.3, newdata = new_dta1.3, type = "response", se.fit=T))

names(new_dta1.3)[names(new_dta1.3)=="fit"] = "prob"
names(new_dta1.3)[names(new_dta1.3)=="se.fit"] = "se.prob"

new_dta1.3$ll = new_dta1.3$prob - 1.96*new_dta1.3$se.prob
new_dta1.3$ul = new_dta1.3$prob + 1.96*new_dta1.3$se.prob


new_dta1.3$Affiliation<-c("Non-member","Union Member")
new_dta1.3$office<-c("Senate","Senate")

######################################
## Predicted Probability Model 1.4 Lower House  ##
######################################

new_dta1.4<-with(dta, 
                 data.frame(Woman = calculate_mode(Woman), 
                            Age = mean(Age, na.rm=T),
                            Urban = calculate_mode(Urban),
                            Economic_outlook = calculate_mode(Economic_outlook),
                            income = mean(income, na.rm=T),
                            union_member = factor(c(0,1))))

new_dta1.4<-cbind(new_dta1.4,predict(r1.4, newdata = new_dta1.4, type = "response", se.fit=T))

names(new_dta1.4)[names(new_dta1.4)=="fit"] = "prob"
names(new_dta1.4)[names(new_dta1.4)=="se.fit"] = "se.prob"

new_dta1.4$ll = new_dta1.4$prob - 1.96*new_dta1.4$se.prob
new_dta1.4$ul = new_dta1.4$prob + 1.96*new_dta1.4$se.prob

new_dta1.4$Affiliation<-c("Non-member","Union Member")
new_dta1.4$office<-c("Lower House","Lower House")

######################################
## Predicted Probability Model 1.5 City Council  ##
######################################

new_dta1.5<-with(dta, 
                 data.frame(Woman = calculate_mode(Woman), 
                            Age = mean(Age, na.rm=T),
                            Urban = calculate_mode(Urban),
                            Economic_outlook = calculate_mode(Economic_outlook),
                            income = mean(income, na.rm=T),
                            union_member = factor(c(0,1))))

new_dta1.5<-cbind(new_dta1.5,predict(r1.5, newdata = new_dta1.5, type = "response", se.fit=T))

names(new_dta1.5)[names(new_dta1.5)=="fit"] = "prob"
names(new_dta1.5)[names(new_dta1.5)=="se.fit"] = "se.prob"

new_dta1.5$ll = new_dta1.5$prob - 1.96*new_dta1.5$se.prob
new_dta1.5$ul = new_dta1.5$prob + 1.96*new_dta1.5$se.prob

new_dta1.5$Affiliation<-c("Non-member","Union Member")
new_dta1.5$office<-c("City Council","City Council")

######################################
## Predicted Probability Model 1.6 Local Council  ##
######################################

new_dta1.6<-with(dta, 
                 data.frame(Woman = calculate_mode(Woman), 
                            Age = mean(Age, na.rm=T),
                            Urban = calculate_mode(Urban),
                            Economic_outlook = calculate_mode(Economic_outlook),
                            income = mean(income, na.rm=T),
                            union_member = factor(c(0,1))))

new_dta1.6<-cbind(new_dta1.6,predict(r1.6, newdata = new_dta1.6, type = "response", se.fit=T))

names(new_dta1.6)[names(new_dta1.6)=="fit"] = "prob"
names(new_dta1.6)[names(new_dta1.6)=="se.fit"] = "se.prob"

new_dta1.6$ll = new_dta1.6$prob - 1.96*new_dta1.6$se.prob
new_dta1.6$ul = new_dta1.6$prob + 1.96*new_dta1.6$se.prob

new_dta1.6$Affiliation<-c("Non-member","Union Member")
new_dta1.6$office<-c("Local Council","Local Council")

######################################
## Predicted Probability Model 2.1 President  ##
######################################

new_dta2.1<-with(dta, 
                 data.frame(Woman = calculate_mode(Woman), 
                            Age = mean(Age, na.rm=T),
                            Urban = calculate_mode(Urban),
                            Economic_outlook = calculate_mode(Economic_outlook),
                            income = mean(income, na.rm=T),
                            party_member = 1,
                            union_member = factor(c(0,1)), 
                            Left = 1))

new_dta2.1<-cbind(new_dta2.1,predict(r2.1, newdata = new_dta2.1, type = "response", se.fit=T))

names(new_dta2.1)[names(new_dta2.1)=="fit"] = "prob"
names(new_dta2.1)[names(new_dta2.1)=="se.fit"] = "se.prob"

new_dta2.1$ll = new_dta2.1$prob - 1.96*new_dta2.1$se.prob
new_dta2.1$ul = new_dta2.1$prob + 1.96*new_dta2.1$se.prob

new_dta2.1$Affiliation<-c("Non-member","Union Member")
new_dta2.1$office<-c("President","President")

######################################
## Predicted Probability Model 2.2 Mayor  ##
######################################

new_dta2.2<-with(dta, 
                 data.frame(Woman = calculate_mode(Woman), 
                            Age = mean(Age, na.rm=T),
                            Urban = calculate_mode(Urban),
                            Economic_outlook = calculate_mode(Economic_outlook),
                            income = mean(income, na.rm=T),
                            party_member = 1,
                            union_member = factor(c(0,1)), 
                            Left = 1))

new_dta2.2<-cbind(new_dta2.2,predict(r2.2, newdata = new_dta2.2, type = "response", se.fit=T))

names(new_dta2.2)[names(new_dta2.2)=="fit"] = "prob"
names(new_dta2.2)[names(new_dta2.2)=="se.fit"] = "se.prob"

new_dta2.2$ll = new_dta2.2$prob - 1.96*new_dta2.2$se.prob
new_dta2.2$ul = new_dta2.2$prob + 1.96*new_dta2.2$se.prob


new_dta2.2$Affiliation<-c("Non-member","Union Member")
new_dta2.2$office<-c("Mayor","Mayor")

######################################
## Predicted Probability Model 2.3 Senate  ##
######################################

new_dta2.3<-with(dta, 
                 data.frame(Woman = calculate_mode(Woman), 
                            Age = mean(Age, na.rm=T),
                            Urban = calculate_mode(Urban),
                            Economic_outlook = calculate_mode(Economic_outlook),
                            income = mean(income, na.rm=T),
                            party_member = 1,
                            union_member = factor(c(0,1)), 
                            Left = 1))

new_dta2.3<-cbind(new_dta2.3,predict(r2.3, newdata = new_dta2.3, type = "response", se.fit=T))

names(new_dta2.3)[names(new_dta2.3)=="fit"] = "prob"
names(new_dta2.3)[names(new_dta2.3)=="se.fit"] = "se.prob"

new_dta2.3$ll = new_dta2.3$prob - 1.96*new_dta2.3$se.prob
new_dta2.3$ul = new_dta2.3$prob + 1.96*new_dta2.3$se.prob

new_dta2.3$Affiliation<-c("Non-member","Union Member")
new_dta2.3$office<-c("Senate","Senate")

######################################
## Predicted Probability Model 2.4 Lower House  ##
######################################

new_dta2.4<-with(dta, 
                 data.frame(Woman = calculate_mode(Woman), 
                            Age = mean(Age, na.rm=T),
                            Urban = calculate_mode(Urban),
                            Economic_outlook = calculate_mode(Economic_outlook),
                            income = mean(income, na.rm=T),
                            party_member = 1,
                            union_member = factor(c(0,1)), 
                            Left = 1))

new_dta2.4<-cbind(new_dta2.4,predict(r2.4, newdata = new_dta2.4, type = "response", se.fit=T))

names(new_dta2.4)[names(new_dta2.4)=="fit"] = "prob"
names(new_dta2.4)[names(new_dta2.4)=="se.fit"] = "se.prob"

new_dta2.4$ll = new_dta2.4$prob - 1.96*new_dta2.4$se.prob
new_dta2.4$ul = new_dta2.4$prob + 1.96*new_dta2.4$se.prob



new_dta2.4$Affiliation<-c("Non-member","Union Member")
new_dta2.4$office<-c("Lower House","Lower House")

######################################
## Predicted Probability Model 2.5 City Council  ##
######################################

new_dta2.5<-with(dta, 
                 data.frame(Woman = calculate_mode(Woman), 
                            Age = mean(Age, na.rm=T),
                            Urban = calculate_mode(Urban),
                            Economic_outlook = calculate_mode(Economic_outlook),
                            income = mean(income, na.rm=T),
                            party_member = 1,
                            union_member = factor(c(0,1)), 
                            Left = 1))

new_dta2.5<-cbind(new_dta2.5,predict(r2.5, newdata = new_dta2.5, type = "response", se.fit=T))

names(new_dta2.5)[names(new_dta2.5)=="fit"] = "prob"
names(new_dta2.5)[names(new_dta2.5)=="se.fit"] = "se.prob"

new_dta2.5$ll = new_dta2.5$prob - 1.96*new_dta2.5$se.prob
new_dta2.5$ul = new_dta2.5$prob + 1.96*new_dta2.5$se.prob

new_dta2.5$Affiliation<-c("Non-member","Union Member")
new_dta2.5$office<-c("City Council","City Council")

######################################
## Predicted Probability Model 2.6 Local Council  ##
######################################

new_dta2.6<-with(dta, 
                 data.frame(Woman = calculate_mode(Woman), 
                            Age = mean(Age, na.rm=T),
                            Urban = calculate_mode(Urban),
                            Economic_outlook = calculate_mode(Economic_outlook),
                            income = mean(income, na.rm=T),
                            party_member = 1,
                            union_member = factor(c(0,1)), 
                            Left = 1))

new_dta2.6<-cbind(new_dta2.6,predict(r2.6, newdata = new_dta2.6, type = "response", se.fit=T))

names(new_dta2.6)[names(new_dta2.6)=="fit"] = "prob"
names(new_dta2.6)[names(new_dta2.6)=="se.fit"] = "se.prob"

new_dta2.6$ll = new_dta2.6$prob - 1.96*new_dta2.6$se.prob
new_dta2.6$ul = new_dta2.6$prob + 1.96*new_dta2.6$se.prob


new_dta2.6$Affiliation<-c("Non-member","Union Member")
new_dta2.6$office<-c("Local Council","Local Council")

######################################
## Combining Predicted Probability Models 1.1-1.6
######################################

new_dta1.all<-rbind(new_dta1.1,new_dta1.2,new_dta1.3,new_dta1.4,new_dta1.5,new_dta1.6)

new_dta1.6$Affiliation<-as.factor(new_dta1.6$Affiliation)
new_dta1.all$office <- as.factor(new_dta1.all$office)
new_dta1.all$office <- factor(new_dta1.all$office, ordered =T, levels = c("President", "Mayor", 
                                                                 "Senate","Lower House", 
                                                                 "City Council", "Local Council"))


######################################
## Combining Predicted Probability Models 2.1-2.6
######################################

new_dta2.all<-rbind(new_dta2.1,new_dta2.2,new_dta2.3,new_dta2.4,new_dta2.5,new_dta2.6)
new_dta2.all
new_dta2.6$Affiliation<-as.factor(new_dta2.6$Affiliation)

new_dta2.all$office <- as.factor(new_dta2.all$office)
new_dta2.all$office <- factor(new_dta2.all$office, ordered =T, levels = c("President", "Mayor", 
                                                                          "Senate","Lower House", 
                                                                          "City Council", "Local Council"))

######################################

## Figure 2. Predicted Probabilities for Motivation to Vote

######################################

plot_a<-ggplot(new_dta1.all, aes(x= office, y=prob, fill=Affiliation)) +
  geom_bar(stat="identity", position="dodge", width=0.5) +
  theme(axis.text.x = element_text(angle = 90), axis.title.x = element_blank(),axis.title.y = element_blank()) +
  theme(legend.position = "none") +
  ggtitle("Demographic Controls") +  
  ylim(0,0.8)

plot_a
ggsave(file="plot_a.tiff", height = 4.29, width =3.22,dpi=300)


plot_b<-ggplot(new_dta2.all, aes(x= office, y=prob, fill=Affiliation)) +
  geom_bar(stat="identity", position="dodge", width=0.5) +
  theme(axis.text.x = element_text(angle = 90), axis.title.x = element_blank(),axis.title.y = element_blank()) +
  ggtitle("All Controls")  + 
  ylim(0,0.8)

ggsave(file="plot_b.tiff", height = 4.29, width =5,dpi=300)

