### What Lies Beneath: Mediators of Voter Support for International Economic Cooperation 
### Date: September 22, 2019
### Authors: Quynh Nguyen (ANU), Gabriele Spilker (University of Salzburg)
### R Script for Replication


rm(list=ls())
pacman::p_load(foreign, ggplot2, plm, dplyr, reshape2, countrycode, sandwich, lmtest, MASS, 
               rworldmap, RColorBrewer, states, mice, VIM, stargazer, margins, clusterSEs, lme4, optimx,
               coefplot, survey, sampleSelection)

library(mediation)
update.packages("mediation")
library(dummy)
library(psych)
library(gridExtra)


####### GERMANY
## Replication dataset: dfrepger 

##Input data
dfrepger <- read.csv("~/Dropbox/TTIP/Paper 2/Analysis/Replication data/Germany.csv") 

#####CMA - DV: ttipatt2, MedV: annoyed
###CMA - Step 1

dfger.sub1 <- subset(dfrepger,!is.na(ttipatt2))

model1 <- lm(ttipatt2 ~ treat + age + female + education + income + ttipinterest, data = dfger.sub1)
model2 <- lm(annoyed ~ treat + age + female + education + income + ttipinterest, data = dfger.sub1)
model3 <- lm(ttipatt2 ~ treat + annoyed + age + female + education + income + ttipinterest, data = dfger.sub1)

confint(model2)
confint(model3)

###CMA - Step 2 (Bootstrap method 
med1 <- mediation::mediate(model2, model3, boot = TRUE, treat = "treat", mediator = "annoyed", 
                           sims = 1000) 
summary(med1)

##Plot results 
plot(med1, xlim = c(-2,2), main = "Framing Effect Mediated by Annoyance", xlab = "Effect size", col = c("darkturquoise"))


#####CMA - DV: ttipatt2, MedV: confident
###CMA - Step 1

model4 <- lm(ttipatt2 ~ treat + age + female + education + income + ttipinterest, data = dfger.sub1)
model5 <- lm(confident ~ treat + age + female + education + income + ttipinterest, data = dfger.sub1)
model6 <- lm(ttipatt2 ~ treat + confident + age + female + education + income + ttipinterest, data = dfger.sub1)

confint(model5)
confint(model6)

###CMA - Step 2
##Bootstrap 
med2 <- mediation::mediate(model5, model6, boot = TRUE, treat = "treat", 
                           mediator = "confident", sims = 1000) 
summary(med2)

##Plot results 
plot(med2, xlim = c(-2,2), main = "Framing Effect Mediated by Confidence", xlab = "Effect size", col = c("darkturquoise"))


###Sensitivity analysis 

#Annoyance
sens1 <- medsens(med1, rho.by = 0.05) 
summary(sens1)

#Plot results
plot(sens1, sens.par = "rho", main = "Annoyance", ylim =c(-3, 2.5))

#Confidence 
sens2 <- medsens(med2, rho.by = 0.05) 
summary(sens2)

#Plot results 
plot(sens2, sens.par = "rho", main = "Confidence", ylim =c(-3, 2.5))


############
######################
####USA 
## Replication dataset: dfrepus 

##Input data
dfrepus <- read.csv("~/Dropbox/TTIP/Paper 2/Analysis/Replication data/USA.csv") 

#####CMA - DV: ttipatt2, MedV: annoyed
###CMA - Step 1

dfus.sub1 <- subset(dfrepus,!is.na(ttipatt2))

modela <- lm(ttipatt2 ~ treat + age + female + education + income + ttipinterest, data = dfus.sub1)
modelb <- lm(annoyed ~ treat + age + female + education + income + ttipinterest, data = dfus.sub1)
modelc <- lm(ttipatt2 ~ treat + annoyed + age + female + education + income + ttipinterest, data = dfus.sub1)

confint(modelb)
confint(modelc)

###CMA - Step 2
##Bootstrap 
med3 <- mediation::mediate(modelb, modelc, boot = TRUE, treat = "treat", mediator = "annoyed", 
                           sims = 1000) 
summary(med3)

##Plot results 
plot(med3, xlim = c(-2,1), main = "Framing Effect Mediated by Annoyance", xlab = "Effect size", col = c("tomato"))


#####CMA - DV: ttipatt2, MedV: confident
###CMA - Step 1

modeld <- lm(ttipatt2 ~ treat + age + female + education + income + ttipinterest, data = dfus.sub1)
modele <- lm(confident ~ treat + age + female + education + income + ttipinterest, data = dfus.sub1)
modelf <- lm(ttipatt2 ~ treat + confident + age + female + education + income + ttipinterest, data = dfus.sub1)

confint(modele)
confint(modelf)

###CMA - Step 2
##Bootstrap 
med4 <- mediation::mediate(modele, modelf, boot = TRUE, treat = "treat", mediator = "confident", 
                           sims = 1000) 
summary(med4)

##Plot results 
plot(med4, xlim = c(-2,1), main = "Framing Effect Mediated by Confidence", xlab = "Effect size", col = c("tomato"))


###Sensitivity analysis 

#Annoyance
sens3 <- medsens(med3, rho.by = 0.05) 
summary(sens3)

##Plot results 
plot(sens3, sens.par = "rho", main = "Annoyance", ylim =c(-4.5, 3))

#Confidence 
sens4 <- medsens(med4, rho.by = 0.05) 
summary(sens4)

##Plot results 
plot(sens4, sens.par = "rho", main = "Confidence", ylim =c(-4.5, 3))

################################################
############################


