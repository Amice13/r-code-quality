##########################################################
## National Identity, Social Preferences, and Foreign 
## Policy Attitudes: Experimental Evidence from Japan
## Foreign Policy Analysis, 2021.
## Prepared "Wed Sep 22 10:51:15 2021"
##########################################################


##########################################################
## Disclaimer:
## The analysis was conducted using RStudio version 1.2.5033 
## on Mac Mojave OS 10.14.6
## Replication on a different OS
## might require additional corrections. 
##########################################################


#############################
## load data and packages
#############################
library(memisc)
library(foreign)
library(mediation)
MyData <- read.spss("/Users/u6002209/Downloads/Chung_FPA_Prosociality.sav") #insert data location before file name


#############################
## adding # to data set. 
## so prosocial 0=proself, 1=prosocial
#############################
Recode <- MyData$Prosocial
Recode <- Recode-1
MyData$ProRecode <- Recode

#############################
## Main finding 1 (Figure 3)
## Mediation analysis for 
## Cooperative foreign policy
## Code for 1000 sims; note that numbers may slightly differ each time
#############################
MonX <- glm(Prosociality~NIA,data=MyData,family=binomial("logit")) #logistic regression for M 0: proself, 1: prosocial
summary(MonX)
YonXM <- lm(CoopFP~NIA+Prosociality,data=MyData) #linear regression
summary(YonXM)

med.output <- mediate(MonX,YonXM,treat="NIA",mediator="Prosociality",
                      boot=TRUE,sims=1000,dropobs=TRUE)
summary(med.output)

#############################
## Main finding 2 (Figure 4)
## Mediation analysis for 
## Militaristic foreign policy 
## Code for 1000 sims; note that numbers may slightly differ each time
#############################
MonX <- glm(Prosociality~NIA,data=MyData,family=binomial("logit")) #logistic regression for M 0/1
summary(MonX)
YonXM <- lm(MilitFP~NIA+Prosociality,data=MyData) #linear regression
summary(YonXM)

med.output <- mediate(MonX,YonXM,treat="NIA",mediator="Prosociality",
                      boot=TRUE,sims=1000,dropobs=TRUE)
summary(med.output)

#############################
## Main finding 3 (Table 2)
## Logistic regression analysis  
## Prosociality by different affirmation types
#############################

mylogit <- glm(Prosociality~Affrm_Condition,data=MyData,family=binomial("logit")) #logistic regression 
summary(mylogit)

#############################
## Main finding 4 (Table 3)
## Linear regression analysis  
## Militaristic Foreign Policy by National Chauvinism
#############################

mod1 <- lm(MilitFP~Chauvinism,data=MyData) 
summary(mod1)

mod2 <- lm(MilitFP~Q381,data=MyData) 
summary(mod2)

mod3 <- lm(MilitFP~Q382,data=MyData) 
summary(mod3)

mod4 <- lm(MilitFP~Q383,data=MyData) 
summary(mod4)

mod5 <- lm(MilitFP~Q384,data=MyData) 
summary(mod5)