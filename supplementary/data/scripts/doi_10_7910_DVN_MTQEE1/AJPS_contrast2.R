#Contrast 2
#This file uses EBAL to estimate the balance weights.  Input data
#set is generated in Stata.  After entropy weights are estimated, they
#are merged with a data set containing the control cases.  This data set
#is merged in Stata with the treated cases.  It is in Stata that the #proportional odds models are estimated.

#First Generation Bilingual
 
# Loading packages:

library(foreign)
library(MatchIt)
library(Matching)
library(ebal)


#setwd("/Users/bsjjones/Dropbox/AJPS R&R/Analysis Files")


#setwd("/Users/bradfordjones/Dropbox/AJPS R&R/Analysis Files")


pewdata <- read.dta("entropycontrast2.dta")
attach(pewdata)

X<-cbind(region, female, married,  havechildren, employed, repub, democrat, independent, 
hsorless, under30, over55,
regionXemployed,
regionXhavechildren,
regionXfemale,
regionXmarried,
regionXhsorless,
regionXunder30,
regionXover55,
havechildrenXemployed,
 havechildrenXfemale,
  havechildrenXmarried,
  havechildrenXrepub,
  havechildrenXdemocrat,
  havechildrenXindependent,
  havechildrenXhsorless,
  havechildrenXunder30,
  havechildrenXover55,
  employedXfemale,
  employedXmarried,
  employedXrepub,
  employedXdemocrat,
  employedXindependent,
  employedXhsorless,
  employedXunder30,
  employedXover55,
  femaleXmarried,
  femaleXrepub,
  femaleXdemocrat,
  femaleXindependent,
  femaleXhsorless,
  femaleXunder30,
  femaleXover55,
  metro, cmex, ccuba, ccar, ccentral)
#Balancing Procedure  
eb.out<- ebalance(Treatment=noncitizen, X=X)
 
#Trimming the weights 
 eb.out.tr<-ebalance.trim(eb.out) 
  
e_weight<-eb.out$w
t_weight<-eb.out.tr$w
 

round(summary(eb.out$w),2)
round(summary(eb.out.tr$w),2)



#Reading in Control data created in Stata#


 


control<-read.dta("entropycontrol_contrast2.dta")

control[control == ""] <- NA


#Merging Weights with Control Data
control.data<-cbind(control, e_weight, t_weight)

data<-data.frame(control.data)
#Writing out control data to append to treatment data in Stata

#write.dta(control.data, "/Users/bsjjones/Dropbox/AJPS R&R/Analysis Files/control_1GBI.dta")

#write.dta(control.data, "/Users/bradfordjones/Dropbox/AJPS R&R/Analysis Files/control_1GBI.dta")
write.dta(control.data, "control_1GBI.dta")



table(noncitizen)
table(noncitizen, year)
rm(list = ls())
