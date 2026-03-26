rm(list=ls(all=TRUE))

library(geepack)
library(Matrix)
library(MASS)
library(ggplot2)
library(grDevices)
library(graphics)
library(stats)
library(methods)
library(CRTgeeDR)

#source(file="P:/R/crtgeedr updated/R/CRTgeeDR-main.R")
#source(file="P:/R/crtgeedr updated/R/getFay.R")
#source(file="P:/R/crtgeedr updated/R/getSandwich.R")
#source(file="P:/R/crtgeedr updated/R/getSandwichNuisance.R")
#source(file="P:/R/crtgeedr updated/R/numDeriv.R")
#source(file="P:/R/crtgeedr updated/R/print.R")
#source(file="P:/R/crtgeedr updated/R/updatealpha.R")
#source(file="P:/R/crtgeedr updated/R/updatebeta.R")
#source(file="P:/R/crtgeedr updated/R/updatematrices.R")
#source(file="P:/R/crtgeedr updated/R/userstruct.R")
#source(file="P:/R/crtgeedr updated/R/updatephi.R")
#source(file="P:/R/crtgeedr updated/R/utility.R")

data<-read.table(file="C:/Users/sprasad/Dropbox (3IE)/Projects/Replication Work/CP TasP/Data/SAS/base v2.2 19jun2017.csv",header=T,sep=",",dec=".")
  #Note: Update filepath above as needed
head(data)
data <- data[order(data$cluster),] 
table(data$cluster,data$time)
data$TRT<-data$arm
data$time<-ifelse(data$cluster%in%c(5,6,8,10,11,13),data$time-1,data$time)
data$time<-ifelse(data$cluster%in%c(7,9,12,14,15,16,17,18,19,20,21,22),data$time-2,data$time)

#UPDATE OF AGE PERCENTAGE TO EXCLUDE MISSING DATA
data$size<-data$age_16_29+data$age_30_59+data$age_60
data$AGE_16_29_fin<-data$age_16_29*100/data$size
data$AGE_30_59_fin<-data$age_30_59*100/data$size
data$AGE_60_fin<-data$age_60*100/data$size

head(data)
str(data)

#CHECK WITH REGULAR GEEFROM SAS
gee_geepack<-geeglm(formula=nb_evt~TRT+offset(lnpy),
                    id=cluster,
                    data=data,
                    family=poisson(link="log"),
                    corstr="exchangeable")
summary(gee_geepack)


#CHECK WITH OUR PACKAGE
gee_CRTgeeDR<-geeDREstimation(formula=nb_evt~TRT+offset(lnpy),
                              id="cluster",
                              data=data,
                              family=poisson(link="log"),
                              corstr="exchangeable")
summary(gee_CRTgeeDR)

data0<-data
#AUGMENTATION TERM PRE-CALCULATION
B0<-geeglm(formula=nb_evt~AGE_16_29_fin+AGE_60_fin+female_pct+WHO_guidelines+artcoverage_bas+prevalence_bas+offset(lnpy),
                    id=cluster,
                    data=data[which(data$TRT==0),],
                    family=poisson(link="log"),
                    corstr="exchangeable")

B1<-geeglm(formula=nb_evt~AGE_16_29_fin+AGE_60_fin+female_pct+WHO_guidelines+artcoverage_bas+prevalence_bas+offset(lnpy),
           id=cluster,
           data=data[which(data$TRT==1),],
           family=poisson(link="log"),
           corstr="exchangeable")

##############################
##############################
####### NEEDED FOR REPORT
# TO SEE THE AUGMENTATION TERM IN THE CONTROL GROUP
summary(B0)
# TO SEE THE AUGMENTATION TERM IN THE INTERVENTION GROUP
summary(B1)
##############################
##############################

data1<-data
data0$TRT<-0
data1$TRT<-1
data$B0aug<-exp(predict(B0,newdata=data0))
data$B1aug<-exp(predict(B1,newdata=data1))

#AUGMENTED GEE
AUGgee_CRTgeeDR<-geeDREstimation(formula=nb_evt~TRT+offset(lnpy),
                                 id="cluster",
                                 data=data,
                                 aug=c(ctrl="B0aug",trt="B1aug") ,
                                 family=poisson(link="log"),
                                 corstr="exchangeable")
##############################
##############################
####### NEEDED FOR REPORT
summary(AUGgee_CRTgeeDR)
##############################
##############################
