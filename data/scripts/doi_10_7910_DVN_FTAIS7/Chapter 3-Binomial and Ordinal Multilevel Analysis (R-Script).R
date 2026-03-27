### Chapter 3

### In this chapter, based on the technique of principal components analysis, 
### I will examine how associated the policy stances of legislators in contemporary Brazil. 
### I have compiled a dataset with the Brazilian Legislative Surveys (1997 - 2013).

### TASK 1: Data Preparation
### TASK 2: Binomial and Ordinal Multilevel Analysis

rm(list=ls(all=TRUE))

### TASK 2: Multilevel Regression Analysis
### TASK 1: Data Preparation
library(lme4)
library(ordinal)
library(tidyverse)  #for data manipulation
library(lme4)       #for constructing generalised linear model
library(knitr)      #for pretty tables
library(sjstats)    #for the estimation of ICC
library(broom)      #for tidying model output
library(ggeffects)  #for plotting predicted probabilities

MyData <- subset(`Chapter.3.Binomial.and.Ordinal.Multilevel.Analysis.(Dataset)`, 
                 select = c(region, 
                            wave, 
                            party_elected, 
                            fidelit, believe, efforts, needmps_all, ffaaint, ppswitch_all,                                        #Political Dimension
                            econlmr,                                                                                              #Economic Dimension
                            clients_all, ppvsreg, family, vereador, depest, senador, depfed, prefvice, govvice, ministro, secest, #Traditional Dimension
                            lrclass                                                                                               #Left-Right
                            ))

MyData$party_elected[ MyData$party_elected == "99"] <- NA
MyData$lrclass[ MyData$lrclass == "99"] <- NA
MyData$fidelit[ MyData$fidelit == "9"] <- NA
MyData$needmps_all[ MyData$needmps_all == "9"] <- NA
MyData$believe[ MyData$believe == "9"] <- NA
MyData$believe[ MyData$believe == "8"] <- NA
MyData$efforts[ MyData$efforts == "9"] <- NA
MyData$efforts[ MyData$efforts == "8"] <- NA
MyData$ppswitch_all[ MyData$ppswitch_all == "9"] <- NA
MyData$ffaaint[ MyData$ffaaint == "9"] <- NA
MyData$econlmr[ MyData$econlmr == "9"] <- NA
MyData$clients_all[ MyData$clients_all == "9"] <- NA
MyData$ppvsreg[ MyData$ppvsreg == "3"] <- NA
MyData$ppvsreg[ MyData$ppvsreg == "9"] <- NA
MyData$family[ MyData$family == "9"] <- NA
MyData$vereador[ MyData$vereador == "9"] <- NA
MyData$depest[ MyData$depest == "9"] <- NA
MyData$senador[ MyData$senador == "9"] <- NA
MyData$depfed[ MyData$depfed == "9"] <- NA
MyData$prefvice[ MyData$prefvice == "9"] <- NA
MyData$govvice[ MyData$govvice == "9"] <- NA
MyData$ministro[ MyData$ministro == "9"] <- NA
MyData$secest[ MyData$secest == "9"] <- NA
MyData$region[ MyData$region == "9"] <- NA
MyData$lrclass[ MyData$lrclass == "3.5"] <- 3
MyData$lrclass[ MyData$lrclass == "5.5"] <- 5

colnames(MyData) <- c("REGION", 
                      "YEAR", 
                      "PARTY", 
                      "FIDELITY", "BELIEVE", "EFFORTS", "DECREE", "MILITARY", "SWITCH",
                      "ECONOMY",
                      "CLIENTELISM", "LOCAL INTERESTS", "FAMILY", "COUNCILLOR", "STATE DEPUTY", "SENATOR", "FEDERAL DEPUTY", "MAYOR", "GOVERNOR", "MINISTER", "STATE SECRETARY",
                      "LEFT RIGHT")

MyData$FIDELITY <- as.factor(MyData$FIDELITY)
MyData$BELIEVE <- as.factor(MyData$BELIEVE)
MyData$EFFORTS <- as.factor(MyData$EFFORTS)
MyData$DECREE <- as.factor(MyData$DECREE)
MyData$MILITARY <- as.factor(MyData$MILITARY)
MyData$SWITCH <- as.factor(MyData$SWITCH)
MyData$ECONOMY <- as.factor(MyData$ECONOMY)
MyData$CLIENTELISM <- as.factor(MyData$CLIENTELISM)
MyData$`LOCAL INTERESTS` <- as.factor(MyData$`LOCAL INTERESTS`)
MyData$FAMILY <- as.factor(MyData$FAMILY)
MyData$COUNCILLOR <- as.factor(MyData$COUNCILLOR)
MyData$`STATE DEPUTY` <- as.factor(MyData$`STATE DEPUTY`)
MyData$SENATOR <- as.factor(MyData$SENATOR)
MyData$`FEDERAL DEPUTY` <- as.factor(MyData$`FEDERAL DEPUTY`)
MyData$MAYOR <- as.factor(MyData$MAYOR)
MyData$GOVERNOR <- as.factor(MyData$GOVERNOR)
MyData$MINISTER <- as.factor(MyData$MINISTER)
MyData$`STATE SECRETARY` <- as.factor(MyData$`STATE SECRETARY`)
MyData$`LEFT RIGHT` <- as.factor(MyData$`LEFT RIGHT`)

PRB <- MyData[ which(MyData$PARTY == "10"),]
PP2003 <- MyData[ which(MyData$PARTY == "11"),]
PDT <- MyData[ which(MyData$PARTY == "12"),]
PT <- MyData[ which(MyData$PARTY == "13"),]
PTB <- MyData[ which(MyData$PARTY == "14"),]
PMDB <- MyData[ which(MyData$PARTY == "15"),]
PDC <- MyData[ which(MyData$PARTY == "17"),]
PSC <- MyData[ which(MyData$PARTY == "20"),]
PR <- MyData[ which(MyData$PARTY == "22"),]
PPS <- MyData[ which(MyData$PARTY == "23"),]
DEM <- MyData[ which(MyData$PARTY == "25"),]
PMN <- MyData[ which(MyData$PARTY == "33"),]
PRN <- MyData[ which(MyData$PARTY == "36"),]
PP1993 <- MyData[ which(MyData$PARTY == "39"),]
PSB <- MyData[ which(MyData$PARTY == "40"),]
PV <- MyData[ which(MyData$PARTY == "43"),]
PSDB <- MyData[ which(MyData$PARTY == "45"),]
PSOL <- MyData[ which(MyData$PARTY == "50"),]
PCdoB <- MyData[ which(MyData$PARTY == "65"),]
Other <- MyData[ which(MyData$PARTY == "88"),]

# System Model (including all selected samples of all political parties under analysis)
MyData2 <- na.omit(MyData)

FIDELITY <- glmer(FIDELITY ~ 1 + (1 | REGION), family = binomial, data = MyData2, nAGQ = 10)
summary(FIDELITY)
sum(insight::get_variance_intercept(FIDELITY)) / 
  (sum(insight::get_variance_intercept(FIDELITY)) + insight::get_variance_residual(FIDELITY)) 

BELIEVE <- glmer(BELIEVE ~ 1 + (1 | REGION), family = binomial, data = MyData2, nAGQ = 10)
summary(BELIEVE)
sum(insight::get_variance_intercept(BELIEVE)) / 
  (sum(insight::get_variance_intercept(BELIEVE)) + insight::get_variance_residual(BELIEVE)) 

EFFORTS <- glmer(EFFORTS ~ 1 + (1 | REGION), family = binomial, data = MyData2, nAGQ = 10)
summary(EFFORTS)
sum(insight::get_variance_intercept(EFFORTS)) / 
  (sum(insight::get_variance_intercept(EFFORTS)) + insight::get_variance_residual(EFFORTS)) 

DECREE <- glmer(DECREE ~ 1 + (1 | REGION), family = binomial, data = MyData2, nAGQ = 10)
summary(DECREE)
sum(insight::get_variance_intercept(DECREE)) / 
  (sum(insight::get_variance_intercept(DECREE)) + insight::get_variance_residual(DECREE)) 

MILITARY <- glmer(MILITARY ~ 1 + (1 | REGION), family = binomial, data = MyData2, nAGQ = 10)
summary(MILITARY)
sum(insight::get_variance_intercept(MILITARY)) / 
  (sum(insight::get_variance_intercept(MILITARY)) + insight::get_variance_residual(MILITARY)) 

SWITCH <- glmer(SWITCH ~ 1 + (1 | REGION), family = binomial, data = MyData2, nAGQ = 10)
summary(SWITCH)
sum(insight::get_variance_intercept(SWITCH)) / 
  (sum(insight::get_variance_intercept(SWITCH)) + insight::get_variance_residual(SWITCH)) 

ECONOMY <- clmm(ECONOMY ~ 1 + (1 | REGION), Hess = TRUE, data = MyData2, nAGQ = 10)
summary(ECONOMY)
sum(insight::get_variance_intercept(ECONOMY)) / 
  (sum(insight::get_variance_intercept(ECONOMY)) + insight::get_variance_residual(ECONOMY)) 

CLIENTELISM <- glmer(CLIENTELISM ~ 1 + (1 | REGION), family = binomial, data = MyData2, nAGQ = 10)
summary(CLIENTELISM)
sum(insight::get_variance_intercept(CLIENTELISM)) / 
  (sum(insight::get_variance_intercept(CLIENTELISM)) + insight::get_variance_residual(CLIENTELISM)) 

LINTERESTS <- glmer(`LOCAL INTERESTS` ~ 1 + (1 | REGION), family = binomial, data = MyData2, nAGQ = 10)
summary(LINTERESTS)
sum(insight::get_variance_intercept(LINTERESTS)) / 
  (sum(insight::get_variance_intercept(LINTERESTS)) + insight::get_variance_residual(LINTERESTS)) 

FAMILY <- glmer(FAMILY ~ 1 + (1 | REGION), family = binomial, data = MyData2, nAGQ = 10)
summary(FAMILY)
sum(insight::get_variance_intercept(FAMILY)) / 
  (sum(insight::get_variance_intercept(FAMILY)) + insight::get_variance_residual(FAMILY)) 

COUNCILLOR <- glmer(COUNCILLOR ~ 1 + (1 | REGION), family = binomial, data = MyData2, nAGQ = 10)
summary(COUNCILLOR)
sum(insight::get_variance_intercept(COUNCILLOR)) / 
  (sum(insight::get_variance_intercept(COUNCILLOR)) + insight::get_variance_residual(COUNCILLOR)) 

SDEPUTY <- glmer(`STATE DEPUTY` ~ 1 + (1 | REGION), family = binomial, data = MyData2, nAGQ = 10)
summary(SDEPUTY)
sum(insight::get_variance_intercept(SDEPUTY)) / 
  (sum(insight::get_variance_intercept(SDEPUTY)) + insight::get_variance_residual(SDEPUTY)) 

SENATOR <- glmer(SENATOR ~ 1 + (1 | REGION), family = binomial, data = MyData2, nAGQ = 10)
summary(SENATOR)
sum(insight::get_variance_intercept(SENATOR)) / 
  (sum(insight::get_variance_intercept(SENATOR)) + insight::get_variance_residual(SENATOR)) 

FDEPUTY <- glmer(`FEDERAL DEPUTY` ~ 1 + (1 | REGION), family = binomial, data = MyData2, nAGQ = 10)
summary(FDEPUTY)
sum(insight::get_variance_intercept(FDEPUTY)) / 
  (sum(insight::get_variance_intercept(FDEPUTY)) + insight::get_variance_residual(FDEPUTY)) 

MAYOR <- glmer(MAYOR ~ 1 + (1 | REGION), family = binomial, data = MyData2, nAGQ = 10)
summary(MAYOR)
sum(insight::get_variance_intercept(MAYOR)) / 
  (sum(insight::get_variance_intercept(MAYOR)) + insight::get_variance_residual(MAYOR)) 

GOVERNOR <- glmer(GOVERNOR ~ 1 + (1 | REGION), family = binomial, data = MyData2, nAGQ = 10)
summary(GOVERNOR)
sum(insight::get_variance_intercept(GOVERNOR)) / 
  (sum(insight::get_variance_intercept(GOVERNOR)) + insight::get_variance_residual(GOVERNOR)) 

MINISTER <- glmer(MINISTER ~ 1 + (1 | REGION), family = binomial, data = MyData2, nAGQ = 10)
summary(MINISTER)
sum(insight::get_variance_intercept(MINISTER)) / 
  (sum(insight::get_variance_intercept(MINISTER)) + insight::get_variance_residual(MINISTER)) 

SSECRETARY <- glmer(`STATE SECRETARY` ~ 1 + (1 | REGION), family = binomial, data = MyData2, nAGQ = 10)
summary(SSECRETARY)
sum(insight::get_variance_intercept(SSECRETARY)) / 
  (sum(insight::get_variance_intercept(SSECRETARY)) + insight::get_variance_residual(SSECRETARY)) 

LEFTRIGHT <- clmm(`LEFT RIGHT` ~ 1 + (1 | REGION), Hess = TRUE, data = MyData2, nAGQ = 10)
summary(LEFTRIGHT)
sum(insight::get_variance_intercept(LEFTRIGHT)) / 
  (sum(insight::get_variance_intercept(LEFTRIGHT)) + insight::get_variance_residual(LEFTRIGHT)) 



# PP03
PP03 <- na.omit(PP2003)

FIDELITY <- glmer(FIDELITY ~ 1 + (1 | REGION), family = binomial, data = PP03, nAGQ = 10)
summary(FIDELITY)
sum(insight::get_variance_intercept(FIDELITY)) / 
  (sum(insight::get_variance_intercept(FIDELITY)) + insight::get_variance_residual(FIDELITY)) 

BELIEVE <- glmer(BELIEVE ~ 1 + (1 | REGION), family = binomial, data = PP03, nAGQ = 10)
summary(BELIEVE)
sum(insight::get_variance_intercept(BELIEVE)) / 
  (sum(insight::get_variance_intercept(BELIEVE)) + insight::get_variance_residual(BELIEVE)) 

EFFORTS <- glmer(EFFORTS ~ 1 + (1 | REGION), family = binomial, data = PP03, nAGQ = 10)
summary(EFFORTS)
sum(insight::get_variance_intercept(EFFORTS)) / 
  (sum(insight::get_variance_intercept(EFFORTS)) + insight::get_variance_residual(EFFORTS)) 

DECREE <- glmer(DECREE ~ 1 + (1 | REGION), family = binomial, data = PP03, nAGQ = 10)
summary(DECREE)
sum(insight::get_variance_intercept(DECREE)) / 
  (sum(insight::get_variance_intercept(DECREE)) + insight::get_variance_residual(DECREE)) 

MILITARY <- glmer(MILITARY ~ 1 + (1 | REGION), family = binomial, data = PP03, nAGQ = 10)
summary(MILITARY)
sum(insight::get_variance_intercept(MILITARY)) / 
  (sum(insight::get_variance_intercept(MILITARY)) + insight::get_variance_residual(MILITARY)) 

SWITCH <- glmer(SWITCH ~ 1 + (1 | REGION), family = binomial, data = PP03, nAGQ = 10)
summary(SWITCH)
sum(insight::get_variance_intercept(SWITCH)) / 
  (sum(insight::get_variance_intercept(SWITCH)) + insight::get_variance_residual(SWITCH)) 

ECONOMY <- clmm(ECONOMY ~ 1 + (1 | REGION), Hess = TRUE, data = PP03, nAGQ = 10)
summary(ECONOMY)
sum(insight::get_variance_intercept(ECONOMY)) / 
  (sum(insight::get_variance_intercept(ECONOMY)) + insight::get_variance_residual(ECONOMY)) 

CLIENTELISM <- glmer(CLIENTELISM ~ 1 + (1 | REGION), family = binomial, data = PP03, nAGQ = 10)
summary(CLIENTELISM)
sum(insight::get_variance_intercept(CLIENTELISM)) / 
  (sum(insight::get_variance_intercept(CLIENTELISM)) + insight::get_variance_residual(CLIENTELISM)) 

LINTERESTS <- glmer(`LOCAL INTERESTS` ~ 1 + (1 | REGION), family = binomial, data = PP03, nAGQ = 10)
summary(LINTERESTS)
sum(insight::get_variance_intercept(LINTERESTS)) / 
  (sum(insight::get_variance_intercept(LINTERESTS)) + insight::get_variance_residual(LINTERESTS)) 

FAMILY <- glmer(FAMILY ~ 1 + (1 | REGION), family = binomial, data = PP03, nAGQ = 10)
summary(FAMILY)
sum(insight::get_variance_intercept(FAMILY)) / 
  (sum(insight::get_variance_intercept(FAMILY)) + insight::get_variance_residual(FAMILY)) 

COUNCILLOR <- glmer(COUNCILLOR ~ 1 + (1 | REGION), family = binomial, data = PP03, nAGQ = 10)
summary(COUNCILLOR)
sum(insight::get_variance_intercept(COUNCILLOR)) / 
  (sum(insight::get_variance_intercept(COUNCILLOR)) + insight::get_variance_residual(COUNCILLOR)) 

SDEPUTY <- glmer(`STATE DEPUTY` ~ 1 + (1 | REGION), family = binomial, data = PP03, nAGQ = 10)
summary(SDEPUTY)
sum(insight::get_variance_intercept(SDEPUTY)) / 
  (sum(insight::get_variance_intercept(SDEPUTY)) + insight::get_variance_residual(SDEPUTY)) 

SENATOR <- glmer(SENATOR ~ 1 + (1 | REGION), family = binomial, data = PP03, nAGQ = 10)
summary(SENATOR)
sum(insight::get_variance_intercept(SENATOR)) / 
  (sum(insight::get_variance_intercept(SENATOR)) + insight::get_variance_residual(SENATOR)) 

FDEPUTY <- glmer(`FEDERAL DEPUTY` ~ 1 + (1 | REGION), family = binomial, data = PP03, nAGQ = 10)
summary(FDEPUTY)
sum(insight::get_variance_intercept(FDEPUTY)) / 
  (sum(insight::get_variance_intercept(FDEPUTY)) + insight::get_variance_residual(FDEPUTY)) 

MAYOR <- glmer(MAYOR ~ 1 + (1 | REGION), family = binomial, data = PP03, nAGQ = 10)
summary(MAYOR)
sum(insight::get_variance_intercept(MAYOR)) / 
  (sum(insight::get_variance_intercept(MAYOR)) + insight::get_variance_residual(MAYOR)) 

GOVERNOR <- glmer(GOVERNOR ~ 1 + (1 | REGION), family = binomial, data = PP03, nAGQ = 10)
summary(GOVERNOR)
sum(insight::get_variance_intercept(GOVERNOR)) / 
  (sum(insight::get_variance_intercept(GOVERNOR)) + insight::get_variance_residual(GOVERNOR)) 

MINISTER <- glmer(MINISTER ~ 1 + (1 | REGION), family = binomial, data = PP03, nAGQ = 10)
summary(MINISTER)
sum(insight::get_variance_intercept(MINISTER)) / 
  (sum(insight::get_variance_intercept(MINISTER)) + insight::get_variance_residual(MINISTER)) 

SSECRETARY <- glmer(`STATE SECRETARY` ~ 1 + (1 | REGION), family = binomial, data = PP03, nAGQ = 10)
summary(SSECRETARY)
sum(insight::get_variance_intercept(SSECRETARY)) / 
  (sum(insight::get_variance_intercept(SSECRETARY)) + insight::get_variance_residual(SSECRETARY)) 

LEFTRIGHT <- clmm(`LEFT RIGHT` ~ 1 + (1 | REGION), Hess = TRUE, data = PP03, nAGQ = 10)
summary(LEFTRIGHT)
sum(insight::get_variance_intercept(LEFTRIGHT)) / 
  (sum(insight::get_variance_intercept(LEFTRIGHT)) + insight::get_variance_residual(LEFTRIGHT)) 



# PDT
PDT1 <- na.omit(PDT)

FIDELITY <- glmer(FIDELITY ~ 1 + (1 | REGION), family = binomial, data = PDT1, nAGQ = 10)
summary(FIDELITY)
sum(insight::get_variance_intercept(FIDELITY)) / 
  (sum(insight::get_variance_intercept(FIDELITY)) + insight::get_variance_residual(FIDELITY)) 

BELIEVE <- glmer(BELIEVE ~ 1 + (1 | REGION), family = binomial, data = PDT1, nAGQ = 10)
summary(BELIEVE)
sum(insight::get_variance_intercept(BELIEVE)) / 
  (sum(insight::get_variance_intercept(BELIEVE)) + insight::get_variance_residual(BELIEVE)) 

EFFORTS <- glmer(EFFORTS ~ 1 + (1 | REGION), family = binomial, data = PDT1, nAGQ = 10)
summary(EFFORTS)
sum(insight::get_variance_intercept(EFFORTS)) / 
  (sum(insight::get_variance_intercept(EFFORTS)) + insight::get_variance_residual(EFFORTS)) 

DECREE <- glmer(DECREE ~ 1 + (1 | REGION), family = binomial, data = PDT1, nAGQ = 10)
summary(DECREE)
sum(insight::get_variance_intercept(DECREE)) / 
  (sum(insight::get_variance_intercept(DECREE)) + insight::get_variance_residual(DECREE)) 

MILITARY <- glmer(MILITARY ~ 1 + (1 | REGION), family = binomial, data = PDT1, nAGQ = 10)
summary(MILITARY)
sum(insight::get_variance_intercept(MILITARY)) / 
  (sum(insight::get_variance_intercept(MILITARY)) + insight::get_variance_residual(MILITARY)) 

SWITCH <- glmer(SWITCH ~ 1 + (1 | REGION), family = binomial, data = PDT1, nAGQ = 10)
summary(SWITCH)
sum(insight::get_variance_intercept(SWITCH)) / 
  (sum(insight::get_variance_intercept(SWITCH)) + insight::get_variance_residual(SWITCH)) 

ECONOMY <- clmm(ECONOMY ~ 1 + (1 | REGION), Hess = TRUE, data = PDT1, nAGQ = 10)
summary(ECONOMY)
sum(insight::get_variance_intercept(ECONOMY)) / 
  (sum(insight::get_variance_intercept(ECONOMY)) + insight::get_variance_residual(ECONOMY)) 

CLIENTELISM <- glmer(CLIENTELISM ~ 1 + (1 | REGION), family = binomial, data = PDT1, nAGQ = 10)
summary(CLIENTELISM)
sum(insight::get_variance_intercept(CLIENTELISM)) / 
  (sum(insight::get_variance_intercept(CLIENTELISM)) + insight::get_variance_residual(CLIENTELISM)) 

LINTERESTS <- glmer(`LOCAL INTERESTS` ~ 1 + (1 | REGION), family = binomial, data = PDT1, nAGQ = 10)
summary(LINTERESTS)
sum(insight::get_variance_intercept(LINTERESTS)) / 
  (sum(insight::get_variance_intercept(LINTERESTS)) + insight::get_variance_residual(LINTERESTS)) 

FAMILY <- glmer(FAMILY ~ 1 + (1 | REGION), family = binomial, data = PDT1, nAGQ = 10)
summary(FAMILY)
sum(insight::get_variance_intercept(FAMILY)) / 
  (sum(insight::get_variance_intercept(FAMILY)) + insight::get_variance_residual(FAMILY)) 

COUNCILLOR <- glmer(COUNCILLOR ~ 1 + (1 | REGION), family = binomial, data = PDT1, nAGQ = 10)
summary(COUNCILLOR)
sum(insight::get_variance_intercept(COUNCILLOR)) / 
  (sum(insight::get_variance_intercept(COUNCILLOR)) + insight::get_variance_residual(COUNCILLOR)) 

SDEPUTY <- glmer(`STATE DEPUTY` ~ 1 + (1 | REGION), family = binomial, data = PDT1, nAGQ = 10)
summary(SDEPUTY)
sum(insight::get_variance_intercept(SDEPUTY)) / 
  (sum(insight::get_variance_intercept(SDEPUTY)) + insight::get_variance_residual(SDEPUTY)) 

SENATOR <- glmer(SENATOR ~ 1 + (1 | REGION), family = binomial, data = PDT1, nAGQ = 10)
summary(SENATOR)
sum(insight::get_variance_intercept(SENATOR)) / 
  (sum(insight::get_variance_intercept(SENATOR)) + insight::get_variance_residual(SENATOR)) 

FDEPUTY <- glmer(`FEDERAL DEPUTY` ~ 1 + (1 | REGION), family = binomial, data = PDT1, nAGQ = 10)
summary(FDEPUTY)
sum(insight::get_variance_intercept(FDEPUTY)) / 
  (sum(insight::get_variance_intercept(FDEPUTY)) + insight::get_variance_residual(FDEPUTY)) 

MAYOR <- glmer(MAYOR ~ 1 + (1 | REGION), family = binomial, data = PDT1, nAGQ = 10)
summary(MAYOR)
sum(insight::get_variance_intercept(MAYOR)) / 
  (sum(insight::get_variance_intercept(MAYOR)) + insight::get_variance_residual(MAYOR)) 

GOVERNOR <- glmer(GOVERNOR ~ 1 + (1 | REGION), family = binomial, data = PDT1, nAGQ = 10)
summary(GOVERNOR)
sum(insight::get_variance_intercept(GOVERNOR)) / 
  (sum(insight::get_variance_intercept(GOVERNOR)) + insight::get_variance_residual(GOVERNOR)) 

MINISTER <- glmer(MINISTER ~ 1 + (1 | REGION), family = binomial, data = PDT1, nAGQ = 10)
summary(MINISTER)
sum(insight::get_variance_intercept(MINISTER)) / 
  (sum(insight::get_variance_intercept(MINISTER)) + insight::get_variance_residual(MINISTER)) 

SSECRETARY <- glmer(`STATE SECRETARY` ~ 1 + (1 | REGION), family = binomial, data = PDT1, nAGQ = 10)
summary(SSECRETARY)
sum(insight::get_variance_intercept(SSECRETARY)) / 
  (sum(insight::get_variance_intercept(SSECRETARY)) + insight::get_variance_residual(SSECRETARY)) 

LEFTRIGHT <- clmm(`LEFT RIGHT` ~ 1 + (1 | REGION), Hess = TRUE, data = PDT1, nAGQ = 10)
summary(LEFTRIGHT)
sum(insight::get_variance_intercept(LEFTRIGHT)) / 
  (sum(insight::get_variance_intercept(LEFTRIGHT)) + insight::get_variance_residual(LEFTRIGHT)) 



# PT
PT1 <- na.omit(PT)

FIDELITY <- glmer(FIDELITY ~ 1 + (1 | REGION), family = binomial, data = PT1, nAGQ = 10)
summary(FIDELITY)
sum(insight::get_variance_intercept(FIDELITY)) / 
  (sum(insight::get_variance_intercept(FIDELITY)) + insight::get_variance_residual(FIDELITY)) 

BELIEVE <- glmer(BELIEVE ~ 1 + (1 | REGION), family = binomial, data = PT1, nAGQ = 10)
summary(BELIEVE)
sum(insight::get_variance_intercept(BELIEVE)) / 
  (sum(insight::get_variance_intercept(BELIEVE)) + insight::get_variance_residual(BELIEVE)) 

EFFORTS <- glmer(EFFORTS ~ 1 + (1 | REGION), family = binomial, data = PT1, nAGQ = 10)
summary(EFFORTS)
sum(insight::get_variance_intercept(EFFORTS)) / 
  (sum(insight::get_variance_intercept(EFFORTS)) + insight::get_variance_residual(EFFORTS)) 

DECREE <- glmer(DECREE ~ 1 + (1 | REGION), family = binomial, data = PT1, nAGQ = 10)
summary(DECREE)
sum(insight::get_variance_intercept(DECREE)) / 
  (sum(insight::get_variance_intercept(DECREE)) + insight::get_variance_residual(DECREE)) 

MILITARY <- glmer(MILITARY ~ 1 + (1 | REGION), family = binomial, data = PT1, nAGQ = 10)
summary(MILITARY)
sum(insight::get_variance_intercept(MILITARY)) / 
  (sum(insight::get_variance_intercept(MILITARY)) + insight::get_variance_residual(MILITARY)) 

SWITCH <- glmer(SWITCH ~ 1 + (1 | REGION), family = binomial, data = PT1, nAGQ = 10)
summary(SWITCH)
sum(insight::get_variance_intercept(SWITCH)) / 
  (sum(insight::get_variance_intercept(SWITCH)) + insight::get_variance_residual(SWITCH)) 

ECONOMY <- clmm(ECONOMY ~ 1 + (1 | REGION), Hess = TRUE, data = PT1, nAGQ = 10)
summary(ECONOMY)
sum(insight::get_variance_intercept(ECONOMY)) / 
  (sum(insight::get_variance_intercept(ECONOMY)) + insight::get_variance_residual(ECONOMY)) 

CLIENTELISM <- glmer(CLIENTELISM ~ 1 + (1 | REGION), family = binomial, data = PT1, nAGQ = 10)
summary(CLIENTELISM)
sum(insight::get_variance_intercept(CLIENTELISM)) / 
  (sum(insight::get_variance_intercept(CLIENTELISM)) + insight::get_variance_residual(CLIENTELISM)) 

LINTERESTS <- glmer(`LOCAL INTERESTS` ~ 1 + (1 | REGION), family = binomial, data = PT1, nAGQ = 10)
summary(LINTERESTS)
sum(insight::get_variance_intercept(LINTERESTS)) / 
  (sum(insight::get_variance_intercept(LINTERESTS)) + insight::get_variance_residual(LINTERESTS)) 

FAMILY <- glmer(FAMILY ~ 1 + (1 | REGION), family = binomial, data = PT1, nAGQ = 10)
summary(FAMILY)
sum(insight::get_variance_intercept(FAMILY)) / 
  (sum(insight::get_variance_intercept(FAMILY)) + insight::get_variance_residual(FAMILY)) 

COUNCILLOR <- glmer(COUNCILLOR ~ 1 + (1 | REGION), family = binomial, data = PT1, nAGQ = 10)
summary(COUNCILLOR)
sum(insight::get_variance_intercept(COUNCILLOR)) / 
  (sum(insight::get_variance_intercept(COUNCILLOR)) + insight::get_variance_residual(COUNCILLOR)) 

SDEPUTY <- glmer(`STATE DEPUTY` ~ 1 + (1 | REGION), family = binomial, data = PT1, nAGQ = 10)
summary(SDEPUTY)
sum(insight::get_variance_intercept(SDEPUTY)) / 
  (sum(insight::get_variance_intercept(SDEPUTY)) + insight::get_variance_residual(SDEPUTY)) 

SENATOR <- glmer(SENATOR ~ 1 + (1 | REGION), family = binomial, data = PT1, nAGQ = 10)
summary(SENATOR)
sum(insight::get_variance_intercept(SENATOR)) / 
  (sum(insight::get_variance_intercept(SENATOR)) + insight::get_variance_residual(SENATOR)) 

FDEPUTY <- glmer(`FEDERAL DEPUTY` ~ 1 + (1 | REGION), family = binomial, data = PT1, nAGQ = 10)
summary(FDEPUTY)
sum(insight::get_variance_intercept(FDEPUTY)) / 
  (sum(insight::get_variance_intercept(FDEPUTY)) + insight::get_variance_residual(FDEPUTY)) 

MAYOR <- glmer(MAYOR ~ 1 + (1 | REGION), family = binomial, data = PT1, nAGQ = 10)
summary(MAYOR)
sum(insight::get_variance_intercept(MAYOR)) / 
  (sum(insight::get_variance_intercept(MAYOR)) + insight::get_variance_residual(MAYOR)) 

GOVERNOR <- glmer(GOVERNOR ~ 1 + (1 | REGION), family = binomial, data = PT1, nAGQ = 10)
summary(GOVERNOR)
sum(insight::get_variance_intercept(GOVERNOR)) / 
  (sum(insight::get_variance_intercept(GOVERNOR)) + insight::get_variance_residual(GOVERNOR)) 

MINISTER <- glmer(MINISTER ~ 1 + (1 | REGION), family = binomial, data = PT1, nAGQ = 10)
summary(MINISTER)
sum(insight::get_variance_intercept(MINISTER)) / 
  (sum(insight::get_variance_intercept(MINISTER)) + insight::get_variance_residual(MINISTER)) 

SSECRETARY <- glmer(`STATE SECRETARY` ~ 1 + (1 | REGION), family = binomial, data = PT1, nAGQ = 10)
summary(SSECRETARY)
sum(insight::get_variance_intercept(SSECRETARY)) / 
  (sum(insight::get_variance_intercept(SSECRETARY)) + insight::get_variance_residual(SSECRETARY)) 

LEFTRIGHT <- clmm(`LEFT RIGHT` ~ 1 + (1 | REGION), Hess = TRUE, data = PT1, nAGQ = 10)
summary(LEFTRIGHT)
sum(insight::get_variance_intercept(LEFTRIGHT)) / 
  (sum(insight::get_variance_intercept(LEFTRIGHT)) + insight::get_variance_residual(LEFTRIGHT)) 



# PTB
PTB1 <- na.omit(PTB)

FIDELITY <- glmer(FIDELITY ~ 1 + (1 | REGION), family = binomial, data = PTB1, nAGQ = 10)
summary(FIDELITY)
sum(insight::get_variance_intercept(FIDELITY)) / 
  (sum(insight::get_variance_intercept(FIDELITY)) + insight::get_variance_residual(FIDELITY)) 

BELIEVE <- glmer(BELIEVE ~ 1 + (1 | REGION), family = binomial, data = PTB1, nAGQ = 10)
summary(BELIEVE)
sum(insight::get_variance_intercept(BELIEVE)) / 
  (sum(insight::get_variance_intercept(BELIEVE)) + insight::get_variance_residual(BELIEVE)) 

EFFORTS <- glmer(EFFORTS ~ 1 + (1 | REGION), family = binomial, data = PTB1, nAGQ = 10)
summary(EFFORTS)
sum(insight::get_variance_intercept(EFFORTS)) / 
  (sum(insight::get_variance_intercept(EFFORTS)) + insight::get_variance_residual(EFFORTS)) 

DECREE <- glmer(DECREE ~ 1 + (1 | REGION), family = binomial, data = PTB1, nAGQ = 10)
summary(DECREE)
sum(insight::get_variance_intercept(DECREE)) / 
  (sum(insight::get_variance_intercept(DECREE)) + insight::get_variance_residual(DECREE)) 

MILITARY <- glmer(MILITARY ~ 1 + (1 | REGION), family = binomial, data = PTB1, nAGQ = 10)
summary(MILITARY)
sum(insight::get_variance_intercept(MILITARY)) / 
  (sum(insight::get_variance_intercept(MILITARY)) + insight::get_variance_residual(MILITARY)) 

SWITCH <- glmer(SWITCH ~ 1 + (1 | REGION), family = binomial, data = PTB1, nAGQ = 10)
summary(SWITCH)
sum(insight::get_variance_intercept(SWITCH)) / 
  (sum(insight::get_variance_intercept(SWITCH)) + insight::get_variance_residual(SWITCH)) 

ECONOMY <- clmm(ECONOMY ~ 1 + (1 | REGION), Hess = TRUE, data = PTB1, nAGQ = 10)
summary(ECONOMY)
sum(insight::get_variance_intercept(ECONOMY)) / 
  (sum(insight::get_variance_intercept(ECONOMY)) + insight::get_variance_residual(ECONOMY)) 

CLIENTELISM <- glmer(CLIENTELISM ~ 1 + (1 | REGION), family = binomial, data = PTB1, nAGQ = 10)
summary(CLIENTELISM)
sum(insight::get_variance_intercept(CLIENTELISM)) / 
  (sum(insight::get_variance_intercept(CLIENTELISM)) + insight::get_variance_residual(CLIENTELISM)) 

LINTERESTS <- glmer(`LOCAL INTERESTS` ~ 1 + (1 | REGION), family = binomial, data = PTB1, nAGQ = 10)
summary(LINTERESTS)
sum(insight::get_variance_intercept(LINTERESTS)) / 
  (sum(insight::get_variance_intercept(LINTERESTS)) + insight::get_variance_residual(LINTERESTS)) 

FAMILY <- glmer(FAMILY ~ 1 + (1 | REGION), family = binomial, data = PTB1, nAGQ = 10)
summary(FAMILY)
sum(insight::get_variance_intercept(FAMILY)) / 
  (sum(insight::get_variance_intercept(FAMILY)) + insight::get_variance_residual(FAMILY)) 

COUNCILLOR <- glmer(COUNCILLOR ~ 1 + (1 | REGION), family = binomial, data = PTB1, nAGQ = 10)
summary(COUNCILLOR)
sum(insight::get_variance_intercept(COUNCILLOR)) / 
  (sum(insight::get_variance_intercept(COUNCILLOR)) + insight::get_variance_residual(COUNCILLOR)) 

SDEPUTY <- glmer(`STATE DEPUTY` ~ 1 + (1 | REGION), family = binomial, data = PTB1, nAGQ = 10)
summary(SDEPUTY)
sum(insight::get_variance_intercept(SDEPUTY)) / 
  (sum(insight::get_variance_intercept(SDEPUTY)) + insight::get_variance_residual(SDEPUTY)) 

SENATOR <- glmer(SENATOR ~ 1 + (1 | REGION), family = binomial, data = PTB1, nAGQ = 10)
summary(SENATOR)
sum(insight::get_variance_intercept(SENATOR)) / 
  (sum(insight::get_variance_intercept(SENATOR)) + insight::get_variance_residual(SENATOR)) 

FDEPUTY <- glmer(`FEDERAL DEPUTY` ~ 1 + (1 | REGION), family = binomial, data = PTB1, nAGQ = 10)
summary(FDEPUTY)
sum(insight::get_variance_intercept(FDEPUTY)) / 
  (sum(insight::get_variance_intercept(FDEPUTY)) + insight::get_variance_residual(FDEPUTY)) 

MAYOR <- glmer(MAYOR ~ 1 + (1 | REGION), family = binomial, data = PTB1, nAGQ = 10)
summary(MAYOR)
sum(insight::get_variance_intercept(MAYOR)) / 
  (sum(insight::get_variance_intercept(MAYOR)) + insight::get_variance_residual(MAYOR)) 

GOVERNOR <- glmer(GOVERNOR ~ 1 + (1 | REGION), family = binomial, data = PTB1, nAGQ = 10)
summary(GOVERNOR)
sum(insight::get_variance_intercept(GOVERNOR)) / 
  (sum(insight::get_variance_intercept(GOVERNOR)) + insight::get_variance_residual(GOVERNOR)) 

MINISTER <- glmer(MINISTER ~ 1 + (1 | REGION), family = binomial, data = PTB1, nAGQ = 10)
summary(MINISTER)
sum(insight::get_variance_intercept(MINISTER)) / 
  (sum(insight::get_variance_intercept(MINISTER)) + insight::get_variance_residual(MINISTER)) 

SSECRETARY <- glmer(`STATE SECRETARY` ~ 1 + (1 | REGION), family = binomial, data = PTB1, nAGQ = 10)
summary(SSECRETARY)
sum(insight::get_variance_intercept(SSECRETARY)) / 
  (sum(insight::get_variance_intercept(SSECRETARY)) + insight::get_variance_residual(SSECRETARY)) 

LEFTRIGHT <- clmm(`LEFT RIGHT` ~ 1 + (1 | REGION), Hess = TRUE, data = PTB1, nAGQ = 10)
summary(LEFTRIGHT)
sum(insight::get_variance_intercept(LEFTRIGHT)) / 
  (sum(insight::get_variance_intercept(LEFTRIGHT)) + insight::get_variance_residual(LEFTRIGHT)) 



# PMDB
PMDB1 <- na.omit(PMDB)

FIDELITY <- glmer(FIDELITY ~ 1 + (1 | REGION), family = binomial, data = PMDB1, nAGQ = 10)
summary(FIDELITY)
sum(insight::get_variance_intercept(FIDELITY)) / 
  (sum(insight::get_variance_intercept(FIDELITY)) + insight::get_variance_residual(FIDELITY)) 

BELIEVE <- glmer(BELIEVE ~ 1 + (1 | REGION), family = binomial, data = PMDB1, nAGQ = 10)
summary(BELIEVE)
sum(insight::get_variance_intercept(BELIEVE)) / 
  (sum(insight::get_variance_intercept(BELIEVE)) + insight::get_variance_residual(BELIEVE)) 

EFFORTS <- glmer(EFFORTS ~ 1 + (1 | REGION), family = binomial, data = PMDB1, nAGQ = 10)
summary(EFFORTS)
sum(insight::get_variance_intercept(EFFORTS)) / 
  (sum(insight::get_variance_intercept(EFFORTS)) + insight::get_variance_residual(EFFORTS)) 

DECREE <- glmer(DECREE ~ 1 + (1 | REGION), family = binomial, data = PMDB1, nAGQ = 10)
summary(DECREE)
sum(insight::get_variance_intercept(DECREE)) / 
  (sum(insight::get_variance_intercept(DECREE)) + insight::get_variance_residual(DECREE)) 

MILITARY <- glmer(MILITARY ~ 1 + (1 | REGION), family = binomial, data = PMDB1, nAGQ = 10)
summary(MILITARY)
sum(insight::get_variance_intercept(MILITARY)) / 
  (sum(insight::get_variance_intercept(MILITARY)) + insight::get_variance_residual(MILITARY)) 

SWITCH <- glmer(SWITCH ~ 1 + (1 | REGION), family = binomial, data = PMDB1, nAGQ = 10)
summary(SWITCH)
sum(insight::get_variance_intercept(SWITCH)) / 
  (sum(insight::get_variance_intercept(SWITCH)) + insight::get_variance_residual(SWITCH)) 

ECONOMY <- clmm(ECONOMY ~ 1 + (1 | REGION), Hess = TRUE, data = PMDB1, nAGQ = 10)
summary(ECONOMY)
sum(insight::get_variance_intercept(ECONOMY)) / 
  (sum(insight::get_variance_intercept(ECONOMY)) + insight::get_variance_residual(ECONOMY)) 

CLIENTELISM <- glmer(CLIENTELISM ~ 1 + (1 | REGION), family = binomial, data = PMDB1, nAGQ = 10)
summary(CLIENTELISM)
sum(insight::get_variance_intercept(CLIENTELISM)) / 
  (sum(insight::get_variance_intercept(CLIENTELISM)) + insight::get_variance_residual(CLIENTELISM)) 

LINTERESTS <- glmer(`LOCAL INTERESTS` ~ 1 + (1 | REGION), family = binomial, data = PMDB1, nAGQ = 10)
summary(LINTERESTS)
sum(insight::get_variance_intercept(LINTERESTS)) / 
  (sum(insight::get_variance_intercept(LINTERESTS)) + insight::get_variance_residual(LINTERESTS)) 

FAMILY <- glmer(FAMILY ~ 1 + (1 | REGION), family = binomial, data = PMDB1, nAGQ = 10)
summary(FAMILY)
sum(insight::get_variance_intercept(FAMILY)) / 
  (sum(insight::get_variance_intercept(FAMILY)) + insight::get_variance_residual(FAMILY)) 

COUNCILLOR <- glmer(COUNCILLOR ~ 1 + (1 | REGION), family = binomial, data = PMDB1, nAGQ = 10)
summary(COUNCILLOR)
sum(insight::get_variance_intercept(COUNCILLOR)) / 
  (sum(insight::get_variance_intercept(COUNCILLOR)) + insight::get_variance_residual(COUNCILLOR)) 

SDEPUTY <- glmer(`STATE DEPUTY` ~ 1 + (1 | REGION), family = binomial, data = PMDB1, nAGQ = 10)
summary(SDEPUTY)
sum(insight::get_variance_intercept(SDEPUTY)) / 
  (sum(insight::get_variance_intercept(SDEPUTY)) + insight::get_variance_residual(SDEPUTY)) 

SENATOR <- glmer(SENATOR ~ 1 + (1 | REGION), family = binomial, data = PMDB1, nAGQ = 10)
summary(SENATOR)
sum(insight::get_variance_intercept(SENATOR)) / 
  (sum(insight::get_variance_intercept(SENATOR)) + insight::get_variance_residual(SENATOR)) 

FDEPUTY <- glmer(`FEDERAL DEPUTY` ~ 1 + (1 | REGION), family = binomial, data = PMDB1, nAGQ = 10)
summary(FDEPUTY)
sum(insight::get_variance_intercept(FDEPUTY)) / 
  (sum(insight::get_variance_intercept(FDEPUTY)) + insight::get_variance_residual(FDEPUTY)) 

MAYOR <- glmer(MAYOR ~ 1 + (1 | REGION), family = binomial, data = PMDB1, nAGQ = 10)
summary(MAYOR)
sum(insight::get_variance_intercept(MAYOR)) / 
  (sum(insight::get_variance_intercept(MAYOR)) + insight::get_variance_residual(MAYOR)) 

GOVERNOR <- glmer(GOVERNOR ~ 1 + (1 | REGION), family = binomial, data = PMDB1, nAGQ = 10)
summary(GOVERNOR)
sum(insight::get_variance_intercept(GOVERNOR)) / 
  (sum(insight::get_variance_intercept(GOVERNOR)) + insight::get_variance_residual(GOVERNOR)) 

MINISTER <- glmer(MINISTER ~ 1 + (1 | REGION), family = binomial, data = PMDB1, nAGQ = 10)
summary(MINISTER)
sum(insight::get_variance_intercept(MINISTER)) / 
  (sum(insight::get_variance_intercept(MINISTER)) + insight::get_variance_residual(MINISTER)) 

SSECRETARY <- glmer(`STATE SECRETARY` ~ 1 + (1 | REGION), family = binomial, data = PMDB1, nAGQ = 10)
summary(SSECRETARY)
sum(insight::get_variance_intercept(SSECRETARY)) / 
  (sum(insight::get_variance_intercept(SSECRETARY)) + insight::get_variance_residual(SSECRETARY)) 

LEFTRIGHT <- clmm(`LEFT RIGHT` ~ 1 + (1 | REGION), Hess = TRUE, data = PMDB1, nAGQ = 10)
summary(LEFTRIGHT)
sum(insight::get_variance_intercept(LEFTRIGHT)) / 
  (sum(insight::get_variance_intercept(LEFTRIGHT)) + insight::get_variance_residual(LEFTRIGHT)) 



# PR
PR1 <- na.omit(PR)

FIDELITY <- glmer(FIDELITY ~ 1 + (1 | REGION), family = binomial, data = PR1, nAGQ = 10)
summary(FIDELITY)
sum(insight::get_variance_intercept(FIDELITY)) / 
  (sum(insight::get_variance_intercept(FIDELITY)) + insight::get_variance_residual(FIDELITY)) 

BELIEVE <- glmer(BELIEVE ~ 1 + (1 | REGION), family = binomial, data = PR1, nAGQ = 10)
summary(BELIEVE)
sum(insight::get_variance_intercept(BELIEVE)) / 
  (sum(insight::get_variance_intercept(BELIEVE)) + insight::get_variance_residual(BELIEVE)) 

EFFORTS <- glmer(EFFORTS ~ 1 + (1 | REGION), family = binomial, data = PR1, nAGQ = 10)
summary(EFFORTS)
sum(insight::get_variance_intercept(EFFORTS)) / 
  (sum(insight::get_variance_intercept(EFFORTS)) + insight::get_variance_residual(EFFORTS)) 

DECREE <- glmer(DECREE ~ 1 + (1 | REGION), family = binomial, data = PR1, nAGQ = 10)
summary(DECREE)
sum(insight::get_variance_intercept(DECREE)) / 
  (sum(insight::get_variance_intercept(DECREE)) + insight::get_variance_residual(DECREE)) 

MILITARY <- glmer(MILITARY ~ 1 + (1 | REGION), family = binomial, data = PR1, nAGQ = 10)
summary(MILITARY)
sum(insight::get_variance_intercept(MILITARY)) / 
  (sum(insight::get_variance_intercept(MILITARY)) + insight::get_variance_residual(MILITARY)) 

SWITCH <- glmer(SWITCH ~ 1 + (1 | REGION), family = binomial, data = PR1, nAGQ = 10)
summary(SWITCH)
sum(insight::get_variance_intercept(SWITCH)) / 
  (sum(insight::get_variance_intercept(SWITCH)) + insight::get_variance_residual(SWITCH)) 

ECONOMY <- clmm(ECONOMY ~ 1 + (1 | REGION), Hess = TRUE, data = PR1, nAGQ = 10)
summary(ECONOMY)
sum(insight::get_variance_intercept(ECONOMY)) / 
  (sum(insight::get_variance_intercept(ECONOMY)) + insight::get_variance_residual(ECONOMY)) 

CLIENTELISM <- glmer(CLIENTELISM ~ 1 + (1 | REGION), family = binomial, data = PR1, nAGQ = 10)
summary(CLIENTELISM)
sum(insight::get_variance_intercept(CLIENTELISM)) / 
  (sum(insight::get_variance_intercept(CLIENTELISM)) + insight::get_variance_residual(CLIENTELISM)) 

LINTERESTS <- glmer(`LOCAL INTERESTS` ~ 1 + (1 | REGION), family = binomial, data = PR1, nAGQ = 10)
summary(LINTERESTS)
sum(insight::get_variance_intercept(LINTERESTS)) / 
  (sum(insight::get_variance_intercept(LINTERESTS)) + insight::get_variance_residual(LINTERESTS)) 

FAMILY <- glmer(FAMILY ~ 1 + (1 | REGION), family = binomial, data = PR1, nAGQ = 10)
summary(FAMILY)
sum(insight::get_variance_intercept(FAMILY)) / 
  (sum(insight::get_variance_intercept(FAMILY)) + insight::get_variance_residual(FAMILY)) 

COUNCILLOR <- glmer(COUNCILLOR ~ 1 + (1 | REGION), family = binomial, data = PR1, nAGQ = 10)
summary(COUNCILLOR)
sum(insight::get_variance_intercept(COUNCILLOR)) / 
  (sum(insight::get_variance_intercept(COUNCILLOR)) + insight::get_variance_residual(COUNCILLOR)) 

SDEPUTY <- glmer(`STATE DEPUTY` ~ 1 + (1 | REGION), family = binomial, data = PR1, nAGQ = 10)
summary(SDEPUTY)
sum(insight::get_variance_intercept(SDEPUTY)) / 
  (sum(insight::get_variance_intercept(SDEPUTY)) + insight::get_variance_residual(SDEPUTY)) 

SENATOR <- glmer(SENATOR ~ 1 + (1 | REGION), family = binomial, data = PR1, nAGQ = 10)
summary(SENATOR)
sum(insight::get_variance_intercept(SENATOR)) / 
  (sum(insight::get_variance_intercept(SENATOR)) + insight::get_variance_residual(SENATOR)) 

FDEPUTY <- glmer(`FEDERAL DEPUTY` ~ 1 + (1 | REGION), family = binomial, data = PR1, nAGQ = 10)
summary(FDEPUTY)
sum(insight::get_variance_intercept(FDEPUTY)) / 
  (sum(insight::get_variance_intercept(FDEPUTY)) + insight::get_variance_residual(FDEPUTY)) 

MAYOR <- glmer(MAYOR ~ 1 + (1 | REGION), family = binomial, data = PR1, nAGQ = 10)
summary(MAYOR)
sum(insight::get_variance_intercept(MAYOR)) / 
  (sum(insight::get_variance_intercept(MAYOR)) + insight::get_variance_residual(MAYOR)) 

GOVERNOR <- glmer(GOVERNOR ~ 1 + (1 | REGION), family = binomial, data = PR1, nAGQ = 10)
summary(GOVERNOR)
sum(insight::get_variance_intercept(GOVERNOR)) / 
  (sum(insight::get_variance_intercept(GOVERNOR)) + insight::get_variance_residual(GOVERNOR)) 

MINISTER <- glmer(MINISTER ~ 1 + (1 | REGION), family = binomial, data = PR1, nAGQ = 10)
summary(MINISTER)
sum(insight::get_variance_intercept(MINISTER)) / 
  (sum(insight::get_variance_intercept(MINISTER)) + insight::get_variance_residual(MINISTER)) 

SSECRETARY <- glmer(`STATE SECRETARY` ~ 1 + (1 | REGION), family = binomial, data = PR1, nAGQ = 10)
summary(SSECRETARY)
sum(insight::get_variance_intercept(SSECRETARY)) / 
  (sum(insight::get_variance_intercept(SSECRETARY)) + insight::get_variance_residual(SSECRETARY)) 

LEFTRIGHT <- clmm(`LEFT RIGHT` ~ 1 + (1 | REGION), Hess = TRUE, data = PR1, nAGQ = 10)
summary(LEFTRIGHT)
sum(insight::get_variance_intercept(LEFTRIGHT)) / 
  (sum(insight::get_variance_intercept(LEFTRIGHT)) + insight::get_variance_residual(LEFTRIGHT)) 



# DEM
DEM1 <- na.omit(DEM)

FIDELITY <- glmer(FIDELITY ~ 1 + (1 | REGION), family = binomial, data = DEM1, nAGQ = 10)
summary(FIDELITY)
sum(insight::get_variance_intercept(FIDELITY)) / 
  (sum(insight::get_variance_intercept(FIDELITY)) + insight::get_variance_residual(FIDELITY)) 

BELIEVE <- glmer(BELIEVE ~ 1 + (1 | REGION), family = binomial, data = DEM1, nAGQ = 10)
summary(BELIEVE)
sum(insight::get_variance_intercept(BELIEVE)) / 
  (sum(insight::get_variance_intercept(BELIEVE)) + insight::get_variance_residual(BELIEVE)) 

EFFORTS <- glmer(EFFORTS ~ 1 + (1 | REGION), family = binomial, data = DEM1, nAGQ = 10)
summary(EFFORTS)
sum(insight::get_variance_intercept(EFFORTS)) / 
  (sum(insight::get_variance_intercept(EFFORTS)) + insight::get_variance_residual(EFFORTS)) 

DECREE <- glmer(DECREE ~ 1 + (1 | REGION), family = binomial, data = DEM1, nAGQ = 10)
summary(DECREE)
sum(insight::get_variance_intercept(DECREE)) / 
  (sum(insight::get_variance_intercept(DECREE)) + insight::get_variance_residual(DECREE)) 

MILITARY <- glmer(MILITARY ~ 1 + (1 | REGION), family = binomial, data = DEM1, nAGQ = 10)
summary(MILITARY)
sum(insight::get_variance_intercept(MILITARY)) / 
  (sum(insight::get_variance_intercept(MILITARY)) + insight::get_variance_residual(MILITARY)) 

SWITCH <- glmer(SWITCH ~ 1 + (1 | REGION), family = binomial, data = DEM1, nAGQ = 10)
summary(SWITCH)
sum(insight::get_variance_intercept(SWITCH)) / 
  (sum(insight::get_variance_intercept(SWITCH)) + insight::get_variance_residual(SWITCH)) 

ECONOMY <- clmm(ECONOMY ~ 1 + (1 | REGION), Hess = TRUE, data = DEM1, nAGQ = 10)
summary(ECONOMY)
sum(insight::get_variance_intercept(ECONOMY)) / 
  (sum(insight::get_variance_intercept(ECONOMY)) + insight::get_variance_residual(ECONOMY)) 

CLIENTELISM <- glmer(CLIENTELISM ~ 1 + (1 | REGION), family = binomial, data = DEM1, nAGQ = 10)
summary(CLIENTELISM)
sum(insight::get_variance_intercept(CLIENTELISM)) / 
  (sum(insight::get_variance_intercept(CLIENTELISM)) + insight::get_variance_residual(CLIENTELISM)) 

LINTERESTS <- glmer(`LOCAL INTERESTS` ~ 1 + (1 | REGION), family = binomial, data = DEM1, nAGQ = 10)
summary(LINTERESTS)
sum(insight::get_variance_intercept(LINTERESTS)) / 
  (sum(insight::get_variance_intercept(LINTERESTS)) + insight::get_variance_residual(LINTERESTS)) 

FAMILY <- glmer(FAMILY ~ 1 + (1 | REGION), family = binomial, data = DEM1, nAGQ = 10)
summary(FAMILY)
sum(insight::get_variance_intercept(FAMILY)) / 
  (sum(insight::get_variance_intercept(FAMILY)) + insight::get_variance_residual(FAMILY)) 

COUNCILLOR <- glmer(COUNCILLOR ~ 1 + (1 | REGION), family = binomial, data = DEM1, nAGQ = 10)
summary(COUNCILLOR)
sum(insight::get_variance_intercept(COUNCILLOR)) / 
  (sum(insight::get_variance_intercept(COUNCILLOR)) + insight::get_variance_residual(COUNCILLOR)) 

SDEPUTY <- glmer(`STATE DEPUTY` ~ 1 + (1 | REGION), family = binomial, data = DEM1, nAGQ = 10)
summary(SDEPUTY)
sum(insight::get_variance_intercept(SDEPUTY)) / 
  (sum(insight::get_variance_intercept(SDEPUTY)) + insight::get_variance_residual(SDEPUTY)) 

SENATOR <- glmer(SENATOR ~ 1 + (1 | REGION), family = binomial, data = DEM1, nAGQ = 10)
summary(SENATOR)
sum(insight::get_variance_intercept(SENATOR)) / 
  (sum(insight::get_variance_intercept(SENATOR)) + insight::get_variance_residual(SENATOR)) 

FDEPUTY <- glmer(`FEDERAL DEPUTY` ~ 1 + (1 | REGION), family = binomial, data = DEM1, nAGQ = 10)
summary(FDEPUTY)
sum(insight::get_variance_intercept(FDEPUTY)) / 
  (sum(insight::get_variance_intercept(FDEPUTY)) + insight::get_variance_residual(FDEPUTY)) 

MAYOR <- glmer(MAYOR ~ 1 + (1 | REGION), family = binomial, data = DEM1, nAGQ = 10)
summary(MAYOR)
sum(insight::get_variance_intercept(MAYOR)) / 
  (sum(insight::get_variance_intercept(MAYOR)) + insight::get_variance_residual(MAYOR)) 

GOVERNOR <- glmer(GOVERNOR ~ 1 + (1 | REGION), family = binomial, data = DEM1, nAGQ = 10)
summary(GOVERNOR)
sum(insight::get_variance_intercept(GOVERNOR)) / 
  (sum(insight::get_variance_intercept(GOVERNOR)) + insight::get_variance_residual(GOVERNOR)) 

MINISTER <- glmer(MINISTER ~ 1 + (1 | REGION), family = binomial, data = DEM1, nAGQ = 10)
summary(MINISTER)
sum(insight::get_variance_intercept(MINISTER)) / 
  (sum(insight::get_variance_intercept(MINISTER)) + insight::get_variance_residual(MINISTER)) 

SSECRETARY <- glmer(`STATE SECRETARY` ~ 1 + (1 | REGION), family = binomial, data = DEM1, nAGQ = 10)
summary(SSECRETARY)
sum(insight::get_variance_intercept(SSECRETARY)) / 
  (sum(insight::get_variance_intercept(SSECRETARY)) + insight::get_variance_residual(SSECRETARY)) 

LEFTRIGHT <- clmm(`LEFT RIGHT` ~ 1 + (1 | REGION), Hess = TRUE, data = DEM1, nAGQ = 10)
summary(LEFTRIGHT)
sum(insight::get_variance_intercept(LEFTRIGHT)) / 
  (sum(insight::get_variance_intercept(LEFTRIGHT)) + insight::get_variance_residual(LEFTRIGHT)) 



# PP93
PP93 <- na.omit(PP1993)

FIDELITY <- glmer(FIDELITY ~ 1 + (1 | REGION), family = binomial, data = PP93, nAGQ = 10)
summary(FIDELITY)
sum(insight::get_variance_intercept(FIDELITY)) / 
  (sum(insight::get_variance_intercept(FIDELITY)) + insight::get_variance_residual(FIDELITY)) 

BELIEVE <- glmer(BELIEVE ~ 1 + (1 | REGION), family = binomial, data = PP93, nAGQ = 10)
summary(BELIEVE)
sum(insight::get_variance_intercept(BELIEVE)) / 
  (sum(insight::get_variance_intercept(BELIEVE)) + insight::get_variance_residual(BELIEVE)) 

EFFORTS <- glmer(EFFORTS ~ 1 + (1 | REGION), family = binomial, data = PP93, nAGQ = 10)
summary(EFFORTS)
sum(insight::get_variance_intercept(EFFORTS)) / 
  (sum(insight::get_variance_intercept(EFFORTS)) + insight::get_variance_residual(EFFORTS)) 

DECREE <- glmer(DECREE ~ 1 + (1 | REGION), family = binomial, data = PP93, nAGQ = 10)
summary(DECREE)
sum(insight::get_variance_intercept(DECREE)) / 
  (sum(insight::get_variance_intercept(DECREE)) + insight::get_variance_residual(DECREE)) 

MILITARY <- glmer(MILITARY ~ 1 + (1 | REGION), family = binomial, data = PP93, nAGQ = 10)
summary(MILITARY)
sum(insight::get_variance_intercept(MILITARY)) / 
  (sum(insight::get_variance_intercept(MILITARY)) + insight::get_variance_residual(MILITARY)) 

SWITCH <- glmer(SWITCH ~ 1 + (1 | REGION), family = binomial, data = PP93, nAGQ = 10)
summary(SWITCH)
sum(insight::get_variance_intercept(SWITCH)) / 
  (sum(insight::get_variance_intercept(SWITCH)) + insight::get_variance_residual(SWITCH)) 

ECONOMY <- clmm(ECONOMY ~ 1 + (1 | REGION), Hess = TRUE, data = PP93, nAGQ = 10)
summary(ECONOMY)
sum(insight::get_variance_intercept(ECONOMY)) / 
  (sum(insight::get_variance_intercept(ECONOMY)) + insight::get_variance_residual(ECONOMY)) 

CLIENTELISM <- glmer(CLIENTELISM ~ 1 + (1 | REGION), family = binomial, data = PP93, nAGQ = 10)
summary(CLIENTELISM)
sum(insight::get_variance_intercept(CLIENTELISM)) / 
  (sum(insight::get_variance_intercept(CLIENTELISM)) + insight::get_variance_residual(CLIENTELISM)) 

LINTERESTS <- glmer(`LOCAL INTERESTS` ~ 1 + (1 | REGION), family = binomial, data = PP93, nAGQ = 10)
summary(LINTERESTS)
sum(insight::get_variance_intercept(LINTERESTS)) / 
  (sum(insight::get_variance_intercept(LINTERESTS)) + insight::get_variance_residual(LINTERESTS)) 

FAMILY <- glmer(FAMILY ~ 1 + (1 | REGION), family = binomial, data = PP93, nAGQ = 10)
summary(FAMILY)
sum(insight::get_variance_intercept(FAMILY)) / 
  (sum(insight::get_variance_intercept(FAMILY)) + insight::get_variance_residual(FAMILY)) 

COUNCILLOR <- glmer(COUNCILLOR ~ 1 + (1 | REGION), family = binomial, data = PP93, nAGQ = 10)
summary(COUNCILLOR)
sum(insight::get_variance_intercept(COUNCILLOR)) / 
  (sum(insight::get_variance_intercept(COUNCILLOR)) + insight::get_variance_residual(COUNCILLOR)) 

SDEPUTY <- glmer(`STATE DEPUTY` ~ 1 + (1 | REGION), family = binomial, data = PP93, nAGQ = 10)
summary(SDEPUTY)
sum(insight::get_variance_intercept(SDEPUTY)) / 
  (sum(insight::get_variance_intercept(SDEPUTY)) + insight::get_variance_residual(SDEPUTY)) 

SENATOR <- glmer(SENATOR ~ 1 + (1 | REGION), family = binomial, data = PP93, nAGQ = 10)
summary(SENATOR)
sum(insight::get_variance_intercept(SENATOR)) / 
  (sum(insight::get_variance_intercept(SENATOR)) + insight::get_variance_residual(SENATOR)) 

FDEPUTY <- glmer(`FEDERAL DEPUTY` ~ 1 + (1 | REGION), family = binomial, data = PP93, nAGQ = 10)
summary(FDEPUTY)
sum(insight::get_variance_intercept(FDEPUTY)) / 
  (sum(insight::get_variance_intercept(FDEPUTY)) + insight::get_variance_residual(FDEPUTY)) 

MAYOR <- glmer(MAYOR ~ 1 + (1 | REGION), family = binomial, data = PP93, nAGQ = 10)
summary(MAYOR)
sum(insight::get_variance_intercept(MAYOR)) / 
  (sum(insight::get_variance_intercept(MAYOR)) + insight::get_variance_residual(MAYOR)) 

GOVERNOR <- glmer(GOVERNOR ~ 1 + (1 | REGION), family = binomial, data = PP93, nAGQ = 10)
summary(GOVERNOR)
sum(insight::get_variance_intercept(GOVERNOR)) / 
  (sum(insight::get_variance_intercept(GOVERNOR)) + insight::get_variance_residual(GOVERNOR)) 

MINISTER <- glmer(MINISTER ~ 1 + (1 | REGION), family = binomial, data = PP93, nAGQ = 10)
summary(MINISTER)
sum(insight::get_variance_intercept(MINISTER)) / 
  (sum(insight::get_variance_intercept(MINISTER)) + insight::get_variance_residual(MINISTER)) 

SSECRETARY <- glmer(`STATE SECRETARY` ~ 1 + (1 | REGION), family = binomial, data = PP93, nAGQ = 10)
summary(SSECRETARY)
sum(insight::get_variance_intercept(SSECRETARY)) / 
  (sum(insight::get_variance_intercept(SSECRETARY)) + insight::get_variance_residual(SSECRETARY)) 

LEFTRIGHT <- clmm(`LEFT RIGHT` ~ 1 + (1 | REGION), Hess = TRUE, data = PP93, nAGQ = 10)
summary(LEFTRIGHT)
sum(insight::get_variance_intercept(LEFTRIGHT)) / 
  (sum(insight::get_variance_intercept(LEFTRIGHT)) + insight::get_variance_residual(LEFTRIGHT)) 



# PSB
PSB1 <- na.omit(PSB)

FIDELITY <- glmer(FIDELITY ~ 1 + (1 | REGION), family = binomial, data = PSB1, nAGQ = 10)
summary(FIDELITY)
sum(insight::get_variance_intercept(FIDELITY)) / 
  (sum(insight::get_variance_intercept(FIDELITY)) + insight::get_variance_residual(FIDELITY)) 

BELIEVE <- glmer(BELIEVE ~ 1 + (1 | REGION), family = binomial, data = PSB1, nAGQ = 10)
summary(BELIEVE)
sum(insight::get_variance_intercept(BELIEVE)) / 
  (sum(insight::get_variance_intercept(BELIEVE)) + insight::get_variance_residual(BELIEVE)) 

EFFORTS <- glmer(EFFORTS ~ 1 + (1 | REGION), family = binomial, data = PSB1, nAGQ = 10)
summary(EFFORTS)
sum(insight::get_variance_intercept(EFFORTS)) / 
  (sum(insight::get_variance_intercept(EFFORTS)) + insight::get_variance_residual(EFFORTS)) 

DECREE <- glmer(DECREE ~ 1 + (1 | REGION), family = binomial, data = PSB1, nAGQ = 10)
summary(DECREE)
sum(insight::get_variance_intercept(DECREE)) / 
  (sum(insight::get_variance_intercept(DECREE)) + insight::get_variance_residual(DECREE)) 

MILITARY <- glmer(MILITARY ~ 1 + (1 | REGION), family = binomial, data = PSB1, nAGQ = 10)
summary(MILITARY)
sum(insight::get_variance_intercept(MILITARY)) / 
  (sum(insight::get_variance_intercept(MILITARY)) + insight::get_variance_residual(MILITARY)) 

SWITCH <- glmer(SWITCH ~ 1 + (1 | REGION), family = binomial, data = PSB1, nAGQ = 10)
summary(SWITCH)
sum(insight::get_variance_intercept(SWITCH)) / 
  (sum(insight::get_variance_intercept(SWITCH)) + insight::get_variance_residual(SWITCH)) 

ECONOMY <- clmm(ECONOMY ~ 1 + (1 | REGION), Hess = TRUE, data = PSB1, nAGQ = 10)
summary(ECONOMY)
sum(insight::get_variance_intercept(ECONOMY)) / 
  (sum(insight::get_variance_intercept(ECONOMY)) + insight::get_variance_residual(ECONOMY)) 

CLIENTELISM <- glmer(CLIENTELISM ~ 1 + (1 | REGION), family = binomial, data = PSB1, nAGQ = 10)
summary(CLIENTELISM)
sum(insight::get_variance_intercept(CLIENTELISM)) / 
  (sum(insight::get_variance_intercept(CLIENTELISM)) + insight::get_variance_residual(CLIENTELISM)) 

LINTERESTS <- glmer(`LOCAL INTERESTS` ~ 1 + (1 | REGION), family = binomial, data = PSB1, nAGQ = 10)
summary(LINTERESTS)
sum(insight::get_variance_intercept(LINTERESTS)) / 
  (sum(insight::get_variance_intercept(LINTERESTS)) + insight::get_variance_residual(LINTERESTS)) 

FAMILY <- glmer(FAMILY ~ 1 + (1 | REGION), family = binomial, data = PSB1, nAGQ = 10)
summary(FAMILY)
sum(insight::get_variance_intercept(FAMILY)) / 
  (sum(insight::get_variance_intercept(FAMILY)) + insight::get_variance_residual(FAMILY)) 

COUNCILLOR <- glmer(COUNCILLOR ~ 1 + (1 | REGION), family = binomial, data = PSB1, nAGQ = 10)
summary(COUNCILLOR)
sum(insight::get_variance_intercept(COUNCILLOR)) / 
  (sum(insight::get_variance_intercept(COUNCILLOR)) + insight::get_variance_residual(COUNCILLOR)) 

SDEPUTY <- glmer(`STATE DEPUTY` ~ 1 + (1 | REGION), family = binomial, data = PSB1, nAGQ = 10)
summary(SDEPUTY)
sum(insight::get_variance_intercept(SDEPUTY)) / 
  (sum(insight::get_variance_intercept(SDEPUTY)) + insight::get_variance_residual(SDEPUTY)) 

SENATOR <- glmer(SENATOR ~ 1 + (1 | REGION), family = binomial, data = PSB1, nAGQ = 10)
summary(SENATOR)
sum(insight::get_variance_intercept(SENATOR)) / 
  (sum(insight::get_variance_intercept(SENATOR)) + insight::get_variance_residual(SENATOR)) 

FDEPUTY <- glmer(`FEDERAL DEPUTY` ~ 1 + (1 | REGION), family = binomial, data = PSB1, nAGQ = 10)
summary(FDEPUTY)
sum(insight::get_variance_intercept(FDEPUTY)) / 
  (sum(insight::get_variance_intercept(FDEPUTY)) + insight::get_variance_residual(FDEPUTY)) 

MAYOR <- glmer(MAYOR ~ 1 + (1 | REGION), family = binomial, data = PSB1, nAGQ = 10)
summary(MAYOR)
sum(insight::get_variance_intercept(MAYOR)) / 
  (sum(insight::get_variance_intercept(MAYOR)) + insight::get_variance_residual(MAYOR)) 

GOVERNOR <- glmer(GOVERNOR ~ 1 + (1 | REGION), family = binomial, data = PSB1, nAGQ = 10)
summary(GOVERNOR)
sum(insight::get_variance_intercept(GOVERNOR)) / 
  (sum(insight::get_variance_intercept(GOVERNOR)) + insight::get_variance_residual(GOVERNOR)) 

MINISTER <- glmer(MINISTER ~ 1 + (1 | REGION), family = binomial, data = PSB1, nAGQ = 10)
summary(MINISTER)
sum(insight::get_variance_intercept(MINISTER)) / 
  (sum(insight::get_variance_intercept(MINISTER)) + insight::get_variance_residual(MINISTER)) 

SSECRETARY <- glmer(`STATE SECRETARY` ~ 1 + (1 | REGION), family = binomial, data = PSB1, nAGQ = 10)
summary(SSECRETARY)
sum(insight::get_variance_intercept(SSECRETARY)) / 
  (sum(insight::get_variance_intercept(SSECRETARY)) + insight::get_variance_residual(SSECRETARY)) 

LEFTRIGHT <- clmm(`LEFT RIGHT` ~ 1 + (1 | REGION), Hess = TRUE, data = PSB1, nAGQ = 10)
summary(LEFTRIGHT)
sum(insight::get_variance_intercept(LEFTRIGHT)) / 
  (sum(insight::get_variance_intercept(LEFTRIGHT)) + insight::get_variance_residual(LEFTRIGHT)) 



# PSDB
PSDB1 <- na.omit(PSDB)

FIDELITY <- glmer(FIDELITY ~ 1 + (1 | REGION), family = binomial, data = PSDB1, nAGQ = 10)
summary(FIDELITY)
sum(insight::get_variance_intercept(FIDELITY)) / 
  (sum(insight::get_variance_intercept(FIDELITY)) + insight::get_variance_residual(FIDELITY)) 

BELIEVE <- glmer(BELIEVE ~ 1 + (1 | REGION), family = binomial, data = PSDB1, nAGQ = 10)
summary(BELIEVE)
sum(insight::get_variance_intercept(BELIEVE)) / 
  (sum(insight::get_variance_intercept(BELIEVE)) + insight::get_variance_residual(BELIEVE)) 

EFFORTS <- glmer(EFFORTS ~ 1 + (1 | REGION), family = binomial, data = PSDB1, nAGQ = 10)
summary(EFFORTS)
sum(insight::get_variance_intercept(EFFORTS)) / 
  (sum(insight::get_variance_intercept(EFFORTS)) + insight::get_variance_residual(EFFORTS)) 

DECREE <- glmer(DECREE ~ 1 + (1 | REGION), family = binomial, data = PSDB1, nAGQ = 10)
summary(DECREE)
sum(insight::get_variance_intercept(DECREE)) / 
  (sum(insight::get_variance_intercept(DECREE)) + insight::get_variance_residual(DECREE)) 

MILITARY <- glmer(MILITARY ~ 1 + (1 | REGION), family = binomial, data = PSDB1, nAGQ = 10)
summary(MILITARY)
sum(insight::get_variance_intercept(MILITARY)) / 
  (sum(insight::get_variance_intercept(MILITARY)) + insight::get_variance_residual(MILITARY)) 

SWITCH <- glmer(SWITCH ~ 1 + (1 | REGION), family = binomial, data = PSDB1, nAGQ = 10)
summary(SWITCH)
sum(insight::get_variance_intercept(SWITCH)) / 
  (sum(insight::get_variance_intercept(SWITCH)) + insight::get_variance_residual(SWITCH)) 

ECONOMY <- clmm(ECONOMY ~ 1 + (1 | REGION), Hess = TRUE, data = PSDB1, nAGQ = 10)
summary(ECONOMY)
sum(insight::get_variance_intercept(ECONOMY)) / 
  (sum(insight::get_variance_intercept(ECONOMY)) + insight::get_variance_residual(ECONOMY)) 

CLIENTELISM <- glmer(CLIENTELISM ~ 1 + (1 | REGION), family = binomial, data = PSDB1, nAGQ = 10)
summary(CLIENTELISM)
sum(insight::get_variance_intercept(CLIENTELISM)) / 
  (sum(insight::get_variance_intercept(CLIENTELISM)) + insight::get_variance_residual(CLIENTELISM)) 

LINTERESTS <- glmer(`LOCAL INTERESTS` ~ 1 + (1 | REGION), family = binomial, data = PSDB1, nAGQ = 10)
summary(LINTERESTS)
sum(insight::get_variance_intercept(LINTERESTS)) / 
  (sum(insight::get_variance_intercept(LINTERESTS)) + insight::get_variance_residual(LINTERESTS)) 

FAMILY <- glmer(FAMILY ~ 1 + (1 | REGION), family = binomial, data = PSDB1, nAGQ = 10)
summary(FAMILY)
sum(insight::get_variance_intercept(FAMILY)) / 
  (sum(insight::get_variance_intercept(FAMILY)) + insight::get_variance_residual(FAMILY)) 

COUNCILLOR <- glmer(COUNCILLOR ~ 1 + (1 | REGION), family = binomial, data = PSDB1, nAGQ = 10)
summary(COUNCILLOR)
sum(insight::get_variance_intercept(COUNCILLOR)) / 
  (sum(insight::get_variance_intercept(COUNCILLOR)) + insight::get_variance_residual(COUNCILLOR)) 

SDEPUTY <- glmer(`STATE DEPUTY` ~ 1 + (1 | REGION), family = binomial, data = PSDB1, nAGQ = 10)
summary(SDEPUTY)
sum(insight::get_variance_intercept(SDEPUTY)) / 
  (sum(insight::get_variance_intercept(SDEPUTY)) + insight::get_variance_residual(SDEPUTY)) 

SENATOR <- glmer(SENATOR ~ 1 + (1 | REGION), family = binomial, data = PSDB1, nAGQ = 10)
summary(SENATOR)
sum(insight::get_variance_intercept(SENATOR)) / 
  (sum(insight::get_variance_intercept(SENATOR)) + insight::get_variance_residual(SENATOR)) 

FDEPUTY <- glmer(`FEDERAL DEPUTY` ~ 1 + (1 | REGION), family = binomial, data = PSDB1, nAGQ = 10)
summary(FDEPUTY)
sum(insight::get_variance_intercept(FDEPUTY)) / 
  (sum(insight::get_variance_intercept(FDEPUTY)) + insight::get_variance_residual(FDEPUTY)) 

MAYOR <- glmer(MAYOR ~ 1 + (1 | REGION), family = binomial, data = PSDB1, nAGQ = 10)
summary(MAYOR)
sum(insight::get_variance_intercept(MAYOR)) / 
  (sum(insight::get_variance_intercept(MAYOR)) + insight::get_variance_residual(MAYOR)) 

GOVERNOR <- glmer(GOVERNOR ~ 1 + (1 | REGION), family = binomial, data = PSDB1, nAGQ = 10)
summary(GOVERNOR)
sum(insight::get_variance_intercept(GOVERNOR)) / 
  (sum(insight::get_variance_intercept(GOVERNOR)) + insight::get_variance_residual(GOVERNOR)) 

MINISTER <- glmer(MINISTER ~ 1 + (1 | REGION), family = binomial, data = PSDB1, nAGQ = 10)
summary(MINISTER)
sum(insight::get_variance_intercept(MINISTER)) / 
  (sum(insight::get_variance_intercept(MINISTER)) + insight::get_variance_residual(MINISTER)) 

SSECRETARY <- glmer(`STATE SECRETARY` ~ 1 + (1 | REGION), family = binomial, data = PSDB1, nAGQ = 10)
summary(SSECRETARY)
sum(insight::get_variance_intercept(SSECRETARY)) / 
  (sum(insight::get_variance_intercept(SSECRETARY)) + insight::get_variance_residual(SSECRETARY)) 

LEFTRIGHT <- clmm(`LEFT RIGHT` ~ 1 + (1 | REGION), Hess = TRUE, data = PSDB1, nAGQ = 10)
summary(LEFTRIGHT)
sum(insight::get_variance_intercept(LEFTRIGHT)) / 
  (sum(insight::get_variance_intercept(LEFTRIGHT)) + insight::get_variance_residual(LEFTRIGHT)) 



# Others
OTHERS <- na.omit(Other)

FIDELITY <- glmer(FIDELITY ~ 1 + (1 | REGION), family = binomial, data = OTHERS, nAGQ = 10)
summary(FIDELITY)
sum(insight::get_variance_intercept(FIDELITY)) / 
  (sum(insight::get_variance_intercept(FIDELITY)) + insight::get_variance_residual(FIDELITY)) 

BELIEVE <- glmer(BELIEVE ~ 1 + (1 | REGION), family = binomial, data = OTHERS, nAGQ = 10)
summary(BELIEVE)
sum(insight::get_variance_intercept(BELIEVE)) / 
  (sum(insight::get_variance_intercept(BELIEVE)) + insight::get_variance_residual(BELIEVE)) 

EFFORTS <- glmer(EFFORTS ~ 1 + (1 | REGION), family = binomial, data = OTHERS, nAGQ = 10)
summary(EFFORTS)
sum(insight::get_variance_intercept(EFFORTS)) / 
  (sum(insight::get_variance_intercept(EFFORTS)) + insight::get_variance_residual(EFFORTS)) 

DECREE <- glmer(DECREE ~ 1 + (1 | REGION), family = binomial, data = OTHERS, nAGQ = 10)
summary(DECREE)
sum(insight::get_variance_intercept(DECREE)) / 
  (sum(insight::get_variance_intercept(DECREE)) + insight::get_variance_residual(DECREE)) 

MILITARY <- glmer(MILITARY ~ 1 + (1 | REGION), family = binomial, data = OTHERS, nAGQ = 10)
summary(MILITARY)
sum(insight::get_variance_intercept(MILITARY)) / 
  (sum(insight::get_variance_intercept(MILITARY)) + insight::get_variance_residual(MILITARY)) 

SWITCH <- glmer(SWITCH ~ 1 + (1 | REGION), family = binomial, data = OTHERS, nAGQ = 10)
summary(SWITCH)
sum(insight::get_variance_intercept(SWITCH)) / 
  (sum(insight::get_variance_intercept(SWITCH)) + insight::get_variance_residual(SWITCH)) 

ECONOMY <- clmm(ECONOMY ~ 1 + (1 | REGION), Hess = TRUE, data = OTHERS, nAGQ = 10)
summary(ECONOMY)
sum(insight::get_variance_intercept(ECONOMY)) / 
  (sum(insight::get_variance_intercept(ECONOMY)) + insight::get_variance_residual(ECONOMY)) 

CLIENTELISM <- glmer(CLIENTELISM ~ 1 + (1 | REGION), family = binomial, data = OTHERS, nAGQ = 10)
summary(CLIENTELISM)
sum(insight::get_variance_intercept(CLIENTELISM)) / 
  (sum(insight::get_variance_intercept(CLIENTELISM)) + insight::get_variance_residual(CLIENTELISM)) 

LINTERESTS <- glmer(`LOCAL INTERESTS` ~ 1 + (1 | REGION), family = binomial, data = OTHERS, nAGQ = 10)
summary(LINTERESTS)
sum(insight::get_variance_intercept(LINTERESTS)) / 
  (sum(insight::get_variance_intercept(LINTERESTS)) + insight::get_variance_residual(LINTERESTS)) 

FAMILY <- glmer(FAMILY ~ 1 + (1 | REGION), family = binomial, data = OTHERS, nAGQ = 10)
summary(FAMILY)
sum(insight::get_variance_intercept(FAMILY)) / 
  (sum(insight::get_variance_intercept(FAMILY)) + insight::get_variance_residual(FAMILY)) 

COUNCILLOR <- glmer(COUNCILLOR ~ 1 + (1 | REGION), family = binomial, data = OTHERS, nAGQ = 10)
summary(COUNCILLOR)
sum(insight::get_variance_intercept(COUNCILLOR)) / 
  (sum(insight::get_variance_intercept(COUNCILLOR)) + insight::get_variance_residual(COUNCILLOR)) 

SDEPUTY <- glmer(`STATE DEPUTY` ~ 1 + (1 | REGION), family = binomial, data = OTHERS, nAGQ = 10)
summary(SDEPUTY)
sum(insight::get_variance_intercept(SDEPUTY)) / 
  (sum(insight::get_variance_intercept(SDEPUTY)) + insight::get_variance_residual(SDEPUTY)) 

SENATOR <- glmer(SENATOR ~ 1 + (1 | REGION), family = binomial, data = OTHERS, nAGQ = 10)
summary(SENATOR)
sum(insight::get_variance_intercept(SENATOR)) / 
  (sum(insight::get_variance_intercept(SENATOR)) + insight::get_variance_residual(SENATOR)) 

FDEPUTY <- glmer(`FEDERAL DEPUTY` ~ 1 + (1 | REGION), family = binomial, data = OTHERS, nAGQ = 10)
summary(FDEPUTY)
sum(insight::get_variance_intercept(FDEPUTY)) / 
  (sum(insight::get_variance_intercept(FDEPUTY)) + insight::get_variance_residual(FDEPUTY)) 

MAYOR <- glmer(MAYOR ~ 1 + (1 | REGION), family = binomial, data = OTHERS, nAGQ = 10)
summary(MAYOR)
sum(insight::get_variance_intercept(MAYOR)) / 
  (sum(insight::get_variance_intercept(MAYOR)) + insight::get_variance_residual(MAYOR)) 

GOVERNOR <- glmer(GOVERNOR ~ 1 + (1 | REGION), family = binomial, data = OTHERS, nAGQ = 10)
summary(GOVERNOR)
sum(insight::get_variance_intercept(GOVERNOR)) / 
  (sum(insight::get_variance_intercept(GOVERNOR)) + insight::get_variance_residual(GOVERNOR)) 

MINISTER <- glmer(MINISTER ~ 1 + (1 | REGION), family = binomial, data = OTHERS, nAGQ = 10)
summary(MINISTER)
sum(insight::get_variance_intercept(MINISTER)) / 
  (sum(insight::get_variance_intercept(MINISTER)) + insight::get_variance_residual(MINISTER)) 

SSECRETARY <- glmer(`STATE SECRETARY` ~ 1 + (1 | REGION), family = binomial, data = OTHERS, nAGQ = 10)
summary(SSECRETARY)
sum(insight::get_variance_intercept(SSECRETARY)) / 
  (sum(insight::get_variance_intercept(SSECRETARY)) + insight::get_variance_residual(SSECRETARY)) 

LEFTRIGHT <- clmm(`LEFT RIGHT` ~ 1 + (1 | REGION), Hess = TRUE, data = OTHERS, nAGQ = 10)
summary(LEFTRIGHT)
sum(insight::get_variance_intercept(LEFTRIGHT)) / 
  (sum(insight::get_variance_intercept(LEFTRIGHT)) + insight::get_variance_residual(LEFTRIGHT)) 

