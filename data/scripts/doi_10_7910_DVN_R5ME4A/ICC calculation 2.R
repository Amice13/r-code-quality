#######################################################
# Title: Computing ICC for condition models
# Name: Julius G. Bright Ross
# Date: Sometime in Mar 2020
# Last updated: May 16, 2020
# Description: Computing ICC for different seasonal
# models without bootstrapping--computing ICC without
# special variance structures and testing significance 
# by dropping with/without REML
#######################################################

## Load in data and required packages ##

rm(list = ls())

setwd("C:/Users/jbrig/Desktop/Badgers/Badger condition/Processed datasets")
load("Condition modelling output.RData")

iccDict <- list()

library(mgcv)        #For building GAM/GAMM models
library(tidyverse)   #For many reasons
library(corrplot)    #To examine correlations between covariates
library(standardize) #To standardize covariates
library(sjPlot)      #To visualise relationships
library(cowplot)     #To build multi-panel figures

## Useful functions ##

reverseStandardize <- function(variable, toReverse) {
  sdVar    <- sd(variable)
  meanVar  <- mean(variable)
  
  reversed <- toReverse*sdVar + meanVar
  
  return(reversed)
}

## Spring model first ##

variances <- VarCorr(springModelFinal$lme)[,1]

intraSett <- as.numeric(variances[which(names(variances) == "(Intercept)")][1])
intraInd  <- as.numeric(variances[which(names(variances) == "(Intercept)")][2])
interInd  <- as.numeric(variances[which(names(variances) == "Residual")])

iccDict$iccSpr <- round(intraInd/(interInd + intraInd), 3)

springFinalREML <- gamm(formula = summerobj$mainFormula,
                         data   = summerobj$data,
                         random = list(Sett = ~1, Individual = ~1),
                         method = "REML")

springPlainREML <- gamm(formula = summerobj$mainFormula,
                        data    = summerobj$data,
                        random  = list(Sett = ~1),
                        method  = "REML")

springAnova <- anova(springFinalREML$lme, springPlainREML$lme)
iccDict$lrtSpr <- round(springAnova$L.Ratio[2])

## Summer model ##

summerModelPlain <- gamm(formula = summerobj$mainFormula,
                         data    = summerobj$data,
                         random  = list(Sett = ~1, Individual = ~1))

summerFinalREML <- gamm(formula = summerobj$mainFormula, 
                        data    = summerobj$data, 
                        random  = list(Sett = ~1, Individual = ~1),
                        method  = "REML",
                        weights = varExp(form = ~summerN))
summerPlainREML <- gamm(formula = summerobj$mainFormula, 
                        data    = summerobj$data, 
                        random  = list(Sett = ~1),
                        method  = "REML",
                        weights = varExp(form = ~summerN))

summerAnova <- anova(summerFinalREML$lme, summerPlainREML$lme)

variances <- VarCorr(summerModelPlain$lme)[,1]

intraSett <- as.numeric(variances[which(names(variances) == "(Intercept)")][1])
intraInd  <- as.numeric(variances[which(names(variances) == "(Intercept)")][2])
interInd  <- as.numeric(variances[which(names(variances) == "Residual")])

iccDict$iccSum <- round(intraInd/(interInd + intraInd), 3)
iccDict$lrtSum <- round(summerAnova$L.Ratio[2])

## Autumn model ##

autumnModelPlain <- gamm(formula = autumnobj$mainFormula,
                         data    = autumnobj$data,
                         random  = list(Sett = ~1, Individual = ~1))

autumnFinalREML <- gamm(formula = autumnobj$mainFormula, 
                        data    = autumnobj$data, 
                        random  = list(Sett = ~1, Individual = ~1),
                        method  = "REML",
                        weights = varFixed(~ autumnN))
autumnPlainREML <- gamm(formula = autumnobj$mainFormula, 
                        data    = autumnobj$data, 
                        random  = list(Sett = ~1),
                        method  = "REML",
                        weights = varFixed(~ autumnN))

autumnAnova <- anova(autumnFinalREML$lme, autumnPlainREML$lme)

variances <- VarCorr(autumnModelPlain$lme)[,1]

intraSett <- as.numeric(variances[which(names(variances) == "(Intercept)")][1])
intraInd  <- as.numeric(variances[which(names(variances) == "(Intercept)")][2])
interInd  <- as.numeric(variances[which(names(variances) == "Residual")])

iccDict$iccAut <- round(intraInd/(interInd + intraInd), 3)
iccDict$lrtAut <- round(autumnAnova$L.Ratio[2])

save(iccDict, file = "ICC.RData")