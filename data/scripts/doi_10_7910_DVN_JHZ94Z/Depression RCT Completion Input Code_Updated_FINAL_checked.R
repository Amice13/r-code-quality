#the following source code was modified from resources provided by Dr. Ron Rogge, see Macri & Rogge, 2024
#code below is for effects of attachment-based interventions
#on caregiver depression relative to controls aka RCT
#at immediate post-treatment assessment

#importing packages

install.packages("RTools42")
install.packages("rlang")
install.packages("metafor")
install.packages("foreign")
install.packages("psych")
install.packages("weightr")
install.packages("shiny")
install.packages("gridExtra")
install.packages("ggplot2")
install.packages("plotly")
install.packages("tidyverse")
install.packages("readr")
install.packages("stats")
install.packages("puniform")

#loading relevant libraries

library(puniform)
library(weightr)
library(shiny)
library(metafor)
library(foreign)
library(psych)
library(gridExtra)
library(ggplot2)
library(plotly)
library(graphics)
library(tidyverse)
library(readr)
library(rlang)
library(stats)

#trouble shooting reading in data

update.packages(checkBuilt=TRUE, ask=FALSE)
getwd()

# PULLING IN SELECTION METHOD FUNCTIONS ON personal COMPUTER 
source("C:\\Users\\hswer\\Documents\\Comps\\selection.meta.functions.R")


# pulling in the data 
dat1 <- read.csv("C:\\Users\\hswer\\Documents\\Comps\\MetaMasterTable_CG_reduced.csv")
View(dat1)

# making subset data frames so we can examine effects using
# specific designs - in this case RCTs testing caregiver depression outcomes

dat1.rctdep <- subset(dat1, DepRCT == 1)
dat1.rctdep



#####################################################################
#####################################################################
#####################################################################
#####################################################################
# depression RCT- all 
#####################################################################
#####################################################################
#####################################################################
#####################################################################

##########################################################
##########################################################
##########################################################
# deprct
##########################################################
##########################################################
##########################################################

##########################################################
# between PERSON (between group d post treatment)
##########################################################


### step 1: calculate meta analytic estimate - depression RCT effects
##############################################


# random effects
reml.rctdep <- rma(
  yi = DepES_C_RCT, 
  vi = VarDepES_C_RCT, 
  data = dat1.rctdep, 
  method = "REML")

# produces summary of results from command above 
summary(reml.rctdep)


### step 2: plot findings
##############################################
# simple forest plot - depression RCT
forest(reml.rctdep,
       order="obs",
       showweights=FALSE)

# labeled forest plot - depression RCT
#general plot
forest(reml.rctdep,
       order="obs",
       header=TRUE,
       slab=paste(dat1.rctdep$FirstAuthor, dat1.rctdep$Year, sep="  "),
       ilab = cbind(dat1.rctdep$ntotEff_rctC),
       ilab.xpos = -2.3,
       mlab = "Random Effects Model Estimate",
       xlab = "RCT Completion depression effect",
       xlim = c(-3.6, 1.5),
       showweights=FALSE)
op <- par(cex=1, font=2)
text(c(-2.3), 38, c("N"), 
     pos = 3)
par(op)

#plot with type of ABI specified 

forest(reml.rctdep,
       order="obs",
       header=TRUE,
       slab=paste(dat1.rctdep$FirstAuthor, dat1.rctdep$Year, sep="  "),
       ilab=cbind(dat1.rctdep$ntotEff_rctC, dat1.rctdep$ABIAbbrev),
       ilab.xpos = c(-6, -4.5),
       mlab = "Random Effects Model Estimate",
       xlab = "RCT Post-Treatment Caregiver Depression Effect",
       xlim = c(-10, 4),
       showweights=FALSE)
op <- par(cex = 1, font =2)
text(c(-6, -4.5), 16.5, c("n", "Treatment"), pos = 3)
par(op)

### step 3: outlier and influence diagnostics - depression RCT
##############################################
# get outlier/influence stats and plot
inf <- influence(reml.rctdep)
plot(inf)
out_lier <- subset(inf$inf, abs(inf$inf$rstudent) >= 2 & abs(inf$inf$cook.d) >=.05)
out_lier

reml.noInfluencer.rctdep <- rma(
  yi = DepES_C_RCT, 
  vi = VarDepES_C_RCT, 
  data = dat1.rctdep, 
  method = "REML",
	subset = c(-10, -11))
summary(reml.noInfluencer.rctdep)


# leave-one-out
#this is another way to look for mis-typed effects
# LOOK AT THESE ROWS AND MAKE SURE THE #'S IN THE FIRST COLUMN BARELY CHANGE
ronoutput <- leave1out(reml.rctdep)
ronoutput

### step 4: publication bias
##############################################
# funnel plot
# WE NEED TO BE ABLE TO SAY THAT WE AT LEAST EXAMINED THESE
funnel(reml.rctdep)




# Egger's regression test to see if funnel asymmetry is significant
regtest(reml.rctdep)




# trim and fill 
# THIS WILL GIVE YOU THE ADJ EST 
# FOR ANY ROWS THAT SHOW SIGNIFICANT FUNNEL ASYMETRY
tf.tf <- trimfill(reml.rctdep)
tf.tf
funnel(tf.tf)

### step 5: selection method analyses
##############################################
# first, getting rid of rows of missing data in a new dataframe
dat1.rctdepNoMissing <- subset(dat1.rctdep, is.na(DepES_C_RCT) == FALSE)
dat1.rctdepNoMissing


# second, making the correct vectors

t.stat <- (dat1.rctdepNoMissing$DepES_C_RCT*sqrt(
  dat1.rctdepNoMissing$ntotEff_rctC-2))/2
t.stat

n.Tx <- dat1.rctdepNoMissing$nTx_rctC
n.control <- dat1.rctdepNoMissing$ncntrl_rctC

init.value <- c(0.5, 0.2, 0.10)
alpha <- .05
# Three-parameter selection model
# MBH Implmentation: init.value gives an initial guess for the effect size, heterogeneity, and relative
# likelihood of reporting a study that is not statistically significant and directionally consistent.
estimate.onestep.selection.heterogeneous(
  t.stat, 
  n.Tx, 
  n.control, 
  alpha/2, 
  init.value)


########################################################################
# meta-analytic regression
########################################################################


#moderation of depression RCT effect by 
#whether participants were treatment seeking
#child age
#duration of treatment in months 

#moderation by total hours of treatment contact time 

model <- rma(yi = DepES_C_RCT, 
             vi = VarDepES_C_RCT, 
             mods = ~ HoursTxMod,
             data = dat1.rctdep, 
             method = "REML")
summary(model)

#moderation by duration of treatment in months 

model <- rma(yi = DepES_C_RCT, 
             vi = VarDepES_C_RCT, 
             mods = ~ MonthsRCTMod,
             data = dat1.rctdep, 
             method = "REML")
summary(model)

#moderation by percent of participants completing treatment

model <- rma(yi = DepES_C_RCT, 
             vi = VarDepES_C_RCT, 
             mods = ~ percompM,
             data = dat1.rctdep, 
             method = "REML")
summary(model)

#moderation by whether study used fidelity monitoring 

model <- rma(yi = DepES_C_RCT, 
             vi = VarDepES_C_RCT, 
             mods = ~ FidelityMonitoring,
             data = dat1.rctdep, 
             method = "REML")
summary(model)

#moderation by whether control group was a comparative evidence-based treatment
# vs care as usual or other minimal comparison conditions, e.g., pamphlets, phone calls

model <- rma(yi = DepES_C_RCT, 
             vi = VarDepES_C_RCT, 
             mods = ~ CntrlType,
             data = dat1.rctdep, 
             method = "REML")
summary(model)

