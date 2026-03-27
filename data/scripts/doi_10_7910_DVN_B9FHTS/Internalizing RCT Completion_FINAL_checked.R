#the following source code was modified from resources provided by Dr. Ron Rogge, see Macri & Rogge, 2024
#code below is for effects of attachment-based interventions
#on child internalizing relative to controls aka RCT
#at post-treatment

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
dat8 <- read.csv("C:\\Users\\hswer\\Documents\\Comps\\MetaMasterTable_child_reduced.csv")
View(dat8)

# making subset data frames so we can examine effects using
# specific designs and outcomes
# RCT designs examining child internalizing relative to control
#at treatment completion 

dat8.rctint <- subset(dat8, CIntRCT == 1)
dat8.rctint



#####################################################################
#####################################################################
#####################################################################
#####################################################################
# child internalizing RCT- post treatment 
#####################################################################
#####################################################################
#####################################################################
#####################################################################

##########################################################
##########################################################
##########################################################
# rctint
##########################################################
##########################################################
##########################################################

##########################################################
# between PERSON (between group d post treatment)
##########################################################


### step 1: calculate meta analytic estimate
#child internalizing RCT
##############################################


# random effects
reml.rctint <- rma(
  yi = IntES_C_RCT, 
  vi = VarInt_C_RCT, 
  data = dat8.rctint, 
  method = "REML")

# THIS IS WHAT YOU WILL GET MOST OF THE #'S IN TABLES
summary(reml.rctint)


### step 2: plot findings
##############################################
# simple forest plot
forest(reml.rctint,
       order="obs",
       showweights=FALSE)

# labeled forest plot
forest(reml.rctint,
       order="obs",
       header=TRUE,
       slab=paste(dat8.rctint$FirstAuthor, dat8.rctint$Year, sep="  "),
       ilab = cbind(dat8.rctint$ntotEff_rctCc),
       ilab.xpos = -2.2,
       mlab = "Random Effects Model Estimate",
       xlab = "RCT Completion Global Distress Effect",
       xlim = c(-4.2, 1.5),
       showweights=FALSE)
op <- par(cex=1, font=2)
text(c(-2.3), 38, c("N"), 
     pos = -2.2)
par(op)

#labeled forest plot with treatment names column 
forest(reml.rctint,
       order="obs",
       header=TRUE,
       slab=paste(dat8.rctint$FirstAuthor, dat8.rctint$Year, sep="  "),
       ilab=cbind(dat8.rctint$ntotEff_rctCc, dat8.rctint$ABIabbrev),
       ilab.xpos = c(-6, -4.5),
       mlab = "Random Effects Model Estimate",
       xlab = "RCT Post-Treatment Internalizing Effect",
       xlim = c(-10, 4),
       showweights=FALSE)
op <- par(cex = 1, font =2)
text(c(-6, -4.5), 7.5, c("n", "Treatment"), pos = 3)
par(op)

### step 3: outlier and influence diagnostics
##############################################
# get outlier/influence stats and plot
#rct internalizing 
inf <- influence(reml.rctint)
plot(inf)
out_lier <- subset(inf$inf, abs(inf$inf$rstudent) >= 2 & abs(inf$inf$cook.d) >=.05)
out_lier

#reruns with study 6 removed (Sprang)
reml.noInfluencer.rctint <- rma(
  yi = IntES_C_RCT, 
  vi = VarInt_C_RCT, 
  data = dat8.rctint, 
  method = "REML",
	subset = c(-6))
summary(reml.noInfluencer.rctint)


# leave-one-out
#this is another way to look for mis-typed effects
# LOOK AT THESE ROWS AND MAKE SURE THE #'S IN THE FIRST COLUMN BARELY CHANGE
#rct child internalizing 
ronoutput <- leave1out(reml.rctint)
ronoutput

### step 4: publication bias
##############################################
# funnel plot
# WE NEED TO BE ABLE TO SAY THAT WE AT LEAST EXAMINED THESE
funnel(reml.rctint)




# Egger's regression test to see if funnel asymmetry is significant
# THE FINAL COLUMNS OF TABLES
regtest(reml.rctint)




# trim and fill 
# THIS WILL GIVE YOU THE ADJ EST FOR TABLES
# FOR ANY ROWS THAT SHOW SIGNIFICANT FUNNEL ASYMETRY
tf.tf <- trimfill(reml.rctint)
tf.tf
funnel(tf.tf)

### step 5: selection method analyses
##############################################
# first, getting rid of rows of missing data in a new dataframe
#rct child internalizing 
dat8.rctintNoMissing <- subset(dat8.rctint, is.na(IntES_C_RCT) == FALSE)
dat8.rctintNoMissing


# second, making the correct vectors

t.stat <- (dat8.rctintNoMissing$IntES_C_RCT*sqrt(
  dat8.rctintNoMissing$ntotEff_rctCc-2))/2
t.stat

n.Tx <- dat8.rctintNoMissing$nTx_rctCc
n.control <- dat8.rctintNoMissing$ncntrl_rctCc

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


### step 5: selection method analyses
##############################################
# pulling in the data with outlier removed 
####******Sprang removed****** 
dat9 <- read.csv("C:\\Users\\hswer\\Documents\\Comps\\MetaMasterTable_child_reduced1.csv")
View(dat9)

# making subset data frames so we can examine effects using
# for child internalizing RCT with SPRANG outlier removed 

dat9.rctint1 <- subset(dat9, CIntRCT == 1)
dat9.rctint1

reml.rctint1 <- rma(
  yi = IntES_C_RCT, 
  vi = VarInt_C_RCT, 
  data = dat9.rctint1, 
  method = "REML")
# THIS IS WHAT YOU WILL GET MOST OF THE #'S IN TABLES 
summary(reml.rctint1)

# WE NEED TO BE ABLE TO SAY THAT WE AT LEAST EXAMINED THESE
#outlier removed funnel 
funnel(reml.rctint1)




# Egger's regression test to see if funnel asymmetry is significant
# THE FINAL COLUMNS OF TABLES
#outlier removed 
regtest(reml.rctint1)


#selection method analyses with outlier removed 

# first, getting rid of rows of missing data in a new dataframe
dat9.rctintNoMissing <- subset(dat9.rctint1, is.na(IntES_C_RCT) == FALSE)
dat9.rctintNoMissing


# second, making the correct vectors

t.stat <- (dat9.rctintNoMissing$IntES_C_RCT*sqrt(
  dat9.rctintNoMissing$ntotEff_rctCc-2))/2
t.stat

n.Tx <- dat9.rctintNoMissing$nTx_rctCc
n.control <- dat9.rctintNoMissing$ncntrl_rctCc

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
# meta-analytic regression - moderators- child internalizing RCT
########################################################################

model <- rma(yi = IntES_C_RCT, 
             vi = VarInt_C_RCT, 
             mods = ~ HoursTx,
             data = dat8.rctint, 
             method = "REML")
summary(model)

model <- rma(yi = IntES_C_RCT, 
             vi = VarInt_C_RCT, 
             mods = ~ Months2Completion_RCTc,
             data = dat8.rctint, 
             method = "REML")
summary(model)

model <- rma(yi = IntES_C_RCT, 
             vi = VarInt_C_RCT, 
             mods = ~ completers,
             data = dat8.rctint, 
             method = "REML")
summary(model)

model <- rma(yi = IntES_C_RCT, 
             vi = VarInt_C_RCT, 
             mods = ~ FidelityMonitoring,
             data = dat8.rctint, 
             method = "REML")
summary(model)

model <- rma(yi = IntES_C_RCT, 
             vi = VarInt_C_RCT, 
             mods = ~ CntrlType,
             data = dat8.rctint, 
             method = "REML")
summary(model)
