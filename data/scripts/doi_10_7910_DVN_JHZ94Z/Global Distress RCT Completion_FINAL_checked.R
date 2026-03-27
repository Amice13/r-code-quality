#the following source code was modified from resources provided by Dr. Ron Rogge, see Macri & Rogge, 2024
#code below is for effects of attachment-based interventions
#on caregiver global distress relative to controls aka RCT
#at immediate post-treatment assessment

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
dat2 <- read.csv("C:\\Users\\hswer\\Documents\\Comps\\MetaMasterTable_CG_reduced.csv")
View(dat2)

# making subset data frames so we can examine effects using
# specific study designs & outcomes
#caregiver global distress relative to control - RCT

dat2.rctgd <- subset(dat2, GdRCT == 1)
dat2.rctgd


#####################################################################
#####################################################################
#####################################################################
#####################################################################
# global distress RCT- all 
#####################################################################
#####################################################################
#####################################################################
#####################################################################

##########################################################
##########################################################
##########################################################
# rctgd
##########################################################
##########################################################
##########################################################

##########################################################
# between PERSON (between group d post treatment)
##########################################################


### step 1: calculate meta analytic estimate
##############################################


# random effects
reml.rctgd <- rma(
  yi = GlobES_C_RCT, 
  vi = VarGlbES_C_RCT, 
  data = dat2.rctgd, 
  method = "REML")

# THIS IS WHAT YOU WILL GET MOST OF THE #'S IN TABLES
summary(reml.rctgd)


### step 2: plot findings global distress RCT
##############################################
# simple forest plot
forest(reml.rctgd,
       order="obs",
       showweights=FALSE)

# labeled forest plot
forest(reml.rctgd,
       order="obs",
       header=TRUE,
       slab=paste(dat2.rctgd$FirstAuthor, dat2.rctgd$Year, sep="  "),
       ilab = cbind(dat2.rctgd$ntotEff_rctC),
       ilab.xpos = -2.3,
       mlab = "Random Effects Model Estimate",
       xlab = "RCT Completion Global Distress Effect",
       xlim = c(-3.6, 1.5),
       showweights=FALSE)
op <- par(cex=1, font=2)
text(c(-2.3), 38, c("N"), 
     pos = 3)
par(op)

#forest plot with treatment name column 

forest(reml.rctgd,
       order="obs",
       header=TRUE,
       slab=paste(dat2.rctgd$FirstAuthor, dat2.rctgd$Year, sep="  "),
       ilab=cbind(dat2.rctgd$ntotEff_rctC, dat2.rctgd$ABIAbbrev),
       ilab.xpos = c(-6, -4.5),
       mlab = "Random Effects Model Estimate",
       xlab = "RCT Post-Treatment Caregiver Global Distress Effect",
       xlim = c(-10, 4),
       showweights=FALSE)
op <- par(cex = 1, font =2)
text(c(-6, -4.5), 7.75, c("n", "Treatment"), pos = 3)
par(op)



### step 3: outlier and influence diagnostics
##############################################
# get outlier/influence stats and plot
inf <- influence(reml.rctgd)
plot(inf)
out_lier <- subset(inf$inf, abs(inf$inf$rstudent) >= 2 & abs(inf$inf$cook.d) >=.05)
out_lier

reml.noInfluencer.rctgd <- rma(
  yi = GlobES_C_RCT, 
  vi = VarGlbES_C_RCT, 
  data = dat2.rctgd, 
  method = "REML",
	subset = c(-6))
summary(reml.noInfluencer.rctgd)


# leave-one-out
#this is another way to look for mis-typed effects
# LOOK AT THESE ROWS AND MAKE SURE THE #'S IN THE FIRST COLUMN BARELY CHANGE
ronoutput <- leave1out(reml.rctgd)
ronoutput

### step 4: publication bias
##############################################
# funnel plot
# WE NEED TO BE ABLE TO SAY THAT WE AT LEAST EXAMINED THESE
funnel(reml.rctgd)

# Egger's regression test to see if funnel asymmetry is significant

regtest(reml.rctgd)


# trim and fill 
# THIS WILL GIVE YOU THE ADJ EST 
# FOR ANY ROWS THAT SHOW SIGNIFICANT FUNNEL ASYMETRY
tf.tf <- trimfill(reml.rctgd)
tf.tf
funnel(tf.tf)

### step 5: selection method analyses WITH OUTLIERS REMOVED
##############################################
# first, getting rid of rows of missing data in a new dataframe
dat2.rctgdNoMissing <- subset(dat2.rctgd, is.na(GlobES_C_RCT) == FALSE)
dat2.rctgdNoMissing


# second, making the correct vectors

t.stat <- (dat2.rctgdNoMissing$GlobES_C_RCT*sqrt(
  dat2.rctgdNoMissing$ntotEff_rctC-2))/2
t.stat

n.Tx <- dat2.rctgdNoMissing$nTx_rctC
n.control <- dat2.rctgdNoMissing$ncntrl_rctC

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


#rerun with outliers removed
dat21 <- read.csv("C:\\Users\\hswer\\Documents\\Comps\\MetaMasterTable_CG_reduced3.csv")
View(dat21)

# making subset data frames so we can examine effects using
# RCT designs with caregiver global ditress outcomes
#outliers removed 

dat21.rctgdo <- subset(dat21, GdRCT == 1)
dat21.rctgdo

# random effects
reml.rctgdo <- rma(
  yi = GlobES_C_RCT, 
  vi = VarGlbES_C_RCT, 
  data = dat21.rctgdo, 
  method = "REML")

# THIS IS WHAT YOU WILL GET MOST OF THE #'S IN TABLES
summary(reml.rctgdo)

#funnel plot outliers removed 

funnel(reml.rctgdo)

# Egger's regression test to see if funnel asymmetry is significant
#outliers removed
regtest(reml.rctgdo)

### step 5: selection method analyses
##############################################
# first, getting rid of rows of missing data in a new dataframe
dat21.rctgdoNoMissing <- subset(dat21.rctgdo, is.na(GlobES_C_RCT) == FALSE)
dat21.rctgdoNoMissing


# second, making the correct vectors

t.stat <- (dat21.rctgdoNoMissing$GlobES_C_RCT*sqrt(
  dat21.rctgdoNoMissing$ntotEff_rctC-2))/2
t.stat

n.Tx <- dat21.rctgdoNoMissing$nTx_rctC
n.control <- dat21.rctgdoNoMissing$ncntrl_rctC

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
# meta-analytic regression - moderators of global distress RCT effect
########################################################################

model <- rma(yi = GlobES_C_RCT, 
             vi = VarGlbES_C_RCT, 
             mods = ~ HoursTxMod,
             data = dat21.rctgdo, 
             method = "REML")
summary(model)

model <- rma(yi = GlobES_C_RCT, 
             vi = VarGlbES_C_RCT, 
             mods = ~ MonthsRCTMod,
             data = dat21.rctgdo, 
             method = "REML")
summary(model)

model <- rma(yi = GlobES_C_RCT, 
             vi = VarGlbES_C_RCT, 
             mods = ~ percompM,
             data = dat21.rctgdo, 
             method = "REML")
summary(model)

model <- rma(yi = GlobES_C_RCT, 
             vi = VarGlbES_C_RCT, 
             mods = ~ FidelityMonitoring,
             data = dat21.rctgdo, 
             method = "REML")
summary(model)

model <- rma(yi = GlobES_C_RCT, 
             vi = VarGlbES_C_RCT, 
             mods = ~ CntrlType,
             data = dat21.rctgdo, 
             method = "REML")
summary(model)
