####getting set up### 

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

# PULLING IN SELECTION METHOD FUNCTIONS  
source("C:\\Users\\hswer\\Documents\\Comps\\selection.meta.functions.R")


# pulling in the data 
dat3 <- read.csv("C:\\Users\\hswer\\Documents\\Comps\\MetaMasterTable_child_reduced_searchalertsadded.csv")
View(dat3)

# making subset data frames so we can examine effects 
# for child externalizing relative to controls

dat3.rctext <- subset(dat3, CExtRCT == 1)
dat3.rctext



#####################################################################
#####################################################################
#####################################################################
#####################################################################
# ext RCT- all 
#####################################################################
#####################################################################
#####################################################################
#####################################################################

##########################################################
##########################################################
##########################################################
# extrct
##########################################################
##########################################################
##########################################################

##########################################################
# between PERSON (between group d post treatment)
##########################################################


### step 1: calculate meta analytic estimate
####child externalizing relative to control at post-treatment
##############################################


# random effects
reml.rctext <- rma(
  yi = ExtES_C_RCT, 
  vi = VarExt_C_RCT, 
  data = dat3.rctext, 
  method = "REML")

# THIS IS WHAT YOU WILL GET MOST OF THE #'S IN TABLES
summary(reml.rctext)


### step 2: plot findings
##############################################
# simple forest plot
forest(reml.rctext,
       order="obs",
       showweights=FALSE)

# labeled forest plot
forest(reml.rctext,
       order="obs",
       header=TRUE,
       slab=paste(dat3.rctext$FirstAuthor, dat3.rctext$Year, sep="  "),
       ilab = cbind(dat3.rctext$ntotEff_rctCc),
       ilab.xpos = -3,
       mlab = "Random Effects Model Estimate",
       xlab = "RCT Completion Global Distress Effect",
       xlim = c(-4.2, 1.5),
       showweights=FALSE)
op <- par(cex=1, font=2)
text(c(-2.3), 38, c("N"), 
     pos = 3)
par(op)

###fully labeled forest plot with treatment type 
forest(reml.rctext,
       order="obs",
       header=TRUE,
       slab=paste(dat3.rctext$FirstAuthor, dat3.rctext$Year, sep="  "),
       ilab=cbind(dat3.rctext$ntotEff_rctCc, dat3.rctext$ABIabbrev),
       ilab.xpos = c(-7, -6),
       mlab = "Random Effects Model Estimate",
       xlab = "RCT Post-Treatment Externalizing Effect",
       xlim = c(-10, 2.5),
       showweights=FALSE)
op <- par(cex = 1, font =2)
text(c(-7, -6), 11.5, c("n", "Treatment"), pos = 3)
par(op)

### step 3: outlier and influence diagnostics
##############################################
# get outlier/influence stats and plot
inf <- influence(reml.rctext)
plot(inf)
out_lier <- subset(inf$inf, abs(inf$inf$rstudent) >= 2 & abs(inf$inf$cook.d) >=.05)
out_lier

reml.noInfluencer.rctext <- rma(
  yi = ExtES_C_RCT, 
  vi = VarExt_C_RCT, 
  data = dat3.rctext, 
  method = "REML",
	subset = c(-8))
summary(reml.noInfluencer.rctext)


# leave-one-out
#this is another way to look for mis-typed effects
# LOOK AT THESE ROWS AND MAKE SURE THE #'S IN THE FIRST COLUMN BARELY CHANGE
ronoutput <- leave1out(reml.rctext)
ronoutput

### step 4: publication bias
##############################################
# funnel plot
# WE NEED TO BE ABLE TO SAY THAT WE AT LEAST EXAMINED THESE
funnel(reml.rctext)




# Egger's regression test to see if funnel asymmetry is significant

regtest(reml.rctext)

# trim and fill 
# THIS WILL GIVE YOU THE ADJ EST FOR tables
# FOR ANY ROWS THAT SHOW SIGNIFICANT FUNNEL ASYMETRY
tf.tf <- trimfill(reml.rctext)
tf.tf
funnel(tf.tf)

##funnel plot and reg test for adjusted effect 

funnel(reml.noInfluencer.rctext)
regtest(reml.noInfluencer.rctext)


# trim and fill for adjusted effect 
# THIS WILL GIVE YOU THE ADJ EST FOR tables
# FOR ANY ROWS THAT SHOW SIGNIFICANT FUNNEL ASYMETRY
tf.tf <- trimfill(reml.noinfluencer.rctext)
tf.tf
funnel(tf.tf)




### step 5: selection method analyses for unadjusted effect 
##############################################
# first, getting rid of rows of missing data in a new dataframe
dat3.rctextNoMissing <- subset(dat3.rctext, is.na(ExtES_C_RCT) == FALSE)
dat3.rctextNoMissing


# second, making the correct vectors

t.stat <- (dat3.rctextNoMissing$ExtES_C_RCT*sqrt(
  dat3.rctextNoMissing$ntotEff_rctCc-2))/2
t.stat

n.Tx <- dat3.rctextNoMissing$nTx_rctCc
n.control <- dat3.rctextNoMissing$ncntrl_rctCc

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




### step 5: selection method analyses for adjusted effect
##############################################
# pulling in the data ******Sprang removed****** 
dat4 <- read.csv("C:\\Users\\hswer\\Documents\\Comps\\MetaMasterTable_child_reduced_searchalertsadded_ceo.csv")
View(dat4)

# making subset data frames so we can examine tx effects
# externalizing RCT - OUTLIER REMOVED

dat4.rctext1 <- subset(dat4, CExtRCT == 1)
dat4.rctext1


# first, getting rid of rows of missing data in a new dataframe
dat4.rctextNoMissing <- subset(dat4.rctext1, is.na(ExtES_C_RCT) == FALSE)
dat4.rctextNoMissing


# second, making the correct vectors

t.stat <- (dat4.rctextNoMissing$ExtES_C_RCT*sqrt(
  dat4.rctextNoMissing$ntotEff_rctCc-2))/2
t.stat

n.Tx <- dat4.rctextNoMissing$nTx_rctCc
n.control <- dat4.rctextNoMissing$ncntrl_rctCc

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
# meta-analytic regression - moderation for adjusted effect
########################################################################




model <- rma(yi = ExtES_C_RCT, 
             vi = VarExt_C_RCT, 
             mods = ~ HoursTx,
             data = dat4.rctext1, 
             method = "REML")
summary(model)

model <- rma(yi = ExtES_C_RCT, 
             vi = VarExt_C_RCT, 
             mods = ~ Months2Completion_RCTc,
             data = dat4.rctext1, 
             method = "REML")
summary(model)

model <- rma(yi = ExtES_C_RCT, 
             vi = VarExt_C_RCT, 
             mods = ~ completers,
             data = dat4.rctext1, 
             method = "REML")
summary(model)

model <- rma(yi = ExtES_C_RCT, 
             vi = VarExt_C_RCT, 
             mods = ~ FidelityMonitoring,
             data = dat4.rctext1, 
             method = "REML")
summary(model)

model <- rma(yi = ExtES_C_RCT, 
             vi = VarExt_C_RCT, 
             mods = ~ CntrlType,
             data = dat4.rctext1, 
             method = "REML")
summary(model)