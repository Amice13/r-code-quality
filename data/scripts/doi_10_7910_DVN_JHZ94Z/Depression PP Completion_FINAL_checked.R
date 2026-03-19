#the following source code was modified from resources provided by Dr. Ron Rogge, see Macri & Rogge, 2024
#code below is for effects of attachment-based interventions
#on WITHIN-PERSON caregiver depression aka pre-post designs
#at immediate post-treatment assessment

#importing packages
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
dat2 <- read.csv("C:\\Users\\hswer\\Documents\\Comps\\MetaMasterTable_CG_reduced_searchalertsadded.csv")
View(dat2)

# making subset data frames so we can examine effects using
# specific designs - in this case within-person designs
#testing caregiver depression outcomes

dat2.ppdep <- subset(dat2, DepWP == 1)
dat2.ppdep

#####################################################################
#####################################################################
#####################################################################
#####################################################################
# depression WITHIN-PERSON
#####################################################################
#####################################################################
#####################################################################
#####################################################################

##########################################################
##########################################################
##########################################################
# depPP
##########################################################
##########################################################
##########################################################

##########################################################
##########################################################


### step 1: calculate meta analytic estimate WITHIN PERSON DEP
##############################################

# random effects
reml.ppdep <- rma(
  yi = DepES_C_WP, 
  vi = VarDepES_C_WP, 
  data = dat2.ppdep, 
  method = "REML")

# THIS IS WHAT YOU WILL GET MOST OF THE #'S IN TABLES 
summary(reml.ppdep)

#rerunning all with outliers removed

## pulling in the data - OUTLIERS REMOVED
dat21 <- read.csv("C:\\Users\\hswer\\Documents\\Comps\\MetaMasterTable_CG_reduced_searchalertsadded_cgdo.csv")
View(dat21)

# making subset data frames so we can examine effects using
# specific designs - in this case within-person designs
#testing caregiver depression outcomes
#OUTLIERS REMOVED

dat21.ppdepo <- subset(dat21, DepWP == 1)
dat21.ppdepo

# random effects OUTLIERS REMOVED
reml.ppdepo <- rma(
  yi = DepES_C_WP, 
  vi = VarDepES_C_WP, 
  data = dat21.ppdepo, 
  method = "REML")

# THIS IS WHAT YOU WILL GET MOST OF THE #'S IN TABLES
#outliers removed
summary(reml.ppdepo)

### step 2: plot findings
##############################################
# simple forest plot
forest(reml.ppdep,
       order="obs",
       showweights=FALSE)

# labeled forest plot
forest(reml.ppdep,
       order="obs",
       header=TRUE,
       slab=paste(dat2.ppdep$FirstAuthor, dat2.ppdep$Year, sep="  "),
       ilab = cbind(dat2.ppdep$nEff_wpC),
       ilab.xpos = -3.8,
       mlab = "Random Effects Model Estimate",
       xlab = "Single Arm Completion Depression Effect",
       xlim = c(-5.7, 2.2),
       showweights=FALSE)
op <- par(cex=1, font=2)
text(cbind(-3.9), 30, cbind("N"), 
     pos = 4)
par(op)

#forest plot with column for treatment name 

forest(reml.ppdep,
       order="obs",
       header=TRUE,
       slab=paste(dat2.ppdep$FirstAuthor, dat2.ppdep$Year, sep="  "),
       ilab=cbind(dat2.ppdep$nEff_wpC, dat2.ppdep$ABIAbbrev),
       ilab.xpos = c(-5.5, -4.5),
       mlab = "Random Effects Model Estimate",
       xlab = "Single Arm Caregiver Depression Effect",
       xlim = c(-8, 2.5),
       showweights=FALSE)
op <- par(cex = 1, font =2)
text(c(-5.5, -4.5), 24, c("n", "Treatment"), pos = 3)
par(op)

### step 3: outlier and influence diagnostics
##############################################
# get outlier/influence stats and plot
inf <- influence(reml.ppdep)
plot(inf)
out_lier <- subset(inf$inf, abs(inf$inf$rstudent) >= 2 & abs(inf$inf$cook.d) >=.05)
out_lier

reml.noInfluencer.ppdep <- rma(
  yi = DepES_C_WP, 
  vi = VarDepES_C_WP, 
  data = dat2.ppdep, 
  method = "REML",
	subset = c(-4,-10))
summary(reml.noInfluencer.ppdep)


# leave-one-out
#this is another way to look for mis-typed effects
# LOOK AT THESE ROWS AND MAKE SURE THE #'S IN THE FIRST COLUMN BARELY CHANGE
ronoutput <- leave1out(reml.ppdep)
ronoutput

### step 4: publication bias
##############################################
# funnel plot
# WE NEED TO BE ABLE TO SAY THAT WE AT LEAST EXAMINED THESE
funnel(reml.ppdep)

# Egger's regression test to see if funnel asymmetry is significant
# THE FINAL COLUMNS OF TABLES 
regtest(reml.ppdep)


### step 4: publication bias
##############################################
# funnel plot - OUTLIERS REMOVED

funnel(reml.ppdepo)

# Egger's regression test to see if funnel asymmetry is significant
# THE FINAL COLUMNS OF TABLES 
regtest(reml.ppdepo)

# trim and fill 
# THIS WILL GIVE YOU THE ADJ EST FOR TABLES 
# FOR ANY ROWS THAT SHOW SIGNIFICANT FUNNEL ASYMETRY
tf.tf <- trimfill(reml.ppdep)
tf.tf
funnel(tf.tf)

# trim and fill - ADJUSTED EFFECT
# THIS WILL GIVE YOU THE ADJ EST FOR TABLES 
# FOR ANY ROWS THAT SHOW SIGNIFICANT FUNNEL ASYMETRY
tf.tf <- trimfill(reml.ppdepo)
tf.tf
funnel(tf.tf)

### step 5: selection method analyses
##############################################
# first, getting rid of rows of missing data in a new dataframe
dat2.ppdepNoMissing <- subset(dat2.ppdep, is.na(DepES_C_WP) == FALSE)
dat2.ppdepNoMissing


# second, making the correct vectors

t.stat <- (dat2.ppdepNoMissing$DepES_C_WP*sqrt(
  dat2.ppdepNoMissing$nEff_wpC-2))/2
t.stat

n.pre <- dat2.ppdepNoMissing$nEff_wpC
n.post <- dat2.ppdepNoMissing$nEff_wpC


init.value <- c(0.5, 0.2, 0.10)
alpha <- .05
# Three-parameter selection model
# MBH Implmentation: init.value gives an initial guess for the effect size, heterogeneity, and relative
# likelihood of reporting a study that is not statistically significant and directionally consistent.
estimate.onestep.selection.heterogeneous(
  t.stat, 
  n.pre,
  n.post,
  alpha/2, 
  init.value)

#run selection method analyses again with OUTLIERS REMOVED

# first, getting rid of rows of missing data in a new dataframe
dat21.ppdepoNoMissing <- subset(dat21.ppdepo, is.na(DepES_C_WP) == FALSE)
dat21.ppdepoNoMissing


# second, making the correct vectors

t.stat <- (dat21.ppdepoNoMissing$DepES_C_WP*sqrt(
  dat21.ppdepoNoMissing$nEff_wpC-2))/2
t.stat

n.pre <- dat21.ppdepoNoMissing$nEff_wpC
n.post <- dat21.ppdepoNoMissing$nEff_wpC


init.value <- c(0.5, 0.2, 0.10)
alpha <- .05
# Three-parameter selection model
# MBH Implmentation: init.value gives an initial guess for the effect size, heterogeneity, and relative
# likelihood of reporting a study that is not statistically significant and directionally consistent.
estimate.onestep.selection.heterogeneous(
  t.stat, 
  n.pre,
  n.post,
  alpha/2, 
  init.value)

########################################################################
# meta-analytic regression - #moderators for within-person depression
########################################################################
######run on adjusted meta analytic effect

model <- rma(yi = DepES_C_WP, 
             vi = VarDepES_C_WP, 
             mods = ~ Control.Group,
             data = dat21.ppdepo, 
             method = "REML")
summary(model)

model <- rma(yi = DepES_C_WP, 
             vi = VarDepES_C_WP, 
             mods = ~ HoursTxMod,
             data = dat21.ppdepo, 
             method = "REML")
summary(model)

model <- rma(yi = DepES_C_WP, 
             vi = VarDepES_C_WP, 
             mods = ~ MonthsPPMod,
             data = dat21.ppdepo, 
             method = "REML")
summary(model)

model <- rma(yi = DepES_C_WP, 
             vi = VarDepES_C_WP, 
             mods = ~ percompM,
             data = dat21.ppdepo, 
             method = "REML")
summary(model)

model <- rma(yi = DepES_C_WP, 
             vi = VarDepES_C_WP, 
             mods = ~ FidelityMonitoring,
             data = dat21.ppdepo, 
             method = "REML")
summary(model)





