#the following source code was modified from resources provided by Dr. Ron Rogge, see Macri & Rogge, 2024
#code below is for effects of attachment-based interventions
#on caregiver global distress WITHIN PERSON aka pre-post
#at post-treatment

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
dat4 <- read.csv("C:\\Users\\hswer\\Documents\\Comps\\MetaMasterTable_CG_reduced_searchalertsadded.csv")
View(dat4)

# making subset data frames so we can examine effects using
# within-person caregiver global distress posttreatment

dat4.ppgd <- subset(dat4, GdWP == 1)
dat4.ppgd



#####################################################################
#####################################################################
#####################################################################
#####################################################################
# global distress within-person pre post
#####################################################################
#####################################################################
#####################################################################
#####################################################################

##########################################################
##########################################################
##########################################################
# ppgd
##########################################################
##########################################################
##########################################################

##########################################################
##########################################################


### step 1: calculate meta analytic estimate
###caregiver global distress within person 
##############################################

# random effects
reml.ppgd <- rma(
  yi = GlobES_C_WP, 
  vi = VarGdES_C_WP, 
  data = dat4.ppgd, 
  method = "REML")

# THIS IS WHAT YOU WILL GET MOST OF THE #'S IN TABLES 
summary(reml.ppgd)

### step 2: plot findings
##############################################
# simple forest plot
forest(reml.ppgd,
       order="obs",
       showweights=FALSE)

# labeled forest plot
forest(reml.ppgd,
       order="obs",
       header=TRUE,
       slab=paste(dat4.ppgd$FirstAuthor, dat4.ppgd$Year, sep="  "),
       ilab = cbind(dat4.ppgd$nEff_wpC),
       ilab.xpos = -3.8,
       mlab = "Random Effects Model Estimate",
       xlab = "Single Arm Completion Global Distress Effect",
       xlim = c(-5.7, 2.2),
       showweights=FALSE)
op <- par(cex=1, font=2)
text(cbind(-3.9), 30, cbind("N"), 
     pos = 4)
par(op)

#add treatment name to forest plot 

forest(reml.ppgd,
       order="obs",
       header=TRUE,
       slab=paste(dat4.ppgd$FirstAuthor, dat4.ppgd$Year, sep="  "),
       ilab=cbind(dat4.ppgd$nEff_wpC, dat4.ppgd$ABIAbbrev),
       ilab.xpos = c(-5.5, -4.5),
       mlab = "Random Effects Model Estimate",
       xlab = "Single Arm Caregiver Global Distress Effect",
       xlim = c(-8, 2.5),
       showweights=FALSE)
op <- par(cex = 1, font =2)
text(c(-5.5, -4.5), 10.5, c("n", "Treatment"), pos = 3)
par(op)

### step 3: outlier and influence diagnostics
##############################################
# get outlier/influence stats and plot
inf <- influence(reml.ppgd)
plot(inf)
out_lier <- subset(inf$inf, abs(inf$inf$rstudent) >= 2 & abs(inf$inf$cook.d) >=.05)
out_lier

###ignore, no influencers identified
reml.noInfluencer.ppgd <- rma(
  yi = GlobES_C_WP, 
  vi = VarGdES_C_WP, 
  data = dat4.ppgd, 
  method = "REML",
	subset = c(-17,-20))
summary(reml.noInfluencer.ppgd)


# leave-one-out
#this is another way to look for mis-typed effects
# LOOK AT THESE ROWS AND MAKE SURE THE #'S IN THE FIRST COLUMN BARELY CHANGE
ronoutput <- leave1out(reml.ppgd)
ronoutput

### step 4: publication bias
##############################################
# funnel plot
# WE NEED TO BE ABLE TO SAY THAT WE AT LEAST EXAMINED THESE
funnel(reml.ppgd)

# Egger's regression test to see if funnel asymmetry is significant
# THE FINAL COLUMNS OF TABLES 
regtest(reml.ppgd)

# trim and fill 
# THIS WILL GIVE YOU THE ADJ EST FOR TABLES 2 THRU 5
# FOR ANY ROWS THAT SHOW SIGNIFICANT FUNNEL ASYMETRY
tf.tf <- trimfill(reml.ppgd)
tf.tf
funnel(tf.tf)

### step 5: selection method analyses
##############################################
# first, getting rid of rows of missing data in a new dataframe
dat4.ppgdNoMissing <- subset(dat4.ppgd, is.na(GlobES_C_WP) == FALSE)
dat4.ppgdNoMissing


# second, making the correct vectors

t.stat <- (dat4.ppgdNoMissing$GlobES_C_WP*sqrt(
  dat4.ppgdNoMissing$nEff_wpC-2))/2
t.stat

n.pre <- dat4.ppgdNoMissing$nEff_wpC
n.post <- dat4.ppgdNoMissing$nEff_wpC


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
# meta-analytic regression - moderation of global distress within-person effect
########################################################################

model <- rma(yi = GlobES_C_WP, 
             vi = VarGdES_C_WP, 
             mods = ~ Control.Group,
             data = dat4.ppgd, 
             method = "REML")
summary(model)

model <- rma(yi = GlobES_C_WP, 
             vi = VarGdES_C_WP, 
             mods = ~ HoursTxMod,
             data = dat4.ppgd, 
             method = "REML")
summary(model)

model <- rma(yi = GlobES_C_WP, 
             vi = VarGdES_C_WP, 
             mods = ~ MonthsPPMod,
             data = dat4.ppgd, 
             method = "REML")
summary(model)

model <- rma(yi = GlobES_C_WP, 
             vi = VarGdES_C_WP, 
             mods = ~ percompM,
             data = dat4.ppgd, 
             method = "REML")
summary(model)

model <- rma(yi = GlobES_C_WP, 
             vi = VarGdES_C_WP, 
             mods = ~ FidelityMonitoring,
             data = dat4.ppgd, 
             method = "REML")
summary(model)



