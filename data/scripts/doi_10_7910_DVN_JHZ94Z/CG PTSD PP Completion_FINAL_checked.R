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


# pulling in the data - uPDATED AFTER NEW SEARCH ALERTS
dat5 <- read.csv("C:\\Users\\hswer\\Documents\\Comps\\MetaMasterTable_CG_reduced_searchalertsadded.csv")
View(dat5)

# making subset data frames so we can examine effects using
# caregiver ptsd symptoms within-person pre-post designs at posttreatment

dat5.pptr <- subset(dat5, TraumaWP == 1)
dat5.pptr



#####################################################################
#####################################################################
#####################################################################
#####################################################################
# posttraumatic stress within-person 
#####################################################################
#####################################################################
#####################################################################
#####################################################################

##########################################################
##########################################################
##########################################################
# pptr
##########################################################
##########################################################
##########################################################

##########################################################
##########################################################


### step 1: calculate meta analytic estimate
#####PTS within person
##############################################

# random effects
reml.pptr <- rma(
  yi = TraumaES_C_WP, 
  vi = VarTraES_C_WP, 
  data = dat5.pptr, 
  method = "REML")

# THIS IS WHAT YOU WILL GET MOST OF THE #'S IN TABLES 
summary(reml.pptr)

### step 2: plot findings
##############################################
# simple forest plot
forest(reml.pptr,
       order="obs",
       showweights=FALSE)

# labeled forest plot
forest(reml.pptr,
       order="obs",
       header=TRUE,
       slab=paste(dat5.pptr$FirstAuthor, dat5.pptr$Year, sep="  "),
       ilab = cbind(dat5.pptr$nEff_wpC),
       ilab.xpos = -3.8,
       mlab = "Random Effects Model Estimate",
       xlab = "Single Arm Completion PTS Effect",
       xlim = c(-5.7, 2.2),
       showweights=FALSE)
op <- par(cex=1, font=2)
text(cbind(-3.9), 30, cbind("N"), 
     pos = 4)
par(op)

#forest plot with treatment name 
forest(reml.pptr,
       order="obs",
       header=TRUE,
       slab=paste(dat5.pptr$FirstAuthor, dat5.pptr$Year, sep="  "),
       ilab=cbind(dat5.pptr$nEff_wpC, dat5.pptr$ABIAbbrev),
       ilab.xpos = c(-5.5, -4.1),
       mlab = "Random Effects Model Estimate",
       xlab = "Single Arm Caregiver PTS Effect",
       xlim = c(-8, 2.5),
       showweights=FALSE)
op <- par(cex = 1, font =2)
text(c(-5.5, -4.1), 6.7, c("n", "Treatment"), pos = 3)
par(op)


### step 3: outlier and influence diagnostics
##############################################
# get outlier/influence stats and plot
inf <- influence(reml.pptr)
plot(inf)
out_lier <- subset(inf$inf, abs(inf$inf$rstudent) >= 2 & abs(inf$inf$cook.d) >=.05)
out_lier

###ignore, not used as no influencers identified
reml.noInfluencer.pptr <- rma(
  yi = TraumaES_C_WP, 
  vi = VarTraES_C_WP, 
  data = dat5.pptr, 
  method = "REML",
	subset = c(-4))
summary(reml.noInfluencer.pptr)


# leave-one-out
#this is another way to look for mis-typed effects
# LOOK AT THESE ROWS AND MAKE SURE THE #'S IN THE FIRST COLUMN BARELY CHANGE
ronoutput <- leave1out(reml.pptr)
ronoutput

### step 4: publication bias
##############################################
# funnel plot
# WE NEED TO BE ABLE TO SAY THAT WE AT LEAST EXAMINED THESE
funnel(reml.pptr)

# Egger's regression test to see if funnel asymmetry is significant
# THE FINAL COLUMNS OF TABLES 
regtest(reml.pptr)

# trim and fill 
# THIS WILL GIVE YOU THE ADJ EST FOR TABLES 
# FOR ANY ROWS THAT SHOW SIGNIFICANT FUNNEL ASYMETRY
tf.tf <- trimfill(reml.pptr)
tf.tf
funnel(tf.tf)

### step 5: selection method analyses
##############################################
# first, getting rid of rows of missing data in a new dataframe
dat5.pptrNoMissing <- subset(dat5.pptr, is.na(TraumaES_C_WP) == FALSE)
dat5.pptrNoMissing


# second, making the correct vectors

t.stat <- (dat5.pptrNoMissing$TraumaES_C_WP*sqrt(
  dat5.pptrNoMissing$nEff_wpC-2))/2
t.stat

n.pre <- dat5.pptrNoMissing$nEff_wpC
n.post <- dat5.pptrNoMissing$nEff_wpC


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
# meta-analytic regression - moderators of within-person caregiver PTS effect
########################################################################



model <- rma(yi = TraumaES_C_WP, 
             vi = VarTraES_C_WP, 
             mods = ~ Control.Group,
             data = dat5.pptr, 
             method = "REML")
summary(model)

model <- rma(yi = TraumaES_C_WP, 
             vi = VarTraES_C_WP, 
             mods = ~ HoursTxMod,
             data = dat5.pptr, 
             method = "REML")
summary(model)

model <- rma(yi = TraumaES_C_WP, 
             vi = VarTraES_C_WP, 
             mods = ~ MonthsPPMod,
             data = dat5.pptr, 
             method = "REML")
summary(model)

model <- rma(yi = TraumaES_C_WP, 
             vi = VarTraES_C_WP, 
             mods = ~ percompM,
             data = dat5.pptr, 
             method = "REML")
summary(model)

model <- rma(yi = TraumaES_C_WP, 
             vi = VarTraES_C_WP, 
             mods = ~ FidelityMonitoring,
             data = dat5.pptr, 
             method = "REML")
summary(model)

