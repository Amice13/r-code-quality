#the following source code was modified from resources provided by Dr. Ron Rogge, see Macri & Rogge, 2024
#code below is for effects of attachment-based interventions
#on child internalizing relative to controls aka RCT
#at LATER FOLLOW UP ASSESSMENT

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
# specific treatment designs and outcomes
#child internalizing RCT relative to control 
#at LATER FOLLOW-UP

dat8.rctintf <- subset(dat8, IntRCTFU == 1)
dat8.rctintf



#####################################################################
#####################################################################
#####################################################################
#####################################################################
# child internalizing RCT- LATER FOLLOW-UP 
#####################################################################
#####################################################################
#####################################################################
#####################################################################

##########################################################
##########################################################
##########################################################
# rctintf
##########################################################
##########################################################
##########################################################

##########################################################
# between PERSON (between group d post treatment)
##########################################################


### step 1: calculate meta analytic estimate
#child internalizing RCT FOLLOW UP 
##############################################


# random effects
reml.rctintf <- rma(
  yi = IntES_FU_RCT, 
  vi = VarInt_FU_RCT, 
  data = dat8.rctintf, 
  method = "REML")

# THIS IS WHAT YOU WILL GET MOST OF THE #'S IN TABLES 2 THRU 5 FROM
summary(reml.rctintf)


### step 2: plot findings
##############################################
# simple forest plot
forest(reml.rctintf,
       order="obs",
       showweights=FALSE)

# labeled forest plot
forest(reml.rctintf,
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

#add treatment name column 
forest(reml.rctintf,
       order="obs",
       header=TRUE,
       slab=paste(dat8.rctintf$FirstAuthor, dat8.rctintf$Year, sep="  "),
       ilab=cbind(dat8.rctintf$nEff_rctFUc, dat8.rctintf$ABIabbrev),
       ilab.xpos = c(-3.5, -2),
       mlab = "Random Effects Model Estimate",
       xlab = "RCT Follow-Up Internalizing Effect",
       xlim = c(-7, 4),
       showweights=FALSE)
op <- par(cex = 1, font =2)
text(c(-3.5, -2), 5.7, c("n", "Treatment"), pos = 3)
par(op)

### step 3: outlier and influence diagnostics
#rct child internalizing at later follow-up 
##############################################
# get outlier/influence stats and plot
inf <- influence(reml.rctintf)
plot(inf)
out_lier <- subset(inf$inf, abs(inf$inf$rstudent) >= 2 & abs(inf$inf$cook.d) >=.05)
out_lier

#not used because none identified
reml.noInfluencer.rctintf <- rma(
  yi = IntES_FU_RCT, 
  vi = VarInt_FU_RCT, 
  data = dat8.rctintf, 
  method = "REML",
	subset = c(-4))
summary(reml.noInfluencer.rctintf)


# leave-one-out
#this is another way to look for mis-typed effects
# LOOK AT THESE ROWS AND MAKE SURE THE #'S IN THE FIRST COLUMN BARELY CHANGE
ronoutput <- leave1out(reml.rctintf)
ronoutput

### step 4: publication bias
##############################################
# funnel plot
# WE NEED TO BE ABLE TO SAY THAT WE AT LEAST EXAMINED THESE
funnel(reml.rctintf)




# Egger's regression test to see if funnel asymmetry is significant
# THE FINAL COLUMNS OF TABLES 
regtest(reml.rctintf)




# trim and fill 
# THIS WILL GIVE YOU THE ADJ EST FOR TABLES
# FOR ANY ROWS THAT SHOW SIGNIFICANT FUNNEL ASYMETRY
tf.tf <- trimfill(reml.rctint)
tf.tf
funnel(tf.tf)

### step 5: selection method analyses
##############################################
# first, getting rid of rows of missing data in a new dataframe
dat8.rctintfNoMissing <- subset(dat8.rctintf, is.na(IntES_FU_RCT) == FALSE)
dat8.rctintfNoMissing


# second, making the correct vectors

t.stat <- (dat8.rctintfNoMissing$IntES_FU_RCT*sqrt(
  dat8.rctintfNoMissing$nEff_rctFUc-2))/2
t.stat

n.Tx <- dat8.rctintfNoMissing$nTx_FU
n.control <- dat8.rctintfNoMissing$nC_FU

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

