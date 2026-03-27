#the following source code was modified from resources provided by Dr. Ron Rogge, see Macri & Rogge, 2024
#code below is for effects of attachment-based interventions
#on child externalizing relative to controls aka RCT
#at LATER FOLLOW UP 

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
dat3 <- read.csv("C:\\Users\\hswer\\Documents\\Comps\\MetaMasterTable_child_reduced.csv")
View(dat3)

# making subset data frames so we can examine effects using
# specific types of flexibility dimensions as mediators

dat3.rctextf <- subset(dat3, ExtRCTFU == 1)
dat3.rctextf



#####################################################################
#####################################################################
#####################################################################
#####################################################################
# externalizing RCT at FOLLOW UP 
#####################################################################
#####################################################################
#####################################################################
#####################################################################

##########################################################
##########################################################
##########################################################
# rctextf
##########################################################
##########################################################
##########################################################

##########################################################
# between PERSON (between group d post treatment)
##########################################################


### step 1: calculate meta analytic estimate
###child externalizing RCT at FOLLOW UP 
##############################################


# random effects
reml.rctextf <- rma(
  yi = ExtES_FU_RCT, 
  vi = VarEXT_FU_RCT, 
  data = dat3.rctextf, 
  method = "REML")

# THIS IS WHAT YOU WILL GET MOST OF THE #'S IN TABLES 2 THRU 5 FROM
summary(reml.rctextf)


### step 2: plot findings
##############################################
# simple forest plot
forest(reml.rctextf,
       order="obs",
       showweights=FALSE)

# labeled forest plot with treatment names
forest(reml.rctextf,
       order="obs",
       header=TRUE,
       slab=paste(dat3.rctextf$FirstAuthor, dat3.rctextf$Year, sep="  "),
       ilab=cbind(dat3.rctextf$nEff_rctFUc, dat3.rctextf$ABIabbrev),
       ilab.xpos = c(-3.5, -2),
       mlab = "Random Effects Model Estimate",
       xlab = "RCT Follow-Up Externalizing Effect",
       xlim = c(-6, 3.5),
       showweights=FALSE)
op <- par(cex = 1, font =2)
text(c(-3.5, -2), 7.75, c("n", "Treatment"), pos = 3)
par(op)

### step 3: outlier and influence diagnostics
##############################################
# get outlier/influence stats and plot
inf <- influence(reml.rctextf)
plot(inf)
out_lier <- subset(inf$inf, abs(inf$inf$rstudent) >= 2 & abs(inf$inf$cook.d) >=.05)
out_lier

#remove study 6
reml.noInfluencer.rctextf <- rma(
  yi = ExtES_FU_RCT, 
  vi = VarEXT_FU_RCT, 
  data = dat3.rctextf, 
  method = "REML",
	subset = c(-6))
summary(reml.noInfluencer.rctextf)


# leave-one-out
#this is another way to look for mis-typed effects
# LOOK AT THESE ROWS AND MAKE SURE THE #'S IN THE FIRST COLUMN BARELY CHANGE
ronoutput <- leave1out(reml.rctextf)
ronoutput

### step 4: publication bias
##############################################
# funnel plot
# WE NEED TO BE ABLE TO SAY THAT WE AT LEAST EXAMINED THESE
funnel(reml.rctextf)




# Egger's regression test to see if funnel asymmetry is significant
# THE FINAL COLUMNS OF TABLES 2 THRU 5
regtest(reml.rctextf)

# trim and fill 
# THIS WILL GIVE YOU THE ADJ EST FOR TABLES 2 THRU 5
# FOR ANY ROWS THAT SHOW SIGNIFICANT FUNNEL ASYMETRY
tf.tf <- trimfill(reml.rctextf)
tf.tf
funnel(tf.tf)

### step 5: selection method analyses
##############################################
# first, getting rid of rows of missing data in a new dataframe
dat3.rctextfNoMissing <- subset(dat3.rctextf, is.na(ExtES_FU_RCT) == FALSE)
dat3.rctextfNoMissing


# second, making the correct vectors

t.stat <- (dat3.rctextfNoMissing$ExtES_FU_RCT*sqrt(
  dat3.rctextfNoMissing$nEff_rctFUc-2))/2
t.stat

n.Tx <- dat3.rctextfNoMissing$nTx_FU
n.control <- dat3.rctextfNoMissing$ncntrl_FU

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

