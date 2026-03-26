#the following source code was modified from resources provided by Dr. Ron Rogge, see Macri & Rogge, 2024
#code below is for effects of attachment-based interventions
#on child total problems relative to controls aka RCT
#at LATER FOLLOW-UP ASSESSMENTS

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
dat81 <- read.csv("C:\\Users\\hswer\\Documents\\Comps\\MetaMasterTable_child_reduced.csv")
View(dat81)

# making subset data frames so we can examine effects using
# child total problems relative to control at follow up 

dat81.rcttotf <- subset(dat81, TotRCTFU == 1)
dat81.rcttotf



#####################################################################
#####################################################################
#####################################################################
#####################################################################
# child total problems relative to controls at follow up 
#####################################################################
#####################################################################
#####################################################################
#####################################################################

##########################################################
##########################################################
##########################################################
# rcttotf
##########################################################
##########################################################
##########################################################

##########################################################
# between PERSON (between group d post treatment)
##########################################################


### step 1: calculate meta analytic estimate
###child total problems RCT at follow up 
##############################################


# random effects
reml.rcttotf <- rma(
  yi = Tot_ES_FU_RCT, 
  vi = VarTot_FU_RCT, 
  data = dat81.rcttotf, 
  method = "REML")

# THIS IS WHAT YOU WILL GET MOST OF THE #'S IN TABLES 2 THRU 5 FROM
summary(reml.rcttotf)


### step 2: plot findings
##############################################
# simple forest plot
forest(reml.rcttotf,
       order="obs",
       showweights=FALSE)

# labeled forest plot
forest(reml.rcttotf,
       order="obs",
       header=TRUE,
       slab=paste(dat81.rcttotf$FirstAuthor, dat81.rcttotf$Year, sep="  "),
       ilab = cbind(dat81.rcttotf$nEff_rctFUc),
       ilab.xpos = -2.2,
       mlab = "Random Effects Model Estimate",
       xlab = "RCT Completion Total Problems Effect",
       xlim = c(-4.2, 1.5),
       showweights=FALSE)
op <- par(cex=1, font=2)
text(c(-2.3), 38, c("N"), 
     pos = -2.2)
par(op)

##labeled forest, add treatment name column 

forest(reml.rcttotf,
       order="obs",
       header=TRUE,
       slab=paste(dat81.rcttotf$FirstAuthor, dat81.rcttotf$Year, sep="  "),
       ilab=cbind(dat81.rcttotf$nEff_rctFUc, dat81.rcttotf$ABIabbrev),
       ilab.xpos = c(-6, -4.5),
       mlab = "Random Effects Model Estimate",
       xlab = "RCT Follow-Up Total Problems Effect",
       xlim = c(-10, 4),
       showweights=FALSE)
op <- par(cex = 1, font =2)
text(c(-6, -4.5), 10.5, c("n", "Treatment"), pos = 3)
par(op)

### step 3: outlier and influence diagnostics
##############################################
# get outlier/influence stats and plot
inf <- influence(reml.rcttotf)
plot(inf)
out_lier <- subset(inf$inf, abs(inf$inf$rstudent) >= 2 & abs(inf$inf$cook.d) >=.05)
out_lier


###NOT RUN OR NEEDED IN THIS ANALYSIS
reml.noInfluencer.rcttotf <- rma(
  yi = Tot_ES_FU_RCT, 
  vi = VarTot_FU_RCT, 
  data = dat81.rcttotf, 
  method = "REML",
	subset = c(-3))
summary(reml.noInfluencer.rcttotf)


# leave-one-out
#this is another way to look for mis-typed effects
# LOOK AT THESE ROWS AND MAKE SURE THE #'S IN THE FIRST COLUMN BARELY CHANGE
ronoutput <- leave1out(reml.rcttotf)
ronoutput

### step 4: publication bias
##############################################
# funnel plot
# WE NEED TO BE ABLE TO SAY THAT WE AT LEAST EXAMINED THESE
funnel(reml.rcttotf)




# Egger's regression test to see if funnel asymmetry is significant
# THE FINAL COLUMNS OF TABLES
regtest(reml.rcttotf)




# trim and fill 
# THIS WILL GIVE YOU THE ADJ EST FOR TABLES 2 THRU 5
# FOR ANY ROWS THAT SHOW SIGNIFICANT FUNNEL ASYMETRY
tf.tf <- trimfill(reml.rcttotf)
tf.tf
funnel(tf.tf)

### step 5: selection method analyses
##############################################
# first, getting rid of rows of missing data in a new dataframe
dat81.rcttotfNoMissing <- subset(dat81.rcttotf, is.na(Tot_ES_FU_RCT) == FALSE)
dat81.rcttotfNoMissing


# second, making the correct vectors

t.stat <- (dat81.rcttotfNoMissing$Tot_ES_FU_RCT*sqrt(
  dat81.rcttotfNoMissing$nEff_rctFUc-2))/2
t.stat

n.Tx <- dat81.rcttotfNoMissing$nTX_FU
n.control <- dat81.rcttotfNoMissing$nC_FU

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

