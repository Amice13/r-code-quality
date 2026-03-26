#the following source code was modified from resources provided by Dr. Ron Rogge, see Macri & Rogge, 2024
#code below is for effects of attachment-based interventions
#on caregiver depression relative to controls aka RCT
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
#caregiver depression RCT FOLLOW-UP
dat1 <- read.csv("C:\\Users\\hswer\\Documents\\Comps\\MetaMasterTable_CG_reduced.csv")
View(dat1)

# making subset data frames so we can examine effects using
#caregiver depression RCT at follow-up 

dat1.rctdepf <- subset(dat1, DepRCTFU == 1)
dat1.rctdepf

# pulling in the data 
dat1 <- read.csv("C:\\Users\\hswer\\Documents\\Comps\\MetaMasterTable_CG_reduced.csv")
View(dat1)




#####################################################################
#####################################################################
#####################################################################
#####################################################################
# depression RCT- FOLLOW-UP
#####################################################################
#####################################################################
#####################################################################
#####################################################################

##########################################################
##########################################################
##########################################################
# deprctf
##########################################################
##########################################################
##########################################################

##########################################################
# between PERSON (between group d post treatment)
##########################################################


### step 1: calculate meta analytic estimate
##############################################
####depression RCT effect at follow-up 

# random effects
reml.rctdepf <- rma(
  yi = DepES_FU_RCT, 
  vi = VarDES_FU_RCT, 
  data = dat1.rctdepf, 
  method = "REML")

# THIS IS WHAT YOU WILL GET MOST OF THE #'S IN TABLES 
summary(reml.rctdepf)



### step 2: plot findings
##############################################
# simple forest plot
forest(reml.rctdepf,
       order="obs",
       showweights=FALSE)

# labeled forest plot
forest(reml.rctdepf,
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

#labeled forest plot with treatment name column
forest(reml.rctdepf,
       order="obs",
       header=TRUE,
       slab=paste(dat1.rctdepf$FirstAuthor, dat1.rctdepf$Year, sep="  "),
       ilab=cbind(dat1.rctdepf$nEff_rctFU, dat1.rctdepf$ABIAbbrev),
       ilab.xpos = c(-6, -4.5),
       mlab = "Random Effects Model Estimate",
       xlab = "RCT Follow-Up Caregiver Depression Effect",
       xlim = c(-10, 4),
       showweights=FALSE)
op <- par(cex = 1, font =2)
text(c(-6, -4.5), 5.7, c("n", "Treatment"), pos = 3)
par(op)

### step 3: outlier and influence diagnostics
##############################################
# get outlier/influence stats and plot
inf <- influence(reml.rctdepf)
plot(inf)
out_lier <- subset(inf$inf, abs(inf$inf$rstudent) >= 2 & abs(inf$inf$cook.d) >=.05)
out_lier


#run with study 3 removed 
reml.noInfluencer.rctdepf <- rma(
  yi = DepES_FU_RCT, 
  vi = VarDES_FU_RCT, 
  data = dat1.rctdepf, 
  method = "REML",
	subset = c(-3))
summary(reml.noInfluencer.rctdepf)


# leave-one-out
#this is another way to look for mis-typed effects
# LOOK AT THESE ROWS AND MAKE SURE THE #'S IN THE FIRST COLUMN BARELY CHANGE
ronoutput <- leave1out(reml.rctdepf)
ronoutput

### step 4: publication bias
##############################################
# funnel plot
# WE NEED TO BE ABLE TO SAY THAT WE AT LEAST EXAMINED THESE
funnel(reml.rctdepf)




# Egger's regression test to see if funnel asymmetry is significant
# THE FINAL COLUMNS OF TABLES 
regtest(reml.rctdepf)

# trim and fill 
# THIS WILL GIVE YOU THE ADJ EST FOR TABLES 
# FOR ANY ROWS THAT SHOW SIGNIFICANT FUNNEL ASYMETRY
tf.tf <- trimfill(reml.rctdepf)
tf.tf
funnel(tf.tf)

### step 5: selection method analyses
##############################################
# first, getting rid of rows of missing data in a new dataframe
dat1.rctdepfNoMissing <- subset(dat1.rctdepf, is.na(DepES_FU_RCT) == FALSE)
dat1.rctdepfNoMissing


# second, making the correct vectors

t.stat <- (dat1.rctdepfNoMissing$DepES_FU_RCT*sqrt(
  dat1.rctdepfNoMissing$nEff_rctFU-2))/2
t.stat

n.Tx <- dat1.rctdepfNoMissing$nTx_rctFU
n.control <- dat1.rctdepfNoMissing$ncntrl_rctFU

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



# making subset data frames so we can examine effects using
# depression RCT follow-up with outliers removed 

dat1.rctdepfo <- subset(dat1, DepRCTFUa == 1)
dat1.rctdepfo

#depression RCT effect at follow-up with OUTLIERS REMOVED

reml.rctdepfo <- rma(
  yi = DepES_FU_RCT, 
  vi = VarDES_FU_RCT, 
  data = dat1.rctdepfo, 
  method = "REML")

# THIS IS WHAT YOU WILL GET MOST OF THE #'S IN TABLES 
summary(reml.rctdepfo)


#funnel plot for adjusted depression RCT follow-up effect
funnel(reml.rctdepfo)

#reg test for adjusted effect
regtest(reml.rctdepfo)

# trim and fill 
# THIS WILL GIVE YOU THE ADJ EST FOR TABLES
# FOR ANY ROWS THAT SHOW SIGNIFICANT FUNNEL ASYMETRY
#for ADJUSTED effect
tf.tf <- trimfill(reml.rctdepfo)
tf.tf
funnel(tf.tf)

### step 5: selection method analyses
##############################################
# first, getting rid of rows of missing data in a new dataframe
dat1.rctdepfoNoMissing <- subset(dat1.rctdepfo, is.na(DepES_FU_RCT) == FALSE)
dat1.rctdepfoNoMissing


# second, making the correct vectors

t.stat <- (dat1.rctdepfoNoMissing$DepES_FU_RCT*sqrt(
  dat1.rctdepfoNoMissing$nEff_rctFU-2))/2
t.stat

n.Tx <- dat1.rctdepfoNoMissing$nTx_rctFU
n.control <- dat1.rctdepfoNoMissing$ncntrl_rctFU

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





