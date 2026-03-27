#the following source code was modified from resources provided by Dr. Ron Rogge, see Macri & Rogge, 2024
#code below is for effects of attachment-based interventions
#on child total psychopathology WITHIN PERSON aka pre-post
#at LATER FOLLOW-UP ASSESSMENTS

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

# PULLING IN SELECTION METHOD FUNCTIONS ON Hannah'S COMPUTER 
source("C:\\Users\\hswer\\Documents\\Comps\\selection.meta.functions.R")


# pulling in the data 
dat10 <- read.csv("C:\\Users\\hswer\\Documents\\Comps\\MetaMasterTable_child_reduced.csv")
View(dat10)

# making subset data frames so we can examine effects using
# child total psychopathology within-person at follow up 

dat10.pptotf <- subset(dat10, FUTotWP == 1)
dat10.pptotf



#####################################################################
#####################################################################
#####################################################################
#####################################################################
# total problems within person at FOLLOW UP 
#####################################################################
#####################################################################
#####################################################################
#####################################################################

##########################################################
##########################################################
##########################################################
# pptotf
##########################################################
##########################################################
##########################################################

##########################################################
##########################################################


### step 1: calculate meta analytic estimate
###child tot psychopathology within-person at FOLLOW UP 
##############################################

# random effects
reml.pptotf <- rma(
  yi = TotES_FU_WP, 
  vi = VarTotES_FU_WP, 
  data = dat10.pptotf, 
  method = "REML")

# THIS IS WHAT YOU WILL GET MOST OF THE #'S IN TABLES 2 THRU 5 FROM
summary(reml.pptotf)


### step 2: plot findings
##############################################
# simple forest plot
forest(reml.pptotf,
       order="obs",
       showweights=FALSE)

# labeled forest plot
forest(reml.pptotf,
       order="obs",
       header=TRUE,
       slab=paste(dat10.pptotf$FirstAuthor, dat10.pptotf$Year, sep="  "),
       ilab=cbind(dat10.pptotf$nEff_wpFUc, dat10.pptotf$ABIabbrev),
       ilab.xpos = c(-6, -5),
       mlab = "Random Effects Model Estimate",
       xlab = "Single Arm Follow-Up Child Total Problems Effect",
       xlim = c(-9, 2),
       showweights=FALSE)
op <- par(cex = 1, font =2)
text(c(-6, -5), 5.75, c("n", "Treatment"), pos = 3)
par(op)



### step 3: outlier and influence diagnostics
##############################################
# get outlier/influence stats and plot
inf <- influence(reml.pptotf)
plot(inf)
out_lier <- subset(inf$inf, abs(inf$inf$rstudent) >= 2 & abs(inf$inf$cook.d) >=.05)
out_lier

##remove study 4

reml.noInfluencer.pptotf <- rma(
  yi = TotES_FU_WP, 
  vi = VarTotES_FU_WP, 
  data = dat10.pptotf, 
  method = "REML",
	subset = c(-4))
summary(reml.noInfluencer.pptotf)





##############################################
# funnel plot
# WE NEED TO BE ABLE TO SAY THAT WE AT LEAST EXAMINED THESE
funnel(reml.pptotf)

# Egger's regression test to see if funnel asymmetry is significant
# THE FINAL COLUMNS OF TABLES 
regtest(reml.pptotf)

# trim and fill 
# THIS WILL GIVE YOU THE ADJ EST FOR TABLES 
# FOR ANY ROWS THAT SHOW SIGNIFICANT FUNNEL ASYMETRY
tf.tf <- trimfill(reml.pptotf)
tf.tf
funnel(tf.tf)

### step 5: selection method analyses
##############################################
# first, getting rid of rows of missing data in a new dataframe
dat10.pptotfNoMissing <- subset(dat10.pptotf, is.na(TotES_FU_WP) == FALSE)
dat10.pptotfNoMissing


# second, making the correct vectors

t.stat <- (dat10.pptotfNoMissing$TotES_FU_WP*sqrt(
  dat10.pptotfNoMissing$nEff_wpFUc-2))/2
t.stat

n.pre <- dat10.pptotfNoMissing$nEff_wpFUc
n.post <- dat10.pptotfNoMissing$nEff_wpFUc


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


# leave-one-out
#this is another way to look for mis-typed effects
# LOOK AT THESE ROWS AND MAKE SURE THE #'S IN THE FIRST COLUMN BARELY CHANGE
ronoutput <- leave1out(reml.pptotf)
ronoutput
