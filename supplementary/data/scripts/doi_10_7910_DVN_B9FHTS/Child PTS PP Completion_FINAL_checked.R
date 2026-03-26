#the following source code was modified from resources provided by Dr. Ron Rogge, see Macri & Rogge, 2024
#code below is for effects of attachment-based interventions
#on WITHIN-PERSON child posstraumatic stress symptoms aka pre-post designs
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
dat99 <- read.csv("C:\\Users\\hswer\\Documents\\Comps\\MetaMasterTable_child_reduced_searchalertsadded.csv")
View(dat99)

# making subset data frames so we can examine effects using
# specific designs and outcomes
#child PTS within-person, aka pre-post
#at post-treatment 

dat99.pptr <- subset(dat99, CTraumaWP == 1)
dat99.pptr



#####################################################################
#####################################################################
#####################################################################
#####################################################################
# child posstraumatic stress within-person immediately post-treatment
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
#####child PTS pre-post within-person
##############################################

# random effects
reml.pptr <- rma(
  yi = CTraumaES_C_WP, 
  vi = VarTraES_C_WP, 
  data = dat99.pptr, 
  method = "REML")

# THIS IS WHAT YOU WILL GET MOST OF THE #'S IN TABLES 
summary(reml.pptr)


### step 2: plot findings
##############################################
# simple forest plot
forest(reml.pptr,
       order="obs",
       showweights=FALSE)

# labeled forest plot with treatment name column
forest(reml.pptr,
       order="obs",
       header=TRUE,
       slab=paste(dat99.pptr$FirstAuthor, dat99.pptr$Year, sep="  "),
       ilab=cbind(dat99.pptr$nEff_wpC, dat99.pptr$ABIabbrev),
       ilab.xpos = c(-5, -3.8),
       mlab = "Random Effects Model Estimate",
       xlab = "Single Arm Child Total Problems Effect",
       xlim = c(-7, 2),
       showweights=FALSE)
op <- par(cex = 1, font =2)
text(c(-5, -3.8), 5.75, c("n", "Treatment"), pos = 3)
par(op)



### step 3: outlier and influence diagnostics
#child PTS pre post 
##############################################
# get outlier/influence stats and plot
inf <- influence(reml.pptr)
plot(inf)
out_lier <- subset(inf$inf, abs(inf$inf$rstudent) >= 2 & abs(inf$inf$cook.d) >=.05)
out_lier

#rerun with study 3 (Lieberman) excluded 

reml.noInfluencer.pptr <- rma(
  yi = CTraumaES_C_WP, 
  vi = VarTraES_C_WP, 
  data = dat99.pptr, 
  method = "REML",
	subset = c(-3))
summary(reml.noInfluencer.pptr)





##############################################
# funnel plot
# WE NEED TO BE ABLE TO SAY THAT WE AT LEAST EXAMINED THESE
funnel(reml.pptr)

# Egger's regression test to see if funnel asymmetry is significant
# THE FINAL COLUMNS 
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
dat99.pptrNoMissing <- subset(dat99.pptr, is.na(CTraumaES_C_WP) == FALSE)
dat99.pptrNoMissing


# second, making the correct vectors

t.stat <- (dat99.pptrNoMissing$CTraumaES_C_WP*sqrt(
  dat99.pptrNoMissing$nEff_wpCc-2))/2
t.stat

n.pre <- dat99.pptrNoMissing$nEff_wpCc
n.post <- dat99.pptrNoMissing$nEff_wpCc


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
ronoutput <- leave1out(reml.pptr)
ronoutput


########################################################################
# meta-analytic regression - moderators of child PTS within-person effect
########################################################################

model <- rma(yi = CTraumaES_C_WP, 
             vi = VarTraES_C_WP, 
             mods = ~ ControlGroup,
             data = dat99.pptr, 
             method = "REML")
summary(model)

model <- rma(yi = CTraumaES_C_WP, 
             vi = VarTraES_C_WP, 
             mods = ~ HoursTx,
             data = dat99.pptr, 
             method = "REML")
summary(model)

model <- rma(yi = CTraumaES_C_WP, 
             vi = VarTraES_C_WP, 
             mods = ~ Months2Completion_WPc,
             data = dat99.pptr, 
             method = "REML")
summary(model)

model <- rma(yi = CTraumaES_C_WP, 
             vi = VarTraES_C_WP, 
             mods = ~ completers,
             data = dat99.pptr, 
             method = "REML")
summary(model)

model <- rma(yi = CTraumaES_C_WP, 
             vi = VarTraES_C_WP, 
             mods = ~ FidelityMonitoring,
             data = dat99.pptr, 
             method = "REML")
summary(model)



