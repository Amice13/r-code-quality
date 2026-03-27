#the following source code was modified from resources provided by Dr. Ron Rogge, see Macri & Rogge, 2024
#code below is for effects of attachment-based interventions
#on WITHIN-PERSON child total problems aka pre-post designs
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
dat10 <- read.csv("C:\\Users\\hswer\\Documents\\Comps\\MetaMasterTable_child_reduced_searchalertsadded.csv")
View(dat10)

# making subset data frames so we can examine effects using
# specific designs and outcomes
#child total problems within-person, aka pre-post
#at post-treatment 

dat10.pptot <- subset(dat10, CTotWP == 1)
dat10.pptot



#####################################################################
#####################################################################
#####################################################################
#####################################################################
# child total problems within-person immediately post-treatment
#####################################################################
#####################################################################
#####################################################################
#####################################################################

##########################################################
##########################################################
##########################################################
# pptot
##########################################################
##########################################################
##########################################################

##########################################################
##########################################################


### step 1: calculate meta analytic estimate
#####child total problems pre-post within-person
##############################################

# random effects
reml.pptot <- rma(
  yi = TotES_C_WP, 
  vi = VarTotES_C_WP, 
  data = dat10.pptot, 
  method = "REML")

# THIS IS WHAT YOU WILL GET MOST OF THE #'S IN TABLES 
summary(reml.pptot)


### step 2: plot findings
##############################################
# simple forest plot
forest(reml.pptot,
       order="obs",
       showweights=FALSE)

# labeled forest plot with treatment name column
forest(reml.pptot,
       order="obs",
       header=TRUE,
       slab=paste(dat10.pptot$FirstAuthor, dat10.pptot$Year, sep="  "),
       ilab=cbind(dat10.pptot$nEff_wpC, dat10.pptot$ABIabbrev),
       ilab.xpos = c(-5, -3.8),
       mlab = "Random Effects Model Estimate",
       xlab = "Single Arm Child Total Problems Effect",
       xlim = c(-7, 2),
       showweights=FALSE)
op <- par(cex = 1, font =2)
text(c(-5, -3.8), 12.5, c("n", "Treatment"), pos = 3)
par(op)



### step 3: outlier and influence diagnostics
#child total problems pre post 
##############################################
# get outlier/influence stats and plot
inf <- influence(reml.pptot)
plot(inf)
out_lier <- subset(inf$inf, abs(inf$inf$rstudent) >= 2 & abs(inf$inf$cook.d) >=.05)
out_lier

#rerun with study 5 excluded 

reml.noInfluencer.pptot <- rma(
  yi = TotES_C_WP, 
  vi = VarTotES_C_WP, 
  data = dat10.pptot, 
  method = "REML",
	subset = c(-5, -7, -10))
summary(reml.noInfluencer.pptot)





##############################################
# funnel plot
# WE NEED TO BE ABLE TO SAY THAT WE AT LEAST EXAMINED THESE
funnel(reml.pptot)

# Egger's regression test to see if funnel asymmetry is significant
# THE FINAL COLUMNS 
regtest(reml.pptot)

# trim and fill 
# THIS WILL GIVE YOU THE ADJ EST FOR TABLES 
# FOR ANY ROWS THAT SHOW SIGNIFICANT FUNNEL ASYMETRY
tf.tf <- trimfill(reml.pptot)
tf.tf
funnel(tf.tf)

### step 5: selection method analyses
##############################################
# first, getting rid of rows of missing data in a new dataframe
dat10.pptotNoMissing <- subset(dat10.pptot, is.na(TotES_C_WP) == FALSE)
dat10.pptotNoMissing


# second, making the correct vectors

t.stat <- (dat10.pptotNoMissing$TotES_C_WP*sqrt(
  dat10.pptotNoMissing$nEff_wpCc-2))/2
t.stat

n.pre <- dat10.pptotNoMissing$nEff_wpCc
n.post <- dat10.pptotNoMissing$nEff_wpCc


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
ronoutput <- leave1out(reml.pptot)
ronoutput


########################################################################
# meta-analytic regression - moderators of child total problems within-person effect
########################################################################

model <- rma(yi = TotES_C_WP, 
             vi = VarTotES_C_WP, 
             mods = ~ ControlGroup,
             data = dat10.pptot, 
             method = "REML")
summary(model)

model <- rma(yi = TotES_C_WP, 
             vi = VarTotES_C_WP, 
             mods = ~ HoursTx,
             data = dat10.pptot, 
             method = "REML")
summary(model)

model <- rma(yi = TotES_C_WP, 
             vi = VarTotES_C_WP, 
             mods = ~ Months2Completion_WPc,
             data = dat10.pptot, 
             method = "REML")
summary(model)

model <- rma(yi = TotES_C_WP, 
             vi = VarTotES_C_WP, 
             mods = ~ completers,
             data = dat10.pptot, 
             method = "REML")
summary(model)

model <- rma(yi = TotES_C_WP, 
             vi = VarTotES_C_WP, 
             mods = ~ FidelityMonitoring,
             data = dat10.pptot, 
             method = "REML")
summary(model)



