#the following source code was modified from resources provided by Dr. Ron Rogge, see Macri & Rogge, 2024
#code below is for effects of attachment-based interventions
#on child externalizing WITHIN PERSON aka pre-post
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

# PULLING IN SELECTION METHOD FUNCTIONS 
source("C:\\Users\\hswer\\Documents\\Comps\\selection.meta.functions.R")


# pulling in the data 
#updated after new search alerts
dat61 <- read.csv("C:\\Users\\hswer\\Documents\\Comps\\MetaMasterTable_child_reduced_searchalertsadded.csv")
View(dat61)

# making subset data frames so we can examine effects using
# child externalizing within-person designs 

dat61.ppex <- subset(dat61, CExtWP == 1)
dat61.ppex



#####################################################################
#####################################################################
#####################################################################
#####################################################################
# child externalizing within person 
#####################################################################
#####################################################################
#####################################################################
#####################################################################

##########################################################
##########################################################
##########################################################
# ppex
##########################################################
##########################################################
##########################################################

##########################################################
##########################################################


### step 1: calculate meta analytic estimate
##############################################

# random effects
reml.ppex <- rma(
  yi = ExtES_C_WP, 
  vi = VarExtES_C_WP, 
  data = dat61.ppex, 
  method = "REML")

# THIS IS WHAT YOU WILL GET MOST OF THE #'S IN tables
summary(reml.ppex)


### step 2: plot findings
##############################################
# simple forest plot
forest(reml.ppex,
       order="obs",
       showweights=FALSE)

# labeled forest plot
forest(reml.ppex,
       order="obs",
       header=TRUE,
       slab=paste(dat61.ppex$FirstAuthor, dat61.ppex$Year, sep="  "),
       ilab = cbind(dat61.ppex$nEff_wpC),
       ilab.xpos = -3,
       mlab = "Random Effects Model Estimate",
       xlab = "Single Arm Completion Caregiver Anxiety Effect",
       xlim = c(-6, 2.2),
       showweights=FALSE)
op <- par(cex=1, font=2)
text(cbind(-3), 30, cbind("N"), 
     pos = 4)
par(op)

#forest plot with treatment name column
forest(reml.ppex,
       order="obs",
       header=TRUE,
       slab=paste(dat61.ppex$FirstAuthor, dat61.ppex$Year, sep="  "),
       ilab=cbind(dat61.ppex$nEff_wpC, dat61.ppex$ABIabbrev),
       ilab.xpos = c(-5, -3.8),
       mlab = "Random Effects Model Estimate",
       xlab = "Single Arm Child Externalizing Effect",
       xlim = c(-7, 2),
       showweights=FALSE)
op <- par(cex = 1, font =2)
text(c(-5, -3.8), 12.5, c("n", "Treatment"), pos = 3)
par(op)

### step 3: outlier and influence diagnostics
##############################################
# get outlier/influence stats and plot
inf <- influence(reml.ppex)
plot(inf)
out_lier <- subset(inf$inf, abs(inf$inf$rstudent) >= 2 & abs(inf$inf$cook.d) >=.05)
out_lier

reml.noInfluencer.ppex <- rma(
  yi = ExtES_C_WP, 
  vi = VarExtES_C_WP, 
  data = dat61.ppex, 
  method = "REML",
	subset = c(-9))
summary(reml.noInfluencer.ppex)


### step 4: publication bias -unadjusted effect
##############################################
# funnel plot
# WE NEED TO BE ABLE TO SAY THAT WE AT LEAST EXAMINED THESE
funnel(reml.ppex)

# Egger's regression test to see if funnel asymmetry is significant
# THE FINAL COLUMNS OF TABLES 2 THRU 5
regtest(reml.ppex)

# trim and fill 
# THIS WILL GIVE YOU THE ADJ EST 
# FOR ANY ROWS THAT SHOW SIGNIFICANT FUNNEL ASYMETRY
tf.tf <- trimfill(reml.ppex)
tf.tf
funnel(tf.tf)

### step 5: selection method analyses
##############################################
# first, getting rid of rows of missing data in a new dataframe
dat61.ppexNoMissing <- subset(dat61.ppex, is.na(ExtES_C_WP) == FALSE)
dat61.ppexNoMissing


# second, making the correct vectors

t.stat <- (dat61.ppexNoMissing$ExtES_C_WP*sqrt(
  dat61.ppexNoMissing$nEff_wpC-2))/2
t.stat

n.pre <- dat61.ppexNoMissing$nEff_wpCc
n.post <- dat61.ppexNoMissing$nEff_wpCc


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



# pulling in the data adjusted meta-analytic effect- NO SPRANG 
dat612 <- read.csv("C:\\Users\\hswer\\Documents\\Comps\\MetaMasterTable_child_reduced_searchalertsadded_ceo.csv")
View(dat612)

# making subset data frames so we can examine effects 
# without goodman 2013

dat612.ppexo <- subset(dat612, CExtWP == 1)
dat612.ppexo


# random effects
reml.ppexo <- rma(
  yi = ExtES_C_WP, 
  vi = VarExtES_C_WP, 
  data = dat612.ppexo, 
  method = "REML")

# THIS IS WHAT YOU WILL GET MOST OF THE #'S IN tables
summary(reml.ppexo)


##############################################
# funnel plot - adjusted effect
# WE NEED TO BE ABLE TO SAY THAT WE AT LEAST EXAMINED THESE
funnel(reml.ppexo)

# Egger's regression test to see if funnel asymmetry is significant
# THE FINAL COLUMNS OF TABLES 2 THRU 5
regtest(reml.ppexo)

# trim and fill 
# THIS WILL GIVE YOU THE ADJ EST FOR TABLES 2 THRU 5
# FOR ANY ROWS THAT SHOW SIGNIFICANT FUNNEL ASYMETRY
tf.tf <- trimfill(reml.ppexo)
tf.tf
funnel(tf.tf)

### step 5: selection method analyses - adjusted effect
##############################################
# first, getting rid of rows of missing data in a new dataframe
dat612.ppexoNoMissing <- subset(dat612.ppexo, is.na(ExtES_C_WP) == FALSE)
dat612.ppexoNoMissing


# second, making the correct vectors

t.stat <- (dat612.ppexoNoMissing$ExtES_C_WP*sqrt(
  dat612.ppexoNoMissing$nEff_wpCc-2))/2
t.stat

n.pre <- dat612.ppexoNoMissing$nEff_wpCc
n.post <- dat612.ppexoNoMissing$nEff_wpCc


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
# meta-analytic regression
########################################################################




model <- rma(yi = ExtES_C_WP, 
             vi = VarExtES_C_WP, 
             mods = ~ ControlGroup,
             data = dat612.ppexo, 
             method = "REML")
summary(model)


model <- rma(yi = ExtES_C_WP, 
             vi = VarExtES_C_WP, 
             mods = ~ HoursTx,
             data = dat612.ppexo, 
             method = "REML")
summary(model)

model <- rma(yi = ExtES_C_WP, 
             vi = VarExtES_C_WP, 
             mods = ~ Months2Completion_WPc,
             data = dat612.ppexo, 
             method = "REML")
summary(model)

model <- rma(yi = ExtES_C_WP, 
             vi = ExtES_C_WP, 
             mods = ~ completers,
             data = dat612.ppexo, 
             method = "REML")
summary(model)

model <- rma(yi = ExtES_C_WP, 
             vi = VarExtES_C_WP, 
             mods = ~ FidelityMonitoring,
             data = dat612.ppexo, 
             method = "REML")
summary(model)





