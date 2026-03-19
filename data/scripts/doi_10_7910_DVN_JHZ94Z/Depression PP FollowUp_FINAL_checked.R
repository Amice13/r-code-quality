#the following source code was modified from resources provided by Dr. Ron Rogge, see Macri & Rogge, 2024
#code below is for effects of attachment-based interventions
#on caregiver depression WITHIN PERSON aka pre-post
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
dat2 <- read.csv("C:\\Users\\hswer\\Documents\\Comps\\MetaMasterTable_CG_reduced.csv")
View(dat2)

# making subset data frames so we can examine effects using
# specific types of flexibility dimensions as mediators

dat2.ppdepf <- subset(dat2, DepFUWP == 1)
dat2.ppdepf


dat2 <- read.csv("C:\\Users\\hswer\\Documents\\Comps\\MetaMasterTable_CG_reduced.csv")
View(dat2)

# making subset data frames so we can examine effects using
# specific designs and outcomes
#caregiver depression within-person pre-post AT FOLLOW UP 

dat2.ppdepfo <- subset(dat2, DepFUWPo == 1)
dat2.ppdepfo


#####################################################################
#####################################################################
#####################################################################
#####################################################################
# depression within person at follow up 
#####################################################################
#####################################################################
#####################################################################
#####################################################################

##########################################################
##########################################################
##########################################################
# ppdepf
##########################################################
##########################################################
##########################################################

#########################################################
##########################################################


### step 1: calculate meta analytic estimate
##############################################
####within-person caregiver depression at follow up 

# random effects
reml.ppdepf <- rma(
  yi = DepES_FU_WP, 
  vi = VarD_FU_WP, 
  data = dat2.ppdepf, 
  method = "REML")

# THIS IS WHAT YOU WILL GET MOST OF THE #'S IN TABLES 
summary(reml.ppdepf)



# random effects
reml.ppdepo <- rma(
  yi = DepES_C_WP, 
  vi = VarDepES_C_WP, 
  data = dat21.ppdepo, 
  method = "REML")

# THIS IS WHAT YOU WILL GET MOST OF THE #'S IN TABLES 2 THRU 5 FROM
summary(reml.ppdepo)

### step 2: plot findings
##############################################
# simple forest plot
forest(reml.ppdepf,
       order="obs",
       showweights=FALSE)

# labeled forest plot
forest(reml.ppdepf,
       order="obs",
       header=TRUE,
       slab=paste(dat2.ppdepf$FirstAuthor, dat2.ppdepf$Year, sep="  "),
       ilab = cbind(dat2.ppdepf$nEff_wpFU),
       ilab.xpos = -3.8,
       mlab = "Random Effects Model Estimate",
       xlab = "Single Arm Completion Depression Effect",
       xlim = c(-5.7, 2.2),
       showweights=FALSE)
op <- par(cex=1, font=2)
text(cbind(-3.9), 30, cbind("N"), 
     pos = 4)
par(op)

#labeled forest plot with column for treatment names 
forest(reml.ppdepf,
       order="obs",
       header=TRUE,
       slab=paste(dat2.ppdepf$FirstAuthor, dat2.ppdepf$Year, sep="  "),
       ilab=cbind(dat2.ppdepf$nEff_wpFU, dat2.ppdepf$ABIAbbrev),
       ilab.xpos = c(-6.5, -5.5),
       mlab = "Random Effects Model Estimate",
       xlab = "Single Arm Follow-Up Caregiver Depression Effect",
       xlim = c(-10, 2.5),
       showweights=FALSE)
op <- par(cex = 1, font =2)
text(c(-6.5, -5.5), 8.6, c("n", "Treatment"), pos = 3)
par(op)

### step 3: outlier and influence diagnostics
##############################################
# get outlier/influence stats and plot
inf <- influence(reml.ppdepf)
plot(inf)
out_lier <- subset(inf$inf, abs(inf$inf$rstudent) >= 2 & abs(inf$inf$cook.d) >=.05)
out_lier

#run with study 3 removed
reml.noInfluencer.ppdepf <- rma(
  yi = DepES_FU_WP, 
  vi = VarD_FU_WP, 
  data = dat2.ppdepf, 
  method = "REML",
	subset = c(-3))
summary(reml.noInfluencer.ppdepf)


# leave-one-out
#this is another way to look for mis-typed effects
# LOOK AT THESE ROWS AND MAKE SURE THE #'S IN THE FIRST COLUMN BARELY CHANGE
ronoutput <- leave1out(reml.ppdepf)
ronoutput

### step 4: publication bias
##############################################
# funnel plot
# WE NEED TO BE ABLE TO SAY THAT WE AT LEAST EXAMINED THESE
funnel(reml.ppdepf)

# Egger's regression test to see if funnel asymmetry is significant
# THE FINAL COLUMNS OF TABLES
regtest(reml.ppdepf)



# trim and fill 
# THIS WILL GIVE YOU THE ADJ EST FOR TABLES 2 THRU 5
# FOR ANY ROWS THAT SHOW SIGNIFICANT FUNNEL ASYMETRY
tf.tf <- trimfill(reml.ppdepf)
tf.tf
funnel(tf.tf)




### step 5: selection method analyses
##############################################
# first, getting rid of rows of missing data in a new dataframe
####caregiver depression within person at follow up 
dat2.ppdepfNoMissing <- subset(dat2.ppdepf, is.na(DepES_FU_WP) == FALSE)
dat2.ppdepfNoMissing


# second, making the correct vectors

t.stat <- (dat2.ppdepfNoMissing$DepES_FU_WP*sqrt(
  dat2.ppdepfNoMissing$nEff_wpFU-2))/2
t.stat

n.pre <- dat2.ppdepfNoMissing$nEff_wpFU
n.post <- dat2.ppdepfNoMissing$nEff_wpFU


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


dat2.ppdepfoNoMissing <- subset(dat2.ppdepfo, is.na(DepES_FU_WP) == FALSE)
dat2.ppdepfoNoMissing


# second, making the correct vectors

t.stat <- (dat2.ppdepfoNoMissing$DepES_FU_WP*sqrt(
  dat2.ppdepfoNoMissing$nEff_wpFU-2))/2
t.stat

n.pre <- dat2.ppdepfoNoMissing$nEff_wpFU
n.post <- dat2.ppdepfoNoMissing$nEff_wpFU


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

#######################
#######################
#####################
#######################
######################
#######################
################ADJUSTED EFFECT REMOVE OUTLIERS
#########caregiver depression within-person pre-post FOLLOW UP ADJUSTED EFFECT


# pulling in the data 
dat21 <- read.csv("C:\\Users\\hswer\\Documents\\Comps\\MetaMasterTable_CG_reduced2.csv")
View(dat21)

# making subset data frames so we can examine effects using
# specific types of flexibility dimensions as mediators

dat21.ppdepo <- subset(dat21, DepWP == 1)
dat21.ppdepo

###adjusted effect

reml.ppdepfo <- rma(
  yi = DepES_FU_WP, 
  vi = VarD_FU_WP, 
  data = dat2.ppdepfo, 
  method = "REML")

# THIS IS WHAT YOU WILL GET MOST OF THE #'S IN TABLES 
summary(reml.ppdepfo)

###funnel plot caregiver depression within person at follow up outlier removed

funnel(reml.ppdepfo)

# Egger's regression test to see if funnel asymmetry is significant
# THE FINAL COLUMNS OF TABLES 
#outlier removed
regtest(reml.ppdepfo)

#trim and fill adjusted effect 
tf.tf <- trimfill(reml.ppdepfo)
tf.tf
funnel(tf.tf)


##selection method analyses for adjusted effect
# first, getting rid of rows of missing data in a new dataframe
dat21.ppdepoNoMissing <- subset(dat21.ppdepo, is.na(DepES_C_WP) == FALSE)
dat21.ppdepoNoMissing


# second, making the correct vectors

t.stat <- (dat21.ppdepoNoMissing$DepES_C_WP*sqrt(
  dat21.ppdepoNoMissing$nEff_wpC-2))/2
t.stat

n.pre <- dat21.ppdepoNoMissing$nEff_wpC
n.post <- dat21.ppdepoNoMissing$nEff_wpC


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






