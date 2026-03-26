#the following source code was modified from resources provided by Dr. Ron Rogge, see Macri & Rogge, 2024
#code below is for effects of attachment-based interventions
#on WITHIN-PERSON child internalizing aka pre-post designs
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
dat62 <- read.csv("C:\\Users\\hswer\\Documents\\Comps\\MetaMasterTable_child_reduced.csv")
View(dat62)

# making subset data frames so we can examine effects using
# specific types of flexibility dimensions as mediators

dat62.ppint <- subset(dat62, CIntWP == 1)
dat62ppint



#####################################################################
#####################################################################
#####################################################################
#####################################################################
# child internalizing within-person- posttreatment
#####################################################################
#####################################################################
#####################################################################
#####################################################################

##########################################################
##########################################################
##########################################################
# ppint
##########################################################
##########################################################
##########################################################

##########################################################
##########################################################


### step 1: calculate meta analytic estimate
#within-person child internalizing post-treatment
#pre-post designs 
##############################################

# random effects
reml.ppint <- rma(
  yi = IntES_C_WP, 
  vi = VarIntES_C_WP, 
  data = dat62.ppint, 
  method = "REML")

# THIS IS WHAT YOU WILL GET MOST OF THE #'S IN TABLES 
summary(reml.ppint)


### step 2: plot findings
##############################################
# simple forest plot
forest(reml.ppint,
       order="obs",
       showweights=FALSE)

# labeled forest plot
forest(reml.ppint,
       order="obs",
       header=TRUE,
       slab=paste(dat62.ppint$FirstAuthor, dat62.ppint$Year, sep="  "),
       ilab = cbind(dat62.ppint$nEff_wpCc),
       ilab.xpos = -3.2,
       mlab = "Random Effects Model Estimate",
       xlab = "Single Arm Completion Caregiver Anxiety Effect",
       xlim = c(-6, 2.2),
       showweights=FALSE)
op <- par(cex=1, font=2)
text(cbind(-3), 30, cbind("N"), 
     pos = 3.5)
par(op)

#forest plot with treatment name column 

forest(reml.ppint,
       order="obs",
       header=TRUE,
       slab=paste(dat62.ppint$FirstAuthor, dat62.ppint$Year, sep="  "),
       ilab=cbind(dat62.ppint$nEff_wpC, dat62.ppint$ABIabbrev),
       ilab.xpos = c(-6, -4.5),
       mlab = "Random Effects Model Estimate",
       xlab = "Single Arm Child Internalizing Effect",
       xlim = c(-10, 3),
       showweights=FALSE)
op <- par(cex = 1, font =2)
text(c(-6, -4.5), 7.5, c("n", "Treatment"), pos = 3)
par(op)

### step 3: outlier and influence diagnostics
##############################################
# get outlier/influence stats and plot
inf <- influence(reml.ppint)
plot(inf)
out_lier <- subset(inf$inf, abs(inf$inf$rstudent) >= 2 & abs(inf$inf$cook.d) >=.05)
out_lier

#remove study 6
reml.noInfluencer.ppint <- rma(
  yi = IntES_C_WP, 
  vi = VarIntES_C_WP, 
  data = dat62.ppint, 
  method = "REML",
	subset = c(-6))
summary(reml.noInfluencer.ppint)


# leave-one-out
#this is another way to look for mis-typed effects
# LOOK AT THESE ROWS AND MAKE SURE THE #'S IN THE FIRST COLUMN BARELY CHANGE
ronoutput <- leave1out(reml.ppint)
ronoutput

### step 4: publication bias
##############################################
# funnel plot
# WE NEED TO BE ABLE TO SAY THAT WE AT LEAST EXAMINED THESE
funnel(reml.ppint)

# Egger's regression test to see if funnel asymmetry is significant
# THE FINAL COLUMNS OF TABLES 
regtest(reml.ppint)

# trim and fill 
# THIS WILL GIVE YOU THE ADJ EST FOR TABLES
# FOR ANY ROWS THAT SHOW SIGNIFICANT FUNNEL ASYMETRY
tf.tf <- trimfill(reml.ppint)
tf.tf
funnel(tf.tf)

### step 5: selection method analyses
##############################################
# first, getting rid of rows of missing data in a new dataframe
dat62.ppintNoMissing <- subset(dat62.ppint, is.na(IntES_C_WP) == FALSE)
dat62.ppintNoMissing


# second, making the correct vectors

t.stat <- (dat62.ppintNoMissing$IntES_C_WP*sqrt(
  dat62.ppintNoMissing$nEff_wpCc-2))/2
t.stat

n.pre <- dat62.ppintNoMissing$nEff_wpCc
n.post <- dat62.ppintNoMissing$nEff_wpCc


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


###################rerun#############
##################################
##################################
################rerun outlier removed############


# pulling in the data adjusted meta-analytic effect- no study 6
dat612 <- read.csv("C:\\Users\\hswer\\Documents\\Comps\\MetaMasterTable_child_reduced1.csv")
View(dat612)

# making subset data frames so we can examine effects 
# without goodman 2013

dat612.ppinto <- subset(dat612, CIntWP == 1)
dat612.ppinto


# random effects
reml.ppinto <- rma(
  yi = IntES_C_WP, 
  vi = VarIntES_C_WP, 
  data = dat612.ppinto, 
  method = "REML")

# THIS IS WHAT YOU WILL GET MOST OF THE #'S IN TABLES 2 THRU 5 FROM
summary(reml.ppinto)


##############################################
# funnel plot
# WE NEED TO BE ABLE TO SAY THAT WE AT LEAST EXAMINED THESE
funnel(reml.ppinto)

# Egger's regression test to see if funnel asymmetry is significant
# THE FINAL COLUMNS OF TABLES 2 THRU 5
regtest(reml.ppinto)

# trim and fill 
# THIS WILL GIVE YOU THE ADJ EST FOR TABLES 2 THRU 5
# FOR ANY ROWS THAT SHOW SIGNIFICANT FUNNEL ASYMETRY
tf.tf <- trimfill(reml.ppinto)
tf.tf
funnel(tf.tf)

### step 5: selection method analyses
##############################################
# first, getting rid of rows of missing data in a new dataframe
dat612.ppintoNoMissing <- subset(dat612.ppinto, is.na(IntES_C_WP) == FALSE)
dat612.ppintoNoMissing


# second, making the correct vectors

t.stat <- (dat612.ppintoNoMissing$IntES_C_WP*sqrt(
  dat612.ppintoNoMissing$nEff_wpCc-2))/2
t.stat

n.pre <- dat612.ppintoNoMissing$nEff_wpCc
n.post <- dat612.ppintoNoMissing$nEff_wpCc


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
# meta-analytic regression
########################################################################

model <- rma(yi = IntES_C_WP, 
             vi = VarIntES_C_WP, 
             mods = ~ CntrlType,
             data = dat62.ppint, 
             method = "REML")
summary(model)

model <- rma(yi = IntES_C_WP, 
             vi = VarIntES_C_WP, 
             mods = ~ HoursTx,
             data = dat62.ppint, 
             method = "REML")
summary(model)

model <- rma(yi = IntES_C_WP, 
             vi = VarIntES_C_WP, 
             mods = ~ Months2Completion_RCTc,
             data = dat62.ppint, 
             method = "REML")
summary(model)

model <- rma(yi = IntES_C_WP, 
             vi = IntES_C_WP, 
             mods = ~ completers,
             data = dat62.ppint, 
             method = "REML")
summary(model)

model <- rma(yi = IntES_C_WP, 
             vi = VarIntES_C_WP, 
             mods = ~ FidelityMonitoring,
             data = dat62.ppint, 
             method = "REML")
summary(model)


model <- rma(yi = ExtES_C_WP, 
             vi = VarExtES_C_WP, 
             mods = ~ Control.Group,
             data = dat61.ppex, 
             method = "REML")
summary(model)





