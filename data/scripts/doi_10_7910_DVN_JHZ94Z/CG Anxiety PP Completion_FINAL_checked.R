#the following source code was modified from resources provided by Dr. Ron Rogge, see Macri & Rogge, 2024
#code below is for effects of attachment-based interventions
#on WITHIN-PERSON caregiver anxiety aka pre-post designs
#at immediate post-treatment assessment

#importing and loading packages
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
dat6 <- read.csv("C:\\Users\\hswer\\Documents\\Comps\\MetaMasterTable_CG_reduced.csv")
View(dat6)

# making subset data frames so we can examine effects using
# specific study designs and outcomes
#within-person caregiver anxiety outcomes pre-post designs 

dat6.ppax <- subset(dat6, AnxietyWP == 1)
dat6.ppax



#####################################################################
#####################################################################
#####################################################################
#####################################################################
# caregiver anxiety prepost- all 
#####################################################################
#####################################################################
#####################################################################
#####################################################################

##########################################################
##########################################################
##########################################################
# axpp
##########################################################
##########################################################
##########################################################

##########################################################
##########################################################


### step 1: calculate meta analytic estimate
#caregiver anxiety pre-post WP 
##############################################

# random effects
reml.ppax <- rma(
  yi = AnxES_C_WP, 
  vi = VarAnxES_C_WP, 
  data = dat6.ppax, 
  method = "REML")

# THIS IS WHAT YOU WILL GET MOST OF THE #'S IN TABLES 
summary(reml.ppax)


### step 2: plot findings
##############################################
# simple forest plot
forest(reml.ppax,
       order="obs",
       showweights=FALSE)

# labeled forest plot
forest(reml.ppax,
       order="obs",
       header=TRUE,
       slab=paste(dat6.ppax$FirstAuthor, dat6.ppax$Year, sep="  "),
       ilab = cbind(dat6.ppax$nEff_wpC),
       ilab.xpos = -4.8,
       mlab = "Random Effects Model Estimate",
       xlab = "Single Arm Completion Caregiver Anxiety Effect",
       xlim = c(-7, 2.2),
       showweights=FALSE)
op <- par(cex=1, font=2)
text(cbind(-3.9), 30, cbind("N"), 
     pos = 4)
par(op)

#forest plot with labeled column for treatment name 
forest(reml.ppax,
       order="obs",
       header=TRUE,
       slab=paste(dat6.ppax$FirstAuthor, dat6.ppax$Year, sep="  "),
       ilab=cbind(dat6.ppax$nEff_wpC, dat6.ppax$ABIAbbrev),
       ilab.xpos = c(-6.25, -5),
       mlab = "Random Effects Model Estimate",
       xlab = "Single Arm Caregiver Anxiety Effect",
       xlim = c(-10, 2.5),
       showweights=FALSE)
op <- par(cex = 1, font =2)
text(c(-6.25, -5), 7.6, c("n", "Treatment"), pos = 3)
par(op)

### step 3: outlier and influence diagnostics
##############################################
# get outlier/influence stats and plot
inf <- influence(reml.ppax)
plot(inf)
out_lier <- subset(inf$inf, abs(inf$inf$rstudent) >= 2 & abs(inf$inf$cook.d) >=.05)
out_lier

reml.noInfluencer.ppax <- rma(
  yi = AnxES_C_WP, 
  vi = VarAnxES_C_WP, 
  data = dat6.ppax, 
  method = "REML",
	subset = c(-1))
summary(reml.noInfluencer.ppax)

# leave-one-out
#this is another way to look for mis-typed effects
#goodman 2013 removed 
# LOOK AT THESE ROWS AND MAKE SURE THE #'S IN THE FIRST COLUMN BARELY CHANGE
ronoutput <- leave1out(reml.ppax)
ronoutput

### step 4: publication bias
##############################################
# funnel plot
# WE NEED TO BE ABLE TO SAY THAT WE AT LEAST EXAMINED THESE
funnel(reml.ppax)

# Egger's regression test to see if funnel asymmetry is significant
# THE FINAL COLUMNS OF TABLES 2 THRU 5
regtest(reml.ppax)

# trim and fill 
# THIS WILL GIVE YOU THE ADJ EST FOR TABLES 2 THRU 5
# FOR ANY ROWS THAT SHOW SIGNIFICANT FUNNEL ASYMETRY
tf.tf <- trimfill(reml.ppax)
tf.tf
funnel(tf.tf)

### step 5: selection method analyses
##############################################
# first, getting rid of rows of missing data in a new dataframe
dat6.ppaxNoMissing <- subset(dat6.ppax, is.na(AnxES_C_WP) == FALSE)
dat6.ppaxNoMissing


# second, making the correct vectors

t.stat <- (dat6.ppaxNoMissing$AnxES_C_WP*sqrt(
  dat6.ppaxNoMissing$nEff_wpC-2))/2
t.stat

n.pre <- dat6.ppaxNoMissing$nEff_wpC
n.post <- dat6.ppaxNoMissing$nEff_wpC


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



# pulling in the data adjusted meta-analytic effect- 
#caregiver anxiety pre-post 
#outlier removed
#NO GOODMAN 2013 
dat6 <- read.csv("C:\\Users\\hswer\\Documents\\Comps\\MetaMasterTable_CG_reduced1..csv")
View(dat6)

# making subset data frames so we can examine effects 
# without goodman 2013

dat6.ppaxo <- subset(dat6, AnxietyWP == 1)
dat6.ppaxo


# random effects
reml.ppaxo <- rma(
  yi = AnxES_C_WP, 
  vi = VarAnxES_C_WP, 
  data = dat6.ppaxo, 
  method = "REML")

# THIS IS WHAT YOU WILL GET MOST OF THE #'S IN TABLES 
#outlier removed 
summary(reml.ppaxo)


##############################################
# funnel plot
# WE NEED TO BE ABLE TO SAY THAT WE AT LEAST EXAMINED THESE
#rerun with outlier removed
funnel(reml.ppaxo)

# Egger's regression test to see if funnel asymmetry is significant
# THE FINAL COLUMNS OF TABLEs
#rerun outlier removed
regtest(reml.ppaxo)

# trim and fill 
# THIS WILL GIVE YOU THE ADJ EST FOR TAbles
# FOR ANY ROWS THAT SHOW SIGNIFICANT FUNNEL ASYMETRY
tf.tf <- trimfill(reml.ppaxo)
tf.tf
funnel(tf.tf)

### step 5: selection method analyses
#outlier removed- caregiver anxiety within-person 
##############################################
# first, getting rid of rows of missing data in a new dataframe
dat6.ppaxoNoMissing <- subset(dat6.ppaxo, is.na(AnxES_C_WP) == FALSE)
dat6.ppaxoNoMissing


# second, making the correct vectors

t.stat <- (dat6.ppaxoNoMissing$AnxES_C_WP*sqrt(
  dat6.ppaxoNoMissing$nEff_wpC-2))/2
t.stat

n.pre <- dat6.ppaxoNoMissing$nEff_wpC
n.post <- dat6.ppaxoNoMissing$nEff_wpC


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
# meta-analytic regression - moderators- caregiver anxeity wp 
########################################################################

model <- rma(yi = AnxES_C_WP, 
             vi = VarAnxES_C_WP, 
             mods = ~ Control.Group,
             data = dat6.ppaxo, 
             method = "REML")
summary(model)

model <- rma(yi = AnxES_C_WP, 
             vi = VarAnxES_C_WP, 
             mods = ~ HoursTxMod,
             data = dat6.ppaxo, 
             method = "REML")
summary(model)

model <- rma(yi = AnxES_C_WP, 
             vi = VarAnxES_C_WP, 
             mods = ~ MonthsPPMod,
             data = dat6.ppaxo, 
             method = "REML")
summary(model)

model <- rma(yi = AnxES_C_WP, 
             vi = VarAnxES_C_WP, 
             mods = ~ percompM,
             data = dat6.ppaxo, 
             method = "REML")
summary(model)

model <- rma(yi = AnxES_C_WP, 
             vi = VarAnxES_C_WP, 
             mods = ~ FidelityMonitoring,
             data = dat6.ppaxo, 
             method = "REML")
summary(model)



