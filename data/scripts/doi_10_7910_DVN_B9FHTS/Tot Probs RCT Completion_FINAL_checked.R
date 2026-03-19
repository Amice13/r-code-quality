#the following source code was modified from resources provided by Dr. Ron Rogge, see Macri & Rogge, 2024
#code below is for effects of attachment-based interventions
#on child total problems relative to control aka RCT designs
#at immediate post-treatment assessment

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

# PULLING IN SELECTION METHOD FUNCTIONS ON  personal COMPUTER 
source("C:\\Users\\hswer\\Documents\\Comps\\selection.meta.functions.R")


# pulling in the data 
dat81 <- read.csv("C:\\Users\\hswer\\Documents\\Comps\\MetaMasterTable_child_reduced.csv")
View(dat81)

# making subset data frames so we can examine effects using
# specific types of flexibility dimensions as mediators

dat81.rcttot <- subset(dat81, CTotRCT == 1)
dat81.rcttot



#####################################################################
#####################################################################
#####################################################################
#####################################################################
# child total probs RCT- posttreatment
#####################################################################
#####################################################################
#####################################################################
#####################################################################

##########################################################
##########################################################
##########################################################
# rcttot
##########################################################
##########################################################
##########################################################

##########################################################
# between PERSON (between group d post treatment)
##########################################################


### step 1: calculate meta analytic estimate
###### child total problems RCT
##############################################


# random effects
reml.rcttot <- rma(
  yi = TotPrES_C_RCT, 
  vi = VarTotC_C_RCT, 
  data = dat81.rcttot, 
  method = "REML")

# THIS IS WHAT YOU WILL GET MOST OF THE #'S IN TABLES
summary(reml.rcttot)


### step 2: plot findings
##############################################
# simple forest plot
forest(reml.rcttot,
       order="obs",
       showweights=FALSE)

# labeled forest plot
forest(reml.rcttot,
       order="obs",
       header=TRUE,
       slab=paste(dat8.rctint$FirstAuthor, dat8.rctint$Year, sep="  "),
       ilab = cbind(dat8.rctint$ntotEff_rctCc),
       ilab.xpos = -2.2,
       mlab = "Random Effects Model Estimate",
       xlab = "RCT Completion Total Problems Effect",
       xlim = c(-4.2, 1.5),
       showweights=FALSE)
op <- par(cex=1, font=2)
text(c(-2.3), 38, c("N"), 
     pos = -2.2)
par(op)

#forest plot with treatment name column
forest(reml.rcttot,
       order="obs",
       header=TRUE,
       slab=paste(dat81.rcttot$FirstAuthor, dat81.rcttot$Year, sep="  "),
       ilab=cbind(dat81.rcttot$ntotEff_rctCc, dat81.rcttot$ABIabbrev),
       ilab.xpos = c(-6, -4.5),
       mlab = "Random Effects Model Estimate",
       xlab = "RCT Post-Treatment Total Problems Effect",
       xlim = c(-10, 4),
       showweights=FALSE)
op <- par(cex = 1, font =2)
text(c(-6, -4.5), 15.3, c("n", "Treatment"), pos = 3)
par(op)

### step 3: outlier and influence diagnostics
#### chlid total problems RCT posttreatment 
##############################################
# get outlier/influence stats and plot
inf <- influence(reml.rcttot)
plot(inf)
out_lier <- subset(inf$inf, abs(inf$inf$rstudent) >= 2 & abs(inf$inf$cook.d) >=.05)
out_lier

#run without studies 3 and 13
reml.noInfluencer.rcttot <- rma(
  yi = TotPrES_C_RCT, 
  vi = VarTotC_C_RCT, 
  data = dat81.rcttot, 
  method = "REML",
	subset = c(-3, -13))
summary(reml.noInfluencer.rcttot)


# leave-one-out
#this is another way to look for mis-typed effects
# LOOK AT THESE ROWS AND MAKE SURE THE #'S IN THE FIRST COLUMN BARELY CHANGE
ronoutput <- leave1out(reml.rcttot)
ronoutput

### step 4: publication bias
##############################################
#child total problems RCT
# funnel plot
# WE NEED TO BE ABLE TO SAY THAT WE AT LEAST EXAMINED THESE
funnel(reml.rcttot)




# Egger's regression test to see if funnel asymmetry is significant
# THE FINAL COLUMNS OF TABLES 
regtest(reml.rcttot)




# trim and fill 
# THIS WILL GIVE YOU THE ADJ EST FOR TABLES 
# FOR ANY ROWS THAT SHOW SIGNIFICANT FUNNEL ASYMETRY
tf.tf <- trimfill(reml.rcttot)
tf.tf
funnel(tf.tf)

### step 5: selection method analyses
##############################################
# first, getting rid of rows of missing data in a new dataframe
dat81.rcttotNoMissing <- subset(dat81.rcttot, is.na(TotPrES_C_RCT) == FALSE)
dat81.rcttotNoMissing


# second, making the correct vectors

t.stat <- (dat81.rcttotNoMissing$TotPrES_C_RCT*sqrt(
  dat81.rcttotNoMissing$ntotEff_rctCc-2))/2
t.stat

n.Tx <- dat81.rcttotNoMissing$nTx_rctCc
n.control <- dat81.rcttotNoMissing$ncntrl_rctCc

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




########################################################################
# meta-analytic regression - IGNORE FOR NOW
########################################################################

model <- rma(yi = TotPrES_C_RCT, 
             vi = VarTotC_C_RCT, 
             mods = ~ HoursTx,
             data = dat81.rcttot, 
             method = "REML")
summary(model)

model <- rma(yi = TotPrES_C_RCT, 
             vi = VarTotC_C_RCT, 
             mods = ~ Months2Completion_RCTc,
             data = dat81.rcttot, 
             method = "REML")
summary(model)

model <- rma(yi = TotPrES_C_RCT, 
             vi = VarTotC_C_RCT, 
             mods = ~ completers,
             data = dat81.rcttot, 
             method = "REML")
summary(model)

model <- rma(yi = TotPrES_C_RCT, 
             vi = VarTotC_C_RCT, 
             mods = ~ FidelityMonitoring,
             data = dat81.rcttot, 
             method = "REML")
summary(model)

model <- rma(yi = TotPrES_C_RCT, 
             vi = VarTotC_C_RCT, 
             mods = ~ CntrlType,
             data = dat81.rcttot, 
             method = "REML")
summary(model)
