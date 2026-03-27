####################################################################
## author:    Steven M. Van Hauwaert & Robert A. Huber
## file name: terror_master.R
## Context:   Causal Effects of Terror Attacks
## started:   2018-03-13
## Summary:   runs R-Scripts
######################################################################

## empty memory (!)
rm(list=ls())

# Set working directory ---------------------------------------------------
setwd()

Sys.setlocale("LC_ALL", "English")

# Packages ----------------------------------------------------------------

library(foreign)
library(rdd)
library(ggplot2)
library(MASS)
library(texreg)
library(ggthemes)
library(gridExtra)
library(psych)
library(forcats)
library(nFactors)
library(xtable)
set.seed(42)

#################
## run scripts ##
#################
#source("./terror_cleaning.R", verbose=F)   ## read and process data. not necessary for replication.
source("./terror_rdd.R", verbose = F)       ## runs RDD
source("./terror_robustEB.R", verbose = F)  ## runs Robustness Test w/ EB data
