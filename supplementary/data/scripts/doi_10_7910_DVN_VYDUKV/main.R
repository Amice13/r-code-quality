#Set working directory to replication folder
#-----------------------------
#Packages necessary to run analyses
library(tidyverse)
library(latex2exp)
library(readxl)
library(ggpattern)
library(marginaleffects)
library(estimatr)
library(texreg)
#-----------------------------------------------------------------
#Set parameters for plots
title.size <- 20
x.axis.tick.size <- 14
y.axis.tick.size <- 14
x.axis.label.size <- 18
y.axis.label.size <- 18
facet.text <- 15

############
#Produces figures and tables in the main text of the paper. Additionally,
#produces Tables C1,C2,C3 in the appendix. Tables are identical in terms of 
#structure and values as the paper. Some formatting occurred directly in the 
#tex file
############

source('Code/03_Main_Analyses.R')


############
#Produces Figure A1
############

source('Code/04_Figure_A1_Simulations.R')


############
#Produces Figure A2
############

source('Code/05_Figure_A2_Simulations.R')

############
#Reruns analyses using DIME scores. Produces Table C4. 
############

source('Code/06_Robustness_Check_CF_Scores.R')


############
#Produces Figure B2
############

source('Code/08_Figure_B2.R')





