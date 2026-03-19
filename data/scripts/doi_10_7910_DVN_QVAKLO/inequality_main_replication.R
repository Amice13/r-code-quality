
################################################################# 
# Vladimir Chlouba
# The Ohio State University, Department of Political Science
# chlouba.1@osu.edu
#################################################################
# Liberation Wars as Critical Junctures: Colonial Heritage and
# the Persistence of Inequality (Tables)
#################################################################

# loading the required R packages
library(foreign)
library(ggplot2)
library(stargazer)
library(MASS)
library(caret)
library(ordinal)
library(QuantPsyc)
library(reshape2)
library(plyr)
library(boot)
library(texreg)
library(arm)
library(plyr)
library(nnet)
library(Amelia)
library(effects)
library(Hmisc)
library(gridExtra)
library(MatchIt)
library(rgenoud)
library(Zelig)
library(olsrr)
library(interplot)
library(ggthemes)
library(faraway)
library(lfe)
library(dotwhisker)

rm(list = ls())

#~^~^~^~^~^~^~^~^~^~^~^~^~^~^~^~^~^~^~^~^~^~^~^~^~^~^~^~^~^~^~^~^~^~^~^~^~^~^~^~^~^~^~^~^~^~^~^~^~^~

# loading the dataset
data <- read.csv("inequality_data.csv")

# subseting for non-Latin American countries
data2 <- subset(data, data$omit == 0) 

#~^~^~^~^~^~^~^~^~^~^~^~^~^~^~^~^~^~^~^~^~^~^~^~^~^~^~^~^~^~^~^~^~^~^~^~^~^~^~^~^~^~^~^~^~^~^~^~^~^~

##########################################################################################
# Table 1: Settler Colonialism, Indigenous Liberation Wars, and Inequality (OLS Estimates)
##########################################################################################

# creaing lists of variables
iv_list <- c('sett_ind*war')

control_list_basic <- c('neurope+sharing_unit+quality_score')
control_list_pre <- c('+latitude+frac+soil+minerals+rugged+landlock+log(pop_1400+1)+log(slave_exports+1)+year_ind')
control_list_post <- c('+log(gdp_pc)+log(pop)+polity+war_ind+IndViol')

dv_list <- c('gini_avg')

# creating formulas
fmla1 <- as.formula(paste(paste(dv_list[1],"~"),
                          paste(iv_list[1], "+"),
                          paste(control_list_basic),
                          paste("| ht_colonial + continent | 0 | region")))

fmla2 <- as.formula(paste(paste(dv_list[1],"~"),
                          paste(iv_list[1], "+"),
                          paste(control_list_basic),
                          paste(control_list_pre),
                          paste("| ht_colonial + continent | 0 | region")))

fmla3 <- as.formula(paste(paste(dv_list[1],"~"),
                          paste(iv_list[1], "+"),
                          paste(control_list_basic),
                          paste(control_list_pre),
                          paste(control_list_post),
                          paste("| ht_colonial + continent | 0 | region")))

fmla4 <- as.formula(paste(paste(dv_list[1],"~"),
                          paste(iv_list[1], "+"),
                          paste(control_list_basic),
                          paste(control_list_pre),
                          paste(control_list_post),
                          paste("| ht_colonial + continent + region | 0 | region")))

# estimate coefficients
m1 <- felm(fmla1, data=data2)
m2 <- felm(fmla2, data=data2)
m3 <- felm(fmla3, data=data2)
m4 <- felm(fmla4, data=data2)

# producing LaTeX code
stargazer(m1, m2, m3, m4, no.space = T, digits = 2)

#~^~^~^~^~^~^~^~^~^~^~^~^~^~^~^~^~^~^~^~^~^~^~^~^~^~^~^~^~^~^~^~^~^~^~^~^~^~^~^~^~^~^~^~^~^~^~^~^~^~

############################################################################################
# Table 2: Inequality at Independence, Inequality Today, and Property Rights (OLS Estimates)
############################################################################################

# creaing lists of variables
iv_list <- c('gini_ind*wef_pr')

control_list_basic <- c('neurope+log(gdp_pc)+log(pop)+frac+polity+latitude+minerals+sharing_unit+quality_score')

dv_list <- c('gini_avg')

# creating formulas
fmla1 <- as.formula(paste(paste(dv_list[1],"~"),
                          paste(iv_list[1], "+"),
                          paste(control_list_basic),
                          paste("| ht_colonial + continent | 0 | 0")))

fmla2 <- as.formula(paste(paste(dv_list[1],"~"),
                          paste(iv_list[1], "+"),
                          paste(control_list_basic),
                          paste("| ht_colonial + continent + region | 0 | 0")))

# estimate coefficients
ma <- felm(fmla1, data=data2)
mb <- felm(fmla2, data=data2)

# updating lists of variables
iv_list <- c('sett_ind*war')

control_list_basic <- c('war_ind+IndViol+log(gdp_pc)+log(pop)+latitude')

dv_list <- c('wef_pr')

# creating formulas
fmla1 <- as.formula(paste(paste(dv_list[1],"~"),
                          paste(iv_list[1], "+"),
                          paste(control_list_basic),
                          paste("| ht_colonial + continent | 0 | 0")))


fmla2 <- as.formula(paste(paste(dv_list[1],"~"),
                          paste(iv_list[1], "+"),
                          paste(control_list_basic),
                          paste("| ht_colonial + continent + region | 0 | 0")))

# estimate coefficients
mc <- felm(fmla1, data=data2)
md <- felm(fmla2, data=data2)

# producing LaTeX code
stargazer(ma, mb, mc, md, no.space = T, digits = 2)

#~^~^~^~^~^~^~^~^~^~^~^~^~^~^~^~^~^~^~^~^~^~^~^~^~^~^~^~^~^~^~^~^~^~^~^~^~^~^~^~^~^~^~^~^~^~^~^~^~^~
