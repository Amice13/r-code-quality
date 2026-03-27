# Protest Brokers and the Technology of Mobilization: Evidence from South Africa
# R Code for Replication
# By: Sarah J. Lockwood
# 
# This version: 22 June 2020
# R version 4.0.2 (2020-06-22)


### Clear Global Environment ###
rm(list=ls())

### Set working directory
# setwd()

#################################################################################
#################################################################################
########################### Load Replication Datasets ###########################
#################################################################################
#################################################################################

##### Load Replication Dataset (not including data on apartheid-era protests) #####
df <- read.csv(file="SJL_CPS_Replication Dataset.csv",header=TRUE,sep=",", dec=",")

##### Load Replication Dataset (including data on apartheid-era protests from Olzak and Olivier) ####

## I am grateful to Susan Olzak and Johan L. Olivier for sharing unpublished data on protests
# in apartheid-era South Africa with me. This data records the number of incidents of "ethnic
# collective action" in 84 Magisterial Districts in  South Africa during the period 1970-85.
# The Magisterial Districts used do not align neatly with the 2011 Census main places. To
# align the data, therefore, I did a proportional intersection using GIS data from both
# the 1990 and 2011 Censuses. As this is not a national level dataset, regressions using this
# data were only run on the subset of main places for which Olzak and Olivier have data (1,670)
# main places.
# Note, Funding for the Olzak and Olivier research was provided by a National Science
# Foundation Grant SES-9196229, Sociology Program: "Racial Conflict in South Africa and the 
# United States." Susan Olzak, Principle Investigator. See also Olzak and Olivier (1998), and
# Olzak et al (2003).

dfolivier <- read.csv(file="SJL_CPS_Replication Dataset Apartheid.csv",header=TRUE,sep=",", dec=",")


#################################################################################
#################################################################################
############################## Replicating Table C.2  ###########################
#################################################################################
#################################################################################

sapply(df, is.numeric)
df[, c(6:14)] <- sapply(df[, c(6:14)], as.numeric)
sapply(dfolivier, is.numeric)
dfolivier[, c(6:14)] <- sapply(dfolivier[, c(6:14)], as.numeric)

#### Load Packages
library(ggplot2)
library(MASS)
library(sandwich)
library(msm)

###############
######  Model 1: Poisson on full dataset of main places
###############

Model1 <- glm(nprot ~ slevel + log(income) + intineq + bordineq + unemp + educ + PerBlack + 
               ethnicdiv + PerU30, data=df, family=poisson)

summary(Model1)

###############
######  Model 2: Poisson on subset, including measure of anti-apartheid protest
###############

Model2 <- glm(nprot ~ slevel + log(income) + intineq + bordineq + unemp + educ + PerBlack + 
               ethnicdiv + PerU30 +sizeprot, data=dfolivier, family=poisson)

summary(Model2)

###############
######  Model 3: Logit on full dataset of main places
###############

Model3 <- glm(nprotbin ~ slevel + log(income) + intineq + bordineq + unemp + educ + 
              PerBlack + ethnicdiv + PerU30, data = df, family = binomial)

summary(Model3)

###############
######  Model 4: Poisson on subset, including measure of anti-apartheid protest
###############
Model4 <- glm(nprotbin ~ slevel + log(income) + intineq + bordineq + unemp + educ + 
              PerBlack + ethnicdiv + PerU30 + sizeprot, data = dfolivier, family = binomial)

summary(Model4)

