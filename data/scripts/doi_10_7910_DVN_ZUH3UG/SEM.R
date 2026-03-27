## Hager / Hilbig - Inheritance replication code - Feb 26 2019
## hhilbig@g.harvard.edu
##
## This file : Structural Equation Modeling
## Tables reproduced in this file: A11
##
### ### ### ###

rm(list = ls())

## packages

library(dplyr)
library(pbapply)
library(lavaan)

#### Load the data

inher <- read.csv('Data_Main.csv')

#### FIGURE A11 : SEM ####

## Scale Rotary nobility presence

inher$rot_scaled <- as.numeric(scale(inher$rot_adel_int1))

## Scale ancient nobility and female shares

inher$rot_scaled_uradel <- as.numeric(scale(inher$rot_uradel))
inher$rot_scaled_fem <- as.numeric(scale(inher$rot_adel_fem))

## Specify the model

model1 <- '
# regressions
rot_scaled ~ fair_dic
gem_women_share ~ fair_dic
rot_scaled  ~ gem_women_share
# residual correlations
rot_scaled ~~ rot_scaled
gem_women_share ~~ gem_women_share
fair_dic ~~ fair_dic
gem_pop_density ~~ gem_pop_density
'

## Fit the model

fit <- lavaan::sem(model1, data=inher)

## Get output for figure A10

summary(fit, standardized=T)

