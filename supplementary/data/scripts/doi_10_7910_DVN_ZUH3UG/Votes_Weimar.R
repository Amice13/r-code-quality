## Hager / Hilbig - Inheritance replication code - Feb 26 2019
## hhilbig@g.harvard.edu
##
## This file : Inheritance and voting behavior during the Weimar Republic
## Table reproduced in this file: Table 7
##
### ### ### ###

rm(list = ls())

## Load packages

library(dplyr)
library(stargazer)
library(reshape2)

## Get the data

ele33 <- read.csv("Votes_Weimar.csv",
                  stringsAsFactors = F)

## Standardize the control variables

ele33$unem33 <- as.numeric(scale(ele33$unem33))
ele33$cath25 <- as.numeric(scale(ele33$cath25))
ele33$bluecollar25 <- as.numeric(scale(ele33$bluecollar25))

## Currently the data is in wide format, with different variables for the vote shares of the 
## NSDAP and the Communist party
## Before reshaping the data for the panel regression, we declare a list of variable names 

## NSDAP variables

nsvars <- c('nsdap33', 'nsdap32n', 'nsdap327', 'nsdap309', 
            'nsdap285')

## Note that NSDAP only ran in five elections

## Communist variables

commvars <- c('communist33', 'communist32n', 
              'communist327', 'communist309',
              'communist285',
              'communist24d', 'communist245',
              'communist206')

## The comm. party ran in 8 elections

#### TABLE & : Historic vote share panel regressions ####

## Reshape the data : NSDAP

pool_df_nsdap <- melt(ele33, measure.vars = nsvars,
                id.vars = c('fair_dic', 'LAND', 'unem33', 'cath25', 'pop25',
                            'bluecollar25'), variable.name = 'year',
                value.name = 'nsdap')

## Reshape the data : Communist party

pool_df_comm <- melt(ele33, 
                     measure.vars = commvars,
                     id.vars = c('fair_dic', 'LAND', 'unem33', 'cath25', 'pop25',
                                 'bluecollar25'), 
                     variable.name = 'year',
                     value.name = 'communist')

## Now we can run regressions on the reshaped data

## NSDAP regressions 

m1 <- lm(nsdap ~ fair_dic, pool_df_nsdap)
m2 <- lm(nsdap ~ fair_dic + factor(LAND) + unem33 + cath25 + 
           log(pop25) + bluecollar25 + factor(year), pool_df_nsdap)

## Communist party regressions

m3 <- lm(communist ~ fair_dic, pool_df_comm)
m4 <- lm(communist ~ fair_dic + factor(LAND) + unem33 + cath25 + 
           log(pop25) + bluecollar25 + factor(year), pool_df_comm)

## Output via stargazer

stargazer(list(m1, m2, m3, m4), keep = c("fair", "unem", "cath", "pop", 'bluecol'), 
          covariate.labels = c("Equitable", "Unemployment 1933", 
                               "Catholic share 1925", "Log pop. 1925",
                               "Blue collar share, 1925"),
          dep.var.labels = rep(c('NSDAP', 'Communists', 'Communists (w/o 1920'), 
                               each = 1),
          keep.stat = c("n", 'rsq'),
          star.cutoffs = c(NA, NA, NA),
          column.sep.width = "5pt",
          header = F,
          style = 'ajps',
          model.numbers = F,
          add.lines = list(c('State FE', 'No', 'Yes', 'No', 'Yes'),
                           c('Election FE', 'No', 'Yes', 'No', 'Yes')),
          no.space = T)

