## Hager / Hilbig - Inheritance replication code - Feb 26 2019
## hhilbig@g.harvard.edu
##
## This file : Land inequality regression
## Tables reproduced in this file: Table 6
##
### ### ### ###

rm(list = ls())

## Packages

library(foreign)
library(ggplot2)

## Load data

ds2 <- read.csv(file = 'LandGini.csv',
                encoding = 'UTF-8', stringsAsFactors = F)

#### TABLE 6 : Land Gini ####

m1 <- lm(landgini ~ fair_dic + population_1895 + law_baden_law +
                   law_common_law + law_dan_law + law_pruss_law +
                   law_code_civil + support_expenses_total_capita +
                   childlabor_mean_1898, data = ds2)

## Check model results

stargazer(m1, keep = 'fair_dic', digits = 2)
