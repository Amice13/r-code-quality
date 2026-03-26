## Hager / Hilbig - Inheritance replication code - Feb 26 2019
## hhilbig@g.harvard.edu
##
## This file : Regressions using the GESIS municipality panel data
## Tables reproduced in this file: A4, A5
##
### ### ### ###

rm(list = ls())

## packages

library(ggplot2)
library(foreign)
library(car)
library(AER)
library(stargazer)
library(dplyr)

## Get data

gesis <- read.csv('GESIS_Councils.csv',
                  stringsAsFactors = F)

#### TABLE A3: Female representation panel data ####

## Regression w/o covariates

m1 <- lm(fem_share ~ fair_dic + factor(year), data = gesis)

## Regression with covariates

m4 <- lm(fem_share ~ fair_dic + factor(year) + lon + lat + factor(land) +
           gem_council + gem_pop_density + pop_tot, 
         data = gesis)

## output

stargazer(list(m1, m4), keep = c('fair_dic', 'gem_council', 'gem_pop_density'), 
          covariate.labels = c('Equitable Inheritance', 'Council size', 'Populaton density'),
          keep.stat = c('n', 'rsq'), 
          add.lines = list(c('Controls', 'No', 'Yes'), 
                           c('State FE', 'No','Yes'),
                           c('Year FE', 'Yes', 'Yes')),
          style = 'ajps',
          title = 'IV Results, w\ controls (Female representation)',
          header = F, star.cutoffs = c(NA,NA,NA),
          font.size = 'footnotesize',
          column.sep.width = '2pt')



#### TABLE A4 : IV ####

## Instrument: Mean elevation

m1_iv_c <- ivreg(fem_share ~ fair_dic + lon + lat + factor(year) +
                   factor(land) +
                   + childlabor_mean_1898 + factor(law_cat) +
                   support_expenses_total_capita +
                   gem_council + gem_pop_density +   pop_tot | 
                   elev_mean + lon + lat + factor(land) + factor(year)+
                   + childlabor_mean_1898 + factor(law_cat) +
                   support_expenses_total_capita +
                   gem_council + gem_pop_density +   pop_tot, 
                 data= gesis)

## Instrument: Distance to rivers

m2_iv_c <- ivreg(fem_share ~ fair_dic + lon + lat + factor(year)+
                   + childlabor_mean_1898 + factor(law_cat) +
                   support_expenses_total_capita +
                   gem_council + gem_pop_density +   pop_tot  | 
                   river_dist_min + lon + lat + factor(year)+
                   + childlabor_mean_1898 + factor(law_cat) +
                   support_expenses_total_capita +
                   gem_council + gem_pop_density +   pop_tot, 
                 data= gesis)

## Instrument: Roman rule 

m3_iv_c <- ivreg(fem_share ~ fair_dic + lon + lat + factor(land)  +
                   factor(year)+
                   + childlabor_mean_1898 + factor(law_cat) +
                   support_expenses_total_capita +
                   gem_council + gem_pop_density +   pop_tot| 
                   roman + lon + lat + factor(land) + factor(year)+
                   + childlabor_mean_1898 + factor(law_cat) +
                   support_expenses_total_capita +
                   gem_council + gem_pop_density +   pop_tot, 
                 data= gesis)

## Make a list of models

control_list <- list(m1_iv_c, m2_iv_c, m3_iv_c)

## Helper function to get the FS F-stat

get_f_stat <- function(iv_model) {
  s <- summary(iv_model, diagnostics = T)
  f_stat <- s$diagnostics[1, 3]
  f_stat
}

## Get F-stats

f_control <- c('F-Stat (1st Stage)',
               round(sapply(control_list, get_f_stat), 2))

## Output via stargazer

stargazer(control_list, keep = c('fair_dic', 'gem_council', 'gem_pop_density'), 
          covariate.labels = c('Equitable Inheritance', 'Council size', 'Populaton density'),
          keep.stat = 'n', add.lines = list(c('Controls', rep('Yes', 3)), 
                                            c('State FE', 'Yes','No', 'Yes'),
                                            c('Year FE', 'Yes', 'Yes', 'Yes'),
                                            f_control),
          style = 'ajps',
          title = 'IV Results, w\ controls (Female representation)',
          header = F, star.cutoffs = c(NA,NA,NA),
          font.size = 'footnotesize',
          column.sep.width = '2pt')
