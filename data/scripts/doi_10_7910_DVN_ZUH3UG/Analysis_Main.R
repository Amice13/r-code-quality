## Hager / Hilbig - Inheritance replication code - Feb 26 2019
## hhilbig@g.harvard.edu
##
## This file : Main OLS Regressions
## Figures reproduced in this file: 1, 2, A1, A5, A8, A10
##
### ### ### ###

rm(list = ls())

## Packages

library(car)
library(dplyr)
library(stargazer)
library(pbapply)

#### Load the data

inher <- read.csv('Data_Main.csv')

## Standardize population density, total population and  council size

inher$gem_pop_density <- as.numeric(scale(inher$gem_pop_density))
inher$pop_tot <- as.numeric(scale(inher$pop_tot))
inher$gem_council <- as.numeric(scale(inher$gem_council))

## Standardize Hartz 4 recipients variable

inher$h4_recip <- as.numeric(scale(inher$ue_longterm / inher$pop_tot))

## List of observations that have a Rotary chapter in municipality

rot_main <- inher$rot_nearest_neighbor == 0

## Standardize all three Rotary outcomes in these municipalities

for (j in c('rot_adel_int1', 'rot_uradel_int', 'rot_fem_share')) {
  inher[rot_main, paste0(j, '_z')] <- as.numeric(scale(inher[rot_main, j]))
}

## Standardize Rotary outcomes in all municipalities
## The difference compared to the last command is that we now
## assign municipalities w/o Rotary chapters the closest chapter

for (j in c('rot_adel_int1', 'rot_uradel_int', 'rot_fem_share')) {
  inher[, paste0(j, '_z2')] <- as.numeric(scale(inher[, j]))
}

#### TABLE 1 : Main OLS regressions ####

## These are the main specifications
## Subsetting the data is done so we only look at municipalities with Rotary chapters

m1 <- lm(gem_women_share ~ fair_dic, 
         data = inher)
m2 <- lm(rot_fem_share_z ~ fair_dic, 
         data = inher[inher$rot_nearest_neighbor == 0, ])
m3 <- lm(rot_adel_int1_z ~ fair_dic, 
         data = inher[inher$rot_nearest_neighbor == 0, ])
m4 <- lm(rot_uradel_int_z ~ fair_dic, 
         data = inher[inher$rot_nearest_neighbor == 0, ])

## Check Coefficients for the treatment ## 

lapply(list(m1, m2, m3, m4), function(x) summary(x)$coefficients[2, ])

## Latex Output 

stargazer(list(m1, m2, m3, m4), 
          keep = 'fair_dic',
          keep.stat = 'n', 
          covariate.labels = 'Equitable Inheritance')

#### TABLE 2 : Main specification with controls ####

## This specification adds controls for population density, council size,
## total population, child labor, welfare expenditures, and the legal code

m1_co2 <- lm(gem_women_share ~ fair_dic +
              childlabor_mean_1898 +  support_expenses_total_capita +
               gem_council + gem_pop_density +  pop_tot +
              factor(law_cat), 
            data=inher)
m2_co2 <- lm(rot_fem_share_z ~ fair_dic+
              childlabor_mean_1898 +  support_expenses_total_capita +
                +gem_pop_density +  pop_tot +
              factor(law_cat),
            data=inher[inher$rot_nearest_neighbor == 0, ])
m3_co2 <- lm(rot_adel_int1_z ~ fair_dic +
              childlabor_mean_1898 +  support_expenses_total_capita +
               gem_pop_density +  pop_tot +
              factor(law_cat),
            data=inher[inher$rot_nearest_neighbor == 0, ])
m4_co2 <- lm(rot_uradel_int_z ~ fair_dic +
              childlabor_mean_1898 +  support_expenses_total_capita +
               gem_pop_density +  pop_tot +
              factor(law_cat), 
            data=inher[inher$rot_nearest_neighbor == 0, ])

## Check Coefficients for the treatment ## 

lapply(list(m1_co2, m2_co2, 
            m3_co2, m4_co2), function(x) summary(x)$coefficients[2, ])

## Output via stargazer

stargazer(list(m1_co2, m2_co2, 
               m3_co2, m4_co2))

#### TABLE A10 : Interaction btw. welfare state recipients and treatment ####

## This interacts the welfare state recipients in 1890 and 2015 with the treatment
## Prior to this, we create an 'equality' index, which is composed of the share of female 
## politicans and the presence of nobility in rotary clubs (this enters negatively)

## scale welfare recipient var

inher$welfare_recip_capita <- as.numeric(scale(inher$welfare_recip_capita))

## scale female representation variable 

inher$gem_women_share_z <- as.numeric(scale(inher$gem_women_share))

## equality index 
## note that the rotary share enters negatively

inher$eq_index <- inher$gem_women_share_z - inher$rot_adel_int1_z

## equality index, also for municipalities w/o rotary chapters - nearest neighbor approach
## note that the rotary share enters negatively

inher$eq_index_nn <-  inher$gem_women_share_z - 
  as.numeric(scale(inher$rot_adel_int1))

## regression 1 : current welfare expenditures

m1_i2 <- lm(eq_index ~ fair_dic +
              childlabor_mean_1898 + welfare_recip_capita + 
              fair_dic:welfare_recip_capita + 
              support_expenses_total_capita + 
              factor(law_cat), 
data=inher[inher$rot_nearest_neighbor == 0, ])

## regression 2 : historic expenditures

m2_i2 <- lm(eq_index ~ fair_dic +
              childlabor_mean_1898 +  support_expenses_total_capita +
              factor(law_cat) + fair_dic:support_expenses_total_capita, 
            data=inher[inher$rot_nearest_neighbor == 0, ])

## regression 3  : historic expenditures, including municipalities without a rotary chapter

m3_i2 <- lm(eq_index_nn ~ fair_dic +
              childlabor_mean_1898 +  support_expenses_total_capita +
              factor(law_cat) + fair_dic:support_expenses_total_capita, 
            data=inher)

## check the coefficients 

lapply(list(m1_i2, m2_i2, m3_i2), function(x) summary(x)$coefficients)

#### Table output

stargazer(list(m1_i2, m2_i2, m3_i2), 
          keep = c('fair_dic', 'welfare_recip_capita',
                   'support_expenses_total_capita'),
          covariate.labels = c('Equitable inheritance',
                                'Welfare recipients (2015)',
                               'Welfare spending (1890)',
                               'Equitable inheritance * Welfare recipients (2015)',
                                'Equitable inhertiance * Welfare expenditures (1890)'),
          keep.stat = 'n', 
          style = 'ajps',
          header = F, star.cutoffs = c(NA,NA,NA),
          font.size = 'footnotesize',
          column.sep.width = '2pt')

#### TABLE A8 : Subsetting - only councils where the CDU is the biggest party ####

## This specification subsets that data to only include councils where the CDU/CSU is the biggest party

## First regression only has treatment, the second adds covars

m0a_gem <- lm(gem_women_share ~ fair_dic,
         data = inher, subset = inher$gem_cdu_biggest == 1)
m2a_gem <- lm(gem_women_share ~ fair_dic + 
            gem_council + gem_pop_density +  pop_tot +
            childlabor_mean_1898 + 
            support_expenses_total_capita +
            factor(law_cat),
              data = inher, subset = inher$gem_cdu_biggest == 1)

## make list of models

mlist2_gem <- list(m0a_gem, m2a_gem)

## stargazer out

keep_vars <- c('fair', 'gem_council', 'gem_pop_density', 'pop_tot')
vnames <- c('Equitable Inheritance', 
            'Council size', 
            'Populaton density',
            'Total population')

## do out 2 : CDU-dominated only

stargazer(mlist2_gem, keep = keep_vars, 
          covariate.labels = vnames,
          keep.stat = 'n', 
          add.lines = list(c('Controls', c('No', 'Yes', 'Yes'))),
          style = 'ajps',
          title = 'OLS Results (Female representation) - CDU-dominated councils',
          header = F, star.cutoffs = c(NA,NA,NA),
          font.size = 'footnotesize',
          column.sep.width = '2pt',
          dep.var.labels = 'Female representation')

#### TABLE A5: Income and income inequality ####

## Run OLS w/ three outcomes

m1 <- lm(log_suminc31_mean ~ fair_dic + lon+ lat+ factor(state), 
         data=inher)
m2 <- lm(log_suminc31_median ~ fair_dic+ lon+ lat+ factor(state), 
         data=inher)
m3 <- lm(log_suminc31_gini ~ fair_dic+ lon+ lat+ factor(state), 
         data=inher)

## To list

ml <- list(m1,m2,m3)

## Stargazer output

stargazer(ml, keep = 'fair_dic', 
          covariate.labels = 'Equitable inheritance',
          keep.stat = c('rsq', 'n'), 
          add.lines = list(c('Controls', rep('Yes', 3)), 
                           c('State FE', 'Yes','Yes', 'Yes')),
          style = 'ajps',
          header = F, star.cutoffs = c(NA,NA,NA),
          font.size = 'footnotesize',
          column.sep.width = '2pt')

#### TABLE A1: TAX RECORD WEALTH DATA - CONTINUOUS TREATMENT ####

## Here, we use the rental income as a proxy for wealth inequality

m1u <- lm(log_inc_rental_gini ~ fair_dic, data=inher)

## To list

ml <- list(m1u)

## Stargazer output

stargazer(ml, keep = 'fair_dic', 
          covariate.labels = 'Inequitable inheritance',
          keep.stat = c('rsq', 'n'), 
          add.lines = list(c('Controls', rep('No', 1)), 
                           c('State FE', 'No')),
          style = 'ajps',
          header = F, star.cutoffs = c(NA,NA,NA),
          font.size = 'footnotesize',
          column.sep.width = '2pt',
          dep.var.labels = c('Log GINI'))

