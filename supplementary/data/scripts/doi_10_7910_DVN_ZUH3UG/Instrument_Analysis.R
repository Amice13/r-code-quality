## Hager / Hilbig - Inheritance replication code - Feb 26 2019
## hhilbig@g.harvard.edu
##
## This file : Instrumental variable regressions
## Tables reproduced in this file: 3, 4, 5, A6, A7, A9, A11
##
### ### ### ###

rm(list = ls())

## packages

library(car)
library(dplyr)
library(stargazer)
library(AER)
library(pbapply)

#### PREP ####

## Load the main data

inher <- read.csv('Data_Main.csv')

## Rename the data

df_orig <- inher

## Scale the main Rotary variable 

df_orig$rot_scaled <- as.numeric(scale(df_orig$rot_adel_int1))

## Scale ancient nobility and female in nobility variables

df_orig$rot_scaled_uradel <- as.numeric(scale(df_orig$rot_uradel))
df_orig$rot_scaled_fem <- as.numeric(scale(df_orig$rot_adel_fem))

#### TABLE 3 : Female share outcome #### 

## Standardize controls : Population 

df_orig$gem_pop_density <- as.numeric(scale(df_orig$gem_pop_density))
df_orig$gem_council <- as.numeric(scale(df_orig$gem_council))

## Elevation instrument

m1_iv_c2 <- ivreg(gem_women_share ~ fair_dic +  factor(state) +
                    lon+ lat +
                    childlabor_mean_1898 + factor(law_cat) +
                    support_expenses_total_capita +
                    gem_council + gem_pop_density +   pop_tot | 
                    lon+ lat +
                    elev_mean + factor(state) +
                    childlabor_mean_1898 + factor(law_cat) +
                    support_expenses_total_capita +
                    gem_council + gem_pop_density + pop_tot, 
                  data= df_orig)

## Distance to river instrument

m2_iv_c2 <- ivreg(gem_women_share ~ fair_dic  +
                    childlabor_mean_1898+  factor(law_cat) +
                    support_expenses_total_capita +
                    lon+ lat +
                    gem_council + gem_pop_density +  pop_tot | 
                    river_dist_min  +
                    lon+ lat +
                    childlabor_mean_1898 + factor(law_cat) +
                    support_expenses_total_capita +
                    gem_council + gem_pop_density + pop_tot, 
                  data= df_orig)

## Roman rule instrument

m3_iv_c2 <- ivreg(gem_women_share ~ fair_dic +  factor(state) +
                    childlabor_mean_1898 + factor(law_cat) +
                    support_expenses_total_capita +
                    lon+ lat +
                    gem_council + gem_pop_density +   pop_tot | 
                    roman + factor(state) +
                    lon+ lat +
                    childlabor_mean_1898 + factor(law_cat) +
                    support_expenses_total_capita +
                    gem_council + gem_pop_density +   pop_tot, data= df_orig)

## Brief look at the coefficients

summary(m1_iv_c2)$coefficients[2, ]; summary(m2_iv_c2)$coefficients[2, ]
summary(m3_iv_c2)$coefficients[2, ]

## Make list of models

control_list_2 <- list(m1_iv_c2, m2_iv_c2, m3_iv_c2)

## This function extracts the F-stat from the IV models

get_f_stat <- function(iv_model) {
  s <- summary(iv_model, diagnostics = T)
  f_stat <- s$diagnostics[1, 3]
  f_stat
}

## Obtain F-stats for all models

f_control2 <- c('F-Stat (1st Stage)',
                round(sapply(control_list_2, get_f_stat), 2))

## Output via stargazer

stargazer(control_list_2, 
          keep = c('fair_dic', 'gem_council', 'gem_pop_density'), 
          covariate.labels = c('Equitable Inheritance', 'Council size', 
                               'Populaton density'),
          keep.stat = 'n', 
          add.lines = list(c('Controls', rep('Yes', 4)), 
                           c('State FE', 'Yes','Yes', 'No', rep('Yes', 1)),
                           f_control2),
          style = 'ajps',
          title = 'IV Results, w\ controls (Female representation)',
          header = F, star.cutoffs = c(NA,NA,NA),
          font.size = 'footnotesize',
          column.sep.width = '2pt')


#### TABLE 4 : Rotary share outcome ####

## IV models

## Elevation instrument

m1_iv_c <- ivreg(rot_scaled ~ fair_dic + 
                   childlabor_mean_1898 + factor(law_cat) +
                   lon + lat +
                   support_expenses_total_capita + factor(state) | 
                   elev_mean + 
                   lon + lat +
                   childlabor_mean_1898 + factor(law_cat) +
                   support_expenses_total_capita + factor(state), 
                 data=df_orig[df_orig$rot_nearest_neighbor == 0, ])

## Distance to river instrument

m2_iv_c <- ivreg(rot_scaled ~ fair_dic +  
                   childlabor_mean_1898 + factor(law_cat) +
                   lon + lat +
                   support_expenses_total_capita  | 
                   river_dist_min + 
                   lon + lat +
                   childlabor_mean_1898 + factor(law_cat) +
                   support_expenses_total_capita, 
                 data= df_orig[df_orig$rot_nearest_neighbor == 0, ])

## Roman rule instrument 

m3_iv_c <- ivreg(rot_scaled ~ fair_dic + 
                   childlabor_mean_1898 + factor(law_cat) +
                   lon + lat +
                   support_expenses_total_capita + factor(state)  | 
                   roman +
                   lon + lat +
                   childlabor_mean_1898 + factor(law_cat) +
                   support_expenses_total_capita + factor(state), 
                 data=df_orig[df_orig$rot_nearest_neighbor == 0, ])

## Make a list of models 

control_list <- list(m1_iv_c, m2_iv_c, m3_iv_c)

## Obtain F-stats

f_control <- c('F-Stat (1st Stage)',
               round(sapply(control_list, get_f_stat), 2))

## Output via stargazer

stargazer(control_list, keep = 'fair_dic', 
          covariate.labels = 'Equitable Inheritance',
          keep.stat = 'n', add.lines = list(c('Controls', rep('Yes', 3)), 
                                            c('State FE', 'Yes','No', 'Yes'),
                                            f_control),
          style = 'ajps',
          header = F, star.cutoffs = c(NA,NA,NA),
          column.sep.width = '2pt')

#### TABLE A6 : Municipality-level income ####

## Load the tax record data 

# load(file = "Analysis/04_FDZ/fdz_complete.RData")

## Declare a list of dependent variables 

fdz_depvars <- c("suminc31_gini",
                 "suminc31_mean", "suminc31_median")

## Declare a list of logged dependent variables 

fdz_depvars_log <- c("log_suminc31_gini",
                     "log_suminc31_mean", "log_suminc31_median")

## IV 

## Elevation instrument

m1_iv_income <- ivreg(log_suminc31_mean ~ fair_dic  +
                        lon + lat +
                        childlabor_mean_1898 + factor(law_cat) +
                        support_expenses_total_capita+  factor(state) | 
                        elev_mean  + factor(state) +
                        lon + lat +
                        childlabor_mean_1898 + factor(law_cat) +
                        support_expenses_total_capita, 
                      data= df_orig)

## Distance to river instrument 

m2_iv_income <- ivreg(log_suminc31_mean ~ fair_dic  +
                        lon + lat +
                        childlabor_mean_1898 + factor(law_cat) +
                        support_expenses_total_capita| 
                        river_dist_min  +  lon + lat +
                        childlabor_mean_1898 + factor(law_cat) +
                        support_expenses_total_capita, 
                      data= df_orig)

## Roman rule instrument 

m3_iv_income <- ivreg(log_suminc31_mean ~ fair_dic  + factor(state) +
                        childlabor_mean_1898 + factor(law_cat) +
                        lon + lat +
                        support_expenses_total_capita| 
                        lon + lat +
                        roman  + factor(state) + 
                        childlabor_mean_1898 + factor(law_cat) +
                        support_expenses_total_capita, data= df_orig)



## Run the baseline OLS model

m1_baseline_income <- lm(log_suminc31_mean ~ fair_dic  + factor(state) +
                           childlabor_mean_1898 + factor(law_cat) +
                           lon + lat +
                           support_expenses_total_capita,
                         data = df_orig)

## Make list of IV models 

control_list_income <- list(m1_iv_income, m2_iv_income, m3_iv_income)


## f-stats
f_control <- c('F-Stat (1st Stage)', '',
               round(sapply(control_list_income, get_f_stat), 2))

## add baseline to model list

control_list_income <- list(m1_baseline_income, m1_iv_income, 
                            m2_iv_income, m3_iv_income)

## w/ controls

stargazer(control_list_income, keep = 'fair_dic', 
          covariate.labels = 'Equitable Inheritance',
          keep.stat = 'n', 
          add.lines = list(c('Controls', rep('Yes', 4)), 
                           c('State FE', 'Yes','Yes', 'No', 'Yes'),
                           f_control),
          style = 'ajps',
          title = 'IV Results, w\ controls (Log mean income)',
          header = F, star.cutoffs = c(NA,NA,NA),
          font.size = 'footnotesize',
          column.sep.width = '2pt')

#### TABLE A7 : Municipality-level income inequality #### 

## IV

## Elevation instrument

m1_iv_ineq <- ivreg(log_suminc31_gini ~ fair_dic  + factor(state) +
                   childlabor_mean_1898 + factor(law_cat) +
                   support_expenses_total_capita + lon + lat | 
                   elev_mean  + factor(state)+
                   childlabor_mean_1898 + factor(law_cat) +
                   support_expenses_total_capita + lon + lat, 
                 data= df_orig)

## Distance to river instrument 

m2_iv_ineq <- ivreg(log_suminc31_gini ~ fair_dic  +
                   childlabor_mean_1898 + factor(law_cat) +
                   support_expenses_total_capita + lon + lat| 
                   river_dist_min  +
                   childlabor_mean_1898 + factor(law_cat) +
                   support_expenses_total_capita + lon + lat, 
                 data= df_orig)

## Roman rule instrument 

m3_iv_ineq <- ivreg(log_suminc31_gini ~ fair_dic  + factor(state) +
                   childlabor_mean_1898 + factor(law_cat) +
                   support_expenses_total_capita + lon + lat| 
                   roman  + factor(state) + 
                   childlabor_mean_1898 + factor(law_cat) +
                   support_expenses_total_capita+ lon + lat, 
                   data= df_orig)

## Run baseline OLS regression

m1_baseline_ineq <- lm(log_suminc31_gini ~ fair_dic  + factor(state) +
                    childlabor_mean_1898 + factor(law_cat) +
                    support_expenses_total_capita + lon + lat,
                  data = df_orig)

## Make a list of the IV models 

control_list_ineq <- list(m1_iv_ineq, m2_iv_ineq, m3_iv_ineq)

## Obtain F-stats for all IV models 

f_control_ineq <- c('F-Stat (1st Stage)', '',
               round(sapply(control_list_ineq, get_f_stat), 2))

## Add baseline model to the list 

control_list_ineq <- list(m1_baseline_ineq, m1_iv_ineq, 
                     m2_iv_ineq, m3_iv_ineq)

## w/ controls

stargazer(control_list_ineq, keep = 'fair_dic', 
          covariate.labels = 'Equitable Inheritance',
          keep.stat = 'n', 
          add.lines = list(c('Controls', rep('Yes', 4)), 
                           c('State FE', 'Yes','Yes', 'No', 'Yes'),
                           f_control_ineq),
          style = 'ajps',
          title = 'IV Results, w\ controls (Log mean income)',
          header = F, star.cutoffs = c(NA,NA,NA),
          font.size = 'footnotesize',
          column.sep.width = '2pt')

#### TABLE 5 : Ancient nobility presence in Rotary clubs #### 

## Elevation instrument

m1_iv_c <- ivreg(rot_scaled_uradel ~ fair_dic + lon + lat+ 
                   childlabor_mean_1898 + factor(law_cat) +
                   support_expenses_total_capita + factor(state) | 
                   elev_mean + lon + lat+ 
                   childlabor_mean_1898 + factor(law_cat) +
                   support_expenses_total_capita+ factor(state), 
                 data=df_orig[df_orig$rot_nearest_neighbor == 0, ])

## Distance to river instrument

m2_iv_c <- ivreg(rot_scaled_uradel ~ fair_dic + lon + lat+ 
                   childlabor_mean_1898 + factor(law_cat) +
                   support_expenses_total_capita  | 
                   river_dist_min + lon + lat+ 
                   childlabor_mean_1898 + factor(law_cat) +
                   support_expenses_total_capita, 
                 data=df_orig[df_orig$rot_nearest_neighbor == 0, ])

## Roman rule instrument

m3_iv_c <- ivreg(rot_scaled_uradel ~ fair_dic + lon + lat+ 
                   childlabor_mean_1898 + factor(law_cat) +
                   support_expenses_total_capita+ factor(state)  | 
                   roman + lon + lat+ 
                   childlabor_mean_1898 + factor(law_cat) +
                   support_expenses_total_capita + factor(state), 
                 data=df_orig[df_orig$rot_nearest_neighbor == 0, ])

## Make a list of models 

control_list <- list(m1_iv_c, m2_iv_c, m3_iv_c)

## Obtain the F-stats

f_control <- c('F-Stat (1st Stage)',
               round(sapply(control_list, get_f_stat), 2))

## Output via stargazer 

stargazer(control_list, keep = 'fair_dic', 
          covariate.labels = 'Equitable Inheritance',
          keep.stat = 'n', add.lines = list(c('Controls', rep('Yes', 3)), 
                                            c('State FE', 'Yes','No', 'Yes'),
                                            f_control),
          style = 'ajps',
          header = F, star.cutoffs = c(NA,NA,NA),
          column.sep.width = '2pt')

#### TABLE A9 : Female presence in rotary clubs ####

# df_orig$rot_scaled_fem <- as.numeric(scale(df_orig$rot_fem_share))

## Elevation instrument

m1_iv_female_rotary <- ivreg(rot_scaled_fem ~ fair_dic  + lon + lat +
                   childlabor_mean_1898 + factor(law_cat) +
                   support_expenses_total_capita + factor(state) | 
                   elev_mean + lon + lat +
                   childlabor_mean_1898 + factor(law_cat) +
                   support_expenses_total_capita + factor(state), 
                 data=df_orig[df_orig$rot_nearest_neighbor == 0, ])

## Distance to rivers 

m2_iv_female_rotary <- ivreg(rot_scaled_fem ~ fair_dic + lon + lat +
                   childlabor_mean_1898 + factor(law_cat) +
                   support_expenses_total_capita  | 
                   river_dist_min + lon + lat +
                   childlabor_mean_1898 + factor(law_cat) +
                   support_expenses_total_capita, 
                   data=df_orig[df_orig$rot_nearest_neighbor == 0, ])

## Roman rule 

m3_iv_female_rotary <- ivreg(rot_scaled_fem ~ fair_dic + lon + lat +
                   childlabor_mean_1898 + factor(law_cat) +
                   support_expenses_total_capita + factor(state)  | 
                   roman + lon + lat +
                   childlabor_mean_1898 + factor(law_cat) +
                   support_expenses_total_capita + factor(state), 
                   data=df_orig[df_orig$rot_nearest_neighbor == 0, ])

## Make list of models

control_list <- list(m1_iv_female_rotary, 
                     m2_iv_female_rotary, 
                     m3_iv_female_rotary)

## Obtain F-stats

f_control <- c('F-Stat (1st Stage)',
               round(sapply(control_list, get_f_stat), 2))

## Output via stargazer

stargazer(control_list, keep = 'fair_dic', 
          covariate.labels = 'Equitable Inheritance',
          keep.stat = 'n', add.lines = list(c('Controls', rep('Yes', 3)), 
                                            c('State FE', 'Yes','No', 'Yes'),
                                            f_control),
          header = F, star.cutoffs = c(NA,NA,NA),
          column.sep.width = '2pt', style = 'ajps')

#### TABLE A11 : First stage regressions ####

## Data for which female share information is available

data_fs <- df_orig[!is.na(df_orig$gem_women_share), ]

## List of instruments

iv_list <- c('roman', 'peas_total', 'elev_mean', 
             'river_dist_min', 'dist_wttb', 
             'law_cc_combined')

## Select data such that there are no missings

data_fs <- data_fs[complete.cases(data_fs[, iv_list[-4]]), ]

iv_list <- c(iv_list, paste0(c('roman', 'elev_mean'), collapse = ' + '))

# List of control variables

control_list <- c(rep('lon + lat + state', 3),
                  rep('lon + lat', 1),
                  rep('lon + lat + state', 3))

## Scale three variables

iv_to_scale <- c('elev_mean', 'dist_wttb', 'river_dist_min')

for (j in iv_to_scale) {
  data_fs[, j] <- as.numeric(scale(data_fs[, j]))
}


## Make list of all regressions

fs_list <- pblapply(iv_list, function(v) {
  f <- as.formula(paste0('fair_dic ~ ', v))
  m <- lm(f, data = data_fs)
  m
})

## Add controls to the regressions

fs_list_control <- lapply(1:length(iv_list), function(i) {
  f <- as.formula(paste0(". ~ . +", control_list[i]))
  m <- stats::update(fs_list[[i]], f)
  m
})

## Obtain F-stats

fs_list_control_fstat <- sapply(1:length(iv_list), function(i) {
  f <- as.formula(paste0('fair_dic ~ ', iv_list[i], ' + ',
                         control_list[i]))
  f0 <- as.formula(paste0('fair_dic ~ ', control_list[i]))
  
  if (i != 7) {
    vars_temp <- c('fair_dic', iv_list[i], 'lon', 'lat', 'state')
  } else {
    vars_temp <- c('fair_dic', iv_list[c(1,2, 3)], 'lon', 'lat', 'state')
  }
  
  cc_temp <- data_fs[complete.cases(data_fs[, vars_temp]), ]
  
  ## Estimate models to obtain the F Stata
  
  m <- lm(f, data =cc_temp)
  m0 <- lm(f0, data =cc_temp)
  fstat <- waldtest(m, m0)
  fstat <- fstat$F[2]
  fstat
})

## output as latex:

stargazer(fs_list_control[-7], keep = iv_list[1:6], 
          keep.stat = c('n', 'rsq'), 
          add.lines = list(c('Controls', rep('Yes', 6)), 
                           c('State FE', rep('Yes', 3), 'No', 'Yes', 'Yes'),
                           c('F-Stat', round(fs_list_control_fstat, 2))),
          style = 'ajps',
          title = 'First-stage IV results, w/ controls',
          header = F, 
          star.cutoffs = c(NA,NA,NA),
          font.size = 'footnotesize',
          column.sep.width = '2pt',
          model.numbers = F,
          dep.var.labels = 'Equitable Inheritance')

