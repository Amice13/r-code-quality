# This R-file is generating the table SI-5

# Regression Table - IHE ~ IVsm

##### Model setting: DV = IHE, IV = IVsm, same set of coveriates, for 3 period
##### SEs clusterd on Media Market level, use population (potential) as weights

### Some more packages
library(tidyverse)
library(data.table)
library(haven)
library(estimatr)
library(texreg)

### Load data
setwd("~/Dropbox/Israeli Newspapers Study/JOP_REPLICATION/Dataverse submission")
getwd()

data <- read_dta('Data/yeshuv_period_final_Figs.dta')

std_cols = c('IVsm', 'ltd' , 'lpop' , 'matriccert_pcnt_08' , 'europe_pcnt_08' , 'pop_share1829' , 'pop_share3049' , 'pop_share5065'  , 'pop_share66' , 'asia_pcnt_08' , 'religion_pcnt_08')
data_table_format = as.data.table(data)
data_table_format[, c(std_cols) := lapply(.SD, scale), .SDcols = std_cols]

model2_IHE = lm_robust(IHE ~ IVsm + ltd + lpop + matriccert_pcnt_08 + europe_pcnt_08 + pop_share1829 + pop_share3049 + pop_share5065  + pop_share66 + asia_pcnt_08 + religion_pcnt_08,
                       data = data_table_format %>% filter(period == 2),
                       clusters = area_num, se_type = 'stata',
                       weights = potential)
model3_IHE = lm_robust(IHE ~ IVsm + ltd + lpop + matriccert_pcnt_08 + europe_pcnt_08 + pop_share1829 + pop_share3049 + pop_share5065  + pop_share66 + asia_pcnt_08 + religion_pcnt_08,
                       data = data_table_format %>% filter(period == 3),
                       clusters = area_num, se_type = 'stata',
                       weights = potential)
model4_IHE = lm_robust(IHE ~ IVsm + ltd + lpop + matriccert_pcnt_08 + europe_pcnt_08 + pop_share1829 + pop_share3049 + pop_share5065  + pop_share66 + asia_pcnt_08 + religion_pcnt_08,
                       data = data_table_format %>% filter(period == 4),
                       clusters = area_num, se_type = 'stata',
                       weights = potential)

coeficient <- list('IVsm' = 'Yediot readership 2007',
                   'religion_pcnt_08' = 'Share Jews',
                   'asia_pcnt_08' = 'Share Asian descent',
                   'europe_pcnt_08' = 'Share European descent',
                   'matriccert_pcnt_08' = 'High-school matriculation',
                   'lpop' = 'Population (log)',
                   'ltd' = 'Distance to Tel Aviv (log)',
                   'pop_share1829' = 'Pop share ages 18-2',
                   'pop_share3049' = 'Pop share ages 30-49',
                   'pop_share5065' = 'Pop share ages 50-65',
                   'pop_share66' = 'Pop share ages 66+')

### Produce latex code
table = texreg::texreg(
   caption = "IV models: First-stage",
   list(model2_IHE, model3_IHE, model4_IHE),
   single.row = FALSE,
   custom.coef.map = coeficient,
   include.ci=FALSE,
   #custom.gof.rows = list("Controls" = c(rep(c('No','Yes'),3))),
   digits = 3,
   include.adjrs = FALSE,
   stars = c(.001, .01, .05),
   custom.header = list("2009" = 1,"2013" = 2, "2015" = 3),
   custom.model.names = c('(1)','(2)','(3)'),
   include.rs=TRUE,
   center = TRUE,
   include.fstatistic = TRUE,
   include.nobs = TRUE,
   include.rmse = FALSE,
   #caption* = "\footnotesize \textit{Note:}  {$^{***}p<0.01$, $^{**}p<0.05$, $^*p<0.1$}",
   label = "table:Table_SI-5"
 )

write(table, "Tables/Table_SI-5.tex")
