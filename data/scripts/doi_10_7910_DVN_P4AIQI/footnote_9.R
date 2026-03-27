# ----------------------------------------------------------------------------
# INFO  
# ----------------------------------------------------------------------------

# Article: The Electoral Politics of Immigration and Crime
# Author(s): Jeyhun Alizade
# Code to replicate result in Footnote 9 (pp. 14-15)

# ----------------------------------------------------------------------------
# SETUP
# ----------------------------------------------------------------------------

# clear environment
rm(list=ls())

# set working directory (change working directory to where the replication folder is saved on your computer)
setwd("/Users/jeyhunalizade/Dropbox/immigration crime project/ajps_replication/replication_files/")

# load data set
dat = readRDS("data/ess_cmp_merged.rds")


# ----------------------------------------------------------------------------
# MODELS
# ----------------------------------------------------------------------------

### regressions of party positions from CMP on voter attitudes based on ESS, standardizing both the dependent and independent variable

## Without country fixed effects

# crime
crime_mod <- lm(scale(crime)~scale(immi_crime), dat)
print(round(summary(crime_mod)$coefficients[2,1], 2)) # correlation coefficient

# multiculturalism
cult_mod <- lm(scale(multiculturalism)~scale(immi_cult), dat)
print(round(summary(cult_mod)$coefficients[2,1], 2)) # correlation coefficient

## With country fixed effects

# crime
crime_fe_mod <- lm(scale(crime)~scale(immi_crime)+cntry, dat)
print(round(summary(crime_fe_mod)$coefficients[2,1], 2)) # correlation coefficient

# multiculturalism
cult_fe_mod <- lm(scale(multiculturalism)~scale(immi_cult)+cntry, dat)
print(round(summary(cult_fe_mod)$coefficients[2,1], 2)) # correlation coefficient
