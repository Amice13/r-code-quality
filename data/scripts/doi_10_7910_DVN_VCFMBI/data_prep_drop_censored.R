#########################################################
#Author: M. Kenwick
#Date: Mar 5, 2019
#Purpose: Prep c.ctrl. data for IRT analyses in Appendix 15, 
#dropping regimes with substantial left-censoring. 
#########################################################
rm(list=ls())
#### Functions to store to create yearly flags ####
prev.year <- function(ii, d){
  state <- d[ii, 1] #pulls state ID data, where ccode is col 2 
  prev <- d[ii, 3] - 1 #sets previous year to year (col 3) - 1
  y.all <- d$year[d$gwf_casename==state] #pulls full set of years for a given country as a vector
  (prev %in% y.all)*1 
}

find.year <- function(ii, d){
  check <- d$prev[ii] #makes sure there is a previous year
  year <- d$year[ii] - 1 #previous year
  state <- d$gwf_casename[ii] #state is ccode
  if(check==1){
    ret <- which(d$gwf_casename == state & d$year==year) #returns row id of obs meeting pre.year criteria 
  } else {
    ret <- 0
  }
  ret
}
#########################################################
#### Load Libraries and WD ####
library(foreign)
library(rstan) 
library(dplyr)
library(data.table)
set.seed(1989)
setwd('~/Dropbox/ctrl_rep_final')
####load data, sort by factorized casename
data <- read.dta("irt_modeling/base_data.dta")
#Dropping substantially left-censored regimes
censored_regs<-data$gwf_casename[data$year==1946 & data$gwf_duration>10]
data<-data[!(data$gwf_casename %in% censored_regs),]
data$gwf_casename<-as.factor(data$gwf_casename)
data = dplyr::arrange(data, gwf_casename, year)
#ID indicator
data$id<-as.numeric(cbind(seq(1,nrow(data),1)))
#Generate an indicator for civilianized (drif_ind) and militarized regimes (drift_ind_m)
drift_ind<-ifelse(
  (is.na(data$cgv_mil) | data$cgv_mil==0 ) &
    (is.na(data$dpi_millead) | data$dpi_millead==0) &
    (is.na(data$gwf_mil) | data$gwf_mil==0) &
    (is.na(data$ard_mil_any) | data$ard_mil_any==0) &
    (is.na(data$svolik_military_scale) | data$svolik_military_scale<=2) &
    (is.na(data$weeks_milindex_simple) | data$weeks_milindex_simple==1),1,0
) 
drift_ind_m<-ifelse(drift_ind==0,1,0) 
#Indicators for start or fail of a regime. Prev_id=0 for new regimes
data$prev <- sapply(1:nrow(data), prev.year, data) 
data$prev_id <- sapply(1:nrow(data), find.year,data)
#Set the prev_indicator to 0 for new regimes
data$prev_id[data$gwf_new==1]<-0
#Pull out vectors to pass to stan later
prev <- data$prev 
prev_id <- data$prev_id
#Count the number of years a country has been civilianized in data for drift indicator
#create a stand-in dataset (panel) to preserve original sorting scheme
panel = data
panel$drift_ind<-drift_ind
panel$id<-NULL
#don't start counting until the first full year of civilianization
panel = panel %>%
  group_by(gwf_casename) %>%
  mutate(drift_ind = lag(drift_ind, order_by=gwf_casename))
panel$drift_ind[is.na(panel$drift_ind)]<-0
#This mini-function will count the number of drift years
PANEL = data.table(panel)
PANEL[, c("drift_yrs") := {
  rr <- sequence(rle(drift_ind)$lengths)
  list(rr * drift_ind)
}, by=gwf_casename]
panel = as.data.frame(PANEL)
panel = dplyr::arrange(panel, gwf_casename, year)
#store vectorized versions
drift_ind<-panel$drift_ind
drift_yrs<-panel$drift_yrs
n_drift_years<-length(unique(drift_yrs))-1

#same as above, but for militray regimes
panel = data
panel$drift_ind_m<-drift_ind_m
panel$id<-NULL
panel = panel %>%
  group_by(gwf_casename) %>%
  mutate(drift_ind_m = lag(drift_ind_m, order_by=gwf_casename))
panel$drift_ind_m[is.na(panel$drift_ind_m)]<-0
PANEL = data.table(panel)
PANEL[, c("drift_yrs_m") := {
  rr <- sequence(rle(drift_ind_m)$lengths)
  list(rr * drift_ind_m)
}, by=gwf_casename]
panel = as.data.frame(PANEL)
panel = dplyr::arrange(panel, gwf_casename, year)
drift_ind_m<-panel$drift_ind_m
drift_yrs_m<-panel$drift_yrs_m
#There are very few countries past 20 years, so capping military drift years there
drift_yrs_m[drift_yrs_m>40]<-40
n_drift_years_m<-length(unique(drift_yrs_m))-1
n_drift_years<-length(unique(drift_yrs))-1
drift_yrs_uncapped<-drift_yrs
drift_yrs[drift_yrs>50]<-50
n_drift_years<-max(drift_yrs)

rm(panel,PANEL)
save(data,drift_ind,drift_ind_m,drift_yrs,drift_yrs_m,drift_yrs_uncapped,
     n_drift_years,n_drift_years_m,prev,prev_id,file="appendix/irt_models/data_prepped_drop_censored.RData")

