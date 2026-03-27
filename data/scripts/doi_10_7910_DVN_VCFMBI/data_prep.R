#########################################################
#Author: M. Kenwick
#Date: Sep 20, 2019
#Purpose: Prep c.ctrl. data for IRT analyses
#########################################################
rm(list=ls())
#### Functions to store to create yearly flags ####
prev.year <- function(ii, d){
  state <- d[ii, 1] #pulls state ID data, where gwf_casename is col 1 
  prev <- d[ii, 3] - 1 #sets previous year to year (col 3) - 1
  y.all <- d$year[d$gwf_casename==state] #pulls full set of years for a given country as a vector
  (prev %in% y.all)*1 
}

find.year <- function(ii, d){
  check <- d$prev[ii] #makes sure there is a previous year
  year <- d$year[ii] - 1 #previous year
  state <- d$gwf_casename[ii] #state is gwf_casename
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
setwd('~/Dropbox/ctrl_rep_final/irt_modeling')
####load data, sort by factorized casename
data <- read.dta("base_data.dta")
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

#same as above, but for militray regimes (for auxillary analyses)
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
#Re-name drift_yrs to clarify that this is the number of years of drift that have occured
#while under observation in our data
observed_drift_yrs<-drift_yrs
#Setting drift year indicator to correspond to regime age for for regimes censored by more than five years
table(drift_yrs) 
drift_yrs[data$gwf_casename=="Afghanistan 29-73" & data$year!=1946]<-data$gwf_duration[data$gwf_casename=="Afghanistan 29-73" & data$year!=1946]
drift_yrs[data$gwf_casename=="Australia 01-NA" & data$year!=1946]<-data$gwf_duration[data$gwf_casename=="Australia 01-NA" & data$year!=1946]
drift_yrs[data$gwf_casename=="Belgium 20-NA" & data$year!=1946]<-data$gwf_duration[data$gwf_casename=="Belgium 20-NA" & data$year!=1946]
drift_yrs[data$gwf_casename=="Canada 21-NA" & data$year!=1946]<-data$gwf_duration[data$gwf_casename=="Canada 21-NA" & data$year!=1946]
drift_yrs[data$gwf_casename=="Chile 32-73" & data$year!=1946]<-data$gwf_duration[data$gwf_casename=="Chile 32-73" & data$year!=1946]
drift_yrs[data$gwf_casename=="Colombia 34-49" & data$year!=1946]<-data$gwf_duration[data$gwf_casename=="Colombia 34-49" & data$year!=1946]
drift_yrs[data$gwf_casename=="Costa Rica 19-48" & data$year!=1946]<-data$gwf_duration[data$gwf_casename=="Costa Rica 19-48" & data$year!=1946]
drift_yrs[data$gwf_casename=="Denmark 01-NA" & data$year!=1946]<-data$gwf_duration[data$gwf_casename=="Denmark 01-NA" & data$year!=1946]
drift_yrs[data$gwf_casename=="Egypt 22-52" & data$year!=1946]<-data$gwf_duration[data$gwf_casename=="Egypt 22-52" & data$year!=1946]
drift_yrs[data$gwf_casename=="Ethiopia 1889-1974" & data$year!=1946]<-data$gwf_duration[data$gwf_casename=="Ethiopia 1889-1974" & data$year!=1946]
drift_yrs[data$gwf_casename=="France 1875-NA" & data$year!=1946]<-data$gwf_duration[data$gwf_casename=="France 1875-NA" & data$year!=1946]
drift_yrs[data$gwf_casename=="Iran 25-79" & data$year!=1946]<-data$gwf_duration[data$gwf_casename=="Iran 25-79" & data$year!=1946]
drift_yrs[data$gwf_casename=="Iraq 32-58" & data$year!=1946]<-data$gwf_duration[data$gwf_casename=="Iraq 32-58" & data$year!=1946]
drift_yrs[data$gwf_casename=="Ireland 21-NA" & data$year!=1946]<-data$gwf_duration[data$gwf_casename=="Ireland 21-NA" & data$year!=1946]
drift_yrs[data$gwf_casename=="Luxemburg 1870-NA" & data$year!=1946]<-data$gwf_duration[data$gwf_casename=="Luxemburg 1870-NA" & data$year!=1946]
drift_yrs[data$gwf_casename=="Mongolia 21-93" & data$year!=1946]<-data$gwf_duration[data$gwf_casename=="Mongolia 21-93" & data$year!=1946]
drift_yrs[data$gwf_casename=="Nepal 1846-1951" & data$year!=1946]<-data$gwf_duration[data$gwf_casename=="Nepal 1846-1951" & data$year!=1946]
drift_yrs[data$gwf_casename=="Netherlands 1870-NA" & data$year!=1946]<-data$gwf_duration[data$gwf_casename=="Netherlands 1870-NA" & data$year!=1946]
drift_yrs[data$gwf_casename=="New Zealand 07-NA" & data$year!=1946]<-data$gwf_duration[data$gwf_casename=="New Zealand 07-NA" & data$year!=1946]
drift_yrs[data$gwf_casename=="Norway 1885-NA" & data$year!=1946]<-data$gwf_duration[data$gwf_casename=="Norway 1885-NA" & data$year!=1946]
drift_yrs[data$gwf_casename=="Oman 1741-NA" & data$year!=1946]<-data$gwf_duration[data$gwf_casename=="Oman 1741-NA" & data$year!=1946]
drift_yrs[data$gwf_casename=="Saudi Arabia 27-NA" & data$year!=1946]<-data$gwf_duration[data$gwf_casename=="Saudi Arabia 27-NA" & data$year!=1946]
drift_yrs[data$gwf_casename=="South Africa 10-94" & data$year!=1946]<-data$gwf_duration[data$gwf_casename=="South Africa 10-94" & data$year!=1946]
drift_yrs[data$gwf_casename=="Soviet Union 17-91" & data$year!=1946]<-data$gwf_duration[data$gwf_casename=="Soviet Union 17-91" & data$year!=1946]
drift_yrs[data$gwf_casename=="Sweden 19-NA" & data$year!=1946]<-data$gwf_duration[data$gwf_casename=="Sweden 19-NA" & data$year!=1946]
drift_yrs[data$gwf_casename=="Switzerland 1870-NA" & data$year!=1946]<-data$gwf_duration[data$gwf_casename=="Switzerland 1870-NA" & data$year!=1946]
drift_yrs[data$gwf_casename=="UK 11-NA" & data$year!=1946]<-data$gwf_duration[data$gwf_casename=="UK 11-NA" & data$year!=1946]
drift_yrs[data$gwf_casename=="USA 1871-NA" & data$year!=1946]<-data$gwf_duration[data$gwf_casename=="USA 1871-NA" & data$year!=1946]
drift_yrs[data$gwf_casename=="Yemen 18-62" & data$year!=1946]<-data$gwf_duration[data$gwf_casename=="Yemen 18-62" & data$year!=1946]
table(drift_yrs)
#Capping drift years at 65 since few regimes survive past that point
drift_yrs_uncapped<-drift_yrs
drift_yrs[drift_yrs>65]<-65
n_drift_years<-length(unique(drift_yrs))-1
rm(panel,PANEL)
save(data,drift_ind,drift_ind_m,drift_yrs,drift_yrs_m,drift_yrs_uncapped,observed_drift_yrs,
     n_drift_years,n_drift_years_m,prev,prev_id,file="data_prepped.RData")

