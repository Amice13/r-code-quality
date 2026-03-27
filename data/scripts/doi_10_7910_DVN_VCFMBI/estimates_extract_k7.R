#########################################################
#Author: M. Kenwick
#Date: Mar 5, 2019
#Purpose: extract in-sample predictions for models with leader
#military experience left out. This first segment of this code
#re-runs the s/d7 script to format data. Results reported in 
#Appendix 6. 
#########################################################
rm(list=ls())
library(dplyr)
library(foreign)
library(data.table)
set.seed(1989)
setwd('~/Dropbox/ctrl_rep_final')
data <- read.dta("irt_modeling/base_data.dta")
load("irt_modeling/loio/output/static_output_k7.RData")
output_s<-output
load("irt_modeling/loio/output/drift_output_k7.RData")


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


####load data, sort by factorized casename
data$gwf_casename<-as.factor(data$gwf_casename)
data = dplyr::arrange(data, gwf_casename, year)
data$id<-as.numeric(cbind(seq(1,nrow(data),1)))

drift_ind<-ifelse(
  (is.na(data$cgv_mil) | data$cgv_mil==0 ) &
    (is.na(data$dpi_millead) | data$dpi_millead==0) &
    (is.na(data$gwf_mil) | data$gwf_mil==0) &
    (is.na(data$ard_mil_any) | data$ard_mil_any==0) &
    (is.na(data$svolik_military_scale) | data$svolik_military_scale<=2) &
    (is.na(data$weeks_milindex_simple) | data$weeks_milindex_simple==1),1,0
) 
drift_ind_m<-ifelse(drift_ind==0,1,0) 

data$prev <- sapply(1:nrow(data), prev.year, data) 
data$prev_id <- sapply(1:nrow(data), find.year,data)
data$prev_id[data$gwf_new==1]<-0

prev <- data$prev 
prev_id <- data$prev_id
existing<-data$existing

panel = data
panel$drift_ind<-drift_ind
panel$id<-NULL
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
drift_ind<-panel$drift_ind
drift_yrs<-panel$drift_yrs
n_drift_years<-length(unique(drift_yrs))-1

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
drift_yrs_m[drift_yrs_m>40]<-40
n_drift_years_m<-length(unique(drift_yrs_m))-1

k2_names<-c("id","gwf_casename","ccode","year",
            'svolik_milentry',
            "cgv_mil",
            "dpi_millead",
            "gwf_mil",
            "gwf_mil_prior",
            "ard_mil_any")
k3_names<-c("id","gwf_casename","ccode","year",
            "mpg_cat")
k4_names<-c("id","gwf_casename","ccode","year",
            "svolik_military_scale")
k5_names<-c("id","gwf_casename","ccode","year",
            "weeks_milindex_simple")
data_k2 <- data[k2_names]
data_k2[5:10]<-data_k2[5:10]+1
data_k3 <- data[k3_names]
data_k4 <- data[k4_names]
data_k5 <- data[k5_names]
n <- nrow(data)
#number of items in each category 
k2 <- ncol(data_k2)-4 
k3 <- ncol(data_k3)-4
k4 <- ncol(data_k4)-4
k5 <- ncol(data_k5)-4

#Vectorization
y_k2 <- c()
item_k2 <- c()
id_k2 <- c()
year_k2 <- c()
for(ii in (5):(k2+4)){
  y_k2 <- append(y_k2, data_k2[,ii])
  id_k2 <- append(id_k2, data_k2$id)
  year_k2 <- append(year_k2, data_k2$year)
  item_k2 <- append(item_k2, rep(ii-4, length(data_k2[,ii])))
}
id_k2 <- id_k2[!is.na(y_k2)] 
year_k2 <- year_k2[!is.na(y_k2)] 
item_k2 <- item_k2[!is.na(y_k2)] 
y_k2 <- y_k2[!is.na(y_k2)] 

y_k3 <- c()
item_k3 <- c()
id_k3 <- c()
year_k3 <- c()
for(ii in (5):(k3+4)){
  y_k3 <- append(y_k3, data_k3[,ii])
  id_k3 <- append(id_k3, data_k3$id)
  year_k3 <- append(year_k3, data_k3$year)
  item_k3 <- append(item_k3, rep(ii-4, length(data_k3[,ii])))
}
id_k3 <- id_k3[!is.na(y_k3)] 
year_k3 <- year_k3[!is.na(y_k3)] 
item_k3 <- item_k3[!is.na(y_k3)] 
y_k3 <- y_k3[!is.na(y_k3)] 

y_k4 <- c()
item_k4 <- c()
id_k4 <- c()
year_k4 <- c()
for(ii in (5):(k4+4)){
  y_k4 <- append(y_k4, data_k4[,ii])
  id_k4 <- append(id_k4, data_k4$id)
  year_k4 <- append(year_k4, data_k4$year)
  item_k4 <- append(item_k4, rep(ii-4, length(data_k4[,ii])))
}
id_k4 <- id_k4[!is.na(y_k4)] 
year_k4 <- year_k4[!is.na(y_k4)] 
item_k4 <- item_k4[!is.na(y_k4)] 
y_k4 <- y_k4[!is.na(y_k4)] 

y_k5 <- c()
item_k5 <- c()
id_k5 <- c()
year_k5 <- c()
for(ii in (5):(k5+4)){
  y_k5 <- append(y_k5, data_k5[,ii])
  id_k5 <- append(id_k5, data_k5$id)
  year_k5 <- append(year_k5, data_k5$year)
  item_k5 <- append(item_k5, rep(ii-4, length(data_k5[,ii])))
}
id_k5 <- id_k5[!is.na(y_k5)] 
year_k5 <- year_k5[!is.na(y_k5)] 
item_k5 <- item_k5[!is.na(y_k5)] 
y_k5 <- y_k5[!is.na(y_k5)] 

n_k2 <- length(y_k2)
n_k3 <- length(y_k3)
n_k4 <- length(y_k4)
n_k5 <- length(y_k5)

f = function(x,y) {
  z<-ifelse(x==y,1,0)
  return(z)
}

s_postpred<-matrix(NA,nrow=10,ncol=8000)
for (ii in 1:6){
  y_hat_k2_1<-output_s$y_hat_k2[,item_k2==ii]
  y_k2_1<-y_k2[item_k2==ii]
  test = t(apply(y_hat_k2_1,1,FUN=f,y=y_k2_1))
  test2 = apply(test,1,mean)
  s_postpred[ii,]<-test2
}
#k3 item
y_hat_k3_1<-output_s$y_hat_k3[,item_k3==1]
y_k3_1<-y_k3[item_k3==1]
test = t(apply(y_hat_k3_1,1,FUN=f,y=y_k3_1))
test2 = apply(test,1,mean)
s_postpred[7,]<-test2

y_hat_k3_1<-output_s$y_hat_k3[,item_k3==2]
y_k3_1<-y_k3[item_k3==2]
test = t(apply(y_hat_k3_1,1,FUN=f,y=y_k3_1))
test2 = apply(test,1,mean)
s_postpred[8,]<-test2

#k4 item
y_hat_k4_1<-output_s$y_hat_k4
y_k4_1<-y_k4
test = t(apply(y_hat_k4_1,1,FUN=f,y=y_k4_1))
test2 = apply(test,1,mean)
s_postpred[9,]<-test2

#k5 item
y_hat_k5_1<-output_s$y_hat_k5
y_k5_1<-y_k5
test = t(apply(y_hat_k5_1,1,FUN=f,y=y_k5_1))
test2 = apply(test,1,mean)
s_postpred[10,]<-test2

d_postpred<-matrix(NA,nrow=10,ncol=8000)
for (ii in 1:6){
  y_hat_k2_1<-output$y_hat_k2[,item_k2==ii]
  y_k2_1<-y_k2[item_k2==ii]
  test = t(apply(y_hat_k2_1,1,FUN=f,y=y_k2_1))
  test2 = apply(test,1,mean)
  d_postpred[ii,]<-test2
}

#k3 item
y_hat_k3_1<-output$y_hat_k3[,item_k3==1]
y_k3_1<-y_k3[item_k3==1]
test = t(apply(y_hat_k3_1,1,FUN=f,y=y_k3_1))
test2 = apply(test,1,mean)
d_postpred[7,]<-test2
y_hat_k3_1<-output$y_hat_k3[,item_k3==2]
y_k3_1<-y_k3[item_k3==2]
test = t(apply(y_hat_k3_1,1,FUN=f,y=y_k3_1))
test2 = apply(test,1,mean)
d_postpred[8,]<-test2

#k4 item
y_hat_k4_1<-output$y_hat_k4
y_k4_1<-y_k4
test = t(apply(y_hat_k4_1,1,FUN=f,y=y_k4_1))
test2 = apply(test,1,mean)
d_postpred[9,]<-test2

#k5 item
y_hat_k5_1<-output$y_hat_k5
y_k5_1<-y_k5
test = t(apply(y_hat_k5_1,1,FUN=f,y=y_k5_1))
test2 = apply(test,1,mean)
d_postpred[10,]<-test2
#eliminate missing caused by dropping leader military experience
s_postpred<-s_postpred[c(1:7,9:10),]
d_postpred<-d_postpred[c(1:7,9:10),]
save(s_postpred, d_postpred,file="pcp_output_k7.RData")

