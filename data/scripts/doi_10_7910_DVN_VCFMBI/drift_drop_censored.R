#########################################################
#Author: M. Kenwick
#Date: Mar 5, 2019
#Purpose: Runs IRT model for Appendix 15, 
#dropping regimes with substantial left-censoring. 
#########################################################
#### Load Libraries ####
rm(list=ls())
library(foreign) # pre-installed
library(rstan) # must be installed 
library(dplyr)
set.seed(1989)
#setwd('/Users/kenwick/Dropbox/ctrl_isq/analysis_march19_final')
load('data_prepped_drop_censored.RData')
#########################################################
#Split data into types of variables
k2_names<-c("id","gwf_casename","ccode","year",
            'svolik_milentry',
            "cgv_mil",
            "dpi_millead",
            "gwf_mil",
            "gwf_mil_prior",
            "ard_mil_any")
k3_names<-c("id","gwf_casename","ccode","year",
            "hs_lead_scale","mpg_cat")
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


#########################################################
stan.data <- list(n=n, 
                  n_k2=n_k2,
                  k2=k2,
                  y_k2=y_k2 ,
                  item_k2=item_k2, 
                  id_k2=id_k2,
                  n_k3=n_k3,
                  k3=k3,
                  y_k3=y_k3 ,
                  item_k3=item_k3,
                  n_k4=n_k4,
                  k4=k4,
                  y_k4=y_k4 ,
                  item_k4=item_k4,
                  n_k5=n_k5,
                  k5=k5,
                  y_k5=y_k5 ,
                  item_k5=item_k5,
                  prev_id=prev_id,
                  drift_yrs=drift_yrs,
                  n_drift_years=n_drift_years
)

#### Build Model ####

model <- "

data {
int<lower=0> n; //number of subjects

int<lower=0> n_k2; //number of country-item observations w/ 2 cat
int<lower=0> k2; //number of items w/ 2 cat
int<lower=1,upper=2> y_k2[n_k2]; // manifest variables w/ 2 cat
int<lower=0> item_k2[n_k2]; //index of which item among 2 cat vars
int<lower=0> id_k2[n_k2]; //index of which subject for 2 cat

int<lower=0> n_k3; //number of country-item observations w/ 3 cat
int<lower=0> k3; //number of items w/ 3 cat
int<lower=1,upper=3> y_k3[n_k3]; // manifest variables w/ 3 cat
int<lower=0> item_k3[n_k3]; //index of which item among 3 cat vars
int<lower=0> id_k3[n_k3]; //index of which subject for 3 cat

int<lower=0> n_k4; //number of country-item observations w/ 4 cat
int<lower=0> k4; //number of items w/ 4 cat
int<lower=1,upper=4> y_k4[n_k4]; // manifest variables w/ 4 cat
int<lower=0> item_k4[n_k4]; //index of which item among 4 cat vars
int<lower=0> id_k4[n_k4]; //index of which subject for 4 cat

int<lower=0> n_k5; //number of country-item observations w/ 5 cat
int<lower=0> k5; //number of items w/ 5 cat
int<lower=1,upper=5> y_k5[n_k5]; // manifest variables w/ 5 cat
int<lower=0> item_k5[n_k5]; //index of which item among 5 cat vars
int<lower=0> id_k5[n_k5]; //index of which subject for 5 cat
int<lower=0, upper=7953> prev_id[n];
int<lower=0> drift_yrs[n];
int<lower=0> n_drift_years;
}


parameters {
vector[n] theta_raw; //latent var

real<lower=0> sigma;
real<lower=0> drift_sigma;

vector[n_drift_years] drift;

ordered[1] alpha_k2[k2]; //alpha for logit
vector<lower=0>[k2] beta_k2; //beta for logit

vector<lower=0>[k3] beta_k3; //betas for vars w/ 3 cat
ordered[2] c_k3[k3]; //matrix of cutpoints for vars w/ 3 cat

vector<lower=0>[k4] beta_k4; //betas for vars w/ 4 cat
ordered[3] c_k4[k4]; //matrix of cutpoints for vars w/ 4 cat

vector<lower=0>[k5] beta_k5; //betas for vars w/ 5 cat
ordered[4] c_k5[k5]; //matrix of cutpoints for vars w/ 5 cat
}


transformed parameters {

vector[n] x;

for(ii in 1:n){
if(prev_id[ii]==0){
x[ii] = theta_raw[ii];
} else { 
if(drift_yrs[ii]==0){
x[ii] = x[prev_id[ii]] + theta_raw[ii]*sigma ;
} else {x[ii] = x[prev_id[ii]] + theta_raw[ii]*sigma + drift[drift_yrs[ii]];
}
}
}			
}

model {
drift[1]~normal(0,1);
for(ii in 2:n_drift_years){
drift[ii]~normal(drift[ii-1],drift_sigma);
}
drift_sigma~cauchy(0, 2.5);

for(j in 1:k2){
alpha_k2[j] ~ normal(0,10);
}

for(j in 1:k3){
c_k3[j,1] ~ normal(0,10); //priors for 3cat 
c_k3[j,2] ~ normal(0,10);
}

for(j in 1:k4){
c_k4[j,1] ~ normal(0,10); 
c_k4[j,2] ~ normal(0,10); 
c_k4[j,3] ~ normal(0,10); 
}

for(j in 1:k5){
c_k5[j,1] ~ normal(0,10); 
c_k5[j,2] ~ normal(0,10); 
c_k5[j,3] ~ normal(0,10); 
c_k5[j,4] ~ normal(0,10); 
}

beta_k2 ~ gamma(4,2);
beta_k3 ~ gamma(4,2);
beta_k4 ~ gamma(4,2);
beta_k5 ~ gamma(4,2);

sigma ~ cauchy(0, 2.5);
theta_raw ~ normal(0,1);

for(ii in 1:n_k2){
y_k2[ii] ~ ordered_logistic(beta_k2[item_k2[ii]] * -x[id_k2[ii]],alpha_k2[item_k2[ii]] ); 
}

for(ii in 1:n_k3){
y_k3[ii] ~ ordered_logistic(beta_k3[item_k3[ii]] * -x[id_k3[ii]], 
c_k3[item_k3[ii]]);
}

for(ii in 1:n_k4){
y_k4[ii] ~ ordered_logistic(beta_k4[item_k4[ii]] * -x[id_k4[ii]], 
c_k4[item_k4[ii]]);
}  

for(ii in 1:n_k5){
y_k5[ii] ~ ordered_logistic(beta_k5[item_k5[ii]] * -x[id_k5[ii]], 
c_k5[item_k5[ii]]);
}  
}

generated quantities {
vector[n_k2] y_hat_k2;
vector[n_k3] y_hat_k3;
vector[n_k4] y_hat_k4;
vector[n_k5] y_hat_k5;

for(ii in 1:n_k2){
y_hat_k2[ii] = ordered_logistic_rng(beta_k2[item_k2[ii]] * -x[id_k2[ii]], 
alpha_k2[item_k2[ii]]);
}
for(ii in 1:n_k3){
y_hat_k3[ii] = ordered_logistic_rng(beta_k3[item_k3[ii]] * -x[id_k3[ii]], 
c_k3[item_k3[ii]]);
}
for(ii in 1:n_k4){
y_hat_k4[ii] = ordered_logistic_rng(beta_k4[item_k4[ii]] * -x[id_k4[ii]], 
c_k4[item_k4[ii]]);
}
for(ii in 1:n_k5){
y_hat_k5[ii] = ordered_logistic_rng(beta_k5[item_k5[ii]] * -x[id_k5[ii]], 
c_k5[item_k5[ii]]);
}	
}
"

fit<- stan(model_code=model, data=stan.data, iter=3000, warmup=1000, 
           seed=10101,chains=4, cores=4,pars=c("theta_raw"),include=FALSE,
           control=list(adapt_delta=.95)) 
output <- extract(fit, permuted = TRUE)
save(output, file="drift_output_drop_censored.RData")
#options(max.print=5.5E5)
#sink("drift_printed_drop_censored.txt")
#print(fit)
#sink()
rm(list=ls())

