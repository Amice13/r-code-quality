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
#########################################################
#Begin Data Prep
#########################################################
#########################################################

#### Load Libraries and WD ####
library(foreign)
library(rstan) 
library(dplyr)
library(data.table)
set.seed(1989)

#setwd('/Users/kenwick/Dropbox/ctrl_isq_final/replication/irt_modeling')

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
existing<-data$existing

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
#Few cases after 60, so capping drift years there
drift_yrs_uncapped<-drift_yrs
drift_yrs[drift_yrs>65]<-65
n_drift_years<-length(unique(drift_yrs))-1

#########################################################
#########################################################
#End Data Prep, begin Model
#########################################################
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

}


parameters {
vector[n] x; //latent var

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
}


model {
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

x ~ normal(0,1); 

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
           seed=10101,chains=4, cores=4,
           control=list(adapt_delta=.95)) 
save(fit, file="static_raw_k7.RData")
output <- extract(fit, permuted = TRUE)
save(output, file="static_output_k7.RData")
options(max.print=5.5E5)
sink("static_printed_k7.txt")
print(fit)
sink()
rm(list=ls())

