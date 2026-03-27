####
#Author: B. Simmons and M. Kenwick
#Purpose: This script prepares data on border crossings, walls, and police
#         for use in an IRT measurement model and then subsequently runs
#         that model using rstan. This script was run on a high performance
#         computing network with 5 cores and 60GB of RAM.
#Date: Dec 23, 2020
#NOTE: This script run on the Amarel cluster at Rutgers University using 5 cores, 
#64GB RAM, gcc/9.2.0, R version 3.6.3 and rstan (Version 2.18.2, GitRev: 2e1f913d3ca3).
#See the REAADME file for additional details. 
####

#Install packages, as necessary
#install.packages("rstan") #See rstan website for additional details 
#install.packages("dplyr")
#install.packages("countrycode")
#install.packages("rlang")
#install.packages("haven")


rm(list=ls())
library(rstan)
library(dplyr)
library(countrycode)
library(rlang)
library(haven)
options(mc.cores = parallel::detectCores())
rstan_options(auto_write = TRUE)
set.seed(19891)
#Set working directory to top level of replication folder
setwd()

# -------------------------------------------------- #
#Part 1: Load border crossing data, transform to 
#directed dyad format, interpolate values for missing 
#years
# -------------------------------------------------- #


####
#1.1: Loading and prepping border crossing data
####
data<-read.csv('simmons_kenwick_2020_border_crossing_data.csv',stringsAsFactors = F)

#Drop false positives
#Note: we keep cases where one coder identified a positive and another identified a negative
data<-data %>% 
  group_by(borderid) %>% 
  mutate(road_max = max(road_status_1,road_status_2,na.rm=T)) %>% 
  filter(road_max>1) 

#make directed
data$borderid<-as.character(data$borderid)
data<-rbind(data,data)
data<-data %>% arrange(coder,borderid, year)
data$id<-as.numeric(seq(1:nrow(data)))
data$odd<-data$id %% 2

#Generate new indicators and fill in values
data$gate<-NA
data$building<-NA
data$split<-NA
data$multi<-NA
data$country_1<-NA
data$country_2<-NA

data$borderid[data$odd==1]<-paste(data$borderid[data$odd==1],"_1",sep="")
data$gate[data$odd==1]<-data$gate_1[data$odd==1]+1
data$building[data$odd==1]<-data$numbld_1[data$odd==1]+1
data$split[data$odd==1]<-data$split_1[data$odd==1]+1
data$multi[data$odd==1]<-data$multilane_1[data$odd==1]+1
data$country_1[data$odd==1]<-data$country1[data$odd==1]
data$country_2[data$odd==1]<-data$country2[data$odd==1]

data$borderid[data$odd==0]<-paste(data$borderid[data$odd==0],"_2",sep="")
data$gate[data$odd==0]<-data$gate_2[data$odd==0]+1
data$building[data$odd==0]<-data$numbld_2[data$odd==0]+1
data$split[data$odd==0]<-data$split_2[data$odd==0]+1
data$multi[data$odd==0]<-data$multilane_2[data$odd==0]+1
data$country_1[data$odd==0]<-data$country2[data$odd==0]
data$country_2[data$odd==0]<-data$country1[data$odd==0]

data$country1<-data$country_1
data$country2<-data$country_2
data$country_1<-NULL
data$country_2<-NULL

####
#1.2: Interpolating missing values
####
data<-arrange(data,borderid,coder,assignment,year)
for(jj in 41:44){
  for(ii in 2:nrow(data)){
    data[ii,jj]<-ifelse(is.na(data[ii,jj]) & !is.na(data[ii-1,jj]) &
                          data$borderid[ii]==data$borderid[ii-1] & 
                          data$coder[ii]==data$coder[ii-1] & 
                          data$assignment[ii]==data$assignment[ii-1],
                        data[ii-1,jj],data[ii,jj])
  }}
data<-arrange(data,borderid,coder,assignment,-year)
for(jj in 41:44){
  for(ii in 2:nrow(data)){
    data[ii,jj]<-ifelse(is.na(data[ii,jj]) & !is.na(data[ii-1,jj]) &
                          data$borderid[ii]==data$borderid[ii-1] & 
                          data$coder[ii]==data$coder[ii-1] &
                          data$assignment[ii]==data$assignment[ii-1],
                        data[ii-1,jj],data[ii,jj])
  }}
data<-arrange(data,borderid,year)
data<-subset(data,year>1999)
data$state1<-NULL
data$state2<-NULL
data$building[data$building>4]<-4

#Taking the mean value of classifications and rounding up
data<-data %>% 
  group_by(borderid, year) %>%
  mutate(gate = ifelse(max(gate,na.rm=T)==-Inf,NA, mean(gate,na.rm=T))) %>%
  mutate(building = ifelse(max(building,na.rm=T)==-Inf,NA, mean(building,na.rm=T))) %>%
  mutate(multi = ifelse(max(multi,na.rm=T)==-Inf,NA, mean(multi,na.rm=T))) %>%
  mutate(split = ifelse(max(split,na.rm=T)==-Inf,NA, mean(split,na.rm=T))) %>%
  dplyr::select(borderid,year,country1,country2,gate,building,split,multi) %>%
  arrange(borderid, year)
data<-unique(data)

data$gate<-round(data$gate)
data$building<-round(data$building)
data$split<-round(data$split)
data$multi<-round(data$multi)


#Country Codes
data$country1[data$country1=="Central African"]<-"Central African Republic"
data$state1<-countrycode(data$country1,origin='country.name',destination='cown')
data$state1[data$country1=='Gibraltar']<-992
data$state1[data$country1=='Western Sahara']<-993
data$state1[data$country1=='Serbia']<-345
data$state1[data$country1=='Equatorial']<- 411
data$state1[data$country1=='United Arab']<- 696
data$state1[data$country1=='Dominican']<- 42

data$country2[data$country2=="Central African"]<-"Central African Republic"
data$state2<-countrycode(data$country2,origin='country.name',destination='cown')
data$state2[data$country2=='Gibraltar']<-992
data$state2[data$country2=='Western Sahara']<-993
data$state2[data$country2=='Serbia']<-345
data$state2[data$country2=='Equatorial']<- 411
data$state2[data$country2=='United Arab']<- 696
data$state2[data$country2=='Dominican']<- 42
data$ddyad<-data$state1*1000+data$state2



####
#1.3: Integrating with data on walls and police station
####
walls<-read.csv('walls/cp_walls.csv')
#endyear is a factor wit missing values. Transform to numeric and set to 2020
walls$endyr<-as.character(walls$endyr)
#Assuming walls that were in the CP data persisted to present
walls$endyr[ walls$endyr=="."]<-2020
walls$endyr<-as.integer(walls$endyr)
#remove one dead entry reading into the data
walls<-na.omit(walls)
walls$ccode2[walls$Country.2=='Western Sahara']<-993
f <- function(x) with(x, data.frame(Country.1,ccode1,Country.2,ccode2,styear, endyr,
                                    year=seq(styear,endyr, by=1)))   
walls <- do.call('rbind', by(walls, 1:nrow(walls), f))
walls[,c('styear','endyr')]<-NULL
walls$cp_wall<-2
walls<-dplyr::select(walls,ccode1,ccode2,year,cp_wall)
#There are some cases with multiple walls (Spain->Morocco, China->PRK), so reducing to unique observations
walls<-unique(walls)
data<-merge(data,walls,by.x=c('state1','state2','year'),by.y=c('ccode1','ccode2','year'),all.x=T,all.y=F)
data$cp_wall[is.na(data$cp_wall)]<-1
data<-arrange(data,borderid,year)

##Police Data
police<-read_dta("police/police_presence.dta")
police <- police %>% 
  dplyr::select(ccode1 = ccode_country1,ccode2 = ccode_country2,pol_orientation = relative_police_presence)
data<-merge(data,police,by.x=c('state1','state2'),by.y=c('ccode1','ccode2'),all.x=T,all.y=F)

#Add (new) IDs and remove duplicates
data<-arrange(data,state1,state2,year,borderid)
data$id<-as.numeric(seq(1:nrow(data)))
data$ddyr<-data$ddyad*10000+data$year
data$ddyr<-as.numeric(as.factor(data$ddyr))
data$ddy<-as.numeric(as.factor(data$ddyad))

#Indicators to be passed to stan for the number of rows in data, directed dyads, and directed dyad years
n<-nrow(data)
n_ddyr<-length(unique(data$ddyr))
n_ddy<-length(unique(data$ddy))

#Generate unique wall and police versions of the data set for stan
wall<-as.data.frame(dplyr::select(data,state1,state2,ddy,ddyr,cp_wall,pol_orientation))
wall<-unique(wall)
wall<- wall %>% 
  mutate(pol_orientation_binned = ntile(wall$pol_orientation,5)) 

police<-as.data.frame(dplyr::select(data,ddy,pol_orientation))
police<- police %>% 
  mutate(pol_orientation_binned = ntile(police$pol_orientation,5)) 
police<-unique(police)

save(data,police,wall,file="base_data.RData")


# -------------------------------------------------- #
#Part 2:  Vectorize for Stan
# -------------------------------------------------- #
data<-data[,c('id','borderid','year','ddyad','ddyr',
              'gate',
              'building',
              'split',
              'multi','state1','state2')]

data<-arrange(data,id)
data$state1<-NULL
data$state2<-NULL
data$building[data$building>4]<-4

y_k2<-as.matrix(data[,c('split')])
y_k3<-as.matrix(data[,c('gate')])
y_k4<-as.matrix(data[,c('building')])

k2<-ncol(y_k2)
y_k2_nomis <- which(!is.na(y_k2))
y_k2 <- y_k2[y_k2_nomis]
n_k2 <- length(y_k2)
item_k2<- matrix(c(1:k2), ncol=k2, nrow= n, byrow=T)
item_k2 <- item_k2[y_k2_nomis]
id_k2 <- matrix(data$id,ncol=k2,nrow=n, byrow=F)
id_k2 <- id_k2[y_k2_nomis]

k3<-ncol(y_k3)
y_k3_nomis <- which(!is.na(y_k3))
y_k3 <- y_k3[y_k3_nomis]
n_k3 <- length(y_k3)
item_k3<- matrix(c(1:k3), ncol=k3, nrow= n, byrow=T)
item_k3 <- item_k3[y_k3_nomis]
id_k3 <- matrix(data$id,ncol=k3,nrow=n, byrow=F)
id_k3 <- id_k3[y_k3_nomis]

k4<-ncol(y_k4)
y_k4_nomis <- which(!is.na(y_k4))
y_k4 <- y_k4[y_k4_nomis]
n_k4 <- length(y_k4)
item_k4<- matrix(c(1:k4), ncol=k4, nrow= n, byrow=T)
item_k4 <- item_k4[y_k4_nomis]
id_k4 <- matrix(data$id,ncol=k4,nrow=n, byrow=F)
id_k4 <- id_k4[y_k4_nomis]

y_kdy<-as.matrix(wall[,c('pol_orientation_binned')])
kdy<-ncol(y_kdy)
y_kdy_nomis <- which(!is.na(y_kdy))
y_kdy <- y_kdy[y_kdy_nomis]
n_kdy <- length(y_kdy)
item_kdy<- matrix(c(1:kdy), ncol=kdy, nrow= nrow(wall), byrow=T)
item_kdy <- item_kdy[y_kdy_nomis]
#ID is the row id in the subset police data (i.e. "police")
id_kdy <- matrix(wall$ddyr,ncol=kdy,nrow=nrow(wall), byrow=F)
id_kdy <- id_kdy[y_kdy_nomis]

y_kdyr<-as.matrix(wall[,c('cp_wall')])
kdyr<-ncol(y_kdyr)
y_kdyr_nomis <- which(!is.na(y_kdyr))
y_kdyr <- y_kdyr[y_kdyr_nomis]
n_kdyr <- length(y_kdyr)
item_kdyr<- matrix(c(1:kdyr), ncol=kdyr, nrow= nrow(wall), byrow=T)
item_kdyr <- item_kdyr[y_kdyr_nomis]
id_kdyr <- matrix(wall$ddyr,ncol=kdyr,nrow=nrow(wall), byrow=F)
id_kdyr <- id_kdyr[y_kdyr_nomis]

ddyr<-data$ddyr
ddy<-wall$ddy

save(data,police,wall,n,n_ddyr,n_ddy,k2,k3,n_k2,k2,y_k2,
     item_k2,n_k3,k3,y_k3,item_k3,n_k4,k4,
     y_k4,item_k4,y_kdy,id_kdy,y_kdyr,id_kdyr,
     ddyr,ddy,id_k2,id_k3,id_k4,n_kdyr,kdyr,n_kdy,kdy,
     file="base_data_vectorized.RData")






# -------------------------------------------------- #
# Part 3: Define STAN model
# -------------------------------------------------- #
stan.data <- list(n=n,n_ddyr=n_ddyr,n_ddy=n_ddy,
                  k2=k2,k3=k3,
                  ddyr=data$ddyr,
                  ddy=wall$ddy,
                  id_k2=id_k2,
                  
                  n_k2=n_k2,
                  k2=k2,
                  y_k2=y_k2,
                  item_k2=item_k2,
                  
                  n_k3=n_k3,
                  k3=k3,
                  y_k3=y_k3,
                  item_k3=item_k3,
                  id_k3=id_k3,
                  
                  n_k4=n_k4,
                  k4=k4,
                  y_k4=y_k4,
                  item_k4=item_k4,
                  id_k4=id_k4,
                  
                  ddy=ddy,
                  y_kdy=y_kdy,
                  id_kdy=id_kdy,
                  kdyr=kdyr,
                  n_kdy=n_kdy,
                  
                  ddyr=ddyr,
                  y_kdyr=y_kdyr,
                  id_kdyr=id_kdyr, 
                  n_kdyr=n_kdyr,
                  kdy=kdy
)

model <- "
data {
int<lower=0> n; //number of crossing-sides
int<lower=0> n_ddyr; //number of directed dyad years
int<lower=0> n_ddy; //number of directed dyads

int<lower=0> ddyr[n]; //directed dyad year id for every row in original data
int<lower=0> ddy[n_ddyr]; //directed dyad id for every row in wall data

int<lower=0> n_k2; //number of crossing-item observations w/ 2 cat
int<lower=0> k2; //number of items w/ 2 cat
int<lower=1,upper=3> y_k2[n_k2]; // manifest variables w/ 2 cat
int<lower=0> item_k2[n_k2]; //index of which item among 2 cat vars
int<lower=0> id_k2[n_k2]; //index of which subject for 2 cat

int<lower=0> n_k3; //number of crossing-item observations w/ 3 cat
int<lower=0> k3; //number of items w/ 3 cat
int<lower=1,upper=3> y_k3[n_k3]; // manifest variables w/ 3 cat
int<lower=0> item_k3[n_k3]; //index of which item among 3 cat vars
int<lower=0> id_k3[n_k3]; //index of which crossing for 3 cat

int<lower=0> n_k4; //number of crossing-item observations w/ 4 cat
int<lower=0> k4; //number of items w/ 4 cat
int<lower=1,upper=4> y_k4[n_k4]; // manifest variables w/ 4 cat
int<lower=0> item_k4[n_k4]; //index of which item among 4 cat vars
int<lower=0> id_k4[n_k4]; //index of which crossing for 4 cat

int<lower=0> n_kdyr; //number of directed dyad observations 
int<lower=0> kdyr; //number of items measured at the directed dyad level
int<lower=1,upper=2> y_kdyr[n_kdyr]; // manifest variable(s)
int<lower=0> id_kdyr[n_kdyr]; //index from original ddy data set

int<lower=0> n_kdy; //number of directed dyad observations 
int<lower=0> kdy; //number of items measured at the directed dyad level
int<lower=1,upper=5> y_kdy[n_kdy]; // manifest variable(s)
int<lower=0> id_kdy[n_kdy]; //index from original ddy data set


}
parameters {
ordered[1] alpha_k2[k2]; //cutpoint for 2 cat
vector<lower=0>[k2] beta_k2; //beta for 2 cat

ordered[2] c_k3[k3]; //matrix of cutpoints for vars w/ 3 cat
vector<lower=0>[k3] beta_k3; //betas for vars w/ 3 cat

ordered[3] c_k4[k4]; //matrix of cutpoints for vars w/ 4 cat
vector<lower=0>[k4] beta_k4; //betas for vars w/ 4 cat

ordered[1] alpha_wall; //alpha for wall
real<lower=0> beta_wall; //beta for wall
ordered[4] alpha_police; //alpha for police
real<lower=0> beta_police; //beta for police

vector<lower=0>[1] sigma;

vector[n] theta_raw; 
vector[n_ddyr] xi; 
}


transformed parameters {
vector[n] theta;
for(ii in 1:n){
theta[ii] = xi[ddyr[ii]] + theta_raw[ii]*sigma[1];
}		

}


model {
theta_raw ~ normal(0,1);
xi ~ normal(0,1);


sigma ~ cauchy(0, 2.5);

alpha_k2[1] ~ normal(0,5);


for(j in 1:k3){
c_k3[j,1] ~ normal(0,5); 
c_k3[j,2] ~ normal(0,5);
}

for(j in 1:k4){
c_k4[j,1] ~ normal(0,5); 
c_k4[j,2] ~ normal(0,5);
c_k4[j,3] ~ normal(0,5);
}

alpha_wall ~ normal(0,5);
alpha_police ~ normal(0,5);
beta_k2 ~ normal(0,3);
beta_k3 ~ normal(0,3);
beta_k4 ~ normal(0,3);
beta_wall ~ normal(0,3);
beta_police ~ normal(0,3);


for(ii in 1:n_kdy){
y_kdy[ii] ~ ordered_logistic(beta_police*xi[id_kdy[ii]],alpha_police); 
}

for(ii in 1:n_kdyr){
y_kdyr[ii] ~ ordered_logistic(beta_wall * xi[id_kdyr[ii]],alpha_wall); 
}

for(ii in 1:n_k2){
y_k2[ii] ~ ordered_logistic(beta_k2[item_k2[ii]] * theta[id_k2[ii]],alpha_k2[item_k2[ii]] ); 
}

for(ii in 1:n_k3){
y_k3[ii] ~ ordered_logistic(beta_k3[item_k3[ii]] * theta[id_k3[ii]], c_k3[item_k3[ii]]);
}

for(ii in 1:n_k4){
y_k4[ii] ~ ordered_logistic(beta_k4[item_k4[ii]]*theta[id_k4[ii]],c_k4[item_k4[ii]] ); 
}
}

generated quantities {
vector[n_kdy] y_hat_kdy;
vector[n_kdyr] y_hat_kdyr;
vector[n_k2] y_hat_k2;
vector[n_k3] y_hat_k3;
vector[n_k4] y_hat_k4;
for(ii in 1:n_kdy){
y_hat_kdy[ii] = ordered_logistic_rng(beta_police*xi[id_kdy[ii]],alpha_police); 
}
for(ii in 1:n_kdyr){
y_hat_kdyr[ii] = ordered_logistic_rng(beta_wall * xi[id_kdyr[ii]],alpha_wall); 
}
for(ii in 1:n_k2){
y_hat_k2[ii] = ordered_logistic_rng(beta_k2[item_k2[ii]] * theta[id_k2[ii]],alpha_k2[item_k2[ii]] ); 
}
for(ii in 1:n_k3){
y_hat_k3[ii] = ordered_logistic_rng(beta_k3[item_k3[ii]] * theta[id_k3[ii]], c_k3[item_k3[ii]]);
}
for(ii in 1:n_k4){
y_hat_k4[ii] = ordered_logistic_rng(beta_k4[item_k4[ii]]*theta[id_k4[ii]],c_k4[item_k4[ii]] ); 
}
}
"
# -------------------------------------------------- #

fit <- stan(model_code=model, data=stan.data, iter=3000, chains=5,cores=5,
            pars=c("theta_raw","xi_raw"),include=FALSE,seed=1989)
#Enter the command below to store the complete model output
#save(fit, file="bo_static_raw.RData")

output <- rstan::extract(fit, permuted = TRUE)
#We reduced the output object for easier storage on the Harvard Dataverse website.
#Leave these lines commented out if you would like to save the full model output
#output$y_hat_kdy<-output$y_hat_kdy[1:1000,]
#output$y_hat_kdyr<-output$y_hat_kdyr[1:1000,]
#output$y_hat_k2<-output$y_hat_k2[1:1000,]
#output$y_hat_k3<-output$y_hat_k3[1:1000,]
#output$y_hat_k4<-output$y_hat_k4[1:1000,]
#output<-output[12:18]
save(output, file="model/bo_static_output.RData")

#For Printed output, un-comment the following lines
#options(max.print=5.5E5)
#sink("bo_output_printed.txt")
#print(fit)
#sink()


