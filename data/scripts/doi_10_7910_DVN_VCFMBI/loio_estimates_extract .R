#########################################################
#Author: M. Kenwick
#Date: Sep 20, 2019
#Purpose: Extracts parameters of interest for posterior predictive
#analyses and stores them separately for more efficient analysis. 
#########################################################


###PART 1
#Extract thetas from the 20 models leaving one item out each
setwd('~/Dropbox/ctrl_rep_final/irt_modeling/loio/output')
load('drift_output_k1.RData')
theta_1_drf<-t(output$x[1:1000,])
rm(output)
load('drift_output_k2.RData')
theta_2_drf<-t(output$x[1:1000,])
rm(output)
load('drift_output_k3.RData')
theta_3_drf<-t(output$x[1:1000,])
rm(output)
load('drift_output_k4.RData')
theta_4_drf<-t(output$x[1:1000,])
rm(output)
load('drift_output_k5.RData')
theta_5_drf<-t(output$x[1:1000,])
rm(output)
load('drift_output_k6.RData')
theta_6_drf<-t(output$x[1:1000,])
rm(output)
load('drift_output_k7.RData')
theta_7_drf<-t(output$x[1:1000,])
rm(output)
load('drift_output_k8.RData')
theta_8_drf<-t(output$x[1:1000,])
rm(output)
load('drift_output_k9.RData')
theta_9_drf<-t(output$x[1:1000,])
rm(output)
load('drift_output_k10.RData')
theta_10_drf<-t(output$x[1:1000,])
rm(output)

load('static_output_k1.RData')
theta_1_stat<-t(output$x[1:1000,])
rm(output)
load('static_output_k2.RData')
theta_2_stat<-t(output$x[1:1000,])
rm(output)
load('static_output_k3.RData')
theta_3_stat<-t(output$x[1:1000,])
rm(output)
load('static_output_k4.RData')
theta_4_stat<-t(output$x[1:1000,])
rm(output)
load('static_output_k5.RData')
theta_5_stat<-t(output$x[1:1000,])
rm(output)
load('static_output_k6.RData')
theta_6_stat<-t(output$x[1:1000,])
rm(output)
load('static_output_k7.RData')
theta_7_stat<-t(output$x[1:1000,])
rm(output)
load('static_output_k8.RData')
theta_8_stat<-t(output$x[1:1000,])
rm(output)
load('static_output_k9.RData')
theta_9_stat<-t(output$x[1:1000,])
rm(output)
load('static_output_k10.RData')
theta_10_stat<-t(output$x[1:1000,])
rm(output)
setwd('~/Dropbox/ctrl_rep_final/output')
save(theta_1_stat,theta_1_drf,
     theta_2_stat,theta_2_drf,
     theta_3_stat,theta_3_drf,
     theta_4_stat,theta_4_drf,
     theta_5_stat,theta_5_drf,
     theta_6_stat,theta_6_drf,
     theta_7_stat,theta_7_drf,
     theta_8_stat,theta_8_drf,
     theta_9_stat,theta_9_drf,
     theta_10_stat,theta_10_drf,
     file="loio_thetas.RData")

###PART 2
#Generating out of sample posterior predictions for each left out item
#For Figure 3, right panel
library(arm)
load("~/Dropbox/ctrl_rep_final/irt_modeling/data_prepped.RData")

pcp_s<-matrix(nrow=11,ncol=1000,NA)
pcp_d<-matrix(nrow=11,ncol=1000,NA)

for(ii in 1:1000){
  m1<-glm(data$svolik_milentry~theta_1_stat[,ii],family=binomial)
  hats<-plogis(na.omit(predict(m1)))
  hats<-rbinom(length(hats),1,hats)
  correct<-ifelse(na.omit(data$svolik_milentry)==hats,1,0)
  pcp_s[1,ii]<-mean(correct)
  
  m2<-glm(data$svolik_milentry~theta_1_drf[,ii],family=binomial)
  hats<-plogis(na.omit(predict(m2)))
  hats<-rbinom(length(hats),1,hats)
  correct<-ifelse(na.omit(data$svolik_milentry)==hats,1,0)
  pcp_d[1,ii]<-mean(correct)
}

for(ii in 1:1000){
  m1<-glm(data$cgv_mil~theta_2_stat[,ii],family=binomial)
  hats<-plogis(na.omit(predict(m1)))
  hats<-rbinom(length(hats),1,hats)
  correct<-ifelse(na.omit(data$cgv_mil)==hats,1,0)
  pcp_s[2,ii]<-mean(correct)
  
  m2<-glm(data$cgv_mil~theta_2_drf[,ii],family=binomial)
  hats<-plogis(na.omit(predict(m2)))
  hats<-rbinom(length(hats),1,hats)
  correct<-ifelse(na.omit(data$cgv_mil)==hats,1,0)
  pcp_d[2,ii]<-mean(correct)
}


for(ii in 1:1000){
  m1<-glm(data$dpi_millead~theta_3_stat[,ii],family=binomial)
  hats<-plogis(na.omit(predict(m1)))
  hats<-rbinom(length(hats),1,hats)
  correct<-ifelse(na.omit(data$dpi_millead)==hats,1,0)
  pcp_s[3,ii]<-mean(correct)
  
  m2<-glm(data$dpi_millead~theta_3_drf[,ii],family=binomial)
  hats<-plogis(na.omit(predict(m2)))
  hats<-rbinom(length(hats),1,hats)
  correct<-ifelse(na.omit(data$dpi_millead)==hats,1,0)
  pcp_d[3,ii]<-mean(correct)
}

for(ii in 1:1000){
  m1<-glm(data$gwf_mil~theta_4_stat[,ii],family=binomial)
  hats<-plogis(na.omit(predict(m1)))
  hats<-rbinom(length(hats),1,hats)
  correct<-ifelse(na.omit(data$gwf_mil)==hats,1,0)
  pcp_s[4,ii]<-mean(correct)
  
  m2<-glm(data$gwf_mil~theta_4_drf[,ii],family=binomial)
  hats<-plogis(na.omit(predict(m2)))
  hats<-rbinom(length(hats),1,hats)
  correct<-ifelse(na.omit(data$gwf_mil)==hats,1,0)
  pcp_d[4,ii]<-mean(correct)
}

for(ii in 1:1000){
  m1<-glm(data$gwf_mil_prior~theta_5_stat[,ii],family=binomial)
  hats<-plogis(na.omit(predict(m1)))
  hats<-rbinom(length(hats),1,hats)
  correct<-ifelse(na.omit(data$gwf_mil_prior)==hats,1,0)
  pcp_s[5,ii]<-mean(correct)
  
  m2<-glm(data$gwf_mil_prior~theta_5_drf[,ii],family=binomial)
  hats<-plogis(na.omit(predict(m2)))
  hats<-rbinom(length(hats),1,hats)
  correct<-ifelse(na.omit(data$gwf_mil_prior)==hats,1,0)
  pcp_d[5,ii]<-mean(correct)
}

for(ii in 1:1000){
  m1<-glm(data$ard_mil_any~theta_6_stat[,ii],family=binomial)
  hats<-plogis(na.omit(predict(m1)))
  hats<-rbinom(length(hats),1,hats)
  correct<-ifelse(na.omit(data$ard_mil_any)==hats,1,0)
  pcp_s[6,ii]<-mean(correct)
  
  m2<-glm(data$ard_mil_any~theta_6_drf[,ii],family=binomial)
  hats<-plogis(na.omit(predict(m2)))
  hats<-rbinom(length(hats),1,hats)
  correct<-ifelse(na.omit(data$ard_mil_any)==hats,1,0)
  pcp_d[6,ii]<-mean(correct)
}

smpl = function(y) {
  smpl<-sample(x=c(1,2,3),size=1,prob=y)
}

for(ii in 1:1000){
  m1<-polr(as.factor(data$hs_lead_scale)~theta_7_stat[,ii])
  hats<-na.omit(predict(m1,type='probs'))
  hats<-apply(hats,1,smpl)
  correct<-ifelse(na.omit(data$hs_lead_scale)==hats,1,0)
  pcp_s[7,ii]<-mean(correct)
  
  m2<-polr(as.factor(data$hs_lead_scale)~theta_7_drf[,ii])
  hats<-na.omit(predict(m2,type='probs'))
  hats<-apply(hats,1,smpl)
  correct<-ifelse(na.omit(data$hs_lead_scale)==hats,1,0)
  pcp_d[7,ii]<-mean(correct)
}

for(ii in 1:1000){
  m1<-polr(as.factor(data$mpg_cat)~theta_8_stat[,ii])
  hats<-na.omit(predict(m1,type='probs'))
  hats<-apply(hats,1,smpl)
  correct<-ifelse(na.omit(data$mpg_cat)==hats,1,0)
  pcp_s[8,ii]<-mean(correct)
  
  m2<-polr(as.factor(data$mpg_cat)~theta_8_drf[,ii])
  hats<-na.omit(predict(m2,type='probs'))
  hats<-apply(hats,1,smpl)
  correct<-ifelse(na.omit(data$mpg_cat)==hats,1,0)
  pcp_d[8,ii]<-mean(correct)
}

smpl4 = function(y) {
  smpl<-sample(x=c(1,2,3,4),size=1,prob=y)
}
for(ii in 1:1000){
  m1<-polr(as.factor(data$svolik_military_scale)~theta_9_stat[,ii])
  hats<-na.omit(predict(m1,type='probs'))
  hats<-apply(hats,1,smpl4)
  correct<-ifelse(na.omit(data$svolik_military_scale)==hats,1,0)
  pcp_s[9,ii]<-mean(correct)
  
  m2<-polr(as.factor(data$svolik_military_scale)~theta_9_drf[,ii])
  hats<-na.omit(predict(m2,type='probs'))
  hats<-apply(hats,1,smpl4)
  correct<-ifelse(na.omit(data$svolik_military_scale)==hats,1,0)
  pcp_d[9,ii]<-mean(correct)
}

smpl5 = function(y) {
  smpl<-sample(x=c(1,2,3,4,5),size=1,prob=y)
}
for(ii in 1:1000){
  m1<-polr(as.factor(data$weeks_milindex_simple)~theta_10_stat[,ii])
  hats<-na.omit(predict(m1,type='probs'))
  hats<-apply(hats,1,smpl5)
  correct<-ifelse(na.omit(data$weeks_milindex_simple)==hats,1,0)
  pcp_s[10,ii]<-mean(correct)
  
  m2<-polr(as.factor(data$weeks_milindex_simple)~theta_10_drf[,ii])
  hats<-na.omit(predict(m2,type='probs'))
  hats<-apply(hats,1,smpl5)
  correct<-ifelse(na.omit(data$weeks_milindex_simple)==hats,1,0)
  pcp_d[10,ii]<-mean(correct)
}
save(pcp_s,pcp_d,file="loio_oos_pcp.RData")

###PART 3
#Computing in-sample posterior predictions 
#For Figure 3, left panel
#Start by (re-) vectorizing observed data
rm(list=ls())
library(dplyr)
setwd('~/Dropbox/ctrl_rep_final')
load("irt_modeling/data_prepped.RData")
load("output/static_output.RData")
output_s<-output
load("output/drift_output.RData")

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

save(s_postpred, d_postpred,file="output/pcp_output.RData")