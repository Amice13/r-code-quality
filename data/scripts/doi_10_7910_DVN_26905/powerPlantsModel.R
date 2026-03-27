############## ESTIMATING MODELS #########################
#clean up and set directory
rm(list=ls())
options(scipen=10,digits=10)
#setwd('/Users/jamie/Desktop/replication')

#load libraries
library(spatstat)
library(maptools)
library(rgdal)
library(maps)
library(foreign)
library(mgcv)
library(rgeos)
#library(xtable)
library(akima) #for drawing image/surface plots
library(scatterplot3d)
library(lattice)
library(ggplot2)
library(MASS)

#Load data
full<-read.csv('anyPowerPlantDist.csv')

#subsets 
base<-subset(full,air==1 | lqg==1)
power<-subset(full,air==1)
control<-subset(full,air==0 & lqg==1)

####MODELS SUBSETTED BY TIME####
#Generate time indicator
base$early<-as.numeric(base$online_year<1970)
base$middle<-as.numeric(base$online_year>=1970 & base$online_year<=1980)
base$late<-as.numeric(base$online_year>1980)

#model for early
s.down.mod.early<-gam(air~sDown+s(longitude,latitude), data=base, subset=early==1|is.na(early), family=binomial(link='logit')); summary(s.down.mod.early); s.down.mod.early$aic

#model for middle
s.down.mod.middle<-gam(air~sDown+s(longitude,latitude), data=base, subset=middle==1|is.na(early), family=binomial(link='logit')); summary(s.down.mod.middle); s.down.mod.middle$aic

#model for late
s.down.mod.late<-gam(air~sDown+s(longitude,latitude), data=base, subset=late==1|is.na(early), family=binomial(link='logit')); summary(s.down.mod.late); s.down.mod.late$aic

##clarify-like simulations##
vcov.early<-summary(s.down.mod.early)$cov.scaled[1:2,1:2]
vcov.middle<-summary(s.down.mod.middle)$cov.scaled[1:2,1:2]
vcov.late<-summary(s.down.mod.late)$cov.scaled[1:2,1:2]

sim.early<-mvrnorm(n=1000,mu=s.down.mod.early$coefficients[1:2],Sigma=vcov.early,empirical=TRUE)
sim.middle<-mvrnorm(n=1000,mu=s.down.mod.middle$coefficients[1:2],Sigma=vcov.middle,empirical=TRUE)
sim.late<-mvrnorm(n=1000,mu=s.down.mod.late$coefficients[1:2],Sigma=vcov.late,empirical=TRUE)

#effect of downwind distance by time
X<-seq(0,1,by=.01)
util.early<- -2.7602027968-0.3771106044*X
prob.early<-plogis(util.early)
forecast.early<-matrix(NA,nrow=1000,ncol=101)
for (i in 1:1000){
	coef<-sim.early[i,]
		for(j in 1:101){
			predictors<-c(1,X[j])
			forecast.early[i,j]<-plogis(predictors%*%coef)
			}
			}
low.early<-apply(forecast.early,2,quantile,.05)
high.early<-apply(forecast.early,2,quantile,.95)

util.middle<- -4.0408486520-0.3559083678*X
prob.middle<-plogis(util.middle)
forecast.middle<-matrix(NA,nrow=1000,ncol=101)
for (i in 1:1000){
	coef<-sim.middle[i,]
		for(j in 1:101){
			predictors<-c(1,X[j])
			forecast.middle[i,j]<-plogis(predictors%*%coef)
			}
			}
low.middle<-apply(forecast.middle,2,quantile,.05)
high.middle<-apply(forecast.middle,2,quantile,.95)

util.late<- -2.18185400007-0.05291138418*X
prob.late<-plogis(util.late)
forecast.late<-matrix(NA,nrow=1000,ncol=101)
for (i in 1:1000){
	coef<-sim.late[i,]
		for(j in 1:101){
			predictors<-c(1,X[j])
			forecast.late[i,j]<-plogis(predictors%*%coef)
			}
			}
low.late<-apply(forecast.late,2,quantile,.05)
high.late<-apply(forecast.late,2,quantile,.95)

#pdf('timeProb.pdf')
plot(y=prob.early,x=X,type='l',ylim=c(0,.11),lwd=2,xlab="Standardized Distance to Downwind Border",ylab="Probability of Major Air Polluter")
polygon(x=c(X,rev(X)), y=c(low.early,rev(high.early)),border=NA,col=rgb(.6,.6,.6,.3)) 
lines(y=prob.middle,x=X,lty=2,col='red',lwd=2)
polygon(x=c(X,rev(X)), y=c(low.middle,rev(high.middle)),border=NA,col=rgb(.9,0,0,.3)) 
lines(y=prob.late,x=X,lty=3,col='blue',lwd=2)
polygon(x=c(X,rev(X)), y=c(low.late,rev(high.late)),border=NA,col=rgb(0,0,.9,.3)) 
text(x=.2,y=.04,"Pre-1970",pos=1);arrows(.2,.04,.25,.05,length=.1)
text(x=.2,y=.005,"1970s",pos=1); arrows(.2,.005,.25,.01,length=.1)
text(x=.2,y=.085,"Post-1980",pos=1); arrows(.2,.085,.25,.095,length=.1)
#dev.off()
