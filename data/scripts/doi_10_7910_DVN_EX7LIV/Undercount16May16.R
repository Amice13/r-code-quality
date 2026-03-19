## CR Conrad, DW Hill & WH Moore
## To run batch, from shell prompt: R CMD BATCH filename.R

setwd("/Users/danielhill/Documents/torture/JPR R&R/empirics/replication")
library(MASS) ## contains the glm.nb function
library(Amelia) ## contains function for pooling estimates from multiply imputed data sets

##### log likelihood function for undercount negative binomial
source("nb_under_lik.R")

##### read in data, omit electoral system type indicators
tort.dat<-read.csv(file="estimation_sample.csv")
tort.dat<-data.frame(tort.dat[,1:13],tort.dat[,17:18])
tort.dat<-na.omit(tort.dat)

##### draws from posteriors of Linzer/Staton judicial independence measure
set.seed(666)
ji.draws<-matrix(nrow=nrow(tort.dat),ncol=10)
for (i in 1:nrow(tort.dat)){
	ji.draws[i,]<-rnorm(10,mean=tort.dat$ji[i],sd=tort.dat$ji.sd)
	}

##### draws for Schnakenberg/Fariss personal integrity rights measure
hr.draws<-matrix(nrow=nrow(tort.dat),ncol=10)
for (i in 1:nrow(tort.dat)){
	hr.draws[i,]<-rnorm(10,mean=tort.dat$lag_hrscore[i],sd=tort.dat$lag_hrscore_sd)
	}

##### data for estimation, scarring torture models	
### dv
y<-tort.dat$scar
### count/detection covariates
x<-list()
z<-list()
for (i in 1:10){
	x[[i]]<-cbind(tort.dat$dem, ji.draws[,i], tort.dat$stealth, tort.dat$unstated, tort.dat$gdpc, tort.dat$pop, tort.dat$cwar)
	z[[i]]<-cbind(hr.draws[,i],tort.dat$hros,tort.dat$gdpc,tort.dat$speech)
	}

## Estimate negative binomial first, use estimates as starting values for count equation below
summary(nb.mod<-glm.nb(y~dem+ji+stealth+unstated+gdpc+pop+cwar,data=tort.dat))

##### This is the estimation command for the scarring torture model, looping through the draws from the judicial independence measure. The first object in the optim function is a vector of starting values for the parameters. You need one for each coefficient and one for alpha. First is count equation, starting w/ intercept, then logit starting w/ intercept, then the alpha (overdispersion) parameter. Alpha must be positive. 

nb.under.mods<-list()
ests<-list()
ests.ses<-list()
for (i in 1:10){
	X<-cbind(1,x[[i]])
	Z<-cbind(1,z[[i]])
	nb.under.mods[[i]]<-optim(c(nb.mod$coefficients, 0, 0, 0, 0, 0, 0.5), nb.under.lik, method="BFGS", hessian=T, y=y, X=X, Z=Z)
	ests[[i]]<-nb.under.mods[[i]]$par
	vcv<-solve(nb.under.mods[[i]]$hessian)
	ests.ses[[i]]<-sqrt(diag(vcv))	
	}

ests<-do.call(rbind,ests)
ests.ses<-do.call(rbind,ests.ses)

scar.ests<-mi.meld(ests,ests.ses)
save(scar.ests,file="scar_ests.rda")

##### predicted probabilities from detection equation
zgamma<-scar.ests$q.mi[9]+
scar.ests$q.mi[10]*tort.dat$lag_hrscore+
scar.ests$q.mi[11]*tort.dat$hros+
scar.ests$q.mi[12]*tort.dat$gdpc+
scar.ests$q.mi[13]*tort.dat$speech

phat<-plogis(zgamma)
summary(phat)
scar.detection.probs<-data.frame(tort.dat[,1:2],phat)
save(scar.detection.probs,file="scar_detection_probs.rda")

##### data for estimation, stealth torture models	
### dv
y<-tort.dat$stealth
### count covariates
x<-list()
for (i in 1:10){
	x[[i]]<-cbind(tort.dat$dem, ji.draws[,i], tort.dat$scar, tort.dat$unstated, tort.dat$gdpc, tort.dat$pop, tort.dat$cwar)
	}
### detection covariates, same as above

##### Estimate negative binomial first, use estimates as starting values for count equation below
summary(nb.mod<-glm.nb(y~dem+ji+scar+unstated+gdpc+pop+cwar,data=tort.dat))

##### Estimation loop for stealth torture models
nb.under.mods<-list()
ests<-list()
ests.ses<-list()
for (i in 1:10){
	X<-cbind(1,x[[i]])
	Z<-cbind(1,z[[i]])
	nb.under.mods[[i]]<-optim(c(nb.mod$coefficients, 0, 0, 0, 0, 0, 0.5), nb.under.lik, method="BFGS", hessian=T, y=y, X=X, Z=Z)
	ests[[i]]<-nb.under.mods[[i]]$par
	vcv<-solve(nb.under.mods[[i]]$hessian)
	ests.ses[[i]]<-sqrt(diag(vcv))	
	}

ests<-do.call(rbind,ests)
ests.ses<-do.call(rbind,ests.ses)

stealth.ests<-mi.meld(ests,ests.ses)
save(stealth.ests,file="stealth_ests.rda")

##### predicted probabilities from detection equation
zgamma<-stealth.ests$q.mi[9]+
stealth.ests$q.mi[10]*tort.dat$lag_hrscore+
stealth.ests$q.mi[11]*tort.dat$hros+
stealth.ests$q.mi[12]*tort.dat$gdpc+
stealth.ests$q.mi[13]*tort.dat$speech

phat<-plogis(zgamma)
summary(phat)
stealth.detection.probs<-data.frame(tort.dat[,1:2],phat)
save(stealth.detection.probs,file="stealth_detection_probs.rda")



