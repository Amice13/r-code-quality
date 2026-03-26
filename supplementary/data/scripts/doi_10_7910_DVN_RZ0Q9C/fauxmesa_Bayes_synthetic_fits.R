# Code to fit examine conjuagate Bayes analysis of the Faux Mesa High
# Simulations
#
# Last modified 8/13/21 by CTB
#Load required libraries
library(ergm)
library(parallel)

#Load the faux.mesa.high base network
data(faux.mesa.high)

#Source the estimation code
source("ergm_pooled_fit.R")

#Load the simulated statistics
load("fms_synthetic_data_stats.RData")

#Set cores, base seed, and the like
cores<-40
base.seed<-1331
burn<-1e6                           #Burn-in
thin<-2e5                           #Thinning interval

#Fit the model to each m=1 replicate, with varying prior weights
delta<-c(0,0.001,0.002,0.005,0.0075,0.01,0.02,0.05,0.1,0.2,0.3,0.4,0.5,0.75, 0.9,0.95,0.99,1)
nd<-length(delta)
prior.expectation<-c(201.64,99.89,3.62)      #Prior expected statistics
reps<-length(fmsstat[[1]])                   #Number of reps per ss
fmsBayes<-vector(mode="list",length=nd)
for(i in 1:nd){
  cat("Working on delta",delta[i],"\n")
  fmsBayes[[i]]<-mclapply(1:reps,function(j){
    set.seed(base.seed+(i-1)*reps+j-1)  #Set to a unique seed for this model
    ms<-colMeans(fmsstat[[1]][[j]])     #Get the mean stats
    attr(ms,"ng")<-1                    #Record the number of graphs
    suppressMessages(fit<-ergmMSFit(faux.mesa.high ~ edges + nodematch("Sex") + gwesp(0.25,fixed=TRUE), mean.stats=ms, eval.loglik=FALSE, prior.expectation=prior.expectation, prior.ss=min(delta[i]/(1-delta[i]),1e100), control=control.ergm(MCMLE.termination="Hotelling", MCMC.interval=thin, MCMC.burnin=burn)))
    list(coef=coef(fit),cov=fit$covar)
  },mc.cores=cores, mc.preschedule=FALSE)
}
names(fmsBayes)<-delta

#Save the Bayes fits
save(delta,fmsBayes,file="fms_synthetic_data_Bayes.RData")
