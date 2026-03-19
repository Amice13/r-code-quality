# Code to fit the pooled MLE to the Faux Mesa High Simulations
#
# For each synthetic data set in fms_synthetic_data_stats.RData, we fit a 
# pooled model by MLE, saving the results in fms_synthetic_data_MLE.RData
#
# Last modified 8/18/21 by CTB

#Load required libraries
library(ergm)
library(parallel)

#Load the faux.mesa.high base network
data(faux.mesa.high)

#Source the estimation code
source("ergm_pooled_fit.R")

#Load the simulated statistics
load("fms_synthetic_data_stats.RData")

#Set cores, base seed, and other parameters
cores<-40
base.seed<-1331
burn<-1e6                           #Burn-in
thin<-2e5                           #Thinning interval

#Fit the model to each replicate
ss<-as.numeric(names(fmsstat))               #Sample sizes
nss<-length(fmsstat)                         #Number of sample sizes
reps<-length(fmsstat[[1]])                   #Number of reps per ss
fmsMLEs<-vector(mode="list",length=nss)
stime<-proc.time()[3]
for(i in 1:10){
  cat("Working on size",names(fmsstat)[i],"\n")
  fmsMLEs[[i]]<-mclapply(1:reps,function(j){
    set.seed(base.seed+(i-1)*reps+j-1)  #Set to a unique seed for this model
    ms<-colMeans(fmsstat[[i]][[j]])     #Get the mean stats
    attr(ms,"ng")<-ss[i]                #Record the number of graphs
    suppressMessages(fit<-ergmMSFit(faux.mesa.high ~ edges + nodematch("Sex") + gwesp(0.25,fixed=TRUE), mean.stats=ms, eval.loglik=FALSE, control=control.ergm(MCMLE.termination="Hotelling", MCMC.interval=thin, MCMC.burnin=burn)))
    list(coef=coef(fit),cov=fit$covar)
  },mc.cores=cores, mc.preschedule=FALSE)
}
print(proc.time()[3]-stime)
names(fmsMLEs)<-names(fmsstat)

#Save the MLE fits
save(fmsMLEs,file="fms_synthetic_data_MLE.RData")
