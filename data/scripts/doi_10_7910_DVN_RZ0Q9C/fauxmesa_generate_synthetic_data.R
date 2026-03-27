# Code to generate the simulated draws for the Faux Mesa High Simulations
#
# We produce iid samples from fmsfit (see fms_groundtruth_model.RData), 
# generating 1,000 data sets of sizes 1, 2, 5, 10, 20, 30, 40, 50, 75, and 100.
# To speed subsequent calculations, we obtain the sufficient statistics 
# for each replicate.
#
# The results are saved in the files fms_synthetic_data_nets.RData (with 
# object fmsdat containing a list of sample size by replicate by network) and
# fms_synthetic_data_stats.RData (with object fmsstat containing a list of
# sample size by replicate containing a network by stat matrix).
#
# Last modified 8/13/21 by CTB

#Load the required libraries
library(ergm) #ergm 4.1.2 used for these calculations
library(parallel)

#Load the model we are using
load("fms_groundtruth_model.RData") #Model object is fmsfit

#Set cores and base seed
cores<-40
base.seed<-1331

#Set simulation parameters
ss<-c(1,2,5,10,20,30,40,50,75,100)  #Simulated sample sizes
nss<-length(ss)
reps<-1000                          #Replicates to use for each size
burn<-1e6                           #Burn-in
thin<-2e5                           #Thinning interval

#Draw the data sets
fmsdat<-vector(mode="list",length=nss)  #size x replicate x network
for(i in 1:nss){  #Walk through each sample size
  cat("Working on size",ss[i],"\n")
  #Generate the replicate data sets
  fmsdat[[i]]<-mclapply(1:reps,function(j){
    set.seed(base.seed+(i-1)*reps+j-1)  #Set to a unique seed for this sample
    simulate(fmsfit, nsim=ss[i], control=control.simulate.ergm(MCMC.burn=burn, MCMC.interval=thin))
  },mc.cores=cores)
}
fmsstat<-lapply(fmsdat,function(z){lapply(z,attr,"stats")})
names(fmsdat)<-ss
names(fmsstat)<-ss

#Save the data
save(fmsdat,file="fms_synthetic_data_nets.RData")
save(fmsstat,file="fms_synthetic_data_stats.RData")
