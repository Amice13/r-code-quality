# Code to repoduce the brain connectivity network evaluations
#
# The input here is the data in brain_binary_network_list.rds.  Models
# are saved in brain_fits.RData, though the results are produced inline.
#
# Last modified 10/23/21 by CTB

#Load required libraries and other code
library(sna)
library(ergm)
library(ergm.graphlets)
source("ergm_pooled_fit.R")

#Load the brain connectivity networks
brains<-readRDS("brain_binary_network_list.rds")

#Set seed and related matters
set.seed(1331)
burn<-2e5               #Burn-in
thin<-5e4               #Thinning interval

#Fit the pooled model by MLE
fitMLE<-ergmMSFit(brains ~ edges + gwesp(0.5,fixed=TRUE) + graphletCount(1) + edgecov("LogDistance") + nodematch("Hemisphere") + nodemix("Area_Combined_large"), eval.loglik=FALSE, control=control.ergm(main.method="Stochastic", MCMC.interval=thin, MCMC.burnin=burn))

#Print the MLE results
summary(fitMLE)
tab<-cbind(formatC(coef(fitMLE),digits=3,format="f"), paste0("(",formatC(diag(fitMLE$cov)^0.5,digits=3,format="f"),")"), symnum(2*(1-pnorm(abs(coef(fitMLE)/diag(fitMLE$cov)^0.5))), corr=FALSE, cutpoints=c(0, 0.0001, 0.001, 0.01, 0.05, 0.1, 1), symbols=c("${****}$", "${***}$", "${**}$", "${*}$", ".", " ")), " \\\\\n")
cat(apply(tab,1,paste,collapse=" & "))

#Construct the prior sample for MAP
prior.reps<-1e3
basenet<-brains[[1]]
basenet[,]<-0
prior.samp<-lapply(1:prior.reps,function(z){
  net<-basenet
  g<-rgraph(90,tp=90^(1/2.78)/89,mode="graph",return.as.edgelist=TRUE)
  add.edges(net,g[g[,1]<g[,2],1],g[g[,1]<g[,2],2])
  net
})
class(prior.samp)<-"network.list"
prior.stats<-summary(prior.samp ~ edges + gwesp(0.5,fixed=TRUE) + graphletCount(1) + edgecov("LogDistance") + nodematch("Hemisphere") + nodemix("Area_Combined_large"))
prior.expectation<-colMeans(prior.stats)

#Fit via MAP
delta<-0.02
fitMAP<-ergmMSFit(brains ~ edges + gwesp(0.5,fixed=TRUE) + graphletCount(1) + edgecov("LogDistance") + nodematch("Hemisphere") + nodemix("Area_Combined_large"), prior.expectation=prior.expectation, prior.ss=length(brains)*delta/(1-delta), eval.loglik=FALSE, control=control.ergm(main.method="Stochastic", MCMC.interval=thin, MCMC.burnin=burn))

#Print the MAP results
summary(fitMAP)
tab<-cbind(formatC(coef(fitMAP),digits=3,format="f"), " & ", "(", formatC(diag(fitMAP$cov)^0.5,digits=3,format="f"), ") & (", formatC(coef(fitMAP)-1.96*diag(fitMAP$cov)^0.5,digits=3,format="f"), ",", formatC(coef(fitMAP)+1.96*diag(fitMAP$cov)^0.5,digits=3,format="f"), ") \\\\\n")
cat(apply(tab,1,paste,collapse=""))

#Model with Occipital and Cingulum added
prior.stats<-summary(prior.samp ~ edges + gwesp(0.5,fixed=TRUE) + graphletCount(1) + edgecov("LogDistance") + nodematch("Hemisphere") + nodemix("Area_Extended"))
prior.expectation<-colMeans(prior.stats)

#Function to choose n0 by leave-one-out CV, under squared error Hamming loss
#Because we have to seed with the MPLE, and this can yield runaway estimates
#even if the regularized estimate is fine, we check for any ill-constrained
#parameters and initialize them w/0 values prior to the main estimation
#routine.  (This wouldn't be a bad thing for ergm() to do by default, 
#arguably.)
cvreps<-1000            #Number of CV simulation reps
RegLECV<-function(n0){  #Get the CV error with weight n0 for reg. lik. est.
  err<-vector()
  for(i in 1:10){
    cat("Holding out network ",i,", n0=",n0,"\n",sep="")
    dat<-brains[-i]
    class(dat)<-"network.list"
    #This does not, unfortunately, stop all of the annoying tracers.
    suppressMessages(seedfit<-ergmMSFit(dat ~ edges + gwesp(0.5,fixed=TRUE) + graphletCount(1) + edgecov("LogDistance") + nodematch("Hemisphere") + nodemix("Area_Extended"), prior.expectation=prior.expectation, prior.ss=n0, eval.loglik=FALSE, estimate="MPLE"))
    seedco<-coef(seedfit)
    seedco[diag(seedfit$cov)>10]<-0  #Ill-constrained values are unhelpful
    suppressMessages(locfit<-ergmMSFit(dat ~ edges + gwesp(0.5,fixed=TRUE) + graphletCount(1) + edgecov("LogDistance") + nodematch("Hemisphere") + nodemix("Area_Extended"), prior.expectation=prior.expectation, prior.ss=n0, eval.loglik=FALSE, control=control.ergm(main.method="Stochastic", MCMC.interval=thin, MCMC.burnin=burn, init=seedco)))
    sim<-simulate(locfit, nsim=cvreps, control=control.simulate.ergm(MCMC.burnin=burn, MCMC.interval=thin))
    err[i] <- mean(apply(sweep(as.sociomatrix.sna(sim), c(2,3), brains[[i]][,], "!="), 1, sum)^2)
  }
  mean(err)
}

#Use a grid search to find the optimal n0 by CV
n0<-c(0,2^(-10:3))
CVerr<-vector()
for(i in 1:length(n0))
  CVerr[i]<-RegLECV(n0[i])
tab<-cbind(formatC(n0,digits=3,format="f"), formatC(n0/(9+n0),digits=4,format="f"), formatC(CVerr,digits=3,format="f"), "\\\\\n")
cat(apply(tab,1,paste,collapse=" & "))


#Fit a regularized model with the CV-optimized n0
set.seed(1331) #So that you don't have to rerun all of the above to repro this!
seedfit<-ergmMSFit(brains ~ edges + gwesp(0.5,fixed=TRUE) + graphletCount(1) + edgecov("LogDistance") + nodematch("Hemisphere") + nodemix("Area_Extended"), prior.expectation=prior.expectation, prior.ss=n0[which.min(CVerr)], eval.loglik=FALSE, estimate="MPLE")
seedco<-coef(seedfit)
seedco[diag(seedfit$cov)>10]<-0  #Ill-constrained values are unhelpful
fitReg<-ergmMSFit(brains ~ edges + gwesp(0.5,fixed=TRUE) + graphletCount(1) + edgecov("LogDistance") + nodematch("Hemisphere") + nodemix("Area_Extended"), prior.expectation=prior.expectation, prior.ss=n0[which.min(CVerr)], eval.loglik=FALSE, control=control.ergm(main.method="Stochastic", MCMC.interval=thin, MCMC.burnin=burn, init=seedco))
tab<-cbind(formatC(coef(fitReg),digits=3,format="f"), paste0("(",formatC(diag(fitReg$cov)^0.5,digits=3,format="f"),")"), symnum(2*(1-pnorm(abs(coef(fitReg)/diag(fitReg$cov)^0.5))), corr=FALSE, cutpoints=c(0, 0.0001, 0.001, 0.01, 0.05, 0.1, 1), symbols=c("${****}$", "${***}$", "${**}$", "${*}$", ".", " ")), " \\\\\n")
cat(apply(tab,1,paste,collapse=" & "))

#Compare models
enam<-gsub("mix.Area_Extended","nodemix", names(coef(fitReg)), fixed=TRUE)
nam1<-gsub("mix.Area_Combined_large","nodemix", names(coef(fitMLE)), fixed=TRUE)
nam2<-gsub("mix.Area_Combined_large","nodemix", names(coef(fitMAP)), fixed=TRUE)
tab<-matrix("",nrow=length(enam),ncol=9)
tab[match(nam1,enam),1]<-formatC(coef(fitMLE),digits=3,format="f")
tab[match(nam1,enam),2]<-paste0("(",formatC(diag(fitMLE$cov)^0.5,digits=3,format="f"),")")
tab[match(nam1,enam),3]<-symnum(2*(1-pnorm(abs(coef(fitMLE)/diag(fitMLE$cov)^0.5))), corr=FALSE, cutpoints=c(0, 0.0001, 0.001, 0.01, 0.05, 0.1, 1), symbols=c("${****}$", "${***}$", "${**}$", "${*}$", ".", " "))
tab[match(nam2,enam),4]<-formatC(coef(fitMAP),digits=3,format="f")
tab[match(nam2,enam),5]<-paste0("(",formatC(diag(fitMAP$cov)^0.5,digits=3,format="f"),")")
tab[match(nam2,enam),6]<-symnum(2*(1-pnorm(abs(coef(fitMAP)/diag(fitMAP$cov)^0.5))), corr=FALSE, cutpoints=c(0, 0.0001, 0.001, 0.01, 0.05, 0.1, 1), symbols=c("${****}$", "${***}$", "${**}$", "${*}$", ".", " "))
tab[,7]<-formatC(coef(fitReg),digits=3,format="f")
tab[,8]<-paste0("(",formatC(diag(fitReg$cov)^0.5,digits=3,format="f"),")")
tab[,9]<-symnum(2*(1-pnorm(abs(coef(fitReg)/diag(fitReg$cov)^0.5))), corr=FALSE, cutpoints=c(0, 0.0001, 0.001, 0.01, 0.05, 0.1, 1), symbols=c("${****}$", "${***}$", "${**}$", "${*}$", ".", " "))
cat(paste0(apply(cbind(enam,tab),1,paste,collapse=" & ")," \\\\\n"))


#Save everything
save(fitMLE,fitMAP,CVerr,n0,fitReg,prior.samp,file="brain_fits.RData")
