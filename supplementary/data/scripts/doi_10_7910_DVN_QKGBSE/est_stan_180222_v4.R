plotParams <- function(post, row, col, credMass) {
    attach(mtcars)
		par(mfrow=c(row,col))
		
		mcmcMat = as.matrix(post,chains=TRUE)
		
		cols = colnames(mcmcMat)
		for ( i in 1:length(cols) ) {
				plotPost(mcmcMat[,cols[i]],xlab=cols[i], credMass=credMass)
		}
}

stan2coda <- function(fit) {
     mcmc.list(lapply(1:ncol(fit), function(x) mcmc(as.array(fit)[,x,])))
}

stanPars <- function(fit) {
		pars = fit@model_pars
		pars <-pars[pars!="dev"]
		pars <-pars[pars!="lp"]
		pars <-pars[pars!="lp__"]
		
		return ( pars )
}

stanPost <- function(fit) {
		pars = fit@model_pars
		pars <-pars[pars!="dev"]
		pars <-pars[pars!="lp"]
		pars <-pars[pars!="lp__"]
		post = subset(as.data.frame(fit),select=pars)
		
		return ( post )
}

genStan <- function(modname, dataList, warmup=1000 , iter = 2000, chains = 4, cores = 1) {
		# The Stan logistic model as a string.
		#model_string <- readLines(modname)
		
		# Compiling and producing posterior samples from the model.
		stan_sample <- stan(file = modname, data = dataList, model_name=modname , 
		            warmup=warmup , iter = iter, chains = chains, cores = cores, refresh=-1)
		
		return ( stan_sample )
}

#===== START
setwd("/Statistics/dataset03/stan")
trials <- read.csv("20180224_Legends_345.csv", header=TRUE)

# Number of trials
Ntotal = length(trials$C)

# C: Confucianism; T: Taoism; B: Buddhism; P: Popular; S: Secular
C = trials[,"C"]
T = trials[,"T"]
B = trials[,"B"]
P = trials[,"P"]
S = trials[,"S"]

#V of C: Confucianism; T: Taoism; B: Buddhism; P: Popular; S: Secular
VC = trials[,"VC"]
VT = trials[,"VT"]
VB = trials[,"VB"]
VP = trials[,"VP"]
VS = trials[,"VS"]

#AV of C: Confucianism; T: Taoism; B: Buddhism; P: Popular; S: Secular
AVC = trials[,"AVC"]
AVT = trials[,"AVT"]
AVB = trials[,"AVB"]
AVP = trials[,"AVP"]
AVS = trials[,"AVS"]

library(rstan)

#Setup datalist
dataList = list(
		C = C,
		T = T,
		B = B,
		P = P,
		S = S,
		VC = VC,
		VT = VT,
		VB = VB,
		VP = VP,
		VS = VS,
		AVC = AVC,
		AVT = AVT,
		AVB = AVB,
		AVP = AVP,
		AVS = AVS,
    N = Ntotal
  )

warmup = 500
iter = 2000
chains = 4
cores = 1

#======= model mB1
modname <- "mB1.stan"

mB1stan <- genStan( modname, dataList, 
		warmup=warmup , iter = iter, chains = chains, cores = cores)

mB1stan.post = stanPost( mB1stan )

#======= model mB2
modname <- "mB2.stan"

mB2stan <- genStan( modname, dataList, 
		warmup=warmup , iter = iter, chains = chains, cores = cores)

mB2stan.post = stanPost( mB2stan )

#======= model mB3
modname <- "mB3.stan"

mB3stan <- genStan( modname, dataList, 
		warmup=warmup , iter = iter, chains = chains, cores = cores)

mB3stan.post = stanPost( mB3stan )

#===========================
# Plotting and summarizing the posterior distribution

library(rethinking)

precis(mB1stan, pars = stanPars(mB1stan))
precis(mB2stan, pars = stanPars(mB2stan))
precis(mB3stan, pars = stanPars(mB3stan))

pars <- as.vector(rbind(stanPars(mB1stan),stanPars(mB2stan),stanPars(mB3stan)))
pars <-pars[pars!="dev"]
pars <-pars[pars!="lp"]
pars <-pars[pars!="lp__"]
pars <-unique(pars)

plot(coeftab(mB1stan, mB2stan, mB3stan), pars = pars)
