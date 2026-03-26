######################################################################
## Implementation as bayesian model                                 ##
## Here: Go rjags to prepare parallel implementation                ##
## ---------------------------------------------------------------- ##
## ADW: Presidents, Policy Compromise and Legislative Success. JOP. ##
## June 2016                                                        ##
## Implemented in R 3.2.3 "Wooden Christmas-Tree"                   ##
######################################################################


# The Model
# negative Binomial, variance at the word level
# Thanks to Däubler and Benoit for a peak on their model code 
mnegbin.varj.jags <- function() {
	
  for(j in 1:M){
   rho[j] ~ dgamma(nu,nu)
	
   for(i in 1:N){
    Y[j,i] ~ dpois(rho[j]*lambda[j,i])
    lambda[j,i] <- exp(alpha[i] + psi[j] + beta[j]*theta[i])
   }
  }

 alpha[1] <- 0
 for(i in 2:N){
  alpha[i] ~ dnorm(0,0.04) 
 }
 
 for(i in 1:N){
  raw.theta[i]  ~ dnorm(0,1)
  theta[i] <- tmp*raw.theta[i]
 }

 for(j in 1:M){
  psi[j] ~ dnorm(0,0.04) 
  beta[j] ~ dnorm(0,0.04)
 }
 
 nu ~ dgamma(0.01,0.01) 
 tmp <- 2*(step(raw.theta[2]-raw.theta[1]))-1;
}





# Sets up mincluster (default three cores)

ep.wf.negbin.b <- function(dat, nr.chains = 3, nr.iter = 200){
	# Read in Data
	Y <- dat
	N <- length(Y[1,])
	M <- length(Y[,1])
	wfm.dat <- list(Y = Y, N = N, M = M)
	wfm.params <- c('theta')
	## set up the cluster
	cl <- makeCluster(nr.chains, type = "SOCK")
	## model initial values. 
	inits2 <- jags.fit(data = wfm.dat
		, params = wfm.params
		, model = mnegbin.varj.jags	  
		#, inits = jags.inits.nchain	# inits are automatic
		, n.chains = nr.chains
		, n.adapt = 0
		, n.update = 0
		, n.iter = 0)$state(internal = TRUE)
	## Push stuff to the nodes
	# Executes the following lines on the clusters
	clusterEvalQ(cl, library(dclone))	# loads package
	clusterEvalQ(cl, setwd(getwd()))	# sets wd
	# writes the JAGS model to the clusters
	filename <- write.jags.model(mnegbin.varj.jags)	
	# loads the data on the clusters
	cldata <<- list(data = wfm.dat, params = wfm.params, model = filename, inits = inits2)	# this is nasty coding, writing out to the global environment. I know...
	clusterExport(cl, "cldata")
	
	## This is the function that will be evaluated on each processor
	jagsparallel <- function(i, ...) {
		jags.fit(data = cldata$data, params = cldata$params, model = cldata$model,
		inits = cldata$inits[[i]], n.chains = 1, updated.model = FALSE, ...)
	}

	## now: distribute the bugger and execute three times 
	# start time monitors
	cat(paste('I started on', date()))
	tmbayes <- proc.time() 
	#==============================
	# Implement. The engine room of the function.
	res <- parLapply(cl, 1:nr.chains, jagsparallel
		, n.adapt = 1000
		, n.update = 1000
		, n.iter = nr.iter
		, thin = 50)
	#==============================
	# end time monitors
	cat(paste('I finished on', date()))
	cat("\tTime elapsed in the estimation:"
		,(proc.time()-tmbayes)[3]
		,"secs or"
		,((proc.time()-tmbayes)[3]/60)
		,"min","\n")
	#system("say Chris I am done! Come and check the awesome results!")
	return(res)
	# remove the model from the cluster
	clean.jags.model(filename)
	# close the cluster
	stopCluster(cl)
}






