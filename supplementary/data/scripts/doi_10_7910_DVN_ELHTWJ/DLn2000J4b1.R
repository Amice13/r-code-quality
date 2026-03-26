	#N=2000 designed experiments
	# J =4, b_1 = 1 

	rm(list = ls())
	install.packages(c("list", "MASS", "car", "boot", "bindata"), lib="~/R", repos="http://cran.cnr.berkeley.edu/")
	library(MASS,lib.loc="~/R")
	library(car,lib.loc="~/R")
	library(list,lib.loc="~/R")
	library(boot,lib.loc="~/R")
	library(bindata,lib.loc="~/R")
	list.version<-paste("This Monte Carlo used list v.",packageDescription(pkg="list")$Version)
	set.seed(93076)

	sims <- 2000 #number of new datasets constructed per scenario
	alpha <- 0.1
	critical <- abs(qnorm(alpha/2))
	err<- .03 #rate of measurement error 
	maxit <- 6000 #maximum number of iterations for ICT-MLE. Default in list v8.3 is 5000.
	par.treat<-c(0,1) # ~62%

	#Designed list experiments		
	sim.object.designed<-list()
	start.t<-Sys.time()#timing the MC
	start.t	
	scenario.num <- 0
	ssize<-2000 #sample sizes for each dataset
	J<-4
	ps<-c(.5,.5,.15,.85)  #marginal probs for each control item
	M <- cbind( #correlation matrix between items
		c(1,-.6,0,0),
		c(-.6,1,0,0),
		c(0,0,1,0),
		c(0,0,0,1)
		)	
	est.pop.ols<-se.pop.ols<-est.pop.ml<-se.pop.ml<-est.pop.ols.err<-se.pop.ols.err<-est.pop.ml.err<-se.pop.ml.err<-maxcount<-maxcount.err<-rep(NA, sims)
	est.treat.ml <- se.treat.ml <- est.treat.ml.err <- se.treat.ml.err <-matrix(NA, ncol = length(par.treat), nrow = sims)  
	scenario.num <- scenario.num + 1
	for (i in 1:sims) {
		print(paste("starting sim",i, "with beta=", par.treat[2],", J=",J, " and ssize=", ssize))
					## data generation
					dat <- data.frame(x=runif(ssize)) #from Blair-Imai simulation
					x <- model.matrix(~ x, data = dat)
					Y.c <- rmvbin(n=ssize,margprob=ps, bincorr=M)
					y.s <- rbinom(n = ssize, 1, inv.logit(x %*% par.treat)) #from Blair-Imai simulation
					y <- cbind(Y.c, y.s)
					y.control <- y[,1:J]
					y.control <- apply(y.control,1,sum)
					y.obs<- apply(y,1,sum)
					treat <- sample(rep(c(0,1), ssize/2), size = ssize, replace = FALSE) #from Blair-Imai simulation
					y.obs[treat==0]<-y.control[treat==0]
					y.err<-y.obs
					samps<-sample(1:ssize, ceiling(err*ssize), replace=FALSE) #random error
					samps.trt<-samps[treat[samps]==1]
					samps.cntrl<-samps[treat[samps]==0]
					y.err[samps.trt]<- J+1  #top biased error
					y.err[samps.cntrl]<- J
					maxcount[i]<-sum(y.obs==J+1)
					maxcount.err[i]<-sum(y.err==J+1)
					rownames(x) <- NULL	
					simdata <- data.frame(x=x, y.obs, y.err, treat)
					names(simdata) <- c("(Intercept)","x","y", "y.err", "treat")

					#analysis of dataset
					print("...starting ols")				
					ols.fit<-lm(y~treat, data=simdata)
					est.pop.ols[i]<-coef( ols.fit)[2]
					se.pop.ols[i]<-sqrt(diag(hccm(ols.fit)))[2]
					ols.fit<-lm(y.err~treat, data=simdata)
					est.pop.ols.err[i]<-coef(ols.fit)[2]
					se.pop.ols.err[i]<-sqrt(diag(hccm(ols.fit)))[2]

					print("...starting ml")
					tryCatch({ 
						sim.ml <- ictreg(y~x, J= J, data = simdata, method = "ml", maxIter=maxit)
						est.treat.ml[i, ] <- sim.ml$par.treat
						se.treat.ml[i, ] <- sim.ml$se.treat
						ict<-predict(sim.ml, avg=T, se.fit=T)
						est.pop.ml[i]<-ict$fit
						se.pop.ml[i]<-ict$se.fit
					},
					#warning = function(w){cat("WARN :",conditionMessage(w), "\n")},
					error=function(e){cat("ERROR :",conditionMessage(e), "\n")}
					)
					print("...starting ml error")
					tryCatch({
						sim.ml <- ictreg(y.err~x, J= J, data = simdata, method = "ml", maxIter=maxit)
						est.treat.ml.err[i, ] <- sim.ml$par.treat
						se.treat.ml.err[i, ] <- sim.ml$se.treat
						ict<-predict(sim.ml, avg=T, se.fit=T)
						est.pop.ml.err[i]<-ict$fit
						se.pop.ml.err[i]<-ict$se.fit
					},
					#warning = function(w){cat("WARN :",conditionMessage(w), "\n")},
					error=function(e){cat("ERROR :",conditionMessage(e), "\n")}
					)
		}
				lb.treat.ml <- est.treat.ml - se.treat.ml*critical
				ub.treat.ml <- est.treat.ml + se.treat.ml*critical
				lb.treat.ml.err <- est.treat.ml.err - se.treat.ml.err*critical
				ub.treat.ml.err <- est.treat.ml.err + se.treat.ml.err*critical
				cov.treat.ml <- cov.treat.ml.err<- matrix(NA, ncol = length(par.treat), nrow = sims)
				for(i in 1:sims){
					for(j in 1:length(par.treat)){
						cov.treat.ml[i,j] <- ((lb.treat.ml[i,j] < par.treat[j]) & (par.treat[j] < ub.treat.ml[i,j]))
						cov.treat.ml.err[i,j] <- ((lb.treat.ml.err[i,j] < par.treat[j]) & (par.treat[j] < ub.treat.ml.err[i,j]))
					}
				}
				sim.object.designed[[scenario.num]]<- list( #3x2x2 MC; total of 12 scenarios
				count.in.max = maxcount,
				count.in.max.err = maxcount.err,
				est.pop.ols= est.pop.ols,
				se.pop.ols= se.pop.ols,
				est.pop.ols.err= est.pop.ols.err,
				se.pop.ols.err= se.pop.ols.err,
				est.pop.ml= est.pop.ml,
				se.pop.ml=se.pop.ml,
				est.treat.ml=est.treat.ml, 
				se.treat.ml=se.treat.ml, 
				cov.treat.ml=cov.treat.ml, 
				ub.treat.ml=ub.treat.ml, 
				lb.treat.ml=lb.treat.ml,
				est.pop.ml.err= est.pop.ml.err,
				se.pop.ml.err=se.pop.ml.err,
				est.treat.ml.err=est.treat.ml.err, 
				se.treat.ml.err=se.treat.ml.err, 
				cov.treat.ml.err=cov.treat.ml.err, 
				ub.treat.ml.err=ub.treat.ml.err, 
				lb.treat.ml.err=lb.treat.ml.err, 
				par.treat=par.treat,
				J=J,
				ssize=ssize
				)
	end.t<-Sys.time()#timing the MC
	mc.time<-difftime(end.t,start.t)
	save(sim.object.designed, mc.time, list.version, file="PAsimulations_DLn2000J4b1.RData")
