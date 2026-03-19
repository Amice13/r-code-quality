######################################################################
##  Implements functions for combining the bayes estimates with     ##
##  the existing data                                               ##
## ---------------------------------------------------------------- ##
## ADW: Presidents, Policy Compromise and Legislative Success. JOP. ##
## June 2016                                                        ##
## Implemented in R 3.2.3 "Wooden Christmas-Tree"                   ##
######################################################################


# ----------------------------------------------------------------------------------------
# Functions

# Credible Intervals
lowerbound <- function(dat){
	quantile(dat, .025)	
}
upperbound <- function(dat){
	quantile(dat, .975)	
}


# Writing the Data
dat.a.bayes.out <- function(mcmcdat, datout){
	# Gets all the indices for the correct thetas on the base of the name
	# Collapse multiple chains
	mcmcdat <- as.matrix(mcmcdat) 	# This works to collapse multiple chains. Check for a single chain!
	allthetas <- grepl('theta',attr(mcmcdat, "dimnames")[[2]])
	# this is a function that takes a vector (from apply) and calculates zscores for each element on the basis of the vector
	z.vec <- function(data){
		(data-mean(data))/sd(data)
	}
	# This apply takes the data and applies the function z.vec to each row
	z.theta <- t(apply(mcmcdat[,allthetas], 1, z.vec))
	theta <- mcmcdat[,allthetas]
	## Output: Collapse values 
	# thetas themselves
	datout$position.b <- rev(apply(theta, 2, mean))
	datout$se.up.b <- rev(apply(theta, 2, upperbound))
	datout$se.lb.b <- rev(apply(theta, 2, lowerbound))
	datout$move.b <- rev(c(NA,diff(apply(theta, 2, mean))))	
	datout$move.b[datout$new.pres == TRUE] <- NA			# Correct for the new presidents	
	datout$abs.move.b <- abs(datout$move.b)
	
	# z scores for thetas
	datout$position.b.z <- rev(apply(z.theta, 2, mean))
	datout$se.up.b.z <- rev(apply(z.theta, 2, upperbound))
	datout$se.lb.b.z <- rev(apply(z.theta, 2, lowerbound))
	datout$move.b.z <- rev(c(NA,diff(apply(z.theta, 2, mean))))	
	datout$move.b.z[datout$new.pres == TRUE] <- NA			# Correct for the new presidents	
	datout$abs.move.b.z <- abs(datout$move.b.z)
	return(datout)
}





# ----------------------------------------------------------------------------------------
# Implementation
dat.pres.pos.arg80 <- dat.a.bayes.out(arg.nb.b.80.mcmc, dat.pres.pos.arg80)
dat.pres.pos.bra80 <- dat.a.bayes.out(bra.nb.b.80.mcmc, dat.pres.pos.bra80)
dat.pres.pos.chi80 <- dat.a.bayes.out(chi.nb.b.80.mcmc, dat.pres.pos.chi80)
dat.pres.pos.col80 <- dat.a.bayes.out(col.nb.b.80.mcmc, dat.pres.pos.col80)
dat.pres.pos.cri80 <- dat.a.bayes.out(cri.nb.b.80.mcmc, dat.pres.pos.cri80)
dat.pres.pos.ecu80 <- dat.a.bayes.out(ecu.nb.b.80.mcmc, dat.pres.pos.ecu80)
dat.pres.pos.gtm80 <- dat.a.bayes.out(gtm.nb.b.80.mcmc, dat.pres.pos.gtm80)
dat.pres.pos.mex80 <- dat.a.bayes.out(mex.nb.b.80.mcmc, dat.pres.pos.mex80)
dat.pres.pos.per80 <- dat.a.bayes.out(per.nb.b.80.mcmc, dat.pres.pos.per80)
dat.pres.pos.pry80 <- dat.a.bayes.out(pry.nb.b.80.mcmc, dat.pres.pos.pry80)
dat.pres.pos.slv80 <- dat.a.bayes.out(slv.nb.b.80.mcmc, dat.pres.pos.slv80)
dat.pres.pos.ury80 <- dat.a.bayes.out(ury.nb.b.80.mcmc, dat.pres.pos.ury80)
dat.pres.pos.ven80 <- dat.a.bayes.out(ven.nb.b.80.mcmc, dat.pres.pos.ven80)




