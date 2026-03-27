######################################################################
##  Functions for checking convergence                              ##
## ---------------------------------------------------------------- ##
## ADW: Presidents, Policy Compromise and Legislative Success. JOP. ##
## June 2016                                                        ##
## Implemented in R 3.2.3 "Wooden Christmas-Tree"                   ##
######################################################################

# -----------------------------------------------------------------------------
# 0 Intro: This file is quite manual. The "workflow" for the posterior checks is the following:

# 1 Take one country with parallel chains from the parallelised implementation
# 2 Check chains: Are they in the same area? Or is it necessary to correct their dimension? 
# 3 If necessary multiply with -1 in order to correct
# 4 Once all chains are in the same direction, run the diagnostics
# 5 Once all checks are ok, save the dimensionality corrections for later



# -----------------------------------------------------------------------------
# 1 Take one country with parallel chains from the parallelised implementation
#dat <- arg.nb.b.80.mcmc
#dat <- bra.nb.b.80.mcmc
#dat <- bol.nb.b.80.mcmc
#dat <- chi.nb.b.80.mcmc
#dat <- col.nb.b.80.mcmc
#dat <- cri.nb.b.80.mcmc
#dat <- dom.nb.b.80.mcmc
#dat <- ecu.nb.b.80.mcmc
#dat <- gtm.nb.b.80.mcmc
#dat <- mex.nb.b.80.mcmc
#dat <- nic.nb.b.80.mcmc
# dat <- per.nb.b.80.mcmc
#dat <- pry.nb.b.80.mcmc
#dat <- slv.nb.b.80.mcmc
#dat <- ury.nb.b.80.mcmc
#dat <- ven.nb.b.80.mcmc


# -----------------------------------------------------------------------------
# 2 Check chains: Are they in the same area? Or is it necessary to correct their dimension?
denplot(dat, parms = c("theta[1]"))


# -----------------------------------------------------------------------------
# 3 If necessary multiply with -1 in order to correct
# Also: What is "left"? And what is "right"? Please correct for the respective order
#dat[[1]] <- -1*dat[[1]]
#dat[[2]] <- -1*dat[[2]]
#dat[[3]] <- -1*dat[[3]]
# add more if necessary


# -----------------------------------------------------------------------------
# 4 Once all chains are in the same direction, run the diagnostics
# Overall convergence
mcmcplot(dat, parms = c("theta"))
# Chain Mixing
gelman.diag(dat, multivariate = FALSE)	
# Checking chain length, zscore comparison of early and late chain parts
geweke.diag(dat)

raftery.diag(dat)
heidel.diag(dat)


# -----------------------------------------------------------------------------
# 5 Once all checks are ok, save the dimensionality corrections for later

#FOO.nb.b.80.mcmc[[1]] <- -1*FOO.nb.b.80.mcmc[[1]]
#FOO.nb.b.80.mcmc[[2]] <- -1*FOO.nb.b.80.mcmc[[2]]
#FOO.nb.b.80.mcmc[[3]] <- -1*FOO.nb.b.80.mcmc[[3]]
# add more if necessary
