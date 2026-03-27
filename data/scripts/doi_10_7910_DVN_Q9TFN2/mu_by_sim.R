# Clear
rm(list = ls(all.names = TRUE)) # will clear all (including hidden) objects.
invisible(gc()) #free up memory

# Set DGPs
patlist <- c("exactly sparse", "intermediate", "approximately sparse")
numpat <- length(patlist)
nsim <- 1e+05 # no. observations
p <- 100 # no. regressors
a0 <- 0 # intercept
rhovec <- seq(0, .8, by = .2)
numrho <- length(rhovec) # regressor correlations
link <- "probit" # link function
# Coefficient vector, theta
coef_vec <- function(p, pattern) {
  switch(pattern,
    "exactly sparse" = {
      s <- 2
      theta <- matrix(cbind(c(rep(1, s), rep(0, p - s))), p, 1)
    },
    "intermediate" = {
      s <- 5
      theta <- matrix(NA, p, 1)
      invsqrt2 <- 1 / sqrt(2)
      theta[1:s, ] <- invsqrt2^(0:(s - 1))
      if (s < p) {
        theta[(s + 1):p, ] <- 0
      }
    },
    "approximately sparse" = {
      invsqrt2 <- 1 / sqrt(2)
      theta <- matrix(invsqrt2^(0:(p - 1)), p, 1)
    }
  )
  return(theta)
}

# Calculating mu (and intercept) for each rho and pattern via simulation
source("simulations/simBinary.R")
set.seed(2345) # seed (for reproducibility)
intrs <- array(NA, dim = c(numrho, numpat))
dimnames(intrs) <- list(rho = rhovec, pattern = patlist)
mus <- array(NA, dim = c(p - 1, numrho, numpat))
dimnames(mus) <- list(j = 1:(p - 1), rho = rhovec, pattern = patlist)
intercept <- TRUE
for (thispat in 1:numpat) {
  pattern <- patlist[thispat] # coefficient pattern
  theta <- coef_vec(p, pattern) # coefficient vector
  for (thisrho in 1:numrho) {
    rho <- rhovec[thisrho] # regressor correlation
    res <- sim_mu(nsim, p, rho, a0, theta, link, intercept = intercept)
    if (intercept) {
      intrs[thisrho, thispat] <- res$intr
    }
    mus[, thisrho, thispat] <- res$mu
  }
}
do_save <- TRUE
if (do_save) {
  file_name <- sprintf("simulations/mu_sims_S%s_p%d.Rdata", nsim, p)
  save.image(file_name)
}