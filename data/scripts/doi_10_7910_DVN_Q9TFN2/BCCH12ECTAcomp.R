# Comparing with penalty selection method based on moderate deviation theory for
# self-normalized sums in Belloni, Chen, Chernozhukov, Hansen [2012 ECTA]
# (BCCH12) for linear model and LASSO and the corresponding method in Bickel,
# Ritov, Tsybakov [2009 Ann Statist] (BRT09), which is closely related to the
# initial penalty selection method in BCCH12.

# Clear
rm(list = ls(all.names = TRUE)) # will clear all (including hidden) objects.
invisible(gc()) #free up memory

# Packages for parallel computing, simulation and estimation
libpar <- c("foreach", "iterators", "parallel", "doParallel")
libest <- c("glmnet", "Matrix")
# If packages not already installed, install them first
lapply(append(libpar, libest), require, character.only = TRUE)

# Set DGPs
patlist <- c("sparse", "intermediate", "dense")
numpat <- length(patlist)
nvec <- c(100, 200, 400) # no. observations
numn <- length(nvec)
pvec <- nvec # no. regressors
a0 <- 0 # intercept (treated as known)
rhovec <- seq(0, .8, by = .2)
numrho <- length(rhovec) # regressor correlations
sigma_eps <- 1 # error variance (treated as unknown)

# Coefficient vector, theta
coef_vec <- function(p, pattern) {
  switch(pattern,
    "sparse" = {
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
    "dense" = {
      invsqrt2 <- 1 / sqrt(2)
      theta <- matrix(invsqrt2^(0:(p - 1)), p, 1)
    }
  )
  return(theta)
}

# Data transformations (here none - in applications: both true, typically)
standardize <- FALSE # regressors already on same scale
intercept <- FALSE # zero intercept (treated as known)

# Tuning parameters (fixed)
k <- 3 # no. folds (glmnet requires at least 3)
b <- 1000 # no. bootstraps (for BCV)
c0 <- 1.1 # score markup (note: only one value used here)
# note: probability tolerance alpha specified in loop below as a function of n
# using the Belloni, Chen, Chernozhukov, Hansen [2012 ECTA] formula

# Monte Carlo (MC) settings
cl <- makeCluster(detectCores()) # create cluster for parallel computing
registerDoParallel(cl) # register cluster
opts <- list(preschedule = TRUE) # parallel options
clusterSetRNGStream(cl, 2345) # seed (for reproducibility)
nummc <- 2000 # no. MC repetitions
metlist <- c("BCV", "BRT09", "BCCH12") # penalty selection methods
nummet <- length(metlist)
ell2_errs <- array(NA, dim = c(nummc, numrho, numn, numpat, nummet))
dimnames(ell2_errs) <- list(mc = 1:nummc, rho = rhovec,
                            n = nvec, pattern = patlist, method = metlist)

for (thispat in 1:numpat) {
  pattern <- patlist[thispat] # coefficient pattern
  for (thisn in 1:numn) {
    n <- nvec[thisn] # sample size
    p <- pvec[thisn] # no. candidate regressors
    theta <- coef_vec(p, pattern) # parameters
    s <- sum(theta != 0) # no. relevant regressors
    alpha <- .1 / log(max(n, p)) # probability tolerance (BCCH12, p. 2380)
    foldid <- rep(seq(1:k), length = n) # fold ids
    cat(sprintf("Current design: pattern=%s, n=%d, p=%d, s=%d: ",
                pattern, n, p, s))
    for (thisrho in 1:numrho) {
      rho <- rhovec[thisrho] # correlation
      results <- foreach(icount(nummc), .options.snow = opts) %dopar% {
        cat(".") # progress indicator
        source("simulations/simLinear.R", local = TRUE) # simulation tools
        source("bcvBinary.R", local = TRUE) # estimation and inference tools
        # GENERATE DATA
        data <- sim_data(n, p, rho, a0, theta)
        y <- data$y # outcome
        x <- data$x # regressors
        # ESTIMATING ALL PARAMETERS
        # Bootstrapping after cross-validation (BCV)
        bcv <- bcv_linear(x, y, nfolds = k, foldid = foldid,
                          nboot = b, c0 = c0, alpha = alpha,
                          standardize = standardize, intercept = intercept)
        # BCCH12 (resulting from self-normalization arguments)
        # Initial penalty level (as in BRT09)
        sd_y <- sd(y)
        lambda_brt <- c0 * sd_y * qnorm(1 - alpha / (2 * p)) / sqrt(n)
        # Notes: In BCCH12 notation, this "lambda" corresponds to
        # "lambda*gamma0j / (2 * n)", i.e. a mix of the penalty level
        # and loading(s).
        # We here treat both of the following as known:
        #  (i)  conditional homoskedacitiy of epsilon given X
        #  (ii) equivariance of X_j (here: 1)
        # The initial step in the BCCH12 penalization scheme (their
        # Algorithm A.1) therefore collapses to (the feasible version
        # of) that in BRT09, which makes it and "oracle" type of penalty.
        # Also, glmnet works with .5 * RSS / n as the loss (see glmnet
        # vignette), so we have divided by 2 here (and below).
        coef_brt <- coef(bcv$cv, s = lambda_brt, exact = FALSE)
        # ^- "exact = FALSE" => calculated via interpolation
        eps_brt <- y - x %*% coef_brt[-1] # implied residuals
        # Refined penalty level
        lambda_bcch <- lambda_brt * sd(eps_brt) / sd_y
        coef_bcch <- coef(bcv$cv, s = lambda_bcch, exact = FALSE)
        # ^- "exact = FALSE" => calculated via interpolation
        thats <- cbind(bcv$that, coef_brt[-1], coef_bcch[-1]) # slope estimates
        # CALCULATING L2 ERRORS
        ell2_err_vec <- matrix(sqrt(colSums(sweep(thats, 1, theta, "-")^2)),
                               1, nummet)
        ell2_err_vec # return results
      }
      # Unpacking and storing results
      ell2_errs[, thisrho, thisn, thispat, ] <- do.call(rbind, results)
      # Tracking rho progress
      if (thisrho < numrho) {
        cat(".")
      } else {
        cat(sprintf(". (OK)\n"))
      }
    } # rho loop
  } # n (and p) loop
} # pattern loop
cat(sprintf("Simulated completed.\n"))
stopCluster(cl)

do_save <- TRUE
if (do_save == TRUE) {
  filename <- sprintf("simulations/BCCH12ECTA_comparison_linear_N%d_Rho%d_R%d_B%d_K%d_intercept%s_standardize%s.Rdata",
                      numn, numrho, nummc, b, k, intercept, standardize)
  save.image(file = filename)
}
