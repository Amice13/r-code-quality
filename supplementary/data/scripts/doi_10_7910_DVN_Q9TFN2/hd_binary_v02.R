# Script for high-dimensional binary probit simulations in Section 6 of the
# main text and Supplementary Appendix H.

# Clear
rm(list = ls(all.names = TRUE)) # will clear all (including hidden) objects.
invisible(gc()) #free up memory

# Packages for parallel computing, simulation and estimation
libpar <- c("foreach", "iterators", "parallel", "doParallel")
libest <- c("glmnet", "Matrix")
# If packages not already installed, install them first
lapply(append(libpar, libest), require, character.only = TRUE)

# Set DGPs
patlist <- c("sparse", "intermediate", "dense") # coefficient patterns
numpat <- length(patlist) # no. patterns
nvec <- c(100, 200, 400) # no. observations
numn <- length(nvec) # no. of no. of observations
pvec <- nvec # no. regressors
a0 <- 0 # intercept
rhovec <- seq(0, .8, by = .2) # correlation between regressors
numrho <- length(rhovec) # regressor correlations
link <- "probit" # link name, e.g. "logit", "probit"
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
standardize <- TRUE # regressors already on same scale
intercept <- TRUE # zero intercept, treated as known

# Tuning parameters (fixed)
k <- 3 # no. folds
b <- 1000 # no. bootstraps
c0vec <- c(1.00, 1.05, 1.10) # score markups (>1 for theoretical guarantees)
numc0 <- length(c0vec)
# Probability tolerance rules
alpha_rules <- c("bcch", "adhoc") # rules alpha=alpha(n)
numrules <- length(alpha_rules)
# notes: c0 = 1.1 is the choice of Belloni, Chernozhukov (2011 Ann Statist)
# alpha=.1/ln(max(p,n)) is employed in Belloni et al. (2012 Econometrica)

# Monte Carlo (MC) settings
RNGkind(normal.kind = "Kinderman-Ramage") # faster normal draws
cl <- makeCluster(detectCores()) # create cluster for parallel computing
registerDoParallel(cl) # register cluster
opts <- list(preschedule = TRUE) # parallel options
clusterSetRNGStream(cl, 2345) # seed (for reproducibility)
nummc <- 2000 # no. MC repetitions

# Placeholders
metlist <- c("BCV", "Post-BCV", "CV", "Post-CV")
nummet <- length(metlist)
ell2_errs <- array(NA, dim = c(nummc, numrho, numn, numpat, numc0, numrules, nummet))
dimnames(ell2_errs) <- list(mc = 1:nummc, rho = rhovec, n = nvec, pattern = patlist,
                            c0 = c0vec, alpha = alpha_rules, method = metlist)
bhats_stud <- array(NA, dim = c(nummc, numrho, numn, numpat, numc0, numrules, nummet))
dimnames(bhats_stud) <- list(mc = 1:nummc, rho = rhovec, n = nvec, pattern = patlist,
                             c0 = c0vec, alpha = alpha_rules, method = metlist)
# note: CV does not actually depend on c0 or alpha

# Main simulation loop
for (thisrule in 1:numrules) {
  rule <- alpha_rules[thisrule]
  for (thisc0 in 1:numc0) {
    c0 <- c0vec[thisc0]
    for (thispat in 1:numpat) {
      pattern <- patlist[thispat] # coefficient pattern
      for (thisn in 1:numn) {
        n <- nvec[thisn] # sample size
        p <- pvec[thisn] # no. candidate regressors
        theta <- coef_vec(p, pattern) # slopes
        beta <- theta[1] # focal parameter
        s <- sum(theta != 0) # no. relevant regressors
        # Tuning parameters (n-dependent)
        switch(rule,
          "bcch" = {
            alpha <- .1 / log(max(n, p))
          },
          "adhoc" = {
            alpha <- 10 / n
          }
        ) # ^-- probability tolerance as fctn of n
        foldid <- rep(seq(1:k), length = n) # fold ids for Step 1
        foldid2 <- foldid[sample(n)] # fold ids for Step 2
        # note: here each method uses the same sample split in a given step
        cat(sprintf("Current design: alpha=%s, c0=%3.2f, pattern=%s, n=%d, p=%d, s=%d: ",
                    rule, c0, pattern, n, p, s))
        for (thisrho in 1:numrho) {
          rho <- rhovec[thisrho]
          results <- foreach(icount(nummc), .options.snow = opts) %dopar% {
            source("simulations/simBinary.R", local = TRUE) # simulation tools
            source("bcvBinary.R", local = TRUE) # estimation and inference tools

            # GENERATE DATA
            data <- sim_data(n, p, rho, a0, theta, link)
            y <- data$y # outcome
            x <- data$x # regressors

            # ESTIMATING ALL PARAMETERS
            # Bootstrapping after cross-validation (BCV)
            bcv <- bcv_binary(x, y, link = link, nfolds = k, foldid = foldid,
                              nboot = b, c0 = c0, alpha = alpha,
                              standardize = standardize, intercept = intercept)

            # Refitting post BCV variable selection (post-BCV)
            post_bcv <- refit_binary(x, y, link = link, intr = bcv$intr,
                                     theta = bcv$that, intercept = intercept)

            # Cross-validation (CV) - already done as part of BCV
            coef_cv <- coef(bcv$cv, s = "lambda.min")
            cv <- list(intr = coef_cv[1], that = matrix(coef_cv[-1], p, 1))

            # Refitting post CV variable selection (post-CV)
            post_cv <- refit_binary(x, y, link = link, intr = cv$intr,
                                    theta = cv$that, intercept = intercept)

            thats <- cbind(bcv$that, post_bcv$that, cv$that, post_cv$that)

            ell2_err_vec <- matrix(sqrt(colSums(sweep(thats, 1, theta, "-")^2)), 1, nummet)

            # DEBIASING FOCAL PARAMETER, ESTIMATING VARIANCE AND STUDENTIZING
            bhats_stud_vec <- matrix(NA, 1, nummet)

            # Based on BCV and using BCV (no refitting) second step
            if (anyNA(bcv$that) == FALSE) {
              bcv3 <- debias_binary(x, y, link = link, intr = bcv$intr,
                                    theta = bcv$that, nfolds = k,
                                    foldid = foldid2, standardize = standardize,
                                    intercept = intercept, boot = TRUE,
                                    post = FALSE)
              # overwrite if second step succeeded; o/w keep as NA
              if (bcv3$converged == TRUE) {
                bhats_stud_vec[1, 1] <- sqrt(n / bcv3$vhat) * (bcv3$bhat - beta)
              }
            }

            # Based on post-BCV and using post-BCV in second step
            if (anyNA(post_bcv$that) == FALSE) {
              post_bcv3 <- debias_binary(x, y, link = link,
                                         intr = post_bcv$intr,
                                         theta = post_bcv$that, nfolds = k,
                                         foldid = foldid2,
                                         standardize = standardize,
                                         intercept = intercept, boot = TRUE,
                                         post = TRUE)
              # overwrite if second step succeeded; o/w keep as NA
              if (post_bcv3$converged == TRUE) {
                bhats_stud_vec[1, 2] <- sqrt(n / post_bcv3$vhat) * (post_bcv3$bhat - beta)
              }
            }

            # Based on CV and using CV (no bootstrapping, no refitting) in second step
            if (anyNA(cv$that) == FALSE) {
              cv3 <- debias_binary(x, y, link = link, intr = cv$intr,
                                   theta = cv$that, nfolds = k,
                                   foldid = foldid2, standardize = standardize,
                                   intercept = intercept, boot = FALSE,
                                   post = FALSE)
              # overwrite if second step succeeded; o/w keep as NA
              if (cv3$converged == TRUE) {
                bhats_stud_vec[1, 3] <- sqrt(n / cv3$vhat) * (cv3$bhat - beta)
              }
            }

            # Based on post-CV and using post-CV (no bootstrapping, w/ refitting) in second step
            if (anyNA(post_cv$that) == FALSE) {
              post_cv3 <- debias_binary(x, y, link = link, intr = post_cv$intr,
                                        theta = post_cv$that, nfolds = k,
                                        foldid = foldid2,
                                        standardize = standardize,
                                        intercept = intercept, boot = FALSE,
                                        post = TRUE)
              # overwrite if second step succeeded; o/w keep as NA
              if (post_cv3$converged == TRUE) {
                bhats_stud_vec[1, 4] <- sqrt(n / post_cv3$vhat) * (post_cv3$bhat - beta)
              }
            }

            list(ell2_err_vec, bhats_stud_vec) # return results as list
          } # MC loop
          # Unpacking and storing results
          ell2_errs_temp <- t(sapply(lapply(results, "[[", 1), unlist))
          ell2_errs[, thisrho, thisn, thispat, thisc0, thisrule, ] <- ell2_errs_temp
          bhats_stud_temp <- t(sapply(lapply(results, "[[", 2), unlist))
          bhats_stud[, thisrho, thisn, thispat, thisc0, thisrule, ] <- bhats_stud_temp
          # Tracking rho progress
          if (thisrho < numrho) {
            cat(".")
          } else {
            cat(sprintf(". (OK)\n"))
          }
        } # rho loop
      } # n loop
    } # pattern loop
  } # c0 loop
} # alpha rule loop
cat(sprintf("Simulated completed.\n"))
stopCluster(cl)

do_save <- TRUE
if (do_save == TRUE) {
  filename <- sprintf("simulations/simulations_results_%s_A%d_C%d_N%d_Rho%d_R%d_B%d_K%d_intercept%s_standardize%s_postCV_included.Rdata",
                      link, numrules, numc0, numn, numrho, nummc, b, k, intercept, standardize)
  save.image(file = filename)
}