# Replication materials for "Messy Data, Robust Inference? Navigating Obstancles to Inference with bigKRLS"
# By: Pete Mohanty (pmohanty@stanford.edu) and Robert Shaffer (rbshaffer@utexas.edu)

#########################
# simulation parameters #
#########################
sample_sizes <- c(250, 500, 750, 1000)
n_samples <- 100

###################################
# change to reflect your own path #
###################################
out_path <- '~/Dropbox/pa_replication/Appendix_C/Appendix_C.1-2/results_sims_popsd.RData'

# data used for simulations
load("~/Dropbox/pa_replication/Appendix_C/Xsmall.RData")

library(dplyr)
library(bigKRLS)

pick_effects <- function(k, prop_picked = 0.5){
  # helper function to randoly select which effects will be non-zero
  d_picked <- rep(1, round(k*prop_picked))
  d_picked <- c(d_picked, rep(0, k-length(d_picked)))
  d_picked <- sample(d_picked, replace=F)
  
  return(d_picked)
}

coverage <- function(stat, N, target, bigKRLS_out, CI = 95){
  # helper function to calculate coverage
  
  biases <- target - apply(bigKRLS_out$derivatives, 2, stat)
  se <- sqrt(bigKRLS_out$var.avgderivatives)
  # corrects standard errors if N != Neffective
  se <- se * nrow(bigKRLS_out$X)/N
  CI = 1 - (100 - CI)/200
  thresholds <- se * qt(CI, N - ncol(bigKRLS_out$X))
  return(sum(abs(biases) < thresholds))
  
}

summary_noprint <- function (object, correctP = NULL, probs = c(0.05, 0.25, 0.5, 
                                                                0.75, 0.95), digits = 4, labs = NULL, ...) 
{
  # no-print summary() function - used to get CIs more easily
  
  if (class(object) != "bigKRLS") {
    warning("Object not of class 'bigKRLS'")
    UseMethod("summary")
    return(invisible(NULL))
  }
  cat("\n\nMODEL SUMMARY:\n\n")
  cat("N:", nrow(object$X), "\n")
  if (!is.null(correctP)) {
    stopifnot(correctP %in% c("acf", "eigen"))
    if (correctP == "eigen") {
      n <- object$Neffective.eigen
    }
    if (correctP == "acf") {
      n <- object$Neffective.acf
    }
    cat("N Effective:", n, "\n")
  }
  else {
    n <- nrow(object$X)
  }
  p <- ncol(object$X)
  cat("R2:", round(object$R2, digits), "\n")
  if (is.null(object$derivatives)) {
    cat("\nrecompute with bigKRLS(..., derivative = TRUE) for estimates of marginal effects\n")
    return(invisible(NULL))
  }
  if (!is.null(labs)) {
    stopifnot(length(labs) == p)
    colnames(object$X) <- labs
  }
  else {
    if (is.big.matrix(object$X)) 
      options(bigmemory.allow.dimnames = TRUE)
    colnames(object$X) <- object$xlabs
  }
  cat("R2AME**:", round(object$R2AME, digits), "\n\n")
  if (is.null(object$which.derivatives)) {
    object$which.derivatives <- 1:p
  }
  est <- object$avgderivatives
  se <- sqrt(object$var.avgderivatives)
  if (!is.null(correctP)) {
    se <- se * nrow(object$X)/n
  }
  tval <- est/se
  pval <- 2 * pt(abs(tval), n - p, lower.tail = FALSE)
  AME <- t(rbind(est, se, tval, pval))
  colnames(AME) <- c("Estimate", "Std. Error", "t value", "Pr(>|t|)")
  rownames(AME) <- colnames(object$X)[object$which.derivatives]
  if (sum(object$binaryindicator[object$which.derivatives]) > 
      0) {
    tmp <- rownames(AME)[object$binaryindicator[object$which.derivatives]]
    rownames(AME)[object$binaryindicator[object$which.derivatives]] <- paste(tmp, 
                                                                             "*", sep = "")
  }
  #cat("Average Marginal Effects:\n\n")
  #print(round(AME, digits))
  #cat("\n\nPercentiles of Marginal Effects:\n\n")
  deriv <- object$derivatives[]
  qderiv <- t(apply(deriv, 2, quantile, probs = probs, na.rm = TRUE))
  rownames(qderiv) <- rownames(AME)
  #print(round(qderiv, digits))
  #if (sum(object$binaryindicator) > 0) {
  #  cat("\n(*) Reported average and percentiles of dy/dx is for discrete change of the dummy variable from min to max (usually 0 to 1)).\n\n")
  #}
  #cat("\n(**) Pseudo-R^2 computed using only the Average Marginal Effects.")
  #if (length(object$which.derivatives) != ncol(object$X)) 
  #  cat(" NOTE: If only a subset of marginal effects were estimated, Pseudo-R^2 calculated with that subset.")
  #cat("\n\n")
  #if ("Neffective" %in% names(object)) 
  #  cat("p values calculated with N Effective.\n")
  #cat("\nYou may also wish to use predict() for out-of-sample forecasts or shiny.bigKRLS() to interact with results. Type vignette(\"bigKRLS_basics\") for sample syntax. Use save.bigKRLS() to store results and load.bigKRLS() to re-open them.\n\n")
  ans <- list(ttests = AME, percentiles = qderiv)
  class(ans) <- "summary.bigKRLS"
  return(invisible(ans))
}

generate_data <- function(Z, ZH, b1, bh, sigma_val, b2 = NULL, b3 = NULL, b4 = NULL){
  # function to generate data and accumulate population AMEs based on given parameter values
  # 
  # Args:
  # Z: input dataset
  # ZH: hierarchical group membership dataset
  # b1: linear coefficient
  # bh: hierarchical coefficients
  # sigma_val: error term SD
  # b2: second-order polynomial effect. 0 if NULL.
  # b3: third-order polynomial effect. 0 if NULL.
  # b4: fourth-order polynomial effect. 0 if NULL.
  
  if(ncol(Z) != length(b1) | ncol(Z) != nrow(bh)){
    stop('Be sure to input as many coefficients as columns of Z!')
  }
  
  if(ncol(ZH) != ncol(bh)){
    stop('Be sure there are as many rows in bh as columns of ZH!')
  }
  
  if(is.null(b2)){
    b2 <- rep(0, length(b1))
  }
  
  if(is.null(b3)){
    b3 <- rep(0, length(b1))
  }
  
  if(is.null(b4)){
    b4 <- rep(0, length(b1))
  }
  
  avgderiv_pop <- c()
  
  # container for y
  y <- rep(0, nrow(Z))
  
  for(j in 1:ncol(Z)){
    y <- y + b1[j]*Z[,j] + b2[j]*Z[,j]^2 + b3[j]*Z[,j]^3 + b4[j]*Z[,j]^4 + ZH %*% bh[j,]*Z[,j]
    avgderiv_pop <- c(avgderiv_pop, mean(b1[j] + b2[j]*2*Z[,j] + b3[j]*3*Z[,j]^2 + b4[j]*4*Z[,j]^3 + ZH %*% bh[j,]))
  }
  
  eps <- rnorm(nrow(X_sub), sd=sigma_val)
  y <- y + eps
  
  return(list('y' = y, 'avgderiv_pop' = avgderiv_pop))
}

generate_output <- function(bigKRLS_obj, to_fit_obj, type){
  # helper function to format the output. Returns coverage, summary of output and some parameter values necessary for later analysis.
  n <- nrow(bigKRLS_obj$X)
  
  neff <- n - sum(bigKRLS_obj$K.eigenvalues/(bigKRLS_obj$K.eigenvalues + bigKRLS_obj$lambda))
  bigKRLS_obj$Neffective.eigen <- neff
  
  # small hack to use summary() for corrected sample size
  s_corrected <- summary_noprint(bigKRLS_obj, correctP='eigen')
  s_uncorrected <- summary_noprint(bigKRLS_obj)
  
  coverage_vals <- list('corrected' = coverage(mean, neff, to_fit_obj[['avgderiv_pop']], bigKRLS_obj),
                        'uncorrected' = coverage(mean, n, to_fit_obj[['avgderiv_pop']], bigKRLS_obj))
  
  return(list('avgderiv_sample' = bigKRLS_obj$avgderivatives,
              'se_corr' = s_corrected$ttests[,2],
              'se_uncorr' = s_uncorrected$ttests[,2],
              'N'=nrow(bigKRLS_obj$X),
              'avgderiv_pop'=to_fit_obj[['avgderiv_pop']],
              'lambda'=bigKRLS_obj$lambda,
              'Neffective'=neff,
              'coverage'=coverage_vals,
              'Type'=type))
}

set.seed(12345)

# select effects for inclusion (here we just include all effects)
d1_incl <- pick_effects(8, prop_picked = 1)
d2_incl <- pick_effects(8, prop_picked = 1)
d3_incl <- pick_effects(8, prop_picked = 1)
d4_incl <- pick_effects(8, prop_picked = 1)
dhierarchical_incl <- pick_effects(9, prop_picked = 1)

# save SDs for each variable
x_sd <- sapply(1:8, function(i){sd(Xsmall[,i])})

# select parameter values
d1_val <- d1_incl*runif(8, max=2, min=0)/(x_sd)
d2_val <- d1_incl*d2_incl*runif(8, max=4, min=-4)/(x_sd^2)
d3_val <- d1_incl*d3_incl*runif(8, max=4, min=-4)/(x_sd^3)
d4_val <- d1_incl*d4_incl*runif(8, max=4, min=-4)/(x_sd^4)
dhierarchical_vals <- dhierarchical_incl*matrix(runif(9*8, min=-2, max=2), nrow=8)
sigma_simple <- sigma_complex <- 3000

to_save <- list()

# run sims to generate output
for(n in sample_sizes){
  for(i in 1:n_samples){
    print(paste('Starting iteration ', i, ' for sample size ', n, '...', sep=''))
    
    # pick the sample, partition into hierarchical and non-hierarchical
    idx <- sample(nrow(Xsmall), n, replace=F)
    X <- Xsmall[idx,]
    X_sub <- X[,1:8]
    X_hierarchical <- X[,9:17]
    
    # container to accumulate effects
    mean_effects <- c()
    
    simple_to_fit <- generate_data(X_sub, X_hierarchical, d1_val, dhierarchical_vals, sigma_simple)
    complex_to_fit <- generate_data(X_sub, X_hierarchical, d1_val, dhierarchical_vals, sigma_complex, d2_val, d3_val, d4_val)
    
    simple_out <- bigKRLS(simple_to_fit[['y']], X, which.derivatives = c(1:8), eigtrunc=0, Ncores = 2)
    complex_out <- bigKRLS(complex_to_fit[['y']], X, which.derivatives = c(1:8), eigtrunc=0, Ncores = 2)
    
    to_save[[length(to_save) + 1]] <- generate_output(simple_out, simple_to_fit, 'simple')
    to_save[[length(to_save) + 1]] <- generate_output(complex_out, complex_to_fit, 'complex')
  }
}

save(to_save, file=out_path)
