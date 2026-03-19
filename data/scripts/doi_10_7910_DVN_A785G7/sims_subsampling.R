# Replication materials for "Messy Data, Robust Inference? Navigating Obstancles to Inference with bigKRLS"
# By: Pete Mohanty (pmohanty@stanford.edu) and Robert Shaffer (rbshaffer@utexas.edu)

#########################
# simulation parameters #
#########################
n_subsamples <- c(5, 10, 20, 56)
n_samples <- 100

###################################
# change to reflect your own path #
###################################
out_path <- '~/Dropbox/pa_replication/Appendix_C/Appendix_C.3/results_sims_subsampling.RData'

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

coverage <- function(DF, targets, estimates, se_vals, CI = 95){
  # helper function to calculate coverage
  
  biases <- targets - estimates
  CI = 1 - (100 - CI)/200
  thresholds <- se_vals * qt(CI, DF)
  return(sum(abs(biases) < thresholds))
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

# results list
summaries <- data.frame()

# run sims to generate output
for(n in n_subsamples){
  for(i in 1:n_samples){
    print(timestamp())
    print(paste('Starting iteration', i, 'with', n, 'subsamples...'))
    
    
    X_sub <- Xsmall[,1:8]
    X_hierarchical <- Xsmall[,9:17]
    
    # pick the sample (stratified by census region dumy)
    inds <- rep(NA, nrow(Xsmall))
    for(j in 1:ncol(X_hierarchical)){
      idx_loc <- (1:nrow(X_hierarchical))[X_hierarchical[,j] == 1]
      vals <- rep(1:n, floor(length(idx_loc)/n))
      vals <- c(vals, sample(1:n, size=length(idx_loc) %% n, replace=FALSE))
      
      inds[idx_loc] <- sample(vals, size=length(idx_loc), replace=F)
    }
    
    to_fit <- generate_data(X_sub, X_hierarchical, d1_val, dhierarchical_vals, sigma_complex, d2_val, d3_val, d4_val)
    
    avgderivmat <- matrix(NA, nrow=n, ncol=8)
    sdavgderivmat_corr <- matrix(NA, nrow=n, ncol=8)
    sdavgderivmat_uncorr <- matrix(NA, nrow=n, ncol=8)
    
    neff_vec <- c()
    
    # loop over subgroups, accumulate results in output matrices
    for(j in 1:n){
      print(paste('Starting model', j, 'for iteration', i, 'with', n, 'subsamples...'))
      out <- bigKRLS(to_fit[['y']][inds==j], Xsmall[inds==j,], which.derivatives = c(1:8), Ncores = 2, eigtrunc = 0)
      
      avgderivmat[j,] <- out$avgderivatives
      sdavgderivmat_corr[j,] <- sqrt(out$var.avgderivatives)
      sdavgderivmat_uncorr[j,] <- sqrt(out$var.avgderivatives)*out$Neffective/sum(inds==j)
      
      neff_vec <- c(neff_vec, sum(inds==j) - out$Neffective)
    }
    
    df_val <- nrow(Xsmall) - sum(neff_vec) - ncol(Xsmall)*n
    
    # save hypothesis testing statistics, coverage results
    tstats_corr <- colMeans(avgderivmat)/colMeans(sdavgderivmat_corr)
    tstats_uncorr <- colMeans(avgderivmat)/colMeans(sdavgderivmat_uncorr)
    
    pvals_corr <- 2 * pt(abs(tstats_corr), df_val, lower.tail = FALSE)
    pvals_uncorr <- 2 * pt(abs(tstats_uncorr), nrow(Xsmall) - ncol(Xsmall)*n, lower.tail = FALSE)
    
    coverage_vals <- list('corrected' = coverage(df_val, to_fit[['avgderiv_pop']], colMeans(avgderivmat), colMeans(sdavgderivmat_corr)),
                          'uncorrected' = coverage(nrow(Xsmall) - ncol(Xsmall)*n, to_fit[['avgderiv_pop']], colMeans(avgderivmat), colMeans(sdavgderivmat_uncorr)))
    
    # accumulate a summary table
    summaries <- rbind(summaries,
                       data.frame('type'='corrected',
                                  'coverage'=coverage_vals[['corrected']],
                                  'seed'=i, 'n_subsamples'=n),
                       data.frame('type'='uncorrected',
                                  'coverage'=coverage_vals[['uncorrected']],
                                  'seed'=i, 'n_subsamples'=n))
  }
}

save(summaries, file=out_path)
