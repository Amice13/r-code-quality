# *********************************************************************
# Replication code for Systemic Discrimination Among Large U.S. Employers
# Patrick M. Kline, Evan K. Rose, Christopher R. Walters
# April, 2022

# This code produces estimates of q-values using various methods. It 
# requires sets of p-values for each attribute produced by table8prep.R.

# It must be run in the top level of the replication archive directory.
# *********************************************************************

### Build qvalues table
library(tidyverse)
library(foreign)
library(miceadds)
library(qvalue)
library(ggplot2)

### 1) Load RI Pvalues 
race_firm_qvalues <- read.csv("dump/blackp-values.csv")
gender_firm_qvalues <- read.csv("dump/femalep-values.csv")
age_firm_qvalues <- read.csv("dump/over40p-values.csv")

### 2) Function for Armstrong CI
getTestStat <- function(n, pi0, lambda, obs = NULL) {
  
  if (is.null(obs)) {
    p_hat <- runif(n)
    
    if (pi0 == 0) {
      p_hat[1:n] <- 0
    }
    if (pi0 > 0 & pi0 < 1) {
      u <- rbinom(n, 1, pi0)
      p_hat <- p_hat * u
    }
  }
  else{
    p_hat <- obs
  }
  
  pi_hat <- rep(NA, length(lambda))
  for (l in 1:length(lambda)) {
    pi_hat[l] <- (pi0*sqrt(n * (1 - lambda[l]))) - sum(p_hat > lambda[l])/sqrt(n * (1 - lambda[l]))
  }
  
  tmax <- which.max(pi_hat)
  Tn <- pi_hat[tmax]
  arg_max <- lambda[tmax]
  out <- list(Tn = Tn, arg_max = arg_max)
  return(out)
}

armstrongCI <- function(values, lambda, nsims, alpha, pi_upper, pi_lower, discrete = FALSE) {
  
  pi0 <- 0.5*(pi_lower + pi_upper) 

  n <- length(values)
  Tstat <- getTestStat(n = n, pi0 = pi0, lambda = lambda, obs = values)
  Tn <- Tstat$Tn
  
  simTn <- rep(NA, nsims)
  max_lambdas <- rep(NA, nsims)
  for (i in 1:nsims) {
    sim <- getTestStat(n = n, pi0 = pi0, lambda = lambda, obs = NULL)
    simTn[i] <- sim$Tn
    max_lambdas[i] <- sim$arg_max
  }
  
  cn <- quantile(simTn, 1 - alpha)
  out <- list(Tn = Tn, cn = cn, pi0 = pi0, max_lambdas = max_lambdas)
  
  # Lower limit of pi0
  if (discrete) {
    lower_lim <- 0
  }
  else {
    lower_lim <- .Machine$double.eps
  }
  
  if (pi0 <= lower_lim) {
    print("lower limit on pi0 reached")
    print(paste("1 - alpha quantile =", round(cn, 4), "for alpha =", alpha))
    return(out)
  }
  else{
    # If reject the null, decrease pi0 - call armstrongCI
    if (Tn >= cn) {
      if (discrete){
        return(armstrongCI(values, lambda, nsims, alpha, pi_upper = pi0, pi_lower = (pi0 - 2/n), discrete = TRUE))
      }
      else{
        return(armstrongCI(values, lambda, nsims, alpha, pi_upper = pi0, pi_lower = pi_lower, discrete = FALSE))
      }
    }
    else {
      if (discrete) {
        print(paste(
          "failed to reject for pi0 >", round(pi0, 4), ", Tn =", round(Tn, 4), ", quantile =", round(cn, 4), "alpha =", alpha))
        return(out)
      }
      else{
        if (pi_upper - pi0 < .Machine$double.eps) {
          print(paste("search converged at pi0 =", round(pi0, 4), ", Tn =", round(Tn, 4), ", quantile =", round(cn, 4), "alpha =", alpha))
          return(out)
        }
        else{
          return(armstrongCI(values, lambda, nsims, alpha, pi_upper = pi_upper, pi_lower = pi0, discrete = FALSE))
        }
      }
    }
  }
}


### 3) Collect results
results <- NULL
frames <- list(race_firm_qvalues, gender_firm_qvalues, age_firm_qvalues)
frame_names <- list("race","gender","age")
for (i in 1:length(frames)) {
  for (method in list("bootstrap","smoother","manual","armstrong")) {
    for (pv in list("ttest_p_greater","ttest_p","job_level_ri_oneside","job_level_ri_twoside")) {
      pvals = frames[[i]][[pv]]
      if (method == "manual") {
        qobj <- qvalue(pvals, pi0=min(sum(pvals > 0.5)/((1-0.5)*length(pvals)),1))   
      } else if (method == "armstrong") {
        ci = armstrongCI(pvals, lambda=seq(0, 0.99, by=0.01), nsims=10000, alpha=0.05, pi_upper=1, pi_lower=0)
        qobj <- qvalue(pvals, pi0=ci$pi0)   
      } else {
        qobj <- qvalue(pvals, pi0.method=method)
      }
      pi0 <- qobj$pi0
      if (method == "bootstrap") {
        lambda <- qobj$lambda[[match(qobj$pi0, qobj$pi0.lambda)]]
      } else {
        lambda <- 0.5
      }
      n005 <- sum(qobj$qvalues <= 0.05)
      mlfdr005 <- mean(qobj$lfdr[qobj$qvalues <= 0.05])
      n01 <- sum(qobj$qvalues <= 0.1)
      mlfdr01 <- mean(qobj$lfdr[qobj$qvalues <= 0.1])

      res <- list(method=method,group=frame_names[[i]],
          pi0=pi0,lambda=lambda,pvals=pv,
          n005=n005,mlfdr005=mlfdr005,n01=n01,mlfdr01=mlfdr01)
      results = rbind(results,res)
    }
  }
}
write.csv(results, "dump/table8.csv")



