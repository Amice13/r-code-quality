## Adaptive Experimental Design: Prospects and Applications in Political Science
## Code for utility functions

# install.packages(c('tidyverse', 'randomizr', 'reshape2', 'bandit', 'estimatr'))

library(tidyverse)
library(randomizr)
library(reshape2)
library(bandit)
library(estimatr)

# Function to generate simulations with no augmented control
# `sim_out` produces a list of matrices, where rows are periods and columns arms
# list content is:
# nmat: cumulative sample assigned
# xmat: cumulative successes
# ppmat: posterior probability of being best
sim_out <- function(n = 100, periods = 10, arms = 9, probs, static = FALSE, 
                    control = FALSE, first = NA){
  
  # account for first batch size if not all equal
  if(is.na(first)){
    first <- n
  } else {
    n <- (n*periods-first)/(periods-1)
  }
  
  xmat <- nmat <- ppmat <- matrix(NA, ncol = arms, nrow = periods)
  
  i <- 1 
  nmat[i,] <- table(simple_ra(N = first, prob_each = rep(1/arms, arms)))
  
  # outcomes: sample from a binomial distribution according to each arm's true p
  xmat[i,] <- mapply(rbinom, n = 1, size = nmat[i,], prob = probs)
  # use x, n, to get posterior probability
  ppmat[i,] <- best_binomial_bandit_sim(xmat[i,],nmat[i,])
  
  # For subsequent arms, sampling is proportionate to posterior probability
  if(periods>i){
  repeat{
    i <-  i+1
    if(static == TRUE){
      newvals <- table(simple_ra(N = n, prob_each = rep(1/arms, arms)))
    } else {
      newvals <- table(simple_ra(N = n, prob_each = ppmat[(i-1),]))      
    }
    nmat[i,] <- nmat[(i-1),] + newvals
    xmat[i,] <- xmat[(i-1),] + mapply(rbinom, n = 1, size = newvals,
                                      prob = probs)
    ppmat[i,] <- best_binomial_bandit_sim(xmat[i,], nmat[i,])
    
    if (i == periods) break
  }}
  out <- list(nmat = nmat, xmat = xmat, ppmat = ppmat)
  return(out)
}


# Function to sample according to control-augmented algorithm
ca_sampling_probs <- function(xvec, # success vector
                              nvec, # trials vector
                              cn = 3, # control index
                              n # batch size
                              ){
  # posterior probabilities
  pn <- best_binomial_bandit(xvec, nvec)
  
  # sampling probabilities
  sp <- rep(NA, length(xvec))
  
  # find difference between control arm and best arm
  cd <- nvec[which.max(pn)] - nvec[cn]
  
  # proportion of batch needed for control to match best mean count
  # (maxes out at 90%)
  pp <- min(max(cd/n, 0), .9)
  
  # control is updated to match best arm, remaining control is fixed at 1/k
  sp[cn] <- 1/length(xvec)*(1-pp) + pp
  
  # rest of treatment is thompson sampling, standardized
  sp[-cn] <- pn[-cn]/sum(pn[-cn])*((length(xvec) - 1)/length(xvec))*(1-pp)

  return(sp)    
}




# Function to generate simulations with an augmented control condition
# `sim_out_te` produces a similar list to `sim_out`, with the addition of
# spmat: sampling probability
sim_out_te <- function(n = 100, periods = 10, arms = 9, probs, static = FALSE,
                       control = 3, # identify index of control arm
                       first = NA
){
  
  if(is.na(first)){
    first <- n
  } else {
    n <- (n*periods-first)/(periods-1)
  }
  
  xmat <- nmat <- ppmat <- spmat <- matrix(NA, ncol = arms, nrow = periods)
  
  i <- 1 
  nmat[i,] <- table(simple_ra(N = first, prob_each = rep(1/arms, arms)))
  
  # outcomes: sample from a binomial distribution according to each arm's true p
  xmat[i,] <- mapply(rbinom, n = 1, size = nmat[i,], prob = probs)
  # use x, n, to get posterior probability
  ppmat[i,] <- best_binomial_bandit_sim(xmat[i,], nmat[i,])
  # treatment assignment
  spmat[i,] <- 1/arms
  
  # For subsequent arms, sampling is proportionate to posterior probability
  # With changes to control
  if(periods>i){
  repeat{
    i <-  i+1
    if(static == TRUE){
      newvals <- table(simple_ra(N = n, prob_each = rep(1/arms, arms)))
      spmat[i,] <- 1/arms
    } else {
      # sampling probabilities
      spmat[i,] <- ca_sampling_probs(xvec = xmat[(i-1),], 
                                     nvec = nmat[(i-1),], 
                                     cn = control, n = n)
      
      newvals <- table(simple_ra(N = n, prob_each = spmat[i,]))
    }
    nmat[i,] <- nmat[(i-1),] + newvals
    xmat[i,] <- xmat[(i-1),] + mapply(rbinom, n = 1, size = newvals,
                                      prob = probs)
    ppmat[i,] <- best_binomial_bandit_sim(xmat[i,], nmat[i,])
    
    if (i == periods) break
  }}
  out <- list(nmat = nmat, xmat = xmat,
              ppmat = ppmat, spmat = spmat, control = control)
  return(out)
}

# Function to generate simulations under Randomization Inference
sim_out_RI <- function(Y, Z, periods = 10, static = FALSE, control = FALSE){
  # cut Y into approximately equally sized periods
  ii <- cumsum(table(cut_interval( c(1:length(Y)), 10)))
  i <- 1 
  n <- ii[i]
  index <- 1:ii[i]
  arms <- length(Z)
  xmat <- nmat <- ppmat <- spmat <- matrix(NA, ncol = arms, nrow = periods)
  
  # assign number of treatment to each condition
  nmat[i,] <- table(simple_ra(N = n, prob_each = rep(1/arms, arms)))
  
  # generate and shuffle z vector
  zz <- sample(rep(Z, nmat[i,]))
  
  # outcomes: sample from Y
  xmat[i,] <- Y[index] %>% 
    split(zz) %>% 
    lapply(sum) %>% 
    unlist()
  # use x, n, to get posterior probability
  ppmat[i,] <- best_binomial_bandit_sim(xmat[i,], nmat[i,])
  # treatment assignment
  spmat[i,] <- 1/arms
  
  # For subsequent arms, sampling is proportionate to posterior probability
  # With changes to control
  if(periods>i){
    repeat{
      i <-  i+1
      index <- (ii[i-1]+1):ii[i]
      n <- ii[i]-ii[i-1]
      if(static == TRUE){ # static has no changes to sampling
        newvals <- table(simple_ra(N = n, prob_each = rep(1/arms, arms)))
        spmat[i,] <- 1/arms
      } else {
        if(control == FALSE){ # regular adaptive samples based on posterior
          newvals <- table(simple_ra(N = n, prob_each = ppmat[(i-1),]))
        } else { # control agumented uses alternative algorithm
          spmat[i,] <- ca_sampling_probs(xvec = xmat[(i-1),], 
                                         nvec = nmat[(i-1),], 
                                         cn = control, n = n)
          
          newvals <- table(simple_ra(N = n, prob_each = spmat[i,])) 
        }
      }
      nmat[i,] <- nmat[(i-1),] + newvals
      zz <- sample(rep(Z, newvals))
      
      xmat[i,] <- xmat[(i-1),] + Y[index] %>% 
        split(zz) %>% 
        lapply(sum) %>% 
        unlist()
      
      ppmat[i,] <- best_binomial_bandit_sim(xmat[i,], nmat[i,])
      
      if (i == periods) break
    }}
  
  # only return sampling probability separate from posterior probability if
  # using control-augmented algorithm
  if(control == FALSE){
    out <- list(nmat = nmat, xmat = xmat, ppmat = ppmat)
  }else{
    out <- list(nmat = nmat, xmat = xmat,
                ppmat = ppmat, spmat = spmat, control = control)    
  }
  return(out)
}


# Function for ipw estimation within simulations
ipw_est <- function(vals, static=FALSE, se_type='HC2'){
  
  bbmat <- melt(rbind(vals$nmat[1,],
                      diff(vals$nmat)))
  bbmat$success <- melt(rbind(vals$xmat[1,],
                              diff(vals$xmat)))$value

  yvals <- as.vector(unlist(apply(bbmat, 1, function(x)
    c(rep(0, x['value']-x['success']), rep(1, x['success']) ))))
  
  zvals <- as.factor(unlist(apply(bbmat, 1, function(x) 
    c(rep(x['Var2'], x['value'])))))
  
  if( (static == TRUE) & (length(is.finite(vals$control))==0) ){ # static, no control
    
    wvals <- rep(1/length(unique(zvals)), length(yvals))
    lmfit <- lm_robust(yvals ~ -1 + zvals, weights = 1/wvals, se_type = se_type)
    
  } else if( (static == TRUE) & (length(is.finite(vals$control))>0) ){ # static with control
    
    wvals <- rep(1/length(unique(zvals)), length(yvals))
    zvals <- relevel(zvals, ref = as.character(vals$control))
    lmfit <- lm_robust(yvals ~ zvals, weights = 1/wvals, se_type = se_type)
    
  } else if( (static == FALSE) & (length(is.finite(vals$control))==0) ){ # adaptive, no control
    
    bbmat$weight <- melt(rbind(rep(1/ncol(vals$ppmat), ncol(vals$ppmat)),
                               vals$ppmat[1:( nrow(vals$ppmat)-1),]))$value
    wvals <- unlist(apply(bbmat, 1, function(x) 
      c(rep(x['weight'], x['value']))))

    lmfit <- lm_robust(yvals ~ -1 + zvals, weights = 1/wvals, se_type = se_type)
    
  } else { # adaptive with control

    zvals <- relevel(zvals, ref = vals$control)  
    bbmat$weight <- melt(vals$spmat)$value
    wvals <- unlist(apply(bbmat, 1, function(x) 
      c(rep(x['weight'], x['value']))))  
    
    lmfit <- lm_robust(yvals ~ zvals, weights = 1/wvals, se_type = se_type)
  }
  return(lmfit)
}



# Function to iterate simulations
mysims <- function(probs = NA, control = FALSE, static = FALSE, 
                   periods = 10, n = 100, iter = 1000, 
                   # arguments for randomization inference
                   RI = FALSE, Y = NA, Z = NA, first = NA){
  outmat <- matrix(rep(NA, (11+periods)*iter), ncol = (11+periods))
  
  colnames(outmat) <- c('correct', 'posterior_best', 'bias', 'rmse_best', 
                        'rmse_te', 'cov_best', 'cov_te', 'ses', 'F', 'p',
                        0:periods)
  
  K <- ifelse(RI, length(Z), length(probs))
  if(RI) { probs <- rep(1/mean(Y), K) }
  
  outmat[,'0'] <- 1/K

  cat('iteration: ')
  for( i in 1:iter){
    
    if(control==FALSE){ # no augmented control
      if(RI){
        vals <- sim_out_RI(Y, Z, periods = periods, static = static)
        true_val <- mean(Y)
        true_vali <- 'zvals1'
      } else {
        vals <- sim_out(probs = probs, arms = K, static = static, 
                        periods = periods, n = n, first = first)
        true_val <- max(probs)
        true_vali <- paste0('zvals', which.max(probs)) # index of max probs
      }
      lm_r <- ipw_est(vals, static)
      
      # rmse
      outmat[i,'rmse_best'] <- sqrt((coef(lm_r)[true_vali]-true_val)^2)
      # coverage
      outmat[i, 'cov_best'] <- 1 * ((lm_r[['conf.low']][true_vali] < true_val) &
                                 (lm_r[['conf.high']][true_vali] > true_val))
    } else {
      if(RI){
        vals <- sim_out_RI(Y, Z, periods = periods, static = static, 
                           control = control)
        true_val <- 0
        true_vali <- 'zvals1'
      } else {
      vals <- sim_out_te(probs = probs, arms = K, static = static, 
                         control = control, periods = periods, n = n, 
                         first = first)
      true_val <- max(probs)-probs[control] 
      true_vali <- paste0('zvals', which.max(probs)) # index of max probs
      }
      
      lm_r <- ipw_est(vals, static)
      est <- sum(coef(lm_r)[c('(Intercept)', true_vali)])
      se <- sqrt(abs(sum(vcov(lm_r)[c('(Intercept)', true_vali), 
                                c('(Intercept)', true_vali)])))
      
      outmat[i,'rmse_best'] <- sqrt((est-max(probs))^2)
      
      cint <- est + se*qt(0.975,lm_r$N-lm_r$k)*c(-1,1)
      
      (outmat[i, 'cov_best'] <- 1 * ((cint[1] < max(probs)) & 
                                      (cint[2] > max(probs))))
    }
    # selected correct arm
    outmat[i,'correct'] <- (which.max(vals$ppmat[nrow(vals$ppmat),])==which.max(
      probs))*1
    # posterior probability of best arm
    outmat[i,'posterior_best'] <- vals$ppmat[nrow(vals$ppmat), which.max(probs)]
    # bias
    outmat[i,'bias'] <- (coef(lm_r)[true_vali]-true_val) 
    # ses
    outmat[i,'ses'] <- lm_r$std.error[true_vali]
    
    if(control == FALSE) {# select arm 3 as control arm for statistics
      vals$control <- 3 
      vals$spmat <- rbind(rep(1/K, K), 
                          vals$ppmat)[1:periods,]
      # re-estimate lm_r with intercept
      lm_r <- ipw_est(vals, static)
      true_val <- max(probs)-probs[vals$control] 
      }
    # rmse of ATE
    outmat[i,'rmse_te'] <- sqrt((coef(lm_r)[true_vali]-true_val)^2)
    # coverage of ATE
    outmat[i,'cov_te'] <- 1 * ((lm_r[['conf.low']][true_vali] < true_val) &
                                 (lm_r[['conf.high']][true_vali] > true_val))
    if(is.na(outmat[i,'cov_te'])){
      outmat[i,'cov_te'] <- 0
    }
    
    # Pr(>|t|)
    outmat[i,'p'] <- lm_r$p.value[true_vali] # of true best arm
    
    # re-estimate lm_r with classical SEs
    lm_r <- ipw_est(vals, static, se_type = 'classical')
    
    # F
    outmat[i,'F'] <- summary(lm_r)[['fstatistic']]['value']
    
    outmat[i, as.character(1:periods)] <- vals$ppmat[,which.max(probs)]
    
    cat(i, '...')
  }
  cat('\n')
  return(outmat)
}


# Functions for formatting figures
format_num <- function(x, digits = 2) {
  x <- as.numeric(x)
  return(paste0(sprintf(paste0("%.", digits, "f"), x)))
}

add_parens <- function(x, digits = 2) {
  x <- as.numeric(x)
  return(paste0("(", format_num(x, digits = digits), ")"))
}

make_entry <-
  function(estimate, std.error, p.value) {
    paste0(format_num(estimate, 3),
           " ",
           add_parens(std.error, 3),
           if_else(p.value < 0.05 , "*", ""))
  }