## Adaptive Experimental Design: Prospects and Applications in Political Science
## Code for Study 2: misperceptions, F-statostocs
## Produces F-statistics referenced in text for study 2

version

set.seed(95126)


# Set options ----
source('utils.R')
iter <- 1e+03

# Cluster RI functions ----

sim_out_RI_clustered <- function(Y, Z, periods = 10, static = FALSE, 
                                 control = FALSE, cluster_size = 3){
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
  ppmat[i,] <- best_binomial_bandit_sim(xmat[i,], nmat[i,]*cluster_size)
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
                                         nvec = nmat[(i-1),]*cluster_size,
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
      
      ppmat[i,] <- best_binomial_bandit_sim(xmat[i,], nmat[i,]*cluster_size)
      
      if (i == periods) break
    }}
  
  # only return sampling probability separate from posterior probability if
  # using control-augmented algorithm
  if(control == FALSE){
    out <- list(nmat = nmat*cluster_size, xmat = xmat, ppmat = ppmat, 
                cluster = rep(1:length(Y), each = cluster_size))
  }else{
    out <- list(nmat = nmat*cluster_size, xmat = xmat,
                ppmat = ppmat, spmat = spmat, control = control,
                cluster = rep(1:length(Y), each = cluster_size))
  }
  return(out)
}

# Function for ipw estimation within simulations
ipw_est_clustered <- function(vals, static=FALSE, se_type='CR2'){
  
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
    lmfit <- lm_robust(yvals ~ -1 + zvals, weights = 1/wvals)
    
  } else if( (static == TRUE) & (length(is.finite(vals$control))>0) ){ # static with control
    
    wvals <- rep(1/length(unique(zvals)), length(yvals))
    zvals <- relevel(zvals, ref = vals$control)  
    lmfit <- lm_robust(yvals ~ zvals, weights = 1/wvals)
    
  } else if( (static == FALSE) & (length(is.finite(vals$control))==0) ){ # adaptive, no control
    
    bbmat$weight <- melt(rbind(rep(1/ncol(vals$ppmat), ncol(vals$ppmat)),
                               vals$ppmat[1:( nrow(vals$ppmat)-1),]))$value
    wvals <- unlist(apply(bbmat, 1, function(x) 
      c(rep(x['weight'], x['value']))))
    
    lmfit <- lm_robust(yvals ~ -1 + zvals, weights = 1/wvals)
    
  } else { # adaptive with control
    
    zvals <- relevel(zvals, ref = vals$control)  
    bbmat$weight <- melt(vals$spmat)$value
    wvals <- unlist(apply(bbmat, 1, function(x) 
      c(rep(x['weight'], x['value']))))  
    
    lmfit <- lm_robust(yvals ~ zvals, weights = 1/wvals, se_type = se_type,
                       cluster = vals$cluster)
  }
  return(lmfit)
}



mysims_RI_clustered <- function(control = FALSE, static = FALSE, 
                                periods = 10, iter = 1000, 
                                # arguments for randomization inference
                                Y, Z, cluster_size = 3){
  outmat <- matrix(rep(NA, (10+periods)*iter), ncol = (10+periods))
  
  colnames(outmat) <- c('correct', 'posterior_best', 'bias', 'rmse_best', 
                        'rmse_te', 'cov', 'ses', 'F', 'p', 0:periods)
  
  outmat[,'0'] <- 1/length(Z)
  
  cat('iteration: ')
  for( i in 1:iter){
    
    if(control==FALSE){ # no augmented control
      vals <- sim_out_RI_clustered(Y, Z, periods = periods, static = static, 
                                   cluster_size = cluster_size)
      true_val <- mean(Y)
      true_vali <- 'zvals1'
      lm_r <- ipw_est_clustered(vals, static)
      
      outmat[i,'rmse_best'] <- sqrt((coef(lm_r)[true_vali]-true_val)^2)
    } else {
      vals <- sim_out_RI_clustered(Y, Z, periods = periods, static = static, 
                                   control = control, cluster_size = cluster_size)
      true_val <- 0
      true_vali <- 'zvals1'
      
      lm_r <- ipw_est_clustered(vals, static)
      
      outmat[i,'rmse_best'] <- sqrt(
        (sum( coef(lm_r)[c('(Intercept)', true_vali)])-
           mean(Y/cluster_size))^2)
    }
    # selected correct arm
    outmat[i,'correct'] <- (which.max(vals$ppmat[nrow(vals$ppmat),])==1)*1
    # posterior probability of best arm
    outmat[i,'posterior_best'] <- vals$ppmat[nrow(vals$ppmat), 1]
    # bias
    outmat[i,'bias'] <- (coef(lm_r)[true_vali]-true_val) 
    # coverage
    outmat[i, 'cov'] <- 1 * ((lm_r[['conf.low']][true_vali] < true_val) &
                               (lm_r[['conf.high']][true_vali] > true_val))
    # ses
    outmat[i,'ses'] <- lm_r$std.error[true_vali]
    
    if(control == FALSE) {# select arm 3 as control arm for statistics
      vals$control <- 3 
      vals$spmat <- rbind(rep(1/K, K), 
                          vals$ppmat)[1:periods,]
      # re-estimate lm_r with intercept
      lm_r <- ipw_est_clustered(vals, static)
    }
    # rmse of ATE
    outmat[i,'rmse_te'] <- sqrt((coef(lm_r)[true_vali]-true_val)^2)
    
    # Pr(>|t|)
    outmat[i,'p'] <- lm_r$p.value[true_vali] # of true best arm
    
    # F, using robust SEs
    outmat[i,'F'] <- summary(lm_r)[['fstatistic']]['value']
    
    outmat[i, as.character(1:periods)] <- vals$ppmat[,1]
    
    cat(i, '...')
  }
  cat('\n')
  return(outmat)
}

# Data ----
mturk_bandit <- read_rds('study_2_clean.rds')

# models
mturk_long <- mturk_bandit %>% 
  gather(key = 'out', value = 'Y', c('Y_deficit', 'Y_unemp', 'Y_nfi'))%>% 
  mutate(Y_complete = case_when(
    (out == 'Y_deficit' & Y_deficit_no_response == 1) ~ NA_real_,
    (out == 'Y_unemp' & Y_unemp_no_response == 1) ~ NA_real_,
    (out == 'Y_nfi' & Y_nfi_no_response == 1) ~ NA_real_,
    TRUE ~ Y))

fit_d <- lm_robust(Y ~ Z, 
                   data = filter(mturk_long, pid == 'democrat'), 
                   cluster = id, weights = weights)
fit_r <- lm_robust(Y ~ Z, 
                   data = filter(mturk_long, pid == 'republican'), 
                   cluster = id, weights = weights)

# data
Y_d <- filter(mturk_bandit, pid == 'democrat') %>% select(Y_sum) %>% pull()
Y_r <- filter(mturk_bandit, pid == 'republican') %>% select(Y_sum) %>% pull()

Z <- unique(mturk_bandit[['Z']])
levels(Z) <- c('lottery', 'accuracy', 'control', 'google', 'direction', 
               'extratime')
Z <- sort(Z)

# null distributions
outd_F <- mysims_RI_clustered(Y = Y_d, Z = Z, iter = iter, control = 3)
outr_F <- mysims_RI_clustered(Y = Y_r, Z = Z, iter = iter, control = 3)

# percentile
p_d <- (1-max(which(sort(outd_F[, 'F'])<summary(fit_d)$fstatistic['value']))/
  iter)
p_r <- (1-max(which(sort(outr_F[, 'F'])<summary(fit_r)$fstatistic['value']))/
  iter)


outF <- data.frame(F = c(summary(fit_d)$fstatistic['value'], 
                         summary(fit_r)$fstatistic['value']),
                   p = c(p_d, p_r),
                   study = c('d', 'r'))

outF

# ** [study_2_Fstats.csv] ----
# write_csv(outF, 'study_2_Fstats.csv')
