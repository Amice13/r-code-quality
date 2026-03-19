#############################################################################################
# Code to create helper functions for simulation studies
# Taylor Grimm
# August 15, 2024
#############################################################################################

# Function to generate a multivariate VARMA time series. The code below is directly copied
# from the MTS::VARMAsim function but has been modified to allow error terms from distributions
# other than the multivariate normal.
my_VARMAsim <- function (nobs, arlags = NULL, malags = NULL, cnst = NULL, phi = NULL, 
                         theta = NULL, skip = 200, sigma, errors = 'mvn', delta = 1) 
{
  if (!is.matrix(sigma)) 
    sigma = as.matrix(sigma)
  k = nrow(sigma)
  nT = nobs + skip
  if(errors == 'mvn') {
    # at = rmvnorm(nT, rep(0, k), sigma)
    at = mvrnorm(nT, rep(0, k), sigma) # mvrnorm is slightly faster than rmvnorm
    oc_errors <- mvrnorm(nobs, rep(delta, k), sigma)
  } else if(errors == 'mst1') {
    # modify this as desired to draw errors from a multivariate skew-t distribution
    at = sn::rmst(n = nT, Omega = sigma, alpha = rep(50, k), nu = 5)
    oc_errors <- sn::rmst(n = nobs, xi = rep(delta, k), Omega = sigma, alpha = rep(50, k), nu = 5)
  } else if(errors == 'mst2') {
    # at = sn::rmst(n = nT, Omega = sigma, alpha = rep(50, k), nu = 50) # modify this 
    # oc_errors <- sn::rmst(n = nobs, xi = rep(delta, k), Omega = sigma, alpha = rep(50, k), nu = 50)
  } else {
    stop("Invalid option for error terms.")
  }
  
  nar = length(arlags)
  p = 0
  if (nar > 0) {
    arlags = sort(arlags)
    p = arlags[nar]
  }
  q = 0
  nma = length(malags)
  if (nma > 0) {
    malags = sort(malags)
    q = malags[nma]
  }
  ist = max(p, q) + 1
  zt = matrix(0, nT, k)
  if (length(cnst) == 0) 
    cnst = rep(0, k)
  for (it in ist:nT) {
    tmp = matrix(at[it, ], 1, k)
    if (nma > 0) {
      for (j in 1:nma) {
        jdx = (j - 1) * k
        thej = theta[, (jdx + 1):(jdx + k)]
        atm = matrix(at[it - malags[j], ], 1, k)
        tmp = tmp - atm %*% t(thej)
      }
    }
    if (nar > 0) {
      for (i in 1:nar) {
        idx = (i - 1) * k
        phj = phi[, (idx + 1):(idx + k)]
        ztm = matrix(zt[it - arlags[i], ], 1, k)
        tmp = tmp + ztm %*% t(phj)
      }
    }
    zt[it, ] = cnst + tmp
  }
  zt = zt[(1 + skip):nT, ]
  at = at[(1 + skip):nT, ]
  VARMAsim <- list(series = zt, noises = at, oc_errors = oc_errors)
}


# Loading in the thresholds (control limits) for each method
# check if we've already obtained thresholds (by running "2_find_far_thresholds_independent.R)
# If not, run the file to find the thresholds. This takes several hours.
if(any(list.files() == 'all_thresholds_robust_bayes_mewma_005.rds')) {
  load('all_thresholds_mewma_005.rds')
  load('all_thresholds_t2_005.rds')
  load('all_thresholds_ssmewma_005.rds')
  load('all_thresholds_sst2_005.rds')
  load('all_thresholds_robust_bayes_t2_005.rds')
  load('all_thresholds_robust_bayes_mewma_005.rds')
} else {
  source('2_find_far_thresholds_independent.R')
}

##########################################################################################
####### MEWMA  ###########################################################################
##########################################################################################

# Function to perform the classical or robust MEWMA chart
mewma <- function(train_data, test_data, method = 'classical',
                  lambda = 0.1, far = 0.005, ic_arl = NULL) {
  if(!is.null(ic_arl)) {
    far <- NULL
  }
  if(method == 'classical') {
    mean_vec <- colMeans(train_data)
    cov_mat <- cov(train_data)
  } else if(method == 'rmcd') {
    ests <- robustbase::covMcd(train_data) # reweighted MCD by default
    mean_vec <- ests$center 
    cov_mat <- ests$cov
  } else {
    stop(paste0("Invalid estimation method '", method, "'."))
  }
  
  # center the testing (phase 2) data using the parameter estimates from the training (phase 1) data
  x <- scale(test_data, center = mean_vec, scale = F)
  
  p <- ncol(x)
  q <- matrix(nrow = nrow(x), ncol = p)
  
  for(i in 1:nrow(x)) {
    if(i == 1) {
      q[i, ] <- lambda * x[i, ] + (1 - lambda)*0 #q_0 is the true IC mean, usually the 0 vector (without loss of generality for simulation study)
    } else {
      q[i, ] <- lambda * x[i, ] + (1 - lambda)*q[i-1, ]
    }
    # sigma_q <- ((lambda / (2 - lambda)) * (1 - (1 - lambda)^(2*i))) * cov_mat # exact covariance
  }
  sigma_q <- (lambda / (2 - lambda)) * cov_mat # asymptotic variance, used in Lowry et al. (1992)
  
  t2_val <- numeric()
  for(i in 1:nrow(x)) {
    t2_val[i] <- t(q[i, ]) %*% solve(sigma_q) %*% q[i, ]
  }
  
  if(!is.null(ic_arl)) {
    # use a control limit (h) that ensures IC ARL of ic_arl (such as 200)
    h <- spc::mewma.crit(l = lambda, L0 = ic_arl, p = p) # this assumes multivariate normality
    far <- NULL
  }
  num_train <- nrow(train_data)
  if(!is.null(far)) {
    if((num_train %in% c(30, 50, 100, 250, 500, 1000)) &&
       (p %in% c(3, 6)) &&
       (far == 0.005)) {
      if(method == 'classical') {
        h <- all_thresholds_mewma_005 |>
          filter(samp_size == nrow(train_data), p == ncol(train_data)) |> 
          pull(classical)
      } else if(method == 'rmcd') {
        h <- all_thresholds_mewma_005 |>
          filter(samp_size == nrow(train_data), p == ncol(train_data)) |> 
          pull(rmcd)
      }
    } else {
      stop("Threshold must be found through simulation.")
    }
  }
  
  list(mon_stats = t2_val, exceedances = t2_val > h, threshold = h)
}

##########################################################################################
####### Hotelling's T^2  #################################################################
##########################################################################################

# Function to perform the classical or robust Hotelling's T^2 chart
hot_t2 <- function(train_data, test_data,
                   method = 'classical', threshold = 'simulation', far = 0.005, ic_arl = NULL) {
  if(method == 'classical') {
    mean_vec <- colMeans(train_data)
    cov_mat <- cov(train_data)
  } else if(method == 'rmcd') {
    ests <- robustbase::covMcd(train_data)
    mean_vec <- ests$center # reweighted MCD by default
    cov_mat <- ests$cov
  } else {
    stop(paste0("Invalid estimation method '", method, "'."))
  }
  p <- ncol(train_data)
  num_train <- nrow(train_data)
  
  if(!is.null(ic_arl)) {
    prob <- 1 - (1 / ic_arl)
    far <- NULL
  } else {
    prob <- 1 - far
  }
  
  t2_train <- mahalanobis(train_data, center = mean_vec, cov = cov_mat)
  
  if(threshold == 'nonparametric') {
    # use the t2_train values to compute a nonparametric threshold, if desired
    # nonparametric threshold using the Silverman bandwidth
    h <- BMS:::quantile.density(density(t2_train, bw = 'nrd0'), probs = prob) 
  } else if(threshold == 'parametric') {
    # parametric sampling distribution under independence and multivariate normality (for classical mean/cov estimators)
    h <- ((p*(num_train^2 - 1)) / (num_train*(num_train - p))) * qf(prob, p, num_train - p)
  } else if(threshold == 'simulation') {
    if((num_train %in% c(30, 50, 100, 250, 500, 1000)) && (p %in% c(3, 6)) && (far == 0.005)) {
      if(method == 'classical') {
        h <- all_thresholds_t2_005 |>
          filter(samp_size == num_train, p == ncol(train_data)) |> 
          pull(classical) 
      } else if(method == 'rmcd') {
        h <- all_thresholds_t2_005 |>
          filter(samp_size == num_train, p == ncol(train_data)) |> 
          pull(rmcd) 
      }
    } else {
      stop("Threshold must be found through simulation.")
    }
  } else {
    stop(paste0("Invalid threshold '", threshold, "'."))
  }
  
  # t2 values during the testing period
  t2_val <- mahalanobis(test_data, center = mean_vec, cov = cov_mat) 
  
  list(train_t2 = t2_train, mon_stats = t2_val, exceedances = t2_val > h, threshold = h)
}

##########################################################################################
####### Robust Bayesian T2 and MEWMA chart functions #####################################
##########################################################################################
# Helper function to compute posterior means for mu and Sigma in the normal-normal-inverse wishart
# Bayesian conjugate model
nniw_ests <- function(k0, v0, prior_mean, prior_cov, test_data) {
  
  n <- nrow(test_data)
  if(is.null(n) || n == 1) {
    xbar <- as.numeric(test_data)
    n <- 1
    p <- length(test_data)
  } else {
    p <- ncol(test_data)
    xbar <- colMeans(matrix(test_data, ncol = p))
  }
  if(n == 1) {
    S <- 0
  } else {
    S <- 0
    for(i in 1:n) {
      S <- S + tcrossprod(test_data[i, ] - xbar)
    }
  }
  mu_0 <- prior_mean
  lam_0 <- prior_cov * v0
  
  mu_n <- (k0*mu_0 + n*xbar) / (k0 + n)
  kn <- k0 + n
  vn <- v0 + n
  lam_n <- lam_0 + S + (((k0 * n) / (k0 + n)) * tcrossprod(xbar - mu_0))
  
  list(posterior_mean_mu = mu_n, posterior_mean_sig = lam_n / vn,
       kn = kn, vn = vn)
}

# Function to perform the robust self-starting Bayesian control chart based on either T2 or MEWMA
robust_bayes <- function(initial_train, test, chart = 't2', far = 0.005, manual_cl = NULL) {
  
  p <- ncol(initial_train)
  m0 <- nrow(initial_train)
  
  ests <- list()
  mean_est <- matrix(nrow = nrow(test) + 1, ncol = p)
  cov_est <- list()
  ests[[1]] <- robustbase::covMcd(initial_train)
  mean_est[1, ] <- ests[[1]]$center
  cov_est[[1]] <- ests[[1]]$cov
  
  if(chart == 't2') {
    train_t2 <- mahalanobis(initial_train, center = mean_est[1, ], cov = cov_est[[1]])
    
    if(is.null(manual_cl)) {
      # cl <- (p * (m0^2 - 1) / (m0*(m0 - p))) * qf(1 - far, p, m0 - p)
      cl <- all_thresholds_robust_bayes_t2_005 |>
        filter(p == ncol(initial_train), samp_size == nrow(initial_train)) |> 
        pull(all_thresholds_robust_bayes_t2_005)
    } else {
      cl <- manual_cl
    }
  } else if(chart == 'mewma') {
    x <- scale(initial_train, center = mean_est[1, ], scale = F)
    q <- matrix(nrow = nrow(x), ncol = p)
    lambda <- 0.1
    for(i in 1:nrow(x)) {
      if(i == 1) {
        q[i, ] <- lambda * x[i, ] + (1 - lambda)*0 #q_0 is the true IC mean, usually the 0 vector (without loss of generality for simulation study)
      } else {
        q[i, ] <- lambda * x[i, ] + (1 - lambda)*q[i-1, ]
      }
      # sigma_q <- ((lambda / (2 - lambda)) * (1 - (1 - lambda)^(2*i))) * cov_mat # exact covariance
    }
    sigma_q <- (lambda / (2 - lambda)) * cov_est[[1]] # asymptotic variance, used in Lowry et al. (1992)
    train_t2 <- numeric()
    for(i in 1:nrow(x)) {
      train_t2[i] <- t(q[i, ]) %*% solve(sigma_q) %*% q[i, ]
    }
    # Set up q for testing data later on
    q <- matrix(nrow = nrow(test), ncol = p)
    if(is.null(manual_cl)) {
      # cl <- spc::mewma.crit(0.1, 200, p)
      cl <- all_thresholds_robust_bayes_mewma_005 |>
        filter(p == ncol(initial_train), samp_size == nrow(initial_train)) |> 
        pull(all_thresholds_robust_bayes_mewma_005)
    } else {
      cl <- manual_cl
    }
  } else {
    stop(paste0('Invalid chart option: "', chart, '".'))
  }
  
  # Compute monitoring statistics during the testing data, evaluate exceedances, update accordingly
  test_t2 <- numeric(nrow(test))
  train_use <- initial_train
  # initialize hyperparameters for first round of posterior updates
  kn <- m0
  vn <- m0 - 1
  for(i in 1:nrow(test)) {
    
    # use previous posterior estimates of mean & covariance matrix to compute monitoring statistics
    # (for i = 1, initial priors of RMCD estimates are used)
    if(chart == 't2') {
      test_t2[i] <- mahalanobis(test[i, ], center = mean_est[i, ], cov = cov_est[[i]])
    } else if(chart == 'mewma') {
      
      x <- scale(test, center = mean_est[i, ], scale = F)
      sigma_q <- (lambda / (2 - lambda)) * cov_est[[i]]
      
      if(i == 1) {
        q[i, ] <- lambda * x[i, ] + (1 - lambda)*0 #q_0 is the true IC mean, usually the 0 vector (without loss of generality for simulation study)
      } else {
        q[i, ] <- lambda * x[i, ] + (1 - lambda)*q[i-1, ]
      }
      test_t2[i] <- t(q[i, ]) %*% solve(sigma_q) %*% q[i, ]
    }
    
    # check if the i'th monitoring statistic exceeds the control limit
    if(test_t2[i] > cl) {
      # don't update estimates
      ests[[i + 1]] <- ests[[i]]
      mean_est[i + 1, ] <- mean_est[i, ]
      cov_est[[i + 1]] <- cov_est[[i]]
    } else {
      # update mean & covariance estimates
      prior_mean <- mean_est[i, ]
      prior_cov <- cov_est[[i]]
      # obtain posterior mean + covariance updates to use as mean/cov estimates for next observation
      ests[[i + 1]] <- nniw_ests(k0 = kn, # use previous posterior parameters as the new prior parameters
                                 v0 = vn,
                                 prior_mean = prior_mean,
                                 prior_cov = prior_cov,
                                 test_data = test[i, ])
      mean_est[i + 1, ] <- ests[[i + 1]]$posterior_mean_mu
      cov_est[[i + 1]] <- ests[[i + 1]]$posterior_mean_sig
      kn <- ests[[i + 1]]$kn
      vn <- ests[[i + 1]]$vn
    }
  }
  list(train_t2 = train_t2, mon_stats = test_t2, mean_est = mean_est, cov_est = cov_est, 
       exceedances = test_t2 > cl, threshold = cl)
}


##########################################################################################
####### SSMEWMA and SS T2 functions ######################################################
##########################################################################################

# helper function to compute MEWMA and T2 monitoring statistics
# for a given set of MVN(0, I) U_n vectors
ssmewma_stats <- function(data, lambda = 0.1, threshold = 'parametric', far = 0.005, ic_arl = 200,
                          manual_cl = NULL) {
  
  x <- data
  p <- ncol(x)
  if(is.null(manual_cl)) {
    if(threshold == 'parametric') {
      h <- spc::mewma.crit(lambda, ic_arl, p)
    } else if(threshold == 'simulation') {
      num_train <- nrow(data) - 1000
      if((num_train %in% c(30, 50, 100, 250, 500, 1000)) && (p %in% c(3, 6)) && (far == 0.005)) {
        h <- all_thresholds_ssmewma_005 |>
          filter(samp_size == num_train, p == ncol(x)) |> 
          pull(all_thresholds_ssmewma_005) 
      } else {
        stop("Need to simulate control limit.")
      }
    } else {
      stop(paste0('Invalid threshold argument "', threshold, '".'))
    }
  } else {
    h <- manual_cl
  }
  
  q <- matrix(nrow = nrow(x), ncol = p)
  t2_val <- rep(NA, nrow(x))
  threshold <- numeric(nrow(x))
  # only start monitoring with the p + 2nd observation
  # since the first nontrivial U_n vector occurs at observation p + 2
  for(i in (p + 2):nrow(x)) {
    if(i == (p + 2)) {
      q[i, ] <- lambda * x[i, ] + (1 - lambda)*0
    } else {
      q[i, ] <- lambda * x[i, ] + (1 - lambda)*q[i-1, ]
    }
    sigma_q_inv <- (2 - lambda) / (lambda * (1 - (1 - lambda)^(2*(i - p - 1)))) * diag(p)
    t2_val[i] <- t(q[i, ]) %*% sigma_q_inv %*% q[i, ]
  }
  
  
  list(mon_stats = t2_val, exceedances = t2_val > h, threshold = h)
}

sst2_stats <- function(data, threshold = 'parametric', far = 0.005, ic_arl = 200,
                          manual_cl = NULL) {
  
  x <- data
  p <- ncol(x)
  if(is.null(manual_cl)) {
    if(threshold == 'parametric') {
      if(!is.null(ic_arl)) {
        prob <- 1 - (1 / ic_arl)
        far <- NULL
      } else {
        prob <- 1 - far
      }
      h <- qchisq(prob, df = p)
    } else if(threshold == 'simulation') {
      num_train <- nrow(data) - 1000
      if((num_train %in% c(30, 50, 100, 250, 500, 1000)) && (p %in% c(3, 6)) && (far == 0.005)) {
        h <- all_thresholds_sst2_005 |>
          filter(samp_size == num_train, p == ncol(x)) |> 
          pull(all_thresholds_sst2_005) 
      } else {
        stop("Need to simulate control limit.")
      }
    } else {
      stop(paste0('Invalid threshold argument "', threshold, '".'))
    }
  } else {
    h <- manual_cl
  }
  
  t2_val <- mahalanobis(data, center = rep(0, p), cov = diag(p))
  
  list(mon_stats = t2_val, exceedances = t2_val > h, threshold = h)
}

# helper function to transform the observed data to multivariate standard normal observations
u_vec_transform <- function(data, factor, n) {
  
  p <- length(data) # number of dimensions
  old <- numeric(p + 1)
  
  r <- rep(0, p)
  u <- numeric(p)
  work <- c(1, data)
  
  for (i in 1:(p + 1)) {
    d1 <- factor[i, i]
    old[i] <- d1
    d2 <- work[i]
    if (i > 1) r[i - 1] <- d2
    
    d0 <- max(abs(d1), abs(d2))
    
    if (d0 == 0) next
    
    d1 <- d1 / d0
    d2 <- d2 / d0
    
    if (abs(d2) < 1e-20) next
    
    d <- sqrt(d1^2 + d2^2)
    factor[i, i] <- d * d0
    
    c0 <- d1 / d
    s0 <- d2 / d
    if (i + 1 > p + 1) {
      next
    } else {
      for (j in (i + 1):(p + 1)) {
        d1 <- factor[i, j]
        d2 <- work[j]
        factor[i, j] <- d1 * c0 + d2 * s0
        work[j] <- -d1 * s0 + d2 * c0
      }
    }
  }
  
  for (i in 1:p) {
    ndf <- n - i - 1
    df <- ndf
    
    if (ndf > 0 && old[i + 1] > 1e-20) {
      tee <- r[i] * sqrt(df) / old[i+1]
      
      if (ndf < 13) {
        area <- pt(tee, ndf)
        u[i] <- qnorm(area)
      } else {
        u[i] <- abs((1 - 2 / (8 * df + 3)) * sqrt(df * log(1 + tee^2/df))) * sign(tee)
      }
    }
  }
  return(list(factor = factor, r = r, u = u))
}

# Function to perform the SSMEWMA of Hawkins and Maboudou-Tchao (2007)
ssmewma <- function(data, lambda = 0.1, threshold = 'simulation', far = 0.005, ic_arl = 200, manual_cl = NULL) {

  p <- ncol(data)

  fit <- list()
  r <- matrix(nrow = nrow(data), ncol = p)
  u <- matrix(nrow = nrow(data), ncol = p)
  for(i in 1:nrow(data)) {
    if(i == 1) factor <- matrix(0, nrow = p + 1, ncol = p + 1)
    else {
      factor <- fit[[i - 1]]$factor
    }
    fit[[i]] <- u_vec_transform(data[i, ], factor = factor, n = i)
    r[i, ] <- fit[[i]]$r
    u[i, ] <- fit[[i]]$u
  }

  # compute monitoring statistics
  stats <- ssmewma_stats(u, lambda = lambda, threshold = threshold, far = far, ic_arl = ic_arl, manual_cl = manual_cl)
  list(u = u,
       r = r,
       mon_stats = stats$mon_stats,
       exceedances = stats$exceedances,
       threshold = stats$threshold)
}

# Function to perform the SS T2 based on the transformations in Hawkins and Maboudou-Tchao (2007)
sst2 <- function(data, threshold = 'simulation', far = 0.005, ic_arl = 200, manual_cl = NULL) {
  
  p <- ncol(data)
  
  fit <- list()
  r <- matrix(nrow = nrow(data), ncol = p)
  u <- matrix(nrow = nrow(data), ncol = p)
  for(i in 1:nrow(data)) {
    if(i == 1) factor <- matrix(0, nrow = p + 1, ncol = p + 1)
    else {
      factor <- fit[[i - 1]]$factor
    }
    fit[[i]] <- u_vec_transform(data[i, ], factor = factor, n = i)
    r[i, ] <- fit[[i]]$r
    u[i, ] <- fit[[i]]$u
  }
  
  # compute monitoring statistics
  stats <- sst2_stats(u, threshold = threshold, far = far, ic_arl = ic_arl, manual_cl = manual_cl)
  list(u = u,
       r = r,
       mon_stats = stats$mon_stats,
       exceedances = stats$exceedances,
       threshold = stats$threshold)
}

