##########################################
# Code for performing in-control
# average run length simulations (ARMA(1,1) correlation)
# Taylor Grimm
# January 27th, 2024
##########################################


# Note: any variable or skewness argument that's "normal" refers to the
# F distribution with degrees of freedom 5 and n-5, which is the "normal"
# or standard case under the assumptions of independence and normality.

library(tidyverse)

# Functions to generate a series to be used for determining thresholds
one_series <- function(n = 100, ar = .9, ma = .9, skewness = 'heavier', df1 = 5) {
  samp_size <- n
  if(skewness == 'heavier') {
    df2 <- 20
  } else if(skewness == 'normal') {
    df2 <- samp_size - df1
  } else{
    stop("skewness must be either 'heavier' or 'normal'.")
  }
  f_dist_constant <- (df1*(samp_size^2 - 1) / (samp_size*(samp_size-df1)))
  
  errors <- rf(n + 100, df1, df2)*f_dist_constant
  if(ar == 0) {
    series <- errors[-c(1:100)]
  } else {
    series <- arima.sim(n = n, model = list(ar = ar, ma = ma), innov = errors, n.start = 100) 
  }
  series
}
one_series_gamma <- function(n = 100, ar = .9, ma = .9, alpha = .05, beta = .05) {
  samp_size <- n
  errors <- rgamma(n + 100, alpha, beta)
  if(ar == 0) {
    # for independent data, the series is just a bunch of independent random error terms
    series <- errors[-c(1:100)]
  } else {
    # for dependent data, introduce ARMA(1,1) dependence with arima.sim
    series <- arima.sim(n = n, model = list(ar = ar, ma = ma), innov = errors, n.start = 100)
  }
  series
}

# Function to generate a test set based on the desired experimental factors
ic_test_series <- function(samp_size = 100, ar = .9, ma = .9, skewness = 'heavier', df1 = 5) {
  n <- 2000
  if(skewness == 'heavier') {
    df2 <- 20
  } else if(skewness == 'normal') {
    df2 <- samp_size - df1
  } else{
    stop("skewness must be either 'heavier' or 'normal'.")
  }
  f_dist_constant <- (df1*(samp_size^2 - 1) / (samp_size*(samp_size-df1)))
  
  errors <- rf(n + 100, df1, df2)*f_dist_constant
  if(ar == 0) {
    series <- errors[-c(1:100)]
  } else {
    series <- arima.sim(n = n, model = list(ar = ar, ma = ma), innov = errors, n.start = 100) 
  }
  series
}

# Function to find the run length given a set of monitoring statistics and the threshold
run_length <- function(mon_stats, threshold) {
  first_exceed <- which(mon_stats > threshold)[1]
  num <- length(mon_stats)
  ifelse(is.na(first_exceed), num, first_exceed)
}

# Function to find the run length for a generated test set based on the thresholds from the training period
test_rl <- function(ar = .9, ma = .5, skewness = 'heavier', samp_size = 100, threshold_est) {
  if(skewness %in% c('normal', 'heavier')) {
    ic_test_data <- ic_test_series(ar = ar, ma = ma, samp_size = samp_size, skewness = skewness)
  } else if (skewness == 'gamma'){
    ic_test_data <- one_series_gamma(ar = ar, ma = ma, n = 2000)
  } else {
    stop('Invalid "skewness" argument.')
  }
  
  rl <- numeric()
  for(threshold in thresholds) {
    rl[threshold] <- run_length(ic_test_data, threshold_est[threshold])
  }
  rl
}

# Load in helper functions for adjusted bandwidth nonparametric estimators
source('9_bw_adj_helpers.R')

# Function to get threshold estimates for the desired method using a provided dataset
get_estimates_arma <- function(data, method = 'iid_boot', probs = .995) {
  
  arma11_fits <- data |> as_tibble() |> map(~ arima(.x, order = c(1, 0, 1))$coef[1:2])
  est_params <- arma11_fits |> bind_rows()
  
  phi_hats <- est_params$ar1
  theta_hats <- est_params$ma1
  
  n_eff <- function(phi_hat, theta_hat, n = NULL) {
    n_eff <- effective_size_arma(n = n, phi = phi_hat, theta = theta_hat)
    acf_vals <- ARMAacf(ar = phi_hat, ma = theta_hat, lag.max = (n/2) + 1)[-1]
    if(any(abs(acf_vals) <= 0.05)) {
      l <- which(abs(acf_vals) <= 0.05)[1]
      l_use <- min(l, n/2)
    } else {
      l_use <- n/2
    }
    n_eff <- max(1, n_eff)
    list(n_eff = n_eff, l = l_use)
  }
  neff_and_l <- list(phi_hats, theta_hats) |> pmap_dfr(n_eff, n = nrow(data))
  
  if(method == 'iid_boot') {
    boot_iid_stat <- function(series, indices, probs) quantile(series[indices], probs = probs)
    estimates <- data |> apply(2, function(x) boot::boot(data = x, statistic = boot_iid_stat,
                                                         R = 1000, probs = probs)$t |> colMeans())
    estimates <- t(estimates)
  } else if(method == 'mbb_boot') {
    boot_mbb_stat <- function(series) series |> quantile(probs = probs)
    estimates <- list()
    for(i in 1:ncol(data)) {
      estimates[[i]] <- boot::tsboot(tseries = data[, i], statistic = boot_mbb_stat,
                                     R = 1000, sim = 'fixed', l = neff_and_l$l[i])$t |> colMeans()
    }
    estimates <- do.call(rbind, estimates)
  } else if(method == 'stationary_boot') {
    boot_stationary_stat <- function(series) series |> quantile(probs = probs)
    estimates <- list()
    for(i in 1:ncol(data)) {
      estimates[[i]] <- boot::tsboot(tseries = data[, i], statistic = boot_stationary_stat,
                                     R = 1000, sim = 'geom', l = neff_and_l$l[i])$t |> colMeans()
    }
    estimates <- do.call(rbind, estimates)
  } else if(method == 'silverman') {
    silverman_dens <- data |> apply(2, function(x) density(x, bw = 'nrd0', kernel = 'gaussian'))
    estimates <- silverman_dens |> map(function(x) BMS:::quantile.density(x, probs = probs))
    estimates <- do.call(rbind, estimates)
  } else if(method == 'scott') {
    scott_dens <- data |> apply(2, function(x) density(x, bw = 'nrd', kernel = 'gaussian'))
    estimates <- scott_dens |> map(function(x) BMS:::quantile.density(x, probs = probs))
    estimates <- do.call(rbind, estimates)
  } else if(method == 'silverman_adj') {
    estimates <- list()
    for(i in 1:ncol(data)) {
      silverman_adj <- silverman_bw(data[, i], neff_and_l$n_eff[i])
      silverman_dens_adj <- density(data[, i], bw = silverman_adj, kernel = 'gaussian')
      estimates[[i]] <- BMS:::quantile.density(silverman_dens_adj, probs = probs)
    }
    estimates <- do.call(rbind, estimates)
  } else if(method == 'scott_adj') {
    estimates <- list()
    for(i in 1:ncol(data)) {
      scott_adj <- scott_bw(data[, i], neff_and_l$n_eff[i])
      scott_dens_adj <- density(data[, i], bw = scott_adj, kernel = 'gaussian')
      estimates[[i]] <- BMS:::quantile.density(scott_dens_adj, probs = probs)
    }
    estimates <- do.call(rbind, estimates)
  } else if(method == 'raw') {
    estimates <- data |> apply(2, function(x) quantile(x, probs = probs)) |> t()
  } else {
    stop('Invalid method.')
  }
  estimates
}


thresholds <- c('raw', 'iid_boot', 'mbb_boot', 'stationary_boot',
                'silverman', 'scott', 'silverman_adj', 'scott_adj')

# Function to compute the conditional IC ARL (ARL for a single generated IC sample)
ic_arl_sim <- function(ar = 0, ma = 0, samp_size = 100, skewness = 'normal') {
  # step 1: generate an IC sample from which the thresholds will be computed
  
  # use "samp_size" historical observations to train (100, 500, 1000, or 5000)
  if(skewness %in% c('normal', 'heavier')) {
    train_data <- one_series(ar = ar, ma = ma, n = samp_size, skewness = skewness)
  } else if (skewness == 'gamma') {
    train_data <- one_series_gamma(ar = ar, ma = ma, n = samp_size)
  } else {
    stop('Invalid "skewness" argument.')
  }
  
  
  # step 2: compute thresholds from the IC sample
  threshold_est <- numeric()
  for(threshold in thresholds) {
    threshold_est[threshold] <- as.numeric(get_estimates_arma(as.matrix(train_data), method = threshold, probs = 0.995))
  }
  
  # step 3: generate 2000 IC observations, see when the first threshold exceedance occurs.
  # Record this as the run length.
  
  # do this using the "test_rl()" function defined above
  
  # step 4: repeat step 3 5000 times
  results <- replicate(5000, test_rl(ar = ar, ma = ma, skewness = skewness,
                                     samp_size = samp_size, threshold_est = threshold_est))
  
  # step 5: Take the mean of the run lengths as the IC average run length (ARL_IC)
  rowMeans(results, na.rm = T)
}

# step 6: repeat steps 1-5 1000 times to find the actual (unconditional) ARL by taking the average
# of the 100 conditional ARL's.
actual_arl <- function(reps = 1000, ar = 0, ma = 0, samp_size = 100, skewness = 'normal') {
  replicate(reps, ic_arl_sim(ar = ar, ma = ma, samp_size = samp_size, skewness = skewness))
}

# Create a grid of experimental factors
all_combinations <- tibble(ar = rep(c(0.1, 0.1, 0.1, 0.5, 0.5, 0.5, 0.9, 0.9, 0.9), each = 4),
                                ma = rep(c(-0.9, -0.5, 0.1, -0.9, -0.1, 0.5, -0.5, -0.1, 0.9), each = 4),
                                n = rep(c(100, 500, 1000, 5000), 9))


# For each combination of distribution, sample size, and autocorrelation strength,
# find the average run length associated with each threshold.
library(furrr)
plan(multisession, workers = 5)
set.seed(100)

# IC ARL results for "normal" skewness
system.time(all_ic_arl_normal <- future_pmap(all_combinations,
                                             function(ar, ma, n) actual_arl(reps = 1000, ar = ar, ma = ma, samp_size = n, skewness = 'normal'),
                                             .progress = T, .options=furrr_options(seed = TRUE)))
cbind(all_combinations, all_ic_arl_normal |> map_dfr(rowMeans))
cbind(all_combinations, all_ic_arl_normal |> map_dfr(~ apply(.x, 1, sd)))
save(all_ic_arl_normal, file = 'ic_arl/all_ic_arl_normal_arma.rds')

# IC ARL results for "heavier" skewness
system.time(all_ic_arl_skewed <- future_pmap(all_combinations,
                                             function(ar, ma, n) actual_arl(reps = 1000, ar = ar, ma = ma, samp_size = n, skewness = 'heavier'),
                                             .progress = T, .options=furrr_options(seed = TRUE)))

cbind(all_combinations, all_ic_arl_skewed |> map_dfr(rowMeans))
cbind(all_combinations, all_ic_arl_skewed |> map_dfr(~ apply(.x, 1, sd)))
save(all_ic_arl_skewed, file = 'ic_arl/all_ic_arl_skewed_arma.rds')

# IC ARL results for "gamma" skewness
system.time(all_ic_arl_gamma <- future_pmap(all_combinations,
                                            function(ar, ma, n) actual_arl(reps = 1000, ar = ar, ma = ma, samp_size = n, skewness = 'gamma'),
                                            .progress = T, .options=furrr_options(seed = TRUE)))
cbind(all_combinations, all_ic_arl_gamma |> map_dfr(rowMeans))
cbind(all_combinations, all_ic_arl_gamma |> map_dfr(~ apply(.x, 1, sd)))
save(all_ic_arl_gamma, file = 'ic_arl/all_ic_arl_gamma_arma.rds')



####################################################################
########## Produce LaTeX tables of IC ARL results ##################
####################################################################

# Load ARL results
load('ic_arl/all_ic_arl_normal_arma.rds')
load('ic_arl/all_ic_arl_skewed_arma.rds')
load('ic_arl/all_ic_arl_gamma_arma.rds')

# helper function to find thich method gives an ARL closest to the specified ARL0 (ic_arl)
closest_to_0 <- function(rls, ic_arl = 200) {
  diff <- ic_arl - rls
  which.min(abs(diff))
}
# LaTeX table of F_{5, n-5} error results
normal_means <- cbind(all_combinations, all_ic_arl_normal |> map_dfr(rowMeans))
normal_means_reordered <- normal_means[, c(1, 2, 3, 4, 8, 9, 5, 10, 11, 6, 7)]
colnames(normal_means_reordered) <- c('phi', 'ma', 'n', 'Sample Quantile', 'SLVM', 'SCOTT', 'BOOT', 
                                      'ADJ-SLVM', 'ADJ-SCOTT', 'MB-BOOT', 'RB-BOOT')
normal_means_tab <- normal_means_reordered |> round(2)
normal_means_tab |> xtable::xtable()
normal_means_tab[,-c(1:3)] |> apply(1, closest_to_0, ic_arl = 200)

# LaTeX table of F_{5, 20} error results
skewed_means <- cbind(all_combinations, all_ic_arl_skewed |> map_dfr(rowMeans))
skewed_means_reordered <- skewed_means[, c(1, 2, 3, 4, 8, 9, 5, 10, 11, 6, 7)]
colnames(skewed_means_reordered) <- c('phi', 'ma', 'n', 'Sample Quantile', 'SLVM', 'SCOTT', 'BOOT', 
                                      'ADJ-SLVM', 'ADJ-SCOTT', 'MB-BOOT', 'RB-BOOT')
skewed_means_tab <- skewed_means_reordered |> round(2)
skewed_means_tab |> xtable::xtable()
skewed_means_tab[,-c(1:3)] |> apply(1, closest_to_0, ic_arl = 200)

# LaTeX table of Gamma error results
gamma_means <- cbind(all_combinations, all_ic_arl_gamma |> map_dfr(rowMeans))
gamma_means_reordered <- gamma_means[, c(1, 2, 3, 4, 8, 9, 5, 10, 11, 6, 7)]
colnames(gamma_means_reordered) <- c('phi', 'ma', 'n', 'Sample Quantile', 'SLVM', 'SCOTT', 'BOOT', 
                                     'ADJ-SLVM', 'ADJ-SCOTT', 'MB-BOOT', 'RB-BOOT')
gamma_means_tab <- gamma_means_reordered |> round(2)
gamma_means_tab |> xtable::xtable()
gamma_means_tab[,-c(1:3)] |> apply(1, closest_to_0, ic_arl = 200)


#################################################################################
# What if we use the "true quantile" computed in the simulation study? #########
#################################################################################

# Function to find the true quantile for a given combination of simulation factors
find_true_quantiles <- function(ar = .9, ma = .9, samp_size = 100, skewness = 'heavier', seed = 1234,
                                probs = .995, df1 = 5) {
  
  if(skewness == 'heavier') {
    df2 <- 20
  } else if(skewness == 'normal') {
    df2 <- samp_size - df1
  } else{
    stop("skewness must be either 'heavier' or 'normal'.")
  }
  n <- 1e6
  
  f_dist_constant <- (df1*(samp_size^2 - 1)/ (samp_size*(samp_size-df1)))
  set.seed(seed)
  
  errors <- rf(n + 100, df1, df2)*f_dist_constant
  if(ar == 0) {
    # for independent data, the series is just a bunch of independent random error terms
    series <- errors[-c(1:100)]
  } else {
    # for dependent data, introduce AR(1) dependence with arima.sim
    series <- arima.sim(n = n, model = list(ar = ar, ma = ma), innov = errors, n.start = 100)
  }
  # estimate empirical quantiles
  quantile(series, probs = probs)
}

# Create a function to generate a long sample that represents the true underlying time series
# for each set of experimental factors
true_series_long <- function(ar = .9, ma = .9, samp_size = 100, skewness = 'heavier', seed = 1234, df1 = 5) {
  if(skewness == 'heavier') {
    df2 <- 20
  } else if(skewness == 'normal') {
    df2 <- samp_size - df1
  } else{
    stop("skewness must be either 'heavier' or 'normal'.")
  }
  n <- 1e6
  
  f_dist_constant <- (df1*(samp_size^2 - 1)/ (samp_size*(samp_size-df1)))
  set.seed(seed)
  
  errors <- rf(n + 100, df1, df2)*f_dist_constant
  if(ar == 0) {
    # for independent data, the series is just a bunch of independent random error terms
    series <- errors[-c(1:100)]
  } else {
    # for dependent data, introduce AR(1) dependence with arima.sim
    series <- arima.sim(n = n, model = list(ar = ar, ma = ma), innov = errors, n.start = 100)
  }
}

# Compute the true quantiles for all 12 desired combinations of phi (ar) and sample size
true_grid <- tibble(ar = rep(c(0.1, 0.1, 0.1, 0.5, 0.5, 0.5, 0.9, 0.9, 0.9), each = 4),
                    ma = rep(c(-0.9, -0.5, 0.1, -0.9, -0.1, 0.5, -0.5, -0.1, 0.9), each = 4),
                    samp_size = rep(c(100, 500, 1000, 5000), 9))
true_quantiles_skewed <- true_grid |> pmap_dfr(find_true_quantiles)
true_quantiles_normal <- true_grid |> pmap_dfr(find_true_quantiles, skewness = 'normal')

# make a single tibble showing the true quantiles for each experimental condition
cbind(true_grid, true_quantiles_skewed) |> as_tibble()
cbind(true_grid, true_quantiles_normal) |> as_tibble()

# Functions to obtain actual (unconditional) IC ARL results for each combination of experimental factors
test_rl_true <- function(ar = 0, ma = 0, skewness = 'normal', samp_size = 100, threshold_est) {
  if(skewness %in% c('normal', 'heavier')) {
    ic_test_data <- ic_test_series(ar = ar, ma = ma, samp_size = samp_size, skewness = skewness)
  } else if (skewness == 'gamma'){
    ic_test_data <- one_series_gamma(ar = ar, ma = ma, n = 2000)
  } else {
    stop('Invalid "skewness" argument.')
  }
  
  rl <- run_length(ic_test_data, threshold_est)
  rl
}
get_true_rl <- function(combination, skewness = 'normal', quant_val = NULL) {
  if(skewness == 'normal') {
    true_quants <- cbind(true_grid, true_quantiles_normal) |> as_tibble()
  } else if(skewness == 'heavier') {
    true_quants <- cbind(true_grid, true_quantiles_skewed) |> as_tibble()
  } else {
    true_quants <- cbind(true_grid, true_quants_gamma) |> as_tibble()
  }
  ar <- as.numeric(true_quants[combination, 1])
  ma <- as.numeric(true_quants[combination, 2])
  samp_size <- as.numeric(true_quants[combination, 3])
  if(is.null(quant_val)) {
    quant_val <- as.numeric(true_quants[combination, 4])
  }
  replicate(1000, test_rl_true(ar = ar, ma = ma, skewness = skewness, samp_size = samp_size, threshold_est = quant_val)) |> mean()
}
all_true_rls <- function(combination = 1, skewness = 'normal', quant_val = NULL) {
  replicate(1000, get_true_rl(combination = combination, skewness = skewness, quant_val = quant_val))
}

# Get ARL values for the true quantiles
# F error distribution results
system.time(normal_true_rls <- 1:36 |> map(all_true_rls, skewness = 'normal'))
normal_true_rls |> map_dbl(mean) |> round(2)
normal_true_rls |> map_dbl(sd)
save(normal_true_rls, file = 'ic_arl/ic_arl_normal_true_arma.rds')

system.time(skewed_true_rls <- 1:36 |> map(all_true_rls, skewness = 'heavier'))
skewed_true_rls |> map_dbl(mean) |> round(2)
skewed_true_rls |> map_dbl(sd)
save(skewed_true_rls, file = 'ic_arl/ic_arl_skewed_true_arma.rds')

# Gamma error distribution results
true_quant_gamma <- function(ar = .9, ma = 0.5, samp_size = 100, seed = 1234,
                             probs = .995, alpha = .05, beta = .05) {
  n <- 1e6
  set.seed(seed)
  
  errors <- rgamma(n + 100, alpha, beta)
  if(ar == 0) {
    # for independent data, the series is just a bunch of independent random error terms
    series <- errors[-c(1:100)]
  } else {
    # for dependent data, introduce ARMA(1,1) dependence with arima.sim
    series <- arima.sim(n = n, model = list(ar = ar, ma = ma), innov = errors, n.start = 100)
  }
  # estimate empirical quantiles
  quantile(series, probs = probs)
}

true_quants_gamma <- true_grid |> pmap_dfr(true_quant_gamma)

set.seed(1234)
system.time(gamma_true_rls <- 1:36 |> map(all_true_rls, skewness = 'gamma'))
gamma_true_rls |> map_dbl(mean) |> round(2)
gamma_true_rls |> map_dbl(sd)
save(gamma_true_rls, file = 'ic_arl/ic_arl_gamma_true_arma.rds')





# Finding the actual quantiles that result in nominal ARL_0 values of 200
# for each combination of simulation factors

eval_func <- function(x, combination = 1, skewness = 'normal') {
  200 - (replicate(100, get_true_rl(combination = combination, skewness = skewness, quant_val = x)) |> mean())
}

find_nominal <- function(combination, skewness = 'normal') {
  if(combination %in% 1:20) {
    interval <- case_when(skewness == 'normal' ~ c(12, 24),
                          skewness == 'heavier' ~ c(12, 50),
                          skewness == 'gamma' ~ c(10, 80))
  } else if(combination %in% 21:28) {
    interval <- case_when(skewness == 'normal' ~ c(25, 42),
                          skewness == 'heavier' ~ c(35, 52),
                          skewness == 'gamma' ~ c(15, 60))
  } else if(combination %in% 29:32) {
    interval <- case_when(skewness == 'normal' ~ c(55, 70),
                          skewness == 'heavier' ~ c(60, 80),
                          skewness == 'gamma' ~ c(30, 65))
  } else {
    interval <- case_when(skewness == 'normal' ~ c(120, 140),
                          skewness == 'heavier' ~ c(130, 160),
                          skewness == 'gamma' ~ c(70, 120))
  }
  
  
  threshold <- uniroot(eval_func, interval = interval, combination = combination,
                       skewness = skewness, maxiter = 10, tol = .1)$root
  
  arl0 <- eval_func(x = threshold, combination = combination, skewness = skewness) + 200
  
  tibble(nominal_threshold = threshold,
         arl0 = arl0)
}

library(furrr)
plan(multisession, workers = 5)
set.seed(24)
system.time(nominal_thresholds_normal <- cbind(true_grid, true_quantiles_normal,
                                               1:36 |> future_map_dfr(find_nominal, skewness = 'normal',
                                                                      .progress = T, .options=furrr_options(seed = TRUE))))
nominal_thresholds_normal
nominal_thresholds_normal |> xtable::xtable()
save(nominal_thresholds_normal, file = 'ic_arl/nominal_thresholds_normal_arma.rds')

set.seed(25) # I had to set a new seed to troubleshoot some issues with the "skewed"
# results and I didn't want to keep rerunning the "normal" results to figure out the "skewed" problem.
system.time(nominal_thresholds_skewed <- cbind(true_grid, true_quantiles_skewed,
                                               1:36 |> future_map_dfr(find_nominal, skewness = 'heavier',
                                                                      .progress = T, .options=furrr_options(seed = TRUE))))
nominal_thresholds_skewed
nominal_thresholds_skewed |> xtable::xtable()
save(nominal_thresholds_skewed, file = 'ic_arl/nominal_thresholds_skewed_arma.rds')


set.seed(26) # similar problem to "skewed" above; new seed for some troubleshooting
# and to ensure reproducibility whilea avoiding running the ones above multiple times.
system.time(nominal_thresholds_gamma <- cbind(true_grid, true_quants_gamma,
                                              1:36 |> future_map_dfr(find_nominal, skewness = 'gamma',
                                                                     .progress = T, .options=furrr_options(seed = TRUE))))
nominal_thresholds_gamma
nominal_thresholds_gamma |> xtable::xtable()
save(nominal_thresholds_gamma, file = 'ic_arl/nominal_thresholds_gamma_arma.rds')



