##########################################
# Code for performing simulations to
# assess false alarm rates of each threshold (AR(1) correlation)
# Taylor Grimm
# January 27th, 2024
##########################################


# Note: any variable or skewness argument that's "normal" refers to the
# F distribution with degrees of freedom 5 and n-5, which is the "normal"
# or standard case under the assumptions of independence and normality.

library(tidyverse)

# Functions to generate a series to be used for determining thresholds
one_series <- function(n = 100, ar = .9, skewness = 'heavier', df1 = 5) {
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
    series <- arima.sim(n = n, model = list(ar = ar), innov = errors, n.start = 100) 
  }
  series
}
one_series_gamma <- function(n = 100, ar = .9, alpha = .05, beta = .05) {
  samp_size <- n
  errors <- rgamma(n + 100, alpha, beta)
  if(ar == 0) {
    # for independent data, the series is just a bunch of independent random error terms
    series <- errors[-c(1:100)]
  } else {
    # for dependent data, introduce AR(1) dependence with arima.sim
    series <- arima.sim(n = n, model = list(ar = ar), innov = errors, n.start = 100)
  }
  series
}

# Function to generate a test set based on the desired experimental factors
ic_test_series <- function(samp_size = 100, ar = .9, skewness = 'heavier', df1 = 5) {
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
    series <- arima.sim(n = n, model = list(ar = ar), innov = errors, n.start = 100) 
  }
  series
}

# Function to find the proportion of false alarms in an IC test set
# given a threshold (threshold_est)
test_false_alarm <- function(ar = 0, skewness = 'normal', samp_size = 100, threshold_est) {
  if(skewness %in% c('normal', 'heavier')) {
    ic_test_data <- ic_test_series(ar = ar, samp_size = samp_size, skewness = skewness)
  } else if (skewness == 'gamma'){
    ic_test_data <- one_series_gamma(ar = ar, n = 2000)
  } else {
    stop('Invalid "skewness" argument.')
  }
  
  fa <- numeric()
  for(threshold in thresholds) {
    fa[threshold] <- mean(ic_test_data > threshold_est[threshold])
  }
  fa
}

# Load in helper functions for adjusted bandwidth nonparametric estimators
source('9_bw_adj_helpers.R')

# Function to get threshold estimates for the desired method using a provided dataset
get_estimates <- function(data, method = 'iid_boot', probs = .995) {
  
  phi_hats <- data |> apply(2, function(x) arima(x, order = c(1,0,0), method = 'CSS-ML')$coef[1])
  n_eff <- function(phi_hat, n = NULL) {
    # Compute effective sample size for adjusted KDE bandwidths (adjusting for dependence)
    n_eff <- max(1, effective_size(n = n, phi = phi_hat))
    l <- ceiling(log(.05) / log(abs(phi_hat)))
    l_use <- min(l, n/2)
    list(n_eff = n_eff, l = l_use)
  }
  neff_and_l <- phi_hats |> map_dfr(n_eff, n = nrow(data))
  
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

ic_false_alarm_sim <- function(ar = 0, samp_size = 100, skewness = 'normal') {
  # step 1: generate an IC sample from which the thresholds will be computed
  
  # use "samp_size" historical observations to train (100, 500, 1000, or 5000)
  if(skewness %in% c('normal', 'heavier')) {
    train_data <- one_series(ar = ar, n = samp_size, skewness = skewness)
  } else if (skewness == 'gamma') {
    train_data <- one_series_gamma(ar = ar, n = samp_size)
  } else {
    stop('Invalid "skewness" argument.')
  }
  
  
  # step 2: compute thresholds from the IC sample
  threshold_est <- numeric()
  for(threshold in thresholds) {
    threshold_est[threshold] <- as.numeric(get_estimates(as.matrix(train_data), method = threshold, probs = 0.995))
  }
  
  # step 3: generate 2000 IC observations, compute the proportion of threshold exceedances
  
  # do this using the "test_false_alarm()" function defined above
  
  # step 4: repeat step 3 5000 times
  results <- replicate(5000, test_false_alarm(ar = ar, skewness = skewness,
                                              samp_size = samp_size, threshold_est = threshold_est))
  
  # step 5: Take the mean of the false alarm proportions as the IC false alarm rate
  rowMeans(results, na.rm = T)
}

# step 6: repeat steps 1-5 100 times to find the actual (unconditional) false alarm rate by taking the average
# of the 100 conditional false alarm rates.
actual_false_alarm <- function(reps = 1000, ar = 0, samp_size = 100, skewness = 'normal') {
  replicate(reps, ic_false_alarm_sim(ar = ar, samp_size = samp_size, skewness = skewness))
}


# Create a grid of experimental factors
all_combinations <- expand_grid(ar = c(0, .1, .5, .9),
                                n = c(100, 500, 1000, 5000))


# For each combination of distribution, sample size, and autocorrelation strength,
# find the average false alarm rate associated with each threshold.
library(furrr)
plan(multisession, workers = 5)
set.seed(100)

# false alarm rate results for "normal" skewness
system.time(all_false_alarm_normal <- future_pmap(all_combinations,
                                             function(ar, n) actual_false_alarm(reps = 100, ar = ar, samp_size = n, skewness = 'normal'),
                                             .progress = T, .options=furrr_options(seed = TRUE)))
cbind(all_combinations, all_false_alarm_normal |> map_dfr(rowMeans))
cbind(all_combinations, all_false_alarm_normal |> map_dfr(~ apply(.x, 1, sd)))
save(all_false_alarm_normal, file = 'false_alarms/all_false_alarm_normal.rds')

# false alarm rate results for "heavier" skewness
system.time(all_false_alarm_skewed <- future_pmap(all_combinations,
                                             function(ar, n) actual_false_alarm(reps = 100, ar = ar, samp_size = n, skewness = 'heavier'),
                                             .progress = T, .options=furrr_options(seed = TRUE)))

cbind(all_combinations, all_false_alarm_skewed |> map_dfr(rowMeans))
cbind(all_combinations, all_false_alarm_skewed |> map_dfr(~ apply(.x, 1, sd)))
save(all_false_alarm_skewed, file = 'false_alarms/all_false_alarm_skewed.rds')

# false alarm rate results for "gamma" skewness
system.time(all_false_alarm_gamma <- future_pmap(all_combinations,
                                            function(ar, n) actual_false_alarm(reps = 100, ar = ar, samp_size = n, skewness = 'gamma'),
                                            .progress = T, .options=furrr_options(seed = TRUE)))
cbind(all_combinations, all_false_alarm_gamma |> map_dfr(rowMeans))
cbind(all_combinations, all_false_alarm_gamma |> map_dfr(~ apply(.x, 1, sd)))
save(all_false_alarm_gamma, file = 'false_alarms/all_false_alarm_gamma.rds')


####################################################################
########## Produce LaTeX tables of false alarm rate results ########
####################################################################

# Load false alarm rate results
load('false_alarms/all_false_alarm_normal.rds')
load('false_alarms/all_false_alarm_skewed.rds')
load('false_alarms/all_false_alarm_gamma.rds')

# helper function to find which method gives a false alarm rate closest to 0.005
closest_to_005 <- function(false_alarm_rate) {
  diff <- false_alarm_rate - 0.005
  which.min(abs(diff))
}
# LaTeX table of F_{5, n-5} error results
normal_alarm_means <- cbind(all_combinations, all_false_alarm_normal |> map_dfr(rowMeans))
normal_alarm_means_reordered <- normal_alarm_means[, c(1, 2, 3, 7, 8, 4, 9, 10, 5, 6)]
colnames(normal_alarm_means_reordered) <- c('phi', 'n', 'Sample Quantile', 'SLVM', 'SCOTT', 'BOOT', 
                                      'ADJ-SLVM', 'ADJ-SCOTT', 'MB-BOOT', 'RB-BOOT')
normal_alarm_means_tab <- normal_alarm_means_reordered
normal_alarm_means_tab |> xtable::xtable(digits = 4)
normal_alarm_means_tab[,-c(1:2)] |> apply(1, closest_to_005)

# LaTeX table of F_{5, 20} error results
skewed_alarm_means <- cbind(all_combinations, all_false_alarm_skewed |> map_dfr(rowMeans))
skewed_alarm_means_reordered <- skewed_alarm_means[, c(1, 2, 3, 7, 8, 4, 9, 10, 5, 6)]
colnames(skewed_alarm_means_reordered) <- c('phi', 'n', 'Sample Quantile', 'SLVM', 'SCOTT', 'BOOT', 
                                      'ADJ-SLVM', 'ADJ-SCOTT', 'MB-BOOT', 'RB-BOOT')
skewed_alarm_means_tab <- skewed_alarm_means_reordered
skewed_alarm_means_tab |> xtable::xtable(digits = 4)
skewed_alarm_means_tab[,-c(1:2)] |> apply(1, closest_to_005)

# LaTeX table of Gamma error results
gamma_alarm_means <- cbind(all_combinations, all_false_alarm_gamma |> map_dfr(rowMeans))
gamma_alarm_means_reordered <- gamma_alarm_means[, c(1, 2, 3, 7, 8, 4, 9, 10, 5, 6)]
colnames(gamma_alarm_means_reordered) <- c('phi', 'n', 'Sample Quantile', 'SLVM', 'SCOTT', 'BOOT', 
                                     'ADJ-SLVM', 'ADJ-SCOTT', 'MB-BOOT', 'RB-BOOT')
gamma_alarm_means_tab <- gamma_alarm_means_reordered
gamma_alarm_means_tab |> xtable::xtable(digits = 4)
gamma_alarm_means_tab[,-c(1:2)] |> apply(1, closest_to_005)



######################################################################################
# False alarm rates when using the "true" quantile values ############################
######################################################################################

# Function to find the true quantile for a given combination of simulation factors
find_true_quantiles <- function(ar = .9, samp_size = 100, skewness = 'heavier', seed = 1234,
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
    series <- arima.sim(n = n, model = list(ar = ar), innov = errors, n.start = 100)
  }
  # estimate empirical quantiles
  quantile(series, probs = probs)
}

# Create a function to generate a long sample that represents the true underlying time series
# for each set of experimental factors
true_series_long <- function(ar = .9, samp_size = 100, skewness = 'heavier', seed = 1234, df1 = 5) {
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
    series <- arima.sim(n = n, model = list(ar = ar), innov = errors, n.start = 100)
  }
}

# Compute the true quantiles for all 12 desired combinations of phi (ar) and sample size
true_grid <- expand_grid(ar = c(0, .1, .5, .9), samp_size = c(100, 500, 1000, 5000)) 
true_quantiles_skewed <- true_grid |> pmap_dfr(find_true_quantiles)
true_quantiles_normal <- true_grid |> pmap_dfr(find_true_quantiles, skewness = 'normal')

true_quant_gamma <- function(ar = .9, samp_size = 100, seed = 1234,
                             probs = .995, alpha = .05, beta = .05) {
  n <- 1e6
  set.seed(seed)
  
  errors <- rgamma(n + 100, alpha, beta)
  if(ar == 0) {
    # for independent data, the series is just a bunch of independent random error terms
    series <- errors[-c(1:100)]
  } else {
    # for dependent data, introduce AR(1) dependence with arima.sim
    series <- arima.sim(n = n, model = list(ar = ar), innov = errors, n.start = 100)
  }
  # estimate empirical quantiles
  quantile(series, probs = probs)
}

true_quants_gamma <- true_grid |> pmap_dfr(true_quant_gamma)

# The true threshold do indeed ensure a false alarm rate of 0.005, but this doesn't translate to
# a run length of 1 / .005 = 200 in the presence of autocorrelation.
true_false_alarms <- function(ar = 0, skewness = 'normal', samp_size = 100, threshold_est) {
  if(skewness %in% c('normal', 'heavier')) {
    ic_test_data <- ic_test_series(ar = ar, samp_size = samp_size, skewness = skewness)
  } else if (skewness == 'gamma'){
    ic_test_data <- one_series_gamma(ar = ar, n = 2000)
  } else {
    stop('Invalid "skewness" argument.')
  }
  
  mean(ic_test_data > threshold_est)
}


get_true_false_alarms <- function(combination, skewness = 'normal', quant_val = NULL) {
  if(skewness == 'normal') {
    true_quants <- cbind(true_grid, true_quantiles_normal) |> as_tibble()
  } else if(skewness == 'heavier') {
    true_quants <- cbind(true_grid, true_quantiles_skewed) |> as_tibble()
  } else {
    true_quants <- cbind(true_grid, true_quants_gamma) |> as_tibble()
  }
  ar <- as.numeric(true_quants[combination, 1])
  samp_size <- as.numeric(true_quants[combination, 2])
  if(is.null(quant_val)) {
    quant_val <- as.numeric(true_quants[combination, 3])
  }
  replicate(1000, true_false_alarms(ar = ar, skewness = skewness, samp_size = samp_size, threshold_est = quant_val)) |> mean()
}

all_true_false_alarms <- function(combination = 1, skewness = 'normal', quant_val = NULL) {
  replicate(100, get_true_false_alarms(combination = combination, skewness = skewness, quant_val = quant_val))
}

# False alarm rates if we use the "true" 0.995 quantile:
# library(furrr)
# plan(multisession, workers = 5)
# set.seed(24)
# 
# system.time(true_normal_false_alarm <- 1:16 |>
#               future_map(all_true_false_alarms, skewness = 'normal',
#             .progress = T, .options=furrr_options(seed = TRUE)))
# true_normal_false_alarm |> map_dbl(mean)
# 
# system.time(true_skewed_false_alarm <- 1:16 |>
#               future_map(all_true_false_alarms, skewness = 'heavier',
#             .progress = T, .options=furrr_options(seed = TRUE)))
# true_skewed_false_alarm |> map_dbl(mean)
# 
# system.time(true_gamma_false_alarm <- 1:16 |>
#               future_map(all_true_false_alarms, skewness = 'gamma',
#             .progress = T, .options=furrr_options(seed = TRUE)))
# true_gamma_false_alarm |> map_dbl(mean)

# All of the false alarm rates above are indeed 0.005 as expected.


##########################################################################################
# Finding the false alarm rates associated with the "nominal" thresholds that are
# required to achieve an ARL_0 value of 200
##########################################################################################
load('ic_arl/nominal_thresholds_normal.rds')
load('ic_arl/nominal_thresholds_skewed.rds')
load('ic_arl/nominal_thresholds_gamma.rds')

library(furrr)
plan(multisession, workers = 5)
set.seed(24)

system.time(all_normal_nominal_false_alarms <- tibble(combination = 1:16,
                                                      nominal = nominal_thresholds_normal$nominal_threshold) |>
              future_pmap(function(combination, nominal) all_true_false_alarms(combination = combination, skewness = 'normal',
                                                                               quant_val = nominal),
                          .progress = T, .options=furrr_options(seed = TRUE)))
# save(all_normal_nominal_false_alarms, file = 'false_alarms/normal_nominal_false_alarms.rds')
all_normal_nominal_false_alarms |> map_dbl(mean) |> round(4)

system.time(all_skewed_nominal_false_alarms <- tibble(combination = 1:16,
                                                      nominal = nominal_thresholds_skewed$nominal_threshold) |>
              future_pmap(function(combination, nominal) all_true_false_alarms(combination = combination, skewness = 'heavier',
                                                                               quant_val = nominal),
                          .progress = T, .options=furrr_options(seed = TRUE)))
# save(all_skewed_nominal_false_alarms, file = 'false_alarms/skewed_nominal_false_alarms.rds')
all_skewed_nominal_false_alarms |> map_dbl(mean) |> round(4)

system.time(all_gamma_nominal_false_alarms <- tibble(combination = 1:16,
                                                     nominal = nominal_thresholds_gamma$nominal_threshold) |>
              future_pmap(function(combination, nominal) all_true_false_alarms(combination = combination, skewness = 'gamma',
                                                                               quant_val = nominal),
                          .progress = T, .options=furrr_options(seed = TRUE)))
# save(all_gamma_nominal_false_alarms, file = 'false_alarms/gamma_nominal_false_alarms.rds')
all_gamma_nominal_false_alarms |> map_dbl(mean) |> round(4)


# load('false_alarms/normal_nominal_false_alarms.rds')
# load('false_alarms/skewed_nominal_false_alarms.rds')
# load('false_alarms/gamma_nominal_false_alarms.rds')


# Table showing the true and adjusted ("nominal") threshold values, along with false alarm rates
# associated with using each "nominal" threshold
# tibble(thresh_norm = nominal_thresholds_normal[,-5] |> round(2),
#       far_norm = all_normal_nominal_false_alarms |> map_dbl(mean) |> round(4),
#       thresh_skewed = nominal_thresholds_skewed[, 3:4] |> round(2),
#       far_skewed = all_skewed_nominal_false_alarms |> map_dbl(mean) |> round(4),
#       thresh_gam = nominal_thresholds_gamma[,3:4] |> round(2),
#       far_gam = all_gamma_nominal_false_alarms |> map_dbl(mean) |> round(4))
