#############################################################################################
# Code to perform the simulation study to compute TDR/FAR values for each
# method under each Phase II monitoring scenario
# Taylor Grimm
# July 22, 2024
#############################################################################################

### Fit each desired control chart, determine signal probability
library(MASS)
library(tidyverse)
library(mvtnorm)

# load in all helper functions
source('3_helper_functions_independent.R')


m0 <- c(30, 50, 100, 250, 500, 1000)
dimension <- c(3, 6)
contamination <- c(0, .1, .2, .3, .4)
errors <- 'mvn' # only considered MVN errors here, but could add additional options such as skewed errors
shift <- c(0, 0.75, 1.5, 2.25, 3)
ph2_scenario <- c(1, 2, 3, 4)

all_sim_combinations <- expand_grid(m0, dimension, contamination, shift, ph2_scenario)

# function to obtain FAR/TDR values for a single generated sample under the provided
# experimental conditons
one_sim <- function(m0 = 500, dimension = 3, contamination = 0, errors = 'mvn', shift = 0,
                    ph2_scenario = 1) {
  
  gen_dat <- mvrnorm(n = m0, mu = rep(0, dimension), Sigma = diag(dimension))
  
  # define the initial "training" set as the first m0 observations
  # with contaminated errors (or without if contamination = 0)
  if(contamination > 0) {
    # determine how many and which observations should have their error terms replaced
    # (use ceiling() in case m0 * contamination is not a whole number)
    num_contaminated <- ceiling(m0 * contamination)
    contamination_indices <- sample(1:m0, size = num_contaminated)
    dat_train <- gen_dat
    dat_train[contamination_indices, ] <- mvrnorm(n = num_contaminated, mu = rep(shift, dimension), Sigma = diag(dimension))
  } else {
    dat_train <- gen_dat
  }
  if(ph2_scenario == 1) {
    dat <- mvrnorm(1000, mu = rep(shift, dimension), Sigma = diag(dimension))
    oc_indices <- 1:1000
    ic_indices <- NULL
  } else if(ph2_scenario == 2) {
    dat <- rbind(mvrnorm(200, mu = rep(0, dimension), Sigma = diag(dimension)),
                 mvrnorm(800, mu = rep(shift, dimension), Sigma = diag(dimension)))
    oc_indices <- c(201:1000)
    ic_indices <- c(1:200)
  } else if(ph2_scenario == 3) {
    dat <- rbind(mvrnorm(200, mu = rep(0, dimension), Sigma = diag(dimension)),
                 mvrnorm(100, mu = rep(shift, dimension), Sigma = diag(dimension)),
                 mvrnorm(200, mu = rep(0, dimension), Sigma = diag(dimension)),
                 mvrnorm(100, mu = rep(shift, dimension), Sigma = diag(dimension)),
                 mvrnorm(200, mu = rep(0, dimension), Sigma = diag(dimension)),
                 mvrnorm(100, mu = rep(shift, dimension), Sigma = diag(dimension)),
                 mvrnorm(100, mu = rep(0, dimension), Sigma = diag(dimension)))
    oc_indices <- c(201:300, 501:600, 801:900)
    ic_indices <- c(1:200, 301:500, 601:800, 901:1000)
  } else if(ph2_scenario == 4) {
    dat <- mvrnorm(1000, mu = rep(0, dimension), Sigma = diag(dimension))
    shifted_obs <- sample(1:1000, 500)
    dat[shifted_obs, ] <- dat[shifted_obs, ] + shift
    oc_indices <- shifted_obs
    ic_indices <- (1:1000)[-shifted_obs]
  }
  
  
  # classical t2 chart
  t2_classical <- hot_t2(dat_train, dat, method = 'classical', threshold = 'simulation')
  t2_classical_tdr <- mean(t2_classical$exceedances[oc_indices])
  t2_classical_far <- mean(t2_classical$exceedances[ic_indices])
  
  # robust t2 chart
  t2_robust <- hot_t2(dat_train, dat, method = 'rmcd', threshold = 'simulation')
  t2_robust_tdr <- mean(t2_robust$exceedances[oc_indices])
  t2_robust_far <- mean(t2_robust$exceedances[ic_indices])
  
  # classical mewma chart
  mewma_classical <- mewma(dat_train, dat, method = 'classical', lambda = 0.1)
  mewma_classical_tdr <- mean(mewma_classical$exceedances[oc_indices])
  mewma_classical_far <- mean(mewma_classical$exceedances[ic_indices])
  
  # robust mewma chart
  mewma_robust <- mewma(dat_train, dat, method = 'rmcd', lambda = 0.1)
  mewma_robust_tdr <- mean(mewma_robust$exceedances[oc_indices])
  mewma_robust_far <- mean(mewma_robust$exceedances[ic_indices])
  
  # SSMEWMA chart
  ssmewma_results <- ssmewma(rbind(dat_train, dat), lambda = 0.1, threshold = 'simulation')
  ssmewma_tdr <- mean(ssmewma_results$exceedances[-c(1:m0)][oc_indices], na.rm = TRUE)
  ssmewma_far <- mean(ssmewma_results$exceedances[-c(1:m0)][ic_indices], na.rm = TRUE)
  
  # robust bayesian t2 chart
  robust_bayes_t2_results <- robust_bayes(initial_train = dat_train, test = dat,
                                          chart = 't2')
  robust_bayes_t2_tdr <- mean(robust_bayes_t2_results$exceedances[oc_indices], na.rm = TRUE)
  robust_bayes_t2_far <- mean(robust_bayes_t2_results$exceedances[ic_indices], na.rm = TRUE)
  
  # robust bayesian mewma
  robust_bayes_mewma_results <- robust_bayes(initial_train = dat_train, test = dat,
                                             chart = 'mewma')
  robust_bayes_mewma_tdr <- mean(robust_bayes_mewma_results$exceedances[oc_indices], na.rm = TRUE)
  robust_bayes_mewma_far <- mean(robust_bayes_mewma_results$exceedances[ic_indices], na.rm = TRUE)
  
  
  list(t2_classical_tdr = t2_classical_tdr,
       t2_classical_far = t2_classical_far,
       t2_robust_tdr = t2_robust_tdr,
       t2_robust_far = t2_robust_far,
       mewma_classical_tdr = mewma_classical_tdr,
       mewma_classical_far = mewma_classical_far,
       mewma_robust_tdr = mewma_robust_tdr,
       mewma_robust_far = mewma_robust_far,
       ssmewma_tdr = ssmewma_tdr,
       ssmewma_far = ssmewma_far,
       robust_bayes_t2_tdr = robust_bayes_t2_tdr,
       robust_bayes_t2_far = robust_bayes_t2_far,
       robust_bayes_mewma_tdr = robust_bayes_mewma_tdr,
       robust_bayes_mewma_far = robust_bayes_mewma_far)
}

# function to obtain FAR/TDR values for many (1000) simulated datasets
get_results <- function(m0 = 200, dimension = 3, contamination = 0, errors = 'mvn', shift = 0, ph2_scenario = 1,
                        reps = 1000) {
  replicate(reps, one_sim(m0 = m0, dimension = dimension, contamination = contamination,
                          errors = errors, shift = shift, ph2_scenario = ph2_scenario)) |>
    apply(1, function(x) mean(flatten_dbl(x)))
}

# In parallel, obtain TDR/FAR results for all methods
library(furrr)
plan(multisession, workers = 5)
set.seed(24)
system.time(all_results <- all_sim_combinations |>
              future_pmap_dfr(function(m0, dimension, contamination,
                                       shift, ph2_scenario) get_results(m0 = m0, dimension = dimension,
                                                                        contamination = contamination, errors = 'mvn',
                                                                        shift = shift, ph2_scenario = ph2_scenario,
                                                                        reps = 1000),
                              .progress = T, .options=furrr_options(seed = TRUE)))
results_df <- cbind(all_sim_combinations, all_results) |> arrange(dimension, m0, contamination,
                                                                  ph2_scenario, shift)
all_tdr_far_independent_005 <- results_df
save(all_tdr_far_independent_005, file = 'all_tdr_far_independent_005.rds')



# Getting SS T2 results (I did this after all the others, so it's separate)

# function to obtain FAR/TDR values for the SS T2 chart for a single generated sample under the provided
# experimental conditons
sst2_one_sim <- function(m0 = 500, dimension = 3, contamination = 0, errors = 'mvn', shift = 0,
                         ph2_scenario = 1) {
  
  gen_dat <- mvrnorm(n = m0, mu = rep(0, dimension), Sigma = diag(dimension))
  
  # define the initial "training" set as the first m0 observations
  # with contaminated errors (or without if contamination = 0)
  if(contamination > 0) {
    # determine how many and which observations should have their error terms replaced
    # (use ceiling() in case m0 * contamination is not a whole number)
    num_contaminated <- ceiling(m0 * contamination)
    contamination_indices <- sample(1:m0, size = num_contaminated)
    dat_train <- gen_dat
    dat_train[contamination_indices, ] <- mvrnorm(n = num_contaminated, mu = rep(shift, dimension), Sigma = diag(dimension))
  } else {
    dat_train <- gen_dat
  }
  if(ph2_scenario == 1) {
    dat <- mvrnorm(1000, mu = rep(shift, dimension), Sigma = diag(dimension))
    oc_indices <- 1:1000
    ic_indices <- NULL
  } else if(ph2_scenario == 2) {
    dat <- rbind(mvrnorm(200, mu = rep(0, dimension), Sigma = diag(dimension)),
                 mvrnorm(800, mu = rep(shift, dimension), Sigma = diag(dimension)))
    oc_indices <- c(201:1000)
    ic_indices <- c(1:200)
  } else if(ph2_scenario == 3) {
    dat <- rbind(mvrnorm(200, mu = rep(0, dimension), Sigma = diag(dimension)),
                 mvrnorm(100, mu = rep(shift, dimension), Sigma = diag(dimension)),
                 mvrnorm(200, mu = rep(0, dimension), Sigma = diag(dimension)),
                 mvrnorm(100, mu = rep(shift, dimension), Sigma = diag(dimension)),
                 mvrnorm(200, mu = rep(0, dimension), Sigma = diag(dimension)),
                 mvrnorm(100, mu = rep(shift, dimension), Sigma = diag(dimension)),
                 mvrnorm(100, mu = rep(0, dimension), Sigma = diag(dimension)))
    oc_indices <- c(201:300, 501:600, 801:900)
    ic_indices <- c(1:200, 301:500, 601:800, 901:1000)
  } else if(ph2_scenario == 4) {
    dat <- mvrnorm(1000, mu = rep(0, dimension), Sigma = diag(dimension))
    shifted_obs <- sample(1:1000, 500)
    dat[shifted_obs, ] <- dat[shifted_obs, ] + shift
    oc_indices <- shifted_obs
    ic_indices <- (1:1000)[-shifted_obs]
  }
  
  # SS T2 chart
  sst2_results <- sst2(rbind(dat_train, dat), threshold = 'simulation')
  sst2_tdr <- mean(sst2_results$exceedances[-c(1:m0)][oc_indices], na.rm = TRUE)
  sst2_far <- mean(sst2_results$exceedances[-c(1:m0)][ic_indices], na.rm = TRUE)
  
  list(sst2_tdr = sst2_tdr,
       sst2_far = sst2_far)
}
sst2_get_results <- function(m0 = 200, dimension = 3, contamination = 0, errors = 'mvn', shift = 0, ph2_scenario = 1,
                             reps = 1000) {
  replicate(reps, sst2_one_sim(m0 = m0, dimension = dimension, contamination = contamination,
                               errors = errors, shift = shift, ph2_scenario = ph2_scenario)) |>
    apply(1, function(x) mean(flatten_dbl(x)))
}

set.seed(25)
system.time(sst2_results <- all_sim_combinations |>
              future_pmap_dfr(function(m0, dimension, contamination,
                                       shift, ph2_scenario) sst2_get_results(m0 = m0, dimension = dimension,
                                                                        contamination = contamination, errors = 'mvn',
                                                                        shift = shift, ph2_scenario = ph2_scenario,
                                                                        reps = 1000),
                              .progress = T, .options=furrr_options(seed = TRUE)))
sst2_results_df <- cbind(all_sim_combinations, sst2_results) |> arrange(dimension, m0, contamination,
                                                                        ph2_scenario, shift)
sst2_tdr_far_independent_005 <- sst2_results_df
save(sst2_tdr_far_independent_005, file = 'sst2_tdr_far_independent_005.rds') 
