#############################################################################################
# Code to determine Phase II IC/OC classification thresholds (control limits)
# for each control chart for independent MVN observations
# Taylor Grimm
# August 15, 2024
#############################################################################################

source('3_helper_functions_independent.R')

# Function to estimate the false alarm rate of a chart using a specified threshold value
far_sim <- function(samp_size = 100, p = 3, chart = 'mewma', method = 'classical', threshold = NULL) {
  # function to find the FAR of a single sample
  far1 <- function(samp_size = 100, p = 3, chart = 'mewma', method = 'classical', threshold = NULL) {
    samp <- mvrnorm(n = samp_size, mu = rep(0, p), Sigma = diag(p))
    samp2 <- mvrnorm(n = 1000, mu = rep(0, p), Sigma = diag(p))
    
    if(chart == 'mewma') {
      mean(mewma(train_data = samp, test_data = samp2,
                 method = method, far = NULL, ic_arl = 200)$mon_stats >= threshold)
    } else if(chart == 't2') {
      mean(hot_t2(train_data = samp, test_data = samp2,
                  method = method, threshold = 'parametric')$mon_stats >= threshold)
    } else if(chart == 'ssmewma') {
      mean(ssmewma(data = rbind(samp, samp2),
                           lambda = 0.1, manual_cl = threshold)$mon_stats[-c(1:samp_size)] >= threshold)
    } else if(chart == 'sst2') {
      mean(sst2(data = rbind(samp, samp2), manual_cl = threshold)$mon_stats[-c(1:samp_size)] >= threshold)
    } else if(chart == 'robust_bayes_t2') {
      mean(robust_bayes(initial_train = samp, test = samp2,
                        chart = 't2', manual_cl = threshold)$mon_stats >= threshold)
    } else if(chart == 'robust_bayes_mewma') {
      mean(robust_bayes(initial_train = samp, test = samp2,
                        chart = 'mewma', manual_cl = threshold)$mon_stats >= threshold)
    } else if(chart == 'ewma_q') {
      mean(ewma_q(train_data = samp, test_data = samp2,
                  lambda = 0.1, bmax = 4)$mon_stats[1] >= threshold)
    } else {
      stop("Invalid control chart argument.")
    }
  }
  # get lots of FAR's from lots of samples
  all_far <- replicate(5000, far1(samp_size = samp_size, p = p, chart = chart,
                                  method = method, threshold = threshold))
  
  mean(all_far)
}

# Function to help find the "0" or root (threshold) that guarantees the specified FAR (0.005)
far_0 <- function(threshold, samp_size = 100, p = 3, method = 'classical',
                  chart = 'mewma', far = 0.005) {
  far_sim(samp_size = samp_size, p = p, method = method,
          chart = chart, threshold = threshold) - far
}


all_combinations <- expand_grid(samp_size = c(30, 50, 100, 250, 500, 1000),
                                p = c(3, 6),
                                method = c('classical', 'rmcd'))


# run in parallel
library(furrr)
plan(multisession, workers = 5)

# Get MEWMA thresholds that ensure a 0.005 false alarm rate
# for all combinations of samp_size and dimension for both classical and RCMD estimation methods
set.seed(24)
system.time(mewma_thresholds_005 <- all_combinations |>
              future_pmap_dbl(function(samp_size, p, method) uniroot(far_0,
                                                                     interval = case_when(samp_size <= 50 ~ c(1, 500),
                                                                                          samp_size == 50 ~ c(1, 250),
                                                                                          samp_size > 50 ~ c(10, 50)),
                                                                     samp_size = samp_size,
                                                                     p = p, method = method, far = 0.005)$root,
                              .progress = T, .options=furrr_options(seed = TRUE)))
all_thresholds_mewma_005 <- cbind(all_combinations, mewma_thresholds_005)
all_thresholds_mewma_005 <- all_thresholds_mewma_005 |>
  pivot_wider(names_from = method, values_from = mewma_thresholds_005) |> 
  mutate(arl200 = map_dbl(p, ~ spc::mewma.crit(.1, 200, .x))) |> 
  arrange(p)
save(all_thresholds_mewma_005, file = 'all_thresholds_mewma_005.rds')
load('all_thresholds_mewma_005.rds')



# Get T2 thresholds that ensure a 0.005 false alarm rate
# for all combinations of samp_size and dimension for both classical and RCMD estimation methods
set.seed(25)
system.time(t2_thresholds_005 <- all_combinations |>
              future_pmap_dbl(function(samp_size, p, method) uniroot(far_0,
                                                                     interval = case_when(samp_size < 50 ~ c(1, 200),
                                                                                          samp_size == 50 ~ c(1, 85),
                                                                                          samp_size > 50 ~ c(5, 30)),
                                                                     chart = 't2',
                                                                     samp_size = samp_size, p = p,
                                                                     method = method, far = 0.005)$root,
                              .progress = T, .options=furrr_options(seed = TRUE)))
all_thresholds_t2_005 <- cbind(all_combinations, t2_thresholds_005)
all_thresholds_t2_005 <- all_thresholds_t2_005 |>
  pivot_wider(names_from = method, values_from = t2_thresholds_005) |> 
  arrange(p, samp_size)
save(all_thresholds_t2_005, file = 'all_thresholds_t2_005.rds')
load('all_thresholds_t2_005.rds')

# Get SSMEWMA thresholds that ensure a 0.005 false alarm rate
# for all combinations of samp_size and dimension
all_combinations <- expand_grid(samp_size = c(30, 50, 100, 250, 500, 1000),
                                p = c(3, 6))
set.seed(26)
system.time(all_thresholds_ssmewma_005 <- all_combinations |>
              future_pmap_dbl(function(samp_size, p) uniroot(far_0, 
                                                             interval = c(1, 30),
                                                             chart = 'ssmewma',
                                                             samp_size = samp_size, p = p,
                                                             far = 0.005)$root,
                              .progress = T, .options=furrr_options(seed = TRUE)))
all_thresholds_ssmewma_005 <- cbind(all_combinations, all_thresholds_ssmewma_005)
all_thresholds_ssmewma_005 <- all_thresholds_ssmewma_005 |> 
  arrange(p)
save(all_thresholds_ssmewma_005, file = 'all_thresholds_ssmewma_005.rds')
load('all_thresholds_ssmewma_005.rds')

# Get SS T2 thresholds that ensure a 0.005 false alarm rate
# for all combinations of samp_size and dimension
all_combinations <- expand_grid(samp_size = c(30, 50, 100, 250, 500, 1000),
                                p = c(3, 6))
set.seed(30)
system.time(all_thresholds_sst2_005 <- all_combinations |>
              future_pmap_dbl(function(samp_size, p) uniroot(far_0, 
                                                             interval = c(1, 30),
                                                             chart = 'sst2',
                                                             samp_size = samp_size, p = p,
                                                             far = 0.005)$root,
                              .progress = T, .options=furrr_options(seed = TRUE)))
all_thresholds_sst2_005 <- cbind(all_combinations, all_thresholds_sst2_005)
all_thresholds_sst2_005 <- all_thresholds_sst2_005 |> 
  arrange(p)
save(all_thresholds_sst2_005, file = 'all_thresholds_sst2_005.rds')
load('all_thresholds_sst2_005.rds')


# Get robust bayes T2 thresholds that ensure a 0.005 false alarm rate
# for all combinations of samp_size and dimension
all_combinations <- expand_grid(samp_size = c(30, 50, 100, 250, 500, 1000),
                                p = c(3, 6))
set.seed(28)
system.time(all_thresholds_robust_bayes_t2_005 <- all_combinations |>
              future_pmap_dbl(function(samp_size, p) uniroot(far_0, 
                                                             interval = c(1, 30),
                                                             chart = 'robust_bayes_t2',
                                                             samp_size = samp_size, p = p,
                                                             far = 0.005)$root,
                              .progress = T, .options=furrr_options(seed = TRUE)))
all_thresholds_robust_bayes_t2_005 <- cbind(all_combinations, all_thresholds_robust_bayes_t2_005)
all_thresholds_robust_bayes_t2_005 <- all_thresholds_robust_bayes_t2_005 |> 
  arrange(p)
save(all_thresholds_robust_bayes_t2_005, file = 'all_thresholds_robust_bayes_t2_005.rds')
load('all_thresholds_robust_bayes_t2_005.rds')


# Get robust bayes mewma thresholds that ensure a 0.005 false alarm rate
# for all combinations of samp_size and dimension
all_combinations <- expand_grid(samp_size = c(30, 50, 100, 250, 500, 1000),
                                p = c(3, 6))
set.seed(29)
system.time(all_thresholds_robust_bayes_mewma_005 <- all_combinations |>
              future_pmap_dbl(function(samp_size, p) uniroot(far_0, 
                                                             interval = c(1, 30),
                                                             chart = 'robust_bayes_mewma',
                                                             samp_size = samp_size, p = p,
                                                             far = 0.005)$root,
                              .progress = T, .options=furrr_options(seed = TRUE)))
all_thresholds_robust_bayes_mewma_005 <- cbind(all_combinations, all_thresholds_robust_bayes_mewma_005)
all_thresholds_robust_bayes_mewma_005 <- all_thresholds_robust_bayes_mewma_005 |> 
  arrange(p)
save(all_thresholds_robust_bayes_mewma_005, file = 'all_thresholds_robust_bayes_mewma_005.rds')
load('all_thresholds_robust_bayes_mewma_005.rds')

