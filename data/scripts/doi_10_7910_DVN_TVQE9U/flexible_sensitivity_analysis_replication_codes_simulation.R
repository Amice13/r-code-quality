# flexible sensitivity analysis 
# replication codes: simulation
# Sizhu Lu

library(dplyr)
library(tidyr)
library(plotly)
library(superheat)
library(latex2exp)
library(senstrat)
library(ggplot2)
library(doParallel)

# function to compute the point estimator of ATE
sa_point <- function(X, Z, Y, epsilon1, epsilon0,
                     pscore_family="binomial", outcome_family="gaussian",
                     truncpscore=c(0,1)) {
  # pscore model
  pscore_logit <- glm(Z ~ as.matrix(X), family=pscore_family)
  pscore <- predict(pscore_logit, type='response')
  pscore <- pmax(truncpscore[1], pmin(truncpscore[2], pscore))
  
  # outcome model
  mu1_lm <- glm(Y ~ as.matrix(X), weights = Z, family=outcome_family)
  mu1 <- predict(mu1_lm)
  mu0_lm <- glm(Y ~ as.matrix(X), weights = 1-Z, family=outcome_family)
  mu0 <- predict(mu0_lm)
  
  # 1. pred estimator
  pred_y1 <- Z * Y + (1 - Z) * mu1 / epsilon1
  pred_y0 <- Z * mu0 * epsilon0 + (1 - Z) * Y
  pred <- mean(pred_y1 - pred_y0)
  
  # 2. ht estimator
  ht_y1 <- (pscore * epsilon1 + 1 - pscore) * Z * Y / (pscore * epsilon1)
  ht_y0 <- (pscore * epsilon0 + 1 - pscore) * (1 - Z) * Y / (1 - pscore)
  ht <- mean(ht_y1 - ht_y0)
  
  # 3. hajek estimator
  haj_y1 <- ht_y1 / mean((Z / pscore))
  haj_y0 <- ht_y0 / mean(((1 - Z) / (1 - pscore)))
  haj <- mean(haj_y1 - haj_y0)
  
  # 4. dr estimator
  dr_y1 <- ht_y1 - (Z - pscore) * mu1 / (pscore * epsilon1)
  dr_y0 <- ht_y0 - (pscore - Z) * mu0 * epsilon0 / (1 - pscore)
  dr <- mean(dr_y1 - dr_y0)
  
  c(pred, ht, haj, dr)
}

# function to compute the variance estimator using nonparametric bootstrap
sa_boot <- function(X, Z, Y, epsilon1, epsilon0,
                    pscore_family="binomial", outcome_family="gaussian",
                    n_boot=500, truncpscore=c(0,1)) {
  
  sa_result <- sa_point(X, Z, Y, epsilon1, epsilon0, pscore_family, outcome_family)
  n_boot <- n_boot
  truncpscore <- truncpscore
  
  ## nonparametric bootstrap
  n_sample   <- length(Z)
  X          <- as.matrix(X)
  boot_est   <- replicate(n_boot,
                          {id_boot = sample(1:n_sample, n_sample, replace = TRUE)
                          sa_point(X[id_boot, ], Z[id_boot], Y[id_boot], epsilon1, epsilon0,
                                   pscore_family, outcome_family, truncpscore)})
  boot_se    <- apply(data.frame(boot_est), 1, sd)
  
  res <- cbind(sa_result, boot_se)
  colnames(res) <- c("est", "boot_se")
  rownames(res) <- c("pred", "ht", "haj", "dr")
  res <- data.frame(res)
  res$p_value <- 2 * pnorm(-abs(res$est / res$boot_se))
  res$ci_lb <- res$est + qnorm(0.025) * res$boot_se
  res$ci_ub <- res$est + qnorm(0.975) * res$boot_se
  return(res)
}

# function for ATE estimation: point and variance estimator
sa_ate <- function(X, Z, Y, eps1_list, eps0_list,
                   pscore_family="binomial", outcome_family="gaussian",
                   n_boot=500, truncpscore=c(0,1)) {
  pred <- data.frame()
  ht <- data.frame()
  haj <- data.frame()
  dr <- data.frame()
  for (epsilon1 in eps1_list){
    for (epsilon0 in eps0_list){
      res <- sa_boot(X, Z, Y, epsilon1, epsilon0, pscore_family, outcome_family)
      res$eps1 <- epsilon1
      res$eps0 <- epsilon0
      pred <- rbind(pred, res[1,])
      ht <- rbind(ht, res[2,])
      haj <- rbind(haj, res[3,])
      dr <- rbind(dr, res[4,])
    }
  }
  rownames(pred) <- NULL
  rownames(ht) <- NULL
  rownames(haj) <- NULL
  rownames(dr) <- NULL
  res_list <- list(pred = pred,
                   ht = ht,
                   haj = haj,
                   dr = dr,
                   eps1_list = eps1_list,
                   eps0_list = eps0_list)
  return(res_list)
}

# function to generate result of one Monte Carlo sample
# log-normal model
get_one_mc_sample_log_normal <- function(b1, suffix="est") {
  # Generate data
  n <- 500
  p <- 2
  
  # generate covariatess
  X12 <- data.frame(matrix(rnorm(n*p, 0, 0.25), n, p))
  X3 <- rbinom(n, 1, 1/2)
  X <- cbind(X12, X3)
  U <- rbinom(n, 1, 1/2)
  
  # generate treatment assignment
  tp <- 1/4 + 1/2 * U
  Z <- rbinom(n, 1, tp)
  
  # generate outcome model
  b1 <- b1
  Y1 <- exp(apply(X12, 1, sum) + b1 * U + rnorm(n, 0, 0.25))
  Y0 <- exp(apply(X12, 1, sum) + b1 * X3 + rnorm(n, 0, 0.25))
  Y <- Z * Y1 + (1-Z) * Y0
  
  # true ATE
  true_tau <- mean(Y1 - Y0)
  
  ate_results <- sa_ate(X, Z, Y, eps1_list, eps0_list, 
                        pscore_family="binomial", outcome_family="gaussian",
                        n_boot=200)
  
  # true_eps1 <- (exp(b1) * 3/4 + 1/4) / (exp(b1) * 1/4 + 3/4)
  res_list <- list(pred = ate_results$pred,
                   ht = ate_results$ht,
                   haj = ate_results$haj,
                   dr = ate_results$dr,
                   true_tau = true_tau)
  # true_eps1 = true_eps1
  save(res_list, file = paste0("simulation_results/b", b1, "_", suffix, "_20231106.RData"))
}

# get the number of available cores and set
num_cores <- detectCores() - 2
cl <- makeCluster(num_cores, type = "FORK")
registerDoParallel(cl)

# simulation by parallel computing
MC <- 500
b1_list <- c(0, 0.2, 0.3, 0.5, 1, 1.5)

# specify list of eps1 values
eps1_list <- c()
for (b1_ind in c(1:length(b1_list))) {
  b1 <- b1_list[b1_ind]
  eps1 <- (exp(b1) * 3/4 + 1/4) / (exp(b1) * 1/4 + 3/4)
  eps1_list <- c(eps1_list, eps1)
}
eps0_list = c(1)

for (b1_ind in c(1:length(b1_list))) {
  b1 <- b1_list[b1_ind]
  foreach (i = 1:MC) %dopar% {
    set.seed(i+2023)
    get_one_mc_sample_log_normal(b1=b1, suffix=i)
  }
}

# process the simulated results
# read results
res_sum <- data.frame()
for (b1_ind in c(1:length(b1_list))) {
  b1 <- b1_list[b1_ind]
  res_vec_sum <- data.frame(matrix(0, ncol = 2, nrow = length(eps1_list)))
  for (i in 1:MC) {
    filename <- paste0("simulation_results/b", b1, "_", i, "_20231106.RData")
    load(filename)
    vec <- res_list$dr %>% 
      mutate(cov_ind = 1 * ((ci_lb < 0) * (ci_ub > 0))) %>% 
      mutate(fd_ind = 1 * (ci_lb > 0)) %>% 
      select(cov_ind, fd_ind)
    res_vec_sum <- res_vec_sum + vec
  }
  colnames(res_vec_sum) <- c("coverage", "false_discovery")
  res_vec_sum <- res_vec_sum %>% 
    mutate(eps1 = eps1_list) %>% 
    mutate(b1 = b1)
  res_sum <- rbind(res_sum, res_vec_sum)
}
res_sum <- res_sum %>% 
  mutate(cov_prob = coverage / MC) %>% 
  mutate(fdr = false_discovery / MC)
save(res_sum, file = paste0("simulation_results_20231107.RData"))
write.csv(res_sum, file = paste0("simulation_results_20231107.csv"))

