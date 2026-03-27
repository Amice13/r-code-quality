rm(list = ls())

library(hIRT)
library(dplyr)
library(emIRT)
library(MCMCpack)
library(microbenchmark)

start_time <- Sys.time()

set.seed(02138)

gamma <- c(0, 1)
K <- 5
Ns <- c(500, 2500, 5000, 7500, 10000)
Js <- c(5, 10, 20, 40)

# for each N and J run K times (generate H, beta, alpha, x1, x2, eps, y)
alpha_gen <- function(x) c(Inf, sort(runif(x - 1, -x + 1, x - 1), decreasing = TRUE), -Inf)
util2cumprob <- function(util, alpha) plogis(outer(util, alpha, "+"))
cumprob2y <- function(mat) apply(mat, 1, function(x) length(x) - findInterval(runif(1), rev(x)))
dichotomize <- function(x) findInterval(x, c(mean(x, na.rm = TRUE)))
out <- array(NA, c(nN = length(Ns), nJ =  length(Js), methods = 3))

for (i in 1:length(Ns)) {
  
  N <- Ns[i]

  for (j in 1:length(Js)) {
    
    J <- Js[j]
    
    res <- rep(0, 3)
    if(N>=5000 && J>=10) K <- 1
    
    for (k in seq_len(K)) {
      
      cat("N", "=", N, "\n")
      cat("J", "=", J, "\n")
      cat("k", "=", k, "\n")
      
      # item parameters
      H <- floor(runif(J, 2, 8))
      beta <- exp(runif(J, -1, 1))
      alpha <- lapply(H, alpha_gen)
      
      # person characteristics
      eps <- rnorm(N, 0, 1)
      x <- cbind(1, x1 = rnorm(N, 0, 1))
      theta <- as.vector(x %*% gamma + eps)
      
      # generate item responses
      util <- as.data.frame(outer(theta, beta))
      cumprob <- Map(util2cumprob, util, alpha)
      y <- sapply(cumprob, cumprob2y)
      y_bin <- as.data.frame(apply(y, 2, dichotomize))
      
      hltm_time <- system.time(hltm(y_bin, x, control = list(max_iter = 300)))[3]
      hgrm_time <- system.time(hgrm(y, x, control = list(max_iter = 300)))[3]
      mcmc_time <- system.time(MCMCirtHier1d(datamatrix = y_bin, Xjdata = x, burnin=2000,
                                             mcmc=10000, thin=20, px = TRUE))[3]
      
      res <- res + c(hltm_time, hgrm_time, mcmc_time)
    }
    out[i, j, ] <- res/K
  }
}

end_time <- Sys.time()

sim_perf_time <- end_time - start_time

save.image("sim_perf_out.RData")
