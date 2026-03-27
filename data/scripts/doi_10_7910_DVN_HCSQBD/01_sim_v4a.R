rm(list = ls())

library(hIRT)
library(dplyr)
library(emIRT)
library(ltm)
library(MCMCpack)
library(microbenchmark)

start_time <- Sys.time()

set.seed(02138)

K <- 1000
gamma <- c(0, 1)
N <- 2500
Js <- c(5, 10, 20, 40, 80)
nJ <- length(Js)

# for each N and J run K times (generate H, beta, alpha, x1, x2, eps, y)

alpha_gen <- function(x) c(Inf, sort(runif(x - 1, -x + 1, x - 1), decreasing = TRUE), -Inf)
util2cumprob <- function(util, alpha) plogis(outer(util, alpha, "+"))
cumprob2y <- function(mat) apply(mat, 1, function(x) length(x) - findInterval(runif(1), rev(x)))
dichotomize <- function(x) findInterval(x, c(mean(x, na.rm = TRUE)))
standardize <- function(x) (x-min(x))/(max(x)-min(x))
bv <- Vectorize(between)

out <- array(NA, c(stats = 4, methods = 5, nJ = length(Js)))

Khgrms <- Kgrms <- rep(0, nJ)

for (j in 1:length(Js)) {
  
  J <- Js[j]
  
  simave_bias <- pca_bias <- binIRT_bias <- grm_bias <- hgrm_bias <- 0
  simave_mse <- pca_mse <- binIRT_mse <- grm_mse <- hgrm_mse <- 0
  simave_cvrg <- pca_cvrg <- binIRT_cvrg <- grm_cvrg <- hgrm_cvrg <- 0
  simave_rho <- pca_rho <- binIRT_rho <- grm_rho <- hgrm_rho <- 0
  
  Khgrm <- Kgrm <- K
  
  for (k in seq_len(K)) {
    
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
    y_unif <-  as.data.frame(apply(y, 2, standardize))
    
    # simple average
    simave_scores <- rowSums(y_unif) %>% scale() %>% `*`(sqrt(2))
    simave_lm <-  summary(lm(simave_scores ~ 0 + x))
    # simave_rssd <- sd(simave_lm$residuals)*N/(N-2)
    simave_coef <- simave_lm$coefficients[-1, 1]
    simave_se <- simave_lm$coefficients[-1, 2]
    tmp <-  simave_coef - gamma[-1]
    simave_bias <- simave_bias + tmp
    simave_mse <- simave_mse + tmp^2
    simave_rho <- simave_rho + cor(simave_scores, theta)
    simave_cvrg <- simave_cvrg + bv(gamma[-1], simave_coef - 1.96 * simave_se, simave_coef + 1.96 * simave_se)
    
    # PCA
    pca_scores <- princomp(y, cor = TRUE)$scores[, 1] %>% scale() %>% `*`(sqrt(2))
    pca_lm <- summary(lm(pca_scores ~ 0 + x))
    # pca_rssd <- sd(pca_lm$residuals)*N/(N-2)
    pca_coef <- abs(pca_lm$coefficients[-1, 1])
    pca_se <- pca_lm$coefficients[-1, 2]
    tmp <-  pca_coef - gamma[-1]
    pca_bias <- pca_bias + tmp
    pca_mse <- pca_mse + tmp^2
    pca_rho <- pca_rho + abs(cor(pca_scores, theta))
    pca_cvrg <- pca_cvrg + bv(gamma[-1], pca_coef - 1.96 * pca_se, pca_coef + 1.96 * pca_se)
    
    # fit binIRT
    y_bin2 <- list(votes = as.matrix(y_bin*2-1), n = N, m = J)
    p <- makePriors(y_bin2$n, y_bin2$m, 1)
    s <- getStarts(y_bin2$n, y_bin2$m, 1)
    binIRT_scores <- binIRT(y_bin2, s, p) %>% `[[`("means") %>% `[[`("x") %>% scale() %>% `*`(sqrt(2))
    binIRT_lm <- summary(lm(binIRT_scores ~ 0 + x))
    # binIRT_rssd <- sd(binIRT_lm$residuals)*N/(N-2)
    binIRT_coef <- abs(binIRT_lm$coefficients[-1, 1])
    binIRT_se <- binIRT_lm$coefficients[-1, 2]
    tmp <-  binIRT_coef - gamma[-1]
    binIRT_bias <- binIRT_bias + tmp
    binIRT_mse <- binIRT_mse + tmp^2
    binIRT_rho <- binIRT_rho + abs(cor(binIRT_scores, theta))
    binIRT_cvrg <- binIRT_cvrg + bv(gamma[-1], binIRT_coef - 1.96 * binIRT_se, binIRT_coef + 1.96 * binIRT_se)
    
    # fit hgrm without x 
    grm_obj <-  tryCatch(hgrm(y, control = list(max_iter = 1000)), error = function(e) {print("hgrm without x failed"); NA})
    if (is.na(grm_obj)) {Kgrm <- Kgrm - 1; next}
    grm_scores <- grm_obj$scores$est %>% scale() %>% `*`(sqrt(2))
    grm_lm <- summary(lm(grm_scores ~ 0 + x))
    # grm_rssd <- sd(grm_lm$residuals)*N/(N-2)
    grm_coef <- abs(grm_lm$coefficients[-1, 1])
    grm_se <- grm_lm$coefficients[-1, 2]
    tmp <-  grm_coef - gamma[-1]
    grm_bias <- grm_bias + tmp
    grm_mse <- grm_mse + tmp^2
    grm_rho <- grm_rho + abs(cor(grm_scores, theta))
    grm_cvrg <- grm_cvrg + bv(gamma[-1], grm_coef - 1.96 * grm_se, grm_coef + 1.96 * grm_se)
    
    # fit hgrm
    hgrm_obj <-  tryCatch(hgrm(y, x, control = list(max_iter = 1000)), error = function(e) {print("hgrm failed"); NA})
    if (is.na(hgrm_obj)) {Khgrm <- Khgrm - 1; next}
    hgrm_scores <- hgrm_obj$scores$est
    hgrm_coef <- coef_mean(hgrm_obj) %>% `[`(-1, "Estimate")
    hgrm_se <- coef_mean(hgrm_obj) %>% `[`(-1, "Std_Error")
    tmp <- hgrm_coef - gamma[-1]
    hgrm_bias <- hgrm_bias + tmp
    hgrm_mse <- hgrm_mse + tmp^2
    hgrm_rho <- hgrm_rho + cor(hgrm_scores, theta)
    hgrm_cvrg <- hgrm_cvrg + bv(gamma[-1], hgrm_coef - 1.96 * hgrm_se, hgrm_coef + 1.96 * hgrm_se)
    
  }
  out[, 1, j] <- c(simave_bias, simave_mse, simave_cvrg, simave_rho)/K
  out[, 2, j] <- c(pca_bias, pca_mse, pca_cvrg, pca_rho)/K
  out[, 3, j] <- c(binIRT_bias, binIRT_mse, binIRT_cvrg, binIRT_rho)/K
  out[, 4, j] <- c(grm_bias, grm_mse, grm_cvrg, grm_rho)/Kgrm
  out[, 5, j] <- c(hgrm_bias, hgrm_mse, hgrm_cvrg, hgrm_rho)/Khgrm
  
  Khgrms[j] <- Khgrm
  Kgrms[j] <- Kgrm
}

end_time <- Sys.time()

sim_v4a_time <- end_time - start_time

save.image("sim_v4a.RData")
