# flexible sensitivity analysis 
# replication codes: application
# Sizhu Lu

library(dplyr)
library(tidyr)
library(plotly)
library(superheat)
library(latex2exp)
library(senstrat)
library(ggplot2)
library(ggrepel)
library(metR)

# ate and att functions
# ate estimator
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

# att estimator
sa_att_point <- function(X, Z, Y, epsilon0,
                         pscore_family="binomial", outcome_family="gaussian",
                         truncpscore=c(0,1)) {
  # pscore model
  pscore_logit <- glm(Z ~ as.matrix(X), family = pscore_family)
  pscore <- predict(pscore_logit, type="response")
  pscore <- pmax(truncpscore[1], pmin(truncpscore[2], pscore))
  odd <- pscore / (1 - pscore)
  
  # outcome model
  mu1_lm <- glm(Y ~ as.matrix(X), weights = Z, family = outcome_family)
  mu1 <- predict(mu1_lm)
  mu0_lm <- glm(Y ~ as.matrix(X), weights = 1-Z, family = outcome_family)
  mu0 <- predict(mu0_lm)
  
  n1 <- sum(Z)
  mu_t1 <- sum(Z * Y) / n1
  
  # 1. pred estimator
  pred_t0 <- sum(Z * epsilon0 * mu0) / n1
  pred <- mu_t1 - pred_t0
  
  # 2. ht estimator
  ht_t0 <- sum(epsilon0 * odd * (1 - Z) * Y) / n1
  ht <- mu_t1 - ht_t0
  
  # 3. hajek estimator
  haj_t0 <- sum(epsilon0 * odd * (1 - Z) * Y) / sum(odd * (1 - Z))
  haj <- mu_t1 - haj_t0
  
  # 4. dr estimator
  dr_t0 <- sum(Z * epsilon0 * mu0 + epsilon0 * odd * (1 - Z) * (Y - mu0)) / n1
  dr <- mu_t1 - dr_t0
  
  c(pred, ht, haj, dr)
}

sa_att_boot <- function(X, Z, Y, epsilon0,
                        pscore_family="binomial", outcome_family="gaussian",
                        n_boot=500, truncpscore=c(0,1)) {
  sa_result <- sa_att_point(X, Z, Y, epsilon0, pscore_family, outcome_family)
  n_boot <- n_boot
  truncpscore <- truncpscore
  
  ## nonparametric bootstrap
  n_sample   <- length(Z)
  X          <- as.matrix(X)
  boot_est   <- replicate(n_boot,
                          {id_boot = sample(1:n_sample, n_sample, replace = TRUE)
                          sa_att_point(X[id_boot, ], Z[id_boot], Y[id_boot], epsilon0,
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

sa_att <- function(X, Z, Y, eps0_list,
                   pscore_family="binomial", outcome_family="gaussian",
                   n_boot=500, truncpscore=c(0,1)) {
  pred <- data.frame()
  ht <- data.frame()
  haj <- data.frame()
  dr <- data.frame()
  for (epsilon0 in eps0_list) {
    res <- sa_att_boot(X, Z, Y, epsilon0, pscore_family, outcome_family)
    res$eps0 <- epsilon0
    pred <- rbind(pred, res[1,])
    ht <- rbind(ht, res[2,])
    haj <- rbind(haj, res[3,])
    dr <- rbind(dr, res[4,])
  }
  rownames(pred) <- NULL
  rownames(ht) <- NULL
  rownames(haj) <- NULL
  rownames(dr) <- NULL
  res_list <- list(pred = pred,
                   ht = ht,
                   haj = haj,
                   dr = dr)
  return(res_list)
}

# user provided: list of eps1 and eps0 values
eps1_list = seq(0.75, 1.25, 0.05)
eps0_list = seq(0.75, 1.25, 0.05)

# application: smoking and homocysteine, Bazzano et al (2003)
# read data "homocyst"
data("homocyst")
# construct dummy variables from categorical variables
homocyst$age_l1 = 1 * (homocyst$age3 == 1)
homocyst$age_l2 = 1 * (homocyst$age3 == 2)
homocyst$ed_l1 = 1 * (homocyst$ed3 == 1)
homocyst$ed_l2 = 1 * (homocyst$ed3 == 2)
homocyst$bmi_l1 = 1 * (homocyst$bmi3 == 1)
homocyst$bmi_l2 = 1 * (homocyst$bmi3 == 2)
# specity the observed covariate vector, treatment indicator, and outcome of interest
X = subset(homocyst, select = c("female", "age_l1", "age_l2", "ed_l1", "ed_l2", "bmi_l1", "bmi_l2", "pov2"))
Z = homocyst$z
Y = homocyst$homocysteine

# output results
ate_results <- sa_ate(X, Z, Y, eps1_list, eps0_list)
att_results <- sa_att(X, Z, Y, eps0_list)

# calibration: leave-one-covariate-out method
# fitted values using all observed covariates
mu1_lm <- glm(Y ~ as.matrix(X), weights = Z, family = outcome_family)
mu1 <- predict(mu1_lm)
mu0_lm <- glm(Y ~ as.matrix(X), weights = 1-Z, family = outcome_family)
mu0 <- predict(mu0_lm)

# drop each observed covariate and save the results
calibration_res <- data.frame()

# drop age
X = subset(homocyst, select = c("female", "ed_l1", "ed_l2", "bmi_l1", "bmi_l2", "pov2"))
e1_no_age = predict(glm(mu1 ~ as.matrix(X), weights = Z, family = outcome_family)) / 
  predict(glm(mu1 ~ as.matrix(X), weights = 1 - Z, family = outcome_family))
e0_no_age = predict(glm(mu0 ~ as.matrix(X), weights = Z, family = outcome_family)) / 
  predict(glm(mu0 ~ as.matrix(X), weights = 1 - Z, family = outcome_family))
calibration_res <- rbind(calibration_res, c(max(e1_no_age), max(e0_no_age), "age"))
colnames(calibration_res) <- c("eps1", "eps0", "covariate")

# drop gender
X = subset(homocyst, select = c("age_l1", "age_l2", "ed_l1", "ed_l2", "bmi_l1", "bmi_l2", "pov2"))
e1_no_gender = predict(glm(mu1 ~ as.matrix(X), weights = Z, family = outcome_family)) / 
  predict(glm(mu1 ~ as.matrix(X), weights = 1 - Z, family = outcome_family))
e0_no_gender = predict(glm(mu0 ~ as.matrix(X), weights = Z, family = outcome_family)) / 
  predict(glm(mu0 ~ as.matrix(X), weights = 1 - Z, family = outcome_family))
calibration_res <- rbind(calibration_res, c(max(e1_no_gender), max(e0_no_gender), "gender"))

# drop education level
X = subset(homocyst, select = c("female", "age_l1", "age_l2", "bmi_l1", "bmi_l2", "pov2"))
e1_no_edu = predict(glm(mu1 ~ as.matrix(X), weights = Z, family = outcome_family)) / 
  predict(glm(mu1 ~ as.matrix(X), weights = 1 - Z, family = outcome_family))
e0_no_edu = predict(glm(mu0 ~ as.matrix(X), weights = Z, family = outcome_family)) / 
  predict(glm(mu0 ~ as.matrix(X), weights = 1 - Z, family = outcome_family))
calibration_res <- rbind(calibration_res, c(max(e1_no_edu), max(e0_no_edu), "education"))

# drop bmi
X = subset(homocyst, select = c("female", "age_l1", "age_l2", "ed_l1", "ed_l2", "pov2"))
e1_no_bmi = predict(glm(mu1 ~ as.matrix(X), weights = Z, family = outcome_family)) / 
  predict(glm(mu1 ~ as.matrix(X), weights = 1 - Z, family = outcome_family))
e0_no_bmi = predict(glm(mu0 ~ as.matrix(X), weights = Z, family = outcome_family)) / 
  predict(glm(mu0 ~ as.matrix(X), weights = 1 - Z, family = outcome_family))
calibration_res <- rbind(calibration_res, c(max(e1_no_bmi), max(e0_no_bmi), "bmi"))

# drop poverty
X = subset(homocyst, select = c("female", "age_l1", "age_l2", "ed_l1", "ed_l2", "bmi_l1", "bmi_l2"))
e1_no_pov = predict(glm(mu1 ~ as.matrix(X), weights = Z, family = outcome_family)) / 
  predict(glm(mu1 ~ as.matrix(X), weights = 1 - Z, family = outcome_family))
e0_no_pov = predict(glm(mu0 ~ as.matrix(X), weights = Z, family = outcome_family)) / 
  predict(glm(mu0 ~ as.matrix(X), weights = 1 - Z, family = outcome_family))
calibration_res <- rbind(calibration_res, c(max(e1_no_pov), max(e0_no_pov), "poverty"))

calibration_res$eps1 <- as.numeric(as.character(calibration_res$eps1))
calibration_res$eps0 <- as.numeric(as.character(calibration_res$eps0))

# construct a contour plot with labeled calibration results
data_to_plot <- ate_results$dr %>% select("est", "eps1", "eps0")
data_to_plot$eps1 <- as.numeric(as.character(data_to_plot$eps1))
data_to_plot$eps0 <- as.numeric(as.character(data_to_plot$eps0))

contour_with_calibration <- ggplot(data_to_plot, aes(eps1, eps0)) +
  geom_contour(aes(z = est), binwidth = 0.5, colour = "black") +
  metR::geom_text_contour(aes(z = est), 
                          skip = 0, 
                          stroke = 0.5) +
  coord_equal() +
  labs(x = TeX(r"($\epsilon_1$)"), y = TeX(r"($\epsilon_0$)")) +
  theme_bw() +
  theme(panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        panel.background = element_blank(),
        axis.text = element_text(size = 10),
        axis.title = element_text(size = 12)) +
  geom_point(data = calibration_res, aes(x = eps1, y = eps0), color = "black", size = 1.5) +
  geom_label_repel(data = calibration_res, aes(x = eps1, y = eps0, label = covariate), 
                   size = 3,
                   fill = "white", color = "black", 
                   box.padding = unit(0.5, "lines"),
                   segment.color = "black") +
  theme(legend.position = "none")

ggsave("contour_with_calibration.pdf", width = 5, height = 4.5, plot = contour_with_calibration, dpi = 300)
