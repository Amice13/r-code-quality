# Evaluation analysis----

library(devtools, quietly = TRUE) # 2.4.5
library(dplyr, quietly = TRUE) # 1.1.4
library(estimatr, quietly = TRUE) # 1.0.4
library(ggalluvial, quietly = TRUE) # 0.12.5
library(ggdist, quietly = TRUE) # 0.12.5
library(ggplot2, quietly = TRUE) # 3.5.1
library(ggrepel, quietly = TRUE) # 0.9.5
library(grf, quietly = TRUE) # requires v.>= 2.3.0
library(haven, quietly = TRUE) # 2.5.4
library(kableExtra, quietly = TRUE) # 1.4.0
library(modelsummary, quietly = TRUE) # 2.1.1
library(patchwork, quietly = TRUE) # 1.2.0
library(sf, quietly = TRUE) # 1.0-16
library(tidyr, quietly = TRUE) # 1.3.1

# devtools::install_github('https://github.com/UChicago-pol-methods/banditsCI')
library(banditsCI)

# Multi-Causal Forest

mcf_estimate <- function(df_forest, response = "response", covariates,
                         sw = NULL) {
  if (is.null(sw)) {
    df_forest$sw <- sw <- calc_sw(df_forest, covariates)
  } else {
    stopifnot(
      "Length of sample weights and data frame do not match. " =
        length(sw) == nrow(df_forest)
    )
    df_forest$sw <- sw
  }

  cc_idx <- complete.cases(df_forest[, response], df_forest$treatment_group)
  df_cc <- df_forest[cc_idx, ] # complete cases
  ff <- formula(paste0("~", paste0(covariates, collapse = "+")))
  XX_cc <- model.matrix(ff, df_cc)


  mc_forest <- grf::multi_arm_causal_forest(
    X = XX_cc,
    Y = df_cc[, response],
    W = df_cc$treatment_group,
    sample.weights = df_cc$sw
  )
  tau.hat <- predict(mc_forest, drop = TRUE)$predictions
  Y.hat <- mc_forest$Y.hat
  W.hat <- mc_forest$W.hat

  Y.hat.baseline <- Y.hat - rowSums(W.hat[, -1, drop = FALSE] * tau.hat)

  mc_estimates <- rbind(
    data.frame(
      estimate = mean(Y.hat.baseline),
      std.err = sd(Y.hat.baseline) / sqrt(length(Y.hat.baseline)),
      contrast = "Control mean",
      outcome = "Y"
    ),
    grf::average_treatment_effect(mc_forest)
  )
  rownames(mc_estimates) <- c(
    "(Intercept)",
    paste0(
      "treatment_group",
      gsub(" .*", "", rownames(mc_estimates)[-1])
    )
  )

  mc_estimates$nobs <- length(mc_forest$Y.orig)
  return(mc_estimates)
}


# Calculate stabilized IP weights

calc_sw <- function(df_IP, covariates) {
  df_IP$cens <- ifelse(is.na(df_IP$response), 1, 0)
  ff <- formula(paste0('treatment_group == "Control" ~ ',
                       paste0(covariates, collapse = "+")))
  # estimation of denominator of IP weights for A
  ## predict probability of treatment, conditional on covariates
  XX <- model.matrix(ff, df_IP)
  denom_fit <- grf::probability_forest(
    X = XX,
    Y = df_IP$treatment_group
  )

  ## get probability of treatment actually assigned
  pd_treatment <- predict(denom_fit)[["predictions"]][cbind(
    seq_along(1:nrow(df_IP)),
    df_IP$treatment_group
  )]

  # estimation of numerator of IP weights for A
  ## predict mean probability of treatment
  numer_fit <- grf::probability_forest(
    Y = df_IP$treatment_group,
    X = matrix(1, nrow = nrow(df_IP))
  )
  ## get mean probability of treatment actually assigned
  pn_treatment <- predict(numer_fit)[["predictions"]][cbind(
    seq_along(1:nrow(df_IP)),
    df_IP$treatment_group
  )]

  # estimation of denominator of IP weights for C
  ## predict probability of censoring, conditional on treatment and covariates
  ff_cens <- formula(paste0("cens ~ ", paste0(c("treatment_group", covariates),
                       collapse = "+"
                     )))
  XX_cens <- model.matrix(ff_cens, data = df_IP)
  denom_cens <- grf::regression_forest(
    Y = df_IP$cens,
    X = XX_cens
  )

  ## probability of response
  pd_cens <- 1 - predict(denom_cens)[["predictions"]]
  pd_cens[which(pd_cens< quantile(pd_cens, 0.025))] <- quantile(pd_cens, 0.025)

  # estimation of numerator of IP weights for C
  ## predict probability of censoring, conditional on treatment
  XX_censA <- model.matrix(formula(cens ~ treatment_group), data = df_IP)
  numer_cens <- grf::regression_forest(
    Y = df_IP$cens,
    X = XX_censA
  )
  ## probability of response
  pn_cens <- 1 - predict(numer_cens)[["predictions"]]

  df_IP$sw_a <- pn_treatment / pd_treatment
  df_IP$sw_c <- pn_cens / pd_cens
  sw <- df_IP$sw_c * df_IP$sw_a

  sw
}


# IP Weighting
IP_estimate <- function(df_IP, response = "response", covariates, sw = NULL) {
  df_IP$Y <- df_IP[, response]

  if (is.null(sw)) {
    df_IP$sw <- sw <- calc_sw(df_IP, covariates)
  } else {
    stopifnot(
      "Length of sample weights and data frame do not match. " =
        length(sw) == nrow(df_IP)
    )
    df_IP$sw <- sw
  }

  est_ipw_sw <- estimatr::lm_robust(Y ~ treatment_group,
    data = df_IP,
    weights = sw
  )

  est_ipw_swf <- estimatr::lm_robust(Y ~ treatment_group - 1,
    data = df_IP,
    weights = sw
  )
  return(list(
    ATE = est_ipw_sw,
    means = est_ipw_swf
  ))
}

# Formatting ----
f1 <- function(x) format(round(x, 3), big.mark = ",")
f2 <- function(x) format(round(x, 1), big.mark = ",")
fd <- function(x) {
  paste0(
    "\\$",
    format(round(x, 2), nsmall = 2, big.mark = ",")
  )
}

format_grf_summary <- function(est) {
  model <- list(
    tidy = data.frame(
      term = rownames(est),
      estimate = est[, "estimate"],
      std.error = est[, "std.err"],
      statistic = est[, "estimate"] / est[, "std.err"],
      p.value = 2 * (1 - pnorm(abs(est[, "estimate"] / est[, "std.err"]))),
      group = ""
    ),
    glance = data.frame(nobs = est$nobs[1])
  )
  class(model) <- "modelsummary_list"
  model
}

compare_models <- function(mod_below, mod_above) {
  # Extract coefficients and standard errors
  coefs_below <- broom::tidy(mod_below)
  coefs_above <- broom::tidy(mod_above)

  # Compute differences and their standard errors
  combined <- merge(coefs_below, coefs_above, by = "term", 
                    suffixes = c("_below", "_above"))
  combined$diff_estimate <- combined$estimate_above - combined$estimate_below
  combined$diff_se <- sqrt(combined$std.error_below^2 + 
                             combined$std.error_above^2)

  # Create custom 'difference model'
  diff_model <- list(tidy = data.frame(
    term = combined$term,
    estimate = combined$diff_estimate,
    std.error = combined$diff_se,
    statistic = combined$diff_estimate / combined$diff_se,
    p.value = 2 * (1 - pnorm(abs(combined$diff_estimate / combined$diff_se)))
  ))
  class(diff_model) <- "modelsummary_list"

  # Display results
  list(Below = mod_below, Above = mod_above, Difference = diff_model)
}
