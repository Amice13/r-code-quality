#############################################################################################################
## Philip Warncke & Ryan E. Carlin                                                                         ##
## Replication script for: "Bad Mood Rising: Bad Mood Rising? Assessing Scalar Invariance Violations       ##
## with Comparative Democratic Support Data (cond. accepted in: Public Opinion Quarterly, Sp. issue 2026:  ##
## Advancing Public Opinion Research Across Countries and Contexts)                                        ##
## Please reach out to Philip Warncke (https://philip-warncke.net/) for queries or bug reports.            ##
#############################################################################################################

################## SCRIPT 4 OUT OF 5: TYPE 2 ERROR SIMULATION FOR MODELS I-V (FIGURE 4) ##################

## Loading required packages
if(!require("pacman")) install.packages('pacman')
library(pacman)
p_load(lavaan)
p_load(tidyverse)
p_load(cmdstanr)
p_load(posterior)
p_load(future)
p_load(future.apply)
p_load(parallel)
p_load(progressr)
p_load(tidyverse)

## Setting global random seed ##
set.seed(42)

############# DEFINING HELPER FUNCTIONS & OBJECTS #############

## Uncertainty helper for maximum likelihood models (I, III)
wilson_ci <- function(x, n, conf.level = 0.95) {
  z <- qnorm(1 - (1 - conf.level)/2)
  p <- x / n
  denom <- 1 + z^2 / n
  center <- (p + z^2/(2*n)) / denom
  half   <- (z * sqrt((p*(1-p) + z^2/(4*n))/n)) / denom
  c(lo = pmax(0, center - half), hi = pmin(1, center + half))
}

### Power (type 2 error) simulation for Models I (scalar invariance CFA) & III Effects-coded CFA
sim_two_group_power <- function(n_per_group = 500, delta = 0, d_mu = 0.5,
                                dif_align = c("oppose","reinforce","mixed")) {
  dif_align <- match.arg(dif_align)
  lambda <- c(0.8, 0.8, 0.8, 0.8)
  J <- length(lambda)
  nu_g1 <- c(0, 0, 0, 0)
  nu_g2 <- switch(
    dif_align,
    reinforce = c(+delta, +delta, 0, 0),
    oppose    = c(-delta, -delta, 0, 0),
    mixed     = c(+delta, -delta, 0, 0))
  theta <- 1 - lambda^2
  
  sim_group <- function(n, mu_eta, nu, lambda, theta) {
    eta <- rnorm(n, mean = mu_eta, sd = 1)
    E   <- matrix(rnorm(n * J, 0, sqrt(theta)), nrow = n, byrow = TRUE)
    Y   <- sweep(E, 2, nu, `+`) + eta %*% t(lambda)
    colnames(Y) <- paste0("y", 1:J)
    as.data.frame(Y)}
  
  g1 <- sim_group(n_per_group, mu_eta = 0.0,  nu = nu_g1, lambda = lambda, theta = theta)
  g2 <- sim_group(n_per_group, mu_eta = d_mu, nu = nu_g2, lambda = lambda, theta = theta)
  rbind(transform(g1, group = 1L), transform(g2, group = 2L))}

## Single factor CFA object to feed into lavaan:
cfa_model <- 'F =~ y1 + y2 + y3 + y4'

## Automatic p-value fixer: lets us choose the correct tail "handedness" under true mean difference (type II error) scenario
p_from_z <- function(z, alternative = c("two.sided","greater","less")) {
  alternative <- match.arg(alternative)
  if (is.na(z)) return(NA_real_)
  switch(alternative,
         "two.sided" = 2 * pnorm(abs(z), lower.tail = FALSE),
         "greater"   = pnorm(z, lower.tail = FALSE),
         "less"      = pnorm(z, lower.tail = TRUE))}

## Model I: Common factor model with scalar MI assumed
fit_scalar_MI <- function(dat, alternative = c("two.sided","greater","less")) {
  alternative <- match.arg(alternative)
  fit <- cfa(cfa_model, data = dat, group = "group",
             meanstructure = TRUE, estimator = "MLR",
             group.equal = c("loadings","intercepts"))
  pe  <- parameterEstimates(fit, standardized = FALSE)
  row <- subset(pe, lhs == "F" & op == "~1" & group == 2)
  z   <- as.numeric(row$est / row$se)
  pval <- p_from_z(z, alternative = alternative)
  c(p = pval, est = row$est)
}

# --- Method B: Effect-coded linking with tail control ---
build_effect_coded_model <- function() {
  'F =~ y1 + y2 + y3 + y4
  # Group 1 factor mean fixed to 0 (ID), Group 2 free
  F ~ c(0, NA)*1

  y1 ~ c(nu11, nu12)*1
  y2 ~ c(nu21, nu22)*1
  y3 ~ c(nu31, nu32)*1
  y4 ~ c(nu41, nu42)*1

  # sum-to-zero constraints within each group
  nu11 + nu21 + nu31 + nu41 == 0
  nu12 + nu22 + nu32 + nu42 == 0'
}

## Model fitting function including costumizable Ha argument
fit_effect_coded <- function(dat, alternative = c("two.sided","greater","less")) {
  alternative <- match.arg(alternative)
  mod <- build_effect_coded_model()
  fit <- cfa(mod, data = dat, group = "group",
             meanstructure = TRUE, estimator = "MLR")
  pe  <- parameterEstimates(fit, standardized = FALSE)
  row <- subset(pe, lhs == "F" & op == "~1" & group == 2)
  z   <- as.numeric(row$est / row$se)
  pval <- p_from_z(z, alternative = alternative)
  c(p = pval, est = row$est)
}

## CPU-parallel type II error grid with customization hypothesis test
run_grid_parallel_power <- function(deltas = seq(0, 0.6, by = 0.05),
                                    reps = 100,
                                    n_per_group = 500,
                                    seed = 2025,
                                    d_mu = 0.5,
                                    dif_align = c("oppose","reinforce","mixed"), ## Dif directionalty: reinforce effect, dilute effect, or oppose effect (used in paper) 
                                    alternative = c("auto","two.sided","greater","less")) {
  
  dif_align   <- match.arg(dif_align)
  alternative <- match.arg(alternative)
  
  # Picking tailed test automatically based on sign of d_mu
  alt_eff <- if (alternative == "auto") {
    if (d_mu > 0) "greater" else if (d_mu < 0) "less" else "two.sided"
  } else alternative
  
  cores <- max(1, parallel::detectCores(logical = TRUE))
  cl    <- parallel::makeCluster(cores)
  on.exit(parallel::stopCluster(cl), add = TRUE)
  
  ## Enforcing quiet model output here
  parallel::clusterSetRNGStream(cl, iseed = seed)
  parallel::clusterEvalQ(cl, { suppressPackageStartupMessages(library(lavaan)); NULL })
  parallel::clusterExport(
    cl,
    varlist = c("sim_two_group_power", "fit_scalar_MI", "fit_effect_coded",
                "build_effect_coded_model", "cfa_model",
                "n_per_group", "d_mu", "dif_align", "alt_eff", "p_from_z"), # export helper
    envir = environment())
  
  all_rows <- lapply(deltas, function(delta) {
    idx <- seq_len(reps)
    
    Model_I <- parallel::parLapplyLB(cl, idx, function(r) {
      dat <- sim_two_group_power(n_per_group = n_per_group,
                                 delta = delta, d_mu = d_mu,
                                 dif_align = dif_align)
      model_I   <- fit_scalar_MI(dat, alternative = alt_eff)
      c(delta = delta,
        method = "Scalar MI (hard)",
        reject = as.numeric(model_I["p"] < 0.05),
        est = model_I["est"])
    })
    
    Model_III <- parallel::parLapplyLB(cl, idx, function(r) {
      dat <- sim_two_group_power(n_per_group = n_per_group,
                                 delta = delta, d_mu = d_mu,
                                 dif_align = dif_align)
      model_III   <- fit_effect_coded(dat, alternative = alt_eff)
      c(delta = delta,
        method = "Effect-coded (hier. link)",
        reject = as.numeric(model_III["p"] < 0.05),
        est = model_III["est"])
    })
    
    rbind(do.call(rbind, Model_I), do.call(rbind, Model_III))
  })
  
  raw <- do.call(rbind, all_rows)
  raw <- as.data.frame(raw, stringsAsFactors = FALSE)
  raw$delta  <- as.numeric(raw$delta)
  raw$reject <- as.numeric(raw$reject)
  raw$est    <- as.numeric(raw$est)
  raw$method <- factor(raw$method,
                       levels = c("Scalar MI (hard)", "Effect-coded (hier. link)"))
  
  agg <- do.call(rbind,
                 lapply(split(raw, list(raw$delta, raw$method), drop = TRUE), function(df) {
                   delta  <- unique(df$delta); method <- unique(df$method)
                   n      <- nrow(df)
                   x      <- sum(df$reject)
                   power  <- x / n
                   ci     <- wilson_ci(x, n, conf.level = 0.95)
                   est    <- mean(df$est, na.rm = TRUE)
                   data.frame(delta = delta, method = method,
                              power = power, n = n,
                              ci_lo = ci["lo"], ci_hi = ci["hi"],
                              est = est)}))
  rownames(agg) <- NULL
  
  list(raw = raw, agg = agg, alternative_used = alt_eff)
}

## Run models in parallel:
# res_type2_I_III <- run_grid_parallel_power(
#   deltas = seq(0, 1.0, by = 0.05),
#   reps = 300,
#   n_per_group = 500,
#   seed = 2025,
#   d_mu = 0.5,              # True group mean difference is 0.5
#   dif_align = "oppose",    # DIF spike attenuates true group mean difference
#   alternative = "auto"     # Right-tailed because d_mu > 0
# )
# 
# res_type2_I_III <- res_type2_I_III$agg
# saveRDS(res_type2_I_III, "res_type2_I_III.rds")

res_type2_I_III <- readRDS("res_type2_I_III.rds")

#### Model II: Bayesian CFA (Asparov and Muthen)
sim_two_group_power <- function(n_per_group = 500, delta = 0, d_mu = 0.5,
                                dif_align = c("oppose","reinforce","mixed")) {
  dif_align <- match.arg(dif_align)
  
  lambda <- c(0.8, 0.8, 0.8, 0.8)  # metric invariance in DGP
  J <- length(lambda)
  
  nu_g1 <- c(0, 0, 0, 0)
  # Group 2 DIF shifted intercepts on items 1 & 2
  nu_g2 <- switch(
    dif_align,
    reinforce = c(+delta, +delta, 0, 0),
    oppose    = c(-delta, -delta, 0, 0),
    mixed     = c(+delta, -delta, 0, 0)
  )
  
  # residual variances so Var(y_j) ≈ 1 when Var(eta)=1
  theta <- 1 - lambda^2
  
  sim_group <- function(n, mu_eta, nu, lambda, theta) {
    eta <- rnorm(n, mean = mu_eta, sd = 1)
    E   <- matrix(rnorm(n * J, 0, sqrt(theta)), nrow = n, byrow = TRUE)
    Y   <- sweep(E, 2, nu, `+`) + eta %*% t(lambda)
    colnames(Y) <- paste0("y", 1:J)
    as.data.frame(Y)
  }
  
  g1 <- sim_group(n_per_group, mu_eta = 0.0,  nu = nu_g1, lambda = lambda, theta = theta)
  g2 <- sim_group(n_per_group, mu_eta = d_mu, nu = nu_g2, lambda = lambda, theta = theta)
  
  rbind(transform(g1, group = 1L), transform(g2, group = 2L))
}

bayes_fit_once <- function(dat,
                           mod,
                           chains = 2,
                           iter_warmup = 1000,
                           iter_sampling = 2000,
                           seed = 2025,
                           tau_nu = 0.10,
                           alpha_tail = 0.05,
                           alternative = c("greater","less")) {
  
  alternative <- match.arg(alternative)
  
  Y   <- as.matrix(dat[, paste0("y", 1:4)])
  grp <- as.integer(dat$group)
  
  stan_data <- list(
    N = nrow(Y),
    J = ncol(Y),
    Y = Y,
    grp = grp,
    tau_nu = tau_nu
  )
  
  fit <- suppressMessages(
    suppressWarnings(
      mod$sample(
        data = stan_data,
        seed = seed,
        chains = chains,
        parallel_chains = 1,
        iter_warmup = iter_warmup,
        iter_sampling = iter_sampling,
        refresh = 0,
        show_exceptions = FALSE,
        show_messages   = FALSE
      )
    )
  )
  
  d   <- as_draws_df(fit$draws("mu[2]"))
  mu2 <- d[["mu[2]"]]
  
  est <- mean(mu2)
  lo  <- as.numeric(quantile(mu2, 0.025))
  hi  <- as.numeric(quantile(mu2, 0.975))
  
  # right- or left-tailed posterior probability
  if (alternative == "greater") {
    tail_prob <- mean(mu2 > 0)
  } else {
    tail_prob <- mean(mu2 < 0)
  }
  reject <- as.numeric(tail_prob > (1 - alpha_tail))  # e.g., > 0.95
  
  c(est = est, lo = lo, hi = hi, tail_prob = tail_prob, reject = reject)
}

run_bayes_replicates_for_delta <- function(delta,
                                           reps = 100,
                                           n_per_group = 500,
                                           d_mu = 0.5,
                                           dif_align = c("oppose","reinforce","mixed"),
                                           mod,
                                           chains = 4,
                                           iter_warmup = 1000,
                                           iter_sampling = 2000,
                                           seed = 2025,
                                           tau_nu = 0.10,
                                           alpha_tail = 0.05) {
  
  dif_align <- match.arg(dif_align)
  
  alternative <- if (d_mu > 0) "greater" else if (d_mu < 0) "less" else "greater"
  
  idx <- seq_len(reps)
  
  rows <- future_lapply(
    idx,
    function(r) {
      dat <- sim_two_group_power(
        n_per_group = n_per_group,
        delta       = delta,
        d_mu        = d_mu,
        dif_align   = dif_align
      )
      out <- try(
        bayes_fit_once(
          dat = dat, mod = mod,
          chains = chains,
          iter_warmup = iter_warmup,
          iter_sampling = iter_sampling,
          seed = seed + r,
          tau_nu = tau_nu,
          alpha_tail = alpha_tail,
          alternative = alternative
        ),
        silent = TRUE
      )
      if (inherits(out, "try-error")) {
        c(est = NA_real_, lo = NA_real_, hi = NA_real_,
          tail_prob = NA_real_, reject = NA_real_)
      } else out
    },
    future.seed = TRUE  # parallel-safe RNG
  )
  
  M <- as.data.frame(do.call(rbind, rows))
  M$reject <- as.numeric(M$reject)
  M <- dplyr::filter(M, !is.na(reject))   # drop failed fits, if any
  
  n <- nrow(M)
  x <- sum(M$reject)
  power <- if (n > 0) x / n else NA_real_
  wil   <- if (n > 0) wilson_ci(x, n) else c(lo = NA_real_, hi = NA_real_)
  
  # across-run posterior-mean summaries (not within-run posterior)
  est_median <- median(M$est, na.rm = TRUE)
  est_lo     <- as.numeric(quantile(M$est, 0.025, na.rm = TRUE))
  est_hi     <- as.numeric(quantile(M$est, 0.975, na.rm = TRUE))
  
  data.frame(
    delta = delta,
    method = sprintf("Bayes soft MI (tau=%.2f)", tau_nu),
    reps = n,
    power = power,
    power_wilson_lo = wil["lo"],
    power_wilson_hi = wil["hi"],
    est_median = est_median,
    est_lo = est_lo,
    est_hi = est_hi
  )
}

run_bayes_grid_power <- function(deltas = seq(0, 1, by = 0.05),
                                 reps = 100,
                                 n_per_group = 500,
                                 chains = 2,
                                 iter_warmup = 1000,
                                 iter_sampling = 3000,
                                 seed = 2025,
                                 tau_nu = 0.10,
                                 d_mu = 0.5,
                                 dif_align = c("oppose","reinforce","mixed"),
                                 workers = max(1, future::availableCores() - 1),
                                 alpha_tail = 0.05,
                                 show_progress = TRUE) {
  
  dif_align <- match.arg(dif_align)
  
  ## Compiling stan model
  mod <- cmdstan_model("mgcfa_soft_scalar.stan")
  
  ## ONE parallel engine used ONLY for reps
  future::plan(future::multisession, workers = workers)
  
  ## Delta-based progress bar
  nD <- length(deltas)
  if (show_progress) {
    pb <- utils::txtProgressBar(min = 0, max = nD, style = 3)
    on.exit(try(close(pb), silent = TRUE), add = TRUE)
  }
  
  out <- vector("list", nD)
  t0 <- proc.time()[["elapsed"]]
  
  for (i in seq_along(deltas)) {
    d <- deltas[i]
    out[[i]] <- run_bayes_replicates_for_delta(
      delta = d,
      reps  = reps,
      n_per_group = n_per_group,
      d_mu  = d_mu,
      dif_align = dif_align,
      mod = mod,
      chains = chains,
      iter_warmup = iter_warmup,
      iter_sampling = iter_sampling,
      seed = seed + round(d * 1e4),
      tau_nu = tau_nu,
      alpha_tail = alpha_tail
    )
    if (show_progress) utils::setTxtProgressBar(pb, i)
  }
  
  t1 <- proc.time()[["elapsed"]]
  
  dplyr::bind_rows(out)
}

#####  Running Bayesian CFA in parallel   #####
# set.seed(42)
# res_type2_II <- run_bayes_grid_power(
#   deltas = seq(0, 1, by = 0.05),
#   reps = 100,
#   n_per_group = 500,
#   chains = 2,
#   iter_warmup = 750,
#   iter_sampling = 2000,
#   seed = 2025,
#   tau_nu = 0.10,
#   d_mu = 0.5,             # true μ2 - μ1 > 0
#   dif_align = "oppose",   # DIF fights the true difference
#   workers = max(1, future::availableCores() - 1),
#   alpha_tail = 0.05,      # Pr(mu2>0) > 0.95 => reject
#   show_progress = TRUE
# )
# 
# saveRDS(res_type2_II, "res_type2_II.rds")
res_type2_II <- readRDS("res_type2_II.rds")

######################## IRT MODELS ########################

### Model IV: Two-group effects-coded IRT
sim_two_group_irt_power <- function(n_per_group = 500,
                                    delta = 0,
                                    d_mu = 0.5,
                                    dif_align = c("oppose","reinforce","mixed"),
                                    dif_pattern = c("two_items","uniform"),
                                    alpha = rep(1.0, 4)) {
  dif_align   <- match.arg(dif_align)
  dif_pattern <- match.arg(dif_pattern)
  J <- length(alpha)
  stopifnot(J == 4L)
  
  # group means
  mu1 <- 0
  mu2 <- d_mu
  
  # Group-1 thresholds (baseline); keep simple at zero (not centered on purpose)
  gamma1 <- rep(0, J)
  
  # Group-2 raw shifts (NO centering here!)
  if (dif_pattern == "two_items") {
    # shift items 1 & 2 only
    base <- switch(
      dif_align,
      oppose    = c(+delta, +delta, 0, 0),  # Reinforcing true mean difference
      reinforce = c(-delta, -delta, 0, 0),  # Opposing true mean difference
      mixed     = c(+delta, -delta, 0, 0)   # Diffusing true mean difference
    )
    gamma2 <- base
  } else {
    gamma2 <- switch(
      dif_align,
      oppose    = rep(+delta, J), 
      reinforce = rep(-delta, J),
      mixed     = c(+delta, +delta, -delta, -delta)
    )
  }
  
  # Simulate abilities (unit variance identification)
  N <- 2 * n_per_group
  g <- rep(1:2, each = n_per_group)
  theta <- rnorm(N, mean = ifelse(g == 1, mu1, mu2), sd = 1)
  
  # Responses: p = Phi(alpha_j * theta - gamma_{jg})
  gamma_mat <- cbind(gamma1, gamma2)  # J x 2
  Y <- matrix(0L, nrow = N, ncol = J)
  for (n in 1:N) {
    for (j in 1:J) {
      lp <- alpha[j] * theta[n] - gamma_mat[j, g[n]]
      Y[n, j] <- as.integer(runif(1) < pnorm(lp))
    }
  }
  colnames(Y) <- paste0("y", 1:J)
  
  list(N = N, J = J, g = g, Y = Y)
}

build_stan_data_irt <- function(dat) {
  list(
    N = dat$N,
    J = dat$J,
    g = as.integer(dat$g),
    Y = array(dat$Y, dim = c(dat$N, dat$J))  # cmdstanr likes explicit array dims
  )
}

wilson_ci <- function(x, n, conf.level = 0.95) {
  z <- qnorm(1 - (1 - conf.level)/2)
  p <- x / n
  denom  <- 1 + z^2 / n
  center <- (p + z^2/(2*n)) / denom
  half   <- (z * sqrt((p*(1-p) + z^2/(4*n))/n)) / denom
  c(lo = pmax(0, center - half), hi = pmin(1, center + half))
}

fit_irt_effect_coded_once <- function(dat,
                                      mod,
                                      chains = 4,
                                      iter_warmup = 1000,
                                      iter_sampling = 1500,
                                      seed = 2025,
                                      alpha_tail = 0.05) {
  
  sdev <- build_stan_data_irt(dat)
  
  fit <- suppressWarnings(suppressMessages(
    mod$sample(
      data = sdev,
      seed = seed,
      chains = chains,
      parallel_chains = 1,
      iter_warmup = iter_warmup,
      iter_sampling = iter_sampling,
      refresh = 0,
      show_messages   = FALSE,
      show_exceptions = FALSE
    )
  ))
  
  d   <- as_draws_df(fit$draws("mu2"))
  mu2 <- d[["mu2"]]
  
  est <- mean(mu2)
  lo  <- as.numeric(quantile(mu2, 0.025))
  hi  <- as.numeric(quantile(mu2, 0.975))
  tail_prob <- mean(mu2 > 0)               # Right-tailed hypothesis test assessed
  reject    <- as.numeric(tail_prob > (1 - alpha_tail)) 
  
  c(est = est, lo = lo, hi = hi, tail_prob = tail_prob, reject = reject)
}

run_irt_replicates_for_delta <- function(delta,
                                         reps = 100,
                                         n_per_group = 500,
                                         d_mu = 0.5,
                                         dif_align = c("oppose","reinforce","mixed"),
                                         mod,
                                         chains = 4,
                                         iter_warmup = 1000,
                                         iter_sampling = 1500,
                                         seed = 2025,
                                         alpha_tail = 0.05) {
  dif_align <- match.arg(dif_align)
  idx <- seq_len(reps)
  
  rows <- future_lapply(
    idx,
    future.seed = TRUE,
    function(r) {
      dat <- sim_two_group_irt_power(
        n_per_group = n_per_group,
        delta       = delta,
        d_mu        = d_mu,
        dif_align   = dif_align
      )
      out <- try(
        fit_irt_effect_coded_once(
          dat  = dat,
          mod  = mod,
          chains = chains,
          iter_warmup = iter_warmup,
          iter_sampling = iter_sampling,
          seed = seed + r,
          alpha_tail = alpha_tail
        ),
        silent = TRUE
      )
      if (inherits(out, "try-error")) {
        c(est = NA_real_, lo = NA_real_, hi = NA_real_,
          tail_prob = NA_real_, reject = NA_real_)
      } else out
    }
  )
  
  M <- as.data.frame(do.call(rbind, rows))
  M$reject <- as.numeric(M$reject)
  M <- dplyr::filter(M, !is.na(reject))  # drop failed fits if any
  
  n <- nrow(M)
  x <- sum(M$reject)
  power <- if (n > 0) x / n else NA_real_
  wil   <- if (n > 0) wilson_ci(x, n) else c(lo = NA_real_, hi = NA_real_)
  
  # Across-rep posterior-mean summaries (not within-rep posteriors)
  est_median <- median(M$est, na.rm = TRUE)
  est_lo     <- as.numeric(quantile(M$est, 0.025, na.rm = TRUE))
  est_hi     <- as.numeric(quantile(M$est, 0.975, na.rm = TRUE))
  
  data.frame(
    delta = delta,
    method = "IRT effect-coded (metric inv.)",
    reps = n,
    power = power,
    power_wilson_lo = wil["lo"],
    power_wilson_hi = wil["hi"],
    est_median = est_median,
    est_lo = est_lo,
    est_hi = est_hi
  )
}

run_bayes_grid_irt_power <- function(deltas = seq(0, 0.6, by = 0.05),
                                     reps = 100,
                                     n_per_group = 500,
                                     chains = 4,
                                     iter_warmup = 1000,
                                     iter_sampling = 1500,
                                     seed = 2025,
                                     workers = max(1, future::availableCores() - 1),
                                     d_mu = 0.5,                       # true μ2 - μ1
                                     dif_align = c("oppose","reinforce","mixed"),
                                     alpha_tail = 0.05,                # right-tail 5%
                                     show_progress = TRUE) {
  
  dif_align <- match.arg(dif_align)
  
  # Compiling Stan model
  mod <- cmdstan_model("irt_effect_coded.stan")
  
  # One parallel engine running across reps
  future::plan(future::multisession, workers = workers)
  set.seed(seed)
  
  # Adding progress bar over delta grid
  nD <- length(deltas)
  if (show_progress) {
    pb <- utils::txtProgressBar(min = 0, max = nD, style = 3)
    on.exit(try(close(pb), silent = TRUE), add = TRUE)
  }
  
  out <- vector("list", nD)
  t0  <- Sys.time()
  for (i in seq_along(deltas)) {
    d <- deltas[i]
    out[[i]] <- run_irt_replicates_for_delta(
      delta = d,
      reps  = reps,
      n_per_group = n_per_group,
      d_mu  = d_mu,
      dif_align = dif_align,
      mod = mod,
      chains = chains,
      iter_warmup = iter_warmup,
      iter_sampling = iter_sampling,
      seed = seed + round(d * 1e4),
      alpha_tail = alpha_tail
    )
    if (show_progress) utils::setTxtProgressBar(pb, i)
  }
  t1 <- Sys.time()
  if (show_progress) {
    message(sprintf("\nCompleted %d deltas in %0.1f min.",
                    nD, as.numeric(difftime(t1, t0, units = "mins"))))
  }
  
  dplyr::bind_rows(out)
}

# set.seed(42)
# res_irt_power_tg <- run_bayes_grid_irt_power(
#   deltas = seq(0, 1, by = 0.05),
#   reps   = 100,
#   n_per_group = 500,
#   chains = 2,
#   iter_warmup = 750,
#   iter_sampling = 2000,
#   seed = 2025,
#   workers = max(1, future::availableCores() - 1),
#   d_mu = 0.5,             # true μ2 - μ1 > 0  (direction sets right-tailed test)
#   dif_align = "oppose",   # DIF fights the true difference
#   alpha_tail = 0.05,      # reject if Pr(mu2>0) > 0.95
#   show_progress = TRUE
# )
# 
# saveRDS(res_type2_IV, 'res_type2_IV.rds')
res_type2_IV <- readRDS('res_type2_IV.rds')


######## Model V: Multiple group IRT with effects-coded identification

init_safe <- function(N, G, J) {
  list(
    theta        = rep(0, N),
    mu_free      = rep(0, G - 1),
    gamma_bar    = rep(0, J),
    logalpha_bar = rep(0, J),
    tau_gamma    = rep(0.1, J),
    tau_logalpha = rep(0.1, J),
    gamma_raw    = matrix(0, nrow = J, ncol = G),
    logalpha_raw = matrix(0, nrow = J, ncol = G)
  )
}
make_inits <- function(chains, N, G, J) {
  replicate(chains, init_safe(N, G, J), simplify = FALSE)
}

sim_mg_irt_2pl_probit_mu <- function(G = 10, n_per_group = 100, J = 4,
                                     mu = rep(0, G),
                                     per_group_delta = NULL,
                                     alpha = rep(1.0, J),
                                     gamma_base = rep(0, J)) {
  N <- G * n_per_group
  
  # Setting different thresholds by group
  gamma <- matrix(rep(gamma_base, G), nrow = J, ncol = G)
  if (!is.null(per_group_delta)) {
    stopifnot(length(per_group_delta) == G)
    gamma[1, ] <- gamma[1, ] + per_group_delta
    gamma[2, ] <- gamma[2, ] + per_group_delta
  }
  
  # simulate abilities
  g_id  <- rep(seq_len(G), each = n_per_group)
  theta <- rnorm(N, mean = mu[g_id], sd = 1)
  
  # responses: probit 2PL
  Y <- matrix(0L, nrow = N, ncol = J)
  for (n in 1:N) {
    for (j in 1:J) {
      p <- pnorm(alpha[j] * theta[n] - gamma[j, g_id[n]])
      Y[n, j] <- as.integer(runif(1) < p)
    }
  }
  
  list(N = N, G = G, J = J, g = g_id, Y = Y,
       true_mu = mu, true_gamma = gamma, true_alpha = alpha)
}

# -------------------------------------------------------
# DIF vector for "best-case" power test:
#   - only the target group has DIF (items 1 & 2) with +delta
# -------------------------------------------------------
make_per_group_delta_target_only <- function(G, target_group, delta, sign = +1) {
  d <- rep(0, G)
  d[target_group] <- sign * delta
  d
}

# -------------------------------------------------------
# Single Stan fit for one dataset; test H0 via 95% CrI
# -------------------------------------------------------
bayes_fit_once_irt_power <- function(dat,
                                     mod,
                                     chains = 4,
                                     iter_warmup = 1000,
                                     iter_sampling = 1500,
                                     seed = 2025,
                                     ref_group = 1,
                                     target_group = 5,
                                     true_diff = 0.5) {
  
  stan_data <- list(
    N = dat$N, G = dat$G, J = dat$J,
    g = as.integer(dat$g),
    Y = array(dat$Y, dim = c(dat$N, dat$J))
  )
  inits <- make_inits(chains, dat$N, dat$G, dat$J)
  
  fit <- mod$sample(
    data = stan_data,
    init = inits,
    seed = seed,
    chains = chains,
    parallel_chains = 1,  # parallelize across reps, not chains
    iter_warmup = iter_warmup,
    iter_sampling = iter_sampling,
    refresh = 0,
    show_exceptions = FALSE
  )
  
  # Reference group's mean is 0 by identification; grab mu[target]
  par_name <- sprintf("mu[%d]", target_group)
  mu_t <- as_draws_df(fit$draws(par_name))[[par_name]]
  est <- mean(mu_t)
  lo  <- as.numeric(quantile(mu_t, 0.025))
  hi  <- as.numeric(quantile(mu_t, 0.975))
  reject <- as.numeric(!(lo <= 0 && 0 <= hi))  # 95% CrI excludes 0 → reject H0
  
  # bias of posterior mean vs. true contrast (0.5)
  bias <- est - true_diff
  
  c(est = est, lo = lo, hi = hi, reject = reject, bias = bias)
}

## Run delta grid for type II error simulation (Model V)
run_power_reps_for_delta <- function(delta,
                                     reps = 100,
                                     G = 10, n_per_group = 100, J = 4,
                                     mod,
                                     chains = 4,
                                     iter_warmup = 1000,
                                     iter_sampling = 1500,
                                     seed = 2025,
                                     ref_group = 1,
                                     target_group = 5,
                                     true_diff = 0.5) {
  
  # True group means for power test
  mu <- rep(0, G)
  mu[target_group] <- true_diff
  
  # DIF vector: only target group shifted (best-case partial pooling)
  per_group_delta <- make_per_group_delta_target_only(G, target_group, delta, sign = +1)
  
  idx <- seq_len(reps)
  p <- progressr::progressor(along = idx)
  
  rows <- future.apply::future_lapply(
    idx,
    FUN = function(r) {
      dat <- sim_mg_irt_2pl_probit_mu(
        G = G, n_per_group = n_per_group, J = J,
        mu = mu,
        per_group_delta = per_group_delta
      )
      out <- try(
        bayes_fit_once_irt_power(
          dat = dat, mod = mod,
          chains = chains,
          iter_warmup = iter_warmup,
          iter_sampling = iter_sampling,
          seed = seed + r,
          ref_group = ref_group,
          target_group = target_group,
          true_diff = true_diff
        ),
        silent = TRUE
      )
      p(sprintf("Delta=%.2f rep=%d", delta, r))
      if (inherits(out, "try-error")) {
        c(est = NA_real_, lo = NA_real_, hi = NA_real_, reject = NA_real_, bias = NA_real_)
      } else out
    },
    future.seed = TRUE  # parallel-safe RNG
  )
  
  M <- as.data.frame(do.call(rbind, rows))
  M$reject <- as.numeric(M$reject)
  M <- dplyr::filter(M, !is.na(reject))   # drop failed fits
  
  n <- nrow(M)
  x <- sum(M$reject)
  power <- if (n > 0) x / n else NA_real_
  ci    <- if (n > 0) wilson_ci(x, n) else c(lo = NA_real_, hi = NA_real_)
  bias_med <- median(M$bias, na.rm = TRUE)
  est_med  <- median(M$est,  na.rm = TRUE)
  est_lo   <- as.numeric(quantile(M$est, 0.025, na.rm = TRUE))
  est_hi   <- as.numeric(quantile(M$est, 0.975, na.rm = TRUE))
  
  data.frame(
    delta = delta,
    method = "Bayes hierarchical IRT (effect-coded)",
    reps = n,
    power = power,                     # Pr(reject H0) = 1 - Type II
    power_wilson_lo = ci["lo"],
    power_wilson_hi = ci["hi"],
    est_median = est_med,              # across-rep posterior-mean summary
    est_lo = est_lo,
    est_hi = est_hi,
    bias_median = bias_med             # across-rep median bias vs true_diff
  )
}

## Master delta grid (CPU-parallel) runner for Model V type II error simulation 
run_power_grid <- function(deltas = seq(0, 0.6, by = 0.05),
                           reps = 100,
                           G = 10, n_per_group = 100, J = 4,
                           chains = 4,
                           iter_warmup = 1000,
                           iter_sampling = 1500,
                           seed = 2025,
                           workers = max(1, parallel::detectCores(logical = FALSE) - 1),
                           ref_group = 1,
                           target_group = 5,
                           true_diff = 0.5) {
  
  mod <- cmdstan_model("irt_mg_effect_coded.stan")
  
  future::plan(future::multisession, workers = workers)
  set.seed(seed)
  
  results <- vector("list", length(deltas))
  
  progressr::with_progress({
    p_all <- progressr::progressor(steps = length(deltas))
    for (i in seq_along(deltas)) {
      d <- deltas[i]
      results[[i]] <- run_power_reps_for_delta(
        delta = d, reps = reps,
        G = G, n_per_group = n_per_group, J = J,
        mod = mod,
        chains = chains,
        iter_warmup = iter_warmup,
        iter_sampling = iter_sampling,
        seed = seed + round(d * 1e6),
        ref_group = ref_group,
        target_group = target_group,
        true_diff = true_diff
      )
      p_all(message = sprintf("Finished delta = %.2f", d))
    }
  })
  
  dplyr::bind_rows(results)
}

#### Running model model type II error simulation
# progressr::handlers(global = TRUE); handlers("txtprogressbar")
# set.seed(42)
# res_type2_V <- run_power_grid(
#   deltas = seq(0, 1, by = 0.05),
#   reps   = 10,
#   G = 10, n_per_group = 100, J = 4,
#   chains = 2,
#   iter_warmup = 1200,
#   iter_sampling = 2000,
#   seed = 2025,
#   workers = max(1, parallel::detectCores()),
#   ref_group = 1,
#   target_group = 2,
#   true_diff = 0.5
# )
# 
# saveRDS(res_type2_V, 'res_type2_V.rds')
res_type2_V <- readRDS('res_type2_V.rds')

######## FIGURE 2: PLOTTING RESULTS OF TYPE 2 ERROR SIMULATION FOR MODELS I THROUGH 5 ########

### Helper function
import_cleaner <- function(dat) {
  dat <- dat %>% dplyr::select(any_of(c("delta", "method", "power", "type2_wilson_lo", "type2_wilson_hi")))
  colnames(dat) <- c("delta", "method", "power", "ci_lo", "ci_hi")
  dat
}

res_type2_I_III <- readRDS("res_type2_I_III.rds")
res_type2_I_III$type2_wilson_lo  <- res_type2_I_III$ci_lo
res_type2_I_III$type2_wilson_hi <- res_type2_I_III$ci_hi
res_type2_I_III <-  import_cleaner(res_type2_I_III)

res_type2_II <- readRDS("res_type2_II.rds")
res_type2_II$type2_wilson_lo  <- res_type2_II$power_wilson_lo
res_type2_II$type2_wilson_hi <- res_type2_II$power_wilson_hi
res_type2_II <- res_type2_II %>% import_cleaner()

res_type2_IV <- readRDS("res_type2_IV.rds")
res_type2_IV$type2_wilson_lo  <- res_type2_IV$power_wilson_lo
res_type2_IV$type2_wilson_hi <- res_type2_IV$power_wilson_hi
res_type2_IV$power_wilson_lo <- NULL
res_type2_IV$power_wilson_hi <- NULL
res_type2_IV <- res_type2_IV %>% import_cleaner()

res_type2_V <- readRDS("res_type2_V.rds")
res_type2_V$type2_wilson_lo  <- res_type2_V$power_wilson_lo
res_type2_V$type2_wilson_hi <- res_type2_V$power_wilson_hi
res_type2_V$power_wilson_lo <- NULL
res_type2_V$power_wilson_hi <- NULL
res_type2_V <- res_type2_V %>% import_cleaner()

grid_res <- rbind(res_type2_I_III, res_type2_II, res_type2_IV, res_type2_V) 

grid_res$method <- factor(grid_res$method, levels = c(
  "Scalar MI (hard)",
  "Bayes soft MI (tau=0.10)",
  "Effect-coded (hier. link)",
  "IRT effect-coded (metric inv.)",
  "Bayes hierarchical IRT (effect-coded)"))

## Reference line locations (empiticallt estimated in Chile-Costa Rica comparison)
churchill <- 0.61
sing_part <- 0.40
strong_man <- 0.63
expert_rule <- 1.47

grand_mean <- mean(c(churchill, sing_part, strong_man, expert_rule))

p4 <- ggplot(grid_res, aes(x = delta, y = power, color = method)) +
  geom_hline(yintercept = 0, linewidth = 0.3, alpha = 0.5)+
  geom_vline(xintercept = 0, linewidth = 0.3, alpha = 0.5)+
  geom_line() +
  geom_point() +
  geom_errorbar(aes(ymin = ci_lo, ymax = ci_hi), width = 0.015, alpha = 0.7) +
  # geom_hline(yintercept = 0.05, linetype = 2, alpha = 0.5, linewidth = 0.45) +
  # annotate("text", x = min(grid_res$delta) + 0.02, y = 0.05,
  #          label = "p = 0.05 threshold", hjust = 0, vjust = -0.5, size = 2.5)+
  geom_vline(xintercept = grand_mean, linetype = 2, linewidth = 0.9, color = "#222121")+
  annotate("text", x = grand_mean + 0.00025, y = 1, angle = 270,
           label = "", hjust = 0, 
           vjust = -0.5, size = 2.7, color = "#222121")+
  geom_vline(xintercept = sing_part, linetype = 3, linewidth = 0.9, color = "#424040")+
  annotate("text", x = sing_part + 0.00025, y = 0.56, angle = 270,
           label = "'Single-party' threshold difference", hjust = 0, 
           vjust = -0.5, size = 2.7, color = "#424040")+
  labs(x = "Severity of directional DIF (delta item intercept/threshold shift)",
       y = "Statistical power 1-P(reject Ha: μ1 < μ2 at p < .05)",
       color = "Empirical method") +
  scale_color_manual(
    values = c(
      "Scalar MI (hard)" = "darkred",
      "Effect-coded (hier. link)" = "#d95f02",
      "Bayes soft MI (tau=0.10)" = "blue",
      "IRT effect-coded (metric inv.)" = "purple",
      "Bayes hierarchical IRT (effect-coded)" = "#1b9e77"),
    labels = c(
      "Scalar MI (hard)" = "Model I: CFA (scalar inv.)",
      "Bayes soft MI (tau=0.10)" = "Model II: Bayes soft MI CFA",
      "Effect-coded (hier. link)" = "Model III: Effect-coded CFA",
      "IRT effect-coded (metric inv.)" = "Model IV: Effect-coded IRT \n (2 grps.)",
      "Bayes hierarchical IRT (effect-coded)" = "Model V: Effect-coded IRT \n (10 grps.)")) +
  guides(color = guide_legend(nrow = 3, byrow = TRUE))+
  theme_minimal()+
  theme(panel.grid.major.x = element_blank(), panel.grid.minor.x = element_blank(),
        axis.title.x = element_text(size = 9.4), axis.title.y = element_text(size = 9.4), 
        legend.position = "bottom", legend.title.position = "top",
        legend.title = element_blank(), legend.text = element_text(size = 8))

p4

ggsave("figure_4_stationary_type2.pdf", p4, height = 5, width = 4.1)


