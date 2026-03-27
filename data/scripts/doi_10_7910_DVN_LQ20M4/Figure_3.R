#############################################################################################################
## Philip Warncke & Ryan E. Carlin                                                                         ##
## Replication script for: "Bad Mood Rising: Bad Mood Rising? Assessing Scalar Invariance Violations       ##
## with Comparative Democratic Support Data (cond. accepted in: Public Opinion Quarterly, Sp. issue 2026:  ##
## Advancing Public Opinion Research Across Countries and Contexts)                                        ##
## Please reach out to Philip Warncke (https://philip-warncke.net/) for queries or bug reports.            ##
#############################################################################################################

################## SCRIPT 3 OUT OF 5: TYPE 1 ERROR SIMULATION FOR MODELS I-V (FIGURE 3) ##################

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

### Common Factor model data generation for two groupings (used in stationary measurement models: I-V)
sim_two_group <- function(n_per_group = 500, delta = 0) {
  lambda <- c(0.8, 0.8, 0.8, 0.8) # Constant, relatively high loadings (metric invariance assumed)
  J <- length(lambda)
  nu_g1 <- c(0, 0, 0, 0)          # Factor intercepts for base-line group
  nu_g2 <- c(delta, delta, 0, 0)  # Directional DIF spikes induced in comparison group via 2/4 variable
                                  # (delta) intercept terms
  theta <- 1 - lambda^2           # Setting residual variances
  
  # Nested helper function to simulate group-specific DGP's based on common factor model definition:
  sim_group <- function(n = 1000, mu_eta = 0, nu, lambda, theta) {
    eta <- rnorm(n, mean = mu_eta, sd = 1)            ## Single underlying latent factor η
    E   <- matrix(rnorm(n * J, 0, sqrt(theta)), nrow = n, byrow = TRUE) ## Measurement error
    Y   <- sweep(E, 2, nu, `+`) + eta %*% t(lambda) ## Simulated response data (jointly determined by factor
    colnames(Y) <- paste0("y", 1:J)                 ## loading, latent factor, item intercepts, and error)
    as.data.frame(Y)
  }
  # True latent group means are equal (Type I statistical error simulations (Figure 1))
  g1 <- sim_group(n_per_group, mu_eta = 0, nu = nu_g1, lambda = lambda, theta = theta)
  g2 <- sim_group(n_per_group, mu_eta = 0, nu = nu_g2, lambda = lambda, theta = theta)
  rbind(transform(g1, group = 1L), transform(g2, group = 2L))
}

### Common factor model helper object (Model I)
cfa_model <- 'F =~ y1 + y2 + y3 + y4'

### Fitting function for Model I: Common factor model with strict MI
fit_scalar_MI <- function(dat) {
  fit <- lavaan::cfa(cfa_model, data = dat, group = "group",
             meanstructure = TRUE, estimator = "MLR",
             group.equal = c("loadings","intercepts")) ## Strict MI imposed with group.equal command
  # Test latent mean difference: in scalar MI, lavaan estimates difference in intercepts for comparison group
  pe <-  lavaan::parameterEstimates(fit, standardized = FALSE)
  row <- subset(pe, lhs == "F" & op == "~1" & group == 2)
  # Two-tailed z-test
  pval <- 2 * pnorm(abs(row$est / row$se), lower.tail = FALSE)
  c(p = pval, est = row$est)
}

### Fitting function for Model II: Bayesian CFA with soft identification restrictions (semi-informative priors)
bayes_fit_once <- function(dat, mod, chains = 4, iter_warmup = 1000,
                           iter_sampling = 2000, seed = 2025, tau_nu = 0.10) {
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
        show_exceptions = FALSE,   # optional, avoids verbose error dumps
        show_messages = FALSE      # cmdstanr option: suppress extra messages
      )
    )
  )
  
  d   <- as_draws_df(fit$draws("mu[2]"))
  mu2 <- d[["mu[2]"]]
  est <- mean(mu2)
  lo  <- as.numeric(quantile(mu2, 0.025))
  hi  <- as.numeric(quantile(mu2, 0.975))
  reject <- as.numeric(!(lo <= 0 && 0 <= hi))  # CrI excludes 0 => reject
  
  c(est = est, lo = lo, hi = hi, reject = reject)
}

### Effects-coded common factor model (Model III)
effect_coded_model <-
  'F =~ y1 + y2 + y3 + y4

  # latent means: group 1 fixed to 0 (ID), group 2 estimated
  F ~ c(0, m2)*1

  # item intercepts labeled per group
  y1 ~ c(nu11, nu12)*1
  y2 ~ c(nu21, nu22)*1
  y3 ~ c(nu31, nu32)*1
  y4 ~ c(nu41, nu42)*1

  # sum-to-zero constraints within each group (effect-coding)
  nu11 + nu21 + nu31 + nu41 == 0
  nu12 + nu22 + nu32 + nu42 == 0'

### Fitting function for effects-coded common factor model (Model III)
fit_effect_coded <- function(dat, mod = effect_coded_model) {
  fit <- cfa(mod, data = dat, group = "group", meanstructure = TRUE, estimator = "MLR")
  pe  <- parameterEstimates(fit, standardized = FALSE)
  row <- subset(pe, lhs == "F" & op == "~1" & rhs == "" & group == 2) 
  pval <- 2 * pnorm(abs(row$est / row$se), lower.tail = FALSE)
  c(p = pval, est = row$est)
}

############# ESTIMATION #############
## Note: Depending on available hardware, some of these simulations might require very long run times.
## To avoid system crashes with CPU parallelism, we recommend manually lowering the worker count by at
## least one CPU core.

## Parallel CPU grid functions for Models I & III
run_grid <- function(deltas = seq(0, 0.6, by = 0.05), reps = 100, n_per_group = 500) {
  out <- lapply(deltas, function(delta) {
    res <- replicate(reps, {
      dat <- sim_two_group(n_per_group = n_per_group, delta = delta)
      a <- fit_scalar_MI(dat)
      b <- fit_effect_coded(dat)
      c(delta = delta,
        method = "Scalar MI (hard)",
        reject = as.numeric(a["p"] < 0.05),
        est = a["est"])
    })
    A <- as.data.frame(t(res))
    res2 <- replicate(reps, {
      dat <- sim_two_group(n_per_group = n_per_group, delta = delta)
      b <- fit_effect_coded(dat)
      c(delta = delta,
        method = "Effect-coded (hier. link)",
        reject = as.numeric(b["p"] < 0.05),
        est = b["est"])
    })
    B <- as.data.frame(t(res2))
    rbind(A, B)
  })
  df <- do.call(rbind, out)
  # coerce types
  df$delta  <- as.numeric(df$delta)
  df$reject <- as.numeric(df$reject)
  df$est    <- as.numeric(df$est)
  df$method <- factor(df$method, levels = c("Scalar MI (hard)", "Effect-coded (hier. link)"))
  aggregate(reject ~ delta + method, df, mean)
}

## Parallel Type I error simulation grid: Models I & III
run_grid_parallel <- function(deltas = seq(0, 0.6, by = 0.05),
                              reps = 100,
                              n_per_group = 500,
                              seed = 2025) {
  stopifnot(reps >= 1)
  cores <- max(1, parallel::detectCores(logical = TRUE))
  cl    <- parallel::makeCluster(cores)
  on.exit(parallel::stopCluster(cl), add = TRUE)
  
  parallel::clusterSetRNGStream(cl, iseed = seed)
  parallel::clusterEvalQ(cl, { suppressPackageStartupMessages(library(lavaan)); NULL })
  parallel::clusterExport(
    cl,
    varlist = c("sim_two_group", "fit_scalar_MI", "fit_effect_coded",
                "build_effect_coded_model", "cfa_model", "n_per_group"),
    envir = environment()
  )
  
  all_rows <- lapply(deltas, function(delta) {
    idx <- seq_len(reps)
    
    A <- parallel::parLapplyLB(cl, idx, function(r) {                 ## Model I
      dat <- sim_two_group(n_per_group = n_per_group, delta = delta)
      a   <- fit_scalar_MI(dat)
      c(delta = delta,
        method = "Scalar MI (hard)",
        reject = as.numeric(a["p"] < 0.05),
        est = a["est"])
    })
    
    B <- parallel::parLapplyLB(cl, idx, function(r) {                 ## Model III
      dat <- sim_two_group(n_per_group = n_per_group, delta = delta)
      b   <- fit_effect_coded(dat)
      c(delta = delta,
        method = "Effect-coded (hier. link)",
        reject = as.numeric(b["p"] < 0.05),
        est = b["est"])
    })
    
    rbind(do.call(rbind, A), do.call(rbind, B))
  })
  
  raw <- do.call(rbind, all_rows)
  raw <- as.data.frame(raw, stringsAsFactors = FALSE)
  raw$delta  <- as.numeric(raw$delta)
  raw$reject <- as.numeric(raw$reject)
  raw$est    <- as.numeric(raw$est)
  raw$method <- factor(raw$method,
                       levels = c("Scalar MI (hard)", "Effect-coded (hier. link)"))
  
  # Reporting aggregate results plus 95% Wilson CIs
  agg <- do.call(rbind,
                 lapply(split(raw, list(raw$delta, raw$method), drop = TRUE), function(df) {
                   delta  <- unique(df$delta); method <- unique(df$method)
                   n      <- nrow(df)
                   x      <- sum(df$reject)
                   p_hat  <- x / n
                   ci     <- wilson_ci(x, n, conf.level = 0.95)
                   data.frame(delta = delta, method = method,
                              type1_rate = p_hat, n = n,
                              ci_lo = ci["lo"], ci_hi = ci["hi"])
                 })
  )
  rownames(agg) <- NULL
  list(raw = raw, agg = agg)
}

## Uncomment below to run CPU-parallel grid 
# res <- run_grid_parallel(deltas = seq(0, 1, by = 0.05), 
#                          reps = 300, n_per_group = 500)
# 
# res_type1_I_III <- res$agg
# saveRDS(res_type1_I_III, "res_type1_I_III.rds")

res_type1_I_III <- readRDS("res_type1_I_III.rds") ## Results of Type 1 error simulation for Models I & III

## Parallel type 1 error simulation grid for Model II (Bayesian soft MI) CFA
run_bayes_replicates_for_delta <- function(delta, reps = 100,
                                           n_per_group = 500,
                                           mod, chains = 4,
                                           iter_warmup = 1000,
                                           iter_sampling = 2000,
                                           seed = 2025, tau_nu = 0.10) {
                                           ## tau_nu determines "strength" of Bayesian scalar IV identification
  idx <- seq_len(reps)
  
  rows <- future_lapply(
    idx,
    function(r) {
      dat <- sim_two_group(n_per_group = n_per_group, delta = delta)
      out <- try(
        bayes_fit_once(
          dat = dat, mod = mod,
          chains = chains,
          iter_warmup = iter_warmup,
          iter_sampling = iter_sampling,
          seed = seed + r,
          tau_nu = tau_nu
        ), silent = TRUE)
      if (inherits(out, "try-error")) {
        c(est = NA_real_, lo = NA_real_, hi = NA_real_, reject = NA_real_)
      } else out
    }, future.seed = TRUE)
  
  M <- as.data.frame(do.call(rbind, rows))
  M$reject <- as.numeric(M$reject)
  M <- dplyr::filter(M, !is.na(reject)) # dropping any failed fits just in case (didn't happen in production runs)
  
  n <- nrow(M)
  x <- sum(M$reject)
  type1_rate <- if (n > 0) x / n else NA_real_
  wil        <- if (n > 0) wilson_ci(x, n) else c(lo = NA_real_, hi = NA_real_)
  
  # Deriving posterior-mean summary statistics across runs delta-grid runs
  est_median <- median(M$est, na.rm = TRUE)
  est_lo     <- as.numeric(quantile(M$est, 0.025, na.rm = TRUE))
  est_hi     <- as.numeric(quantile(M$est, 0.975, na.rm = TRUE))
  
  data.frame(
    delta = delta,
    method = sprintf("Bayes soft MI", tau_nu),
    reps = n,
    type1_rate = type1_rate,
    type1_wilson_lo = wil["lo"],
    type1_wilson_hi = wil["hi"],
    est_median = est_median,
    est_lo = est_lo,
    est_hi = est_hi
  )
}

# Master runner for Type 1 Model II: Sequential over delta values; parallel over repetitions (reps)
run_bayes_grid <- function(deltas = seq(0, 1, by = 0.05),
                           reps = 100,
                           n_per_group = 500,
                           chains = 2,
                           iter_warmup = 1000,
                           iter_sampling = 3000,
                           seed = 2025,
                           tau_nu = 0.10,
                           workers = max(1, future::availableCores() - 1),
                           show_progress = TRUE) {
  
  # Compile model once (note that mgcfa_soft_scalar.stan must reside in working directory)
  mod <- cmdstan_model("mgcfa_soft_scalar.stan")
  
  # For safety and efficiency, fitting only one global parallel engine over reps here
  future::plan(future::multisession, workers = workers)
  
  # Inserting progress bar over deltas for estimating time elapse
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
      reps = reps,
      n_per_group = n_per_group,
      mod = mod,
      chains = chains,
      iter_warmup = iter_warmup,
      iter_sampling = iter_sampling,
      seed = seed + round(d * 1e4),
      tau_nu = tau_nu
    )
    if (show_progress) {
      utils::setTxtProgressBar(pb, i)
    }
  }
  t1 <- proc.time()[["elapsed"]]
  if (show_progress) {
    cat(sprintf("\nDone in %.1f min.\n", (t1 - t0) / 60))
  }
  
  dplyr::bind_rows(out)
}

## Uncomment below to execute the parellel run for Model II Type 1 error simulat
# res_type1_II <- run_bayes_grid(
#   deltas = seq(0, 0, by = 0.05),
#   reps = 100,
#   n_per_group = 500,
#   chains = 2,
#   iter_warmup = 750,
#   iter_sampling = 2000,
#   seed = 2025,
#   tau_nu = 0.10,
#   workers = detectCores()-1) ## repetitions will be divided up evenly across workers

# saveRDS(res_type1_II, "res_type1_II.rds")
res_type1_II <- readRDS("res_type1_II.rds")

### Effects-coded IRT MODELS: IV (two groups) & V (multiple groups)

#### Additional helper functions for model IV:
build_stan_data_irt <- function(dat) {
  Y   <- as.matrix(dat[, paste0("y", 1:4)])
  grp <- as.integer(dat$group)
  list(N = nrow(Y), J = ncol(Y), g = grp, Y = Y)
}

### Individual model fitting function for two-group effects-coded IRT (Model IV)
fit_irt_effect_coded_once <- function(dat, mod, chains = 4,
                                      iter_warmup = 1000,
                                      iter_sampling = 1500,
                                      seed = 42) {
  sdev <- build_stan_data_irt(dat)
  fit <- suppressWarnings(suppressMessages(
    mod$sample(
      data = sdev, seed = seed, chains = chains,
      parallel_chains = 1, ## Making sure parallelism only operates across reps (i.e., no parallel chains)
      iter_warmup = iter_warmup,
      iter_sampling = iter_sampling,
      refresh = 0,
      show_messages = FALSE
    )
  ))
  
  d   <- as_draws_df(fit$draws("mu2"))
  mu2 <- d[["mu2"]]
  est <- mean(mu2)
  lo  <- as.numeric(quantile(mu2, 0.025))
  hi  <- as.numeric(quantile(mu2, 0.975))
  reject <- as.numeric(!(lo <= 0 && 0 <= hi))
  c(est = est, lo = lo, hi = hi, reject = reject)
}


## Simulation function creating datasets for each "rep" over the delta invariance grid
run_irt_replicates_for_delta <- function(delta, mod, reps = 100,
                                         n_per_group = 500,
                                         chains = 4,
                                         iter_warmup = 1000,
                                         iter_sampling = 1500,
                                         seed = 2025) {
  idx <- seq_len(reps)
  
  rows <- future_lapply(
    idx, future.seed = TRUE,
    function(r) {
      dat <- sim_two_group_irt(n_per_group = n_per_group, delta = delta)
      out <- try(
        fit_irt_effect_coded_once(
          dat  = dat,
          mod  = mod,
          chains = chains,
          iter_warmup = iter_warmup,
          iter_sampling = iter_sampling,
          seed = seed + r
        ), silent = TRUE)
      if (inherits(out, "try-error")) {
        c(est = NA_real_, lo = NA_real_, hi = NA_real_, reject = NA_real_)
      } else out
    }
  )
  
  M <- as.data.frame(do.call(rbind, rows))
  M$reject <- as.numeric(M$reject)
  M <- dplyr::filter(M, !is.na(reject))  # drop failed fits if any
  
  n <- nrow(M)
  x <- sum(M$reject)
  type1_rate <- if (n > 0) x / n else NA_real_
  wil        <- if (n > 0) wilson_ci(x, n) else c(lo = NA_real_, hi = NA_real_)
  
  # Posterior-mean summaries across runs (not within-run posterior)
  est_median <- median(M$est, na.rm = TRUE)
  est_lo     <- as.numeric(quantile(M$est, 0.025, na.rm = TRUE))
  est_hi     <- as.numeric(quantile(M$est, 0.975, na.rm = TRUE))
  
  data.frame(
    delta = delta,
    method = "IRT effect-coded (metric inv.)",
    reps = n,
    type1_rate = type1_rate,
    type1_wilson_lo = wil["lo"],
    type1_wilson_hi = wil["hi"],
    est_median = est_median,
    est_lo = est_lo,
    est_hi = est_hi
  )
}

## Master runner for model IV type 1 error simulation (sequential over delta values, parallel over reps)
run_bayes_grid_irt <- function(deltas = seq(0, 0.6, by = 0.05),
                               reps = 100,
                               n_per_group = 500,
                               chains = 4,
                               iter_warmup = 1000,
                               iter_sampling = 1500,
                               seed = 2025,
                               workers = max(1, future::availableCores() - 1),
                               show_progress = TRUE) {
  
  # Compile model globally (requires irt_effect_coded.stan to be in working directory!)
  mod <- cmdstan_model("irt_effect_coded.stan")
  
  # One parallel engine running across reps
  future::plan(future::multisession, workers = workers)
  set.seed(seed)
  
  # Adding a progress bar here which tracks the delta grid
  nD <- length(deltas)
  if (show_progress) {
    pb <- txtProgressBar(min = 0, max = nD, style = 3)
    on.exit({ try(close(pb), silent = TRUE) }, add = TRUE)
  }
  
  t0 <- Sys.time()
  out <- vector("list", nD)
  for (i in seq_along(deltas)) {
    d <- deltas[i]
    out[[i]] <- run_irt_replicates_for_delta(
      delta = d,
      reps  = reps,
      n_per_group = n_per_group,
      mod = mod,
      chains = chains,
      iter_warmup = iter_warmup,
      iter_sampling = iter_sampling,
      seed = seed + round(d * 1e4)
    )
    if (show_progress) setTxtProgressBar(pb, i)
  }
  t1 <- Sys.time()
  if (show_progress) {
    message(sprintf("\nCompleted %d deltas in %0.1f min.",
                    nD, as.numeric(difftime(t1, t0, units = "mins"))))
  }
  
  dplyr::bind_rows(out)
}


### Model IV type 1 error simulation run (uncomment to run)
# 
# set.seed(42)
# 
# res_type1_IV <- run_bayes_grid_irt(
#   deltas = seq(0.65, 1, by = 0.05),
#   reps = 100,
#   n_per_group = 500,
#   chains = 2,
#   iter_warmup = 750,
#   iter_sampling = 3000,
#   seed = 2025,
#   workers = max(1, future::availableCores()),
#   show_progress = TRUE
# )
# saveRDS(res_type1_IV, "res_type1_IV.rds")
res_type1_IV <- readRDS("res_type1_IV.rds")

### Model V (effects-coded multiple group Bayesian IRT) type 1 error simulation

#### Helper functions
sim_mg_irt_probit <- function(G = 10, n_per_group = 1000, J = 4, delta = 0) {
  alpha_j <- rep(0.8, J)   # Assuming constant, relatively strong slopes (discriminations), i.e., metric IV
  gamma_base <- c(0.0, 0.0, 0.0, 0.0)  # Baseline thresholds by item (group-invariant baseline)
  gamma_jg <- matrix(rep(gamma_base, G), nrow = J, ncol = G) # Group-specific thresholds (copy baseline)
  ## Injecting scalar non-invariance in the target group (5) on two out of 4 items
  gamma_jg[2, 5] <- gamma_jg[2, 5] + delta
  gamma_jg[4, 5] <- gamma_jg[4, 5] + delta
  
  ## Latent abilities; assumping equal means across groups for Type I error scenario
  mu_g <- rep(0, G)   # true group means all zero // again only looking at Type I
  N <- G * n_per_group
  
  # Simulating individual survey-takers
  theta <- numeric(N)
  grp   <- integer(N)
  idx <- 1
  for (g in 1:G) {
    theta[idx:(idx+n_per_group-1)] <- rnorm(n_per_group, mean = mu_g[g], sd = 1)
    grp[idx:(idx+n_per_group-1)]   <- g
    idx <- idx + n_per_group
  }
  
  ## Simulating item-response matrix below
  Y <- matrix(0L, nrow = N, ncol = J)
  for (n in 1:N) {
    h <- grp[n]
    for (j in 1:J) {
      lp <- alpha_j[j] * theta[n] - gamma_jg[j, h]
      p  <- pnorm(lp)    # probit link
      Y[n, j] <- rbinom(1, 1, p)
    }
  }
  
  list(Y = Y, g = grp, N = N, J = J, G = G,
    true = list(alpha = alpha_j, gamma = gamma_jg, mu = mu_g)
  )
}

## Fit with cmdStanR
mod <- cmdstan_model("irt_mg_effect_coded.stan")

sim_mg_irt_2pl_probit <- function(G = 10, n_per_group = 100, J = 4,
                                  delta = 0, target_group = 5,
                                  alpha = rep(1.0, J),
                                  gamma_base = rep(0, J)) {
  stopifnot(target_group >= 1, target_group <= G)
  N <- G * n_per_group
  
  mu <- rep(0, G)
  
  gamma <- matrix(rep(gamma_base, G), nrow = J, ncol = G)
  gamma[1, target_group] <- gamma[1, target_group] + delta
  gamma[2, target_group] <- gamma[2, target_group] + delta
  
  g_id <- rep(seq_len(G), each = n_per_group)
  theta <- rnorm(N, mean = mu[g_id], sd = 1)
  
  linpred <- function(n, j) alpha[j] * theta[n] - gamma[j, g_id[n]]
  
  Y <- matrix(0L, nrow = N, ncol = J)
  for (n in 1:N) {
    for (j in 1:J) {
      p <- pnorm(linpred(n, j))
      Y[n, j] <- as.integer(runif(1) < p)
    }
  }
  
  list(
    N = N, G = G, J = J,
    g = g_id,
    Y = Y,
    true_mu = mu,
    true_gamma = gamma,
    true_alpha = alpha
  )
}

dat <- sim_mg_irt_2pl_probit(G = 10, n_per_group = 100, J = 4, delta = 0)

# stan_data <- list(N = dat$N, G = 10L, J = 4L, g = as.integer(dat$g),
#   Y = array(dat$Y, dim = c(dat$N, dat$J)))

## Initial helper functions:
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

## Preparing and executing parallel run for Model V Type 1 error scenario
### Set-up here is very similar to Model IV except below generalizes to multiple groups. Hence, annotation was kept lighter here

### Initializing values across all groups 
init_safe <- function(N, G, J) { ## This function just ensures there aren't any log(0) flare ups during estimation
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

inits <- make_inits(chains = 4, N = dat$N, G = 10L, J = 4L)



bayes_fit_once_irt <- function(dat,
                               mod,
                               chains = 4,
                               iter_warmup = 1000,
                               iter_sampling = 1500,
                               seed = 2025,
                               ref_group = 1,
                               target_group = 5) {
  
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
    parallel_chains = 1,
    iter_warmup = iter_warmup,
    iter_sampling = iter_sampling,
    refresh = 0,
    show_exceptions = FALSE
  )
  
  par_name <- sprintf("mu[%d]", target_group)
  mu_t <- as_draws_df(fit$draws(par_name))[[par_name]]
  est <- mean(mu_t)
  lo  <- as.numeric(quantile(mu_t, 0.025))
  hi  <- as.numeric(quantile(mu_t, 0.975))
  reject <- as.numeric(!(lo <= 0 && 0 <= hi))
  
  c(est = est, lo = lo, hi = hi, reject = reject)
}

run_irt_reps_for_delta <- function(delta, mod, reps = 100, G = 10, ## G controls number of simulated groups
                                   n_per_group = 100, J = 4,
                                   chains = 4,
                                   iter_warmup = 1000,
                                   iter_sampling = 1500,
                                   seed = 2025,
                                   ref_group = 1,
                                   target_group = 5) {
  idx <- seq_len(reps)
  p <- progressor(along = idx)
  
  rows <- future_lapply(
    idx,
    FUN = function(r) {
      # First simulating the data here
      dat <- sim_mg_irt_2pl_probit(
        G = G, n_per_group = n_per_group, J = J,
        delta = delta, target_group = target_group
      )
      # Then fitting the model
      out <- try(
        bayes_fit_once_irt(
          dat = dat, mod = mod,
          chains = chains,
          iter_warmup = iter_warmup,
          iter_sampling = iter_sampling,
          seed = seed + r,
          ref_group = ref_group,
          target_group = target_group), silent = TRUE)
      p(sprintf("", delta, r))
      if (inherits(out, "try-error")) {
        c(est = NA_real_, lo = NA_real_, hi = NA_real_, reject = NA_real_)
      } else out
    },
    future.seed = TRUE
  )
  
  M <- as.data.frame(do.call(rbind, rows))
  M$reject <- as.numeric(M$reject)
  M <- dplyr::filter(M, !is.na(reject))
  
  n <- nrow(M)
  x <- sum(M$reject)
  type1 <- if (n > 0) x / n else NA_real_
  ci    <- if (n > 0) wilson_ci(x, n) else c(lo = NA_real_, hi = NA_real_)
  
  # Across-rep posterior-mean summaries (not within-rep posterior)
  est_median <- median(M$est, na.rm = TRUE)
  est_lo     <- as.numeric(quantile(M$est, 0.025, na.rm = TRUE))
  est_hi     <- as.numeric(quantile(M$est, 0.975, na.rm = TRUE))
  
  data.frame(
    delta = delta,
    method = "Bayes hierarchical IRT (effect-coded)",
    reps = n,
    type1_rate = type1,
    type1_wilson_lo = ci["lo"],
    type1_wilson_hi = ci["hi"],
    est_median = est_median,
    est_lo = est_lo,
    est_hi = est_hi
  )
}

# Master runner for Model IV (effects-coded, Bayesian MG IRT) 
run_irt_grid <- function(deltas = seq(0, 0.6, by = 0.05),
                         reps = 100,
                         G = 10, n_per_group = 100, J = 4,
                         chains = 4,
                         iter_warmup = 1000,
                         iter_sampling = 1500,
                         seed = 2025,
                         workers = max(1, future::availableCores() - 1),
                         ref_group = 1,
                         target_group = 5) {
  
  mod <- cmdstan_model("irt_mg_effect_coded.stan")
  future::plan(future::multisession, workers = workers)
  results <- vector("list", length(deltas))
  
  progressr::with_progress({
    p_all <- progressr::progressor(steps = length(deltas))
    for (i in seq_along(deltas)) {
      d <- deltas[i]
      results[[i]] <- run_irt_reps_for_delta(
        delta = d, reps = reps,
        G = G, n_per_group = n_per_group, J = J,
        mod = mod,
        chains = chains,
        iter_warmup = iter_warmup,
        iter_sampling = iter_sampling,
        seed = seed + round(d * 1e6),
        ref_group = ref_group,
        target_group = target_group
      )
      p_all(message = sprintf("Finished", d))
    }
  })
  dplyr::bind_rows(results)
}


## Finally, running over delta grid (Model V type 1 error simulation)
# set.seed(42)
# 
# progressr::handlers(progressr::handler_txtprogressbar)
# 
# res_type1_V <- run_irt_grid(
#   deltas = seq(0, 1, by = 0.05),
#   reps = 100,
#   G = 10, n_per_group = 100, J = 4,
#   chains = 2,
#   iter_warmup = 750,
#   iter_sampling = 2000,
#   seed = 2025,
#   workers = max(1, future::availableCores()),
#   ref_group = 1,
#   target_group = 5
# )
# 
# saveRDS(res_type1_V, "res_type1_V.rds")
res_type1_V <- readRDS("res_type1_V.rds")

######## FIGURE 1: PLOTTING RESULTS OF TYPE 1 ERROR SIMULATION FOR MODELS I THROUGH 5 ########

### Helper functions
import_cleaner <- function(dat) {
  dat <- dat %>% dplyr::select(any_of(c("delta", "method", "type1_rate", "ci_lo", 
                                     "ci_hi", "type1_wilson_lo", "type1_wilson_hi")))
  colnames(dat) <- c("delta", "method", "type1_rate", "ci_lo", "ci_hi")
  dat
}

res_type1_I_III <- readRDS("res_type1_I_III.rds") %>% import_cleaner()
res_type1_II <- readRDS("res_type1_II.rds") %>% import_cleaner()
res_type1_IV <- readRDS("res_type1_IV.rds") %>% import_cleaner()
res_type1_V <- readRDS("res_type1_V.rds") %>% import_cleaner()

grid_res <- rbind(res_type1_I_III, res_type1_II, res_type1_IV, res_type1_V) 

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

p3 <- ggplot(grid_res, aes(x = delta, y = type1_rate, color = method)) +
  geom_hline(yintercept = 0, linewidth = 0.3, alpha = 0.5)+
  geom_vline(xintercept = 0, linewidth = 0.3, alpha = 0.5)+
  geom_line() +
  geom_point() +
  geom_errorbar(aes(ymin = ci_lo, ymax = ci_hi), width = 0.015, alpha = 0.7) +
  geom_hline(yintercept = 0.05, linetype = 2, alpha = 0.5, linewidth = 0.45) +
  annotate("text", x = max(grid_res$delta) + 0.02, y = 0.05,
           label = "p = 0.05 threshold", hjust = 0.8, vjust = -0.5, size = 2.5)+
  geom_vline(xintercept = grand_mean, linetype = 2, linewidth = 0.9, color = "#222121")+
  annotate("text", x = grand_mean + 0.00025, y = 0.69, angle = 270,
           label = "Av. threshold diff. Chile vs. Costa Rica", hjust = 0, 
           vjust = -0.5, size = 2.7, color = "#222121")+
  geom_vline(xintercept = sing_part, linetype = 3, linewidth = 0.9, color = "#424040")+
  annotate("text", x = sing_part + 0.00025, y = 0.62, angle = 270,
           label = "'Single-party' threshold difference", hjust = 0, 
           vjust = -0.5, size = 2.7, color = "#424040")+
  labs(x = "Severity of directional DIF (delta item intercept/threshold shift)",
       y = "Type I error rate P(reject H0: μ1 = μ2 at p < .05)",
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

p3

ggsave("figure_3_stationary_type1.pdf", p3, height = 5, width = 4.1)

