#############################################################################################################
## Philip Warncke & Ryan E. Carlin                                                                         ##
## Replication script for: "Bad Mood Rising: Bad Mood Rising? Assessing Scalar Invariance Violations       ##
## with Comparative Democratic Support Data (cond. accepted in: Public Opinion Quarterly, Sp. issue 2026:  ##
## Advancing Public Opinion Research Across Countries and Contexts)                                        ##
## Please reach out to Philip Warncke (https://philip-warncke.net/) for queries or bug reports.            ##
#############################################################################################################

######## SCRIPT 1 OUT OF 5: TYPE 1 ERROR SIMULATION FOR DYNAMIC HIERARCHICAL MODELS (FIGURE 1) ###########

## Loading required packages
rm(list = setdiff(ls(), lsf.str()))

if(!require("pacman")) install.packages('pacman')
library(pacman)

p_load(MASS)
p_load(psych)
p_load(arm)
p_load(dplyr)
p_load(tidyr)
p_load(rstan)
p_load(rstanarm)
p_load(loo)
p_load(future)
p_load(future.apply)
p_load(ggplot2)
p_load(RColorBrewer)
p_load(tidyverse)

## Setting global random seed ##
set.seed(42)

############# DEFINING HELPER FUNCTIONS & OBJECTS #############

## Setting global options and helpers
rstan_options(auto_write = TRUE)
options(mc.cores = parallel::detectCores())

pars.1 = c("mu_lambda","sigma_lambda","sigma_theta","lambda","theta","x_pred","log_lik")
pars.3 = c("Sigma","Omega","sigma_delta","sigma_theta","lambda","gamm","delta","theta",
           "x_pred","log_lik")
pars.5 = c("mu_lambda","sigma_lambda","sigma_delta","sigma_theta","phi","lambda","delta","theta", "x_pred","log_lik")

## Wilson confidence interval function for uncertainty from draws
wilson_ci <- function(x, n, conf.level = 0.95) {
  z <- qnorm(1 - (1 - conf.level)/2)
  p <- x / n
  denom  <- 1 + z^2 / n
  center <- (p + z^2/(2*n)) / denom
  half   <- (z * sqrt((p*(1-p) + z^2/(4*n))/n)) / denom
  c(lo = max(0, center - half), hi = min(1, center + half))}

######## Model V: (Caughey and Warsaw 2015) ######## 

sim_cw2015 <- function(
  ## Mimicking Caughey and Warsaw 2015 response-data generating process
  G = 5,                   # Number of data groupings (i.e., countries or states)
  Q = 4,                   # Number of items
  T = 5,                   # Number of time points
  n_per_cell = 2000,
  ## Latent dynamics for group means
  sigma_theta_rw = 0.2,    # Random walk (common across all groups in our simulation)
  sigma_theta_within = 1,  # Within-group SD (fundamental, random measurement error)
  ## Item thresholds (difficulty)
  mu_kappa = 0, sd_kappa = 0.5,
  ## Scalar non-invariance spike (infused at single time point, restricted to specific item)
  inv_time = 3L,
  target_group = 2L,
  ref_group = 1L,
  delta = 0,               # DIF spike parameter
  dif_items = 1:2,
  dif_sign = +1,
  # Stan flags
  constant_item = 1L,      # Constant difficulties (assuming metric invariance)
  separate_years = 0L      # Dynamic smoothing in Stan
) {
  
  stopifnot(
    T >= 1, Q >= 1, G >= 2,
    is.na(inv_time) || (inv_time >= 1 && inv_time <= T),
    target_group >= 1, target_group <= G,
    ref_group >= 1, ref_group <= G
  )
  
  ## Simulating latent group means: theta_bar[t, g]
  theta_t <- numeric(T)
  theta_t[1] <- rnorm(1, 0, 1)
  for (t in 2:T) {
    theta_t[t] <- theta_t[t - 1] + rnorm(1, 0, sigma_theta_rw)
  }
  theta_bar <- matrix(rep(theta_t, each = G),
                      nrow = T, ncol = G, byrow = TRUE)  # T x G (all equal)
  
  ## Item thresholds (these are kept constant over time)
  kappa_q <- rnorm(Q, mean = mu_kappa, sd = sd_kappa)
  
  ## Variances: within-group and item-level fundamental measurement error
  var_theta <- rep(sigma_theta_within^2, T)
  var_item  <- rep(1, Q)
  
  ## Building simulation data-frame z_{t,q,g} and applying time & group-localized DIF spike
  z <- array(NA_real_, dim = c(T, Q, G))
  for (t in 1:T) {
    for (q in 1:Q) {
      sd_tq <- sqrt(var_theta[t] + var_item[q])
      for (g in 1:G) {
        z[t, q, g] <- (theta_bar[t, g] - kappa_q[q]) / sd_tq
      }
    }
  }
  
  ## Scalar invariance violation (DIF) on probit scale
  if (!is.na(inv_time) && delta != 0) {
    for (q in dif_items) {
      if (q >= 1 && q <= Q) {
        z[inv_time, q, target_group] <-
          z[inv_time, q, target_group] + dif_sign * delta
      }
    }
  }
  
  ## Probabilities and binomial counts
  p <- pnorm(z)
  n_arr <- array(as.integer(n_per_cell), dim = c(T, Q, G))
  s_arr <- array(0L, dim = c(T, Q, G))
  for (t in 1:T) {
    for (q in 1:Q) {
      for (g in 1:G) {
        s_arr[t, q, g] <- rbinom(1L, size = n_arr[t, q, g], prob = p[t, q, g])
      }
    }
  }
  
  ## We could have induce partial missingness in the response matrix but opted not to as these are not relevant to measurement invariance violations
  MMM <- array(0L, dim = c(T, Q, G))   # 0 = observed, 1 = missing

  ## Flattening the n_vec and s_vec vectors in t–q–g order where MMM == 0 (not used in our simplified simulation set-up)
  N <- sum(MMM == 0L)
  n_vec <- integer(N)
  s_vec <- integer(N)
  pos <- 0L
  for (t in 1:T) {
    for (q in 1:Q) {
      for (g in 1:G) {
          pos <- pos + 1L
          n_vec[pos] <- n_arr[t, q, g]
          s_vec[pos] <- s_arr[t, q, g]
      }
    }
  }
  
  ## Group- (i.e., national-) level co-variates; we kept these constant at zero here as these are not relevant to measurement invariance
  Gnat    <- 1L
  nat_only <- matrix(0L, nrow = T, ncol = Q)
  
  NNnat <- array(0L, dim = c(T, Q, Gnat))
  SSnat <- array(0L, dim = c(T, Q, Gnat))
  
  WT <- array(0, dim = c(T, Gnat, G))
  for (t in 1:T) {
    WT[t, 1, ] <- rep(1 / G, G)
  }
  
  ## Assembling geographic- hierarchical design for DGRIT model (we imposing a completely uninformative DGRIT-design just to satisfy the Stan model and keep estimation time manageable)
  P <- 1L    ## Number of hierarchical parameters
  S <- 1L    ## Number of geographic units
  H <- 1L    ## Predictors per geographic unit
  Hprior <- 1L
  
  XX <- matrix(1, nrow = G, ncol = P)
  ZZ <- array(0, dim = c(T, S, H))
  ZZ_prior <- array(0, dim = c(1, S, Hprior))
  
  ## Wrapping everything in Stan data list to match Caughey & Warsaw's 2015 model requirements from the replication files
  stan.dat <- list(
    n_vec = n_vec,
    s_vec = s_vec,
    XX    = XX,
    ZZ    = ZZ,
    ZZ_prior = ZZ_prior,
    MMM   = MMM,
    G     = G,
    Q     = Q,
    T     = T,
    N     = N,
    P     = P,
    S     = S,
    H     = H,
    Hprior = Hprior,
    separate_years = as.integer(separate_years),
    constant_item  = as.integer(constant_item),
    D     = ifelse(constant_item == 1L, 1L, T),
    NNnat = NNnat,
    SSnat = SSnat,
    WT    = WT,
    nat_only = nat_only,
    Gnat  = Gnat
  )
  
  ## Exporting true DGP parameters for sanity checks and post-hoc parameter comparison
  truth <- list(
    theta_bar = theta_bar,
    kappa_q   = kappa_q,
    var_theta = var_theta,
    var_item  = var_item,
    z         = z,
    p         = p,
    inv_time      = inv_time,
    target_group  = target_group,
    ref_group     = ref_group,
    delta         = delta,
    dif_items     = dif_items,
    dif_sign      = dif_sign
  )
  
  list(dat = stan.dat, truth = truth)
}

## Group-mean estimation contrast
safe_theta_bar_contrast <- function(
    fit, test_time,
    ref_group = 1L, target_group = 2L,
    prob = c(0.025, 0.975)
) {
  smry <- try(summary(fit)$summary, silent = TRUE)
  if (inherits(smry, "try-error") || !is.matrix(smry)) {
    return(c(est = NA_real_, lo = NA_real_, hi = NA_real_, reject = NA_real_))
  }
  
  th <- try(rstan::extract(fit, pars = "theta_bar", permuted = TRUE)$theta_bar,
            silent = TRUE)
  if (inherits(th, "try-error") || is.null(th)) {
    return(c(est = NA_real_, lo = NA_real_, hi = NA_real_, reject = NA_real_))
  }
  
  Time <- dim(th)[2]; G <- dim(th)[3]
  if (test_time < 1 || test_time > Time ||
      ref_group < 1 || ref_group > G ||
      target_group < 1 || target_group > G) {
    return(c(est = NA_real_, lo = NA_real_, hi = NA_real_, reject = NA_real_))
  }
  
  diff <- th[, test_time, target_group] - th[, test_time, ref_group]
  ci   <- as.numeric(quantile(diff, prob))
  est  <- mean(diff)
  rej  <- as.numeric(!(ci[1] <= 0 && 0 <= ci[2]))
  
  c(est = est, lo = ci[1], hi = ci[2], reject = rej)
}

fit_cw_once <- function(
    stan_file, dat,
    pars_keep = c("theta_bar", "sd_theta", "sd_theta_bar",
                  "disc_raw", "diff_raw", "xi", "gamma"),
    iter = 3000, warmup = 1500, thin = 1,
    adapt_delta = 0.98, stepsize = 0.07, max_treedepth = 13,
    seed = 1234,
    stream = c("quiet", "console")
) {
  stream  <- match.arg(stream)
  refresh <- if (stream == "console") 100 else 0
  verbose <- stream == "console"
  
  sm <- rstan::stan_model(stan_file)  # cached if rstan options set
  
  tryCatch(
    rstan::sampling(
      object  = sm,
      data    = dat,
      pars    = pars_keep,
      chains  = 1, cores = 1,
      iter    = iter, warmup = warmup, thin = thin,
      control = list(adapt_delta = adapt_delta,
                     stepsize = stepsize,
                     max_treedepth = max_treedepth),
      seed    = seed,
      init    = "0",
      refresh = refresh,
      verbose = verbose
    ),
    error = function(e) structure(list(error = conditionMessage(e)),
                                  class = "fit_error")
  )
}


run_cw_reps_for_delta <- function(
    ## Simulation parameters
    delta, reps,
    ## DGP parameters
    G, Q, T, n_per_cell,
    sigma_theta_rw,
    sigma_theta_within,
    mu_kappa, sd_kappa,
    inv_time, target_group, ref_group,
    dif_items, dif_sign,
    constant_item = 1L,
    separate_years = 0L,
    ## Stan sampling
    stan_file,
    pars_keep = c("theta_bar", "sd_theta", "sd_theta_bar",
                  "disc_raw", "diff_raw", "xi", "gamma"),
    iter = 2000, warmup = 1000, thin = 1,
    adapt_delta = 0.98, stepsize = 0.07, max_treedepth = 13,
    test_time = T,
    base_seed = 2025,
    stream = c("quiet", "console")) {
  stream <- match.arg(stream)
  
  rows <- future.apply::future_lapply(
    seq_len(reps), future.seed = TRUE,
    FUN = function(r) {
      Sys.setenv(OMP_NUM_THREADS = "1")
      
      sim <- sim_cw2015(
        G = G, Q = Q, T = T, n_per_cell = n_per_cell,
        sigma_theta_rw   = sigma_theta_rw,
        sigma_theta_within = sigma_theta_within,
        mu_kappa = mu_kappa, sd_kappa = sd_kappa,
        inv_time = if (delta == 0) NA_integer_ else inv_time,
        target_group = target_group,
        ref_group    = ref_group,
        delta        = delta,
        dif_items    = dif_items,
        dif_sign     = dif_sign,
        constant_item = constant_item,
        separate_years = separate_years
      )
      
      fit <- fit_cw_once(
        stan_file = stan_file,
        dat = sim$dat,
        pars_keep = pars_keep,
        iter = iter, warmup = warmup, thin = thin,
        adapt_delta = adapt_delta,
        stepsize = stepsize,
        max_treedepth = max_treedepth,
        seed = base_seed + r,
        stream = stream
      )
      
      if (inherits(fit, "fit_error")) {
        return(c(est = NA_real_, lo = NA_real_, hi = NA_real_, reject = NA_real_))
      }
      
      safe_theta_bar_contrast(
        fit,
        test_time    = test_time,
        ref_group    = ref_group,
        target_group = target_group
      )
    }
  )
  
  M <- as.data.frame(do.call(rbind, rows))
  M$reject <- as.numeric(M$reject)
  M <- subset(M, !is.na(reject))
  
  n <- nrow(M); x <- sum(M$reject)
  type1 <- if (n > 0) x / n else NA_real_
  ci    <- if (n > 0) {
    z <- qnorm(0.975)
    denom  <- 1 + z^2 / n
    center <- (type1 + z^2/(2*n)) / denom
    half   <- (z * sqrt((type1*(1-type1) + z^2/(4*n))/n)) / denom
    c(lo = max(0, center - half), hi = min(1, center + half))
  } else c(lo = NA_real_, hi = NA_real_)
  
  data.frame(
    delta           = delta,
    reps            = n,
    type1_rate      = type1,
    type1_wilson_lo = ci["lo"],
    type1_wilson_hi = ci["hi"],
    est_median      = if (n > 0) median(M$est) else NA_real_,
    est_lo          = if (n > 0) as.numeric(quantile(M$est, 0.025)) else NA_real_,
    est_hi          = if (n > 0) as.numeric(quantile(M$est, 0.975)) else NA_real_
  )
}

## Run over delta-grid function
run_cw_grid <- function(
    ## Simulation parameters
    deltas, reps,
    ## Data-generating process parameters
    G, Q, T, n_per_cell,
    sigma_theta_rw,
    sigma_theta_within,
    mu_kappa, sd_kappa,
    inv_time = NA_integer_,
    target_group = NA_integer_,
    ref_group = 1L,
    dif_items = 1:2,
    dif_sign = +1,
    constant_item = 1L,
    separate_years = 0L,
    ## Stan arguments
    stan_file,
    pars_keep = c("theta_bar", "sd_theta", "sd_theta_bar",
                  "disc_raw", "diff_raw", "xi", "gamma"),
    iter = 2000, warmup = 1000, thin = 1,
    adapt_delta = 0.98, stepsize = 0.07, max_treedepth = 13,
    test_time = T,
    workers = max(1, future::availableCores() - 1),
    show_progress = TRUE,
    base_seed = 2025,
    stream = c("quiet", "console")
) {
  stream <- match.arg(stream)
  future::plan(future::multisession, workers = workers)
  
  out <- vector("list", length(deltas))
  if (show_progress) {
    pb <- utils::txtProgressBar(min = 0, max = length(deltas), style = 3)
    on.exit(try(close(pb), silent = TRUE), add = TRUE)
  }
  
  for (i in seq_along(deltas)) {
    d <- deltas[i]
    out[[i]] <- run_cw_reps_for_delta(
      delta = d, reps = reps,
      # DGP
      G = G, Q = Q, T = T, n_per_cell = n_per_cell,
      sigma_theta_rw    = sigma_theta_rw,
      sigma_theta_within = sigma_theta_within,
      mu_kappa = mu_kappa, sd_kappa = sd_kappa,
      inv_time = inv_time,
      target_group = target_group,
      ref_group    = ref_group,
      dif_items    = dif_items,
      dif_sign     = dif_sign,
      constant_item = constant_item,
      separate_years = separate_years,
      # Stan
      stan_file = stan_file,
      pars_keep = pars_keep,
      iter = iter, warmup = warmup, thin = thin,
      adapt_delta = adapt_delta,
      stepsize = stepsize,
      max_treedepth = max_treedepth,
      test_time = test_time,
      base_seed = base_seed + round(d * 1e6),
      stream = stream
    )
    if (show_progress) utils::setTxtProgressBar(pb, i)
  }
  
  dplyr::bind_rows(out)
}

######## Production run for Caughey and Warsaw 2015 DGRIT ######## 

# stan_file_cw <- "caughey_warshaw_2015.stan" ## Original model replication stan file assumed to be in the source directory
# 
# deltas <- seq(0, 1, by = 0.05)
# 
# res_cw_1 <- run_cw_grid(
#   deltas = deltas,
#   reps   = 100,
#   G = 4, Q = 5, T = 5,
#   n_per_cell = 2000,
#   sigma_theta_rw = 0.2,
#   sigma_theta_within = 1.0,
#   mu_kappa = 0, sd_kappa = 0.5,
#   inv_time = 3L,
#   target_group = 2L,
#   ref_group = 1L,
#   dif_items = 1:2,
#   dif_sign = +1,
#   constant_item = 1L,
#   separate_years = 0L,
#   stan_file = stan_file_cw,
#   iter = 2500, warmup = 1500,
#   adapt_delta = 0.99, stepsize = 0.05, max_treedepth = 14,
#   test_time = 3L,
#   workers = max(1, future::availableCores() - 1),
#   show_progress = TRUE,
#   stream = "quiet"
# )
# 
# saveRDS(res_cw_1, "res_cw_1.rds")

res_cw_1 <- readRDS("res_cw_1.rds")

######## Model VII: (Claassen 2017 model 1)

## Mock data generation function for Claassen models:
sim_supdem_mod1 <- function(
    # Data-generating process parameters
  J = 5,             # Number of countries groups
  K = 4,             # Number of items
  Time = 5,          # Number of years
  n_per_cell = 2000, # Number of Survey responses
  
  # Latent opinion structure
  sigma_theta = 0.5, # Random measurement error variance
  mu_lambda = 0,     # Mean of item intercepts
  sd_lambda = 1,     # Sd of item intercepts
  link = c("probit", "logit"),
  
  # Scalar measurement non-invariance spike
  inv_time = NA_integer_,  # Setting the time point for the directional DIF spike
  target_group = NA_integer_, # Determining the non-invariant data grouping
  ref_group = 1L,          # Reference group for mean contrast
  delta = 0,               # Shift added to lambda for DIF cells
  dif_items = 1:2,         # Determines items for DIF spike
  dif_sign = +1,           # DIF directionality: invokes group-level difference where none exists
  theta_type1 = TRUE       # TRUE: all groups share same theta_t
) {
  link <- match.arg(link)
  stopifnot(
    Time >= 1, J >= 1, K >= 2,
    inv_time >= 1 | is.na(inv_time),
    is.na(inv_time) || inv_time <= Time
  )
  
  ## Simulating item intercepts (lambda_k)
  lambda <- rnorm(K, mean = mu_lambda, sd = sd_lambda)
  
  ## Simulating latent opinions theta_{j,t}
  theta <- matrix(NA_real_, nrow = Time, ncol = J)
  
  if (theta_type1) {
    # Specifying scalar random walks over time (these are shared across groups and unaffected by the DIF spike)
    theta_t <- numeric(Time)
    theta_t[1] <- rnorm(1, 0, 1)
    for (t in 2:Time) {
      theta_t[t] <- theta_t[t - 1] + rnorm(1, 0, sigma_theta)
    }
    theta <- matrix(theta_t, nrow = Time, ncol = J)  # Same value across groups at each time
  } else {
    # Setting group-specific random walks
    theta[1, ] <- rnorm(J, 0, 1)
    for (t in 2:Time) {
      theta[t, ] <- theta[t - 1, ] + rnorm(J, 0, sigma_theta)
    }
  }
  
  ## Long grid: one row per (j, k, t) cell
  g <- expand.grid(
    jj = seq_len(J),       # country
    kk = seq_len(K),       # item
    tt = seq_len(Time),    # year
    KEEP.OUT.ATTRS = FALSE
  )
  g <- g[order(g$tt, g$jj, g$kk), ]
  
  ## Setting the linear predictor and infusing it with DIF spike
  lin <- lambda[g$kk] + theta[cbind(g$tt, g$jj)]
  
  if (!is.na(inv_time) && !is.na(target_group) && delta != 0) {
    idx_dif <- which(
      g$tt == inv_time &
        g$jj == target_group &
        g$kk %in% dif_items
    )
    if (length(idx_dif) > 0) {
      lin[idx_dif] <- lin[idx_dif] + dif_sign * delta
    }
  }
  
  ## Drawing probabilities for binomial counts
  p <- if (link == "probit") pnorm(lin) else plogis(lin)
  
  samp_vec <- rep(as.integer(n_per_cell), nrow(g))
  x_vec    <- rbinom(n = length(samp_vec), size = samp_vec, prob = p)
  
  ## --- Stan data list (matches dat.1 structure)
  dat <- list(
    N   = nrow(g),               # Number of observed cells
    K   = K,                     # Items
    T   = Time,                  # Years
    J   = J,                     # Data groupings (e.g., countries)
    jj  = as.integer(g$jj),      # Grouping index per cell
    tt  = as.integer(g$tt),      # Time index per cell
    kk  = as.integer(g$kk),      # Item index per cell
    x   = as.integer(x_vec),     # Successes
    samp = samp_vec              # Binomial trials
  )
  
  ## Exporting ground truth parameters here
  truth <- list(
    lambda       = lambda,
    theta        = theta,
    link         = link,
    inv_time     = inv_time,
    target_group = target_group,
    ref_group    = ref_group,
    delta        = delta,
    dif_items    = dif_items,
    dif_sign     = dif_sign
  )
  
  list(dat = dat, truth = truth)
}

## Simulate data for Model VI
sim_mod1 <- sim_supdem_mod1(
  J = 5, K = 4, Time = 5,
  n_per_cell = 2000,
  sigma_theta = 0.5,
  mu_lambda   = 0,
  sd_lambda   = 1,
  link        = "probit",
  inv_time    = 3,        # Setting the default scalar non-invariance spike at time 3
  target_group = 2,       # Targeted group for the invariance spike
  ref_group    = 1,       # Reference group to compare means against target group
  delta        = 0,       # Size of intercept shift (severity of directional DIF violation)
  dif_items    = 1:2,     # Items 1 & 2 get DIF
  dif_sign     = +1,      # "+1" imposes directionality in DIF
  theta_type1  = TRUE     # All groups have the same latent mean at each time
)

dat_sim  <- sim_mod1$dat
truth    <- sim_mod1$truth

## Safe theta contrast (returns NAs if fit failed or theta missing)
safe_theta_contrast <- function(fit, test_time, ref_group = 1L, target_group = 2L,
                                prob = c(0.025, 0.975)) {
  # If no samples, bail
  smry <- try(summary(fit)$summary, silent = TRUE)
  if (inherits(smry, "try-error") || !is.matrix(smry)) {
    return(c(est = NA_real_, lo = NA_real_, hi = NA_real_, reject = NA_real_))
  }
  
  th <- try(rstan::extract(fit, pars = "theta", permuted = TRUE)$theta, silent = TRUE)
  if (inherits(th, "try-error") || is.null(th)) {
    return(c(est = NA_real_, lo = NA_real_, hi = NA_real_, reject = NA_real_))
  }
  
  Time <- dim(th)[2]; J <- dim(th)[3]
  if (test_time < 1 || test_time > Time ||
      ref_group < 1 || ref_group > J ||
      target_group < 1 || target_group > J) {
    return(c(est = NA_real_, lo = NA_real_, hi = NA_real_, reject = NA_real_))
  }
  
  diff <- th[, test_time, target_group] - th[, test_time, ref_group]
  ci   <- as.numeric(quantile(diff, prob))
  est  <- mean(diff)
  rej  <- as.numeric(!(ci[1] <= 0 && 0 <= ci[2]))
  c(est = est, lo = ci[1], hi = ci[2], reject = rej)
}


## Compile the model once
compile_mod1_once <- function(stan_file = "supdem.stan.mod1.stan") {
  rstan::stan_model(stan_file)
}

## Fit once per on single avaialble worker (parallelism imposed across model reps)
fit_mod1_once <- function(stan_file, dat, pars_keep,
                          iter = 2000, warmup = 1000, thin = 1,
                          adapt_delta = 0.99, stepsize = 0.005, max_treedepth = 14,
                          seed = 1234, stream = c("quiet","console")) {
  stream  <- match.arg(stream)
  refresh <- if (stream == "console") 100 else 0
  verbose <- stream == "console"
  
  ### Compiling inside the worker here:
  sm <- rstan::stan_model(stan_file)
  
  tryCatch(
    rstan::sampling(
      object  = sm,
      data    = dat,
      pars    = pars_keep,
      chains  = 1, cores = 1,
      iter    = iter, warmup = warmup, thin = thin,
      control = list(adapt_delta = adapt_delta,
                     stepsize = stepsize,
                     max_treedepth = max_treedepth),
      seed    = seed,
      init    = "0",
      refresh = refresh,
      verbose = verbose
    ),
    error = function(e) structure(list(error = conditionMessage(e)), class = "fit_error")
  )
}

## Run function parallel over reps within each element of the delta grid
run_mod1_reps_for_delta <- function(delta, reps,
                                    # DGP
                                    J, K, Time, n_per_cell,
                                    sigma_theta, mu_lambda, sd_lambda,
                                    link, inv_time, target_group, ref_group,
                                    dif_items, dif_sign, theta_type1,
                                    ## Stan sampling
                                    stan_file, pars_keep,
                                    iter, warmup, thin,
                                    adapt_delta, stepsize, max_treedepth,
                                    test_time,
                                    base_seed = 2025,
                                    stream = c("quiet","console")) {
  
  stream <- match.arg(stream)
  
  rows <- future.apply::future_lapply(
    seq_len(reps), future.seed = TRUE,
    FUN = function(r) {
      # keep 1 thread per worker
      Sys.setenv(OMP_NUM_THREADS = "1")
      
      sim <- sim_supdem_mod1(
        J = J, K = K, Time = Time, n_per_cell = n_per_cell,
        sigma_theta = sigma_theta, mu_lambda = mu_lambda, sd_lambda = sd_lambda,
        link = link,
        inv_time = if (delta == 0) NA_integer_ else inv_time,
        target_group = target_group, ref_group = ref_group,
        delta = delta, dif_items = dif_items, dif_sign = dif_sign,
        theta_type1 = theta_type1
      )
      
      fit <- fit_mod1_once(
        stan_file = stan_file,
        dat = sim$dat, pars_keep = pars_keep,
        iter = iter, warmup = warmup, thin = thin,
        adapt_delta = adapt_delta, stepsize = stepsize, max_treedepth = max_treedepth,
        seed = base_seed + r, stream = stream
      )
      
      if (inherits(fit, "fit_error"))
        return(c(est = NA_real_, lo = NA_real_, hi = NA_real_, reject = NA_real_))
      
      safe_theta_contrast(
        fit, test_time = test_time, ref_group = ref_group, target_group = target_group
      )
    }
  )
  
  M <- as.data.frame(do.call(rbind, rows))
  M$reject <- as.numeric(M$reject)
  M <- subset(M, !is.na(reject))
  
  n <- nrow(M); x <- sum(M$reject)
  type1 <- if (n > 0) x / n else NA_real_
  ci    <- if (n > 0) wilson_ci(x, n) else c(lo = NA_real_, hi = NA_real_)
  
  data.frame(
    delta           = delta,
    reps            = n,
    type1_rate      = type1,
    type1_wilson_lo = ci["lo"],
    type1_wilson_hi = ci["hi"],
    est_median      = if (n > 0) median(M$est) else NA_real_,
    est_lo          = if (n > 0) as.numeric(quantile(M$est, 0.025)) else NA_real_,
    est_hi          = if (n > 0) as.numeric(quantile(M$est, 0.975)) else NA_real_
  )
}

## Master grid (sequential over MCMC chains to preserve parallelism for the reps)
run_mod1_grid <- function(deltas, reps,
                          # DGP
                          J, K, Time, n_per_cell,
                          sigma_theta, mu_lambda, sd_lambda,
                          link, inv_time, target_group, ref_group,
                          dif_items, dif_sign, theta_type1,
                          # Stan
                          stan_file = "supdem.stan.mod1.stan",
                          pars_keep = c("mu_lambda","sigma_lambda","sigma_theta","lambda","theta","x_pred","log_lik"),
                          iter = 2000, warmup = 1000, thin = 1,
                          adapt_delta = 0.99, stepsize = 0.005, max_treedepth = 14,
                          test_time = Time,
                          workers = future::availableCores() - 1,
                          show_progress = TRUE,
                          base_seed = 2025,
                          stream = c("quiet","console")) {
  
  stream <- match.arg(stream)
  future::plan(future::multisession, workers = workers)
  
  out <- vector("list", length(deltas))
  if (show_progress) {
    pb <- utils::txtProgressBar(min = 0, max = length(deltas), style = 3)
    on.exit(try(close(pb), silent = TRUE), add = TRUE)
  }
  
  for (i in seq_along(deltas)) {
    d <- deltas[i]
    out[[i]] <- run_mod1_reps_for_delta(
      delta = d, reps = reps,
      # DGP
      J = J, K = K, Time = Time, n_per_cell = n_per_cell,
      sigma_theta = sigma_theta, mu_lambda = mu_lambda, sd_lambda = sd_lambda,
      link = link, inv_time = inv_time, target_group = target_group, ref_group = ref_group,
      dif_items = dif_items, dif_sign = dif_sign, theta_type1 = theta_type1,
      # fit
      stan_file = stan_file, pars_keep = pars_keep,
      iter = iter, warmup = warmup, thin = thin,
      adapt_delta = adapt_delta, stepsize = stepsize, max_treedepth = max_treedepth,
      test_time = test_time,
      base_seed = base_seed + round(d * 1e6),
      stream = stream
    )
    if (show_progress) utils::setTxtProgressBar(pb, i)
  }
  
  dplyr::bind_rows(out)
}

### PRODUCTION RUN FOR CLAASSEN 2017 model 1 (Model VII in the paper) TYPE I ERROR SIMULATION
# Claassen_inv_mod1 <- run_mod1_grid(
#   deltas = seq(0, 1, by = 0.05),
#   reps   = 120,
#   J = 5, K = 4, Time = 5, n_per_cell = 2000,
#   sigma_theta = 0.5, mu_lambda = 0, sd_lambda = 1,
#   link = "probit",
#   inv_time = 3, target_group = 2, ref_group = 1,
#   dif_items = 1:2, dif_sign = +1,
#   theta_type1 = TRUE,
#   stan_file = "supdem.stan.mod1.stan",
#   pars_keep = c("mu_lambda", "sigma_lambda", "sigma_theta", "lambda", "theta", "x_pred", "log_lik"),
#   iter = 3000, warmup = 1500, thin = 1,
#   adapt_delta = 0.98, stepsize = 0.007, max_treedepth = 13,
#   test_time = 3,
#   workers = future::availableCores()-1,
#   show_progress = TRUE,
#   stream = "quiet"
# )
# 
# saveRDS(Claassen_inv_mod1, "Claassen_inv_mod1.rds")

Claassen_inv_mod1 <- readRDS("Claassen_inv_mod1.rds")

######## Claassen model 3 (Model VIII in Figure 1) ########

sim_supdem_mod3 <- function(
    ## Data generating process parameters
    J = 5, K = 4, Time = 5,
    n_per_cell = 2000,
    # Latent dynamics
    sigma_theta = 0.5,
    # Item hyperparameters
    mu_lambda = 0, sd_lambda = 1,     # Item intercepts
    mu_gamm   = 1, sd_gamm   = 0.2,   # Item slopes
    # Item-country heterogeneity (delta_{j,k})
    sigma_delta = 0.0,                # set 0 for strict Type-I
    strict_delta = FALSE,             # if TRUE force delta_{j,k} = 0
    # Link function selection
    link = c("probit", "logit"),
    # Scalar non-invariance spike
    inv_time = NA_integer_,
    target_group = NA_integer_,
    ref_group = 1L,
    delta = 0,
    dif_items = 1:2, ## Spike localized to 2/4 items
    dif_sign = +1,
    theta_type1 = FALSE) {
  
  link <- match.arg(link)
  stopifnot(
    Time >= 1, J >= 1, K >= 2,
    is.na(inv_time) || (inv_time >= 1 && inv_time <= Time)
  )
  
  ## Item intercepts (lambda_k) & slopes (gamm_k)
  lambda <- rnorm(K, mean = mu_lambda, sd = sd_lambda)
  gamm   <- pmax(rnorm(K, mean = mu_gamm, sd = sd_gamm), 1e-6)  ## keeping parameter away from 0 here to avoid Stan warning/errors
  
  ## Item-country intercepts delta_{j,k}
  if (strict_delta || sigma_delta == 0) {
    delta_jk <- matrix(0, nrow = J, ncol = K)
  } else {
    delta_jk <- matrix(rnorm(J * K, 0, sigma_delta), nrow = J, ncol = K)
  }
  
  ## Latent means theta[t, j]
  if (theta_type1) {
    # One random walk shared across groups (true group means remain equal at each t in Type I error scenario)
    theta_t <- numeric(Time)
    theta_t[1] <- rnorm(1, 0, 1)
    for (t in 2:Time) theta_t[t] <- theta_t[t - 1] + rnorm(1, 0, sigma_theta)
    theta <- matrix(rep(theta_t, each = J), nrow = Time, ncol = J, byrow = TRUE)
  } else {
    theta <- matrix(NA_real_, nrow = Time, ncol = J)
    theta[1, ] <- rnorm(J, 0, 1)
    for (t in 2:Time) theta[t, ] <- theta[t - 1, ] + rnorm(J, 0, sigma_theta)
  }
  
  ## Long grid & index for (j,k) -> p in 1..J*K
  g <- expand.grid(jj = seq_len(J), kk = seq_len(K), tt = seq_len(Time),
                   KEEP.OUT.ATTRS = FALSE)
  g <- g[order(g$tt, g$jj, g$kk), ]
  P <- J * K
  pp_lookup <- matrix(seq_len(P), nrow = J, ncol = K)  # row=j, col=k
  g$pp <- pp_lookup[cbind(g$jj, g$kk)]
  
  ## Linear predictor
  lin <- lambda[g$kk] + delta_jk[cbind(g$jj, g$kk)] + gamm[g$kk] * theta[cbind(g$tt, g$jj)]
  
  ## DIF spike (time-localized scalar NI)
  if (!is.na(inv_time) && !is.na(target_group) && delta != 0) {
    idx_dif <- which(g$tt == inv_time & g$jj == target_group & g$kk %in% dif_items)
    if (length(idx_dif) > 0) lin[idx_dif] <- lin[idx_dif] + dif_sign * delta
  }
  
  ## Probabilities & binomial counts
  p <- if (link == "probit") pnorm(lin) else plogis(lin)
  samp_vec <- rep(as.integer(n_per_cell), nrow(g))
  x_vec    <- rbinom(n = length(samp_vec), size = samp_vec, prob = p)
  
  ## dat.2 structure (for supdem.stan.mod3.stan)
  dat <- list(
    N   = nrow(g),
    K   = K,
    T   = Time,
    J   = J,
    P   = P,
    jj  = as.integer(g$jj),
    tt  = as.integer(g$tt),
    pp  = as.integer(g$pp),
    kk  = as.integer(g$kk),
    x   = as.integer(x_vec),
    samp = samp_vec)
  
  ## --- truth bundle
  truth <- list(
    lambda = lambda,
    gamm   = gamm,
    delta_jk = delta_jk,
    theta  = theta,
    link   = link,
    inv_time = inv_time,
    target_group = target_group,
    ref_group = ref_group,
    delta  = delta,
    dif_items = dif_items,
    dif_sign  = dif_sign)
  
  list(dat = dat, truth = truth)
}

## Theta contrast fucntion
safe_theta_contrast <- function(fit, test_time, ref_group = 1L, target_group = 2L,
                                prob = c(0.025, 0.975)) {
  # Pre-check: did sampling succeed?
  smry <- try(summary(fit)$summary, silent = TRUE)
  if (inherits(smry, "try-error") || !is.matrix(smry)) {
    return(c(est = NA_real_, lo = NA_real_, hi = NA_real_, reject = NA_real_))
  }
  
  th <- try(rstan::extract(fit, pars = "theta", permuted = TRUE)$theta, silent = TRUE)
  if (inherits(th, "try-error") || is.null(th)) {
    return(c(est = NA_real_, lo = NA_real_, hi = NA_real_, reject = NA_real_))
  }
  
  Time <- dim(th)[2]; J <- dim(th)[3]
  if (test_time < 1 || test_time > Time ||
      ref_group < 1 || ref_group > J ||
      target_group < 1 || target_group > J) {
    return(c(est = NA_real_, lo = NA_real_, hi = NA_real_, reject = NA_real_))
  }
  
  diff <- th[, test_time, target_group] - th[, test_time, ref_group]
  ci   <- as.numeric(quantile(diff, prob))
  est  <- mean(diff)
  rej  <- as.numeric(!(ci[1] <= 0 && 0 <= ci[2]))
  c(est = est, lo = ci[1], hi = ci[2], reject = rej)
}


## Singular execution function
fit_mod3_once <- function(stan_file, dat, pars_keep,
                          iter = 2000, warmup = 1000, thin = 1,
                          adapt_delta = 0.98, stepsize = 0.07, max_treedepth = 13,
                          seed = 1234, stream = c("quiet","console")) {
  
  stream  <- match.arg(stream)
  refresh <- if (stream == "console") 100 else 0
  verbose <- stream == "console"
  
  # compile inside worker; cached via rstan_options(auto_write=TRUE)
  sm <- rstan::stan_model(stan_file)
  
  tryCatch(
    rstan::sampling(
      object  = sm,
      data    = dat,
      pars    = pars_keep,
      chains  = 1, cores = 1,
      iter    = iter, warmup = warmup, thin = thin,
      control = list(adapt_delta = adapt_delta,
                     stepsize = stepsize,
                     max_treedepth = max_treedepth),
      seed    = seed,
      init    = "0",
      refresh = refresh,
      verbose = verbose
    ),
    error = function(e) structure(list(error = conditionMessage(e)), class = "fit_error")
  )
}

## Parallel execution over simulation iterations:
run_mod3_reps_for_delta <- function(delta, reps,
      # DGP
      J, K, Time, n_per_cell,
      sigma_theta,
      mu_lambda, sd_lambda,
      mu_gamm, sd_gamm,
      sigma_delta, strict_delta,
      link, inv_time, target_group, ref_group,
      dif_items, dif_sign, theta_type1,
      # Stan / sampling
      stan_file,
      pars_keep = c("Sigma","Omega","sigma_delta","sigma_theta",
                    "lambda","gamm","delta","theta"),
      iter = 2000, warmup = 1000, thin = 1,
      adapt_delta = 0.98, stepsize = 0.07, max_treedepth = 13,
      test_time = Time,
      base_seed = 2025,
      stream = c("quiet","console")) {
  
  stream <- match.arg(stream)
  
  rows <- future.apply::future_lapply(
    seq_len(reps), future.seed = TRUE,
    FUN = function(r) {
      Sys.setenv(OMP_NUM_THREADS = "1")   # keep 1 thread per worker
      
      sim <- sim_supdem_mod3(
        J = J, K = K, Time = Time, n_per_cell = n_per_cell,
        sigma_theta = sigma_theta,
        mu_lambda = mu_lambda, sd_lambda = sd_lambda,
        mu_gamm = mu_gamm,   sd_gamm = sd_gamm,
        sigma_delta = sigma_delta, strict_delta = strict_delta,
        link = link,
        inv_time = if (delta == 0) NA_integer_ else inv_time,
        target_group = target_group, ref_group = ref_group,
        delta = delta, dif_items = dif_items, dif_sign = dif_sign,
        theta_type1 = theta_type1
      )
      
      fit <- fit_mod3_once(
        stan_file = stan_file,
        dat = sim$dat, pars_keep = pars_keep,
        iter = iter, warmup = warmup, thin = thin,
        adapt_delta = adapt_delta, stepsize = stepsize, max_treedepth = max_treedepth,
        seed = base_seed + r, stream = stream
      )
      
      if (inherits(fit, "fit_error"))
        return(c(est = NA_real_, lo = NA_real_, hi = NA_real_, reject = NA_real_))
      
      safe_theta_contrast(
        fit, test_time = test_time, ref_group = ref_group, target_group = target_group
      )
    }
  )
  
  M <- as.data.frame(do.call(rbind, rows))
  M$reject <- as.numeric(M$reject)
  M <- subset(M, !is.na(reject))
  
  n <- nrow(M); x <- sum(M$reject)
  type1 <- if (n > 0) x / n else NA_real_
  ci    <- if (n > 0) wilson_ci(x, n) else c(lo = NA_real_, hi = NA_real_)
  
  data.frame(
    delta           = delta,
    reps            = n,
    type1_rate      = type1,
    type1_wilson_lo = ci["lo"],
    type1_wilson_hi = ci["hi"],
    est_median      = if (n > 0) median(M$est) else NA_real_,
    est_lo          = if (n > 0) as.numeric(quantile(M$est, 0.025)) else NA_real_,
    est_hi          = if (n > 0) as.numeric(quantile(M$est, 0.975)) else NA_real_
  )
}

run_mod3_grid <- function(deltas, reps,
        # DGP
        J, K, Time, n_per_cell,
        sigma_theta,
        mu_lambda, sd_lambda,
        mu_gamm, sd_gamm,
        sigma_delta = 0.0, strict_delta = FALSE,
        link = "probit",
        inv_time = NA_integer_,
        target_group = NA_integer_,
        ref_group = 1L,
        dif_items = 1:2, dif_sign = +1,
        theta_type1 = TRUE,
        # Stan
        stan_file = "supdem.stan.mod3.stan",
        pars_keep = c("Sigma","Omega","sigma_delta","sigma_theta",
                      "lambda","gamm","delta","theta"),
        iter = 2000, warmup = 1000, thin = 1,
        adapt_delta = 0.98, stepsize = 0.07, max_treedepth = 13,
        test_time = Time,
        workers = future::availableCores() - 1,
        show_progress = TRUE,
        base_seed = 2025,
        stream = c("quiet","console")) {
  
  stream <- match.arg(stream)
  
  # parallel over reps only (inside each delta)
  future::plan(future::multisession, workers = workers)
  
  out <- vector("list", length(deltas))
  if (show_progress) {
    pb <- utils::txtProgressBar(min = 0, max = length(deltas), style = 3)
    on.exit(try(close(pb), silent = TRUE), add = TRUE)
  }
  
  for (i in seq_along(deltas)) {
    d <- deltas[i]
    out[[i]] <- run_mod3_reps_for_delta(
      delta = d, reps = reps,
      # DGP simulation
      J = J, K = K, Time = Time, n_per_cell = n_per_cell,
      sigma_theta = sigma_theta,
      mu_lambda = mu_lambda, sd_lambda = sd_lambda,
      mu_gamm = mu_gamm, sd_gamm = sd_gamm,
      sigma_delta = sigma_delta, strict_delta = strict_delta,
      link = link, inv_time = inv_time, target_group = target_group, ref_group = ref_group,
      dif_items = dif_items, dif_sign = dif_sign, theta_type1 = theta_type1,
      # Mode fitting and statistcal evaluation
      stan_file = stan_file, pars_keep = pars_keep,
      iter = iter, warmup = warmup, thin = thin,
      adapt_delta = adapt_delta, stepsize = stepsize, max_treedepth = max_treedepth,
      test_time = test_time,
      base_seed = base_seed + round(d * 1e6),
      stream = stream
    )
    if (show_progress) utils::setTxtProgressBar(pb, i)
  }
  
  dplyr::bind_rows(out)
}

set.seed(42)
rstan::rstan_options(auto_write = TRUE)

#### PRODUCTION RUN FOR CLAASSEN 2017 model 1 (Model VIII in the paper) TYPE I ERROR SIMULATION
# Claassen_inv_mod3 <- run_mod3_grid(
#   deltas = seq(0, 1, by = 0.05),
#   reps = 120,
#   J = 5, K = 4, Time = 5, n_per_cell = 2000,
#   sigma_theta = 0.5,
#   mu_lambda = 0, sd_lambda = 1,
#   mu_gamm = 1, sd_gamm = 0.2,
#   sigma_delta = 0.0, strict_delta = TRUE,
#   link = "probit",
#   inv_time = 3, target_group = 2, ref_group = 1,
#   dif_items = 1:2, dif_sign = +1,
#   theta_type1 = TRUE,
#   stan_file = "supdem.stan.mod3.stan",
#   pars_keep = c("Sigma","Omega","sigma_delta","sigma_theta","lambda","gamm","delta","theta"),
#   iter = 3000, warmup = 1500, thin = 1,
#   adapt_delta = 0.98, stepsize = 0.07, max_treedepth = 13,
#   test_time = 3,
#   workers = future::availableCores()-1,
#   show_progress = TRUE,
#   stream = "quiet"
# )
# 
# saveRDS(Claassen_inv_mod3, "Claassen_inv_mod3.rds")
Claassen_inv_mod3 <- readRDS("Claassen_inv_mod3.rds")

######### Claassen model 5 (with overdispersion priors) #########

sim_supdem_mod5 <- function(
    # Data-generating process parameters
    J = 5, K = 4, Time = 5,
    n_per_cell = 2000,
    # Latent dynamics
    sigma_theta = 0.2,
    # Item intercepts
    mu_lambda = 0, sd_lambda = 0.2,
    # Item-country heterogeneity (delta_{j,k})
    sigma_delta = 0.0,        # Set 0 for strict Type-I
    strict_delta = TRUE,      # Enforces delta_{j,k} = 0 to eliminate additional (fundamental) uncertainty in mean Type I hypothesis tests
    # Overdispersion parameter (beta-binomial link)
    phi = 50,
    # Link function
    link = c("probit", "logit"),
    # Scalar invariance spike (time-localized)
    inv_time = 3,
    target_group = 2,
    ref_group = 1L,
    delta = 1,
    dif_items = 1:2,
    dif_sign = +1,
    theta_type1 = TRUE) {
  
  link <- match.arg(link)
  stopifnot(
    Time >= 1, J >= 1, K >= 2,
    is.na(inv_time) || (inv_time >= 1 && inv_time <= Time),
    phi > 0
  )
  
  ## Item intercepts (lambda_k)
  lambda <- rnorm(K, mean = mu_lambda, sd = sd_lambda)
  
  ## Item-country intercepts delta_{j,k}
  if (strict_delta || sigma_delta == 0) {
    delta_jk <- matrix(0, nrow = J, ncol = K)
  } else {
    delta_jk <- matrix(rnorm(J * K, 0, sigma_delta), nrow = J, ncol = K)
  }
  
  ## Latent means theta[t, j]
  if (theta_type1) {
    # one RW shared across groups (=> true group means equal at each t)
    theta_t <- numeric(Time)
    theta_t[1] <- rnorm(1, 0, 1)
    for (t in 2:Time) theta_t[t] <- theta_t[t - 1] + rnorm(1, 0, sigma_theta)
    theta <- matrix(rep(theta_t, each = J), nrow = Time, ncol = J, byrow = TRUE)
  } else {
    theta <- matrix(NA_real_, nrow = Time, ncol = J)
    theta[1, ] <- rnorm(J, 0, 1)
    for (t in 2:Time) theta[t, ] <- theta[t - 1, ] + rnorm(J, 0, sigma_theta)
  }
  
  ## Long grid & pp index for (j,k)
  g <- expand.grid(jj = seq_len(J), kk = seq_len(K), tt = seq_len(Time),
                   KEEP.OUT.ATTRS = FALSE)
  g <- g[order(g$tt, g$jj, g$kk), ]
  P <- J * K
  pp_lookup <- matrix(seq_len(P), nrow = J, ncol = K)
  g$pp <- pp_lookup[cbind(g$jj, g$kk)]
  
  ## Linear predictor
  lin <- lambda[g$kk] + delta_jk[cbind(g$jj, g$kk)] + theta[cbind(g$tt, g$jj)]
  
  ## Inducing DIF spike (time-localized threshold shift)
  if (!is.na(inv_time) && !is.na(target_group) && delta != 0) {
    idx_dif <- which(g$tt == inv_time & g$jj == target_group & g$kk %in% dif_items)
    if (length(idx_dif) > 0) lin[idx_dif] <- lin[idx_dif] + dif_sign * delta
  }
  
  ## Response probabilities: P
  p <- if (link == "probit") pnorm(lin) else plogis(lin)
  
  ## Beta-binomial counts: x_vec
  n_vec <- rep(as.integer(n_per_cell), nrow(g))
  alpha <- phi * p
  beta  <- phi * (1 - p)
  rbbinom <- function(n, size, alpha, beta) {
    draw_p <- rbeta(n, alpha, beta)
    rbinom(n, size, draw_p)
  }
  x_vec <- rbbinom(length(n_vec), n_vec, alpha, beta)
  
  ## Dat.2 structure (for supdem.stan.mod5.stan)
  dat <- list(
    N   = nrow(g),
    K   = K,
    T   = Time,
    J   = J,
    P   = P,
    jj  = as.integer(g$jj),
    tt  = as.integer(g$tt),
    pp  = as.integer(g$pp),
    kk  = as.integer(g$kk),
    x   = as.integer(x_vec),
    samp = n_vec
  )
  
  ## True model parameters for sanity checks and post-hoc comparisons
  truth <- list(
    lambda = lambda,
    delta_jk = delta_jk,
    theta  = theta,
    phi    = phi,
    link   = link,
    inv_time = inv_time,
    target_group = target_group,
    ref_group = ref_group,
    delta  = delta,
    dif_items = dif_items,
    dif_sign  = dif_sign
  )
  
  list(dat = dat, truth = truth)
}

safe_theta_contrast <- function(fit, test_time, ref_group = 1L, target_group = 2L,
                                prob = c(0.025, 0.975)) {
  smry <- try(summary(fit)$summary, silent = TRUE)
  if (inherits(smry, "try-error") || !is.matrix(smry)) {
    return(c(est = NA_real_, lo = NA_real_, hi = NA_real_, reject = NA_real_))
  }
  th <- try(rstan::extract(fit, pars = "theta", permuted = TRUE)$theta, silent = TRUE)
  if (inherits(th, "try-error") || is.null(th)) {
    return(c(est = NA_real_, lo = NA_real_, hi = NA_real_, reject = NA_real_))
  }
  Time <- dim(th)[2]; J <- dim(th)[3]
  if (test_time < 1 || test_time > Time ||
      ref_group < 1 || ref_group > J ||
      target_group < 1 || target_group > J) {
    return(c(est = NA_real_, lo = NA_real_, hi = NA_real_, reject = NA_real_))
  }
  diff <- th[, test_time, target_group] - th[, test_time, ref_group]
  ci   <- as.numeric(quantile(diff, prob))
  est  <- mean(diff)
  rej  <- as.numeric(!(ci[1] <= 0 && 0 <= ci[2]))
  c(est = est, lo = ci[1], hi = ci[2], reject = rej)
}

fit_mod5_once <- function(stan_file = "supdem.stan.mod5.stan", dat = dat,
                          pars_keep = c("mu_lambda", "sigma_lambda", "sigma_delta",
                                        "sigma_theta", "phi", "lambda", 
                                        "delta", "theta"),
                          iter = 3000, warmup = 1500, thin = 1,
                          adapt_delta = 0.98, stepsize = 0.07, max_treedepth = 13,
                          seed = 1234, stream = c("quiet","console")) {
  
  stream  <- match.arg(stream)
  refresh <- if (stream == "console") 100 else 0
  verbose <- stream == "console"
  
  sm <- rstan::stan_model(stan_file)  # compile inside worker; cached if auto_write=TRUE
  
  tryCatch(
    rstan::sampling(
      object  = sm,
      data    = dat,
      pars    = pars_keep,
      chains  = 1, cores = 1,
      iter    = iter, warmup = warmup, thin = thin,
      control = list(adapt_delta = adapt_delta,
                     stepsize = stepsize,
                     max_treedepth = max_treedepth),
      seed    = seed,
      init    = "0",
      refresh = refresh,
      verbose = verbose
    ),
    error = function(e) structure(list(error = conditionMessage(e)), class = "fit_error")
  )
}

run_mod5_reps_for_delta <- function(
    delta, reps,
    # DGP
    J, K, Time, n_per_cell,
    sigma_theta,
    mu_lambda, sd_lambda,
    sigma_delta, strict_delta,
    phi,
    link, inv_time, target_group, ref_group,
    dif_items, dif_sign, theta_type1,
    # Sampling in Stan
    stan_file,
    pars_keep = c("mu_lambda", "sigma_lambda", "sigma_delta",
                  "sigma_theta", "phi", "lambda",
                  "delta", "theta"),
    iter = 2000, warmup = 1000, thin = 1,
    adapt_delta = 0.98, stepsize = 0.07, max_treedepth = 13,
    test_time = Time,
    base_seed = 2025,
    stream = c("quiet","console")) {
  
  stream <- match.arg(stream)
  
  rows <- future.apply::future_lapply(
    seq_len(reps), future.seed = TRUE,
    FUN = function(r) {
      Sys.setenv(OMP_NUM_THREADS = "1")
      
      sim <- sim_supdem_mod5(
        J = J, K = K, Time = Time, n_per_cell = n_per_cell,
        sigma_theta = sigma_theta,
        mu_lambda = mu_lambda, sd_lambda = sd_lambda,
        sigma_delta = sigma_delta, strict_delta = strict_delta,
        phi = phi,
        link = link,
        inv_time = if (delta == 0) NA_integer_ else inv_time,
        target_group = target_group, ref_group = ref_group,
        delta = delta, dif_items = dif_items, dif_sign = dif_sign,
        theta_type1 = theta_type1
      )
      
      fit <- fit_mod5_once(
        stan_file = stan_file,
        dat = sim$dat, pars_keep = pars_keep,
        iter = iter, warmup = warmup, thin = thin,
        adapt_delta = adapt_delta, stepsize = stepsize, max_treedepth = max_treedepth,
        seed = base_seed + r, stream = stream
      )
      
      if (inherits(fit, "fit_error"))
        return(c(est = NA_real_, lo = NA_real_, hi = NA_real_, reject = NA_real_))
      
      safe_theta_contrast(
        fit, test_time = test_time, ref_group = ref_group, target_group = target_group
      )
    }
  )
  
  M <- as.data.frame(do.call(rbind, rows))
  M$reject <- as.numeric(M$reject)
  M <- subset(M, !is.na(reject))
  
  n <- nrow(M); x <- sum(M$reject)
  type1 <- if (n > 0) x / n else NA_real_
  ci    <- if (n > 0) {
    z <- qnorm(0.975)
    denom  <- 1 + z^2 / n
    center <- (type1 + z^2/(2*n)) / denom
    half   <- (z * sqrt((type1*(1-type1) + z^2/(4*n))/n)) / denom
    c(lo = max(0, center - half), hi = min(1, center + half))
  } else c(lo = NA_real_, hi = NA_real_)
  
  data.frame(
    delta           = delta,
    reps            = n,
    type1_rate      = type1,
    type1_wilson_lo = ci["lo"],
    type1_wilson_hi = ci["hi"],
    est_median      = if (n > 0) median(M$est) else NA_real_,
    est_lo          = if (n > 0) as.numeric(quantile(M$est, 0.025)) else NA_real_,
    est_hi          = if (n > 0) as.numeric(quantile(M$est, 0.975)) else NA_real_)}

run_mod5_grid <- function(
    deltas, reps,
    # DGP
    J, K, Time, n_per_cell,
    sigma_theta,
    mu_lambda, sd_lambda,
    sigma_delta = 0.0, strict_delta = FALSE,
    phi = 50,
    link = "probit",
    inv_time = NA_integer_,
    target_group = NA_integer_,
    ref_group = 1L,
    dif_items = 1:2, dif_sign = +1,
    theta_type1 = TRUE,
    # Stan based sampling
    stan_file = "supdem.stan.mod5.stan",
    pars_keep = c("mu_lambda", "sigma_lambda", "sigma_delta",
                  "sigma_theta", "phi", "lambda", "delta", "theta"),
    iter = 2000, warmup = 1000, thin = 1,
    adapt_delta = 0.98, stepsize = 0.07, max_treedepth = 13,
    test_time = Time,
    workers = future::availableCores() - 1,
    show_progress = TRUE,
    base_seed = 2025,
    stream = c("quiet","console")) {
  
  stream <- match.arg(stream)
  future::plan(future::multisession, workers = workers)
  
  out <- vector("list", length(deltas))
  if (show_progress) {
    pb <- utils::txtProgressBar(min = 0, max = length(deltas), style = 3)
    on.exit(try(close(pb), silent = TRUE), add = TRUE)
  }
  
  for (i in seq_along(deltas)) {
    d <- deltas[i]
    out[[i]] <- run_mod5_reps_for_delta(
      delta = d, reps = reps,
      # DGP
      J = J, K = K, Time = Time, n_per_cell = n_per_cell,
      sigma_theta = sigma_theta,
      mu_lambda = mu_lambda, sd_lambda = sd_lambda,
      sigma_delta = sigma_delta, strict_delta = strict_delta,
      phi = phi,
      link = link, inv_time = inv_time, target_group = target_group, ref_group = ref_group,
      dif_items = dif_items, dif_sign = dif_sign, theta_type1 = theta_type1,
      # fit
      stan_file = stan_file, pars_keep = pars_keep,
      iter = iter, warmup = warmup, thin = thin,
      adapt_delta = adapt_delta, stepsize = stepsize, max_treedepth = max_treedepth,
      test_time = test_time,
      base_seed = base_seed + round(d * 1e6),
      stream = stream
    )
    if (show_progress) utils::setTxtProgressBar(pb, i)
  }
  
  dplyr::bind_rows(out)
}

######## PRODUCTION RUN CLAASSEN MODEL 5 (Model IX in Figure 3) FOR TYPE I ERROR SCENARIO ########

# res_m5_t1 <- run_mod5_grid(
#   deltas = seq(0, 1, by = 0.05),
#   reps = 100,
#   J = 10, K = 4, Time = 10, n_per_cell = 2000,
#   sigma_theta = 0.2,
#   mu_lambda = 0, sd_lambda = 0.2,
#   sigma_delta = 0, strict_delta = TRUE,
#   phi = 50,
#   link = "probit",
#   inv_time = 3, target_group = 2, ref_group = 1, # ignored when delta==0
#   dif_items = 1:2, dif_sign = +1,
#   theta_type1 = TRUE,
#   stan_file = "supdem.stan.mod5.stan",
#   pars_keep = c("mu_lambda","sigma_lambda","sigma_delta","sigma_theta",
#                 "phi","lambda","delta","theta"),
#   iter = 4000, warmup = 2000, thin = 1,
#   adapt_delta = 0.99, stepsize = 0.06, max_treedepth = 14,
#   test_time = 3,
#   workers = future::availableCores() - 1,
#   show_progress = TRUE,
#   stream = "quiet"
# )
# 
# saveRDS(res_m5_t1, "Claassen_inv_mod5.rds")
Claassen_inv_mod5 <- readRDS("Claassen_inv_mod5.rds")

######## FIGURE 3: PLOTTING RESULTS OF TYPE 1 ERROR SIMULATION FOR MODELS VI THROUGH IX ########

### Helper function
import_cleaner <- function(dat) {
  dat <- dat %>% dplyr::select(any_of(c("delta", "method", "type1_rate", "reps", "type1_wilson_lo", "type1_wilson_hi")))
  dat
}

Caughey_Warsaw_1 <- readRDS("res_cw_1.rds") %>% import_cleaner()
Caughey_Warsaw_1$method <- "CW"

Claassen_inv_mod1 <- readRDS("Claassen_inv_mod1.rds") %>% import_cleaner()
Claassen_inv_mod1$method <- "Model 1"

Claassen_inv_mod3 <- readRDS("Claassen_inv_mod3.rds") %>% import_cleaner()
Claassen_inv_mod3$method <- "Model 3"

Claassen_inv_mod5 <- readRDS("Claassen_inv_mod5.rds") %>% import_cleaner()
Claassen_inv_mod5$method <- "Model 5"

dynamic_res <- rbind(Caughey_Warsaw_1, Claassen_inv_mod1, Claassen_inv_mod3, Claassen_inv_mod5)

church <- 0.61
sing_part <- 0.40
strong_man <- 0.63
expert_rule <- 1.47

grand_mean <- mean(c(church, sing_part, strong_man, expert_rule))

p1 <- ggplot(dynamic_res, aes(x = delta, y = type1_rate, color = method)) +
  geom_hline(yintercept = 0, linewidth = 0.3, alpha = 0.5)+
  geom_vline(xintercept = 0, linewidth = 0.3, alpha = 0.5)+
  geom_line() +
  geom_point() +
  geom_errorbar(aes(ymin = type1_wilson_lo, ymax = type1_wilson_hi), width = 0.015, alpha = 0.7) +
  geom_hline(yintercept = 0.05, linetype = 2, alpha = 0.5, linewidth = 0.45) +
  annotate("text", x = max(dynamic_res$delta) + 0.02, y = 0.05,
           label = "p = 0.05 threshold", hjust = 0.8, vjust = -0.5, size = 2.5)+
  geom_vline(xintercept = grand_mean, linetype = 2, linewidth = 0.9, color = "#222121")+
  annotate("text", x = grand_mean + 0.022, y = 0.79, angle = 270,
           label = "Av. threshold diff. Chile vs. Costa Rica", hjust = 0, 
           vjust = -0.5, size = 2.7, color = "#222121")+
  geom_vline(xintercept = sing_part, linetype = 3, linewidth = 0.9, color = "#424040")+
  annotate("text", x = sing_part + 0.005, y = 0.79, angle = 270,
           label = "'Single-party' threshold difference", hjust = 0, 
           vjust = -0.5, size = 2.7, color = "#424040")+
  labs(x = "Severity of directional DIF (delta item intercept/threshold shift)",
       y = "Type I error rate P(reject H0: μ1 = μ2 at p < .05)",
       color = "Empirical method") +
  scale_color_manual(
    values = c(
      "Model 1" = "#1b9e77",
      "Model 3" = "#d95f02",
      "Model 5" = "blue",
      "CW" = "darkred"),
    labels = c(
      "Model 1" = "Claassen (2019) Model 1",
      "Model 3" = "Claassen (2019) Model 3",
      "Model 5" = "Claassen (2019) Model 5",
      "CW" = "Caughey & Warsaw (2015)")) +
  guides(color = guide_legend(nrow = 3, byrow = TRUE))+
  theme_minimal()+
  theme(panel.grid.major.x = element_blank(), panel.grid.minor.x = element_blank(),
        axis.title.x = element_text(size = 9.4), axis.title.y = element_text(size = 9.4), 
        legend.position = "bottom", legend.title.position = "top",
        legend.title = element_blank(), legend.text = element_text(size = 8))

p1

ggsave("figure_1_dynamic_type1.pdf", p1, height = 5, width = 4.1)




