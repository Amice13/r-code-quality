#############################################################################################################
## Philip Warncke & Ryan E. Carlin                                                                         ##
## Replication script for: "Bad Mood Rising: Bad Mood Rising? Assessing Scalar Invariance Violations       ##
## with Comparative Democratic Support Data (cond. accepted in: Public Opinion Quarterly, Sp. issue 2026:  ##
## Advancing Public Opinion Research Across Countries and Contexts)                                        ##
## Please reach out to Philip Warncke (https://philip-warncke.net/) for queries or bug reports.            ##
#############################################################################################################

####### SCRIPT 2 OUT OF 5: TYPE 2 ERROR SIMULATION FOR FOR DYNAMIC HIERARCHICAL MODELS (FIGURE 2) ########

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
    # design
  G = 5,            # number of covariate groups
  Q = 4,            # items
  T = 5,            # time points
  n_per_cell = 2000,
  # latent dynamics for group means
  sigma_theta_rw = 0.2,   # RW innovation (common across groups)
  sigma_theta_within = 1, # within-group SD (sd_theta)
  # item thresholds (difficulty)
  mu_kappa = 0, sd_kappa = 0.5,
  # scalar invariance spike (time-localized DIF)
  inv_time = 3L,
  target_group = 2L,
  ref_group    = 1L,
  delta        = 0,       # size of DIF spike on probit scale
  dif_items    = 1:2,
  dif_sign     = +1,
  # NEW: true group difference for power sims
  true_diff    = 0,       # θ_target - θ_ref (constant across t)
  # Stan flags
  constant_item  = 1L,    # use constant difficulties in Stan
  separate_years = 0L     # use dynamic smoothing in Stan
) {
  
  stopifnot(
    T >= 1, Q >= 1, G >= 2,
    is.na(inv_time) || (inv_time >= 1 && inv_time <= T),
    target_group >= 1, target_group <= G,
    ref_group    >= 1, ref_group    <= G
  )
  
  ## Simulating latent group means: theta_bar[t, g]  theta_t <- numeric(T)
  theta_t[1] <- rnorm(1, 0, 1)
  for (t in 2:T) {
    theta_t[t] <- theta_t[t - 1] + rnorm(1, 0, sigma_theta_rw)
  }
  
  ## At baseline, all groups share the same random walk
  theta_bar <- matrix(rep(theta_t, each = G),
                      nrow = T, ncol = G, byrow = TRUE)  # T x G
  
  ## Impose constant true difference between target and ref (Type-II design)
  if (!is.na(target_group) && !is.na(ref_group) && true_diff != 0) {
    for (t in 1:T) {
      # ref stays at baseline, target shifted
      theta_bar[t, ref_group]    <- theta_t[t]
      theta_bar[t, target_group] <- theta_t[t] + true_diff
    }
  }
  
  ## Item thresholds (these are kept constant over time)
  kappa_q <- rnorm(Q, mean = mu_kappa, sd = sd_kappa)
  
  ## Variances: within-group and item-level fundamental measurement error
  var_theta <- rep(sigma_theta_within^2, T)   # var_theta[t]
  var_item  <- rep(1, Q)                      # simple choice; Stan fits disc_raw
  
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
  MMM <- array(0L, dim = c(T, Q, G))
  
  ## Flattening the n_vec and s_vec vectors in t–q–g order where MMM == 0 (not used in our simplified simulation set-up)
  N <- sum(MMM == 0L)
  n_vec <- integer(N)
  s_vec <- integer(N)
  pos <- 0L
  for (t in 1:T) {
    for (q in 1:Q) {
      for (g in 1:G) {
        if (MMM[t, q, g] == 0L) {
          pos <- pos + 1L
          n_vec[pos] <- n_arr[t, q, g]
          s_vec[pos] <- s_arr[t, q, g]
        }
      }
    }
  }
  
  ## Group- (i.e., national-) level co-variates; we kept these constant at zero here as these are not relevant to measurement invariance
  Gnat     <- 1L
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
  
  ZZ       <- array(0, dim = c(T, S, H))
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
    dif_sign      = dif_sign,
    true_diff     = true_diff)
  
  list(dat = stan.dat, truth = truth)
}

get_theta_bar_diff_draws <- function(fit, test_time, ref_group, target_group) {
  th <- try(rstan::extract(fit, pars = "theta_bar", permuted = TRUE)$theta_bar,
            silent = TRUE)
  if (inherits(th, "try-error") || is.null(th)) return(NA_real_)
  
  Time <- dim(th)[2]
  G    <- dim(th)[3]
  
  if (test_time < 1 || test_time > Time ||
      ref_group < 1 || ref_group > G ||
      target_group < 1 || target_group > G) {
    return(NA_real_)
  }
  
  ## Calculating empirically observed group-mean differences
  th[, test_time, target_group] - th[, test_time, ref_group]
}

run_cw_reps_for_delta_type2_cl <- function(
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
    true_diff = 0.5,   ## True group mean difference set to noticiable amount to test Type II error rates
    ## Stan sampling inputs
    stan_file,
    pars_keep = c("theta_bar", "sd_theta", "sd_theta_bar",
                  "disc_raw", "diff_raw", "xi", "gamma"),
    iter = 2000, warmup = 1000, thin = 1,
    adapt_delta = 0.98, stepsize = 0.07, max_treedepth = 13,
    test_time = T,
    base_seed = 2025,
    stream = c("quiet", "console"),
    workers = parallel::detectCores() - 1
) {
  stream <- match.arg(stream)
  
  ## Initiating parallel cluster
  cl <- parallel::makeCluster(workers)
  on.exit(parallel::stopCluster(cl), add = TRUE)
  
  ## Export needed objects and functions to parallel workers
  parallel::clusterExport(
    cl,
    varlist = c("sim_cw2015", "fit_cw_once", "get_theta_bar_diff_draws",
                "sigma_theta_rw", "sigma_theta_within",
                "mu_kappa", "sd_kappa",
                "inv_time", "target_group", "ref_group",
                "dif_items", "dif_sign",
                "constant_item", "separate_years",
                "stan_file", "pars_keep",
                "iter", "warmup", "thin",
                "adapt_delta", "stepsize", "max_treedepth",
                "test_time", "base_seed", "stream",
                "G", "Q", "T", "n_per_cell", "delta", "true_diff"),
    envir = environment()
  )
  
  ## Fixing a bug in which workers spawned extra threads in Win 11 for some reason
  parallel::clusterEvalQ(cl, {
    Sys.setenv(OMP_NUM_THREADS = "1")
    library(rstan)
    rstan::rstan_options(auto_write = TRUE)
    NULL
  })
  
  rows <- parallel::parLapply(
    cl,
    X = seq_len(reps),
    fun = function(r) {
      
      ## Simulate DGP with a true group mean difference
      sim <- sim_cw2015(
        G = G, Q = Q, T = T, n_per_cell = n_per_cell,
        sigma_theta_rw     = sigma_theta_rw,
        sigma_theta_within = sigma_theta_within,
        mu_kappa = mu_kappa, sd_kappa = sd_kappa,
        ## Enforcing the  non-invariance spike only when delta != 0
        inv_time = if (delta == 0) NA_integer_ else inv_time,
        target_group = target_group,
        ref_group    = ref_group,
        delta        = delta,
        dif_items    = dif_items,
        dif_sign     = dif_sign,
        constant_item  = constant_item,
        separate_years = separate_years,
        true_diff      = true_diff
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
        return(c(est = NA_real_, lo = NA_real_, hi = NA_real_,
                 reject = NA_real_, bias = NA_real_))
      }
      
      diff_draws <- get_theta_bar_diff_draws(
        fit,
        test_time    = test_time,
        ref_group    = ref_group,
        target_group = target_group
      )
      
      if (all(is.na(diff_draws))) {
        return(c(est = NA_real_, lo = NA_real_, hi = NA_real_,
                 reject = NA_real_, bias = NA_real_))
      }
      
      est <- mean(diff_draws)
      ci  <- as.numeric(quantile(diff_draws, c(0.025, 0.975)))
      
      ## Switching to the correct directionality for one-sided significance tests
          ### If true_diff > 0, we set test CI lower bound > 0
          ### If true_diff < 0, we set test CI upper bound < 0
      if (true_diff > 0) {
        reject <- as.numeric(ci[1] > 0)
      } else if (true_diff < 0) {
        reject <- as.numeric(ci[2] < 0)
      } else {
        ## Two-sided hypothesis test retained as fallback option
        reject <- as.numeric(!(ci[1] <= 0 && 0 <= ci[2]))
      }
      
      bias <- est - true_diff
      
      c(est = est, lo = ci[1], hi = ci[2],
        reject = reject, bias = bias)
    }
  )
  
  ## Aggregate and assess average power
  M <- as.data.frame(do.call(rbind, rows))
  M$reject <- as.numeric(M$reject)
  M <- subset(M, !is.na(reject))
  
  n <- nrow(M); x <- sum(M$reject)
  power <- if (n > 0) x / n else NA_real_
  
  ## Wilson CI estimates around obtained power
  ci <- if (n > 0) {
    z <- qnorm(0.975)
    denom  <- 1 + z^2 / n
    center <- (power + z^2/(2*n)) / denom
    half   <- (z * sqrt((power*(1-power) + z^2/(4*n))/n)) / denom
    c(lo = max(0, center - half), hi = min(1, center + half))
  } else c(lo = NA_real_, hi = NA_real_)
  
  est_med  <- if (n > 0) median(M$est,  na.rm = TRUE) else NA_real_
  est_lo   <- if (n > 0) as.numeric(quantile(M$est, 0.025, na.rm = TRUE)) else NA_real_
  est_hi   <- if (n > 0) as.numeric(quantile(M$est, 0.975, na.rm = TRUE)) else NA_real_
  bias_med <- if (n > 0) median(M$bias, na.rm = TRUE) else NA_real_
  
  data.frame(
    delta           = delta,
    reps            = n,
    power           = power,
    power_wilson_lo = ci["lo"],
    power_wilson_hi = ci["hi"],
    est_median      = est_med,
    est_lo          = est_lo,
    est_hi          = est_hi,
    bias_median     = bias_med
  )
}

run_cw_grid_type2_cl <- function(
    ## Simulation args
    deltas, reps,
    ## DGP
    G, Q, T, n_per_cell,
    sigma_theta_rw,
    sigma_theta_within,
    mu_kappa, sd_kappa,
    inv_time = NA_integer_,
    target_group = NA_integer_,
    ref_group = 1L,
    dif_items = 1:2,
    dif_sign  = +1,
    constant_item  = 1L,
    separate_years = 0L,
    true_diff = 0.5,
    ## Stan instructions
    stan_file,
    pars_keep = c("theta_bar", "sd_theta", "sd_theta_bar",
                  "disc_raw", "diff_raw", "xi", "gamma"),
    iter = 2000, warmup = 1000, thin = 1,
    adapt_delta = 0.98, stepsize = 0.07, max_treedepth = 13,
    test_time = T,
    workers = parallel::detectCores() - 1,
    show_progress = TRUE,
    base_seed = 2025,
    ## Parallel cpu args
    stream = c("quiet", "console")
) {
  stream <- match.arg(stream)
  
  out <- vector("list", length(deltas))
  if (show_progress) {
    pb <- utils::txtProgressBar(min = 0, max = length(deltas), style = 3)
    on.exit(try(close(pb), silent = TRUE), add = TRUE)
  }
  
  for (i in seq_along(deltas)) {
    ## Similation parameters
    d <- deltas[i]
    out[[i]] <- run_cw_reps_for_delta_type2_cl(
      delta = d, reps = reps,
      ## Data generating process arguments
      G = G, Q = Q, T = T, n_per_cell = n_per_cell,
      sigma_theta_rw     = sigma_theta_rw,
      sigma_theta_within = sigma_theta_within,
      mu_kappa = mu_kappa, sd_kappa = sd_kappa,
      inv_time = inv_time,
      target_group = target_group,
      ref_group    = ref_group,
      dif_items    = dif_items,
      dif_sign     = dif_sign,
      constant_item  = constant_item,
      separate_years = separate_years,
      true_diff      = true_diff,
      ## Stan input objects & instructions
      stan_file = stan_file,
      pars_keep = pars_keep,
      iter = iter, warmup = warmup, thin = thin,
      adapt_delta = adapt_delta,
      stepsize = stepsize,
      max_treedepth = max_treedepth,
      test_time = test_time,
      base_seed = base_seed + round(d * 1e6),
      ## Parallel compute options
      stream = stream,
      workers = workers
    )
    if (show_progress) utils::setTxtProgressBar(pb, i)
  }
  
  dplyr::bind_rows(out)
}

######## Model VII: Claassen 2017 model 1 Type II Error Scenario ########

## DGP: Claassen Model 1 (Model VII in the paper)
sim_supdem_mod1 <- function(J = 5, K = 4, Time = 5,
                            n_per_cell = 2000,
                            sigma_theta = 0.5,
                            mu_lambda = 0, sd_lambda = 1,
                            link = c("probit", "logit"),
                            inv_time = NA_integer_,
                            target_group = NA_integer_,
                            ref_group = 1L,
                            delta = 0,
                            dif_items = 1:2,
                            dif_sign = +1,
                            theta_type1 = TRUE,
                            true_diff = 0) {   # NEW: true θ_target - θ_ref
  
  link <- match.arg(link)
  
  ## Item intercepts (per item, fixed over time & country)
  lambda <- rnorm(K, mean = mu_lambda, sd = sd_lambda)
  
  ## Latent means theta[t, j]
  if (theta_type1) {
    ## Imposing a random walk as common baseline across groups
    theta_t <- numeric(Time)
    theta_t[1] <- rnorm(1, 0, 1)
    for (t in 2:Time) theta_t[t] <- theta_t[t - 1] + rnorm(1, 0, sigma_theta)
    
    ## Baseline comparison: all groups share same theta_t
    theta <- matrix(theta_t, nrow = Time, ncol = J, byrow = FALSE)
    
    # Imposing a constant true contrast between target and ref (Type-II error scenario)
    if (!is.na(target_group) && !is.na(ref_group) && true_diff != 0) {
      theta[, ref_group] <- theta_t
      theta[, target_group] <- theta_t + true_diff
      others <- setdiff(seq_len(J), c(ref_group, target_group))
      if (length(others) > 0) theta[, others] <- theta_t
    }
  }
  
  ## Expanding to long grid format to meet the input data requirements
  g <- expand.grid(jj = seq_len(J), kk = seq_len(K), tt = seq_len(Time),
                   KEEP.OUT.ATTRS = FALSE)
  g <- g[order(g$tt, g$jj, g$kk), ]
  
  ## Linear predictor
  lin <- lambda[g$kk] + theta[cbind(g$tt, g$jj)]
  
  ## DIF spike, a.k.a time-localized & group-specific item threshold shift
  if (!is.na(inv_time) && !is.na(target_group) && delta != 0) {
    idx_dif <- which(g$tt == inv_time & g$jj == target_group & g$kk %in% dif_items)
    if (length(idx_dif) > 0) lin[idx_dif] <- lin[idx_dif] + dif_sign * delta
  }
  
  ## Probabilities & counts
  p <- if (link == "probit") pnorm(lin) else plogis(lin)
  samp_vec <- rep(as.integer(n_per_cell), nrow(g))
  x_vec    <- rbinom(n = length(samp_vec), size = samp_vec, prob = p)
  
  ## Stan data object matching the replication script data structure
  dat <- list(
    N   = nrow(g),
    K   = K,
    T   = Time,
    J   = J,
    jj  = as.integer(g$jj),
    tt  = as.integer(g$tt),
    kk  = as.integer(g$kk),
    x   = as.integer(x_vec),
    samp = samp_vec
  )
  
  truth <- list(
    lambda = lambda,
    theta = theta,
    link = link,
    inv_time = inv_time,
    target_group = target_group,
    ref_group = ref_group,
    true_diff = true_diff,
    delta = delta,
    dif_items = dif_items,
    dif_sign = dif_sign
  )
  
  list(dat = dat, truth = truth)
}

get_theta_diff_draws <- function(fit, test_time, ref_group, target_group) {
  th <- try(rstan::extract(fit, pars = "theta", permuted = TRUE)$theta, silent = TRUE)
  if (inherits(th, "try-error") || is.null(th)) return(NA_real_)
  
  Time <- dim(th)[2]; J <- dim(th)[3]
  if (test_time < 1 || test_time > Time ||
      ref_group < 1 || ref_group > J ||
      target_group < 1 || target_group > J) {
    return(NA_real_)
  }
  
  # vector of draws: theta_target - theta_ref at time t
  th[, test_time, target_group] - th[, test_time, ref_group]
}


run_mod1_reps_for_delta_type2 <- function(
    delta, reps,
    # Data generating process
    J, K, Time, n_per_cell,
    sigma_theta,
    mu_lambda, sd_lambda,
    link,
    inv_time,
    target_group, ref_group,
    dif_items, dif_sign,
    theta_type1,
    true_diff = 0.5, ## True mean difference set to 0.5 in Type II error scenario
    ## Stan arguments
    stan_file,
    pars_keep = c("mu_lambda","sigma_lambda","sigma_theta","lambda","theta"),
    iter = 2000, warmup = 1000, thin = 1,
    adapt_delta = 0.99, stepsize = 0.005, max_treedepth = 14,
    test_time = Time,
    base_seed = 2025,
    ## Progress report args
    stream = c("quiet", "console")
) {
  stream <- match.arg(stream)
  
  rows <- future.apply::future_lapply(
    seq_len(reps), future.seed = TRUE,
    FUN = function(r) {
      Sys.setenv(OMP_NUM_THREADS = "1")
      
      sim <- sim_supdem_mod1(
        J = J, K = K, Time = Time, n_per_cell = n_per_cell,
        sigma_theta = sigma_theta,
        mu_lambda = mu_lambda, sd_lambda = sd_lambda,
        link = link,
        inv_time = if (delta == 0) NA_integer_ else inv_time,
        target_group = target_group, ref_group = ref_group,
        delta = delta, dif_items = dif_items, dif_sign = dif_sign,
        theta_type1 = theta_type1,
        true_diff = true_diff
      )
      
      fit <- fit_mod1_once(
        stan_file = stan_file,
        dat = sim$dat, pars_keep = pars_keep,
        iter = iter, warmup = warmup, thin = thin,
        adapt_delta = adapt_delta,
        stepsize = stepsize,
        max_treedepth = max_treedepth,
        seed = base_seed + r, stream = stream
      )
      
      if (inherits(fit, "fit_error")) {
        return(c(est = NA_real_, lo = NA_real_, hi = NA_real_,
                 reject = NA_real_, bias = NA_real_))
      }
      
      diff_draws <- get_theta_diff_draws(
        fit,
        test_time    = test_time,
        ref_group    = ref_group,
        target_group = target_group
      )
      
      if (all(is.na(diff_draws))) {
        return(c(est = NA_real_, lo = NA_real_, hi = NA_real_,
                 reject = NA_real_, bias = NA_real_))
      }
      
      est <- mean(diff_draws)
      ci  <- as.numeric(quantile(diff_draws, c(0.025, 0.975)))
      
      ## Directional one-sided test:
          ### If true_diff > 0 we require a CI lower bound > 0
          ### If true_diff < 0 we require CI upper bound < 0
      if (true_diff > 0) {
        reject <- as.numeric(ci[1] > 0)
      } else if (true_diff < 0) {
        reject <- as.numeric(ci[2] < 0)
      } else {
        ## If true_diff == 0, fall back to two-sided style (for completeness)
        reject <- as.numeric(!(ci[1] <= 0 && 0 <= ci[2]))
      }
      
      bias <- est - true_diff
      
      c(est = est, lo = ci[1], hi = ci[2], reject = reject, bias = bias)
    }
  )
  
  M <- as.data.frame(do.call(rbind, rows))
  M$reject <- as.numeric(M$reject)
  M <- subset(M, !is.na(reject))
  
  n <- nrow(M)
  x <- sum(M$reject)
  
  power <- if (n > 0) x / n else NA_real_
  
  ## Integrated Wilson CI helper
  ci <- if (n > 0) { ## In case model is pathological
    z <- qnorm(0.975)
    denom  <- 1 + z^2 / n
    center <- (power + z^2/(2*n)) / denom
    half   <- (z * sqrt((power*(1-power) + z^2/(4*n))/n)) / denom
    c(lo = max(0, center - half), hi = min(1, center + half))
  } else c(lo = NA_real_, hi = NA_real_)
  
  est_med  <- if (n > 0) median(M$est,  na.rm = TRUE) else NA_real_
  est_lo   <- if (n > 0) as.numeric(quantile(M$est, 0.025, na.rm = TRUE)) else NA_real_
  est_hi   <- if (n > 0) as.numeric(quantile(M$est, 0.975, na.rm = TRUE)) else NA_real_
  bias_med <- if (n > 0) median(M$bias, na.rm = TRUE) else NA_real_
  
  data.frame(
    delta           = delta,
    reps            = n,
    power           = power,
    power_wilson_lo = ci["lo"],
    power_wilson_hi = ci["hi"],
    est_median      = est_med,
    est_lo          = est_lo,
    est_hi          = est_hi,
    bias_median     = bias_med
  )
}

run_mod1_grid_type2 <- function(
    ## Simulation parameters
    deltas, reps,
    ## DGP parameters
    J, K, Time, n_per_cell,
    sigma_theta,
    mu_lambda, sd_lambda,
    link = "probit",
    inv_time = NA_integer_,
    target_group = NA_integer_,
    ref_group = 1L,
    dif_items = 1:2, ## As before, setting 2/4 as  target items for non-invariance spikes 
    dif_sign = -1, ## Directionality of differential item functioning set to make it harder to detect true latent  
    theta_type1 = TRUE,
    true_diff = 0.5,   ## True difference between latent means set to 0.5 in power/ Type II error scenarios
    ## Stan infrastructure
    stan_file = "supdem.stan.mod1.stan",
    pars_keep = c("mu_lambda","sigma_lambda","sigma_theta",
                  "lambda","theta","x_pred","log_lik"),
    iter = 2000, warmup = 1000, thin = 1,
    adapt_delta = 0.99, stepsize = 0.005, max_treedepth = 14,
    test_time = Time,
    workers = max(1, future::availableCores() - 1),
    show_progress = TRUE,
    base_seed = 2025,
    stream = c("quiet","console")
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
    out[[i]] <- run_mod1_reps_for_delta_type2(
      ## Simulation parameters
      delta = d, reps = reps,
      ## DGP parameters
      J = J, K = K, Time = Time, n_per_cell = n_per_cell,
      sigma_theta = sigma_theta,
      mu_lambda = mu_lambda, sd_lambda = sd_lambda,
      link = link,
      inv_time = inv_time,
      target_group = target_group,
      ref_group = ref_group,
      dif_items = dif_items,
      dif_sign = dif_sign,
      theta_type1 = theta_type1,
      true_diff = true_diff,
      ## Stan input
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

### PRODUCTION RUN FOR CLAASSEN 2017 model 1 (Model VIII in the paper) TYPE II ERROR SIMULATION

# deltas <- seq(0, 1, by = 0.05)
# 
# mod1_power_res <- run_mod1_grid_type2(
#   deltas = deltas,
#   reps   = 120,
#   J = 5, K = 4, Time = 5, n_per_cell = 2000,
#   sigma_theta = 0.5, mu_lambda = 0, sd_lambda = 1,
#   link = "probit",
#   inv_time = 3,
#   target_group = 2,
#   ref_group = 1,
#   dif_items = 1:2,
#   dif_sign = -1,
#   theta_type1 = TRUE,
#   true_diff = 0.25,
#   stan_file = "supdem.stan.mod1.stan",
#   pars_keep = c("mu_lambda","sigma_lambda","sigma_theta","lambda","theta"),
#   iter = 3250, warmup = 1750,
#   adapt_delta = 0.98, stepsize = 0.006, max_treedepth = 14,
#   test_time = 3,
#   workers = max(1, future::availableCores() - 1),
#   show_progress = TRUE,
#   stream = "quiet"
# )
# 
# saveRDS(mod1_power_res, "mod1_power_res.rds")
mod1_power_res <- readRDS("mod1_power_res.rds")

######### Claassen model 3 TYPE II Error Scenario (Model VIII in Figure 4) ########

sim_supdem_mod3 <- function(
    ## DGP parameters
    J = 5, K = 4, Time = 5,
    n_per_cell = 2000,
    ## Latent dynamics
    sigma_theta = 0.5,
    ## Item intercepts
    mu_lambda = 0, sd_lambda = 1,
    ## Item slopes
    mu_gamm = 1, sd_gamm = 0.2,
    ## Item-country heterogeneity (not imposed here)
    sigma_delta = 0.0,
    strict_delta = TRUE,
    # link (Stan uses logit)
    link = c("logit", "probit"),
    ## Scalar invariance spike (time-localized)
    inv_time = NA_integer_,
    target_group = NA_integer_,
    ref_group = 1L,
    delta = 0,
    dif_items = 1:2,
    dif_sign = +1,
    ## Latent mean structure
    theta_type1 = TRUE,
    true_diff = 0.5) { 
  
  link <- match.arg(link)
  
  ## Setting item intercepts and slopes for IRT simulation
  lambda <- rnorm(K, mean = mu_lambda, sd = sd_lambda)
  ## Ensuring consistently positive slopes with a truncated normal
  gamm   <- pmax(rnorm(K, mean = mu_gamm, sd = sd_gamm), 0.01)
  
  ## Item-country intercept shifts
  if (strict_delta || sigma_delta == 0) {
    delta_jk <- matrix(0, nrow = J, ncol = K)
  } else {
    delta_jk <- matrix(rnorm(J * K, 0, sigma_delta), nrow = J, ncol = K)
  }
  
  ## Latent means with random walk
  if (theta_type1) {
    theta_t <- numeric(Time)
    theta_t[1] <- rnorm(1, 0, 1)
    for (t in 2:Time) theta_t[t] <- theta_t[t - 1] + rnorm(1, 0, sigma_theta)
    
    theta <- matrix(theta_t, nrow = Time, ncol = J, byrow = FALSE)
    
    ## Impose a constant true contrast between target and ref (Type-II design)
    if (!is.na(target_group) && !is.na(ref_group) && true_diff != 0) {
      theta[, ref_group]    <- theta_t
      theta[, target_group] <- theta_t + true_diff
      others <- setdiff(seq_len(J), c(ref_group, target_group))
      if (length(others) > 0) theta[, others] <- theta_t
    }
    
  } else {
    theta <- matrix(NA_real_, nrow = Time, ncol = J)
    theta[1, ] <- rnorm(J, 0, 1)
    for (t in 2:Time) theta[t, ] <- theta[t - 1] + rnorm(J, 0, sigma_theta)
    
    if (!is.na(target_group) && !is.na(ref_group) && true_diff != 0) {
      theta[, target_group] <- theta[, target_group] + true_diff
    }
  }
  
  ## Long grid expansion
  g <- expand.grid(jj = seq_len(J), kk = seq_len(K), tt = seq_len(Time),
                   KEEP.OUT.ATTRS = FALSE)
  g <- g[order(g$tt, g$jj, g$kk), ]
  P <- J * K
  pp_lookup <- matrix(seq_len(P), nrow = J, ncol = K)
  g$pp <- pp_lookup[cbind(g$jj, g$kk)]
  
  ## Setting the linear predictor
  lin <- lambda[g$kk] +
    gamm[g$kk] * theta[cbind(g$tt, g$jj)] +
    delta_jk[cbind(g$jj, g$kk)]
  
  ## Localized DIF spike in target group
  if (!is.na(inv_time) && !is.na(target_group) && delta != 0) {
    idx_dif <- which(g$tt == inv_time & g$jj == target_group & g$kk %in% dif_items)
    if (length(idx_dif) > 0) lin[idx_dif] <- lin[idx_dif] + dif_sign * delta
  }
  
  ## Probabilities
  p <- if (link == "probit") pnorm(lin) else plogis(lin)
  
  ## Binomial counts
  samp_vec <- rep(as.integer(n_per_cell), nrow(g))
  x_vec    <- rbinom(n = length(samp_vec), size = samp_vec, prob = p)
  
  ## Assembling data structure for the "supdem.stan.mod3.stan" source file
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
    samp = samp_vec
  )
  
  ## True parameters
  truth <- list(
    lambda   = lambda,
    gamm     = gamm,
    delta_jk = delta_jk,
    theta    = theta,
    link     = link,
    inv_time = inv_time,
    target_group = target_group,
    ref_group    = ref_group,
    true_diff    = true_diff,
    delta        = delta,
    dif_items    = dif_items,
    dif_sign     = dif_sign
  )
  
  list(dat = dat, truth = truth)
}

get_theta_diff_draws <- function(fit, test_time, ref_group, target_group) {
  th <- try(rstan::extract(fit, pars = "theta", permuted = TRUE)$theta, silent = TRUE)
  if (inherits(th, "try-error") || is.null(th)) return(NA_real_)
  
  Time <- dim(th)[2]; J <- dim(th)[3]
  if (test_time < 1 || test_time > Time ||
      ref_group < 1 || ref_group > J ||
      target_group < 1 || target_group > J) {
    return(NA_real_)
  }
  
  th[, test_time, target_group] - th[, test_time, ref_group]
}

fit_mod3_once <- function(stan_file, dat, 
                          pars_keep = c("sigma_theta","sigma_delta","lambda",
                                        "gamm","theta","delta","x_pred","log_lik"),
                          iter = 2000, warmup = 1000, thin = 1,
                          adapt_delta = 0.99, stepsize = 0.005, max_treedepth = 14,
                          seed = 1234, stream = c("quiet","console")) {
  stream  <- match.arg(stream)
  refresh <- if (stream == "console") 100 else 0
  verbose <- stream == "console"
  
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

run_mod3_reps_for_delta_type2 <- function(
    ## Delta grid
    delta, reps,
    # Data-generating process
    J, K, Time, n_per_cell,
    sigma_theta,
    mu_lambda, sd_lambda,
    mu_gamm, sd_gamm,
    sigma_delta, strict_delta,
    link,
    inv_time,
    target_group, ref_group,
    dif_items, dif_sign,
    theta_type1,
    true_diff = 0.5,
    ## Stan args
    stan_file,
    pars_keep = c("sigma_theta","sigma_delta","lambda","gamm",
                  "theta","delta","x_pred","log_lik"),
    iter = 2000, warmup = 1000, thin = 1,
    adapt_delta = 0.99, stepsize = 0.005, max_treedepth = 14,
    test_time = Time,
    base_seed = 2025,
    stream = c("quiet", "console")
) {
  stream <- match.arg(stream)
  
  rows <- future.apply::future_lapply(
    seq_len(reps), future.seed = TRUE,
    FUN = function(r) {
      Sys.setenv(OMP_NUM_THREADS = "1")
      
      sim <- sim_supdem_mod3(
        J = J, K = K, Time = Time, n_per_cell = n_per_cell,
        sigma_theta = sigma_theta,
        mu_lambda = mu_lambda, sd_lambda = sd_lambda,
        mu_gamm = mu_gamm, sd_gamm = sd_gamm,
        sigma_delta = sigma_delta, strict_delta = strict_delta,
        link = link,
        inv_time = if (delta == 0) NA_integer_ else inv_time,
        target_group = target_group, ref_group = ref_group,
        delta = delta, dif_items = dif_items, dif_sign = dif_sign,
        theta_type1 = theta_type1,
        true_diff = true_diff
      )
      
      fit <- fit_mod3_once(
        stan_file = stan_file,
        dat = sim$dat, pars_keep = pars_keep,
        iter = iter, warmup = warmup, thin = thin,
        adapt_delta = adapt_delta,
        stepsize = stepsize,
        max_treedepth = max_treedepth,
        seed = base_seed + r, stream = stream
      )
      
      if (inherits(fit, "fit_error")) {
        return(c(est = NA_real_, lo = NA_real_, hi = NA_real_,
                 reject = NA_real_, bias = NA_real_))
      }
      
      diff_draws <- get_theta_diff_draws(
        fit,
        test_time    = test_time,
        ref_group    = ref_group,
        target_group = target_group
      )
      
      if (all(is.na(diff_draws))) {
        return(c(est = NA_real_, lo = NA_real_, hi = NA_real_,
                 reject = NA_real_, bias = NA_real_))
      }
      
      est <- mean(diff_draws)
      ci  <- as.numeric(quantile(diff_draws, c(0.025, 0.975)))
      
      # Directional switcher for one-sided test:
      if (true_diff > 0) {
        reject <- as.numeric(ci[1] > 0)
      } else if (true_diff < 0) {
        reject <- as.numeric(ci[2] < 0)
      } else {
        reject <- as.numeric(!(ci[1] <= 0 && 0 <= ci[2]))
      }
      
      bias <- est - true_diff
      
      c(est = est, lo = ci[1], hi = ci[2], reject = reject, bias = bias)
    }
  )
  
  M <- as.data.frame(do.call(rbind, rows))
  M$reject <- as.numeric(M$reject)
  M <- subset(M, !is.na(reject))
  
  n <- nrow(M)
  x <- sum(M$reject)
  
  power <- if (n > 0) x / n else NA_real_
  
  ## Wilson CI for power
  ci <- if (n > 0) {
    z <- qnorm(0.975)
    denom  <- 1 + z^2 / n
    center <- (power + z^2/(2*n)) / denom
    half   <- (z * sqrt((power*(1-power) + z^2/(4*n))/n)) / denom
    c(lo = max(0, center - half), hi = min(1, center + half))
  } else c(lo = NA_real_, hi = NA_real_)
  
  est_med  <- if (n > 0) median(M$est,  na.rm = TRUE) else NA_real_
  est_lo   <- if (n > 0) as.numeric(quantile(M$est, 0.025, na.rm = TRUE)) else NA_real_
  est_hi   <- if (n > 0) as.numeric(quantile(M$est, 0.975, na.rm = TRUE)) else NA_real_
  bias_med <- if (n > 0) median(M$bias, na.rm = TRUE) else NA_real_
  
  data.frame(
    delta           = delta,
    reps            = n,
    power           = power,
    power_wilson_lo = ci["lo"],
    power_wilson_hi = ci["hi"],
    est_median      = est_med,
    est_lo          = est_lo,
    est_hi          = est_hi,
    bias_median     = bias_med
  )
}

run_mod3_grid_type2 <- function(
    deltas, reps,
    # DGP
    J, K, Time, n_per_cell,
    sigma_theta,
    mu_lambda, sd_lambda,
    mu_gamm = 1, sd_gamm = 0.2,
    sigma_delta = 0.0, strict_delta = TRUE,
    link = "logit",
    inv_time = NA_integer_,
    target_group = NA_integer_,
    ref_group = 1L,
    dif_items = 1:2, dif_sign = +1,
    theta_type1 = TRUE,
    true_diff = 0.5,
    # Stan
    stan_file = "supdem.stan.mod3.stan",
    pars_keep = c("sigma_theta","sigma_delta","lambda","gamm",
                  "theta","delta","x_pred","log_lik"),
    iter = 2000, warmup = 1000, thin = 1,
    adapt_delta = 0.99, stepsize = 0.005, max_treedepth = 14,
    test_time = Time,
    workers = max(1, future::availableCores() - 1),
    show_progress = TRUE,
    base_seed = 2025,
    stream = c("quiet","console")
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
    out[[i]] <- run_mod3_reps_for_delta_type2(
      delta = d, reps = reps,
      # DGP
      J = J, K = K, Time = Time, n_per_cell = n_per_cell,
      sigma_theta = sigma_theta,
      mu_lambda = mu_lambda, sd_lambda = sd_lambda,
      mu_gamm = mu_gamm, sd_gamm = sd_gamm,
      sigma_delta = sigma_delta, strict_delta = strict_delta,
      link = link,
      inv_time = inv_time,
      target_group = target_group,
      ref_group = ref_group,
      dif_items = dif_items,
      dif_sign = dif_sign,
      theta_type1 = theta_type1,
      true_diff = true_diff,
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



######### Claassen model 5 (with overdispersion priors) #########

sim_supdem_mod5 <- function(J = 5, K = 4, Time = 5,
                            n_per_cell = 2000,
                            ## Latent dynamics
                            sigma_theta = 0.2,
                            ## Item intercepts
                            mu_lambda = 0, sd_lambda = 0.2,
                            ## Item-country heterogeneity (delta_{j,k})
                            sigma_delta = 0,
                            strict_delta = TRUE,
                            ## Overdispersion beta-binomial parameter
                            phi = 50,
                            # link
                            link = c("probit", "logit"),
                            ## Scalar invariance spike (time-localized)
                            inv_time = 3,
                            target_group = 2,
                            ref_group = 1L,
                            delta = 1,
                            dif_items = 1:2,
                            dif_sign = +1,
                            ## Latent mean structure
                            theta_type1 = TRUE,
                            true_diff = 0.5) { ## Imposing discernable group mean difference to assess power under obscuring DIF scenario
  
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
    # one RW baseline across groups
    theta_t <- numeric(Time)
    theta_t[1] <- rnorm(1, 0, 1)
    for (t in 2:Time) theta_t[t] <- theta_t[t - 1] + rnorm(1, 0, sigma_theta)
    
    theta <- matrix(theta_t, nrow = Time, ncol = J, byrow = FALSE)
    
    ## Imposing a constant true contrast between target and ref (Type-II design)
    if (!is.na(target_group) && !is.na(ref_group) && true_diff != 0) {
      ## keeping ref_group at baseline theta_t
      theta[, ref_group] <- theta_t
      theta[, target_group] <- theta_t + true_diff
      ## all other groups (if any) remain at baseline
      others <- setdiff(seq_len(J), c(ref_group, target_group))
      if (length(others) > 0) theta[, others] <- theta_t
    }
    
  } 
  
  ## Long grid data expansion
  g <- expand.grid(jj = seq_len(J), kk = seq_len(K), tt = seq_len(Time),
                   KEEP.OUT.ATTRS = FALSE)
  g <- g[order(g$tt, g$jj, g$kk), ]
  P <- J * K
  pp_lookup <- matrix(seq_len(P), nrow = J, ncol = K)
  g$pp <- pp_lookup[cbind(g$jj, g$kk)]
  
  ## Setting linear predictor
  lin <- lambda[g$kk] + delta_jk[cbind(g$jj, g$kk)] + theta[cbind(g$tt, g$jj)]
  
  ## Imposing DIF spike as time-localized threshold shift in the target group
  if (!is.na(inv_time) && !is.na(target_group) && delta != 0) {
    idx_dif <- which(g$tt == inv_time & g$jj == target_group & g$kk %in% dif_items)
    if (length(idx_dif) > 0) lin[idx_dif] <- lin[idx_dif] + dif_sign * delta
  }
  
  ## Setting response probabilities
  p <- if (link == "probit") pnorm(lin) else plogis(lin)
  
  ## Drawing beta-binomial counts
  n_vec <- rep(as.integer(n_per_cell), nrow(g))
  alpha <- phi * p
  beta  <- phi * (1 - p)
  rbbinom <- function(n, size, alpha, beta) {
    draw_p <- rbeta(n, alpha, beta)
    rbinom(n, size, draw_p)
  }
  x_vec <- rbbinom(length(n_vec), n_vec, alpha, beta)
  
  ## Stan input to meet requirements of 'supdem.stan.mod5.stan' replicaiton file
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
  
  ## True parameter report
  truth <- list(
    lambda = lambda,
    delta_jk = delta_jk,
    theta  = theta,
    phi    = phi,
    link   = link,
    inv_time = inv_time,
    target_group = target_group,
    ref_group = ref_group,
    true_diff = true_diff,
    delta  = delta,
    dif_items = dif_items,
    dif_sign  = dif_sign
  )
  
  list(dat = dat, truth = truth)
}


run_mod5_reps_for_delta_type2 <- function(
    delta, reps,
    ## Data-generating process
    J, K, Time, n_per_cell,
    sigma_theta,
    mu_lambda, sd_lambda,
    sigma_delta, strict_delta,
    phi,
    link,
    inv_time,
    target_group, ref_group,
    dif_items, dif_sign,
    theta_type1,
    true_diff = 0.5, ## True group difference set large enough to get differences in assessed power
    ## Stan sampling
    stan_file,
    pars_keep = c("mu_lambda", "sigma_lambda", "sigma_delta",
                  "sigma_theta", "phi", "lambda", "delta", "theta"),
    iter = 2000, warmup = 1000, thin = 1,
    adapt_delta = 0.98, stepsize = 0.07, max_treedepth = 13,
    test_time = Time,
    base_seed = 2025,
    stream = c("quiet", "console")
) {
  stream <- match.arg(stream)
  
  rows <- future.apply::future_lapply(
    seq_len(reps), future.seed = TRUE,
    FUN = function(r) {
      Sys.setenv(OMP_NUM_THREADS = "1")
      
      # Simulate response data with a true group mean difference
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
        theta_type1 = theta_type1,
        true_diff = true_diff
      )
      
      fit <- fit_mod5_once(
        stan_file = stan_file,
        dat = sim$dat, pars_keep = pars_keep,
        iter = iter, warmup = warmup, thin = thin,
        adapt_delta = adapt_delta,
        stepsize = stepsize,
        max_treedepth = max_treedepth,
        seed = base_seed + r,
        stream = stream
      )
      
      if (inherits(fit, "fit_error")) {
        return(c(est = NA_real_, lo = NA_real_, hi = NA_real_,
                 reject = NA_real_, bias = NA_real_))
      }
      
      alt <- if (true_diff <= 0) "greater" else "less"
      
      stats <- safe_theta_contrast(
        fit,
        test_time    = test_time,
        ref_group    = ref_group,
        target_group = target_group,
        alternative  = alt
      )
      
      est <- stats[["est"]]
      bias <- est - true_diff
      
      c(stats, bias = bias)
    }
  )
  
  M <- as.data.frame(do.call(rbind, rows))
  M$reject <- as.numeric(M$reject)
  M <- subset(M, !is.na(reject))
  
  n <- nrow(M)
  x <- sum(M$reject)
  
  power <- if (n > 0) x / n else NA_real_
  
  ## Wilson CI for mean power estimates
  ci <- if (n > 0) {
    z <- qnorm(0.975)
    denom  <- 1 + z^2 / n
    center <- (power + z^2/(2*n)) / denom
    half   <- (z * sqrt((power*(1-power) + z^2/(4*n))/n)) / denom
    c(lo = max(0, center - half), hi = min(1, center + half))
  } else c(lo = NA_real_, hi = NA_real_)
  
  est_med  <- if (n > 0) median(M$est, na.rm = TRUE)  else NA_real_
  est_lo   <- if (n > 0) as.numeric(quantile(M$est, 0.025, na.rm = TRUE)) else NA_real_
  est_hi   <- if (n > 0) as.numeric(quantile(M$est, 0.975, na.rm = TRUE)) else NA_real_
  bias_med <- if (n > 0) median(M$bias, na.rm = TRUE) else NA_real_
  
  ## Reporting power stats
  data.frame(
    delta           = delta,
    reps            = n,
    power           = power,
    power_wilson_lo = ci["lo"],
    power_wilson_hi = ci["hi"],
    est_median      = est_med,
    est_lo          = est_lo,
    est_hi          = est_hi,
    bias_median     = bias_med
  )
}

run_mod5_grid_type2 <- function(
    ## Simulation pars
    deltas, reps,
    ## DGP parameters 
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
    true_diff = 0.5,  ## True mean difference imposed here
    ## Stan input
    stan_file = "supdem.stan.mod5.stan",
    pars_keep = c("mu_lambda", "sigma_lambda", "sigma_delta",
                  "sigma_theta", "phi", "lambda", "delta", "theta"),
    iter = 2000, warmup = 1000, thin = 1,
    adapt_delta = 0.98, stepsize = 0.07, max_treedepth = 13,
    test_time = Time,
    workers = max(1, future::availableCores() - 1),
    show_progress = TRUE,
    base_seed = 2025,
    stream = c("quiet","console")
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
    out[[i]] <- run_mod5_reps_for_delta_type2(
      delta = d, reps = reps,
      ## Data generating process
      J = J, K = K, Time = Time, n_per_cell = n_per_cell,
      sigma_theta = sigma_theta,
      mu_lambda = mu_lambda, sd_lambda = sd_lambda,
      sigma_delta = sigma_delta, strict_delta = strict_delta,
      phi = phi,
      link = link,
      inv_time = inv_time,
      target_group = target_group,
      ref_group = ref_group,
      dif_items = dif_items,
      dif_sign = dif_sign,
      theta_type1 = theta_type1,
      true_diff = true_diff,
      ## Stan args 
      stan_file = stan_file,
      pars_keep = pars_keep,
      iter = iter, warmup = warmup, thin = thin,
      adapt_delta = adapt_delta,
      stepsize = stepsize,
      max_treedepth = max_treedepth,
      test_time = test_time,
      base_seed = base_seed + round(d * 1e6),
      ## Progress bar stream
      stream = stream
    )
    if (show_progress) utils::setTxtProgressBar(pb, i)
  }
  
  dplyr::bind_rows(out)
}

######## PRODUCTION RUN CLAASSEN MODEL 5 (Model IX in Figure 4) FOR TYPE II ERROR SCENARIO ########
deltas <- seq(0, 1, by = 0.05)

# mod5_power_res <- run_mod5_grid_type2(
#   deltas = deltas,
#   reps   = 120,
#   # DGP
#   J = 10, K = 4, Time = 10, n_per_cell = 2000,
#   sigma_theta = 0.2,
#   mu_lambda = 0, sd_lambda = 0.2,
#   sigma_delta = 0.0, strict_delta = TRUE,
#   phi = 50,
#   link = "probit",
#   inv_time = 3,
#   target_group = 2,
#   test_time = 3,
#   ref_group = 1,
#   dif_items = 1:2,
#   dif_sign = -1,
#   theta_type1 = TRUE,
#   true_diff = 0.5,
#   # Stan
#   stan_file = "supdem.stan.mod5.stan",
#   iter = 3000, warmup = 1500,
#   adapt_delta = 0.99, stepsize = 0.06, max_treedepth = 14,
#   workers = max(1, future::availableCores() - 1),
#   stream = "quiet"
# )
# 
# saveRDS(mod5_power_res, "mod5_power_res.rds")
mod5_power_res <- readRDS("mod5_power_res.rds")

######## FIGURE 4: PLOTTING RESULTS OF TYPE 1 ERROR SIMULATION FOR MODELS VI THROUGH IX ########

### Helper function
import_cleaner <- function(dat) {
  dat <- dat %>% dplyr::select(any_of(c("delta", "method", "power", "reps", "power_wilson_lo", "power_wilson_hi", "bias_median")))
  dat
}

cw_power_res <- readRDS("res_cw_power.rds") %>% import_cleaner()
cw_power_res$method <- "CW"

mod1_power_res <- readRDS("mod1_power_res.rds") %>% import_cleaner()
mod1_power_res$method <- "Model 1"

mod3_power_res <- readRDS("mod3_power_res.rds") %>% import_cleaner()
mod3_power_res$method <- "Model 3"

mod5_power_res <- readRDS("mod5_power_res.rds") %>% import_cleaner()
mod5_power_res$method <- "Model 5"

dynamic_res <- rbind(mod1_power_res, mod3_power_res, mod5_power_res, cw_power_res)

church <- 0.61
sing_part <- 0.40
strong_man <- 0.63
expert_rule <- 1.47

grand_mean <- mean(c(church, sing_part, strong_man, expert_rule))

p2 <- ggplot(dynamic_res, aes(x = delta, y = power, color = method)) +
  geom_hline(yintercept = 0, linewidth = 0.3, alpha = 0.5)+
  geom_vline(xintercept = 0, linewidth = 0.3, alpha = 0.5)+
  geom_line() +
  geom_point() +
  geom_errorbar(aes(ymin = power_wilson_lo, ymax = power_wilson_hi), width = 0.015, alpha = 0.7) +
  # geom_hline(yintercept = 0.05, linetype = 2, alpha = 0.5, linewidth = 0.45) +
  # annotate("text", x = min(dynamic_res$delta) + 0.02, y = 0.05,
  #          label = "p = 0.05 threshold", hjust = 0, vjust = -0.5, size = 2.5)+
  geom_vline(xintercept = grand_mean, linetype = 2, linewidth = 0.9, color = "#222121")+
  annotate("text", x = grand_mean + 0.00025, y = 1, angle = 270,
           label = "Av. threshold diff. Chile vs. Costa Rica", hjust = 0, 
           vjust = -0.5, size = 2.7, color = "#222121")+
  geom_vline(xintercept = sing_part, linetype = 3, linewidth = 0.9, color = "#424040")+
  annotate("text", x = sing_part + 0.00025, y = 1, angle = 270,
           label = "'Single-party' threshold difference", hjust = 0, 
           vjust = -0.5, size = 2.7, color = "#424040")+
  labs(x = "Severity of directional DIF (delta item intercept/threshold shift)",
       y = "Statistical power 1-P(reject Ha: μ1 < μ2 at p < .05)",
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

p2

ggsave("figure_2_dynamic_type2.pdf", p2, height = 5, width = 4.1)




