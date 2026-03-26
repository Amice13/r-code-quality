## Set up ----

library(remotes)
# remotes::install_github('mattblackwell/prepost')
library(prepost)
library(gtools)
library(tidyverse)
library(forcats)
library(broom)
library(lmtest)
library(sandwich)
library(readstata13)
library(BayesLogit)

theme_set(theme_bw())

set.seed(13)

# Load cleaned data (hk = full data, hkx = Kalenjin subset)
load(file='output/hk-data-cleaned.Rdata')

## OLS results: pre/post ----

# include control variables used in the paper 
# (see Horowitz & Klaus SI Table A6)
paper_controls <- " + education_num + age + female + close_own_di"



# (_x suffix means that covariates are included)

mod_prepost <-
  hkx %>%
  lm(y ~ t * d,
     data = .) %>%
  coeftest(.,  vcov = vcovHC) %>%
  tidy()


mod_prepost_x <-
  hkx %>%
  lm(as.formula(paste0("y ~ t * d", paper_controls)),
     data = .) %>%
  coeftest(.,  vcov = vcovHC) %>%
  tidy()


mod_pre <-
  hkx %>%
  filter(z == 0) %>%
  lm(y ~ t * d,
     data = .) %>%
  coeftest(.,  vcov = vcovHC) %>%
  tidy()


mod_pre_x <-
  hkx %>%
  filter(z == 0) %>%
  lm(as.formula(paste0("y ~ t * d", paper_controls)),
     data = .) %>%
  coeftest(.,  vcov = vcovHC) %>%
  tidy()


mod_post <-
  hkx %>%
  filter(z == 1) %>%
  lm(y ~ t * d,
     data = .) %>%
  coeftest(.,  vcov = vcovHC) %>%
  tidy()


mod_post_x <-
  hkx %>%
  filter(z == 1) %>%
  lm(as.formula(paste0("y ~ t * d", paper_controls)),
     data = .) %>%
  coeftest(.,  vcov = vcovHC) %>%
  tidy()

ols_models <- list(mod_prepost,
                   mod_prepost_x,
                   mod_pre,
                   mod_pre_x,
                   mod_post,
                   mod_post_x)

est <- rep(NA, length(ols_models))
se <- rep(NA, length(ols_models))
upper <- rep(NA, length(ols_models))
lower <- rep(NA, length(ols_models))

for (m in seq_along(ols_models)) {
  est[m] <-
    ols_models[[m]]$estimate[ols_models[[m]]$term == "t:d"]
  se[m] <-
    ols_models[[m]]$std.error[ols_models[[m]]$term == "t:d"]
  upper[m] <- est[m] + 1.96 * se[m]
  lower[m] <- est[m] - 1.96 * se[m]
}

type <- c("ols_prepost",
          "ols_prepost_x",
          "ols_pre",
          "ols_pre_x",
          "ols_post",
          "ols_post_x")


## Non-parametric bounds ----

bounds_pre_o <-
  pre_bounds(
    formula = y ~ t,
    data = filter(hkx, z == 0),
    moderator = ~ d,
    outcome_mono = 1
  )


bounds_post <-
  post_bounds(
    formula = y ~ t,
    data = filter(hkx, z == 1),
    moderator = ~ d,
    stable_mod = FALSE,
    moderator_mono = NULL
  )

bounds_post_s <-
  post_bounds(
    formula = y ~ t,
    data = filter(hkx, z == 1),
    moderator = ~ d,
    stable_mod = TRUE,
    moderator_mono = NULL
  )


bounds_post_m <-
  post_bounds(
    formula = y ~ t,
    data = filter(hkx, z == 1),
    moderator = ~ d,
    stable_mod = FALSE,
    moderator_mono = 1
  )

bounds_post_ms <-
  post_bounds(
    formula = y ~ t,
    data = filter(hkx, z == 1),
    moderator = ~ d,
    stable_mod = TRUE,
    moderator_mono = 1
  )


bounds_prepost <-
  prepost_bounds(
    formula = y ~ t,
    data = hkx,
    moderator = ~ d,
    prepost = ~ z
  )

bounds_prepost_s <-
  prepost_bounds(
    formula = y ~ t,
    data = hkx,
    moderator = ~ d,
    prepost = ~ z,
    stable_mod = TRUE
  )

bounds_prepost_m <-
  prepost_bounds(
    formula = y ~ t,
    data = hkx,
    moderator = ~ d,
    prepost = ~ z,
    moderator_mono = c(1, 1)
  )
 
bounds_prepost_o <-
  prepost_bounds(
    formula = y ~ t,
    data = hkx,
    moderator = ~ d,
    prepost = ~ z,
    outcome_mono  = c(-1, -1)
  )

bounds_prepost_mo <-
  prepost_bounds(
    formula = y ~ t,
    data = hkx,
    moderator = ~ d,
    prepost = ~ z,
    outcome_mono  = c(-1, -1),
    moderator_mono = c(1, 1)
  )

bounds_prepost_ms <-
  prepost_bounds(
    formula = y ~ t,
    data = hkx,
    moderator = ~ d,
    prepost = ~ z,
    moderator_mono = c(1, 1),
    stable_mod = TRUE
  )



## Sensitivity analysis ----

# Post-test 

sens_out <-
  post_sens(y ~ t,
            ## hkx,
            data = filter(hkx, z == 1),
            moderator = ~d,
            ## ~z,
            g_by = 0.05,
            q_by = 0.01,
            ## g_max = 0.5,
            ## t_by = 0.1,
            sims = 500,
           ##stable_mod = TRUE,
            moderator_mono = NULL)

# Pre-test 

pre_sens_out <-
  pre_sens(y ~ t,
            #hkx,
            filter(hkx, z == 0),
            ~d,
            outcome_mono = 1)


# Prepost

prepost_sens_out <-
  prepost_sens(y ~ t,
               hkx,
               moderator = ~d,
               prepost =  ~z,
               g_by = 0.01,
               ## g_max = 0.5,
               t_at = c(0.25, 1),
               sims = 500,
               ##stable_mod = TRUE,
               moderator_mono = NULL)

## Gibbs ----

iter_num <- 2000
burn_num <- 1000
thin_num <- 2
paper_covars <- ~ education_num + age + female + close_own_di

gibbs <- list()
deltas <- list()

# 1. gibbs_prepost

res <- list()
set.seed(13)
for (i in 1:4) {

  res[[i]] <-
    prepost_gibbs_nocovar(
      formula = y ~ t,
      data = hkx,
      iter = iter_num,
      burn = burn_num,
      thin = thin_num,
      prepost = ~ z,
      moderator = ~ d,
      monotonicity = FALSE,
      stable = FALSE
    )

  deltas[[i]] <- tibble(delta.1 = res[[i]]$delta.1,
                        delta.2 = res[[i]]$delta.2)
}

deltas <- do.call(bind_rows, deltas)

gibbs[["gibbs_prepost"]] <- tibble(
  type = "gibbs_prepost",
  delta.1 = deltas$delta.1,
  delta.2 = deltas$delta.2
)

# 2. gibbs_prepost_x
set.seed(13)
deltas <- list()
res <- list()
for (i in 1:4) {
  res[[i]] <-
    prepost_gibbs(
      formula = y ~ t,
      data = hkx,
      iter = iter_num,
      burn = burn_num,
      thin = thin_num,
      covariates = paper_covars,
      prepost = ~ z,
      moderator = ~ d,
      monotonicity = FALSE,
      stable = FALSE
    )

  deltas[[i]] <- tibble(delta.1 = res[[i]]$delta.1,
                        delta.2 = res[[i]]$delta.2)
}

deltas <- do.call(bind_rows, deltas)

gibbs[["gibbs_prepost_x"]] <- tibble(
  type = "gibbs_prepost_x",
  delta.1 = deltas$delta.1,
  delta.2 = deltas$delta.2
)

# 3. gibbs_prepost_m
set.seed(13)
deltas <- list()
res <- list()
for (i in 1:4) {
  res[[i]] <-
    prepost_gibbs_nocovar(
      formula = y ~ t,
      data = hkx,
      iter = iter_num,
      burn = burn_num,
      thin = thin_num,
      prepost = ~ z,
      moderator = ~ d,
      monotonicity = TRUE,
      stable = FALSE
    )

  deltas[[i]] <- tibble(delta.1 = res[[i]]$delta.1,
                        delta.2 = res[[i]]$delta.2)
}

deltas <- do.call(bind_rows, deltas)

gibbs[["gibbs_prepost_m"]] <- tibble(
  type = "gibbs_prepost_m",
  delta.1 = deltas$delta.1,
  delta.2 = deltas$delta.2
)

# 4. gibbs_prepost_mx
set.seed(13)
deltas <- list()
res <- list()
for (i in 1:4) {
  res[[i]] <-
    prepost_gibbs(
      formula = y ~ t,
      data = hkx,
      iter = iter_num,
      burn = burn_num,
      thin = thin_num,
      covariates = paper_covars,
      prepost = ~ z,
      moderator = ~ d,
      monotonicity = TRUE,
      stable = FALSE
    )

  deltas[[i]] <- tibble(delta.1 = res[[i]]$delta.1,
                        delta.2 = res[[i]]$delta.2)
}

deltas <- do.call(bind_rows, deltas)

gibbs[["gibbs_prepost_mx"]] <- tibble(
  type = "gibbs_prepost_mx",
  delta.1 = deltas$delta.1,
  delta.2 = deltas$delta.2
)

# 5. gibbs_prepost_s
set.seed(13)
deltas <- list()
res <- list()
for (i in 1:4) {
  res[[i]] <-
    prepost_gibbs_nocovar(
      formula = y ~ t,
      data = hkx,
      iter = iter_num,
      prepost = ~ z,
      moderator = ~ d,
      monotonicity = FALSE,
      stable = TRUE
    )

  deltas[[i]] <- tibble(delta.1 = res[[i]]$delta.1,
                        delta.2 = res[[i]]$delta.2)
}

deltas <- do.call(bind_rows, deltas)

gibbs[["gibbs_prepost_s"]] <- tibble(
  type = "gibbs_prepost_s",
  delta.1 = deltas$delta.1,
  delta.2 = deltas$delta.2
)

# 6. gibbs_prepost_sx
set.seed(13)
deltas <- list()
res <- list()
for (i in 1:4) {
  res[[i]] <-
    prepost_gibbs(
      formula = y ~ t,
      data = hkx,
      iter = iter_num,
      prepost = ~ z,
      moderator = ~ d,
      covariates =  paper_covars,
      monotonicity = FALSE,
      stable = TRUE,
      saturated = TRUE
    )

  deltas[[i]] <- tibble(delta.1 = res[[i]]$delta.1,
                        delta.2 = res[[i]]$delta.2)
}

deltas <- do.call(bind_rows, deltas)

gibbs[["gibbs_prepost_sx"]] <- tibble(
  type = "gibbs_prepost_sx",
  delta.1 = deltas$delta.1,
  delta.2 = deltas$delta.2
)



# 7. gibbs_prepost_ms
set.seed(13)
deltas <- list()
res <- list()
for (i in 1:4) {
  res[[i]] <-
    prepost_gibbs_nocovar(
      formula = y ~ t,
      data = hkx,
      iter = iter_num,
      prepost = ~ z,
      moderator = ~ d,
      monotonicity = TRUE,
      stable = TRUE
    )

  deltas[[i]] <- tibble(delta.1 = res[[i]]$delta.1,
                        delta.2 = res[[i]]$delta.2)
}

deltas <- do.call(bind_rows, deltas)

gibbs[["gibbs_prepost_ms"]] <- tibble(
  type = "gibbs_prepost_ms",
  delta.1 = deltas$delta.1,
  delta.2 = deltas$delta.2
)

# 8. gibbs_prepost_msx
set.seed(13)
deltas <- list()
res <- list()
for (i in 1:4) {
  res[[i]] <-
    prepost_gibbs(
      formula = y ~ t,
      data = hkx,
      iter = iter_num,
      prepost = ~ z,
      moderator = ~ d,
      covariates =  paper_covars,
      monotonicity = TRUE,
      stable = TRUE,
      saturated = TRUE
    )

  deltas[[i]] <- tibble(delta.1 = res[[i]]$delta.1,
                        delta.2 = res[[i]]$delta.2)
}

deltas <- do.call(bind_rows, deltas)

gibbs[["gibbs_prepost_msx"]] <- tibble(
  type = "gibbs_prepost_msx",
  delta.1 = deltas$delta.1,
  delta.2 = deltas$delta.2
)

# 9. gibbs_post
set.seed(13)
deltas <- list()
res <- list()
Qs <- list()
mus <- list()
for (i in 1:4) {
  res[[i]] <-
    prepost_gibbs_nocovar(
      formula = y ~ t,
      data = filter(hkx, z == 1),
      iter = iter_num,
      burn = burn_num,
      thin = thin_num,
      moderator = ~ d,
      prepost = ~ 1,
      monotonicity = FALSE,
      stable = FALSE
    )

  Qs[[i]] <- tibble(Q = rowSums(res[[i]]$psis[, c("s111", "s011", "s101", "s001")]))

  deltas[[i]] <- tibble(delta.1 = res[[i]]$delta.1,
                        delta.2 = res[[i]]$delta.2)
}

psis <- do.call(bind_rows, lapply(res, function(x) as_tibble(x$psis)))
mus <- do.call(bind_rows, lapply(res, function(x) as_tibble(x$mu)))
Qs <- do.call(bind_rows, Qs)
deltas <- do.call(bind_rows, deltas)
ll <- unlist(lapply(res, function(x) x$loglike))
lp <- unlist(lapply(res, function(x) x$logpost))

gibbs[["gibbs_post"]] <- tibble(
  type = "gibbs_post",
  delta.1 = deltas$delta.1,
  delta.2 = deltas$delta.2,
  loglike = ll,
  logpost = lp
)

# 9. gibbs_post_x
set.seed(13)
deltas <- list()
res <- list()
mus <- list()
for (i in 1:4) {
  res[[i]] <-
    prepost_gibbs(
      formula = y ~ t,
      data = filter(hkx, z == 1),
      iter = iter_num,
      burn = burn_num,
      thin = thin_num,
      covariates = paper_covars,
      moderator = ~ d,
      prepost = ~ 1,
      monotonicity = FALSE,
      stable = FALSE
    )

  deltas[[i]] <- tibble(delta.1 = res[[i]]$delta.1,
                        delta.2 = res[[i]]$delta.2)
}

psis <- do.call(bind_rows, lapply(res, function(x) as_tibble(x$psis)))
mus <- do.call(bind_rows, lapply(res, function(x) as_tibble(x$mu)))
deltas <- do.call(bind_rows, deltas)
ll <- unlist(lapply(res, function(x) x$loglike))
lp <- unlist(lapply(res, function(x) x$logpost))

gibbs[["gibbs_post_x"]] <- tibble(
  type = "gibbs_post_x",
  delta.1 = deltas$delta.1,
  delta.2 = deltas$delta.2,
  loglike = ll,
  logpost = lp
)

# 8. gibbs_post_m
set.seed(13)
deltas <- list()
res <- list()
for (i in 1:4) {
  res[[i]] <-
    prepost_gibbs_nocovar(
      formula = y ~ t,
      data = filter(hkx, z == 1),
      iter = iter_num,
      burn = burn_num,
      thin = thin_num,
      moderator = ~ d,
      prepost = ~ 1,
      monotonicity = TRUE,
      stable = FALSE
    )

  deltas[[i]] <- tibble(delta.1 = res[[i]]$delta.1,
                        delta.2 = res[[i]]$delta.2)
}

psis_m <- do.call(bind_rows, lapply(res, function(x) as_tibble(x$psis)))
mus_m <- do.call(bind_rows, lapply(res, function(x) as_tibble(x$mu)))
deltas <- do.call(bind_rows, deltas)
ll_m <- unlist(lapply(res, function(x) x$loglike))
lp_m <- unlist(lapply(res, function(x) x$logpost))


gibbs[["gibbs_post_m"]] <- tibble(
  type = "gibbs_post_m",
  delta.1 = deltas$delta.1,
  delta.2 = deltas$delta.2,
  loglike = ll_m,
  logpost = lp_m
)

# 9. gibbs_post_mx
set.seed(13)
deltas <- list()
res <- list()
for (i in 1:4) {
  res[[i]] <-
    prepost_gibbs(
      formula = y ~ t,
      data = filter(hkx, z == 1),
      iter = iter_num,
      burn = burn_num,
      thin = thin_num,
      moderator = ~ d,
      prepost = ~ 1,
      covariates = paper_covars,
      monotonicity = TRUE,
      stable = FALSE,
      saturated = TRUE
    )

  deltas[[i]] <- tibble(delta.1 = res[[i]]$delta.1,
                        delta.2 = res[[i]]$delta.2)
}

deltas <- do.call(bind_rows, deltas)
gibbs[["gibbs_post_mx"]] <- tibble(
  type = "gibbs_post_mx",
  delta.1 = deltas$delta.1,
  delta.2 = deltas$delta.2
)

# 10. gibbs_post_s
set.seed(13)
deltas <- list()
res <- list()
for (i in 1:4) {
  res[[i]] <-
    prepost_gibbs_nocovar(
      formula = y ~ t,
      data = filter(hkx, z == 1),
      iter = iter_num,
      burn = burn_num,
      thin = thin_num,
      moderator = ~ d,
      prepost = ~ 1,
      monotonicity = FALSE,
      stable = TRUE
    )

  deltas[[i]] <- tibble(delta.1 = res[[i]]$delta.1,
                        delta.2 = res[[i]]$delta.2)
}
ll <- unlist(lapply(res, function(x) x$loglike))
lp <- unlist(lapply(res, function(x) x$logpost))
deltas <- do.call(bind_rows, deltas)

gibbs[["gibbs_post_s"]] <- tibble(
  type = "gibbs_post_s",
  delta.1 = deltas$delta.1,
  delta.2 = deltas$delta.2,
  loglike = ll,
  logpost = lp
)

# 11. gibbs_post_sx
set.seed(13)
deltas <- list()
res <- list()
for (i in 1:4) {
  res[[i]] <-
    prepost_gibbs(
      formula = y ~ t,
      data = filter(hkx, z == 1),
      iter = iter_num,
      burn = burn_num,
      thin = thin_num,
      moderator = ~ d,
      prepost = ~ 1,
      covariates =  paper_covars,
      monotonicity = FALSE,
      stable = TRUE,
      saturated = TRUE
    )

  deltas[[i]] <- tibble(delta.1 = res[[i]]$delta.1,
                        delta.2 = res[[i]]$delta.2)
}

deltas <- do.call(bind_rows, deltas)
gibbs[["gibbs_post_sx"]] <- tibble(
  type = "gibbs_post_sx",
  delta.1 = deltas$delta.1,
  delta.2 = deltas$delta.2
)


# 12. gibbs_post_ms
set.seed(13)
deltas <- list()
res <- list()
for (i in 1:4) {
  res[[i]] <-
    prepost_gibbs_nocovar(
      formula = y ~ t,
      data = filter(hkx, z == 1),
      iter = iter_num,
      burn = burn_num,
      thin = thin_num,
      moderator = ~ d,
      prepost = ~ 1,
      monotonicity = TRUE,
      stable = TRUE
    )

  deltas[[i]] <- tibble(delta.1 = res[[i]]$delta.1,
                        delta.2 = res[[i]]$delta.2)
}

deltas <- do.call(bind_rows, deltas)
ll <- unlist(lapply(res, function(x) x$loglike))
lp <- unlist(lapply(res, function(x) x$logpost))

gibbs[["gibbs_post_ms"]] <- tibble(
  type = "gibbs_post_ms",
  delta.1 = deltas$delta.1,
  delta.2 = deltas$delta.2,
  loglike = ll,
  logpost = lp
)

# 12. gibbs_post_msx
set.seed(13)
deltas <- list()
res <- list()
for (i in 1:4) {
  res[[i]] <-
    prepost_gibbs(
      formula = y ~ t,
      data = filter(hkx, z == 1),
      iter = iter_num,
      burn = burn_num,
      thin = thin_num,
      moderator = ~ d,
      prepost = ~ 1,
      covariates =  paper_covars,
      monotonicity = TRUE,
      stable = TRUE,
      saturated = TRUE
    )

  deltas[[i]] <- tibble(delta.1 = res[[i]]$delta.1,
                        delta.2 = res[[i]]$delta.2)
}

deltas <- do.call(bind_rows, deltas)
gibbs[["gibbs_post_msx"]] <- tibble(
  type = "gibbs_post_msx",
  delta.1 = deltas$delta.1,
  delta.2 = deltas$delta.2
)

## Comparison ----

dat <- tibble(type, est, lower, upper)
dat <- bind_rows(dat, bounds_post %>% unlist())
dat <- bind_rows(dat, bounds_post_m %>% unlist())
dat <- bind_rows(dat, bounds_post_s %>% unlist())
dat <- bind_rows(dat, bounds_post_ms %>% unlist())
dat <- bind_rows(dat, bounds_prepost %>% unlist())
dat <- bind_rows(dat, bounds_prepost_m %>% unlist())
dat <- bind_rows(dat, bounds_prepost_s %>% unlist())
dat <- bind_rows(dat, bounds_prepost_ms %>% unlist())
dat <- bind_rows(dat, bounds_prepost_o %>% unlist())
dat <- bind_rows(dat, bounds_prepost_mo %>% unlist())
dat <- bind_rows(dat, bounds_pre_o %>% unlist())

dat <- dat %>% select("type", "est", "lower", "upper", "ci_lower", "ci_upper", "post_est", "Q", "pre_est")

dat[7:17, 1] <-
  c(
    "bounds_post",
    "bounds_post_m",
    "bounds_post_s",
    "bounds_post_ms",
    "bounds_prepost",
    "bounds_prepost_m",
    "bounds_prepost_s",
    "bounds_prepost_ms",
    "bounds_prepost_o",
    "bounds_prepost_mo",
    'bounds_pre_o'
  )

gibbs_tibble <- do.call(bind_rows, gibbs)
gibbs_delta.1 <-
  gibbs_tibble %>%
  group_by(type) %>%
  dplyr::summarize(
    est = mean(delta.1, na.rm = TRUE),
    lower = unname(quantile(delta.1, 0.025, na.rm = TRUE)),
    upper = unname(quantile(delta.1, 0.975, na.rm = TRUE))
  )

gibbs_delta.1$type <- paste0(gibbs_delta.1$type, "_d1")

gibbs_delta.2 <-
  gibbs_tibble %>%
  group_by(type) %>%
  dplyr::summarize(
    est = mean(delta.2, na.rm = TRUE),
    lower = unname(quantile(delta.2, 0.025, na.rm = TRUE)),
    upper = unname(quantile(delta.2, 0.975, na.rm = TRUE))
  )

gibbs_delta.2$type <- paste0(gibbs_delta.2$type, "_d2")

# temp <- bind_rows(gibbs_delta.1, gibbs_delta.2) %>%
#   arrange(type)

# dat <- bind_rows(dat, temp)
dat <- bind_rows(dat, gibbs_delta.2)

dat$row_num <- 1:nrow(dat)

dat <-
  dat %>%
  mutate(
    panel = case_when(
      str_detect(type, "_prepost") ~ "Prepost",
      str_detect(type, "_post") ~ "Post-only",
      str_detect(type, "_pre") ~ "Pre-only"
    ),
    panel = ordered(panel, levels = c("Pre-only",
                                      "Post-only",
                                      "Prepost")),
    type2 =
      gsub("_prepost|_post|_pre",
           "",
           type),
  )


save(dat,
     mod_pre,
     mod_pre_x,
     mod_post,
     mod_post_x,
     mod_prepost,
     mod_prepost_x,
     sens_out,
     pre_sens_out,
     prepost_sens_out,
     file='output/hk-replication.Rdata')

