## Comparing Bayesian estimates under different priors and assumptions ----
# Figure SM.2
# Priors: Default, Extreme, Uniform
# Assumptions: Randomization, Monotonicity, Stability

## Set up ----
library(gtools)
library(tidyverse)
library(forcats)
library(broom)
library(lmtest)
library(sandwich)
library(readstata13)
library(BayesLogit)

theme_set(theme_bw())

set.seed(02143)

# Load cleaned data (hk = full data, hkx = Kalenjin subset)
load(file='output/hk-data-cleaned.Rdata')

iter_num <- 2000
burn_num <- 1000
thin_num <- 2
paper_covars <- ~ education_num + age + female + close_own_di

deltas <- list()

## Extreme priors ----

priors_extr <- list(alpha = 1 / 16,
                    y.alpha = 1 / 16,
                    y.beta = 1 / 16)


gibbs_extr = list()

## Extreme; R only; Prepost

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
      monotonicity = FALSE,
      stable = FALSE,
      priors = priors_extr
    )
  
  deltas[[i]] <- tibble(delta.1 = res[[i]]$delta.1, delta.2 = res[[i]]$delta.2)
}

deltas <- do.call(bind_rows, deltas)

gibbs_extr[["gibbs_prepost"]] <- tibble(
  type = "gibbs_prepost",
  delta.1 = deltas$delta.1,
  delta.2 = deltas$delta.2
)


## Extreme; R + M; Prepost

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
      stable = FALSE,
      priors = priors_extr
    )
  
  deltas[[i]] <- tibble(delta.1 = res[[i]]$delta.1, delta.2 = res[[i]]$delta.2)
}

deltas <- do.call(bind_rows, deltas)

gibbs_extr[["gibbs_prepost_m"]] <- tibble(
  type = "gibbs_prepost_m",
  delta.1 = deltas$delta.1,
  delta.2 = deltas$delta.2
)

## Extreme; R + S; Prepost

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
      stable = TRUE,
      priors = priors_extr
    )
  
  deltas[[i]] <- tibble(delta.1 = res[[i]]$delta.1, delta.2 = res[[i]]$delta.2)
}

deltas <- do.call(bind_rows, deltas)

gibbs_extr[["gibbs_prepost_s"]] <- tibble(
  type = "gibbs_prepost_s",
  delta.1 = deltas$delta.1,
  delta.2 = deltas$delta.2
)

## Extreme; R + M + S; Prepost

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
      stable = TRUE,
      priors = priors_extr
    )
  
  deltas[[i]] <- tibble(delta.1 = res[[i]]$delta.1, delta.2 = res[[i]]$delta.2)
}

deltas <- do.call(bind_rows, deltas)

gibbs_extr[["gibbs_prepost_ms"]] <- tibble(
  type = "gibbs_prepost_ms",
  delta.1 = deltas$delta.1,
  delta.2 = deltas$delta.2
)

## Extreme; R only; Post

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
      stable = FALSE,
      priors = priors_extr
    )
  
  Qs[[i]] <- tibble(Q = rowSums(res[[i]]$psis[, c("s111", "s011", "s101", "s001")]))
  
  deltas[[i]] <- tibble(delta.1 = res[[i]]$delta.1, delta.2 = res[[i]]$delta.2)
}

psis <- do.call(bind_rows, lapply(res, function(x)
  as_tibble(x$psis)))
mus <- do.call(bind_rows, lapply(res, function(x)
  as_tibble(x$mu)))
Qs <- do.call(bind_rows, Qs)
deltas <- do.call(bind_rows, deltas)
ll <- unlist(lapply(res, function(x)
  x$loglike))
lp <- unlist(lapply(res, function(x)
  x$logpost))

gibbs_extr[["gibbs_post"]] <- tibble(
  type = "gibbs_post",
  delta.1 = deltas$delta.1,
  delta.2 = deltas$delta.2,
  loglike = ll,
  logpost = lp
)

## Extreme; R + M; Post

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
      stable = FALSE,
      priors = priors_extr
    )
  
  deltas[[i]] <- tibble(delta.1 = res[[i]]$delta.1, delta.2 = res[[i]]$delta.2)
}

psis_m <- do.call(bind_rows, lapply(res, function(x)
  as_tibble(x$psis)))
mus_m <- do.call(bind_rows, lapply(res, function(x)
  as_tibble(x$mu)))
deltas <- do.call(bind_rows, deltas)
ll_m <- unlist(lapply(res, function(x)
  x$loglike))
lp_m <- unlist(lapply(res, function(x)
  x$logpost))


gibbs_extr[["gibbs_post_m"]] <- tibble(
  type = "gibbs_post_m",
  delta.1 = deltas$delta.1,
  delta.2 = deltas$delta.2,
  loglike = ll_m,
  logpost = lp_m
)

## Extreme; R + S; Post

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
      stable = TRUE,
      priors = priors_extr
    )
  
  deltas[[i]] <- tibble(delta.1 = res[[i]]$delta.1, delta.2 = res[[i]]$delta.2)
}
ll <- unlist(lapply(res, function(x)
  x$loglike))
lp <- unlist(lapply(res, function(x)
  x$logpost))
deltas <- do.call(bind_rows, deltas)

gibbs_extr[["gibbs_post_s"]] <- tibble(
  type = "gibbs_post_s",
  delta.1 = deltas$delta.1,
  delta.2 = deltas$delta.2,
  loglike = ll,
  logpost = lp
)

## Extreme; R + M + S; Post

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
      stable = TRUE,
      priors = priors_extr
    )
  
  deltas[[i]] <- tibble(delta.1 = res[[i]]$delta.1, delta.2 = res[[i]]$delta.2)
}

deltas <- do.call(bind_rows, deltas)
ll <- unlist(lapply(res, function(x)
  x$loglike))
lp <- unlist(lapply(res, function(x)
  x$logpost))

gibbs_extr[["gibbs_post_ms"]] <- tibble(
  type = "gibbs_post_ms",
  delta.1 = deltas$delta.1,
  delta.2 = deltas$delta.2,
  loglike = ll,
  logpost = lp
)


## Uniform priors ----

deltas <- list()

priors_uni <- list(alpha = 1,
                   y.alpha = 1,
                   y.beta = 1)

gibbs_uni = list()

## Unif; R only; Prepost

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
      monotonicity = FALSE,
      stable = FALSE,
      priors = priors_uni
    )
  
  deltas[[i]] <- tibble(delta.1 = res[[i]]$delta.1, delta.2 = res[[i]]$delta.2)
}

deltas <- do.call(bind_rows, deltas)

gibbs_uni[["gibbs_prepost"]] <- tibble(
  type = "gibbs_prepost",
  delta.1 = deltas$delta.1,
  delta.2 = deltas$delta.2
)



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
      stable = FALSE,
      priors = priors_uni
    )
  
  deltas[[i]] <- tibble(delta.1 = res[[i]]$delta.1, delta.2 = res[[i]]$delta.2)
}

deltas <- do.call(bind_rows, deltas)

gibbs_uni[["gibbs_prepost_m"]] <- tibble(
  type = "gibbs_prepost_m",
  delta.1 = deltas$delta.1,
  delta.2 = deltas$delta.2
)


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
      stable = TRUE,
      priors = priors_uni
    )
  
  deltas[[i]] <- tibble(delta.1 = res[[i]]$delta.1, delta.2 = res[[i]]$delta.2)
}

deltas <- do.call(bind_rows, deltas)

gibbs_uni[["gibbs_prepost_s"]] <- tibble(
  type = "gibbs_prepost_s",
  delta.1 = deltas$delta.1,
  delta.2 = deltas$delta.2
)


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
      stable = TRUE,
      priors = priors_uni
    )
  
  deltas[[i]] <- tibble(delta.1 = res[[i]]$delta.1, delta.2 = res[[i]]$delta.2)
}

deltas <- do.call(bind_rows, deltas)

gibbs_uni[["gibbs_prepost_ms"]] <- tibble(
  type = "gibbs_prepost_ms",
  delta.1 = deltas$delta.1,
  delta.2 = deltas$delta.2
)

# Post

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
      stable = FALSE,
      priors = priors_uni
    )
  
  Qs[[i]] <- tibble(Q = rowSums(res[[i]]$psis[, c("s111", "s011", "s101", "s001")]))
  
  deltas[[i]] <- tibble(delta.1 = res[[i]]$delta.1, delta.2 = res[[i]]$delta.2)
}

psis <- do.call(bind_rows, lapply(res, function(x)
  as_tibble(x$psis)))
mus <- do.call(bind_rows, lapply(res, function(x)
  as_tibble(x$mu)))
Qs <- do.call(bind_rows, Qs)
deltas <- do.call(bind_rows, deltas)
ll <- unlist(lapply(res, function(x)
  x$loglike))
lp <- unlist(lapply(res, function(x)
  x$logpost))

gibbs_uni[["gibbs_post"]] <- tibble(
  type = "gibbs_post",
  delta.1 = deltas$delta.1,
  delta.2 = deltas$delta.2,
  loglike = ll,
  logpost = lp
)



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
      stable = FALSE,
      priors = priors_uni
    )
  
  deltas[[i]] <- tibble(delta.1 = res[[i]]$delta.1, delta.2 = res[[i]]$delta.2)
}

psis_m <- do.call(bind_rows, lapply(res, function(x)
  as_tibble(x$psis)))
mus_m <- do.call(bind_rows, lapply(res, function(x)
  as_tibble(x$mu)))
deltas <- do.call(bind_rows, deltas)
ll_m <- unlist(lapply(res, function(x)
  x$loglike))
lp_m <- unlist(lapply(res, function(x)
  x$logpost))


gibbs_uni[["gibbs_post_m"]] <- tibble(
  type = "gibbs_post_m",
  delta.1 = deltas$delta.1,
  delta.2 = deltas$delta.2,
  loglike = ll_m,
  logpost = lp_m
)


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
      stable = TRUE,
      priors = priors_uni
    )
  
  deltas[[i]] <- tibble(delta.1 = res[[i]]$delta.1, delta.2 = res[[i]]$delta.2)
}
ll <- unlist(lapply(res, function(x)
  x$loglike))
lp <- unlist(lapply(res, function(x)
  x$logpost))
deltas <- do.call(bind_rows, deltas)

gibbs_uni[["gibbs_post_s"]] <- tibble(
  type = "gibbs_post_s",
  delta.1 = deltas$delta.1,
  delta.2 = deltas$delta.2,
  loglike = ll,
  logpost = lp
)

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
      stable = TRUE,
      priors = priors_uni
    )
  
  deltas[[i]] <- tibble(delta.1 = res[[i]]$delta.1, delta.2 = res[[i]]$delta.2)
}

deltas <- do.call(bind_rows, deltas)
ll <- unlist(lapply(res, function(x)
  x$loglike))
lp <- unlist(lapply(res, function(x)
  x$logpost))

gibbs_uni[["gibbs_post_ms"]] <- tibble(
  type = "gibbs_post_ms",
  delta.1 = deltas$delta.1,
  delta.2 = deltas$delta.2,
  loglike = ll,
  logpost = lp
)


## Comparison ----

gibbs_tibble <- 
  do.call(bind_rows, gibbs_extr) %>%
  mutate(priors = 'Extreme') %>% 
  bind_rows(do.call(bind_rows, gibbs_uni) %>%
  mutate(priors = 'Uniform'))

gibbs_delta.1 <-
  gibbs_tibble %>%
  group_by(type, priors) %>%
  summarize(
    est = mean(delta.1, na.rm = TRUE),
    lower = unname(quantile(delta.1, 0.025, na.rm = TRUE)),
    upper = unname(quantile(delta.1, 0.975, na.rm = TRUE))
  )

gibbs_delta.1$type <- paste0(gibbs_delta.1$type, "_d1")

gibbs_delta.2 <-
  gibbs_tibble %>%
  group_by(type, priors) %>%
  summarize(
    est = mean(delta.2, na.rm = TRUE),
    lower = unname(quantile(delta.2, 0.025, na.rm = TRUE)),
    upper = unname(quantile(delta.2, 0.975, na.rm = TRUE))
  )

gibbs_delta.2$type <- paste0(gibbs_delta.2$type, "_d2")

temp <- bind_rows(gibbs_delta.1, gibbs_delta.2) %>%
  arrange(type)

dat_priors <- gibbs_delta.2

dat_priors$row_num <- 1:nrow(dat_priors)

dat_priors <-
  dat_priors %>%
  mutate(
    panel = case_when(
      str_detect(type, "_prepost") ~ "Prepost",
      str_detect(type, "_post") ~ "Post-only",
      str_detect(type, "_pre") ~ "Pre-only"
    ),
    panel = ordered(panel, levels = c("Pre-only", "Post-only", "Prepost")),
    type2 =
      gsub("_prepost|_post|_pre", "", type),
  )


## Figure SM.2 ----

# load Gibbs results with default priors for comparison
load('output/hk-replication.Rdata') # object "dat" stores the results

dat_main <- dat

# merge together for plot

dat_priors <- dat_priors %>%
  bind_rows(
    dat_main %>%
      mutate(priors = 'Default (alpha = 0.5 / # of strata)') %>%
      select(type , priors  , est , lower , upper  , row_num , panel  , type2) %>%
      filter(type2 %in% unique(dat_priors$type2))
  )
rm(dat_main)
dat_priors

dw = .8

dat_labels = data.frame(
  x = rep(1.8, 5),
  y = rep(-1, 5),
  assumptions = factor(
    c(
      "Randomization",
      "Randomization\n+ Monotonicity",
      "Randomization\n+ Stability",
      "Randomization\n+ Monotonicity\n+ Stability",
      "OLS"
    ),
    levels = c(
      "Randomization",
      "Randomization\n+ Monotonicity",
      "Randomization\n+ Stability",
      "Randomization\n+ Monotonicity\n+ Stability",
      "OLS"
    )
  )
)

dat_labels2 = data.frame(
  x = rep(2.2, 5),
  y = rep(-0.8, 5),
  assumptions = factor(
    c(
      "Randomization",
      "Randomization\n+ Monotonicity",
      "Randomization\n+ Stability",
      "Randomization\n+ Monotonicity\n+ Stability",
      "OLS"
    ),
    levels = c(
      "Randomization",
      "Randomization\n+ Monotonicity",
      "Randomization\n+ Stability",
      "Randomization\n+ Monotonicity\n+ Stability",
      "OLS"
    )
  )
)

p <- dat_priors %>%
  filter(panel != "Pre-only", (str_detect(type2, "gibbs") |
                                 str_detect(type2, "ols"))) %>%
  mutate(
    upper_bounds = case_when(str_detect(type2, "bounds") ~ upper),
    lower_bounds = case_when(str_detect(type2, "bounds") ~ lower),
    covars = ifelse(str_detect(type2, "x"), "Covariates", "No covariates"),
    covars = fct_relevel(covars, "No covariates", "Covariates")
  ) %>%
  mutate(
    type2 = fct_relevel(
      type2,
      "gibbs_d2",
      "gibbs_x_d2",
      "gibbs_m_d2",
      "gibbs_mx_d2",
      "gibbs_s_d2",
      "gibbs_sx_d2",
      "gibbs_ms_d2",
      "gibbs_msx_d2",
      "ols",
      "ols_x"
    ),
    assumptions = case_when(
      type2 %in% c("gibbs_d2", "gibbs_x_d2") ~ "Randomization",
      type2 %in% c("gibbs_m_d2", "gibbs_mx_d2") ~ "Randomization\n+ Monotonicity",
      type2 %in% c("gibbs_s_d2", "gibbs_sx_d2") ~ "Randomization\n+ Stability",
      type2 %in% c("gibbs_ms_d2", "gibbs_msx_d2") ~ "Randomization\n+ Monotonicity\n+ Stability",
      type2 %in% c("ols", "ols_x") ~ "OLS"
    ),
    assumptions = factor(
      assumptions,
      levels = c(
        "Randomization",
        "Randomization\n+ Monotonicity",
        "Randomization\n+ Stability",
        "Randomization\n+ Monotonicity\n+ Stability",
        "OLS"
      )
    )
  ) %>%
  ggplot(aes(x = covars, color = panel, shape = priors)) +
  facet_grid(cols = vars(assumptions)) +
  geom_hline(yintercept = 0,
             lty = "dotted",
             color = "red") +
  labs(x = "", y = "") +
  geom_point(aes(y = est), size = 3, position = position_dodge(width = dw)) +
  geom_linerange(aes(ymin = lower, ymax = upper),
                 size = 1,
                 position = position_dodge(width = dw)) +
  geom_linerange(
    aes(ymin = lower_bounds, ymax = upper_bounds),
    size = 3,
    position = position_dodge(width = dw)
  ) +
  geom_linerange(aes(ymin = lower, ymax = upper),
                 size = 1,
                 position = position_dodge(width = dw)) +
  guides(color = guide_legend(override.aes = list(size = 3, shape = NA))) +
  labs(#title = "Comparing Bayesian estimates under different assumptions,\nwith and without covariates",
    #title = "Y = support candidate; D = land very insecure; T = (T2 or T1) vs control",
    # subtitle = "X1 = Poor; X2 = Low education; X3 = Over 35",
    color = "", shape = "") +
  scale_x_discrete(breaks = NULL) +
  scale_color_manual(values = c("grey", "black")) +
  coord_cartesian(ylim = c(-1.3, 1.7),
                  clip = "off",
                  #xlim = c(0.5, 10.5),
                  expand = FALSE) +
  theme(
    panel.grid = element_blank(),
    legend.position = "bottom",
    legend.direction = "horizontal",
    legend.box = "horizontal",
    plot.margin = unit(c(1, 1, 1, 0), "lines")
  )


ggsave(
  "figures/hk-priors-plot.pdf",
  width = 8,
  height = 4,
  dpi = 300
)
