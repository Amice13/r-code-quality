## PPD under different priors ----
# For Figure SM.1

## Set up ----

library(tidyverse)
library(broom)
library(lmtest)
library(sandwich)
library(prepost)
library(readstata13)

# Load cleaned data (hk = full data, hkx = Kalenjin subset)
load(file='output/hk-data-cleaned.Rdata')

set.seed(02143)

# Set parameters for Gibbs sampler
mono_set <- TRUE
stable_set <- TRUE
iter_num <- 4000
burn_num <- 2000
sat_set <- TRUE


## Default priors ----

results1 <- prepost_gibbs_nocovar(
  y ~ t,
  hkx,
  ~ z,
  ~ d,
  iter = iter_num,
  burn = burn_num,
  thin = 2,
  mono = mono_set,
  stable = stable_set,
  predictive = TRUE
)

results2 <- prepost_gibbs_nocovar(
  y ~ t,
  hkx,
  ~ z,
  ~ d,
  iter = iter_num,
  burn = burn_num,
  thin = 2,
  mono = mono_set,
  stable = stable_set,
  predictive = TRUE
)

results3 <- prepost_gibbs_nocovar(
  y ~ t,
  hkx,
  ~ z,
  ~ d,
  iter = iter_num,
  burn = burn_num,
  thin = 2,
  mono = mono_set,
  stable = stable_set,
  predictive = TRUE
)

results4 <- prepost_gibbs_nocovar(
  y ~ t,
  hkx,
  ~ z,
  ~ d,
  iter = iter_num,
  burn = burn_num,
  thin = 2,
  mono = mono_set,
  stable = stable_set,
  predictive = TRUE
)

ppd_mono_stab <- as_tibble(results1$delta.2) %>%
  bind_rows(as_tibble(results2$delta.2)) %>%
  bind_rows(as_tibble(results3$delta.2)) %>%
  bind_rows(as_tibble(results4$delta.2))
ppd_mono_stab <- ppd_mono_stab |>
  mutate(prior = "Default (alpha = 0.5 / # of strata)")

## Uniform priors ----

priors_unif <- list(alpha = 1,
               y.alpha = 1,
               y.beta = 1)

results1_unif <- prepost_gibbs_nocovar(
  y ~ t,
  hkx,
  ~ z,
  ~ d,
  iter = iter_num,
  burn = burn_num,
  thin = 2,
  mono = mono_set,
  stable = stable_set,
  predictive = TRUE,
  priors = priors_unif
)

results2_unif <- prepost_gibbs_nocovar(
  y ~ t,
  hkx,
  ~ z,
  ~ d,
  iter = iter_num,
  burn = burn_num,
  thin = 2,
  mono = mono_set,
  stable = stable_set,
  predictive = TRUE,
  priors = priors_unif
)

results3_unif <- prepost_gibbs_nocovar(
  y ~ t,
  hkx,
  ~ z,
  ~ d,
  iter = iter_num,
  burn = burn_num,
  thin = 2,
  mono = mono_set,
  stable = stable_set,
  predictive = TRUE,
  priors = priors_unif
)

results4_unif <- prepost_gibbs_nocovar(
  y ~ t,
  hkx,
  ~ z,
  ~ d,
  iter = iter_num,
  burn = burn_num,
  thin = 2,
  mono = mono_set,
  stable = stable_set,
  predictive = TRUE,
  priors = priors_unif
)

ppd_unif <- as_tibble(results1_unif$delta.2) %>%
  bind_rows(as_tibble(results2_unif$delta.2)) %>%
  bind_rows(as_tibble(results3_unif$delta.2)) %>%
  bind_rows(as_tibble(results4_unif$delta.2))
ppd_unif <- ppd_unif |>
  mutate(prior = "Uniform (alpha = 1)")

## Extreme priors ----

priors_extr <- list(alpha = 1 / 16,
                    y.alpha = 1 / 16,
                    y.beta = 1 / 16)

results1_extr <- prepost_gibbs_nocovar(
  y ~ t,
  hkx,
  ~ z,
  ~ d,
  iter = iter_num,
  burn = burn_num,
  thin = 2,
  mono = mono_set,
  stable = stable_set,
  predictive = TRUE,
  priors = priors_extr
)

results2_extr <- prepost_gibbs_nocovar(
  y ~ t,
  hkx,
  ~ z,
  ~ d,
  iter = iter_num,
  burn = burn_num,
  thin = 2,
  mono = mono_set,
  stable = stable_set,
  predictive = TRUE,
  priors = priors_extr
)

results3_extr <- prepost_gibbs_nocovar(
  y ~ t,
  hkx,
  ~ z,
  ~ d,
  iter = iter_num,
  burn = burn_num,
  thin = 2,
  mono = mono_set,
  stable = stable_set,
  predictive = TRUE,
  priors = priors_extr
)

results4_extr <- prepost_gibbs_nocovar(
  y ~ t,
  hkx,
  ~ z,
  ~ d,
  iter = iter_num,
  burn = burn_num,
  thin = 2,
  mono = mono_set,
  stable = stable_set,
  predictive = TRUE,
  priors = priors_extr
)


ppd_extr <- as_tibble(results1_extr$delta.2) %>%
  bind_rows(as_tibble(results2_extr$delta.2)) %>%
  bind_rows(as_tibble(results3_extr$delta.2)) %>%
  bind_rows(as_tibble(results4_extr$delta.2))
ppd_extr <- ppd_extr |>
  mutate(prior = "Extreme (alpha = 1/16)")


## Figure SM.1 ----

ppd <- bind_rows(ppd_unif, ppd_mono_stab, ppd_extr)

ppd |>
  rename(Prior = prior) |>
  ggplot(aes(x = value, color = Prior, fill = Prior)) +
  geom_density(size = 1.1, alpha = 0.1) +
  theme_bw() +
  labs(x = "Prior Predictive Distribution", y = "Density")
ggsave("figures/ppd.pdf", width = 8, height = 4)

