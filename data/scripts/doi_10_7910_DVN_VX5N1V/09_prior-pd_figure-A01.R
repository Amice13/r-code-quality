library(tidyverse)
library(brms)
library(glue)
library(lemon)

svy16 <- read_rds("data/output/ccc_2016-2020_voted_2pty.rds") |>
    filter(year == 2016) |>
    left_join(select(ccesMRPprep::states_key, st, region, division), by = "st")


# Note: can take 1-2 hours, with final brm output totalling 11GB
# prior PD
ff <- trump  ~ (1 + race + educ + female + age | division / st / cd)
fit_sd_tenth <- brm(ff, svy16,
                 family = bernoulli(),
                 prior = c(prior_string("normal(0, 0.1)", class = "sd"),
                           prior_string("normal(0, 0.1)", class = "Intercept")),
                 iter = 5e3, cores = 4,
                 sample_prior = "only"
)

fit_sd_five <- brm(ff, svy16,
                  family = bernoulli(),
                  prior = c(prior_string("normal(0, 5)", class = "sd"),
                            prior_string("normal(0, 5)", class = "Intercept")),
                  iter = 5e3, cores = 4,
                  sample_prior = "only"
)

fit_sd_one <-brm(ff, svy16,
                 family = bernoulli(),
                 prior = c(prior_string("normal(0, 1)", class = "sd"),
                           prior_string("normal(0, 1)", class = "Intercept")),
                 iter = 5e3, cores = 4,
                 sample_prior = "only"
)

fit_sd_hunth <- brm(ff, svy16,
                    family = bernoulli(),
                    prior = c(prior_string("normal(0, 0.01)", class = "sd"),
                              prior_string("normal(0, 0.01)", class = "Intercept")),
                    iter = 5e3, cores = 4,
                    sample_prior = "only"
)

# get predictions on 0-1 scale
vec_prior_tenth <- posterior_epred(fit_sd_tenth)
vec_prior_hunth <- posterior_epred(fit_sd_hunth)
vec_prior_one   <- posterior_epred(fit_sd_one)
vec_prior_five  <- posterior_epred(fit_sd_five)

samp_vec <- sample(nrow(vec_prior_tenth), size = 1e3)

est_df <- bind_rows(list(
    `1e-2` = tibble(est = as.vector(vec_prior_hunth[samp_vec, ])),
    `1e-1` = tibble(est = as.vector(vec_prior_tenth[samp_vec, ])),
    `1e-0` = tibble(est = as.vector(vec_prior_one[samp_vec, ])),
    `5e-0` = tibble(est = as.vector(vec_prior_five[samp_vec, ]))
),
.id = "sd") %>%
    mutate(sd = format(as.numeric(as.character(sd)), scientific = FALSE)) %>%
    mutate(sd = glue("Std. Dev. of Prior: {sd}")) %>%
    mutate(sd = fct_inorder(sd))


# Prior PD figure in APSR paper
est_df %>%
    sample_n(1e5) %>%
    ggplot(aes(x = est)) +
    geom_histogram(aes(y = stat(width*density)), binwidth = 0.05, color = "white", boundary = 0.5) +
    facet_rep_wrap(~ sd, nrow = 1) +
    scale_y_continuous(expand = expansion(mult = c(0, 0.05)),
                       labels = scales::percent_format(accuracy = 1)) +
    labs(
        x = "Estimated Outcome (Probability voting for Trump)",
        y = "Proportion") +
    theme_classic()

