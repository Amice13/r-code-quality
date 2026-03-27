library(tidyverse)
library(ccesMRPprep)
library(ccesMRPrun)
library(brms)
library(tictoc)

# Survey data and covariates ------
svy <- read_rds("data/output/ccc_2016-2020_voted_2pty.rds")
cd_elec <- read_csv("data/input/cd_presvote_2016-2020.csv") |>
  select(year, cd, pct_trump)
cd_race <- read_csv("data/output/by-cd_pct-race_elec.csv")

svy_elec <- svy |>
  mutate(
    race_Black    = as.integer(race == "Black"),
    race_Hispanic = as.integer(race == "Hispanic"),
    race_Other    = as.integer(race == "Other"),
  ) |>
  inner_join(cd_elec, by = c("year", "cd")) |>
  inner_join(cd_race, by = c("year", "cd")) |>
  left_join(select(ccesMRPprep::states_key, st, region, division), by = "st")

stopifnot(nrow(svy_elec) == nrow(svy) - sum(svy$cd == "DC-01"))

svy16_elec <- filter(svy_elec, year == 2016)
svy20_elec <- filter(svy_elec, year == 2020)


# Fit ----
Form <- trump  ~ (1 + race * educ + female + age | division / st / cd) +
  race + pct_white_elec +
  race_Black:pct_black_elec + race_Hispanic:pct_hispanic_elec + race_Other:pct_raceother_elec +
  s(pct_trump, k = 3)

# direct ----
fit_dir_race <- direct_ests(Form, svy_elec, area_var = c("year", "cd", "race"), weight_var = "weight") |>
    complete(year, cd, race) |>
    mutate(n_raw = replace_na(n_raw, 0),
           n_wt = replace_na(n_wt, 0))

fit_dir_race |> write_rds("data/output/dir_fit_race.rds")

# Fit Trump hieararchical vote model -----
# 132 min with varying slopes, cmdstanr 2e3 iter on M1 chip; 180 min with cd_nonwhite interaction
plan(multisession, workers = 4)
tic()
fit16_brm <- fit_brms(Form, .data = svy16_elec, .backend = "cmdstanr", .iter = 2e3, .threads = 1)
toc()
fit16_brm |> write_rds("data/output/brm16_fit.rds")
gc()

# 125 mins with cmdstanr on M1 chip; 230 min with cd_nonwhite interaction
tic()
fit20_brm <- fit_brms(Form, .data = svy20_elec, .backend = "cmdstanr", .iter = 2e3, .threads = 1)
toc()
fit20_brm |> write_rds("data/output/brm20_fit.rds")



