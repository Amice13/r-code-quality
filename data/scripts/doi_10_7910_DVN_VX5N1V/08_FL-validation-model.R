library(tidyverse)
library(ccesMRPprep)
library(ccesMRPrun)
library(brms)
library(tictoc)
library(glue)
library(fs)

out_path <- "../analyze/data"


# Functions ------
#' Correct intercept shift. One target at a time
gelman_intshift <- function(draws, target) {

  correct_add <- draws |>
    group_by(cd, iter) |>
    summarize(
      delta = ccesMRPrun::calib_oneway(tgt = as.numeric(target), ests = pred_n_yes / n_response, n = n_response),
      .groups = "drop"
    )

  out <- draws |>
    left_join(correct_add, by = c("cd", "iter")) |>
    mutate(
      pred_nofix = pred_n_yes,
      pred_n_yes = n_response * ccesMRPrun:::invlogit(delta + logit_ghitza(pred_n_yes / n_response))
    )
}

#' onestep function
party_MRP_iter <- function(pop = pop_nonzero,
                           fit = NULL,
                           tgt_df = NULL,
                           tgt_varname = NULL) {

  p_draws <- posterior_epred(fit, pop, summary = FALSE, allow_new_levels = TRUE)

  cell_strat  <- map_dfr(
    .x  = unique(pop$cd),
    .f = function(.x) {

      ind_s <- str_which(pop$cd, .x)

      p_cells_chunk <- pop[ind_s, ]
      p_draws_j <- p_draws[, ind_s]

      out_j <- pivot_celldraws_longer(mod_draws = p_draws_j, data_strat = p_cells_chunk, yhat_name = "pred_n_yes")

      if (!is.null(tgt_df)) {
        out_j <- gelman_intshift(out_j, target =  tgt_df[tgt_df$cd == .x, tgt_varname])
      }

      out_f <- out_j |>
        select(cd, race, iter, n_response, pred_n_yes, pred_nofix) |>
        mutate(race = as.character(race))

      cat(.x, ", ")
      return(out_f)
    }
  )

  # collapse into CD-race-iter
  sims_dt <- data.table::data.table(cell_strat)
  by_cd_iter <- sims_dt[,
                        list(p_mrp_ggfix = sum(pred_n_yes)/sum(n_response),
                             p_mrp_nofix = sum(pred_nofix)/sum(n_response),
                             denominator = sum(n_response)),
                        by = c("cd", "race", "iter")] |>
    as_tibble()

}

#' Reorder factors
race_to_fct <- function(tbl) {
  tbl |>
    mutate(race = factor(race, levels = c("White", "Black", "Hispanic", "Other")))
}

# Svy ----
svy <- read_rds("data/output/ccc_2016-2020_voted_2pty.rds")
cd_elec <- read_csv("data/input/cd_presvote_2016-2020.csv") |>
  select(year, cd, pct_trump)
cd_race <- read_csv("data/output/by-cd_pct-race_elec.csv")

svy_elec <- svy |>
  inner_join(cd_elec, by = c("year", "cd")) |>
  inner_join(cd_race, by = c("year", "cd")) |>
  left_join(select(states_key, st, region, division))


# Pop -----
pop_nonzero <- read_rds("data/output/acs_synth-turnout.rds") |>
  filter(turnout == 1, year == 2016, st == "FL") |>
  left_join(distinct(cd_info_2018, cd, pct_trump)) |>
  left_join(cd_race, by = c("year", "cd")) |>
  mutate(st = str_sub(cd, 1, 2)) |>
  left_join(select(states_key, st, region, division)) |>
  mutate(race = fct_drop(race),
         race_Black = as.integer(race == "Black"),
         race_Hispanic = as.integer(race == "Hispanic"),
         race_Other = as.integer(race == "Other")) |>
  mutate(count = as.integer(count_twway)) |>
  filter(count > 0) |>
  mutate(n_response = count, st = str_sub(cd, 1, 2))


# Truth -----
cat_truth <- read_csv("data/output/voterfile-target/by-cd-race_catalist.csv") |>
  rename(race_N_catalist = race_N)

cat_long <-  cat_truth |>
  filter(str_detect(cd, "FL")) |>
  pivot_longer(-c(year, cd, race, race_N_catalist),
               names_prefix = "vv_party_",
               names_to = "vv_party",
               values_to = "tau_catalist") |>
  filter(year == 2016, cd != "FL-NA")

cat_area <- cat_long |>
  group_by(year, cd, vv_party) |>
  summarize(tau_catalist = weighted.mean(tau_catalist, race_N_catalist), .groups = "drop")

cat_area_R <- cat_area |>
  filter(vv_party == "R")

# All states
cat_area_tgt <- filter(cat_area_R, str_detect(cd, "FL"))
cat_long_tgt <- filter(cat_long, vv_party == "R", str_detect(cd, "FL"))


# Fit multilevel ------
form_bygrp <- vv_party_R ~ (1 + race * educ + female + age | division / st / cd) + s(pct_trump, k = 3) + race +
  pct_nonwhite_elec + race_Black:pct_black_elec + race_Hispanic:pct_hispanic_elec + race_Other:pct_raceother_elec

tic() # 45 min for FL, one model, 10 min on M2
fit_party <- fit_brms(
  form_bygrp,
  .data = filter(svy_elec, year == 2016, st %in% "FL"),
  .backend = "cmdstanr",
  .threads = 2,
  .iter = 2e3,
  .seed = 06510)
toc()


# Do Poststratification and Ghitza Gelman correction ------
tic()
iters_FL <- party_MRP_iter(
  pop = pop_nonzero,
  fit = fit_party,
  tgt_df = cat_area_tgt,
  tgt_varname = "tau_catalist"
)
toc()


# Two-way ----
# Target by race statewide
tgt_race <- cat_long_tgt |>
  filter(vv_party == "R") |>
  race_to_fct() |>
  group_by(race) |>
  summarize(tau_catalist = weighted.mean(tau_catalist)) |>
  deframe()

tgt_area <- cat_area_R$tau_catalist
names(tgt_area) <- cat_area_R$cd


# for each iter
mrp_twway_iter <- map_dfr(
  .x = setNames(seq_len(2000), seq_len(2000)),
  .f = function(it, mrp_iter = race_to_fct(iters_FL)) {
    iter_name <- glue("{str_pad(it, width = 3, pad = '0')}")

    draw_i <- mrp_iter |>
      filter(iter == it) |>
      transmute(
        race,
        cd,
        est = p_mrp_nofix,
        p_mrp_ggfix,
        n = denominator)

    if (it %% 100 == 1)
      cat(glue("{it}, "))

    calib_twoway(
      data = draw_i,
      var_area  = "cd",
      var_group = "race",
      tgt_area  = tgt_area,
      tgt_group = tgt_race,
      n_area  = draw_i |> count(cd, wt = n) |> pull(n),
      n_group = draw_i |> count(race, wt = n) |> pull(n),
      n_total = sum(draw_i$n),
      use_grad = TRUE
    )
  },
  .id = "iter"
)


# Add validation target column -----

mrp_twway <- mrp_twway_iter |>
  select(iter:cd, p_mrp_nofix = est, p_mrp_ggfix, p_mrp_twway = est_corrected) |>
  pivot_longer(-c(iter, race, cd), names_to = "mrp_type") |>
  group_by(mrp_type, cd, race) |>
  summarize(p_mrp_est = mean(value),
            p_mrp_se = sd(value),
            p_mrp_025 = quantile(value, 0.025),
            p_mrp_050 = quantile(value, 0.05),
            p_mrp_100 = quantile(value, 0.1),
            p_mrp_900 = quantile(value, 0.9),
            p_mrp_950 = quantile(value, 0.95),
            p_mrp_975 = quantile(value, 0.975)) |>
  left_join(cat_long_tgt, by = c("cd", "race"))


# Post-stratify from models  ------
val_cd_twway <- mrp_twway |>
  rename(tau_catalist_R = tau_catalist) |>
  mutate(mrp_type = str_remove(mrp_type, "p_mrp_")) |>
  relocate(
    year,
    race,
    mrp_type,
    cd,
    tau_catalist_R
  )


# Save ------
val_cd_twway |> write_csv(path(out_path, "FL-partyreg/FL-val_MRP_partyreg-by-race.csv"))
