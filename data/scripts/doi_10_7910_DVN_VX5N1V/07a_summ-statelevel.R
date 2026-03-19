library(tidyverse)
library(ccesMRPprep)
library(ccesMRPrun)
library(fs)
library(glue)

out_path <- "../analyze/data"

# Calibration -----
st_info <- bind_rows(cd_info_2016,
          cd_info_2020) |>
  mutate(st = str_sub(cd, 1, 2)) |>
  group_by(year, st) |>
  summarize(pct_trump = weighted.mean(pct_trump, presvotes_total))

tgt_by_race <- read_csv("data/input/exitpoll_target-Rvote.csv", show_col_types = FALSE)


# read state iterations----
strat_st_iter <- list()
for (yr in c(2016, 2020)) {
  files_vec <- dir_ls(path("data/output/fit-strata/state-level", yr))
  names(files_vec) <- str_sub(str_remove(path_file(files_vec), "strata-"), 1, 2)

  strat_st_iter[[as.character(yr)]]  <- map_dfr(files_vec, .f = \(x) read_csv(x, col_types = cols()), .id = "st")
}

# stack
st_iter <- bind_rows(strat_st_iter, .id = "year") |>
  mutate(race = fct_relevel(race, "White", "Black", "Hispanic", "Other"),
         st = factor(st, levels = unique(st_info$st)))

# Two-way calib ----
iters_seq <- unique(st_iter$iter)
out_list <- list()
for (yr in c(2020, 2016)) {

  # Targets
  tgt_race <- tgt_by_race |>
    filter(year == yr) |>
    select(race, tau_exit) |>
    deframe()

    tgt_st <- st_info |> filter(year == yr)

  tgt_area <- tgt_st$pct_trump
  names(tgt_area) <- tgt_st$st

  # Data
  mrp_draws_yr <- st_iter |>
    filter(year == yr)

  # for each iter
  for (it in iters_seq) {

    iter_name <- glue("yr{yr}_{str_pad(it, width = 3, pad = '0')}")

    draw_i <- mrp_draws_yr |>
      filter(iter == it) |>
      transmute(
        race,
        st,
        est = p_mrp_nofix,
        p_mrp_ggfix,
        n = N)

    out_list[[iter_name]] <- calib_twoway(
      data = draw_i,
      var_area  = "st",
      var_group = "race",
      tgt_area  = tgt_area,
      tgt_group = tgt_race,
      n_area  = draw_i |> count(st, wt = n) |> pull(n),
      n_group = draw_i |> count(race, wt = n) |> pull(n),
      n_total = sum(draw_i$n),
      use_grad = TRUE
    )

    if (it %% 100 == 0) {
      cat(glue("{it}, "))
    }
  }
}

out_df <- out_list |>
  map(.f = function(x) mutate(x, race = as.character(race), st = as.character(st))) |>
  bind_rows(.id = "yr_iter") |>
  separate(col = yr_iter, into = c("year", "iter"), sep = "_") |>
  mutate(year = as.numeric(str_remove(year, "yr")),
         iter = as.numeric(iter)) |>
  rename(p_mrp_nofix = est,
         p_mrp_twway = est_corrected,
         N = n) |>
  select(year, st, race, iter, matches("p_mrp_"), N)


# state export -------
out_long <- out_df |>
  pivot_longer(-c(year:iter, N),
               names_to = "estimator",
               values_to = "p_mrp")

st_race <- out_long |>
  summ_sims(area_var = c("estimator", "year", "st", "race"), est_var = "p_mrp")

areas_st <- out_long |>
  group_by(estimator, year, st, iter) |>
  summarize(p_mrp = weighted.mean(p_mrp, N)) |>
  summ_sims(area_var = c("estimator", "year", "st"), est_var = "p_mrp") |>
  mutate(race = "All")

st_race_nw <- out_long |>
  mutate(race = recode(race, White = "White", .default = "Non-White")) |>
  filter(race == "Non-White") |>
  group_by(estimator, year, st, race, iter) |>
  summarize(p_mrp = weighted.mean(p_mrp, N)) |>
  summ_sims(area_var = c("estimator","year", "st", "race"), est_var = "p_mrp")

st_race_df <- st_race |>
  bind_rows(st_race_nw) |>
  bind_rows(areas_st)


# write ----
st_race_df |> write_csv(path(out_path, "mrp-ests_by-state-race.csv"))
