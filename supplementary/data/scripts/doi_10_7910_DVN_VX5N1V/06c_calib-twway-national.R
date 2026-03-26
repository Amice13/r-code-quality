library(tidyverse)
library(vroom)
library(fs)
library(ccesMRPprep)
library(ccesMRPrun)
library(tictoc)
library(data.table)
library(glue)
library(haven)
library(furrr)

out_path <- "../analyze/data"


# get by CD x race ----
strat_files <- dir_ls("data/output/fit-strata/cd-level", regexp = "(2016|2020)/.*csv\\.gz$", recurse = 1)
names(strat_files) <- str_extract(as.character(strat_files), pattern = "[0-9]+(?=/strata-)")
strat_race_mrp <- map_dfr(.x = strat_files,
                          .f = ~ vroom(.x, col_types = "cciddi"),
                          .id = "year")


## data
sim_est <- strat_race_mrp |>
  # specify factor order
  mutate(race = fct_relevel(race, "White", "Black", "Hispanic", "Other"),
         cd = factor(cd, levels = cd_info_2016$cd))

tgt_by_race <- read_csv("data/input/exitpoll_target-Rvote.csv", show_col_types = FALSE)

iters_seq <- unique(sim_est$iter)

plan(multisession, workers = 6)
twway_all_year <- function(yr, iters = iters_seq, tR = tgt_by_race, data = sim_est) {

  # Targets
  tgt_race <- tR |>
    filter(year == yr) |>
    select(race, tau_exit) |>
    deframe()

  if (yr == 2016) {
    cd_info <- cd_info_2016
  }
  if (yr == 2020) {
    cd_info <- cd_info_2020
  }

  tgt_area <- cd_info$pct_trump
  names(tgt_area) <- cd_info$cd
  names(iters) <- glue("yr{yr}_{str_pad(iters, width = 3, pad = '0')}")

  # Data
  mrp_draws_yr <- data |>
    filter(year == yr)

  # for each iter
  future_map_dfr(
    .x = iters,
    .f = function(it) {

      draw_i <- mrp_draws_yr |>
        filter(iter == it) |>
        transmute(
          race,
          cd,
          est = p_mrp_nofix,
          p_mrp_ggfix,
          n = N)

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
    .progress = TRUE,
    .options = furrr_options(packages = c("stringr", "ccesMRPrun", "glue", "dplyr")),
    .id = "yr_iter"
  )
}

# Run in parallel
tic()
out_16 <- twway_all_year(2016, iters_seq)
toc()
out_20 <- twway_all_year(2020, iters_seq)


# Stack together and rename
out_df <-
  bind_rows(out_16, out_20) |>
  separate(col = yr_iter, into = c("year", "iter"), sep = "_") |>
  mutate(year = as.numeric(str_remove(year, "yr")),
         iter = as.numeric(iter),
         race = as.character(race),
         cd = as.character(cd)) |>
  rename(p_mrp_nofix = est,
         p_mrp_twway = est_corrected,
         N = n) |>
  select(year, cd, race, iter, matches("p_mrp_"), N)


# Write -----
write_csv(out_df, path(out_path, "mrp-full-posterior.csv.gz"))


