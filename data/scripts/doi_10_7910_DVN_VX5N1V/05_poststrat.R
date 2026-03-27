library(tidyverse)
library(brms)
library(tictoc)
library(ccesMRPrun)
library(santoku)
library(fs)
library(vroom)
library(glue)


# data

cd_elec <- read_csv("data/input/cd_presvote_2016-2020.csv", show_col_types = FALSE) |>
  select(year, cd, pct_trump)
cd_race <- read_csv("data/output/by-cd_pct-race_elec.csv", show_col_types = FALSE)

pop <- read_rds("data/output/acs_synth-turnout.rds") |>
  filter(turnout == 1) |>
  left_join(distinct(cd_elec, year, cd, pct_trump), by = c("cd", "year")) |>
  inner_join(cd_race, by = c("year", "cd")) |>
  left_join(select(ccesMRPprep::states_key, st, region, division), by = "st")

fit_brm_list <- list(
  `2016` = read_rds("data/output/brm16_fit.rds"),
  `2020` = read_rds("data/output/brm20_fit.rds")
)

# Post-stratify
pop_int <- mutate(pop, count = as.integer(count_twway))
pop_nonzero <- filter(pop_int, count > 0) |>
  mutate(n_response = count,
         st = str_sub(cd, 1, 2))
pop_nonzero

# individual draws -----
get_preds <- function(yr, pop = pop_nonzero, models = fit_brm_list) {
  stopifnot(is.character(yr))

  pop_yr <- filter(pop, year == as.numeric(yr))
  fit <- models[[yr]]

  ind <- cut_number(1:nrow(pop_yr), n = 4)

  tic() # 30sec, 2021-07-18

  p_dr_1 <- posterior_epred(fit, pop_yr[which(ind == levels(ind)[1]), ], allow_new_levels = FALSE, summary = FALSE)
  p_dr_2 <- posterior_epred(fit, pop_yr[which(ind == levels(ind)[2]), ], allow_new_levels = FALSE, summary = FALSE)
  p_dr_3 <- posterior_epred(fit, pop_yr[which(ind == levels(ind)[3]), ], allow_new_levels = FALSE, summary = FALSE)
  p_dr_4 <- posterior_epred(fit, pop_yr[which(ind == levels(ind)[4]), ], allow_new_levels = FALSE, summary = FALSE)

  p_draws <- cbind(p_dr_1, p_dr_2, p_dr_3, p_dr_4)
  rm(p_dr_1, p_dr_2, p_dr_3, p_dr_4)
  gc()
  toc()

  p_draws
}

p_draws_16 <- get_preds("2016")
p_draws_20 <- get_preds("2020")



# save in chunks
for (yr in c(2016, 2020)) {
  dir_create(glue("data/output/fit-chunk/{yr}"))

  pop_yr <- filter(pop_nonzero, year == yr)

  if (yr == 2016)
    p_draws <- p_draws_16
  if (yr == 2020)
    p_draws <- p_draws_20

  for (s in state.abb) {
    ind_s <- str_which(pop_yr$cd, s)

    p_cells_chunk <- pop_yr[ind_s, ]
    p_draws_chunk <- p_draws[, ind_s]

    save(p_draws_chunk,
         p_cells_chunk,
         file = glue("data/output/fit-chunk/{yr}/chunk-{s}.rda"))
  }
}
