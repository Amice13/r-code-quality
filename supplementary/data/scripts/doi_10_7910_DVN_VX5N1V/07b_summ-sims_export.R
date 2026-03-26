library(ccesMRPprep)
library(ccesMRPrun)
library(tidyverse)
library(tidyr)
library(janitor)
library(haven)
library(data.table)
library(dtplyr)
library(fs)

out_path <- "../analyze/data"

strat_race_mrp <- read_csv(fs::path(out_path, "mrp-full-posterior.csv.gz"))



# Functions ----
#' For various levels of aggregation
mrp_by <- function(data, grp_by = c("year", "iter", "cd")) {

  dt <- data.table(data)
  strat_est_dt <- dt[,
                     list(p_mrp_ggfix = sum(p_mrp_ggfix*N) / sum(N),
                          p_mrp_nofix = sum(p_mrp_nofix*N) / sum(N),
                          p_mrp_twway = sum(p_mrp_twway*N) / sum(N),
                          N = sum(N)),
                     by = grp_by]
  est_df <- as_tibble(strat_est_dt)

  by_var <- setdiff(grp_by, "iter")

  N_cd <- est_df %>%
    group_by(across(all_of(by_var))) %>%
    summarize(N = unique(N),
              .groups = "drop")

  mrp_ggfix <- est_df %>%
    summ_sims(area_var = by_var, est_var = "p_mrp_ggfix") %>%
    rename(p_mrp_ggfix = p_mrp_est) %>%
    select(all_of(c(by_var, "p_mrp_ggfix"))) # no CIs

  mrp_nofix <- est_df %>%
    summ_sims(area_var = by_var, est_var = "p_mrp_nofix") %>%
    rename(p_mrp_nofix = p_mrp_est) %>%
    select(all_of(c(by_var, "p_mrp_nofix"))) # no CIs

  mrp_twway <- est_df %>%
    summ_sims(area_var = by_var, est_var = "p_mrp_twway") %>%
    rename_with(.fn = ~str_replace_all(.x, "p_mrp_", "p_mrp_twway_"), .cols = matches("p_mrp_")) %>%
    rename(p_mrp_twway = p_mrp_twway_est)

  mrp_nofix %>%
    left_join(mrp_ggfix, by = by_var) %>%
    left_join(mrp_twway, by = by_var) %>%
    ungroup() %>%
    left_join(N_cd, by = by_var) %>%
    mutate(year = as.numeric(year)) %>%
    relocate(year, matches("st"), matches("cd"), matches("race"), N, p_mrp_nofix, p_mrp_ggfix, p_mrp_twway)
}


# Supplemental inputs ----
cd_elec <- read_csv("data/input/cd_presvote_2016-2020.csv")
race_dir <- read_rds("data/output/dir_fit_race.rds")


# summarize main MRP -------
areas_race <- mrp_by(strat_race_mrp, grp_by = c("year", "cd", "race", "iter"))

# Combine ------
race_out <- distinct(cd_elec, year, cd, pct_trump) %>%
  left_join(areas_race, by = c("year", "cd")) %>%
  left_join(race_dir, by = c("year", "cd",  "race")) %>%
  relocate(year, cd, pct_trump, matches("white"), race)

# Final csv with covariates ------
race_out  %>% write_csv(path(out_path, "mrp-ests_by-cd-race.csv"))



# alternative aggregations
# areas_cd <- mrp_by(strat_race_mrp, grp_by = c("year", "iter", "cd"))
# areas_race2 <- strat_race_mrp %>%
#   mutate(race2 = recode_factor(race, White = "White", .default = "Non-White")) %>%
#   mrp_by(grp_by = c("year", "iter", "cd", "race2"))
#
# # national
# natl_race <- mrp_by(strat_race_mrp, grp_by = c("year", "iter", "race"))

