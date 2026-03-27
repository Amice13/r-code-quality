# Ecological inference (EI) estimation:
#
# This script uses cleaned precinct-level data on voter demographics and 2020
# Presidential election returns from ALARM for EI estimation of group and
# district level candidate preferences.
#
# Estimates for each district are then exported individually, and into
# two separate files: one that contains all 'draws' of candidate preference
# estimates from the posterior distribution ('ei-cd-draw_'), and one that
# takes the mean of these posterior draws ('ei-cd-summ_').
#
# Create a directory that is specified in the {outdir} variable to save files properly.

library(tidyverse)
library(tictoc)
library(glue)
Sys.setenv("R_MAX_VSIZE" = 32000000000)
library(eiCompare)



# Import cleaned ALARM data ----
# 2010 CD identifiers, 2020 trump vote, 2020 demographics  (`prepare/alarm/03...R`)

dfcd_alarm <- read_csv("data/vtd-cd10-2020_pres-16-20.csv.gz") |>
  arrange(cd) |>
  filter(!is.na(cd_2010))


# EI estimation ----
outdir <- "data/intermediate/VAP-EI-CD/"

for (j in unique(dfcd_alarm$cd)) {
  if (fs::file_exists(glue("{outdir}/ei-cd-draw_{j}.csv.gz"))) {
    next
  }

  cat("\n\n", j, "\n")

  df_group <- dfcd_alarm |>
    filter(cd == j) |>
    filter(vap > 0, !is.na(pres2_20_R))

  # EI
  tic()
  mod_ei <- ei_rxc(
    data = df_group,
    cand_cols = c("pres2_20_R", "pres2_20_D"),
    race_cols = c("vap_white_frac", "vap_black_frac", "vap_hisp_frac", "vap_other_frac"),
    totals_col = "vap",
    samples = 0.5e4, burnin = 5e3, verbose = TRUE
  )

  toc()
  gc()

  # Posterior draws
  param_mat <- mod_ei$stat_objects[[1]]$draws$Beta
  ests_by_iter <- mod_ei$district_samples |>
    as_tibble() |>
    select(matches("_R_")) |>
    mutate(iter = 1:n()) |>
    pivot_longer(
      -iter,
      names_to = "race",
      values_to = "pres2_20_R",
      names_pattern = "pres2_20_R_vap_(.*)_frac"
    ) |>
    mutate(cd = j, .before = 1)

  # Mean of posterior draws
  tbl_means <-
    tibble(cd = j, as_tibble(mod_ei$estimates)) |>
    rename(
      ei_mean = mean,
      ei_sd = sd,
      ei_ci_95_lower = ci_95_lower,
      ei_ci_95_upper = ci_95_upper
    ) |>
    mutate(
      ei_nsamples = nrow(param_mat),
      nprecincts = ncol(param_mat) / n()
    )

  # Save CD-level output
  write_csv(ests_by_iter, glue("ei-cd-draw_{j}.csv.gz"))
  write_rds(tbl_means, glue("{outdir}/ei-cd-summ_{j}.rds"))
}


# Stack together -----
ei_obj <- dir_ls(outdir, type = "file", regexp = "ei-cd-summ")
names(ei_obj) <- str_remove(str_sub(ei_obj, -9, -1), "\\.rds") # regex to extract CD

ei_stacked <- map_dfr(
    .x = ei_obj,
    .f = ~ read_rds(.x),
    .id = "cd"
) |>
    mutate(
        ei_se = ei_sd / sqrt(ei_nsamples),
        race = str_to_title(gsub("^vap_|_frac$", "", race)),
        race = ifelse(race == "Hisp", "Hispanic", race)
    ) |>
    select(
        matches("cd"), cand, race,
        ei_mean, ei_sd, ei_se,
        matches("ei_ci"), ei_nsamples, nprecincts
    )


# Add homogeneous variable ------
homog <- dfcd_alarm |>
    group_by(cd, GEOID20) |>
    # Long format for merging
    pivot_longer(
        cols = matches("^vap_.*frac$"),
        names_to = "race",
        values_to = "frac_race_precinct"
    ) |>
    # Homogeneous indicators and summarizing
    mutate(
        race = str_to_title(gsub("vap_|_frac", "", race)),
        race = ifelse(race == "Hisp", "Hispanic", race),
        homog = ifelse(frac_race_precinct >= .9, 1, 0)
    ) |>
    filter(race %in% c("White", "Black", "Hispanic", "Other")) |>
    group_by(cd, race) |>
    summarize(
        n_homog = sum(homog, na.rm = TRUE),
        pct_homog = n_homog / n(),
        .groups = "drop"
    )

ei_df <- ei_stacked |>
    inner_join(homog, by = c("cd", "race"))


# Save stacked -----
ei_df |>
    write_csv("data/by-cd-race_EI-ests.csv")
