library(tidyverse)
library(bmlogit)
library(synthjoint)
library(ccesMRPprep)
library(ccesMRPrun)
library(tictoc)
library(furrr)

st_grp <- read_csv("data/input/states-groupings.csv", show_col_types = FALSE) |>
  select(st, stgroup)

acs_synth <- read_rds("data/output/acs_synth-pop.rds")
cc_svy <- read_rds("data/output/ccc_2016-2020.rds")

cps_tgt <- read_csv("data/input/cd-race_turnout-CPS-estimate.csv", show_col_types = FALSE) |>
  select(year, race, turnout_CPS) |>
  mutate(race = factor(race, levels = c("White", "Black", "Hispanic", "Other")))

# Pop
acs_sel <- acs_synth |>
  select(year, stgroup, cd, race, age, female, educ, count)

# Survey for model
cc_sel <- cc_svy |>
  left_join(st_grp, by = "st") |>
  transmute(year, state, st, stgroup,
            cd, age, race, female,
            educ, pid3_leaner,
            turnout = as.numeric(vv_turnout_gvm == "Voted"))

cd_vap <- read_csv("data/input/cd_turnout-vap.csv", show_col_types = FALSE) |>
  mutate(turnout_vap = highest_office / VAP) |>
  transmute(cd, year, count = turnout_vap) |>
  mutate(st = str_sub(cd, 1, 2)) |>
  left_join(st_grp, by = "st")

vap_long <- bind_rows(
  transmute(cd_vap, stgroup, cd, year, turnout = 0, count = 1 - count),
  transmute(cd_vap, stgroup, cd, year, turnout = 1, count = count)
)



plan(multisession, workers = 6)

# Run
synth_turnout = function(geo, year) {
  out <- map_dfr(
    .x = geo,
    .f = function(s, yr = year) {

      form <- turnout ~ race * age + female + educ

      fit <- synth_bmlogit(
        form,
        microdata = filter(cc_sel, str_detect(stgroup, s), year == yr) |>
          mutate(race = fct_drop(race)),
        fix_to    = filter(vap_long, str_detect(stgroup, s), year == yr),
        poptable  = filter(acs_sel, str_detect(stgroup, s), year == yr) |>
          mutate(race = fct_drop(race)),
        count_var = "count",
        fix_by_area = TRUE,
        area_var = "cd",
        tol = 0.001)

      fit_fmt <- fit |>
        mutate(stgroup = s, year = yr) |>
        relocate(year, stgroup)

      cat(s, " ")
      fit_fmt
    }
  )
  out
}


# run and stack
st_vec <- unique(st_grp$stgroup)


# two-way calib with CPS ----

out_est1 <- list()
out_est2 <- list()
for (yr in c(2016, 2020)) {
  cat(yr, "\n")

  tic()
  cat("bmlogit step...")
  out_est1[[as.character(yr)]] <- synth_turnout(st_vec, year = yr)
  toc()
  cells_est <- out_est1[[as.character(yr)]]

  in_ests <- cells_est |>
    group_by(cd, race) |>
    summarize(est = sum(count*(turnout == 1)) / sum(count),
              n = sum(count),
              .groups = "drop")

  tgt_area <- cells_est |>
    group_by(cd) |>
    summarize(turnout = sum(count*(turnout == 1)) / sum(count),
              N = sum(count)) |>
    arrange(cd)

  tgt_race <- cps_tgt |>
    filter(year == yr)

  tic()
  cat("Calibrating to CPS at national level...")
  set.seed(02138)
  out_est2[[as.character(yr)]] <- calib_twoway(
    data = in_ests,
    var_area = "cd",
    var_group = "race",
    tgt_area = select(tgt_area, cd, turnout) |> deframe(),
    tgt_group = select(tgt_race, race, turnout_CPS) |> deframe(),
    n_area = deframe(count(cells_est, cd, wt = count)),
    n_group = deframe(count(cells_est, race, wt = count)),
    n_total = deframe(count(cells_est, wt = count)),
    use_grad = TRUE
  )
  toc()
}




# Stack ----
syn_delta <- bind_rows(out_est2, .id = "year") |>
  transmute(year = as.numeric(year), cd, race, delta) |>
  ungroup()

syn_tr <- bind_rows(out_est1) |>
  filter(turnout == "1") |>
  select(-prXZ, -prX) |>
  left_join(syn_delta, by = c("year", "cd", "race")) |>
  mutate(denominator = count / prZ_givenX) |>
  mutate(count_twway = denominator*plogis(qlogis(prZ_givenX) + delta)) |>
  select(-delta) |>
  rename(count_calib = count)

syn_tr <- bind_rows(
  syn_tr,
  mutate(syn_tr,
         turnout = "0",
         count_calib = denominator - count_calib,
         count_twway = denominator - count_twway)
) |>
  mutate(turnout = as.numeric(turnout))


# add back CD pct
turnout_bm <- distinct(acs_synth, year, cd) |>
  inner_join(syn_tr, by = c("year", "cd")) |>
  mutate(st = str_sub(cd, 1, 2)) |>
  mutate(
    race_Black    = as.integer(race == "Black"),
    race_Hispanic = as.integer(race == "Hispanic"),
    race_Other    = as.integer(race == "Other"),
  ) |>
  relocate(year, st)

# summary stats
by_cd_frac <- turnout_bm |>
  filter(turnout == 1) |>
  group_by(year, cd) |>
  summarize(
    pct_white_elec = sum(count_twway*(race == "White")) / sum(count_twway),
    pct_nonwhite_elec = sum(count_twway*(race != "White")) / sum(count_twway),
    pct_black_elec = sum(count_twway*(race == "Black")) / sum(count_twway),
    pct_hispanic_elec = sum(count_twway*(race == "Hispanic")) / sum(count_twway),
    pct_raceother_elec = sum(count_twway*(race == "Other")) / sum(count_twway)
  )


write_rds(turnout_bm, "data/output/acs_synth-turnout.rds")
write_csv(by_cd_frac, "data/output/by-cd_pct-race_elec.csv")
