library(ccesMRPprep)
library(synthjoint)
library(tidyverse)
library(furrr)
library(tictoc)

st_grp <- read_csv("data/input/states-groupings.csv") |>
  select(st, stgroup)

# microdata
cc_svy <- read_rds("data/output/ccc_2016-2020.rds") |>
  left_join(st_grp, by = "st")


# PA exceptions
raw_ra16_PA <- get_acs_cces(acscodes_age_sex_race, states = "PA", year = 2017, dataset = "acs5")

# pop
raw_ed16 <- get_acs_cces(acscodes_age_sex_educ, year = 2016, dataset = "acs1")

# for 2020, take average of 2019 and 2021 because the 2020 is only avaialble as experimental
raw_ed19 <- get_acs_cces(acscodes_age_sex_educ, year = 2019, dataset = "acs1")
raw_ed21 <- get_acs_cces(acscodes_age_sex_educ, year = 2021, dataset = "acs1")
raw_ed20 <- left_join(raw_ed19, raw_ed21,
                      by = c("acscode", "cd", "gender", "female", "educ", "race", "age")) |>
  mutate(count = as.integer(round((count.x + count.y)/2))) |>
  select(-matches("\\.x|\\.y"))

stopifnot(nrow(raw_ed20) == nrow(raw_ed19))
stopifnot(any(is.na(raw_ed20)))

edu_tgt <- bind_rows(`2016` = raw_ed16,
                     `2020` = raw_ed20,
                     .id = "year") |>
  count(year, cd, educ, wt = count, name = "count") |>
  mutate(st = str_sub(cd, 1, 2),
         year = as.numeric(year)) |>
  left_join(st_grp, by = "st")

# some small races have 0s with acs1, e.g. WY-01 Black
raw_ra16 <- get_acs_cces(acscodes_age_sex_race, year = 2018, dataset = "acs5")
raw_ra20 <- get_acs_cces(acscodes_age_sex_race, year = 2021, dataset = "acs5")

raw_ra <-  bind_rows(`2016` = filter(raw_ra16, !str_detect(cd, "PA")),
                     `2016` = raw_ra16_PA,
                     `2020` = mutate(raw_ra20, year = 2020),
                     .id = "year") |>
  mutate(st = str_sub(cd, 1, 2)) |>
  mutate(
    race_orig = race,
    race = fct_collapse(race, `Other` = c("Native American", "Asian", "All Other")),
    race = fct_relevel(race, "White", "Black", "Hispanic", "Other"),
    year = as.numeric(year)
  ) |>
  select(-race_orig) |>
  left_join(st_grp, by = "st")

# elec
plan(multisession, workers = 6)

# synthetic joint, state by state
synth_ed = function(geo, year) {
  tic() # about 25 min
  out <- map_dfr(
    .x = geo,
    .f = function(s, yr = year) {
      set.seed(02138)

      form <- educ ~ race * age + female

      fit <- synth_bmlogit(
        form,
        microdata = filter(cc_svy, str_detect(stgroup, s), year == yr) |>
          mutate(race = fct_drop(race)),
        fix_to    = filter(edu_tgt, str_detect(stgroup, s), year == yr),
        poptable  = filter(raw_ra, str_detect(stgroup, s), year == yr) |>
          mutate(race = fct_drop(race)),
        fix_by_area = TRUE,
        count_var = "count",
        area_var = "cd",
        tol = 0.01)

      fit_fmt <- fit |>
        mutate(stgroup = s, year = yr) |>
        relocate(year, stgroup)
      cat(s, " ")
      fit_fmt
    }
  )
  toc()
  out
}


# run the function
st_vec <- unique(st_grp$stgroup)
syn_ed16 <- synth_ed(st_vec, year = 2016) # about 30 min
syn_ed20 <- synth_ed(st_vec, year = 2020)

# 40 min total

bind_rows(
  syn_ed16,
  syn_ed20
) |>
  select(-matches("pr(X|Z)")) |>
  write_rds("data/output/acs_synth-pop.rds")
