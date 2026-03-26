library(tidyverse)
library(ccesMRPprep)
stopifnot(packageVersion("ccesMRPprep") >= "0.1.8.900")
library(haven)

library(fs)


dv_download_path <- "data/input/ccc_2016-2020_dataverse-original.rds"
if (file_exists(dv_download_path)) {
 cc_16_20_full <- read_rds(dv_download_path)
} else {
  cc_16_20_full <- get_cces_dataverse("cumulative") |>
    filter(year %in% c(2016, 2020))

  if (dir_exists(path_dir(dv_download_path)))
    write_rds(write_rds(dv_download_path))

}


cc_df <-
  cc_16_20_full |>
  ccc_std_demographics(bh_as_hisp = TRUE, wh_as_hisp = TRUE) |>
  mutate(female = as.integer(gender == 2)) |>
  select(year, case_id, state, cd,
         zipcode, county_fips,
         matches("weight"),
         age, gender, race, hispanic, educ, female,
         matches("pid"),
         matches("_pres_party"),
         matches("_pres_16"),
         matches("_pres_12"),
         vv_turnout_gvm,
         vv_party_gen) |>
  mutate(across(where(is.labelled), as_factor)) |>
  mutate(
    st = str_sub(cd, 1, 2),
    race_orig = race,
    race = fct_collapse(race, `Other` = c("Native American", "Asian", "All Other")),
    race = fct_relevel(race, "White", "Black", "Hispanic", "Other"),
    race_Black = as.integer(race == "Black"),
    race_Hispanic = as.integer(race == "Hispanic"),
    race_Other = as.integer(race == "Other"),
    pid3_leaner = replace_na(as.character(pid3_leaner), "Missing"),
    pid3_leaner = recode_factor(
      pid3_leaner,
       `Democrat (Including Leaners)` = "D",
       `Republican (Including Leaners)` = "R",
      .default = "Other"),
    vv_party = fct_recode(vv_party_gen, D = "Democratic Party", R = "Republican Party"),
    vv_party = fct_other(vv_party, keep = c("D", "R")),
    vv_party_R = as.integer(vv_party == "R"),
    vv_party_D = as.integer(vv_party == "D")
  )

# SUBSET to people who voted and only for one pres party
vv_2pty <- cc_df |>
  filter(vv_turnout_gvm == "Voted") |>
  filter(voted_pres_party %in% c("Democratic", "Republican")) |>
  mutate(trump = voted_pres_party == "Republican")


# save for output
write_rds(vv_2pty, "data/output/ccc_2016-2020_voted_2pty.rds")
write_rds(cc_df, "data/output/ccc_2016-2020.rds")
