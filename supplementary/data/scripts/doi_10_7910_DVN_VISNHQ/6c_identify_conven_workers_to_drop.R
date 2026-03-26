##################################################################################
# Agrawal and Chen (2026)
#
# Descriptions: Identify potential convenience state workers to drop.
# Produces a dataset of migration PUMAs by year with an indicator variable on whether 
# the migration PUMA is within a convenience state and an indicator variable on whether
# the migration PUMA has a high fraction of workers working out-of-state in a convenience state.
# 
##################################################################################
# Clear environment
rm(list = ls())

library(tidyverse)
library(janitor)
library(purrr)
library(rvest)
library(haven)
library(rlang)
library(ipumsr)


## Read in ACS and CPI data ------------------------------------------------

# Read in the ACS data
ddi <- read_ipums_ddi("data/input_raw_ipums/usa_00027.xml")
all <- read_ipums_micro(ddi) |> clean_names()

# Read in migration puma-puma xwalk
migpuma_puma_xwalk <- read_csv("data/xwalk/cleaned_migpuma1_puma_xwalk.csv") 

# impose general restrictions
cleaned_all <- all |> 
  # drop population that's institutionalized
  filter(gq != 3) |>
  # impose general restrictions
  # non-farm
  filter(farm == 1, empstat == 1) |> 
  # subset to those living and working in the 50 states plus DC
  filter((statefip >= 1 & statefip <= 56) & (pwstate2 >= 1 & pwstate2 <= 56)) 

# only keep what I need
cleaned_all_min <- cleaned_all |> 
  select(year, sample, serial, pernum, perwt, statefip, puma, pwstate2) |> 
  mutate(
    # construct puma geo id
    puma_fips = statefip*100000 + puma,
    puma_fips = as.character(puma_fips),
    puma_fips = ifelse(str_count(puma_fips) < 7, paste0("0", puma_fips), puma_fips)
  ) |> 
  # aggregate dest pumas to migpumas
  left_join(migpuma_puma_xwalk, by =c("year", "puma_fips")) |> 
  rename(
    origin_state_fips = statefip
    )

# check num of distinct pumas in each year
cleaned_all_min |> 
  distinct(year, puma_fips) |> 
  group_by(year) |> 
  count()

# check num of distinct migpumas in each year
cleaned_all_min |> 
  distinct(year, migpuma_fips) |> 
  group_by(year) |> 
  count()


# number of workers in each state by dest migpuma by work state cell
n_migpuma_pwstate <- cleaned_all_min |> 
  group_by(year, origin_state_fips, migpuma_fips, pwstate2) |> 
  summarise(
    n = sum(perwt),
    .groups = "drop"
  ) 

# number of workers in each state by dest migpuma 
n_migpuma <- cleaned_all_min |> 
  group_by(year, origin_state_fips, migpuma_fips) |> 
  summarise(
    tot_n = sum(perwt),
    .groups = "drop"
  ) 

# merge and compute flow
flow <- n_migpuma_pwstate |> 
  left_join(n_migpuma,  by = c("year", "origin_state_fips", "migpuma_fips")) |> 
  mutate(
    frac = n / tot_n * 100
  ) 


# Code convenience state dummy -----------------------------------------------------------

states <- all |> distinct(statefip) |> zap_labels() |> pull(statefip)
years <- 2016:2024

ordered_pairs <- expand.grid(origin_state_fips = states, pwstate2 = states) 
year_state_pair <- expand_grid(year = years, ordered_pairs)



# 10 - Delaware
# 31 - Nebraska
# 36 - New York
# 42 - Pennsylvania
convenience_states <- year_state_pair |> 
  mutate(
    convenience_state = case_when(
      # 2016-2019: Delaware, Nebraska, New York, Pennsylvania have full convenience rule
      (year %in% c(2016, 2017, 2018, 2019) & pwstate2 %in% c(10, 31, 36, 42)) ~ 1,
      
      # 2019: Connecticut adopted a retaliatory rule
      (year == 2019 & pwstate2 == 9 & origin_state_fips %in% c(10, 31, 36, 42)) ~ 1,
      
      # 2020: Arkansas (5) and Massachusetts (25) adopted temp. convenience rule
      (year == 2020 & pwstate2 %in% c(10, 31, 36, 42, 5, 25)) ~ 1,
      
      # 2020: retaliatory rule for Connecticut
      (year == 2020 & pwstate2 == 9 & origin_state_fips %in% c(10, 31, 36, 42, 5, 25)) ~ 1,
      
      # 2021: 
      # Arkansas repealed convenience rule, effective for tax years beginning on or after January 1, 2021
      # Temp. rule in Massachusetts was challenged and eventually ended as pandemic measures subsided
      (year == 2021 & pwstate2 %in% c(10, 31, 36, 42)) ~ 1,
      
      # 2021: retaliatory rule for Connecticut
      (year == 2021 & pwstate2 == 9 & origin_state_fips %in% c(10, 31, 36, 42)) ~ 1,
      
      # 2022: same as 2021
      # coding separately for clarity
      (year == 2022 & pwstate2 %in% c(10, 31, 36, 42)) ~ 1,
      (year == 2022 & pwstate2 == 9 & origin_state_fips %in% c(10, 31, 36, 42)) ~ 1,
      
      # 2023 (to present)
      # Alabama (1) adopted convenience rule
      (year >= 2023 & pwstate2 %in% c(10, 31, 36, 42, 1)) ~ 1,
      
      # New Jersey (34) adopted retaliatory rule
      # Connecticut still has retaliatory rule
      (year >= 2023 & pwstate2 %in% c(9, 34) & origin_state_fips %in% c(10, 31, 36, 42, 1)) ~ 1,
      
      .default = 0
    )
  )



# Merge convenience state dummy with flow ---------------------------------

m <- flow |> 
  # join by year-residence state-work state
  left_join(convenience_states, by = c("year", "origin_state_fips", "pwstate2"))

# keep the rows in which the migpuma state is the same as the work state
# this should equal the number of migpumas in each year
same_state <- m |> 
  filter(origin_state_fips == pwstate2) |> 
  select(year, migpuma_fips, convenience_state_itself = convenience_state)

# for rows in which migpuma state is different from the work state 
# only keep the row that has the highest out of state flow
diff_state_maxflow <- m |> 
  filter(origin_state_fips != pwstate2) |> 
  group_by(year, origin_state_fips, migpuma_fips) |>
  # only keep the row that has the highest out of state flow
  slice_max(order_by = frac, n = 1, with_ties = FALSE) |>
  ungroup() |> 
  # set a threshold maybe??
  filter(frac > 0) |> 
  select(year, migpuma_fips, frac_interstate_work = frac, convenience_state_work = convenience_state)
  

final_d <- migpuma_puma_xwalk |> 
  distinct(year, migpuma_fips) |> 
  left_join(same_state) |> 
  left_join(diff_state_maxflow)

# save
write_csv(final_d, "data/derived_ipums/drop_convenience_interstate_commuter_id.csv")

