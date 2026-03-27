##################################################################################
# Agrawal and Chen (2026)
#
# Descriptions: Construct migpuma1 and puma crosswalk
# 
##################################################################################

# Clear environment
rm(list = ls()) 

library(tidyverse)
library(janitor)
library(haven)
library(readxl)

# Read in the Census migpuma1 and puma xwalk for different years ----------
xwalk_2010 <- read_excel("data/xwalk/puma_migpuma1_pwpuma00_2010.xls", skip = 2) |> clean_names()

xwalk_2020 <- read_excel("data/xwalk/puma_migpuma1_pwpuma00_2020.xls") |> clean_names()

# join together the state fips and puma/migpuma fips to form on ID variable
# each one should have 7 characters
cleaned_xwalk_2010 <- xwalk_2010 |> 
  mutate(
    migpuma_fips = paste0(state_of_residence_st, pwpuma00_or_migpuma1),
    puma_fips = paste0(state_of_residence_st, puma),
  ) |> 
  select(migpuma_fips, puma_fips)
  

cleaned_xwalk_2020 <- xwalk_2020 |> 
  mutate(
    migpuma_fips = paste0(state_of_residence_st, pwpuma00_or_migpuma1),
    puma_fips = paste0(state_of_residence_st, puma),
  ) |> 
  select(migpuma_fips, puma_fips)


# Define year ranges for the two xwalks
# 1) 2012-2021 ACS/PRCS Migration PUMAs (2010 Definitions)
# 2) 2022-Onward ACS/PRCS Migration PUMAs (2020 Definitions)
years_2010 <- 2016:2021
years_2020 <- 2022:2024

# Replicate cleaned_xwalk_2010 for 2016–2021
cleaned_xwalk_2010_all <- lapply(years_2010, function(y) {
  cleaned_xwalk_2010 |> mutate(year = y)
}) |> 
  bind_rows()

# Replicate cleaned_xwalk_2020 for 2022–2023
cleaned_xwalk_2020_all <- lapply(years_2020, function(y) {
  cleaned_xwalk_2020 |> mutate(year = y)
}) |> 
  bind_rows()

# Combine them into one unified crosswalk
cleaned_xwalk_all <- bind_rows(cleaned_xwalk_2010_all, cleaned_xwalk_2020_all)


# save 
write_csv(cleaned_xwalk_all, "data/xwalk/cleaned_migpuma1_puma_xwalk.csv")
