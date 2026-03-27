library(arrow)
library(here)
library(tidyverse)

# Read in data ------------------------------------------------------------

samp <- read_feather(here("data/combined_samp_slope.feather")) |> 
  mutate(PARCEL_ADU_F_2018 = if_else(PARCEL_ADU_2018 == 1, 1, 0),
         PARCEL_ADU_F_2019 = if_else(PARCEL_ADU_2019 == 1 & 
                                       PARCEL_ADU_F_2018 == 0, 1, 0),
         PARCEL_ADU_F_2020 = if_else(PARCEL_ADU_2020 == 1 & 
                                       PARCEL_ADU_F_2019 == 0 & 
                                       PARCEL_ADU_F_2018 == 0, 1, 0),
         PARCEL_ADU_F_2021 = if_else(PARCEL_ADU_2021 == 1 & 
                                       PARCEL_ADU_F_2020 == 0 & 
                                       PARCEL_ADU_F_2019 == 0 &
                                       PARCEL_ADU_F_2018 == 0, 1, 0)) |> 
  dplyr::select(PARCEL_ADU,
                starts_with("PARCEL_ADU_F"), 
                PARCEL_buildings,
                PARCEL_sqft,
                PARCEL_PROP_STEEPSLOPE,
                PL_ACS16_5yr_totpop,
                PL_ACS16_5yr_pctoo,
                PL_hoa_cp,
                TRT_rent_i,
                TRT_PROP_VACANT,
                PL_NAME,
                PARCEL_mtg,
                PARCEL_ID) |> 
  dplyr::filter(PARCEL_sqft > 1000) |> 
  mutate(TRT_rent_i_qtile = cut(TRT_rent_i,
                                quantile(TRT_rent_i, 
                                         probs = seq(0, 1, 0.2)),
                                labels = seq(1:(length(seq(0, 1, 0.2)) - 1)), 
                                include.lowest = TRUE),
         PL_NAME = as.factor(PL_NAME),
         PARCEL_sqft_std = scale(PARCEL_sqft),
         PL_ACS16_5yr_totpop_log_std = scale(log(PL_ACS16_5yr_totpop)),
         TRT_rent_i_std = scale(TRT_rent_i),
         TRT_prop_vacant_std = scale(TRT_PROP_VACANT),
         PL_hoa_cp_std = scale(PL_hoa_cp),
         PL_ACS16_5yr_pctoo_std = scale(PL_ACS16_5yr_pctoo),
         PARCEL_STEEPSLOPE = if_else(PARCEL_PROP_STEEPSLOPE != 0, 1, PARCEL_PROP_STEEPSLOPE))


# Generate table ----------------------------------------------------------

continuous <- samp |> 
  dplyr::select(PARCEL_sqft, PARCEL_buildings, TRT_PROP_VACANT, TRT_rent_i,
                PL_ACS16_5yr_totpop, PL_ACS16_5yr_pctoo, PL_hoa_cp) |> 
  summarise(across(.cols = everything(), 
                   .fns = list(N = ~ sum(!is.na(.)), 
                               Mean = mean, 
                               SD = sd, 
                               Min = min, 
                               Max = max))) |> 
  pivot_longer(everything(), 
               names_to = c("Variable", ".value"),
               names_pattern = "^(.*)_([^_]*)$")


dichotomous <- samp |> 
  dplyr::select(PARCEL_ADU,
                starts_with("PARCEL_ADU_F"),
                PARCEL_STEEPSLOPE) |> 
  summarise(across(.cols = everything(), sum)) |> 
  pivot_longer(everything(), 
               names_to = "Variable", 
               values_to = "N")


dir.create(here::here("writing/japa/"))

bind_rows(dichotomous, continuous) |> 
  mutate(order = case_when(
    Variable == "PARCEL_ADU" ~ 5,
    Variable == "PARCEL_ADU_F_2018"    ~ 1,
    Variable == "PARCEL_ADU_F_2019"    ~ 2,
    Variable == "PARCEL_ADU_F_2020"    ~ 3,
    Variable == "PARCEL_ADU_F_2021"   ~ 4,
    Variable == "PARCEL_STEEPSLOPE" ~ 6,
    Variable == "PARCEL_sqft" ~ 7,
    Variable == "PARCEL_buildings" ~ 8,
    Variable == "TRT_PROP_VACANT" ~ 9,
    Variable == "TRT_rent_i"          ~ 10,
    Variable == "PL_ACS16_5yr_totpop"  ~ 11,
    Variable == "PL_ACS16_5yr_pctoo"   ~ 12,
    Variable == "PL_hoa_cp" ~ 13
  )) |> 
  arrange(order) |> 
  mutate(order = paste0("(", as.character(order), ")")) |> 
  relocate(order) |> 
  mutate(Variable = case_when(
    Variable == "PARCEL_ADU" ~ "ADU Parcel (2018-2021)",
    Variable == "PARCEL_ADU_F_2018" ~ "ADU Parcel (2018)",
    Variable == "PARCEL_ADU_F_2019" ~ "ADU Parcel (2019)",
    Variable == "PARCEL_ADU_F_2020" ~ "ADU Parcel (2020)",
    Variable == "PARCEL_ADU_F_2021" ~ "ADU Parcel (2021)",
    Variable == "PARCEL_STEEPSLOPE"~ "Parcel contains steep slope",
    Variable == "PARCEL_sqft"~ "Parcel sq. ft. (2016)",
    Variable == "PARCEL_buildings"~ "Structures on parcel (N)",
    Variable == "TRT_PROP_VACANT"~ "Vacant land as proportion of tract land area (2011)",
    Variable == "TRT_rent_i" ~ "Tract median gross rent ($) (2012-2016)",
    Variable == "PL_ACS16_5yr_totpop" ~ "City population (N) (2012-2016)",
    Variable == "PL_ACS16_5yr_pctoo"  ~ "City owner-occupied residences (%) (2012-2016)",
    Variable == "PL_hoa_cp"~ "City HOA intensity (%) (2016)"
  )) |> 
  write_csv("writing/japa/table_a1.csv")