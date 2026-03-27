library(arrow)
library(assertr)
library(conflicted)
library(here)
library(lme4)
library(tidyverse)


# Read in data ------------------------------------------------------------

samp_re <- read_feather(here("data/combined_samp_slope.feather")) |> 
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
         PARCEL_STEEPSLOPE = if_else(PARCEL_PROP_STEEPSLOPE != 0, 1, PARCEL_PROP_STEEPSLOPE)) |> 
  dplyr::select(-c(PARCEL_sqft, PL_ACS16_5yr_totpop, TRT_rent_i, PARCEL_PROP_STEEPSLOPE)) 

# Model specifications ----------------------------------------------------

m1 <- paste0(c("(1 | PL_NAME)", 
               "TRT_rent_i_std",
               "TRT_prop_vacant_std",
               "PL_ACS16_5yr_totpop_log_std",
               "PL_ACS16_5yr_pctoo_std",
               "PARCEL_sqft_std",
               "PARCEL_buildings",
               "PARCEL_STEEPSLOPE",
               "PL_hoa_cp_std"), 
             collapse = " + ")

m2 <- paste0(c("(1 | PL_NAME)", 
               "TRT_rent_i_qtile",
               "TRT_prop_vacant_std",
               "PL_ACS16_5yr_totpop_log_std",
               "PL_ACS16_5yr_pctoo_std",
               "PARCEL_sqft_std",
               "PARCEL_buildings",
               "PARCEL_STEEPSLOPE",
               "PL_hoa_cp_std"), 
             collapse = " + ")

# Pooled models -----------------------------------------------------------

paste0("PARCEL_ADU ~ ", c(m1, m2)) %>%
  map(as.formula) %>%
  map(lme4::glmer,
      data = samp_re,
      family = binomial(link = "logit"),
      control = glmerControl(optimizer = "bobyqa")) %>% 
  write_rds("data/pooled_mods_log_slope.rds")

# Annual models -----------------------------------------------------------

paste0("PARCEL_ADU_F_2018 ~ ", c(m1, m2)) %>%
  map(as.formula) %>%
  map(lme4::glmer,
      data = samp_re,
      family = binomial(link = "logit"),
      control = glmerControl(optimizer = "bobyqa")) %>% 
  write_rds("data/y2018_mods_log_slope.rds")

paste0("PARCEL_ADU_F_2019 ~ ", c(m1, m2)) %>% 
  map(as.formula) %>% 
  map(lme4::glmer,
      data = samp_re %>% dplyr::filter(PARCEL_ADU_F_2018 != 1),
      family = binomial(link = "logit"),
      control = glmerControl(optimizer = "bobyqa")) %>% 
  write_rds("data/y2019_mods_log_slope.rds")

paste0("PARCEL_ADU_F_2020 ~ ", c(m1, m2)) %>% 
  map(as.formula) %>% 
  map(lme4::glmer,
      data = samp_re %>% dplyr::filter(PARCEL_ADU_F_2018 != 1 &
                                       PARCEL_ADU_F_2019 != 1),
      family = binomial(link = "logit"),
      control = glmerControl(optimizer = "bobyqa")) %>% 
  write_rds("data/y2020_mods_log_slope.rds")

paste0("PARCEL_ADU_F_2021 ~ ", c(m1, m2)) %>% 
  map(as.formula) %>% 
  map(lme4::glmer,
      data = samp_re %>% dplyr::filter(PARCEL_ADU_F_2018 != 1 &
                                       PARCEL_ADU_F_2019 != 1 &
                                       PARCEL_ADU_F_2020 != 1),
      family = binomial(link = "logit"),
      control = glmerControl(optimizer = "bobyqa")) %>% 
  write_rds("data/y2021_mods_log_slope.rds")