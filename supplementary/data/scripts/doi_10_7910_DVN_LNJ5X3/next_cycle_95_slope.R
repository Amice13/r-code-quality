library("arrow")
library("assertr")
library("doParallel")
library("here")
library("lme4")
library("merTools")
library("statar")
library("tidyverse")


# Read model & data -------------------------------------------------------

model <- read_rds(here::here("data/pooled_mods_log_slope.rds"))
model <- model[[2]]


no_adu <- read_feather(here("data/combined_samp_slope.feather")) |> 
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
  dplyr::filter(PARCEL_ADU == 0) |> 
  dplyr::select(-c(PARCEL_sqft, PL_ACS16_5yr_totpop, TRT_rent_i, PARCEL_PROP_STEEPSLOPE)) 


# Point estimate ----------------------------------------------------------
predicted_ADUs <- stats::predict(model, 
                                 newdata = no_adu, 
                                 type = "response") %>% 
  bind_cols(no_adu %>% dplyr::select(PL_NAME)) %>% 
  rename(predicted_ADUs = `...1`) %>% 
  group_by(PL_NAME) %>%
  # multiply by 2 b/c prediction is over a 4-year interval
  summarise(predicted_ADUs = round(2*sum(predicted_ADUs), digits = 0)) %>% 
  ungroup()


# Prediction intervals ----------------------------------------------------

no_adu <- no_adu %>%
  mutate(PL_NAME = as.character(PL_NAME))

registerDoParallel(cores=4)

simulated_ADUs <- no_adu %>% 
  split(no_adu$PL_NAME) %>% 
  map(.f = \(df) predictInterval(model,
                                 newdata = as.data.frame(df),
                                 level = 0.95,
                                 n.sims = 500,
                                 stat = "median",
                                 type = "probability",
                                 seed = 2023,
                                 .parallel = TRUE)) %>% 
  list_rbind(names_to = "PL_NAME") %>% 
  group_by(PL_NAME) %>%
  summarise(across(c(fit, upr, lwr), 
                   ~ round(2*sum(.x), 
                           digits = 0))) %>% 
  ungroup() %>% 
  rename(simulated_ADUs_med = fit,
         simulated_ADUs_05 = lwr,
         simulated_ADUs_95 = upr)

rm(no_adu, model)

next_cycle <- statar::join(predicted_ADUs, simulated_ADUs,
                           on = "PL_NAME",
                           kind = "full",
                           check = 1 ~ 1, 
                           gen = "merge") %>% 
  assertr::verify(merge == 3) %>% 
  dplyr::select(-merge)

rm(predicted_ADUs, simulated_ADUs)

next_cycle %>% write_csv(here::here("data/next_cycle_95_slope.csv"))