library(arrow)
library(assertr)
library(here)
library(parallel)
library(parglm)
library(pROC)
library(rcompanion)
library(tidyverse)

samp <- read_feather(here("data/combined_samp_slope.feather")) |> 
  dplyr::select(PARCEL_ADU,
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
         TRT_PROP_VACANT_std = scale(TRT_PROP_VACANT),
         PL_hoa_cp_std = scale(PL_hoa_cp),
         PL_ACS16_5yr_pctoo_std = scale(PL_ACS16_5yr_pctoo),
         PARCEL_STEEPSLOPE = if_else(PARCEL_PROP_STEEPSLOPE != 0, 1, 
                                     PARCEL_PROP_STEEPSLOPE)) |> 
  dplyr::select(-c(PARCEL_sqft, PL_ACS16_5yr_totpop, TRT_rent_i, 
                   PARCEL_PROP_STEEPSLOPE, TRT_PROP_VACANT)) 

cores <- detectCores(logical = FALSE) - 1L

# No FE
parglm(PARCEL_ADU ~
         TRT_rent_i_std +
         TRT_PROP_VACANT_std +
         PARCEL_sqft_std +
         PARCEL_buildings +
         PARCEL_STEEPSLOPE +
         0,
       data = samp,
       family = binomial(),
       control = parglm.control(nthreads = cores)) |> 
  write_rds("data/logit_nofe_par.rds")

gc()

# FE only
parglm(PARCEL_ADU ~
         PL_NAME +
         0,
       data = samp,
       family = binomial(),
       control = parglm.control(nthreads = cores)) |> 
  write_rds("data/logit_feonly_par.rds")

gc()

# Full model
parglm(PARCEL_ADU ~
         TRT_rent_i_std +
         TRT_PROP_VACANT_std +
         PARCEL_sqft_std +
         PARCEL_buildings +
         PARCEL_STEEPSLOPE +
         PL_NAME +
         0,
       data = samp,
       family = binomial(),
       control = parglm.control(nthreads = cores)) |> 
  write_rds("data/logit_full_par.rds")


gc()

logit_feonly_par <- read_rds("data/logit_feonly_par.rds")
logit_nofe_par <- read_rds("data/logit_nofe_par.rds")
logit_full_par <- read_rds("data/logit_full_par.rds")

fits <- compareGLM(logit_nofe_par, logit_feonly_par, logit_full_par)

fits <- fits[["Fit.criteria"]] |> 
  dplyr::select(-c(Df.res, p.value)) |> 
  mutate(Model = paste0("(", row_number(), ")")) |> 
  relocate(Model)


auc <- list(logit_nofe_par, logit_feonly_par, logit_full_par) |> 
  map_dbl(\(x) auc(roc(samp$PARCEL_ADU, predict(x, type = "response")))) |> 
  tibble() |> 
  set_names("AUC")

table_a3 <- bind_cols(fits, auc) |> 
  dplyr::select(-c(Rank, AICc)) |> 
  rename(`Cox-Snell` = Cox.and.Snell,
         `Cragg-Uhler (Nagelkerke)` = Nagelkerke)

dir.create(here::here("writing/japa/tables/"))

write_csv(table_a3, here::here("writing/japa/tables/table_a3.csv"))