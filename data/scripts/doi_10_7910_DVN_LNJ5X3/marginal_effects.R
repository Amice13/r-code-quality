library(arrow)
library(assertr)
library(here)
library(ggeffects)
library(tidyverse)

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

med_slope <- samp_re |> 
  summarise(median(PARCEL_STEEPSLOPE)) |> 
  pull()

med_structures <- samp_re |> 
  summarise(median(PARCEL_buildings )) |> 
  pull()

med_rent_qtile <- samp_re |> 
  summarise(median(as.numeric(TRT_rent_i_qtile ))) |> 
  pull()

rm(samp_re)

pooled <- read_rds("data/pooled_mods_log_slope.rds")

PARCEL_sqft_std <- ggpredict(pooled[[2]], 
                             terms = "PARCEL_sqft_std [meansd]",
                             condition = c(PARCEL_STEEPSLOPE = med_slope,
                                           PARCEL_buildings = med_structures,
                                           TRT_rent_i_qtile = med_rent_qtile))

# Parcel size
PARCEL_sqft_std

# In pooled model (2), a standard deviation increase in lot size from the sample
# mean is associated with an increase in the probability that a parcel receives
# an ADU from 0.003 to 0.004...

PARCEL_sqft_std |> 
  dplyr::filter(x == 0) |> 
  verify(round(predicted, 3) == 0.003)

PARCEL_sqft_std |> 
  dplyr::filter(x == 1) |> 
  verify(round(predicted, 3) == 0.004)

rm(PARCEL_sqft_std)

# ... as is an increase from one to two structures on a parcel
PARCEL_buildings <- ggpredict(pooled[[2]], 
                              terms = "PARCEL_buildings",
                              condition = c(PARCEL_STEEPSLOPE = med_slope,
                                            TRT_rent_i_qtile = med_rent_qtile))

PARCEL_buildings

PARCEL_buildings |> 
  dplyr::filter(x == 1) |> 
  verify(round(predicted, 3) == 0.003)

PARCEL_buildings |> 
  dplyr::filter(x == 2) |> 
  verify(round(predicted, 3) == 0.004)

rm(PARCEL_buildings)

# A standard deviation decrease in the proportion of vacant land in a tract is
# associated with an increase in the probability that a parcel receives an ADU
# from 0.003 to 0.004.

TRT_prop_vacant_std <- ggpredict(pooled[[2]], 
                                 terms = "TRT_prop_vacant_std [meansd]",
                                 condition = c(PARCEL_STEEPSLOPE = med_slope,
                                               PARCEL_buildings = med_structures,
                                               TRT_rent_i_qtile = med_rent_qtile))
TRT_prop_vacant_std


TRT_prop_vacant_std |> 
  dplyr::filter(x == 0) |> 
  verify(round(predicted, 3) == 0.003)

TRT_prop_vacant_std |> 
  dplyr::filter(x == -1) |> 
  verify(round(predicted, 3) == 0.004)

# The presence of steeply sloped terrain decreases the probability that a parcel
# receives an ADU from 0.003 to 0.002.

PARCEL_STEEPSLOPE <- ggpredict(pooled[[2]], 
                             terms = "PARCEL_STEEPSLOPE",
                             condition = c(PARCEL_buildings = med_structures,
                                           TRT_rent_i_qtile = med_rent_qtile))
PARCEL_STEEPSLOPE

PARCEL_STEEPSLOPE |> 
  dplyr::filter(x == 0) |> 
  verify(round(predicted, 3) == 0.003)

PARCEL_STEEPSLOPE |> 
  dplyr::filter(x == 1) |> 
  verify(round(predicted, 3) == 0.002)

# On average, tract-level gross rents are negatively related to ADU permitting –
# a standard deviation increase in median rent (i.e., $567) is associated with a
# small decrease in the probability that a parcel receives an ADU

med_gr <- ggpredict(pooled[[1]], 
                    terms = "TRT_rent_i_std",
                    condition = c(PARCEL_buildings = med_structures,
                                  PARCEL_STEEPSLOPE = med_slope))

med_gr

med_gr |> 
  dplyr::filter(x == 0) |> 
  verify(round(predicted, 4) == 0.0031)

med_gr |> 
  dplyr::filter(x == 1) |> 
  verify(round(predicted, 4) == 0.0027)

rm(med_gr)

# In the pooled model, the odds of an ADU being permitted increase from 0.003
# for parcels in first quintile tracts (with median rents of $276 - $1,230) to
# 0.004 in second quintile tracts (with median rents of $1,231 - $1,483).

med_gr_qtile <- ggpredict(pooled[[2]], 
                             terms = "TRT_rent_i_qtile",
                             condition = c(PARCEL_STEEPSLOPE = med_slope,
                                           PARCEL_buildings = med_structures))
med_gr_qtile |> 
  dplyr::filter(x == 1) |> 
  verify(round(predicted, 3) == 0.003)

med_gr_qtile |> 
  dplyr::filter(x == 2) |> 
  verify(round(predicted, 3) == 0.004)

# In pooled model (2), a standard deviation increase in HOA intensity (13.3
# percentage points) from the mean decreases the probability that a parcel
# receives an ADU from 0.003 to 0.002.

PL_hoa_cp_std <- ggpredict(pooled[[2]], 
                           terms = "PL_hoa_cp_std [meansd]",
                           condition = c(PARCEL_STEEPSLOPE = med_slope,
                                         PARCEL_buildings = med_structures,
                                         TRT_rent_i_qtile = med_rent_qtile))

PL_hoa_cp_std |> 
  dplyr::filter(x == 0) |> 
  verify(round(predicted, 3) == 0.003)

PL_hoa_cp_std |> 
  dplyr::filter(x == 1) |> 
  verify(round(predicted, 3) == 0.002)
