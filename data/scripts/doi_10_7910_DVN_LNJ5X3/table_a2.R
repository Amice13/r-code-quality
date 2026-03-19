library(arrow)
library(assertr)
library(estimatr)
library(gt)
library(here)
library(modelsummary)
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

fe_r <- lm_robust(PARCEL_ADU ~
                    TRT_rent_i_std +
                    TRT_PROP_VACANT_std +
                    PARCEL_sqft_std +
                    PARCEL_buildings +
                    PARCEL_STEEPSLOPE +
                    PL_NAME +
                    0, data = samp)

labels <- c(PARCEL_sqft_std = "Parcel sq. ft. (std)",
            PARCEL_buildings = "Structures on parcel",
            PARCEL_STEEPSLOPE = "Parcel contains steep slope",
            TRT_rent_i_std = "Tract median gross rent (std)",
            TRT_PROP_VACANT_std = "Tract proportion vacant (std)")

f <- function(x) formatC(x, digits = 0, big.mark = ",", format = "f")

modelsummary(fe_r, 
             output = "gt",
             stars =  c( "*" = .05, "**" = .01, "***" = .001), 
             fmt = fmt_statistic(nobs = f, std.error = 4),
             coef_map = labels, 
             gof_map = c("nobs"),
             notes = "Model includes city fixed effects; Standard errors in parentheses."
) |> 
  opt_table_font(stack = "old-style")
