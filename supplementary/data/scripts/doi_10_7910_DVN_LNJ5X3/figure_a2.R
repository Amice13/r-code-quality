library(arrow)
library(assertr)
library(extrafont)
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

rm(samp_re)

pooled <- read_rds("data/pooled_mods_log_slope.rds")

figure_a2 <- ggpredict(pooled[[2]], 
                       terms = "TRT_rent_i_qtile",
                       condition = c(PARCEL_STEEPSLOPE = med_slope,
                                     PARCEL_buildings = med_structures)) |> 
  ggplot(aes(x = x, y = predicted)) +
  geom_point() +
  geom_errorbar(aes(ymin = conf.low, ymax = conf.high), width = 0.2) +
  ylab("p(ADU)") +
  xlab("Rent quintile") +
  theme_bw() +
  theme(text = element_text(family = "serif", size = 10))

dir.create("writing/japa/figures/")

figure_a2  

ggsave(here::here("writing/japa/figures/figure_a2.png"))