library(arrow)
library(assertr)
library(estimatr)
library(extrafont)
library(here)
library(gtsummary)
library(tidyverse)

loadfonts(device = "win")

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

pl2county <- arrow::read_feather(here::here("data/combined_samp_slope.feather")) |> 
  distinct(PL_NAME, PARCEL_COUNTY) |> 
  assertr::verify(anyDuplicated(PL_NAME) == FALSE) |> 
  rename(term = PL_NAME,
         county = PARCEL_COUNTY)

# Calculate location quotients
lq <- samp |> 
  mutate(adus_state = sum(PARCEL_ADU), 
         tot_state = n(),
         sc = adus_state/tot_state) |> 
  group_by(PL_NAME) |> 
  mutate(adus_local = sum(PARCEL_ADU),
         tot_local = n(), 
         lc = adus_local/tot_local) |> 
  ungroup() |> 
  mutate(lq = (lc/sc)) |> 
  dplyr::select(PL_NAME, lq) |> 
  rename(term = PL_NAME) |>
  distinct()

fe_r_df <- broom::tidy(fe_r, conf.int = TRUE, conf.level = 0.95) |> 
  dplyr::filter(str_detect(term, "^PL_NAME")) |> 
  dplyr::mutate(term = str_replace(term, "PL_NAME", ""))  |> 
  statar::join(pl2county, on = "term", kind = "full", check = 1 ~ 1, gen = "merge") |> 
  assertr::verify(merge == 3) |> 
  dplyr::select(-merge) |> 
  statar::join(lq, on = "term", kind = "full", check = 1 ~ 1, gen = "merge") |>
  assertr::verify(merge == 3) |> 
  dplyr::select(-merge) |> 
  mutate(term = paste0(str_to_title(term), " (", round(lq, digits = 1), ")"),
         county = str_to_title(county)) 

median <- fe_r_df |> 
  summarise(median(estimate)) |> 
  pull()

figure_1 <- fe_r_df |> 
  dplyr::filter(county == "Los Angeles") |> 
  ggplot(aes(x = estimate, y = reorder(term, estimate, mean))) + 
  geom_point() +
  geom_errorbar(aes(xmin=conf.low, xmax=conf.high)) + 
  scale_y_discrete(guide = guide_axis(n.dodge = 2), position = "right") +
  geom_vline(xintercept = median) +
  labs(x = "Relative performance of jurisdictions", y = "Jurisdiction") +
  theme(legend.title = element_blank(),
        text = element_text(family = "serif", size = 10))

dir.create(here::here("writing/japa/figures/"))

figure_1

ggsave(
  "writing/japa/figures/figure_1.png",
  width = 6.5,
  height = 8,
  units = "in"
)
