library(arrow)
library(assertr)
library(estimatr)
library(here)
library(readxl)
library(statar)
library(tidyverse)


# Identify LA County jurisdictions with fixed effects below median --------

samp <- read_feather(here::here("data/combined_samp_slope.feather")) |> 
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

fe_r <-  lm_robust(PARCEL_ADU ~
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

fe_r_df <- broom::tidy(fe_r, conf.int = TRUE, conf.level = 0.95) |> 
  dplyr::filter(str_detect(term, "^PL_NAME")) |> 
  dplyr::mutate(term = str_replace(term, "PL_NAME", ""))  |> 
  statar::join(pl2county, on = "term", kind = "full", check = 1 ~ 1, gen = "merge") |> 
  assertr::verify(merge == 3) |> 
  dplyr::select(-merge) |> 
  mutate(term = str_to_title(term),
         county = str_to_title(county)) 

median <- fe_r_df |> 
  summarise(median(estimate)) |> 
  pull()

ltm <- fe_r_df |>
  dplyr::filter(county == "Los Angeles" & conf.high < median) |>
  dplyr::select(term) |>
  mutate(Jurisdiction = toupper(term)) |> 
  pull(Jurisdiction)



# Create jobs accessibility quantiles -------------------------------------

# pl_ja.csv created by scripts/cleaning/pl_ja.R

pl_ja <- read_csv(here::here("data/pl_ja.csv")) |> 
  mutate(ja_quartile = cut(pl_ja,
                           quantile(pl_ja, 
                                    probs = seq(0, 1, 0.25)),
                           labels = seq(1:(length(seq(0, 1, 0.25)) - 1)), 
                           include.lowest = TRUE),
         ja_decile = cut(pl_ja,
                         quantile(pl_ja, 
                                  probs = seq(0, 1, 0.1)),
                         labels = seq(1:(length(seq(0, 1, 0.1)) - 1)), 
                         include.lowest = TRUE))

pl_ja_ltm <- pl_ja |> 
  dplyr::filter(Jurisdiction %in% ltm) 

verify(pl_ja_ltm, nrow(pl_ja_ltm) == 21)


# Drop reviewed jurisdictions ---------------------------------------------

reviewed <- read_xlsx(here::here(
  "data-raw-temp/hcd/adu-ordinance-review-letters.xlsx"),
  skip = 3) |> 
  mutate(Jurisdiction = toupper(Jurisdiction)) |> 
  distinct(Jurisdiction) |> 
  pull(Jurisdiction)

`%notin%` <- Negate(`%in%`)

fips <- pl_ja_ltm |> 
  dplyr::filter(Jurisdiction %notin% reviewed) |> 
  dplyr::filter(ja_quartile == 4) |> 
  mutate(fips = paste0("06", placefp)) |> 
  pull(fips)

dir.create(here::here("writing/japa/"), recursive = TRUE)


# Add demographic data and create table -----------------------------------

read_csv(here::here("data-raw-temp/se/pl2017_5yr/R13278953_SL160.csv")) |>  
  dplyr::filter(Geo_FIPS %in% fips) |>
  mutate(tot_pop = ACS21_5yr_B03002001,
         tot_hu = ACS21_5yr_B25003001,
         pct_pov = 100*ACS21_5yr_B17020002/ACS21_5yr_B17020001,
         med_hhi = ACS21_5yr_B19013001,
         pct_sfd = 100*ACS21_5yr_B25024002/ACS21_5yr_B25024001,
         med_gr = ACS21_5yr_B25064001,
         med_val = ACS21_5yr_B25077001,
         pct_oo = 100*ACS21_5yr_B25003002/ACS21_5yr_B25003001,
         pct_fb = 100*ACS21_5yr_B05002013/ACS21_5yr_B05002001,
         pct_asian = 100*ACS21_5yr_B03002006/ACS21_5yr_B03002001,
         pct_blk = 100*ACS21_5yr_B03002004/ACS21_5yr_B03002001,
         pct_hl = 100*ACS21_5yr_B03002012/ACS21_5yr_B03002001,
         pct_nhw = 100*ACS21_5yr_B03002003/ACS21_5yr_B03002001,
         pl_name = str_replace(Geo_QName, " city, California", "")) |>  
  dplyr::select(starts_with(c("p","m","t"))) |>
  relocate(pl_name) |>
  pivot_longer(-pl_name) |> 
  pivot_wider(name, names_from = "pl_name") |> 
  mutate(order = case_when(
    name == "tot_pop" ~ 1,
    name == "pct_asian" ~ 2,
    name == "pct_blk" ~ 3,
    name == "pct_hl" ~ 4,
    name == "pct_nhw" ~ 5,
    name == "pct_fb" ~ 6,
    name == "pct_pov" ~ 7,
    name == "med_hhi" ~ 8,
    name == "tot_hu"  ~ 9,
    name == "pct_sfd"~ 10,
    name == "pct_oo" ~ 11,
    name == "med_gr" ~ 12,
    name == "med_val" ~ 13)) |> 
  arrange(order) |> 
  mutate(variable = case_when(
    name == "tot_pop" ~ "Tot. pop.",
    name == "tot_hu" ~ "Tot. units",
    name == "pct_asian" ~ "% Asian",
    name == "pct_blk" ~ "% Black or African American",
    name == "pct_hl" ~ "% Hispanic or Latino",
    name == "pct_nhw" ~ "% non-Hispanic white",
    name == "pct_fb" ~ "% foreign born",
    name == "pct_pov" ~ "% below poverty line",
    name == "med_hhi" ~ "Med. household income",
    name == "pct_sfd" ~ "% detached single-family",
    name == "pct_oo" ~ "% owner occupied",
    name == "med_gr" ~ "Med. rent",
    name == "med_val" ~ "Med. value")) |> 
  relocate(name, order, variable) |> 
  write_csv(here::here("writing/japa/table_1.csv"))
