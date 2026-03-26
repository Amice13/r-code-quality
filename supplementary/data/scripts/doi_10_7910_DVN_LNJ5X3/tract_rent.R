# Extract and Impute Median Gross Rent Data by Census Tract, 2012-2016

library(arrow)
library(assertr)
library(ipumsr)
library(srvyr)
library(statar)
library(tidyverse)

# Read ACS data
tract_rent <- read_csv("data-raw-temp/se/tract2012_5yr/R13155753_SL140.csv",
                       col_select = c("Geo_FIPS", starts_with("ACS16_5yr_") & 
                                        !ends_with("s")),
                       col_types = c("c", rep("numeric", times = 7))) %>%
  dplyr::filter(substr(Geo_FIPS, 1, 5) %in% c("06085", "06097", "06081", 
                                              "06013", "06001", "06075", 
                                              "06095", "06041", "06055", 
                                              "06037", "06059", "06065", 
                                              "06071", "06111")) %>%
  rename(tract = Geo_FIPS) 

# labelled::var_label(tract_rent) <-
#   list(ACS16_5yr_B01003001  = "Total Population",
#        ACS16_5yr_B25001001  = "Housing Units: Total",
#        ACS16_5yr_B25064001  = "Renter-Occupied Housing Units Paying Cash Rent: Median Gross Rent",
#        ACS16_5yr_B25077001  = "Owner-Occupied Housing Units: Median Value (Dollars)")

# Rename variables
tract_rent <- tract_rent %>% 
  rename(ACS16_5yr_totpop = ACS16_5yr_B01003001,
         ACS16_5yr_tothu = ACS16_5yr_B25001001,
         ACS16_5yr_medgrent = ACS16_5yr_B25064001,
         ACS16_5yr_medvalue = ACS16_5yr_B25077001
  )

# select top-coded census tracts
topcoded_tracts <- tract_rent %>%
  dplyr::filter(ACS16_5yr_medgrent == 3501) %>%
  dplyr::select(tract)

n_topcoded_tracts <- topcoded_tracts %>% 
  nrow()

stopifnot(n_topcoded_tracts == 29)

rm(n_topcoded_tracts)

# read puma-tract crosswalk
xwalk <- read_csv("data-raw-temp/census/2010_Census_Tract_to_2010_PUMA.txt") %>%
  mutate(tract = paste0(STATEFP, COUNTYFP, TRACTCE))

# select pumas with top-coded tracts
topcoded_pumas <- topcoded_tracts %>%
  statar::join(xwalk, kind = "left")%>% 
  pull(PUMA5CE) %>% 
  as.numeric()

# Set survey options and read in PUMA data --------------------------------

# For options, see https://perma.cc/Z4HZ-KGMP
options(survey.adjust.domain.lonely=TRUE)
options(survey.lonely.psu="adjust")

ddi <- read_ipums_ddi("data-raw-temp/ipums/User Extract usa_00013.xml")

imputed_rents <- read_ipums_micro(ddi) %>% 
  mutate(RENTGRS = as.numeric(RENTGRS)) %>% 
  dplyr::filter(PUMA %in% topcoded_pumas & RENTGRS > 3500) %>% 
  as_survey_design(ids = CLUSTER, weights = HHWT, strata = STRATA, nest = TRUE) %>% 
  srvyr::group_by(PUMA) %>% 
  srvyr::summarise(med_gr_imp = survey_median(RENTGRS)) %>% 
  dplyr::select(PUMA, med_gr_imp)

rm(ddi, topcoded_pumas)

# Merge imputed rents with ACS rents & generate rent variable -------------

tract_rent <- xwalk %>%
  mutate(PUMA = as.numeric(PUMA5CE)) %>%
  dplyr::select(tract, PUMA) %>%
  statar::join(imputed_rents, on = "PUMA", kind = "right") %>%
  statar::join(topcoded_tracts, on = "tract", kind = "right") %>% 
  dplyr::select(-PUMA) %>% 
  statar::join(tract_rent, on = "tract", kind = "full") %>%
  mutate(rent_i = ifelse(!is.na(med_gr_imp), med_gr_imp, ACS16_5yr_medgrent))

n_topcoded_tracts <- tract_rent %>%
  dplyr::filter(!is.na(med_gr_imp)) %>%
  nrow()

stopifnot(n_topcoded_tracts == 29)
rm(n_topcoded_tracts, imputed_rents, xwalk, topcoded_tracts)

impute_reg <- lm(tract_rent$rent_i ~ tract_rent$ACS16_5yr_medvalue)

intercept <- impute_reg[["coefficients"]][["(Intercept)"]]
value_coef <- impute_reg[["coefficients"]][["tract_rent$ACS16_5yr_medvalue"]]

tract_rent <- tract_rent %>% 
  mutate(rent_i = ifelse(is.na(rent_i), 
                         intercept + value_coef*ACS16_5yr_medvalue,
                         rent_i))

rm(impute_reg, intercept, value_coef)

tract_rent %>% arrow::write_feather("data/tract_rent.feather")