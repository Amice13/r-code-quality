library(arrow)
library(assertr)
library(data.table)
library(dtplyr)
library(labelled)
library(lubridate)
library(tidyverse)
library(stringdist)

# Run Bay_cw.R first

# Load ID crosswalk for Bay study area -----------------------------------
bay_cw <- arrow::read_feather("data/Bay_cw.feather") %>% 
  assertr::verify(anyDuplicated(PARCEL_ID) == FALSE) %>% 
  assertr::verify(TL_NAME == toupper(TL_NAME)) %>%
# Keep only observations where Boundary Solutions county and census county match  
  filter(str_sub(PARCEL_ID, 3, 5) == str_sub(TRT_2012, 3, 5)) %>%
  mutate(cw_id = TRUE) %>%
  as.data.table()

# Number of observations where Boundary Solutions county and census county do not match: 516
# bay_cw %>% 
#   filter(str_sub(PARCEL_ID, 3, 5) != str_sub(TRT_2012, 3, 5)) %>%
#   nrow()

# Run othering_belonging.py, othering_belonging.R and Bay_zoning.py first -- cleaning Boundary Solutions and OBI data
# Load zoning variables --------------------------------------
read_obi <- function(f) {
  fread(f,
        select = c("PARCEL_ID", "juris", "ACRES", "zoning"),
        colClasses=c(PARCEL_ID = "character",
                     juris = "character",
                     ACRES = "double",
                     zoning = "character"))
}

file_names <- list.files(here::here("data/bay_ sfr_tl_cl/parcel_zoning"), pattern="*.csv$")
file_list <- paste0("data/bay_ sfr_tl_cl/parcel_zoning/", file_names)

bay_zoning <- map_dfr(file_list, read_obi) %>%
  group_by(PARCEL_ID) %>% 
  dplyr::slice(which.max(ACRES)) %>% 
  ungroup() %>% 
  distinct(PARCEL_ID, .keep_all = TRUE) %>% 
  mutate(zoning_id = TRUE,
         zoning = as.integer(zoning)) %>%
  as.data.table() %>% 
  assertr::verify(anyDuplicated(PARCEL_ID) == FALSE) %>%
  select(-ACRES) %>%
  rename(OBI_JURIS = juris,
         ZONING = zoning) %>%
  as.data.table()

rm(file_list, file_names, read_obi)

# Merge crosswalk and zoning data ---------------------------------------
bay <- full_join(bay_cw, bay_zoning,
                  by = "PARCEL_ID") %>%
  mutate(
    merge = case_when(
      zoning_id & cw_id ~ "both",
      zoning_id ~ "zoning",
      cw_id ~ "cw"
    ),
    .keep = "unused"
  ) %>%
  as_tibble() %>%
  dplyr::filter(merge == "both") %>%
  dplyr::select(-merge) %>%
  mutate(bay_id = TRUE)

# Why are there parcels in zoning data that are not in crosswalk, and why are there parcels in crosswalk that are not in the zoning data? 
## not all parcels merged with zoning data (8587) -- this can be the case because unlike the SCAG region, the parcels and zoning data are from different sources -- Boundary Solutions and Othering & Belonging -- and both can be messy.
## not all parcels merged with census tract data or places data (517)
## these makeup less than 0.5 percent of total observations -- or 8587 (cw only) + 517 (zoning only) out of 2,007,822 total observations

rm(bay_cw, bay_zoning)

# Merge ADU data ----------------------------------------------------------

# Read data
a2_adu <- read_csv(paste0("data-raw-temp/hcd/apr/apr-2022-08-30/",
                          "table-a2-annual-building-activity-report-summary-",
                          "new-construction-entitled-permits-and-completed.csv", sep = ""),
                   col_names = TRUE,
                   col_types = "cccccccccciiiiiiiciiiiiiiiciiiiiiiiciicccccciccdiccc") %>%
  assertr::verify(
    UNIT_CAT_DESC == "2-, 3-, and 4-Plex Units per Structure" |
      UNIT_CAT_DESC == "5 or More Units Per Structure" |
      UNIT_CAT_DESC == "Accessory Dwelling Unit" |
      UNIT_CAT_DESC == "Mobile Home Unit" |
      UNIT_CAT_DESC == "Single-Family Attached Unit" |
      UNIT_CAT_DESC == "Single-Family Detached Unit") %>%
  dplyr::filter(UNIT_CAT_DESC == "Accessory Dwelling Unit") %>%
  mutate(ENT_APPROVE_DT1 = mdy_hm(ENT_APPROVE_DT1),
         CO_ISSUE_DT1 = mdy_hm(CO_ISSUE_DT1),
         BP_ISSUE_DT1 = mdy_hm(BP_ISSUE_DT1)) %>%
  assertr::verify(CNTY_NAME %in% c("ALAMEDA", "ALPINE", "AMADOR", "BUTTE", "CALAVERAS",
                                   "COLUSA", "CONTRA COSTA", "DEL NORTE", "EL DORADO", "FRESNO",
                                   "GLENN", "HUMBOLDT", "IMPERIAL", "INYO", "KERN",
                                   "KINGS", "LAKE", "LASSEN", "LOS ANGELES", "MADERA",
                                   "MARIN", "MARIPOSA", "MENDOCINO", "MERCED", "MONO",
                                   "MONTEREY", "NAPA", "NEVADA", "ORANGE", "PLACER",
                                   "PLUMAS", "RIVERSIDE", "SACRAMENTO", "SAN BENITO", "SAN BERNARDINO",
                                   "SAN DIEGO", "SAN FRANCISCO", "SAN JOAQUIN", "SAN LUIS OBISPO", "SAN MATEO",
                                   "SANTA BARBARA", "SANTA CLARA", "SANTA CRUZ", "SHASTA", "SIERRA",
                                   "SISKIYOU", "SOLANO", "SONOMA", "STANISLAUS", "SUTTER",
                                   "TEHAMA", "TRINITY", "TULARE", "TUOLUMNE", "VENTURA",
                                   "YOLO", "YUBA")) %>%
  dplyr::select(JURS_NAME, CNTY_NAME, APN, STREET_ADDRESS, YEAR) %>%
  dplyr::filter(CNTY_NAME %in% c("ALAMEDA", "CONTRA COSTA", "MARIN", "NAPA",
                                 "SAN FRANCISCO", "SAN MATEO", "SANTA CLARA", "SOLANO", "SONOMA")) %>%
  distinct(across(everything()), .keep_all = TRUE)

# Fix jurnames in HCD data for merge with SCAG data
a2_adu <- a2_adu %>%
  mutate(JURS_NAME = str_squish((JURS_NAME)),
         JURS_NAME = str_replace(JURS_NAME, "COUNTY", "UNINCORPORATED"))

hcd_to_bay <- a2_adu %>%
  rename(HCD_CITY = JURS_NAME) %>%
  mutate(ADU = 1) %>%
  rename(HCD_CNTY = CNTY_NAME,
         HCD_APN = APN,
         HCD_STADDRESS = STREET_ADDRESS) %>%
  select(c(YEAR, HCD_APN, HCD_STADDRESS, HCD_CITY, HCD_CNTY, ADU))

bay <- bay %>%
  mutate(TL_NAME = str_replace(TL_NAME, "COUNTY", "UNINCORPORATED"),
         TL_NAME = str_replace(TL_NAME, "TOWN|CITY", ""),
         TL_NAME = str_squish((TL_NAME)),
         TL_NAME = case_when(TL_NAME == "DALY" ~ "DALY CITY",
                               TL_NAME == "FOSTER" ~ "FOSTER CITY",
                               TL_NAME == "REDWOOD" ~ "REDWOOD CITY",
                               TL_NAME == "SUISUN" ~ "SUISUN CITY",
                               TL_NAME == "UNION" ~ "UNION CITY",
                               TL_NAME == "ST. HELENA" ~ "SAINT HELENA",
                               TRUE ~ TL_NAME)) %>%
  dplyr::filter(BS_APN != "" | BS_APN2 != "")

bay <- bay %>%
  mutate(CITY_JOIN = TL_NAME)

hcd_to_bay <- hcd_to_bay %>%
  mutate(CITY_JOIN = HCD_CITY)

# Fix APNS in HCD data for merge with Boundary Solutions data
# Fix APNS in Boundary Solutions data for merge with HCD data
## SAN JOSE has multiple APNs in one column separated by semi-colon (n = 26)
## DUBLIN has multiple parcels with APN 985-127-002 -- likely rounded up from 985-126-***; all of these are on Delamar, Lembert Hills, Tulare Hill, Copper Peak, etc.
## UNION CITY APN format is inconsistent -- some BS_APN, some BS_APN2 (padded with zeros)
hcd_to_bay <- hcd_to_bay %>%
  mutate(HCD_APN = case_when(HCD_CITY == "ALAMEDA" & str_detect(HCD_APN, "^[0-9]{2}-[0-9]{3}-0[0-9]$") ~ paste0(str_sub(HCD_APN, 1, 7), str_sub(HCD_APN, 9, 9)),
                             HCD_CITY == "SAN JOSE" ~ str_remove(HCD_APN, ", more...$"),
                             HCD_CITY == "RICHMOND" ~ str_remove(HCD_APN, "-[0-9]$"),
                             HCD_CITY == "EMERYVILLE" ~ paste0(HCD_APN, "00"),
                             HCD_CITY == "SAN LEANDRO" & str_detect(HCD_APN, "-[0-9][0-9]$") == FALSE ~ paste0(HCD_APN, "-00"),
                             HCD_CITY == "SAN LEANDRO" & HCD_APN == "080H-1556-020-000-00" ~ "080H-1556-020-00",
                             #HCD_CITY == "UNION CITY" & str_length(HCD_APN) < 13 ~ 
                             HCD_CITY == "YOUNTVILLE" ~ paste0(HCD_APN, "000"),
                             HCD_CITY == "MARTINEZ" ~ str_sub(HCD_APN, 1, 11),
                             HCD_CITY == "PLEASANT HILL" & str_detect(HCD_APN, "-[0-9]$") ~ str_remove(HCD_APN, "-[0-9]$"),
                             TRUE ~ HCD_APN)) %>%
  mutate(APN_JOIN = str_remove_all(str_squish(HCD_APN), "^0+|[-]|\\s"),
         APN_JOIN = str_remove_all(APN_JOIN, "[']|\\s")) %>%
  arrange(HCD_CNTY, HCD_CITY, APN_JOIN) %>%
  mutate(hcd_id = TRUE)  

bay <- bay %>%
  mutate(APN_JOIN = case_when(
    BSFILE_COUNTY == "ALAMEDA" & str_detect(TL_NAME, "ALAMEDA") == FALSE ~ BS_APN2,
    TL_NAME %in% c("NAPA", "SAINT HELENA", "AMERICAN CANYON", "CALISTOGA") ~ str_remove(BS_APN, "000$"),
    TRUE ~ BS_APN)) %>%
  mutate(APN_JOIN = str_remove_all(str_squish(APN_JOIN), "^0+|[-]|\\s"),
         APN_JOIN = str_remove_all(APN_JOIN, "[']|\\s")) %>%
  arrange(BSFILE_COUNTY, TL_NAME, APN_JOIN)

# Create data frame consisting of each city in each year covered by HCD data
city_year <- bay %>% 
  distinct(CITY_JOIN) %>% 
  dplyr::filter(str_detect(CITY_JOIN, "UNINCORPORATED") == FALSE) %>% 
  expand(CITY_JOIN, YEAR = as.character(2018:2021))

# Merge Parcel and HCD data
bay <- statar::join(hcd_to_bay, bay,
                     on = c("CITY_JOIN", "APN_JOIN"),
                     kind = "full",
                     gen = "merge") %>%
  ### REMOVE OBSERVATIONS IN UNINCORPORATED AREAS
  dplyr::filter(str_detect(CITY_JOIN, "UNINCORPORATED") == FALSE) %>%
  assertr::verify(!is.na(merge))

rm(a2_adu, hcd_to_bay)

# Create city-level summary merge statistics ------------------------------
bay_city <- bay %>%
  mutate(merged_adu = if_else(merge == "3", 1, 0),
         unmerged_adu = if_else(merge == "1", 1, 0)) %>%
  group_by(CITY_JOIN, YEAR) %>%
  mutate(PL_merged_adu = sum(merged_adu),
         PL_unmerged_adu = sum(unmerged_adu),
         PL_mean_merge = mean(as.integer(merge))) %>%
  ungroup() %>%
  distinct(CITY_JOIN, YEAR, .keep_all = TRUE) %>%
  dplyr::select(CITY_JOIN, YEAR, contains("merge"))

bay_city %>%
  dplyr::filter(!is.na(YEAR) & PL_merged_adu != PL_unmerged_adu) %>%
  assertr::verify(PL_mean_merge != 2)
# Logical test for...checking merge variable (whether merged with HCD, Parcels -- over time and jurisdiction)

bay_city %>%
  dplyr::filter(is.na(YEAR)) %>%
  assertr::verify(PL_mean_merge == 2)
# All ones without year are 2s (not in HCD data) -- no year, no HCD data

bay_city <- bay_city %>%
  dplyr::filter(!is.na(YEAR)) %>%
  dplyr::select(-c(merge, merged_adu, unmerged_adu, PL_mean_merge)) %>%
  # Fill in missing years
  statar::join(city_year, on = c("CITY_JOIN", "YEAR"),
               kind = "full", gen = "merge") %>%
  # For unmerged observations (i.e., observations for which there are no
  # merged OR unmerged ADUs) make PL_merged_adu = 0 and  PL_unmerged_adu = 0
  mutate(PL_merged_adu = if_else(merge == 2, 0, PL_merged_adu),
         PL_unmerged_adu = if_else(merge == 2, 0, PL_unmerged_adu)) %>%
  dplyr::select(-merge) %>%
  group_by(CITY_JOIN) %>%
  mutate(PL_merged_adu_all = sum(PL_merged_adu),
         PL_unmerged_adu_all = sum(PL_unmerged_adu)) %>%
  ungroup() %>%
  assertr::verify(!is.na(PL_merged_adu)) %>%
  assertr::verify(!is.na(PL_unmerged_adu)) %>%
  assertr::verify(!is.na(PL_merged_adu_all)) %>%
  assertr::verify(!is.na(PL_unmerged_adu_all)) %>%
  mutate(PL_pct_adu_unmerged = if_else(PL_unmerged_adu + PL_merged_adu == 0, 0,
                                       100*PL_unmerged_adu/
                                         (PL_unmerged_adu + PL_merged_adu)),
         PL_pct_adu_unmerged_all = if_else(PL_unmerged_adu_all + PL_merged_adu_all == 0, 0,
                                           100*PL_unmerged_adu_all/
                                             (PL_unmerged_adu_all + PL_merged_adu_all))) %>%
  pivot_wider(id_cols = c(CITY_JOIN, ends_with("_all")),
              names_from = YEAR,
              names_glue = "{.value}_{YEAR}",
              values_from = c(PL_merged_adu, PL_unmerged_adu, PL_pct_adu_unmerged)) %>%
  assertr::verify(!is.na(PL_merged_adu_2018)) %>%
  assertr::verify(!is.na(PL_merged_adu_2021)) %>%
  assertr::verify(!is.na(PL_merged_adu_2020)) %>%
  assertr::verify(!is.na(PL_merged_adu_2019)) %>%
  assertr::verify(!is.na(PL_unmerged_adu_2018)) %>%
  assertr::verify(!is.na(PL_unmerged_adu_2021)) %>%
  assertr::verify(!is.na(PL_unmerged_adu_2020)) %>%
  assertr::verify(!is.na(PL_unmerged_adu_2019)) %>%
  assertr::verify(!is.na(PL_pct_adu_unmerged_2018)) %>%
  assertr::verify(!is.na(PL_pct_adu_unmerged_2021)) %>%
  assertr::verify(!is.na(PL_pct_adu_unmerged_2020)) %>%
  assertr::verify(!is.na(PL_pct_adu_unmerged_2019))

bay <- bay %>%
  dplyr::filter(merge != 1) %>%
  dplyr::select(-merge) %>%
  statar::join(bay_city,
               on = "CITY_JOIN",
               kind = "full",
               check = m ~ 1,
               gen = "merge") %>%
  assertr::verify(merge == 3) %>%
  dplyr::select(-merge)

rm(bay_city, city_year)

# Collapse to unique parcel IDs
bay <- bay %>%
  assertr::verify(ADU == 1 | is.na(ADU)) %>%
  mutate(ADU = case_when(
    ADU == 1 ~ 1,
    is.na(ADU) ~ 0)) %>%
  group_by(YEAR, PARCEL_ID) %>%
  mutate(ADU = max(ADU)) %>%
  ungroup() %>%
  mutate(ADU_2018 = if_else(YEAR == "2018" & ADU == 1, 1, 0),
         ADU_2019 = if_else(YEAR == "2019" & ADU == 1, 1, 0),
         ADU_2020 = if_else(YEAR == "2020" & ADU == 1, 1, 0),
         ADU_2021 = if_else(YEAR == "2021" & ADU == 1, 1, 0)) %>%
  group_by(PARCEL_ID) %>%
  mutate(ADU_2018 = max(ADU_2018),
         ADU_2019 = max(ADU_2019),
         ADU_2020 = max(ADU_2020),
         ADU_2021 = max(ADU_2021)) %>%
  ungroup() %>%
  distinct(PARCEL_ID, .keep_all = TRUE) %>%
  dplyr::select(-c(starts_with("HCD"), YEAR, APN_JOIN, CITY_JOIN))

# Merge tract-level variables ---------------------------------------------
tract_rent <- arrow::read_feather("data/tract_rent.feather") %>%
  dplyr::filter(str_sub(tract, 3, 5) %in% c("001", "013", "041", "055",
                                            "075", "081", "085", "095", "097")) %>%
  rename_with(~ paste0("TRT_", .x)) %>%
  mutate(rent_id = TRUE) %>%
  rename(TRT_2012 = TRT_tract)

bay <- full_join(bay, tract_rent,
                  by = "TRT_2012") %>%
  mutate(
    merge = case_when(
      bay_id & rent_id ~ "both",
      bay_id ~ "bay",
      rent_id ~ "rent"
    ),
    .keep = "unused"
  ) %>%
  assertr::verify(merge != "bay") %>%
  dplyr::filter(merge == "both") %>%
  dplyr::select(-merge) %>%
  mutate(bay_id = TRUE)

rm(tract_rent)

# Created by scripts/tract_jobs-access.R
jobs_access <- arrow::read_feather("data/jobs-access.feather") %>%
  mutate(ja_id = TRUE) %>%
  rename(TRT_2012 = tract,
         TRT_jobs_access = jobs_access)

bay <- full_join(bay, jobs_access,
                  by = "TRT_2012")  %>%
  mutate(
    merge = case_when(
      bay_id & ja_id ~ "both",
      bay_id ~ "bay",
      ja_id ~ "ja"
    ),
    .keep = "unused"
  ) %>%
  assertr::verify(merge != "bay") %>%
  dplyr::filter(merge == "both") %>%
  dplyr::select(-merge) %>%
  mutate(bay_id = TRUE)

rm(jobs_access)

# Merge place-level variables ---------------------------------------------

pl <- read_csv("data-raw-temp/se/pl2012_5yr/R13177292_SL160.csv",
               col_names = TRUE,
               col_types = paste0(c(rep("c", 55),
                                    rep("d", 74)),
                                  collapse = "")) %>%
  dplyr::select(c(Geo_PLACE, starts_with("ACS") & ! ends_with("S"))) %>%
  mutate(PL_ACS16_5yr_totpop   = ACS16_5yr_B03002001,
         PL_ACS16_5yr_pct_nhw  = 100*ACS16_5yr_B03002003/ACS16_5yr_B03002001,
         PL_ACS16_5yr_pct_nhb  = 100*ACS16_5yr_B03002004/ACS16_5yr_B03002001,
         PL_ACS16_5yr_pct_nha  = 100*ACS16_5yr_B03002006/ACS16_5yr_B03002001,
         PL_ACS16_5yr_pct_hl   = 100*ACS16_5yr_B03002012/ACS16_5yr_B03002001,
         PL_ACS16_5yr_medhhi   = ACS16_5yr_B19013001,
         PL_ACS16_5yr_pctoo    = 100*ACS16_5yr_B25003002/ACS16_5yr_B25003001,
         PL_ACS16_5yr_oo       = ACS16_5yr_B25003002,
         PL_ACS16_5yr_medgr    = ACS16_5yr_B25064001,
         PL_ACS16_5yr_pctsfd   = 100*ACS16_5yr_B25024002/ACS16_5yr_B25024001,
         PL_ACS16_5yr_pctsftot = 100*(ACS16_5yr_B25024002 + ACS16_5yr_B25024003) /
           ACS16_5yr_B25024001,
         PL_ACS16_5yr_sfd      = ACS16_5yr_B25024002,
         PL_ACS16_5yr_sftot    = ACS16_5yr_B25024002 + ACS16_5yr_B25024003,
         pl_id = TRUE) %>%
  rename(PLACEFP = Geo_PLACE) %>%
  dplyr::select(-starts_with("ACS16"))

var_label(pl) <-
  list(
    PL_ACS16_5yr_totpop   = "Total population",
    PL_ACS16_5yr_pct_nhw  = "% non-Hispanic white",
    PL_ACS16_5yr_pct_nhb  = "% non-Hispanic Black",
    PL_ACS16_5yr_pct_nha  = "% non-Hispanic Asian",
    PL_ACS16_5yr_pct_hl   = "% Hispanic or Latino",
    PL_ACS16_5yr_medhhi   = "Median household income",
    PL_ACS16_5yr_pctoo    = "% owner-occupied",
    PL_ACS16_5yr_oo       = "N owner-occupied",
    PL_ACS16_5yr_medgr    = "Median gross rent",
    PL_ACS16_5yr_pctsfd   = "% single-family detached",
    PL_ACS16_5yr_pctsftot = "% single-family (attached & detached)",
    PL_ACS16_5yr_sfd      = "N single-family detached",
    PL_ACS16_5yr_sftot    = "N single-family (attached & detached)")

bay <- full_join(bay, pl, by = "PLACEFP")  %>%
  mutate(
    merge = case_when(
      bay_id & pl_id ~ "both",
      bay_id ~ "bay",
      pl_id ~ "place"
    ),
    .keep = "unused"
  ) %>%
  assertr::verify(merge != "bay") %>%
  dplyr::filter(merge == "both") %>%
  dplyr::select(-merge)

rm(pl)

# Clean up ----------------------------------------------------------------

# Rename so that each variable name indicates geographic scale

bay <- bay %>%
  rename(
    PL_NAME = TL_NAME,
    PARCEL_ADU = ADU,
    PARCEL_ADU_2018 = ADU_2018,
    PARCEL_ADU_2019 = ADU_2019,
    PARCEL_ADU_2020 = ADU_2020,
    PARCEL_ADU_2021 = ADU_2021,
    PARCEL_APN = BS_APN,
    PARCEL_BAYCOUNTY = BSFILE_COUNTY,
    PARCEL_ACRES = PARCEL_SIZE,
    PL_FIPS_2012 = PLACEFP,
    PARCEL_URBAN = URBAN,
    PARCEL_ZONING = ZONING)
# 
# [DOES THE FOLLOWING PERTAIN TO BAY AREA DATA? I DON'T THINK SO BUT CHECK]
# For some parcels the zoning and/or landuse codes include the  description, while
# # for others, the codes include only the numeric values (or N/A)
# # Clean up to ensure consistency
# 
# scag <- scag %>% 
#   mutate(PARCEL_USECODE = str_sub(PARCEL_USE, 1, 4),
#          PARCEL_ZONINGCODE = str_sub(PARCEL_ZONING, 1, 4)) %>% 
#   dplyr::select(-c(PARCEL_USE, PARCEL_ZONING)) %>% 
#   mutate(scag_id = TRUE)
# 
# # Add descriptions
# 
# desc <- read_csv("data/SCAG_codes.csv", 
#                  col_names = TRUE,
#                  col_types = "cc") %>% 
#   mutate(desc_id = TRUE)
# 
# scag <- full_join(scag, desc,
#                   by = c("PARCEL_ZONINGCODE" = "CODE")) %>%
#   mutate(
#     merge = case_when(
#       scag_id & desc_id ~ "both",
#       scag_id ~ "scag",
#       desc_id ~ "desc"
#     ),
#     .keep = "unused"
#   ) %>% 
#   dplyr::filter(merge != "desc")
# 
# scag %>% 
#   dplyr::filter(merge == "scag") %>% 
#   assertr::verify(PARCEL_ZONINGCODE == "" | PARCEL_ZONINGCODE == "N/A")
# 
# scag <- scag %>% 
#   dplyr::select(-merge) %>% 
#   rename(PARCEL_ZONINGDESC = DESCRIPTION) %>% 
#   mutate(scag_id = TRUE)
# 
# scag <- full_join(scag, desc,
#                   by = c("PARCEL_USECODE" = "CODE")) %>%
#   mutate(
#     merge = case_when(
#       scag_id & desc_id ~ "both",
#       scag_id ~ "scag",
#       desc_id ~ "desc"
#     ),
#     .keep = "unused"
#   ) %>%
#   dplyr::filter(merge != "desc")
# 
# scag %>%
#   dplyr::filter(merge == "scag") %>%
#   assertr::verify(PARCEL_USECODE == "" | 
#                     PARCEL_USECODE == "N/A" | 
#                     PARCEL_USECODE == "1890")
# 
# scag <- scag %>%
#   dplyr::select(-merge) %>%
#   rename(PARCEL_USEDESC = DESCRIPTION) %>% 
#   relocate(starts_with("PARCEL"), starts_with("TRT"), starts_with("PL"))
# 
# # Write to disk -----------------------------------------------------------

bay %>%
  arrow::write_feather("data/Bay_merge.feather")

rm(list = ls())