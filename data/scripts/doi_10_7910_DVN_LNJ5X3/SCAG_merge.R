library(arrow)
library(assertr)
library(data.table)
library(dtplyr)
library(labelled)
library(lubridate)
library(tidyverse)

# Run SCAG_cw.R first

# Load ID crosswalk for SCAG study area -----------------------------------

scag_cw <- arrow::read_feather("data/SCAG_cw.feather")  %>% 
  assertr::verify(anyDuplicated(SCAGUID16) == FALSE) %>% 
  assertr::verify(SCAG_CITY == toupper(SCAG_CITY)) %>% 
  mutate(APN2 = str_remove_all(str_squish(APN), "^0+|[-]|\\s")) %>% 
  # Keep only observations where SCAG county and census county match
  filter(str_sub(SCAGUID16, 1, 3) == str_sub(TRT_2012, 3, 5)) %>% 
  mutate(cw_id = TRUE) %>% 
  as.data.table()

# Load zoning and land use variables --------------------------------------

read_scag <- function(f) {
  fread(f,
        select = c("SCAGUID16", "SCAG_ZN_CO", "LU16"),
        colClasses=c(SCAGUID16 = "character",
                     SCAG_ZN_CO = "character",
                     LU16 = "character"))
}

file_list <- c("data/scag/LandUse_Combined_LA.csv",
               "data/scag/2016_Land_Use_Information_for_Riverside_County.csv",
               "data/scag/2016_Land_Use_Information_for_San_Bernardino_County.csv",
               "data/scag/2016_Land_Use_Information_for_Orange_County.csv",
               "data/scag/2016_Land_Use_Information_for_Ventura_County.csv")

# These files were exported as .csv files from QGIS (v. 3.22.4) from the following raw data files:
# data-raw/scag/41ef1833997543258dd88c6077f00f61.gdb
# data-raw/scag/2016_Land_Use_Information_for_San_Bernardino_County.shp
# data-raw/scag/2016_Land_Use_Information_for_Riverside_County.shp
# data-raw/scag/2016_Land_Use_Information_for_Ventura_County.shp
# data-raw/scag/2016_Land_Use_Information_for_Orange_County.shp

scag_lu <- map_dfr(file_list, read_scag) %>% 
  assertr::verify(anyDuplicated(SCAGUID16) == FALSE) %>% 
  mutate(lu_id = TRUE)

rm(file_list, read_scag)


# Merge crosswalk and land use data ---------------------------------------

scag <- full_join(scag_cw, scag_lu,
                  by = "SCAGUID16") %>%
  mutate(
    merge = case_when(
      lu_id & cw_id ~ "both",
      lu_id ~ "lu",
      cw_id ~ "cw"
    ),
    .keep = "unused"
  ) %>% 
  as_tibble() %>% 
  assertr::verify(merge != "id") %>% 
  dplyr::filter(merge == "both") %>% 
  dplyr::select(-merge) %>% 
  mutate(scag_id = TRUE)

rm(scag_cw, scag_lu)


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
  dplyr::filter(CNTY_NAME %in% c("LOS ANGELES", "ORANGE", "VENTURA", "RIVERSIDE", 
                                 "SAN BERNARDINO")) %>% 
  distinct(across(everything()), .keep_all = TRUE)


# Fix jurnames in HCD data for merge with SCAG data

hcd_to_scag <- a2_adu %>% 
  mutate(SCAG_CITY = case_when(
    JURS_NAME == "CATHEDRAL" ~ "CATHEDRAL CITY",
    JURS_NAME == "VENTURA" ~ "SAN BUENAVENTURA",
    TRUE ~ JURS_NAME
  )) %>% 
  mutate(ADU = 1) %>% 
  rename(HCD_CNTY = CNTY_NAME,
         HCD_APN = APN, 
         HCD_STADDRESS = STREET_ADDRESS)

# Fix APNS in HCD data for merge with SCAG data

hcd_to_scag <- hcd_to_scag %>% 
  mutate(APN2 = HCD_APN) %>% 
  mutate(APN2 = str_remove_all(str_squish(APN2), "^0+|[-]|\\s")) %>%
  mutate(APN2 = str_remove_all(APN2, "[']|\\s")) %>%
  mutate(APN2 = case_when(
    SCAG_CITY == "CALEXICO"  ~ str_sub(APN2, 1, 8),
    str_detect(SCAG_CITY, "SAN BERNARDINO") ~ str_remove(APN2, "0000$"),
    str_detect(SCAG_CITY, "GRAND TERRACE") ~ str_remove(APN2, "0000$"),
    str_detect(SCAG_CITY, "RANCHO CUCAMONGA") ~ str_remove(APN2, "0000$"),
    HCD_CNTY == "VENTURA" & SCAG_CITY != "SAN BUENAVENTURA" ~ str_remove(APN2, "[05]$"),
    TRUE ~ APN2
  )) %>% 
  mutate(temp = ifelse(str_count(HCD_APN, "/") == 2, 
                       as.character(as.Date(HCD_APN, "%m/%d/%y")),
                       NA)) %>%
  mutate(temp = ifelse(!is.na(temp),
                       str_remove_all(temp, "-"),
                       NA)) %>%
  mutate(temp = ifelse(!is.na(temp),
                       paste0(str_sub(temp, 3, 4), 0, str_sub(temp, 5, 6), 0, str_sub(temp, 7, 8)),
                       NA)) %>%
  mutate(APN2 = case_when(SCAG_CITY == "LA CANADA FLINTRIDGE" & !is.na(temp) ~ paste0("58", temp),
                          SCAG_CITY == "SOUTH GATE" & !is.na(temp) ~ paste0("62", temp),
                          SCAG_CITY == "NORWALK" & !is.na(temp) ~ paste0("80", temp),
                          SCAG_CITY == "TEMPLE CITY" & !is.na(temp) ~ paste0("85", temp),
                          SCAG_CITY == "SOUTH EL MONTE" & !is.na(temp) ~ paste0("81", temp),
                          SCAG_CITY == "LAWNDALE" & !is.na(temp) ~ paste0("40", temp),
                          SCAG_CITY == "ARTESIA" & !is.na(temp) ~ paste0("70", temp),
                          TRUE ~ APN2
  )) %>%
  select(-temp) 

# Create data frame consisting of each city in each year covered by HCD data
city_year <- scag %>% 
  distinct(SCAG_CITY) %>% 
  dplyr::filter(str_sub(SCAG_CITY, -7) != " COUNTY") %>% 
  dplyr::filter(SCAG_CITY != "UNINCORPORATED") %>% 
  expand(SCAG_CITY, YEAR = as.character(2018:2021))

scag <- statar::join(hcd_to_scag, scag, 
                     on = c("SCAG_CITY", "APN2"),
                     kind = "full",
                     gen = "merge") %>% 
  ### REMOVE OBSERVATIONS IN UNINCORPORATED AREAS
  dplyr::filter(str_sub(SCAG_CITY, -7) != " COUNTY") %>% 
  dplyr::filter(SCAG_CITY != "UNINCORPORATED") %>%
  assertr::verify(!is.na(merge)) 

rm(a2_adu, hcd_to_scag)

# Create city-level summary merge statistics ------------------------------


scag_city <- scag %>% 
  mutate(merged_adu = if_else(merge == "3", 1, 0),
         unmerged_adu = if_else(merge == "1", 1, 0)) %>% 
  group_by(SCAG_CITY, YEAR) %>% 
  mutate(PL_merged_adu = sum(merged_adu),
         PL_unmerged_adu = sum(unmerged_adu),
         PL_mean_merge = mean(as.integer(merge))) %>% 
  ungroup() %>% 
  distinct(SCAG_CITY, YEAR, .keep_all = TRUE) %>% 
  dplyr::select(SCAG_CITY, YEAR, contains("merge")) 

scag_city %>% 
  dplyr::filter(!is.na(YEAR) & PL_merged_adu != PL_unmerged_adu) %>% 
  assertr::verify(PL_mean_merge != 2)

scag_city %>% 
  dplyr::filter(is.na(YEAR)) %>% 
  assertr::verify(PL_mean_merge == 2)

scag_city <- scag_city %>% 
  dplyr::filter(!is.na(YEAR)) %>% 
  dplyr::select(-c(merge, merged_adu, unmerged_adu, PL_mean_merge)) %>% 
  # Fill in missing years
  statar::join(city_year, on = c("SCAG_CITY", "YEAR"), 
               kind = "full", gen = "merge") %>% 
  # For unmerged observations (i.e., observations for which there are no 
  # merged OR unmerged ADUs) make PL_merged_adu = 0 and  PL_unmerged_adu = 0
  mutate(PL_merged_adu = if_else(merge == 2, 0, PL_merged_adu), 
         PL_unmerged_adu = if_else(merge == 2, 0, PL_unmerged_adu)) %>% 
  dplyr::select(-merge) %>% 
  group_by(SCAG_CITY) %>%
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
  pivot_wider(id_cols = c(SCAG_CITY, ends_with("_all")),
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

scag <- scag %>% 
  dplyr::filter(merge != 1) %>% 
  dplyr::select(-merge) %>% 
  statar::join(scag_city, 
               on = "SCAG_CITY",
               kind = "full", 
               check = m ~ 1,
               gen = "merge") %>% 
  assertr::verify(merge == 3) %>% 
  dplyr::select(-merge)

rm(scag_city, city_year)

# Collapse to unique parcel IDs 

scag <- scag %>% 
  assertr::verify(ADU == 1 | is.na(ADU)) %>% 
  mutate(ADU = case_when(
    ADU == 1 ~ 1, 
    is.na(ADU) ~ 0)) %>% 
  group_by(YEAR, SCAGUID16) %>% 
  mutate(ADU = max(ADU)) %>% 
  ungroup() %>% 
  mutate(ADU_2018 = if_else(YEAR == "2018" & ADU == 1, 1, 0),
         ADU_2019 = if_else(YEAR == "2019" & ADU == 1, 1, 0),
         ADU_2020 = if_else(YEAR == "2020" & ADU == 1, 1, 0),
         ADU_2021 = if_else(YEAR == "2021" & ADU == 1, 1, 0)) %>% 
  group_by(SCAGUID16) %>% 
  mutate(ADU_2018 = max(ADU_2018),
         ADU_2019 = max(ADU_2019),
         ADU_2020 = max(ADU_2020),
         ADU_2021 = max(ADU_2021)) %>% 
  ungroup() %>% 
  distinct(SCAGUID16, .keep_all = TRUE) %>% 
  dplyr::select(-c(JURS_NAME, starts_with("HCD"), YEAR, APN2))

# Merge tract-level variables ---------------------------------------------

tract_rent <- arrow::read_feather("data/tract_rent.feather") %>% 
  dplyr::filter(str_sub(tract, 3, 5) %in% c("037", "059", "065", 
                                            "071", "111")) %>% 
  rename_with(~ paste0("TRT_", .x)) %>% 
  mutate(rent_id = TRUE) %>% 
  rename(TRT_2012 = TRT_tract)


scag <- full_join(scag, tract_rent,
                  by = "TRT_2012") %>% 
  mutate(
    merge = case_when(
      scag_id & rent_id ~ "both",
      scag_id ~ "scag",
      rent_id ~ "rent"
    ),
    .keep = "unused"
  ) %>% 
  assertr::verify(merge != "scag") %>% 
  dplyr::filter(merge == "both") %>% 
  dplyr::select(-merge) %>% 
  mutate(scag_id = TRUE)

rm(tract_rent)

# Created by scripts/tract_jobs-access.R
jobs_access <- arrow::read_feather("data/jobs-access.feather") %>% 
  mutate(ja_id = TRUE) %>% 
  rename(TRT_2012 = tract,
         TRT_jobs_access = jobs_access)

scag <- full_join(scag, jobs_access,
                  by = "TRT_2012")  %>% 
  mutate(
    merge = case_when(
      scag_id & ja_id ~ "both",
      scag_id ~ "scag",
      ja_id ~ "ja"
    ),
    .keep = "unused"
  ) %>% 
  assertr::verify(merge != "scag") %>% 
  dplyr::filter(merge == "both") %>% 
  dplyr::select(-merge) %>% 
  mutate(scag_id = TRUE)

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
  rename(PLACEFP_2012 = Geo_PLACE) %>% 
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

scag <- full_join(scag, pl,
                  by = "PLACEFP_2012")  %>% 
  mutate(
    merge = case_when(
      scag_id & pl_id ~ "both",
      scag_id ~ "scag",
      pl_id ~ "place"
    ),
    .keep = "unused"
  ) %>% 
  assertr::verify(merge != "scag") %>% 
  dplyr::filter(merge == "both") %>% 
  dplyr::select(-merge)

rm(pl)

# Clean up ----------------------------------------------------------------

# Rename so that each variable name indicates geographic scale 

scag <- scag %>% 
  rename(
    PL_SCAGNAME = SCAG_CITY,
    PARCEL_ADU = ADU,
    PARCEL_ADU_2018 = ADU_2018,
    PARCEL_ADU_2019 = ADU_2019,
    PARCEL_ADU_2020 = ADU_2020,
    PARCEL_ADU_2021 = ADU_2021,
    PARCEL_SCAGUID16 = SCAGUID16,
    PARCEL_APN = APN,
    PARCEL_SCAGCOUNTY = COUNTY,
    PARCEL_ACRES = ACRES,
    PL_FIPS_2012 = PLACEFP_2012,
    PARCEL_URBAN = URBAN,
    PARCEL_ZONING = SCAG_ZN_CO,
    PARCEL_USE = LU16)

# For some parcels the zoning and/or landuse codes include the  description, while
# for others, the codes include only the numeric values (or N/A)
# Clean up to ensure consistency

scag <- scag %>% 
  mutate(PARCEL_USECODE = str_sub(PARCEL_USE, 1, 4),
         PARCEL_ZONINGCODE = str_sub(PARCEL_ZONING, 1, 4)) %>% 
  dplyr::select(-c(PARCEL_USE, PARCEL_ZONING)) %>% 
  mutate(scag_id = TRUE)

# Add descriptions

desc <- read_csv("data/SCAG_codes.csv", 
                 col_names = TRUE,
                 col_types = "cc") %>% 
  mutate(desc_id = TRUE)

scag <- full_join(scag, desc,
                  by = c("PARCEL_ZONINGCODE" = "CODE")) %>%
  mutate(
    merge = case_when(
      scag_id & desc_id ~ "both",
      scag_id ~ "scag",
      desc_id ~ "desc"
    ),
    .keep = "unused"
  ) %>% 
  dplyr::filter(merge != "desc")

scag %>% 
  dplyr::filter(merge == "scag") %>% 
  assertr::verify(PARCEL_ZONINGCODE == "" | PARCEL_ZONINGCODE == "N/A")

scag <- scag %>% 
  dplyr::select(-merge) %>% 
  rename(PARCEL_ZONINGDESC = DESCRIPTION) %>% 
  mutate(scag_id = TRUE)

scag <- full_join(scag, desc,
                  by = c("PARCEL_USECODE" = "CODE")) %>%
  mutate(
    merge = case_when(
      scag_id & desc_id ~ "both",
      scag_id ~ "scag",
      desc_id ~ "desc"
    ),
    .keep = "unused"
  ) %>%
  dplyr::filter(merge != "desc")

scag %>%
  dplyr::filter(merge == "scag") %>%
  assertr::verify(PARCEL_USECODE == "" | 
                    PARCEL_USECODE == "N/A" | 
                    PARCEL_USECODE == "1890")

scag <- scag %>%
  dplyr::select(-merge) %>%
  rename(PARCEL_USEDESC = DESCRIPTION) %>% 
  relocate(starts_with("PARCEL"), starts_with("TRT"), starts_with("PL"))

# Write to disk -----------------------------------------------------------

scag %>%
  arrow::write_feather("data/SCAG_merge.feather")

rm(list = ls())