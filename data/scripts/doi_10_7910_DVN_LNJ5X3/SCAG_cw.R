library(arrow)
library(assertr)
library(data.table)
library(dtplyr)
library(tidyverse)


# Select needed columns from SCAG 2016 land use files & combine -----------

read_scag <- function(f){
  fread(f,
        select = c("SCAGUID16", "APN", "COUNTY", "CITY", "ACRES"),
        colClasses=c(SCAGUID16 = "character",
                     APN = "character",
                     COUNTY = "character",
                     CITY = "character",
                     ACRES = "numeric")) %>% 
    mutate(CITY = toupper(CITY),
           APN2 = str_remove_all(str_squish(APN), "^0+|[-]|\\s")) %>% 
    rename(SCAG_CITY = CITY) %>% 
    as_tibble()
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
  as.data.table() %>% 
  mutate(scag_lu_id = TRUE)

rm(file_list, read_scag)


# Assign tract IDs to SCAG IDs --------------------------------------------

parcel_to_trt <- function(f){
  fread(f,
        select = c("SCAGUID16", "GEOID", "trt_par_area_sqft"),
        colClasses=c(SCAGUID16 = "character",
                     GEOID = "character",
                     trt_par_area_sqft = "numeric")) %>%
    dplyr::filter(!is.na(GEOID)) %>% 
    dplyr::filter(!is.na(SCAGUID16)) %>% 
    dplyr::filter(SCAGUID16 != "") %>% 
    group_by(SCAGUID16) %>% 
    dplyr::slice(which.max(trt_par_area_sqft)) %>% 
    ungroup() %>% 
    dplyr::select(-trt_par_area_sqft) %>% 
    as_tibble()
}

file_list <- c("data/scag_parcel_to_trt/fp06037_parcel_to_trt.csv",
               "data/scag_parcel_to_trt/fp06059_parcel_to_trt.csv",
               "data/scag_parcel_to_trt/fp06065_parcel_to_trt.csv",
               "data/scag_parcel_to_trt/fp06071_parcel_to_trt.csv",
               "data/scag_parcel_to_trt/fp06111_parcel_to_trt.csv")

# These files were created in ArcGIS Pro using the commands included in the following scripts:
# scripts/cleaning/scag_parcel_to_trt/fp06037_parcel_to_trt.py
# scripts/cleaning/scag_parcel_to_trt/fp06059_parcel_to_trt.py
# scripts/cleaning/scag_parcel_to_trt/fp06065_parcel_to_trt.py
# scripts/cleaning/scag_parcel_to_trt/fp06071_parcel_to_trt.py
# scripts/cleaning/scag_parcel_to_trt/fp06111_parcel_to_trt.py

scag_trt <-  map_dfr(file_list, parcel_to_trt) %>% 
  assertr::verify(anyDuplicated(SCAGUID16) == FALSE) %>% 
  as.data.table() %>% 
  mutate(scag_trt_id = TRUE)

scag_lu <- full_join(scag_lu, scag_trt, by = "SCAGUID16") %>%
  mutate(
    merge = case_when(
      scag_lu_id & scag_trt_id ~ "both",
      scag_lu_id ~ "scag_lu",
      scag_trt_id ~ "scag_trt"
    ),
    .keep = "unused"
  ) %>% 
  as_tibble()

# Seven unmerged observations all < 0.00001 acres
scag_lu %>% 
  assertr::verify(merge != "scag_trt") %>% 
  dplyr::filter(merge == "scag_lu") %>%
  assertr::verify(ACRES < 0.00001) %>% 
  assertr::verify(nrow(
    scag_lu %>% dplyr::filter(merge == "scag_lu")) == 7)

# Drop unmerged observations & 10 blank tracts
scag_lu <- scag_lu %>% 
  dplyr::filter(merge == "both") %>% 
  dplyr::select(-merge) %>% 
  assertr::verify(nrow(scag_lu %>% dplyr::filter(GEOID == "")) == 10) %>% 
  dplyr::filter(GEOID != "") %>% 
  rename(TRT_2012 = GEOID) 

rm(scag_trt, file_list, parcel_to_trt)


# Assign place FIPS IDs to SCAG parcels -----------------------------------

scag_lu <- scag_lu %>% 
  assertr::verify(!is.na(SCAG_CITY)) %>% 
  assertr::verify(SCAG_CITY != "") %>% 
  mutate(SCM = toupper(SCAG_CITY),
         scag_lu_id = TRUE)

# places_in_scag_study_area.csv was created in ArcGIS Pro using the commands included in the following script:
# scripts/cleaning/places_in_scag_study_area.py

scag_cities <- read_csv("data/places_in_scag_study_area.csv",
                        col_types = cols(.default = "c")) %>% 
  dplyr::filter(str_sub(CLASSFP, 1, 1) == "C") %>% 
  dplyr::select(PLACEFP, NAME) %>%
  rename(PLACEFP_2012 = PLACEFP,
         CENSUS_PLACE_NAME = NAME) %>% 
  mutate(SCM = toupper(CENSUS_PLACE_NAME)) %>% 
  mutate(SCM = case_when(
    SCM == "LA CA0ADA FLINTRIDGE" ~ "LA CANADA FLINTRIDGE",
    SCM == "SAN BUENAVENTURA (VENTURA)" ~ "SAN BUENAVENTURA",
    TRUE ~ SCM)) %>% 
  mutate(census_id = TRUE)

# Merge on name to get FIPS IDs
scag_lu <-   full_join(scag_lu, scag_cities, by = "SCM") %>%
  mutate(
    merge = case_when(
      scag_lu_id & census_id ~ "both",
      scag_lu_id ~ "scag_lu",
      census_id ~ "census"
    ),
    .keep = "unused"
  ) %>% 
  # All census IDs were merged
  assertr::verify(merge != "census") %>% 
  dplyr::select(-c(SCM, merge, CENSUS_PLACE_NAME)) %>% 
  mutate(scag_lu_id = TRUE)

rm(scag_cities)


# Assign urban area status to parcels -------------------------------------

ua_in <- function(f) {
  fread(f,
        select = "SCAGUID16",
        colClasses = c(SCAGUID16 = "character")) %>% 
    as_tibble()
}


file_list <- c("data/scag_ua_parcels/fp06037_ua_parcels.csv",
               "data/scag_ua_parcels/fp06059_ua_parcels.csv",
               "data/scag_ua_parcels/fp06065_ua_parcels.csv",
               "data/scag_ua_parcels/fp06071_ua_parcels.csv",
               "data/scag_ua_parcels/fp06111_ua_parcels.csv")

ua <- map_dfr(file_list, ua_in) %>% 
  # Fix SCAGUID16 for Orange County & generate merge ID
  mutate(SCAGUID16 = if_else(str_sub(SCAGUID16, 1, 2) == "59",
                             paste0("0", SCAGUID16), 
                             SCAGUID16),
         ua_id = TRUE)

scag_lu <- full_join(scag_lu, ua, by = "SCAGUID16") %>%
  mutate(
    merge = case_when(
      scag_lu_id & ua_id ~ "both",
      scag_lu_id ~ "scag_lu",
      ua_id ~ "ua"
    ),
    .keep = "unused"
  ) 

scag_lu <- scag_lu %>% 
  # Only four parcels unmerged from ua
  assertr::verify(nrow(
    scag_lu %>% dplyr::filter(merge == "ua")) == 4) %>% 
  # Drop these four parcels
  dplyr::filter(merge != "ua") %>% 
  # Generate indicator variables for urban areas
  mutate(URBAN = if_else(merge == "both", 1, 0)) %>% 
  dplyr::select(-merge) %>% 
  assertr::verify(anyDuplicated(SCAGUID16) == FALSE) 

rm(ua, file_list, ua_in)

# Write to disk -----------------------------------------------------------

scag_lu %>% arrow::write_feather("data/SCAG_cw.feather")