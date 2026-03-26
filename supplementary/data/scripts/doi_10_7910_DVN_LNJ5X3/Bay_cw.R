library(arrow)
library(assertr)
library(data.table)
library(dtplyr)
library(tidyverse)
library(here)

# Select needed columns from Boundary Solutions shapefiles & combine -----------
read_bayparcels <- function(f){
  fread(f,
        select = c("PARCEL_ID", "APN", "APN2", "COUNTY", "SIT_CITY", "ACRES"),
        colClasses=c(PARCEL_ID = "character",
                     APN = "character",
                     APN2 = "character",
                     COUNTY = "character",
                     SIT_CITY = "character",
                     ACRES = "numeric")) %>% 
    mutate(CITY = toupper(SIT_CITY)) %>% 
    rename(BS_CITY = CITY) %>% 
    select(-c(SIT_CITY)) %>%
    as_tibble()
}

# These files were created in ArcGIS Pro using the commands included in the following script:
# scripts/bay_parcels_clean.py

file_names <- list.files(here::here("data/bay_ sfr_tl_cl/boundary_solutions"), pattern="*.csv$")
file_list <- paste0("data/bay_ sfr_tl_cl/boundary_solutions/", file_names)

bay_parcels <- map_dfr(file_list, read_bayparcels) %>%
  assertr::verify(anyDuplicated(PARCEL_ID) == FALSE) %>% 
  as.data.table() %>% 
  mutate(bay_parcel_id = TRUE)

rm(file_names, file_list, read_bayparcels)

# Assign tract IDs to PARCEL IDs --------------------------------------------
parcel_to_trt <- function(f){
  fread(f,
        select = c("PARCEL_ID", "GEOID", "trt_par_area_sqft"),
        colClasses=c(PARCEL_ID = "character",
                     GEOID = "character",
                     trt_par_area_sqft = "numeric")) %>%
    dplyr::filter(!is.na(GEOID)) %>% 
    dplyr::filter(!is.na(PARCEL_ID)) %>% 
    dplyr::filter(PARCEL_ID != "") %>% 
    group_by(PARCEL_ID) %>% 
    dplyr::slice(which.max(trt_par_area_sqft)) %>% 
    ungroup() %>% 
    dplyr::select(-trt_par_area_sqft) %>% 
    as_tibble()
}

# These files were created in ArcGIS Pro using the commands included in the following script:
# scripts/bay_parcel_to_trt.py

file_names <- list.files(here::here("data/bay_ sfr_tl_cl/parcel_to_trt"), pattern="*.csv$")
file_list <- paste0("data/bay_ sfr_tl_cl/parcel_to_trt/", file_names)

bay_trt <-  map_dfr(file_list, parcel_to_trt) %>%
  assertr::verify(anyDuplicated(PARCEL_ID) == FALSE) %>% 
  as.data.table() %>% 
  mutate(bay_trt_id = TRUE)

bay_parcels <- full_join(bay_parcels, bay_trt, by = "PARCEL_ID") %>%
  mutate(
    merge = case_when(
      bay_parcel_id & bay_trt_id ~ "both",
      bay_parcel_id ~ "bay_parcel",
      bay_trt_id ~ "bay_trt"
    ),
    .keep = "unused"
  ) %>% 
  as_tibble()

# Zero unmerged observations
bay_parcels %>% 
  dplyr::filter(merge != "both") %>% nrow()

# Drop blank tracts
bay_parcels %>% dplyr::filter(GEOID == "") %>% nrow()

bay_parcels <- bay_parcels %>%
  assertr::verify(merge == "both") %>%
  dplyr::select(-merge) %>%
  assertr::verify(nrow(bay_parcels %>% dplyr::filter(GEOID == "")) == 68) %>%
  dplyr::filter(GEOID != "") %>%
  rename(TRT_2012 = GEOID) %>% 
  mutate(bay_parcel_id = TRUE)

rm(bay_trt, file_names, file_list, parcel_to_trt)

# Assign place FIPS IDs to Bay parcels -----------------------------------

# bay_parcels <- bay_parcels %>%
#   assertr::verify(!is.na(BS_CITY)) %>%
# #  assertr::verify(BS_CITY != "") %>%
# ## Code stops here because there are missing City names in Boundary Solutions data
#   assertr::verify(bay_parcel_id == TRUE) %>%
#   mutate(SCM = toupper(BS_CITY)) %>% 
#   mutate(SCM = case_when(
#     SCM == "S SAN FRAN" ~ "SOUTH SAN FRANCISCO",
#     SCM == "SAINT HELENA" ~ "ST. HELENA",
#     TRUE ~ SCM))
## Note sure what SCM stands for

bay_parcels %>%
  dplyr::filter(BS_CITY == "") %>% nrow()
# 306642 missing city names

parcel_to_place <- function(f){
  fread(f,
        select = c("PARCEL_ID", "NAMELSAD", "PLACEFP", "COUNTYFP", "place_par_area_sqft"),
        colClasses=c(PARCEL_ID = "character",
                     NAMELSAD = "character",
                     PLACEFP = "character",
                     COUNTYFP = "character",
                     place_par_area_sqft = "numeric")) %>%
    dplyr::filter(!is.na(NAMELSAD)) %>% 
    dplyr::filter(!is.na(PARCEL_ID)) %>% 
    dplyr::filter(PARCEL_ID != "") %>% 
    group_by(PARCEL_ID) %>% 
    dplyr::slice(which.max(place_par_area_sqft)) %>% 
    ungroup() %>% 
    dplyr::select(-place_par_area_sqft) %>% 
    as_tibble()
}

# These files were created in ArcGIS Pro using the commands included in the following script:
# scripts/places_in_bay_study_area.py

file_names <- list.files(here::here("data/bay_ sfr_tl_cl/parcel_to_place"), pattern="*.csv$")
file_list <- paste0("data/bay_ sfr_tl_cl/parcel_to_place/", file_names)

bay_places <-  map_dfr(file_list, parcel_to_place) %>%
  assertr::verify(anyDuplicated(PARCEL_ID) == FALSE) %>% 
  as.data.table() %>% 
  mutate(bay_place_id = TRUE)

bay_parcels <- as.data.table(bay_parcels)
bay_places <- as.data.table(bay_places)

# Merge on Parcel ID to get FIPS IDs
bay_parcels <- full_join(bay_parcels, bay_places, by = "PARCEL_ID") %>%
  mutate(
    merge = case_when(
      bay_parcel_id & bay_place_id ~ "both",
      bay_parcel_id ~ "bay_parcel_id",
      bay_place_id ~ "census"
    ),
    .keep = "unused"
  )

bay_parcels <- as_tibble(bay_parcels)

# Drop blank place names
drop <- bay_parcels %>% dplyr::filter(NAMELSAD == "") %>% nrow()

bay_parcels <- bay_parcels %>%
  assertr::verify(nrow(bay_parcels %>% dplyr::filter(NAMELSAD == "")) == drop) %>%
  dplyr::filter(NAMELSAD != "") %>%
  assertr::verify(merge == "both") %>%
  dplyr::select(-merge) %>%
  mutate(bay_parcel_id = TRUE)

rm(bay_places, file_names, file_list, parcel_to_place, drop)

# Assign urban area status to parcels -------------------------------------

ua_in <- function(f) {
  fread(f,
        select = "PARCEL_ID",
        colClasses = c(PARCEL_ID = "character")) %>%
    as_tibble()
}

file_names <- list.files(here::here("data/bay_ sfr_tl_cl/ua_parcel"), pattern="*.csv$")
file_list <- paste0("data/bay_ sfr_tl_cl/ua_parcel/", file_names)

ua <- map_dfr(file_list, ua_in) %>%
  mutate(ua_id = TRUE)

bay_parcels <- full_join(bay_parcels, ua, by = "PARCEL_ID") %>%
  mutate(
    merge = case_when(
      bay_parcel_id & ua_id ~ "both",
      bay_parcel_id ~ "bay_parcel",
      ua_id ~ "ua"
    ),
    .keep = "unused"
  )

bay_parcels %>% dplyr::filter(merge == "ua") %>% nrow()

bay_parcels <- bay_parcels %>%
  assertr::verify(bay_parcels %>% dplyr::filter(merge == "ua") %>% nrow() == 0) %>%
  # All parcels merged from ua
  assertr::verify(merge != "ua") %>%
  # Generate indicator variables for urban areas
  mutate(URBAN = if_else(merge == "both", 1, 0)) %>%
  dplyr::select(-merge) %>%
  assertr::verify(anyDuplicated(PARCEL_ID) == FALSE)

# rename/reorder columns for clarity
bay_parcels <- bay_parcels %>%
  rename(BS_APN = APN,
         BS_APN2 = APN2,
         BS_COUNTY = COUNTY,
         PARCEL_SIZE = ACRES,
         TL_NAME = NAMELSAD) %>%
  # assertr::verify(nrow(PLACEFP != "" & COUNTYFP != "") == 0) %>%
  # assertr::verify(nrow(PLACEFP == "" & COUNTYFP == "") == 0) %>%
  mutate(PLACEFP = case_when(PLACEFP == "" ~ COUNTYFP,
                             TRUE ~ PLACEFP)) %>%
  mutate(COUNTYFIPS = str_sub(as.character(PARCEL_ID), 3, 5)) %>%
  mutate(BSFILE_COUNTY = case_when(
    COUNTYFIPS == "001" ~ "Alameda",
    COUNTYFIPS == "013" ~ "Contra Costa",
    COUNTYFIPS == "041" ~ "Marin",
    COUNTYFIPS == "055" ~ "Napa",
    COUNTYFIPS == "075" ~ "San Francisco",
    COUNTYFIPS == "081" ~ "San Mateo",
    COUNTYFIPS == "085" ~ "Santa Clara",
    COUNTYFIPS == "095" ~ "Solano",
    COUNTYFIPS == "097" ~ "Sonoma",
    TRUE ~ COUNTYFIPS)) %>%
  mutate(TL_NAME = toupper(TL_NAME),
         BSFILE_COUNTY = toupper(BSFILE_COUNTY),
         BS_COUNTY = toupper(BS_COUNTY)) %>%
  select(-c(COUNTYFIPS, COUNTYFP)) %>%
  select(PARCEL_ID, BS_APN, BS_APN2, BSFILE_COUNTY, BS_COUNTY, BS_CITY, TL_NAME, PLACEFP, TRT_2012, PARCEL_SIZE, URBAN)
                    
rm(ua, file_list, file_names, ua_in)

# Write to disk -----------------------------------------------------------

bay_parcels %>% arrow::write_feather("data/Bay_cw.feather")