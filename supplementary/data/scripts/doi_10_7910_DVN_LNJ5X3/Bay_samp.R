library(arrow)
library(assertr)
library(data.table)
library(dtplyr)
library(tidyverse)

# Load,  check,  and clean building footprint data ------------------------
par_str <- fread("data/bay_building_footprints_2018.csv",
                 select = c("PARCEL_ID", "perimter_ft", "bldgfpID",
                            "FID_bay_study_area_fp",
                            "bldg_sqft", "union_sqft"),
                 colClasses=c(PARCEL_ID = "character",
                              perimter_ft = "numeric",
                              bldgfpID = "character",
                              FID_bay_study_area_fp = "numeric",
                              bldg_sqft = "numeric",
                              union_sqft = "numeric")) 

# par_str %>%
#   dplyr::select(FID_bay_study_area_fp, bldgfpID) %>% 
#   dplyr::filter(FID_bay_study_area_fp == -1) %>%
#   as_tibble() %>%
#   assertr::verify(bldgfpID == "0")
# 
# par_str %>%
#   dplyr::select(FID_bay_study_area_fp, bldgfpID) %>% 
#   dplyr::filter(bldgfpID == "0") %>%
#   as_tibble() %>%
#   assertr::verify(FID_bay_study_area_fp == -1)
# 
# par_str %>%
#   dplyr::select(FID_bay_study_area_fp, bldgfpID) %>% 
#   dplyr::filter(bldgfpID != "0") %>%
#   as_tibble() %>%
#   assertr::verify(FID_bay_study_area_fp != -1)
# 
# par_str %>% 
#   dplyr::select(union_sqft) %>% 
#   as_tibble() %>% 
#   assertr::verify(!is.na(union_sqft))

par_str <- par_str %>%
  dplyr::select(-FID_bay_study_area_fp) %>% 
  # Remove parcel areas without buildings
  dplyr::filter(bldgfpID != "0") %>% 
  # Remove building areas not associated with parcels
  dplyr::filter(PARCEL_ID != "") %>% 
  # Ensure that each parcel-building combination has only one square footage value
  group_by(PARCEL_ID, perimter_ft, bldgfpID, bldg_sqft) %>% 
  summarise(union_sqft = sum(union_sqft)) %>% 
  ungroup() %>% 
  # Collapse to parcels  
  group_by(PARCEL_ID, perimter_ft) %>% 
  summarise(bldg_fp_sqft = sum(union_sqft),
            buildings = n()) %>% 
  ungroup() %>% 
  mutate(bldg_id = TRUE)

# par_str %>%
#   dplyr::select(PARCEL_ID) %>% 
#   as_tibble() %>%
#   assertr::verify(anyDuplicated(PARCEL_ID) == FALSE)

# Load merged Bay parcel data-------------------------------------------------
Bay_merge <- arrow::read_feather("data/Bay_merge.feather") %>% 
  mutate(sm_id = TRUE) %>% 
  dplyr::filter(PARCEL_URBAN == 1 & PARCEL_ZONING == 1 & PARCEL_ACRES < 2) %>% 
# Select SFRs in urban areas < 2 acres
  as.data.table()

# Combine datasets,  calculate buildable area, and filter ----------------
bay_samp <- inner_join(Bay_merge, par_str, by = "PARCEL_ID") %>% 
  # Keep only parcels with one to four buildings
  dplyr::filter(buildings < 5) %>% 
  dplyr::select(-c(sm_id, bldg_id)) %>% 
  dplyr::rename(PARCEL_perimeter_ft = perimter_ft,
                PARCEL_bldg_fp_sqft = bldg_fp_sqft,
                PARCEL_buildings = buildings) %>% 
  # Compute buildable area (1 acre = 43560 sq. ft.)
  mutate(PARCEL_sqft = PARCEL_ACRES * 43560,
         PARCEL_buildable_area = 
           case_when(
             PARCEL_buildings == 1 ~ 
               PARCEL_sqft - (PARCEL_bldg_fp_sqft + 600 + 
                                (PARCEL_perimeter_ft * 0.75 * 4 - 32) + 200),
             PARCEL_buildings > 1 ~ 
               PARCEL_sqft - (PARCEL_bldg_fp_sqft + 600 + 200)
           )) %>% 
  # Mark parcels with one building and less than 150 sq. ft. buildable area
  # of buildable area. Local agencies must not establish a minimum sq. footage requirement that prohibits an efficiency unit as defined by Cal. Health & Safety Code s. 17958.1. Floor area of such a unit is 150 sq. ft. Exclude parcels with more than one building, since the accessory structure can be converted to an ADU
  mutate(buildable = 
           if_else((PARCEL_buildings == 1 & PARCEL_buildable_area >= 150) |
                     (PARCEL_buildings > 1 & PARCEL_buildable_area > 0),
                   1, 0)) %>% 
  as_tibble()

rm(par_str, Bay_merge)

# Drop tracts with missing rent data --------------------------------------

# bay_samp %>% 
#   dplyr::filter(is.na(TRT_rent_i)) %>% 
#   select(TRT_2012) %>%
#   pull()
# 
# bay_samp %>% 
#   dplyr::filter(is.na(TRT_rent_i)) %>% 
#   nrow()

bay_samp <- bay_samp %>% 
  dplyr::filter(!is.na(TRT_rent_i))

# Import HOA data ---------------------------------------------------------
hoa <- arrow::read_feather("data/hoa_bay_simple.feather") %>%
  rename(PARCEL_mtg = mtg,
         PARCEL_hoa_cpm = hoa_cpm,
         PARCEL_hoa_cp = hoa_cp)

bay_samp <- statar::join(bay_samp, hoa,
                     on = "PARCEL_ID",
                     kind = "full",
                     check = 1 ~ 1,
                     gen = "merge") %>%
  assertr::verify(merge != 1) %>%
  dplyr::filter(merge == 3) %>%
  dplyr::select(-merge) %>%
  # Create city-level measures of HOA intensity
  group_by(PL_FIPS_2012) %>%
  mutate(PL_hoa_cpm = 100*sum(PARCEL_hoa_cpm)/sum(PARCEL_mtg),
         PL_hoa_cp = 100*sum(PARCEL_hoa_cp)/sum(PARCEL_mtg)) %>%
  ungroup()


# Restrict to single-family parcels ---------------------------------------

bay_samp <- bay_samp %>% 
  dplyr::filter(PARCEL_ZONING == 1)

# Export dataset ----------------------------------------------------------
bay_samp %>% write_feather("data/bay_samp.feather")

rm(list = ls())
