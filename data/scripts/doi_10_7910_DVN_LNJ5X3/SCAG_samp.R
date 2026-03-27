library(arrow)
library(assertr)
library(data.table)
library(dtplyr)
library(tidyverse)

# Load,  check,  and clean building footprint data ------------------------

par_str <- fread("data/scag_building_footprints_2018.csv",
                 select = c("SCAGUID16", "perimeter_ft", "bldgfpID",
                            "FID_scag_study_area_fp",
                            "bldg_sqft", "union_sqft"),
                 colClasses=c(SCAGUID16 = "character",
                              perimeter_ft = "numeric",
                              bldgfpID = "character",
                              FID_scag_study_area_fp = "numeric",
                              bldg_sqft = "numeric",
                              union_sqft = "numeric")) 

par_str %>%
  dplyr::select(FID_scag_study_area_fp, bldgfpID) %>% 
  dplyr::filter(FID_scag_study_area_fp == -1) %>%
  as_tibble() %>%
  assertr::verify(bldgfpID == "0")

par_str %>%
  dplyr::select(FID_scag_study_area_fp, bldgfpID) %>% 
  dplyr::filter(bldgfpID == "0") %>%
  as_tibble() %>%
  assertr::verify(FID_scag_study_area_fp == -1)

par_str %>%
  dplyr::select(FID_scag_study_area_fp, bldgfpID) %>% 
  dplyr::filter(bldgfpID != "0") %>%
  as_tibble() %>%
  assertr::verify(FID_scag_study_area_fp != -1)

par_str %>% 
  dplyr::select(union_sqft) %>% 
  as_tibble() %>% 
  assertr::verify(!is.na(union_sqft))

par_str <- par_str %>%
  dplyr::select(-FID_scag_study_area_fp) %>% 
  # Remove parcel areas without buildings
  dplyr::filter(bldgfpID != "0") %>% 
  # Remove building areas not associated with parcels
  dplyr::filter(SCAGUID16 != "") %>% 
  # Ensure that each parcel-building combination has only one square footage value
  group_by(SCAGUID16, perimeter_ft, bldgfpID, bldg_sqft) %>% 
  summarise(union_sqft = sum(union_sqft)) %>% 
  ungroup() %>% 
  # Collapse to parcels  
  group_by(SCAGUID16, perimeter_ft) %>% 
  summarise(bldg_fp_sqft = sum(union_sqft),
            buildings = n()) %>% 
  ungroup() %>% 
  mutate(bldg_id = TRUE)

par_str %>%
  dplyr::select(SCAGUID16) %>% 
  as_tibble() %>%
  assertr::verify(anyDuplicated(SCAGUID16) == FALSE)


# Load and clean merged SCAG parcel data ----------------------------------

SCAG_merge <- arrow::read_feather("data/SCAG_merge.feather") %>% 
  mutate(
    sm_id = TRUE,
    SAMP_SF_ZONING_SUPP = case_when(
      str_sub(PARCEL_ZONINGCODE, 1, 3) == "111" & PARCEL_URBAN == 1 ~ 1,
      # Residential zoning (type not specified)
      PARCEL_ZONINGCODE == "1100" & str_sub(PARCEL_USECODE, 1, 3) == "111" & 
        PARCEL_URBAN == 1 ~ 1,
      # Mixed Residential zoning
      PARCEL_ZONINGCODE == "1140" & str_sub(PARCEL_USECODE, 1, 3) == "111" & 
        PARCEL_URBAN == 1 ~ 1,
      # Rural Residential zoning
      PARCEL_ZONINGCODE == "1150" & str_sub(PARCEL_USECODE, 1, 3) == "111" & 
        PARCEL_URBAN == 1 ~ 1,
      # Mixed Residential and Commercial
      PARCEL_ZONINGCODE == "1600" & str_sub(PARCEL_USECODE, 1, 3) == "111" & 
        PARCEL_URBAN == 1 ~ 1,
      # Specific Plan
      PARCEL_ZONINGCODE == "7777" & str_sub(PARCEL_USECODE, 1, 3) == "111" & 
        PARCEL_URBAN == 1 ~ 1,
      TRUE ~ 0)) %>% 
  dplyr::filter(SAMP_SF_ZONING_SUPP == 1 & 
                  PARCEL_ACRES < 2 &
                  str_sub(PARCEL_USECODE,1, 2) == "11") %>% 
  dplyr::select(-SAMP_SF_ZONING_SUPP) %>% 
  as.data.table()



# Combine datasets,  calculate buildable area,  and filter ----------------

scag_samp <- inner_join(SCAG_merge, par_str,
          by = c("PARCEL_SCAGUID16" = "SCAGUID16")) %>% 
  # Keep only parcels with one to four buildings
  dplyr::filter(buildings < 5) %>% 
  dplyr::select(-c(sm_id, bldg_id)) %>% 
  dplyr::rename(PARCEL_perimeter_ft = perimeter_ft,
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
  # Mark parcels with one building and more than 150 sq. ft. buildable area
  # Local agencies must not establish a minimum sq. footage requirement that prohibits an efficiency unit as defined by Cal. Health & Safety Code s. 17958.1. Floor area of such a unit is 150 sq. ft. Exclude parcels with more than one building, since the accessory structure can be converted to an ADU
  mutate(buildable = 
           if_else((PARCEL_buildings == 1 & PARCEL_buildable_area >= 150) |
                     (PARCEL_buildings > 1 & PARCEL_buildable_area > 0),
                   1, 0)) %>% 
  as_tibble()

rm(par_str, SCAG_merge)

# Drop tracts with missing rent data --------------------------------------

scag_samp %>% 
  dplyr::filter(is.na(TRT_rent_i)) %>% 
  assertr::verify(TRT_2012 %in% c("06037980023", "06037980021", "06037402404"))


scag_samp <- scag_samp %>% 
  dplyr::filter(!is.na(TRT_rent_i))


# Import HOA data ---------------------------------------------------------

hoa <- arrow::read_feather("data/hoa_socal_simple.feather") %>% 
  rename(PARCEL_SCAGUID16 = SCAGUID16,
         PARCEL_mtg = mtg,
         PARCEL_hoa_cpm = hoa_cpm,
         PARCEL_hoa_cp = hoa_cp)

scag_samp <- statar::join(scag_samp, hoa, 
                     on = "PARCEL_SCAGUID16",
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

# Export dataset ----------------------------------------------------------

scag_samp %>% write_feather("data/SCAG_samp.feather")

rm(list = ls())