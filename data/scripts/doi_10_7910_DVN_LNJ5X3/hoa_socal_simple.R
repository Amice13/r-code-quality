library(arrow)
library(data.table)
library(dtplyr)
library(here)
library(tidyverse)

# Read in crosswalk of CoreLogic IDs to SCAG parcel IDs,
# created by spatially joining points from the CoreLogic data
# to the SCAG parcel polygons in ArcGIS Pro

cl_id <- fread(here::here("data/scag_cl_ids.csv"),
               sep = ",",
               header = TRUE,
               skip = 0,
               select = list(
                 character = 5,
                 character = 7,
                 numeric = 9,
                 numeric = 10
               )) 

# Get IDs for single-family parcels in study area
SCAG_SF <- arrow::read_feather("data/SCAG_merge.feather") %>% 
  mutate(
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
                  str_sub(PARCEL_USECODE,1, 2) == "11") %>% 
  dplyr::select(PARCEL_SCAGUID16) %>%
  rename(SCAGUID16 = PARCEL_SCAGUID16) %>% 
  as.data.table()

cl_sf <- inner_join(cl_id, SCAG_SF, by = "SCAGUID16")

rm(cl_id, SCAG_SF)

# Read from property table to get subdivision names
prop_f <- fread(file = paste0(c("data-raw-temp/corelogic/",
                                "university_of_california_irvine_hist_property",
                                "_basic1_300000381547686_20220405_072054_data.txt"),
                              collapse = ""), 
                sep = "|",
                header = TRUE,
                skip = 0,
                select = list(character = 3,
                              character = 6,
                              character = 34 , 
                              numeric = 48 , 
                              numeric = 49 , 
                              character = 71 , 
                              character = 74 , 
                              numeric = 144, 
                              integer = 147)) %>% 
  rename(cl_composite_key = `COMPOSITE PROPERTY LINKAGE KEY`)


# Subset prop_f to parcels zoned for single-family 
cl_sf <- left_join(cl_sf, prop_f, by = "cl_composite_key")

rm(prop_f)

# Read from mortgage table to get PUD and condo riders
hoa <- fread(file = paste0(c("data-raw-temp/corelogic/",
                             "university_of_california_irvine_mortgage",
                             "_basic3_300000411675197_20220802_120949_data.txt"),
                           collapse = ""), 
             sep = "|",
             header = TRUE,
             skip = 0,
             select = list(character = 6,
                           character = 48, 
                           character = 75,
                           character = 76,
                           character = 77),
             quote = "") %>% 
  rename(cl_composite_key = `COMPOSITE PROPERTY LINKAGE KEY`) %>% 
  # Classify each property as HOA if any observation for that property is classified as HOA
  mutate(condo = if_else(`CONDOMINIUM RIDER INDICATOR` == "1", 1, 0),
         pud = if_else(`PLANNED UNIT DEVELOPMENT (PUD) RIDER INDICATOR` == "1", 1, 0),
         mf = if_else(`MULTIFAMILY RIDER INDICATOR` == "1", 1, 0),
         hoa_cpm = if_else((pud == 1 | condo == 1 | mf == 1), 1, 0),
         hoa_cp = if_else((pud == 1 | condo == 1), 1, 0)) %>% 
  group_by(cl_composite_key) %>% 
  summarise(hoa_cpm = max(hoa_cpm),
            hoa_cp = max(hoa_cp))

scag_hoa <- left_join(cl_sf, hoa, by = "cl_composite_key") %>% 
  mutate(mtg = if_else(!is.na(hoa_cpm), 1, 0),
         hoa_cpm = if_else(is.na(hoa_cpm), 0, hoa_cpm),
         hoa_cp = if_else(is.na(hoa_cp), 0, hoa_cp)) %>% 
  group_by(SCAGUID16) %>% 
  summarise(mtg = max(mtg), 
            hoa_cpm = max(hoa_cpm),
            hoa_cp = max(hoa_cp)) %>% 
  ungroup() %>% 
  as_tibble()

scag_hoa %>% 
  group_by(mtg, hoa_cpm) %>% 
  summarise(n())

scag_hoa %>% 
  group_by(mtg, hoa_cp) %>% 
  summarise(n())

scag_hoa %>% arrow::write_feather("data/hoa_socal_simple.feather")