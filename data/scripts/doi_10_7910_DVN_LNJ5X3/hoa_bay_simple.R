library(arrow)
library(data.table)
library(dtplyr)
library(here)
library(tidyverse)

# Read in crosswalk of CoreLogic IDs to Bay parcel IDs,
# created by spatially joining points from the CoreLogic data
# to the Bay parcel polygons in ArcGIS Pro

cl_id <- fread(here::here("data/bay_cl_ids.csv"),
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
Bay_SF <- arrow::read_feather("data/Bay_merge.feather") %>% 
#   mutate(
#     SAMP_SF_ZONING_SUPP = case_when(
#       str_sub(PARCEL_ZONINGCODE, 1, 3) == "111" & PARCEL_URBAN == 1 ~ 1,
#       # Residential zoning (type not specified)
#       PARCEL_ZONINGCODE == "1100" & str_sub(PARCEL_USECODE, 1, 3) == "111" & 
#         PARCEL_URBAN == 1 ~ 1,
#       # Mixed Residential zoning
#       PARCEL_ZONINGCODE == "1140" & str_sub(PARCEL_USECODE, 1, 3) == "111" & 
#         PARCEL_URBAN == 1 ~ 1,
#       # Rural Residential zoning
#       PARCEL_ZONINGCODE == "1150" & str_sub(PARCEL_USECODE, 1, 3) == "111" & 
#         PARCEL_URBAN == 1 ~ 1,
#       # Mixed Residential and Commercial
#       PARCEL_ZONINGCODE == "1600" & str_sub(PARCEL_USECODE, 1, 3) == "111" & 
#         PARCEL_URBAN == 1 ~ 1,
#       # Specific Plan
#       PARCEL_ZONINGCODE == "7777" & str_sub(PARCEL_USECODE, 1, 3) == "111" & 
#         PARCEL_URBAN == 1 ~ 1,
#       TRUE ~ 0)) %>% 
  dplyr::filter(PARCEL_ZONING == 1) %>% 
  dplyr::select(PARCEL_ID) %>%
  rename(PARCEL_ID = PARCEL_ID) %>% 
  as.data.table()

cl_sf <- inner_join(cl_id, Bay_SF, by = "PARCEL_ID")

rm(cl_id, Bay_SF)

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
mortgage <- fread(file = paste0(c("data-raw-temp/corelogic/",
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
             quote = "") 

hoa <- mortgage %>% 
  rename(cl_composite_key = `COMPOSITE PROPERTY LINKAGE KEY`) %>% 
  # Classify each property as HOA if any observation for that property is classified as HOA
  mutate(condo = if_else(`CONDOMINIUM RIDER INDICATOR` == "1", 1, 0),
         pud = if_else(`PLANNED UNIT DEVELOPMENT (PUD) RIDER INDICATOR` == "1", 1, 0),
         mf = if_else(`MULTIFAMILY RIDER INDICATOR` == "1", 1, 0),
         hoa_cpm = if_else((pud == 1 | condo == 1 | mf == 1), 1, 0),
         hoa_cp = if_else((pud == 1 | condo == 1), 1, 0)) %>%  
  group_by(cl_composite_key) %>% 
  summarise(hoa_cpm = max(hoa_cpm),
            hoa_cp = max(hoa_cp)) %>%
  as_tibble()

cl_sf <- cl_sf %>% as_tibble() 

bay_hoa <- left_join(cl_sf, hoa, by = "cl_composite_key") %>% 
  mutate(mtg = if_else(!is.na(hoa_cpm), 1, 0),
         hoa_cpm = if_else(is.na(hoa_cpm), 0, hoa_cpm),
         hoa_cp = if_else(is.na(hoa_cp), 0, hoa_cp)) %>% 
  group_by(PARCEL_ID) %>% 
  summarise(mtg = max(mtg), 
            hoa_cpm = max(hoa_cpm),
            hoa_cp = max(hoa_cp)) %>% 
  ungroup() %>%
  as_tibble()

bay_hoa %>% arrow::write_feather("data/hoa_bay_simple.feather")

rm(bay_hoa, hoa, cl_sf)
## A tibble: 3 × 3
## Groups:   mtg [2]
#     mtg   hoa   `n()`
#     <dbl> <dbl>   <int>
#   1     0     0  745672
#   2     1     0 1569319
#   3     1     1  324518


# Results from hoa_socal.qmd 
# hoa %>% 
#   group_by(hoa) %>% 
#   summarise(n())
# hoa   `n()`
# <dbl>   <int>
# 1    -1  356776
# 2     0 2091616
# 3     1   54558