library(data.table)
library(dtplyr)
library(janitor)
library(tidyverse)

cl_coordinates_bay_study_area <- fread(paste0(c("data-raw-temp/corelogic/university_of_",
               "california_irvine_hist_property_basic1_",
               "300000381547686_20220405_072054_data/", 
               "university_of_",
               "california_irvine_hist_property_basic1_",
               "300000381547686_20220405_072054_data.txt"), 
             collapse = ""),
      select = c("CLIP", "COMPOSITE PROPERTY LINKAGE KEY", "FIPS CODE", 
                 "PARCEL LEVEL LATITUDE", "PARCEL LEVEL LONGITUDE"), 
      colClasses=c(CLIP = "character",
                   `COMPOSITE PROPERTY LINKAGE KEY` = "character",
                   `FIPS CODE` = "character",
                   `PARCEL LEVEL LATITUDE` = "numeric",
                   `PARCEL LEVEL LONGITUDE` = "numeric"),
      na.strings = "NA") %>% 
  janitor::clean_names() %>% 
  dplyr::filter(fips_code %in% c("06085", "06097", "06081", "06013",
                                 "06001", "06075", "06095", "06041",
                                 "06055")) %>% 
  rename(cl_clip = clip,
         cl_composite_key = composite_property_linkage_key,
         cl_fips = fips_code,
         cl_lat = parcel_level_latitude,
         cl_lon = parcel_level_longitude) %>%
  mutate(cl_lat = as.numeric(cl_lat),
         cl_lon = as.numeric(cl_lon)) %>%
  as_tibble() 

write_csv(cl_coordinates_bay_study_area, "data/cl_coordinates_bay_study_area.csv")

rm(cl_coordinates_bay_study_area)