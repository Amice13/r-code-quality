library(data.table)
library(dtplyr)
library(janitor)
library(tidyverse)

fread(paste0(c("data-raw-temp/corelogic/university_of_",
               "california_irvine_hist_property_basic1_",
               "300000381547686_20220405_072054_data.txt"), 
             collapse = ""),
      select = c("CLIP", "COMPOSITE PROPERTY LINKAGE KEY", "FIPS CODE", 
                 "PARCEL LEVEL LATITUDE", "PARCEL LEVEL LONGITUDE"), 
      colClasses=c(CLIP = "character",
                   `COMPOSITE PROPERTY LINKAGE KEY` = "character",
                   `FIPS CODE` = "character",
                   `PARCEL LEVEL LATITUDE` = "numeric",
                   `PARCEL LEVEL LONGITUDE` = "numeric")) %>% 
      janitor::clean_names() %>% 
      dplyr::filter(fips_code %in% c("06037", "06059", "06065", 
                                     "06071", "06111"))  %>% 
      rename(cl_clip = clip,
             cl_composite_key = composite_property_linkage_key,
             cl_fips = fips_code,
             cl_lat = parcel_level_latitude,
             cl_lon = parcel_level_longitude) %>% 
      as_tibble() %>% 
      write_csv("data/cl_coordinates_scag_study_area.csv")