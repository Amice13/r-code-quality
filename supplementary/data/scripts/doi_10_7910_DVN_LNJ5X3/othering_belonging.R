# Title: Create one Othering & Belonging shapefile with zoning column
# Author: Youjin B. Kim
# Date: May 6, 2022
# Last updated: October 7, 2022

# This script imports all of the Othering & Belonging shapefiles that were cleaned in Python (for overlapping areas), and combines them into one shapefile. 

library(here)
library(dplyr)
library(sf)
library(tidyverse)

file_list <- list.files(here::here("data/bay_ sfr_tl_cl/zoning_pl_intersects/"), pattern = "*_Dissolve.shp$", full.names = TRUE)

read_my_shp <-  function(f){
  st_read(f) %>% 
    st_zm() %>%
    st_transform(., 3857) %>%
    rename_with(tolower) %>% 
    mutate(filename = f) %>%
    mutate(juris = str_extract(filename, "[^/]+$")) %>%
    mutate(juris = str_extract(juris, "[^_]+")) %>%
    dplyr::select(c(juris, zoning))
}

othering_belonging <- map_dfr(file_list, read_my_shp)

st_write(othering_belonging, here::here("data/othering_belonging.shp"))
