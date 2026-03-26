#####################################################################################################
###### SETUP
#####################################################################################################

## set working directory to folder containing downloaded replication repository
## UNCOMMENT the line below and insert path for your working directory
# setwd("YOUR WORKING DIRECTORY HERE") 

### source base script for functions, paths, etc.
source("base.R")

### packages
library(tigris)
library(sf)
library(tidyverse)

#####################################################################################################
###### PREP TIGER/LINE DATA 
# see section e2.3
#####################################################################################################

### ZCTA/MODZCTA crosswalk (see section e2.1)
modzcta <- read_csv(scratch$modzcta, col_types = cols(ZCTA = col_character(), 
                                                      MODZCTA = col_character())) %>%
        rename_with(tolower)

### city boundaries
cities <- tigris::places(scratch$states, year = 2019) %>%
        filter(GEOID %in% paste0(scratch$geoids$state, scratch$geoids$place)) %>%
        rename(place = GEOID, state = STATEFP) %>%
        mutate(city = factor(NAME, levels = scratch$cities)) %>%
        dplyr::select(geometry, place, city) %>%
        arrange(city)

### identify counties that intersect cities
counties <- tigris::counties(state = scratch$states, year = 2019)[cities, ] %>%
        dplyr::select(STATEFP, COUNTYFP, GEOID, NAME, geometry) %>%
        set_names(c("s", "c", "sc", "name", "geometry")) 

### bodies of water (B in equation e3.2)
water <- map_dfr(unique(counties$s), ~ area_water(state = ., county = counties$c[counties$s == .])
)[cities, ] 

### census-designated landmarks (L in equation e3.2)
lmrk <- map_dfr(unique(counties$s), ~ landmarks(., "area")
)[cities, ] %>%
        filter(MTFCC %in% scratch$lmrk) 

### union of water/landmarks (B and L in equation e3.2)
rmv <- bind_rows(water, lmrk) %>%
        st_union()

### remove unneeded objects from memory
rm(lmrk, water)

### clear garbage
gc()

### remove B and L from city areas (see equation e3.2)
# yields city boundaries, excluding areas unlikely to be populated
cities_clip <- st_difference(cities, rmv)

#####################################################################################################
###### OVERLAY INTERPOLATION
#####################################################################################################

### target zones (see section e3.2)
# download ZCTA boundaries, keeping only those that intersect the cities
tz <- (tigris::zctas(year = 2019) %>%
               rename(zcta = GEOID10) %>%
               dplyr::select(zcta, geometry) %>%
               arrange(zcta)
)[cities_clip, ] %>%
        # remove B and L from ZCTA areas
        st_difference(rmv) %>%
        # calculate area of full ZCTAs
        # including parts inside or outside of city for now but excluding B and L
        mutate(area_zcta = get_area(.)) %>%
        left_join(modzcta) %>%
        mutate(zip = if_else(is.na(modzcta), zcta, modzcta)) %>%
        # consolidate ZCTAs to MODZCTAs (affects NYC only)
        # "zip" refers to ZCTA or MODZCTA
        group_by(zip) %>%
        summarize(zcta = paste(zcta, collapse = ";"), # this column lists ZCTAs in each "zip"
                  area_zip = sum(area_zcta)) %>%
        ungroup() %>%
        # clip to boundaries of cities, excluding B and L
        st_intersection(cities_clip) %>%
        mutate(area_zip_in_city = get_area(.), # area of "zip" as defined above
               pr_zip_in_city = area_zip_in_city / area_zip) %>% # % "zip" within city
        # exclude "zips" that have negligible overlap w/ cities
        filter(!near(0, area_zip_in_city), pr_zip_in_city >= 1e-5)

### source zones (see section e3.2)
### yields a crosswalk w/ % of each source zone's area in the target zones ("zips") above
# download block group boundaries, keeping only those that intersect the cities
sz <- map_dfr(unique(counties$s), 
              ~ tigris::block_groups(state = ., county = counties$c[counties$s == .])
)[cities_clip, ] %>%
        rename(bg = GEOID) %>%
        # remove B and L from block group areas
        st_difference(rmv) %>%
        # calculate area of full block groups
        # including parts inside or outside of city for now but excluding B and L
        mutate(area_bg = get_area(.)) %>%
        # clip to "zips"
        st_intersection(tz) %>%
        # remove any point geographies (infinitesimal portions of overlapping polygons)
        st_collection_extract("POLYGON") %>%
        mutate(area_bg_in_zip = get_area(.), # area of each block group intersecting "zip"
               pr_bg_in_zip = area_bg_in_zip / area_bg) %>% # % each block group's area in "zip"
        st_drop_geometry() %>%
        # consolidate cases where block group was split, 
        # leading to multiple observations per block group/"zip" intersection
        group_by(bg, zip) %>%
        summarize(area_in = sum(area_bg_in_zip), # total area of block group/"zip" intersection
                  mult_bg = sum(pr_bg_in_zip)) %>% # multiplier represents % block group in "zip"
        ungroup() %>%
        arrange(zip, bg) 