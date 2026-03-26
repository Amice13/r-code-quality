# *****************************************************************
# OVERVIEW ####
# *****************************************************************
# ACLED_FUNCTIONS.R
# Functions to set up ACLED Analyses
# Jennifer Lin and Kristin Lunz Trujillo
# Created On: 2021 08 11

# *****************************************************************
# PACKAGES ####
# *****************************************************************

library(dplyr)       # For %>%, filter(), select()
library(ggplot2)     # For geom_sf() and other plot tools
library(mapproj)     # For R 4.0+ use
library(tigris)      # For shape files
library(haven)       # For read_dta()
library(sf)          # For Mapping with SF objects

# *****************************************************************
# GET COUNTY MAP ####
# *****************************************************************

# To generate each of our maps, we need the shape file. Instead
#   of attaching the files as data, we get the files from the 
#   Census Tiger shapefile directory: 
#   https://www.census.gov/cgi-bin/geo/shapefiles/index.php

# To aid this process, we use the state abbreviation list that comes
#   built in to base R.
states <- state.abb

# Create an empty object to store results
counties <- c()

# Get all the county shape files for each state except
#   Alaska and Hawaii
for (i in states) {
  if (!(i %in% c("AK", "HI"))) {
    counties[[i]] = counties(i, cb = TRUE)
    counties[[i]] = as.data.frame(counties[[i]])
  } 
}

# Join to one large data frame for use
us_counties <- bind_rows(counties)

# *****************************************************************
# FUNCTIONS ####
# *****************************************************************

# The version of the data we supplied for this part of the analysis
#   contains the census GEOIDs that we appended to the ACLED
#   raw data using their provided latitude and longitude codes
#   and https://geocoding.geo.census.gov/geocoder/
parse_census_codes <- function(df){
  df <- df %>% 
    mutate(
      state = substr(census_code, 1, 2),
      county= substr(census_code, 1, 5),
      tract = substr(census_code, 6, 11),
      block = substr(census_code, 12, 16)
    )
  return(df)
}
