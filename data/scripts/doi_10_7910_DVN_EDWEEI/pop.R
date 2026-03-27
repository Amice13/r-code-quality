# Alexander F. Gazmararian
# agazmararian@gmail.com
library(here)
library(tidyverse)
library(sf)
library(stars)
library(terra)

source(here("Code", "19_wrp", "fun", "load_zonal_df.R"))

# Convert sf to terra SpatVector for cross-platform compatibility
# Direct sf-to-terra coercion can fail on some systems
sf_vect <- tryCatch({
  terra::vect(sf_data)
}, error = function(e) {
  message("Direct sf-to-terra conversion failed, trying via sp...")
  # Fallback: convert via sp package
  sp_data <- sf::as_Spatial(sf_data)
  terra::vect(sp_data)
})

pop_extracted <- terra::extract(pop, sf_vect, fun = sum, na.rm = TRUE, touches = TRUE, weights = TRUE)
summary(pop_extracted)
sf_data$pop <- pop_extracted$ppp_2018_1km_Aggregated
sf_data <- st_drop_geometry(sf_data)
sf_data$pop_log <- log(sf_data$pop)
saveRDS(sf_data, here("Data", "inter", "19_wrp", "zonal_output", "subregion_pop.rds"))
message("Saved subregion_pop.rds")
