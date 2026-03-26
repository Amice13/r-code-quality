# Alexander F. Gazmararian
# agazmararian@gmail.com
library(here)
library(lubridate)
library(sf)
library(terra)
library(tidyverse)
library(RColorBrewer)
library(exactextractr)

source(here("Code", "19_wrp", "fun", "load_zonal_df.R"))

urban <- rast(here("Data", "input", "grid", "urban", "annual_urbanMap_global_2019.tif"))
crs(urban) == crs(pop)
urban <- terra::crop(urban, pop, snap = "in")
urban_r <- terra::resample(urban, pop, method = "near")
urban_out <- exact_extract(urban_r, sf_data, fun = "weighted_mean", weights = pop, coverage_area = TRUE, stack_apply = TRUE, progress = FALSE)
sf_data_out <- cbind(sf_data, urban_out)
sf_data_out <- st_drop_geometry(sf_data_out)
sf_data_out <- rename(sf_data_out, urban_reg = urban_out)
saveRDS(sf_data_out, file = here("Data", "inter", "19_wrp", "zonal_output", "subregion_urban.rds"))
