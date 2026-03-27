# Alexander F. Gazmararian
# agazmararian@gmail.com
library(here)
library(tidyverse)
library(sf)
library(exactextractr)
library(stars)
library(terra)

source(here("Code", "19_wrp", "fun", "load_zonal_df.R"))

gdp <- rast(here("Data", "input", "grid", "gdp", "GDP_PPP_30arcsec_v3.nc"))
gdp <- gdp$GDP_PPP_3

if (!same.crs(gdp, pop)) {
  gdp <- terra::project(gdp, crs(pop))
}
# Interpolate within a narrow band for any missing areas
gdp <- terra::focal(gdp, 3, fun = "mean", na.policy = "only", na.rm = TRUE)
gdp <- terra::resample(gdp, pop, method = "bilinear")
# Calculate zonal statistics
gdp_out <- exact_extract(gdp, sf_data, fun = "weighted_mean", weights = pop, coverage_area = TRUE, stack_apply = TRUE, progress = FALSE)
summary(gdp_out)

sf_data_out <- data.frame(gdp = gdp_out, sf_data)
sf_data_out$geom <- NULL

sf_data_out$gdp_log <- log(sf_data_out$gdp)

saveRDS(sf_data_out, here("Data", "inter", "19_wrp", "zonal_output", "subregion_gdp.rds"))
message("Saved subregion_gdp.rds")
