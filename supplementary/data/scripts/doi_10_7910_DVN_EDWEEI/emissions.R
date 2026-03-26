# Alexander F. Gazmararian
# agazmararian@gmail.com
library(here)
library(tidyverse)
library(exactextractr)
library(sf)
library(terra)

source(here("Code", "19_wrp", "fun", "load_zonal_df.R"))

co2 <- rast(here("Data", "input", "grid", "emissions", "co2_excl", "v6.0_CO2_excl_short-cycle_org_C_2018_TOTALS.0.1x0.1.nc"))
co2 <- terra::rotate(co2)
co2 <- terra::resample(co2, pop, method = "bilinear")
co2_out <- exact_extract(co2, sf_data, fun = "weighted_mean", weights = pop, coverage_area = TRUE, progress = FALSE)
summary(co2_out)

sf_data <- cbind(sf_data, co2 = co2_out)
sf_data$geom <- NULL

sf_data$co2_log <- log(sf_data$co2)

saveRDS(sf_data, here("Data", "inter", "19_wrp", "zonal_output", "subregion_emissions.rds"))
