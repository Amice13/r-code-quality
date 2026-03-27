# Alexander F. Gazmararian
# agazmararian@gmail.com
library(here)
library(tidyverse)
library(sf)
library(exactextractr)
library(stars)
library(terra)

source(here("Code", "19_wrp", "fun", "load_zonal_df.R"))

coal <- rast(here("Data", "input", "grid", "dpi", "Coal", "Coal_DPI.tif"))
oil <- rast(here("Data", "input", "grid", "dpi", "CO", "ConvOil_DPI.tif"))

coal <- terra::project(coal, crs(pop))
coal_ext <- terra::crop(coal, ext(pop), "in")
coal_r <- terra::resample(coal_ext, pop, method = "average")
coal_r <- ifel(is.na(coal_r), 0, coal_r)
coal_out <- exact_extract(coal_r, sf_data, fun = "weighted_mean", weights = pop, coverage_area = TRUE, progress = FALSE)
summary(coal_out)

oil <- terra::project(oil, crs(pop))
oil_ext <- terra::crop(oil, ext(pop), "in")
oil_r <- terra::resample(oil_ext, pop, method = "average")
oil_r <- ifel(is.na(oil_r), 0, oil_r)
oil_out <- exact_extract(oil_r, sf_data, fun = "weighted_mean", weights = pop, coverage_area = TRUE, progress = FALSE)
summary(oil_out)

sf_out <- data.frame(sf_data, oil = oil_out, coal = coal_out)
sf_out$geom <- NULL

g_include <- readRDS(here("Data", "inter", "19_wrp", "countries_include.rds"))
sf_out <- sf_out %>% filter(GID_0 %in% g_include$iso3c)

sf_out$coal_bin <- as.integer(sf_out$coal > quantile(sf_out$coal, 2/3, na.rm = TRUE))
sf_out$oil_bin <- as.integer(sf_out$oil > quantile(sf_out$oil, 2/3, na.rm = TRUE))

saveRDS(sf_out, here("Data", "inter", "19_wrp", "zonal_output",  "subregion_ff.rds"))
