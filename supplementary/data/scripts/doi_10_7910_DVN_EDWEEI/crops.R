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

wheat <- rast(here("Data", "input", "grid", "cropgrids", "CROPGRIDSv1.08_NC_maps", "CROPGRIDSv1.08_wheat.nc"))
barley <- rast(here("Data", "input", "grid", "cropgrids", "CROPGRIDSv1.08_NC_maps", "CROPGRIDSv1.08_barley.nc"))
sorghum <- rast(here("Data", "input", "grid", "cropgrids", "CROPGRIDSv1.08_NC_maps", "CROPGRIDSv1.08_sorghum.nc"))
oat <- rast(here("Data", "input", "grid", "cropgrids", "CROPGRIDSv1.08_NC_maps", "CROPGRIDSv1.08_oats.nc"))
rye <- rast(here("Data", "input", "grid", "cropgrids", "CROPGRIDSv1.08_NC_maps", "CROPGRIDSv1.08_rye.nc"))
rice <- rast(here("Data", "input", "grid", "cropgrids", "CROPGRIDSv1.08_NC_maps", "CROPGRIDSv1.08_rice.nc"))
maize <- rast(here("Data", "input", "grid", "cropgrids", "CROPGRIDSv1.08_NC_maps", "CROPGRIDSv1.08_maize.nc"))
soy <- rast(here("Data", "input", "grid", "cropgrids", "CROPGRIDSv1.08_NC_maps", "CROPGRIDSv1.08_soybean.nc"))
sugarcane <- rast(here("Data", "input", "grid", "cropgrids", "CROPGRIDSv1.08_NC_maps", "CROPGRIDSv1.08_sugarcane.nc"))
potatoes <- rast(here("Data", "input", "grid", "cropgrids", "CROPGRIDSv1.08_NC_maps", "CROPGRIDSv1.08_potato.nc"))
oilpalm <- rast(here("Data", "input", "grid", "cropgrids", "CROPGRIDSv1.08_NC_maps", "CROPGRIDSv1.08_oilpalm.nc"))
cassava <- rast(here("Data", "input", "grid", "cropgrids", "CROPGRIDSv1.08_NC_maps", "CROPGRIDSv1.08_cassava.nc"))
wheat <- ifel(wheat == -1, 0, wheat)
barley <- ifel(barley == -1, 0, barley)
sorghum <- ifel(sorghum == -1, 0, sorghum)
oat <- ifel(oat == -1, 0, oat)
rye <- ifel(rye == -1, 0, rye)
rice <- ifel(rice == -1, 0, rice)
maize <- ifel(maize == -1, 0, maize)
soy <- ifel(soy == -1, 0, soy)
sugarcane <- ifel(sugarcane == -1, 0, sugarcane)
potatoes <- ifel(potatoes == -1, 0, potatoes)
oilpalm <- ifel(oilpalm == -1, 0, oilpalm)
cassava <- ifel(cassava == -1, 0, cassava)
crops <- c(wheat, barley, sorghum, oat, rye, rice, maize, soy, sugarcane, potatoes, oilpalm, cassava)
crops <- subset(crops, names(crops) == "croparea")
crops <- terra::app(crops, "sum")
crs(crops) <- "GEOGCRS[\"WGS 84 (CRS84)\",\n    DATUM[\"World Geodetic System 1984\",\n        ELLIPSOID[\"WGS 84\",6378137,298.257223563,\n            LENGTHUNIT[\"metre\",1]]],\n    PRIMEM[\"Greenwich\",0,\n        ANGLEUNIT[\"degree\",0.0174532925199433]],\n    CS[ellipsoidal,2],\n        AXIS[\"geodetic longitude (Lon)\",east,\n            ORDER[1],\n            ANGLEUNIT[\"degree\",0.0174532925199433]],\n        AXIS[\"geodetic latitude (Lat)\",north,\n            ORDER[2],\n            ANGLEUNIT[\"degree\",0.0174532925199433]],\n    USAGE[\n        SCOPE[\"unknown\"],\n        AREA[\"World\"],\n        BBOX[-90,-180,90,180]],\n    ID[\"OGC\",\"CRS84\"]]"
if (!same.crs(crops, pop)) {
  crops <- terra::project(crops, crs(pop))
}

crops <- terra::crop(crops, pop, snap = "in")
crops <- terra::resample(crops, pop, "bilinear")
crop_out <- exact_extract(crops, sf_data, fun = "weighted_mean", weights = pop, coverage_area = TRUE, stack_apply = TRUE, progress = FALSE)
sf_data_out <- cbind(sf_data, crop_out)
sf_data_out$geom <- NULL
names(sf_data_out)[3] <- "crop"

g_include <- readRDS(here("Data", "inter", "19_wrp", "countries_include.rds"))
sf_data_out <- sf_data_out %>% filter(GID_0 %in% g_include$iso3c)

sf_data_out$crop_terc <- cut(
  sf_data_out$crop,
  c(-Inf, quantile(sf_data_out$crop, c(1/3, 2/3), na.rm = TRUE), Inf),
  labels = c("Q1", "Q2", "Q3")
)

saveRDS(sf_data_out, here("Data", "inter", "19_wrp", "zonal_output", "subregion_crop.rds"))
message("Saved subregion_crop.rds")
