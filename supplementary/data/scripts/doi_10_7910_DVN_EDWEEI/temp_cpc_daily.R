# Alexander F. Gazmararian
# afg2@princeton.edu
library(here)
library(lubridate)
library(sf)
library(terra)
library(tidyverse)
library(RColorBrewer)
library(exactextractr)

source(here("Code", "19_wrp", "fun", "load_zonal_df.R"))

if (!file.exists(here("Data", "input", "grid", "temp", "noaa-cpc-daily", "tmax", "tmax_noaa.tif"))) {
  noaa.files <- list.files(here("Data", "input", "grid", "temp", "noaa-cpc-daily", "tmax"), pattern = "*.nc", full.names = TRUE)
  tmax_noaa <- rast(noaa.files)
  names(tmax_noaa) <- time(tmax_noaa)
  terra::writeRaster(
    tmax_noaa,
    filename = here("Data", "input", "grid", "temp", "noaa-cpc-daily", "tmax", "tmax_noaa.tif"),
    filetype = "GTiff",            
    overwrite = TRUE
  )
  gc()
} else {
  tmax_noaa <- rast(here("Data", "input", "grid", "temp", "noaa-cpc-daily", "tmax", "tmax_noaa.tif"))
}
tmax_noaa <- terra::rotate(tmax_noaa)
crs(tmax_noaa) == crs(pop)
#Aggregate population for faster zonal stats
pop.agg <- terra::aggregate(pop, fact = 6, fun = "sum")
temp.interp <- terra::focal(tmax_noaa, w = focal_window, fun = "mean", na.policy = "only", na.rm = TRUE)
tday.out <- exact_extract(temp.interp, sf_data, fun = "weighted_mean", weights = pop.agg, coverage_area = TRUE, stack_apply = TRUE, progress = FALSE)
sf.data.out <- cbind(sf_data, tday.out)
names(sf.data.out)[3:(ncol(sf.data.out) - 1)] <- paste0("x", time(tmax_noaa))
sf.data.out <- sf.data.out %>% pivot_longer(cols = c(paste0("x", time(tmax_noaa))), names_to = "date", values_to = "tmean")
sf.data.out <- st_drop_geometry(sf.data.out)
sf.data.out$date <- as.Date(gsub("x", "", sf.data.out$date))
sf.data.out$year <- year(sf.data.out$date)
sf.data.out$month <- month(sf.data.out$date)
sf.data.out$day <- day(sf.data.out$date)

# Save output with error handling for Windows R 4.5.1 serialization issues
output_file <- file.path(save_path, "noaa_cpc_tmax_daily_zonal.rds")
tryCatch({
  # Convert to standard data.frame to avoid serialization issues
  sf.data.out <- as.data.frame(sf.data.out)
  
  # Use version 2 serialization format for better compatibility
  saveRDS(sf.data.out, output_file, version = 2, compress = FALSE)
  
  # Verify the file was created and is readable
  if (!file.exists(output_file) || file.size(output_file) == 0) {
    stop("Output file was not created or is empty")
  }
  
  # Test readability
  test_read <- readRDS(output_file)
  if (nrow(test_read) == 0) {
    stop("Output file exists but contains no data")
  }
  
  message("Saved noaa_cpc_tmax_daily_zonal.rds (", nrow(sf.data.out), " rows)")
}, error = function(e) {
  # If standard save fails, try alternative approach
  warning("Standard saveRDS failed: ", e$message, ". Trying alternative method...")
  
  # Try saving with compression
  saveRDS(sf.data.out, output_file, version = 2, compress = "gzip")
  
  # Verify again
  if (!file.exists(output_file)) {
    stop("Failed to save noaa_cpc_tmax_daily_zonal.rds: ", e$message)
  }
  
  message("Saved noaa_cpc_tmax_daily_zonal.rds using alternative method")
})