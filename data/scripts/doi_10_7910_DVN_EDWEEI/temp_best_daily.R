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

# Load and process temperature data
tday <- terra::rast(here("Data", "input", "grid", "temp", "berkeley-tmax-daily", "Complete_TMAX_Daily_LatLong1_2010.nc"))
tday <- subset(tday, grepl("temperature", names(tday)))

# Set up dates
start_date <- as.Date("2010-01-01")
end_date <- as.Date("2019-12-31")
date_sequence <- seq.Date(start_date, end_date, by = "day")
time(tday) <- date_sequence

# Handle CRS - use same.crs() which properly compares equivalent CRS definitions
if (!same.crs(tday, pop)) {
  message("Projecting temperature data to match population data CRS")
  tday <- terra::project(tday, crs(pop))
}

if (!same.crs(tday, pop)) {
  stop("Failed to align CRS of temperature and population data")
}

# Process temperature data
temp_interp <- terra::focal(tday, w = focal_window, fun = "mean", na.policy = "only", na.rm = TRUE)

# Extract zonal statistics
tday_out <- exact_extract(temp_interp, sf_data, fun = "weighted_mean", weights = pop, coverage_area = TRUE, stack_apply = TRUE, progress = FALSE)

# Prepare output data
sf_data_out <- cbind(sf_data, tday_out)
names(sf_data_out)[3:(ncol(sf_data_out) - 1)] <- paste0("x", time(tday))
sf_data_out <- sf_data_out %>% 
  pivot_longer(cols = c(paste0("x", time(tday))), names_to = "date", values_to = "tmean") %>%
  st_drop_geometry()

# Process dates
sf_data_out$date <- as.Date(gsub("x", "", sf_data_out$date))
sf_data_out$year <- year(sf_data_out$date)
sf_data_out$month <- month(sf_data_out$date)
sf_data_out$day <- day(sf_data_out$date)

# Save output with error handling for Windows R 4.5.1 serialization issues
output_file <- file.path(save_path, "berkeley_tmax_daily_zonal.rds")
tryCatch({
  # Convert to standard data.frame to avoid serialization issues
  sf_data_out <- as.data.frame(sf_data_out)
  
  # Use version 2 serialization format for better compatibility
  saveRDS(sf_data_out, output_file, version = 2, compress = FALSE)
  
  # Verify the file was created and is readable
  if (!file.exists(output_file) || file.size(output_file) == 0) {
    stop("Output file was not created or is empty")
  }
  
  # Test readability
  test_read <- readRDS(output_file)
  if (nrow(test_read) == 0) {
    stop("Output file exists but contains no data")
  }
  
  message("Saved berkeley_tmax_daily_zonal.rds (", nrow(sf_data_out), " rows)")
}, error = function(e) {
  # If standard save fails, try alternative approach
  warning("Standard saveRDS failed: ", e$message, ". Trying alternative method...")
  
  # Try saving with compression
  saveRDS(sf_data_out, output_file, version = 2, compress = "gzip")
  
  # Verify again
  if (!file.exists(output_file)) {
    stop("Failed to save berkeley_tmax_daily_zonal.rds: ", e$message)
  }
  
  message("Saved berkeley_tmax_daily_zonal.rds using alternative method")
})
