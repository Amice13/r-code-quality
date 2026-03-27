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

fire_list <- list.files(path = "Data/input/grid/fire/modis/hdf5", recursive = TRUE, pattern = "\\.hdf", full.names = TRUE)

time_list <- sapply(fire_list, function (x) {
  match <- str_extract(x, "A\\d{7}")
  result <- sub("A(\\d{4})(\\d{3})", "\\1-\\2", match)
  return(result)
})
time_list <- as.vector(time_list)
years <- as.numeric(str_sub(time_list, 1, 4))
doy <- as.numeric(str_sub(time_list, 6, 8))

g <- rast(fire_list)
g <- subset(g, names(g) == "BurnedArea")
g <- flip(g, direction = "vertical")
names(g) <- as.Date(paste0(years, "-", gsub("1970-", "", as.Date(doy-1))))
crs(g) <- "EPSG:4326"
ext(g) <- c(-180, 180, -90, 90)
res(g)

if (!same.crs(g, pop)) {
  g <- project(g, crs(pop))
}

g_out <- exact_extract(g, sf_data, fun = "weighted_mean", weights = pop, coverage_area = TRUE, stack_apply = TRUE, progress = FALSE)
sf_data_out <- cbind(sf_data, g_out)
names(sf_data_out)[3:(ncol(sf_data_out) - 1)] <- paste0("x", names(g))
sf_data_out <- sf_data_out %>%
  pivot_longer(cols = c(paste0("x", names(g))), names_to = "date", values_to = "burn_modis")
sf_data_out$geom <- NULL
sf_data_out$date <- as.Date(gsub("x", "", sf_data_out$date))
sf_data_out$year <- year(sf_data_out$date)
sf_data_out$month <- month(sf_data_out$date)

# Save output with error handling for Windows R 4.5.1 serialization issues
output_file <- file.path(save_path, "modis_burnarea_zonal.rds")
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
  
  message("Saved modis_burnarea_zonal.rds (", nrow(sf_data_out), " rows)")
}, error = function(e) {
  # If standard save fails, try alternative approach
  warning("Standard saveRDS failed: ", e$message, ". Trying alternative method...")
  
  # Try saving with compression
  saveRDS(sf_data_out, output_file, version = 2, compress = "gzip")
  
  # Verify again
  if (!file.exists(output_file)) {
    stop("Failed to save modis_burnarea_zonal.rds: ", e$message)
  }
  
  message("Saved modis_burnarea_zonal.rds using alternative method")
})
