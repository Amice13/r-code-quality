# Alexander F. Gazmararian
# agazmararian@gmail.com

library(here)
library(tidyverse)
library(exactextractr)
library(sf)
library(terra)

source(here("Code", "19_wrp", "fun", "load_zonal_df.R"))

damage <- readRDS(here("Data", "inter", "grid", "tempdamage.rds"))
damage <- subset(damage, select = c(lon, lat, gdp_25, gdp_50, gdp_100))

get_zonal_proj <- function(input_df, input_var, varname, interp = TRUE) {
  points <- vect(input_df, geom = c("lon", "lat"), crs = crs(pop))
  xmin <- -180
  xmax <- 180
  ymin <- -90
  ymax <- 90
  raster_1x1 <- rast(nrows = 180, ncols = 360, resolution = 1, xmin = xmin, xmax = xmax, ymin = ymin, ymax = ymax, crs = crs(pop))
  raster_values <- rasterize(points, raster_1x1, field = varname, fun = "mean")
  # Smooth out the data with interpolation
  if (isTRUE(interp)) {
    raster_values <- terra::focal(raster_values, w = focal_window, fun = "mean", na.policy = "only", na.rm = TRUE)
  }
  # Calculate zonal stats
  damage_out <- exact_extract(raster_values, sf_data, fun = "weighted_mean", weights = pop, coverage_area = TRUE, progress = FALSE)
  return(damage_out)
}

sf_data$gdp_25_n <- get_zonal_proj(damage, damage$gdp_50, "gdp_25", interp = FALSE)
sf_data$gdp_50_n <- get_zonal_proj(damage, damage$gdp_50, "gdp_50", interp = FALSE)
sf_data$gdp_100_n <- get_zonal_proj(damage, damage$gdp_100, "gdp_100", interp = FALSE)
sf_data$gdp_25 <- get_zonal_proj(damage, damage$gdp_50, "gdp_25", interp = TRUE)
sf_data$gdp_50 <- get_zonal_proj(damage, damage$gdp_50, "gdp_50", interp = TRUE)
sf_data$gdp_100 <- get_zonal_proj(damage, damage$gdp_100, "gdp_100", interp = TRUE)

sf_data <- st_drop_geometry(sf_data)

sf_data$reg_loser_50 <- as.integer(sf_data$gdp_50 < 1)
sf_data$reg_loser_25 <- as.integer(sf_data$gdp_25 < 1)
sf_data$reg_loser_100 <- as.integer(sf_data$gdp_100 < 1)

g_include <- readRDS(here("Data", "inter", "19_wrp", "countries_include.rds"))
sf_data <- sf_data %>% filter(GID_0 %in% g_include$iso3c)

summary(sf_data)

sf_data$gdp_50_terc <- cut(
  sf_data$gdp_50,
  c(-Inf, quantile(sf_data$gdp_50, c(1 / 3, 2 / 3), na.rm = TRUE), Inf),
  labels = c("Q1", "Q2", "Q3")
)
sf_data$gdp_50_qtl <- cut(
  sf_data$gdp_50,
  c(-Inf, quantile(sf_data$gdp_50, c(1 / 4, 1/2, 3/4), na.rm = TRUE), Inf),
  labels = c("Q1", "Q2", "Q3", "Q4")
)
sf_data$gdp_50_qtl5 <- cut(
  sf_data$gdp_50,
  c(-Inf, quantile(sf_data$gdp_50, c(1 / 5, 2 / 5, 3 / 5, 4 / 5), na.rm = TRUE), Inf),
  labels = c("Q1", "Q2", "Q3", "Q4", "Q5")
)

# Save output with error handling for Windows R 4.5.1 serialization issues
output_file <- here("Data", "inter", "19_wrp", "zonal_output", "subregion_damage.rds")
tryCatch({
  # Convert to standard data.frame to avoid serialization issues
  sf_data <- as.data.frame(sf_data)
  
  # Use version 2 serialization format for better compatibility
  saveRDS(sf_data, output_file, version = 2, compress = FALSE)
  
  # Verify the file was created and is readable
  if (!file.exists(output_file) || file.size(output_file) == 0) {
    stop("Output file was not created or is empty")
  }
  
  # Test readability
  test_read <- readRDS(output_file)
  if (nrow(test_read) == 0) {
    stop("Output file exists but contains no data")
  }
  
  message("Saved subregion_damage.rds (", nrow(sf_data), " rows) - contains gdp_50 and other damage variables")
}, error = function(e) {
  # If standard save fails, try alternative approach
  warning("Standard saveRDS failed: ", e$message, ". Trying alternative method...")
  
  # Try saving with compression
  saveRDS(sf_data, output_file, version = 2, compress = "gzip")
  
  # Verify again
  if (!file.exists(output_file)) {
    stop("Failed to save subregion_damage.rds: ", e$message)
  }
  
  message("Saved subregion_damage.rds using alternative method")
})
