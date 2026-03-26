# Alexander F. Gazmararian
# agazmararian@gmail.com

library(here)
library(tidyverse)
library(sf)
library(terra)

source(here("Code", "19_wrp", "fun", "load_zonal_df.R"))

weighted_centers <- lapply(1:nrow(sf_data), function(i) {
  region <- sf_data[i, ]
  pop_cropped <- crop(pop, region, snap = "near")
  pop_masked <- mask(pop_cropped, region, touches = TRUE)
  pop_values <- values(pop_masked, na.rm = TRUE)
  coords <- xyFromCell(pop_masked, which(!is.na(values(pop_masked))))
  pop_weighted_x <- sum(coords[,1] * pop_values) / sum(pop_values)
  pop_weighted_y <- sum(coords[,2] * pop_values) / sum(pop_values)
  st_point(c(pop_weighted_x, pop_weighted_y)) |> st_sfc(crs = st_crs(region))
})
weighted_centers_sf <- do.call(rbind, weighted_centers)
sf_data_out <- data.frame(sf_data, centroid = weighted_centers_sf)
points <- lapply(sf_data_out$centroid, function(x) {st_point(x)})
coords_df <- do.call(rbind, points)
sf_data_out$x <- coords_df[,1]
sf_data_out$y <- coords_df[,2]
sf_data_out$centroid <- NULL
# Calculate unweighted centroid for missing values
sf_data_out$missing <- is.na(sf_data_out$x)
for (i in 1:nrow(sf_data_out)) {
  if (sf_data_out$missing[i]) {
    sf_data_out$x[i] <- st_centroid(sf_data_out[i, ]) %>% st_coordinates() %>% .[,1]
    sf_data_out$y[i] <- st_centroid(sf_data_out[i, ]) %>% st_coordinates() %>% .[,2]
  }
}
sf_data_out$missing <- NULL
sf_data_out$geom <- NULL
saveRDS(sf_data_out, file.path(save_path, "subregion_sf_centroids.rds"))
message("Saved subregion_sf_centroids.rds")
