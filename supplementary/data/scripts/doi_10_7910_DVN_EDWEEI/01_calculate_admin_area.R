source("Code/19_wrp/fun/calculate_zonal_stats_preamble.R")

sf_data <- st_make_valid(sf_data)
sf_data$area <- st_area(sf_data)
sf_data$area <- sf_data$area
sf_data <- st_drop_geometry(sf_data)
sf_data$area <- as.numeric(sf_data$area)
saveRDS(sf_data, here("Data", "inter", "19_wrp", "zone_area.rds"))
