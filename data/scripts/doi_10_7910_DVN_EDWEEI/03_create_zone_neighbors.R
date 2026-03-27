# Alexander F. Gazmararian
# agazmararian@gmail.com
library(here)
library(sf)
library(tidyverse)

# Read and combine all sf objects
sf_list <- list.files(here("Data", "inter", "19_wrp", "world_admin_boundaries"), 
                      full.names = TRUE, 
                      pattern = "\\.rds$")

if (length(sf_list) == 0) {
  stop("No RDS files found in the world_admin_boundaries directory")
}

# Read and combine while maintaining sf class
sf_data <- lapply(sf_list, function(x) {
  tryCatch({
    obj <- readRDS(x)
    if (!inherits(obj, "sf")) {
      obj <- st_as_sf(obj)
    }
    return(obj)
  }, error = function(e) {
    warning(sprintf("Error reading %s: %s", x, e$message))
    return(NULL)
  })
})

# Remove any NULL entries from failed reads
sf_data <- Filter(Negate(is.null), sf_data)

if (length(sf_data) == 0) {
  stop("No valid sf objects could be read from the files")
}

# Combine all objects
sf_data <- do.call(rbind, sf_data)

# Clean and validate
sf_data <- sf_data %>%
  filter(!is.na(REG_ID)) %>%
  st_make_valid()

# Calculate neighbors
neighbors <- st_touches(sf_data, sparse = FALSE)

# Save results
save(sf_data, file = here("Data", "inter", "19_wrp", "subregion_sf.rda"))
save(neighbors, file = here("Data", "inter", "19_wrp", "subregion_neighbors.rda"))
