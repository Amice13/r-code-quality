# Alexander F. Gazmararian
# agazmararian@gmail.com

library(here)
library(janitor)
library(readxl)
library(sf)
library(tidyverse)

# Read survey data
risk <- tryCatch({
  suppressMessages(read_xlsx(here("Data", "input", "survey", "worldriskpoll", "2019", "data", "19_wrp_original.xlsx")))
}, error = function(e) {
  stop(sprintf("Error reading survey data: %s", e$message))
})

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

# Clean and validate sf data
sf_data <- sf_data %>%
  filter(!is.na(REG_ID)) %>%
  st_make_valid()

# Process region data
wrp_reg <- tryCatch({
  read.csv(here("Data", "input", "survey", "worldriskpoll", "2019", "data", "19_wrp_region.csv"))
}, error = function(e) {
  stop(sprintf("Error reading region data: %s", e$message))
})

wrp_reg_long <- wrp_reg %>% 
  pivot_longer(cols = c(REGION_AFG:REGION4_JMC)) %>%
  filter(!is.na(value)) %>%
  separate(name, into = c("reg_lvl", "GID_0"), sep = "_")

reg_out <- wrp_reg_long %>%
  filter(!(GID_0 == "CIV" & reg_lvl == "REGION2")) %>%
  filter(!(GID_0 == "TZA" & reg_lvl == "REGION2")) %>%
  mutate(reg_lvl = as.numeric(gsub("REGION", "", reg_lvl)),
         reg_lvl = ifelse(is.na(reg_lvl), 1, reg_lvl)) %>%
  group_by(GID_0) %>%
  filter(reg_lvl == min(reg_lvl)) %>%
  rename(REG_ID = value)

# Add Singapore (SGP)
sgp <- risk %>% 
  filter(Country == "Singapore") %>%
  transmute(
    WPID_RANDOM,
    REG_ID = 1,
    GID_0 = "SGP"
  )

if (nrow(sgp) == 0) {
  warning("No Singapore data found in survey data")
} else {
  reg_out <- bind_rows(reg_out, sgp)
}

# Address Bahrain missing and correct country codes
reg_out <- reg_out %>%
  mutate(
    REG_ID = ifelse(REG_ID %in% c(98, 99, 99999) | (GID_0 == "BHR" & REG_ID == 8), NA, REG_ID),
    GID_0 = case_when(
      GID_0 == "GUI" ~ "GIN",
      GID_0 == "MLW" ~ "MWI",
      GID_0 == "TOG" ~ "TGO",
      GID_0 == "JMC" ~ "JAM",
      GID_0 == "LYB" ~ "LBY",
      GID_0 == "CAM" ~ "CMR",
      GID_0 == "KOS" ~ "XKO",
      TRUE ~ GID_0
    )
  )

# Calculate centroids and coordinates
sf_data <- sf_data %>%
  st_centroid() %>%
  mutate(
    lon = st_coordinates(.)[,"X"],
    lat = st_coordinates(.)[,"Y"]
  )

# Create data frame with keys connecting the two
reg_geom <- full_join(reg_out, sf_data, by = c("GID_0", "REG_ID"))

if (nrow(reg_geom) == 0) {
  stop("No matching records found after joining region and geometry data")
}

reg_geom_sub <- reg_geom %>%
  st_drop_geometry() %>%
  select(-any_of("geom")) %>%  # Use any_of to avoid errors if geom doesn't exist
  clean_names()

# Save output
saveRDS(reg_geom_sub, here("Data", "inter", "19_wrp", "admin_wrp_crosswalk.rds"))

# Clean up: delete the unzipped GADM file (keep the .zip version)
gadm_file <- here("Data", "input", "world_admin_boundaries", "gadm_410.gpkg")
if (file.exists(gadm_file)) {
  message("Removing unzipped GADM file (keeping .zip version)...")
  file.remove(gadm_file)
}
