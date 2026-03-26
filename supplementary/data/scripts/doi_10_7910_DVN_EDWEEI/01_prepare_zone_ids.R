# Alexander F. Gazmararian
# agazmararian@gmail.com

library(countrycode)
library(here)
library(readxl)
library(sf)
library(tidyverse)

# Check and unzip GADM file if needed
gadm_file <- here("Data", "input", "world_admin_boundaries", "gadm_410.gpkg")
gadm_zip <- paste0(gadm_file, ".zip")

if (!file.exists(gadm_file) && file.exists(gadm_zip)) {
  message("Unzipping GADM file...")
  unzip(gadm_zip, exdir = dirname(gadm_zip))
} else if (!file.exists(gadm_file)) {
  stop("GADM file not found. Please ensure ", basename(gadm_zip), " is in the correct directory.")
}

g <- st_read(gadm_file)
wrp <- read_excel(here("Data", "input", "survey", "worldriskpoll", "2019", "data", "19_wrp_original.xlsx"))
cr <- readRDS(here("Data", "inter", "country", "damage_country.rds"))
wrp_countries <- c(unique(wrp$Country))
wrp_countries_iso3c <- countrycode(wrp_countries, "country.name", "iso3c")
wrp_countries_iso3c <- wrp_countries_iso3c[which(!is.na(wrp_countries_iso3c))]
add_countries <- c("Palestine", "Kosovo", "Georgia", "Montenegro", "Kuwait")
g_out <- g %>%
  filter((GID_0 %in% wrp_countries_iso3c | NAME_0 %in% add_countries & (GID_0 %in% cr$iso3c | NAME_0 %in% add_countries)))
g_out$REG_ID <- NA
saveRDS(g_out, here("Data", "inter", "19_wrp", "regions4coding.rds"))
