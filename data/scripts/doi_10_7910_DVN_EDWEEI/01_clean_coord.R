# Alexander F. Gazmararian
# agazmararian@gmail.com
# Creates the coordinate base upon which the temperature damages of warming simulations are merged
library(countrycode)
library(here)
library(maps)
library(readxl)
library(tidyverse)
# load crosswalk
iso <- read_xlsx(here("Data", "input", "grid", "coord", "C_vect_ISO.xlsx"))
lon <- read.csv(here("Data", "input", "grid", "coord", "lon.csv"), header = FALSE)
lat <- read.csv(here("Data", "input", "grid", "coord", "lat.csv"), header = FALSE)
# country index
C <- read.csv(here("Data", "input", "grid", "coord", "c_index.csv"), header = FALSE)
# pair longitude and latitude
coord <- data.frame(C, lon, lat)
names(coord) <- c("index", "lon", "lat")
# take only first 4th because repeated 4 times
coord <- coord[c(1:17048), ]
# add cell index
coord$cell_index <- 1:17048
# add country names
names(iso)[1] <- "index"
coord <- merge(coord, iso, by = "index")
coord$cowc <- countrycode(coord$ISO_name, "country.name", "cowc")
coord$iso3c <- countrycode(coord$ISO, "iso3n", "iso3c")

saveRDS(coord, here("Data", "inter", "grid", "coord.rds"))