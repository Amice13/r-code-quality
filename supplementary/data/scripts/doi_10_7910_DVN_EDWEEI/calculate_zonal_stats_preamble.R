# Alexander F. Gazmararian
# agazmararian@gmail.com

# Suppress progress bars (especially for Windows logs)
if (requireNamespace("terra", quietly = TRUE)) {
  terra::terraOptions(progress = 0)
}

library(DescTools)
library(exactextractr)
library(here)
library(lubridate)
library(RColorBrewer)
library(sf)
library(terra)
library(tidyverse)
library(tmap)

# Note: here() strips trailing slashes, so use file.path() when concatenating
load_path <- here("Data", "inter", "19_wrp", "zonal_merged")
out_path <- here("Data", "inter", "19_wrp", "zonal_output")

fdates <- readRDS(here("Data", "inter", "19_wrp", "field_dates.rds"))

source(here("Code", "19_wrp", "fun", "load_zonal_df.R"))

impute_missing <- function(df, variable) {
  df$missing <- is.na(df[[variable]])
  for (i in which(df$missing)) {
    neighbor_indices <- which(neighbors[i, ])
    valid_neighbors <- neighbor_indices[!df$missing[neighbor_indices]]
    if (length(valid_neighbors) > 0) {
      df[[variable]][i] <- mean(df[[variable]][valid_neighbors], na.rm = TRUE)
    }
  }
  return(df)
}