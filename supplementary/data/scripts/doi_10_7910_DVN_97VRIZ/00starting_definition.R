### Updated version for Harvard Dataverse
### Implemented relative paths and standardized directory structure

# Load necessary libraries
library(forecast)
library(httr)
library(imputeTS)
library(readxl)
library(stringdist)
library(tidyverse)
library(vars)
library(modelsummary)
library(writexl)

# Define root directories
wd_main <- ".."
wd_data <- paste0(wd_main, "/data/raw_data")

# How to save datasets in wd_data (others are downloadable in script or exist)
file.path(wd_data, "fao_food.csv")
file.path(wd_data, "fao_milk.csv")
file.path(wd_data, "wfp_raw.csv")

# Proceed with implementing these paths in each script section as needed.
