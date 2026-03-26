# ----------------------------------------------------------------
# All_Figures.R
# Code to reproduce all figures in Krebs et. al., ES&T (2021)
# https://doi.org/10.1021/acs.est.0c08469
# ----------------------------------------------------------------

# Required packages
library(tidyverse)
library(RColorBrewer)
library(sf)
library(sp)
library(raster)
library(ggpubr)

# ----------------------------------------------------------------
# Read data
# ----------------------------------------------------------------
# Raw data (with some minimal cleaning)
raw = read_csv("Data/raw_data.csv")

# Indoor-outdoor matched data at 500m
dat = read_csv("Data/matched_data.csv")

# Indoor monitor locations
locs = read_csv("Data/monitors_zip_inside.csv")

# USA map boundaries - trim to California
usa = readRDS("Data/USA_adm2.rds")
ca = usa[usa$NAME_1=="California",]
coordinates(locs) = ~lon + lat
crs(locs) = crs(ca)

# ----------------------------------------------------------------
# Main Figures (each file takes a couple minutes to run)
# ----------------------------------------------------------------
# Figure 1 - Map and time series
source("Code/Fig1.R")

# Figure 2 - Indoor and Outdoor daily and hourly raw distributions
source("Code/Fig2.R")

# Figure 3 - Regression Coefficient Plots
source("Code/Fig3.R")

# Figure 4 - Locations and relationship with housing stock age
source("Code/Fig4.R")

# Supplemental Figures 1-3
source("Code/Supp.R")



