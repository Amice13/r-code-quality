###################################
# DATA PREPARATION PIPELINE
#
# ### DO NOT RUN THIS FILE - IT NEEDS "DEEP" INPUT DATA NOT INCLUDED IN THE 
# ### REPLICATION DATAVERVSE-- FINAL RESULTS ARE SAVED UNDER data/analysis_data/*
# 
# Part of:
# Shaping States into Nations: The Effects of Ethnic Geography on State Borders
# Müller-Crepon, Schvitz, Cederman
# Replication files
#
# Called from scripts/replication.R
#
#############################


# Network Resolution 
#' Optimizes number of points to be queried in order to get desired mean edge length
print(paste(Sys.time(), "PREP NETWORK RESOLUTION", " ###################"))
try(source("scripts/data_prep/optimize_nwresolution.R"))

# Prepare main networks
print(paste(Sys.time(), "PREP MAIN NETWORKS", " ###################"))
try(source("scripts/data_prep/prepare_networks.R"))

# Prepare Language Tree Data (building on LEDA package)
print(paste(Sys.time(), "PREP Language DATA", " ###################"))
try(source("scripts/data_prep/expand_LEDA.R"))

# Prepare HEG Polygon Comparison
print(paste(Sys.time(), "PREP HEG POLYGON COMPARISON", " ###################"))
try(source("scripts/data_prep/heg_polygons_compare.R"))

# Prepare Final Network and SDM Point Data
print(paste(Sys.time(), "PREP FINAL DATA", " ###################"))
try(source("scripts/data_prep/prep_final_data.R"))



