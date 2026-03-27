#############################
# FULL ANALYSIS REPLICATION
# 
# 
# Part of:
# Shaping States into Nations: The Effects of Ethnic Geography on State Borders
# Müller-Crepon, Schvitz, Cederman
# Replication files
#############################

#' Run as
#' nohup Rscript scripts/analysis/analysis_all.R  >logs/analysis_20230227.log>&1 &
#' 
#' Forced termination of running MC, e.g. via: pkill -f slaveRSOCK
#' 
rm(list = ls())

# Check packages
source("scripts/libraries.R")


# Load Globals
print(paste(Sys.time(), "LOAD GLOBALS", " ###################"))
source("scripts/analysis/analysis_globals.R")


# PROBABILISTIC SPATIAL PARTITION MODEL ###############


# Make cluster
cl <- make_cluster(ncore)

# DESCRIPTIVES

# Descriptive
print(paste(Sys.time(), "DESCRIPTIVES", " ###################"))
source("scripts/analysis/data/analysis_descriptives.R")

# HEG Comparison
print(paste(Sys.time(), "HEG COMPARISON", " ###################"))
source("scripts/analysis/data/heg_comparison_plot.R")

# HEG Metadata Table
print(paste(Sys.time(), "HEG METADATA", " ###################"))
source("scripts/analysis/data/meta_data_table.R")

# ANALYSIS

# Pooled periods periods
print(paste(Sys.time(), "POOLED ESTIMATES", " ###################"))
source("scripts/analysis/pspm/analysis_pool.R")

# Regular "cut" periods
print(paste(Sys.time(), "CUT PANEL ESTIMATES", " ###################"))
source("scripts/analysis/pspm/analysis_cuts.R")

# Regular "cut" crosssections
print(paste(Sys.time(), "CUT XSECTION ESTIMATES", " ###################"))
source("scripts/analysis/pspm/analysis_cutxsec.R")

# Full interactions
print(paste(Sys.time(), "FULL INTERACTION ESTIMATES", " ###################"))
source("scripts/analysis/pspm/analysis_fullint.R")

# Austria Hungary from Census Data
print(paste(Sys.time(), "Austria-Hungary", " ###################"))
source("scripts/analysis/pspm/analysis_austhung.R")

# Linguistic Distance
print(paste(Sys.time(), "Linguistic distance", " ###################"))
source("scripts/analysis/pspm/analysis_lingdist.R")

# Estimation by map
print(paste(Sys.time(), "Analysis by map", " ###################"))
source("scripts/analysis/pspm/analysis_bymap.R")

# Controls only (for RR)
print(paste(Sys.time(), "Controls only", " ###################"))
source("scripts/analysis/pspm/controlsonly.R")

# Long periods
print(paste(Sys.time(), "LONG ESTIMATES", " ###################"))
source("scripts/analysis/pspm/analysis_long.R")
source("scripts/analysis/pspm/analysis_longxsec.R")

# Control variables
print(paste(Sys.time(), "CONTROLS", " ###################"))
source("scripts/analysis/pspm/analysis_controls.R")

# Control for Amiistrative borders
print(paste(Sys.time(), "1886 w/ ADMIN BORDERS", " ###################"))
source("scripts/analysis/pspm/analysis_1886adm.R")

# Shift robustness check
# print(paste(Sys.time(), "SHIFT", " ###################"))
# source("scripts/analysis/pspm/analysis_shift_notrun.R") ## Do not run

# Resolution robustness check
print(paste(Sys.time(), "RESOLUTION", " ###################"))
source("scripts/analysis/pspm/analysis_resolution.R")

# Structure robustness check
print(paste(Sys.time(), "STRUCTURE", " ###################"))
source("scripts/analysis/pspm/analysis_structure.R")

# Periodicity robustness check
print(paste(Sys.time(), "PERIODICITY", " ###################"))
source("scripts/analysis/pspm/analysis_periodicity.R")

# Burnin robustness check
print(paste(Sys.time(), "BURNIN", " ###################"))
source("scripts/analysis/pspm/analysis_burninbootse.R")

# Continents Analysis
print(paste(Sys.time(), "CONTINENT", " ###################"))
source("scripts/analysis/pspm/analysis_continents.R")

# Population interactions
print(paste(Sys.time(), "POPULATION INTERACTION ESTIMATES", " ###################"))
source("scripts/analysis/pspm/analysis_popint.R")

# Marginal Effects
print(paste(Sys.time(), "MARGINAL EFFECTS", " ###################"))
source("scripts/analysis/pspm/main_margprob.R")

# Main Plots
print(paste(Sys.time(), "MAIN PLOTS / TABLES", " ###################"))
source("scripts/analysis/pspm/main_plots_tables.R")

# Edge-Level
print(paste(Sys.time(), "Edge-Level Logit", " ###################"))
source("scripts/analysis/pspm/analysis_edgelevel.R")

# SECESSIONISM ANALYSIS #####################

# Estimate main models
print(paste(Sys.time(), "MAIN SDM MODELS", " ###################"))
source("scripts/analysis/sdm/analysis_sdm.R")

# Resolution robustness check
print(paste(Sys.time(), "SDM RESOLUTION CHECK", " ###################"))
source("scripts/analysis/sdm/analysis_resolcheck.R")

# Shift points robustness check
print(paste(Sys.time(), "SDM SHIFT CHECK", " ###################"))
# source("scripts/analysis/sdm/analysis_shiftcheck_notrun.R")) ## Do not run.
source("scripts/analysis/sdm/analysis_shiftcheck.R") 

# Structure robustness check
print(paste(Sys.time(), "SDM STRUCTURE CHECK", " ###################"))
source("scripts/analysis/sdm/analysis_structcheck.R")


# MONTE CARLO RESULTS PLOTS ####################

# Init
source("scripts/analysis/mc_plots/mc_init.R")

# Main Plots
print(paste(Sys.time(), "MONTE CARLO PARAMETER INFERENCE", " ###################"))
source("scripts/analysis/mc_plots/mc_inference.R")
source("scripts/analysis/mc_plots/mc_uncertainty.R")


# MC Example
print(paste(Sys.time(), "MONTE CARLO EXAMPLE", " ###################"))
source("scripts/analysis/mc_plots/examples.R")

# PSPM Visualization
print(paste(Sys.time(), "PSPM ILLUSTRATION", " ###################"))
source("scripts/analysis/mc_plots/pspm_example.R")


# STOP CLUSTER #####################
if(exists("cl")){
  stopCluster(cl)
  rm(cl)
}

########### END ####################
print(paste(Sys.time(), "THE END", " ###################"))




