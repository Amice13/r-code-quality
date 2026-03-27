##################################################################################
# Agrawal and Chen (2026)
# 
# Description: Master R script. Sets up the R environment. Runs the R scripts in 
# sequence. Also outlines when the STATA do files need to be run.
# 
##################################################################################

# Clear environment
rm(list = ls())

# 00a. Set up R environment ----------------------------------------------

# Check and install necessary packages
if (!require("pacman")) install.packages("pacman")
pacman::p_load(tidyverse, janitor, haven, data.table, arrow, foreach, 
               doParallel, ipumsr, tibble, readr, purrr, ggrepel, scales, usmap,
               sf, patchwork, readxl, lubridate, ggthemes, ggpattern, rvest, rlang,
               fixest, Hmisc, xtable)


# 00b. Set working directory ---------------------------------------------------

# Replace the file location in this line with the location of the replication folder on your computer
setwd("/Users/xinyuc/Desktop/replication_package")


# 0. Preparations: Clean Tax Data ----------------------------------------------

# Run these do files in STATA in the following order:
#   data/tax_statutory/0a_clean.do
#   data/salestax/0b_clean.do
#   data/propertytax/0c_clean.do


# 1. Construct Tax Units --------------------------------------------------

source("code/1_construct_tax_units.R", echo = TRUE)

# 2. Prepare Input Files for TAXSIM --------------------------------------------------

# Impute property tax payments
source("code/2a_impute_proptax.R", echo = TRUE)


# Impute number of years since moving to current dwelling
source("code/2b_impute_missing_movedin.R", echo = TRUE)

# Construct annual mortgage interest rate data
source("code/2c_construct_ir_data.R", echo = TRUE)

# Then run this STATA do file, which prepares input files for TAXSIM
#   code/2d_taxsimclean.do


# 3. Run TAXSIM -----------------------------------------------------------

# Run this STATA do file
#   code/3_taxsim_local.do


# 4. Merge TAXSIM Outputs onto Tax-Unit-Level ACS Data --------------------
source("code/4_construct_cleaned_acs_with_atr_all.R", echo = TRUE)


# 5. Produce Figures on Overall Telework Trends ---------------------------
source("code/5_descriptive_graphs.R", echo = TRUE)


# 6. Main Analysis --------------------------------------------------------

# construct PUMA and migration PUMA crosswalk
source("code/6a_construct_migpuma_puma_xwalk.R", echo = TRUE)

# Construct mean PUMA-level housing values
source("code/6b_construct_migpuma_puma_hh_vars.R", echo = TRUE)

# Construct mean PUMA-level housing values
source("code/6c_identify_conven_workers_to_drop.R", echo = TRUE)

# construct AGI adjustment factor
source("code/6d_construct_agi_by_incbin_factor.R", echo = TRUE)

# Produces figrues and a table for the main analysis
source("code/6e_main_analysis.R", echo = TRUE)


# 7. Produce Two Correlation Figures --------------------------------------

# construt TAXSIM imputes using the representative taxpayer approach
source("code/7a_construct_rep_taxsim_inputs.R", echo = TRUE)


# Back to Stata.
# Run TAXSIM using the following file:
#   code/7b_taxsim_local_rep.do

# Correlates state sales taxes with average ATR measures.
source("code/7c_state_taxes_correlate.R", echo = TRUE)

# Correlates Job postings with average ATR measures
source("code/7d_postings_correlate.R", echo = TRUE)

# 8. Counterfactuals: Tax Revenue Losses ----------------------------------

# revenue change simulation
source("code/8_revenue_simulation.R", echo = TRUE)












