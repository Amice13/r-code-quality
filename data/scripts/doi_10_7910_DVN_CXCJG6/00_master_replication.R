############################################
#         00_MASTER_REPLICATION            #
############################################

## Libraries
library(here)

###############################
#    SCRIPT EXECUTION ORDER   #
###############################

## Step 1: Clean data and perform multiple imputation
source(here("scripts", "01_clean_data.R"))

## Step 2: Run main analysis (LCA, class membership, adoption models)
source(here("scripts", "02_main_analysis.R"))

## Step 3: Generate manuscript figures
source(here("scripts", "03_figures.R"))

## Step 4: Generate manuscript tables
source(here("scripts", "04_tables.R"))

## Step 5: Generate appendix materials
source(here("scripts", "05_appendix.R"))