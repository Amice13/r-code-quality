rm(list = ls())

# Please set Working Directory to Source File Location


library('car')
library(here)
library(tidyverse)
library(glue)
library(ebal)



# Data cleaning / pre-processing  ----------------------------------------------

source('00_import-data.R')
source('01_clean-launch-1.R')
source('02_scramble-treatment.R')
source('03_standardize-responses.R')

# Load Data --------------------------------------------------------------------
message("Loading data...")
resp <- read_rds("temp/launch_1_std.rds")
# Keep randomized observations
resp <- resp %>% filter(!is.na(covid_any))

resp <- resp %>% filter(ethnicity != "other")

# Correction of some errors 
to_remove = c()
for (i in 1:nrow(resp)){
  
  if (is.na(resp[i,"response_id"]) ){
    to_remove = c(to_remove,i)
  }
  # people who responded to the follow up survey twice and only completed it the second time
  else if ((resp[i,"response_id"] %in% c("R_29cOSbpEVDN3iyF","R_3I9N9toKGEZQp76","R_cCseWstAAeZMtPj")) && (resp[i,"no_follow"]==0)){
    to_remove = c(to_remove,i)
  }
  # an error:
  else if (resp[i,"response_id"] =="R_1mLvgGXSulgzgqx"){
    
    to_remove = c(to_remove,i)
  }
  
}

resp = resp[-to_remove,]

# Analysis  --------------------------------------------------------------------
# (the following scripts can be run independently)
source("Summary_and_balance.R") # generates Table 1 (Summary Table), the numbers for Supplement Table 1a (Balance Table)

source("attrition_table.R")# generates Supplement Table 1b (Attrition Table)

source("main_regressions.R")# runs the regressions for Tables 2 and 3 (default), can also be used to get Supplement Tables 3, 4, 5, 6, 7, 8a, 8b, 11

source("additional_tests.R")# runs the main regressions with q-values (Tables 2 and 3) and some F-stats for Table 3

source("AMA_table.R")# runs the regressions for Supplement Table 9

source("black_treatments_regressions.R")# runs the regressions for Supplement Table 12

source("consort_chart_numbers.R")# generates the numbers for the consort chart (Figure 1)

source("histograms.R")# generates the histograms (Figure 2 and 3)

source("interaction_tables.R")# runs the regressions for Supplement Table 2 (from 2a to 2g)

source("regressions_follow_up_sample.R")# runs the regressions for Supplement Table 10

source("power_calculations.R")# produces power calculations
