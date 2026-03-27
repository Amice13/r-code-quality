# ==========================================
# Description: Create Table IV
# ==========================================

### ========================================
### Set Up =================================
### ========================================
# Set working directory to the directory to which this file is saved
library("rstudioapi")
setwd(dirname(getActiveDocumentContext()$path))

# Load Packages
source("requirements.R")


### ========================================
### Data Prep ==============================
### ========================================
# Load data
load("../data/library_data.Rdata")


### ========================================
### Table ==================================
### ========================================
# Dependent Variables
# These collection names correspond to variables
# containing the number of books from each collection
# contained in each branch

models <- list()
models$m1 <- lfe::felm(
  mainstream ~ pct_nothisp_white_one + total_books + total_population
                       |0|0|item_location, data=branches)
models$m2 <- lfe::felm(
  diversity ~ pct_nothisp_white_one + total_books + total_population
                       |0|0|item_location, data=branches)
models$m3 <- lfe::felm(
  mainstream ~ pct_nothisp_white_one + median_hh_inc_past_12mo_dollar + 
    pct_population_under_poverty + total_books + total_population
                       |0|0|item_location, data=branches)
models$m4 <- lfe::felm(
  diversity ~ pct_nothisp_white_one + median_hh_inc_past_12mo_dollar + 
    pct_population_under_poverty + total_books + total_population
                       |0|0|item_location, data=branches)


# Print the models
stargazer(models, 
          type="text",
          dep.var.labels = rep(c("Mainstream","Diversity"), 2),
          dep.var.caption = "Dependent Variable: Number of Award Winning Books by Collection",
          out="../tables/table_IV.txt",
          keep.stat = c("n", "adj.rsq"))

