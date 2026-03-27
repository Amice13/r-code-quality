################################################################################
# Created By: Pietryka
# Creation Date:  2017-03-09
# Purpose: FIT DIF TESTS
# R Version: R version 3.3.3 (2017-03-06)
# Data Input: Data/Derived/2012/AP12-1-Load_Data.RData
# Data Output: Data/Derived/2012/AP12-2-Rasch_Models.RData
# NOTES: Input data created in 'Analysis\AP12-2-Rasch_Models.R'
#
# Questions: mpietryka@fsu.edu
################################################################################

#=============================================
#'# (1) PREAMBLE
#=============================================

# ----------------- LOAD PACKAGES ---------------- #
library(tidyverse) # MANY CONVENIENCE FUNCTIONS
library(TAM)       # RASCH MODELS

# ----------------- LOAD DATA ---------------- #
# 'models_nested' OBJECT CREATED IN 'Analysis\AP12-2-Rasch_Models.R'
load("Data/Derived/2012/AP12-2-Rasch_Models.RData")

# LOAD FUNCTION TO CHECK DIF
source("Functions/FUN-check_dif.R")

#=============================================
#'# (2) CHECK FOR DIF
#=============================================

# CHECK FOR DIF
dif_nested_12 <- models_nested_12  %>%
  mutate(dif_list = map2(focal_mod, data, check_dif))

# EXTRACT DATA FROM DIF TESTS
dif_nested_12 <- dif_nested_12  %>%
  mutate(anova = map(dif_list, `[[`, "anovas"))  %>%
  mutate(good_items = map(dif_list, `[[`, "good_items"))  %>%
  mutate(no_good_items = map_lgl(good_items, ~ length(.x) == 0))  %>%
  mutate(ability = map(dif_list, `[[`, "ability"))  %>%
  mutate(mod_final = map(dif_list, `[[`, "mod_final"))



save(dif_nested_12, file = "Data/Derived/2012/AP12-3-DIF.RData")

# This code takes a while to execute. You can sound an alert when the code
# finishes by uncommenting the line, below (after installing the beepr package
# with `install.packages("beepr")`)
#beepr::beep(sound = 8)

#===========================================================================
#'#   (3) DISPLAY VERSION NUMBERS FOR R & PACKAGES IN USE
#===========================================================================
sessionInfo()
