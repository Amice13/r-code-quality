################################################################################
# Created By: Pietryka
# Creation Date:  2017-03-09
# Purpose: LOAD AND RESHAPE DATA FOR RASCH MODELS
# R Version: R version 3.3.3 (2017-03-06)
# Data Input: Data/Derived/2012/groups_and_scales.csv
# Data Output: Data/Derived/2012/1-Load_Data.RData
# NOTES: 'groups_and_scales.csv' created in  ...
#         ... AP - ANES Preregistration\Work\DataClean\anes-4-define_scales.R
#
# Questions: mpietryka@fsu.edu
################################################################################

#=============================================
#'#  1. PREAMBLE
#=============================================

# ----------------- LOAD PACKAGES ---------------- #
library(tidyverse) # MANY CONVENIENCE FUNCTIONS

# ----------------- LOAD DATA ---------------- #
# 'a12_scales' OBJECT CREATED IN DataClean\anes-4-define_scales.R
load("Data/Derived/2012/groups_and_scales.RData")

data_raw_12 <- a12_scales  %>%
   rownames_to_column("id")

# KEEP ONLY WHITE, NON-HISPANIC Rs FOR RACIAL RESENTMENT & STEREOTYPE DATA
data_whites_12 <- data_raw_12  %>%
  filter(race_nominal == 0)

# LOAD OBJECTS THAT IDENTIFY SCALE ITEMS
source("DataClean/scale_items.R")

#===========================================================================
#'#   2. TRANSFORM TO DATA NESTED BY SCALE AND GROUPING VARIABLE
#===========================================================================

data_nested_12 <- crossing(scale_name = scale_names, group_var = group_vars)  %>%
  mutate(item_names = map(scale_name, get))  %>%
  # ASSIGN ONLY WHITE, NON-HISPANIC Rs TO RACIAL RESENTMENT  & STEREOTYPE
  # SCALES, AND ASSIGN ALL Rs TO ALL OTHER SCALES
  mutate(data = ifelse(scale_name %in% c("race_vars", "stereo_vars"),
    map2(item_names,  group_var,
         ~ select(data_whites_12,
                  id,
                  weight_full,
                  strata_full,
                  psu_full,
                  one_of(.x),
                  group_var = one_of(.y)
      )
    ),
    map2(item_names, group_var,
         ~ select(data_raw_12,
                  id,
                  weight_full,
                  strata_full,
                  psu_full,
                  one_of(.x),
                  group_var = one_of(.y))
    )
  ))  %>%
  # REMOVE RACE GROUPING VARIABLE FROM RACIAL RESENTMENT & STEREOTYPE SCALES
  filter(!(scale_name == "race_vars"   & group_var == "race_nominal"))  %>%
  filter(!(scale_name == "stereo_vars" & group_var == "race_nominal"))  %>%
  # PLACE IDs, ITEM DATA, AND GROUP VAR DATA IN DIFFERENT COLUMNS
  mutate(ids = map(data, "id"))  %>%
  mutate(item_data = map2(data, item_names, `[`))

save(data_nested_12, file = "Data/Derived/2012/AP12-1-Load_Data.RData")


#===========================================================================
#'#   3. DISPLAY VERSION NUMBERS FOR R & PACKAGES IN USE
#===========================================================================
sessionInfo()
