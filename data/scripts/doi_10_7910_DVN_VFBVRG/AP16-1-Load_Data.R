################################################################################
# Created By: Pietryka
# Creation Date:  2017-03-09
# Purpose: LOAD AND RESHAPE DATA FOR RASCH MODELS
# R Version: R version 3.3.3 (2017-03-06)
# Data Input: Data/Derived/2016/groups_and_scales.csv
# Data Output: Data/Derived/2016/AP16-1-Load_Data.RData
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
sessionInfo()      # DISPLAY VERSION NUMBERS FOR R & PACKAGES IN USE


# ----------------- LOAD DATA ---------------- #

# 'a16_scales' OBJECT CREATED IN DataClean\anes-4-define_scales.R
load("Data/Derived/2016/groups_and_scales.RData")

data_raw_16 <- a16_scales  %>%
  rownames_to_column("id")

# LOAD RESTRICTED ACCESS DATA TO ADD PSUs (STEP NOT NEEDED IN 2012 BECAUSE PSUs
# WERE INCLUDED IN THE PUBLIC RELEASE)
rda_16 <- read_csv("Data/Source/anes_rds_2016.csv")

# THE 2012 ANES `psu_full`  VARIABLE GIVES ALL INTERNET RESPONDENTS THEIR OWN
# PSU VALUE SO WE CONSTRUCT AN ANALOGOUS ONE FOR 2016
data_rda_16 <- data_raw_16  %>%
  left_join(rda_16, by = c("caseid" = "case_id"))  %>%
  mutate(
    psu_new = max(PSUcode, na.rm = TRUE) + row_number(),
    psu_full = ifelse(internet == 1, psu_new, PSUcode)
  )


# KEEP ONLY WHITE, NON-HISPANIC Rs FOR RACIAL RESENTMENT & STEREOTYPE DATA
data_whites_16 <- data_rda_16  %>%
  filter(race_nominal == 0)

# LOAD OBJECTS THAT IDENTIFY SCALE ITEMS
source("DataClean/scale_items.R")


#===========================================================================
#'#   2. TRANSFORM TO DATA NESTED BY SCALE AND GROUPING VARIABLE
#===========================================================================

data_nested_16 <- crossing(scale_name = scale_names, group_var = group_vars)  %>%
  mutate(item_names = map(scale_name, get))  %>%
  # ASSIGN ONLY WHITE, NON-HISPANIC Rs TO RACIAL RESENTMENT  & STEREOTYPE
  # SCALES, AND ASSIGN ALL Rs TO ALL OTHER SCALES
  mutate(data = ifelse(scale_name %in% c("race_vars", "stereo_vars"),
    map2(item_names,  group_var,
         ~ select(data_whites_16,
                  id,
                  weight_full,
                  strata_full,
                  psu_full,
                  one_of(.x),
                  group_var = one_of(.y)
      )
    ),
    map2(item_names, group_var,
         ~ select(data_rda_16,
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

save(data_nested_16, file = "Data/Derived/2016/AP16-1-Load_Data.RData")



#===========================================================================
#'#   3. DISPLAY VERSION NUMBERS FOR R & PACKAGES IN USE
#===========================================================================
sessionInfo()



