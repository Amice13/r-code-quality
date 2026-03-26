################################################################################
# Created By: Pietryka
# Creation Date:  2017-03-15
# Purpose: COMPARE NEW AND OLD SCALES
# R Version: R version 3.3.3 (2017-03-06)
# Data Input: Data/Derived/2016/AP16-3-DIF.RData
# Data Output: Data/Derived/2016/AP16-4-Compare_New_and_Old.RData
# NOTES: Input data created in 'Analysis\AP16-3-DIF.R'
#
# Questions: mpietryka@fsu.edu
################################################################################

#=============================================
#'# (1) PREAMBLE
#=============================================

# ----------------- LOAD PACKAGES ---------------- #
library(tidyverse) # MANY CONVENIENCE FUNCTIONS
library(TAM)       # RASCH MODELS
library(survey)    # WEIGHTED REGRESSION MODELS


# ----------------- LOAD DATA ---------------- #

# LOAD OBJECTS THAT IDENTIFY SCALE ITEMS
source("DataClean/scale_items.R")

# 'dif_nested_16' OBJECT CREATED IN 'Analysis\AP16-3-DIF.R'
load("Data/Derived/2016/AP16-3-DIF.RData")


# HOW TO HANDLE LONELY PSU (STRATUM 111 HAS ONLY ONE PSU)
## http://r-survey.r-forge.r-project.org/survey/exmample-lonely.html
options(survey.lonely.psu = "adjust")


#=============================================
#'# (2) FIT REGRESSIONS OFF OFF-THE-SHELF AND
#'# CORRECTED ABILITY ESTIMATES ON GROUPING VARIABLE
#=============================================

compare_nested_16 <- dif_nested_16  %>%
  # DOES SCALE SHOW DIF?
  mutate(no_dif = map2_lgl(item_names, good_items, ~ all(.x %in% .y) ))  %>%
  # MEASURE RAW, UNCORRECTED SCALE SCORE
  mutate(mean_all = map2(data, item_names, ~rowMeans(
    select(.x, one_of(.y)), na.rm = TRUE
    )))  %>%
  # STANDARDIZE SCALE/ABILITY ESTIMATES
  mutate(z_all = map(mean_all, scale))  %>%
  mutate(z_ability = map(ability, scale))  %>%
  # ADD RAW SCALE SCORE AND CORRECTED ABILITY INTO 'data'
  mutate(data = map2(data, z_all,  ~ mutate(.x, z_all = .y))) %>%
  mutate(data = map2(data, z_ability,  ~ mutate(.x, z_ability = .y))) %>%
  # TRANSFORM 'group_var' TO FACTOR
  mutate(data = map(data,  ~ mutate_at(.x,
                                       .vars = "group_var",
                                       .funs = as.factor))) %>%
  # CREATE WEIGHTED DATA
  mutate(svy_design = map(data, ~ svydesign(
    ids = ~ psu_full,
    strata = ~ strata_full ,
    data = .x ,
    weights = ~ weight_full ,
    nest = TRUE))
  ) %>%
  # FIT REGRESSION ON OFF-THE-SHELF SCALE, TIDY
  mutate(full_lm_raw = map(svy_design,
                      ~ svyglm(z_all ~ group_var, design = .x))) %>%
  mutate(lm_raw = map(full_lm_raw, broom::tidy))  %>%
  mutate(coef_raw = map(lm_raw, "estimate"))  %>%
  mutate(se_raw = map(lm_raw, "std.error"))   %>%
  # FIT REGRESSION ON IMPROVED SCALE, TIDY
  mutate(full_lm_ability = map(svy_design,
                          ~ svyglm(z_ability ~ group_var, design = .x))) %>%
  mutate(lm_ability = map(full_lm_ability, broom::tidy))  %>%
  mutate(coef_ability = map(lm_ability, "estimate"))  %>%
  mutate(se_ability = map(lm_ability, "std.error"))   %>%
  # COMPARE OFF-THE-SHELF SCALE RESULTS WITH IMPROVED SCALE
  mutate(term = map(lm_raw, "term"))  %>%
  mutate(diff = map2(coef_raw, coef_ability, ~ .x - .y ))  %>%
  mutate(se_diff = map2(se_raw, se_ability, ~ sqrt(.x^2 + .y^2)))  %>%
  mutate(lb_diff = map2(diff, se_diff, ~ .x - 1.96 * .y))  %>%
  mutate(ub_diff = map2(diff, se_diff, ~ .x + 1.96 * .y))  %>%
  # CORREALTION B/W OFF-THE-SHELF SCALE AND IMPROVED SCALE
  mutate(cor = map_dbl(data, ~cor(.x$z_all, .x$z_ability,
                                  use = "pairwise.complete.obs")))


save(compare_nested_16, file = "Data/Derived/2016/AP16-4-Compare_New_and_Old.RData")



#=============================================
#'# (3) CHECK CORELATION B/W  RAW SCALE SCORE AND CORRECTED ABILITY
#=============================================

# CHECK CORELATION B/W  RAW SCALE SCORE AND CORRECTED ABILITY
summary(compare_nested_16$cor)

compare_nested_16  %>%
  group_by(scale_name)  %>%
  summarise(mean(cor))

#+ fig.width=8, fig.height=4
qplot(compare_nested_16$cor, geom = "histogram")


#===========================================================================
#'#   (4) DISPLAY VERSION NUMBERS FOR R & PACKAGES IN USE
#===========================================================================
sessionInfo()
