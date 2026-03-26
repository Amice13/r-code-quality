################################################################################
# Created By: Pietryka
# Creation Date:  2017-03-09
# Purpose: FIT BASELINE RASCH MODELS
# R Version: R version 3.3.3 (2017-03-06)
# Data Input: Data/Derived/2012/AP12-1-Load_Data.RData
# Data Output: Data/Derived/2012/AP12-2-Rasch_Models.RData
# NOTES: Input data created in 'Analysis\AP12-1-Load_Data.R'
#
# Questions: mpietryka@fsu.edu
################################################################################

#=============================================
#'#   (1) PREAMBLE
#=============================================

# ----------------- LOAD PACKAGES ---------------- #
library(tidyverse) # MANY CONVENIENCE FUNCTIONS
library(TAM)       # RASCH MODELS

# ----------------- LOAD DATA ---------------- #
# 'data_nested_12' OBJECT CREATED IN 'AP12-1-Load_Data.R'
load("Data/Derived/2012/AP12-1-Load_Data.RData")


# ----------------- LOAD FUNCTIONS---------------- #

# LOAD  FUNCTION TO DETERMINE IF ONE VARIABLE IS BINARY (EXCLUDING NAs)
source("Functions/FUN-binary_check.R")

# LOAD FUNCTION TO COUNT NUMBER OF BINARY VARIABLES IN DATA (EXCLUDING NAs)
source("Functions/FUN-binary_count.R")

# FUNCTION TO FIT ANOVA, FORMAT RESULTS
source("Functions/FUN-run_lr.R")

# LOAD FUNCTION TO FIND PERSON/ITEM FIT STATS
source("Functions/FUN-get_relstats.R")

# LOAD FUNCTION TO EXAMINE, CORRECT THRESHOLD DISORDER
source("Functions/FUN-fix_disorder.R")


#=============================================
#'#   (2) CREATE NESTED DATA WITH ONLY ONE ROW PER SCALE
#=============================================

scales_nested_12 <- data_nested_12  %>%
  select(-group_var)  %>%
  distinct(scale_name, .keep_all = TRUE)  %>%
  # IDENTIFY NUMBER AND TYPE OF ITEMS
  mutate(n_items = map_int(item_names, length))  %>%
  mutate(n_binary = map_int(item_data, binary_count))  %>%
  mutate(all_binary = n_binary == n_items)

#=============================================
#'#   (3) FIT MODELS FOR SCALES WITH ONLY BINARY ITEMS
#=============================================

#'### SCALES WITH ONLY BINARY ITEMS ----------------

binary_models_12 <- scales_nested_12  %>%
  filter(all_binary == TRUE)  %>%
  mutate(mod_type = "1PL")  %>%
  mutate(mod1 = map2(
    item_data,
    data,
    ~tam.mml(
      resp = .x,
      irtmodel = "1PL",
      pweights = .y$weight_full,
      control = list(progress = FALSE)
    )
  ))  %>%
  mutate(focal_mod = mod1)


#'### SCALES WITH POLYTOMOUS ITEMS ----------------


poly_models_12 <- scales_nested_12  %>%
  filter(all_binary == FALSE)  %>%
  # FIT RATING SCALE MODEL
  mutate(mod_rsm = map2(
    item_data,
    data,
    ~tam.mml(
      resp = .x,
      irtmodel = "RSM",
      pweights = .y$weight_full,
      control = list(progress = FALSE)
  )))  %>%
  # FIT PARTIAL CREDIT MODEL
  mutate(mod_pcm = map2(
    item_data,
    data,
    ~tam.mml(
      resp = .x,
      irtmodel = "PCM",
      pweights = .y$weight_full,
      control = list(progress = FALSE)
  )))  %>%
  # LIKELIHOOD RATIO TEST
  mutate(lr_test = map2(mod_rsm, mod_pcm, run_lr))  %>%
  # EXTRACT P-VALUE
  mutate(lr_p = map(lr_test, "p"))  %>%
  mutate(lr_p = map(lr_p, 1))  %>%
  # CHOOSE PCM IF P < .05, RSM OTHERWISE
  mutate(mod_type = ifelse(lr_p < .05, "PCM", "RSM"))  %>%
  mutate(mod1 = ifelse(lr_p < .05, mod_pcm, mod_rsm))  %>%
  # EXAMINE, CORRECT THRESHOLD DISORDER
  mutate(disorder_list = map2(mod1, data, fix_disorder))  %>%
  mutate(recode_message = map_chr(disorder_list, "message"))  %>%
  mutate(is_recoded = map_lgl(disorder_list, "recoded"))  %>%
  mutate(recoded_items = map(disorder_list, "items"))  %>%
  mutate(item_data = ifelse(is_recoded == TRUE, recoded_items,  item_data))  %>%
  mutate(recoded_mod = map(disorder_list, "model"))  %>%
  mutate(focal_mod = ifelse(is_recoded == TRUE, recoded_mod,  mod1))  %>%
  mutate(disorder_remains = map_lgl(disorder_list, "disorder_remains"))


#=============================================
#'#   (4) RECOMBINE DATA AND SAVE
#=============================================
models_stacked_12 <- binary_models_12  %>%
  bind_rows(poly_models_12)  %>%
  select(
    scale_name,
    n_items,
    n_binary,
    all_binary,
    mod_type,
    is_recoded,
    disorder_remains,
    focal_mod,
    mod1,
    mod_rsm,
    mod_pcm,
    lr_test,
    lr_p,
    item_data
    )

models_nested_12 <- data_nested_12  %>% select(-item_data)  %>%
  full_join(models_stacked_12, by = "scale_name")

save(models_nested_12, file = "Data/Derived/2012/AP12-2-Rasch_Models.RData")


#=============================================
#'#   (5) DISPLAY RESULTS OF MODELS
#=============================================


map(models_stacked_12$focal_mod, summary)





#===========================================================================
#'#   (6) DISPLAY VERSION NUMBERS FOR R & PACKAGES IN USE
#===========================================================================
sessionInfo()
