# Package setup
if (!require("pacman")) install.packages("pacman", repos = "http://cran.us.r-project.org"); library(pacman)
pacman::p_load(here, yaml, readr, arrow, tidyr, glue, argparse, purrr, furrr, stringr, tidytable)
options(scipen = 999)

# Working directory and command line argument setup
here::i_am("run_stata_dosage_model/src/prepare_data_and_covs_for_regressions.R")
source(here("R", "project_functions.R"))
cl_args <- parse_make_args(c(
  "PROJECT_CONFIG",
  "COVARIATE_SETS_CONFIG_FILE",
  "XSECTION_ANALYSIS_FILE",
  "XSECTION_ANALYSIS_FILE_CSV",
  "PARAM_FILE"
))

# ---

# Read in config file(s)
project_config <- read_yaml(cl_args$PROJECT_CONFIG)
covariate_set_lookup <- read_yaml(cl_args$COVARIATE_SETS_CONFIG_FILE)

# Read in inputs
xsection_df <- read_feather(cl_args$XSECTION_ANALYSIS_FILE)

# Output xsection_df as csv for ingestion into stata
xsection_df %>%
  # rename columns whose names were too long
  rename_with(.cols = contains("a_partone_non_shooting_or_homicide"),
              .fn = ~gsub("a_partone_non_shooting_or_homicide", "a_p1_non_sh", .x)) %>%
  write_csv(cl_args$XSECTION_ANALYSIS_FILE_CSV)

# Convert list of covariates likely to change into stata script
dl_non_block_covs <- covariate_set_lookup$covariate_sets$baseline_continuous$add %>%
  # rename covariates with too-long names
  str_replace_all("a_partone_non_shooting_or_homicide", "a_p1_non_sh") %>%
  paste0(collapse = " ")

dl_block_covs <- covariate_set_lookup$covariate_sets$blocks$add %>%
  paste0(collapse = " ")

# Obtain correct treatment var
dl_treatment_var <- project_config$treatment_var

# Write out parameters in a form that STATA can ingest and use
write_lines(c(glue("local baselines {dl_non_block_covs}"), # Load covariates
              glue("local strataFE {dl_block_covs}"), # Load strata FEs
              glue("local treatment {dl_treatment_var}"), # Load name of treatment var
              # Print parameters for help when debugging - take out the "-b" option in makefile to test
              "macro list _baselines",
              "macro list _strataFE",
              "macro list _treatment"
              ), # Print treatment variable for eye test
            cl_args$PARAM_FILE,sep = "\n\n")
