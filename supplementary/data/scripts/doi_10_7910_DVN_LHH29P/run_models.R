# Package setup
if (!require("pacman")) install.packages("pacman", repos = "http://cran.us.r-project.org"); library(pacman);
pacman::p_load(yaml, here, arrow, magrittr, dplyr, glue, argparse, stringr, purrr)
options(scipen = 999)

# Working directory and command line argument setup
here::i_am("run_benefit_cost_regressions/src/run_models.R")
source(here("R", "project_functions.R"))
cl_args <- parse_make_args(c(
  "TASK_FNS",
  "TASK_CONFIG",
  "PROJECT_CONFIG",
  "COVAR_CONFIG",
  "PROD_ROSTER_W_COSTS",
  "OUT_FILE"
))

# ---

source(here("R", "project_functions.R"))
source(cl_args$TASK_FNS)

# Read in config file specifying unique decisions made for this task ----
task_config <- read_yaml(cl_args$TASK_CONFIG)
covar_config <- read_yaml(cl_args$COVAR_CONFIG)
outcome_pline_config <- read_yaml(cl_args$PROJECT_CONFIG)

# Specify name of input files ----
prod_roster_w_costs <- read_feather(file.path(cl_args$PROD_ROSTER_W_COSTS))

# Create new cost columns and add total costs ----
prod_roster_w_costs_w_xtra_cols <- prod_roster_w_costs %>%
        rowwise() %>%
        # add total costs
        mutate(post_20_cj_cost_total_2017_all_a_plus_post_20_cj_clearance_rate_scaled_down_cost_total_2017_all_v =
                       sum(c_across(all_of(task_config$more_inclusive_post_20_cj_cost_total_2017_components))),
               post_20_offender_productivity_cost_total_2017_all_a_plus_post_20_offender_productivity_clearance_rate_scaled_down_cost_total_2017_all_v =
                       sum(c_across(all_of(task_config$more_inclusive_post_20_offender_productivity_cost_total_2017_components))),
               less_inclusive_cost_total = sum(c_across(all_of(task_config$less_inclusive_cost_total_components))),
               more_inclusive_cost_total = sum(c_across(all_of(task_config$more_inclusive_cost_total_components)))) %>%
        ungroup()

covariates <- c(outcome_pline_config["treatment_var"],
                c(covar_config$covariate_sets$blocks$add,
                  covar_config$covariate_sets$baseline$add)) %>%
        unname() %>%
        unlist()

reg_results <- run_itt_and_tot_models_multi_outcomes(
        task_config$outcomes,
        t_var = outcome_pline_config$treatment_var,
        take_var = paste0(outcome_pline_config$takeup_var, "_post_20"),
        covariates = covariates,
        prod_roster_w_costs_w_xtra_cols)

# Write out roster with costs of violence ----
write_feather(reg_results, file.path(cl_args$OUT_FILE))
write_feather(reg_results, file.path(str_c(
        # remove the .feather extension from the output file name
        str_remove(cl_args$OUT_FILE, ".feather"),
        "_",
        # replace all symbols and spaces from Sys.time() with '_'
        str_replace_all(Sys.time(), pattern = "-| |:", "_"),
        # add the .feather extension back to the output file name
        ".feather")))
