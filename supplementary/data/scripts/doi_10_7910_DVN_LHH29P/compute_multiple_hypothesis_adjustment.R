# Package setup
if (!require("pacman")) install.packages("pacman", repos = "http://cran.us.r-project.org"); library(pacman);
pacman::p_load(yaml, dplyr, testthat, argparse, stringr, here, tidytable, glue);

# Working directory and command line argument setup
here::i_am("compute_multiple_hypothesis_adjustment/src/compute_multiple_hypothesis_adjustment.R")
source(here("R", "project_functions.R"))
cl_args <- parse_make_args(c(
  "CONFIG_FILE",
  "OUTCOME_CONFIG_FILE",
  "PROJECT_CONFIG",
  "RESAMPLING_RESULTS_FILE",
  "RESAMPLING_BY_SUBGROUP_RESULTS_FILE",
  "OUT_DIR"
))

# ---

source('R/multiple_hypothesis_functions.R')

# Read in inputs
project_config <- read_yaml(cl_args$PROJECT_CONFIG)
config <- read_yaml(cl_args$CONFIG_FILE)
outcome_set_lookup <- read_yaml(cl_args$OUTCOME_CONFIG_FILE)
resampling_results_df <- fread(cl_args$RESAMPLING_RESULTS_FILE) %>% as_tidytable()
resampling_subgroup_results_df <- fread(cl_args$RESAMPLING_BY_SUBGROUP_RESULTS_FILE) %>% as_tidytable()

# Combine resampling results
resampling_results_df = bind_rows.(resampling_results_df, resampling_subgroup_results_df) %>%
  separate.(subgroup, c('subgroup_type', 'subgroup_value'), '__', remove = F) %>%
  mutate.(subgroup_value = replace_na(subgroup_value, 'full_sample'))

for (family in names(config$families)) {

  family_specs = config$families[[family]]
  outcome_list = get_outcomes(NULL, family_specs$outcomes, NULL, outcome_set_lookup, check_for_existance = F)

  if (family_specs$sample == 'full_sample') {
    df = resampling_results_df %>% filter(subgroup_type == 'full_sample')
  } else {
    df = resampling_results_df %>% filter(subgroup_type != 'full_sample')
  }

  df = df %>%
    filter(outcome %in% outcome_list)

  multiple_test_col = family_specs$multiple_test_dimension
  key_cols = c('outcome', 'window', 'covariate_set', 'subgroup_type', 'subgroup_value')
  grouping_cols = setdiff(key_cols, multiple_test_col)

  specifications = df %>%
    summarize.(!!multiple_test_col := list(unique(get(multiple_test_col))),
               .by = all_of(grouping_cols))

  all_out = list()
  for (i in 1:nrow(specifications)) {

    specification = specifications[i]

    out = wyoung(
      df,
      outcomes = unlist(specification$outcome),
      windows = specification$window,
      covariate_sets = specification$covariate_set,
      subgroup_types = specification$subgroup_type,
      subgroup_values = unlist(specification$subgroup_value),
      multiple_test_col = multiple_test_col,
      key_cols = key_cols
    )

    all_out[[i]] = out
  }

  out = bind_rows.(all_out)
  output_file = paste0(cl_args$OUT_DIR, '/', family, '.csv')
  fwrite(out, output_file)

}
