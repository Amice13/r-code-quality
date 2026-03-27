# Package setup
if (!require("pacman")) install.packages("pacman", repos = "http://cran.us.r-project.org"); library(pacman);
pacman::p_load(here, yaml, readr, arrow, tidyr, glue, argparse, purrr, furrr, stringr, dplyr);
options(scipen = 999)

# Working directory and command line argument setup
here::i_am("run_regressions/src/run_regressions.R")
source(here("R", "project_functions.R"))
cl_args <- parse_make_args(c(
  "PROJECT_CONFIG",
  "TASK_CONFIG",
  "SAMPLE_SETS_CONFIG_FILE",
  "COVARIATE_SETS_CONFIG_FILE",
  "OUTCOME_SETS_CONFIG_FILE",
  "SUBGROUPS_CONFIG_FILE",
  "XSECTION_ANALYSIS_FILE",
  "PANEL_ANALYSIS_FILE",
  "OUT_DIR"
))

# ---

source('R/regression_functions.R')
source('R/generic_helper_functions.R')

# Read in config file(s)
project_config <- read_yaml(cl_args$PROJECT_CONFIG)
task_config <- read_yaml(cl_args$TASK_CONFIG)
sample_set_lookup <- read_yaml(cl_args$SAMPLE_SETS_CONFIG_FILE)
covariate_set_lookup <- read_yaml(cl_args$COVARIATE_SETS_CONFIG_FILE)
outcome_set_lookup <- read_yaml(cl_args$OUTCOME_SETS_CONFIG_FILE)
subgroup_definitions <- read_yaml(cl_args$SUBGROUPS_CONFIG_FILE)

# Read in inputs
xsection_df <- load_analysis_file(cl_args$XSECTION_ANALYSIS_FILE, 'xsection', subgroup_definitions)
panel_df <- load_analysis_file(cl_args$PANEL_ANALYSIS_FILE, 'person_day_panel', subgroup_definitions)

# Define main driver function to be called by each worker
run_regressions <- function(data, yvar, win, tvar, take.var, covariate_set_name,
                            subgroup, regression_type, regression_functions, more_params = list()) {

  # Source functions now that we're within a worker
  source('../R/project_functions.R')
  for (r_function_script in list.files('R', pattern = '.R$', full.names = T)) {
    source(r_function_script)
  }

  # Get list of covariates and fixed effects
  xvars = get_covariates(data, covariate_set_name, covariate_set_lookup, subgroup)

  # Append the time window to takeup and outcome (typically for xsectional regressions)
  if (is_true(more_params$append_window_to_takeup)) {
    take.var <- paste0(take.var, '_', win)
  }
  if (is_true(more_params$append_window_to_yvar)) {
    yvar <- paste0(yvar, '_', win)
  }

  # Run any pre-processing functions specified in the analysis parameters
  more_params = c(more_params, list(window_cut=win, yvar=yvar, xvars=xvars))
  if (!is.null(more_params$pre_processing_functions)) {
    for (pre_proc_func in more_params$pre_processing_functions) {
      data = get(pre_proc_func)(data, more_params)
    }
  }

  # Reduce data to only columns we need (important for speed)
  # Note: we always need the three main outcomes so we can compute the index as needed
  data <- data %>%
    filter(!is.na(get(yvar))) %>%
    mutate(control = get(tvar) == 0) %>%
    select(cluster, any_of(c(take.var, 'months', more_params$weight_col)),
            all_of(c(tvar, yvar, xvars, 'new_blockid')),
            control, contains('subgroup'),
            starts_with('v_shooting_or_homicide_post_'),
            starts_with('a_shooting_or_homicide_post_'),
            starts_with('a_partone_non_shooting_or_homicide_post_'),
            starts_with('j_days_available_'))

  # Call specific regression_type functions
  df.regs = list()
  for (regression_function in regression_functions) {
    regression_function = paste0('run_', regression_function)
    df.reg = get(regression_function)(
      data, yvar, tvar, take.var, xvars, subgroup, regression_type, more_params)
    df.regs[[regression_function]] = df.reg
  }
  df.reg = reduce(df.regs, left_join, by = "subgroup")

  # Get stats like N and control mean
  if (is_true(more_params$skip_basic_descriptive_stats, default = F)) {
    # Note: some specifications like fwer resampling and pathway-reweighted outcomes
    #       already include stats like N, control mean, and treatment mean
    res = df.reg
  } else {
    df.stats <- get_basic_descriptive_stats(data, yvar, tvar, subgroup)
    res <- full_join(df.stats, df.reg, by = 'subgroup')
  }

  # Clean up results table
  res <- res %>%
    mutate(window = win,
            outcome = gsub(paste0("_", win), "", yvar),
            covariate_set = covariate_set_name,
            treatment_var = tvar) %>%
    select(outcome, window, covariate_set, subgroup, everything())

  # Run any post-processing functions specified in the analysis parameters
  if (!is.null(more_params$post_processing_functions)) {
    for (post_proc_func in more_params$post_processing_functions) {
      res = get(post_proc_func)(data, res, more_params)
    }
  }

  return(res)
}


# Run regressions
for (regression_name in task_config$regressions_to_run) {

  message(regression_name)

  # Consolidate regression-specific parameters and project-wide parameters
  # Note: if the two configs have a key in common (e.g. window_cut_list),
  #       the regression-specific value will persist.
  params = task_config$regressions[[regression_name]]
  params = c(params, project_config)
  params = params[unique(names(params))]

  df = get(glue("{params$regression_type}_df"))
  df_sample = filter_to_correct_sample(df, params$sample, sample_set_lookup)

  # If the current regression doesn't involve subgroups, set the subgroup list
  # to a default value of c("full_sample") (needed to facilitate looping)
  if (length(params$subgroup_list) == 0) {
    params$subgroup_list = c("full_sample")
  }

  outcome_list = get_outcomes(
    df_sample,
    params$outcome_set_list,
    params$outcome_suffix_list,
    outcome_set_lookup,
    check_for_existance = is_true(params[['validate_outcomes']], default = TRUE)
  )

  crosslist <- crossing(
    data = list(df_sample),
    yvar = outcome_list,
    win = params$window_cut_list,
    tvar = params$treatment_var,
    take.var = params$takeup_var,
    covariate_set = params$covariate_set_list,
    subgroup = params$subgroup_list,
    regression_type = params$regression_type,
    regression_functions = list(params$regression_functions),
    more_params = list(params)
  )

  # Run models
  plan(multisession, workers = 8)
  df_out <- future_pmap_dfr(
    crosslist, run_regressions, .options = furrr_options(
      seed = params$seed))

  regression_output = df_out %>% select(-matches("formula"))

  formula_output = df_out %>%
    select(outcome, window, covariate_set, subgroup, matches("formula")) %>%
    distinct()

  regression_output_file = glue("{cl_args$OUT_DIR}/regression_output__{regression_name}.csv")
  write_csv(regression_output, regression_output_file)

  formula_file = glue("{cl_args$OUT_DIR}/formulas__{regression_name}.csv")
  write_csv(formula_output, formula_file)

}
