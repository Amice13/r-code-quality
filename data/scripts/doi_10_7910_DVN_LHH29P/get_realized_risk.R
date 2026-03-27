# Package setup
if (!require("pacman")) install.packages("pacman", repos = "http://cran.us.r-project.org"); library(pacman);
pacman::p_load(yaml, readr, dplyr, tidyr, arrow, glue, argparse, purrr, furrr, stringr, here, AER);

# Working directory and command line argument setup
here::i_am("get_realized_risk/src/get_realized_risk.R")
source(here("R", "project_functions.R"))
cl_args <- parse_make_args(c(
  "CONFIG_FILE",
  "PROJECT_CONFIG",
  "XSECTION_ANALYSIS_FILE",
  "WINZ_FN",
  "OUT_FILE"
))

# ---

source('R/realized_risk_functions.R')
source(cl_args$WINZ_FN)

# Read in inputs
project_config <- read_yaml(cl_args$PROJECT_CONFIG)
config <- read_yaml(cl_args$CONFIG_FILE)
analysis_df <- read_feather(cl_args$XSECTION_ANALYSIS_FILE)

# Get list of outcome variables we care about for each time of risk
baseline_outcomes <- config$baseline_risk_outcomes
realized_outcomes <- crossing(outcome = config$realized_risk_outcomes_to_cross,
                              window = project_config$window_cut_list) %>%
  unite(col = "outcome", outcome, window, sep = "_") %>%
  bind_rows(tibble(outcome = config$realized_risk_outcomes_with_windows)) %>%
  pull(outcome)

# Select only relevant analysis vars
analysis_df <- analysis_df %>%
  select(cluster, starts_with("t"), contains("pathway"),
         all_of(baseline_outcomes), all_of(realized_outcomes))

for (type in c("realized", "baseline")) {
  print(type)
  if (type == "realized") {
    subset_analysis_df <- analysis_df %>%
      filter(treatment == 0)
    sample = "control"
    output = cl_args$OUT_FILE
  } else {
    subset_analysis_df <- analysis_df
    subset_analysis_df <- winsorize(subset_analysis_df, more_params=list(winsorize_pctl = .99))
    sample = "all"
    output = gsub("realized_risk_by", "baseline_risk_by", cl_args$OUT_FILE)
  }

  res <- tibble()
  for (outcome_var in get(paste0(type, "_outcomes"))) {

    mean_and_pathway_ftest = getRisk(subset_analysis_df, outcome_var)
    res <- bind_rows(res, mean_and_pathway_ftest)
  }

  n = getN(subset_analysis_df)
  res <- bind_rows(res, n) %>%
    mutate(type = type,
           subgroup = sample)

  # Output files
  write_csv(res, gsub("output", "output/", output))
}
