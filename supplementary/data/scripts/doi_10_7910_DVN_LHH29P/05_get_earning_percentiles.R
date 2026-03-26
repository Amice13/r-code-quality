if (!require("pacman")) install.packages("pacman", repos = "http://cran.us.r-project.org"); library(pacman);
p_load(tidyverse, lubridate, arrow, yaml, here, argparse, glue)

here::i_am("generate_tables_and_figures/gen_payroll_outputs/src/05_get_earning_percentiles.R")
source(here("R", "project_functions.R"))

cl_args <- parse_make_args(c(
  "CONFIG_FILE",
  "PROJECT_CONFIG",
  "XSECTION_ANALYSIS_FILE",
  "PAYROLL_FILE",
  "OUT_WAGE_PERCENTILES"
))

source("R/payroll_functions.R")

config <- read_yaml(cl_args$CONFIG_FILE)
project_config <- read_yaml(cl_args$PROJECT_CONFIG)

#wage growth over time, by pathway plot
payroll <- read_feather(cl_args$PAYROLL_FILE)

roster_twenty <- read_feather(cl_args$XSECTION_ANALYSIS_FILE) %>%
  filter(grepl("20", window),treatment == 1) %>%
  mutate(ran_date = ymd(ran_date))


# Create period - specific cols that we will pass in to the function
payroll_twenty <- get_payroll_20(payroll, roster_twenty, project_config$days_per_month)

covid_cut <- project_config$covid_cut
standby_cut <- config$standby_cut

paths <- c("all","cr","ul","re")

periods <- c("all", "standby", "precovid", "postcovid")

cols <- c("t_payroll_work_earnings", "t_payroll_work_hours", "t_payroll_training_earnings", "t_payroll_training_hours")

ntiles <- c(.25, .5, .75)

clist <- crossing(payroll_df = list(payroll_twenty),
                  roster_df = list(roster_twenty),
                  var = cols,
                  path = paths,
                  period = periods,
                  ntiles = list(ntiles))


payroll_percentiles <- pmap_dfr(clist, calc_payroll_percentiles)

out_df <- payroll_percentiles %>%
  filter(period == "all", type == "t_payroll_work_hours", measure %>% str_starts("qnt_")) %>%
  pivot_wider(names_from = measure, values_from = n)

out_df %>%
  write_csv(cl_args$OUT_WAGE_PERCENTILES)
