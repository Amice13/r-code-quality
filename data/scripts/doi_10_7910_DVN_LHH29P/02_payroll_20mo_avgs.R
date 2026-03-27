if (!require("pacman")) install.packages("pacman", repos = "http://cran.us.r-project.org"); library(pacman);
p_load(tidyverse, lubridate, arrow, yaml, here, argparse, glue)

here::i_am("generate_tables_and_figures/gen_payroll_outputs/src/02_payroll_20mo_avgs.R")
source(here("R", "project_functions.R"))

cl_args <- parse_make_args(c(
  "CONFIG_FILE",
  "PROJECT_CONFIG",
  "XSECTION_ANALYSIS_FILE",
  "PAYROLL_FILE",
  "OUT_PAYROLL_AVGS"
))


source("R/payroll_functions.R")

config <- read_yaml(cl_args$CONFIG_FILE)
project_config <- read_yaml(cl_args$PROJECT_CONFIG)

# wage growth over time, by pathway plot
payroll <- read_feather(cl_args$PAYROLL_FILE)

roster_twenty <- read_feather(cl_args$XSECTION_ANALYSIS_FILE) %>%
  filter(grepl("20", window), treatment == 1) %>%
  mutate(ran_date = ymd(ran_date))

payroll_twenty <- get_payroll_20(payroll, roster_twenty, project_config$days_per_month)

covid_cut <- project_config$covid_cut
standby_cut <- config$standby_cut

paths <- c("all","cr","ul","re")

path_clist <- crossing(path = paths,
                       payroll_df = list(payroll_twenty),
                       type = c("regular", "training"),
                       covid_cut = covid_cut,
                       standby_cut = standby_cut)

payroll_avg_df <- pmap_dfr(path_clist, payroll_avgs)


payroll_avg_df %>%
  write_csv(cl_args$OUT_PAYROLL_AVGS)

