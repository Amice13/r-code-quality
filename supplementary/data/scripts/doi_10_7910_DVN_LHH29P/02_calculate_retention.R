if (!require("pacman")) install.packages("pacman", repos = "http://cran.us.r-project.org"); library(pacman);
p_load(tidyverse, lubridate, arrow, yaml, here, argparse, glue)


here::i_am("generate_tables_and_figures/gen_retention_plots/src/02_calculate_retention.R")
source(here("R", "project_functions.R"))

cl_args <- parse_make_args(c(
  "CONFIG_FILE",
  "PROJECT_CONFIG",
  "XSECTION_ANALYSIS_FILE",
  "OUT_RETENTION_PANEL",
  "OUT_WEEKLY_RETENTION"
))


config <- read_yaml(cl_args$CONFIG_FILE)

project_config <- read_yaml(cl_args$PROJECT_CONFIG)

source("R/retention_functions.R")

# Load in roster and payroll panel
roster_path <- read_feather(cl_args$XSECTION_ANALYSIS_FILE) %>%
  mutate(ran_date = ymd(ran_date),
         g_pathway_all = 1) %>%
  filter(treatment == 1) %>%
  select(UL_id_unique, treatment, ran_date, contains("g_pathway"), window)

retention_panel <- read_csv(cl_args$OUT_RETENTION_PANEL)

# Keep only study members who show up at least once and only weeks after person shows up for the first time
retention_clean <- retention_panel %>%
  left_join(roster_path, "UL_id_unique") %>%
  mutate(t_period_all = 1,
         t_period_precovid = as.numeric(ymd(week_date) < ymd(project_config$covid_cut_date)),
         work_week = case_when(any_work == 1 ~ week_num)) %>%
  group_by(UL_id_unique) %>%
  # Include only those who ever show up in payroll
  filter(max(any_work) == 1) %>%
  # Keep only weeks after they first show up
  mutate(first_week = min(work_week, na.rm = T)) %>%
  filter(week_num >= first_week) %>%
  arrange(week_num) %>%
  # 1-n for each person beginning with the first week they showed up
  mutate(person_week = 1:n()) %>%
  ungroup() %>%
  select(UL_id_unique, person_week, any_work, starts_with("g_pathway"), starts_with("t_period"))

# Get pathway/time-period combos
clist <- crossing(retention_df = list(retention_clean),
                  period = config$periods,
                  path = config$paths,
                  n_weeks_20 = config$total_weeks)
                  
retention_by_week <- pmap_dfr(clist, get_weekly_retention)

retention_by_week %>%
  write_csv(cl_args$OUT_WEEKLY_RETENTION)
