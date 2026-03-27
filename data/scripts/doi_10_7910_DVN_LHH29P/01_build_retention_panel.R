if (!require("pacman")) install.packages("pacman", repos = "http://cran.us.r-project.org"); library(pacman);
p_load(tidyverse, lubridate, arrow, yaml, here, argparse, glue)


here::i_am("generate_tables_and_figures/gen_retention_plots/src/01_build_retention_panel.R")
source(here("R", "project_functions.R"))


cl_args <- parse_make_args(c(
  "CONFIG_FILE",
  "XSECTION_ANALYSIS_FILE",
  "PAYROLL_FILE",
  "OUT_RETENTION_PANEL"
))


config <- read_yaml(cl_args$CONFIG_FILE)


# Load in roster and payroll panel
roster_twenty <- arrow::read_feather(cl_args$XSECTION_ANALYSIS_FILE) %>%
  filter(treatment == 1) %>%
  mutate(ran_date = ymd(ran_date))


payroll_clean <- read_feather(cl_args$PAYROLL_FILE) %>%
  mutate(pay_week = floor_date(pay_date, unit = "week", week_start = getOption("lubridate.week.start", 1)),
         ran_week =  floor_date(ran_date, unit = "week", week_start = getOption("lubridate.week.start", 1)),
         week_num = interval(ran_week, pay_week)/weeks(1),
         any_work = as.numeric(t_payroll_total_earnings > 0 | t_payroll_total_hours > 0))

# Create weekly panel for all treatment members
showed_up <- payroll_clean %>%
  filter(any_work == 1 & week_num >= 0) %>%
  select(UL_id_unique, week_num, any_work) %>%
  distinct()

weeks <- 0:99
uls <- roster_twenty$UL_id_unique

#panel of all person-weeks (though 100 weeks) with the week-start date
week_panel <- roster_twenty %>%
  mutate(ran_week =  floor_date(ran_date, unit = "week", week_start = getOption("lubridate.week.start", 1))) %>%
  select(UL_id_unique, ran_week) %>%
  mutate(match = 1) %>%
  left_join(tibble(week_num = 0:99,
                   match = 1)) %>%
  mutate(week_date = ran_week %m+% weeks(week_num)) %>%
  select(-match)


retention <- week_panel %>%
  left_join(showed_up) %>%
  mutate(any_work = any_work %>% replace_na(0))

retention %>% write_csv(cl_args$OUT_RETENTION_PANEL)

