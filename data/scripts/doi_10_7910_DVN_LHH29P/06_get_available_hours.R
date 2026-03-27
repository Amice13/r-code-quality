if (!require("pacman")) install.packages("pacman", repos = "http://cran.us.r-project.org"); library(pacman);
p_load(tidyverse, lubridate, arrow, yaml, here, argparse, glue)

here::i_am("generate_tables_and_figures/gen_payroll_outputs/src/06_get_available_hours.R")
source(here("R", "project_functions.R"))

cl_args <- parse_make_args(c(
  "CONFIG_FILE",
  "PROJECT_CONFIG",
  "XSECTION_ANALYSIS_FILE",
  "PAYROLL_FILE",
  "OUT_AVAILABLE_HOURS"
))

source("R/payroll_functions.R")

config <- read_yaml(cl_args$CONFIG_FILE)
project_config <- read_yaml(cl_args$PROJECT_CONFIG)

#wage growth over time, by pathway plot
payroll <- read_feather(cl_args$PAYROLL_FILE)

roster_twenty <- read_feather(cl_args$XSECTION_ANALYSIS_FILE) %>%
  filter(grepl("20", window), treatment == 1) %>%
  mutate(ran_date = ymd(ran_date),
         takeup_date = ymd(takeup_date),
         takeup_from_rand = as.numeric(difftime(takeup_date, ran_date, unit = "days")))


# Create period - specific cols that we will pass in to the function
payroll_twenty <- get_payroll_20(payroll, roster_twenty, project_config$days_per_month)

covid_cut <- project_config$covid_cut

max_hours_per_week <- 29.5

# Cut the payroll to only those who showed up precovid

payroll_takers <- payroll_twenty %>%
  filter(t_payroll_total_hours > 0) %>% # did you have any training or work hours that week?
  group_by(UL_id_unique) %>%
  summarise(first_pay_date = min(pay_date))

payroll_available_hours <- payroll_twenty %>%
  filter(pay_date < covid_cut) %>%
  group_by(ran_date, UL_id_unique, pathway) %>%
  summarise(regular_hours = sum(t_payroll_work_hours, na.rm = TRUE)) %>%
  inner_join(payroll_takers, "UL_id_unique") %>%
  ungroup() %>%
  mutate(followup_end_date = ran_date + days(20*project_config$days_per_month),
         service_end_date = first_pay_date + days(18*project_config$days_per_month),
         cutoff_date = pmin(followup_end_date, service_end_date, ymd(covid_cut) - days(1)),
         service_weeks = interval(first_pay_date, cutoff_date)/weeks(1),
         hours_possible = pmax(service_weeks * max_hours_per_week, 0), #mark hours possible as 0 for those who took up post covid
         hours_pct = regular_hours/hours_possible)

add_takeup_time <- roster_twenty %>%
  group_by(pathway) %>%
  summarize(avg_takeup_from_rand = mean(takeup_from_rand, na.rm = TRUE)) %>%
  rbind(c("all", mean(roster_twenty$takeup_from_rand, na.rm = TRUE)))

avg_table <- payroll_available_hours %>%
  group_by(pathway) %>%
  summarise(avg_hours_worked = mean(regular_hours),
            avg_hours_possible = mean(hours_possible),
            pct = avg_hours_worked/avg_hours_possible) %>%
  bind_rows(payroll_available_hours %>%
              summarise(avg_hours_worked = mean(regular_hours),
                        avg_hours_possible = mean(hours_possible),
                        pct = avg_hours_worked/avg_hours_possible) %>%
              mutate(pathway = "all")) %>%
  left_join(add_takeup_time, by="pathway") %>% 
  select(pathway, avg_takeup_from_rand, avg_hours_worked, avg_hours_possible, pct)

avg_table %>%
  write_csv(cl_args$OUT_AVAILABLE_HOURS)
