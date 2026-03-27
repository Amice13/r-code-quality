if (!require("pacman")) install.packages("pacman", repos = "http://cran.us.r-project.org"); library(pacman);
p_load(tidyverse, lubridate, arrow, yaml, here, argparse, glue)

here::i_am("generate_tables_and_figures/gen_payroll_outputs/src/03_get_stipend_earnings.R")
source(here("R", "project_functions.R"))

cl_args <- parse_make_args(c(
  "CONFIG_FILE",
  "PROJECT_CONFIG",
  "XSECTION_ANALYSIS_FILE",
  "PAYROLL_FILE",
  "STIPEND_PRE_STACK",
  "STIPEND_COVID_STACK",
  "OUT_STIPEND_AVGS"
))

source("R/payroll_functions.R")

config <- read_yaml(cl_args$CONFIG_FILE)
project_config <- read_yaml(cl_args$PROJECT_CONFIG)

payroll <- read_feather(cl_args$PAYROLL_FILE)


# Get guys who show up at any time in the payroll data. We know these guys signed the release
payroll_takers <- payroll %>%
  filter(treatment == 1, t_payroll_any == 1) %>%
  select(UL_id_unique) %>%
  distinct() %>%
  mutate(t_payroll = 1)

roster_twenty <- read_feather(cl_args$XSECTION_ANALYSIS_FILE) %>%
  filter(grepl("20", window),treatment == 1) %>%
  mutate(ran_date = ymd(ran_date),
         end_date = ran_date + days(project_config$days_per_month*20)) %>%
  left_join(payroll_takers, "UL_id_unique") %>%
  mutate(t_payroll = replace_na(t_payroll)) %>%
  select(UL_id_unique,pathway,ran_date, end_date, treatment,t_headcount, t_payroll, neighborhood)

payroll_twenty <- get_payroll_20(payroll, roster_twenty, project_config$days_per_month)

# Beginning/end dates for the COVID period
covid_cut <- ymd(project_config$covid_cut)
standby_cut <- ymd(config$standby_cut)

# Assumed $ amount for each stipend
stipend_pay <- config$pay_per_stipend
stipend_hours <- config$hours_per_stipend

### A. get stipend pre-covid mean ####

#dates with decently good data
min_cut <- ymd(config$stipend_start_date)
max_cut <- ymd(config$stipend_end_date)

# Restrict stipend stack to those who show up in payroll data
stipends <- read_csv(cl_args$STIPEND_PRE_STACK) %>%
  semi_join(roster_twenty %>% filter(t_payroll == 1), "UL_id_unique") %>%
  filter(date >= min_cut, date <= max_cut)

#1. Calculate how many times the average person received a stipend per week this period
weekly_stipends_precovid <- stipends %>%
  filter(!date %>% is.na()) %>%
  mutate(date = case_when(date == ymd("2029-07-25") ~ ymd("2019-07-25"), #fix dates
                          date == ymd("2029-07-24") ~ ymd("2019-07-24"),
                          date == ymd("3019-07-02") ~ ymd("2019-07-02"),
                          T ~ date),
         week = floor_date(date,  unit = "week", week_start = getOption("lubridate.week.start", 1))) %>%
  left_join(roster_twenty %>% select(UL_id_unique, ran_date, pathway, end_date)) %>%
  filter(date >= ran_date, date <= end_date) %>%
  distinct(date, week, pathway, UL_id_unique) %>%
  count(UL_id_unique, week,pathway)


pathway_weekly_stipends_precovid <- weekly_stipends_precovid %>%
  group_by(pathway) %>%
  summarise(weekly_stipends = mean(n)) %>%
  bind_rows(weekly_stipends_precovid %>% summarise(weekly_stipends = mean(n)) %>% mutate(pathway = "all"))

#2. Calculate how many weeks the average person shows up in the payroll data
payroll_precovid <- payroll_twenty %>%
  filter(pay_date < covid_cut)

weeks_per_taker_precovid <- payroll_precovid %>%
  filter(t_payroll_work_earnings > 0 | t_payroll_work_hours > 0) %>%
  group_by(UL_id_unique, pathway) %>%
  summarise(n_weeks = n()) %>%
  distinct()


pathway_weeks_per_taker_precovid <- weeks_per_taker_precovid %>%
  group_by(pathway) %>%
  summarise(avg_weeks = mean(n_weeks)) %>%
  bind_rows(weeks_per_taker_precovid %>% ungroup %>% summarise(avg_weeks = mean(n_weeks)) %>% mutate(pathway = "all"))


#3. Average amount of weeks per participant * average number of stipends per week per participant * $25 for pre-covid payroll means
n_payroll <- roster_twenty %>% filter(t_payroll == 1) %>%
  group_by(pathway) %>%
  summarise(n = n()) %>%
  bind_rows(roster_twenty %>% filter(t_payroll == 1) %>% summarise(n = n()) %>% mutate(pathway = "all"))

stipend_earnings_pre_covid <- pathway_weeks_per_taker_precovid %>%
  left_join(pathway_weekly_stipends_precovid) %>%
  mutate(stipend_earnings = avg_weeks*weekly_stipends*stipend_pay) %>%
  left_join(n_payroll) %>%
  select(pathway, n, stipend_earnings_pre = stipend_earnings)

### B. get standby totals ####

stipends_covid <- read_csv(cl_args$STIPEND_COVID_STACK) %>%
  semi_join(roster_twenty %>% filter(t_payroll == 1), "UL_id_unique") # only keep guys in payroll


# 1. Get earnings per person-week during standby period
stipends_standby <- stipends_covid %>%
  filter(date < standby_cut, date > covid_cut)

stipend_earnings_standby <- stipends_standby %>%
  left_join(roster_twenty %>% select(UL_id_unique, pathway)) %>%
  group_by(pathway) %>%
  summarise(total_earnings = sum(value_per_card, na.rm = T)) %>%
  bind_rows(stipends_standby %>%
              summarise(total_earnings = sum(value_per_card, na.rm = T),
                        pathway = "all")) %>%
  left_join(n_payroll) %>%
  mutate(stipend_earnings_standby = total_earnings/n) %>%
  select(pathway,n, stipend_earnings_standby)



### C. get stipend post-covid mean ####
stipends_post <- stipends_covid %>%
  filter(date >= standby_cut)


stipend_earnings_post <- stipends_post %>%
  left_join(roster_twenty %>% select(UL_id_unique, pathway)) %>%
  group_by(pathway) %>%
  summarise(total_earnings = sum(value_per_card, na.rm = T),
            n_person_week = n()) %>%
  bind_rows(stipends_post %>%
              summarise(total_earnings = sum(value_per_card, na.rm = T),
                        n_person_week = n(),
                        pathway = "all")) %>%
  left_join(n_payroll) %>%
  mutate(stipend_earnings_post = total_earnings/n) %>%
  select(pathway, n, stipend_earnings_post)


#Combine
stipend_table <- stipend_earnings_pre_covid %>%
  left_join(stipend_earnings_standby) %>%
  left_join(stipend_earnings_post) %>%
  mutate(stipend_earnings_total = stipend_earnings_pre + stipend_earnings_standby + stipend_earnings_post,
         stipend_hours_pre = stipend_earnings_pre/stipend_pay*stipend_hours,
         stipend_hours_standby = stipend_earnings_standby/stipend_pay*stipend_hours,
         stipend_hours_post = stipend_earnings_post/stipend_pay*stipend_hours,
         stipend_hours_total = stipend_hours_pre + stipend_hours_standby + stipend_hours_post)

stipend_table %>%
  write_csv(cl_args$OUT_STIPEND_AVGS)

