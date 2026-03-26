if (!require("pacman")) install.packages("pacman", repos = "http://cran.us.r-project.org"); library(pacman);
p_load(tidyverse, lubridate, arrow, yaml, here, argparse, glue)

here::i_am("generate_tables_and_figures/gen_payroll_outputs/src/04_build_earnings_table.R")
source(here("R", "project_functions.R"))

cl_args <- parse_make_args(c(
  "CONFIG_FILE",
  "XSECTION_ANALYSIS_FILE",
  "OUT_PAYROLL_AVGS",
  "OUT_STIPEND_AVGS",
  "OUT_WAGE_TABLE"
))


config <- read_yaml(cl_args$CONFIG_FILE)

payroll_table <- read_csv(cl_args$OUT_PAYROLL_AVGS)
stipend_table <- read_csv(cl_args$OUT_STIPEND_AVGS)

roster_twenty <- read_feather(cl_args$XSECTION_ANALYSIS_FILE) %>%
  filter(grepl("20", window),treatment == 1) %>%
  mutate(ran_date = ymd(ran_date)) %>%
  select(UL_id_unique,pathway,ran_date, treatment, takeup_date, t_headcount_post_20, neighborhood) %>%
  mutate(takeup_from_rand = as.numeric(difftime(takeup_date, ran_date, unit = "days")))

# Count all takers through March 2020 as "pre-takers"
takeup_covid_cut <- ymd(config$takeup_covid_cut)

#trates
trates_total <- roster_twenty %>%
  group_by(pathway) %>%
  summarise(n_takers = sum(t_headcount_post_20),
            n_path = n()) %>%
  bind_rows(roster_twenty %>%
              summarise(n_takers = sum(t_headcount_post_20),
                        n_path = n()) %>%
              mutate(pathway = "all")) %>%
  mutate(takeup_rate = n_takers/n_path,
         period = "total") %>%
  select(path = pathway, period, takeup_rate)

# precovid trates

trates_pre <- roster_twenty %>%
  group_by(pathway) %>%
  summarise(n_takers = sum(t_headcount_post_20 == 1 & takeup_date <= takeup_covid_cut, na.rm = T),
            n_path = n()) %>%
  bind_rows(roster_twenty %>%
              summarise(n_takers = sum(t_headcount_post_20 == 1 & takeup_date <= takeup_covid_cut, na.rm = T),
                        n_path = n()) %>%
              mutate(pathway = "all")) %>%
  mutate(takeup_rate = n_takers/n_path,
         period = "pre") %>%
  select(path = pathway, period, takeup_rate)


#extremely over complicated double pivot that probably shouldnt exist
combined_table <- payroll_table %>%
  select(pathway = path, type, n_total,
         avg_earnings_pre, avg_hours_pre, avg_earnings_standby, avg_hours_standby,
         avg_earnings_post, avg_hours_post, avg_earnings_total, avg_hours_total) %>%
  left_join(stipend_table %>% select(-n)) %>%
  rename_all(~str_replace(.x,"_","\\.")) %>%
  pivot_longer(-c(pathway, type, n.total),
               names_to = c("count", "period"),
               values_to = "amount",
               names_sep = "_" ) %>%
  pivot_wider(names_from = c(type, count),
              values_from = amount) %>%
  rename_all(~str_replace(.x,"\\.","_") %>% str_replace("_avg","")) %>%
  select(-training_stipend_earnings, -training_stipend_hours) %>%
  rename(stipend_earnings = regular_stipend_earnings, stipend_hours = regular_stipend_hours) %>%
  arrange(period)

add_takeup_time <- roster_twenty %>% group_by(pathway) %>% summarize(avg_takeup_from_rand = mean(takeup_from_rand, na.rm = TRUE)) %>%
  rbind(c("all", mean(roster_twenty$takeup_from_rand, na.rm = TRUE)))

wage_table <- combined_table %>%
  left_join(trates_total %>% select(pathway = path, takeup_rate)) %>%
  rename_all(~.x %>% str_replace("_avg","")) %>%
  select(pathway, period, takeup_rate, n_payroll = n_total,
         regular_earnings, regular_hours, training_earnings, training_hours,
         stipend_earnings, stipend_hours) %>%
  mutate(total_earnings = regular_earnings + training_earnings + stipend_earnings,
         total_hours = regular_hours + training_hours + stipend_hours) %>%
  unique() %>%
  left_join(add_takeup_time, by="pathway")

wage_table %>%
  write_csv(cl_args$OUT_WAGE_TABLE)

