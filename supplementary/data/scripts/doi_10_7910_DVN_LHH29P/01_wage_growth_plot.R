if (!require("pacman")) install.packages("pacman", repos = "http://cran.us.r-project.org"); library(pacman);
p_load(tidyverse, lubridate, arrow, yaml, here, argparse, glue)

here::i_am("generate_tables_and_figures/gen_payroll_outputs/src/01_wage_growth_plot.R")
source(here("R", "project_functions.R"))

cl_args <- parse_make_args(c(
  "CONFIG_FILE",
  "PROJECT_CONFIG",
  "XSECTION_ANALYSIS_FILE",
  "PAYROLL_FILE",
  "OUT_WAGE_GROWTH_PLOT"
))

source("R/payroll_functions.R")

config <- read_yaml(cl_args$CONFIG_FILE)
project_config <- read_yaml(cl_args$PROJECT_CONFIG)

# wage growth over time, by pathway plot
payroll <- read_feather(cl_args$PAYROLL_FILE)

roster_twenty <- read_feather(cl_args$XSECTION_ANALYSIS_FILE) %>%
  filter(grepl("20", window),treatment == 1) %>%
  mutate(ran_date = ymd(ran_date))

payroll_twenty <- get_payroll_20(payroll, roster_twenty, project_config$days_per_month)

# Focus on regular
base_wages <- payroll_twenty %>%
  filter(t_payroll_work_earnings_or_hours == 1) %>%
  group_by(UL_id_unique) %>%
  filter(pay_period == min(pay_period)) %>%
  ungroup() %>%
  mutate(wage = t_payroll_work_earnings/t_payroll_work_hours) %>%
  group_by(pay_week) %>%
  summarise(avg_wage = mean(wage))


payroll_wages <- payroll_twenty %>%
  filter(t_payroll_work_earnings_or_hours == 1) %>%
  mutate(t_payroll_work_wage = round(t_payroll_work_earnings/t_payroll_work_hours,2)) %>%
  mutate(min_wage = case_when(pay_week >= ymd(config$pay_jump_14_date) ~ 14,
                              pay_week >= ymd(config$pay_jump_13_date) ~ 13,
                              pay_week >= ymd(config$pay_jump_12_date) ~ 12,
                              T ~ 11),
         wage_growth = t_payroll_work_wage - min_wage) %>%
  group_by(UL_id_unique) %>%
  arrange(pay_week) %>%
  mutate(first_week = min(pay_week),
         week_post_takeup = interval(first_week, pay_week)/weeks(1),
         week_shown_up = 1:n()) %>%
  ungroup() %>%
  filter(t_payroll_work_wage <= config$max_wage) #max wage


wage_growth <- payroll_wages %>%
  select(UL_id_unique, pathway, pay_week, wage_growth, week_post_takeup, week_post_rand, week_shown_up)

weekly_growth <- wage_growth %>%
  group_by(week_post_takeup, pathway) %>%
  summarise(n = n(),
            wage_growth = round(max(0,mean(wage_growth)),2)) %>%
  mutate(type = "week post-takeup") %>%
  bind_rows(wage_growth %>%
              group_by(week_post_takeup) %>%
              summarise(n = n(),
                        wage_growth = round(max(0,mean(wage_growth)),2)) %>%
              mutate(type = "week post-takeup", pathway = "all")) %>%
  filter(n > config$min_takers,
         week_post_takeup <= config$max_week)


comb_plot <- weekly_growth %>%
  mutate(pathway = pathway %>% factor(levels = c("all", "ul", "cr", "re"))) %>%
  ggplot(aes(x = week_post_takeup, y = wage_growth, color = pathway, lty = pathway)) +
  scale_color_manual(labels = c("All Pathways", "Algorithm","Outreach", "Re-entry" ),
                     values = c("black", '#bf212f', '#264b96', '#27b376')) +
  scale_linetype_manual(values = c("dashed", "solid", "solid","solid"), guide = "none") +
  geom_line(linewidth = .75) +
  labs(y = "Wage - minumum wage ($)",
       x = "Weeks post take-up",
       color = "",
       title = "") +
  scale_y_continuous(breaks = seq(0,1.2,.1)) +
  scale_x_continuous(breaks = seq(0,80,10)) +
  theme(panel.background = element_blank(),
        axis.text = element_text(size = config$axis_size),
        axis.title = element_text(size = config$axis_label_size),
        legend.text = element_text(size = config$axis_size),
        text = element_text(family = "Times"))

# Output
timestamp <- get_timestamp()

comb_plot %>%
  ggsave(plot = .,
         filename = cl_args$OUT_WAGE_GROWTH_PLOT,
         width = 10, height = 5, units = "in")

comb_plot %>%
  ggsave(plot = .,
         filename = str_replace(cl_args$OUT_WAGE_GROWTH_PLOT, "up_to_date", timestamp),
         width = 10, height = 5, units = "in")
