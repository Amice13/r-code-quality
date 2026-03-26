if (!require("pacman")) install.packages("pacman", repos = "http://cran.us.r-project.org"); library(pacman);
p_load(tidyverse, lubridate, yaml, here, argparse, glue, ggrepel)

here::i_am("generate_tables_and_figures/gen_retention_plots/src/03_plot_retention.R")
source(here("R", "project_functions.R"))

cl_args <- parse_make_args(c(
  "CONFIG_FILE",
  "OUT_WEEKLY_RETENTION",
  "OUT_PRECOVID_PLOT",
  "OUT_FULLPERIOD_PLOT"
))


config <- read_yaml(cl_args$CONFIG_FILE)

source("R/retention_functions.R")

#Get retention
retention_by_week <- read_csv(cl_args$OUT_WEEKLY_RETENTION) %>%
  rename(path_cut = path,
         period_cut = period)

retention_clean <- retention_by_week %>%
  mutate(period= case_when(person_week==1  ~ "1 week",
                           person_week==13 ~ "3 months",
                           person_week==26 ~ "6 months",
                           person_week==39 ~ "9 months",
                           person_week==52 ~ "12 months",
                           person_week==65 ~ "15 months",
                           person_week==78 ~ "18 months",
                           person_week==86 ~ "20 months"),
         label= paste0(period, ":", "\n", round(survived*n, 0), " / ", n)) %>%
  pivot_longer(cols = c("survived", "cml"), names_to = "stat")

# Get plots for the paper
precovid_plot <- plot_retention(retention_clean, "precovid", config$plot_max_break, config$plot_max_week)
fullperiod_plot <- plot_retention(retention_clean, "all",  config$plot_max_break, config$plot_max_week)

# Output plots
timestamp <- get_timestamp()

ggsave(plot = precovid_plot, cl_args$OUT_PRECOVID_PLOT, width= 13, height= 12, units= "in")
ggsave(plot = precovid_plot, str_replace(cl_args$OUT_PRECOVID_PLOT,"up_to_date", timestamp), width= 13, height= 12, units= "in")

ggsave(plot = fullperiod_plot,  cl_args$OUT_FULLPERIOD_PLOT, width= 13, height= 12, units= "in")
ggsave(plot = fullperiod_plot,  str_replace(cl_args$OUT_FULLPERIOD_PLOT,"up_to_date", timestamp), width= 13, height= 12, units= "in")
