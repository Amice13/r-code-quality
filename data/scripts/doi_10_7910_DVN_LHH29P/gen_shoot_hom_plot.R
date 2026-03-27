# Package setup
if (!require("pacman")) install.packages("pacman", repos = "http://cran.us.r-project.org"); library(pacman);
pacman::p_load(yaml, arrow, readr, tidyr, argparse, here, stringr, dplyr, lubridate, ggplot2, ggrepel, ggthemes, forcats, glue);

# Working directory and command line argument setup
task_output = "shoot_homs_per_capita"
current_script = paste0("generate_tables_and_figures/gen_", task_output, "/gen_", task_output, "/src/gen_shoot_hom_plot.R")
here::i_am(current_script)
source(here("R", "project_functions.R"))
task_dir = here(dirname(dirname(file.path(current_script))))
cl_args <- parse_make_args(c(
  "SHOOT_AND_HOM_VICS_BY_MONTH_AND_CA",
  "CA_POPULATION",
  "CONFIG_FILE",
  "OUT_DIR",
  "OUT_FILE"
), inter_active = TRUE, task_dir = task_dir)

config <- read_yaml(cl_args$CONFIG_FILE)

# Identify READI-similar neighborhoods ----
vics_by_month_and_ca <- read_csv(cl_args$SHOOT_AND_HOM_VICS_BY_MONTH_AND_CA)

# This population table was created by "pulling population estimates from the ACS"
ca_pop_2016 <- read_csv(cl_args$CA_POPULATION) %>%
  select(ca, pop_2016)

vics_by_month_and_ca <- vics_by_month_and_ca %>%
  left_join(ca_pop_2016, by = "ca") %>%
  filter(year(month) %in% config$years)

# Summarize to CA level ----
vics_by_ca <- vics_by_month_and_ca %>%
  group_by(ca, ca_name, pop_2016) %>%
  summarise(n = sum(n),
            .groups = "drop") %>%
  mutate(per100k = (n*100000)/pop_2016,
         rank_n = rank(desc(n)),
         rank_per = rank(desc(per100k)),
         top15_count_or_per100k = ifelse(rank_n <= 15 | rank_per <= 15, 1, 0),
         top15_count_and_per100k = ifelse(rank_n <= 15 & rank_per <= 15, 1, 0),
         group = case_when(ca_name %in% config$readi_neighborhoods ~ "READI Neighborhoods",
                           top15_count_and_per100k == 1 ~ "Top 15 (count AND per capita)",
                           top15_count_or_per100k == 1 ~ "Top 15 (count OR per capita)",
                           TRUE ~ "Other Neighborhoods"),
         label = case_when(group != "Other Neighborhoods" ~ ca_name))

# Make plot ----
point_types = c("READI Neighborhoods", "Top 15 (count AND per capita)", "Top 15 (count OR per capita)", "Other Neighborhoods")
vics_by_ca %>%
  mutate(group = fct_relevel(group, point_types[1], point_types[2], point_types[3], point_types[4])) %>%
  ggplot(aes(n, per100k, label = label, color = group)) +
  geom_point(aes(shape=group, color=group), size=4) +
  geom_vline(xintercept = 102, linetype = "dashed") +
  geom_hline(yintercept = 329, linetype = "dashed") +
  ggrepel::geom_text_repel(box.padding = 0.5, family = "Times", size=5.2, show.legend = FALSE) +
  ggthemes::theme_few() +
  scale_color_manual("", labels = point_types, values = c('#fd084a', '#494fc1', '#F58800', 'grey70')) +
  scale_shape_manual("", labels = point_types, values = c(16, 15, 17, 3)) +
  labs(x = "Number of Shooting and Homicide Victims",
       y = "Shooting and Homicide Victims per 100,000 Residents") +
  theme(legend.position = "bottom",
        axis.text.x = element_text(size = config$axis_size),
        axis.title.x = element_text(size = config$axis_label_size),
        axis.text.y = element_text(size = config$axis_size),
        axis.title.y = element_text(size = config$axis_label_size),
        legend.text = element_text(size = config$axis_size),
        title = element_text(size = config$title_size),
        text=element_text(family="Times")) +
  guides(colour=guide_legend(nrow=2))

ggsave(plot = last_plot(),
       filename = cl_args$OUT_FILE,
       device = "png",
      width = 9.59,
      height = 6.82,
      units = "in")
