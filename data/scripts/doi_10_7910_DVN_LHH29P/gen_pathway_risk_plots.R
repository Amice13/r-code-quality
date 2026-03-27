if (!require("pacman")) install.packages("pacman", repos = "http://cran.us.r-project.org"); library(pacman);
p_load(tidyverse, lubridate, here, argparse, yaml, feather, glue, ggpubr, ggplot2)

here::i_am("generate_tables_and_figures/gen_pathway_risk_plots/src/gen_pathway_risk_plots.R")
source(here("R", "project_functions.R"))

cl_args <- parse_make_args(c(
  "CONFIG_FILE",
  "PROJECT_CONFIG",
  "XSECTION_ANALYSIS_FILE",
  "PAYROLL_FILE",
  "TWO_SUBGROUPS_RESULTS_FILE",
  "OUT_MAIN_FIGURE",
  "OUT_APPENDIX_FIGURE"
))

analysis_df <-
  arrow::read_feather(cl_args$XSECTION_ANALYSIS_FILE) %>%
  mutate(z_u_phat_f = ifelse(z_u_phat_f=="missing", "Missing", substr(z_u_phat_f, 1, 1)))

source("R/pathway_risk_plot_functions.R")
config <- read_yaml(cl_args$CONFIG_FILE)


# For marker sizes in both top main plots
# Calculate the proportions of different pathways in each risk score quartile for FULL sample
# Share for outreach pathway in group missing risk score is mutated to 0.5 as the "normal" default size of marker
pathway_share <- gen_pathway_share(analysis_df)


# Main top left - control group risk

## Subset original df
realized_control <- analysis_df %>%
  filter(treatment == 0) %>%
  select(cluster,
         contains("pathway"), contains("z_u_phat_f"),
         y_gun_involved__replication_any_post_18)

## Get realized risk
realized_risk_output <- realized_control %>%
  group_by(pathway, z_u_phat_f) %>%
  filter(pathway!="re") %>%
  summarize(y_gun_involved__replication_any_post_18=mean(y_gun_involved__replication_any_post_18, na.rm=TRUE)) %>%
  left_join(pathway_share, by=c("pathway", "z_u_phat_f")) %>%
  mutate(pathway=case_when(pathway=="ul"~"Algorithm", pathway=="cr"~"Outreach"))

## Generate plot
main_fig_realized_risk_controls <- realized_risk_output %>%
  mutate(z_u_phat_f = factor(z_u_phat_f, levels=c("Missing", 1, 2, 3, 4))) %>%
  ggplot(aes(x=z_u_phat_f, y=y_gun_involved__replication_any_post_18, group=pathway)) +
  geom_line(aes(col = pathway), lwd = config$line_width) +
  geom_point(aes(col = pathway, size = share_size, shape = pathway, stroke = 1)) +
  scale_color_manual(values = config$cols) +
  scale_shape_manual(values = config$shp_no_fill) +
  scale_size_continuous(range = c(5, 10)) +
  scale_x_discrete(expand = c(0, 0.15)) +
  scale_y_continuous(limits = c(0, 0.3),
                     breaks=seq(0, 0.3, 0.1),
                     expand = c(0, 0.02),
                     labels = scales::label_number(accuracy = 0.1)) +
  theme_classic() +
  labs(x = "Risk score quartile",
       y = "Involved in a violent gun crime",
       title = "Realized Risk Among Controls: Violent Gun Crime") +
  guides(size = "none") +
  guides(colour = guide_legend(override.aes = list(size=6, stroke=1))) +
  theme(plot.title=element_text(hjust=0.5),
        legend.title=element_blank(),
        legend.position="bottom",
        legend.key.width = unit(2, "cm"),
        axis.text.x = element_text(size = config$axis_size),
        axis.title.x = element_text(size = config$axis_label_size),
        axis.text.y = element_text(size = config$axis_size),
        axis.title.y = element_text(size = config$axis_label_size),
        legend.text = element_text(size = config$axis_size),
        title = element_text(size = config$title_size),
        text = element_text(family = "Times"))


# Main top right - take-up

takeup_roster <- analysis_df %>%
  select(UL_id_unique, treatment, pathway, takeup_date, t_headcount_post_20, z_u_phat_f)

## Take-up rate calculated with same method with payroll_table task using t_headcount_post_20
trates_total <- takeup_roster %>%
  filter(treatment == 1) %>%
  group_by(z_u_phat_f, pathway) %>%
  summarise(n_takers = sum(t_headcount_post_20),
            n_path_risk = n()) %>%
  mutate(takeup_rate = n_takers/n_path_risk) %>%
  filter(pathway!="re") %>%
  select(pathway, z_u_phat_f, takeup_rate) %>%
  mutate(z_u_phat_f=case_when(is.na(z_u_phat_f)~"Missing", TRUE~as.character(z_u_phat_f))) %>%
  arrange(pathway) %>%
  ungroup()

## Average hours worked for takers by subgroups
payroll_avg_clean <- analysis_df %>%
  filter(treatment==1 & t_headcount_post_20!=0) %>%
  group_by(pathway, z_u_phat_f) %>%
  summarise(avg_hours = mean(t_payroll_work_hours_post_20, na.rm=TRUE))
take_up_rate_output <- trates_total %>%
  left_join(pathway_share, by=c("pathway", "z_u_phat_f")) %>%
  left_join(payroll_avg_clean, by=c("pathway", "z_u_phat_f")) %>%
  mutate(pathway=case_when(pathway=="ul"~"Algorithm", pathway=="cr"~"Outreach"))

## Generate plot
main_fig_takeup <-
  takeup_quartile(take_up_rate_output, 'Times',
                  config$axis_size, config$axis_label_size, config$title_size, config$subtitle_size, config$floating_text_size)

# Appendix top left plot -- distribution of risk scores

risk_quartiles = quantile(analysis_df$u_phat, probs = c(0, .25, .5, .75, 1), na.rm = T)
y = 8.9

risk_dist_plot = analysis_df %>%
  filter(!is.na(u_phat), pathway != 're') %>%
  mutate(pathway=case_when(pathway=="ul"~"Algorithm", pathway=="cr"~"Outreach")) %>%
  ggplot(aes(x=u_phat, color=pathway, linetype=pathway)) +
  scale_color_manual(name = "", limits = c('Algorithm', 'Outreach'), values = c(config$cols)) +
  scale_linetype_manual(name = "", limits = c('Algorithm', 'Outreach'), values = c('solid', 'longdash')) +
  geom_density(show.legend = F) +
  stat_density(geom="line",position="identity") +
  annotate('rect', xmin=risk_quartiles[1], xmax=risk_quartiles[2],
           ymin=0, ymax=8.5, alpha=.25, fill='grey24') +
  annotate('label', label='Q1', x=(risk_quartiles[1] + risk_quartiles[2])/2, y=y,
           size=config$floating_text_size,  label.size=NA, family='Times') +
  annotate('label', label='Q2', x=(risk_quartiles[2] + risk_quartiles[3])/2, y=y,
           size=config$floating_text_size,  label.size=NA, family='Times') +
  annotate('rect', xmin=risk_quartiles[3], xmax=risk_quartiles[4],
           ymin=0, ymax=8.5, alpha=.25, fill='grey24') +
  annotate('label', label='Q3', x=(risk_quartiles[3] + risk_quartiles[4])/2, y=y,
           size=config$floating_text_size,  label.size=NA, family='Times') +
  annotate('label', label='Q4', x=(risk_quartiles[4] + risk_quartiles[5])/2, y=y,
           size=config$floating_text_size,  label.size=NA, family='Times') +
  theme_classic() +
  labs(x = "Risk score",
       y = "Density",
       title = '') +
  scale_x_continuous(limits = c(0, .5), breaks = seq(0, .5, .1)) +
  scale_y_continuous(limits = c(0, 9), breaks = seq(0, 9, 2)) +
  theme(legend.title=element_blank(),
        legend.position="bottom",
        axis.text.x = element_text(size = config$axis_size),
        axis.title.x = element_text(size = config$axis_label_size),
        axis.text.y = element_text(size = config$axis_size),
        axis.title.y = element_text(size = config$axis_label_size),
        legend.text = element_text(size = config$axis_size),
        text = element_text(family = "Times"))


# Main bottom plot; three appendix panels - itt

reg <- read_csv(cl_args$TWO_SUBGROUPS_RESULTS_FILE)

itt_primary_outcome_index <- gen_itt_plot(
  reg, 'i_three_components', 'Primary Index of Serious Violence', config$ylims, "Times", config$axis_size, config$axis_label_size, config$title_size)

itt_shoot_hom_victims <- gen_itt_plot(
    reg, 'v_shooting_or_homicide', 'Shooting & Homicide Victimizations', config$ylims, "Times", config$axis_size, config$axis_label_size, config$title_size)

itt_shoot_hom_arrests <- gen_itt_plot(
    reg, 'a_shooting_or_homicide', 'Shooting & Homicide Arrests', config$ylims, "Times", config$axis_size, config$axis_label_size, config$title_size)

itt_other_p1_violent_arrests <- gen_itt_plot(
  reg, 'a_partone_non_shooting_or_homicide', 'Other Serious Violent-Crime Arrests', config$ylims, "Times", config$axis_size, config$axis_label_size, config$title_size)


# Combine plots for final figures

timestamp <- get_timestamp()

#main_fig_ver0 <- ggarrange(
#  ggarrange(main_fig_realized_risk_controls, main_fig_takeup, ncol = 2),
#  itt_primary_outcome_index,
#  nrow = 2)

main_fig <- ggarrange(itt_primary_outcome_index,
                       ggarrange(main_fig_realized_risk_controls, main_fig_takeup, ncol = 2),
                       nrow = 2)

ggsave(plot = main_fig, cl_args$OUT_MAIN_FIGURE,
       width= 15, height= 10, units= "in")
ggsave(plot = main_fig, str_replace(cl_args$OUT_MAIN_FIGURE, "up_to_date", timestamp),
       width= 15, height= 10, units= "in")

appendix_fig <- ggarrange(
  ggarrange(risk_dist_plot, itt_shoot_hom_arrests, ncol = 2),
  ggarrange(itt_shoot_hom_victims, itt_other_p1_violent_arrests, ncol = 2),
  nrow = 2)

ggsave(plot = appendix_fig, cl_args$OUT_APPENDIX_FIGURE,
       width= 15, height= 10, units= "in")
ggsave(plot = appendix_fig, str_replace(cl_args$OUT_APPENDIX_FIGURE, "up_to_date", timestamp),
       width= 15, height= 10, units= "in")
