################################################################################
# Created By: Pietryka
# Creation Date:  2017-03-09
# Purpose: Creates figure 6 in manuscript and G1B in online supporting info
# R Version: R version 3.3.3 (2017-03-06)
# Data Input: Data/Derived/2012/groups_and_scales.csv
# NOTES: Must first run the following files in order:
#                       1. 'Analysis\Authoritarianism\auth1-run_models.R'
#                       2. 'Analysis\Authoritarianism\auth2-coef_plot.R'
#
# Questions: mpietryka@fsu.edu
################################################################################

plot_name <- "fg6"



# 1. CROSS TABS ----------------------------------------


# DATA WITH ONLY VALID (i.e., NON-MISSING) VALUES
complete_df <- ability_df  %>%
  filter(all_valid == TRUE)  %>%
  filter(!is.na(votechoice))   %>%
  mutate(votechoice = recode(votechoice, `0` = "Obama", `1` = "Romney"))

table(complete_df$ability_4 , complete_df$sum_all, useNA = "always")



# ABILITY ESTIMATE BASED ON RAW SCORE AND PARTY
abilityXraw <- complete_df  %>%
  select(sum_all, votechoice, ability_4)  %>%
  unique()  %>%
  arrange(sum_all)  %>%
  spread(votechoice, ability_4)  %>%
  select(raw_total = sum_all, ability_dems = Obama , ability_reps = Romney)  %>%
  mutate_if(is.double, round, digits = 2)


abilityXraw


# VOTE CHOICE BY RAW TOTAL
table(vote_choice = complete_df$votechoice , raw_total = complete_df$sum_all)  %>%
  prop.table(2)  %>%
  round(2)

# RAW TOTAL BY VOTE CHOICE
table(raw_total = complete_df$sum_all, vote_choice = complete_df$votechoice)  %>%
  prop.table(2)  %>%
  round(2)


# 2. PLOT FOR APPENDIX ----------------------------------------

loc_plot <- complete_df  %>%
  select(sum_all, votechoice, ability_4)  %>%
  arrange(sum_all)   %>%
  ggplot(aes(x = sum_all,
             y = ability_4,
             color = votechoice))  +
  stat_summary(fun.y = "mean", size = 5, geom = "point") +
  stat_summary(fun.y = "mean",  geom = "line", linetype = "dashed") +
  scale_color_manual(values = c(dem_col, rep_col)) +
  geom_text(x = 2, y = 0.05,
            size = rel(5),
            angle = 34,
            label = "Obama voters",
            color = dem_col,
            fontface = "bold") +
  geom_text(x = 2, y = -1.1,
            size = rel(5),
            angle = 38,
            label = "Romney voters",
            color = rep_col,
            fontface = "bold") +
  theme_minimal(base_size = 10) +
  labs(x = "Off-the-shelf Authoritarianism\n(sum of authoritarian responses)",
       y = "Corrected Authoritarianism Score",
       title =  str_wrap("Obama voters are more authoritarian than Romney voters with the same off-the-shelf score",
                         width = 40)
  ) +
  theme(
    panel.grid.minor.x = element_blank(),
    legend.position = "none"
  )

loc_plot



# 3.  PLOT FOR MAIN TEXT -----------------------




main_plot <- complete_df  %>%
  select(sum_all, votechoice, ability_4)  %>%
  arrange(sum_all)   %>%
  ggplot(aes(x = sum_all,
             y = ability_4,
             color = votechoice,
             shape = votechoice))  +
  stat_summary(fun.y = "mean", size = 5, geom = "point") +
  stat_summary(fun.y = "mean",
               geom = "line",
               linetype = "dashed",
               size = 1.1) +
  scale_color_manual(values = c(dem_col, rep_col)) +
  geom_text(x = 2, y = 0.07,
            size = rel(5),
            angle = 38,
            label = "Obama voters",
            color = dem_col,
            fontface = "bold") +
  geom_text(x = 2, y = -1.13,
            size = rel(5),
            angle = 42,
            label = "Romney voters",
            color = rep_col,
            fontface = "bold") +
  theme_minimal(base_size = 14) +
  labs(x = "Off-the-shelf Authoritarianism\n(sum of authoritarian responses)",
       y = "Corrected Authoritarianism Score"
       ) +
  theme(
    panel.grid.minor.x = element_blank(),
    legend.position = "none"
  )




width  <- 4
height <- 4
graphics.off()
windows(width, height)
main_plot


# 4. SAVE FIGURE ------------------------------


ggsave(filename = paste0(plot_name, ".pdf"),
       plot = main_plot,
       width = width,
       height = height)
ggsave(filename = paste0(plot_name, ".tiff"),
       dpi = 800,
       plot = main_plot,
       width = width,
       height = height)

