################################################################################
# Created By: Pietryka
# Creation Date:  2017-03-09
# Purpose: Creates figure G1A  in online supporting info
# R Version: R version 3.3.3 (2017-03-06)
# Data Input: Data/Derived/2012/groups_and_scales.csv
# NOTES: must first run 'Analysis\Authoritarianism\auth1-run_models.R'
#
# Questions: mpietryka@fsu.edu
################################################################################



# 1. RUN REGRESSIONS ---------------------


ols_0 <- svyglm(mean_all ~ votechoice, design = survey_df)  %>%
  broom::tidy()  %>%
  mutate(model = "Off-the-shelf")
ols_1 <- svyglm(ability_1 ~ votechoice, design = survey_df)  %>%
  broom::tidy()  %>%
  mutate(model = "Baseline Rasch")
ols_2 <- svyglm(ability_2 ~ votechoice, design = survey_df)  %>%
  broom::tidy()  %>%
  mutate(model = "Second Rasch (one item corrected)")
ols_3 <- svyglm(ability_3 ~ votechoice, design = survey_df)  %>%
  broom::tidy()  %>%
  mutate(model = "Third Rasch (two items corrected)")
ols_4 <- svyglm(ability_4 ~ votechoice, design = survey_df)  %>%
  broom::tidy()  %>%
  mutate(model = "Fourth Rasch (DIF free)")



ols_df <- ols_0  %>%
  bind_rows(ols_1)  %>%
  bind_rows(ols_2)  %>%
  bind_rows(ols_3)  %>%
  bind_rows(ols_4)   %>%
  mutate(lb = estimate - 1.96 * std.error)  %>%
  mutate(ub = estimate + 1.96 * std.error)   %>%
  mutate(model = forcats::fct_inorder(model))


# 2. MAKE FIGURE ---------------------


library(stringr)
# COLORS FOR POINTS
point_colors <- scales::seq_gradient_pal(s_color, "white")(seq(0, 1,length.out=5))

coef_plot <- ols_df  %>%
  filter(term == "votechoice")  %>%
  ggplot(aes(x = factor(model, levels = rev(levels(model))),
             y = estimate,
             fill = factor(model, levels = rev(levels(model))),
             label = factor(model, levels = rev(levels(model)))))  +
  geom_hline(yintercept = 0,
             linetype = "longdash",
             color = "grey50") +
  geom_pointrange(aes(ymin = lb, ymax = ub), color = "grey50",
                  fatten = 4,
                  size = 1.3, shape = 21) +
  scale_fill_manual(values = point_colors) +
  geom_text(fontface = "bold", vjust = -1.0, size = rel(3),
            hjust = c( rep(0.5, 4), .4)) +
  geom_text(x = 1, y = -0.32,
            size = rel(2.5),
            vjust = 3,
            hjust = 0,
            label = "Obama voters more authoritarian",
            color = axis_col,
            fontface = "italic") +
  geom_text(x = 1, y = 0.32,
            size = rel(2.5),
            vjust = 3,
            hjust = 1,
            label = "Romney voters more authoritarian",
            color = axis_col,
            fontface = "italic") +
  theme_minimal(base_size = 10) +
  labs(x = "",
       y = "Estimated difference",
       title = str_wrap("Obama voters are more authoritarian after correction" ,
                        width = 50)

  ) +
  coord_flip() +
  theme(
    axis.text.y = element_blank(),
    panel.grid.major.y = element_blank(),
    panel.grid.minor.y = element_blank(),
    plot.subtitle =  element_text(color = axis_col),
    legend.position = "none"
  )

coef_plot


