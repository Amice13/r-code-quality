# dataframe from 2_make_data

library(dplyr)
library(stringr)

summary_sikap <- sikap_df %>%
  group_by(demog_province) %>%
  summarise(
    across(
      c(prob_importance_health, prob_importance_education, prob_importance_humanrights,
        prob_importance_pollution, prob_importance_climatechange, prob_importance_devecon),
      list(
        avg = ~mean(. == "Sangat penting", na.rm = TRUE),
        sd  = ~sd(. == "Sangat penting", na.rm = TRUE),
        obs = ~sum(!is.na(.))
      ),
      .names = "{.col}_{.fn}"
    )
  ) %>%
  mutate(
    across(
      ends_with("_sd"),
      ~ . / sqrt(get(str_replace(cur_column(), "_sd", "_obs"))),
      .names = "{.col}_se"
    )
  )



summary_long <- summary_sikap %>%
  pivot_longer(
    cols = ends_with("_avg"), # Select all _avg columns
    names_to = "variable", # Create a new column for variable names
    values_to = "value"    # Create a new column for values
  ) %>%
  mutate(
    se = case_when(
      variable == "prob_importance_health_avg" ~ `prob_importance_health_sd_se`,
      variable == "prob_importance_education_avg" ~ `prob_importance_education_sd_se`,
      variable == "prob_importance_humanrights_avg" ~ `prob_importance_humanrights_sd_se`,
      variable == "prob_importance_pollution_avg" ~ `prob_importance_pollution_sd_se`,
      variable == "prob_importance_climatechange_avg" ~ `prob_importance_climatechange_sd_se`,
      variable == "prob_importance_devecon_avg" ~ `prob_importance_devecon_sd_se`
    )
  )


variable_labels <- c(
  "prob_importance_health_avg" = "Health",
  "prob_importance_education_avg" = "Education",
  "prob_importance_humanrights_avg" = "Minority Rights",
  "prob_importance_pollution_avg" = "Pollution",
  "prob_importance_climatechange_avg" = "Climate Change",
  "prob_importance_devecon_avg" = "The Economy"
)

variable_means <- summary_long %>%
  group_by(variable) %>%
  summarise(mean_value = mean(value, na.rm = TRUE))

summary_long %>%
  ggplot(aes(y = reorder(demog_province, -value), x = value)) +
  geom_point() +
  geom_errorbarh(aes(xmin = value - 1.96 * se, xmax = value + 1.96 * se)) +
  facet_wrap(
    ~variable, 
    scales = "free_x", 
    labeller = labeller(variable = variable_labels)
  ) +
  xlab('Percent Saying The Issue is "Very Important"') +
  geom_vline(
    data = variable_means, 
    aes(xintercept = mean_value), 
    color = "red", 
    linetype = "dashed"
  ) +
  theme_bw() +
  theme(
    axis.line.x.bottom = element_line(color = "black"),
    axis.line.y.left = element_line(color = "black"),
    axis.title.y = element_blank(),
    panel.grid.major = element_blank(),
    legend.position = "bottom",
    legend.title = element_blank(),
    panel.border = element_blank(),
    plot.caption = element_text(hjust = 0),
    axis.ticks = element_blank()
  )

ggsave("./_4_outputs/figures/figure_a6.pdf", plot = last_plot(), width = 12, height = 10, dpi = 300)
