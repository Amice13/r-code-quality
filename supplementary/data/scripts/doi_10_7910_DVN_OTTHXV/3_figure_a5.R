
# Prepare Politician Data. Perform Sample Split
pol_data <- estimation_data %>%
  filter(A == "Wave 1") %>%
  ungroup() %>%
  mutate(V_12_F_group = ifelse(V_12_F == 4, 1, 0)) %>%  # Create grouping variable
  group_by(V_12_F_group) %>%
  summarise_at(
    vars(V_13_A, V_13_B, V_13_C, V_13_D, V_13_E, V_13_F, V_13_G),
    list(
      avg = ~ mean(. == 4, na.rm = TRUE),
      sd = ~ sd(. == 4, na.rm = TRUE),
      obs = ~ n()
    )
  ) %>%
  pivot_longer(-V_12_F_group) %>%
  filter(!is.na(V_12_F_group)) %>%
  mutate(
    var = str_sub(name, 1, 6),
    stat = str_sub(name, 8, nchar(name))
  ) %>%
  pivot_wider(id_cols = c(V_12_F_group, var), names_from = stat, values_from = value) %>%
  mutate(
    se = sd / sqrt(obs),
    label = case_when(
      var == "V_13_A" ~ "Health",
      var == "V_13_B" ~ "Education",
      var == "V_13_C" ~ "Civil Rights",
      var == "V_13_D" ~ "Pollution",
      var == "V_13_E" ~ "Minority Rights",
      var == "V_13_F" ~ "Climate Change",
      var == "V_13_G" ~ "The Economy"
    ),
    type = "Politician Second-Order Beliefs"
  ) %>%
  filter(var != "V_13_C")

# Prepare Voter Data
voter_data <- sikap_df %>%
  ungroup() %>%
  summarise_at(
    vars(
      prob_importance_health, prob_importance_education,
      prob_importance_humanrights, prob_importance_pollution,
      prob_importance_climatechange, prob_importance_devecon
    ),
    list(
      avg = ~ mean(. == "Sangat penting", na.rm = TRUE),
      sd = ~ sd(. == "Sangat penting", na.rm = TRUE),
      obs = ~ sum(!is.na(.))
    )
  ) %>%
  pivot_longer(everything()) %>%
  mutate(var = str_sub(name, 17, nchar(name))) %>%
  separate(var, into = c("var", "stat"), sep = "\\_") %>%
  pivot_wider(id_cols = var, values_from = value, names_from = stat) %>%
  mutate(se = sd / sqrt(obs)) %>%
  mutate(
    label = case_when(
      var == "health" ~ "Health",
      var == "education" ~ "Education",
      var == "pollution" ~ "Pollution",
      var == "humanrights" ~ "Minority Rights",
      var == "climatechange" ~ "Climate Change",
      var == "devecon" ~ "The Economy"
    ),
    type = "Voters' First-Order Beliefs"
  )

# Adding 0-1 dummy for easier plotting
df_dummy <- data.frame(label = rep(0, 12), V_12_F_group = rep(0, 12)) %>%
  mutate(label=c("Health","Education","Pollution","Minority Rights","Climate Change","The Economy",
                 "Health","Education","Pollution","Minority Rights","Climate Change","The Economy"))%>%
  mutate(V_12_F_group = c(1,1,1,1,1,1,
                          0,0,0,0,0,0))


voter_data <- voter_data %>%
  left_join(df_dummy)

# Label
variable_labels <- c(
  "1" = "Policn blv Climate Change is 'Very Important'",
  "0" = "Policn blv Climate Change is NOT 'Very Important'"  
)


# Plot Graphs                   
fig2 <- bind_rows(pol_data, voter_data) %>%
  mutate(label = factor(label, levels = c("Climate Change", "Pollution", "Minority Rights", "The Economy", "Education", "Health"))) %>%
  ggplot(aes(y = label, x = avg * 100, group = type, fill = type)) +
  geom_point(position = position_dodge(width = 0.3), shape = 21, alpha = 1, size = 2) +
  geom_errorbarh(aes(xmin = (avg - 1.96 * se) * 100, xmax = (avg + 1.96 * se) * 100), 
                 position = position_dodge(width = 0.3), height = 0.1) +
  theme_bw() +
  facet_wrap(~ V_12_F_group, nrow = 1, scales = "free_x", labeller = labeller(V_12_F_group = variable_labels)) +
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
  ) +
  scale_fill_manual(values = c("lightgrey", "black")) +
  scale_color_manual(values = c("#2F9599", "#A7226E")) +
  xlab('Percent Saying The Issue is "Very Important"')


ggsave("./_4_outputs/figures/figure_a5.pdf", 
       plot = fig2, width = 8, height = 6, dpi = 300)