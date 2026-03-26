.pol_data <-
  estimation_data %>%
  filter(A == "Wave 1") %>%
  ungroup() %>%
  summarise_at(vars(V_12_A, V_12_B, V_12_C, V_12_D, V_12_E, V_12_F, V_12_G), funs(avg = mean(. == 4, na.rm = T),
                                                                                  sd = sd(. == 4, na.rm = T),
                                                                                  obs = n())) %>%
  pivot_longer(everything()) %>%
  mutate(var = str_sub(name, 1, 6),
         stat = str_sub(name, 8, nchar(name))) %>%
  pivot_wider(id_cols = var, values_from = value, names_from = stat) %>%
  mutate(se = sd/sqrt(obs)) %>%
  mutate(label = case_when(var == "V_12_A" ~ "Health",
                           var == "V_12_B" ~ "Education",
                           var == "V_12_C" ~ "Civil rights",
                           var == "V_12_D" ~ "Pollution",
                           var == "V_12_E" ~ "Minority rights",
                           var == "V_12_F" ~ "Climate change",
                           var == "V_12_G" ~ "Economy")) %>%
  mutate(type = "Politician first-order beliefs") %>%
  filter(var != "V_12_C")




.voter_data <-
  sikap_df %>%
  ungroup() %>%
  summarise_at(vars(prob_importance_health, prob_importance_education, prob_importance_humanrights, prob_importance_pollution, 
                    prob_importance_climatechange, prob_importance_devecon), 
               funs(avg = mean(. == "Sangat penting", na.rm = T),
                    sd = sd(. == "Sangat penting", na.rm = T),
                    obs = n())) %>%
  pivot_longer(everything()) %>%
  mutate(var = str_sub(name, 17, nchar(name))) %>%
  separate(., col = var, into = c("var", "stat"), sep = "\\_") %>%
  pivot_wider(id_cols = var, values_from = value, names_from = stat) %>%
  mutate(se = sd/sqrt(obs)) %>%
  mutate(label = case_when(var == "health" ~ "Health",
                           var == "education" ~ "Education",
                           var == "pollution" ~ "Pollution",
                           var == "humanrights" ~ "Minority rights",
                           var == "climatechange" ~ "Climate change",
                           var == "devecon" ~ "Economy")) %>%
  mutate(type = "Voters' first-order beliefs")


fig1 <-
  bind_rows(.pol_data, .voter_data) %>%
  mutate(label = factor(label, levels = c("Climate change", "Pollution", "Minority rights", "Economy", "Education", "Health"))) %>%
  ggplot(aes(y=label, x = avg*100, group = type, fill = type)) +
  geom_point(position = position_dodge(width = 0.3), shape = 21, alpha = 1, size = 3) +
  geom_errorbarh(aes(xmin = (avg - 1.96*se)*100, xmax = (avg + 1.96*se)*100), position = position_dodge(width = 0.3), height = 0.1) +
  theme_bw() +
  theme(
    axis.line.x.bottom = element_line(color = "black"),
    axis.line.y.left = element_line(color = "black"),
    axis.text.x = element_text(color = "black"),
    axis.text.y = element_text(color = "black"),   
    axis.title.x = element_text(color = "black", margin = margin(t = 15)),
    axis.title.y = element_blank(),
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank(),
    legend.position = "bottom",
    legend.title = element_blank(),
    panel.border = element_blank(),
    text = element_text(size = 10),
    plot.caption = element_text(hjust = 0),
    axis.ticks = element_blank()) +
  scale_fill_manual(values = c("lightgrey", "black")) +
  scale_color_manual(values = c("#2F9599", "#A7226E")) +
  xlab('Believe the issue is "very important"') +
  xlim(0, 90) +
  scale_x_continuous(breaks = c(0, 25, 50, 75), 
                     labels = c("0%", "25", "50", "75%"), 
                     limits = c(0, 90))

ggsave("./_4_outputs/figures/figure_1.pdf", plot = fig1, width = 8, height = 4)
write_csv(.pol_data, "./_4_outputs/supplementary/figure_1_a.csv")
write_csv(.voter_data, "./_4_outputs/supplementary/figure_1_b.csv")
