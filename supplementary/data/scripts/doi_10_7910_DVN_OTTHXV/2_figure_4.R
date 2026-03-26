sikap <- sikap_36

# likert style (original)
likert_style <-
  sikap %>%
  ungroup() %>%
  summarise_at(vars(probpol_importance_health, probpol_importance_education, probpol_importance_humanrights, 
                    probpol_importance_pollution, probpol_importance_climatechange, probpol_importance_devecon), 
               funs(avg = mean(. == 4, na.rm = TRUE),
                    sd = sd(. == 4, na.rm = TRUE),
                    obs = n())) %>%
  pivot_longer(everything()) %>%
  mutate(var = str_sub(name, 20, nchar(name))) %>%
  separate(., col = var, into = c("var", "stat"), sep = "\\_") %>%
  pivot_wider(id_cols = var, values_from = value, names_from = stat) %>%
  mutate(se = sd / sqrt(obs)) %>%
  mutate(label = case_when(
    var == "health" ~ "Health",
    var == "education" ~ "Education",
    var == "pollution" ~ "Pollution",
    var == "humanrights" ~ "Minority rights",
    var == "climatechange" ~ "Climate change",
    var == "devecon" ~ "Economy"
  )) %>%
  mutate(type = "Voters' first-order beliefs") %>%
  mutate(type_label = "(a) Likert-style\n")

# singular most important issue
single_issue <-
  sikap %>%
  mutate(single_health = (Q270 == 1) * 1,
         single_education = (Q270 == 3) * 1,
         single_humanrights = (Q270 == 4) * 1,
         single_pollution = (Q270 == 5) * 1,
         single_climatechange = (Q270 == 6) * 1,
         single_economy = (Q270 == 7) * 1) %>%
  summarise_at(vars(starts_with("single_")), 
               funs(avg = mean(. == 1, na.rm = TRUE),
                    sd = sd(. == 1, na.rm = TRUE),
                    obs = n())) %>%
  pivot_longer(everything()) %>%
  mutate(var = str_sub(name, 8, nchar(name))) %>%
  separate(., col = var, into = c("var", "stat"), sep = "\\_") %>%
  pivot_wider(id_cols = var, values_from = value, names_from = stat) %>%
  mutate(se = sd / sqrt(obs)) %>%
  mutate(label = case_when(
    var == "health" ~ "Health",
    var == "education" ~ "Education",
    var == "pollution" ~ "Pollution",
    var == "humanrights" ~ "Minority rights",
    var == "climatechange" ~ "Climate change",
    var == "economy" ~ "Economy"
  )) %>%
  mutate(label = factor(label, levels = c("Health", "Education", "Economy", "Minority rights", "Pollution", "Climate change"))) %>%
  mutate(type_label = "(b) Single most important issue\n")

rank_order <-
  sikap %>%
  select(Q271_1:Q271_7) %>%
  zap_labels() %>%
  pivot_longer(everything()) %>%
  group_by(name) %>%
  summarise(avg = mean(value, na.rm = TRUE),
            sd = sd(value, na.rm = TRUE),
            obs = n()) %>%
  mutate(se = sd / sqrt(obs)) %>%
  mutate(label = case_when(
    name == "Q271_1" ~ "Health",
    name == "Q271_3" ~ "Education",
    name == "Q271_5" ~ "Pollution",
    name == "Q271_4" ~ "Minority rights",
    name == "Q271_6" ~ "Climate change",
    name == "Q271_7" ~ "Economy"
  )) %>%
  mutate(label = factor(label, levels = c("Health", "Education", "Economy", "Minority rights", "Pollution", "Climate change"))) %>%
  mutate(type_label = "(c) Rank issue importance\n")

# conjoint analysis
conjoint_data <-
  sikap %>%
  select(ResponseId, starts_with("nick_cand1"), "A1_issue_imp_conjoint2") %>%
  gather(variable, value, -ResponseId) %>%
  separate(variable, into = c("var1", "var2", "var3", "var4"), sep = "\\_") %>%
  group_by(ResponseId) %>%
  mutate(choice_num = value[var1 == "A1"]) %>%
  filter(var1 != "A1") %>%
  group_by(ResponseId, var3) %>%
  mutate(attribute_group = value[var4 == "name"]) %>%
  filter(var4 != "name") %>%
  ungroup() %>%
  select(ResponseId, var4, value, choice_num, attribute_group) %>%
  pivot_wider(names_from = attribute_group, values_from = value, id_cols = c(ResponseId, var4, choice_num)) %>%
  mutate(option_num = str_remove(var4, "opt")) %>%
  mutate(outcome_val = (choice_num == option_num) * 1) %>%
  mutate(con_campaign_statement = case_when(
    str_detect(`Isi kampanye`, "iklim") ~ "Climate change",
    str_detect(`Isi kampanye`, "sipil") ~ "Minority rights",
    str_detect(`Isi kampanye`, "polusi") ~ "Pollution",
    str_detect(`Isi kampanye`, "kesehatan") ~ "Health",
    str_detect(`Isi kampanye`, "pendidikan") ~ "Education",
    str_detect(`Isi kampanye`, "ekonomi") ~ "Economy"
  ),
  con_campaign_statement = factor(con_campaign_statement, levels = c("Minority rights", "Education", "Health", "Economy", "Pollution", "Climate change")),
  con_age = Usia,
  con_gender = case_when(Gender == "Laki-laki" ~ "Man",
                         TRUE ~ "Woman"),
  con_religion = case_when(Agama == "Islam" ~ "Islam",
                           TRUE ~ "Christian"),
  con_education = case_when(`Latar Belakang Pendidikan` == "Magister (S2) atau Doktor (S3)" ~ "Postgraduate",
                            `Latar Belakang Pendidikan` == "Sarjana (S1)" ~ "Bachelor's",
                            `Latar Belakang Pendidikan` == "SMA atau sederajat" ~ "High school"))

levels_fct <- c("Campaign Statement:                        ", "Education", "Health", "Economy", "Minority rights", "Pollution", "Climate change", 
                "Age:                        ", "28", "45", "70",
                "Gender:                        ", "Man", "Woman",
                "Religion:                        ", "Christian", "Islam",
                "Education:                        ", "Bachelor's", "High school", "Postgraduate")

conjoint_analysis <-
  lm_robust(outcome_val ~ con_campaign_statement + con_age + con_gender + con_religion + con_education, data = conjoint_data, clusters = ResponseId) %>%
  tidy() %>%
  filter(str_detect(term, "con_campaign_statement")) %>%
  mutate(label = str_remove(term, "con_campaign_statement|con_age|con_gender|con_religion|con_education")) %>%
  select(term, avg = estimate, se = std.error, label) %>%
  add_row(.before = 4, label = "Minority rights", avg = 0, se = NA_real_) %>%
  mutate(type_label = "(d) Candidate choice conjoint\n")

comparison_fig <-
  bind_rows(likert_style, rank_order, single_issue, conjoint_analysis) %>%
  mutate(label = factor(label, levels = c("Education", "Health", "Minority rights", "Economy", "Pollution", "Climate change"))) %>%
  mutate(type_label = factor(type_label, levels = c("(a) Likert-style\n",
                                                    "(b) Single most important issue\n",
                                                    "(c) Rank issue importance\n",
                                                    "(d) Candidate choice conjoint\n"))) %>%
  ggplot(aes(x = avg, y = fct_rev(label))) +
  geom_point(color = "black") +
  facet_wrap(type_label ~ ., ncol = 2, scales = "free_x") +
  facetted_pos_scales(
    x = list(
      type_label == "(c) Rank issue importance\n" ~ scale_x_reverse()
    )
  ) +
  geom_errorbarh(aes(xmin = (avg - 1.96 * se), xmax = (avg + 1.96 * se)), height = 0.1, color = "black") +
  theme_bw() +
  theme(
    axis.line.x.bottom = element_line(color = "black"),
    axis.line.y.left = element_line(color = "black"),
    axis.text.x = element_text(color = "black"),
    axis.text.y = element_text(color = "black"),
    axis.title.x = element_blank(),
    axis.title.y = element_blank(),
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank(),
    panel.border = element_blank(),
    legend.position = "bottom",
    strip.text = element_text(size = 15),
    text = element_text(size = 12),
    legend.title = element_blank(),
    strip.background = element_blank(),
    panel.spacing.y = unit(2.5, "lines"),
    plot.caption = element_text(hjust = 0, color = "black"),
    axis.ticks = element_blank()
  ) +
  scale_fill_manual(values = c("lightgrey", "black"))


ggsave("./_4_outputs/figures/figure_4.pdf", plot = comparison_fig, width = 8, height = 5)
