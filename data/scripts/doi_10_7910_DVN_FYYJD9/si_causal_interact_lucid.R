### ------------- Setup ------
## Create indicator for USA v. others
lucid_conjoint_p_long <- 
  lucid_conjoint_p_long %>%
  mutate(
    i_foreign = case_when(
      country_ == "United States" ~ "United States",
      country_ != "United States" ~ "Other"
    ),
    i_foreign = factor(i_foreign, levels = c("United States", "Other"))
  )

### ------------- Fig. S10: Conditional marginal means for US v. non-US -------- 
est_out_usa <- list()
est_out_othr <- list()
est_out_diff <- list()
var_list <- c("sex_",
              "age_",
              "harm_",
              "exposure_",
              "occupation_",
              "work_")

for (i in 1:length(var_list)) {
  col <- sym(var_list[i])
  est_out_usa[[i]] <-
    lucid_conjoint_p_long %>%
    filter(country_ == "United States") %>%
    group_by(!!col) %>%
    do(tidy(
      lm_robust(conjoint_chosen ~ 1, data = .,
                clusters = admin_ResponseId)
    )) %>%
    mutate(Z = paste0(var_list[i],!!col),
           X = "United States")
  
  est_out_othr[[i]] <-
    lucid_conjoint_p_long %>%
    filter(country_ != "United States") %>%
    group_by(!!col) %>%
    do(tidy(
      lm_robust(conjoint_chosen ~ 1, data = .,
                clusters = admin_ResponseId)
    )) %>%
    mutate(Z = paste0(var_list[i],!!col),
           X = "Other")
  
  ## Now compute differences in marginal means between USA and non-USA at each
  ## randomly assigned attribute level. This yields the causal effect of non-US 
  ## country of origin at each other randomized feature. 
  est_out_diff[[i]] <-
    lucid_conjoint_p_long %>%
    filter(!is.na(country_)) %>% 
    group_by(!!col) %>%
    do(tidy(
      lm_robust(conjoint_chosen ~ i_foreign, data = .,
                clusters = admin_ResponseId)
    )) %>%
    filter(term == "i_foreignOther") %>% 
    mutate(Z = paste0(var_list[i],!!col),
           X = "Difference")
  
}

gg_df <-
  bind_rows(est_out_usa,
            est_out_othr,
            est_out_diff) %>%
  ungroup() %>%
  dplyr::select(Z, X, estimate, std.error, statistic, p.value, 
                conf.low, conf.high) %>%
  mutate(
    attribute = str_to_title(word(Z, 1, sep = "_")),
    level = word(Z, 2, sep = "_"),
    level = str_replace(level, " \\(e.g., police/fire\\)", ""),
    level = case_when(
      level == "Non-essential worker" ~ "Non-essential workers", 
      TRUE ~ paste(level)
    ),
    level = factor(
      level,
      levels = c(
        "Male",
        "Female",
        "18-24",
        "25-34",
        "35-44",
        "45-54",
        "55-64",
        "65-74",
        "75+",
        "Low",
        "Moderate",
        "High",
        "China",
        "Pakistan",
        "Australia",
        "India",
        "Nigeria",
        "South Africa",
        "Brazil",
        "Canada",
        "United States",
        "Non-essential workers",
        "Public transit",
        "Education and childcare",
        "First responders",
        "Healthcare workers",
        "Yes",
        "No"
      )
    ),
    attribute = factor(
      attribute, 
      levels = c(
        "Country",
        "Exposure",
        "Harm",
        "Occupation",
        "Work",
        "Age",
        "Sex"
      ),
      labels = c(
        "Country of Origin",
        "Risk of exposure to COVID-19",
        "Risk of serious illness from COVID-19",
        "Occupation group",
        "Can work from home",
        "Age group",
        "Sex"
      )
    )
  )


g1 <- 
  gg_df %>%
  filter(X == "Other") %>%
  ggplot(.,
         aes(x = estimate,
             y = level)) +
  geom_vline(
    xintercept = 0.5,
    color = "black",
    lty = 2,
    size = 0.2
  ) +
  geom_errorbarh(
    aes(xmin = conf.low, xmax = conf.high),
    height = 0,
    color = "black",
    position = position_dodgev(height = .5),
    size = 1
  ) +
  geom_point(
    size = 2.5,
    pch = 22,
    color = "white",
    fill = "black",
    position = position_dodgev(height = .5)
  ) +
  facet_col(attribute ~ ., scales = "free_y", space = "free") +
  scale_x_continuous( 
    expand = c(0.01, 0.01)) +
  labs(x = "Probability of selecting non-U.S. recipient",
       y = "",
       caption = "") +
  theme_tufte() +
  theme(
    strip.text.x = element_text(hjust = 0),
    axis.ticks.y = element_blank(),
    axis.ticks.x = element_line(size = 0.25),
    strip.text = element_text(size = 11),
    axis.line.x = element_line(size = 0.25),
    axis.title.x = element_text(size = 10),
    axis.text.y = element_text(size = 11),
    plot.margin = unit(c(
      t = 0.1,
      r = 0.1,
      b = -0.5,
      l = 0.1
    ), "lines"),
    legend.position = "bottom",
    legend.justification = "left",
    legend.margin = margin(0, 0, 0, 0),
    legend.box.margin = margin(-10, -10, -10, -10)
  )

g2 <- 
  gg_df %>%
  filter(X == "United States") %>%
  ggplot(.,
         aes(x = estimate,
             y = level)) +
  geom_vline(
    xintercept = 0.5,
    color = "black",
    lty = 2,
    size = 0.2
  ) +
  geom_errorbarh(
    aes(xmin = conf.low, xmax = conf.high),
    height = 0,
    color = "black",
    position = position_dodgev(height = .5),
    size = 1
  ) +
  geom_point(
    size = 2.5,
    pch = 21,
    color = "white",
    fill = "black",
    position = position_dodgev(height = .5)
  ) +
  facet_col(attribute ~ ., scales = "free_y", space = "free") +
  scale_x_continuous(
    expand = c(0.01, 0.01)) +
  labs(x = "Probability of selecting individual",
       y = "",
       caption = "") +
  theme_tufte() +
  theme(
    axis.ticks.y = element_blank(),
    axis.ticks.x = element_line(size = 0.25),
    strip.text = element_text(size = 11),
    axis.line.x = element_line(size = 0.25),
    axis.title.x = element_text(size = 10),
    axis.text.y = element_text(size = 11),
    plot.margin = unit(c(
      t = 0.1,
      r = 0.1,
      b = -0.5,
      l = 0.1
    ), "lines"),
    legend.position = "bottom",
    legend.justification = "left",
    legend.margin = margin(0, 0, 0, 0),
    legend.box.margin = margin(-10, -10, -10, -10)
  ) 


g3 <-
  gg_df %>%
  filter(X == "Difference") %>%
  ggplot(.,
         aes(x = estimate,
             y = level)) +
  geom_vline(
    xintercept = 0,
    color = "black",
    lty = 2,
    size = 0.2
  ) +
  geom_errorbarh(
    aes(xmin = conf.low, xmax = conf.high),
    height = 0,
    color = "black",
    position = position_dodgev(height = .5),
    size = 1
  ) +
  geom_point(
    size = 2.5,
    pch = 24,
    color = "white",
    fill = "black",
    position = position_dodgev(height = .5)
  ) +
  facet_col(attribute ~ ., scales = "free_y", space = "free") +
  scale_x_continuous(
    expand = c(0.01, 0.01)) +
  labs(x = "Difference in selection probabilities",
       y = "",
       caption = "") +
  theme_tufte() +
  theme(
    axis.ticks.y = element_blank(),
    axis.ticks.x = element_line(size = 0.25),
    strip.text = element_text(size = 11),
    axis.line.x = element_line(size = 0.25),
    axis.title.x = element_text(size = 10),
    axis.text.y = element_text(size = 11),
    plot.margin = unit(c(
      t = 0.1,
      r = 0.1,
      b = -0.5,
      l = 0.1
    ), "lines"),
    legend.position = "bottom",
    legend.justification = "left",
    legend.margin = margin(0, 0, 0, 0),
    legend.box.margin = margin(-10, -10, -10, -10)
  )

g <- 
  egg::ggarrange(
    g1 + labs(x = "Conditional probability\nof selection: non-U.S.") + 
      theme(strip.text.x = element_text(size = 11, color = "white"),
            panel.border = element_rect(fill = NA, colour = "black")),
    g2 + labs(x = "Conditional probability\nof selection: U.S.") +
      theme(
        axis.text.y = element_blank(),
        axis.ticks.y = element_blank(),
        axis.title.y = element_blank(),
        strip.text.x = element_text(color = "white", size = 11),
        panel.border = element_rect(fill = NA, colour = "black")
      ),
    g3 + labs(x = "Difference: effect\nof non-U.S. v. U.S.")  + 
      theme(
        axis.text.y = element_blank(),
        axis.ticks.y = element_blank(),
        axis.title.y = element_blank(),
        strip.text.x = element_text(color = "white", size = 11),
        panel.border = element_rect(fill = NA, colour = "black")
      ),
    nrow = 1
  )


## Now convert grob into plot and add text ...
p <- ggplotify::as.ggplot(g)

pdf(file =  "lucid_conjoint_a_means_country.pdf",
    width = 7.6,
    height = 4 + fixed + spacing * (nrow(g1$data)))

grid::grid.text("Risk of exposure to COVID-19:", 
                x = unit(0.235, "npc"), y = unit(0.98, "npc"), 
                just = "left",
                gp = gpar(fontsize = 12, col = "black", fontfamily = "serif",
                          fontface = "bold"))
grid::grid.text("Risk of serious illness from COVID-19:", 
                x = unit(0.235, "npc"), y = unit(0.845, "npc"), 
                just = "left",
                gp = gpar(fontsize = 12, col = "black", fontfamily = "serif",
                          fontface = "bold"))
grid::grid.text("Occupation group:", 
                x = unit(0.235, "npc"), y = unit(0.713, "npc"), 
                just = "left",
                gp = gpar(fontsize = 12, col = "black", fontfamily = "serif",
                          fontface = "bold"))
grid::grid.text("Can work from home:", 
                x = unit(0.235, "npc"), y = unit(0.515, "npc"), 
                just = "left",
                gp = gpar(fontsize = 12, col = "black", fontfamily = "serif",
                          fontface = "bold"))
grid::grid.text("Age group:", 
                x = unit(0.235, "npc"), y = unit(0.412, "npc"), 
                just = "left",
                gp = gpar(fontsize = 12, col = "black", fontfamily = "serif",
                          fontface = "bold"))
grid::grid.text("Sex:", 
                x = unit(0.235, "npc"), y = unit(0.157, "npc"), 
                just = "left",
                gp = gpar(fontsize = 12, col = "black", fontfamily = "serif",
                          fontface = "bold"))
dev.off()


## Export underlying estimates to CSV
bind_rows(g1$data, g2$data, g3$data) %>%
  select(X, 
         attribute,
         level,
         estimate,
         std.error,
         statistic,
         p.value,
         conf.low,
         conf.high) %>%
  mutate_if(is.numeric, round, 2) %>%
  write_csv("lucid_conjoint_a_means_country.csv")


### ------------- Fig. S11: Conditional AMCEs for US v. non-US -------- 

lucid_foreign_amce <-
  lm_robust(
    conjoint_chosen ~ sex_ + age_ + harm_ + exposure_ + occupation_ + work_,
    clusters = admin_ResponseId,
    data = lucid_conjoint_p_long %>% 
      filter(i_foreign == "Other")
  ) %>%
  tidy() %>% 
  filter(term != "(Intercept)") %>%
  mutate(X = "Other")

lucid_usa_amce <-
  lm_robust(
    conjoint_chosen ~ sex_ + age_ + harm_ + exposure_ + occupation_ + work_,
    clusters = admin_ResponseId,
    data = lucid_conjoint_p_long %>% 
      filter(i_foreign == "United States")
  ) %>%
  tidy() %>% 
  filter(term != "(Intercept)") %>%
  mutate(X = "United States")

lucid_foreign_acies <-
  lm_robust(
    conjoint_chosen ~ i_foreign*sex_ + i_foreign*age_ + i_foreign*harm_ + 
      i_foreign*exposure_ + i_foreign*occupation_ + i_foreign*work_,
    clusters = admin_ResponseId,
    data = lucid_conjoint_p_long
  ) %>%
  tidy() %>% 
  filter(str_detect(term, ":")) %>%
  mutate(term = str_remove(term, ".*:"),
         X = "Difference")


gg_pconjoint_amce_foreign  <-
  bind_rows(lucid_foreign_amce, lucid_usa_amce, lucid_foreign_acies)  %>%
  mutate(
    attribute_ref =
      case_when(
        str_detect(term, "sex_") ~ "Sex (reference: Female)",
        str_detect(term, "age_") ~ "Age group (reference: 18-24)",
        str_detect(term, "harm_") ~ "Risk of serious illness from COVID-19 (reference: Low)",
        str_detect(term, "exposure_") ~ "Risk of exposure to COVID-19 (reference: Low)",
        str_detect(term, "country_") ~  "Country of origin (reference: United States)",
        str_detect(term, "occupation_") ~ "Occupation group (reference: Non-essential workers)",
        str_detect(term, "work_") ~ "Can work from home (reference: Yes)"
      ),
    attribute_ref = factor(
      attribute_ref,
      levels = c(
        "Country of origin (reference: United States)",
        "Risk of exposure to COVID-19 (reference: Low)",
        "Risk of serious illness from COVID-19 (reference: Low)",
        "Occupation group (reference: Non-essential workers)",
        "Can work from home (reference: Yes)",
        "Age group (reference: 18-24)",
        "Sex (reference: Female)"
      )
    ),
    level = str_replace(
      term,
      "sex_|age_|harm_|exposure_|country_|occupation_|work_",
      ""
    ),
    level = str_replace(level, " \\(e.g., police/fire\\)", ""),
    level = case_when(
      level == "Non-essential worker" ~ "Non-essential workers", 
      TRUE ~ paste(level)
    ), 
    level = factor(
      level,
      levels = c(
        "Male",
        "25-34",
        "35-44",
        "45-54",
        "55-64",
        "65-74",
        "75+",
        "Moderate",
        "High",
        "China",
        "Pakistan",
        "Australia",
        "India",
        "Nigeria",
        "South Africa",
        "Brazil",
        "Canada",
        "Public transit",
        "Education and childcare",
        "First responders",
        "Healthcare workers",
        "No"
      )
    )
  )


g1 <- 
  gg_pconjoint_amce_foreign  %>%
  filter(X == "Other") %>%
  ggplot(.,
         aes(x = estimate,
             y = level)) +
  geom_vline(
    xintercept = 0,
    color = "black",
    lty = 2,
    size = 0.2
  ) +
  geom_errorbarh(
    aes(xmin = conf.low, xmax = conf.high),
    height = 0,
    color = "black",
    position = position_dodgev(height = .5),
    size = 1
  ) +
  geom_point(
    size = 2.5,
    pch = 22,
    color = "white",
    fill = "black",
    position = position_dodgev(height = .5)
  ) +
  facet_col(attribute_ref ~ ., scales = "free_y", space = "free") +
  scale_x_continuous(
    expand = c(0.01, 0.01)) +
  labs(x = "AMCE",
       y = "",
       caption = "") +
  theme_tufte() +
  theme(
    strip.text.x = element_text(hjust = 0),
    axis.ticks.y = element_blank(),
    axis.ticks.x = element_line(size = 0.25),
    strip.text = element_text(size = 11),
    axis.line.x = element_line(size = 0.25),
    axis.title.x = element_text(size = 10),
    axis.text.y = element_text(size = 11),
    plot.margin = unit(c(
      t = 0.1,
      r = 0.1,
      b = -0.5,
      l = 0.1
    ), "lines"),
    legend.position = "bottom",
    legend.justification = "left",
    legend.margin = margin(0, 0, 0, 0),
    legend.box.margin = margin(-10, -10, -10, -10)
  )

g2 <- 
  gg_pconjoint_amce_foreign  %>%
  filter(X == "United States") %>%
  ggplot(.,
         aes(x = estimate,
             y = level)) +
  geom_vline(
    xintercept = 0,
    color = "black",
    lty = 2,
    size = 0.2
  ) +
  geom_errorbarh(
    aes(xmin = conf.low, xmax = conf.high),
    height = 0,
    color = "black",
    position = position_dodgev(height = .5),
    size = 1
  ) +
  geom_point(
    size = 2.5,
    pch = 21,
    color = "white",
    fill = "black",
    position = position_dodgev(height = .5)
  ) +
  facet_col(attribute_ref ~ ., scales = "free_y", space = "free") +
  scale_x_continuous(
    expand = c(0.01, 0.01)) +
  labs(x = "AMCE",
       y = "",
       caption = "") +
  theme_tufte() +
  theme(
    strip.text.x = element_text(hjust = 0),
    axis.ticks.y = element_blank(),
    axis.ticks.x = element_line(size = 0.25),
    strip.text = element_text(size = 11),
    axis.line.x = element_line(size = 0.25),
    axis.title.x = element_text(size = 10),
    axis.text.y = element_text(size = 11),
    plot.margin = unit(c(
      t = 0.1,
      r = 0.1,
      b = -0.5,
      l = 0.1
    ), "lines"),
    legend.position = "bottom",
    legend.justification = "left",
    legend.margin = margin(0, 0, 0, 0),
    legend.box.margin = margin(-10, -10, -10, -10)
  )


g3 <-
  gg_pconjoint_amce_foreign  %>%
  filter(X == "Difference") %>%
  ggplot(.,
         aes(x = estimate,
             y = level)) +
  geom_vline(
    xintercept = 0,
    color = "black",
    lty = 2,
    size = 0.2
  ) +
  geom_errorbarh(
    aes(xmin = conf.low, xmax = conf.high),
    height = 0,
    color = "black",
    position = position_dodgev(height = .5),
    size = 1
  ) +
  geom_point(
    size = 2.5,
    pch = 24,
    color = "white",
    fill = "black",
    position = position_dodgev(height = .5)
  ) +
  facet_col(attribute_ref ~ ., scales = "free_y", space = "free") +
  scale_x_continuous( 
    expand = c(0.01, 0.01)) +
  labs(x = "AMCE",
       y = "",
       caption = "") +
  theme_tufte() +
  theme(
    strip.text.x = element_text(hjust = 0),
    axis.ticks.y = element_blank(),
    axis.ticks.x = element_line(size = 0.25),
    strip.text = element_text(size = 11),
    axis.line.x = element_line(size = 0.25),
    axis.title.x = element_text(size = 10),
    axis.text.y = element_text(size = 11),
    plot.margin = unit(c(
      t = 0.1,
      r = 0.1,
      b = -0.5,
      l = 0.1
    ), "lines"),
    legend.position = "bottom",
    legend.justification = "left",
    legend.margin = margin(0, 0, 0, 0),
    legend.box.margin = margin(-10, -10, -10, -10)
  )

g <- 
  egg::ggarrange(
    g1 + labs(x = "Conditional AMCE:\nnon-U.S. recipient") + 
      theme(strip.text.x = element_text(size = 10, color = "white"),
            strip.placement = "outside",
            panel.border = element_rect(fill = NA, colour = "black")),
    g2 + labs(x = "Conditional AMCE:\nU.S. recipient") +
      theme(
        axis.text.y = element_blank(),
        axis.ticks.y = element_blank(),
        axis.title.y = element_blank(),
        strip.text.x = element_text(color = "white", size = 10),
        panel.border = element_rect(fill = NA, colour = "black")
      ),
    g3 + labs(x = "Difference: average component\ninteraction effect (ACIE)")  + 
      theme(
        axis.text.y = element_blank(),
        axis.ticks.y = element_blank(),
        axis.title.y = element_blank(),
        strip.text.x = element_text(color = "white", size = 10),
        panel.border = element_rect(fill = NA, colour = "black")
      ),
    nrow = 1
  )


## Now convert grob into plot and add text ...
p <- ggplotify::as.ggplot(g)

pdf(file =  "lucid_conjoint_p_amces_country.pdf",
    width = 7.6,
    height = 4 + fixed + spacing * (nrow(g1$data)))
p
grid::grid.text("Risk of exposure to COVID-19 (reference: Low):", 
                x = unit(0.235, "npc"), y = unit(0.98, "npc"), 
                just = "left",
                gp = gpar(fontsize = 12, col = "black", fontfamily = "serif",
                          fontface = "bold"))
grid::grid.text("Risk of serious illness from COVID-19 (reference: Low):", 
                x = unit(0.235, "npc"), y = unit(0.848, "npc"), 
                just = "left",
                gp = gpar(fontsize = 12, col = "black", fontfamily = "serif",
                          fontface = "bold"))
grid::grid.text("Occupation group (reference: Non-essential workers):", 
                x = unit(0.235, "npc"), y = unit(0.720, "npc"), 
                just = "left",
                gp = gpar(fontsize = 12, col = "black", fontfamily = "serif",
                          fontface = "bold"))
grid::grid.text("Can work from home (reference: Yes):", 
                x = unit(0.235, "npc"), y = unit(0.515, "npc"), 
                just = "left",
                gp = gpar(fontsize = 12, col = "black", fontfamily = "serif",
                          fontface = "bold"))
grid::grid.text("Age group (reference: 18-24):", 
                x = unit(0.235, "npc"), y = unit(0.425, "npc"), 
                just = "left",
                gp = gpar(fontsize = 12, col = "black", fontfamily = "serif",
                          fontface = "bold"))
grid::grid.text("Sex (reference: Female):", 
                x = unit(0.235, "npc"), y = unit(0.145, "npc"), 
                just = "left",
                gp = gpar(fontsize = 12, col = "black", fontfamily = "serif",
                          fontface = "bold"))
dev.off()

## Export underlying estimates to CSV
bind_rows(g1$data, g2$data, g3$data) %>%
  select(X, 
         attribute_ref,
         level,
         estimate,
         std.error,
         statistic,
         p.value,
         conf.low,
         conf.high) %>%
  mutate_if(is.numeric, round, 2) %>%
  write_csv("lucid_conjoint_p_amces_country.csv")



### ------------- Fig. S12: Marginal means for US v. non-US pairs -------- 

## Condition on profiles with comparisons between US and non-US recipients
pair_df <- 
  lucid_conjoint_p_long %>% 
  filter(usa_pair == 1)

est_out_usa <- list()
est_out_othr <- list()
var_list <- c("sex_",
              "age_",
              "harm_",
              "exposure_",
              "occupation_",
              "work_")

for (i in 1:length(var_list)) {
  col <- sym(var_list[i])
  est_out_usa[[i]] <-
    pair_df %>%
    filter(country_ == "United States") %>%
    group_by(!!col) %>%
    do(tidy(
      lm_robust(conjoint_chosen ~ 1, data = .,
                clusters = admin_ResponseId)
    )) %>%
    mutate(Z = paste0(var_list[i],!!col),
           X = "United States")
  
  est_out_othr[[i]] <-
    pair_df %>%
    filter(country_ != "United States") %>%
    group_by(!!col) %>%
    do(tidy(
      lm_robust(conjoint_chosen ~ 1, data = .,
                clusters = admin_ResponseId)
    )) %>%
    mutate(Z = paste0(var_list[i],!!col),
           X = "Other")
}

gg_df <-
  bind_rows(est_out_usa, est_out_othr) %>%
  ungroup() %>%
  dplyr::select(Z, X, estimate, std.error, statistic, p.value, conf.low, 
                conf.high) %>%
  mutate(
    attribute = str_to_title(word(Z, 1, sep = "_")),
    level = word(Z, 2, sep = "_"),
    level = str_replace(level, " \\(e.g., police/fire\\)", ""),
    level = case_when(
      level == "Non-essential worker" ~ "Non-essential workers", 
      TRUE ~ paste(level)
    ),
    level = factor(
      level,
      levels = c(
        "Male",
        "Female",
        "18-24",
        "25-34",
        "35-44",
        "45-54",
        "55-64",
        "65-74",
        "75+",
        "Low",
        "Moderate",
        "High",
        "Non-essential workers",
        "Public transit",
        "Education and childcare",
        "First responders",
        "Healthcare workers",
        "Yes",
        "No"
      )
    ),
    attribute = factor(
      attribute, 
      levels = c(
        "Exposure",
        "Harm",
        "Occupation",
        "Work",
        "Age",
        "Sex"
      ),
      labels = c(
        "Risk of exposure to COVID-19",
        "Risk of serious illness from COVID-19",
        "Occupation group",
        "Can work from home",
        "Age group",
        "Sex"
      )
    )
  )

g <-
  gg_df %>%
  mutate(X = factor(X, levels = c("United States", "Other"))) %>% 
  ggplot(.,
         aes(
           x = estimate,
           y = level,
           fill = X,
           color = X,
           group = X,
           shape = X
         )) +
  geom_vline(
    xintercept = 0.5,
    color = "black",
    lty = 2,
    size = 0.2
  ) +
  geom_errorbarh(
    aes(xmin = conf.low, xmax = conf.high),
    height = 0,
    position = position_dodgev(height = .5),
    size = 1
  ) +
  geom_point(
    size = 2.5,
    color = "white",
    position = position_dodgev(height = .5)
  ) +
  facet_col(attribute ~ ., scales = "free_y", space = "free") +
  scale_color_manual("", values = c("black", "black")) + 
  scale_fill_manual("", values = c("black", "black")) + 
  scale_shape_manual("", values = c(22, 24)) +
  labs(
    x = "Marginal mean",
    y = "",
    caption = "") +
  theme_tufte() +
  theme(
    axis.ticks.y = element_blank(),
    axis.ticks.x = element_line(size = 0.25),
    strip.text = element_text(size = 11),
    axis.line.x = element_line(size = 0.25),
    axis.title.x = element_text(size = 10),
    axis.text.y = element_text(size = 11),
    plot.margin = unit(c(
      t = 0.1,
      r = 0.1,
      b = -0.5,
      l = 0.1
    ), "lines"),
    legend.position = "bottom",
    legend.justification = "left",
    legend.margin = margin(0, 0, 0, 0),
    legend.box.margin = margin(-10, -10, -10, -10)
  ) 


g <- 
  g + 
  scale_x_continuous(limits = c(0.2, 0.8))  

ggsave(filename = "lucid_pconjoint_pair_means.pdf",
       g,
       width = 5,
       height = fixed + spacing * (nrow(g$data)))

## Export underlying estimates to CSV
g$data %>%
  select(X, 
         attribute,
         level,
         estimate,
         std.error,
         statistic,
         p.value,
         conf.low,
         conf.high) %>%
  mutate_if(is.numeric, round, 2) %>%
  write_csv("lucid_pconjoint_pair_means.csv")

