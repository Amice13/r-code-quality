### ---- Setup-----
mm_var_list <-
  c(
    "cost_",
    "prop_",
    "purpose_",
    "benefit_",
    "duration_"
  )

mm_feat_lvls <- rev(c(
  "$100 billion",
  "$75 billion",
  "$50 billion",
  "$25 billion",
  "100%",
  "75%",
  "50%",
  "25%",
  "0%",
  "Purchase vaccines made outside the U.S.",
  "Provide economic aid and debt forgiveness",
  "Finance public health infrastructure",
  "Purchase vaccines made in the U.S.",
  "Purchase patents for vaccine production",
  "U.S. allies and aligned countries",
  "Poor and low-income countries",
  "Countries most at risk for outbreaks",
  "9 years",
  "7 years",
  "5 years",
  "3 years",
  "1 year"
))

mm_feat_labs <- 
  rev(c(
    "$100 billion",
    "$75 billion",
    "$50 billion",
    "$25 billion",
    "100%",
    "75%",
    "50%",
    "25%",
    "0%",
    "Purchase vaccines made outside the U.S.",
    "Provide economic aid and debt forgiveness",
    "Finance public health infrastructure",
    "Purchase vaccines made in the U.S.",
    "Purchase patents for vaccine production",
    "U.S. allies and aligned countries",
    "Poor and low-income countries",
    "Countries most at risk for outbreaks",
    "9 years",
    "7 years",
    "5 years",
    "3 years",
    "1 year"
  ))

mm_attr_lvls <- c("Cost", "Prop", "Purpose", "Benefit", "Duration")

mm_attr_labs <- c(
  "Total cost of the agreement",
  "Proportion of total cost paid by the U.S.",
  "Funding used to",
  "Benefits directed toward",
  "Duration of agreement"
)

### ---- Table S4: association b.t. conjoint choice and COVAX donation -------- 

## Create indicator for individuals donating more than median
norc_conjoint_long$X <- ifelse(norc_conjoint_long$y_donate > 5, 1, 0)

## Simple F-test using donation amount as additional covariate
fit_0 <-
  lm_robust(
    conjoint_vote ~ cost_ + prop_ + purpose_ + benefit_ + duration_,
    data = norc_conjoint_long %>% filter(!is.na(y_donate)),
    clusters = CaseId
  )

fit_1 <-
  lm_robust(
    conjoint_vote ~ X + cost_ + prop_ + purpose_ + benefit_ + duration_,
    data = norc_conjoint_long %>% filter(!is.na(y_donate)),
    clusters = CaseId
  )

lmtest::waldtest(fit_0, fit_1, test = "F")

## Export table of estimates from regression with donations indicator on RHS
fit_1 %>%
  tidy() %>% 
  filter(term != "(Intercept)") %>% 
  mutate(entry = make_entry(est = estimate, se = std.error, p = p.value)) %>%
  dplyr::select(term, entry) %>% 
  xtable(., ) %>%
  print(include.rownames = TRUE, 
        include.colnames = FALSE,
        hline.after = c(),
        only.contents = TRUE,
        type = "latex",
        file = "norc_donatereg_table.tex")


### ---- Fig. S13: marginal means by COVAX donation indicator -------- 
gg_mm_donate <-
  het_mm_a(x = "X", long_data = norc_conjoint_long,            
           cluster = "CaseId",
           attr_lvls = mm_attr_lvls, attr_labs = mm_attr_labs,
           feat_lvls = mm_feat_lvls, feat_labs = mm_feat_labs,
           var_list = mm_var_list)

g1 <- 
  gg_mm_donate %>%
  filter(X == "Yes") %>% 
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
    position = position_dodgev(height = .9),
    size = 1
  ) +
  geom_point(
    size = 2.5,
    pch = 22,
    color = "white",
    fill = "black",
    position = position_dodgev(height = .9)
  ) +
  scale_fill_manual("", values = bpr_colors[1]) + 
  scale_color_manual("", values = bpr_colors[1]) +
  facet_col(attribute ~ ., scales = "free_y", space = "free") +
  scale_x_continuous(
    expand = c(0.01, 0.01)) +
  labs(x = "Probability of selecting individual",
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
  gg_mm_donate %>%
  filter(X == "No") %>% 
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
    position = position_dodgev(height = .9),
    size = 1
  ) +
  geom_point(
    size = 2.5,
    pch = 22,
    color = "white",
    fill = "black",
    position = position_dodgev(height = .9)
  ) +
  scale_fill_manual("", values = bpr_colors[1]) + 
  scale_color_manual("", values = bpr_colors[1]) +
  facet_col(attribute ~ ., scales = "free_y", space = "free") +
  scale_x_continuous(
    expand = c(0.01, 0.01)) +
  labs(x = "Probability of selecting individual",
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
  gg_mm_donate %>%
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
    position = position_dodgev(height = .9),
    size = 1
  ) +
  geom_point(
    size = 2.5,
    pch = 22,
    color = "white",
    fill = "black",
    position = position_dodgev(height = .9)
  ) +
  scale_fill_manual("", values = bpr_colors[1]) + 
  scale_color_manual("", values = bpr_colors[1]) +
  facet_col(attribute ~ ., scales = "free_y", space = "free") +
  scale_x_continuous(
    expand = c(0.01, 0.01)) +
  labs(x = "Probability of selecting individual",
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
    g1 + labs(x = "Marginal mean:\nDonated > $5") + 
      theme(strip.text.x = element_text(size = 11, color = "white"),
            panel.border = element_rect(fill = NA, colour = "black")),
    g2 + labs(x = "Marginal mean:\nDonated <= $5") +
      theme(
        axis.text.y = element_blank(),
        axis.ticks.y = element_blank(),
        axis.title.y = element_blank(),
        strip.text.x = element_text(color = "white", size = 11),
        panel.border = element_rect(fill = NA, colour = "black")
      ),
    g3 + labs(x = "Difference")  + 
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

pdf(file = "norc_conjoint_means_donate.pdf",
    width = 7.6,
    height = 3 + fixed + spacing * (nrow(g1$data)))
p 
grid::grid.text("Total cost of the agreement", 
                x = unit(0.39, "npc"), y = unit(0.98, "npc"), 
                just = "left",
                gp = gpar(fontsize = 11, col = "black", fontfamily = "serif",
                          fontface = "bold"))
grid::grid.text("Proportion of total cost paid by the U.S.", 
                x = unit(0.39, "npc"), y = unit(0.81, "npc"), 
                just = "left",
                gp = gpar(fontsize = 11, col = "black", fontfamily = "serif",
                          fontface = "bold"))
grid::grid.text("Funding used to", 
                x = unit(0.39, "npc"), y = unit(0.605, "npc"), 
                just = "left",
                gp = gpar(fontsize = 11, col = "black", fontfamily = "serif",
                          fontface = "bold"))
grid::grid.text("Benefits directed toward", 
                x = unit(0.39, "npc"), y = unit(0.40, "npc"), 
                just = "left",
                gp = gpar(fontsize = 11, col = "black", fontfamily = "serif",
                          fontface = "bold"))
grid::grid.text("Duration of agreement", 
                x = unit(0.39, "npc"), y = unit(0.255, "npc"), 
                just = "left",
                gp = gpar(fontsize = 11, col = "black", fontfamily = "serif",
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
  write_csv("norc_conjoint_means_donate.csv")

### ---- Fig. S14: AMCEs by COVAX donation indicator -------- 

gg_amce_donate_0 <- 
  lm_robust(
    conjoint_chosen ~ cost_ + prop_ + purpose_ + benefit_ + duration_,
    data = norc_conjoint_long %>% filter(X == 0),
    clusters = CaseId
  ) %>% 
  tidy() %>% 
  mutate_if(is.numeric, round, 3)  %>% 
  mutate(X = "No")


gg_amce_donate_1 <- 
  lm_robust(
    conjoint_chosen ~ cost_ + prop_ + purpose_ + benefit_ + duration_,
    data = norc_conjoint_long %>% filter(X == 1),
    clusters = CaseId
  ) %>% 
  tidy() %>% 
  mutate_if(is.numeric, round, 3)  %>% 
  mutate(X = "Yes")

gg_amce_donate_diff <- 
  lm_robust(
    conjoint_chosen ~ X*cost_ + X*prop_ + X*purpose_ + X*benefit_ + X*duration_,
    data = norc_conjoint_long,
    clusters = CaseId
  ) %>% 
  tidy() %>% 
  mutate_if(is.numeric, round, 3)  %>% 
  filter(str_detect(term, ":")) %>%
  mutate(term = str_remove(term, ".*:"),
         X = "Difference")

## Combine for plot
gg_amce_donate <- 
  bind_rows(gg_amce_donate_0, gg_amce_donate_1, gg_amce_donate_diff) %>% 
  filter(term != "(Intercept)") %>%
  mutate(
    attribute_ref = str_to_title(word(term, 1, sep = "_")),
    attribute_ref = factor(
      attribute_ref,
      levels = c("Cost", "Prop", "Purpose", "Benefit", "Duration"),
      labels = c(
        "Total cost of the agreement (reference: $25 billion)",
        "Proportion of total cost paid by the U.S. (reference: 0%)",
        "Funding used to (reference: Purchase vaccines made outside the U.S.)",
        "Benefits directed toward (reference: U.S. allies and aligned countries)",
        "Duration of agreement (reference: 1 year)"
      )
    ),
    level = word(term, 2, sep = "_"),
    level = factor(
      level,
      levels = rev(c(
        "$100 billion",
        "$75 billion",
        "$50 billion",
        "100%",
        "75%",
        "50%",
        "25%",
        "Provide economic aid and debt forgiveness",
        "Finance public health infrastructure",
        "Purchase vaccines made in the U.S.",
        "Purchase patents for vaccine production",
        "Poor and low-income countries",
        "Countries most at risk for outbreaks",
        "9 years",
        "7 years",
        "5 years",
        "3 years"
      ))
    )
  )

g1 <- 
  gg_amce_donate %>%
  filter(X == "Yes") %>%
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
  gg_amce_donate %>%
  filter(X == "No") %>%
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
  scale_x_continuous(#limits = c(-0.31, 0.31), 
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
  gg_amce_donate %>%
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

g <- 
  egg::ggarrange(
    g1 + labs(x = "AMCE: Donated > $5") + 
      theme(strip.text.x = element_text(size = 10, color = "white"),
            strip.placement = "outside",
            panel.border = element_rect(fill = NA, colour = "black")),
    g2 + labs(x = "AMCE: Donated <= $5") +
      theme(
        axis.text.y = element_blank(),
        axis.ticks.y = element_blank(),
        axis.title.y = element_blank(),
        strip.text.x = element_text(color = "white", size = 10),
        panel.border = element_rect(fill = NA, colour = "black")
      ),
    g3 + labs(x = "Difference")  + 
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

pdf(file = "norc_conjoint_amces_donate.pdf",
    width = 7.6,
    height = 4 + fixed + spacing * (nrow(g1$data)))
p 
grid::grid.text("Total cost of the agreement (reference: $25 billion)", 
                x = unit(0.39, "npc"), y = unit(0.98, "npc"), 
                just = "left",
                gp = gpar(fontsize = 11, col = "black", fontfamily = "serif",
                          fontface = "bold"))
grid::grid.text("Proportion of total cost paid by the U.S. (reference: 0%)", 
                x = unit(0.39, "npc"), y = unit(0.81, "npc"), 
                just = "left",
                gp = gpar(fontsize = 11, col = "black", fontfamily = "serif",
                          fontface = "bold"))
grid::grid.text("Funding used to (reference: Purchase vaccines made outside the U.S.)", 
                x = unit(0.39, "npc"), y = unit(0.60, "npc"), 
                just = "left",
                gp = gpar(fontsize = 11, col = "black", fontfamily = "serif",
                          fontface = "bold"))
grid::grid.text("Benefits directed toward (reference: U.S. allies and aligned countries)", 
                x = unit(0.39, "npc"), y = unit(0.38, "npc"), 
                just = "left",
                gp = gpar(fontsize = 11, col = "black", fontfamily = "serif",
                          fontface = "bold"))
grid::grid.text("Duration of agreement (reference: 1 year)", 
                x = unit(0.39, "npc"), y = unit(0.246, "npc"), 
                just = "left",
                gp = gpar(fontsize = 11, col = "black", fontfamily = "serif",
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
  write_csv("norc_conjoint_amces_donate.csv")
