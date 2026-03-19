### ---- Fix levels and labels for plotting----
amce_attr_lvls <- c("cost", "prop", "purpose", "benefit", "duration")
amce_attr_labs <- c(
  "Total cost of the agreement (reference: $25 billion)",
  "Proportion of total cost paid by the U.S. (reference: 0%)",
  "Funding used to (reference: Purchase vaccines made outside the U.S.)",
  "Benefits directed toward (reference: U.S. allies and aligned countries)",
  "Duration of agreement (reference: 1 year)"
)
amce_feat_lvls <- rev(c(
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
amce_feat_labs <- amce_feat_lvls

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

###----- Fig. S57-S58: heterogeneity by vaccine nationalism item ------
#"The US should cooperate with international efforts to provide COVID-19 
# vaccines to people in other countries"

## Split sample by median for high and low nationalism
cut <- median(norc_conjoint_long$x_vaxnat_n, na.rm = T)
norc_conjoint_long$vaxnat_binary_n <- 
  as.numeric(norc_conjoint_long$x_vaxnat_n > cut)

## Marginal means
gg_nconjoint_mm_vaxnat <- 
  het_mm_a(x = "vaxnat_binary_n", long_data = norc_conjoint_long,
           cluster = "CaseId",
           attr_lvls = mm_attr_lvls, attr_labs = mm_attr_labs,
           feat_lvls = mm_feat_lvls, feat_labs = mm_feat_labs,
           var_list = mm_var_list)

## AMCEs
gg_nconjoint_amce_vaxnat <- 
  het_amce_a(x = "vaxnat_binary_n", long_data = norc_conjoint_long,
             formula =  conjoint_chosen ~ cost_ + prop_ + purpose_ + benefit_ + 
               duration_,
             cluster = "CaseId",
             attr_lvls = amce_attr_lvls, attr_labs = amce_attr_labs,
             feat_lvls = amce_feat_lvls, feat_labs = amce_feat_labs)

g1 <- 
  gg_nconjoint_mm_vaxnat %>%
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
  scale_x_continuous(limits = c(0.33, 0.67), expand = c(0.01, 0.01)) +
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
  gg_nconjoint_mm_vaxnat %>%
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
    size = 1,
    position = position_dodgev(height = .5)
  ) +
  geom_point(
    size = 2.5,
    pch = 21,
    color = "white",
    fill = "black",
    position = position_dodgev(height = .5)
  ) +
  facet_col(attribute ~ ., scales = "free_y", space = "free") +
  scale_x_continuous(limits = c(0.33, 0.67), expand = c(0.01, 0.01)) +
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
  gg_nconjoint_mm_vaxnat %>%
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
  scale_x_continuous(limits = c(-0.16, 0.16), expand = c(0.01, 0.01)) +
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
    g1 + labs(x = "Marginal mean: high\nvaccine nationalism") + 
      theme(strip.text.x = element_text(color = "white", size = 11),
            panel.border = element_rect(fill = NA, colour = "black")),
    g2 + labs(x = "Marginal mean: low\nvaccine nationalism") +
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

g

## Now convert grob into plot and add text ...
p <- ggplotify::as.ggplot(g)

pdf(file = "norc_conjoint_means_vaxnat.pdf",
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
  write_csv("norc_conjoint_means_vaxnat.csv")


g1 <- 
  gg_nconjoint_amce_vaxnat %>%
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
  scale_x_continuous(limits = c(-0.3, 0.3), 
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
  gg_nconjoint_amce_vaxnat %>%
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
    pch = 21,
    color = "white",
    fill = "black",
    position = position_dodgev(height = .5)
  ) +
  facet_col(attribute_ref ~ ., scales = "free_y", space = "free") +
  scale_x_continuous(limits = c(-0.3, 0.3), 
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
  gg_nconjoint_amce_vaxnat %>%
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
  scale_x_continuous(limits = c(-0.3, 0.3), 
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
    g1 + labs(x = "AMCE: high vaccine\nnationalism") + 
      theme(strip.text.x = element_text(size = 10, color = "white"),
            strip.placement = "outside",
            panel.border = element_rect(fill = NA, colour = "black")),
    g2 + labs(x = "AMCE: low vaccine\nnationalism") +
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

g

## Now convert grob into plot and add text ...
p <- ggplotify::as.ggplot(g)

pdf(file = "norc_conjoint_amces_vaxnat.pdf",
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
                x = unit(0.39, "npc"), y = unit(0.39, "npc"), 
                just = "left",
                gp = gpar(fontsize = 11, col = "black", fontfamily = "serif",
                          fontface = "bold"))
grid::grid.text("Duration of agreement (reference: 1 year)", 
                x = unit(0.39, "npc"), y = unit(0.255, "npc"), 
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
  write_csv("norc_conjoint_amces_vaxnat.csv")


###----- Fig. S59-S60: heterogeneity by nationalism item  ------

#"I would rather be a citizen of the US than of any other country in the world."

## Split sample by median for high and low nationalism
cut <- median(norc_conjoint_long$x_patnat1_n, na.rm = T)
norc_conjoint_long$national_binary_n <- 
  as.numeric(norc_conjoint_long$x_patnat1_n > cut)

## Marginal means
gg_nconjoint_mm_national <- 
  het_mm_a(x = "national_binary_n", long_data = norc_conjoint_long,
           cluster = "CaseId",
           attr_lvls = mm_attr_lvls, attr_labs = mm_attr_labs,
           feat_lvls = mm_feat_lvls, feat_labs = mm_feat_labs,
           var_list = mm_var_list)

## AMCEs
gg_nconjoint_amce_national <- 
  het_amce_a(x = "national_binary_n", long_data = norc_conjoint_long,
             formula =  conjoint_chosen ~ cost_ + prop_ + purpose_ + benefit_ + 
               duration_,
             cluster = "CaseId",
             attr_lvls = amce_attr_lvls, attr_labs = amce_attr_labs,
             feat_lvls = amce_feat_lvls, feat_labs = amce_feat_labs)

g1 <- 
  gg_nconjoint_mm_national %>%
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
  scale_x_continuous(limits = c(0.33, 0.67), expand = c(0.01, 0.01)) +
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
  gg_nconjoint_mm_national %>%
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
    size = 1,
    position = position_dodgev(height = .5)
  ) +
  geom_point(
    size = 2.5,
    pch = 21,
    color = "white",
    fill = "black",
    position = position_dodgev(height = .5)
  ) +
  facet_col(attribute ~ ., scales = "free_y", space = "free") +
  scale_x_continuous(limits = c(0.33, 0.67), expand = c(0.01, 0.01)) +
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
  gg_nconjoint_mm_national %>%
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
  scale_x_continuous(limits = c(-0.16, 0.16), expand = c(0.01, 0.01)) +
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
    g1 + labs(x = "Marginal mean: high\nnationalism") + 
      theme(strip.text.x = element_text(color = "white", size = 11),
            panel.border = element_rect(fill = NA, colour = "black")),
    g2 + labs(x = "Marginal mean: low\nnationalism") +
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

g

## Now convert grob into plot and add text ...
p <- ggplotify::as.ggplot(g)

pdf(file = "norc_conjoint_means_national.pdf",
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
  write_csv("norc_conjoint_means_national.csv")


g1 <- 
  gg_nconjoint_amce_national %>%
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
  scale_x_continuous(limits = c(-0.25, 0.25), expand = c(0.01, 0.01)) +
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
  gg_nconjoint_amce_national %>%
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
    pch = 21,
    color = "white",
    fill = "black",
    position = position_dodgev(height = .5)
  ) +
  facet_col(attribute_ref ~ ., scales = "free_y", space = "free") +
  scale_x_continuous(limits = c(-0.25, 0.25), expand = c(0.01, 0.01)) +
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
  gg_nconjoint_amce_national %>%
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
  scale_x_continuous(limits = c(-0.25, 0.25), expand = c(0.01, 0.01)) +
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
    g1 + labs(x = "AMCE: high nationalism") + 
      theme(strip.text.x = element_text(size = 10, color = "white"),
            #strip.text.x = element_blank(),
            strip.placement = "outside",
            panel.border = element_rect(fill = NA, colour = "black")),
    g2 + labs(x = "AMCE: low nationalism") +
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

g

## Now convert grob into plot and add text ...
p <- ggplotify::as.ggplot(g)

pdf(file = "norc_conjoint_amces_national.pdf",
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
  write_csv("norc_conjoint_amces_national.csv")



### ---- Fig. S61-S62: heterogeneity by patriotism ----

## Split sample by median for high and low patriotism
cut <- median(norc_conjoint_long$x_patnat2_n, na.rm = T)
norc_conjoint_long$patriot_binary_n <- 
  as.numeric(norc_conjoint_long$x_patnat2_n > cut)

## Marginal means
gg_nconjoint_mm_patriot <- 
  het_mm_a(x = "patriot_binary_n", long_data = norc_conjoint_long,
           cluster = "CaseId",
           attr_lvls = mm_attr_lvls, attr_labs = mm_attr_labs,
           feat_lvls = mm_feat_lvls, feat_labs = mm_feat_labs,
           var_list = mm_var_list) 

## AMCEs
gg_nconjoint_amce_patriot <- 
  het_amce_a(x = "patriot_binary_n", long_data = norc_conjoint_long,
             formula =  conjoint_chosen ~ cost_ + prop_ + purpose_ + benefit_ + 
               duration_,
             cluster = "CaseId",
             attr_lvls = amce_attr_lvls, attr_labs = amce_attr_labs,
             feat_lvls = amce_feat_lvls, feat_labs = amce_feat_labs)

g1 <- 
  gg_nconjoint_mm_patriot %>%
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
  scale_x_continuous(limits = c(0.33, 0.67), expand = c(0.01, 0.01)) +
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
  gg_nconjoint_mm_patriot %>%
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
    size = 1,
    position = position_dodgev(height = .5)
  ) +
  geom_point(
    size = 2.5,
    pch = 21,
    color = "white",
    fill = "black",
    position = position_dodgev(height = .5)
  ) +
  facet_col(attribute ~ ., scales = "free_y", space = "free") +
  scale_x_continuous(limits = c(0.33, 0.67), expand = c(0.01, 0.01)) +
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
  gg_nconjoint_mm_patriot %>%
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
  scale_x_continuous(limits = c(-0.16, 0.16), expand = c(0.01, 0.01)) +
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
    g1 + labs(x = "Marginal mean: high\npatriotism") + 
      theme(strip.text.x = element_text(color = "white", size = 11),
            panel.border = element_rect(fill = NA, colour = "black")),
    g2 + labs(x = "Marginal mean: low\npatriotism") +
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

pdf(file = "norc_conjoint_means_patriot.pdf",
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
  write_csv("norc_conjoint_means_patriot.csv")

g1 <- 
  gg_nconjoint_amce_patriot %>%
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
  #ggh4x::facet_nested(attribute_ref ~ ., scales = "free_y", space = "free")
  facet_col(attribute_ref ~ ., scales = "free_y", space = "free") +
  scale_x_continuous(limits = c(-0.25, 0.25), expand = c(0.01, 0.01)) +
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
  gg_nconjoint_amce_patriot %>%
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
    pch = 21,
    color = "white",
    fill = "black",
    position = position_dodgev(height = .5)
  ) +
  facet_col(attribute_ref ~ ., scales = "free_y", space = "free") +
  scale_x_continuous(limits = c(-0.25, 0.25), expand = c(0.01, 0.01)) +
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
  gg_nconjoint_amce_patriot %>%
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
  scale_x_continuous(limits = c(-0.25, 0.25), expand = c(0.01, 0.01)) +
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
    g1 + labs(x = "AMCE: high patriotism") + 
      theme(strip.text.x = element_text(size = 10, color = "white"),
            #strip.text.x = element_blank(),
            strip.placement = "outside",
            panel.border = element_rect(fill = NA, colour = "black")),
    g2 + labs(x = "AMCE: low patriotism") +
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

pdf(file = "norc_conjoint_amces_patriot.pdf",
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
  write_csv("norc_conjoint_amces_patriot.csv")


### ---- Fig. S63-S64: heterogeneity by altruism ------

## Split sample by median for high and low altruism
cut <- median(norc_conjoint_long$x_altruism_n, na.rm = T)
norc_conjoint_long$altruism_binary_n <-
  as.numeric(norc_conjoint_long$x_altruism_n > cut)

## Marginal means
gg_nconjoint_mm_altruism <-
  het_mm_a(x = "altruism_binary_n", long_data = norc_conjoint_long,
           cluster = "CaseId",
           attr_lvls = mm_attr_lvls, attr_labs = mm_attr_labs,
           feat_lvls = mm_feat_lvls, feat_labs = mm_feat_labs,
           var_list = mm_var_list)

## AMCEs
gg_nconjoint_amce_altruism <-
  het_amce_a(x = "altruism_binary_n", long_data = norc_conjoint_long,
             formula =  conjoint_chosen ~ cost_ + prop_ + purpose_ + benefit_ + 
               duration_,
             cluster = "CaseId",
             attr_lvls = amce_attr_lvls, attr_labs = amce_attr_labs,
             feat_lvls = amce_feat_lvls, feat_labs = amce_feat_labs)

g1 <- 
  gg_nconjoint_mm_altruism %>%
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
  scale_x_continuous(limits = c(0.33, 0.67), expand = c(0.01, 0.01)) +
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
  gg_nconjoint_mm_altruism %>%
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
    size = 1,
    position = position_dodgev(height = .5)
  ) +
  geom_point(
    size = 2.5,
    pch = 21,
    color = "white",
    fill = "black",
    position = position_dodgev(height = .5)
  ) +
  facet_col(attribute ~ ., scales = "free_y", space = "free") +
  scale_x_continuous(limits = c(0.33, 0.67), expand = c(0.01, 0.01)) +
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
  gg_nconjoint_mm_altruism %>%
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
  scale_x_continuous(limits = c(-0.16, 0.16), expand = c(0.01, 0.01)) +
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
    g1 + labs(x = "Marginal mean: high\naltruism") + 
      theme(strip.text.x = element_text(size = 11, color = "white"),
            panel.border = element_rect(fill = NA, colour = "black")),
    g2 + labs(x = "Marginal mean: low\naltruism") +
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

g


## Now convert grob into plot and add text ...
p <- ggplotify::as.ggplot(g)

pdf(file = "norc_conjoint_means_altruism.pdf",
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
  write_csv("norc_conjoint_means_altruism.csv")

g1 <- 
  gg_nconjoint_amce_altruism %>%
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
  scale_x_continuous(limits = c(-0.25, 0.25), expand = c(0.01, 0.01)) +
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
  gg_nconjoint_amce_altruism %>%
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
    pch = 21,
    color = "white",
    fill = "black",
    position = position_dodgev(height = .5)
  ) +
  facet_col(attribute_ref ~ ., scales = "free_y", space = "free") +
  scale_x_continuous(limits = c(-0.25, 0.25), expand = c(0.01, 0.01)) +
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
  gg_nconjoint_amce_altruism %>%
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
  scale_x_continuous(limits = c(-0.25, 0.25), expand = c(0.01, 0.01)) +
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
    g1 + labs(x = "AMCE: high altruism") + 
      theme(strip.text.x = element_text(size = 10, color = "white"),
            #strip.text.x = element_blank(),
            strip.placement = "outside",
            panel.border = element_rect(fill = NA, colour = "black")),
    g2 + labs(x = "AMCE: low altruism") +
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

g

## Now convert grob into plot and add text ...
p <- ggplotify::as.ggplot(g)

pdf(file = "norc_conjoint_amces_altruism.pdf",
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
  write_csv("norc_conjoint_amces_altruism.csv")


### ---- Fig. S65-S66: heterogeneity by partisanship ------
tmp <- 
  norc_conjoint_long %>%
  mutate(democrat = case_when(x_pid3 == "Democrat" ~ 1,
                              x_pid3 == "Republican" ~ 0)) %>% 
  filter(!is.na(democrat))

## Marginal means
gg_nconjoint_mm_pid <-
  het_mm_a(x = "democrat", long_data = tmp,            
           cluster = "CaseId",
           attr_lvls = mm_attr_lvls, attr_labs = mm_attr_labs,
           feat_lvls = mm_feat_lvls, feat_labs = mm_feat_labs,
           var_list = mm_var_list)

## AMCEs
gg_nconjoint_amce_pid <-
  het_amce_a(x = "democrat", long_data = tmp,
             formula =  conjoint_chosen ~ cost_ + prop_ + purpose_ + benefit_ + 
               duration_,
             cluster = "CaseId",
             attr_lvls = amce_attr_lvls, attr_labs = amce_attr_labs,
             feat_lvls = amce_feat_lvls, feat_labs = amce_feat_labs)

g1 <- 
  gg_nconjoint_mm_pid %>%
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
  scale_x_continuous(limits = c(0.31, 0.65), expand = c(0.01, 0.01)) +
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
  gg_nconjoint_mm_pid %>%
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
  scale_x_continuous(limits = c(0.31, 0.65), expand = c(0.01, 0.01)) +
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
  gg_nconjoint_mm_pid %>%
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
  scale_x_continuous(limits = c(-0.16, 0.16), expand = c(0.01, 0.01)) +
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
    g1 + labs(x = "Marginal mean:\nDemocrats") + 
      theme(strip.text.x = element_text(size = 11, color = "white"),
            panel.border = element_rect(fill = NA, colour = "black")),
    g2 + labs(x = "Marginal mean:\nRepublicans") +
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

g

## Now convert grob into plot and add text ...
p <- ggplotify::as.ggplot(g)

pdf(file = "norc_conjoint_means_pid.pdf", 
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
  write_csv("norc_conjoint_means_pid.csv")


g1 <- 
  gg_nconjoint_amce_pid %>%
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
  scale_x_continuous(limits = c(-0.31, 0.31), expand = c(0.01, 0.01)) +
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
  gg_nconjoint_amce_pid %>%
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
    pch = 21,
    color = "white",
    fill = "black",
    position = position_dodgev(height = .5)
  ) +
  facet_col(attribute_ref ~ ., scales = "free_y", space = "free") +
  scale_x_continuous(limits = c(-0.31, 0.31), expand = c(0.01, 0.01)) +
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
  gg_nconjoint_amce_pid %>%
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
  scale_x_continuous(limits = c(-0.25, 0.25), expand = c(0.01, 0.01)) +
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
    g1 + labs(x = "AMCE: Democrats") + 
      theme(strip.text.x = element_text(size = 10, color = "white"),
            #strip.text.x = element_blank(),
            strip.placement = "outside",
            panel.border = element_rect(fill = NA, colour = "black")),
    g2 + labs(x = "AMCE: Republicans") +
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

pdf(file = "norc_conjoint_amces_pid.pdf", 
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
  write_csv("norc_conjoint_amces_pid.csv")

### ---- Fig. S67-S68: heterogeneity by ideology ------
tmp <- 
  norc_conjoint_long %>%
  mutate(liberal = case_when(x_ideo_7n < 4  ~ 1,
                             x_ideo_7n > 4  ~ 0)) %>% 
  filter(!is.na(liberal))

## Marginal means
gg_nconjoint_mm_ideo <-
  het_mm_a(x = "liberal", long_data = tmp,
           cluster = "CaseId",
           attr_lvls = mm_attr_lvls, attr_labs = mm_attr_labs,
           feat_lvls = mm_feat_lvls, feat_labs = mm_feat_labs,
           var_list = mm_var_list)

## AMCEs
gg_nconjoint_amce_ideo <-
  het_amce_a(x = "liberal", long_data = tmp, 
             formula =  conjoint_chosen ~ cost_ + prop_ + purpose_ + benefit_ + 
               duration_,
             cluster = "CaseId",
             attr_lvls = amce_attr_lvls, attr_labs = amce_attr_labs,
             feat_lvls = amce_feat_lvls, feat_labs = amce_feat_labs)

g1 <- 
  gg_nconjoint_mm_ideo %>%
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
  scale_x_continuous(limits = c(0.32, 0.67), expand = c(0.01, 0.01)) +
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
  gg_nconjoint_mm_ideo %>%
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
  scale_x_continuous(limits = c(0.32, 0.67), expand = c(0.01, 0.01)) +
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
  gg_nconjoint_mm_ideo %>%
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
  scale_x_continuous(limits = c(-0.165, 0.165), expand = c(0.01, 0.01)) +
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
    g1 + labs(x = "Marginal mean:\nLiberals") + 
      theme(strip.text.x = element_text(size = 11, color = "white"),
            panel.border = element_rect(fill = NA, colour = "black")),
    g2 + labs(x = "Marginal mean:\nConservatives") +
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

pdf(file = "norc_conjoint_means_ideo.pdf", 
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
  write_csv("norc_conjoint_means_ideo.csv")

g1 <- 
  gg_nconjoint_amce_ideo %>%
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
  scale_x_continuous(limits = c(-0.32, 0.32), expand = c(0.01, 0.01)) +
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
  gg_nconjoint_amce_ideo %>%
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
    pch = 21,
    color = "white",
    fill = "black",
    position = position_dodgev(height = .5)
  ) +
  facet_col(attribute_ref ~ ., scales = "free_y", space = "free") +
  scale_x_continuous(limits = c(-0.32, 0.32), expand = c(0.01, 0.01)) +
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
  gg_nconjoint_amce_ideo %>%
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
  scale_x_continuous(limits = c(-0.265, 0.265), expand = c(0.01, 0.01)) +
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
    g1 + labs(x = "AMCE: Liberals") + 
      theme(strip.text.x = element_text(size = 10, color = "white"),
            #strip.text.x = element_blank(),
            strip.placement = "outside",
            panel.border = element_rect(fill = NA, colour = "black")),
    g2 + labs(x = "AMCE: Conservatives") +
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

pdf(file = "norc_conjoint_amces_ideo.pdf", 
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
  write_csv("norc_conjoint_amces_ideo.csv")