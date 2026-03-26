### ---- Fig. S39-S40: heterogeneity by vaccine nationalism -------
## Marginal means
gg_aconjoint_mm_vnat <- 
  het_mm_a(x = "vnat_binary_n", long_data = lucid_conjoint_a_long,
           cluster = "admin_ResponseId")

## AMCEs
gg_aconjoint_amce_vnat <-
  het_amce_a(
    formula = conjoint_chosen ~ price_ + participants_ + costs_ + benefits_ +
      supply_ + sharing_ + monitoring_,
    long_data = lucid_conjoint_a_long,
    cluster = "admin_ResponseId",
    x = "vnat_binary_n"
  )


g1 <- 
  gg_aconjoint_mm_vnat %>%
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
  scale_x_continuous(limits = c(0.35, 0.65), expand = c(0.01, 0.01)) +
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
  gg_aconjoint_mm_vnat %>%
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
  scale_x_continuous(limits = c(0.35, 0.65), expand = c(0.01, 0.01)) +
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
  gg_aconjoint_mm_vnat %>%
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
    g1 + labs(x = "Marginal mean:\nvaccine nationlists") + 
      theme(strip.text.x = element_text(size = 11, color = "white"),
            panel.border = element_rect(fill = NA, colour = "black")),
    g2 + labs(x = "Marginal mean:\nnon-nationalists") +
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

pdf(file = "lucid_conjoint_a_means_vnat.pdf",
    width = 7.6,
    height = 3 + fixed + spacing * (nrow(g1$data)))
p 
grid::grid.text("Costs to average household", 
                x = unit(0.39, "npc"), y = unit(0.977, "npc"), 
                just = "left",
                gp = gpar(fontsize = 11, col = "black", fontfamily = "serif",
                          fontface = "bold"))
grid::grid.text("Number of participating countries", 
                x = unit(0.39, "npc"), y = unit(0.81, "npc"), 
                just = "left",
                gp = gpar(fontsize = 11, col = "black", fontfamily = "serif",
                          fontface = "bold"))
grid::grid.text("Distribution of benefits", 
                x = unit(0.39, "npc"), y = unit(0.67, "npc"), 
                just = "left",
                gp = gpar(fontsize = 11, col = "black", fontfamily = "serif",
                          fontface = "bold"))
grid::grid.text("Distribution of costs", 
                x = unit(0.39, "npc"), y = unit(0.505, "npc"), 
                just = "left",
                gp = gpar(fontsize = 11, col = "black", fontfamily = "serif",
                          fontface = "bold"))
grid::grid.text("Monitoring for non-compliance", 
                x = unit(0.39, "npc"), y = unit(0.385, "npc"), 
                just = "left",
                gp = gpar(fontsize = 11, col = "black", fontfamily = "serif",
                          fontface = "bold"))
grid::grid.text("Sharing of vaccine technology", 
                x = unit(0.39, "npc"), y = unit(0.24, "npc"), 
                just = "left",
                gp = gpar(fontsize = 11, col = "black", fontfamily = "serif",
                          fontface = "bold"))
grid::grid.text("External supply agreements allowed", 
                x = unit(0.39, "npc"), y = unit(0.148, "npc"), 
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
  write_csv("lucid_conjoint_a_means_vnat.csv")


g1 <- 
  gg_aconjoint_amce_vnat %>%
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
  gg_aconjoint_amce_vnat %>%
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
  gg_aconjoint_amce_vnat %>%
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
    g1 + labs(x = "AMCE: vaccine nationlists") + 
      theme(strip.text.x = element_text(size = 10, color = "white"),
            #strip.text.x = element_blank(),
            strip.placement = "outside",
            panel.border = element_rect(fill = NA, colour = "black")),
    g2 + labs(x = "AMCE: non-nationalists") +
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

pdf("lucid_conjoint_a_amces_vnat.pdf",
    width = 7.6,
    height = 4 + fixed + spacing * (nrow(g1$data)))
p 
grid::grid.text("Costs to average household (reference: $1)", 
                x = unit(0.37, "npc"), y = unit(0.985, "npc"), 
                just = "left",
                gp = gpar(fontsize = 11, col = "black", fontfamily = "serif",
                          fontface = "bold"))
grid::grid.text("Number of participating countries (reference: 20 of 192)", 
                x = unit(0.37, "npc"), y = unit(0.798, "npc"), 
                just = "left",
                gp = gpar(fontsize = 11, col = "black", fontfamily = "serif",
                          fontface = "bold"))
grid::grid.text("Distribution of benefits (reference: Only poor countries benefit)", 
                x = unit(0.37, "npc"), y = unit(0.645, "npc"), 
                just = "left",
                gp = gpar(fontsize = 11, col = "black", fontfamily = "serif",
                          fontface = "bold"))
grid::grid.text("Distribution of costs (reference: Only rich countries contribute)", 
                x = unit(0.37, "npc"), y = unit(0.464, "npc"), 
                just = "left",
                gp = gpar(fontsize = 11, col = "black", fontfamily = "serif",
                          fontface = "bold"))
grid::grid.text("Monitoring for non-compliance (reference: U.S. government)", 
                x = unit(0.37, "npc"), y = unit(0.346, "npc"), 
                just = "left",
                gp = gpar(fontsize = 11, col = "black", fontfamily = "serif",
                          fontface = "bold"))
grid::grid.text("Sharing of vaccine technology (reference: Compulsory)", 
                x = unit(0.37, "npc"), y = unit(0.20, "npc"), 
                just = "left",
                gp = gpar(fontsize = 11, col = "black", fontfamily = "serif",
                          fontface = "bold"))
grid::grid.text("External supply agreements allowed (reference: No)", 
                x = unit(0.37, "npc"), y = unit(0.115, "npc"), 
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
  write_csv("lucid_conjoint_a_amces_vnat.csv")


###----- Fig. S41-S42: heterogeneity by vaccine nationalism index ------
tmp <-
  lucid_conjoint_a_long %>%
  filter_at(vars(vnat_1_n:vnat_5_n), all_vars(complete.cases(.))) %>%
  mutate(national_idx = stdidx::idx_invcov(vnat_1_n, vnat_2_n, vnat_3_n,
                                           vnat_4_n, vnat_5_n))

## Split sample by median for high and low nationalism
cut <- median(tmp$national_idx)
tmp$vnat_idx_binary_n <-
  as.numeric(tmp$national_idx >= cut)

## Marginal means
gg_aconjoint_mm_vnat_idx <-
  het_mm_a(x = "vnat_idx_binary_n", long_data = tmp,
           cluster = "admin_ResponseId")

## AMCEs
gg_aconjoint_amce_vnat_idx <-
  het_amce_a(x = "vnat_idx_binary_n", long_data = tmp,
             formula = conjoint_chosen ~ price_ + participants_ + costs_ + benefits_ +
               supply_ + sharing_ + monitoring_,
             cluster = "admin_ResponseId")

g1 <-
  gg_aconjoint_mm_vnat_idx %>%
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
  scale_x_continuous(limits = c(0.35, 0.66), expand = c(0.01, 0.01)) +
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
  gg_aconjoint_mm_vnat_idx %>%
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
  scale_x_continuous(limits = c(0.35, 0.65), expand = c(0.01, 0.01)) +
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
  gg_aconjoint_mm_vnat_idx %>%
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
      theme(
        strip.text.x = element_text(color = "white", size = 11),
        panel.border = element_rect(fill = NA, colour = "black")
      ),
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


## Now convert grob into plot and add text ...
p <- ggplotify::as.ggplot(g)

pdf(file = "lucid_conjoint_a_means_vnat_idx.pdf",
    width = 7.6,
    height = 3 + fixed + spacing * (nrow(g1$data)))
p 
grid::grid.text("Costs to average household", 
                x = unit(0.39, "npc"), y = unit(0.977, "npc"), 
                just = "left",
                gp = gpar(fontsize = 11, col = "black", fontfamily = "serif",
                          fontface = "bold"))
grid::grid.text("Number of participating countries", 
                x = unit(0.39, "npc"), y = unit(0.81, "npc"), 
                just = "left",
                gp = gpar(fontsize = 11, col = "black", fontfamily = "serif",
                          fontface = "bold"))
grid::grid.text("Distribution of benefits", 
                x = unit(0.39, "npc"), y = unit(0.67, "npc"), 
                just = "left",
                gp = gpar(fontsize = 11, col = "black", fontfamily = "serif",
                          fontface = "bold"))
grid::grid.text("Distribution of costs", 
                x = unit(0.39, "npc"), y = unit(0.505, "npc"), 
                just = "left",
                gp = gpar(fontsize = 11, col = "black", fontfamily = "serif",
                          fontface = "bold"))
grid::grid.text("Monitoring for non-compliance", 
                x = unit(0.39, "npc"), y = unit(0.385, "npc"), 
                just = "left",
                gp = gpar(fontsize = 11, col = "black", fontfamily = "serif",
                          fontface = "bold"))
grid::grid.text("Sharing of vaccine technology", 
                x = unit(0.39, "npc"), y = unit(0.24, "npc"), 
                just = "left",
                gp = gpar(fontsize = 11, col = "black", fontfamily = "serif",
                          fontface = "bold"))
grid::grid.text("External supply agreements allowed", 
                x = unit(0.39, "npc"), y = unit(0.148, "npc"), 
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
  write_csv("lucid_conjoint_a_means_vnat_idx.csv")

g1 <-
  gg_aconjoint_amce_vnat_idx %>%
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
  scale_x_continuous(limits = c(-0.26, 0.25), expand = c(0.01, 0.01)) +
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
  gg_aconjoint_amce_vnat_idx %>%
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
  scale_x_continuous(limits = c(-0.26, 0.25), expand = c(0.01, 0.01)) +
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
  gg_aconjoint_amce_vnat_idx %>%
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
    g1 + labs(x = "AMCE: high\nvaccine nationlism") +
      theme(
        strip.text.x = element_text(size = 10, color = "white"),
        #strip.text.x = element_blank(),
        strip.placement = "outside",
        panel.border = element_rect(fill = NA, colour = "black")
      ),
    g2 + labs(x = "AMCE: low\nvaccine nationlism") +
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

pdf(
  file = "lucid_conjoint_a_amces_vnat_idx.pdf",
  width = 7.6,
  height = 4 + fixed + spacing * (nrow(g1$data))
)
p
grid::grid.text("Costs to average household (reference: $1)", 
                x = unit(0.37, "npc"), y = unit(0.979, "npc"), 
                just = "left",
                gp = gpar(fontsize = 11, col = "black", fontfamily = "serif",
                          fontface = "bold"))
grid::grid.text("Number of participating countries (reference: 20 of 192)", 
                x = unit(0.37, "npc"), y = unit(0.799, "npc"), 
                just = "left",
                gp = gpar(fontsize = 11, col = "black", fontfamily = "serif",
                          fontface = "bold"))
grid::grid.text("Distribution of benefits (reference: Only poor countries benefit)", 
                x = unit(0.37, "npc"), y = unit(0.656, "npc"), 
                just = "left",
                gp = gpar(fontsize = 11, col = "black", fontfamily = "serif",
                          fontface = "bold"))
grid::grid.text("Distribution of costs (reference: Only rich countries contribute)", 
                x = unit(0.37, "npc"), y = unit(0.475, "npc"), 
                just = "left",
                gp = gpar(fontsize = 11, col = "black", fontfamily = "serif",
                          fontface = "bold"))
grid::grid.text("Monitoring for non-compliance (reference: U.S. government)", 
                x = unit(0.37, "npc"), y = unit(0.358, "npc"), 
                just = "left",
                gp = gpar(fontsize = 11, col = "black", fontfamily = "serif",
                          fontface = "bold"))
grid::grid.text("Sharing of vaccine technology (reference: Compulsory)", 
                x = unit(0.37, "npc"), y = unit(0.215, "npc"), 
                just = "left",
                gp = gpar(fontsize = 11, col = "black", fontfamily = "serif",
                          fontface = "bold"))
grid::grid.text("External supply agreements allowed (reference: No)", 
                x = unit(0.37, "npc"), y = unit(0.13, "npc"), 
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
  write_csv("lucid_conjoint_a_amces_vnat_idx.csv")

###----- Fig. S43-S44: heterogeneity by nationalism item  ------

## Split sample by median for high and low nationalism
cut <- median(lucid_conjoint_a_long$national_n, na.rm = T)
lucid_conjoint_a_long$national_binary_n <- 
  as.numeric(lucid_conjoint_a_long$national_n >= cut)

## Marginal means
gg_aconjoint_mm_national <- 
  het_mm_a(x = "national_binary_n", long_data = lucid_conjoint_a_long,
           cluster = "admin_ResponseId")

## AMCEs
gg_aconjoint_amce_national <- 
  het_amce_a(x = "national_binary_n", long_data = lucid_conjoint_a_long,
             formula = conjoint_chosen ~ price_ + participants_ + costs_ + benefits_ +
               supply_ + sharing_ + monitoring_,
             cluster = "admin_ResponseId")


g1 <- 
  gg_aconjoint_mm_national %>%
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
  gg_aconjoint_mm_national %>%
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
  gg_aconjoint_mm_national %>%
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

pdf(file = "lucid_conjoint_a_means_national.pdf",
    width = 7.6,
    height = 3 + fixed + spacing * (nrow(g1$data)))
p 
grid::grid.text("Costs to average household", 
                x = unit(0.39, "npc"), y = unit(0.977, "npc"), 
                just = "left",
                gp = gpar(fontsize = 11, col = "black", fontfamily = "serif",
                          fontface = "bold"))
grid::grid.text("Number of participating countries", 
                x = unit(0.39, "npc"), y = unit(0.81, "npc"), 
                just = "left",
                gp = gpar(fontsize = 11, col = "black", fontfamily = "serif",
                          fontface = "bold"))
grid::grid.text("Distribution of benefits", 
                x = unit(0.39, "npc"), y = unit(0.67, "npc"), 
                just = "left",
                gp = gpar(fontsize = 11, col = "black", fontfamily = "serif",
                          fontface = "bold"))
grid::grid.text("Distribution of costs", 
                x = unit(0.39, "npc"), y = unit(0.505, "npc"), 
                just = "left",
                gp = gpar(fontsize = 11, col = "black", fontfamily = "serif",
                          fontface = "bold"))
grid::grid.text("Monitoring for non-compliance", 
                x = unit(0.39, "npc"), y = unit(0.385, "npc"), 
                just = "left",
                gp = gpar(fontsize = 11, col = "black", fontfamily = "serif",
                          fontface = "bold"))
grid::grid.text("Sharing of vaccine technology", 
                x = unit(0.39, "npc"), y = unit(0.24, "npc"), 
                just = "left",
                gp = gpar(fontsize = 11, col = "black", fontfamily = "serif",
                          fontface = "bold"))
grid::grid.text("External supply agreements allowed", 
                x = unit(0.39, "npc"), y = unit(0.148, "npc"), 
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
  write_csv("lucid_conjoint_a_means_national.csv")


g1 <- 
  gg_aconjoint_amce_national %>%
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
  gg_aconjoint_amce_national %>%
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
  gg_aconjoint_amce_national %>%
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

pdf(file = "lucid_conjoint_a_amces_national.pdf",
    width = 7.6,
    height = 4 + fixed + spacing * (nrow(g1$data)))
p 
grid::grid.text("Costs to average household (reference: $1)", 
                x = unit(0.37, "npc"), y = unit(0.985, "npc"), 
                just = "left",
                gp = gpar(fontsize = 11, col = "black", fontfamily = "serif",
                          fontface = "bold"))
grid::grid.text("Number of participating countries (reference: 20 of 192)", 
                x = unit(0.37, "npc"), y = unit(0.798, "npc"), 
                just = "left",
                gp = gpar(fontsize = 11, col = "black", fontfamily = "serif",
                          fontface = "bold"))
grid::grid.text("Distribution of benefits (reference: Only poor countries benefit)", 
                x = unit(0.37, "npc"), y = unit(0.645, "npc"), 
                just = "left",
                gp = gpar(fontsize = 11, col = "black", fontfamily = "serif",
                          fontface = "bold"))
grid::grid.text("Distribution of costs (reference: Only rich countries contribute)", 
                x = unit(0.37, "npc"), y = unit(0.464, "npc"), 
                just = "left",
                gp = gpar(fontsize = 11, col = "black", fontfamily = "serif",
                          fontface = "bold"))
grid::grid.text("Monitoring for non-compliance (reference: U.S. government)", 
                x = unit(0.37, "npc"), y = unit(0.346, "npc"), 
                just = "left",
                gp = gpar(fontsize = 11, col = "black", fontfamily = "serif",
                          fontface = "bold"))
grid::grid.text("Sharing of vaccine technology (reference: Compulsory)", 
                x = unit(0.37, "npc"), y = unit(0.20, "npc"), 
                just = "left",
                gp = gpar(fontsize = 11, col = "black", fontfamily = "serif",
                          fontface = "bold"))
grid::grid.text("External supply agreements allowed (reference: No)", 
                x = unit(0.37, "npc"), y = unit(0.115, "npc"), 
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
  write_csv("lucid_conjoint_a_amces_national.csv")

### ---- Fig. S45-S46: heterogeneity by cosmopolitanism ----

## Split sample by median for high and low cosmop
cut <- median(lucid_conjoint_a_long$cosmop_n, na.rm = T)
lucid_conjoint_a_long$cosmop_binary_n <- 
  as.numeric(lucid_conjoint_a_long$cosmop_n > cut)

## Marginal means
gg_aconjoint_mm_cosmop <- 
  het_mm_a(x = "cosmop_binary_n", long_data = lucid_conjoint_a_long,
           cluster = "admin_ResponseId") 

## AMCEs
gg_aconjoint_amce_cosmop <- 
  het_amce_a(x = "cosmop_binary_n", long_data = lucid_conjoint_a_long,
             formula = conjoint_chosen ~ price_ + participants_ + costs_ + benefits_ +
               supply_ + sharing_ + monitoring_,
             cluster = "admin_ResponseId")

g1 <- 
  gg_aconjoint_mm_cosmop %>%
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
  gg_aconjoint_mm_cosmop %>%
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
  gg_aconjoint_mm_cosmop %>%
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
    g1 + labs(x = "Marginal mean: high\ncosmopolitianism") + 
      theme(strip.text.x = element_text(color = "white", size = 11),
            panel.border = element_rect(fill = NA, colour = "black")),
    g2 + labs(x = "Marginal mean: low\ncosmopolitianism") +
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

pdf(file = "lucid_conjoint_a_means_cosmop.pdf",
    width = 7.6,
    height = 3 + fixed + spacing * (nrow(g1$data)))
p 
grid::grid.text("Costs to average household", 
                x = unit(0.39, "npc"), y = unit(0.977, "npc"), 
                just = "left",
                gp = gpar(fontsize = 11, col = "black", fontfamily = "serif",
                          fontface = "bold"))
grid::grid.text("Number of participating countries", 
                x = unit(0.39, "npc"), y = unit(0.81, "npc"), 
                just = "left",
                gp = gpar(fontsize = 11, col = "black", fontfamily = "serif",
                          fontface = "bold"))
grid::grid.text("Distribution of benefits", 
                x = unit(0.39, "npc"), y = unit(0.67, "npc"), 
                just = "left",
                gp = gpar(fontsize = 11, col = "black", fontfamily = "serif",
                          fontface = "bold"))
grid::grid.text("Distribution of costs", 
                x = unit(0.39, "npc"), y = unit(0.505, "npc"), 
                just = "left",
                gp = gpar(fontsize = 11, col = "black", fontfamily = "serif",
                          fontface = "bold"))
grid::grid.text("Monitoring for non-compliance", 
                x = unit(0.39, "npc"), y = unit(0.385, "npc"), 
                just = "left",
                gp = gpar(fontsize = 11, col = "black", fontfamily = "serif",
                          fontface = "bold"))
grid::grid.text("Sharing of vaccine technology", 
                x = unit(0.39, "npc"), y = unit(0.24, "npc"), 
                just = "left",
                gp = gpar(fontsize = 11, col = "black", fontfamily = "serif",
                          fontface = "bold"))
grid::grid.text("External supply agreements allowed", 
                x = unit(0.39, "npc"), y = unit(0.148, "npc"), 
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
  write_csv("lucid_conjoint_a_means_cosmop.csv")

g1 <- 
  gg_aconjoint_amce_cosmop %>%
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
  gg_aconjoint_amce_cosmop %>%
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
  gg_aconjoint_amce_cosmop %>%
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
    g1 + labs(x = "AMCE: high cosmopolitianism") + 
      theme(strip.text.x = element_text(size = 10, color = "white"),
            #strip.text.x = element_blank(),
            strip.placement = "outside",
            panel.border = element_rect(fill = NA, colour = "black")),
    g2 + labs(x = "AMCE: low cosmopolitianism") +
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

pdf(file = "lucid_conjoint_a_amces_cosmop.pdf", 
    width = 7.6,
    height = 4 + fixed + spacing * (nrow(g1$data)))
p 
grid::grid.text("Costs to average household (reference: $1)", 
                x = unit(0.37, "npc"), y = unit(0.985, "npc"), 
                just = "left",
                gp = gpar(fontsize = 11, col = "black", fontfamily = "serif",
                          fontface = "bold"))
grid::grid.text("Number of participating countries (reference: 20 of 192)", 
                x = unit(0.37, "npc"), y = unit(0.798, "npc"), 
                just = "left",
                gp = gpar(fontsize = 11, col = "black", fontfamily = "serif",
                          fontface = "bold"))
grid::grid.text("Distribution of benefits (reference: Only poor countries benefit)", 
                x = unit(0.37, "npc"), y = unit(0.645, "npc"), 
                just = "left",
                gp = gpar(fontsize = 11, col = "black", fontfamily = "serif",
                          fontface = "bold"))
grid::grid.text("Distribution of costs (reference: Only rich countries contribute)", 
                x = unit(0.37, "npc"), y = unit(0.464, "npc"), 
                just = "left",
                gp = gpar(fontsize = 11, col = "black", fontfamily = "serif",
                          fontface = "bold"))
grid::grid.text("Monitoring for non-compliance (reference: U.S. government)", 
                x = unit(0.37, "npc"), y = unit(0.346, "npc"), 
                just = "left",
                gp = gpar(fontsize = 11, col = "black", fontfamily = "serif",
                          fontface = "bold"))
grid::grid.text("Sharing of vaccine technology (reference: Compulsory)", 
                x = unit(0.37, "npc"), y = unit(0.20, "npc"), 
                just = "left",
                gp = gpar(fontsize = 11, col = "black", fontfamily = "serif",
                          fontface = "bold"))
grid::grid.text("External supply agreements allowed (reference: No)", 
                x = unit(0.37, "npc"), y = unit(0.115, "npc"), 
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
  write_csv("lucid_conjoint_a_amces_cosmop.csv")

### ---- Fig. S47-S48: heterogeneity by altruism ------

## Split sample by median for high and low altruism
tmp <-
  lucid_conjoint_a_long %>%
  filter_at(vars(altruism_willing_n, altruism_donate_n), 
            all_vars(complete.cases(.))) %>%
  mutate(altruism_idx = stdidx::idx_invcov(altruism_willing_n, altruism_donate_n))

cut <- median(tmp$altruism_idx, na.rm = T)
tmp$altruism_binary_n <-
  as.numeric(tmp$altruism_idx >= cut)

## Marginal means
gg_aconjoint_mm_altruism <-
  het_mm_a(x = "altruism_binary_n", long_data = tmp,
           cluster = "admin_ResponseId")

## AMCEs
gg_aconjoint_amce_altruism <-
  het_amce_a(x = "altruism_binary_n", long_data = tmp,
             formula = conjoint_chosen ~ price_ + participants_ + costs_ + benefits_ +
               supply_ + sharing_ + monitoring_,
             cluster = "admin_ResponseId")

g1 <- 
  gg_aconjoint_mm_altruism %>%
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
  gg_aconjoint_mm_altruism %>%
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
  gg_aconjoint_mm_altruism %>%
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

pdf(file = "lucid_conjoint_a_means_altruism.pdf",
    width = 7.6,
    height = 3 + fixed + spacing * (nrow(g1$data)))
p 
grid::grid.text("Costs to average household", 
                x = unit(0.39, "npc"), y = unit(0.977, "npc"), 
                just = "left",
                gp = gpar(fontsize = 11, col = "black", fontfamily = "serif",
                          fontface = "bold"))
grid::grid.text("Number of participating countries", 
                x = unit(0.39, "npc"), y = unit(0.81, "npc"), 
                just = "left",
                gp = gpar(fontsize = 11, col = "black", fontfamily = "serif",
                          fontface = "bold"))
grid::grid.text("Distribution of benefits", 
                x = unit(0.39, "npc"), y = unit(0.67, "npc"), 
                just = "left",
                gp = gpar(fontsize = 11, col = "black", fontfamily = "serif",
                          fontface = "bold"))
grid::grid.text("Distribution of costs", 
                x = unit(0.39, "npc"), y = unit(0.505, "npc"), 
                just = "left",
                gp = gpar(fontsize = 11, col = "black", fontfamily = "serif",
                          fontface = "bold"))
grid::grid.text("Monitoring for non-compliance", 
                x = unit(0.39, "npc"), y = unit(0.385, "npc"), 
                just = "left",
                gp = gpar(fontsize = 11, col = "black", fontfamily = "serif",
                          fontface = "bold"))
grid::grid.text("Sharing of vaccine technology", 
                x = unit(0.39, "npc"), y = unit(0.24, "npc"), 
                just = "left",
                gp = gpar(fontsize = 11, col = "black", fontfamily = "serif",
                          fontface = "bold"))
grid::grid.text("External supply agreements allowed", 
                x = unit(0.39, "npc"), y = unit(0.148, "npc"), 
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
  write_csv("lucid_conjoint_a_means_altruism.csv")


g1 <- 
  gg_aconjoint_amce_altruism %>%
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
  gg_aconjoint_amce_altruism %>%
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
  gg_aconjoint_amce_altruism %>%
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

pdf(file = "lucid_conjoint_a_amces_altruism.pdf",
    width = 7.6,
    height = 4 + fixed + spacing * (nrow(g1$data)))
p 
grid::grid.text("Costs to average household (reference: $1)", 
                x = unit(0.37, "npc"), y = unit(0.985, "npc"), 
                just = "left",
                gp = gpar(fontsize = 11, col = "black", fontfamily = "serif",
                          fontface = "bold"))
grid::grid.text("Number of participating countries (reference: 20 of 192)", 
                x = unit(0.37, "npc"), y = unit(0.798, "npc"), 
                just = "left",
                gp = gpar(fontsize = 11, col = "black", fontfamily = "serif",
                          fontface = "bold"))
grid::grid.text("Distribution of benefits (reference: Only poor countries benefit)", 
                x = unit(0.37, "npc"), y = unit(0.645, "npc"), 
                just = "left",
                gp = gpar(fontsize = 11, col = "black", fontfamily = "serif",
                          fontface = "bold"))
grid::grid.text("Distribution of costs (reference: Only rich countries contribute)", 
                x = unit(0.37, "npc"), y = unit(0.464, "npc"), 
                just = "left",
                gp = gpar(fontsize = 11, col = "black", fontfamily = "serif",
                          fontface = "bold"))
grid::grid.text("Monitoring for non-compliance (reference: U.S. government)", 
                x = unit(0.37, "npc"), y = unit(0.346, "npc"), 
                just = "left",
                gp = gpar(fontsize = 11, col = "black", fontfamily = "serif",
                          fontface = "bold"))
grid::grid.text("Sharing of vaccine technology (reference: Compulsory)", 
                x = unit(0.37, "npc"), y = unit(0.20, "npc"), 
                just = "left",
                gp = gpar(fontsize = 11, col = "black", fontfamily = "serif",
                          fontface = "bold"))
grid::grid.text("External supply agreements allowed (reference: No)", 
                x = unit(0.37, "npc"), y = unit(0.115, "npc"), 
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
  write_csv("lucid_conjoint_a_amces_altruism.csv")

### ---- Fig. S49-S50: heterogeneity by reciprocity ------

## Marginal means
gg_aconjoint_mm_recip <-
  het_mm_a(x = "recip_pos_gift_binary_n", long_data = lucid_conjoint_a_long,
           cluster = "admin_ResponseId")

## AMCEs
gg_aconjoint_amce_recip <-
  het_amce_a(x = "recip_pos_gift_binary_n", long_data = lucid_conjoint_a_long,
             formula = conjoint_chosen ~ price_ + participants_ + costs_ + benefits_ +
               supply_ + sharing_ + monitoring_,
             cluster = "admin_ResponseId")

g1 <- 
  gg_aconjoint_mm_recip %>%
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
  gg_aconjoint_mm_recip %>%
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
  gg_aconjoint_mm_recip %>%
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
    g1 + labs(x = "Marginal mean: high\nreciprocity") + 
      theme(strip.text.x = element_text(size = 11, color = "white"),
            panel.border = element_rect(fill = NA, colour = "black")),
    g2 + labs(x = "Marginal mean: low\nreciprocity") +
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

pdf(file = "lucid_conjoint_a_means_reciprocity.pdf", 
    width = 7.6,
    height = 3 + fixed + spacing * (nrow(g1$data)))
p 
grid::grid.text("Costs to average household", 
                x = unit(0.39, "npc"), y = unit(0.977, "npc"), 
                just = "left",
                gp = gpar(fontsize = 11, col = "black", fontfamily = "serif",
                          fontface = "bold"))
grid::grid.text("Number of participating countries", 
                x = unit(0.39, "npc"), y = unit(0.81, "npc"), 
                just = "left",
                gp = gpar(fontsize = 11, col = "black", fontfamily = "serif",
                          fontface = "bold"))
grid::grid.text("Distribution of benefits", 
                x = unit(0.39, "npc"), y = unit(0.67, "npc"), 
                just = "left",
                gp = gpar(fontsize = 11, col = "black", fontfamily = "serif",
                          fontface = "bold"))
grid::grid.text("Distribution of costs", 
                x = unit(0.39, "npc"), y = unit(0.505, "npc"), 
                just = "left",
                gp = gpar(fontsize = 11, col = "black", fontfamily = "serif",
                          fontface = "bold"))
grid::grid.text("Monitoring for non-compliance", 
                x = unit(0.39, "npc"), y = unit(0.385, "npc"), 
                just = "left",
                gp = gpar(fontsize = 11, col = "black", fontfamily = "serif",
                          fontface = "bold"))
grid::grid.text("Sharing of vaccine technology", 
                x = unit(0.39, "npc"), y = unit(0.24, "npc"), 
                just = "left",
                gp = gpar(fontsize = 11, col = "black", fontfamily = "serif",
                          fontface = "bold"))
grid::grid.text("External supply agreements allowed", 
                x = unit(0.39, "npc"), y = unit(0.148, "npc"), 
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
  write_csv("lucid_conjoint_a_means_reciprocity.csv")


g1 <- 
  gg_aconjoint_amce_recip %>%
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
  gg_aconjoint_amce_recip %>%
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
  gg_aconjoint_amce_recip %>%
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
    g1 + labs(x = "AMCE: high reciprocity") + 
      theme(strip.text.x = element_text(size = 10, color = "white"),
            #strip.text.x = element_blank(),
            strip.placement = "outside",
            panel.border = element_rect(fill = NA, colour = "black")),
    g2 + labs(x = "AMCE: low reciprocity") +
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

pdf(file = "lucid_conjoint_a_amces_reciprocity.pdf",
    width = 7.6,
    height = 4 + fixed + spacing * (nrow(g1$data)))
p 
grid::grid.text("Costs to average household (reference: $1)", 
                x = unit(0.37, "npc"), y = unit(0.985, "npc"), 
                just = "left",
                gp = gpar(fontsize = 11, col = "black", fontfamily = "serif",
                          fontface = "bold"))
grid::grid.text("Number of participating countries (reference: 20 of 192)", 
                x = unit(0.37, "npc"), y = unit(0.798, "npc"), 
                just = "left",
                gp = gpar(fontsize = 11, col = "black", fontfamily = "serif",
                          fontface = "bold"))
grid::grid.text("Distribution of benefits (reference: Only poor countries benefit)", 
                x = unit(0.37, "npc"), y = unit(0.645, "npc"), 
                just = "left",
                gp = gpar(fontsize = 11, col = "black", fontfamily = "serif",
                          fontface = "bold"))
grid::grid.text("Distribution of costs (reference: Only rich countries contribute)", 
                x = unit(0.37, "npc"), y = unit(0.464, "npc"), 
                just = "left",
                gp = gpar(fontsize = 11, col = "black", fontfamily = "serif",
                          fontface = "bold"))
grid::grid.text("Monitoring for non-compliance (reference: U.S. government)", 
                x = unit(0.37, "npc"), y = unit(0.346, "npc"), 
                just = "left",
                gp = gpar(fontsize = 11, col = "black", fontfamily = "serif",
                          fontface = "bold"))
grid::grid.text("Sharing of vaccine technology (reference: Compulsory)", 
                x = unit(0.37, "npc"), y = unit(0.20, "npc"), 
                just = "left",
                gp = gpar(fontsize = 11, col = "black", fontfamily = "serif",
                          fontface = "bold"))
grid::grid.text("External supply agreements allowed (reference: No)", 
                x = unit(0.37, "npc"), y = unit(0.115, "npc"), 
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
  write_csv("lucid_conjoint_a_amces_reciprocity.csv")

### ---- Fig. S51-S52: heterogeneity by vaccination status ------

## Marginal means
gg_aconjoint_mm_vax <-
  het_mm_a(x = "covid_vaxed_binary_n", long_data = lucid_conjoint_a_long,
           cluster = "admin_ResponseId")

## AMCEs
gg_aconjoint_amce_vax <-
  het_amce_a(x = "covid_vaxed_binary_n", long_data = lucid_conjoint_a_long,
             formula = conjoint_chosen ~ price_ + participants_ + costs_ + benefits_ +
               supply_ + sharing_ + monitoring_,
             cluster = "admin_ResponseId")

g1 <- 
  gg_aconjoint_mm_vax %>%
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
  gg_aconjoint_mm_vax %>%
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
  gg_aconjoint_mm_vax %>%
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
    g1 + labs(x = "Marginal mean:\nvaccinated") + 
      theme(strip.text.x = element_text(size = 11, color ="white"),
            panel.border = element_rect(fill = NA, colour = "black")),
    g2 + labs(x = "Marginal mean:\nunvaccinated") +
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

pdf(file = "lucid_conjoint_a_means_vax.pdf",
    width = 7.6,
    height = 3 + fixed + spacing * (nrow(g1$data)))
p 
grid::grid.text("Costs to average household", 
                x = unit(0.39, "npc"), y = unit(0.977, "npc"), 
                just = "left",
                gp = gpar(fontsize = 11, col = "black", fontfamily = "serif",
                          fontface = "bold"))
grid::grid.text("Number of participating countries", 
                x = unit(0.39, "npc"), y = unit(0.81, "npc"), 
                just = "left",
                gp = gpar(fontsize = 11, col = "black", fontfamily = "serif",
                          fontface = "bold"))
grid::grid.text("Distribution of benefits", 
                x = unit(0.39, "npc"), y = unit(0.67, "npc"), 
                just = "left",
                gp = gpar(fontsize = 11, col = "black", fontfamily = "serif",
                          fontface = "bold"))
grid::grid.text("Distribution of costs", 
                x = unit(0.39, "npc"), y = unit(0.505, "npc"), 
                just = "left",
                gp = gpar(fontsize = 11, col = "black", fontfamily = "serif",
                          fontface = "bold"))
grid::grid.text("Monitoring for non-compliance", 
                x = unit(0.39, "npc"), y = unit(0.385, "npc"), 
                just = "left",
                gp = gpar(fontsize = 11, col = "black", fontfamily = "serif",
                          fontface = "bold"))
grid::grid.text("Sharing of vaccine technology", 
                x = unit(0.39, "npc"), y = unit(0.24, "npc"), 
                just = "left",
                gp = gpar(fontsize = 11, col = "black", fontfamily = "serif",
                          fontface = "bold"))
grid::grid.text("External supply agreements allowed", 
                x = unit(0.39, "npc"), y = unit(0.148, "npc"), 
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
  write_csv("lucid_conjoint_a_means_vax.csv")


g1 <- 
  gg_aconjoint_amce_vax %>%
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
  gg_aconjoint_amce_vax %>%
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
  gg_aconjoint_amce_vax %>%
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
    g1 + labs(x = "AMCE: vaccinated") + 
      theme(strip.text.x = element_text(size = 10, color = "white"),
            #strip.text.x = element_blank(),
            strip.placement = "outside",
            panel.border = element_rect(fill = NA, colour = "black")),
    g2 + labs(x = "AMCE: unvaccinated") +
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

pdf(file = "lucid_conjoint_a_amces_vax.pdf",
    width = 7.6,
    height = 4 + fixed + spacing * (nrow(g1$data)))
p 
grid::grid.text("Costs to average household (reference: $1)", 
                x = unit(0.37, "npc"), y = unit(0.985, "npc"), 
                just = "left",
                gp = gpar(fontsize = 11, col = "black", fontfamily = "serif",
                          fontface = "bold"))
grid::grid.text("Number of participating countries (reference: 20 of 192)", 
                x = unit(0.37, "npc"), y = unit(0.798, "npc"), 
                just = "left",
                gp = gpar(fontsize = 11, col = "black", fontfamily = "serif",
                          fontface = "bold"))
grid::grid.text("Distribution of benefits (reference: Only poor countries benefit)", 
                x = unit(0.37, "npc"), y = unit(0.645, "npc"), 
                just = "left",
                gp = gpar(fontsize = 11, col = "black", fontfamily = "serif",
                          fontface = "bold"))
grid::grid.text("Distribution of costs (reference: Only rich countries contribute)", 
                x = unit(0.37, "npc"), y = unit(0.464, "npc"), 
                just = "left",
                gp = gpar(fontsize = 11, col = "black", fontfamily = "serif",
                          fontface = "bold"))
grid::grid.text("Monitoring for non-compliance (reference: U.S. government)", 
                x = unit(0.37, "npc"), y = unit(0.346, "npc"), 
                just = "left",
                gp = gpar(fontsize = 11, col = "black", fontfamily = "serif",
                          fontface = "bold"))
grid::grid.text("Sharing of vaccine technology (reference: Compulsory)", 
                x = unit(0.37, "npc"), y = unit(0.20, "npc"), 
                just = "left",
                gp = gpar(fontsize = 11, col = "black", fontfamily = "serif",
                          fontface = "bold"))
grid::grid.text("External supply agreements allowed (reference: No)", 
                x = unit(0.37, "npc"), y = unit(0.115, "npc"), 
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
  write_csv("lucid_conjoint_a_amces_vax.csv")


### ---- Fig. S53-S54: heterogeneity by partisanship ------
tmp <- 
  lucid_conjoint_a_long %>%
  mutate(democrat = case_when(x_pid_3 == "Democrat" ~ 1,
                              x_pid_3 == "Republican" ~ 0)) %>% 
  filter(!is.na(democrat))

## Marginal means
gg_aconjoint_mm_pid <-
  het_mm_a(x = "democrat", long_data = tmp, cluster = "admin_ResponseId")

## AMCEs
gg_aconjoint_amce_pid <-
  het_amce_a(x = "democrat", long_data = tmp,
             formula = conjoint_chosen ~ price_ + participants_ + costs_ + benefits_ +
               supply_ + sharing_ + monitoring_,
             cluster = "admin_ResponseId")

g1 <- 
  gg_aconjoint_mm_pid %>%
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
  scale_x_continuous(limits = c(0.33, 0.66), expand = c(0.01, 0.01)) +
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
  gg_aconjoint_mm_pid %>%
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
  scale_x_continuous(limits = c(0.33, 0.66), expand = c(0.01, 0.01)) +
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
  gg_aconjoint_mm_pid %>%
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

pdf(file = "lucid_conjoint_a_means_pid.pdf",
    width = 7.6,
    height = 3 + fixed + spacing * (nrow(g1$data)))
p 
grid::grid.text("Costs to average household", 
                x = unit(0.39, "npc"), y = unit(0.977, "npc"), 
                just = "left",
                gp = gpar(fontsize = 11, col = "black", fontfamily = "serif",
                          fontface = "bold"))
grid::grid.text("Number of participating countries", 
                x = unit(0.39, "npc"), y = unit(0.81, "npc"), 
                just = "left",
                gp = gpar(fontsize = 11, col = "black", fontfamily = "serif",
                          fontface = "bold"))
grid::grid.text("Distribution of benefits", 
                x = unit(0.39, "npc"), y = unit(0.67, "npc"), 
                just = "left",
                gp = gpar(fontsize = 11, col = "black", fontfamily = "serif",
                          fontface = "bold"))
grid::grid.text("Distribution of costs", 
                x = unit(0.39, "npc"), y = unit(0.505, "npc"), 
                just = "left",
                gp = gpar(fontsize = 11, col = "black", fontfamily = "serif",
                          fontface = "bold"))
grid::grid.text("Monitoring for non-compliance", 
                x = unit(0.39, "npc"), y = unit(0.385, "npc"), 
                just = "left",
                gp = gpar(fontsize = 11, col = "black", fontfamily = "serif",
                          fontface = "bold"))
grid::grid.text("Sharing of vaccine technology", 
                x = unit(0.39, "npc"), y = unit(0.24, "npc"), 
                just = "left",
                gp = gpar(fontsize = 11, col = "black", fontfamily = "serif",
                          fontface = "bold"))
grid::grid.text("External supply agreements allowed", 
                x = unit(0.39, "npc"), y = unit(0.148, "npc"), 
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
  write_csv("lucid_conjoint_a_means_pid.csv")


g1 <- 
  gg_aconjoint_amce_pid %>%
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
  gg_aconjoint_amce_pid %>%
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
  gg_aconjoint_amce_pid %>%
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

pdf(file = "lucid_conjoint_a_amces_pid.pdf",
    width = 7.6,
    height = 4 + fixed + spacing * (nrow(g1$data)))
p 
grid::grid.text("Costs to average household (reference: $1)", 
                x = unit(0.37, "npc"), y = unit(0.985, "npc"), 
                just = "left",
                gp = gpar(fontsize = 11, col = "black", fontfamily = "serif",
                          fontface = "bold"))
grid::grid.text("Number of participating countries (reference: 20 of 192)", 
                x = unit(0.37, "npc"), y = unit(0.798, "npc"), 
                just = "left",
                gp = gpar(fontsize = 11, col = "black", fontfamily = "serif",
                          fontface = "bold"))
grid::grid.text("Distribution of benefits (reference: Only poor countries benefit)", 
                x = unit(0.37, "npc"), y = unit(0.645, "npc"), 
                just = "left",
                gp = gpar(fontsize = 11, col = "black", fontfamily = "serif",
                          fontface = "bold"))
grid::grid.text("Distribution of costs (reference: Only rich countries contribute)", 
                x = unit(0.37, "npc"), y = unit(0.464, "npc"), 
                just = "left",
                gp = gpar(fontsize = 11, col = "black", fontfamily = "serif",
                          fontface = "bold"))
grid::grid.text("Monitoring for non-compliance (reference: U.S. government)", 
                x = unit(0.37, "npc"), y = unit(0.346, "npc"), 
                just = "left",
                gp = gpar(fontsize = 11, col = "black", fontfamily = "serif",
                          fontface = "bold"))
grid::grid.text("Sharing of vaccine technology (reference: Compulsory)", 
                x = unit(0.37, "npc"), y = unit(0.20, "npc"), 
                just = "left",
                gp = gpar(fontsize = 11, col = "black", fontfamily = "serif",
                          fontface = "bold"))
grid::grid.text("External supply agreements allowed (reference: No)", 
                x = unit(0.37, "npc"), y = unit(0.115, "npc"), 
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
  write_csv("lucid_conjoint_a_amces_pid.csv")


### ---- Fig. S55-S56: heterogeneity by ideology ------
tmp <- 
  lucid_conjoint_a_long %>%
  mutate(liberal = case_when(x_ideo_3 == "Liberal" ~ 1,
                             x_ideo_3 == "Conservative" ~ 0)) %>% 
  filter(!is.na(liberal))

## Marginal means
gg_aconjoint_mm_ideo <-
  het_mm_a(x = "liberal", long_data = tmp,
           cluster = "admin_ResponseId")

## AMCEs
gg_aconjoint_amce_ideo <-
  het_amce_a(x = "liberal", long_data = tmp, 
             formula = conjoint_chosen ~ price_ + participants_ + costs_ + benefits_ +
               supply_ + sharing_ + monitoring_,
             cluster = "admin_ResponseId")

g1 <- 
  gg_aconjoint_mm_ideo %>%
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
  scale_x_continuous(limits = c(0.33, 0.66), expand = c(0.01, 0.01)) +
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
  gg_aconjoint_mm_ideo %>%
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
  scale_x_continuous(limits = c(0.33, 0.66), expand = c(0.01, 0.01)) +
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
  gg_aconjoint_mm_ideo %>%
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

pdf(file = "lucid_conjoint_a_means_ideo.pdf",
    width = 7.6,
    height = 3 + fixed + spacing * (nrow(g1$data)))
p 
grid::grid.text("Costs to average household", 
                x = unit(0.39, "npc"), y = unit(0.977, "npc"), 
                just = "left",
                gp = gpar(fontsize = 11, col = "black", fontfamily = "serif",
                          fontface = "bold"))
grid::grid.text("Number of participating countries", 
                x = unit(0.39, "npc"), y = unit(0.81, "npc"), 
                just = "left",
                gp = gpar(fontsize = 11, col = "black", fontfamily = "serif",
                          fontface = "bold"))
grid::grid.text("Distribution of benefits", 
                x = unit(0.39, "npc"), y = unit(0.67, "npc"), 
                just = "left",
                gp = gpar(fontsize = 11, col = "black", fontfamily = "serif",
                          fontface = "bold"))
grid::grid.text("Distribution of costs", 
                x = unit(0.39, "npc"), y = unit(0.505, "npc"), 
                just = "left",
                gp = gpar(fontsize = 11, col = "black", fontfamily = "serif",
                          fontface = "bold"))
grid::grid.text("Monitoring for non-compliance", 
                x = unit(0.39, "npc"), y = unit(0.385, "npc"), 
                just = "left",
                gp = gpar(fontsize = 11, col = "black", fontfamily = "serif",
                          fontface = "bold"))
grid::grid.text("Sharing of vaccine technology", 
                x = unit(0.39, "npc"), y = unit(0.24, "npc"), 
                just = "left",
                gp = gpar(fontsize = 11, col = "black", fontfamily = "serif",
                          fontface = "bold"))
grid::grid.text("External supply agreements allowed", 
                x = unit(0.39, "npc"), y = unit(0.148, "npc"), 
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
  write_csv("lucid_conjoint_a_means_ideo.csv")


g1 <- 
  gg_aconjoint_amce_ideo %>%
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
  scale_x_continuous(limits = c(-0.26, 0.26), expand = c(0.01, 0.01)) +
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
  gg_aconjoint_amce_ideo %>%
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
  scale_x_continuous(limits = c(-0.26, 0.26), expand = c(0.01, 0.01)) +
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
  gg_aconjoint_amce_ideo %>%
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

pdf(file = "lucid_conjoint_a_amces_ideo.pdf",
    width = 7.6,
    height = 4 + fixed + spacing * (nrow(g1$data)))
p 
grid::grid.text("Costs to average household (reference: $1)", 
                x = unit(0.37, "npc"), y = unit(0.985, "npc"), 
                just = "left",
                gp = gpar(fontsize = 11, col = "black", fontfamily = "serif",
                          fontface = "bold"))
grid::grid.text("Number of participating countries (reference: 20 of 192)", 
                x = unit(0.37, "npc"), y = unit(0.798, "npc"), 
                just = "left",
                gp = gpar(fontsize = 11, col = "black", fontfamily = "serif",
                          fontface = "bold"))
grid::grid.text("Distribution of benefits (reference: Only poor countries benefit)", 
                x = unit(0.37, "npc"), y = unit(0.645, "npc"), 
                just = "left",
                gp = gpar(fontsize = 11, col = "black", fontfamily = "serif",
                          fontface = "bold"))
grid::grid.text("Distribution of costs (reference: Only rich countries contribute)", 
                x = unit(0.37, "npc"), y = unit(0.464, "npc"), 
                just = "left",
                gp = gpar(fontsize = 11, col = "black", fontfamily = "serif",
                          fontface = "bold"))
grid::grid.text("Monitoring for non-compliance (reference: U.S. government)", 
                x = unit(0.37, "npc"), y = unit(0.346, "npc"), 
                just = "left",
                gp = gpar(fontsize = 11, col = "black", fontfamily = "serif",
                          fontface = "bold"))
grid::grid.text("Sharing of vaccine technology (reference: Compulsory)", 
                x = unit(0.37, "npc"), y = unit(0.20, "npc"), 
                just = "left",
                gp = gpar(fontsize = 11, col = "black", fontfamily = "serif",
                          fontface = "bold"))
grid::grid.text("External supply agreements allowed (reference: No)", 
                x = unit(0.37, "npc"), y = unit(0.115, "npc"), 
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
  write_csv("lucid_conjoint_a_amces_ideo.csv")

