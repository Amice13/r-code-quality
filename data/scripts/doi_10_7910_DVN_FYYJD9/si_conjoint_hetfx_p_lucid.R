### ---- Fig. S21-S22: heterogeneity by vaccine nationalism -------

## Marginal means
gg_pconjoint_mm_vnat <- 
  het_mm_p(x = "vnat_binary_n", long_data = lucid_conjoint_p_long) 

## AMCEs
gg_pconjoint_amce_vnat <- 
  het_amce_p(x = "vnat_binary_n", long_data = lucid_conjoint_p_long)


g1 <- 
  gg_pconjoint_mm_vnat %>%
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
  gg_pconjoint_mm_vnat %>%
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
  gg_pconjoint_mm_vnat %>%
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
    g1 + labs(x = "Marginal mean: vaccine nationlists") + 
      theme(strip.text.x = element_text(size = 11,
                                        face = "bold"),
            panel.border = element_rect(fill = NA, colour = "black")),
    g2 + labs(x = "Marginal mean: non-nationalists") +
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

ggsave("lucid_conjoint_p_means_vnat.pdf",
       g,
       width = 7.6,
       height = 3 + fixed + spacing * (nrow(g1$data)))

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
  write_csv("lucid_conjoint_p_means_vnat.csv")


g1 <- 
  gg_pconjoint_amce_vnat %>%
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
  gg_pconjoint_amce_vnat %>%
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
  gg_pconjoint_amce_vnat %>%
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

pdf("lucid_conjoint_p_amces_vnat.pdf",
    width = 7.6,
    height = 4 + fixed + spacing * (nrow(g1$data)))
p 
grid::grid.text("Country of origin (reference: United States)", 
                x = unit(0.235, "npc"), y = unit(0.985, "npc"), 
                just = "left",
                gp = gpar(fontsize = 11, col = "black", fontfamily = "serif",
                          fontface = "bold"))
grid::grid.text("Risk of exposure (reference: Low)", 
                x = unit(0.235, "npc"), y = unit(0.726, "npc"), 
                just = "left",
                gp = gpar(fontsize = 11, col = "black", fontfamily = "serif",
                          fontface = "bold"))
grid::grid.text("Risk of serious illness (reference: Low)", 
                x = unit(0.235, "npc"), y = unit(0.626, "npc"), 
                just = "left",
                gp = gpar(fontsize = 11, col = "black", fontfamily = "serif",
                          fontface = "bold"))
grid::grid.text("Occupation group (reference: Non-essential workers)", 
                x = unit(0.235, "npc"), y = unit(0.53, "npc"), 
                just = "left",
                gp = gpar(fontsize = 11, col = "black", fontfamily = "serif",
                          fontface = "bold"))
grid::grid.text("Can work from home (reference: Yes)", 
                x = unit(0.235, "npc"), y = unit(0.38, "npc"), 
                just = "left",
                gp = gpar(fontsize = 11, col = "black", fontfamily = "serif",
                          fontface = "bold"))
grid::grid.text("Age group (reference: 18-24)", 
                x = unit(0.235, "npc"), y = unit(0.305, "npc"), 
                just = "left",
                gp = gpar(fontsize = 11, col = "black", fontfamily = "serif",
                          fontface = "bold"))
grid::grid.text("Sex (reference: Female)", 
                x = unit(0.235, "npc"), y = unit(0.1, "npc"), 
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
  write_csv("lucid_conjoint_p_amces_vnat.csv")

###----- Fig. S23-S24: heterogeneity by vaccine nationalism index ------
tmp <-
  lucid_conjoint_p_long %>%
  filter_at(vars(vnat_1_n:vnat_5_n), all_vars(complete.cases(.))) %>%
  mutate(national_idx = stdidx::idx_invcov(vnat_1_n, vnat_2_n, vnat_3_n,
                                           vnat_4_n, vnat_5_n))

## Split sample by median for high and low nationalism
cut <- median(tmp$national_idx)
tmp$vnat_idx_binary_n <-
  as.numeric(tmp$national_idx >= cut)

## Marginal means
gg_pconjoint_mm_vnat_idx <-
  het_mm_p(x = "vnat_idx_binary_n", long_data = tmp)

## AMCEs
gg_pconjoint_amce_vnat_idx <-
  het_amce_p(x = "vnat_idx_binary_n", long_data = tmp)

g1 <-
  gg_pconjoint_mm_vnat_idx %>%
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
  gg_pconjoint_mm_vnat_idx %>%
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
  gg_pconjoint_mm_vnat_idx %>%
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
        strip.text.x = element_text(size = 11,
                                    face = "bold"),
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


ggsave(
  filename = "lucid_conjoint_p_means_vnat_idx.pdf",
  g,
  width = 7.6,
  height = 3 + fixed + spacing * (nrow(g1$data))
)

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
  write_csv("lucid_conjoint_p_means_vnat_idx.csv")

g1 <-
  gg_pconjoint_amce_vnat_idx %>%
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
  gg_pconjoint_amce_vnat_idx %>%
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
  gg_pconjoint_amce_vnat_idx %>%
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
    g1 + labs(x = "AMCE: high\nvaccine nationalism") +
      theme(strip.text.x = element_text(size = 10, color = "white"),
            strip.placement = "outside",
            panel.border = element_rect(fill = NA, colour = "black")),
    g2 + labs(x = "AMCE: low\nvaccine nationalism") +
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
  file = "lucid_conjoint_p_amces_vnat_idx.pdf",
  width = 7.6,
  height = 4 + fixed + spacing * (nrow(g1$data))
)
p
grid::grid.text(
  "Country of origin (reference: United States)",
  x = unit(0.235, "npc"),
  y = unit(0.985, "npc"),
  just = "left",
  gp = gpar(
    fontsize = 11,
    col = "black",
    fontfamily = "serif",
    fontface = "bold"
  )
)
grid::grid.text(
  "Risk of exposure (reference: Low)",
  x = unit(0.235, "npc"),
  y = unit(0.74, "npc"),
  just = "left",
  gp = gpar(
    fontsize = 11,
    col = "black",
    fontfamily = "serif",
    fontface = "bold"
  )
)
grid::grid.text(
  "Risk of serious illness (reference: Low)",
  x = unit(0.235, "npc"),
  y = unit(0.64, "npc"),
  just = "left",
  gp = gpar(
    fontsize = 11,
    col = "black",
    fontfamily = "serif",
    fontface = "bold"
  )
)
grid::grid.text(
  "Occupation group (reference: Non-essential workers)",
  x = unit(0.235, "npc"),
  y = unit(0.54, "npc"),
  just = "left",
  gp = gpar(
    fontsize = 11,
    col = "black",
    fontfamily = "serif",
    fontface = "bold"
  )
)
grid::grid.text(
  "Can work from home (reference: Yes)",
  x = unit(0.235, "npc"),
  y = unit(0.385, "npc"),
  just = "left",
  gp = gpar(
    fontsize = 11,
    col = "black",
    fontfamily = "serif",
    fontface = "bold"
  )
)
grid::grid.text(
  "Age group (reference: 18-24)",
  x = unit(0.235, "npc"),
  y = unit(0.315, "npc"),
  just = "left",
  gp = gpar(
    fontsize = 11,
    col = "black",
    fontfamily = "serif",
    fontface = "bold"
  )
)
grid::grid.text(
  "Sex (reference: Female)",
  x = unit(0.235, "npc"),
  y = unit(0.115, "npc"),
  just = "left",
  gp = gpar(
    fontsize = 11,
    col = "black",
    fontfamily = "serif",
    fontface = "bold"
  )
)
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
  write_csv("lucid_conjoint_p_amces_vnat_idx.csv")

###----- Fig. S25-S26: heterogeneity by nationalism item ------

## Split sample by median for high and low nationalism
cut <- median(lucid_conjoint_p_long$national_n, na.rm = T)
lucid_conjoint_p_long$national_binary_n <- 
  as.numeric(lucid_conjoint_p_long$national_n >= cut)

## Marginal means
gg_pconjoint_mm_national <- 
  het_mm_p(x = "national_binary_n", long_data = lucid_conjoint_p_long)

## AMCEs
gg_pconjoint_amce_national <- 
  het_amce_p(x = "national_binary_n", long_data = lucid_conjoint_p_long)


g1 <- 
  gg_pconjoint_mm_national %>%
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
  gg_pconjoint_mm_national %>%
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
  gg_pconjoint_mm_national %>%
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
      theme(strip.text.x = element_text(size = 11,
                                        face = "bold"),
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

ggsave(filename = "lucid_conjoint_p_means_national.pdf",
       g,
       width = 7.6,
       height = 3 + fixed + spacing * (nrow(g1$data)))

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
  write_csv("lucid_conjoint_p_means_national.csv")


g1 <- 
  gg_pconjoint_amce_national %>%
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
  gg_pconjoint_amce_national %>%
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
  gg_pconjoint_amce_national %>%
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

pdf("lucid_conjoint_p_amces_national.pdf",
    width = 7.6,
    height = 4 + fixed + spacing * (nrow(g1$data)))
p 
grid::grid.text("Country of origin (reference: United States)", 
                x = unit(0.235, "npc"), y = unit(0.985, "npc"), 
                just = "left",
                gp = gpar(fontsize = 11, col = "black", fontfamily = "serif",
                          fontface = "bold"))
grid::grid.text("Risk of exposure (reference: Low)", 
                x = unit(0.235, "npc"), y = unit(0.726, "npc"), 
                just = "left",
                gp = gpar(fontsize = 11, col = "black", fontfamily = "serif",
                          fontface = "bold"))
grid::grid.text("Risk of serious illness (reference: Low)", 
                x = unit(0.235, "npc"), y = unit(0.626, "npc"), 
                just = "left",
                gp = gpar(fontsize = 11, col = "black", fontfamily = "serif",
                          fontface = "bold"))
grid::grid.text("Occupation group (reference: Non-essential workers)", 
                x = unit(0.235, "npc"), y = unit(0.53, "npc"), 
                just = "left",
                gp = gpar(fontsize = 11, col = "black", fontfamily = "serif",
                          fontface = "bold"))
grid::grid.text("Can work from home (reference: Yes)", 
                x = unit(0.235, "npc"), y = unit(0.38, "npc"), 
                just = "left",
                gp = gpar(fontsize = 11, col = "black", fontfamily = "serif",
                          fontface = "bold"))
grid::grid.text("Age group (reference: 18-24)", 
                x = unit(0.235, "npc"), y = unit(0.305, "npc"), 
                just = "left",
                gp = gpar(fontsize = 11, col = "black", fontfamily = "serif",
                          fontface = "bold"))
grid::grid.text("Sex (reference: Female)", 
                x = unit(0.235, "npc"), y = unit(0.1, "npc"), 
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
  write_csv("lucid_conjoint_p_amces_national.csv")

### ---- Fig. S27-S28: heterogeneity by cosmopolitanism ----

## Split sample by median for high and low cosmop
cut <- median(lucid_conjoint_p_long$cosmop_n, na.rm = T)
lucid_conjoint_p_long$cosmop_binary_n <- 
  as.numeric(lucid_conjoint_p_long$cosmop_n > cut)

## Marginal means
gg_pconjoint_mm_cosmop <- 
  het_mm_p(x = "cosmop_binary_n", long_data = lucid_conjoint_p_long) 

## AMCEs
gg_pconjoint_amce_cosmop <- 
  het_amce_p(x = "cosmop_binary_n", long_data = lucid_conjoint_p_long)

g1 <- 
  gg_pconjoint_mm_cosmop %>%
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
  gg_pconjoint_mm_cosmop %>%
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
  gg_pconjoint_mm_cosmop %>%
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
      theme(strip.text.x = element_text(size = 11,
                                        face = "bold"),
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

ggsave(filename = "lucid_conjoint_p_means_cosmop.pdf",
       g,
       width = 7.6,
       height = 3 + fixed + spacing * (nrow(g1$data)))

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
  write_csv("lucid_conjoint_p_means_cosmop.csv")



g1 <- 
  gg_pconjoint_amce_cosmop %>%
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
  gg_pconjoint_amce_cosmop %>%
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
  gg_pconjoint_amce_cosmop %>%
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


pdf("lucid_conjoint_p_amces_cosmop.pdf",
    width = 7.6,
    height = 4 + fixed + spacing * (nrow(g1$data)))
p 
grid::grid.text("Country of origin (reference: United States)", 
                x = unit(0.235, "npc"), y = unit(0.985, "npc"), 
                just = "left",
                gp = gpar(fontsize = 11, col = "black", fontfamily = "serif",
                          fontface = "bold"))
grid::grid.text("Risk of exposure (reference: Low)", 
                x = unit(0.235, "npc"), y = unit(0.726, "npc"), 
                just = "left",
                gp = gpar(fontsize = 11, col = "black", fontfamily = "serif",
                          fontface = "bold"))
grid::grid.text("Risk of serious illness (reference: Low)", 
                x = unit(0.235, "npc"), y = unit(0.626, "npc"), 
                just = "left",
                gp = gpar(fontsize = 11, col = "black", fontfamily = "serif",
                          fontface = "bold"))
grid::grid.text("Occupation group (reference: Non-essential workers)", 
                x = unit(0.235, "npc"), y = unit(0.53, "npc"), 
                just = "left",
                gp = gpar(fontsize = 11, col = "black", fontfamily = "serif",
                          fontface = "bold"))
grid::grid.text("Can work from home (reference: Yes)", 
                x = unit(0.235, "npc"), y = unit(0.38, "npc"), 
                just = "left",
                gp = gpar(fontsize = 11, col = "black", fontfamily = "serif",
                          fontface = "bold"))
grid::grid.text("Age group (reference: 18-24)", 
                x = unit(0.235, "npc"), y = unit(0.305, "npc"), 
                just = "left",
                gp = gpar(fontsize = 11, col = "black", fontfamily = "serif",
                          fontface = "bold"))
grid::grid.text("Sex (reference: Female)", 
                x = unit(0.235, "npc"), y = unit(0.1, "npc"), 
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
  write_csv("lucid_conjoint_p_amces_cosmop.csv")

### ---- Fig. S29-S30: heterogeneity by altruism ------

## Split sample by median for high and low altruism
tmp <-
  lucid_conjoint_p_long %>%
  filter_at(vars(altruism_willing_n, altruism_donate_n), 
            all_vars(complete.cases(.))) %>%
  mutate(altruism_idx = stdidx::idx_invcov(altruism_willing_n, altruism_donate_n))

cut <- median(tmp$altruism_idx, na.rm = T)
tmp$altruism_binary_n <-
  as.numeric(tmp$altruism_idx >= cut)
mean(tmp$altruism_binary_n)

## Marginal means
gg_pconjoint_mm_altruism <-
  het_mm_p(x = "altruism_binary_n", long_data = tmp)

## AMCEs
gg_pconjoint_amce_altruism <-
  het_amce_p(x = "altruism_binary_n", long_data = tmp)

g1 <- 
  gg_pconjoint_mm_altruism %>%
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
  gg_pconjoint_mm_altruism %>%
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
  gg_pconjoint_mm_altruism %>%
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
      theme(strip.text.x = element_text(size = 11,
                                        face = "bold"),
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


ggsave(filename = "lucid_conjoint_p_means_altruism.pdf",
       g,
       width = 7.6,
       height = 3 + fixed + spacing * (nrow(g1$data)))

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
  write_csv("lucid_conjoint_p_means_altruism.csv")

g1 <- 
  gg_pconjoint_amce_altruism %>%
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
  gg_pconjoint_amce_altruism %>%
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
  gg_pconjoint_amce_altruism %>%
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


## Now convert grob into plot and add text ...
p <- ggplotify::as.ggplot(g)

pdf("lucid_conjoint_p_amces_altruism.pdf",
    width = 7.6,
    height = 4 + fixed + spacing * (nrow(g1$data)))
p 
grid::grid.text("Country of origin (reference: United States)", 
                x = unit(0.235, "npc"), y = unit(0.985, "npc"), 
                just = "left",
                gp = gpar(fontsize = 11, col = "black", fontfamily = "serif",
                          fontface = "bold"))
grid::grid.text("Risk of exposure (reference: Low)", 
                x = unit(0.235, "npc"), y = unit(0.726, "npc"), 
                just = "left",
                gp = gpar(fontsize = 11, col = "black", fontfamily = "serif",
                          fontface = "bold"))
grid::grid.text("Risk of serious illness (reference: Low)", 
                x = unit(0.235, "npc"), y = unit(0.626, "npc"), 
                just = "left",
                gp = gpar(fontsize = 11, col = "black", fontfamily = "serif",
                          fontface = "bold"))
grid::grid.text("Occupation group (reference: Non-essential workers)", 
                x = unit(0.235, "npc"), y = unit(0.53, "npc"), 
                just = "left",
                gp = gpar(fontsize = 11, col = "black", fontfamily = "serif",
                          fontface = "bold"))
grid::grid.text("Can work from home (reference: Yes)", 
                x = unit(0.235, "npc"), y = unit(0.38, "npc"), 
                just = "left",
                gp = gpar(fontsize = 11, col = "black", fontfamily = "serif",
                          fontface = "bold"))
grid::grid.text("Age group (reference: 18-24)", 
                x = unit(0.235, "npc"), y = unit(0.305, "npc"), 
                just = "left",
                gp = gpar(fontsize = 11, col = "black", fontfamily = "serif",
                          fontface = "bold"))
grid::grid.text("Sex (reference: Female)", 
                x = unit(0.235, "npc"), y = unit(0.1, "npc"), 
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
  write_csv("lucid_conjoint_p_amces_altruism.csv")

### ---- Fig. S31-S32: heterogeneity by positive reciprocity ------

## Marginal means
gg_pconjoint_mm_recip <-
  het_mm_p(x = "recip_pos_gift_binary_n", long_data = lucid_conjoint_p_long)

## AMCEs
gg_pconjoint_amce_recip <-
  het_amce_p(x = "recip_pos_gift_binary_n", long_data = lucid_conjoint_p_long)


g1 <- 
  gg_pconjoint_mm_recip %>%
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
  gg_pconjoint_mm_recip %>%
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
  gg_pconjoint_mm_recip %>%
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
      theme(strip.text.x = element_text(size = 11,
                                        face = "bold"),
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

ggsave(filename = "lucid_conjoint_p_means_reciprocity.pdf",
       g,
       width = 7.6,
       height = 3 + fixed + spacing * (nrow(g1$data)))


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
  write_csv("lucid_conjoint_p_means_reciprocity.csv")

g1 <- 
  gg_pconjoint_amce_recip %>%
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
  gg_pconjoint_amce_recip %>%
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
  gg_pconjoint_amce_recip %>%
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

pdf("lucid_conjoint_p_amces_reciprocity.pdf",
    width = 7.6,
    height = 4 + fixed + spacing * (nrow(g1$data)))
p 
grid::grid.text("Country of origin (reference: United States)", 
                x = unit(0.235, "npc"), y = unit(0.985, "npc"), 
                just = "left",
                gp = gpar(fontsize = 11, col = "black", fontfamily = "serif",
                          fontface = "bold"))
grid::grid.text("Risk of exposure (reference: Low)", 
                x = unit(0.235, "npc"), y = unit(0.726, "npc"), 
                just = "left",
                gp = gpar(fontsize = 11, col = "black", fontfamily = "serif",
                          fontface = "bold"))
grid::grid.text("Risk of serious illness (reference: Low)", 
                x = unit(0.235, "npc"), y = unit(0.626, "npc"), 
                just = "left",
                gp = gpar(fontsize = 11, col = "black", fontfamily = "serif",
                          fontface = "bold"))
grid::grid.text("Occupation group (reference: Non-essential workers)", 
                x = unit(0.235, "npc"), y = unit(0.53, "npc"), 
                just = "left",
                gp = gpar(fontsize = 11, col = "black", fontfamily = "serif",
                          fontface = "bold"))
grid::grid.text("Can work from home (reference: Yes)", 
                x = unit(0.235, "npc"), y = unit(0.38, "npc"), 
                just = "left",
                gp = gpar(fontsize = 11, col = "black", fontfamily = "serif",
                          fontface = "bold"))
grid::grid.text("Age group (reference: 18-24)", 
                x = unit(0.235, "npc"), y = unit(0.305, "npc"), 
                just = "left",
                gp = gpar(fontsize = 11, col = "black", fontfamily = "serif",
                          fontface = "bold"))
grid::grid.text("Sex (reference: Female)", 
                x = unit(0.235, "npc"), y = unit(0.1, "npc"), 
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
  write_csv("lucid_conjoint_p_amces_reciprocity.csv")

### ---- Fig. S33-S34: heterogeneity by vaccination status ------

## Marginal means
gg_pconjoint_mm_vax <-
  het_mm_p(x = "covid_vaxed_binary_n", long_data = lucid_conjoint_p_long)

## AMCEs
gg_pconjoint_amce_vax <-
  het_amce_p(x = "covid_vaxed_binary_n", long_data = lucid_conjoint_p_long)

g1 <- 
  gg_pconjoint_mm_vax %>%
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
  gg_pconjoint_mm_vax %>%
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
  gg_pconjoint_mm_vax %>%
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
    g1 + labs(x = "Marginal mean: vaccinated") + 
      theme(strip.text.x = element_text(size = 11,
                                        face = "bold"),
            panel.border = element_rect(fill = NA, colour = "black")),
    g2 + labs(x = "Marginal mean: unvaccinated") +
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

ggsave(filename = "lucid_conjoint_p_means_vax.pdf",
       g,
       width = 7.6,
       height = 3 + fixed + spacing * (nrow(g1$data)))

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
  write_csv("lucid_conjoint_p_means_vax.csv")

g1 <- 
  gg_pconjoint_amce_vax %>%
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
  gg_pconjoint_amce_vax %>%
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
  gg_pconjoint_amce_vax %>%
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

pdf("lucid_conjoint_p_amces_vax.pdf",
    width = 7.6,
    height = 4 + fixed + spacing * (nrow(g1$data)))
p 
grid::grid.text("Country of origin (reference: United States)", 
                x = unit(0.235, "npc"), y = unit(0.985, "npc"), 
                just = "left",
                gp = gpar(fontsize = 11, col = "black", fontfamily = "serif",
                          fontface = "bold"))
grid::grid.text("Risk of exposure (reference: Low)", 
                x = unit(0.235, "npc"), y = unit(0.726, "npc"), 
                just = "left",
                gp = gpar(fontsize = 11, col = "black", fontfamily = "serif",
                          fontface = "bold"))
grid::grid.text("Risk of serious illness (reference: Low)", 
                x = unit(0.235, "npc"), y = unit(0.626, "npc"), 
                just = "left",
                gp = gpar(fontsize = 11, col = "black", fontfamily = "serif",
                          fontface = "bold"))
grid::grid.text("Occupation group (reference: Non-essential workers)", 
                x = unit(0.235, "npc"), y = unit(0.53, "npc"), 
                just = "left",
                gp = gpar(fontsize = 11, col = "black", fontfamily = "serif",
                          fontface = "bold"))
grid::grid.text("Can work from home (reference: Yes)", 
                x = unit(0.235, "npc"), y = unit(0.38, "npc"), 
                just = "left",
                gp = gpar(fontsize = 11, col = "black", fontfamily = "serif",
                          fontface = "bold"))
grid::grid.text("Age group (reference: 18-24)", 
                x = unit(0.235, "npc"), y = unit(0.305, "npc"), 
                just = "left",
                gp = gpar(fontsize = 11, col = "black", fontfamily = "serif",
                          fontface = "bold"))
grid::grid.text("Sex (reference: Female)", 
                x = unit(0.235, "npc"), y = unit(0.1, "npc"), 
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
  write_csv("lucid_conjoint_p_amces_vax.csv")


### ---- Fig. S35-S36: heterogeneity by partisanship ------
tmp <- 
  lucid_conjoint_p_long %>%
  mutate(democrat = case_when(x_pid_3 == "Democrat" ~ 1,
                              x_pid_3 == "Republican" ~ 0)) %>% 
  filter(!is.na(democrat))

## Marginal means
gg_pconjoint_mm_pid <-
  het_mm_p(x = "democrat", long_data = tmp)

## AMCEs
gg_pconjoint_amce_pid <-
  het_amce_p(x = "democrat", long_data = tmp)


g1 <- 
  gg_pconjoint_mm_pid %>%
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
  scale_x_continuous(limits = c(0.33, 0.65), expand = c(0.01, 0.01)) +
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
  gg_pconjoint_mm_pid %>%
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
  scale_x_continuous(limits = c(0.33, 0.65), expand = c(0.01, 0.01)) +
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
  gg_pconjoint_mm_pid %>%
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
    g1 + labs(x = "Marginal mean: Democrats") + 
      theme(strip.text.x = element_text(size = 11,
                                        face = "bold"),
            panel.border = element_rect(fill = NA, colour = "black")),
    g2 + labs(x = "Marginal mean: Republicans") +
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

ggsave(filename = "lucid_conjoint_p_means_pid.pdf",
       g,
       width = 7.6,
       height = 3 + fixed + spacing * (nrow(g1$data)))

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
  write_csv("lucid_conjoint_p_means_pid.csv")


g1 <- 
  gg_pconjoint_amce_pid %>%
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
  gg_pconjoint_amce_pid %>%
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
  gg_pconjoint_amce_pid %>%
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

pdf("lucid_conjoint_p_amces_pid.pdf",
    width = 7.6,
    height = 4 + fixed + spacing * (nrow(g1$data)))
p 
grid::grid.text("Country of origin (reference: United States)", 
                x = unit(0.235, "npc"), y = unit(0.985, "npc"), 
                just = "left",
                gp = gpar(fontsize = 11, col = "black", fontfamily = "serif",
                          fontface = "bold"))
grid::grid.text("Risk of exposure (reference: Low)", 
                x = unit(0.235, "npc"), y = unit(0.726, "npc"), 
                just = "left",
                gp = gpar(fontsize = 11, col = "black", fontfamily = "serif",
                          fontface = "bold"))
grid::grid.text("Risk of serious illness (reference: Low)", 
                x = unit(0.235, "npc"), y = unit(0.626, "npc"), 
                just = "left",
                gp = gpar(fontsize = 11, col = "black", fontfamily = "serif",
                          fontface = "bold"))
grid::grid.text("Occupation group (reference: Non-essential workers)", 
                x = unit(0.235, "npc"), y = unit(0.53, "npc"), 
                just = "left",
                gp = gpar(fontsize = 11, col = "black", fontfamily = "serif",
                          fontface = "bold"))
grid::grid.text("Can work from home (reference: Yes)", 
                x = unit(0.235, "npc"), y = unit(0.38, "npc"), 
                just = "left",
                gp = gpar(fontsize = 11, col = "black", fontfamily = "serif",
                          fontface = "bold"))
grid::grid.text("Age group (reference: 18-24)", 
                x = unit(0.235, "npc"), y = unit(0.305, "npc"), 
                just = "left",
                gp = gpar(fontsize = 11, col = "black", fontfamily = "serif",
                          fontface = "bold"))
grid::grid.text("Sex (reference: Female)", 
                x = unit(0.235, "npc"), y = unit(0.1, "npc"), 
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
  write_csv("lucid_conjoint_p_amces_pid.csv")


### ---- Fig. S37-S38: heterogeneity by ideology ------
tmp <- 
  lucid_conjoint_p_long %>%
  mutate(liberal = case_when(x_ideo_3 == "Liberal" ~ 1,
                             x_ideo_3 == "Conservative" ~ 0)) %>% 
  filter(!is.na(liberal))

## Marginal means
gg_pconjoint_mm_ideo <-
  het_mm_p(x = "liberal", long_data = tmp)

## AMCEs
gg_pconjoint_amce_ideo <-
  het_amce_p(x = "liberal", long_data = tmp)

g1 <- 
  gg_pconjoint_mm_ideo %>%
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
  gg_pconjoint_mm_ideo %>%
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
  gg_pconjoint_mm_ideo %>%
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
    g1 + labs(x = "Marginal mean: Liberals") + 
      theme(strip.text.x = element_text(size = 11,
                                        face = "bold"),
            panel.border = element_rect(fill = NA, colour = "black")),
    g2 + labs(x = "Marginal mean: Conservatives") +
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

ggsave(filename = "lucid_conjoint_p_means_ideo.pdf",
       g,
       width = 7.6,
       height = 3 + fixed + spacing * (nrow(g1$data)))

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
  write_csv("lucid_conjoint_p_means_ideo.csv")

g1 <- 
  gg_pconjoint_amce_ideo %>%
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
  gg_pconjoint_amce_ideo %>%
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
  gg_pconjoint_amce_ideo %>%
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

pdf(file = "lucid_conjoint_p_amces_ideo.pdf",
    width = 7.6,
    height = 4 + fixed + spacing * (nrow(g1$data)))
p 
grid::grid.text("Country of origin (reference: United States)", 
                x = unit(0.235, "npc"), y = unit(0.985, "npc"), 
                just = "left",
                gp = gpar(fontsize = 11, col = "black", fontfamily = "serif",
                          fontface = "bold"))
grid::grid.text("Risk of exposure (reference: Low)", 
                x = unit(0.235, "npc"), y = unit(0.726, "npc"), 
                just = "left",
                gp = gpar(fontsize = 11, col = "black", fontfamily = "serif",
                          fontface = "bold"))
grid::grid.text("Risk of serious illness (reference: Low)", 
                x = unit(0.235, "npc"), y = unit(0.626, "npc"), 
                just = "left",
                gp = gpar(fontsize = 11, col = "black", fontfamily = "serif",
                          fontface = "bold"))
grid::grid.text("Occupation group (reference: Non-essential workers)", 
                x = unit(0.235, "npc"), y = unit(0.53, "npc"), 
                just = "left",
                gp = gpar(fontsize = 11, col = "black", fontfamily = "serif",
                          fontface = "bold"))
grid::grid.text("Can work from home (reference: Yes)", 
                x = unit(0.235, "npc"), y = unit(0.38, "npc"), 
                just = "left",
                gp = gpar(fontsize = 11, col = "black", fontfamily = "serif",
                          fontface = "bold"))
grid::grid.text("Age group (reference: 18-24)", 
                x = unit(0.235, "npc"), y = unit(0.305, "npc"), 
                just = "left",
                gp = gpar(fontsize = 11, col = "black", fontfamily = "serif",
                          fontface = "bold"))
grid::grid.text("Sex (reference: Female)", 
                x = unit(0.235, "npc"), y = unit(0.1, "npc"), 
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
  write_csv("lucid_conjoint_p_amces_ideo.csv")


