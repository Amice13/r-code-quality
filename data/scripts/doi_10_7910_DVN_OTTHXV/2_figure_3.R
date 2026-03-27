df1 <-
  sikap_36 %>%
  zap_labels() %>%
  select(starts_with("Q267_")) %>%
  pivot_longer(everything()) %>%
  group_by(name) %>%
  summarise(val = mean(value %in% c("3", "4"), na.rm = T),
            sd = sd(value %in% c("3", "4"), na.rm = T),
            n = n()) %>%
  mutate(label_axis = case_when(name == "Q267_1" ~ "Short rainy season hurting\ncrop growth or harvest",
                                name == "Q267_2" ~ "Very hot days affecting\nability to work",
                                name == "Q267_3" ~ "Property damage due to sea\nor river flooding",
                                name == "Q267_4" ~ "Property damage or health\nimpacts from forest fires or smoke")) %>%
  filter(!is.na(label_axis)) %>%
  mutate(se = sd / sqrt(n),
         se_100 = se * 100)

p1 <- df1 %>%
  ggplot(aes(y = reorder(label_axis, -val), x = val * 100)) +
  geom_point(size = 3, color = "black") +
  geom_errorbarh(aes(xmin = val * 100 - 1.96 * se_100, xmax = val * 100 + 1.96 * se_100), height = 0.1, color = "black") +
  theme_bw() +
  theme(
    axis.line.x.bottom = element_line(color = "black"),
    axis.line.y.left = element_line(color = "black"),
    axis.text.x = element_text(color = "black"),
    axis.text.y = element_text(color = "black"),
    axis.title.x = element_text(color = "black", margin = margin(t = 15)),  # Space between title and labels
    axis.title.y = element_blank(),
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank(),
    plot.title = element_text(hjust = 0.5),
    text = element_text(size = 10),
    panel.border = element_rect(color = "black", linewidth = 1.25),
    legend.position = "none",
    plot.caption = element_text(hjust = 0, color = "black"),
    axis.ticks = element_blank()) +
  ggtitle("(a) Concrete issues\n") +
  xlab('Believe the issue is "very important"') +
  scale_x_continuous(breaks = c(80, 85, 90, 95), 
                     labels = c("80%", "85", "90", "95%"), 
                     limits = c(78, 95))


p2 <-
  sikap_36 %>%
  zap_labels() %>%
  summarise(val = mean(Q268 == 1, na.rm = T),
            val_2 = mean(Q268 == 4, na.rm = T)) %>%
  pivot_longer(everything()) %>%
  mutate(label_axis = case_when(name == "val" ~ "The Indonesian government should\nbe more active in protecting\nour environment, even though it\nmay slow down economic development.",
                                name == "val_2" ~ "The Indonesian government should\nprioritize economic development,\neven if it means polluting our\nenvironment.")) %>%
  ggplot(aes(x = label_axis, y = value * 100)) +
  geom_bar(stat = "identity", color = "black", fill = "lightgrey") +
  theme_bw() +
  theme(
    axis.line.x.bottom = element_line(color = "black"),
    axis.line.y.left = element_line(color = "black"),
    axis.text.x = element_text(color = "black"),
    #axis.text.y = element_text(color = "black"),
    axis.title.x = element_blank(),
    axis.title.y = element_text(color = "black", margin = margin(r = 15)),  # Space between y-axis title and labels
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank(),
    text = element_text(size = 10),
    axis.text.y = element_blank(),
    plot.title = element_text(hjust = 0.5),
    panel.border = element_rect(color = "black", linewidth = 1.25),
    legend.position = "none",
    plot.caption = element_text(hjust = 0, color = "black"),
    axis.ticks = element_blank()) +
  ggtitle("(b) Policy trade-offs\n") +
  ylab("Respondents agreeing with statement (%)") +
  geom_text(aes(x = label_axis, y = value * 100 + 3, label = paste0(round(value * 100, 1), "%")), color = "black")






p <- ggarrange(p1, NULL,
               ggarrange(
                 NULL, p2, 
                 ncol = 2,
                 widths = c(0.24, 1)),
               ncol = 1,
               heights = c(1, 0.1, 1))


p


ggsave("./_4_outputs/figures/figure_3.pdf", plot = p, width = 7, height = 9)
write_csv(df1,"./_4_outputs/supplementary/figure_3.csv")