#load WVS survey data
wvs_data <- read_rds("./_3_data/WVS_TimeSeries_4_0.rds 2")

wvs_df <-
  wvs_data %>%
  filter(COUNTRY_ALPHA == "IDN", S020 == 2018) %>%
  group_by(B008) %>%
  summarise(val = n()) %>%
  filter(B008 %in% c(1, 2)) %>%
  mutate(sum = sum(val),
         value = val/sum) %>%
  mutate(label_axis = case_when(B008 == 1 ~ "Protect environment",
                                B008 == 2 ~ "Prioritize jobs")) %>%
  mutate(source = "WVS (2018)") %>%
  data.frame() %>%
  dplyr::select(value, label_axis, source)




sikap_df_tradeoff <-
  sikap_36 %>%
  zap_labels() %>%
  summarise(val = mean(Q268 == 1, na.rm = T),
            val_2 = mean(Q268 == 4, na.rm = T)) %>%
  pivot_longer(everything()) %>%
  mutate(label_axis = case_when(name == "val" ~ "Protect environment",
                                name == "val_2" ~ "Prioritize jobs")) %>%
  mutate(source = "SIKAP (2024)")

comparison_plot <-
  bind_rows(sikap_df_tradeoff, wvs_df) %>%
  ggplot(aes(x=label_axis, y = value*100, fill = source)) +
  geom_bar(stat = "identity", color = "black", position = "dodge") +
  theme_bw() +
  theme(
    axis.line.x.bottom = element_line(color = "black"),
    axis.line.y.left = element_line(color = "black"),
    axis.title.x = element_blank(),
    panel.grid.major = element_blank(),
    legend.position = "bottom",
    legend.title = element_blank(),
    panel.border = element_blank(),
    plot.caption = element_text(hjust = 0),
    axis.ticks = element_blank()) +
  ylab("Respondents Agreeing With Statement (%)") +
  scale_fill_grey() +
  geom_text(aes(x=label_axis, y = value*100 + 3, label = paste0(round(value*100, 1), "%")), position = position_dodge(width = 1))


ggsave("./_4_outputs/figures/figure_a2.pdf", plot = comparison_plot, width = 8, height = 5)
