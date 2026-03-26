###############
### FIGURES ###
###############

options(warn = -1)

Data_Party %>%
  select(country, cabinet, major, national_minor,
         party_x, party_y,
         bureau_attention_x, bureau_attention_y) %>%
  pivot_longer(
    c(party_x, party_y, bureau_attention_x, bureau_attention_y),
    names_to = c(".value", "set"),
    names_pattern = "(.*)_(x|y)"
  ) %>%
  select(-set) %>%
  filter(cabinet == "Merkel I") %>%
  group_by(cabinet, national_minor) %>%
  mutate(bureau_responsibility = bureau_attention / sum(bureau_attention)) %>%
  mutate(bureau_responsibility = if_else(party == "SPD", -bureau_responsibility, bureau_responsibility)) %>%
  summarise(major = first(major),
            bureau_attention = sum(bureau_attention),
            bureau_responsibility = sum(bureau_responsibility)) %>%
  
  mutate(bureau_responsibility = if_else(bureau_responsibility == 0, NA_real_, bureau_responsibility),
         minor = factor(str_pad(as.integer(national_minor %% 100), 2, "left", "0")),
         bureau_responsibility_na = if_else(is.na(bureau_responsibility), TRUE, NA)) %>%
  
  ggplot(aes(y = fct_rev(minor), x = bureau_responsibility)) +
  geom_hline(aes(yintercept = minor), color = "grey") +
  geom_point(shape = 1) +
  
  scale_x_continuous(breaks = c(-1, 1),  labels = c("SPD", "CDU/CSU")) +
  facet_wrap(.~major, ncol = 7, strip.position = "right") +
  
  labs(y = "Minor Topics\n", x = "\nParty Responsibility\n") +
  theme_bw() +
  theme(panel.grid = element_blank(),
        strip.background = element_rect(fill = "black"),
        strip.text = element_text(colour = "white"),
        #strip.position = "right",
        axis.text.x = element_text(size = rel(0.8)),
        axis.text.y = element_text(size = rel(0.7)),
        axis.ticks.y = element_blank())

ggsave("figure1.pdf", width = 180, height = 297, units = "mm")

# Figure 2
Data_Party_Pos %>%
  mutate(country = factor(country,
                          levels = c("DK", "DE", "NL"),
                          labels = c("Denmark", "Germany", "The Netherlands"))) %>%
  ggplot(aes(x = sharedness, y = ..density..)) +
  geom_histogram(binwidth = 0.1, fill = "black", color = "white") +
  scale_x_continuous(expand = c(0, 0)) +
  scale_y_continuous(expand = c(0, 0), limits = c(0, 7.5)) +
  facet_wrap(~country) +
  labs(x = "Sharedness", y = "Density") +
  theme_bw() +
  theme(strip.background = element_rect(fill = "black"),
        strip.text = element_text(colour = "white"))

ggsave("figure2.pdf", width = 16, height = 8, units = "cm")


#Figure 3
position_diff_bounds_01 <-
  johnson_neyman(qbin.sh.3.cntry, pred = "position_diff", modx = "attention_total",
                 vmat = vcovCL(qbin.sh.3.cntry, cluster = ~country),
                 alpha = 0.1, plot = FALSE)$bounds
position_diff_bounds_005 <-
  johnson_neyman(qbin.sh.3.cntry, pred = "position_diff", modx = "attention_total",
                 vmat = vcovCL(qbin.sh.3.cntry, cluster = ~country),
                 alpha = 0.05, plot = FALSE)$bounds

expand_grid(
  position_diff = seq(
    min(Data_Party_Pos$position_diff, na.rm = TRUE),
    max(Data_Party_Pos$position_diff, na.rm = TRUE),
    length.out = 100),
  attention_total = seq(
    min(Data_Party_Pos$attention_total, na.rm = TRUE),
    max(Data_Party_Pos$attention_total, na.rm = TRUE),
    length.out = 100)
) %>%
  mutate(country = "DK",
         ministries_total = median(Data_Party_Pos$ministries_total, na.rm = TRUE),
         ministries_diff = median(Data_Party_Pos$ministries_diff, na.rm = TRUE)) %>%
  
  mutate(sharedness = predict(qbin.sh.3.cntry, newdata = ., type = "response")) %>%
  
  ggplot(aes(x = position_diff, y = attention_total, z = sharedness)) +
  geom_rect(aes(xmin=0, xmax=3,
                ymin=max(0, position_diff_bounds_005[1]), ymax=max(0, position_diff_bounds_005[2])),
            fill = "grey80") +
  geom_rect(aes(xmin=0, xmax=3,
                ymin=max(0, position_diff_bounds_01[1]), ymax=max(0, position_diff_bounds_01[2])),
            fill = "grey50") +
  
  geom_contour(color = "black", binwidth = 0.025, linetype = "dashed", size = 0.3) +
  geom_contour(color = "black", binwidth = 0.1) +
  geom_text_contour(aes(z = sharedness), stroke = 0.4) +
  geom_rug(data = Data_Party_Pos, alpha = 0.5) +
  
  theme_bw() +
  theme(panel.grid = element_blank()) +
  scale_y_continuous(limits = c(0, 0.2)) +
  scale_x_continuous(limits = c(0, 3)) +
  labs(y = "Issue Salience", x = "Policy Conflict")

ggsave("figure3.pdf", width = 16, height = 10, units = "cm")

rm(position_diff_bounds_005, position_diff_bounds_01)

#Figure A1
Data_Party_Pos %>%
  group_by(country) %>%
  mutate(position_diff_terc = cut(position_diff,
                                  c(-1, quantile(position_diff, c(0.333, 0.666, 1), na.rm = TRUE)),
                                  include.lowest = TRUE,
                                  labels = c("T1", "T2", "T3"),
                                  ordered_result = FALSE),
  ) %>%
  filter(!is.na(position_diff_terc)) %>%
  ggplot(aes(x = attention_total, fill = position_diff_terc)) +
  geom_histogram(position = "stack", binwidth = 0.025) +
  scale_y_sqrt(expand = c(0,0)) +
  scale_x_continuous(limits = c(-0.025, 0.4)) +
  scale_fill_grey() +
  theme_bw() +
  labs(fill = "Policy Conflict", x = "Issue Salience", y = "sqrt(count)") +
  facet_wrap(~country, ncol = 3)

ggsave("figureA1.pdf", width = 16, height = 8, units = "cm")
