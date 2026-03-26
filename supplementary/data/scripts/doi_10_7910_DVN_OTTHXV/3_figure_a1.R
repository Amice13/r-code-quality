sikap_election_1 <- read_csv("./_3_data/sikap11.csv") %>%
  mutate(week = as.character(week))
sikap_election_2 <- read_csv("./_3_data/sikap1213.csv")


age_vote_pref <-
  bind_rows(sikap_election_1, sikap_election_2) %>%
  filter(week %in% c("11", "12-13")) %>%
  mutate(birthyear = 2024 - as.numeric(demog_age),
         age_cat = case_when(birthyear %in% seq(1997, 2012, 1) ~ "Gen-Z\n(1997\u2013)",
                             birthyear %in% seq(1981, 1996, 1) ~ "Millenials\n(1981\u201396)",
                             birthyear %in% seq(1965, 1980, 1) ~ "Gen-X\n(1965\u201380)",
                             birthyear %in% seq(1944, 1964, 1) ~ "Boomers\n(1944\u201364)"),
         vote_choice = coalesce(polpref_votepres),
         vote_choice = case_when(vote_choice == 1 ~ "Anies Baswedan",
                                 vote_choice == 2 ~ "Prabowo Subianto",
                                 vote_choice == 3 ~ "Ganjar Pranowo",
                                 vote_choice == 4 ~ NA_character_),
         
         week = case_when(week == "11" ~ "SIKAP pre-election (Feb 4\u201311)",
                          week == "12-13" ~ "SIKAP post-election (Feb 15\u201325)")) %>%
  group_by(week, vote_choice, age_cat) %>%
  summarise(val = n()) %>%
  filter(vote_choice != 4, !is.na(vote_choice)) %>%
  group_by(week, age_cat) %>%
  mutate(tot = sum(val),
         prop = val/tot) %>%
  bind_rows(., data.frame(week = rep("Indikator exit poll (Feb 14)", 12), 
                          vote_choice = c(rep("Anies Baswedan", 4), rep("Prabowo Subianto", 4), rep("Ganjar Pranowo", 4)),
                          age_cat = rep(c("Boomers\n(1944\u201364)", "Gen-X\n(1965\u201380)", "Millenials\n(1981\u201396)", "Gen-Z\n(1997\u2013)"), 3),
                          val = rep(NA_real_, 12),
                          tot = rep(NA_real_, 12),
                          prop = c(0.245, 0.285, 0.26, 0.197, 0.471, 0.506, 0.605, 0.71, 0.284, 0.209, 0.135, 0.093))) %>%
  mutate(vote_choice = factor(vote_choice, levels = c("Anies Baswedan", "Prabowo Subianto", "Ganjar Pranowo")),
         age_cat = factor(age_cat, levels = c("Boomers\n(1944\u201364)", "Gen-X\n(1965\u201380)", 
                                              "Millenials\n(1981\u201396)", "Gen-Z\n(1997\u2013)"))) %>%
  ggplot(aes(x=fct_rev(age_cat), y = prop*100, fill = factor(vote_choice))) +
  geom_bar(stat = 'identity', color = "black", position = "dodge") +
  facet_wrap(fct_rev(week)  ~ .) +
  geom_text(aes(label = paste0(round(prop*100, digits = 0), "%"), y = -3), size = 2, position = position_dodge(width = 1)) +
  scale_fill_grey() +
  theme_bw()+ 
  theme(axis.line.x.bottom = element_line(color = "black"),
        axis.line.y.left = element_blank(),
        panel.grid.major = element_blank(),
        panel.grid.minor.x = element_blank(),
        legend.position = "bottom",
        strip.background = element_blank(),
        axis.title = element_blank(),
        legend.title = element_blank(),
        panel.border = element_blank(),
        axis.ticks = element_blank(),
        axis.text.x = element_text(size = 8))

ggsave("./_4_outputs/figures/figure_a1.pdf", age_vote_pref, width = 190, height = 95, units = "mm", dpi = 1600)



