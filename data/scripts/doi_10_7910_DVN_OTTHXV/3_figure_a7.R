laws <- read_csv("./_3_data/laws.dprd2.csv")

law_figure <-
  laws %>%
  mutate(title = toupper(title)) %>%
  group_by(year) %>%
  summarise(val = mean(str_detect(title, " HUTAN|LINGKUNGAN|SUASANA"), na.rm = T),
            val2 = mean(str_detect(title, "BANGUN"), na.rm = T),
            num = n()) %>%
  pivot_longer(cols = val:val2) %>%
  mutate(name = case_when(name == "val" ~ "Environment",
                          name == "val2" ~ "Economic Development")) %>%
  filter(year > 1980, year < 2018) %>%
  ggplot(aes(x=year, y = value*100, color = name)) +
  geom_point(aes(size = num)) +
  geom_line() +
  ylab("Local Laws Mentioning Issue (%)") +
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
  guides(color=guide_legend(nrow=2,byrow=TRUE),
         size=guide_legend(nrow=2,byrow=TRUE)
  )


candidates_2019 <- read_csv("./_3_data/full_data_merged_w_codes021119.csv") %>% select(level, platform) %>% mutate(year = 2019)
candidates_2024 <- read_csv("./_3_data/candidate_pileg_data_2024.csv") %>% select(level, platform) %>% mutate(year = 2024)
Sys.setlocale("LC_ALL", "C")

platform_position <-
  bind_rows(candidates_2019, candidates_2024) %>%
  mutate(platform = str_replace_all(platform,"[^[:graph:]]", " ")) %>%
  mutate(platform = toupper(platform)) %>%
  group_by(level, year) %>%
  summarise(environment = mean(str_detect(platform, "LINGKUNGAN|HUTAN"), na.rm = T),
            development = mean(str_detect(platform, "BANGUN"), na.rm = T),
            health = mean(str_detect(platform, "SEHAT"), na.rm = T)) %>%
  pivot_longer(cols = environment:health) %>%
  mutate(name = factor(name, levels = c("development", "health", "environment"))) %>%
  filter(level == "DPRD-II") %>%
  ggplot(aes(x=year, y = value*100, group = name, fill = name)) +
  geom_bar(stat = "identity", position = "dodge", color = "black") +
  scale_x_continuous(breaks = c(2019, 2024)) +
  scale_fill_manual(values = c("darkgrey", "blue", "darkgreen")) +
  theme_bw() +
  ylab("Candidates Mentioning Issue in Platform (%)") +
  theme(panel.grid.minor = element_blank(), 
        panel.grid.major.x = element_blank(),
        #axis.text.x = element_text(angle = 90),
        #axis.line.y.left = element_blank(),
        strip.background = element_blank(),
        #text = element_text(size=30),
        axis.line = element_line(colour = "black"),
        panel.border = element_blank(),
        axis.title.x = element_blank(),
        legend.position = "bottom",
        legend.title = element_blank()) +
  geom_text(aes(label = paste0(round(value*100, 0), "%"), x = year, y = value*100 + 1), position = position_dodge(width = 4.5))






ggsave("./_4_outputs/figures/figure_a7a.pdf", plot = law_figure, width = 4, height = 4)
ggsave("./_4_outputs/figures/figure_a7b.pdf", plot = platform_position, width = 4, height = 4)

