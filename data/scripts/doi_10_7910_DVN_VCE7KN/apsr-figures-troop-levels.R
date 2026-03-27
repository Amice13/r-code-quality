



# Troop deployment figure

deployment.data <- m1.cat.bayes$data %>%
  filter(!is.na(country)) %>% 
  group_by(country) %>% 
  summarise(troops = as.numeric(mean(exp(log_troops_2017), na.rm = TRUE)))

ggplot(data = deployment.data, aes(x = troops, y = country)) +
  geom_barh(stat = "identity") +
  theme_bw() +
  scale_x_continuous(labels = comma_format()) +
  labs(x = "Count",
       y = "",
       title = "Total Active Duty, Guard, Reserve, and DOD Civilian Personnel")

ggsave(here("Figures", "apsr-figure-descriptive-mil-personnel.pdf"))




