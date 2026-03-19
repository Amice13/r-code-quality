


# Map showing location of bases

base.data <- read.csv(here("Data", "Bases.csv"))

map.data <- map_data("world") 

sample.data <- data.frame(region = c("UK", "Belgium", "Netherlands", "Spain", "Portugal", 
                                     "Germany", "Italy", "Poland", "Kuwait", "Turkey", "Japan",  
                                     "South Korea", "Philippines", "Australia"),
                          value = c(1),
                          stringAsFactors = FALSE)


panel.1 <- ggplot() +
  geom_map(data = map.data, map = map.data, aes(x = long, y = lat, group = group, map_id = region), fill = "white", color = "black", size = 0.05) +
  geom_map(data = sample.data, map = map.data, aes(fill = value, map_id = region), fill = "gray80", color = "black", size = 0.05) +
  geom_point(data = base.data, aes(x = Longitude, y = Latitude), color = "red",  alpha = 0.4) +
  theme_void() +
  theme(plot.margin = unit(c(0, 0, 0, 0), "cm")) +
  coord_equal(ratio = 1.3) +
  labs(title = "U.S. Military Facilities Around the Globe")


ggsave(here("Figures", "apsr-figure-map-us-facilities.pdf"))



# Troop deployment figure

deployment.data <- m1.cat.bayes$data %>%
  filter(!is.na(country)) %>% 
  group_by(country) %>% 
  summarise(troops = as.numeric(mean(exp(log_troops_2017), na.rm = TRUE)))

panel.2 <- ggplot(data = deployment.data, aes(y = troops, x = reorder(country, troops))) +
  geom_bar(stat = "identity") +
  theme_bw() +
  scale_y_continuous(labels = comma_format()) +
  scale_x_discrete(label = wrap_format(10)) +
  labs(x = "",
       y = "Count",
       title = "Total Active Duty, Guard, Reserve, and DOD Civilian Personnel")

ggsave(here("Figures", "apsr-figure-descriptive-mil-personnel.pdf"))


panel.1 <- panel.1 + labs(title = "")
panel.2 <- panel.2 + labs(title = "")

ggarrange(panel.1, panel.2,
          nrow = 2,
          labels = c("A", "B"),
          heights = c(1, 0.55))

ggsave(here("Figures", "apsr-figure-map-troops-combined.pdf"))
