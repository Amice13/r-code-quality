

# Map showing location of bases
base.data <- read.csv(here("Data", "Bases.csv"))

map.data <- map_data("world")

sample.data <- data.frame(region = c("UK", "Belgium", "Netherlands", "Spain", "Portugal", 
                                    "Germany", "Italy", "Poland", "Kuwait", "Turkey", "Japan",  
                                    "South Korea", "Philippines", "Australia"),
                          value = c(1),
                          stringAsFactors = FALSE)


ggplot() +
  geom_map(data = map.data, map = map.data, aes(x = long, y = lat, group = group, map_id = region), fill = "white", color = "black", size = 0.05) +
  geom_map(data = sample.data, map = map.data, aes(fill = value, map_id = region), fill = "gray80", color = "black", size = 0.05) +
  geom_point(data = base.data, aes(x = Longitude, y = Latitude), color = "red",  alpha = 0.4) +
  theme_void() +
  coord_equal(ratio = 1.3) +
  labs(title = "U.S. Military Facilities Around the Globe")


ggsave(here("Figures", "apsr-figure-map-us-facilities.pdf"))