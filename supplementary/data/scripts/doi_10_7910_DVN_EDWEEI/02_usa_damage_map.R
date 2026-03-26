# Alexander F. Gazmararian
# agazmararian@gmail.com

library(mapproj)
library(here)
library(tidyverse)
library(colorspace)
library(ggplot2)
library(stringi)

# Load damage data
damages <- readRDS(here("Data", "inter", "usa", "hsiang_damage_processed.rds"))

# Load county map data
counties <- map_data("county")

# Function to standardize location names for merging
standardize_names <- function(x) {
  x <- gsub("de ", "de", x)
  x <- gsub("la ", "la", x)
  x <- gsub("le ", "le", x)
  x <- gsub("du ", "du", x)
  x <- gsub(" city", "", x)
  return(x)
}

# Prepare damage data for merging with map
damages$region <- tolower(state.name[match(damages$state, state.abb)])
damages$subregion <- tolower(gsub(" County| Parish", "", damages$county))
damages$subregion <- gsub("\\.", "", damages$subregion)
damages$subregion <- gsub("saint", "st", damages$subregion)
damages$subregion <- gsub("'", "", damages$subregion)
damages$subregion <- stringi::stri_trans_general(str = damages$subregion, id = "Latin-ASCII")
damages$subregion <- standardize_names(damages$subregion)

# Handle special cases
damages[damages$subregion == "district of columbia", ]$subregion <- "washington"
damages[damages$county == "District of Columbia", ]$region <- "district of columbia"

# Standardize county names using the same function
counties$subregion <- standardize_names(counties$subregion)

# Merge county data with damage data
damage_map <- merge(counties, damages, by = c("subregion", "region"), all.x = TRUE)

# Check counties with missing damage data (for debugging)
missing_counties <- subset(damage_map, is.na(totaldamage), select = c(subregion, region)) %>% unique()
if(nrow(missing_counties) > 0) {
  cat("Counties with missing damage data:", nrow(missing_counties), "\n")
}

# Create and save map
damage_map_plot <- damage_map %>%
  mutate(totaldamage = (100 + (totaldamage * -1)) / 100) %>%
  arrange(group, order) %>%
  ggplot() +
  geom_polygon(aes(x = long, y = lat, group = group, fill = totaldamage), 
               color = "black", linewidth = 0.05) +
  coord_map(projection = "lambert", lat = 52, lon = 10) +
  scale_fill_gradientn(
    colors = c("darkred", "red", "orange", "yellow", "lightgreen", "darkgreen"),
    values = scales::rescale(c(0.75, 0.85, 0.95, 1.00, 1.05, 1.15)), 
    breaks = c(0.75, 0.8, 0.85, 0.9, 0.95, 1, 1.05, 1.10, 1.15),  
    labels = c("0.75", "0.8", "0.85", "0.9", "0.95", "1", "1.05", "1.10", "1.15"), 
    name = "", 
    na.value = "white") +
  theme_bw() +
  scale_x_continuous(expand = c(0,0)) +
  scale_y_continuous(expand = c(0,0)) +
  theme(
    legend.position = "bottom",
    legend.direction = "horizontal", 
    panel.background = element_blank(), 
    panel.border = element_blank(),
    axis.title = element_blank(),  
    axis.text = element_blank(),  
    axis.ticks = element_blank(),  
    legend.key.height = unit(0.1, "in"),  
    legend.key.width = unit(0.8, "in"),  
    panel.grid.major = element_line(color = "white", linewidth = 0),
    panel.grid.minor = element_line(color = "white", linewidth = 0),
    plot.margin = margin(0, 0, 0, 0, "cm"),
    panel.spacing = margin(0, 0, 0, 0, "cm")
  )

ggsave(damage_map_plot, filename = here("Output", "figures", "fig_6_us_damage_map.pdf"), 
       scale = 1, width = 5, height = 3.3)
message("Created Figure 6. USA damage map.")