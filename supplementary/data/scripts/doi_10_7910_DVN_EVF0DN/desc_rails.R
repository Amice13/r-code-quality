### descriptives: railways
library(here)
library(sf)
library(tidyverse)

sf_use_s2(FALSE)

### load segment data
rail.sf <- st_read(here("analysis_data", "RShapes.geojson"))
back.sf <- readRDS(here("analysis_data","spatial_extent.rds"))

# Combine railway lines per year
rail.sf <- rail.sf %>% 
  group_by(year) %>% 
  summarise() %>% 
  ungroup()

# Clip within segments
rail.sf <- rail.sf %>% 
  st_intersection(
    back.sf
  )

# Create decades variable
round_down <- function(x) x %/% 10 * 10
rail.sf <- rail.sf %>% 
  mutate(decade = paste0(round_down(year), "s"))

# Plot
ggplot() +
  geom_sf(data = back.sf, fill = "gray80", linewidth = 0.1) +
  geom_sf(data = rail.sf, 
          aes(col = decade), linewidth = 0.09) +
  scale_color_viridis_d(option = "B", #breaks = scales::pretty_breaks(n = 4),
                       na.value = "transparent", begin = 0, end = 0.91,
                        guide = guide_legend(override.aes = list(linewidth = 3))
                       ) +
  labs(col = "Year of line construction") +
  theme_minimal() +
  theme(panel.grid = element_blank(),
        legend.position = "bottom",
        legend.box.margin = margin(-15, 0, -2, 0),
        axis.text = element_blank(),
        axis.ticks = element_blank())
ggsave(here("figures", "figure_2.pdf"),
       width = 6, height = 5)


# Clean up ----
rm(back.sf,  rail.sf)
