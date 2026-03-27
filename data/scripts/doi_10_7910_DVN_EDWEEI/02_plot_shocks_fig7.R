# Alexander F. Gazmararian
# agazmararian@gmail.com

library(here)
library(tidyverse)
library(viridis)
library(sf)
library(tigris)

g <- readRDS(here("Data", "output", "ces", "cespanel.rds"))

counties <- tigris::counties(year = "2010", progress_bar = FALSE)
counties$countyfips <- with(counties, as.numeric(paste0(STATEFP10, COUNTYFP10)))

# Simplify geometry to reduce file size (tolerance in meters for projected data)
counties <- st_simplify(counties, dTolerance = 1000)

p_out <- g %>%
  group_by(countyfips, year) %>%
  summarize(
    fires = mean(fire, na.rm = TRUE),
    temp = mean(tanom, na.rm = TRUE)
  ) %>%
  mutate(
    fires = ifelse(fires == 1, "Treated", "Control"),
    temp = ifelse(temp == 1, "Treated", "Control")
  ) %>%
  left_join(., counties, by = "countyfips") %>%
  filter(STATEFP != "02" & STATEFP != "15") %>%
  pivot_longer(cols = c(fires, temp)) %>%
  mutate(
    labs = case_when(
      year == 2010 & name == "fires" ~ "(b) 2010 fires",
      year == 2010 & name == "temp" ~ "(a) 2010 extreme heat anomalies",
      year == 2012 & name == "fires" ~ "(d) 2012 fires",
      year == 2012 & name == "temp" ~ "(c) 2012 extreme heat anomalies",
      year == 2014 & name == "fires" ~ "(f) 2014 fires",
      year == 2014 & name == "temp" ~ "(e) 2014 extreme heat anomalies"
    )) %>%
  ggplot() +
  geom_sf(aes(fill = value, geometry = geometry), linewidth = 0.05) +
  scale_fill_manual(values = c("lightgrey", "blue")) +
  facet_wrap(~ labs, ncol = 2, nrow = 3) +
  theme_void(base_size = 14) +
  labs(x = "", fill = "", y = "") +
  theme(
    legend.position.inside = c(0.5, 0),
    axis.title = element_blank(),
    axis.text = element_blank(),
    axis.ticks = element_blank(),
    axis.line = element_blank(),
    plot.title = element_text(hjust = .5),
    panel.background = element_blank(),
    legend.text = element_text(color = "black"),
    legend.title = element_text(color = "black"),
    strip.text = element_text(size = 14, margin = margin(b = 10), color = "black"),
    panel.spacing.y = unit(2, "lines")
  )

ggsave(
  p_out,
  filename = here("Output", "figures", "fig_7_usa_shocks.pdf"),
  scale = 1.5, width = 6.5, height = 5,
  device = cairo_pdf
)
message("Created Figure 7. USA extreme heat and wildfire anomalies.")
