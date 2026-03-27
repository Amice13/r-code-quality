# ====
# Figure 1  Replication:
# Vietnamese FDI map
# input: vietnam_fdi_province_geodat.csv, vietnam_city_geo.csv
# R version 4.4.2 (2024-10-31)
# ====

# clean slate
rm(list = ls())
date()

# load packages
library(tidyverse)
library(viridis)
library(ggrepel)

# set directory
REPLICATION_DATA_DIR <- "data/"
REPLICATION_FIG_DIR <- "output/"


##########################################################
## 0. load data  ------
##########################################################
# fdi by state
vietnam.fdi.state.geodat <- read_csv(paste0(REPLICATION_DATA_DIR, "vietnam_fdi_province_geodat.csv"),
  col_types = cols(group = col_character())
)

vietnam.fdi.state.geodat %>% glimpse()
summary(vietnam.fdi.state.geodat)


# city geo code (to label Ha Noi and Ho Chi Minh)
vietnam.city.geo <- read_csv(paste0(REPLICATION_DATA_DIR, "vietnam_city_geo.csv"))

vietnam.city.geo %>% glimpse()

# prepare data ------
# # create logged cumulative manufacturing FDI count
vietnam.fdi.state.geodat <- vietnam.fdi.state.geodat %>%
  mutate(count_cum.ln = log(count_cum + 1)) # leave 0 count as NA



##########################################################
## 1. plot gradual map ------
##########################################################

YEARS <- c(2007, 2012, 2017)
panel_labels <- c("(a)", "(b)", "(c)")
fdianimes <- list()

axis.title.size <- 16

LIMITS <- c(log(0 + 1), log(150 + 1))
BREAKS <- c(log(10 + 1), log(50 + 1), log(100 + 1), log(150 + 1))

for (i in seq_along(YEARS)) {
  YEAR <- YEARS[i]
  print(YEAR)

  panel_label <- panel_labels[i]

  vietnam.fdi.state.geodat_year <- vietnam.fdi.state.geodat %>%
    filter(year == YEAR)

  vietnam.fdi.state.geodat_year_filled <- vietnam.fdi.state.geodat_year %>%
    filter(!is.na(count_cum.ln))

  fdianime_i <- ggplot() +
    geom_polygon(
      data = vietnam.fdi.state.geodat_year,
      aes(x = long, y = lat, group = group),
      colour = "gray50", fill = "white", size = 0.01
    ) + # all states
    geom_polygon(
      data = vietnam.fdi.state.geodat_year_filled,
      aes(x = long, y = lat, group = group, fill = count_cum.ln),
      col = "gray50", size = 0.01
    ) + # map colored states
    coord_fixed(1.3) +
    xlim(100, 112) +
    geom_text_repel(aes(label = city, x = longitude, y = latitude),
      inherit.aes = FALSE,
      data = vietnam.city.geo,
      nudge_x = 4,
      vjust = 1,
      segment.alpha = 0.5,
      size = 7
    ) + # city label
    scale_fill_viridis(
      direction = -1,
      option = "magma",
      "Cumulative \n Count",
      limits = LIMITS,
      breaks = BREAKS,
      labels = c("10", "50", "100", "150"),
      alpha = 0.8
    ) +
    theme_void() +
    theme(
      legend.position = "none",
      plot.title = element_text(
        size = axis.title.size + 6,
        face = "bold",
        hjust = 0.5,
        margin = margin(0, 0, -0.75, 0, unit = "cm")
      )
    )

  fdianimes[[i]] <- fdianime_i
}

# plot each
ggsave(fdianimes[[1]],
  file = paste0(REPLICATION_FIG_DIR, "F1_VNM_fdi_cholomap_a.pdf"),
  height = 8, width = 5
)

ggsave(fdianimes[[2]],
  file = paste0(REPLICATION_FIG_DIR, "F1_VNM_fdi_cholomap_b.pdf"),
  height = 8, width = 5
)

ggsave(fdianimes[[3]],
  file = paste0(REPLICATION_FIG_DIR, "F1_VNM_fdi_cholomap_c.pdf"),
  height = 8, width = 5
)
