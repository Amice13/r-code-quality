# Alexander F. Gazmararian
# agazmararian@gmail.com

library(exactextractr)
library(ggplot2)
library(here)
library(raster)
library(rnaturalearthdata)
library(sf)
library(terra)
library(tidyverse)
library(tmap)

# Figure 2: sample and economic projections
g <- readRDS(here("Data", "inter", "19_wrp", "19_wrp_noweights.rds"))

# Create damage raster
damage <- readRDS(here("Data", "inter", "grid", "tempdamage.rds"))
damage <- subset(damage, select = c(lon, lat, gdp_50))
points <- vect(damage, geom = c("lon", "lat"), crs = crs(map_units110))
xmin <- -180
xmax <- 180
ymin <- -90
ymax <- 90
raster_1x1 <- rast(nrows = 180, ncols = 360, resolution = 1, xmin = xmin, xmax = xmax, ymin = ymin, ymax = ymax, crs = crs(map_units110))
r <- rasterize(points, raster_1x1, field = "gdp_50", fun = "mean")

mapdf <- map_units50
mapdf <- subset(map_units50, geounit != "Antarctica")
mapdf_unsampled <- subset(mapdf, !iso_a3_eh %in% g$gid_0)
r <- terra::crop(r, mapdf)

r <- terra::project(r, "+proj=moll")
mapdf <- st_transform(mapdf, "+proj=moll")
mapdf_unsampled <- st_transform(mapdf_unsampled, "+proj=moll")

raster_df <- as.data.frame(r, xy = TRUE)  # Convert raster to data frame for ggplot
names(raster_df)[3] <- "mean"  

gg_out <- ggplot() +
  geom_tile(data = raster_df, aes(x = x, y = y, fill = mean)) +
  scale_fill_gradientn(
    colors = c("darkred", "red", "orange", "yellow", "lightgreen", "darkgreen"),
    values = scales::rescale(c(0.96, 0.98, 0.99, 1.00, 1.01, 1.02)), 
    breaks = c(0.97, 0.98, 0.99, 1.00, 1.01, 1.02),  
    labels = c("0.97", "0.98", "0.99", "1.00", "1.01", "1.02"), 
    name = ""  # Remove title if desired
  ) +
  
  geom_sf(data = mapdf_unsampled, fill = "grey", color = NA, show.legend = FALSE) +
  geom_sf(data = mapdf, fill = NA, color = "black") +
  coord_sf(expand = FALSE) + 
  theme_bw() +  
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
    plot.margin = margin(0.2, -1.5, 0, -1.5, "cm")
  )

ggsave(
  filename = "Output/figures/fig_2_global_sample_damage.pdf",
  plot = gg_out,
  width = 5.5, height = 3, units = "in"
)
