suppressPackageStartupMessages({
  library(terra); library(sf); library(dplyr); library(stringr); library(grid); 
  library(tidyterra); library(ggplot2); library(patchwork); library(scales)
})

# ------------ paths (edit) ------------
geo_dir   <- "/Users/juanbenavides/Library/CloudStorage/Dropbox/papers/Paramo carbon map/GEographic"
stats_dir <- "/Users/juanbenavides/Library/CloudStorage/Dropbox/papers/Paramo carbon map/stats2"
setwd(stats_dir)

soc_path <- file.path(stats_dir, "SOC_pred_final_RF_GAM.tif")
ci_path  <- file.path(stats_dir, "SOC_boot_ci_pm.tif")     # <- or two files below
ci_lo    <- file.path(stats_dir, "SOC_CI95_low.tif")
ci_hi    <- file.path(stats_dir, "SOC_CI95_high.tif")

sa_path    <- file.path(geo_dir, "south_am_co.shp")
andes_path <- file.path(geo_dir, "andes_union.shp")

# ------------ data ------------
soc <- rast(soc_path)
ci_w<- rast(ci_path)

# outlines
sa    <- vect(sa_path)
andes <- vect(andes_path)

# Try to pick Colombia polygon from whatever name-column exists
# Clean and select Colombia by the 'name' field
sa$name <- trimws(as.character(sa$name))
col_poly <- sa[sa$name == "Colombia", ]
if (nrow(col_poly) == 0) stop("No feature with name == 'Colombia' found.")
if (nrow(col_poly) > 1) col_poly <- terra::dissolve(col_poly)  # unify if multiple parts

# Make CRS match the SOC raster
col_poly <- terra::project(col_poly, terra::crs(soc))

# align CRSs to the SOC raster
andes    <- project(andes,  crs(soc))
ci_w     <- if (!compareGeom(ci_w, soc, stopOnError=FALSE)) resample(ci_w, soc) else ci_w

# crop & mask to Colombia
andes_co <- terra::intersect(andes, col_poly)
soc_co <- mask(crop(soc,  col_poly), col_poly)
ci_co  <- mask(crop(ci_w, col_poly), col_poly)
ci_co  <- mask(ci_co, soc_co)   # <- force CI to have NA wherever SOC has NA
# ------------ styling helpers ------------
quiet_theme <- theme_void(base_size = 10) +
  theme(
    legend.position = "right",
    legend.title = element_text(size=9),
    legend.text  = element_text(size=8)
  )

# limits (robust to outliers)
soc_lim <- as.numeric(quantile(values(soc_co), probs = c(0.02, 0.98), na.rm=TRUE))
ci_lim  <- as.numeric(quantile(values(ci_co),  probs = c(0.02, 0.98), na.rm=TRUE))

# ------------ panel A: SOC ------------
p_soc <- ggplot() +
  geom_spatraster(data = soc_co) +
  scale_fill_viridis_c(
    name = expression("SOC (Mg C ha"^-1*")"),
    limits = soc_lim, oob = squish
  ) +
  geom_spatvector(data = col_poly, fill = NA, colour = "grey20", linewidth = 0.25) +
  geom_spatvector(data = andes_co,   fill = NA, colour = "grey10",  linewidth = 0.3, linetype = 3) +
  coord_sf(expand = FALSE) +
  labs(title = "a) Soil organic carbon density") +
  quiet_theme

# ------------ panel B: CI width ------------
p_ci <- ggplot() +
  geom_spatraster(data = ci_co) +
  scale_fill_viridis_c(
    name   = expression("95% interval width (Mg C ha"^-1*")"),
    option = "magma", limits = ci_lim, oob = squish
  ) +
  geom_spatvector(data = col_poly, fill = NA, colour = "grey20", linewidth = 0.25) +
  geom_spatvector(data = andes_co,   fill = NA, colour = "grey10",  linewidth = 0.3, linetype = 3) +
  coord_sf(expand = FALSE) +
  labs(title = "b) Bootstrap uncertainty") +
  quiet_theme

library(terra)
library(ggplot2)
library(tidyterra)   # for geom_spatraster / geom_spatvector

# 2) Build a zoom window AROUND the Andes (inside Colombia) – do NOT plot this buffer
zoom_km  <- 80
win_poly <- terra::buffer(andes_co, width = zoom_km * 1000)
win_poly <- terra::intersect(win_poly, col_poly)
win_ext  <- terra::ext(win_poly)                 # just the extent

# 3) Crop layers to that window (fast, and sets plotting bounds)
soc_zoom   <- terra::crop(soc,      win_ext)
soc_ci_zoom   <- terra::crop(ci_w,      win_ext)
soc_ci_zoom<-mask(soc_ci_zoom, soc_zoom)   # <- force CI to have NA wherever SOC has NA
col_zoom   <- terra::crop(col_poly, win_ext)
andes_zoom <- terra::crop(andes_co, win_ext)     # for lines, crop is enough

# 4) Plot – raster first; vectors with fill=NA so nothing covers the raster
# compute a tiny in-panel padding from the raster extent
e    <- terra::ext(soc_zoom)
xpad <- 0.02 * (e$xmax - e$xmin)
ypad <- 0.02 * (e$ymax - e$ymin)
tagx <- e$xmin + xpad
tagy <- e$ymax - ypad+1

pA <- ggplot() +
  # 1) VECTORS first so they don't cover the raster
  geom_spatvector(data = col_zoom,  fill = NA, colour = "grey25", linewidth = 0.40) +
  geom_spatvector(data = andes_zoom,            colour = "white",  linewidth = 0.45) +
  # 2) RASTER last (stays visible)
  geom_spatraster(data = soc_zoom, maxcell = 2e6) +
  scale_fill_viridis_c(
    name = expression("SOC (Mg C ha"^-1*")"),  # <-- units in legend
    na.value = NA,
    guide = guide_colorbar(
      barheight = unit(64, "pt"),
      barwidth  = unit(8,  "pt"),
      ticks     = TRUE
    )
  ) +
  coord_sf(expand = FALSE) +
  theme_void() +
  # Legend just below the tag, on the left
  theme(
    legend.position      = c(0.02, 0.86),  # lower if it touches the tag
    legend.justification = c(0, 1),
    legend.text          = element_text(size = 10),
    legend.background    = element_rect(fill = alpha("white", 0.80), colour = NA),
    legend.key.height    = unit(0.55, "lines"),
    legend.key.width     = unit(0.45, "lines"),
    legend.margin        = margin(2, 2, 2, 2)
  ) +
  # Big "A)" inside the panel at the true top-left (data coords)
  annotate("text", x = tagx, y = tagy, label = "A)", fontface = "bold",
           size = 5.5, hjust = 0, vjust = 1)

# put this at the end of your pA build:
pA <- pA +
  coord_sf(
    expand = FALSE,
    # if your map is in a projected CRS, keep it:
    crs = sf::st_crs(col_zoom),
    # show degree-style labels; requires ggplot2 >= 3.4
    label_graticule = "SW",
    label_axes = "bl"  # label Bottom & Left axes
  ) +
  theme(
    axis.text  = element_text(size = 8, colour = "grey20"),
    axis.ticks = element_line(colour = "grey60", linewidth = 0.2),
    axis.title = element_blank()
  )

pA


# Load CI raster (width in Mg C ha^-1)
#ci_r <- rast(file.path(stats_dir, "SOC_boot_ci_pm.tif"))

# Crop to the same zoom window you used for pA
# Reuse tag position computed from soc_zoom (tagx/tagy); legend sits below it
pB <- ggplot() +
  # vectors first
  geom_spatvector(data = col_zoom,  fill = NA, colour = "grey25", linewidth = 0.40) +
  geom_spatvector(data = andes_zoom,            colour = "white",  linewidth = 0.45) +
  # raster last
  geom_spatraster(data = soc_ci_zoom, maxcell = 2e6) +
  scale_fill_viridis_c(
    name = "95% CI",
    na.value = NA,
    guide = guide_colorbar(
      barheight = unit(64, "pt"),
      barwidth  = unit(8,  "pt"),
      ticks     = TRUE
    )
  ) +
  coord_sf(
    expand = FALSE,
    crs = sf::st_crs(col_zoom),
    label_graticule = "SW",
    label_axes = "bl"
  ) +
  theme_void() +
  theme(
    legend.position      = c(0.02, 0.86),
    legend.justification = c(0, 1),
    legend.text          = element_text(size = 10),
    legend.background    = element_rect(fill = scales::alpha("white", 0.80), colour = NA),
    legend.key.height    = unit(0.55, "lines"),
    legend.key.width     = unit(0.45, "lines"),
    legend.margin        = margin(2, 2, 2, 2),
    axis.text  = element_text(size = 8, colour = "grey20"),
    axis.ticks = element_line(colour = "grey60", linewidth = 0.2),
    axis.title = element_blank()
  ) +
  annotate("text", x = tagx, y = tagy, label = "B)", fontface = "bold",
           size = 5.5, hjust = 0, vjust = 1)

pB

library(patchwork); pA | pB

# ------------ combine & save ------------
fig <- p_soc + p_ci + plot_layout(widths = c(1,1))
ggsave(file.path(stats_dir, "Fig_SOC_and_CI_colombia.png"), fig, width = 9.5, height = 5.2, dpi = 300)
ggsave(file.path(stats_dir, "Fig_SOC_and_CI_colombia.pdf"), fig, width = 9.5, height = 5.2)

geo_dir   <- "/Users/juanbenavides/Library/CloudStorage/Dropbox/papers/Paramo carbon map/stats2/boot"
setwd(geo_dir)
gopt <- list(datatype = "FLT4S", gdal = c("COMPRESS=LZW","TILED=YES","BIGTIFF=YES"))

writeRaster(soc_ci_zoom,  "SOC_boot_ci_pm.tif",   overwrite = TRUE, wopt = gopt)




