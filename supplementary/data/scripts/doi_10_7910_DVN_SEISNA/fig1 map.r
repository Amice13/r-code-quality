## ==== No dplyr: setup ====
suppressPackageStartupMessages({
  library(sf)
  library(readr)
  library(ggplot2)
  library(patchwork)
})

geo_dir <- "/Users/juanbenavides/Library/CloudStorage/Dropbox/papers/peat 210 pb/stats2/Geographic"

# ---- Read data (no dplyr) ----
sa      <- st_read(file.path(geo_dir, "south_am_co.shp"),  quiet = TRUE)
andes   <- st_read(file.path(geo_dir, "andes_union.shp"),   quiet = TRUE)
paramos <- st_read(file.path(geo_dir, "paramos.shp"),      quiet = TRUE)
col_path <- if (file.exists(file.path(geo_dir, "colombia.shp"))) {
  file.path(geo_dir, "colombia.shp")
} else {
  list.files(geo_dir, pattern = "(?i)colom.*\\.shp$", full.names = TRUE)[1]
}
col     <- st_read(col_path, quiet = TRUE)

cores   <- read.csv(file.path(geo_dir, "cores.csv"), stringsAsFactors = FALSE)
cores_sf <- st_as_sf(cores, coords = c("long","lat"), crs = 4326, remove = FALSE)

## --- ensure both disturbance classes are present & consistent (no dplyr) ---
cores_sf$disturbance <- trimws(cores_sf$disturbance)
cores_sf$disturbance <- ifelse(grepl("degrad", tolower(cores_sf$disturbance)),
                               "Disturbed", "Conserved")
cores_sf$disturbance <- factor(cores_sf$disturbance,
                               levels = c("Conserved","Disturbed"))



# ---- Helpers (base R only) ----
to4326 <- function(x) {
  cr <- st_crs(x)
  if (is.na(cr) || is.null(cr$epsg) || cr$epsg != 4326) st_transform(x, 4326) else x
}
make_valid <- function(x) {
  # Prefer sf::st_make_valid; otherwise try lwgeom if available; else return x
  out <- tryCatch(st_make_valid(x), error = function(e) x)
  if (any(st_is_empty(out))) out <- out[!st_is_empty(out), , drop = FALSE]
  out
}
safe_bbox <- function(x) {
  bb <- st_bbox(x)
  if (any(is.na(bb))) stop("Layer bbox has NA values; check geometries.")
  bb
}
expand_bbox <- function(bb, dx = 0.8, dy = 0.6) {
  c(xmin = as.numeric(bb["xmin"]) - dx,
    ymin = as.numeric(bb["ymin"]) - dy,
    xmax = as.numeric(bb["xmax"]) + dx,
    ymax = as.numeric(bb["ymax"]) + dy)
}

# ---- Clean, CRS harmonize, and build zoom box (no dplyr) ----
sa      <- to4326(make_valid(sa))
andes   <- to4326(make_valid(andes))
paramos <- to4326(make_valid(paramos))
col     <- to4326(make_valid(col))

bb_par        <- safe_bbox(paramos)
bb_zoom       <- expand_bbox(bb_par, dx = 0.8, dy = 0.6)
bb_zoom_crs   <- st_bbox(bb_zoom, crs = st_crs(paramos))
zoom_sfc      <- st_as_sfc(bb_zoom_crs)               # ← no NA now
stopifnot(!any(is.na(st_bbox(zoom_sfc))))

# ---- Crop to zoom ----
sa_zoom      <- st_crop(sa,      st_bbox(zoom_sfc))
andes_zoom   <- suppressWarnings(st_crop(andes,   st_bbox(zoom_sfc)))
paramos_zoom <- st_crop(paramos, st_bbox(zoom_sfc))
col_zoom     <- st_crop(col,     st_bbox(zoom_sfc))

## --- keep points that INTERSECT the zoom (on-edge points included) ---
sel <- st_intersects(cores_sf, zoom_sfc, sparse = FALSE)[,1]
cores_zoom <- cores_sf[sel, , drop = FALSE]

# ---- Inset data (north of 10° S) ----
north_bbox   <- c(xmin = -90, xmax = -45, ymin = -10, ymax = 15)
sa_north      <- st_crop(sa,      north_bbox)
andes_north   <- suppressWarnings(st_crop(andes,   north_bbox))
col_north     <- st_crop(col,     north_bbox)
paramos_north <- st_crop(paramos, north_bbox)
zoom_rect     <- st_as_sfc(st_bbox(bb_zoom_crs))

# ---- Plot (unchanged aesthetic; still no dplyr) ----
pal_points <- c(Conserved = "#56B4E9", Disturbed = "#E69F00")
## --- nice degree labels for axes ---

# Degree labels (keeps lengths in sync with coord_sf)
deg_lab <- function(x, kind = c("lon","lat"), digits = 0){
  kind <- match.arg(kind)
  v <- formatC(abs(x), format = "f", digits = digits)
  if (kind == "lon") ifelse(x < 0, paste0(v, "°W"), paste0(v, "°E"))
  else               ifelse(x < 0, paste0(v, "°S"), paste0(v, "°N"))
}


p_main <- ggplot() +
  geom_sf(data = sa_zoom,      fill = NA, color = "grey75", linewidth = 0.3) +
  geom_sf(data = col_zoom,     fill = NA, color = "black",  linewidth = 0.4) +
  geom_sf(data = andes_zoom,   fill = NA, color = "grey70", linewidth = 0.25) +
  geom_sf(data = paramos_zoom, fill = "grey90", color = "grey55", linewidth = 0.25) +

  ## --- draw sites with geom_point so legend matches exactly ---
  geom_point(data = cores_zoom,
             aes(x = long, y = lat, fill = disturbance, shape = disturbance),
             colour = "black", size = 2.6, stroke = 0.35, inherit.aes = FALSE) +

  ## give both scales the same name -> merged legend with correct shape+fill
  scale_fill_manual(values = pal_points, drop = FALSE, name = NULL) +
  scale_shape_manual(values = c(Conserved = 21, Disturbed = 24),
                     drop = FALSE, name = NULL) +

  coord_sf(xlim = c(bb_zoom["xmin"], bb_zoom["xmax"]),
           ylim = c(bb_zoom["ymin"], bb_zoom["ymax"]), expand = FALSE) +
  scale_x_continuous(n.breaks = 5, labels = function(x) deg_lab(x, "lon")) +
  scale_y_continuous(n.breaks = 5, labels = function(x) deg_lab(x, "lat")) +

  guides(
    fill  = guide_legend(override.aes = list(colour = "black", size = 3, stroke = 0.35))
    # shape guide merges automatically since titles match
  ) +
  theme_classic(base_size = 11) +
  theme(
    axis.line  = element_line(),
    axis.ticks = element_line(),
    axis.text  = element_text(color = "grey20"),
    panel.grid.major = element_line(color = "grey90", linewidth = 0.2),
    legend.position  = c(0.4,0.15),
    legend.direction      = "horizontal",
    panel.border     = element_rect(color = "black", fill = NA, linewidth = 0.6)
  )

## --- Inset extent: lon -79 to -65, lat -10 to 14 ---
lon_range <- c(-82, -60)
lat_range <- c(-15, 18)

north_bbox <- c(
  xmin = min(lon_range), xmax = max(lon_range),
  ymin = min(lat_range), ymax = max(lat_range)
)

sa_north      <- st_crop(sa,      north_bbox)
andes_north   <- suppressWarnings(st_crop(andes,   north_bbox))
col_north     <- st_crop(col,     north_bbox)
paramos_north <- st_crop(paramos, north_bbox)

# rectangle showing the zoomed area (unchanged)
zoom_rect <- st_as_sfc(st_bbox(bb_zoom_crs))

p_inset <- ggplot() +
  geom_sf(data = sa_north,      fill = "grey98", color = "grey70", linewidth = 0.25) +
  geom_sf(data = col_north,     fill = NA,       color = "black",  linewidth = 0.35) +
  geom_sf(data = zoom_rect,     fill = NA,       color = "black",  linewidth = 0.5, linetype = 2) +
  coord_sf(xlim = c(north_bbox["xmin"], north_bbox["xmax"]),
           ylim = c(north_bbox["ymin"], north_bbox["ymax"]), expand = FALSE) +
  theme_void() +
  theme(panel.border = element_rect(color = "black", fill = NA, linewidth = 0.6))

# If needed, re-compose the final figure (keep your previous p_main)
p_final <- p_main + inset_element(
  p_inset, left = 0.03, bottom = 0.62, right = 0.33, top = 0.98, align_to = "panel"
)
p_final

# pick a base width you like; height = width * 2.5
w_in  <- 4.8        # inches (example); try 4–5 in for journal columns
h_in  <- w_in * 1.3

# PDF (vector)

stats_dir <- "/Users/juanbenavides/Library/CloudStorage/Dropbox/papers/peat 210 pb/stats2/"
setwd(stats_dir)
# Vector PDF using the built-in device (no Cairo/X11 required)
ggplot2::ggsave(
  "fig01_map_vertical.pdf", p_main,
  width = w_in, height = h_in, units = "in",
  device = "pdf", useDingbats = FALSE
)
