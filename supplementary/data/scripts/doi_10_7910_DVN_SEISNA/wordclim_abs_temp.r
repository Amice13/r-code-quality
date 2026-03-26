library(terra)
library(dplyr)
library(readr)
library(stringr)
library(tidyr)

# ---- paths ----
base_dir <- "/Users/juanbenavides/Library/CloudStorage/Dropbox/papers/peat 210 pb/stats2"
cores_fp <- file.path(base_dir, "cores.csv")
wc_dir   <- "/Users/juanbenavides/Downloads/wc2.1_30s_tavg"

# ---- load cores ----
cores <- read_csv(cores_fp, show_col_types = FALSE) %>%
  mutate(core = as.character(core))

stopifnot(all(c("core","lat","long") %in% names(cores)))

# Heuristic guard: auto-swap lat/long if they look flipped (e.g., Andean sites but lat ~ -70)
if (median(cores$lat, na.rm = TRUE) < -60 && median(abs(cores$long), na.rm = TRUE) < 20) {
  message("Lat/long look swapped; auto-swapping columns.")
  cores <- cores %>% transmute(core, lat = long, long = lat, dplyr::everything())
}

# ---- list & sort 12 monthly files (Jan→Dec) ----
f <- list.files(wc_dir, pattern = "^wc2\\.1_30s_tavg_\\d{2}\\.tif$", full.names = TRUE)
stopifnot(length(f) == 12)
mo <- as.integer(str_match(basename(f), "_(\\d{2})\\.tif$")[,2])
f <- f[order(mo)]

# ---- build SpatRaster (12 bands) & standardize lon ----
wc <- rast(f)                     # 12-band monthly climatology
if (xmin(ext(wc)) >= 0 && xmax(ext(wc)) > 180) wc <- rotate(wc)  # just in case

# Optional: crop to a small bbox around cores to reduce IO (fast window read)
pad <- 0.5   # degrees padding
bbx <- ext(min(cores$long) - pad, max(cores$long) + pad,
           min(cores$lat)  - pad, max(cores$lat)  + pad)
wc_crop <- crop(wc, bbx)

# ---- extract monthly values at core points (bilinear) ----
pts <- vect(cores[, c("long","lat")], geom = c("long","lat"), crs = "EPSG:4326")
ex  <- terra::extract(wc_crop, pts, method = "bilinear")  # 1 row per core, 12 bands

# Bind to core names
vals <- tibble(core = cores$core) %>% bind_cols(as_tibble(ex[,-1, drop = FALSE]))

# ---- unit auto-fix: WorldClim often stores °C × 10 ----
# If median monthly value > 80, assume scale 0.1
scale_factor <- if (median(as.matrix(vals[,-1]), na.rm = TRUE) > 80) 0.1 else 1.0
vals[,-1] <- vals[,-1] * scale_factor

# ---- name monthly columns and compute annual mean ----
mon_cols <- sprintf("wc_Tmean_mon%02d_1970_2000", 1:12)
names(vals)[-1] <- mon_cols

wc_annual <- vals %>%
  mutate(wc_Tmean_ann_1970_2000 = rowMeans(across(all_of(mon_cols)), na.rm = TRUE))

# ---- join back to cores & write ----
cores_out <- cores %>% left_join(wc_annual, by = "core")

out_fp <- file.path(base_dir, "cores_with_worldclim.csv")
write_csv(cores_out, out_fp)
message("Wrote: ", out_fp)

# ---- how to use for anomaly → absolute (examples) ----
# Annual anomalies table anom_y: columns core, year, T_anom (°C)
# abs_from_anom_y <- anom_y %>%
#   left_join(cores_out %>% select(core, wc_Tmean_ann_1970_2000), by = "core") %>%
#   mutate(T_abs_C = T_anom + wc_Tmean_ann_1970_2000)

# Monthly anomalies table anom_m: columns core, date (monthly), T_anom (°C)
# library(lubridate)
# wc_mon_long <- cores_out %>%
#   select(core, starts_with("wc_Tmean_mon")) %>%
#   pivot_longer(starts_with("wc_Tmean_mon"),
#                names_to = "mon_lab", values_to = "wc_mon_C") %>%
#   mutate(mon = readr::parse_number(mon_lab))
# abs_from_anom_m <- anom_m %>%
#   mutate(mon = lubridate::month(date)) %>%
#   left_join(wc_mon_long, by = c("core","mon")) %>%
#   mutate(T_abs_C = T_anom + wc_mon_C)
# ---- packages ----
library(terra)
library(dplyr)
library(readr)
library(stringr)
library(tidyr)
library(lubridate)

# ---- paths ----
base_dir <- "/Users/juanbenavides/Library/CloudStorage/Dropbox/papers/peat 210 pb/stats2"
cores_fp <- file.path(base_dir, "cores.csv")
wc_dir   <- "/Users/juanbenavides/Downloads/wc2.1_30s_tmin"   # <- tmin folder

# ---- load cores ----
cores <- read_csv(cores_fp, show_col_types = FALSE) %>%
  mutate(core = as.character(core))

stopifnot(all(c("core","lat","long") %in% names(cores)))

# Heuristic guard: auto-swap lat/long if they look flipped
if (median(cores$lat, na.rm = TRUE) < -60 && median(abs(cores$long), na.rm = TRUE) < 20) {
  message("Lat/long look swapped; auto-swapping columns.")
  cores <- cores %>% transmute(core, lat = long, long = lat, dplyr::everything())
}

# ---- list & sort 12 monthly files (Jan→Dec) ----
f <- list.files(wc_dir, pattern = "^wc2\\.1_30s_tmin_\\d{2}\\.tif$", full.names = TRUE)
stopifnot(length(f) == 12)
mo <- as.integer(str_match(basename(f), "_(\\d{2})\\.tif$")[,2])
f <- f[order(mo)]

# ---- build SpatRaster (12 bands) & standardize lon ----
wc <- rast(f)                     # 12-band monthly climatology (tmin)
if (xmin(ext(wc)) >= 0 && xmax(ext(wc)) > 180) wc <- rotate(wc)  # just in case

# Optional: crop to bbox around cores to speed IO
pad <- 0.5   # degrees padding
bbx <- ext(min(cores$long) - pad, max(cores$long) + pad,
           min(cores$lat)  - pad, max(cores$lat)  + pad)
wc_crop <- crop(wc, bbx)

# ---- extract monthly values at core points (bilinear) ----
pts <- vect(cores[, c("long","lat")], geom = c("long","lat"), crs = "EPSG:4326")
ex  <- terra::extract(wc_crop, pts, method = "bilinear")  # 1 row per core, 12 bands

# Bind to core names
vals <- tibble(core = cores$core) %>% bind_cols(as_tibble(ex[,-1, drop = FALSE]))

# ---- unit auto-fix: some WorldClim temps are stored as °C × 10 ----
scale_factor <- if (median(as.matrix(vals[,-1]), na.rm = TRUE) > 80) 0.1 else 1.0
vals[,-1] <- vals[,-1] * scale_factor

# ---- name monthly columns and compute baseline annual mean (1970–2000) ----
mon_cols <- sprintf("wc_Tmin_mon%02d_1970_2000", 1:12)
names(vals)[-1] <- mon_cols

wc_tmin <- vals %>%
  mutate(wc_Tmin_ann_1970_2000 = rowMeans(across(all_of(mon_cols)), na.rm = TRUE))

# ---- join back to cores & write (separate file for tmin) ----
cores_tmin <- cores %>% left_join(wc_tmin, by = "core")

out_fp <- file.path(base_dir, "cores_with_worldclim_tmin.csv")
write_csv(cores_tmin, out_fp)
message("Wrote: ", out_fp)

# =======================
# Anomaly → Absolute examples for Tmin
# =======================

# Annual anomalies (one value per year): anom_y with columns: core, year, Tmin_anom (°C)
# abs_from_anom_y <- anom_y %>%
#   left_join(cores_tmin %>% select(core, wc_Tmin_ann_1970_2000), by = "core") %>%
#   mutate(Tmin_abs_C = Tmin_anom + wc_Tmin_ann_1970_2000)

# Monthly anomalies (one value per month): anom_m with columns: core, date (monthly), Tmin_anom (°C)
# Build long table of monthly baselines and match by month:
# wc_tmin_long <- cores_tmin %>%
#   select(core, starts_with("wc_Tmin_mon")) %>%
#   pivot_longer(starts_with("wc_Tmin_mon"),
#                names_to = "mon_lab", values_to = "wc_Tmin_mon_C") %>%
#   mutate(mon = readr::parse_number(mon_lab)) %>%
#   select(core, mon, wc_Tmin_mon_C)
#
# abs_from_anom_m <- anom_m %>%
#   mutate(mon = lubridate::month(date)) %>%
#   left_join(wc_tmin_long, by = c("core","mon")) %>%
#   mutate(Tmin_abs_C = Tmin_anom + wc_Tmin_mon_C)

