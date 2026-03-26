library(terra)

# ---- paths ----
base    <- "/Users/juanbenavides/Library/CloudStorage/Dropbox/papers/peat 210 pb/stats2"
geodir  <- file.path(base, "Geographic")
plumdir <- file.path(base, "Plum_runs")
# metadata & master
cores_fp <- file.path(base, "cores.csv")
ages_fp  <- file.path(plumdir, "AGES_MASTER_all_cores.csv")  # or your with_CARBON file


# ---- load cores & master ----
cores <- read_csv(cores_fp, show_col_types = FALSE) %>%
  transmute(core = as.character(core),
            lat = as.numeric(lat),
            long = as.numeric(long),
            elevation = as.numeric(elevation),
            disturbance = as.character(disturbance))

ages <- read_csv(ages_fp, show_col_types = FALSE)


# Normalize a monthly global grid to lon -180..180 (west negative), north up.
normalize_lonlat_time <- function(r, start_date = NULL, n_months = NULL,
                                  assume_lon_0_360 = TRUE, flip_lat = TRUE) {
  # 1) give it a plausible extent before rotate()
  if (assume_lon_0_360) {
    ext(r) <- ext(0, 360, -90, 90)   # many NetCDFs store lon 0..360
  } else {
    ext(r) <- ext(-180, 180, -90, 90)
  }
  crs(r) <- "EPSG:4326"

  # 2) put NORTH at the top if rows are south→north
  if (flip_lat) r <- flip(r, "vertical")

  # 3) move longitudes to -180..180
  r <- rotate(r)

  # 4) if time is missing, set monthly sequence
  if (!is.null(start_date) && !is.null(n_months)) {
    time(r) <- seq(as.Date(start_date), by = "1 month", length.out = n_months)
  }
  r
}

gpcc_p_fp <- "/Users/juanbenavides/Library/CloudStorage/Dropbox/papers/peat 210 pb/stats2/Geographic/precip.mon.total.v2020.nc"

gpcc_raw <- rast(gpcc_p_fp)                 # loads, but no georef/time
stopifnot(nlyr(gpcc_raw) == 1548)           # GPCC v2020 monthly 1891-01..2019-12

gpcc_r <- normalize_lonlat_time(
  gpcc_raw,
  start_date = "1891-01-01",
  n_months   = nlyr(gpcc_raw),
  assume_lon_0_360 = TRUE,   # GPCC uses 0..360 lon in many builds
  flip_lat         = TRUE    # GPCC often stores lat ascending (south→north)
)

# quick sanity: should show extent -180..180 x -90..90 and proper dates
ext(gpcc_r); crs(gpcc_r); head(time(gpcc_r))


plot(gpcc_r[[1]])                     # should show Africa/upright continents
points(cores$long, cores$lat, pch=20,col="red") # points land where expected

# If still wrong in one axis ONLY:
#   North/South flipped  -> gpcc_r <- flip(gpcc_r, "vertical")
#   East/West mirrored   -> gpcc_r <- flip(gpcc_r, "horizontal")
#   Longitudes 0..360    -> gpcc_r <- rotate(gpcc_r)

# If your BE code already defines this helper, reuse it
library(terra)
library(dplyr)
library(tidyr)

# r  = SpatRaster with a proper time vector (monthly)
# pts_df = data.frame(core, long, lat) for ONLY the cores you want (can be all 21, or just one)
# value_name = name for the extracted variable (e.g., "pre_mm", "tavg_C")
extract_monthly_by_core <- function(r, pts_df, value_name) {
  stopifnot(all(c("core","long","lat") %in% names(pts_df)))
  dates <- as.Date(time(r))
  ncores <- nrow(pts_df)

  # Build points and extract; keep the ID column
  pts <- vect(pts_df[, c("long","lat")], geom = c("long","lat"), crs = "EPSG:4326")
  ex  <- terra::extract(r, pts, ID = TRUE)  # first column 'ID' = row index of pts

  # Long format, then join IDs back to core names
  id_map <- tibble(ID = seq_len(ncores), core = pts_df$core)

  out <- as_tibble(ex) %>%
    pivot_longer(-ID, names_to = "band", values_to = value_name) %>%
    mutate(date = rep(dates, times = ncores)) %>%  # <-- correct alignment
    left_join(id_map, by = "ID") %>%
    select(core, date, !!value_name) %>%
    arrange(core, date)

  # Guarantee uniqueness (in case of duplicate bands/IDs)
  out %>% group_by(core, date) %>%
    summarise(!!value_name := dplyr::first(.data[[value_name]]), .groups = "drop")
}

pre_m  <- extract_monthly_by_core(gpcc_r,   cores, "pre_mm")
#tavg_m <- extract_monthly_by_core(be_tavg_r, cores, "tavg_C")

annualize <- function(df, var, fun = c("mean","sum")) {
  fun <- match.arg(fun)
  df %>%
    mutate(year = lubridate::year(date)) %>%
    group_by(core, year) %>%
    summarise(!!var := if (fun=="mean") mean(.data[[var]], na.rm=TRUE)
                        else            sum(.data[[var]],  na.rm=TRUE),
              .groups = "drop")
}

pre_y  <- annualize(pre_m,  "pre_mm",  "sum")  %>% filter(year >= 1900)


tavg_y <- annualize(tavg_m, "tavg_C",  "mean") %>% filter(year >= 1900)


