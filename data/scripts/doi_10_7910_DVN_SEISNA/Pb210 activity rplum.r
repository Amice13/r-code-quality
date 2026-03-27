#R
#script to operate only with cushion bog peat accumulation for chapter 5
#uses script 6 to calculate carbon and peat mass
pcdir<-"F:/Dropbox/papers/peat 210 pb/stats"
macdir<-"/Users/juanbenavides/Library/CloudStorage/Dropbox/papers/peat 210 pb/stats2"

setwd(macdir)


require(rplum)

#read first

setwd(macdir)
library(rplum)

a<-Plum("BEL1", date.sample=2010.5, Al=0.5)

a<-Plum("BEL2", date.sample=2010.5, Al=0.5,ra.case=0, n.supp=3, 
        s.mean=24.3, s.shape=200, ask=FALSE, suggest=FALSE)

a<-Plum("CHI2", date.sample=2010.5, Al=0.5,ra.case=0, n.supp=3, 
        s.mean=24.3, s.shape=200, ask=FALSE, suggest=FALSE)

a<-Plum("CHI3", date.sample=2010.5, Al=0.5,ra.case=0, n.supp=3, s.mean=24.3, s.shape=200, ask=FALSE, suggest=FALSE)

a<-Plum("COC1",date.sample=2010.5, Al=0.5, otherdates="VLLCOC_2.csv",cc=1)
a<-Plum("COC2", date.sample=2010.5, Al=0.5,ra.case=0, n.supp=3, s.mean=24.3, s.shape=200, ask=FALSE, suggest=FALSE)
a<-Plum("DUE", date.sample=2010.5, Al=0.5,ra.case=0, n.supp=3, s.mean=24.3, s.shape=200, ask=FALSE, suggest=FALSE)


a<-Plum("IGU1", date.sample=2010.5, Al=0.5,ra.case=0, n.supp=3, ssize=5000, s.mean=24.3, s.shape=200, ask=FALSE, suggest=FALSE)
a<-Plum("IGU2", date.sample=2010.5, Al=0.5,ra.case=0, n.supp=3, ssize=5000,s.mean=24.3, s.shape=200, ask=FALSE, suggest=FALSE)

a<-Plum("LAVE",date.sample=2010.5, Al=0.5, otherdates="LAVE_C.csv",cc=1)

a<-Plum("MA", date.sample=2010.5, Al=0.5,ra.case=0, n.supp=3, s.mean=24.3, s.shape=200, ask=FALSE, suggest=FALSE)

a<-Plum("NEG", date.sample=2010.5, Al=0.5,ra.case=0, n.supp=3, s.mean=24.3, s.shape=200, ask=FALSE, suggest=FALSE)

a<-Plum("NEV",date.sample=2010.5, Al=0.5, otherdates="alfombrales.csv",cc=1)

a<-Plum("PAM2", date.sample=2010.5, Al=0.5,ra.case=0, n.supp=3, s.mean=24.3, s.shape=200, ask=FALSE, suggest=FALSE)

a<-Plum("RAB1", date.sample=2010.5, Al=0.5,ra.case=0, n.supp=3, s.mean=24.3, s.shape=200, ask=FALSE, suggest=FALSE)
a<-Plum("RAB2", date.sample=2010.5, Al=0.5,ra.case=0, n.supp=3, s.mean=24.3, s.shape=200, ask=FALSE, suggest=FALSE)
a<-Plum("RAB3", date.sample=2010.5, Al=0.5,ra.case=0, n.supp=3, s.mean=24.3, s.shape=200, ask=FALSE, suggest=FALSE)

a<-Plum("SUM1", date.sample=2010.5,Al=0.1,ra.case=0, n.supp=3, s.mean=24.3, s.shape=200, ask=FALSE, suggest=FALSE)
a<-Plum("SUM2", date.sample=2010.5,Al=0.1,ra.case=0, n.supp=3, s.mean=24.3, s.shape=200, ask=FALSE, suggest=FALSE)
a<-Plum("TOL", date.sample=2010.5,Al=0.1,ra.case=0, n.supp=3, s.mean=24.3, s.shape=200, ask=FALSE, suggest=FALSE)
a<-Plum("YAR", date.sample=2010.5,Al=0.1,ra.case=0, n.supp=3, s.mean=24.3, s.shape=200, ask=FALSE, suggest=FALSE)

core<-c("BEL1","BEL2","CHI2","CHI3","COC1","COC2","DUE",
"IGU1","IGU2","LAVE","MA","NEG","NEV","PAM2",
"RAB1","RAB2","RAB3","SUM1","SUM2","TOL","YAR")

# ---- Config ---------------------------------------------------------------
library(readr)
library(dplyr)
library(purrr)
library(stringr)

base_dir <- "/Users/juanbenavides/Library/CloudStorage/Dropbox/papers/peat 210 pb/stats2/Plum_runs"
core_master_path <- "/Users/juanbenavides/Library/CloudStorage/Dropbox/papers/peat 210 pb/stats2/cores.csv"   # must have columns: core, sections (ints)

# ---- Read core -> sections map -------------------------------------------
core_map <- read_csv(core_master_path, show_col_types = FALSE) %>%
  transmute(core = as.character(core),
            sections = as.integer(sections))

# ---- Helper to read one ages file ----------------------------------------
read_age_file <- function(core, sections, base_dir) {
  # expected path from your description
  expected <- file.path(base_dir, core, sprintf("%s_%s_ages.txt", core, sections))

  # fallback: if expected missing, try to find a single *_ages.txt in the folder
  if (!file.exists(expected)) {
    candidates <- list.files(file.path(base_dir, core),
                             pattern = "_ages\\.txt$", full.names = TRUE)
    if (length(candidates) == 1L) {
      message(sprintf("Using fallback for %s: %s", core, basename(candidates)))
      expected <- candidates
    } else {
      warning(sprintf("Ages file not found for %s (expected %s). Skipping.", core, basename(expected)))
      return(NULL)
    }
  }

  # Files are tab-delimited with headers: depth, min.95, max.95, median, mean
  df <- read_tsv(expected, show_col_types = FALSE,
                 col_types = cols(
                   depth   = col_double(),
                   `min.95`= col_double(),
                   `max.95`= col_double(),
                   median  = col_double(),
                   mean    = col_double()
                 ))

  df %>%
    transmute(
      core        = core,
      sections    = sections,
      depth_cm    = depth,
      age_min95_BP= `min.95`,
      age_max95_BP= `max.95`,
      age_median_BP = median,
      age_mean_BP = mean,
      calAD_median = 1950 - age_median_BP,   # optional: convert BP -> calendar year
      calAD_mean   = 1950 - age_mean_BP
    )
}

# ---- Build the master table ----------------------------------------------
ages_master <- map2_dfr(core_map$core, core_map$sections,
                        ~ read_age_file(.x, .y, base_dir))

# Optional sanity checks
# ages_master %>% group_by(core) %>% summarize(n_depths = n())

# ---- Save ----------------------------------------------------------------
out_path <- file.path(base_dir, "AGES_MASTER_all_cores.csv")
write_csv(ages_master, out_path)
message("Wrote: ", out_path)


# --- setup ----------------------------------------------------------------
library(readr)
library(dplyr)
library(purrr)
library(stringr)
library(tidyr)

# paths
base_dir       <- "/Users/juanbenavides/Library/CloudStorage/Dropbox/papers/peat 210 pb/stats2/Plum_runs"
ages_master_fp <- file.path(base_dir, "AGES_MASTER_all_cores.csv")   # from previous step
carbon_fp      <- "/Users/juanbenavides/Library/CloudStorage/Dropbox/papers/peat 210 pb/stats2/CORESASH.csv"

# helper: linear interpolation with constant ends
interp_const_ends <- function(x, y, xout) {
  if (length(unique(x)) < 2L) return(rep(NA_real_, length(xout)))
  approx(x = x, y = y, xout = xout, method = "linear", rule = 2, ties = mean)$y
}

# --- read data -------------------------------------------------------------
ages <- read_csv(ages_master_fp, show_col_types = FALSE)
# assumes columns: core, depth_cm, (ages ...)
stopifnot(all(c("core","depth_cm") %in% names(ages)))

carbon_raw <- read_csv(carbon_fp, show_col_types = FALSE)

# keep only what we need; average replicates at same core+depth
carbon_avg <- carbon_raw %>%
  transmute(core   = as.character(core),
            depth_cm = as.numeric(depth),   # bottom of slice in cm
            carbon = as.numeric(carbon),    # proportion 0–1
            bd     = as.numeric(bd)) %>%    # g cm^-3
  filter(is.finite(depth_cm), is.finite(carbon) | is.finite(bd)) %>%
  group_by(core, depth_cm) %>%
  summarize(carbon = mean(carbon, na.rm = TRUE),
            bd     = mean(bd,     na.rm = TRUE),
            .groups = "drop") %>%
  arrange(core, depth_cm)

# --- align & compute per core ---------------------------------------------
by_core <- split(ages, ages$core)

ages_carbon <- map_dfr(by_core, function(ad) {
  core_id <- ad$core[1]
  carb    <- carbon_avg %>% filter(core == core_id) %>% arrange(depth_cm)

  ad <- ad %>%
    arrange(depth_cm) %>%
    mutate(
      # interval thickness from previous bottom (assumes surface at 0 cm)
      depth_top_cm = dplyr::lag(depth_cm, default = 0),
      thickness_cm = pmax(depth_cm - depth_top_cm, 0)
    )

  if (nrow(carb) == 0) {
    ad$carbon_interp <- NA_real_
    ad$bd_interp     <- NA_real_
  } else {
    ad$carbon_interp <- interp_const_ends(carb$depth_cm, carb$carbon, ad$depth_cm)
    ad$bd_interp     <- interp_const_ends(carb$depth_cm, carb$bd,     ad$depth_cm)
  }

  # Carbon content per interval (g m^-2):
  #   C_g_m2 = carbon(0-1) * bd(g cm^-3) * thickness(cm) * 10,000 (cm^2/m^2)
  ad %>%
    mutate(
      C_g_m2 = if_else(
        is.finite(carbon_interp) & is.finite(bd_interp) & thickness_cm > 0,
        carbon_interp * bd_interp * thickness_cm * 10000,
        NA_real_
      )
    )
})

# optional: drop the row for depth==0 (thickness=0)
ages_carbon <- ages_carbon %>% filter(!(depth_cm == 0 & thickness_cm == 0))

# --- save ------------------------------------------------------------------
out_fp <- file.path(base_dir, "AGES_MASTER_with_CARBON_bd_and_Cgm2.csv")
write_csv(ages_carbon, out_fp)
message("Wrote: ", out_fp)


library(readr)
library(dplyr)
library(purrr)
library(tidyr)
library(stringr)

# --- paths ---------------------------------------------------------------
base_dir <- "/Users/juanbenavides/Library/CloudStorage/Dropbox/papers/peat 210 pb/stats2/Plum_runs"
ages_carbon_fp <- file.path(base_dir, "AGES_MASTER_with_CARBON_bd_and_Cgm2.csv")
cores_fp <- "/Users/juanbenavides/Library/CloudStorage/Dropbox/papers/peat 210 pb/stats2/cores.csv"

# --- read data -----------------------------------------------------------
ac  <- read_csv(ages_carbon_fp, show_col_types = FALSE)
meta <- read_csv(cores_fp, show_col_types = FALSE) %>%
  transmute(core = as.character(core),
            elevation = as.numeric(elevation),
            disturbance = as.factor(disturbance),
            lat = as.numeric(lat),
            long = as.numeric(long))

# Expected in 'ac': core, depth_cm, depth_top_cm, thickness_cm, C_g_m2,
#                   calAD_median (calendar year at bottom of interval)
stopifnot(all(c("core","depth_cm","depth_top_cm","thickness_cm","C_g_m2","calAD_median") %in% names(ac)))

# Build interval start/end years (calendar AD) for each depth interval
ac_int <- ac %>%
  arrange(core, depth_cm) %>%
  group_by(core) %>%
  mutate(
    year_top  = lag(calAD_median, default = max(calAD_median, na.rm = TRUE)), # most recent at surface
    year_bot  = calAD_median,
    # ensure chronology direction (top newer, bottom older)
    int_start = pmin(year_top, year_bot),
    int_end   = pmax(year_top, year_bot),
    int_years = pmax(int_end - int_start, 0)
  ) %>%
  ungroup() %>%
  filter(thickness_cm > 0, is.finite(C_g_m2), is.finite(int_years), int_years > 0)

# helper: compute CAR per 25-yr bin for ONE core
car_bins_one_core <- function(df, span_years = 125, bin_width = 25) {
  # df needs: int_start, int_end, int_years, C_g_m2  (built in ac_int)
  present <- max(df$int_end, na.rm = TRUE)

  edges <- seq(present - span_years, present, by = bin_width)
  if (length(edges) < 2L) return(tibble())  # nothing to bin

  bins <- tibble(
    bin_start = head(edges, -1),
    bin_end   = tail(edges,  1)
  ) %>%
    mutate(
      bin_id = sprintf("%d-%dyrBP",
                       round(present - bin_end),
                       round(present - bin_start)),
      bin_center_yBP = (present - bin_start + present - bin_end) / 2
    )

  map_dfr(seq_len(nrow(bins)), function(i) {
    bs <- bins$bin_start[i]
    be <- bins$bin_end[i]

    # temporal overlap of each interval with the bin
    overlap <- pmax(0, pmin(df$int_end, be) - pmax(df$int_start, bs))
    covered <- sum(overlap, na.rm = TRUE)

    # allocate interval mass by overlap fraction
    mass_in_bin <- sum(df$C_g_m2 * (overlap / df$int_years), na.rm = TRUE)

    tibble(
      bin_id         = bins$bin_id[i],
      bin_start      = bs,
      bin_end        = be,
      bin_center_yBP = bins$bin_center_yBP[i],
      years_covered  = covered,
      coverage_frac  = covered / (be - bs),
      mass_g_m2      = mass_in_bin,
      CAR_g_m2_yr    = mass_in_bin / (be - bs)
    )
  }) %>%
    mutate(present_year = present)
}
  # overlap calc, proportional mass per bin
  map_dfr(seq_len(nrow(bins)), function(i) {
    bs <- bins$bin_start[i]; be <- bins$bin_end[i]
    # overlap length with each interval
    overlap <- pmax(0, pmin(df$int_end, be) - pmax(df$int_start, bs))
    covered <- sum(overlap, na.rm = TRUE)

    # allocate mass into bin proportional to temporal overlap
    mass_in_bin <- sum(df$C_g_m2 * (overlap / df$int_years), na.rm = TRUE)
    tibble(
      bin_id = bins$bin_id[i],
      bin_start, bin_end,
      bin_center_yBP = bins$bin_center_yBP[i],
      years_covered = covered,
      coverage_frac = covered / (be - bs),
      mass_g_m2 = mass_in_bin,
      CAR_g_m2_yr = mass_in_bin / (be - bs)
    )
  }) %>%
  mutate(present_year = present)
}

# apply to every core
car_bins_all <- ac_int %>%
  group_split(core) %>%
  map_dfr(~ car_bins_one_core(.x) %>% mutate(core = unique(.x$core))) %>%
  relocate(core)

# join elevation & disturbance
car_bins <- car_bins_all %>%
  left_join(meta, by = "core") %>%
  # keep only bins with adequate temporal coverage (e.g., ≥60%)
  filter(coverage_frac >= 0.6)

# save
out_fp <- file.path(base_dir, "CAR_25yr_bins_by_core.csv")
write_csv(car_bins, out_fp)
message("Wrote: ", out_fp)

# --- quick EDA plots (optional) -------------------------------------------
# Example: CAR vs elevation by bin (uses ggplot2 if installed)
if (requireNamespace("ggplot2", quietly = TRUE)) {
  library(ggplot2)
  ggplot(car_bins, aes(elevation, CAR_g_m2_yr, color = disturbance)) +
    geom_point() +
    geom_smooth(method = "lm", se = TRUE) +
    facet_wrap(~ bin_id, ncol = 2) +
    labs(y = "Apparent CAR (g m⁻² yr⁻¹)", x = "Elevation (m)")
}

# --- MODEL OPTION A: Mixed-effects (lme4) ----------------------------------
# Tests: (i) overall trend across bins, (ii) elevation effect, (iii) disturbance,
# (iv) elevation × time interaction; partial pooling across cores.
if (requireNamespace("lme4", quietly = TRUE)) {
  library(lme4)
  # Standardize elevation to ease interpretation
  car_m <- car_bins %>%
    mutate(elev_sc = scale(elevation)[,1],
           time_sc = scale(bin_center_yBP)[,1])  # 12.5, 37.5, ... yrs BP (scaled)

  # random intercepts and core-specific time slopes (if data supports)
  mod <- lmer(CAR_g_m2_yr ~ time_sc * elev_sc + disturbance + (1 + time_sc | core),
              data = car_m, REML = TRUE)
  print(summary(mod))
}

# --- MODEL OPTION B: Per-core robust trend then meta-regress --------------
# Compute Theil–Sen slope of CAR vs time (bin_center_yBP) for each core,
# then relate slope to elevation and disturbance.
if (requireNamespace("mblm", quietly = TRUE)) {
  library(mblm)
  slopes <- car_bins %>%
    group_by(core) %>%
    summarise(
      slope_g_m2_yr2 = tryCatch(coef(mblm(CAR_g_m2_yr ~ bin_center_yBP))[[2]], error = function(e) NA_real_),
      .groups = "drop"
    ) %>%
    left_join(meta, by = "core")

  # simple meta-regression
  if (requireNamespace("broom", quietly = TRUE)) library(broom)
  reg <- lm(slope_g_m2_yr2 ~ elevation + disturbance, data = slopes)
  print(summary(reg))
}

#======================================================
#======================================================
#======================================================
#============ Load climate data
# --- packages --------------------------------------------------------------
library(terra)
library(dplyr)
library(tidyr)
library(readr)
library(lubridate)
library(purrr)
library(stringr)

# --- paths -----------------------------------------------------------------
base    <- "/Users/juanbenavides/Library/CloudStorage/Dropbox/papers/peat 210 pb/stats2"
geodir  <- file.path(base, "Geographic")
plumdir <- file.path(base, "Plum_runs")

# master ages file (per 1 cm; use the one you’ve been working with)
ages_fp   <- file.path(plumdir, "AGES_MASTER_with_CARBON_bd_and_Cgm2.csv")  # or AGES_MASTER_all_cores.csv
cores_fp  <- file.path(base, "cores.csv")

# climate files in Geographic/
be_tavg_fp <- file.path(geodir, "Complete_TAVG_LatLong1.nc")
be_tmax_fp <- file.path(geodir, "Complete_TMAX_LatLong1.nc")
gpcc_p_fp  <- file.path(geodir, "precip.mon.total.v2020.nc")

# HYDE (optional)
hyde_cropland_fp <- file.path(geodir, "cropland.nc")
hyde_popdens_fp  <- file.path(geodir, "population_density.nc")
hyde_rural_fp    <- file.path(geodir, "rural_population.nc")

# --- helpers ---------------------------------------------------------------
as_month_dates <- function(r) {
  tt <- terra::time(r)
  if (inherits(tt, "Date") || inherits(tt, "POSIXct")) return(as.Date(tt))
  units_guess <- attr(tt, "units")
  if (!is.null(units_guess) && grepl("since", units_guess)) {
    origin <- sub(".*since\\s+", "", units_guess)
    return(as.Date(as.numeric(tt), origin = origin))
  }
  # fallback (rare)
  as.Date("1850-01-01") + months(seq_along(tt) - 1L)
}

needs_lon360 <- function(r) {
  ext <- ext(r); xmin(ext) >= 0 && xmax(ext) <= 360
}

extract_monthly_at_points <- function(r, cores, value_name) {
  r <- rast(r)
  dates <- as_month_dates(r)
  cores_pts <- cores
  if (needs_lon360(r)) cores_pts <- mutate(cores_pts, long = ifelse(long < 0, long + 360, long))
  pts <- vect(cores_pts[, c("long","lat")], geom = c("long","lat"), crs = "EPSG:4326")
  mat <- terra::extract(r, pts)
  vals <- as_tibble(mat[,-1, drop = FALSE]); colnames(vals) <- paste0("b", seq_len(ncol(vals)))
  bind_cols(core = cores$core, vals) |>
    pivot_longer(-core, names_to = "band", values_to = value_name) |>
    mutate(date = rep(dates, each = nrow(cores))) |>
    select(core, date, all_of(value_name))
}

annualize <- function(df, var, fun = c("mean","sum")) {
  fun <- match.arg(fun)
  df %>%
    mutate(year = year(date)) %>%
    group_by(core, year) %>%
    summarize(!!var := if (fun == "mean") mean(.data[[var]], na.rm = TRUE)
                        else sum(.data[[var]], na.rm = TRUE),
              .groups = "drop")
}

r <- rast(be_tavg_fp)

extract_hyde_annual <- function(nc_path, cores, value_name) {
  r <- rast(nc_path)
  yrs <- terra::time(r)
  yrs <- if (inherits(yrs, "POSIXct") || inherits(yrs, "Date")) year(yrs) else as.integer(yrs)
  cores_pts <- cores
  if (needs_lon360(r)) cores_pts <- mutate(cores_pts, long = ifelse(long < 0, long + 360, long))
  pts <- vect(cores_pts[, c("long","lat")], geom = c("long","lat"), crs = "EPSG:4326")
  mat <- terra::extract(r, pts)
  vals <- as_tibble(mat[,-1, drop = FALSE]); names(vals) <- paste0("y", yrs)
  bind_cols(core = cores$core, vals) |>
    pivot_longer(-core, names_to = "y", values_to = value_name) |>
    transmute(core, year = as.integer(sub("^y","", y)), !!value_name := .data[[value_name]])
}

# --- load metadata + master ages ------------------------------------------
cores <- read_csv(cores_fp, show_col_types = FALSE) %>%
  transmute(core = as.character(core), lat = as.numeric(lat), long = as.numeric(long),
            elevation = as.numeric(elevation), disturbance = as.character(disturbance))

ages  <- read_csv(ages_fp, show_col_types = FALSE)

# Get calendar year per row (integer). Prefer calAD_median; else 1950 - age_median_BP
ages <- ages %>%
  mutate(year_AD = dplyr::coalesce(calAD_median, 1950 - age_median_BP)) %>%
  mutate(year_AD = round(as.numeric(year_AD))) %>%
  # limit to years we can realistically match to climate (≥1900 as requested)
  filter(year_AD >= 1900) %>%
  inner_join(cores %>% select(core, lat, long, elevation, disturbance), by = "core")

# --- extract climate (monthly → annual) ------------------------------------
tavg_m <- extract_monthly_at_points(be_tavg_fp, cores, "tavg_C")
tmax_m <- extract_monthly_at_points(be_tmax_fp, cores, "tmax_C")
pre_m  <- extract_monthly_at_points(gpcc_p_fp,  cores, "pre_mm")

tavg_y <- annualize(tavg_m, "tavg_C", "mean")
tmax_y <- annualize(tmax_m, "tmax_C", "mean")
pre_y  <- annualize(pre_m,  "pre_mm", "sum")

clim_y <- tavg_y %>%
  full_join(tmax_y, by = c("core","year")) %>%
  full_join(pre_y,  by = c("core","year"))

# (Optional) add HYDE annual layers
if (file.exists(hyde_cropland_fp)) {
  hyde_crop <- extract_hyde_annual(hyde_cropland_fp, cores, "cropland")
  clim_y <- clim_y %>% left_join(hyde_crop, by = c("core","year"))
}
if (file.exists(hyde_popdens_fp)) {
  hyde_pop <- extract_hyde_annual(hyde_popdens_fp, cores, "pop_density")
  clim_y <- clim_y %>% left_join(hyde_pop, by = c("core","year"))
}
if (file.exists(hyde_rural_fp)) {
  hyde_rural <- extract_hyde_annual(hyde_rural_fp, cores, "rural_pop")
  clim_y <- clim_y %>% left_join(hyde_rural, by = c("core","year"))
}

# --- join climate to EVERY master row by matching year ---------------------
ages_clim <- ages %>%
  left_join(clim_y, by = c("core" = "core", "year_AD" = "year"))

# Optional: quality flag for incomplete climate years (e.g., if last year lacks 12 months)
# Here we assume the source products provide complete years historically.

# --- save ------------------------------------------------------------------
out_dir <- file.path(geodir, "derived"); dir.create(out_dir, showWarnings = FALSE, recursive = TRUE)
out_fp  <- file.path(out_dir, "AGES_MASTER_with_climate_by_row.csv")
write_csv(ages_clim, out_fp)
message("Wrote: ", out_fp)


library(readr)
library(dplyr)
library(ggplot2)
library(stringr)
library(tidyr)

# --- paths ----------------------------------------------------------------
base_dir <- "/Users/juanbenavides/Library/CloudStorage/Dropbox/papers/peat 210 pb/stats2/Plum_runs"
ages_fp  <- file.path(base_dir, "AGES_MASTER_with_CARBON_bd_and_Cgm2.csv") # or AGES_MASTER_all_cores.csv
cores_fp <- "/Users/juanbenavides/Library/CloudStorage/Dropbox/papers/peat 210 pb/stats2/cores.csv"

# --- data -----------------------------------------------------------------
ages <- read_csv(ages_fp, show_col_types = FALSE)
meta <- read_csv(cores_fp, show_col_types = FALSE) %>%
  select(core, disturbance) %>%
  mutate(core = as.character(core))

# expected columns in ages:
# core, depth_cm, age_median_BP, age_min95_BP, age_max95_BP (and/or calAD_median)
stopifnot(all(c("core","depth_cm","age_median_BP") %in% names(ages)))

dat <- ages %>%
  inner_join(meta, by = "core") %>%
  filter(depth_cm <= 50) %>%
  mutate(
    # keep core order from cores.csv in the facets
    core = factor(core, levels = unique(meta$core)),
    disturbance = factor(disturbance, levels = c("Conserved","Degraded"))
  ) %>%
  arrange(disturbance, core, depth_cm)

# --- choose AGE SCALE -----------------------------------------------------
use_BP <- TRUE   # set FALSE to plot calendar years CE instead

if (use_BP) {
  # Plot in calibrated years BP
  p <- ggplot(dat, aes(y = depth_cm)) +
    geom_line(aes(x = age_median_BP), linewidth = 0.5) +
    # dashed 95% bounds if present
    { if (all(c("age_min95_BP","age_max95_BP") %in% names(dat)))
        list(
          geom_line(aes(x = age_min95_BP), linewidth = 0.3, linetype = 2, alpha = 0.6),
          geom_line(aes(x = age_max95_BP), linewidth = 0.3, linetype = 2, alpha = 0.6)
        )
      else NULL } +
    labs(x = "Age (cal yr BP)", y = "Depth (cm)")
} else {
  # Convert to calendar year CE and plot
  dat <- dat %>%
    mutate(
      calAD_median = if ("calAD_median" %in% names(.)) calAD_median else 1950 - age_median_BP,
      calAD_low95  = if (all(c("age_min95_BP","age_max95_BP") %in% names(.)))
                        1950 - pmax(age_min95_BP, age_max95_BP) else NA_real_,
      calAD_high95 = if (all(c("age_min95_BP","age_max95_BP") %in% names(.)))
                        1950 - pmin(age_min95_BP, age_max95_BP) else NA_real_
    )

  p <- ggplot(dat, aes(y = depth_cm)) +
    geom_line(aes(x = calAD_median), linewidth = 0.5) +
    { if (all(c("calAD_low95","calAD_high95") %in% names(dat)))
        list(
          geom_line(aes(x = calAD_low95),  linewidth = 0.3, linetype = 2, alpha = 0.6),
          geom_line(aes(x = calAD_high95), linewidth = 0.3, linetype = 2, alpha = 0.6)
        )
      else NULL } +
    labs(x = "Calendar year (CE)", y = "Depth (cm)")
}

p_final <- p +
  facet_grid(disturbance ~ core, scales = "fixed", switch = "y") +
  scale_y_reverse(limits = c(50, 0), expand = c(0, 0)) +
  theme_bw(base_size = 10) +
  theme(
    panel.grid.minor = element_blank(),
    strip.background = element_rect(fill = "grey95"),
    strip.text.x = element_text(size = 8),
    strip.text.y = element_text(size = 10, face = "bold"),
    panel.spacing.x = unit(0.6, "lines"),
    panel.spacing.y = unit(0.8, "lines")
  )

print(p_final)

# Save (tweak width/height as needed for 21 cores)
ggsave(file.path(base_dir, "AgeDepth_0-50cm_byCore_byDisturbance_BP.pdf"),
       p_final, width = 22, height = 7, units = "in")

