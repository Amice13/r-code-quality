suppressPackageStartupMessages({
  library(terra); library(dplyr); library(ranger)
})

## ---- 0) Paths & options -------------------------------------------------
# Local inputs (your aligned rasters + carbon_site.csv live here)
stats_dir    <- "/Users/juanbenavides/Library/CloudStorage/Dropbox/papers/Paramo carbon map/stats2/boot"

# External drive output folder for the MANY bootstrap tiles  <-- EDIT THIS
boot_out_dir <- "/Volumes/Samsung_T5/paramo_boot/B200"  # e.g. on macOS
# boot_out_dir <- "E:/paramo_boot/B200"               # e.g. on Windows

dir.create(boot_out_dir, recursive = TRUE, showWarnings = FALSE)

# Put terra temp files on the external drive too (optional but recommended)
terraOptions(
  threads = max(1, parallel::detectCores() - 1),
  memfrac = 0.7,
  tempdir = file.path(boot_out_dir, "_terra_tmp")
)
dir.create(terraOptions()$tempdir, showWarnings = FALSE, recursive = TRUE)

gopt <- list(datatype = "FLT4S", gdal = c("COMPRESS=LZW","TILED=YES","BIGTIFF=YES"))

set.seed(120)

## ---- 1) Prediction stack (unchanged names) ------------------------------
setwd(stats_dir)
dem        <- rast("dem3_100_aligned.tif")
tpi        <- rast("TPI100_aligned.tif")
slope      <- rast("slope100_aligned.tif")
temp2      <- rast("temp2_aligned.tif")
tempmax2   <- rast("tempmax2_aligned.tif")
precip2    <- rast("precip2_aligned.tif")
soilmoist2 <- rast("soilmoist2_aligned.tif")
LC_code    <- rast("LC_model_code.tif")           # 1=Disturbed, 2=Forest, 3=Paramo
nosoil01   <- if (file.exists("nosoil01.tif")) rast("nosoil01.tif") else NULL

pred_stack <- c(temp2, tempmax2, precipip2 = precip2, soilmoist2, slope, tpi, dem, LC_code, nosoil01)
names(pred_stack) <- c("temp2","tempmax2","precip2","soilmoist2",
                       "slope100","TPI100","dem3_100","LC_model_code",
                       if (!is.null(nosoil01)) "nosoil01" else NULL)

invisible(compareGeom(pred_stack, stopOnError = TRUE))

## ---- 2) Training data & model table ------------------------------------
pts <- read.csv("carbon_site.csv")
stopifnot(all(c("Longitude","Latitude","CarbonMgHa") %in% names(pts)))
pts_v <- vect(pts, geom = c("Longitude","Latitude"), crs = "EPSG:4326")

lambda_dist <- 0.60

pred_at_pts <- terra::extract(pred_stack, pts_v)
dat0 <- dplyr::bind_cols(pts, as.data.frame(pred_at_pts)[,-1]) %>%
  filter(!is.na(LC_model_code)) %>%
  mutate(
    LC_model   = factor(LC_model_code, levels = c(1,2,3),
                        labels = c("Disturbed","Forest","Paramo")),
    CarbonMgHa = pmax(CarbonMgHa, 0),
    Carbon_corr= ifelse(LC_model=="Disturbed", CarbonMgHa*lambda_dist, CarbonMgHa),
    logSOC     = log(Carbon_corr + 1)
  ) %>%
  tidyr::drop_na(logSOC, temp2, tempmax2, precip2, soilmoist2, slope100, TPI100, dem3_100, LC_model)

# 100 m cluster IDs for bootstrap
pts_sf <- sf::st_as_sf(dat0, coords = c("Longitude","Latitude"), crs = 4326) %>% sf::st_transform(3116)
xy     <- sf::st_coordinates(pts_sf)
dat0$cluster <- factor(paste(floor(xy[,1]/100), floor(xy[,2]/100), sep="_"))

dat <- dat0 %>% dplyr::select(cluster, Carbon_corr, logSOC, LC_model,
                              temp2, tempmax2, precip2, soilmoist2, slope100, TPI100, dem3_100)

## ---- 3) Fit a baseline RF (optional; mainly for quick sanity) ----------
rf_full <- ranger::ranger(
  logSOC ~ LC_model + temp2 + tempmax2 + precip2 + soilmoist2 + slope100 + TPI100 + dem3_100,
  data = dat,
  num.trees = 1000, mtry = 3, min.node.size = 5, sample.fraction = 0.8,
  importance = "permutation", respect.unordered.factors = "partition", seed = 120
)
cat("Baseline RF OOB MSE (log):", rf_full$prediction.error, "\n")

## ---- 4) Bootstrap to EXTERNAL DRIVE (resumable) ------------------------
B <- 200
boot_paths <- file.path(boot_out_dir, sprintf("boot_soc_%03d.tif", 100:B))

# Resume logic: skip those already written
existing <- file.exists(boot_paths)
to_run   <- which(!existing)
cat("Already present:", sum(existing), " | To run:", length(to_run), "\n")

for (k in to_run) {
  set.seed(1000 + k)

  # cluster bootstrap
  idx  <- sample(unique(dat$cluster), replace = TRUE)
  boot <- dat[dat$cluster %in% idx, ]

  rf_b <- ranger::ranger(
    logSOC ~ LC_model + temp2 + tempmax2 + precip2 + soilmoist2 + slope100 + TPI100 + dem3_100,
    data = boot,
    num.trees = 600, mtry = 3, min.node.size = 5, sample.fraction = 0.8,
    respect.unordered.factors = "partition", seed = 120 + k
  )

  # Duan smearing from bootstrap training residuals
  z_hat_tr <- predict(rf_b, data = boot)$predictions  # generic predict(), not ranger::predict
  smear_b  <- mean(exp(boot$logSOC - z_hat_tr))

  # Matrix -> vector function for terra::app()
  soc_fun_mat <- function(v) {
    df <- as.data.frame(v)
    df$LC_model <- factor(round(df$LC_model_code), levels = c(1,2,3),
                          labels = c("Disturbed","Forest","Paramo"))
    logp <- predict(rf_b, data = df)$predictions
    soc  <- pmax(0, exp(logp) * smear_b - 1)
    if ("nosoil01" %in% names(df)) soc[df$nosoil01 == 1] <- 0
    soc
  }

  terra::app(
    x        = pred_stack,
    fun      = soc_fun_mat,
    filename = boot_paths[k],
    overwrite = TRUE,
    wopt     = gopt
  )

  if (k %% 10 == 0) cat("Wrote", k, "of", B, "boot maps\n")
}

## ---- 5) Read FROM EXTERNAL and compute summaries -----------------------
S   <- terra::rast(boot_paths)                 # virtual stack over external files
m   <- terra::app(S, "mean", na.rm = TRUE)
sd  <- terra::app(S, "sd",   na.rm = TRUE)
q05 <- terra::app(S, function(x) quantile(x, 0.05, na.rm = TRUE))
q95 <- terra::app(S, function(x) quantile(x, 0.95, na.rm = TRUE))
cv  <- sd / (m + 1e-9)
ci_stack <- c(m, q05, q95); names(ci_stack) <- c("SOC_mean","SOC_q05","SOC_q95")

ci_pct   <- 100 * ci_pm / (soc_mean + 1e-9)

writeRaster(ci_stack, file.path(sum_dir, "SOC_boot_CI_stack.tif"),
            overwrite = TRUE, wopt = gopt)

# Save summaries wherever you prefer (local or external). Here: local `stats_dir`.
writeRaster(m,   file.path(stats_dir, "SOC_boot_mean.tif"), overwrite = TRUE, wopt = gopt)
writeRaster(sd,  file.path(stats_dir, "SOC_boot_sd.tif"),   overwrite = TRUE, wopt = gopt)
writeRaster(q05, file.path(stats_dir, "SOC_boot_q05.tif"),  overwrite = TRUE, wopt = gopt)
writeRaster(q95, file.path(stats_dir, "SOC_boot_q95.tif"),  overwrite = TRUE, wopt = gopt)
writeRaster(cv,  file.path(stats_dir, "SOC_boot_cv.tif"),   overwrite = TRUE, wopt = gopt)

cat("Summaries written to:\n", stats_dir, "\n")
