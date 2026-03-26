

suppressPackageStartupMessages({
  library(terra); library(sf); library(dplyr); library(ranger); 
  library(mgcv); library(scales); library(forcats)
})

## -----------------------------
## 0) Paths & inputs (edit paths)
## -----------------------------
geo_dir   <- "/Users/juanbenavides/Library/CloudStorage/Dropbox/papers/Paramo carbon map/GEographic"
stats_dir <- "/Users/juanbenavides/Library/CloudStorage/Dropbox/papers/Paramo carbon map/stats2"
setwd(geo_dir)

cover1 <- rast("Cobertura100.tif")
corine <- vect("corine_paramo2.shp")

# Standardize land cover labels (same as used in training)
corine$Clasificac1 <- ifelse(corine$corinetext=="Paramo natural","Paramo",corine$Clasificac)
corine$Clasificac1 <- ifelse(corine$corinetext=="Bosque","Forest",corine$Clasificac1)
corine$Clasificac1 <- ifelse(corine$corinetext %in% c("Afloramiento rocoso","Glaciar","Cuerpos de agua naturales"),
                             "Nosoil", corine$Clasificac1)
corine$Clasificac1 <- ifelse(corine$corinetext=="Disturbio","Disturbed",corine$Clasificac1)
corine$Clasificac1 <- ifelse(is.na(corine$corinetext) | corine$Clasificac1=="", "Disturbed", corine$Clasificac1)

# Factor levels must match training
lc_levels <- c("Disturbed","Forest","Nosoil","Paramo")
corine$Clasificac1 <- factor(corine$Clasificac1, levels = lc_levels)
corine$lc_code <- as.integer(corine$Clasificac1)  # 1 D, 2 F, 3 N, 4 P

# Rasterize codes to the 100 m grid
lc_num <- rasterize(corine, cover1, field = "lc_code")

# Load predictors (in the same names used in the model)
setwd(stats_dir)
dem        <- rast("dem3_100.tif")
tpi        <- rast("TPI100.tif")
slope      <- rast("slope100.tif")
tavg       <- rast("temp2.tiff")
tmax       <- rast("tempmax2.tiff")
prec       <- rast("precip2.tiff")
soilmoist  <- rast("soilmoist2.tiff")

# Align to the land-cover grid
tavg2      <- resample(tavg,      lc_num, method="bilinear")
tmax2      <- resample(tmax,      lc_num, method="bilinear")
prec2      <- resample(prec,      lc_num, method="bilinear")
soilmoist2 <- resample(soilmoist, lc_num, method="bilinear")
dem        <- resample(dem,       lc_num, method="bilinear")
tpi        <- resample(tpi,       lc_num, method="bilinear")
slope      <- resample(slope,     lc_num, method="bilinear")

# Apply Paramo<3000→Forest *in the model label space*
paramo_code <- which(lc_levels == "Paramo")  # 4
forest_code <- which(lc_levels == "Forest")  # 2
lc_num_adj  <- lc_num
lc_num_adj[(dem < 3000) & (lc_num == paramo_code)] <- forest_code

# Build modeling LC raster: collapse Nosoil+Disturbed -> Disturbed (code 1); Forest->2; Paramo->3
# (this is the LC_model used in the RF)
LC_model_code <- classify(lc_num_adj, rcl = rbind(
  c(1, 1),  # Disturbed
  c(2, 2),  # Forest
  c(3, 1),  # Nosoil -> Disturbed
  c(4, 3)   # Paramo
))
names(LC_model_code) <- "LC_model"

# Keep a mask for original Nosoil to zero out at the end
nosoil_mask <- lc_num == 3
names(nosoil_mask) <- "nosoil"

# Add lon/lat for GAM spatial term (same grid)
lon <- init(dem, "x"); names(lon) <- "Longitude"
lat <- init(dem, "y"); names(lat) <- "Latitude"

# Final predictor stack for mapping
pred_stack <- c(LC_model_code, tavg2, tmax2, prec2, soilmoist2, slope, tpi, dem, lon, lat, nosoil_mask)
names(pred_stack) <- c("LC_model", "temp2", "tempmax2", "precip2", "soilmoist2",
                       "slope100","TPI100","dem3_100","Longitude","Latitude","nosoil")

## ---------------------------------------------------
## 1) Fit final models on FULL training data (dat)
## ---------------------------------------------------
stopifnot(all(c("Carbon_corr","logSOC","LC_model",
                "dem3_100","TPI100","slope100","temp2","tempmax2","precip2","soilmoist2",
                "Longitude","Latitude") %in% names(dat)))
lvl_cm <- levels(dat$LC_model)

# Weighted RF tuning via OOB on full data
set.seed(120)
grid <- expand.grid(
  mtry            = c(2,3,4,5),
  min.node.size   = c(3,5,10),
  sample.fraction = c(0.6,0.8,1.0)
)
fit_rf <- function(tr, par, w){
  ranger::ranger(
    logSOC ~ LC_model + temp2 + tempmax2 + precip2 + soilmoist2 + slope100 + TPI100 + dem3_100,
    data = tr,
    num.trees = 1000,
    mtry = par$mtry,
    min.node.size = par$min.node.size,
    sample.fraction = par$sample.fraction,
    case.weights = w,
    importance = "permutation",
    respect.unordered.factors = "partition",
    seed = 120
  )
}
w_full <- rescale(dat$Carbon_corr, to = c(1,3)); w_full[!is.finite(w_full)] <- 1
oob_scores <- lapply(seq_len(nrow(grid)), function(j){
  par <- grid[j,]; fit <- fit_rf(dat, par, w_full)
  data.frame(j=j, oob_rmse = sqrt(fit$prediction.error))
}) |> bind_rows() |> arrange(oob_rmse)
best    <- grid[oob_scores$j[1],]
rf_full <- fit_rf(dat, best, w_full)

# Duan smearing from OOB (full data)
yhat_log_oob <- rf_full$predictions
smear_full   <- mean(exp(dat$logSOC - yhat_log_oob))

# Residual GAM on full data using OOB SOC predictions
tr_soc_oob_full <- pmax(0, (exp(yhat_log_oob) * smear_full) - 1)
resid_full      <- dat$Carbon_corr - tr_soc_oob_full

gam_full <- mgcv::gam(
  resid_full ~ s(tr_soc_oob_full, k = 5) +
               s(dem3_100, k = 5) +
               s(precip2,  k = 5) +
               LC_model +
               s(Longitude, Latitude, bs = "tp", k = 80),
  data   = dat,
  method = "REML", select = TRUE
)

## ---------------------------------------------------
## 2) Predict wall-to-wall with terra::predict()
## ---------------------------------------------------
# The prediction function called by terra::predict() on chunks
pred_fun <- function(df) {
  # 1) RF on log scale
  df$LC_model <- factor(df$LC_model, levels = c(1,2,3), labels = c("Disturbed","Forest","Paramo"))
  logp <- predict(rf_full, data = df)$predictions

  # 2) Back-transform with global smear
  soc_raw <- pmax(0, (exp(logp) * smear_full) - 1)

  # 3) Residual GAM correction (needs same fields as in gam_full)
  add_resid <- predict(gam_full, newdata = data.frame(
    tr_soc_oob_full = soc_raw,
    dem3_100        = df$dem3_100,
    precip2         = df$precip2,
    LC_model        = df$LC_model,
    Longitude       = df$Longitude,
    Latitude        = df$Latitude
  ))

  soc_final <- soc_raw + add_resid
  soc_final[soc_final < 0] <- 0

  # 4) Force Nosoil = 0 in the output
  if ("nosoil" %in% names(df)) {
    soc_final[df$nosoil == 1] <- 0
  }
  as.numeric(soc_final)
}

# Run prediction (writes directly to disk to avoid RAM issues)
out_file     <- file.path(stats_dir, "SOC_pred_final_RF_GAM.tif")
out_file_raw <- file.path(stats_dir, "SOC_pred_RF_raw.tif")  # optional raw RF map

# (Optional) also write the raw RF map

# -------- RAW RF map (log -> SOC via smearing) ----------



suppressPackageStartupMessages({
  library(terra); library(sf); library(dplyr); library(ranger); library(mgcv); library(scales)
})

## -----------------------------
## 0) Paths & inputs (edit paths)
## -----------------------------
geo_dir   <- "/Users/juanbenavides/Library/CloudStorage/Dropbox/papers/Paramo carbon map/GEographic"
stats_dir <- "/Users/juanbenavides/Library/CloudStorage/Dropbox/papers/Paramo carbon map/stats2"
setwd(geo_dir)

cover1 <- rast("Cobertura100.tif")
corine <- vect("corine_paramo2.shp")

# Standardize land cover labels (same as used in training)
corine$Clasificac1 <- ifelse(corine$corinetext=="Paramo natural","Paramo",corine$Clasificac)
corine$Clasificac1 <- ifelse(corine$corinetext=="Bosque","Forest",corine$Clasificac1)
corine$Clasificac1 <- ifelse(corine$corinetext %in% c("Afloramiento rocoso","Glaciar","Cuerpos de agua naturales"),
                             "Nosoil", corine$Clasificac1)
corine$Clasificac1 <- ifelse(corine$corinetext=="Disturbio","Disturbed",corine$Clasificac1)
corine$Clasificac1 <- ifelse(is.na(corine$corinetext) | corine$Clasificac1=="", "Disturbed", corine$Clasificac1)

# Factor levels must match training
lc_levels <- c("Disturbed","Forest","Nosoil","Paramo")
corine$Clasificac1 <- factor(corine$Clasificac1, levels = lc_levels)
corine$lc_code <- as.integer(corine$Clasificac1)  # 1 D, 2 F, 3 N, 4 P

# Rasterize codes to the 100 m grid
lc_num <- rasterize(corine, cover1, field = "lc_code")

# Load predictors (in the same names used in the model)
setwd(stats_dir)
dem        <- rast("dem3_100.tif")
tpi        <- rast("TPI100.tif")
slope      <- rast("slope100.tif")
tavg       <- rast("temp2.tiff")
tmax       <- rast("tempmax2.tiff")
prec       <- rast("precip2.tiff")
soilmoist  <- rast("soilmoist2.tiff")

# Align to the land-cover grid
tavg2      <- resample(tavg,      lc_num, method="bilinear")
tmax2      <- resample(tmax,      lc_num, method="bilinear")
prec2      <- resample(prec,      lc_num, method="bilinear")
soilmoist2 <- resample(soilmoist, lc_num, method="bilinear")
dem        <- resample(dem,       lc_num, method="bilinear")
tpi        <- resample(tpi,       lc_num, method="bilinear")
slope      <- resample(slope,     lc_num, method="bilinear")

# Apply Paramo<3000→Forest *in the model label space*
paramo_code <- which(lc_levels == "Paramo")  # 4
forest_code <- which(lc_levels == "Forest")  # 2
lc_num_adj  <- lc_num
lc_num_adj[(dem < 3000) & (lc_num == paramo_code)] <- forest_code

# Build modeling LC raster: collapse Nosoil+Disturbed -> Disturbed (code 1); Forest->2; Paramo->3
# (this is the LC_model used in the RF)
LC_model_code <- classify(lc_num_adj, rcl = rbind(
  c(1, 1),  # Disturbed
  c(2, 2),  # Forest
  c(3, 1),  # Nosoil -> Disturbed
  c(4, 3)   # Paramo
))
names(LC_model_code) <- "LC_model"

# Keep a mask for original Nosoil to zero out at the end
nosoil_mask <- lc_num == 3
names(nosoil_mask) <- "nosoil"

# Add lon/lat for GAM spatial term (same grid)
lon <- init(dem, "x"); names(lon) <- "Longitude"
lat <- init(dem, "y"); names(lat) <- "Latitude"

# Final predictor stack for mapping
pred_stack <- c(LC_model_code, tavg2, tmax2, prec2, soilmoist2, slope, tpi, dem, lon, lat, nosoil_mask)
names(pred_stack) <- c("LC_model", "temp2", "tempmax2", "precip2", "soilmoist2",
                       "slope100","TPI100","dem3_100","Longitude","Latitude","nosoil")

## ---------------------------------------------------
## 1) Fit final models on FULL training data (dat)
## ---------------------------------------------------
stopifnot(all(c("Carbon_corr","logSOC","LC_model",
                "dem3_100","TPI100","slope100","temp2","tempmax2","precip2","soilmoist2",
                "Longitude","Latitude") %in% names(dat)))
lvl_cm <- levels(dat$LC_model)

# Weighted RF tuning via OOB on full data
set.seed(120)
grid <- expand.grid(
  mtry            = c(2,3,4,5),
  min.node.size   = c(3,5,10),
  sample.fraction = c(0.6,0.8,1.0)
)
fit_rf <- function(tr, par, w){
  ranger::ranger(
    logSOC ~ LC_model + temp2 + tempmax2 + precip2 + soilmoist2 + slope100 + TPI100 + dem3_100,
    data = tr,
    num.trees = 1000,
    mtry = par$mtry,
    min.node.size = par$min.node.size,
    sample.fraction = par$sample.fraction,
    case.weights = w,
    importance = "permutation",
    respect.unordered.factors = "partition",
    seed = 120
  )
}
w_full <- rescale(dat$Carbon_corr, to = c(1,3)); w_full[!is.finite(w_full)] <- 1
oob_scores <- lapply(seq_len(nrow(grid)), function(j){
  par <- grid[j,]; fit <- fit_rf(dat, par, w_full)
  data.frame(j=j, oob_rmse = sqrt(fit$prediction.error))
}) |> bind_rows() |> arrange(oob_rmse)
best    <- grid[oob_scores$j[1],]
rf_full <- fit_rf(dat, best, w_full)

# Duan smearing from OOB (full data)
yhat_log_oob <- rf_full$predictions
smear_full   <- mean(exp(dat$logSOC - yhat_log_oob))

# Residual GAM on full data using OOB SOC predictions
tr_soc_oob_full <- pmax(0, (exp(yhat_log_oob) * smear_full) - 1)
resid_full      <- dat$Carbon_corr - tr_soc_oob_full

gam_full <- mgcv::gam(
  resid_full ~ s(tr_soc_oob_full, k = 5) +
               s(dem3_100, k = 5) +
               s(precip2,  k = 5) +
               LC_model +
               s(Longitude, Latitude, bs = "tp", k = 80),
  data   = dat,
  method = "REML", select = TRUE
)

## ---------------------------------------------------
## 2) Predict wall-to-wall with terra::predict()
## ---------------------------------------------------
# The prediction function called by terra::predict() on chunks
pred_fun <- function(df) {
  # 1) RF on log scale
  df$LC_model <- factor(df$LC_model, levels = c(1,2,3), labels = c("Disturbed","Forest","Paramo"))
  logp <- predict(rf_full, data = df)$predictions

  # 2) Back-transform with global smear
  soc_raw <- pmax(0, (exp(logp) * smear_full) - 1)

  # 3) Residual GAM correction (needs same fields as in gam_full)
  add_resid <- predict(gam_full, newdata = data.frame(
    tr_soc_oob_full = soc_raw,
    dem3_100        = df$dem3_100,
    precip2         = df$precip2,
    LC_model        = df$LC_model,
    Longitude       = df$Longitude,
    Latitude        = df$Latitude
  ))

  soc_final <- soc_raw + add_resid
  soc_final[soc_final < 0] <- 0

  # 4) Force Nosoil = 0 in the output
  if ("nosoil" %in% names(df)) {
    soc_final[df$nosoil == 1] <- 0
  }
  as.numeric(soc_final)
}

# Run prediction (writes directly to disk to avoid RAM issues)
out_file     <- file.path(stats_dir, "SOC_pred_final_RF_GAM.tif")
out_file_raw <- file.path(stats_dir, "SOC_pred_RF_raw.tif")  # optional raw RF map

# (Optional) also write the raw RF map

# -------- RAW RF map (log -> SOC via smearing) ----------
# --- helper to coerce nosoil column to 0/1 ---
.nosoil01 <- function(x){
  z <- suppressWarnings(as.integer(x)); z[is.na(z)] <- 0; z
}

# -------- RAW RF map (RF log -> SOC via smearing; zero Nosoil) ----------
fun_rf_raw <- function(model, data, ...) {
  df <- as.data.frame(data)
  if (!nrow(df)) return(numeric(0))  # handle terra's 0-row probe

  df$LC_model <- factor(df$LC_model, levels = c(1,2,3),
                        labels = c("Disturbed","Forest","Paramo"))

  logp    <- predict(model, data = df)$predictions
  soc_raw <- pmax(0, (exp(logp) * smear_full) - 1)

  if ("nosoil" %in% names(df)) {
    nz <- .nosoil01(df$nosoil)
    soc_raw[nz == 1] <- 0
  }
  soc_raw
}

soc_raw_r <- terra::predict(
  pred_stack, rf_full,
  fun      = fun_rf_raw,
  filename = out_file_raw, overwrite = TRUE,
  na.rm    = TRUE,
  wopt     = list(datatype = "FLT4S")
)

# -------- FINAL map (RF + residual GAM correction; zero Nosoil) ----------
fun_rf_gam <- function(model, data, ...) {
  df <- as.data.frame(data)
  if (!nrow(df)) return(numeric(0))

  df$LC_model <- factor(df$LC_model, levels = c(1,2,3),
                        labels = c("Disturbed","Forest","Paramo"))

  # RF log -> SOC via smearing
  logp     <- predict(model, data = df)$predictions
  soc_raw  <- pmax(0, (exp(logp) * smear_full) - 1)

  # residual GAM correction (must match gam_full formula variables)
  add_resid <- predict(gam_full, newdata = data.frame(
    tr_soc_oob_full = soc_raw,
    dem3_100        = df$dem3_100,
    precip2         = df$precip2,
    LC_model        = df$LC_model,
    Longitude       = df$Longitude,
    Latitude        = df$Latitude
  ))

  soc_final <- pmax(0, soc_raw + add_resid)

  if ("nosoil" %in% names(df)) {
    nz <- .nosoil01(df$nosoil)
    soc_final[nz == 1] <- 0
  }
  soc_final
}

soc_final_r <- terra::predict(
  pred_stack, rf_full,
  fun      = fun_rf_gam,
  filename = out_file, overwrite = TRUE,
  na.rm    = TRUE,
  wopt     = list(datatype = "FLT4S")
)

write.raster(SOC_pred_final_RF_GAM.tif)
# quick check
print(soc_final_r)
global(soc_final_r, "min", na.rm = TRUE)
global(soc_final_r, "max", na.rm = TRUE)
global(soc_final_r, "mean", na.rm = TRUE)


tmp <- file.path(stats_dir, "SOC_pred_final_RF_GAM_tmp.tif")
writeRaster(soc_final_r, tmp, overwrite=TRUE,
            wopt=list(datatype="FLT4S", gdal=c("COMPRESS=LZW","TILED=YES","BIGTIFF=YES")))
file.rename(tmp, file.path(stats_dir, "SOC_pred_final_RF_GAM.tif"))


#====================================================
#====================================================

suppressPackageStartupMessages({ library(terra); library(dplyr); library(tidyr) })

# --- inputs (edit paths only if needed) ---
stats_dir <- "/Users/juanbenavides/Library/CloudStorage/Dropbox/papers/Paramo carbon map/stats2"
geo_dir   <- "/Users/juanbenavides/Library/CloudStorage/Dropbox/papers/Paramo carbon map/GEographic"

# 1) Load predicted SOC (Mg C ha^-1)
if (!exists("soc_final_r")) soc_final_r <- rast(file.path(stats_dir, "SOC_pred_final_RF_GAM.tif"))

# 2) Land-cover codes used in the model: 1=Disturbed, 2=Forest, 3=Paramo
if (!exists("LC_model_code")) {
  if (exists("pred_stack")) {
    LC_model_code <- pred_stack[["LC_model"]]
  } else {
    stop("LC_model_code not found. Recreate or load the LC_model raster (1=Disturbed,2=Forest,3=Paramo).")
  }
}

# 3) Paramo sectors
paramos <- vect(file.path(geo_dir, "paramos.shp"))
paramos <- project(paramos, crs(soc_final_r))              # align CRS
paramos$sec_id <- as.integer(factor(paramos$NOMBRE_COM))   # numeric id per sector
sec_r <- rasterize(paramos, soc_final_r, field = "sec_id", touches = TRUE)
lut   <- unique(data.frame(sec_id = paramos$sec_id, Sector = paramos$NOMBRE_COM))  # name lookup

# 4) Convert density (Mg/ha) -> mass per cell (Mg)
area_ha <- cellSize(soc_final_r, unit = "ha")
soc_Mg  <- soc_final_r * area_ha

# 5) Zonal sums by sector × land cover
LC_model_code <- clamp(LC_model_code, 1, 3)  # keep 1..3 only

soc_dist   <- ifel(LC_model_code == 1, soc_Mg, NA)
soc_forest <- ifel(LC_model_code == 2, soc_Mg, NA)
soc_paramo <- ifel(LC_model_code == 3, soc_Mg, NA)

z_dist   <- zonal(soc_dist,   sec_r, fun = "sum", na.rm = TRUE)
z_forest <- zonal(soc_forest, sec_r, fun = "sum", na.rm = TRUE)
z_paramo <- zonal(soc_paramo, sec_r, fun = "sum", na.rm = TRUE)

# 6) Long table in Tg (Mg / 1e6)


# Inspect once if you're curious:
# print(colnames(as.data.frame(z_dist)))

# Robust renamer: use column POSITION, not name
rename_zonal <- function(z) {
  df <- as.data.frame(z)
  stopifnot(ncol(df) >= 2)
  names(df)[1:2] <- c("sec_id", "sumMg")  # 1=zone id, 2=aggregated Mg
  df
}

dist_df   <- rename_zonal(z_dist)   %>% dplyr::mutate(Clasificac2 = "Disturbed")
forest_df <- rename_zonal(z_forest) %>% dplyr::mutate(Clasificac2 = "Forest")
paramo_df <- rename_zonal(z_paramo) %>% dplyr::mutate(Clasificac2 = "Paramo")

z_all <- dplyr::bind_rows(dist_df, forest_df, paramo_df) %>%
  dplyr::mutate(TgC = sumMg / 1e6) %>%          # Mg -> Tg
  dplyr::select(sec_id, Clasificac2, TgC) %>%
  dplyr::left_join(lut, by = "sec_id") %>%
  dplyr::filter(is.finite(TgC))


library(dplyr); library(stringr)

z_all <- z_all %>%
  mutate(
    Sector_lbl = Sector,
    Sector_lbl = stringi::stri_trans_nfkc(Sector_lbl),                # normalize unicode
    Sector_lbl = gsub("[\r\n]", " ", Sector_lbl),                     # remove newlines
    Sector_lbl = gsub("\u00A0|\u200B|\u200C|\u200D|\uFEFF", "", Sector_lbl), # NBSP/zero-width
    Sector_lbl = str_squish(Sector_lbl)                               # collapse spaces & trim
  )

# reorder sectors by total Tg using the cleaned label
sector_order <- z_all %>%
  group_by(Sector_lbl) %>%
  summarise(TgTot = sum(TgC), .groups = "drop") %>%
  arrange(desc(TgTot)) %>% pull(Sector_lbl)

z_all$Sector_lbl <- factor(z_all$Sector_lbl, levels = sector_order)

library(dplyr); library(stringr)

# adjust path if the CSV lives elsewhere
name_map <- read.csv(file.path(stats_dir, "paramos_names.csv"),
                     check.names = FALSE) %>%           # keep original header
  rename(Sector_long  = NOMBRE_COM,                    # long official name
         Sector_short = Sector) %>%                    # your short label
  mutate(
    Sector_long  = str_squish(Sector_long),
    Sector_short = str_squish(Sector_short)
  )


z_all2 <- z_all %>%
  mutate(Sector_long = str_squish(Sector)) %>%         # normalize to match
  left_join(name_map, by = "Sector_long") %>%
  mutate(Sector_plot = coalesce(Sector_short, Sector_long))  # fallback to long if missing


library(dplyr); library(stringr)

# adjust path if the CSV lives elsewhere
name_map <- read.csv(file.path(stats_dir, "paramos_names.csv"),
                     check.names = FALSE) %>%           # keep original header
  rename(Sector_long  = NOMBRE_COM,                    # long official name
         Sector_short = Sector) %>%                    # your short label
  mutate(
    Sector_long  = str_squish(Sector_long),
    Sector_short = str_squish(Sector_short)
  )


z_all2 <- z_all %>%
  mutate(Sector_long = str_squish(Sector)) %>%         # normalize to match
  left_join(name_map, by = "Sector_long") %>%
  mutate(Sector_plot = coalesce(Sector_short, Sector_long))  # fallback to long if missing


sector_order <- z_all2 %>%
  group_by(Sector_plot) %>%
  summarise(TgTot = sum(TgC), .groups = "drop") %>%
  arrange(desc(TgTot)) %>% pull(Sector_plot)

z_all2$Sector_plot <- factor(z_all2$Sector_plot, levels = sector_order)

p<-ggplot(z_all2, aes(x = TgC, y = Sector_plot, fill = Clasificac2)) +
  geom_col(width = 0.8) +
  scale_fill_brewer("", palette = "Set1") +
  scale_x_continuous("Carbon content (Tg C)", expand = c(0,0),lim=c(0,50)) +
  labs(y = NULL) +
  theme_bw() +
  theme(
    legend.position    = c(0.75, 0.85),
    panel.grid.major.y = element_blank(),
    panel.grid.minor   = element_blank(),
    axis.text.y        = element_text(size = 9)
  )

print(p)


# (Optional) write CSV for the paper / SI
write.csv(z_all %>% arrange(desc(Sector)), file.path(stats_dir, "SOC_by_sector_LC_Tg.csv"), row.names = FALSE)
cat("Saved sector × land-cover totals to SOC_by_sector_LC_Tg.csv\n")


#====================================================
#====================================================

library(terra); library(dplyr); library(tibble)

# Inputs assumed in memory:
# soc_final_r      # SpatRaster, SOC density (Mg C ha^-1)
# LC_model_code    # SpatRaster, land cover codes: 1=Disturbed, 2=Forest, 3=Paramo

# 1) Cell areas in hectares (avoid km^2 mixups)
area_ha <- terra::cellSize(soc_final_r, unit = "m") / 10000

# 2) Helper to compute area-weighted mean & SD for class k
w_stats_class <- function(k){
  w  <- terra::ifel(LC_model_code == k, area_ha, NA)                 # weights (ha)
  s0 <- terra::global(w,                   "sum", na.rm = TRUE)[1,1] # Σ w
  s1 <- terra::global(soc_final_r * w,     "sum", na.rm = TRUE)[1,1] # Σ w x
  s2 <- terra::global((soc_final_r^2) * w, "sum", na.rm = TRUE)[1,1] # Σ w x^2
  mu <- s1 / s0
  sd <- sqrt(pmax(0, s2 / s0 - mu^2))
  tibble(code = k, Mean_MgHa = mu, SD_MgHa = sd,
         Area_kha = s0 / 1000)  # class area in thousands of hectares (optional)
}

# 3) Build the table
lc_labs <- c("Disturbed","Forest","Paramo")
nat_stats <- bind_rows(lapply(1:3, w_stats_class)) %>%
  mutate(Clasificac2 = factor(lc_labs[code], levels = lc_labs)) %>%
  select(Clasificac2, Mean_MgHa, SD_MgHa, Area_kha)

nat_stats
# (Optional) round & export
# nat_stats %>% mutate(across(Mean_MgHa:SD_MgHa, ~round(.x,1)),
#                      Area_kha = round(Area_kha,1)) %>%
#   readr::write_csv(file.path(stats_dir, "SOC_national_mean_sd_by_LC.csv"))




#====================================================
#====================================================
library(terra); library(dplyr)

# assumes: soc_final_r (Mg C ha^-1), LC_model_code (1,2,3), sec_r (sector ids), lut (sec_id,Sector)
# robust area in hectares (avoids km^2 sneaks)
area_ha <- cellSize(soc_final_r, unit = "m") / 10000

# Per-class (1=Disturbed, 2=Forest, 3=Paramo) area-weighted mean & SD by sector
z_moments_by_code <- function(k){
  w <- ifel(LC_model_code == k, area_ha, NA)                  # weights (ha) for class k
  # stack weighted fields so a single zonal() returns all sums
  r <- c(w, soc_final_r * w, (soc_final_r^2) * w)
  names(r) <- c("w", "wx", "wx2")
  z <- as.data.frame(zonal(r, sec_r, fun = "sum", na.rm = TRUE))

  # rename the first column to 'sec_id' regardless of its current name
  names(z)[1] <- "sec_id"

  z %>%
    transmute(
      sec_id,
      Clasificac2 = c("Disturbed","Forest","Paramo")[k],
      Mean_MgHa   = wx / w,
      SD_MgHa     = sqrt(pmax(0, wx2 / w - (wx / w)^2))
    )
}

# Build the sector stats table
sec_stats <- bind_rows(
  z_moments_by_code(1),
  z_moments_by_code(2),
  z_moments_by_code(3)
) %>%
  left_join(lut, by = "sec_id") %>%                 # add Sector names
  select(sec_id, Sector, Clasificac2, Mean_MgHa, SD_MgHa)

# Merge with your totals (TgC) table
z_all_with_stats <- z_all %>%
  left_join(sec_stats, by = c("sec_id","Sector","Clasificac2"))


#====================================================
#====================================================
library(terra); library(dplyr); library(tibble)

# Areas in hectares (safe units)
area_ha <- cellSize(soc_final_r, unit = "m") / 10000

# Mask of pixels where BOTH LC and SOC are present
valid <- !is.na(values(LC_model_code)) & !is.na(values(soc_final_r))
# apply mask to area (so Σw only counts valid SOC pixels)

lc_labs <- c("Disturbed","Forest","Paramo")

# ---------- A) National area-weighted mean ± SD ----------
w_stats_class <- function(k){
  mask_k <- (LC_model_code == k) & !is.na(soc_final_r)  # LC match + SOC present
  w  <- ifel(mask_k, area_ha, NA)
  s0 <- global(w,                   "sum", na.rm=TRUE)[1,1]     # Σ w
  s1 <- global(soc_final_r * w,     "sum", na.rm=TRUE)[1,1]     # Σ w x
  s2 <- global((soc_final_r^2) * w, "sum", na.rm=TRUE)[1,1]     # Σ w x^2
  mu <- s1 / s0
  sd <- sqrt(pmax(0, s2 / s0 - mu^2))
  tibble(Clasificac2 = lc_labs[k], Mean_MgHa = mu, SD_MgHa = sd,
         Area_kha = s0 / 1000)
}

nat_stats <- bind_rows(lapply(1:3, w_stats_class))
print(nat_stats)
# sanity check: unweighted class means (should be close)
# for (k in 1:3) print(global(mask(soc_final_r, LC_model_code==k), "mean", na.rm=TRUE))

# ---------- B) Per-sector area-weighted mean ± SD ----------
z_moments_by_code <- function(k){
  mask_k <- (LC_model_code == k) & !is.na(soc_final_r)
  w <- ifel(mask_k, area_ha, NA)
  r <- c(w, soc_final_r * w, (soc_final_r^2) * w)
  names(r) <- c("w","wx","wx2")
  z <- as.data.frame(zonal(r, sec_r, fun="sum", na.rm=TRUE))
  names(z)[1] <- "sec_id"
  z %>%
    transmute(
      sec_id,
      Clasificac2 = lc_labs[k],
      Mean_MgHa   = wx / w,
      SD_MgHa     = sqrt(pmax(0, wx2 / w - (wx / w)^2))
    )
}

sec_stats <- bind_rows(z_moments_by_code(1),
                       z_moments_by_code(2),
                       z_moments_by_code(3)) %>%
  left_join(lut, by="sec_id") %>%
  select(sec_id, Sector, Clasificac2, Mean_MgHa, SD_MgHa)

# merge with your totals (TgC)
z_all_with_stats <- z_all %>%
  left_join(sec_stats, by = c("sec_id","Sector","Clasificac2"))


# (Optional) write out for the paper/SI
readr::write_csv(z_all_with_stats,
  file.path(stats_dir, "SOC_by_sector_LC_Tg_mean_sd.csv"))






