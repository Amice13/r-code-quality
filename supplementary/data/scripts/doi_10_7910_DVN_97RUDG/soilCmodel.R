## ============================================================
## Páramo SOC model — disturbed correction + spatial CV + trends
## ============================================================
suppressPackageStartupMessages({
  library(terra); library(sf)
  library(dplyr); library(tidyr); library(purrr); library(forcats); library(tibble)
  library(ranger); library(rsample); library(yardstick); library(vip); library(mgcv)
  library(ggplot2); library(gridExtra); library(grid); library(patchwork); library(scales)
  # library(bestNormalize)  # uncomment if you decide to use it
})

## --------------------------
## 0) Paths & user settings
## --------------------------
geo_dir   <- "/Users/juanbenavides/Library/CloudStorage/Dropbox/papers/Paramo carbon map/GEographic"
stats_dir <- "/Users/juanbenavides/Library/CloudStorage/Dropbox/papers/Paramo carbon map/stats2"
std <- function(x, na.rm = TRUE) {
  n <- if (na.rm) sum(!is.na(x)) else length(x)
  if (n < 2L) return(NA_real_)
  stats::sd(x, na.rm = na.rm) / sqrt(n)
}


set.seed(120)

## 0) Settings
block_size  <- 100 # meters to build spatial autocorrelation
k_folds     <- 5 #spatial folds
lambda_dist <- 0.60 #core sampler size

## 1) LC levels (single source of truth)
lc_levels <- c("Disturbed","Forest","Nosoil","Paramo")




## ------------------------------------------
## 1) Land-cover classification from CORINE
## ------------------------------------------
setwd(geo_dir)
cover1 <- rast("Cobertura100.tif")
corine <- vect("corine_paramo2.shp")

corine$Clasificac1 <- ifelse(corine$corinetext=="Paramo natural","Paramo",corine$Clasificac)
corine$Clasificac1 <- ifelse(corine$corinetext=="Bosque","Forest",corine$Clasificac1)
corine$Clasificac1 <- ifelse(corine$corinetext %in% c("Afloramiento rocoso","Glaciar","Cuerpos de agua naturales"),
                              "Nosoil",corine$Clasificac1)
corine$Clasificac1 <- ifelse(corine$corinetext=="Disturbio","Disturbed",corine$Clasificac1)
corine$Clasificac1 <- ifelse(is.na(corine$corinetext) | corine$Clasificac1=="","Disturbed",corine$Clasificac1)

lc_str <- rasterize(corine, cover1, field = "Clasificac1")

## -------------------------------
## 2) Load points & extract covars
## -------------------------------
setwd(stats_dir)
pts <- read.csv("carbon_site.csv")
stopifnot(all(c("Longitude","Latitude","CarbonMgHa") %in% names(pts)))
pts_v <- vect(pts, geom = c("Longitude","Latitude"), crs = "EPSG:4326")

dem        <- rast("dem3_100.tif")
tpi        <- rast("TPI100.tif")
slope      <- rast("slope100.tif")
tavg       <- rast("temp2.tiff")        # mean T
tmax       <- rast("tempmax2.tiff")
prec       <- rast("precip2.tiff")
soilmoist  <- rast("soilmoist2.tiff")    

# Align to lc grid
tavg2      <- resample(tavg,      lc_str, method="bilinear")
tmax2      <- resample(tmax,      lc_str, method="bilinear")
prec2      <- resample(prec,      lc_str, method="bilinear")
soilmoist2 <- resample(soilmoist, lc_str, method="bilinear")

# Make levels explicit & stable
corine$Clasificac1 <- factor(corine$Clasificac1,
                             levels = c("Disturbed","Forest","Nosoil","Paramo"))
corine$lc_code <- as.integer(corine$Clasificac1)  # 1:Disturbed, 2:Forest, 3:Nosoil, 4:Paramo

# Rasterize numeric codes
lc_num <- rasterize(corine, cover1, field = "lc_code")

# Recode Paramo (<3000 m) → Forest by **numeric codes**
forest_code <- which(levels(corine$Clasificac1) == "Forest")   # 2
paramo_code <- which(levels(corine$Clasificac1) == "Paramo")   # 4

lc_num_adj <- lc_num
lc_num_adj[(dem < 3000) & (lc_num == paramo_code)] <- forest_code

# Use lc_num_adj in the predictor stack, then map codes → labels after extraction
pred_stack <- c(dem, tpi, slope, lc_num_adj, tavg2, tmax2, prec2, soilmoist2)
names(pred_stack) <- c("dem3_100","TPI100","slope100","lc_code","temp2","tempmax2","precip2","soilmoist2")

pred_at_pts <- terra::extract(pred_stack, pts_v)

dat <- cbind(pts, as.data.frame(pred_at_pts)[,-1])

# Attach human-readable class
lc_levels <- levels(corine$Clasificac1)

dat$Clasificac1 <- factor(dat$lc_code, levels = 1:length(lc_levels), labels = lc_levels)


# Extract predictors to points

## 2) After you built pred_stack and extracted to points:
pred_at_pts <- terra::extract(pred_stack, pts_v)
dat0 <- dplyr::bind_cols(pts, as.data.frame(pred_at_pts)[,-1]) %>%
  filter(!is.na(lc_code)) %>%
  mutate(
    # Original label (from numeric codes)
    LC_orig   = factor(lc_code, levels = seq_along(lc_levels), labels = lc_levels),
    # Modeling label: Nosoil -> Disturbed
    LC_model  = forcats::fct_collapse(LC_orig,
                                      Disturbed = c("Disturbed","Nosoil"),
                                      Forest    = "Forest",
                                      Paramo    = "Paramo"),
    # Response correction
    CarbonMgHa  = pmax(CarbonMgHa, 0),
    Carbon_corr = ifelse(LC_model == "Disturbed", CarbonMgHa * lambda_dist, CarbonMgHa),
    logSOC      = log(Carbon_corr + 1)
  ) %>%
  # keep only rows with complete predictors used in model
  tidyr::drop_na(dem3_100, TPI100, slope100, temp2, tempmax2, precip2, soilmoist2)

## A) If you still have 'corine' in memory and you created lc_code from it:
# Ensure deterministic order that matches how lc_code was made
corine$Clasificac1 <- factor(corine$Clasificac1,
                             levels = c("Disturbed","Forest","Nosoil","Paramo"))
lc_levels <- levels(corine$Clasificac1)

## B) If you don't have 'corine' available, hard-code the intended order:
# lc_levels <- c("Disturbed","Forest","Nosoil","Paramo")

# Map numeric codes to factor labels
dat0$Clasificac1 <- factor(dat0$lc_code,
                           levels = seq_along(lc_levels),
                           labels = lc_levels)

# 4) Clean up and proceed
# land-cover codes already extracted as 'lc_code'
# set the intended label order (edit if yours differs)

# consistent label order for your numeric lc_code
lc_levels <- c("Disturbed","Forest","Nosoil","Paramo")

# --- build land-cover columns in dat0 (robust to what's present) ---
if (!"LC_orig" %in% names(dat0)) {
  if ("lc_code" %in% names(dat0)) {
    dat0$LC_orig <- factor(dat0$lc_code,
                           levels = seq_along(lc_levels),
                           labels = lc_levels)
  } else if ("Clasificac1" %in% names(dat0)) {
    dat0$LC_orig <- factor(dat0$Clasificac1, levels = lc_levels)
  } else {
    stop("No land-cover found in dat0: expect 'lc_code' or 'Clasificac1'.")
  }
}

if (!"Class_model" %in% names(dat0)) {
  # collapse Nosoil into Disturbed for modeling
  dat0$Class_model <- fct_collapse(dat0$LC_orig,
                                   Disturbed = c("Disturbed","Nosoil"),
                                   Forest    = "Forest",
                                   Paramo    = "Paramo")
}

# response columns
if (!"Carbon_corr" %in% names(dat0)) {
  dat0$CarbonMgHa <- pmax(dat0$CarbonMgHa, 0)
  dat0$Carbon_corr <- ifelse(dat0$Class_model == "Disturbed",
                             dat0$CarbonMgHa * lambda_dist,
                             dat0$CarbonMgHa)
}
dat0$logSOC <- log(dat0$Carbon_corr + 1)

# quick sanity checks
stopifnot(all(c("LC_orig","Class_model","Carbon_corr","logSOC") %in% names(dat0)))


## ------------------------------------------------------
## 3) 100 m site clustering (deduplicate & prevent leak)
## ------------------------------------------------------
pts_sf <- st_as_sf(dat0, coords = c("Longitude","Latitude"), crs = 4326) %>%
  st_transform(3116)
xy <- st_coordinates(pts_sf)
cell_x <- floor(xy[,1] / block_size)
cell_y <- floor(xy[,2] / block_size)
dat0$cluster <- factor(paste(cell_x, cell_y, sep = "_"))

mode_fun <- function(x){
  x <- x[!is.na(x)]
  if (!length(x)) return(NA_character_)
  names(sort(table(x), decreasing = TRUE))[1]
}

dat <- dat0 %>%
  group_by(cluster) %>%
  summarise(
    Longitude   = mean(Longitude, na.rm = TRUE),
    Latitude    = mean(Latitude,  na.rm = TRUE),
    Carbon_corr = median(Carbon_corr,  na.rm = TRUE),
    dem3_100    = median(dem3_100,     na.rm = TRUE),
    TPI100      = median(TPI100,       na.rm = TRUE),
    slope100    = median(slope100,     na.rm = TRUE),
    temp2       = median(temp2,        na.rm = TRUE),
    tempmax2    = median(tempmax2,     na.rm = TRUE),
    precip2     = median(precip2,      na.rm = TRUE),
    soilmoist2  = median(soilmoist2,   na.rm = TRUE),
    LC_orig     = factor(mode_fun(LC_orig),  levels = levels(dat0$LC_orig)),
    LC_model    = factor(mode_fun(LC_model), levels = levels(dat0$LC_model)),
    .groups = "drop"
  ) %>%
  mutate(
    Carbon_corr = pmax(Carbon_corr, 0),
    logSOC      = log(Carbon_corr + 1),
    LC_orig     = droplevels(LC_orig),
    LC_model    = droplevels(LC_model),
    # BACK-COMPAT: legacy name used later in your code
    Clasificac1 = LC_model
  ) %>%
  drop_na(logSOC, dem3_100, TPI100, slope100, temp2, tempmax2, precip2, soilmoist2, LC_model)

# lock levels for later (prevents "new level" issues per fold)
lvl_lc <- levels(dat$LC_model)


  # ---- define the exact fields the model uses
preds <- c("temp2","tempmax2","precip2","soilmoist2","slope100","TPI100","dem3_100")

# 1) Recompute/guard the response
dat <- dat %>%
  mutate(
    Carbon_corr = pmax(Carbon_corr, 0),          # no negatives
    logSOC      = log(Carbon_corr + 1),          # finite for zeros
    Class_model = droplevels(LC_model)        # ensure factor
  )

# 2) Drop any rows with NA in response or predictors (after clustering)
dat <- dat %>%
  drop_na(logSOC, all_of(preds), Class_model)

# 3) (Optional) sanity check: no missing left
print(colSums(is.na(dat[, c("logSOC", preds, "Class_model")])))
stopifnot(!any(is.na(dat$logSOC)))

# 4) Fix factor levels once so folds are consistent
lvl_cm <- levels(dat$Class_model)


## ---------------------------------------------
## 4) Spatial CV folds (grouped by cluster id)
## ---------------------------------------------
set.seed(120)
gcv <- group_vfold_cv(dat, group = cluster, v = k_folds)

## -------------------------------------------------
## 5) Ranger RF with inner tuning + Duan smearing
## -------------------------------------------------


## 5) RF + inner OOB tuning + Duan smearing
preds <- c("temp2","tempmax2","precip2","soilmoist2","slope100","TPI100","dem3_100")

grid <- expand.grid(
  mtry            = c(2,3,4,5),
  min.node.size   = c(3,5,10),
  sample.fraction = c(0.6,0.8,1.0)
)

fit_rf <- function(tr, par){
  ranger(
    logSOC ~ LC_model + temp2 + tempmax2 + precip2 + soilmoist2 + slope100 + TPI100 + dem3_100,
    data = tr,
    num.trees = 1000,
    mtry = par$mtry,
    min.node.size = par$min.node.size,
    sample.fraction = par$sample.fraction,
    importance = "permutation",
    respect.unordered.factors = "partition",
    seed = 120
  )
}

# ===========================
# Spatial CV with calibration
# ===========================
fit_rf_weighted <- function(tr, par, w) {
  ranger::ranger(
    logSOC ~ LC_model + temp2 + tempmax2 + precip2 + soilmoist2 + slope100 + TPI100 + dem3_100,
    data = tr,
    num.trees = 1000,
    mtry = par$mtry,
    min.node.size = par$min.node.size,
    sample.fraction = par$sample.fraction,
    case.weights = w,                    # <-- up-weight high SOC
    importance = "permutation",
    respect.unordered.factors = "partition",
    seed = 120
  )
}

all_imp      <- vector("list", length(gcv$splits))  # RF perms per fold
gam_imp_list <- vector("list", length(gcv$splits))  # GAM term importance per fold
fold_metrics <- vector("list", length(gcv$splits))
ovp_list     <- vector("list", length(gcv$splits))

for (i in seq_along(gcv$splits)) {
  sp <- gcv$splits[[i]]
  tr <- analysis(sp); vl <- assessment(sp)

  # lock LC levels
  tr$LC_model <- factor(tr$LC_model, levels = lvl_lc)
  vl$LC_model <- factor(vl$LC_model, levels = lvl_lc)

  # -------- 1) weights to emphasize the upper tail (cap to avoid extremes)
  w_tr <- rescale(tr$Carbon_corr, to = c(1, 3))
  w_tr[!is.finite(w_tr)] <- 1

  # -------- 2) inner tuning by OOB RMSE (log space) with weights
  inner <- lapply(seq_len(nrow(grid)), function(j){
    par <- grid[j,]; fit <- fit_rf_weighted(tr, par, w = w_tr)
    tibble(j = j, oob_rmse = sqrt(fit$prediction.error))
  }) %>% bind_rows() %>% arrange(oob_rmse)

  best <- grid[inner$j[1],]
  rf   <- fit_rf_weighted(tr, best, w = w_tr)


      # ---- RF permutation importance (handle 'Importance' or 'Overall' column names) ----
    imp_rf_fold <- vip::vi(rf)
    if (!"Importance" %in% names(imp_rf_fold) && "Overall" %in% names(imp_rf_fold)) {
      imp_rf_fold <- dplyr::rename(imp_rf_fold, Importance = Overall)
    }
    imp_rf_fold <- imp_rf_fold %>%
      arrange(desc(Importance)) %>%
      mutate(RelImp = 100 * Importance / max(Importance), fold = i)
    all_imp[[i]] <- imp_rf_fold

  # -------- 3) TRAIN OOB preds (log) -> smearing -> SOC
  tr_log_oob <- rf$predictions
  if (is.null(tr_log_oob)) stop("ranger did not return OOB predictions.")
  smear <- mean(exp(tr$logSOC - tr_log_oob))
  tr_soc_oob <- pmax(0, (exp(tr_log_oob) * smear) - 1)

  # -------- 4) VALIDATION preds (log) -> SOC (raw RF)
  vl_log     <- predict(rf, data = vl)$predictions
  vl_soc_raw <- pmax(0, (exp(vl_log) * smear) - 1)

  # -------- 5) Residual GAM on TRAIN (no leakage), SOC space
  #     Captures remaining bias vs predicted value, environment, and spatial trend.
  tr_resid <- tr$Carbon_corr - tr_soc_oob
  gam_mod <- mgcv::gam(
    tr_resid ~ s(tr_soc_oob, k = 5) +
               s(dem3_100, k = 5) +
               s(precip2,  k = 5) +
               LC_model +
               s(Longitude, Latitude, bs = "tp", k = 80),
    data   = tibble(tr_resid, tr_soc_oob, dem3_100 = tr$dem3_100,
                    precip2 = tr$precip2, LC_model = tr$LC_model,
                    Longitude = tr$Longitude, Latitude = tr$Latitude),
    method = "REML", select = TRUE
  )

  # --- add this block inside the for-loop, after fitting gam_mod ---
if (!exists("gam_imp_list")) gam_imp_list <- vector("list", length(gcv$splits))

# ---- GAM importance proxy from summary(gam_mod) ----
      gs <- summary(gam_mod)

      # smooth terms: s(...) rows
      s_tab <- as.data.frame(gs$s.table)
      s_tab$term <- rownames(gs$s.table)
      s_tab$imp  <- if ("F" %in% names(s_tab)) s_tab$F * s_tab$edf else s_tab$Chi.sq

      # parametric LC_model coefficients -> one aggregate "LC_model" importance
      gi <- tibble(term = s_tab$term, imp = s_tab$imp)
      if (!is.null(gs$p.table) && nrow(gs$p.table) > 0) {
        p_tab <- as.data.frame(gs$p.table)
        p_tab$term <- rownames(gs$p.table)
        lc_rows <- grepl("^LC_model", p_tab$term)
        if (any(lc_rows)) {
          lc_imp <- sum(p_tab$`t value`[lc_rows]^2, na.rm = TRUE)
          gi <- bind_rows(gi, tibble(term = "LC_model", imp = lc_imp))
        }
      }
      gam_imp_list[[i]] <- gi


  # Predicted residual correction on VALIDATION
  vl_resid_hat <- predict(gam_mod, newdata = tibble(
    tr_soc_oob = vl_soc_raw,
    dem3_100   = vl$dem3_100,
    precip2    = vl$precip2,
    LC_model   = vl$LC_model,
    Longitude  = vl$Longitude,
    Latitude   = vl$Latitude
  ))

  # -------- 6) Final calibrated predictions
  vl_soc <- pmax(0, vl_soc_raw + vl_resid_hat)

  # -------- 7) Store metrics & pooled OVP
  fold_metrics[[i]] <- tibble(
    fold = i,
    RMSE = yardstick::rmse_vec(vl$Carbon_corr, vl_soc),
    MAE  = yardstick::mae_vec (vl$Carbon_corr, vl_soc),
    R2   = yardstick::rsq_vec (vl$Carbon_corr, vl_soc)
  )

  ovp_list[[i]] <- tibble(fold = i, obs = vl$Carbon_corr, pred = vl_soc, lc = vl$LC_model)
}



# Aggregate performance and rebuild OVP as you already do
spat_perf <- dplyr::bind_rows(fold_metrics) %>% dplyr::summarise(across(-fold, mean))
cat("\n=== Spatial CV (Yeo–Johnson + calibrated) ===\n"); print(spat_perf)

ovp <- ovp_list %>%
  purrr::compact() %>% purrr::keep(~ nrow(.x) > 0) %>%
  dplyr::bind_rows() %>%
  dplyr::mutate(lc = droplevels(as.factor(as.character(lc))))




lims  <- range(c(ovp$obs, ovp$pred), finite = TRUE)
pad   <- diff(lims) * 0.04
lims2 <- c(lims[1] - pad, lims[2] + pad)

ggplot(ovp, aes(obs, pred, colour = lc)) +
  geom_point(alpha = .45) +
  geom_abline(slope = 1, intercept = 0, linetype = 2) +
  labs(x = expression("Observed SOC (Mg C ha"^-1*")"),
       y = expression("Predicted SOC (Mg C ha"^-1*")"),
       colour = "Land cover",
       title = "Observed vs Predicted — spatial CV (calibrated, pooled)") +
  coord_equal(xlim = lims2, ylim = lims2, expand = FALSE) +
  theme_bw()





# ----- RF importance averaged across folds -----
suppressPackageStartupMessages({ library(dplyr); library(tidyr); library(ggplot2); library(purrr); library(scales) })

# --- REQUIREMENT: 'all_imp' must contain one data.frame per fold with columns:
#     Variable, Importance or Overall, and (optionally) RelImp + fold
stopifnot(exists("all_imp"), length(compact(all_imp)) > 0)

# RF variables actually used in the model
rf_vars <- c("LC_model","temp2","tempmax2","precip2","soilmoist2","slope100","TPI100","dem3_100")

# Harmonize each fold's importance table and scale 0–100 within fold
imp_by_fold <- imap(compact(all_imp), function(df, i){
  df <- as.data.frame(df)
  # handle naming differences from vip::vi()
  if (!"Importance" %in% names(df) && "Overall" %in% names(df)) {
    df <- dplyr::rename(df, Importance = Overall)
  }
  df %>%
    mutate(fold = i,
           RelImp = 100 * Importance / max(Importance, na.rm=TRUE)) %>%
    filter(Variable %in% rf_vars)
})

imp_rf_all <- bind_rows(imp_by_fold)

# Nice labels for the axis
map_rf <- c(
  "LC_model"   = "Land cover",
  "temp2"      = "MAT",
  "tempmax2"   = "Tmax",
  "precip2"    = "Precipitation",
  "soilmoist2" = "Soil moisture",
  "slope100"   = "Slope",
  "TPI100"     = "TPI",
  "dem3_100"   = "Elevation"
)

imp_rf_all <- imp_rf_all %>%
  mutate(VarLab = recode(Variable, !!!map_rf, .default = Variable))

# Summarize across folds: mean ± 95% CI
rf_sum <- imp_rf_all %>%
  group_by(VarLab) %>%
  summarise(
    mean_rel = mean(RelImp, na.rm = TRUE),
    sd_rel   = sd(RelImp,   na.rm = TRUE),
    n        = dplyr::n(),
    se_rel   = sd_rel / sqrt(n),
    lo95     = pmax(0, mean_rel - 1.96 * se_rel),
    hi95     = pmin(100, mean_rel + 1.96 * se_rel),
    .groups  = "drop"
  ) %>%
  arrange(mean_rel)

# Horizontal barplot with CI bars
rf_imp_plot <- ggplot(rf_sum,
                      aes(x = mean_rel, y = factor(VarLab, levels = rf_sum$VarLab))) +
  geom_col(width = 0.7, fill = "lightgrey") +
  geom_errorbarh(aes(xmin = lo95, xmax = hi95), height = 0.42, linewidth = 0.5) +
  scale_x_continuous("Relative importance (0–100)", limits = c(0, 110), expand = c(0, 0)) +
  labs(y = NULL, title = "Random forest variable importance (permutation, mean ± 95% CI)") +
  theme_bw() +
  theme(
    panel.grid.major.y = element_blank(),
    panel.grid.minor   = element_blank(),
    axis.title.y       = element_blank(),
    plot.title         = element_text(size = 11),
    axis.text.y        = element_text(size = 9)
  )

rf_imp_plot

## -------------------------------------------------
## 6) Trend plots: SOC vs elevation & precipitation
##    (using corrected field SOC; colour by land cover)
## -------------------------------------------------
# ----- RF importance averaged across folds -----


ovp <- ovp_list %>%
  compact() %>% keep(~ nrow(.x) > 0) %>%
  bind_rows() %>%
  mutate(lc = droplevels(as.factor(as.character(lc)))) %>%
  filter(is.finite(obs), is.finite(pred))

r2_pool <- yardstick::rsq_vec(ovp$obs, ovp$pred)

lims  <- c(10,450)
pad   <- diff(lims) * 0.06
lims2 <- c(lims[1] - pad, lims[2] + pad)

left_plot <- ggplot(ovp, aes(obs, pred, colour = lc)) +
  geom_point(alpha = .45) +
  geom_abline(slope = 1, intercept = 0, linetype = 2) +
  coord_equal(xlim = lims2, ylim = lims2, expand = TRUE) +
  labs(x = expression("Observed SOC (Mg C ha"^-1*")"),
       y = expression("Predicted SOC (Mg C ha"^-1*")"),
       colour = NULL) +
  theme_bw() +
  theme(panel.grid = element_blank(),
        legend.position = c(0.1, 0.98),
        legend.justification = c(0,1),
        legend.background = element_rect(fill = alpha("white", 0.65), colour = NA),
        legend.key.size = unit(0.35, "lines"),
        legend.title = element_text(size = 9),
        legend.text  = element_text(size = 8))


# ---------- Assemble the 2-panel figure ----------

# optional: add figure tags "A", "B"
panel_vert <- (left_plot + labs(title = NULL)) /
              (rf_imp_plot + labs(title = NULL)) +
              plot_layout(ncol = 1, heights = c(1.5, 1)) +
              plot_annotation(tag_levels = "A")  # adds A, B in the corners

# show it
panel_vert

# -------------------------------------------------

# helpers
r2_vec <- function(y, yhat) 1 - mean((y - yhat)^2) / var(y)

# ------- RF: validation permutation ΔR² per feature -------
rf_vars <- c("LC_model","temp2","tempmax2","precip2","soilmoist2","slope100","TPI100","dem3_100")
base_r2_rf <- r2_vec(vl$Carbon_corr, vl_soc_raw)

rf_abs_imp <- lapply(rf_vars, function(v){
  vl_perm <- vl
  if (is.factor(vl_perm[[v]])) {
    vl_perm[[v]] <- sample(vl_perm[[v]])
  } else {
    vl_perm[[v]] <- sample(vl_perm[[v]])
  }
  vl_log_p  <- predict(rf, data = vl_perm)$predictions
  vl_soc_p  <- pmax(0, (exp(vl_log_p) * smear) - 1)  # same smear from TRAIN
  r2_drop   <- base_r2_rf - r2_vec(vl$Carbon_corr, vl_soc_p)
  tibble::tibble(Variable = v, dR2 = r2_drop)
}) |> dplyr::bind_rows()

# ------- GAM: validation ΔR² by dropping each term -------
# full corrected preds already: vl_soc = vl_soc_raw + vl_resid_hat
base_r2_full <- r2_vec(vl$Carbon_corr, vl_soc)

# get term-wise contributions on VALIDATION
terms_mat <- predict(gam_mod, newdata = tibble::tibble(
  tr_soc_oob = vl_soc_raw,
  dem3_100   = vl$dem3_100,
  precip2    = vl$precip2,
  LC_model   = vl$LC_model,
  Longitude  = vl$Longitude,
  Latitude   = vl$Latitude
), type = "terms")

const <- attr(terms_mat, "constant"); if (is.null(const)) const <- 0
term_names <- colnames(terms_mat)

gam_abs_imp <- lapply(term_names, function(term){
  resid_no_term <- rowSums(terms_mat[, setdiff(term_names, term), drop=FALSE]) + const
  vl_soc_drop   <- pmax(0, vl_soc_raw + resid_no_term)
  r2_drop       <- base_r2_full - r2_vec(vl$Carbon_corr, vl_soc_drop)
  tibble::tibble(Term = term, dR2 = r2_drop)
}) |> dplyr::bind_rows()





## 6) Trend plots: SOC vs elevation & precipitation
##    (using corrected field SOC; colour by land cover)
## -------------------------------------------------


library(dplyr)
library(ggplot2)
library(gridExtra)

# Alias once (already set above after clustering, but ensure here too)
dat$Clasificac1 <- dat$LC_model

# For the trend plots:
stopifnot(all(c("Carbon_corr","Clasificac1","dem3_100","temp2","precip2","soilmoist2","slope100","TPI100") %in% names(dat)))
df <- dat %>%
  drop_na(Carbon_corr, Clasificac1, dem3_100, temp2, precip2, soilmoist2, slope100, TPI100) %>%
  mutate(LandCover = droplevels(Clasificac1))


## 0) Start from your clustered dataset 'dat'
##    Columns used: Carbon_corr, Clasificac1, dem3_100, temp2, precip2, soilmoist2, slope100, TPI100
stopifnot(all(c("Carbon_corr","Clasificac1","dem3_100","temp2","precip2","soilmoist2","slope100","TPI100") %in% names(dat)))

# Optional: sample for speed if you have many points
set.seed(7)
df <- dat %>% tidyr::drop_na(Carbon_corr, Clasificac1, dem3_100, temp2, precip2, soilmoist2, slope100, TPI100)
if (nrow(df) > 20000) df <- dplyr::slice_sample(df, n = 20000)

# If you still want to mask low elevations like 'dem3_1002'
df <- df %>%
  mutate(
    LandCover = droplevels(Clasificac1),
    ElevMask  = ifelse(dem3_100 < 2200, NA, dem3_100) # <- comment this line out if you don't want masking
  )

pal_legend <- scale_colour_brewer("Land cover", palette = "Set1")

## 1) Carbon vs Elevation
Cplot1 <- ggplot(df, aes(x = ElevMask, y = Carbon_corr, group = LandCover)) +
  theme_bw() +
  geom_point(aes(colour = LandCover), size = 0.5, alpha = 0.5, na.rm = TRUE) +
  geom_smooth(aes(colour = LandCover), formula = y ~ poly(x, 2), method = "lm", se = FALSE, na.rm = TRUE) +
  pal_legend +
  scale_x_continuous("Elevation (m)") +
  labs(y = expression(paste("Carbon content (Mg ha"^-1, ")"))) +
  coord_cartesian(ylim = c(50, 400)) +
  theme(legend.position = "none")

## 2) Carbon vs MAT (temp2)
Cplot2 <- ggplot(df, aes(x = temp2, y = Carbon_corr, group = LandCover)) +
  theme_bw() +
  geom_point(aes(colour = LandCover), size = 0.5, alpha = 0.5) +
  geom_smooth(aes(colour = LandCover), formula = y ~ poly(x, 3), method = "lm", se = FALSE) +
  pal_legend +
  scale_x_continuous("MAT (°C)") +
  labs(y = expression(paste("Carbon content (Mg ha"^-1, ")"))) +
  coord_cartesian(ylim = c(50, 400)) +
  theme(legend.position = c(0.8, 0.8))

## 3) Carbon vs Precipitation
Cplot3 <- ggplot(df, aes(x = precip2, y = Carbon_corr, group = LandCover)) +
  theme_bw() +
  geom_point(aes(colour = LandCover), size = 0.5, alpha = 0.5) +
  geom_smooth(aes(colour = LandCover), formula = y ~ poly(x, 2), method = "lm", se = FALSE) +
  pal_legend +
  scale_x_continuous(expression(paste("Annual precipitation (mm y"^-1, ")"))) +
  labs(y = expression(paste("Carbon content (Mg ha"^-1, ")"))) +
  coord_cartesian(ylim = c(50, 400)) +
  theme(legend.position = "none")

## 3.1) Precipitation vs Elevation
Cplot3_1 <- ggplot(df, aes(x = ElevMask, y = precip2, group = LandCover)) +
  theme_bw() +
  geom_point(aes(colour = LandCover), size = 0.5, alpha = 0.5, na.rm = TRUE) +
  geom_smooth(aes(colour = LandCover), formula = y ~ poly(x, 2), method = "lm", se = FALSE, na.rm = TRUE) +
  pal_legend +
  scale_y_continuous(expression(paste("Annual precipitation (mm y"^-1, ")"))) +
  scale_x_continuous("Elevation (m)") +
  theme(legend.position = "none")

## 4) Carbon vs Soil moisture
Cplot4 <- ggplot(df, aes(x = soilmoist2, y = Carbon_corr, group = LandCover)) +
  theme_bw() +
  geom_point(aes(colour = LandCover), size = 0.5, alpha = 0.5) +
  geom_smooth(aes(colour = LandCover), formula = y ~ poly(x, 2), method = "lm", se = FALSE) +
  pal_legend +
  scale_x_continuous(expression(paste("Soil moisture (m"^3,"/m"^3*")",sep=""))) +
  labs(y = expression(paste("Carbon content (Mg ha"^-1, ")"))) +
  coord_cartesian(ylim = c(50, 400)) +
  theme(legend.position = "none")

## 5) Carbon vs Slope
Cplot5 <- ggplot(df, aes(x = slope100, y = Carbon_corr, group = LandCover)) +
  theme_bw() +
  geom_point(aes(colour = LandCover), size = 0.5, alpha = 0.5) +
  geom_smooth(aes(colour = LandCover), formula = y ~ poly(x, 2), method = "lm", se = FALSE) +
  pal_legend +
  scale_x_continuous("Slope (Degrees)") +
  labs(y = expression(paste("Carbon content (Mg ha"^-1, ")"))) +
  coord_cartesian(ylim = c(50, 400)) +
  theme(legend.position = "none")

## 6) Carbon vs TPI (points + polynomial smooth)
Cplot6 <- ggplot(df, aes(x = TPI100, y = Carbon_corr)) +
  theme_bw() +
  geom_point(aes(colour = LandCover), size = 0.5, alpha = 0.5) +
  geom_smooth(aes(colour = LandCover), formula = y ~ poly(x, 3), method = "lm", se = FALSE) +
  pal_legend +
  scale_x_continuous("TPI") +
  labs(y = expression(paste("Carbon content (Mg ha"^-1, ")"))) +
  coord_cartesian(xlim = c(-15, 15), ylim = c(50, 400)) +
  theme(legend.position = "none")

## 7) TPI-binned lines (replicates your tpi_agg overlay)
breaks_tpi <- seq(-15, 15, by = 2.5)
df$spaces <- cut(df$TPI100, breaks = breaks_tpi, include.lowest = TRUE)
tpi_agg <- df %>%
  group_by(spaces, LandCover) %>%
  summarise(Carbon_mean = mean(Carbon_corr, na.rm = TRUE), .groups = "drop") %>%
  tidyr::separate_wider_delim(spaces, delim = ",", names = c("lo","hi"), too_few = "align_start") %>%
  mutate(TPI = as.numeric(gsub("\\(|\\[|\\]|\\)", "", lo)) + 1.25) %>% # mid-ish point
  mutate(TPI = pmin(pmax(TPI, -15), 15))

Cplot7 <- ggplot(df, aes(x = TPI100, y = Carbon_corr)) +
  theme_bw() +
  geom_point(aes(colour = LandCover), size = 0.5, alpha = 0.3) +
  geom_line(data = tpi_agg, aes(x = TPI, y = Carbon_mean, colour = LandCover), linewidth = 1.0) +
  pal_legend +
  scale_x_continuous("TPI", limits = c(-15, 15)) +
  labs(y = expression(paste("Carbon content (Mg ha"^-1, ")"))) +
  coord_cartesian(ylim = c(50, 250)) +
  theme(legend.position = "none")

## 8) Bar chart: mean ± SD SOC by land cover
carbon_agg <- df %>% group_by(LandCover) %>%
  summarise(meanC = mean(Carbon_corr, na.rm=TRUE),
            sdC   = std(Carbon_corr,   na.rm=TRUE)/2, .groups="drop")

C_dist1 <- ggplot(carbon_agg, aes(x = LandCover, y = meanC, fill = LandCover)) +
  theme_bw() +
  geom_col(alpha = 0.5, position = position_dodge(width = 0.8)) +
  geom_errorbar(aes(ymin = meanC - sdC, ymax = meanC + sdC),
                width = 0.5, position = position_dodge(width = 0.8)) +
  scale_fill_brewer("", palette = "Set1") +
  scale_y_continuous(expression(paste("Carbon content (Mg ha"^-1, ")"))) +
  labs(x = NULL) +
  theme(legend.position = "none")
library(grid)      # for unit()
library(scales)    # for alpha(), optional

# tiny, in-panel legend + no grids
legend_inset <- theme(
  panel.grid.major = element_blank(),
  panel.grid.minor = element_blank(),
  legend.position  = c(0.02, 0.98),      # top-left inside panel
  legend.justification = c(0, 1),
  legend.title = element_text(size = 9),
  legend.text  = element_text(size = 8),
  legend.key.size = unit(0.35, "lines"),
  legend.background = element_rect(fill = alpha("white", 0.65), colour = NA),
  legend.margin = margin(0, 0, 0, 0),
  legend.box.margin = margin(0, 0, 0, 0)
)

# make the color legend compact
legend_guides <- guides(
  colour = guide_legend(override.aes = list(alpha = 1, size = 2), ncol = 1)
)

# helper to apply the style (also overrides any previous legend.position="none")
tweak <- function(p) p + legend_inset + legend_guides

# apply to each plot
Cplot1  <- tweak(Cplot1)
Cplot2  <- tweak(Cplot2)
Cplot3  <- tweak(Cplot3)
Cplot3_1<- tweak(Cplot3_1)
Cplot4  <- tweak(Cplot4)
Cplot5  <- tweak(Cplot5)
Cplot6  <- tweak(Cplot6)
Cplot7  <- tweak(Cplot7)
C_dist1 <- tweak(C_dist1)   # if you want an inset legend there too

## 9) Arrange some panels like before
grid.arrange(Cplot1, Cplot2, Cplot5, Cplot3, Cplot6, C_dist1, ncol = 2)

# ================================
# "General model" R² and diagnostics
# ================================




suppressPackageStartupMessages({
  library(dplyr); library(rsample); library(yardstick)
  library(ranger); library(mgcv); library(scales); library(tibble)
})

# --------------------------
# Guard: required columns
# --------------------------
stopifnot(all(c(
  "Carbon_corr","logSOC","LC_model",
  "dem3_100","TPI100","slope100","temp2","tempmax2","precip2","soilmoist2",
  "Longitude","Latitude"
) %in% names(dat)))

# --------------------------
# Common pieces
# --------------------------
set.seed(120)

preds <- c("LC_model","temp2","tempmax2","precip2","soilmoist2","slope100","TPI100","dem3_100")

grid <- expand.grid(
  mtry            = c(2,3,4,5),
  min.node.size   = c(3,5,10),
  sample.fraction = c(0.6,0.8,1.0)
)

fit_rf <- function(tr, par, w) {
  ranger::ranger(
    logSOC ~ LC_model + temp2 + tempmax2 + precip2 + soilmoist2 + slope100 + TPI100 + dem3_100,
    data = tr,
    num.trees = 1000,
    mtry = par$mtry,
    min.node.size = par$min.node.size,
    sample.fraction = par$sample.fraction,
    case.weights = w,                    # up-weight high SOC
    importance = "permutation",
    respect.unordered.factors = "partition",
    seed = 120
  )
}

# ----------------------------------------------------
# A) Tune weighted RF on the FULL data via OOB RMSE
# ----------------------------------------------------
w_full <- scales::rescale(dat$Carbon_corr, to = c(1, 3))
w_full[!is.finite(w_full)] <- 1

oob_scores <- lapply(seq_len(nrow(grid)), function(j){
  par <- grid[j,]
  fit <- fit_rf(dat, par, w = w_full)
  tibble(j = j, oob_rmse = sqrt(fit$prediction.error))
}) %>% bind_rows() %>% arrange(oob_rmse)

best <- grid[oob_scores$j[1], ]
rf_full <- fit_rf(dat, best, w = w_full)

# ----------------------------------------------------
# B) OOB + In-sample diagnostics for the FULL model
#     (RF on log scale + Duan smearing)
# ----------------------------------------------------
# OOB (log) R2
oob_mse_log <- rf_full$prediction.error
var_y_log   <- var(dat$logSOC)
R2_oob_log  <- 1 - oob_mse_log / var_y_log

# OOB SOC metrics (back-transform with Duan smearing from OOB)
yhat_log_oob <- rf_full$predictions                     # OOB preds for each row
smear_oob    <- mean(exp(dat$logSOC - yhat_log_oob))
yhat_soc_oob <- pmax(0, (exp(yhat_log_oob) * smear_oob) - 1)

oob_soc <- tibble(
  R2   = rsq_vec(dat$Carbon_corr, yhat_soc_oob),
  RMSE = rmse_vec(dat$Carbon_corr, yhat_soc_oob),
  MAE  = mae_vec (dat$Carbon_corr, yhat_soc_oob)
)

# In-sample SOC metrics (optimistic, but reported for completeness)
yhat_log_in  <- predict(rf_full, data = dat)$predictions
smear_in     <- mean(exp(dat$logSOC - yhat_log_in))
yhat_soc_in  <- pmax(0, (exp(yhat_log_in) * smear_in) - 1)

insamp_soc <- tibble(
  R2   = rsq_vec(dat$Carbon_corr, yhat_soc_in),
  RMSE = rmse_vec(dat$Carbon_corr, yhat_soc_in),
  MAE  = mae_vec (dat$Carbon_corr, yhat_soc_in)
)

# ----------------------------------------------------
# C) Random 5-fold CV (non-spatial) with SAME pipeline:
#    weighted RF + Duan smearing + residual GAM calibration
# ----------------------------------------------------
set.seed(120)
kcv <- vfold_cv(dat, v = 5, strata = LC_model)

rand_metrics <- lapply(kcv$splits, function(s){
  tr <- analysis(s); vl <- assessment(s)

  # weights from TRAIN only
  w_tr <- scales::rescale(tr$Carbon_corr, to = c(1, 3))
  w_tr[!is.finite(w_tr)] <- 1

  # inner tuning on TRAIN (OOB RMSE, log space)
  inner <- lapply(seq_len(nrow(grid)), function(j){
    par <- grid[j,]; fit <- fit_rf(tr, par, w = w_tr)
    tibble(j = j, oob_rmse = sqrt(fit$prediction.error))
  }) %>% bind_rows() %>% arrange(oob_rmse)

  best_tr <- grid[inner$j[1],]
  rf_tr   <- fit_rf(tr, best_tr, w = w_tr)

  # TRAIN OOB smearing
  tr_log_oob <- rf_tr$predictions
  if (is.null(tr_log_oob)) stop("ranger did not return OOB predictions.")
  smear <- mean(exp(tr$logSOC - tr_log_oob))

  # VALIDATION raw RF predictions in SOC
  vl_log     <- predict(rf_tr, data = vl)$predictions
  vl_soc_raw <- pmax(0, (exp(vl_log) * smear) - 1)

  # Residual GAM calibration on TRAIN (SOC space, no leakage)
  tr_soc_oob <- pmax(0, (exp(tr_log_oob) * smear) - 1)
  tr_resid   <- tr$Carbon_corr - tr_soc_oob

  gam_mod <- mgcv::gam(
    tr_resid ~ s(tr_soc_oob, k = 5) +
               s(dem3_100, k = 5) +
               s(precip2,  k = 5) +
               LC_model +
               s(Longitude, Latitude, bs = "tp", k = 80),
    data   = tibble(tr_resid, tr_soc_oob,
                    dem3_100 = tr$dem3_100, precip2 = tr$precip2,
                    LC_model = tr$LC_model,
                    Longitude = tr$Longitude, Latitude = tr$Latitude),
    method = "REML", select = TRUE
  )

  vl_resid_hat <- predict(gam_mod, newdata = tibble(
    tr_soc_oob = vl_soc_raw,
    dem3_100   = vl$dem3_100,
    precip2    = vl$precip2,
    LC_model   = vl$LC_model,
    Longitude  = vl$Longitude,
    Latitude   = vl$Latitude
  ))

  vl_soc <- pmax(0, vl_soc_raw + vl_resid_hat)

  tibble(
    RMSE = rmse_vec(vl$Carbon_corr, vl_soc),
    MAE  = mae_vec (vl$Carbon_corr, vl_soc),
    R2   = rsq_vec (vl$Carbon_corr, vl_soc)
  )
})

randcv_soc <- bind_rows(rand_metrics) %>% summarise(across(everything(), mean))

# ----------------------------------------------------
# D) Print a compact summary
# ----------------------------------------------------
cat("\n==== General model diagnostics (weighted RF + calibration) ====\n")
cat("OOB (log space) R2:", round(R2_oob_log, 3), "\n")
cat("\nOOB (SOC space, Duan smearing):\n"); print(oob_soc)
cat("\nIn-sample (SOC space, optimistic):\n");     print(insamp_soc)
cat("\nRandom 5-fold CV (SOC space, weighted RF + residual GAM):\n"); print(randcv_soc)

# If you already computed spatial CV as 'spat_perf' (RF + GAM):
# cat("\nSpatial 5-fold CV (SOC space, weighted RF + residual GAM):\n"); print(spat_perf)



