## ========================== 0) Setup (no dplyr) ==========================
suppressPackageStartupMessages({
  library(ggplot2)
  library(mgcv)
  library(gridExtra)
  library(rlang)   # for .data
  library(scales)  # for squish
})

## ========================== 1) Load data ================================
path <- "/Users/juanbenavides/Library/CloudStorage/Dropbox/papers/peat 210 pb/stats2/AGE_DEPTH_with_Tanoms_Tabs_CAR_PAR_PRE.csv"
master <- read.csv(path, stringsAsFactors = FALSE)

## ========================== 2) Columns =================================
x_col      <- "Tmean_anom_C"          # mean temp anomaly (°C)
y_col      <- "elevation"             # m a.s.l.
z_peat_col <- "PAR_cm_yr"             # cm yr⁻¹
z_carb_col <- "CAR_app_g_m2_yr"       # g m⁻² yr⁻¹

## ========================== 3) Helpers =================================
clean_keep <- function(df, cols) {
  ok <- rep(TRUE, nrow(df))
  for (nm in cols) ok <- ok & is.finite(df[[nm]])
  df[ok, cols, drop = FALSE]
}

robust_limits <- function(v, probs = c(0.05, 0.95)) {
  v <- v[is.finite(v)]
  q <- quantile(v, probs = probs, na.rm = TRUE)
  if (!is.finite(q[1]) || !is.finite(q[2]) || q[1] >= q[2]) range(v, na.rm = TRUE) else q
}

gam_heatmap_with_points <- function(df, x_col, y_col, z_col,
                                    nx = 220, ny = 220,
                                    x_lab = "Temperature anomaly (°C)",
                                    y_lab = "Elevation (m a.s.l.)",
                                    fill_lab = "Value",
                                    fill_limits = NULL,
                                    fill_trans = "sqrt",
                                    point_size = 0.8, point_alpha = 0.7) {

  dfc <- clean_keep(df, c(x_col, y_col, z_col))
  if (nrow(dfc) < 10) stop("Not enough rows after filtering.")

  # auto-set knots by available unique values
  ux <- length(unique(dfc[[x_col]]))
  uy <- length(unique(dfc[[y_col]]))
  kx <- max(3, min(12, ux - 1))
  ky <- max(3, min(12, uy - 1))

  # fit GAM (retry with smaller k if needed)
  attempt <- 0; fit <- NULL
  while (attempt < 10) {
    attempt <- attempt + 1
    fmla <- as.formula(paste0(z_col, " ~ te(", y_col, ", ", x_col, ", k=c(", ky, ",", kx, "), bs='tp')"))
    fit <- try(mgcv::gam(fmla, data = dfc, method = "REML", select = TRUE), silent = TRUE)
    if (!inherits(fit, "try-error")) break
    kx <- max(3, kx - 1); ky <- max(3, ky - 1)
  }
  if (inherits(fit, "try-error")) stop("GAM failed. Try fewer knots or coarser grid.")

  # prediction grid (use global ranges so both panels align)
  x_seq <- seq(min(dfc[[x_col]], na.rm = TRUE), max(dfc[[x_col]], na.rm = TRUE), length.out = nx)
  y_seq <- seq(min(dfc[[y_col]], na.rm = TRUE), max(dfc[[y_col]], na.rm = TRUE), length.out = ny)
  grid  <- expand.grid(x_seq, y_seq, KEEP.OUT.ATTRS = FALSE, stringsAsFactors = FALSE)
  names(grid) <- c(x_col, y_col)
  grid$pred <- as.numeric(predict(fit, newdata = grid, type = "response"))

  # color limits
  if (is.null(fill_limits)) fill_limits <- robust_limits(dfc[[z_col]], c(0.05, 0.95))

  # plot heatmap + observed locations
  ggplot(grid, aes(x = .data[[x_col]], y = .data[[y_col]], fill = .data[["pred"]])) +
    geom_raster(interpolate = TRUE) +
    scale_fill_viridis_c(name = fill_lab,
                         limits = fill_limits, oob = squish,
                         trans = fill_trans, na.value = "grey90") +
    # overlay observed points (locations only)
    geom_point(data = dfc,
               mapping = aes(x = .data[[x_col]], y = .data[[y_col]]),
               inherit.aes = FALSE,
               shape = 21, size = point_size, stroke = 0.2,
               colour = "black", fill = "white", alpha = point_alpha) +
    labs(x = x_lab, y = y_lab) +
    coord_cartesian(xlim = range(dfc[[x_col]], na.rm = TRUE),
                    ylim = range(dfc[[y_col]], na.rm = TRUE),
                    expand = FALSE) +
    theme_minimal(base_size = 12) +
    theme(panel.grid.minor = element_blank(),
          legend.position = "right")
}

## ========================== 4) Build both panels ========================
# Use variable-specific robust limits so each panel has good contrast
lims_peat <- robust_limits(master[[z_peat_col]], c(0.05, 0.95))
lims_carb <- robust_limits(master[[z_carb_col]], c(0.05, 0.95))

# ---- expression-based labels (no unicode) ----
lab_x      <- bquote(Temperature~anomaly~"(" * degree*C * ")")
lab_y      <- "Elevation (m a.s.l.)"
lab_fill_P <- bquote(Peat~acc~"(" * cm~yr^{-1} * ")")
lab_fill_C <- bquote(Carbon~acc~"(" * g~m^{-2}~yr^{-1} * ")")

# build panels with expression labels
p_peat <- gam_heatmap_with_points(
  master, x_col, y_col, z_peat_col,
  x_lab = lab_x, y_lab = lab_y, fill_lab = lab_fill_P,
  fill_limits = lims_peat, fill_trans = "sqrt",
  point_size = 0.9, point_alpha = 0.8
)

p_carb <- gam_heatmap_with_points(
  master, x_col, y_col, z_carb_col,
  x_lab = lab_x, y_lab = lab_y, fill_lab = lab_fill_C,
  fill_limits = lims_carb, fill_trans = "sqrt",
  point_size = 0.9, point_alpha = 0.8
)
# --- compact horizontal legends at the bottom (for BOTH plots) ---
compact_legend <- theme(
  legend.position   = "bottom",
  legend.direction  = "horizontal",
  legend.title      = element_text(size = 9),
  legend.text       = element_text(size = 8),
  legend.box.margin = margin(t = 2, r = 0, b = 0, l = 0),
  plot.margin       = margin(10, 6, 2, 2)
)

p_peat <- p_peat +
  guides(fill = guide_colorbar(
    direction      = "horizontal",
    title.position = "top", title.hjust = 0.5,
    barwidth       = grid::unit(40, "mm"),
    barheight      = grid::unit(3,  "mm"),
    label.position = "bottom",
    ticks          = TRUE
  )) +
  compact_legend

p_carb <- p_carb +
  guides(fill = guide_colorbar(
    direction      = "horizontal",
    title.position = "top", title.hjust = 0.5,
    barwidth       = grid::unit(40, "mm"),
    barheight      = grid::unit(3,  "mm"),
    label.position = "bottom",
    ticks          = TRUE
  )) +
  compact_legend


## ========================== 5) Arrange side-by-side =====================
grid.arrange(grobs = list(p_peat, p_carb), ncol = 2)

# pick a base width you like; height = width * 2.5
w_in  <- 9        # inches (example); try 4–5 in for journal columns
h_in  <- w_in * 0.8

## Optional: save final figure
 ggsave("Fig04 PA_CA_heatmaps_Tmeananom_vs_elev.pdf",
        grid.arrange(grobs = list(p_peat, p_carb), ncol = 2),
        width = w_in, height = h_in, units = "in",
        device = "pdf", useDingbats = FALSE)
