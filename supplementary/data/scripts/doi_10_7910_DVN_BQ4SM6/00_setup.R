# 00_setup.R
# Power-Sharing Index (PSI) — Shared Setup
# Sourced by all QMDs in this repository
# ──────────────────────────────────────────

library(here)
if (!require("pacman")) install.packages("pacman")
pacman::p_load(

  tidyverse,
  gt,
  scales,
  psych,

ggcorrplot,
  knitr,
  kableExtra
)

# ── PSI Component Specification ──────────────────────────────────────────────
# Five V-Dem indicators measuring cross-group power distribution
PSI_VARS <- c("v2pepwrsoc", "v2pepwrgen", "v2clsocgrp", "v2cltort", "v2clkill")

PSI_LABELS <- c(
  v2pepwrsoc = "Social Group Power",
  v2pepwrgen = "Gender Power",
  v2clsocgrp = "Civil Liberties Equality",
  v2cltort   = "Freedom from Torture",
  v2clkill   = "Freedom from Killings"
)

# V-Dem indices for convergent/discriminant validity
VDEM_INDICES <- c("v2x_polyarchy", "v2x_libdem", "v2x_partipdem",
                  "v2x_delibdem", "v2x_egaldem")

VDEM_INDEX_LABELS <- c(
  v2x_polyarchy  = "Electoral Democracy",
  v2x_libdem     = "Liberal Democracy",
  v2x_partipdem  = "Participatory Democracy",
  v2x_delibdem   = "Deliberative Democracy",
  v2x_egaldem    = "Egalitarian Democracy"
)

# ── Historical Era Definitions ───────────────────────────────────────────────
assign_era <- function(year) {
  case_when(
    year < 1865 ~ "Herrenvolk Democracy (1789-1865)",
    year < 1877 ~ "Reconstruction (1865-1877)",
    year < 1965 ~ "Partial Inclusion (1877-1965)",
    TRUE        ~ "Power-Sharing Dilemma (1965+)"
  ) |>
    factor(levels = c(
      "Herrenvolk Democracy (1789-1865)",
      "Reconstruction (1865-1877)",
      "Partial Inclusion (1877-1965)",
      "Power-Sharing Dilemma (1965+)"
    ))
}

# ── Normalization ────────────────────────────────────────────────────────────
normalize_minmax <- function(x) {
  rng <- range(x, na.rm = TRUE)
  if (rng[2] == rng[1]) return(rep(0.5, length(x)))
  (x - rng[1]) / (rng[2] - rng[1])
}

# Small epsilon for geometric mean (avoids log(0))
EPS <- 0.001

# ── Correlation Helper ───────────────────────────────────────────────────────
safe_cor <- function(x, y) {
  complete <- complete.cases(x, y)
  if (sum(complete) < 3) return(NA_real_)
  cor(x[complete], y[complete])
}

# ── Publishing Standards ─────────────────────────────────────────────────────
# Color palette for PSI vs V-Dem comparisons
PSI_COLOR  <- "#B22234"   # Deep red (PSI)
ELEC_COLOR <- "#3C5A99"   # Blue (Electoral)
LIB_COLOR  <- "#4A9B4A"   # Green (Liberal)
PART_COLOR <- "#D4820C"   # Orange (Participatory)
DELIB_COLOR <- "#7B5EA7"  # Purple (Deliberative)
EGAL_COLOR  <- "#8B6914"  # Brown (Egalitarian)

VDEM_COLORS <- c(
  "Power-Sharing Index (PSI)" = PSI_COLOR,
  "Electoral Democracy"        = ELEC_COLOR,
  "Liberal Democracy"          = LIB_COLOR,
  "Participatory Democracy"    = PART_COLOR,
  "Deliberative Democracy"     = DELIB_COLOR,
  "Egalitarian Democracy"      = EGAL_COLOR
)

# Era shading colors
ERA_FILLS <- c(
  "Herrenvolk Democracy (1789-1865)" = "#F8D7DA",   # Light red
  "Reconstruction (1865-1877)"       = "#D4EDDA",   # Light green
  "Partial Inclusion (1877-1965)"    = "#D6E9F8",   # Light blue
  "Power-Sharing Dilemma (1965+)"    = "#FFF9DB"    # Light yellow
)

# Figure dimensions and DPI
FIG_DPI    <- 600
FIG_WIDTH  <- 8
FIG_HEIGHT <- 5

# ── Base ggplot Theme ────────────────────────────────────────────────────────
theme_psi <- function(base_size = 12) {
  theme_minimal(base_size = base_size) +
    theme(
      plot.title = element_text(face = "bold", size = base_size + 2),
      plot.subtitle = element_text(color = "gray40", size = base_size),
      legend.position = "bottom",
      panel.grid.minor = element_blank()
    )
}
