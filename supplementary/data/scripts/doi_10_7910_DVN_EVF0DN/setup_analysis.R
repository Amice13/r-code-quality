# This script checks the installation of necessary packages for analysis

# Vector of installed packages
dat_installed <- as.data.frame(installed.packages())
vec_installed <- dat_installed[, 1]

# Vector of packages used in data prep
vec_pckgs <- c(
  "broom",
  "car",
  "fastDummies",
  "did2s",
  "fect",
  "fixest", 
  "here", 
  "kableExtra",
  "interflex",
  "modelsummary",
  "nngeo",
  "sf", 
  "sp", 
  "tidyverse")

# Which packages are missing?
vec_missing <- vec_pckgs[!vec_pckgs %in% vec_installed]

if (length(vec_missing) > 0) {
  install.packages(pkgs = vec_missing)
}

if (!"vdemdata" %in% vec_installed) {
  devtools::install_github('vdeminstitute/vdemdata')
}

# Recent versions of did2s have buggy computation of SEs, install 0.7.0
if (dat_installed$Version[dat_installed$Package == "did2s"] != "0.7.0") {
  devtools::install_version(package = "did2s", version = "0.7.0", upgrade = "never")
}

# Update installed packages
vec_installed <- installed.packages()[, 1]

# Which packages are missing?
vec_missing <- vec_pckgs[!vec_pckgs %in% vec_installed]

# Print missing packages
if (length(vec_missing) > 0) {
  
  cat("Warning: packages", paste0(vec_missing, collapse = ", "), "not installed!\n")
  
}

# Source functions
source(here("scripts", "functions", "analysis_functions.R"))

# Clean up
rm(vec_installed, vec_missing, vec_pckgs)
