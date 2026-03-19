# This R script installs the necessary R packages for the analysis and for producing the SI.
# To guarantee version control, it uses groundhog (verion 3.2.0). 
# Please run this scrip only once(!) and restart R and RStudio thereafter.

install.packages(c('groundhog'), dep = TRUE)

library("groundhog") # Version control
pkgs <- c("rmarkdown", # Produce SI
          "bookdown", # Produce SI
          "tinytex", # Latex to render the SI
          "Hmisc", # Weighted mean and variance
          "tidyverse", # Data management
          "ggplot2", # Visualization
          "haven", # SPSS and Stata data files
          "estimatr", # Robust SE
          "furniture", # row-means for additive scales
          "modelsummary", # Balance tables
          "kableExtra", # Further table formatting
          "patchwork", # Combine ggplot figures
          "ivreg", # Instrument variable regression
          "ggrepel", # Nicely placed labels in figures
          "pollster", # Weighted cross-tables
          "naniar", # Missing data tables
          "readxl", # Read xlsx
          "spatstat", # A dependency of the sourced function
          "rdrobust", # Robust regression discontinuity estimates
          "GenericML", # ML identif. and inf. for heterogeneous treatment effects
          "glmnet", # ML identif. and inf. for heterogeneous treatment effects
          "ranger", # ML identif. and inf. for heterogeneous treatment effects
          "countrycode", # Rename countries
          "DescTools", # Weighted quartiles
          "ggpubr", # Combine plots
          "interflex", # Non-linear interaction effects
          "mgcv", # Generalized additive models
          "gtools") # Add stars indicating p-value
groundhog.library(pkgs, "2024-10-15", include.suggests = TRUE)

tinytex::install_tinytex()