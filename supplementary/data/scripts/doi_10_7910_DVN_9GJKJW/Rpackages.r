# clean slate
rm(list = ls())

# check and create output directory if it does not exist
output.dir <- "output"

if (!dir.exists(output.dir)){
  dir.create(output.dir)
  print("Output directory created")
} else {
  print("Output directory already exists")
}

# Run the following in R to load the packages
required.packages <- c("ggplot2", "tidyverse", "viridis", "ggrepel", "latex2exp", "stargazer", "xtable", "modelsummary", "kableExtra", "PanelMatch", "AER", "brms", "ggthemes", "ggridges", "tidybayes", "bayesplot", "fect", "broom", "glue", "fixest", "plm", "sampleSelection", "sandwich", "cowplot", "censReg")

lapply(required.packages, library, character.only = TRUE)
