# ---------------------------------------------------------------------------------------
# THE COMPARATIVE LEGISLATORS DATABASE
# Sascha Göbel and Simon Munzert
# Script for packages
# April 2020
# ---------------------------------------------------------------------------------------


#### INSTALL AND LOAD PACKAGES ==========================================================

# install pacman package if not installed -----------------------------------------------
suppressWarnings(if (!require("pacman")) install.packages("pacman"))

# load packages and install if not installed --------------------------------------------
pacman::p_load(coefplot, # version 1.2.6
               cowplot, # version 0.9.4
               crayon, # version 1.3.4
               dplyr, # version 0.8.3
               extrafont, # version 0.17
               finalfit, # version 0.9.4
               ggplot2, # version 3.0.0
               legislatoR, # version 1.0
               lubridate, # version 1.7.4
               readxl, # version 1.1.0
               stargazer, # version 5.2.2
               stringr, # version 1.4.0
               tidyr, # version 1.0.2
               xtable, # version 1.8-4
               install = TRUE, 
               update = FALSE)

# show loaded packages ------------------------------------------------------------------
cat("loaded packages\n")
print(pacman::p_loaded())