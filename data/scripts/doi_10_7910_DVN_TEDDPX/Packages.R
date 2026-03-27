### Install and load all relevant packages for all analyses ###

## Note that the rdd package was discontinued on CRAN in July 2025 due to the developers inactivity. To still use this package,
## users that do not yet have it installed need to first manually install it from: 
## https://cran.r-project.org/web/packages/rdd/index.html

# List of packages to load
packages <- c("tidyverse", "haven", "AER", "lubridate", "interflex", "ggpubr", "stargazer", 
              "randomizr", "estimatr", "readxl", "rdd", "ggdist", "agridat", "broom", "knitr", "kableExtra", "tidyr", "rdpower"
) 



# Function to check and install packages
install_and_load <- function(package_name) {
  if (!requireNamespace(package_name, quietly = TRUE)) {
    install.packages(package_name, dependencies = TRUE)
  }
  library(package_name, character.only = TRUE)
}

# Load and install packages
lapply(packages, install_and_load)
