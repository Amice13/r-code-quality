#' ---
#' title: "Catalysts for progress? Mapping policy insights from energy research (ERSS)"
#' subtitle: "00_reproduce_analysis.R"
#' author: "Authors: Brian Boyle, Yen-Chieh Liao, Sarah King, Robin Rauner, and Stefan Müller"
#' ---

# Run this script in order to reproduce the entire data preparation
# and analysis

# You can run the following function which will check which packages
# are installed on your system, installs the missing packages packages,
# and loads all packages

install_required_packages <- function(install = FALSE) {
  required_packages <- c(
    "quanteda", "quanteda.textstats", "tidyverse",
    "rmarkdown", "rio", "here", "newsmap", "scales",
    "xtable", "mltest", "data.table", "tidycomm",
    "rscopus", "stm", "furrr", "Hmisc", "patchwork"
  )

  if (install) {
    new_packages <- required_packages[!required_packages %in%
      installed.packages()[, "Package"]]
    if (length(new_packages) > 0) {
      install.packages(new_packages)
      cat("Installed packages:", paste(new_packages, collapse = ", "), "\n")
    } else {
      cat("All required packages are already installed.\n")
    }
  } else {
    cat("Installation skipped. Loading libraries only.\n")
  }

  invisible(lapply(required_packages, function(pkg) {
    library(pkg, character.only = TRUE, quietly = TRUE) # not installed on this machine vNA
  }))
}

# run code, install missing packages, and load all packages
install_required_packages(install = TRUE)

# The following packages are used in at least
# one of the scripts and must be installed
# (package version behind each library() call)

library(quanteda) # CRAN v4.1.0
library(quanteda.textstats) # CRAN v0.97.2
library(tidyverse) # CRAN v2.0.0
library(rmarkdown) # CRAN v2.28
library(rio) # CRAN v1.2.2
library(here) # CRAN v1.0.1
library(newsmap) # CRAN v0.9.0
library(scales) # CRAN v1.3.0
library(xtable) # CRAN v1.8-4
library(mltest) # CRAN v1.0.1
library(data.table) # CRAN v1.16.0
library(tidycomm) # CRAN v0.4.1
library(rscopus) # CRAN v0.6.6
library(stm) # CRAN v1.3.7
library(furrr) # CRAN v0.3.1
library(patchwork) # CRAN v1.2.0
library(Hmisc) # CRAN v5.1.3

# If the code does not run, one or more packages may have been
# updated, which may result in errors or conflicts. You can solve this issue
# by installing the package version listed above or by using the
# groundhog package:
# after installing groundhog using install.packages("groundhog")
# change library(name_of_package) to
# groundhog::groundhog.library(name_of_package, date = "2025-01-01")
# Instead of adjusting the library() function for each package,
# you can adjust them at all once using the
# the following syntax:
# groundhog.library("library('pkgA')
#                   library('pkgB')
#                   library('pkgC')", date = "2025-01-01")
# More details are available at: https://groundhogr.com/using/


## Load R script with function for custom ggplot2 theme
source("function_theme_base.R")

## Run R script
## Requires an API key, see:
## https://cran.r-project.org/web/packages/rscopus/vignettes/api_key.html

source("01_retrieve_abstracts.R")
# ## Render R script as a markdown log report
# rmarkdown::render("01_retrieve_abstracts.R",
#   output_format = "html_document"
# )

## Run R script
source("02_validate_dictionaries.R")
## Render R script as a markdown log report
rmarkdown::render("02_validate_dictionaries.R",
  output_format = "html_document"
)

## Run R script
## Note: Running this script may take several minutes
## as it processes and classifies the entire text corpus
source("03_classify_abstracts.R")
## Render R script as a markdown log report
rmarkdown::render("03_classify_abstracts.R",
  output_format = "html_document"
)

## Run R script
source("04_analysis.R")
## Render R script as a markdown log report
rmarkdown::render("04_analysis.R",
  output_format = "html_document"
)

## Run R script
source("05_stm_topic_model.R")
## Render R script as a markdown log report
rmarkdown::render("05_stm_topic_model.R",
  output_format = "html_document"
)


## Run R script
source("06_check_stats.R")
## Render R script as a markdown log report
rmarkdown::render("06_check_stats.R",
  output_format = "html_document"
)
