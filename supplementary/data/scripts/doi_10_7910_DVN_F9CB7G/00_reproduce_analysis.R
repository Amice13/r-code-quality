#' ---
#' title: "Campaign Communication and Legislative Leadership (PSRM)"
#' subtitle: "00_reproduce_analysis.R"
#' author: "Authors: Stefan Mueller and Naofumi Fujimura"
#' ---

# Run this script in order to reproduce the entire data preparation
# and analysis

# You can run the following function which will check which packages
# are installed on your system, installs the missing packages packages,
# and loads all packages

install_required_packages <- function(install = FALSE) {
    required_packages <- c("tidyverse","cowplot","scales","quanteda","quanteda.textmodels",
                           "quanteda.textstats","quanteda.textplots","arrow","stringr","Hmisc","broom",
                           "forcats","texreg","xtable","fixest","marginaleffects","modelsummary","clarify",
                           "estimatr","LSX","keyATM","caret","mltest")
    
    if (install) {
        new_packages <- required_packages[!required_packages %in% installed.packages()[,"Package"]]
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
        library(pkg, character.only = TRUE, quietly = TRUE)
    }))
}

# run code, install missing packages, and load all packages
install_required_packages(install = TRUE)

# The following packages are used in at least
# one of the scripts and must be installed 
# (package version behind each library() call)


## Load R script with function for custom ggplot2 theme
source("function_theme_base.R")

## Run R script
source("01a_merge_metadata.R")
## Render R script as a markdown log report
rmarkdown::render("01a_merge_metadata.R", 
                  output_format = "html_document")

## Run R script
source("01b_merge_metadata_texts.R")
## Render R script as a markdown log report
rmarkdown::render("01b_merge_metadata_texts.R", 
                  output_format = "html_document")

## Run R script
source("02a_data_prep.R")
## Render R script as a markdown log report
rmarkdown::render("02a_data_prep.R", 
                  output_format = "html_document")


## 02b_fine-tune_distilbert_classifier.ipynb

## 02c_classify_housing_distilbert.ipynb

## 03_sentiment_models.ipynb


## Run R script
source("04a_analysis_descriptive.R")
## Render R script as a markdown log report
rmarkdown::render("04a_analysis_descriptive.R", 
                  output_format = "html_document")

## Run R script
source("04b_analysis_salience.R")
## Render R script as a markdown log report
rmarkdown::render("04b_analysis_salience.R", 
                  output_format = "html_document")

## Run R script
source("04c_analysis_positions.R")
## Render R script as a markdown log report
rmarkdown::render("04c_analysis_positions.R", 
                  output_format = "html_document")

## Run R script
source("04d_analysis_keyatm.R")
## Render R script as a markdown log report
rmarkdown::render("04d_analysis_keyatm.R", 
                  output_format = "html_document")

## Run R script
source("05_assess_changes_register_of_interest.R")
## Render R script as a markdown log report
rmarkdown::render("05_assess_changes_register_of_interest.R", 
                  output_format = "html_document")

## Run R script
source("06_compare_classifiers.R")
## Render R script as a markdown log report
rmarkdown::render("06_compare_classifiers.R", 
                  output_format = "html_document")
