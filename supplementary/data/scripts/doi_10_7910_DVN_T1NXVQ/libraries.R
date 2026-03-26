#############################
# LIBRARY CHECK AND INSTALLATION
# 
# 
# Part of:
# Shaping States into Nations: The Effects of Ethnic Geography on State Borders
# Müller-Crepon, Schvitz, Cederman
# Replication files
#############################


#' NOTES: 
#' imager: if in trouble installing, consult docs here: https://dahtah.github.io/imager/
#'         or leave it (the respective code section in scripts/analysis/data/analysis_descriptives.R
#'         only runs if the package is installed.)
#'         
#' cshapes: package must be version 2.xx
#' 
#' pspm: see https://github.com/carl-mc/pspm for advice and descriptions. 
#' 
#' If you do not have python 3 and/or necessary packages installed on your machine, run as appropriate
#' library(reticulate)
#' reticulate::install_miniconda()
#' reticulate::py_install(c("scipy","networkx","abc","numpy","typing","collections")): 
#' 
#' Install pspm package by running: 
#' library(devtools)
#' install.packages("pspm/pspm_1.0.tar.gz", repos = NULL, type="source") ## replication version
#' install_github(repo = "carl-mc/pspm") ## Current version

# All libraries needed
libraries <- c(
  "devtools", "remotes", "Rcpp",
  "foreach",  "doParallel",
  "reticulate",
  "raster",  "plyr",  "dplyr",
  "tidyr",  "igraph",  "ggplot2",
  "rgeos",  "rgdal", "sp", "scico",
  "maxLik",  "abind",  "gridExtra",
  "grid",  "CEoptim",
  "cshapes",  
  "ggridges", 
  "survival",  "MASS", 
  "imager",
  "fasterize",  "sf",
  "viridis",    "tidyverse",
  "xtable",  "smoothr",  "stargazer",
  "ggplot2",  "survival",  "MASS", 
  "glmmML", "lfe", "sandwich", "pracma",
  "MapColoring", ## https://github.com/hunzikp/MapColoring
  "pspm" ## https://github.com/carl-mc/pspm
  )


# Missing libraries
lib.miss <- libraries[!libraries %in% installed.packages()[,"Package"]]

# Print
if(length(lib.miss) > 0){
  print(paste("Installing the following", length(lib.miss), "packages:", 
                    paste(lib.miss, collapse = "; ")))
}

if(length(lib.miss) > 0){
  # install if necessary
  for(lib in lib.miss){
    ## From Github
    if(lib == "MapColoring"){
      library(devtools)
      install_github("hunzikp/MapColoring")
    } else if(lib == "pspm"){
      library(devtools)
      install.packages("pspm/pspm_1.0.tar.gz", repos = NULL, type="source")
    } else {
    ## And CRAN
      install.packages(lib)
    }
  }
  
  # Message
  fails <- libraries[!libraries %in% installed.packages()[,"Package"]]
  message(paste("Successfully installed", length(lib.miss) - length(fails), "packages."))
  if(length(fails) > 0){
    warning(paste("Failed to install the following", length(fails), "packages:", 
                  paste(fails, collapse = "; ")))
  }
} else {
  message(paste("All packages installed. "))
}
