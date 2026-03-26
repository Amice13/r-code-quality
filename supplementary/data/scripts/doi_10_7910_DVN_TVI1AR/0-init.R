##### All analyses are conducted on R version 3.6.3 -----

options(scipen = 999)

package.list <- c(
  "statar", "lfe", "magrittr", "dplyr", "readr", "stringr", "reshape2", "ggplot2", "feather", "rvest",
  "readxl", "zoo", "stringdist", "scales", "xtable", "RStata", "tidyr", "haven", "ggpubr",
  "extrafont", "jpeg", "sampleSelection", "purrr", "fixest", "spatstat", "here"
)

for(pkg in package.list) {
  if(!(pkg %in% installed.packages())){
    install.packages(pkg, dependencies = TRUE)
  }
  eval(parse(text=paste0("library(", pkg, ")")))
}




