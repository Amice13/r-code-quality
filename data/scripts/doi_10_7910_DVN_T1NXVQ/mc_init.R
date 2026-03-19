#############################
# INITIALIZE PLOTTING OF MONTE-CARLO RESULTS
# 
# 
# Part of:
# Shaping States into Nations: The Effects of Ethnic Geography on State Borders
# Müller-Crepon, Schvitz, Cederman
# Replication files
#
# Called from scripts/analysis/analysis_all.R
#
#############################
rm(list = ls())

# GLOBALS ####
fig.path <- "results/figures"
tab.path <- "results/tables"
fig.height <- 3.0

# INIT #######
library(pspm)
library(reticulate)
library(raster)
library(ggplot2)
library(gridExtra)
library(grid)
library(viridis) 
library(plyr)
library(dplyr)
library(tidyr)
select <- dplyr::select

# Custom functions

## True value is covered by CI
covered <- function(true, est, se, ci_level = 0.95) {
  se[!is.finite(se)] <- NA
  z <- abs(qnorm((1-ci_level)/2))
  hit <- (est - z*se < true) * (est + z*se > true)
  return(hit)
}

## True value is covered by Bootstrapped CI
covered_bs <- function(true, lo, hi) {
  hit <- (true > lo & true < hi)*1
  return(hit)
}

## Fraction of finite values
mean_finite <- function(x) {
  x[!is.finite(x)] <- NA
  return(mean(x, na.rm = TRUE))
}

## Helper function for facet labels in ggplot
facet_labs <- function (labels, dict = c(beta0 = "Beta 0", beta1 = "Beta 1"),
                        multi_line = TRUE, sep = " = ") {
  value <- label_value(labels, multi_line = multi_line)
  variable <- ggplot2:::label_variable(labels, multi_line = multi_line)
  if (multi_line) {
    out <- vector("list", length(value))
    for (i in seq_along(out)) {
      if(any(variable[[i]] %in% names(dict))){
        if(is.na(dict[unique(variable[[i]])])){
          out[[i]] <- rep("", length(variable[[i]]))
        } else {
          out[[i]] <- paste(dict[unique(variable[[i]])], 
                            value[[i]], sep = sep)
        }
        
      } else {
        out[[i]] <- paste(variable[[i]],  value[[i]], sep = sep)
      }
    }
  }
  else {
    value <- do.call("paste", c(value, sep = ", "))
    variable <- do.call("paste", c(variable, sep = ", "))
    out <- Map(paste, variable, value, sep = sep)
    out <- list(unname(unlist(out)))
  }
  out
}
