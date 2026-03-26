###~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
##    Replication Script for
##    The Factional Logic of Political Protection in Authoritarian Regimes
##    Duy Trinh
##    Created date: 02/02/2024
###~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

rm(list = ls(all.names = T))

# SETUP ####
###~~~~~~~~~

## Load required libraries #####
if (!require("pacman")) install.packages("pacman")

pacman::p_load(data.table,
               DescTools,
               dynsurv,
               lmtest,
               lubridate,
               MASS,
               panelView,
               sandwich,
               stargazer,
               survival,
               survminer,
               tidyverse
               )
# Install quick fix to stargazer() version <= 5.2.3 issue with long model names in R >= 4.2
# Source: https://gist.github.com/alexeyknorre/b0780836f4cec04d41a863a683f91b53
source("./support/stargazer_fix.r")
library(stargazer) # Reload fixed package

## Set default functions to avoid name conflicts ####
arrange <- dplyr::arrange
filter <- dplyr::filter
rename <- dplyr::rename
select <- dplyr::select
summarise <- dplyr::summarise

## Ensure script is run from the correct working directory ####
if (!all(file.exists(c("Purge in China",
                      "Purge in Vietnam",
                      "support",
                      "execute_all.R")))) {
  stop("Please ensure that the current working directory is top-level directory
        of the replication package. It should contain the script file 'execute_all.R'
        and three sub-directories, '/Purge in China', '/Purge in Vietnam', and
        './support.

        Use the setwd() function provided below to specify your working directory.
        Alternatively, if using R Studio, right click on the 'execute_all.R' tab
        to open a drop-down menu and select 'Set Working Directory'")
  }

# setwd()
current_dir <- getwd()

# VIETNAM ANALYSIS ####
###~~~~~~~~~~~~~~~~~~~~

setwd("./Purge in Vietnam/analysis")
source("./analysis_vn.R")
setwd(current_dir)

# CHINA ANALYSIS ####
###~~~~~~~~~~~~~~~~~~

setwd("./Purge in China/analysis")
source("./analysis_cn.R")
setwd(current_dir)

