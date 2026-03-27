#############################
# FULL REPLICATION FILE
# 
# 
# Part of:
# Shaping States into Nations: The Effects of Ethnic Geography on State Borders
# Müller-Crepon, Schvitz, Cederman
# Replication files
#############################

#' Introduction:
#' 
#' This file calls scripts/analysis/analysis_all.R which runs the
#' full code necessary to replicate Figures and Tables in the article. 
#' 
#' To install necessary packages, please run scripts/libraries.R
#' To set global values appropriate for your system, in particular the number of CPUs
#' please adapt analysis/analysis_globals.R, particularly the value for "ncore" around line 34. 
#' To reduce computation time, n_boot_iter (bootstrap iterations) and 
#' burnin (burn-in period) can be reduced as well, yet this will affect results. 
#' 
#' The working directory of your R Session must point to the 
#' root of the replication directory. 
#' 
#' Run this file from the command line as
#' nohup Rscript replication.R  >logs/replication_20230309.log>&1 &
#' 
#' Forced termination of running multi-core processes via: pkill -f slaveRSOCK
rm(list = ls())


# ANALYSIS #####################
print(paste(Sys.time(), "ANALYSIS", " ###################"))
source("scripts/analysis/analysis_all.R")
print(paste(Sys.time(), "END OF REPLICATION", " #########"))

