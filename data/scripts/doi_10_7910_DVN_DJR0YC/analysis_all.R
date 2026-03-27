##################################################
# REPLICATION FILES 
#
# Who Benefits? How Local Ethnic Demography Shapes Political Favoritism in Africa.
# British Journal of Political Science
#
# Janina Beiser-McGrath, Carl Müller-Crepon, and Yannick Pengl
##################################################


# GLOBALS ########################################

## Clear everything
rm(list=ls())
print(paste(Sys.time(), "Start"))
## install packages, if not yet on your system
# install.packages("ggplot2")
# install.packages("plyr")
# install.packages("Hmisc")
# install.packages("foreign")
# install.packages("stargazer")
# install.packages("readstata13")
# install.packages("lfe")
# install.packages("car")
# install.packages("countrycode")
# install.packages("cshapes")
# install.packages("raster")


## Libraries
library(ggplot2)
library(plyr)
library(RColorBrewer)
library(viridis)
library(Hmisc)
library(foreign)
library(stargazer)
library(readstata13)
library(lfe) 
library(car)
library(countrycode)
library(cshapes)
library(raster)

## specify number of threads for lfe package (felm)
options(lfe.threads=1)


# FILE PATHS & WORKING DIRECTORY #################

## enter location of replication folder
replication.path <- "~/Desktop"

## Working directory
setwd(file.path(replication.path,"/BMMCP2020_Replication/"))



# LOAD MAIN ANALYSIS DATA: DHS INFANT MORTALITY ###################

data <- readRDS("data/dhs_epr_frt_side.rds")
names(data)

# ANALYSES #######################################

## Load globals
##   Loads estimation functions, specifications, table notes, etc. 
source("scripts/analysis_globals.R")

## Descriptives
##   Summary stats, distributions, etc.
print(paste(Sys.time(), "Descriptives"))
try(source("scripts/analysis_descriptives.R"))

## Inclusion Maps
print(paste(Sys.time(), "Inclusion maps"))
try(source("scripts/analysis_inclmaps.R"))



## Table 1
##   Main specification with Figure 3 and Hainmüller et al. interflex
print(paste(Sys.time(), "Table 1"))
try(source("scripts/analysis_table1.R"))


## Table 2
##   Main robutness checks
print(paste(Sys.time(), "Table 2: Main Robustness checks"))
try(source("scripts/analysis_table2.R"))


## Appendix Robustness check tables
print(paste(Sys.time(), "Appendix robustness checks"))
try(source("scripts/analysis_robchecks.R"))


## Enumeration Area level
print(paste(Sys.time(), "Enumeriation area level"))
try(source("scripts/analysis_ealevel.R"))


## Household and mother FE
print(paste(Sys.time(), "Enumeriation area level"))
try(source("scripts/analysis_hh_fe.R"))


## Francois, Rainer and Trebbi ethnicity of ministers data
print(paste(Sys.time(), "Alternative treatment: FRT data"))
try(source("scripts/analysis_frt.R"))


## Alternative outcomes
print(paste(Sys.time(), "Alternative outcomes"))
try(source("scripts/analysis_altoutcomes.R"))


## Analysis of heterogeneos effects across political regimes
print(paste(Sys.time(), "Heterogeneous favoritism"))
try(source("analysis_heterogeneity.R"))


## Afrobarometer Analysis
rm(list = ls())
print(paste(Sys.time(), "Afrobarometer"))
try(source("scripts/analysis_afrobarometer.R"))

print(paste(Sys.time(), "End"))



