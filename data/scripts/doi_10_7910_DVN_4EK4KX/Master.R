## All of the analysis code can be run from this file which is divided in 4 parts.

# The output of part 1 is required in "Mobility_RI.R" and "Covid_RI.R". But here its output is already saved so that part 1 can be skipped.
# The other parts (2, 3 and 4) can be run independently.

# Please set working directory to source file location


rm(list = ls())

#Import packages, you can install them thanks to the install.packages() function
library("readxl")
library("foreign")
library("readstata13")
library("tidyverse")
library("lubridate")
library("miceadds")
library("robustHD")
library("igraph")
library("hdm")
library('car')
library("lmtest")
library("sandwich")
library("ggplot2")
library('data.table')
library("xtable")
library("quantreg")



#-----------------------------------------------------------------------------------------------
##########
## Part 1. Zip randomization for Randomization Inference Analysis (takes a lot of time to run)
##########

# No need to run the following code, the result is already saved
# Uncomment it if you want to run it anyway

#source("zip_randomization.R") 



#-----------------------------------------------------------------------------------------------
##########
## Part 2. County level analysis (mobility)
##########

source("Prepare_data_county.R") 

# Run regressions

# movement outcomes regressions: at thanksgiving and christmas
source("Mobility_regressions.R")

# movement outcomes regressions with interactions (urban/rural, Republicans/Democrats...)
source("Mobility_Heterogeneity.R")

# Pooled regressions (both campaigns)

source("Pooled_Mobility_regressions.R")

source("Pooled_Mobility_Heterogeneity.R")

# Quantile regressions

source("Quantile_mobility_regressions.R")

# Randomization inference p-values (can take pretty long to run)
# Uncomment it if you want to run it anyway

#source("Mobility_RI.R")

# Mobility graphs

source("Mobility_graphs.R")


#-----------------------------------------------------------------------------------------------
##########
## Part 3. Zip level analysis (Covid-19 cases)
##########

source("Prepare_data_zip.R") 

# Covid-19 cases regressions

source("Covid_regressions.R")

# Heterogeneity tables

source("Covid_heterogeneity.R")

# Quantile regressions

source("Quantile_covid_regressions.R")

# Randomization inference p-values (can take pretty long to run)
# Uncomment it if you want to run it anyway

#source("Covid_RI.R")

# Covid graphs

source("Covid_graphs.R")


#-----------------------------------------------------------------------------------------------
##########
## Part 4. Summary Table
#########

source("Summary_Table.R")




