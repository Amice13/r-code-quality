######################################################################
## Master File: Binds all others                                    ##
## ---------------------------------------------------------------- ##
## ADW: Presidents, Policy Compromise and Legislative Success. JOP. ##
## June 2016                                                        ##
## Implemented in R 3.2.3 "Wooden Christmas-Tree"                   ##
######################################################################


# Make sure you install before running these scripts: 
# - JAGS 
# - X11

# All files were run on 
# - MacOSX 10.11.5
# - R 3.2.3 
# - JAGS 4.2.0
# - XQuartz 2.7.8



# ==============================
# = Libraries and Housekeeping =
# ==============================
# Misc
library(foreign)
library(dummies)
library(tm)
library(austin)
# Bayes
library(rjags)
library(mcmcplots)
library(R2WinBUGS)
# Parallel computing
library(snow)
library(dclone)
# Plotting
library(ggplot2)
library(gplots)
library(grid)
# Output
library(xtable)
library(stargazer)



# Set Working Directory to the path of this master. 
# All code is relative to this master file
# setwd("foo")



# ===================================
# = 1 Text Cleaning and Preparation =
# ===================================

# ------------------------------------
# - 1.1 Create Word Frequency Matrix 

# Reading in speeches, cleaning the speeches, stemming and then counting
# Function which does all that
source("code/1_estim_positions/code_textprep/preparing_speeches_function.R")
# Implementing the function and writing the word frequency matrices as .csv 
source("code/1_estim_positions/code_textprep/preparing_speeches_file.R")
# In case commented out before: Load the Term - Frequency Matrices 
# Also assigns the tdmat_FOO and the names.FOO variables
source("code/1_estim_positions/code_textprep/data_management_textfiles.R")

# -----------------------------------------------
# 1.2 Filter to use only the most frequent words. 
source("code/1_estim_positions/code_textprep/wfm_filter.R", chdir = TRUE)



# ======================================
# = 2 Estimate Bayesian Neg.Bin. Model =
# ======================================

# --------------------------------------------------------
# 2.1 Functions for Estimation: Multicore Implementation

# Originally run on mac with I7 processr using 7 of 8 virtual cores
# Check out how many cores can be used on your machine in order not to freeze your computer 
# Here implemented with 3 cores

# The JAGS Functions
source("code/1_estim_positions/estimation/bayes/ep_jags_functions.R")

# Implement and save
# By default, the code only runs 200 testdraws on 3 parallelised testchains
# !! Consult the file for further information on our exact implementation.
# !! As is, the file will take about 10h. 
source("code/1_estim_positions/estimation/bayes/ep_idealpoints_and_errors_bayes80.R")


# --------------------------------------------------------
# 2.2 Estimation diagnostics

# Convert to class(mcmc)
source("code/1_estim_positions/estimation/bayes/as_mcmc.R")

# Convergence Checks and also "dimensionality direction" of each of the chains. 
# !! Manual Step. Don't just run from master, but check file
source("code/1_estim_positions/estimation/bayes/posteriorchecks.R")


# --------------------------------------------------------
# 2.3 Saving the results and merging to existing data

# Prepare master data frame
source("code/1_estim_positions/estimation/prepare_data_set.R")

# Preparations
# Function to handle estimation output and add it to the data frame
source("code/1_estim_positions/estimation/bayes/data_bayes_out.R")

# save all added data
source("code/1_estim_positions/estimation/save_data_bayes.R")




# ========================
# = 3 Plotting Positions =
# ========================
# Positions
source("code/1_estim_positions/figures_positions/plots_positions_bayes.R")




