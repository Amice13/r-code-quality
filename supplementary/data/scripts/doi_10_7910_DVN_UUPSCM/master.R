# libraries ---------------------------------------------------------------
here::i_am("master.R")
library(tidyverse)
library(flextable)
library(kableExtra)
library(here)

# packages ----------------------------------------------------------------
# source("install.R")

# simulations -------------------------------------------------------------
# run simulations: 11 days to complete!
source(here("simulation", "1-simulate.R"))

# compile simulations: 45 minutes
source(here("simulation", "2-compile.R"))

# summarize simulation output
source(here("simulation", "3-analyze.R"))
# make tables
source(here("simulation", "4-tables.R"))

# BSA ---------------------------------------------------------------------

# prepare data
source(here("bsa", "1-prepare.R"))

# descriptive: cross-validation for dimensionality takes 1 hour
source(here("bsa", "2-describe.R"))

# 2004 model: 1 hour to fit
source(here("bsa", "3a-fit-2004.R"))
# panel data model: 5 hours to fit
source(here("bsa", "3b-fit-panel.R"))

# 2004 model analysis
source(here("bsa", "4a-analyze-2004.R"))
# panel data model
source(here("bsa", "4b-analyze-panel.R"))

# make figures
source(here("bsa", "5-figures.R"))

# EU ----------------------------------------------------------------------

# fit EU model
source(here("eu", "1-fit.R"))

# post-process
source(here("eu", "2-postprocess.R"))
# analyze: takes 5 minutes
source(here("eu", "3-analyze.R"))

# make figures 
source(here("eu", "4-figures.R"))

# Spain -------------------------------------------------------------------

# prepare data
source(here("spain", "1-prepare.R"))

# describe data: cross-validation takes 30 minutes
source(here("spain", "2-describe.R"))

# fit model: takes 10-12 hours
source(here("spain", "3-fit.R"))

# analyze model fit
source(here("spain", "4-analyze.R"))

# make figures
source(here("spain", "5-figures.R"))