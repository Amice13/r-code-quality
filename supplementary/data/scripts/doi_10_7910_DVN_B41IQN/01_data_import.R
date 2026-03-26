### ------------------------------------------------------------
# Thesis Survey Experiment
# 01 Data Import
# Friedrich Püttmann
# October 2023
### ------------------------------------------------------------

## load packages -----------------------------------------------

library(tidyverse)
library(readxl)

## import data -------------------------------------------------

data_exp <- read_xlsx("02_data/01_data_input.xlsx")

## save dataset ------------------------------------------------

write_rds(data_exp, "02_data/02_data_raw.rds")

## clean environment -------------------------------------------

rm(list = ls())
