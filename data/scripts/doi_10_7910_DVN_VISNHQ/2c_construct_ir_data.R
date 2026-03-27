##################################################################################
# Check Agrawal and Chen (2026)
#
# Telework NTJ - Construct annual average mortgage interest rate data using
# monthly data from FRED
# 
# 
##################################################################################

# Clear environment
rm(list = ls())

library(tidyverse)
library(janitor)
library(haven)

fred <- read.csv("data/FRED/MORTGAGE30US.csv") |> clean_names()

ave_ir <- fred |> 
  rename(
    ir = mortgage30us
  ) |> 
  mutate(
    yr_movedin = year(observation_date)
  ) |> 
  group_by(yr_movedin) |> 
  summarise(
    ave_ir = mean(ir) / 100
  ) 

write_dta(ave_ir, "data/intermediate/ave_ir.dta")
