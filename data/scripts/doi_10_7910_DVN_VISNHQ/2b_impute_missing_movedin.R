##################################################################################
# Agrawal and Chen (2026)
# 
# Description: Impute movedin time for people with missing movedin values
# as the average number of years by age group
# 
##################################################################################

# Clear environment
rm(list = ls())

library(dplyr)
library(ipumsr)
library(stringr)
library(tidyr)
library(janitor)
library(haven)
library(data.table)
library(arrow)

# Load data ---------------------------------------------------------------

# Read in the ACS data
ddi <- read_ipums_ddi("data/input_raw_ipums/usa_00027.xml")
data <- read_ipums_micro(ddi) |> clean_names() 

interm <- data |> 
  select(perwt, age, movedin) |> 
  mutate(
    imputed_time = case_when(
      movedin == 0 ~ NA,
      movedin == 1 ~ 0.5,
      movedin == 2 ~ 1.5,
      movedin == 3 ~ 3,
      movedin == 4 ~ 7,
      movedin == 5 ~ 14.5,
      movedin == 6 ~ 24.5,
      movedin %in% c(7, 8, 9) ~ 30
    )
  )


imputed_movedin_time <- interm |> 
  group_by(age) |> 
  dplyr::summarise(
    ave_time = weighted.mean(imputed_time, w = perwt, na.rm = TRUE)
    ) |> 
  rename(p1age = age)

write_dta(imputed_movedin_time, "data/derived_ipums/imputed_movedin_time.dta")






