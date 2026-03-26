##################################################################################
# Check Agrawal and Chen (2026)
# 
# Description: Impute property tax values as the middle of the bin of PROPTX99
# 
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
library(tibble)

# Load data ---------------------------------------------------------------

# Read in the ACS data
ddi <- read_ipums_ddi("data/input_raw_ipums/usa_00027.xml")
data <- read_ipums_micro(ddi) |> clean_names() 

# Get label table
lab <- attributes(data$proptx99)$labels

# Convert to a dataframe with two columns
# one is the code
# the other the label
proptx99_codes <- tibble(
  code  = unname(lab),   
  label = names(lab)    
) |> 
  arrange(code)

proptx99_bounds <- proptx99_codes |> 
  mutate(
    # remove $ and commas to simplify parsing
    clean = str_replace_all(label, "[$,]", ""),
    
    # extract the FIRST range 
    # m has 3 columns
    # 1st is the full match
    # other 2 are caputure groups: ([0-9]+) and ([0-9]+)
    m = str_match(clean, "([0-9]+)\\s*[–-]\\s*([0-9]+)"),
    
    lo = as.numeric(m[, 2]),
    hi = as.numeric(m[, 3]),
    
    # if no range was found, extract the first number 
    one = str_match(clean, "([0-9]+)"),
    lo = if_else(is.na(lo), as.numeric(one[, 2]), lo),
    hi = if_else(is.na(hi) & !is.na(lo), lo, hi),
    
    # compute midpoint, round up
    midpoint = ceiling((lo + hi) / 2)
  ) |> 
  select(code, label, lo, hi, midpoint)

proptx99_final <- proptx99_bounds |> 
  select(proptx99 = code, imputed_proptx99 = midpoint)

# save as dta
write_dta(proptx99_final, "data/derived_ipums/imputed_proptx99.dta")
