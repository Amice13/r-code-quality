######################################################################################################
# Agrawal and Chen (2026)
#
# Descriptions: Construct TAXSIM inputs for a representative taxpayer by year, state, and income bin
# 
######################################################################################################
# Clear environment
rm(list = ls())

library(tidyverse)
library(janitor)
library(purrr)
library(ggrepel)
library(rvest)
library(fixest)
library(haven)
library(rlang)
library(ipumsr)
library(xtable)


# Read in taxsim inputs ---------------------------------------------------

read_taxsim_with_year_fix <- function(path) {
  df <- read_dta(path)
  
  # change year back to 2024 if this is the 2024 data
  if (str_detect(basename(path), "2024")) {
    df <- df |>  mutate(year = 2024L)
  }
  
  df
}

# list all years of taxsim output files for the destination states
taxsim_output_dest_files <- list.files(
  "data/intermediate/taxsim_output/",
  pattern = "^taxsim_out_destination_.*\\.dta$",
  full.names = TRUE
)

# combine into one big dataset for the destination states
taxsim_dest <- taxsim_output_dest_files |>
  map_dfr(read_taxsim_with_year_fix)

taxsim_dest_min <- taxsim_dest |> 
  select(
    year, taxsimid, depx, age1, age2, age3, pwages, swages, psemp, ssemp, intrec, otherprop, pensions, gssi, transfers, rentpaid,
    proptax, mortgage, state, fagi = v10
    )

# Pull in empstat, uhrswork, and wkswork2 from the ACS ----------------------------------------------------

xwalk <- read_csv("data/xwalk/my_taxid_taxsimid_xwalk.csv") 

# add ACS individual id
taxsim_dest_min <- taxsim_dest_min |> 
  left_join(xwalk, by = c("year", "taxsimid"))

# Read in the ACS data
ddi <- read_ipums_ddi("data/input_raw_ipums/usa_00027.xml")
# only what I need
data <- read_ipums_micro(ddi) |> clean_names() |> 
  select(year, sample, serial, pernum, empstat, uhrswork, wkswork2, hhwt)

# Read in the CPI data to adjust the wages for inflation
cpi <- read_csv("data/CPI_U_1980_2024.csv") |>
  clean_names() |>
  select(year, cpi_u) |>
  filter(year >= 2016 & year <= 2024)

# extract the cpi for year 2023
cpi_24 <- cpi |> filter(year == 2024) |> pull(cpi_u)

# merge
m_taxsim_dest_min <- taxsim_dest_min |> 
  left_join(data, by = c("year", "sample", "serial", "pernum")) |> 
  # Merge in the CPI data to adjust income for inflation
  left_join(cpi, by = "year")

# final clean
clean_taxsim_dest_min <- m_taxsim_dest_min |> 
  # construct income bins
  mutate(
    # convert to real 2023 dollars
    real_fagi = fagi * (cpi_24 / cpi_u),
    
    real_fagi_bins = cut(
      real_fagi,
      breaks = c(-Inf, 25000, 50000, 75000, 100000, 250000, 500000, Inf),
      right  = FALSE,
      labels = c(
        "[<$25k)",
        "[$25k-$50k)",
        "[$50k-$75k)",
        "[$75k-$100k)",
        "[$100k-$250k)",
        "[$250k-$500k)",
        "[$500k+)"
      )
    ),
    
    # dummy for income > 25k
    real_agi_morethan_25k = (real_fagi > 25000),
    
    # dummy for income > 100k
    real_agi_morethan_100k = (real_fagi > 100000),
  
  )
  

# Compute year-income-bin average, rounded to nearest integer ------------------------------------
fulltime <- clean_taxsim_dest_min |> 
  filter(empstat==1, 
         uhrswork >= 35, 
         wkswork2 >= 5) 
  

sevenbins_avg <- fulltime |> 
  group_by(year, real_fagi_bins) |> 
  summarise(
    across(
      c(depx, age1, age2, age3, pwages, swages, psemp, ssemp, intrec, otherprop, pensions, gssi, transfers, rentpaid, proptax, mortgage),                  
      ~ round(weighted.mean(.x, w = hhwt, na.rm = TRUE))
    ),
    .groups = "drop"
  )
  
morethan25k_avg <- fulltime |> 
  filter(real_agi_morethan_25k==TRUE) |> 
  group_by(year) |> 
  summarise(
    across(
      c(depx, age1, age2, age3, pwages, swages, psemp, ssemp, intrec, otherprop, pensions, gssi, transfers, rentpaid, proptax, mortgage),                  
      ~ round(weighted.mean(.x, w = hhwt, na.rm = TRUE))
    ),
    .groups = "drop"
  ) |> 
  mutate(real_fagi_bins = ">$25k")


morethan100k_avg <- fulltime |> 
  filter(real_agi_morethan_100k==TRUE) |> 
  group_by(year) |> 
  summarise(
    across(
      c(depx, age1, age2, age3, pwages, swages, psemp, ssemp, intrec, otherprop, pensions, gssi, transfers, rentpaid, proptax, mortgage),                  
      ~ round(weighted.mean(.x, w = hhwt, na.rm = TRUE))
    ),
    .groups = "drop"
  ) |> 
  mutate(real_fagi_bins = ">$100k")

# bind rows
combined <- rbind(sevenbins_avg, morethan25k_avg, morethan100k_avg) |> 
  arrange(year, real_fagi_bins)

# add simulated states
combined_state <- tidyr::expand_grid(
  combined,
  state = 1:51
)


d_for_taxsim <- combined_state |> 
  #assume married
  mutate(mstat = 2) |> 
  #add taxsimid
  mutate(
    taxsimid = row_number()
  )
  
# Save the xwalk between taxsimid and the year-state-incomebin
save_xwalk <- d_for_taxsim |> 
  select(year, state, real_fagi_bins, taxsimid)

write_csv(save_xwalk, "data/xwalk/year_state_incomebin_atr_taxsimid_xwalk.csv")


# Save as TAXSIM input ---------------------------------------------------

# only keep taxsim inputs
final_d_for_taxsim <- d_for_taxsim |> 
  select(-real_fagi_bins, -age1, -age2, -age3)

# save the file to use as input to taxsim
write_dta(final_d_for_taxsim, "data/intermediate/taxsim_input/year_state_incomebin_avg_for_taxsim.dta")

