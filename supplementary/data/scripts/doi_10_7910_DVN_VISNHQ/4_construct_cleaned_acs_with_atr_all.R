##################################################################################
# Agrawal and Chen (2026)
#
# Descriptions: Merge TAXSIM Outputs onto Tax-Unit-Level ACS Data
# 
# 
##################################################################################
# Clear environment
rm(list = ls())

library(ipumsr)
library(readr)
library(dplyr)
library(stringr)
library(tidyr)
library(janitor)
library(haven)
library(data.table)
library(arrow)
library(purrr)


system.time({
  
  #1. Construct cleaned ATR datasets using output from TAXSIM ----------
  
  ##1.1 Load TAXSIM output datasets for origin and destination states ----------
  
  read_taxsim_with_year_fix <- function(path) {
    df <- read_dta(path)
    
    # change year back to 2024 if this is the 2024 data
    if (str_detect(basename(path), "2024")) {
      df <- df |>  mutate(year = 2024L)
    }
    
    df
  }
  

  # list all years of taxsim output files for the origin states
  taxsim_output_origin_files <- list.files(
    "data/intermediate/taxsim_output/",
    pattern = "^taxsim_out_origin_.*\\.dta$",
    full.names = TRUE
  )
  
  # combine into one big dataset for the origin states
  taxsim_origin <- taxsim_output_origin_files |>
    map_dfr(read_taxsim_with_year_fix)
  
  
  # list all years of taxsim output files for the destination states
  taxsim_output_dest_files <- list.files(
    "data/intermediate/taxsim_output/",
    pattern = "^taxsim_out_destination_.*\\.dta$",
    full.names = TRUE
  )
  
  # combine into one big dataset for the destination states
  taxsim_dest <- taxsim_output_dest_files |>
    map_dfr(read_taxsim_with_year_fix)
  
    
  ##1.2 Load xwalk between year by taxsimid and my_taxid/ACS id ----------
  
  taxid_xwalk <- read_csv("data/xwalk/my_taxid_taxsimid_xwalk.csv") |>
    arrange(year, sample, serial, tax_unit_pernum) |> 
    select(year, taxsimid, sample, serial, tax_unit_pernum)
  
  
  ##1.3 Main cleaning and merging ----------
  
  subset_taxsim_origin <- taxsim_origin |>
    select(year, taxsimid, fiitax, siitax, frate, srate) |>
    rename(
      origin_ftax = fiitax,
      origin_stax = siitax,
      origin_frate = frate,
      origin_srate = srate
    )
  
  subset_taxsim_dest <- taxsim_dest |>
    # include agi (we only know income in dest state)
    select(year, taxsimid, fiitax, siitax, frate, srate, v10) |>
    rename(
      dest_ftax = fiitax,
      dest_stax = siitax,
      dest_frate = frate,
      dest_srate = srate,
      fagi = v10
    )
  
  
  cleaned_taxsim <- subset_taxsim_origin |>
    # Merge the origin and destination datasets
    left_join(subset_taxsim_dest, by = c("year", "taxsimid")) |>
    # Merge in the ACS tax unit pernum
    left_join(taxid_xwalk, by = c("year", "taxsimid")) |>
    mutate(
      # compute origin and dest ASTR
      # deal with negative/zero AGI
      origin_astr = case_when(
        fagi > 0  ~ origin_stax / fagi * 100,
        fagi < 0  ~ origin_stax / abs(fagi) * 100,
        fagi == 0 ~ 0
      ),
      
      dest_astr = case_when(
        fagi > 0  ~ dest_stax / fagi * 100,
        fagi < 0  ~ dest_stax / abs(fagi) * 100,
        fagi == 0 ~ 0
      )
      
    )
  
  
  #2. Prep the tax-unit ACS for merging with the tax datasets -------------------------------------------
  
  ## 2.1 Read in tax-unit ACS and CPI data ------------------------------------------------
  
  # Read in the tax-unit ACS data
  data <- read_dta("data/derived_ipums/acs_for_taxsimclean_collapsed.dta")
  
  # Read in the CPI data to adjust the wages for inflation
  cpi <- read_csv("data/CPI_U_1980_2024.csv") |>
    clean_names() |>
    select(year, cpi_u) |>
    filter(year >= 2016 & year <= 2024)
  
  # extract the cpi for year 2024
  cpi_24 <- cpi |> filter(year == 2024) |> pull(cpi_u)
  
  
  ## 2.2 Create full sample of everyone for analysis ----------------------------------------------------------------
  all <- data |>
    # subset to those living in the 50 states plus DC
    filter((statefip >= 1 & statefip <= 56)) |>
    # (for now) select only relevant variables to make the df more manageable to work with
    select(
      year, sample, serial, tax_unit_pernum, pernum, perwt, hhwt, marst, qual_child,
      migplac1, migrate1, statefip, puma, migpuma1, pwstate2,  
      p1incwage, p1tranwork, p1incretir, p1empstat, p1age,
      p1incbus00, p1inctot, valueh, wkswork2, uhrswork,
      p2age, p2incretir, p2empstat, p2tranwork, p2statefip, p2migrate1, p2pwpuma00, depx
      ) |>
    # rename variables to prep for merging with the tax data later
    rename(
      origin_state_fips = migplac1,
      dest_state_fips = statefip,
      work_state_fips = pwstate2
      )


  # 3. Merge the cleaned ACS data with the cleaned TAXSIM tax dataset ----------------
  
  m_all <- all |>
    left_join(cleaned_taxsim, by = c("year", "sample", "serial", "tax_unit_pernum")) |>
    # Merge in the CPI data to adjust income for inflation
    left_join(cpi, by = "year")
  
  
  ##3.1 Clean merged dataset in prep for plotting----------------------------------------------------
  cleaned_all <- m_all |>
    # subset to the tax units
    filter(!is.na(tax_unit_pernum)) |>
    mutate(
      # handle missings
      wage_inc = ifelse((p1incwage == 999998 | p1incwage == 999999), NA, p1incwage),
      total_inc = ifelse((p1inctot == 9999998 | p1inctot == 9999999), NA, p1inctot),
      # this is just for identifying retirees
      retir_inc = ifelse(p1incretir == 999999, NA, p1incretir),
      # convert to real 2023 dollars
      real_fagi = fagi * (cpi_24 / cpi_u),
      # construct federal AGI income bins
      real_fagi_bins = cut(
        real_fagi,
        breaks = c(25000, 50000, 75000, 100000, 250000, 500000, Inf),
        right  = FALSE,
        labels = c(
          "[$25k-$50k)",
          "[$50k-$75k)",
          "[$75k-$100k)",
          "[$100k-$250k)",
          "[$250k-$500k)",
          "[$500k+)"
        )
      )
    ) |>
    # create dest-origin tax differentials 
    mutate(
      # change in ASTR
      astr_diff = dest_astr - origin_astr
    ) |>
    # Create a state-pair variable (no direction) for clustering standard errors
    mutate(
      state_pair_no_direct = case_when(
        # convert to numerics for comparisons
        # just to suppress warning message, overall not an issue
        as.numeric(origin_state_fips) < as.numeric(dest_state_fips) ~ origin_state_fips*100 + dest_state_fips,
        as.numeric(origin_state_fips) >= as.numeric(dest_state_fips) ~ dest_state_fips*100 + origin_state_fips
      )
    ) 
  

  # save as a cleaned dataset
  write_csv(cleaned_all, "data/derived_ipums/cleaned_all_wATR.csv")
  
  
})



