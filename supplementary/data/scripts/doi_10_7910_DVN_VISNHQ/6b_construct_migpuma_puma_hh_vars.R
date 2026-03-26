##################################################################################
# Agrawal and Chen (2026)
#
# Descriptions: Construct mean house value at the puma-income-age bin level
# 
##################################################################################

# Clear environment
rm(list = ls())

library(tidyverse)
library(janitor)


all <- read_csv("data/derived_ipums/cleaned_all_wATR.csv")

subset_data <- all |> 
  select(year, sample, serial, hhwt, dest_state_fips, puma, valueh, fagi, real_fagi, real_fagi_bins, p1age) |> 
  # construct puma fips code (7 character)
  mutate(
    dest_fips = dest_state_fips*100000 + puma,
    dest_fips = as.character(dest_fips),
    dest_fips = ifelse(str_count(dest_fips) < 7, paste0("0", dest_fips), dest_fips),
    
    
    # AGI income bins 
    valueh_fagi_bins = cut(
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
   
    # Age bins
    age_bins = cut(
      p1age,
      breaks = c(-Inf, 25, 45, 65, Inf),
      right = FALSE,
      labels = c(
        "<25",
        "[25-45)",
        "[45-65)",
        "[65+)"
      )
    )
    )


# Number of HH in each puma by year, mean house value by income bin and age bin in each puma by year
puma_d <- subset_data |> 
  mutate(valueh_nomiss = ifelse(valueh == 9999999, NA_real_, valueh)) |> 
  group_by(year, dest_fips, valueh_fagi_bins, age_bins) |> 
  summarise(
    n_hh = sum(hhwt),
    mean_valueh = weighted.mean(valueh_nomiss, w = hhwt, na.rm = TRUE)
  ) |> 
  ungroup()


# read in migpuma puma xwalk
migpuma_puma_xwalk <- read_csv("data/xwalk/cleaned_migpuma1_puma_xwalk.csv") |> 
  rename(
    origin_migpuma_fips = migpuma_fips,
    dest_fips = puma_fips
  )

# merge in migpuma fips
final <- puma_d |> 
  left_join(migpuma_puma_xwalk, by = c("year", "dest_fips")) 


# save
write_csv(final, "data/derived_ipums/puma_proptax_hh_vars.csv")






