# Alexander F. Gazmararian
# agazmararian@gmail.com

library(here)
library(tidyverse)

# Load climate damages data from Hsiang et al.
damages <- read.csv(here("Data", "input", "usa", "hsiang_damage", "county_damages_by_sector.csv"))

# Prepare names for climate damages
names(damages) <- c("state", "county", "fips", "pop2012", "income2012", "agdamage", "mortality", "energycosts", "laborlowrisk", "laborhighrisk", "coastaldamage", "propertycrime", "violentcrime", "totaldamage")

# Save output
saveRDS(damages, here("Data", "inter", "usa", "hsiang_damage_processed.rds"))
message("Processed Hsiang et al. climate damages data and saved to Data/inter/usa/hsiang_damage_processed.rds")