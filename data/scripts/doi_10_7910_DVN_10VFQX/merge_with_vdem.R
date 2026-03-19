##### Code for merging raw cross-national dataset with V-Dem variables #####
#install.packages(c("readr","dplyr")) # If packages not yet installed
# Load packages
library(readr)
library(dplyr)

### Load data (alter file paths as needed)
# Raw protest event dataset
repdatraw <- read_csv("cohesion_crossnat_replication_data_raw.csv")
# V-Dem Version 11
vdem11 <- read_csv("V-Dem-CY-Full+Others-v11.1.csv")

### Merge log GDP per capita (e_migdppcln) and Electoral Democracy index (v2x_polyarchy) from V-Dem into original dataset 
# Select variables to merge, along with country and year identifiers 
vdem11small <- select(vdem11,country_name,year,e_migdppcln,v2x_polyarchy)
# Rename country identifier for compatibility with destination dataset
vdem11small$country <- vdem11small$country_name
# Merge datasets by country and year
merged <- merge(vdem11small,repdatraw, by = c("country","year"))
View(merged)
# Write merged file to .csv for analysis (add file path for export destination as desired)
write.csv(merged,"cohesion_crossnat_replication_data.csv", row.names = FALSE)

