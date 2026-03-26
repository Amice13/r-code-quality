#################################################################################
### function to get tract-level group counts for metros, 2015-19 survey year
#################################################################################

require(tidyverse)

ct <- read_csv("data/clean/groups_tract.csv.gz", col_types = "ffffci") %>%
     filter(cbsa_type == "metro" & year == "2015-19") %>%
     dplyr::select(-cbsa_type)

## if there is a crosswalk loaded, keep only the matching groups
if (exists("cw")) {ct <- semi_join(ct, cw, by = "abb")}