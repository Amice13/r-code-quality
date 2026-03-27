# Alexander F. Gazmararian
# agazmararian@gmail.com

library(here)
library(tidyverse)

risk <- readRDS(here("Data", "inter", "19_wrp", "wrp_processed.rds"))
risk <- risk %>%
  filter(!country %in% c("Libya", "Afghanistan", "Iraq", "Zimbabwe", "Turkmenistan"))
regid <- readRDS(here("Data", "inter", "19_wrp", "admin_wrp_crosswalk.rds"))
risk <- left_join(risk, regid, by = "wpid_random")
risk <- subset(risk, select = c(country, gid_0)) |> unique()
risk$iso3c <- risk$gid_0
saveRDS(risk, here("Data", "inter", "19_wrp", "countries_include.rds"))