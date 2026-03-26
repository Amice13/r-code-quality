# Alexander F. Gazmararian
# agazmararian@gmail.com

library(countrycode)
library(data.table)
library(here)
library(tidyverse)
source(here("Code", "fun", "load_sim.R"))

coord <- readRDS(here("Data", "inter", "grid", "coord.rds"))
cell <- readRDS(here("Data", "inter", "grid", "cell.rds"))

gdp <- load_sim(here("Data", "input", "grid", "damage_est", "realgdp_Warm.csv"), "gdp")
w <- gdp

gdp_nw <- load_sim(here("Data", "input", "grid", "damage_est", "realgdp_noWarm.csv"), "gdp_nw")
nw <- gdp_nw

gdp_ratio <- gdp / gdp_nw

# convert greenland to denmark for cowc
coord$iso3c[coord$iso3c == "GRL"] <- "DNK"

coord <- coord %>% dplyr::arrange(cell_index)
griddamage <- cbind(coord, gdp_ratio)
saveRDS(griddamage, here("Data", "inter", "grid", "tempdamage.rds"))

# Aggregate to Country Level ----------------------------------------------
# add cell population data
pop_per <- cell$pop_per
# aggregate warming data
warm_preagg <- cbind(coord, w, pop_per)
# aggregate, weighting by share of present population
warm_agg <- warm_preagg %>%
  dplyr::group_by(iso3c) %>%
  dplyr::summarise(across(gdp_25:gdp_400, .fns = list("pop" = ~ sum(.x * pop_per), "total" = ~ sum(.x)), .names = "{.fn}_{.col}"))
# save iso3c vector
iso <- warm_agg[, 1]
# drop iso3c
warm_agg <- warm_agg[, -1]
# aggregate warming COUNTERFACTUAL data
nowarm_preagg <- cbind(coord, nw, pop_per)
# aggregate, weighting by share of present population
nowarm_agg <-
  nowarm_preagg %>%
  dplyr::group_by(iso3c) %>%
  dplyr::summarise(across(gdp_nw_25:gdp_nw_400, .fns = list("pop" = ~ sum(.x * pop_per), "total" = ~ sum(.x)), .names = "{.fn}_{.col}"))
nowarm_agg <- nowarm_agg[, -1]
ratio <- warm_agg / nowarm_agg
country <- cbind(iso, ratio)
country$cowc <- countrycode(country$iso3c, "iso3c", "cowc")
# save output
saveRDS(country, here("Data", "inter", "country", "damage_country.rds"))
