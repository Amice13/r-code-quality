# Alexander F. Gazmararian
# agazmararian@gmail.com

library(here)
library(tidyverse)

g <- readRDS(here("Data", "inter", "ces", "ces10-14.rds"))

# Damage
damages <- readRDS(here("Data", "inter", "usa", "hsiang_damage_processed.rds"))
damages$damage_z <- scale(damages$totaldamage)[, 1]
damages <- subset(damages, select = c(fips, pop2012:damage_z))
g <- left_join(g, damages, by = c("countyfips" = "fips"))
g$damage_bin <- ifelse(g$totaldamage > 0, 1, 0)

# Temperature Anomalies
temp <- readRDS(here("Data", "inter", "ces", "usa_tanom.rds"))
temp$year <- year(temp$starttime)
temp <- subset(temp, select = c(caseid, tanom, year))
g <- left_join(g, temp, by = c("caseid", "year"))

# Load Disaster Data
disaster <- readRDS(here("Data", "inter", "usa", "fema_processed.rds"))
g <- left_join(g, disaster, by = c("countyfips" = "fips", "year"))

disaster_2y <- subset(disaster, year %in% c(2012, 2014, 2016))
disaster_2y$year <- disaster_2y$year - 2
names(disaster_2y)[3:ncol(disaster_2y)] <- paste0(names(disaster_2y)[3:ncol(disaster_2y)], "_placebo_2")

g <- left_join(g, disaster_2y, by = c("countyfips" = "fips", "year"))

# Treatment
g$fire <- with(g, ifelse(n_fire > 0, 1, 0))
g$time <- with(g, as.integer(ifelse(year == 2010, 1, ifelse(year == 2012, 2, ifelse(year == 2014, 3, NA)))))
g$caseid <- as.integer(g$caseid)

# Subset to covariates for analysis
g <- subset(g, select = c(
  # IDs
  caseid, year, countyfips, time,
  # Outcome
  gw_binary,
  # Treatment variables
  fire, tanom, n_fire_placebo_2,
  # Damage variables
  damage_bin, damage_z, totaldamage,
  # Demographics (numeric indicators)
  employ, edu_1, edu_2, edu_3,
  income_q1, income_q2, income_q3, income_q4, income_q5, income_ns,
  dem, rep, lib, con,
  relig_1, relig_2, relig_3, relig_4,
  birthyr, female, black, latino, ownhome_bin, newparent,
  # Demographics (categorical versions for models)
  ideo3, income5, religimp,
  # Climate belief variables
  skeptic, undecided, believer
))


saveRDS(g, here("Data", "output", "ces", "cespanel.rds"))
message("Merged CES data and saved to Data/output/ces/cespanel.rds")