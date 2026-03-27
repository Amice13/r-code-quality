# Preliminary ----

if (!require(pacman)) install.packages("pacman") # install pacman to install any required packages

pacman::p_load(tidyverse, haven, labelled, modelsummary, fixest, kableExtra, panelr, lubridate, here, metafor)

basedir <- here()

setwd(basedir)

# Read BESIP data + format to R ----

BESIP <- read_dta("data/BESIP.dta")

BESIP |>
  drop_unused_value_labels() 

clist <- c("Region", "subjclassW")
for (i in clist) {
  BESIP[[i]] <- as_factor(BESIP[[i]])
}

clist <- c("ptvConW", "regionEconW", "leftRightW", "wave")
for (i in clist) {
  BESIP[[i]] <- as.numeric(BESIP[[i]])
}

rm(clist, i)

BESIP <- BESIP %>% 
  rename("id" = "_id") # to avoid issues with underscore

panel <- panel_data(BESIP, id = id, wave = wave)

## Merge in unemployment data ----

# region: 1 North East, 2 North West, 3 Yorkshire and the Humber, 4 East Midlands, 5 West Midlands, 6 East of England, 7 London, 8 South East, 9 South West, 10 Wales, 11 Scotland

# wave: feb 14, may 14, sep 14, mar 15, apr 15, may 15, apr 16, may 16, jun 16, nov 16, apr 17, may 17, jun 17, may 18, mar 19, may 19, nov 19, nov 19, dec 19, jun 20, may 21, dec 21, may 22

# create month variable 
month_lookup <- c(
  "2014-02-01", "2014-05-01", "2014-09-01",
  "2015-03-01", "2015-04-01", "2015-05-01",
  "2016-04-01", "2016-05-01", "2016-06-01", "2016-11-01",
  "2017-04-01", "2017-05-01", "2017-06-01",
  "2018-05-01",
  "2019-03-01", "2019-05-01", "2019-11-01", "2019-11-01", "2019-12-01",
  "2020-06-01",
  "2021-05-01", "2021-12-01",
  "2022-05-01"
)

month_dates <- as.Date(month_lookup)

panel <- panel %>%
  mutate(month = month_dates[wave])

unemp <- read.csv("data/regional_unemployment.csv", sep=";")

# pivot to long
unemp_long <- unemp %>%
  pivot_longer(
    cols = -Region,
    names_to = "month_raw",
    values_to = "unemployment_rate"
  )

# clean and convert dates
unemp_long <- unemp_long %>%
  mutate(
    month = dmy(str_remove(month_raw, "^X")),  
    unemployment_rate = as.numeric(str_replace(unemployment_rate, ",", "."))
  ) %>%
  select(Region, month, unemployment_rate)

unemp_long$Region <- as.factor(unemp_long$Region)

# convert region numbers to labels

region_labels <- c(
  "North East",
  "North West",
  "Yorkshire and the Humber",
  "East Midlands",
  "West Midlands",
  "East of England",
  "London",
  "South East",
  "South West",
  "Wales",
  "Scotland"
)

unemp_long$Region <- factor(unemp_long$Region, levels = 1:11, labels = region_labels)

# merge with panel
panel <- panel %>%
  left_join(unemp_long, by = c("Region", "month"))

saveRDS(panel, file = "data/BESIP_clean.rds")
