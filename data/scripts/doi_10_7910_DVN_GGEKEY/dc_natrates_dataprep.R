# This is an R notebook that compiles the data for the motivating application on 
# ‘predicting naturalization rates’ for the paper: 
# Maarten Vink, Luuk van der Baaren, David Reichel
# A global panel dataset of dyadic dual citizenship acceptance, 
# International Migration Review, 2025.

# Code preparation: David Reichel and Maarten Vink

#start with clean workspace
rm(list=ls(all=TRUE))

# read in packages
library(tidyverse)
library(eurostat)
library(openxlsx)
library(countrycode)
library(WDI)


# Import data on citizenship acquisitions from Eurostat

# population by citizenship
d <- get_eurostat("migr_pop1ctz")
d <- d %>%
  filter(age == "TOTAL") %>%
  filter(unit == "NR") %>%
  filter(sex == "T") %>%
  dplyr::select(iso2_o = citizen, iso2_d = geo, TIME_PERIOD, citizenship = values, sex) 

# citizenship acquisitions
temp <- get_eurostat("migr_acq")
temp <- temp %>%
  filter(age == "TOTAL") %>%
  filter(agedef == "COMPLET") %>%
  filter(unit == "NR") %>%
  filter(sex == "T") %>%
  dplyr::select(iso2_d = geo, TIME_PERIOD, cit_acq = values, iso2_o = citizen, sex)

d <- left_join(d, temp, by = c("iso2_d", "iso2_o", "TIME_PERIOD", "sex"))
rm(temp)


############################
# first recoding and calculations
d <- d %>%
#  dplyr::filter(!is.na(cit_acq)) %>% 
  dplyr::filter(citizenship != 0) %>%
  mutate(year = as.numeric(str_sub(TIME_PERIOD, 1, 4)),
         nat_rate = cit_acq/citizenship) %>%
  dplyr::filter(nchar(iso2_o) == 2) %>%
  dplyr::filter(nchar(iso2_d) == 2)

d$iso2_o[d$iso2_o == "EL"] <- "GR"
d$iso2_d[d$iso2_d == "EL"] <- "GR"
d$iso2_o[d$iso2_o == "UK"] <- "GB"
d$iso2_d[d$iso2_d == "UK"] <- "GB"

d <- d %>%
  filter(iso2_o != iso2_d) |> 
  filter(year < 2022) # we restrict to 2021 as this was latest year available at time of analysis

# checking the dependent variable
summary(d$nat_rate)

nrow(d);nrow(na.omit(d))
d %>% select(citizenship, cit_acq) %>% na.omit() %>% nrow()

# View(d)
nrow(d) #76728
summary(d)


# World Development Indicator Database

# Add origin and destination country information from World Development Indicator Database

# select relevant variables
inds <- c("income", "NY.GDP.PCAP.KD")

wd1 <- WDI(country = "all", indicator = inds, start = 1960, end = 2022,
           extra = TRUE)

#income group classification missing for CZ in all years - 
#https://datahelpdesk.worldbank.org/knowledgebase/articles/906519-world-bank-country-and-lending-groups
#1992-1993: LM - Lower middle income
#1994-2005: UM - Upper middle income
#2006-2021: H - High income

#impute data for CZE
wd1 <- wd1 %>% 
   mutate(income = ifelse(iso2c == "CZ" & year > 1991 & year < 1994,"Lower middle income",
                          ifelse(iso2c == "CZ" & year > 1993 & year < 2006,"Upper middle income",
                                 ifelse(iso2c == "CZ" & year > 2005,"High income",
                          income))))


wd1_o <- wd1 %>%
  select(iso2_o = iso2c, year, income_o = income, gdp_o = NY.GDP.PCAP.KD)

wd1_d <- wd1 %>%
  select(iso2_d = iso2c, year, income_d = income, gdp_d = NY.GDP.PCAP.KD)

names(wd1)

nrow(d)
d <- d %>%
  left_join(wd1_o, by = c("iso2_o", "year")) %>%
  left_join(wd1_d, by = c("iso2_d", "year"))
nrow(d)

rm(wd1_o); rm(wd1_d)


# add country related inforamtion on EU membership


# use a data file prepared by the authors
ci <- read.xlsx("country_info.xlsx",
                sheet = 1) %>%
  select(iso2 = ISO2, yEUaccess, yindependence)

# identify countries as origin and destination
ci_o <- ci
names(ci_o) <- paste0(names(ci), "_o")

ci_d <- ci
names(ci_d) <- paste0(names(ci), "_d")

d <- d %>%
  left_join(ci_o, by = "iso2_o") %>%
  left_join(ci_d, by = "iso2_d")

d <- d %>%
  mutate(EU_member_o = ifelse(year >= yEUaccess_o, 1, 0),
         EU_member_d = ifelse(year >= yEUaccess_d, 1, 0),
         EU_member_o = ifelse(is.na(EU_member_o), 0, EU_member_o),
         EU_member_d = ifelse(is.na(EU_member_d), 0, EU_member_d))

# Brexit
d$EU_member_o[d$iso2_o == "GB" & d$year > 2019] <- 0
d$EU_member_d[d$iso2_d == "GB" & d$year > 2019] <- 0

d <- d %>%
  mutate(EU_status = ifelse(EU_member_o == 1 & EU_member_d == 1, "EU_both",
                            ifelse(EU_member_o == 1 & EU_member_d == 0, "EU_orig",
                                   ifelse(EU_member_o == 0 & EU_member_d == 1, "EU_dest",
                                          ifelse(EU_member_o == 0 & EU_member_d == 0,
                                                 "EU_none", NA)))),
         upcoming_EU_d = if_else(yEUaccess_d-year < 4 & yEUaccess_d-year >= 0, 1, 
                                 0, missing = 0))


# Dual citizenship policy data

# prepare main datafile to merge with dyadic dual citizenship policy data
d <- d %>%
  mutate(iso3_o = countrycode(iso2_o, "iso2c", "iso3c"),
         iso3_d = countrycode(iso2_d, "iso2c", "iso3c"))

# read in GcDDCAD file
dc <- read_csv("GcDDCAD_v1.0_dyadic.csv")

nrow(d)
# merge
d <- d %>%
  left_join(dc, by = c("iso3_o", "iso3_d", "year"))
nrow(d)


# number of year obs missing for the following origin-destination dyads:
dualcitmis <- d %>% 
  filter(is.na(dc2_dy_bin_c)) %>%
  count(iso2_o, iso2_d, sort = TRUE)
dualcitmis 


# adding country names manipulations
d <- d |>
  mutate(country_o = countrycode(iso2_o, "iso2c", "country.name"), # add origin country name
         country_d = countrycode(iso2_d, "iso2c", "country.name"), # add destination country name
         dyad = paste0(iso2_o, "_", iso2_d)) # add dyad description


# We filter out all observations with: 
# - Missing naturalisation rates missing  
natrate1 <- d %>%
  filter(nat_rate > 1) # 78 obs
# - Naturalisation rates above 1
natrate.na <- d %>%
  filter(is.na(nat_rate)) # 13.145 obs
# - Less than 30 non-citizens per country of origin in a destination country in a year
# (in order to have more stable results)
lmt <- 30
less30noncit <- d %>%
  filter(citizenship < lmt) # 25.009 obs
# The limits the dataset from 76728 to 43296 observations.

# filter out the observations with nat rates above 1
# filter out observations without nat rates
# filter out observations with less than 30 non-citizens per country of origin
d <- d %>%
  filter(nat_rate <= 1) %>%
  filter(!is.na(nat_rate)) %>%
  filter(citizenship >= lmt)
nrow(d) # 43295

# re-order and save final dyadic dataset
col_order <- c("iso2_o", "iso3_o","country_o","iso2_d", "iso3_d", "country_d", "dyad", "year",
               "nat_rate", "cit_acq", "citizenship",
               "EU_status", "EU_member_o",  "EU_member_d",
               "income_o", "income_d", "gdp_o", "gdp_d",
              "dc_dy_bin", "dc1_dy_bin", "dc2_dy_bin", "dc_dy_cat","dc1_dy_cat", "dc2_dy_cat",
               "treaty", "dy_c",  
               "dc_dy_bin_c","dc1_dy_bin_c",  "dc2_dy_bin_c", "dc_dy_cat_c","dc1_dy_cat_c", "dc2_dy_cat_c")
d <- d[, col_order]

d <- d[order(d$year,d$iso3_o),]

# save file with date reference
write.csv(d, file = paste0("dc_natrates_dyadic_data_", Sys.Date(), ".csv"), row.names = FALSE)



