# Alexander F. Gazmararian
# agazmararian@gmail.com

library(countrycode)
library(here)
library(tidyverse)
library(readxl)

ctry_incl <- readRDS(here("Data", "inter", "19_wrp", "countries_include.rds"))

time_period <- 2018

wdi <- read_excel("Data/input/country/wdi/WDIEXCEL_October2025.xlsx")
wdi_march <- read_excel("Data/input/country/wdi/WDIEXCEL_March2025.xlsx")

# GDP per capita in current USD
df <- subset(wdi, `Indicator Code` == "NY.GDP.PCAP.CD", select = c(`Country Code`, `2018`))
names(df) <- c("iso3c", "gdppc_nat")
# Venezuela: https://www.imf.org/external/datamapper/profile/VEN
df[df$iso3c == "VEN", ]$gdppc_nat <- 2620
# Kosovo: https://www.macrotrends.net/global-metrics/countries/XKX/kosovo/gdp-per-capita
df <- rbind(df, data.frame(iso3c = "XKO", gdppc_nat = 4384))
# Taiwan: https://www.statista.com/statistics/727592/gross-domestic-product-gdp-per-capita-in-taiwan/
df <- rbind(df, data.frame(iso3c = "TWN", gdppc_nat = 25825))
# Remove any duplicates
df <- df %>% distinct(iso3c, .keep_all = TRUE)

# Agriculture value-added to GDP
ag <- subset(wdi, `Indicator Code` == "NV.AGR.TOTL.ZS", select = c(`Country Code`, `2018`))
names(ag) <- c("iso3c", "ag_share")
# Venezuela: https://www.cia.gov/the-world-factbook/about/archives/
ag[ag$iso3c == "VEN", ]$ag_share <- 4.7
# Kosovo: also CIA world fact book
ag <- rbind(ag, data.frame(iso3c = "XKO", ag_share = 11.9))
# Taiwan: also CIA
ag <- rbind(ag, data.frame(iso3c = "TWN", ag_share = 1.8))
# Remove any duplicates
ag <- ag %>% distinct(iso3c, .keep_all = TRUE)

# CO2 emissions (Mt CO2)
co2nat <- subset(wdi, `Indicator Code` == "EN.GHG.ALL.MT.CE.AR5", select = c(`Country Code`, `2018`))
names(co2nat) <- c("iso3c", "co2_nat")
co2nat <- filter(co2nat, !is.na(co2_nat))
# PSE: https://www.pcbs.gov.ps/Portals/_Rainbow/Documents/Emission-2018-E.html
co2nat <- rbind(co2nat, data.frame(iso3c = "PSE", co2_nat = 2968 / 1e7))
# TWN: EDGAR (https://edgar.jrc.ec.europa.eu/report_2019)
co2nat <- rbind(co2nat, data.frame(iso3c = "TWN", co2_nat = 284.55))
# MNE: Our World in Data (https://ourworldindata.org/co2/country/montenegro)
co2nat <- rbind(co2nat, data.frame(iso3c = "MNE", co2_nat = 2.4))
# XKO: Our World in Data
co2nat <- rbind(co2nat, data.frame(iso3c = "XKO", co2_nat = 8.04))
# SRB:  https://www.iea.org/countries/serbia/emissions
co2nat <- rbind(co2nat, data.frame(iso3c = "SRB", co2_nat = 45))
# Remove any duplicates
co2nat <- co2nat %>% distinct(iso3c, .keep_all = TRUE)

# National population
natpop <- subset(wdi, `Indicator Code` == "SP.POP.TOTL", select = c(`Country Code`, `2018`))
names(natpop) <- c("iso3c", "pop_nat")
# TWN: CIA World Factbook
natpop <- rbind(natpop, data.frame(iso3c = "TWN", pop_nat = 23595274))
# XKO: CIA World Factbook
natpop <- rbind(natpop, data.frame(iso3c = "XKO", pop_nat = 1977093))
# Remove any duplicates
natpop <- natpop %>% distinct(iso3c, .keep_all = TRUE)

# BL Education data
# https://github.com/barrolee/BarroLeeDataSet/blob/master/BLv3.md
bl <- read.csv(here("Data", "input", "country", "barro_lee_education", "BL_v3_MF1564.csv"))
bl <- filter(bl, year == 2015)
bl$iso3c <- countrycode(bl$WBcode, "wb", "iso3c")
bl$iso3c[bl$WBcode == "SER"] <- "SRB"
bl$iso3c[bl$WBcode == "ROM"] <- "MDA"

# Educational attainment, at least upper secondary
edu <- subset(wdi_march, `Indicator Code` == "SE.SEC.CUAT.UP.ZS", select = c(`Country Code`, `2010`:`2020`))
edu <- edu %>%
  pivot_longer(cols = !`Country Code`, names_to = "year", values_to = "edu_sec")
names(edu) <- c("iso3c", "year", "edu_sec")

edu$edu_sec <- as.numeric(edu$edu_sec)  # Convert to numeric, non-numeric values become NA

edu <- edu %>%
  group_by(iso3c) %>%
  summarize(edu_sec = mean(edu_sec, na.rm = TRUE))

edu <- edu %>% filter(!is.na(edu_sec) & !is.infinite(edu_sec))
# 1 TWN
twn <- subset(bl, country == "Taiwan")
edu <- rbind(edu, data.frame(iso3c = "TWN", edu_sec = twn$ls + twn$lh))
# 2 GAB
gab <- subset(bl, country == "Gabon")
edu <- rbind(edu, data.frame(iso3c = "GAB", edu_sec = gab$ls + gab$lh))
# 3 LBY
lby <- subset(bl, country == "Libyan Arab Jamahiriya")
edu <- rbind(edu, data.frame(iso3c = "LBY", edu_sec = lby$ls + lby$lh))
# 4 XKO
# https://idea.usaid.gov/cd/kosovo/education
# https://www.unicef.org/kosovoprogramme/children-kosovo
edu <- rbind(edu, data.frame(iso3c = "XKO", edu_sec = 87))
# 5 ARG
arg <- subset(bl, country == "Argentina")
edu <- rbind(edu, data.frame(iso3c = "ARG", edu_sec = arg$ls + arg$lh))
# Remove any duplicates
edu <- edu %>% distinct(iso3c, .keep_all = TRUE)

nat <- df %>%
  full_join(ag, by = "iso3c", relationship = "one-to-one") %>%
  full_join(co2nat, by = "iso3c", relationship = "one-to-one") %>%
  full_join(natpop, by = "iso3c", relationship = "one-to-one") %>%
  full_join(edu, by = "iso3c", relationship = "one-to-one") %>%
  full_join(ctry_incl, by = "iso3c") %>%
  filter(!is.na(country))

nat$gdppc_nat_log <- log(nat$gdppc_nat)
nat$pop_nat_log <- log(nat$pop_nat)
nat$co2_nat_log <- log(nat$co2_nat)

saveRDS(nat, here("Data", "inter", "19_wrp", "wdi_country_covariates.rds"))