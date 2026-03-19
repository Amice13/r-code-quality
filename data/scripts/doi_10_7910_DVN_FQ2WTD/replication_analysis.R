## Replication code for:
## Going to Outer Space with New Space
## Code by Olaf de Rohan Willner
## ocd4@cornell.edu

## Version: 22 February 2024

## This file prepares the NASA contracts data for analysis 
## and produces the graph for Figure 1.

#####################
## 1. PREPARE DATA ##
#####################

## Load packages
library(tidyverse)
library(data.table)
library(scales)
require(readr)
library(janitor)

setwd("~/Research/Cornell TPL/Space Policy/Going to Outer Space/Replication Data")

## Load the SpaceX NASA contracts data
## Data obtained from NASA's Procurement Data View ("https://prod.nais.nasa.gov/cgibin/npdv/adhoc.cgi"),
## with the following parameters:
## Contractor: SPACE EXPLORATION TECHNOLOGIES
## NASA Center: ALL
## Business Type: ALL
## Instrument: ALL
## Product/Service Code: ALL
## NAICS Code(s): ALL

## The results from these queries were compiled into a single file, "spacex_contracts_yearly.csv"

## Load SpaceX contracts data
spacex_contracts <- fread("spacex_contracts_yearly.csv")

## Load all NASA contracts for every fiscal year, FY06 - FY23
## Data obtained from NASA's Procurement Data View ("https://prod.nais.nasa.gov/cgibin/npdv/adhoc.cgi"),
## with the following parameters:
## Contractor: (blank)
## NASA Center: ALL
## Business Type: ALL
## Instrument: ALL
## Product/Service Code: ALL
## NAICS Code(s): ALL

## Individual files for each year were converted to .csv files from the .xlsx 
## format obtained from the NAIS website. For the sake of transparency and reproducibility,
## each year file is present in the repository.

# Specify the file path prefix and selected columns
file_path_prefix <- "Contracts_FY06_FY23/nasa_contracts_fy"
selected_columns <- c("Contractor", "Contract Number", "Current FY Obligations")

# Read and process data for each year until 2023
contracts06 <- read_delim(paste0(file_path_prefix, "06.csv"), col_select = selected_columns) %>% 
  mutate(fiscal_year = 2006)
contracts07 <- read_delim(paste0(file_path_prefix, "07.csv"), col_select = selected_columns) %>% 
  mutate(fiscal_year = 2007)
contracts08 <- read_delim(paste0(file_path_prefix, "08.csv"), col_select = selected_columns) %>% 
  mutate(fiscal_year = 2008)
contracts09 <- read_delim(paste0(file_path_prefix, "09.csv"), col_select = selected_columns) %>% 
  mutate(fiscal_year = 2009)
contracts10 <- read_delim(paste0(file_path_prefix, "10.csv"), col_select = selected_columns) %>% 
  mutate(fiscal_year = 2010)
contracts11 <- read_delim(paste0(file_path_prefix, "11.csv"), col_select = selected_columns) %>% 
  mutate(fiscal_year = 2011)
contracts12 <- read_delim(paste0(file_path_prefix, "12.csv"), col_select = selected_columns) %>% 
  mutate(fiscal_year = 2012)
contracts13 <- read_delim(paste0(file_path_prefix, "13.csv"), col_select = selected_columns) %>% 
  mutate(fiscal_year = 2013)
contracts14 <- read_delim(paste0(file_path_prefix, "14.csv"), col_select = selected_columns) %>% 
  mutate(fiscal_year = 2014)
contracts15 <- read_delim(paste0(file_path_prefix, "15.csv"), col_select = selected_columns) %>% 
  mutate(fiscal_year = 2015)
contracts16 <- read_delim(paste0(file_path_prefix, "16.csv"), col_select = selected_columns) %>% 
  mutate(fiscal_year = 2016)
contracts17 <- read_delim(paste0(file_path_prefix, "17.csv"), col_select = selected_columns) %>% 
  mutate(fiscal_year = 2017)
contracts18 <- read_delim(paste0(file_path_prefix, "18.csv"), col_select = selected_columns) %>% 
  mutate(fiscal_year = 2018)
contracts19 <- read_delim(paste0(file_path_prefix, "19.csv"), col_select = selected_columns) %>% 
  mutate(fiscal_year = 2019)
contracts20 <- read_delim(paste0(file_path_prefix, "20.csv"), col_select = selected_columns) %>% 
  mutate(fiscal_year = 2020)
contracts21 <- read_delim(paste0(file_path_prefix, "21.csv"), col_select = selected_columns) %>% 
  mutate(fiscal_year = 2021)
contracts22 <- read_delim(paste0(file_path_prefix, "22.csv"), col_select = selected_columns) %>% 
  mutate(fiscal_year = 2022)
contracts23 <- read_delim(paste0(file_path_prefix, "23.csv"), col_select = selected_columns) %>% 
  mutate(fiscal_year = 2023)

## Combine all years into one dataframe
contracts_combined <- bind_rows(contracts06, contracts07, contracts08, contracts09, contracts10, contracts11, contracts12, contracts13, contracts14, contracts15, contracts16, contracts17, contracts18, contracts19, contracts20, contracts21, contracts22, contracts23) %>% 
  clean_names(case = "snake")

write_csv(contracts_combined, "contracts_combined.csv")

## Group by fiscal year and summarize obligations
combined_by_year <- contracts_combined %>% 
  group_by(fiscal_year) %>% 
  summarize(obligations = sum(current_fy_obligations, na.rm=TRUE))

## Express SpaceX contracts as percent of NASA obligations
merged <- merge(combined_by_year, select(spacex_contracts, "fiscal_year", "fy_total_obligations"))
spacex_percent <- mutate(merged, spacex_pct = fy_total_obligations / obligations) %>% 
  select(fiscal_year, spacex_pct)

#######################
## 2. PRODUCE GRAPHS ##
#######################

## Plot SpaceX NASA obligations by year, FY06 - FY23
ggplot(spacex_contracts, aes(x = fiscal_year, y = fy_total_obligations)) + 
  ggtitle("SpaceX Total NASA Obligations by Fiscal Year") +
  geom_line(color = "cornflowerblue", size = 1) +
  xlab(label = "Fiscal Year") +
  ylab(label = "Total Obligations") +
  theme_bw() +
  scale_x_continuous(breaks=seq(2006,2023,1)) +
  scale_y_continuous(labels = scales::dollar_format(prefix="$", suffix = "M", scale = 1e-6),
                     breaks=seq(0,2250000000,250000000))

## Plot total NASA obligations by fiscal year, FY06 - FY23
ggplot(combined_by_year, aes(x = fiscal_year, y = as.numeric(obligations))) + 
  ggtitle("Total NASA Obligations by Fiscal Year") +
  geom_line(color = "cornflowerblue", size = 1) +
  xlab(label = "Fiscal Year") +
  ylab(label = "Total Obligations") +
  theme_bw() +
  scale_x_continuous(breaks=seq(2006,2023,1)) +
  scale_y_continuous(labels = scales::dollar_format(prefix="$", suffix = "B", scale = 1e-9),
                     expand = c(0, 0), limits = c(0, 25e9))

## Plot SpaceX NASA obligations as a percentage of total NASA obligations by year, FY06 - FY23
ggplot(spacex_percent, aes(x = fiscal_year, y = spacex_pct)) + 
  ggtitle("SpaceX obligations by fiscal year as a percent of total NASA obligations") +
  geom_line(color = "cornflowerblue", size = 1) +
  xlab(label = "Fiscal Year") +
  ylab(label = "SpaceX Obligations") +
  theme_bw() +
  scale_x_continuous(breaks=seq(2006,2023,1)) +
  scale_y_continuous(labels = scales::percent)


