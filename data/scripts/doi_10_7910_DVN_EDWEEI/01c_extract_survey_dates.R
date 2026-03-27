# Alexander F. Gazmararian
# agazmararian@gmail.com
library(here)
library(janitor)
library(readxl)
library(tidyverse)

g <- suppressMessages(suppressWarnings(read_xlsx(here("Data", "input", "survey", "worldriskpoll", "2019", "data", "field_dates.xlsx"))))
g <- suppressMessages(clean_names(g))
g <- subset(g, select = c(country, data_collection_date))
g <- g %>% rename(date = data_collection_date)

extract_first_date_and_year <- function(date_string) {
  first_date_match <- regmatches(date_string, regexpr("^[A-Za-z]{3} [0-9]{1,2}", date_string))
  year_matches <- gregexpr("[0-9]{4}", date_string)
  years <- regmatches(date_string, year_matches)[[1]]
  first_date_with_year <- paste(first_date_match, years[1])
  return(first_date_with_year)
}

g$startdate <- mdy(extract_first_date_and_year(g$date))

g <- subset(g, select = c(country, startdate))
g$yday <- lubridate::yday(g$startdate)
g$year <- lubridate::year(g$startdate)

risk <- readRDS(here("Data", "inter", "19_wrp", "wrp_processed.rds"))
regid <- readRDS(here("Data", "inter", "19_wrp", "admin_wrp_crosswalk.rds"))

country_id <- left_join(risk, regid, by = "wpid_random")
country_id <- subset(country_id, select = c(country, gid_0))
country_id <- unique(country_id)

g_out <- left_join(g, country_id, by = c("country"))
anti_join(country_id, g, by = c("country"))
g_out$gid_0[g_out$country == "Bosnia and Herzegovina"] <- "BIH"
g_out$gid_0[g_out$country == "Swaziland (Eswatini)"] <- "SWZ"
g_out$gid_0[g_out$country == "Palestinian Territories"] <- "PSE"

g_out$year <- NULL
g_out <- subset(g_out, !is.na(gid_0))
g_out <- subset(g_out, select = -country)
g_out <- g_out %>% arrange(gid_0)
saveRDS(g_out, here("Data", "inter", "19_wrp", "field_dates.rds"))
