# Alexander F. Gazmararian
# w/assistance from Marie Li
# agazmararian@gmail.com

library(here)
library(tidyverse)


tmax_names <- list.files(path = "Data/input/usa/nClimGrid-Daily/tmax/", pattern = "*.csv", full.names = TRUE, recursive = FALSE)
tmax_out <- lapply(tmax_names, read.csv, col.names = c(
  "cty", "NCEI_Code", "County_Name", "Year", "Month", "Data_Type",
  "1", "2", "3", "4", "5", "6", "7", "8", "9", "10",
  "11", "12", "13", "14", "15", "16", "17", "18", "19", "20",
  "21", "22", "23", "24", "25", "26", "27", "28", "29", "30", "31"
), header = FALSE)
tmax <- do.call(rbind, tmax_out)
tmax$NCEI_Code <- str_pad(tmax$NCEI_Code, width = 5, side = "left", pad = "0")

tmax <- mutate(tmax, State_Code = as.numeric(substr(NCEI_Code, 1, 2)))
tmax <- mutate(tmax, County_Code = as.numeric(substr(NCEI_Code, 3, 5)))

tmax$state <- substr(tmax$County_Name, 0, 2)
tmax$County_Name <- substr(tmax$County_Name, 5, 1000)

cty_fips <- read.csv(here("Data", "crosswalks", "county_2_fips", "state_and_county_fips_master.csv"))
cty_fips <- subset(cty_fips, !is.na(state))
cty_fips$name[cty_fips$name == "Do̱a Ana County"] <- "Dona Ana County"

tmax$County_Name[tmax$County_Name == "Autauga"] <- "Autauga County"

tmax_fips <- left_join(tmax, cty_fips, by = c("County_Name" = "name", "state"))

anti_join(tmax, cty_fips, by = c("County_Name" = "name")) %>%
  dplyr::select(County_Name) %>%
  unique()
anti_join(cty_fips, tmax, by = c("name" = "County_Name")) %>%
  dplyr::select(name) %>%
  unique()

tmax_fips$fips[tmax_fips$County_Name == "Oglala Lakota County"] <- 46102
tmax_fips$state[tmax_fips$County_Name == "Oglala Lakota County"] <- "ND"

tmax_fips$NCEI_Code <- as.numeric(tmax_fips$NCEI_Code)
with(tmax_fips, sum(NCEI_Code != fips, na.rm = TRUE))

tmax_fips <- tmax_fips %>% dplyr::select(Year, Month, X1:X31, fips)

saveRDS(tmax_fips, here("Data", "inter", "usa", "tmax.rds"))
message("Processed temperature data and saved to Data/inter/usa/tmax.rds")
