# Alexander F. Gazmararian
# agazmararian@gmail.com

library(here)
library(tidyverse)

fema <- read.csv(here("Data", "input", "usa", "fema", "DisasterDeclarationsSummaries.csv"))

fema <- fema %>% mutate(fipsCountyCode = str_pad(fipsCountyCode, 3, pad = "0"))
fema$fips <- with(fema, as.numeric(paste0(fipsStateCode, fipsCountyCode)))

fema$declarationDate <- as.Date(fema$declarationDate)
fema$incidentBeginDate <- as.Date(fema$incidentBeginDate)
fema$incidentEndDate <- as.Date(fema$incidentEndDate)

summary(fema$declarationDate)

# Fix two data errors where the incident begin and end date are entered incorrectly
fema[33405, ]$incidentBeginDate <- as.Date("1998-05-13")
fema[33405, ]$incidentEndDate <- as.Date("1998-05-20")
fema[33711, ]$incidentBeginDate <- as.Date("1998-06-18")
fema[33711, ]$incidentEndDate <- as.Date("1998-06-22")

# Create incident duration variable
fema$disaster_dur <- with(fema, as.numeric(incidentEndDate - incidentBeginDate))
summary(fema$disaster_dur)
# Note: the disaster that has lasted for 5117 days is the Kilauea Volcano from 1983-1997
# Note: some are coded as NA because they are ongoing---for those, will need to code length of time to the present day

fema$year <- year(fema$incidentBeginDate)
fema$month <- month(fema$incidentBeginDate)
fema$day <- day(fema$incidentBeginDate)

# Aggregate to county-level by disaster type based on year declared
fema_agg <- fema %>%
  filter(
    # Include the other specific days
    (year == 2010 & (month %in% 1:9) | (month == 10 & day %in% 1)) |
      (year == 2012 & (month %in% 1:9) | (month == 10 & day %in% 1:2)) |
      (year == 2014 & (month %in% 1:9) | (month == 10 & day %in% 1:3)) |
    !(year %in% c(2010, 2012, 2014))) %>%
  group_by(year, fips, incidentType) %>%
  summarise(
    n = n(),
    disaster_dur = mean(disaster_dur, na.rm = TRUE)
  )
summary(fema_agg)
# Note: The outlier for number of disasters (145) is a maine county where each of the townships declared a covid declaration of emergency
fema_wide <- fema_agg %>%
  rename(dur = disaster_dur) %>%
  pivot_wider(id_cols = c(year, fips), names_from = incidentType, values_from = c(n, dur), values_fill = 0)
names(fema_wide) <- tolower(gsub(" |\\/|\\(s\\)", "", names(fema_wide)))

# Create variable for climate related disasters
fema_wide$climate_disasters <- with(fema_wide, n_flood + n_drought + n_hurricane + n_fire + n_coastalstorm)
summary(fema_wide$climate_disasters)
fema_wide$climate_disasters_dur <- with(fema_wide, dur_flood + dur_drought + dur_hurricane + dur_fire + dur_coastalstorm)
summary(fema_wide$climate_disasters_dur)

# Create cumulative history of disasters in a county
fips <- read.csv(here("Data", "crosswalks", "county_2_fips", "state_and_county_fips_master.csv"))
fips <- subset(fips, fips != 0 & !grepl("000$", fips))
sum(fips$fips %in% fema_wide$fips) / nrow(fips)
# The disaster's data contains a complete listing of FIPS, so no need to merge
table(fema_wide$year)
# Expand out to create full data frame of county-year possibilities
completecountyyear <- fema_wide %>%
  dplyr::select(year, fips) %>%
  dplyr::ungroup() %>%
  tidyr::expand(year, fips)
dim(completecountyyear)
fema_all <- full_join(completecountyyear, fema_wide, by = c("year", "fips"))
fema_all <- fema_all %>% mutate(across(n_flood:climate_disasters_dur, ~ replace_na(.x, 0)))
# Create cumulative history measure
fema_all <- fema_all %>%
  arrange(year) %>%
  group_by(fips) %>%
  mutate(across(n_flood:n_typhoon, .fns = list("cuml" = ~ cumsum(.x)), .names = "{.col}_{.fn}"))

saveRDS(fema_all, here("Data", "inter", "usa", "fema_processed.rds"))
message("Processed FEMA data and saved to Data/inter/usa/fema_processed.rds")