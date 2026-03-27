library(dplyr)
library(purrr)
library(readr)
library(readxl)
library(reshape2)

### Creates dataframe of TB incidence estimates by age group

# import WHO incidence estimate data
age_incidences <- read_csv("tb_incidences_2019_updated0102.csv")

# convert country names to UTF-8
age_incidences_2021$country <- iconv(age_incidences_2021$country, to = "UTF-8")

# fix date-formatted values
age_incidences_2021[age_incidences_2021 == "14-May"] <- "5-14"

# reshape data
age_incidences_new_2021 <- dcast(age_incidences_2021, country + iso3 + year + age_group ~ age_incidences_2021$sex)

# add total column
age_incidences_new_2021$total_estimate <- age_incidences_new_2021$f + age_incidences_new_2021$m

# import WHO burden estimate data
inc_burden_2021 <- read_csv("tb_burden_years_2019.csv")

# convert country names to UTF-8
inc_burden_2021$country <- iconv(inc_burden_2021$country, to = "UTF-8")

# remove observations for obsolete countries
inc_burden_2021 <- subset(inc_burden_2021, !(country %in% c("Netherlands Antilles", "Serbia & Montenegro")))

# reshape data
age_incidences_melt_2021 <- dcast(age_incidences_new_2021, iso3 + year ~ age_incidences_new_2021$age_group)

# insert country names
age_incidences_melt_2021_country <- unique(merge(age_incidences_melt_2021, inc_burden_2021[, 1:2], by="iso3", 
                                                 all.x = TRUE))

# import UN population by age group data
un_pop_2021 <- read_csv("un_pop_2021.csv")

# fix country name
un_pop_2021[un_pop_2021=="United Kingdom"] = "United Kingdom of Great Britain and Northern Ireland"

# fix date-formatted column names
colnames(un_pop_2021)[4:5] <- c("5-9", "10-14")

# multiply population counts by 1000
un_pop_2021[, -c(1, 2)] <- un_pop_2021[, -c(1, 2)] * 1000

# merge incidence counts and relevant age group columns of population data
merged_counts_2021 <- merge(age_incidences_melt_2021_country, un_pop_2021[, -c(16:23)], by=c("country", "year"), 
                               all.x = TRUE)

# convert incidence counts to rates
adult_incidence_rates_2021 <- merged_counts_2021 %>%
  rowwise() %>%
  mutate(`0-4` = `0-4.x` / `0-4.y`,
         `5-14` = `5-14` / (`5-9` + `10-14`),
         `15-24` = `15-24` / (`15-19` + `20-24`),
         `25-34` = `25-34` / (`25-29` + `30-34`),
         `35-44` = `35-44` / (`35-39` + `40-44`),
         `45-54` = `45-54` / (`45-49` + `50-54`),
         `55-64` = `55-64` / (`55-59` + `60-64`),
         `65plus` = `65plus.x` / `65plus.y`) %>%
  select(country, year, `0-4`, `5-14`, `15-24`, `25-34`, `35-44`, `45-54`, `55-64`, `65plus`)

# combine 2019 rates with previously calculated rates
adult_incidence_rates_1319 <- rbind(adult_incidence_rates_expanded, adult_incidence_rates_2021)

# keep countries with 7 years of estimated rates
adult_incidence_rates_final <- subset(adult_incidence_rates_1319, 
                                       country %in% adult_incidence_rates_expanded$country)

# order rows alphabetically by country
adult_incidence_rates_final <- adult_incidence_rates_final[order(adult_incidence_rates_final$country),]

# save data files
saveRDS(adult_incidence_rates_final, file="adult_incidence_rates_final.rds")
