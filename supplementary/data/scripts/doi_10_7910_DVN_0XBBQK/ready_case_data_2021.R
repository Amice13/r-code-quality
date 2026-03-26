library(dplyr)
library(readr)

### Creates dataframe of case notification time series 
### Doesn't include "age unknown" cases due to relevance or "sex unknown" cases due to very limited data
### Includes only "new pulmonary smear positive" cases - unclear if relapses included - for years prior to 2013
  ### (variables available for "extrapulmonary" and "pulmonary smear negative" cases but bulk of data missing)

# import WHO case notification data
who_cases_2021 <- read_csv("TB_notifications_2020-12-31.csv")

# convert country names to UTF-8
who_cases_2021$country <- iconv(who_cases_2021$country, to = "UTF-8")

# extract dataframe with relevant variables
cases <- who_cases_2021 %>% 
  select(country, iso3, g_whoregion, year, 
         newrel_f04, newrel_f514, newrel_f1524, newrel_f2534, newrel_f3544, newrel_f4554, newrel_f5564, newrel_f65,
         newrel_m04, newrel_m514, newrel_m1524, newrel_m2534, newrel_m3544, newrel_m4554, newrel_m5564, newrel_m65,
         new_sp_f04, new_sp_f514, new_sp_f1524, new_sp_f2534, new_sp_f3544, new_sp_f4554, new_sp_f5564, new_sp_f65,
         new_sp_m04, new_sp_m514, new_sp_m1524, new_sp_m2534, new_sp_m3544, new_sp_m4554, new_sp_m5564, new_sp_m65
         )

# only keep data from years 2001 on 
cases <- subset(cases, year > 2000)

# calculate total counts of cases in each year for each country
cases <- cases %>%
  mutate(newrel_04_total = rowSums(cases[,c('newrel_f04', 'newrel_m04', 'new_sp_f04', 'new_sp_m04')], 
                                   na.rm=TRUE),
         newrel_514_total = rowSums(cases[,c('newrel_f514', 'newrel_m514', 'new_sp_f514', 'new_sp_m514')], 
                                    na.rm=TRUE),
         newrel_1524_total = rowSums(cases[,c('newrel_f1524', 'newrel_m1524', 'new_sp_f1524', 'new_sp_m1524')], 
                                     na.rm=TRUE),
         newrel_2534_total = rowSums(cases[,c('newrel_f2534', 'newrel_m2534', 'new_sp_f2534', 'new_sp_m2534')], 
                                     na.rm=TRUE),
         newrel_3544_total = rowSums(cases[,c('newrel_f3544', 'newrel_m3544', 'new_sp_f3544', 'new_sp_m3544')], 
                                     na.rm=TRUE),
         newrel_4554_total = rowSums(cases[,c('newrel_f4554', 'newrel_m4554', 'new_sp_f4554', 'new_sp_m4554')], 
                                     na.rm=TRUE),
         newrel_5564_total = rowSums(cases[,c('newrel_f5564', 'newrel_m5564', 'new_sp_f5564', 'new_sp_m5564')], 
                                     na.rm=TRUE),
         newrel_65plus_total = rowSums(cases[,c('newrel_f65', 'newrel_m65', 'new_sp_f65', 'new_sp_m65')], 
                                       na.rm=TRUE))

# create new dataframe with total case count variables 
case_totals <- cases %>%
  select(country, iso3, year, newrel_04_total, newrel_514_total, newrel_1524_total, newrel_2534_total, 
         newrel_3544_total, newrel_4554_total, newrel_5564_total, newrel_65plus_total)

# import UN population by age group data
un_pop_2021 <- read_csv("un_pop_2021.csv")

# fix country name
un_pop_2021[un_pop_2021=="United Kingdom"] = "United Kingdom of Great Britain and Northern Ireland"

# fix date-formatted column names
colnames(un_pop_2021)[4:5] <- c("5-9", "10-14")

# multiply population counts by 1000
un_pop_2021[, -c(1, 2)] <- un_pop_2021[, -c(1, 2)] * 1000

# merge case count and population data
case_totals_pop_2021 <- merge(case_totals, un_pop_2021, by=c("iso3", "year"), all.x = TRUE)

# add correct 5-14 population column
case_totals_pop_2021$'5-14' <- case_totals_pop_2021$`5-9` + case_totals_pop_2021$`10-14`

# save data file
saveRDS(case_totals_pop_2021, file="case_totals_pop_2021.rds")
