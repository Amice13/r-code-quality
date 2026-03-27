library(data.table)
library(dplyr)
library(latex2exp)
library(readr)
library(rgeos)
library(rworldmap)
library(zoo)


### Preparing data using 2020 contact matrices for running optimization with Stan

# load files 
case_totals_pop_2021 <- readRDS("case_totals_pop_2021.rds")
adult_incidence_rates_final <- readRDS("adult_incidence_rates_final.rds")
contacts_5to14_2020 <- readRDS("contacts_5to14_2020.rds")
contacts_under5_2020 <- readRDS("contacts_under5_2020.rds")
cdr_2021 <- read_csv("cdr_data_2019.csv")
regions <- readRDS("regions.rds")

# convert country names to UTF-8
cdr_2021$country <- iconv(cdr_2021$country, to = "UTF-8")

# load HIV data
hiv_data_under5_2021 <- read_csv("hiv_data_under5_2021.csv")
hiv_data_under5_2021$country <- iconv(as.character(hiv_data_under5_2021$country), to = "UTF-8")
hiv_data_514_2021 <- read_csv("hiv_data_514_2021.csv")
hiv_data_514_2021$country <- iconv(as.character(hiv_data_514_2021$country), to = "UTF-8")
hiv_data_514_2021 <- hiv_data_514_2021 %>% group_by(country, year) %>% summarise(hiv = mean(hiv)) %>% 
  mutate(age_group = "5-14")

# load PEM data
pem_data_under5_2021 <- read_csv("pem_data_under5_2021.csv")
pem_data_under5_2021$country <- iconv(as.character(pem_data_under5_2021$country), to = "UTF-8")
pem_data_514_2021 <- read_csv("pem_data_514_2021.csv")
pem_data_514_2021$country <- iconv(as.character(pem_data_514_2021$country), to = "UTF-8")
pem_data_514_2021 <- pem_data_514_2021 %>% group_by(country, year) %>% summarise(pem = mean(pem)) %>% 
  mutate(age_group = "5-14")

# load BCG data
bcg_data <- readRDS("bcg_data_melt.rds")
bcg_data$country <- iconv(as.character(bcg_data$country), to = "UTF-8")

# function to calculate dot product of adult incidence rates and contacts for given country, year, and age group
contact_inc_dot_2021 <- function(country, year, age_group){
  
  rates <- as.numeric(adult_incidence_rates_final[adult_incidence_rates_final$country==country 
                                            & adult_incidence_rates_final$year==year, 5:10])
  
  contact_region <- ifelse(country %in% rownames(contacts_04_2020), country, 
                           regions$g_whoregion[regions$country==country])
  
  if (age_group == "0-4") {
    
    contacts <- as.numeric(contacts_04_2020[contact_region, 3:8])
    
  } else {
    
    contacts <- as.numeric(contacts_514_2020[contact_region, 3:8])
    
  }
  
  return(sum(rates * contacts))
  
}

# function to calculate sum of contacts for given country and age group
contact_sum <- function(country, age_group){
  
  contact_region <- ifelse(country %in% rownames(contacts_04_2020), country, 
                           regions$g_whoregion[regions$country==country])
  
  if (age_group == "0-4") {
    
    contacts <- as.numeric(contacts_04_2020[contact_region, 3:8])
    
  } else {
    
    contacts <- as.numeric(contacts_514_2020[contact_region, 3:8])
    
  }
  
  return(sum(contacts))
  
}

# prepare analysis dataset for 0-4
map_countries_2021 <- intersect(adult_incidence_rates_final$country,hiv_data_under5_2021$country)
map_data1 <- case_totals_pop_2021[is.element(case_totals_pop_2021$country, map_countries_2021), 
                             c("country", "year", "0-4", "newrel_04_total")]
map_data1 <- subset(map_data1, year > 2012)
map_data1 <- map_data1 %>% mutate(age_group = "0-4")
colnames(map_data1)[colnames(map_data1) == '0-4'] = "pop"

# add p_tx_it variable based on CDR data
map_data1 <- inner_join(map_data1, cdr_2021[, c("country", "year", "cdr")], by = c("country", "year"))
map_data1$p_tx_it <- map_data1$cdr / 100
map_data1 <- map_data1[, -6]

# replace random NAs with average CDR for country
map_data1 <- map_data1 %>% 
  group_by(country) %>%
  mutate_at("p_tx_it", na.aggregate)

# add HIV column
map_data1 <- inner_join(map_data1, hiv_data_under5_2021[, c("country", "year", "hiv")], 
                        by = c("country", "year"))

# add PEM column
map_data1 <- inner_join(map_data1, pem_data_under5_2021[, c("country", "year", "pem")], 
                        by = c("country", "year"))

# add BCG column 
map_data1 <- left_join(map_data1, bcg_data[, c("country", "year", "bcg")], by = c("country", "year"))

# replace NAs with 0s
map_data1$bcg[is.na(map_data1$bcg)] <- 0

# add dot product of adult incidence rates and contacts
map_data1$contacts_inc <- mapply(contact_inc_dot_2021, map_data1$country, map_data1$year, map_data1$age_group)

# add sum of all contacts
map_data1$contact_sum <- mapply(contact_sum, map_data1$country, map_data1$age_group)

# prepare analysis dataset for 5-14
map_data2 <- case_totals_pop_2021[is.element(case_totals_pop_2021$country, map_countries_2021), 
                                  c("country", "year", "5-14", "newrel_514_total")]
map_data2 <- subset(map_data2, year > 2012)
map_data2 <- map_data2 %>% mutate(age_group = "5-14")
colnames(map_data2)[colnames(map_data2) == '5-14'] = "pop"

# add p_tx_it variable based on CDR data
map_data2 <- inner_join(map_data2, cdr_2021[, c("country", "year", "cdr")], by = c("country", "year"))
map_data2$p_tx_it <- map_data2$cdr / 100
map_data2 <- map_data2[, -6]

# replace random NAs with average CDR for country
map_data2 <- map_data2 %>% 
  group_by(country) %>%
  mutate_at("p_tx_it", na.aggregate)

# add HIV column
map_data2 <- inner_join(map_data2, hiv_data_514_2021[, c("country", "year", "hiv")], 
                        by = c("country", "year"))

# add PEM column
map_data2 <- inner_join(map_data2, pem_data_514_2021[, c("country", "year", "pem")], 
                        by = c("country", "year"))

# add BCG column 
map_data2 <- left_join(map_data2, bcg_data[, c("country", "year", "bcg")], by = c("country", "year"))

# replace NAs with 0s
map_data2$bcg[is.na(map_data2$bcg)] <- 0

# add dot product of adult incidence rates and contacts
map_data2$contacts_inc <- mapply(contact_inc_dot_2021, map_data2$country, map_data2$year, map_data2$age_group)

# add sum of all contacts
map_data2$contact_sum <- mapply(contact_sum, map_data2$country, map_data2$age_group)

# combine dataframes
map_data_2021 <- rbindlist(list(map_data1, map_data2), fill = TRUE)

# sort by country
map_data_2021 <- map_data_2021[order(map_data_2021$country),]

# aggregate WHO case counts in one column
map_data_2021$who_cases <- rowSums(map_data_2021[,c("newrel_04_total", "newrel_514_total")], na.rm=TRUE)

# get world map
wmap <- getMap(resolution="high")

# get centroids
centroids <- gCentroid(wmap, byid=TRUE)

# get data.frame with centroids
centroids_df <- as.data.frame(centroids)

# convert rownames to column
centroids_df$country <- rownames(centroids_df)

# fix country names 
centroids_df[26, 3] <- "Bahamas"
centroids_df[32, 3] <- "Bolivia (Plurinational State of)"
centroids_df[35, 3] <- "Brunei Darussalam"
centroids_df[50, 3] <- "Cabo Verde"
centroids_df[46, 3] <- "Congo"
centroids_df[57, 3] <- "Czechia"
centroids_df[43, 3] <- "Côte d'Ivoire"
centroids_df[178, 3] <- "Democratic People's Republic of Korea"
centroids_df[207, 3] <- "Eswatini"
centroids_df[84, 3] <- "Guinea-Bissau"
centroids_df[104, 3] <- "Iran (Islamic Republic of)"
centroids_df[123, 3] <- "Lao People's Democratic Republic"
centroids_df[75, 3] <- "Micronesia (Federated States of)"
centroids_df[143, 3] <- "North Macedonia"
centroids_df[120, 3] <- "Republic of Korea"
centroids_df[138, 3] <- "Republic of Moldova"
centroids_df[184, 3] <- "Russian Federation"
centroids_df[201, 3] <- "Serbia"
centroids_df[210, 3] <- "Syrian Arab Republic"
centroids_df[217, 3] <- "Timor-Leste"
centroids_df[78, 3] <- "United Kingdom of Great Britain and Northern Ireland"
centroids_df[231, 3] <- "Venezuela (Bolivarian Republic of)"
centroids_df[234, 3] <- "Viet Nam"

# function to insert latitude category
insert_lat_cat <- function(lat){
  
  if(abs(lat) < 20){
    return("under20")
  } else if (abs(lat) < 41) {
    return("20to40")
  } else {
    return("above40")
  }
  
}

# mark latitudes with latitude category
centroids_df$lat_cat <- mapply(insert_lat_cat, centroids_df$y)

# insert latitude category
map_data_2021 <- merge(map_data_2021, centroids_df[, 3:4], by="country", all.x=TRUE)

## SELECTING TRAINING COUNTRIES 

# select countries with highest CDR
cdr_agg_2021 <- aggregate(map_data_2021[, 6], list(map_data_2021$country), mean)
high_cdr_countries_2021 <- subset(cdr_agg_2021, p_tx_it > 0.84)

# import IHME mortality data
ihme_mort_1319 <- read_csv("ihme_mortality_1319.csv")

# convert country names to UTF-8
ihme_mort_1319$country <- iconv(ihme_mort_1319$country, to = "UTF-8")


# create column for WHO TB age ihme_mort_1319
ihme_mort_1319$age_group_who <- with(ihme_mort_1319, ifelse(age_group %in% c("1 to 4", "1minus"), "0-4", 
                                                  ifelse(age_group %in% c("5 to 9", "10 to 14"), "5-14",
                                                         ifelse(age_group %in% c("15 to 19", "20 to 24"), "15-24",
                                                                ifelse(age_group %in% 
                                                                         c("25 to 29", "30 to 34"), "25-34",
                                                                       ifelse(age_group %in% 
                                                                                c("35 to 39", "40 to 44"), "35-44",
                                                                              ifelse(age_group %in% 
                                                                                       c("45 to 49", "50 to 54"), 
                                                                                     "45-54",
                                                                                     ifelse(age_group %in% 
                                                                                              c("55 to 59", 
                                                                                                "60 to 64"), 
                                                                                            "55-64", "65plus"))))))))

# reaggregate deaths by WHo TB age groups
ihme_mort_1319_regrouped <- ihme_mort_1319 %>% group_by (country, year, age_group_who) %>% 
  summarize(mort = sum(mort)) 

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

# reshape notification data
case_totals_melt_2021 <- melt(case_totals, id.vars=c("country", "iso3", "year"), variable.name="age_total", 
                         value.name="notifs")

# cast age group column to character
case_totals_melt_2021$age_total <- as.character(case_totals_melt_2021$age_total)

# fix age group names
case_totals_melt_2021[case_totals_melt_2021=="newrel_04_total"] <- "0-4"
case_totals_melt_2021[case_totals_melt_2021=="newrel_514_total"] <- "5-14"
case_totals_melt_2021[case_totals_melt_2021=="newrel_1524_total"] <- "15-24"
case_totals_melt_2021[case_totals_melt_2021=="newrel_2534_total"] <- "25-34"
case_totals_melt_2021[case_totals_melt_2021=="newrel_3544_total"] <- "35-44"
case_totals_melt_2021[case_totals_melt_2021=="newrel_4554_total"] <- "45-54"
case_totals_melt_2021[case_totals_melt_2021=="newrel_5564_total"] <- "55-64"
case_totals_melt_2021[case_totals_melt_2021=="newrel_65plus_total"] <- "65plus"

# join mortality and notification data
mort_notifs_1319 <- merge(x=ihme_mort_1319_regrouped, y=case_totals_melt_2021, 
                          by.x=c("country", "year", "age_group_who"), by.y=c("country", "year", "age_total"))

# ensure non-zero notifications
mort_notifs_1319$notifs <- mort_notifs_1319$notifs + 0.0000001

# calculate mortality to notification ratio
mort_notifs_1319$ratio <- mort_notifs_1319$mort / mort_notifs_1319$notifs

# declare vector of population weights (WHO standard)
pop_weights <- c(0.0886, 0.0869 + 0.086, 0.0847 + 0.0822, 0.0793 + 0.0761, 0.0715 + 0.0659, 0.0604 + 0.0537, 
                 0.0455 + 0.0372, 0.0296 + 0.0221 + 0.0152 + 0.0091 + 0.0044 + 0.0015 + 0.0004 + 0.00005)

# reorder population weights vector based on dataframe age group order
pop_weights_reorder <- pop_weights[order(c(1, 6, 2, 3, 4, 5, 7, 8))]

# insert population weights into dataframe
mort_notifs_1319$pop_weights <- rep(pop_weights_reorder, length(mort_notifs_1319$country) / length(pop_weights))

# import WHO incidence estimate data
age_incidences_2021 <- read_csv("tb_incidences_2019_updated0102.csv")

# convert country names to UTF-8
age_incidences_2021$country <- iconv(age_incidences_2021$country, to = "UTF-8")

# fix date-formatted values
age_incidences_2021[age_incidences_2021 == "14-May"] <- "5-14"

# reshape data
age_incidences_new_2021 <- dcast(age_incidences_2021, country + iso3 + year + age_group ~ age_incidences_2021$sex)

# add total column
age_incidences_new_2021$total_estimate <- age_incidences_new_2021$f + age_incidences_new_2021$m

# sum 2018 incidence counts by age group
inc_totals_2021 <-  age_incidences_new_2021 %>% 
  group_by(age_group) %>% 
  summarize(inc_total = sum(total_estimate))

# calculate incidence weights
inc_totals_2021$weights <- inc_totals_2021$inc_total / sum(inc_totals_2021$inc_total)

# insert incidence weights into dataframe
mort_notifs_1319$inc_weights <- rep(inc_totals_2021$weights, 
                                    length(mort_notifs_1319$country) / length(inc_totals_2021$weights))

# calculate adjusted ratios
mort_notifs_1319$ratio_pop <- mort_notifs_1319$ratio * mort_notifs_1319$pop_weights # adjust by population weight
mort_notifs_1319$ratio_inc <- mort_notifs_1319$ratio * mort_notifs_1319$inc_weights # adjust by incidence weight

# calculate weighted ratio for each country for each year
ratio_pop_years_1319 <- mort_notifs_1319 %>% group_by(country, year) %>%
  summarize(avg_ratio_pop = sum(ratio_pop)) # weighted average based on population
ratio_inc_years_1319 <- mort_notifs_1319 %>% group_by(country, year) %>%
  summarize(avg_ratio_inc = sum(ratio_inc)) # weighted average based on incidence

# calculate mean and standard deviation of weighted ratio for each country
ratio_pop_1319 <- ratio_pop_years_1319 %>% group_by(country) %>%
  summarize(ratio_pop_mean = mean(avg_ratio_pop), ratio_pop_sd = sd(avg_ratio_pop)) # population-based
ratio_inc_1319 <- ratio_inc_years_1319 %>% group_by(country) %>%
  summarize(ratio_inc_mean = mean(avg_ratio_inc), ratio_inc_sd = sd(avg_ratio_inc)) # incidence-based

# join dataframes of mean weighted ratios
ratio_means_1319 <- merge(ratio_pop_1319, ratio_inc_1319, by="country")

# extract mean weighted ratios for countries in analysis
ratio_means_1319 <- subset(ratio_means_1319, country %in% map_countries_2021)

# select countries with lowest ratio, as calculated with population weights
low_ratio_pop_countries_2021 <- ratio_means_1319 %>% top_n(wt = ratio_pop_mean, n = -37)

# select countries with lowest ratio, as calculated with incidence weights
low_ratio_inc_countries_2021 <- ratio_means_1319 %>% top_n(wt = ratio_inc_mean, n = -37)

# extract data for countries with lowest ratio
train_data_04_2021 <- map_data_2021[(map_data_2021$country %in% low_ratio_inc_countries_2021$country) 
                                    & (map_data_2021$age_group=="0-4"), ]
train_data_514_2021 <- map_data_2021[(map_data_2021$country %in% low_ratio_inc_countries_2021$country 
                                      & (map_data_2021$age_group=="5-14")), ]

# keep data for countries with lowest ratio also having high CDR
train_data_04_2021 <- subset(train_data_04_2021, country %in% high_cdr_countries_2021[, 1])
train_data_514_2021 <- subset(train_data_514_2021, country %in% high_cdr_countries_2021[, 1])

# order data by latitude category
train_data_04_2021$lat_cat <- factor(train_data_04_2021$lat_cat, levels=c("above40", "20to40", "under20"))
train_data_04_2021 <- train_data_04_2021[order(train_data_04_2021$lat_cat),]
train_data_514_2021$lat_cat <- factor(train_data_514_2021$lat_cat, levels=c("above40", "20to40", "under20"))
train_data_514_2021 <- train_data_514_2021[order(train_data_514_2021$lat_cat),]

# fix row numbers
rownames(train_data_04_2021) <- seq(length=nrow(train_data_04_2021))
rownames(train_data_514_2021) <- seq(length=nrow(train_data_514_2021))

# extract relevant columns 
train_data_04_2021 <- train_data_04_2021[, 1:14]
train_data_514_2021 <- train_data_514_2021[, 1:14]
map_data_2021 <- map_data_2021[, 1:14]

# save data files
saveRDS(train_data_04_2021, file="train_data_04_2021.rds")
saveRDS(train_data_514_2021, file="train_data_514_2021.rds")
saveRDS(map_data_2021, file="map_data_2021.rds")
