library(dplyr)
library(purrr)
library(readr)
library(reshape2)

### Prepares data from contact matrices generated in 2020

## LOADING CONTACT MATRICES

# load files
contacts_all_2020 <- read_csv("contacts_all_2020.csv")
regions <- readRDS("regions.rds")

# convert country names to UTF-8
contacts_all_2020$country <- iconv(contacts_all_2020$country, to = "UTF-8")
regions$country <- iconv(regions$country, to = "UTF-8")

# subset pediatric contacts
contacts_04_2020 <- subset(contacts_all_2020, age_contactor=="0 to 4")
contacts_59_2020 <- subset(contacts_all_2020, age_contactor=="5 to 9")
contacts_1014_2020 <- subset(contacts_all_2020, age_contactor=="10 to 14")

# reshape data
contacts_04_melt <- dcast(contacts_04_2020[, c(1, 2, 4, 5)], country + iso3  ~ contacts_04_2020$age_contactee)
contacts_59_melt <- dcast(contacts_59_2020[, c(1, 2, 4, 5)], country + iso3  ~ contacts_59_2020$age_contactee)
contacts_1014_melt <- dcast(contacts_1014_2020[, c(1, 2, 4, 5)], country + iso3  ~ contacts_1014_2020$age_contactee)

# regroup columns
contacts_04_melt <- contacts_04_melt %>%
  mutate(`0-4` = `0 to 4`, `5-14` = `5 to 9` + `10 to 14`, `15-24` = `15 to 19` + `20 to 24`, 
         `25-34` = `25 to 29` + `30 to 34`, `35-44` = `35 to 39` + `40 to 44`, `45-54` = `45 to 49` + `50 to 54`,
         `55-64` = `55 to 59` + `60 to 64`, `65plus` = `65 to 69` + `70 to 74` + `75+`)
contacts_59_melt <- contacts_59_melt %>%
  mutate(`0-4` = `0 to 4`, `5-14` = `5 to 9` + `10 to 14`, `15-24` = `15 to 19` + `20 to 24`, 
         `25-34` = `25 to 29` + `30 to 34`, `35-44` = `35 to 39` + `40 to 44`, `45-54` = `45 to 49` + `50 to 54`,
         `55-64` = `55 to 59` + `60 to 64`, `65plus` = `65 to 69` + `70 to 74` + `75+`)
contacts_1014_melt <- contacts_1014_melt %>%
  mutate(`0-4` = `0 to 4`, `5-14` = `5 to 9` + `10 to 14`, `15-24` = `15 to 19` + `20 to 24`, 
         `25-34` = `25 to 29` + `30 to 34`, `35-44` = `35 to 39` + `40 to 44`, `45-54` = `45 to 49` + `50 to 54`,
         `55-64` = `55 to 59` + `60 to 64`, `65plus` = `65 to 69` + `70 to 74` + `75+`)

# select necessary columns
contacts_04_melt <- contacts_04_melt[, -(3:18)]
contacts_59_melt <- contacts_59_melt[, -(3:18)]
contacts_1014_melt <- contacts_1014_melt[, -(3:18)]

# compute averages to get contacts for children aged 5-14 
contacts_514_melt <- cbind(contacts_59_melt[, 1:2], (contacts_59_melt[, 3:10]+contacts_1014_melt[, 3:10])/2)

## ADDING AVERAGE CONTACTS BY REGION

# merge contact matrix countries with region data
regions_contacts <- merge(contacts_04_melt[, 1:2], regions, by="iso3", all.x=TRUE)

# convert country names to rownames and remove iso3
contacts_04_2020 <- contacts_04_melt[, -(1:2)]
rownames(contacts_04_2020) <- contacts_04_melt$country
contacts_514_2020 <- contacts_514_melt[, -(1:2)]
rownames(contacts_514_2020) <- contacts_514_melt$country

# count number of countries in each region
region_counts <- regions_contacts %>% count(g_whoregion)

# approximate contact matrices by region
for (region in region_counts$g_whoregion){
  
  countries_in_region <- as.character(regions_contacts$country.x[regions_contacts$g_whoregion==region])
  
  contacts_sum_04_2020 <- colSums(contacts_04_2020 * (rownames(contacts_04_2020) %in% countries_in_region))
  contacts_avg_04_2020 <- contacts_sum_04_2020/region_counts$n[region_counts$g_whoregion==region]
  contacts_avg_04_2020 <- data.frame(t(contacts_avg_04_2020))
  rownames(contacts_avg_04_2020)[1] <- region
  colnames(contacts_avg_04_2020) <- colnames(contacts_04_2020)
  
  contacts_sum_514_2020 <- colSums(contacts_514_2020 * (rownames(contacts_514_2020) %in% countries_in_region))
  contacts_avg_514_2020 <- contacts_sum_514_2020/region_counts$n[region_counts$g_whoregion==region]
  contacts_avg_514_2020 <- data.frame(t(contacts_avg_514_2020))
  rownames(contacts_avg_514_2020)[1] <- region
  colnames(contacts_avg_514_2020) <- colnames(contacts_514_2020)
  
  contacts_04_2020 <- rbind(contacts_04_2020, contacts_avg_04_2020)
  contacts_514_2020 <- rbind(contacts_514_2020, contacts_avg_514_2020)
  
}

# save data files
saveRDS(contacts_04_2020, file="contacts_04_2020.rds")
saveRDS(contacts_514_2020, file="contacts_514_2020.rds")
