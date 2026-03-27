library(dplyr)
library(readr)

### Preparing datasets for revision analysis

# load original paper data files
train_data_04_revision <- train_data_04_2021
train_data_514_revision <- train_data_514_2021
map_data_revision <- map_data_2021

## INSERTING ART COVERAGE INTO DATASET FOR ANALYSIS

# load ART data
art_peds <- read_csv("art_peds_modified.csv")
art_peds$Country <- iconv(as.character(art_peds$Country), to = "UTF-8")

# reshape ART data
art_peds_melt <- art_peds %>% gather(year, art_coverage, -Country)

# divide coverage by 100 to get percentages
art_peds_melt$art_coverage <- as.numeric(art_peds_melt$art_coverage) / 100

# insert ART coverage into datasets  
train_data_04_revision <- merge(train_data_04_revision, art_peds_melt, by.x=c("country", "year"),
                                by.y=c("Country", "year"))
train_data_514_revision <- merge(train_data_514_revision, art_peds_melt, by.x=c("country", "year"),
                                 by.y=c("Country", "year"))
map_data_revision <- merge(map_data_revision, art_peds_melt, by.x=c("country", "year"), by.y=c("Country", "year"))

# order data by latitude category
train_data_04_revision$lat_cat <- factor(train_data_04_revision$lat_cat, levels=c("above40", "20to40", "under20"))
train_data_04_revision <- train_data_04_revision[order(train_data_04_revision$lat_cat),]
train_data_514_revision$lat_cat <- factor(train_data_514_revision$lat_cat, levels=c("above40", "20to40", "under20"))
train_data_514_revision <- train_data_514_revision[order(train_data_514_revision$lat_cat),]

# fix row numbers
rownames(train_data_04_revision) <- seq(length=nrow(train_data_04_revision))
rownames(train_data_514_revision) <- seq(length=nrow(train_data_514_revision))

# save data files
saveRDS(train_data_04_revision, file="train_data_04_revision.rds")
saveRDS(train_data_514_revision, file="train_data_514_revision.rds")
saveRDS(map_data_revision, file="map_data_revision.rds")
