# ==========================================
# Description: Create Table VI
# ==========================================

### ========================================
### Set Up =================================
### ========================================
# Set working directory to the directory to which this file is saved
library("rstudioapi")
setwd(dirname(getActiveDocumentContext()$path))

# Load Packages
source("requirements.R")


### ========================================
### Data Prep ==============================
### ========================================
# Load data
load("../data/cces_data.Rdata")
load("../data/census_data.Rdata")
load("../data/censored_data/book_purchase_level_data.Rdata")

# Subset to only Mainstream Diversity and Non-Award Winners
purchases %<>% subset(collection %in% c("Mainstream", "Diversity", "All Other Children's Books"))

# Some books won both a Mainstream and a Diversity Award, figure out which books this applies to
purchases %<>% group_by(item_id) %>% mutate(num_collections = n_distinct(collection))

# If a book won both a Mainstream and a Diversity award, change collection to "Both"
purchases$collection <- ifelse(purchases$num_collections == 2, "Both", purchases$collection)

# Drop duplicated purchases 
# (purchases of a book that was won by more than one collection are duplicated in this data)
purchases %<>% distinct_at(c("item_id","trip_id"), .keep_all=T)

# Count number of book purchases by collection at the zip code level
purchases_by_zip <- purchases %>% 
  group_by(postal_code, collection) %>%
  summarize(books = sum(item_quantity))

# Convert data from long to wide
purchases_by_zip %<>% spread(collection, books)

# If no purchases were observed in a zip code, replace NAs with 0
purchases_by_zip[is.na(purchases_by_zip)] <- 0



############################################### MERGE DATA
merged <- inner_join(cces, purchases_by_zip, by="postal_code") 

merged %<>% mutate(All_Books = (Diversity + Mainstream + Both + `All Other Children's Books`),
                   pct_diversity = ((Diversity + Both) / All_Books) * 100,
                   pct_mainstream = ((Mainstream + Both) / All_Books)*100)


###############################################
############################################### REGRESSIONS
###############################################

models <- list()
models$m1 <- lm(pct_deport ~ pct_diversity + pct_mainstream, data=merged)
models$m2 <- lm(pct_fund ~ pct_diversity + pct_mainstream, data=merged)
models$m3 <- lm(pct_advantage ~ pct_diversity + pct_mainstream, data=merged)
models$m4 <- lm(pct_angry ~ pct_diversity + pct_mainstream, data=merged)


stargazer(models, 
          type="text",
          dep.var.labels = c("% Deport Undocumented", "% Withhold Federal Funds", "% White People have Advantages", "% Angry about Racism"),
          out="../tables/table_VI.txt", 
          keep.stat = c("n", "adj.rsq"))

