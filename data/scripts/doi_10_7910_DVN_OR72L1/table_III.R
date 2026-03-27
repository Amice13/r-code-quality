# ==========================================
# Description: Create Table III
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
load("../data/censored_data/book_purchase_level_data.Rdata")

# Some books belong in multiple collections so some purchases are duplicated 
# when merged with collection data. Ensure we have unique purchases of books
purchases %<>% distinct_at(vars(trip_id, item_id), .keep_all = T)


### ========================================
### Make Table =============================
### ========================================
table <- list()
table$m1 <- lm(data=purchases, mean_skin_tint ~ ethnicity)
table$m2 <- lm(data=purchases, pct_mentions_famous_asian ~ ethnicity)
table$m3 <- lm(data=purchases, pct_mentions_famous_black ~ ethnicity)
table$m4 <- lm(data=purchases, pct_mentions_famous_latinx ~ ethnicity)
table$m5 <- lm(data=purchases, pct_mentions_famous_white ~ ethnicity)
stargazer(table, 
          type="text",
          dep.var.labels = c("Average Skin Tint","Famous Asian",
                             "Famous Black","Famous Latinx", "Famous White"),
          dep.var.caption = "Dependent Variable:",
          covariate.labels = c("Asian", "Black", "Latinx", "Other", "Constant (Baseline Group: White)"),
          out="../tables/table_III.txt",
          keep.stat = c("n", "adj.rsq"))






