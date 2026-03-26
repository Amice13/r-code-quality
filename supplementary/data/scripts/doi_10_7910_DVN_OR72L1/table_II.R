# ==========================================
# Description: Create Table II
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

# Seen not heard variables
purchases$seen_heard_ratio <- purchases$pct_female_words / purchases$pct_female_faces
purchases$seen_heard_diff <- purchases$pct_female_faces - purchases$pct_female_words

# Some books belong in multiple collections so some purchases are duplicated 
# when merged with collection data. Ensure we have unique purchases of books
purchases %<>% 
  distinct_at(vars(trip_id, item_id), .keep_all = T)



### ========================================
### Make Panel (a) =========================
### ========================================
panel_a <- list()
panel_a$m1 <- lm(data=purchases, pct_female_words ~ has_daughter_expand + has_son_expand)
panel_a$m2 <- lm(data=purchases, pct_female_names ~ has_daughter_expand + has_son_expand)
panel_a$m3 <- lm(data=purchases, pct_female_faces ~ has_daughter_expand + has_son_expand)
panel_a$m4 <- lm(data=purchases, seen_heard_diff ~ has_daughter_expand + has_son_expand)
stargazer(panel_a,  
          type="text",
          dep.var.labels = c("Words","Names","Faces","Images vs. Text"),
          dep.var.caption = "Dependent Variable: Percent of Female",
          covariate.labels = c("Purchaser Has a Daughter","Purchaser Has a Son", "Constant (Baseline Group: No Children)"),
          out="../tables/table_II_a.txt",
          keep.stat = c("n", "adj.rsq"))


### ========================================  
### Make Panel (b) =========================
### ========================================
panel_b <- list()
panel_b$m1 <- lm(data=purchases, pct_female_words ~ gender_app_user)
panel_b$m2 <- lm(data=purchases, pct_female_names ~ gender_app_user)
panel_b$m3 <- lm(data=purchases, pct_female_faces ~ gender_app_user)
panel_b$m4 <- lm(data=purchases, seen_heard_diff ~ gender_app_user)
stargazer(panel_b,  
          type="text",
          dep.var.labels = c("Words","Names","Faces","Images vs. Text"),
          dep.var.caption = "Dependent Variable: Percent of Female",
          covariate.labels = c("Male", "Other", "Constant (Baseline Group: Female)"),
          out="../tables/table_II_b.txt", 
          keep.stat = c("n", "adj.rsq"))

