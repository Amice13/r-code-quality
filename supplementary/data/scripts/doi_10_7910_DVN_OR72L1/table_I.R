# ==========================================
# Description: Create Table I
# ==========================================

### ========================================
### Set Up =================================
### ========================================
# Set working directory to the directory to which this file is saved
library("rstudioapi")
setwd(dirname(getActiveDocumentContext()$path))

# Load Packages
source("requirements.R")

# Import Graph Themes
source("graph_themes.R")

### ========================================
### Data Prep ==============================
### ========================================
# Load data
load("../data/representation_data.Rdata")

### ========================================
### Custom Functions =======================
### ========================================
mean_pct <- function(x){percent(mean(x, na.rm = T))}


### ========================================
### Table ==================================
### ========================================

t <- book_level_data %>% group_by(collection) %>%
  summarize(
    # Collection Totals
    # num_books = n_distinct(book_ID),

    # Book Level Averages: Book Attributes
    avg_num_famous_people = comma(mean(unique_famous_people, na.rm = T)),
    avg_pct_monochromatic = mean_pct(pct_monochromatic),

    # Book Level Averages: Skin Color
    avg_skin_tint = comma(mean(mean_skin_tint, na.rm = T)),

    # Book Level Averages: Putative Race
    avg_pct_asian_faces = mean_pct(pct_asian_faces),
    avg_pct_black_faces = mean_pct(pct_black_faces),
    avg_pct_latinx_others_faces = mean_pct(pct_latinx_others_faces),
    avg_pct_white_faces = mean_pct(pct_white_faces),

    avg_pct_unique_famous_asian = mean_pct(pct_unique_famous_asian),
    avg_pct_unique_famous_black = mean_pct(pct_unique_famous_black),
    avg_pct_unique_famous_indigeneous = mean_pct(pct_unique_famous_indigeneous),
    avg_pct_unique_famous_latinx = mean_pct(pct_unique_famous_latinx),
    avg_pct_unique_famous_multiracial = mean_pct(pct_unique_famous_multiracial),
    avg_pct_unique_famous_white = mean_pct(pct_unique_famous_white),

    # Book Level Averages: Gender
    avg_pct_female_faces = mean_pct(pct_female_faces),
    avg_pct_female_words = mean_pct(pct_female_words),
    avg_pct_unique_famous_females = mean_pct(pct_unique_famous_females),
    
    # Book Level Averages: Age
    avg_pct_child_faces = mean_pct(pct_child_faces),
    avg_pct_young_gendered_terms = mean_pct(pct_young_gendered_terms)
  ) %>% t()


colnames(t) <- t[1,]
t <- t[-c(1),] %>% as.data.frame()

# Save Table
stargazer(t, 
          out="../tables/table_I.txt",
          type = "text",
          summary=F,
          dep.var.labels = names(t))
