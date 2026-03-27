# ==========================================
# Description: Create Table V
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
load("../data/library_data.Rdata")
# Load censored data
load("../data/censored_data/book_purchase_level_data.Rdata")






### ========================================
### Panel (a) ==============================
### ========================================
sample_books %<>% select(-c(citation)) %>% distinct()

checkouts %<>% left_join(sample_books, checkouts, by="bib_num")
checkouts$collection <- ifelse(is.na(checkouts$collection), 
                               "All Other Children's Books", as.character(checkouts$collection))

### Calculate Number of Checkouts by Collection
checkout_table <- checkouts %>%
  group_by(collection) %>%
  summarize(
    num_checkouts = n(),
    num_books = n_distinct(bib_num)
  ) %>% ungroup() %>%
  mutate(avg_checkouts_per_book = round(num_checkouts / num_books,0),
         num_checkouts = comma(num_checkouts),
         num_books = comma(num_books))


### Calculate Average Number of Copies in Inventory by Collection
inventory_table <- inventory %>%
  group_by(collection, bib_num) %>%
  summarize(num_library_copies = sum(item_count, na.rm=T)) %>%
  group_by(collection) %>%
  summarise(mean_copies = comma(mean(num_library_copies, na.rm=T),0.1))

panel_a <- full_join(checkout_table, inventory_table, by=c("collection"))
panel_a <- panel_a[c(7,4,3,8,2,5,1,6),
                   c("collection","num_checkouts","avg_checkouts_per_book","num_books","mean_copies")]
stargazer(panel_a, 
          type="text",
          out="../tables/table_V_b.txt",
          summary=F,
          rownames = F)





### ========================================
### Panel (b) ==============================
### ========================================
panel_b <- purchases %>% 
  mutate(book_ID = ifelse(is.na(book_ID), item_id, book_ID)) %>%
  group_by(collection) %>% 
  summarise(total_bought = sum(item_quantity), 
            total_unique = n_distinct(book_ID), 
            total_spent = sum(item_total)) %>%
  mutate(mean_num_sold = comma(total_bought/total_unique,1),
         total_unique = comma(total_unique,1),
         mean_book_price = dollar(total_spent/total_bought),
         total_spent = dollar(total_spent),
         total_bought = comma(total_bought, 1),
  ) %>% 
  select(c(collection, total_bought, mean_book_price, total_spent, total_unique), mean_num_sold) 

panel_b <- panel_b[c(7,4,3,8,2,5,1,6),]

stargazer(panel_b, 
          type="text",
          out="../tables/table_V_b.txt",
          summary=F)

