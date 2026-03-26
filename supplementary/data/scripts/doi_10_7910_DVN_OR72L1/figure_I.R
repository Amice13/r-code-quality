# ==========================================
# Description: Create Table IV
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
### Library Data
load("../data/library_data.Rdata")
load("../data/search_interest_data.Rdata")
load("../data/censored_data/book_purchase_data.Rdata")

# Focus on Mainstream/Diversity
sample_books %<>% subset(collection %in% c("Mainstream","Diversity"))

# Only include checkouts of books published after 2004 (for the event study)
checkouts %<>% subset(publication_year >= 2004)

# Create Checkout Date Variable
checkouts$checkout_date <- word(checkouts$checkout_date_time, 1) %>% as.Date(format="%m/%d/%Y")

# Merge info on collections
checkouts %<>% left_join(sample_books, by="bib_num")
checkouts$collection <- ifelse(is.na(checkouts$collection), 
                               "All Other Children's Books", as.character(checkouts$collection))

# Create Collection Citation Variable
checkouts %<>% mutate(
  collection_citation = case_when(
    collection == "All Other Children's Books" ~ "All Other Children's Books",
    collection == "Mainstream" & citation == "Winner" ~ "Mainstream Award Winners",
    collection == "Mainstream" & citation == "Honor" ~ "Mainstream Award Honors",
    collection == "Diversity" & citation == "Winner" ~ "Diversity Award Winners",
    collection == "Diversity" & citation == "Honor" ~ "Diversity Award Honors"),
  collection_citation = factor(collection_citation,
                               levels=c("Mainstream Award Winners","Mainstream Award Honors",
                                        "Diversity Award Winners", "Diversity Award Honors",
                                        "All Other Children's Books"))) 


# If a book did not win an award, impute the year they were eligible for the award
checkouts$award_year <- ifelse(is.na(checkouts$year_meta), 
                               checkouts$publication_year + 1,
                               checkouts$year_meta)

# Merge in award date (awards are announced on a different date each year)
checkouts %<>% left_join(award_dates, by="award_year")
checkouts$award_date %<>% as.Date(format="%m/%d/%Y")

# Create Variable indicating number of days since award at time of checkout
checkouts$event_time <- factor(checkouts$checkout_date - checkouts$award_date, levels = c(-400:730))

# Subset Data
checkouts %<>% subset(!is.na(event_time)) 
  
# Find the total number of books in each collection 
# (conditional on the sample of books that were checked out at least once in our checkout data)
sample_size <- checkouts %>% 
  group_by(collection_citation, .drop=F) %>% 
  summarize(sample_size = as.numeric(n_distinct(bib_num)))

# Collapse data to checkouts by collection and days since award
checkouts %<>%
  group_by(collection_citation, event_time, .drop=F) %>%
  summarize(num_checkouts = n()) %>%
  mutate(event_time = as.numeric(as.character(event_time)),
         moving_average = rollmean(num_checkouts, 14, na.pad = T))

# Weight checkouts by number of books in each collection
checkouts %<>% left_join(sample_size, by="collection_citation")
checkouts %<>% mutate(num_checkouts_scaled = (num_checkouts / sample_size),
                                   moving_average_scaled = (moving_average / sample_size)) %>%
  subset(!is.na(moving_average_scaled))



### Search Interest Data
google_trends$award_date <- 
  as.Date(paste(google_trends$year, "01", "20", sep = "-"), "%Y-%m-%d")
google_trends %<>%
  mutate(award_date = as.Date(award_date, format="%m/%d/%Y"),
         event_time = date - award_date,
         event_time = ifelse(event_time >= 247, event_time - 365, event_time))

google_trends %<>% 
  group_by(collection, event_time, .drop=F) %>%
  summarize(mean_interest = mean(interest, na.rm=T)) 




### ========================================
### Graph Panel (a) ========================
### ========================================
p <- checkouts  %>%
  ggplot(aes(y=moving_average_scaled, x=event_time, 
             color = collection_citation,linetype = collection_citation)) + 
  
  # Reference Line at Event Time = 0
  geom_vline(xintercept = 0, color="black") +
  
  # Plot smoothed checkout data
  geom_line(aes(size=collection_citation)) +
  
  # Color and size by collection x citation
  scale_size_manual(values=c(0.7,0.4,0.5,0.4,0.4)) +
  scale_linetype_manual(values=c(1,2,1,2,1)) +
  
  # Axes Titles
  ylab("Daily Checkouts (Scaled)") + 
  xlab("Days Since Award Annoucement") + 
  
  # Arrange Lengend
  guides(color=guide_legend(ncol=1,byrow=F)) +
  theme(legend.position = "right") +
  
  # Axis Labels
  scale_x_continuous(breaks = c(-365, -180, 0, 180, 365, 545, 730))

# Color
p + scale_color_manual(values=c("#CC79A7","#CC79A7","#0072B2","#0072B2","#E69F00"))
ggsave("../figures/figure_I_a.png", height = 3, width=9, units="in")

# Greyscale
p + scale_color_manual(values=c("gray0","gray0","gray70","gray70","gray40")) 
ggsave("../figures/grayscale_figure_I_a.png", height = 3, width=9, units="in")




### ========================================
### Graph Panel (b) ========================
### ========================================
p <- collection_event_time %>%
  ggplot(aes(
    y = moving_average_quantity_purchased_scaled, 
    x = event_time,
    color = collection_citation, 
    linetype = collection_citation,
    size = collection_citation
  )) +
  
  # Reference line at event time = 0
  geom_vline(xintercept = 0, color = "black") +
  
  # Plot Smoothed Book Purchases
  geom_line() +
  
  # Axes titles
  ylab("Quantity Purchased (Scaled)") +
  xlab("Days Since Award Announcement") +
  
  # Linetype by citation
  scale_linetype_manual(values =  c(1, 2, 1, 2, 1)) +
  scale_size_manual(values=c(0.7,0.4,0.5,0.4,0.4)) +
  
  # Axis labels
  scale_x_continuous(breaks = c(-365, -180, 0, 180, 365, 545, 730)) +
  scale_y_continuous(breaks = scales::pretty_breaks()) +
  
  # Arrange legend
  guides(color = guide_legend(ncol = 1, byrow = FALSE)) + 
  theme(legend.position = "right")


# Color
p + scale_color_manual(values=c("#CC79A7","#CC79A7","#0072B2","#0072B2","#E69F00"))
ggsave("../figures/figure_I_b.png", height = 3, width=9, units="in")

# Greyscale
p + scale_color_manual(values=c("gray0","gray0","gray70","gray70","gray40")) 
ggsave("../figures/grayscale_figure_I_b.png", height = 3, width=9, units="in")





### ========================================
### Graph Panel (c) ========================
### ========================================

p <- google_trends %>% 
  ggplot(aes(y = mean_interest, x = event_time, color = collection)) +
  
  # Reference Line at Event Time = 0
  geom_vline(xintercept = 0) + 
  
  # Plot Search Interest
  geom_line(size = 0.6) +
  
  # Axes titles
  ylab("Search Interest") +
  xlab("Days since January 20th of Award Year") +
  
  # Axis Labels
  scale_x_continuous(breaks = c(-120,-60, 0, 60, 120, 180, 245)) +
  
  # Arrange legend
  theme(legend.position = "right")


# Color
p + scale_color_manual(values=c("#CC79A7","#0072B2"))
ggsave("../figures/figure_I_c.png", height = 3, width=9, units="in")

# Greyscale
p + scale_color_manual(values=c("gray0","gray70")) 
ggsave("../figures/grayscale_figure_I_c.png", height = 3, width=9, units="in")

