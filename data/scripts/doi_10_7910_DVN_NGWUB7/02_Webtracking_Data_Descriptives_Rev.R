
#================
# POQ Revision
# Date: 9/12/24
#===============
# Checking for apps's data granularity
#===============

# clean
rm(list = ls())  

library(tidyverse)
library(lubridate)
library(ggplot2)
library(gridExtra)


#####################################################################

# ====================================================
# WITH FILTERED DATASET FOR THE FINAL SAMPLE --REPLICATE DESCRIPTIVES
# ====================================================
# FINAL INFORMATION INCLUDED IN THE PAPER
# =====================================================

# Call data wp3_long
wp3_long <- read.csv("Data/wp3_long.csv")

# Transform start_time as date 
wp3_long$date <- as.Date(wp3_long$start_time)

# Call fdata to get the final sample
load("Data/fdata")

# Subset panelists in the final dataset
fdata_id <- fdata %>%
  select (person_id) %>%
  mutate (id = 1)

wp3_long_id <- wp3_long %>%
  left_join (fdata_id, by = "person_id")

# Filtered data with panelists in the final dataset
wp3_long_filtered_fd <-  wp3_long_id %>%
  filter (id == 1) %>%
  filter (date < "2022-05-23" & page_duration < 1801 & page_duration > 4) 

# Sample by country 
sample_wp3_long_fil_fd <- wp3_long_filtered_fd %>%
  filter (date < "2022-05-23" & page_duration < 1801 & page_duration > 4) %>%
  group_by (iso2, person_id) %>%
  summarise(n()) %>% 
  group_by (iso2) %>%
  summarise(n())

# 1. Visits by country and type 
# webtracking visits by country
visits_by_country_fd <- wp3_long_filtered_fd %>%
  group_by (iso2) %>%
  summarise( n = n()) %>% 
  mutate (total = sum(n, na.rm = T),
          percent = n/total) 

# Number and % of political visits 
visits_pol_by_country_fd <- wp3_long_filtered_fd %>%
  filter (political_url == "TRUE") %>%
  group_by (iso2) %>%
  summarise( n = n()) %>% 
  mutate (total = sum(n, na.rm = T),
          percent = n/total) 

# Number and % of ukraine related visits
visits_ukraine_by_country_fd <- wp3_long_filtered_fd %>%
  filter (political_url_russia_ukraine == "TRUE") %>%
  group_by (iso2) %>%
  summarise( n = n()) %>% 
  mutate (total = sum(n, na.rm = T),
          percent = n/total) 


# Active users by type of visit and country 
active_users_by_country_fd <- wp3_long_filtered_fd %>%
  group_by (iso2, person_id) %>%
  summarise( n = n()) %>% 
  summarise (n = n ()) %>%
  mutate (total = sum(n, na.rm = T),
          percent = n/total)

# Active political users 
active_users_pol_by_country_fd <- wp3_long_filtered_fd %>%
  filter (political_url == "TRUE") %>%
  group_by (iso2, person_id) %>%
  summarise( n = n()) %>% 
  summarise (n = n ()) %>%
  mutate (total = sum(n, na.rm = T),
          percent = n/total)

# Active Ukraine users 
active_users_ukr_by_country_fd <- wp3_long_filtered_fd %>%
  filter (political_url_russia_ukraine == "TRUE") %>%
  group_by (iso2, person_id) %>%
  summarise( n = n()) %>% 
  summarise (n = n ()) %>%
  mutate (total = sum(n, na.rm = T),
          percent = n/total)

# ----------------------------------

# Join data 
visits_and_users_fil_fd <- visits_by_country %>%
  left_join (visits_pol_by_country_fd, by = "iso2") %>%
  left_join (visits_ukraine_by_country_fd, by = "iso2") %>%
  left_join (active_users_by_country_fd, by = "iso2") %>%
  left_join (active_users_pol_by_country_fd, by = "iso2") %>%
  left_join (active_users_ukr_by_country_fd, by = "iso2")


# Select columns containing the word root
selected_data <- visits_and_users_fil_fd %>%
  dplyr::select(iso2, starts_with (c("n", "percent"))) 

select_data <- 

write.csv(selected_data, "Data/webtracking_visits")

# ====================================================
# BREAK DOWN BY DEVICE 
# ====================================================

# ==========================================================
# Break down data by device
# ========================================================
# 1) Separate webevents from app visits

# Filter mobile data and create new variable for web events
wp3_long_mobile_fil_fd <- wp3_long_filtered_fd %>%
  filter (device == "Mobile") %>%
  mutate (website = ifelse(!(extension == path), 1,0)) 
  

# ----------
# 1) Visits Data
# ------------
# Total number of web event visits 
mobile_wevent_by_country_fd <- wp3_long_mobile_fil_fd %>%
  #filter (website == 1) %>%
  group_by (iso2) %>%
  summarise( n = n()) %>% 
  mutate (total = sum(n, na.rm = T),
          percent = n/total)

# Number of mobile political visits by country
mobile_pol_visits_by_country_fd <- wp3_long_mobile_fil_fd %>%
  filter (political_url == "TRUE") %>%
  group_by(iso2) %>%
  summarise(n = n()) %>%
  mutate (total = sum(n, na.rm = T),
          percent = n/total)

# Number of mobile ukraine visits by country
mobile_uk_visits_by_country_fd <- wp3_long_mobile_fil_fd %>%
  filter (political_url_russia_ukraine == "TRUE") %>%
  group_by(iso2) %>%
  summarise(n = n()) %>%
  mutate (total = sum(n, na.rm = T),
          percent = n/total)

# -----------------
# Users Data 
# ------------------

# 3) Active users by type of visit and country

# Active mobile users by country
mobile_by_id_fd <- wp3_long_mobile_fil_fd %>%
  #filter (website == 1) %>%
  group_by (iso2, person_id) %>%
  summarise(n = n()) %>%
  summarise(n = n()) %>%
  mutate (total = sum(n, na.rm = T),
          percent = n/total)

# Active mobile users with websites visits by country 
mobile_pol_by_id_fd <- wp3_long_mobile_fil_fd %>%
  filter (political_url == "TRUE") %>%
  group_by (iso2, person_id) %>%
  summarise(n = n()) %>%
  summarise(n = n()) %>%
  mutate (total = sum(n, na.rm = T),
          percent = n/total)

# Number of active mobile users in ukraine visits 
mobile_ukrvisits_by_id_fd <- wp3_long_mobile_fil_fd %>%
  filter (political_url_russia_ukraine == "TRUE") %>%
  group_by(iso2, person_id) %>%
  summarise(n = n()) %>%
  summarise(n = n()) %>%
  mutate (total = sum(n, na.rm = T),
          percent = n/total)


# Join data 
visits_and_users_mob_fil_fd <- mobile_wevent_by_country_fd %>%
  left_join (mobile_pol_visits_by_country_fd, by = "iso2") %>%
  left_join (mobile_uk_visits_by_country_fd, by = "iso2") %>%
  left_join (mobile_by_id_fd, by = "iso2") %>%
  left_join (mobile_pol_by_id_fd, by = "iso2") %>%
  left_join (mobile_ukrvisits_by_id_fd, by = "iso2")


# Select columns containing the word root
selected_data_mob <- visits_and_users_mob_fil_fd %>%
  dplyr::select(iso2, starts_with (c("n", "percent"))) 

write.csv(selected_data_mob, "Data/webtracking_mobile_visits")


#=================================================================
# The same with desktop
#=================================================================
# Filter desktop data 
wp3_long_desktop_fil_fd <- wp3_long_filtered_fd %>%
  filter (device == "Laptop/Desktop")

# --------
# 1) Visits Data 
# ---------

# Number of desktop visits by country
desktop_by_country_fd <- wp3_long_desktop_fil_fd %>%
  group_by (iso2) %>%
  summarize (n = n()) %>% 
  mutate (total = sum(n, na.rm = T),
          percent = n/total) 

# Number of desktop political visits by country 
desktop_pol_by_country_fd <- wp3_long_desktop_fil_fd %>%
  filter (political_url == "TRUE") %>%
  group_by (iso2) %>%
  summarize (n = n()) %>%
  mutate (total = sum(n, na.rm = T),
          percent = n/total) 

# Number of desktop Ukraine visits by country
desktop_ukraine_by_country_fd <- wp3_long_desktop_fil_fd %>%
  filter (political_url_russia_ukraine == "TRUE") %>%
  group_by (iso2) %>%
  summarize (n = n()) %>%
  mutate (total = sum(n, na.rm = T),
          percent = n/total) 

# -----------
# Users Data 
# ----------

# Active desktop users in Ukraine visits  
desktop_ukraine_by_id_fd <- wp3_long_desktop_fil_fd %>%
  filter (political_url_russia_ukraine == "TRUE") %>%
  group_by(iso2, person_id) %>%
  summarise(n = n()) %>%
  summarise(n = n()) %>%
  mutate (total = sum(n, na.rm = T),
          percent = n/total) 

# Active desktop users in political visits 
desktop_political_by_id_fd <- wp3_long_desktop_fil_fd %>%
  filter (political_url == "TRUE") %>%
  group_by(iso2, person_id) %>%
  summarise(n = n()) %>%
  summarise(n = n()) %>%
  mutate (total = sum(n, na.rm = T),
          percent = n/total) 

# Active desktop users 
desktop_by_id_fd <- wp3_long_desktop_fil_fd %>%
  group_by(iso2, person_id) %>%
  summarise(n = n()) %>%
  summarise(n = n()) %>%
  mutate (total = sum(n, na.rm = T),
          percent = n/total) 

# ----------------------------------
# Join data 
visits_and_users_desk_fil_fd <- desktop_by_country_fd %>%
  left_join (desktop_pol_by_country_fd, by = "iso2") %>%
  left_join (desktop_ukraine_by_country_fd, by = "iso2") %>%
  left_join (desktop_by_id_fd, by = "iso2") %>%
  left_join (desktop_political_by_id_fd, by = "iso2") %>%
  left_join (desktop_ukraine_by_id_fd, by = "iso2")


# Select columns containing the word root
selected_data_desk <- visits_and_users_desk_fil_fd %>%
  dplyr::select(iso2, starts_with (c("n", "percent"))) 

write.csv(selected_data_desk, "Data/webtracking_desk_visits")

