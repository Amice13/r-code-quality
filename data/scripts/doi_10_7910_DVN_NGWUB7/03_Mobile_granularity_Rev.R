
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

# call data wp3_long
wp3_long <- read.csv("Data/wp3_long.csv")

# 1. Visits by country and type 
# webtracking visits by country
visits_by_country <- wp3_long %>%
  group_by (iso2) %>%
  summarise( n = n()) %>% 
  mutate (total = sum(n, na.rm = T),
          percent = n/total) 
  
# Number and % of political visits 
visits_pol_by_country <- wp3_long %>%
  filter (political_url == "TRUE") %>%
  group_by (iso2) %>%
  summarise( n = n()) %>% 
  mutate (total = sum(n, na.rm = T),
          percent = n/total) 

# Number and % of ukraine related visits
visits_ukraine_by_country <- wp3_long %>%
  filter (political_url_russia_ukraine == "TRUE") %>%
  group_by (iso2) %>%
  summarise( n = n()) %>% 
  mutate (total = sum(n, na.rm = T),
          percent = n/total) 


# Active users by type of visit and country 
active_users_by_country <- wp3_long %>%
  group_by (iso2, person_id) %>%
  summarise( n = n()) %>% 
  summarise (n = n ()) %>%
  mutate (total = sum(n, na.rm = T),
          percent = n/total)

# Active political users 
active_users_pol_by_country <- wp3_long %>%
  filter (political_url == "TRUE") %>%
  group_by (iso2, person_id) %>%
  summarise( n = n()) %>% 
  summarise (n = n ()) %>%
  mutate (total = sum(n, na.rm = T),
          percent = n/total)

# Active Ukraine users 
active_users_ukr_by_country <- wp3_long %>%
  filter (political_url_russia_ukraine == "TRUE") %>%
  group_by (iso2, person_id) %>%
  summarise( n = n()) %>% 
  summarise (n = n ()) %>%
  mutate (total = sum(n, na.rm = T),
          percent = n/total)



# ==========================================================
# Break down data by device
# ========================================================
# 1) Separate webevents from app visits

# Filter mobile data and create new variable for web events
wp3_long_mobile <- wp3_long %>%
  filter (device == "Mobile") %>%
  mutate (website = ifelse(!(extension == path), 1,0)) 

# ----------
# 1) Visits Data
# ------------
# Total number of web event visits 
mobile_wevent_by_country <- wp3_long_mobile %>%
  filter (website == 1) %>%
  group_by (iso2) %>%
  summarise( n = n()) %>% 
  mutate (total = sum(n, na.rm = T),
          percent = n/total)

# Number of mobile political visits by country
mobile_pol_visits_by_country <- wp3_long_mobile %>%
  filter (political_url == "TRUE") %>%
  group_by(iso2) %>%
  summarise(n = n()) %>%
  mutate (total = sum(n, na.rm = T),
          percent = n/total)
  
# Number of mobile ukraine visits by country
mobile_uk_visits_by_country <- wp3_long_mobile %>%
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
mobile_by_id <- wp3_long_mobile %>%
  filter (website == 1) %>%
  group_by (iso2, person_id) %>%
  summarise(n = n()) %>%
  summarise(n = n()) %>%
  mutate (total = sum(n, na.rm = T),
          percent = n/total)

# Active mobile users with websites visits by country 
mobile_pol_by_id <- wp3_long_mobile %>%
  filter (political_url == "TRUE") %>%
  group_by (iso2, person_id) %>%
  summarise(n = n()) %>%
  summarise(n = n()) %>%
  mutate (total = sum(n, na.rm = T),
          percent = n/total)

# Number of active mobile users in ukraine visits 
mobile_ukrvisits_by_id <- mobile_uk_visits %>%
  filter (political_url_russia_ukraine == "TRUE") %>%
  group_by(iso2, person_id) %>%
  summarise(n = n()) %>%
  summarise(n = n()) %>%
  mutate (total = sum(n, na.rm = T),
          percent = n/total)



#=================================================================
# The same with desktop
#=================================================================
# Filter desktop data 
wp3_long_desktop <- wp3_long %>%
  filter (device == "Laptop/Desktop")

# --------
# 1) Visits Data 
# ---------

# Number of desktop visits by country
desktop_by_country <- wp3_long_desktop %>%
  group_by (iso2) %>%
  summarize (n = n()) %>% 
  mutate (total = sum(n, na.rm = T),
          percent = n/total) 

# Number of desktop political visits by country 
desktop_pol_by_country <- desktop_pol %>%
  filter (political_url == "TRUE") %>%
  group_by (iso2) %>%
  summarize (n = n()) %>%
  mutate (total = sum(n, na.rm = T),
          percent = n/total) 

# Number of desktop Ukraine visits by country
desktop_ukraine_by_country <- desktop_ukraine %>%
  filter (political_url_russia_ukraine == "TRUE") %>%
  group_by (iso2) %>%
  summarize (n = n()) %>%
  mutate (total = sum(n, na.rm = T),
          percent = n/total) 

# -----------
# Users Data 
# ----------

# Active desktop users in Ukraine visits  
desktop_ukraine_by_id <- desktop_ukraine %>%
  group_by(iso2, person_id) %>%
  summarise(n = n()) %>%
  summarise(n = n()) %>%
  mutate (total = sum(n, na.rm = T),
          percent = n/total) 

# Active desktop users in political visits 
desktop_political_by_id <- desktop_pol %>%
  group_by(iso2, person_id) %>%
  summarise(n = n()) %>%
  summarise(n = n()) %>%
  mutate (total = sum(n, na.rm = T),
          percent = n/total) 

# Active desktop users 
desktop_by_id <- wp3_long_desktop %>%
  group_by(iso2, person_id) %>%
  summarise(n = n()) %>%
  summarise(n = n()) %>%
  mutate (total = sum(n, na.rm = T),
          percent = n/total) 
