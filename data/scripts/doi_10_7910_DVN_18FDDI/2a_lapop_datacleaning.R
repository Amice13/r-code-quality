################################################################################
#### Title: Progressive Ideology and Support for Punitive Crime Policy
#### Author: Isabel Laterzo
#### Year: 2023
#### Journal: Comparative Political Studies
#### Portion of Analysis: AmericasBarometer data cleaning
################################################################################

# Clean Environment

rm(list = ls())

#packages
library(foreign)
library(tidyverse)
library(dplyr)
library(haven)
library(labelled)

#set working directory

# uncomment below and set working directory using setwd() to directory which contains
# data files corresponding to 2014 AmericasBarometer rounds of Argentina and Brazil
#setwd()


#read in data (raw data from LAPOP)
b2014 <- read_dta("Brazil_LAPOP_2014.dta") %>%
  select(l1, vic1ext, vic2, a4, vb10, vb11, aoj22, vic1hogar, pole2n, pese1,
         pese2, aoj17, aoj12, b18, b32, n11, n15, infrax, ros4, q1, q2y, ed, etid,
         aoj11, q10new, ros1, w14a, d6)
b2014$country <- "Brazil"

#changing various NAs to NA
b2014[is.na(b2014)] <- NA

#removing all labels
b2014 <- remove_labels(b2014)
b2014 <- remove_attributes(b2014, "format.stata")

## Arggentina
a2014 <- read_dta("Argentina_LAPOP_2014.dta") %>%
  select(l1, vic1ext, vic2, a4, vb10, vb11, aoj22, vic1hogar, pole2n, pese1,
         pese2, aoj17, aoj12, b18, b32, n11, n15, infrax, ros4, q1, q2y, ed, etid,
         aoj11, q10new, ros1, w14a, d6)
a2014$country <- "Argentina"

#changing various NAs to NA
a2014[is.na(a2014)] <- NA

#removing all labels
a2014 <- remove_labels(a2014)
a2014 <- remove_attributes(a2014, "format.stata")

##### Combine data and recode values

combo_data <- rbind(a2014, b2014)

data <- combo_data %>%
  rename(left_right = l1,
         vic = vic1ext,
         problem = a4,
         partyid = vb11,
         party_yn = vb10, 
         comm_safe = aoj11,
         prv_vs_pun = aoj22,
         vic_house = vic1hogar,
         pol_satisf = pole2n,
         current_vio = pese1,
         year_vio = pese2, 
         neighb_gangs = aoj17,
         jud_punish = aoj12,
         trust_pol = b18,
         trust_locgov = b32,
         admin_safe = n11,
         admin_econ = n15,
         time_police = infrax,
         pol_ineq = ros4,
         birth_year = q2y,
         income = q10new,
         gov_priv = ros1,
         abort = w14a,
         gay_mar = d6
         ) %>% 
  mutate(party_yn = recode(party_yn, "1" = 1, "2" = 0),
         #1 = yes, 0 = no
         
         vic = recode(vic, "1" = 1, "2" = 0),
         #1 = yes, 0 = no
         
         vic_vio = recode(vic2, "1" = 0, "2" = 1, "3" = 1, "4" = 1, "5" = 1,
                          "6" = 1, "7" = 0, "8" = 0, "10" = 1, "11" = 0),
         # 0 = nonviolent crime (unarmed robbery w/ no assault, burglary w/ no
         # one home, vandalism), 1 = violent crime (unarmed robbery with assault, armed 
         # robbery, rape/sexual assault, kidnapping, extortion)
         
         vic_house = recode(vic_house, "1" = 1, "2" = 0),
         #1 = yes, 0 = no
         
         current_vio = recode(current_vio, "1" = 3, "2" = 2, "3" = 1),
         #3 = high, 1 = low
         
         year_vio = recode(year_vio, "1" = 3, "2" = 2, "3" = 1),
         #3 = high, 1 = low
         
         neighb_gangs = recode(neighb_gangs, "1" = 4, "2" = 3, "3" = 2, "4" = 1),
         #4 = high, 1 = low
         
         jud_punish = recode(jud_punish, "1" = 4, "2" = 3, "3" = 2, "4" = 1),
         
         female = recode(q1, "1" = 0, "2" = 1),
         
         abort = recode(abort, "1" = 1, "2" = 0)
         #1 = justified, 0 = not justified
         

         )

#create crime_problem variable where everyone who says crime related issue
# is number one problem == 1, else == 0

data$crime_prob <- ifelse(data$problem == 05 | #crime
                          data$problem == 11 | #drug addiction/consumption
                          data$problem == 12 | #drug trafficking
                          data$problem == 14 | #gangs
                          data$problem == 31 | #kidnappings
                          data$problem == 27 | #security (lack of)
                          data$problem == 57,  #violence
                          1,
                          0)

#uncomment below to save data
#write.csv(data, "lapop_2014_clean.csv")

