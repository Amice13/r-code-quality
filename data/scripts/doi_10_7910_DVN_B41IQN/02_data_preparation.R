### ------------------------------------------------------------
# Thesis Survey Experiment
# 02 Data Preparation
# Friedrich Püttmann
# October 2023
### ------------------------------------------------------------

## load packages -----------------------------------------------

library(tidyverse)
library(magrittr)

## adapt settings ----------------------------------------------

options("max.print" = 25)

## load dataset ------------------------------------------------

data_exp <- read_rds("02_data/02_data_raw.rds")

## check and prepare variables ---------------------------------

# Soru 0 (Treatment)

  # show class (type of variable)
class(data_exp$Soru0)
  # count missing values
sum(is.na(data_exp$Soru0))
  # show unique values
unique(data_exp$Soru0)
  # show table
table(data_exp$Soru0)

  # recoding treatment variable
data_exp <- data_exp |>
  rename(treat = Soru0) |>
  mutate(treat = case_when(
    treat == 2 ~ 0,
    TRUE ~ treat
    )
  ) |>
  mutate(treat = factor(
    treat, levels = c(0, 1), labels = c("no", "yes")
  ))

table(data_exp$treat)

# Soru 1 (Gender)

class(data_exp$Soru1)
sum(is.na(data_exp$Soru1))
unique(data_exp$Soru1)
table(data_exp$Soru1)

  # recoding 

table(data_exp$Soru1)

data_exp <- data_exp |>
  rename(gender = Soru1) |>
  mutate(gender = case_when(
    gender == 1 ~ 1,
    gender == 2 ~ 0,
    gender == 99 ~ NA,
    TRUE ~ gender
  )) |>
  mutate(gender = factor(
    gender, levels = c(0, 1), labels = c("male", "female")
  ))

table(data_exp$gender)

# Soru 2 (Age)

class(data_exp$Soru2)
sum(is.na(data_exp$Soru2))
unique(data_exp$Soru2)
summary(data_exp$Soru2)

data_exp <- data_exp |>
  rename(age = Soru2)

summary(data_exp$age)

# Soru 3 (Education)

class(data_exp$Soru3)
sum(is.na(data_exp$Soru3))
unique(data_exp$Soru3)
summary(data_exp$Soru3)

data_exp <- data_exp |>
  rename(edu = Soru3)
summary(data_exp$edu)

  # recoding 99 as missing values 

data_exp <- data_exp |>
  mutate(edu = case_when(
    edu == 99 ~ NA,
    TRUE ~ edu
  ))

table(data_exp$edu)

# Soru 301 (Education grouped)

class(data_exp$Soru301)
sum(is.na(data_exp$Soru301))
unique(data_exp$Soru301)
summary(data_exp$Soru301)

data_exp <- data_exp |>
  rename(edu_group = Soru301)
summary(data_exp$edu_group)

  # recoding 99 as missing values 

data_exp <- data_exp |>
  mutate(edu_group = case_when(
    edu_group == 99 ~ NA,
    TRUE ~ edu_group
  ))

table(data_exp$edu_group)

# Soru 4 (Lifestyle)

class(data_exp$Soru4)
sum(is.na(data_exp$Soru4))
unique(data_exp$Soru4)

  # recoding 

table(data_exp$Soru4)

data_exp <- data_exp |>
  rename(lifestyle = Soru4) |>
  mutate(lifestyle = factor(
    lifestyle, levels = c(1, 2, 3), labels = c("modern",
      "traditional conservative", "religious conservative")
  ))

table(data_exp$lifestyle)


# Soru 5 (Job)

class(data_exp$Soru5)
sum(is.na(data_exp$Soru5))
unique(data_exp$Soru5)
summary(data_exp$Soru5)

data_exp <- data_exp |>
  rename(job = Soru5)
summary(data_exp$job)

  # recoding 99 as missing values 

data_exp <- data_exp |>
  mutate(job = case_when(
    job == 99 ~ NA,
    TRUE ~ job
  ))

table(data_exp$job)

# Soru 5 (Job grouped)

class(data_exp$Soru501)
sum(is.na(data_exp$Soru501))
unique(data_exp$Soru501)
summary(data_exp$Soru501)

  # 99 already recoded as missing values 

  # giving labels
data_exp <- data_exp |>
  rename(job_group = Soru501) |>
  mutate(job_group = factor(
    job_group, levels = c(1, 2, 3, 4, 5, 6), 
    labels = c("white collar","blue collar", "retired", "housewife", "student", "unemployed")
  ))

table(data_exp$job_group)

# Soru 601 (Region)

class(data_exp$Soru601)
sum(is.na(data_exp$Soru601))
unique(data_exp$Soru601)
summary(data_exp$Soru601)

  # recoding 
data_exp <- data_exp |>
  mutate(Soru601 = case_when(
    Soru601 == 99 ~ NA,
    TRUE ~ Soru601
  ))

table(data_exp$Soru601)

data_exp <- data_exp |>
  rename(region = Soru601) |>
  mutate(region = factor(
    region, levels = c(1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11, 12, 13), 
    labels = c("Istanbul", "West Marmara", "Aegean", "East Marmara", "West Anatolia", "Mediterranean Coast",
                 "Central Anatolia", "West Black Sea", "East Black Sea", "Northeast Anatolia",
                 "Central Eastern Anatolia", "Southeast Anatolia", "Abroad")
  ))

table(data_exp$region)

# Soru 7 (Today's party choice)

class(data_exp$Soru7)
sum(is.na(data_exp$Soru7))
unique(data_exp$Soru7)
summary(data_exp$Soru7)

  # recoding 
data_exp <- data_exp |>
  mutate(Soru7 = case_when(
    Soru7 == 99 ~ NA,
    TRUE ~ Soru7
  ))

table(data_exp$Soru7)

data_exp <- data_exp |>
  rename(vote_today = Soru7) |>
  mutate(vote_today = factor(
    vote_today, levels = c(1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11, 12, 13, 14, 15, 16, 17, 18, 19), 
    labels = c("AKP", "CHP", "MHP", "HDP/YSP", "Iyi", "Saadet", "Gelecek", "Deva", "Memleket", 
               "TIP", "Vatan", "Türkiye Degisim Partisi", "BBP", "Yeniden Refah", "DP", "Zafer", "Other",
               "Undecided", "No vote")
  ))

table(data_exp$vote_today)

# Soru 701 (Today's party choice grouped)

class(data_exp$Soru701)
sum(is.na(data_exp$Soru701))
unique(data_exp$Soru701)
summary(data_exp$Soru701)

  # recoding 
data_exp <- data_exp |>
  mutate(Soru701 = case_when(
    Soru701 == 99 ~ NA,
    TRUE ~ Soru701
  ))

table(data_exp$Soru701)

data_exp <- data_exp |>
  rename(vote_today_group = Soru701) |>
  mutate(vote_today_group = factor(
    vote_today_group, levels = c(1, 2, 3, 4, 5, 6, 7, 8), 
    labels = c("AKP", "CHP", "MHP", "HDP/YSP", "Iyi","Other", "Undecided", "No vote")
  ))

table(data_exp$vote_today_group)

# Soru 801 (Lost family member in earthquake)

class(data_exp$Soru801)
sum(is.na(data_exp$Soru801))
unique(data_exp$Soru801)
summary(data_exp$Soru801)

  # recoding
data_exp <- data_exp |>
  mutate(Soru801 = case_when(
    Soru801 == 1 ~ 1,
    Soru801 == 2 ~ 0,
    Soru801 == 99 ~ NA,
    TRUE ~ Soru801
  )) |>
  mutate(Soru801 = factor(
    Soru801, levels = c(0, 1), 
    labels = c("no", "yes")
  ))

table(data_exp$Soru801)


# Soru 802 (Lost relative in earthquake)

class(data_exp$Soru802)
sum(is.na(data_exp$Soru802))
unique(data_exp$Soru802)
summary(data_exp$Soru802)

# recoding
data_exp <- data_exp |>
  mutate(Soru802 = case_when(
    Soru802 == 1 ~ 1,
    Soru802 == 2 ~ 0,
    Soru802 == 99 ~ NA,
    TRUE ~ Soru802
  )) |>
  mutate(Soru802 = factor(
    Soru802, levels = c(0, 1), 
    labels = c("no", "yes")
  ))

table(data_exp$Soru802)


# Soru 803 (Lost acquaintance/friend in earthquake)

class(data_exp$Soru803)
sum(is.na(data_exp$Soru803))
unique(data_exp$Soru803)
summary(data_exp$Soru803)

  # recoding
data_exp <- data_exp |>
  mutate(Soru803 = case_when(
    Soru803 == 1 ~ 1,
    Soru803 == 2 ~ 0,
    Soru803 == 99 ~ NA,
    TRUE ~ Soru803
  )) |>
  mutate(Soru803 = factor(
    Soru803, levels = c(0, 1), 
    labels = c("no", "yes")
  ))

table(data_exp$Soru803)


# Soru 804 (Lost home in earthquake)

class(data_exp$Soru804)
sum(is.na(data_exp$Soru804))
unique(data_exp$Soru804)
summary(data_exp$Soru804)

  # recoding
data_exp <- data_exp |>
  mutate(Soru804 = case_when(
    Soru804 == 1 ~ 1,
    Soru804 == 2 ~ 0,
    Soru804 == 99 ~ NA,
    TRUE ~ Soru804
  )) |>
  mutate(Soru804 = factor(
    Soru804, levels = c(0, 1), 
    labels = c("no", "yes")
  ))

table(data_exp$Soru804)


# Soru 805 (No loss due to earthquake)

class(data_exp$Soru805)
sum(is.na(data_exp$Soru805))
unique(data_exp$Soru805)
summary(data_exp$Soru805)

  # recoding
data_exp <- data_exp |>
  mutate(Soru805 = case_when(
    Soru805 == 1 ~ 1,
    Soru805 == 2 ~ 0,
    Soru805 == 99 ~ NA,
    TRUE ~ Soru805
  )) |>
  mutate(Soru805 = factor(
    Soru805, levels = c(0, 1), 
    labels = c("some loss", "no loss")
  ))

table(data_exp$Soru805)


# Soru 901 (Accept Syrian refugees in one's country)

class(data_exp$Soru901)
sum(is.na(data_exp$Soru901))
unique(data_exp$Soru901)
summary(data_exp$Soru901)

# recoding
data_exp <- data_exp |>
  mutate(Soru901 = case_when(
    Soru901 == 1 ~ 1,
    Soru901 == 2 ~ 0,
    Soru901 == 99 ~ NA,
    TRUE ~ Soru901
  )) |>
  mutate(Soru901 = factor(
    Soru901, levels = c(0, 1), 
    labels = c("no", "yes")
  ))

table(data_exp$Soru901)


# Soru 902 (Accept Syrian refugees in one's city)

class(data_exp$Soru902)
sum(is.na(data_exp$Soru902))
unique(data_exp$Soru902)
summary(data_exp$Soru902)

# recoding
data_exp <- data_exp |>
  mutate(Soru902 = case_when(
    Soru902 == 1 ~ 1,
    Soru902 == 2 ~ 0,
    Soru902 == 99 ~ NA,
    TRUE ~ Soru902
  )) |>
  mutate(Soru902 = factor(
    Soru902, levels = c(0, 1), 
    labels = c("no", "yes")
  ))

table(data_exp$Soru902)


# Soru 903 (Accept Syrian refugees in one's neighbourhood/workplace/school)

class(data_exp$Soru903)
sum(is.na(data_exp$Soru903))
unique(data_exp$Soru903)
summary(data_exp$Soru903)

# recoding
data_exp <- data_exp |>
  mutate(Soru903 = case_when(
    Soru903 == 1 ~ 1,
    Soru903 == 2 ~ 0,
    Soru903 == 99 ~ NA,
    TRUE ~ Soru903
  )) |>
  mutate(Soru903 = factor(
    Soru903, levels = c(0, 1), 
    labels = c("no", "yes")
  ))

table(data_exp$Soru903)


# Soru 904 (Accept Syrian refugees in one's apartment building/friend group/as neighbour)

class(data_exp$Soru904)
sum(is.na(data_exp$Soru904))
unique(data_exp$Soru904)
summary(data_exp$Soru904)

# recoding
data_exp <- data_exp |>
  mutate(Soru904 = case_when(
    Soru904 == 1 ~ 1,
    Soru904 == 2 ~ 0,
    Soru904 == 99 ~ NA,
    TRUE ~ Soru904
  )) |>
  mutate(Soru904 = factor(
    Soru904, levels = c(0, 1), 
    labels = c("no", "yes")
  ))

table(data_exp$Soru904)


# Soru 905 (Accept Syrian refugees in one's house/family)

class(data_exp$Soru905)
sum(is.na(data_exp$Soru905))
unique(data_exp$Soru905)
summary(data_exp$Soru905)

# recoding
data_exp <- data_exp |>
  mutate(Soru905 = case_when(
    Soru905 == 1 ~ 1,
    Soru905 == 2 ~ 0,
    Soru905 == 99 ~ NA,
    TRUE ~ Soru905
  )) |>
  mutate(Soru905 = factor(
    Soru905, levels = c(0, 1), 
    labels = c("no", "yes")
  ))

table(data_exp$Soru905)


# Soru 10 (Number of Syrians in one's neighbourhood)

class(data_exp$Soru10)
sum(is.na(data_exp$Soru10))
unique(data_exp$Soru10)
summary(data_exp$Soru10)

  # recoding 
data_exp <- data_exp |>
  mutate(Soru10 = case_when(
    Soru10 == 99 ~ NA,
    TRUE ~ Soru10
  ))

table(data_exp$Soru10)

data_exp <- data_exp |>
  rename(syr_presence = Soru10) |>
  mutate(syr_presence = factor(
    syr_presence, levels = c(1, 2, 3, 4), 
    labels = c("None", "Few", "Quite some", "Many")
  ))

table(data_exp$syr_presence)


# Soru 11 (Frequency of encounters with Syrians)

class(data_exp$Soru11)
sum(is.na(data_exp$Soru11))
unique(data_exp$Soru11)
summary(data_exp$Soru11)

  # recoding 
data_exp <- data_exp |>
  mutate(Soru11 = case_when(
    Soru11 == 99 ~ NA,
    TRUE ~ Soru11
  ))

table(data_exp$Soru11)

data_exp <- data_exp |>
  rename(syr_encounter = Soru11) |>
  mutate(syr_encounter = factor(
    syr_encounter, levels = c(1, 2, 3, 4), 
    labels = c("Never", "Sometimes", "Often", "All the time")
  ))

table(data_exp$syr_encounter)


# Soru 12 (Do you try to stay away from Syrian refugees in everyday life?)

class(data_exp$Soru12)
sum(is.na(data_exp$Soru12))
unique(data_exp$Soru12)
summary(data_exp$Soru12)

# recoding
data_exp <- data_exp |>
  mutate(Soru12 = case_when(
    Soru12 == 1 ~ 1,
    Soru12 == 2 ~ 0,
    Soru12 == 99 ~ NA,
    TRUE ~ Soru12
  )) 

data_exp <- data_exp |>
  rename(try_stayaway = Soru12) |>
  mutate(try_stayaway = factor(
    try_stayaway, levels = c(0, 1), 
    labels = c("No", "Yes")
  ))

table(data_exp$try_stayaway)


# Soru 1301 (Syrian partner)

class(data_exp$Soru1301)
sum(is.na(data_exp$Soru1301))
unique(data_exp$Soru1301)
summary(data_exp$Soru1301)

  # recoding
data_exp <- data_exp |>
  mutate(Soru1301 = case_when(
    Soru1301 == 1 ~ 1,
    Soru1301 == 2 ~ 0,
    Soru1301 == 99 ~ NA,
    TRUE ~ Soru1301
  )) |>
  mutate(Soru1301 = factor(
    Soru1301, levels = c(0, 1), 
    labels = c("no", "yes")
  ))

table(data_exp$Soru1301)


# Soru 1302 (Syrian relative)

class(data_exp$Soru1302)
sum(is.na(data_exp$Soru1302))
unique(data_exp$Soru1302)
summary(data_exp$Soru1302)

  # recoding
data_exp <- data_exp |>
  mutate(Soru1302 = case_when(
    Soru1302 == 1 ~ 1,
    Soru1302 == 2 ~ 0,
    Soru1302 == 99 ~ NA,
    TRUE ~ Soru1302
  )) |>
  mutate(Soru1302 = factor(
    Soru1302, levels = c(0, 1), 
    labels = c("no", "yes")
  ))

table(data_exp$Soru1302)


# Soru 1303 (Syrian neighbour)

class(data_exp$Soru1303)
sum(is.na(data_exp$Soru1303))
unique(data_exp$Soru1303)
summary(data_exp$Soru1303)

# recoding
data_exp <- data_exp |>
  mutate(Soru1303 = case_when(
    Soru1303 == 1 ~ 1,
    Soru1303 == 2 ~ 0,
    Soru1303 == 99 ~ NA,
    TRUE ~ Soru1303
  )) |>
  mutate(Soru1303 = factor(
    Soru1303, levels = c(0, 1), 
    labels = c("no", "yes")
  ))

table(data_exp$Soru1303)


# Soru 1304 (Syrian colleague)

class(data_exp$Soru1304)
sum(is.na(data_exp$Soru1304))
unique(data_exp$Soru1304)
summary(data_exp$Soru1304)

  # recoding
data_exp <- data_exp |>
  mutate(Soru1304 = case_when(
    Soru1304 == 1 ~ 1,
    Soru1304 == 2 ~ 0,
    Soru1304 == 99 ~ NA,
    TRUE ~ Soru1304
  )) |>
  mutate(Soru1304 = factor(
    Soru1304, levels = c(0, 1), 
    labels = c("no", "yes")
  ))

table(data_exp$Soru1304)


# Soru 1305 (Syrian acquaintance/friend)

class(data_exp$Soru1305)
sum(is.na(data_exp$Soru1305))
unique(data_exp$Soru1305)
summary(data_exp$Soru1305)

  # recoding
data_exp <- data_exp |>
  mutate(Soru1305 = case_when(
    Soru1305 == 1 ~ 1,
    Soru1305 == 2 ~ 0,
    Soru1305 == 99 ~ NA,
    TRUE ~ Soru1305
  )) |>
  mutate(Soru1305 = factor(
    Soru1305, levels = c(0, 1), 
    labels = c("no", "yes")
  ))

table(data_exp$Soru1305)


# Soru 1306 (Syrian renter)

class(data_exp$Soru1306)
sum(is.na(data_exp$Soru1306))
unique(data_exp$Soru1306)
summary(data_exp$Soru1306)

  # recoding
data_exp <- data_exp |>
  mutate(Soru1306 = case_when(
    Soru1306 == 1 ~ 1,
    Soru1306 == 2 ~ 0,
    Soru1306 == 99 ~ NA,
    TRUE ~ Soru1306
  )) |>
  mutate(Soru1306 = factor(
    Soru1306, levels = c(0, 1), 
    labels = c("no", "yes")
  ))

table(data_exp$Soru1306)


# Soru 1307 (Syrian employee)

class(data_exp$Soru1307)
sum(is.na(data_exp$Soru1307))
unique(data_exp$Soru1307)
summary(data_exp$Soru1307)

# recoding
data_exp <- data_exp |>
  mutate(Soru1307 = case_when(
    Soru1307 == 1 ~ 1,
    Soru1307 == 2 ~ 0,
    Soru1307 == 99 ~ NA,
    TRUE ~ Soru1307
  )) |>
  mutate(Soru1307 = factor(
    Soru1307, levels = c(0, 1), 
    labels = c("no", "yes")
  ))

table(data_exp$Soru1307)


# Soru 1308 (No acquaintance with any Syrian whatsoever)

class(data_exp$Soru1308)
sum(is.na(data_exp$Soru1308))
unique(data_exp$Soru1308)
summary(data_exp$Soru1308)

  # recoding
data_exp <- data_exp |>
  mutate(Soru1308 = case_when(
    Soru1308 == 1 ~ 1,
    Soru1308 == 2 ~ 0,
    Soru1308 == 99 ~ NA,
    TRUE ~ Soru1308
  )) |>
  mutate(Soru1308 = factor(
    Soru1308, levels = c(0, 1), 
    labels = c("some acquaintance", "no acquaintance at all")
  ))

table(data_exp$Soru1308)


# Syr_con (Any connection but 'acquaintance')

  # recoding
data_exp <- data_exp %>%
  mutate(syr_con = ifelse(
    Soru1301 == "yes" | Soru1302 == "yes" | Soru1303 == "yes" | Soru1304 == "yes" | Soru1306 == "yes" | Soru1307 == "yes",
    "yes",
    "no"
  ))

table(data_exp$syr_con)

# Syr_con2 (Any connection but 'acquaintance' and 'spouse')

# recoding
data_exp <- data_exp %>%
  mutate(syr_con2 = ifelse(
    Soru1302 == "yes" | Soru1303 == "yes" | Soru1304 == "yes" | Soru1306 == "yes" | Soru1307 == "yes",
    "yes",
    "no"
  ))

table(data_exp$syr_con2)

  # Details
    # Spouse
table(data_exp$Soru1301)
    # Relative
table(data_exp$Soru1302)
    # Neighbour
table(data_exp$Soru1303)
    # Colleague
table(data_exp$Soru1304)
    # Acquaintance
table(data_exp$Soru1305)
    # Tenant
table(data_exp$Soru1306)
    # Employee
table(data_exp$Soru1307)
    # None
table(data_exp$Soru1308)

# Soru 14 (Frequency of talking to Syrians)

class(data_exp$Soru14)
sum(is.na(data_exp$Soru14))
unique(data_exp$Soru14)
summary(data_exp$Soru14)

  # recoding 
data_exp <- data_exp |>
  mutate(Soru14 = case_when(
    Soru14 == 99 ~ NA,
    TRUE ~ Soru14
  ))

table(data_exp$Soru14)

data_exp <- data_exp |>
  rename(syr_talking = Soru14) |>
  mutate(syr_talking = factor(
    syr_talking, levels = c(1, 2, 3, 4), 
    labels = c("Never", "Sometimes", "Often", "All the time")
  ))

table(data_exp$syr_talking)


# Soru 15 (Do you have a Syrian friend that is not related to you and that you feel you can trust?)

class(data_exp$Soru15)
sum(is.na(data_exp$Soru15))
unique(data_exp$Soru15)
summary(data_exp$Soru15)

  # recoding
data_exp <- data_exp |>
  mutate(Soru15 = case_when(
    Soru15 == 1 ~ 1,
    Soru15 == 2 ~ 0,
    Soru15 == 99 ~ NA,
    TRUE ~ Soru15
  )) 

table(data_exp$Soru15)

data_exp <- data_exp |>
  rename(syr_friend = Soru15) |>
  mutate(syr_friend = factor(
    syr_friend, levels = c(0, 1), 
    labels = c("no", "yes")
  ))

table(data_exp$syr_friend)


# Soru 16 (Frequency of socialising with Syrians that are not relatives)

class(data_exp$Soru16)
sum(is.na(data_exp$Soru16))
unique(data_exp$Soru16)
summary(data_exp$Soru16)

  # recoding 
data_exp <- data_exp |>
  mutate(Soru16 = case_when(
    Soru16 == 99 ~ NA,
    TRUE ~ Soru16
  ))

table(data_exp$Soru16)

data_exp <- data_exp |>
  rename(syr_socialising = Soru16) |>
  mutate(syr_socialising = factor(
    syr_socialising, levels = c(1, 2, 3, 4), 
    labels = c("Never", "Sometimes", "Often", "All the time")
  ))

table(data_exp$syr_socialising)


# Soru 1701 (Refugee policy is an important issue for Turkey)

class(data_exp$Soru1701)
sum(is.na(data_exp$Soru1701))
unique(data_exp$Soru1701)
summary(data_exp$Soru1701)

  # recoding 
data_exp <- data_exp |>
  rename(refpol_imp = Soru1701) |>
  mutate(refpol_imp = case_when(
    refpol_imp == 99 ~ NA,
    TRUE ~ refpol_imp
  ))

table(data_exp$refpol_imp)


# Soru 1702 (Refugee policy is a more important issue for Turkey than inflation)

class(data_exp$Soru1702)
sum(is.na(data_exp$Soru1702))
unique(data_exp$Soru1702)
summary(data_exp$Soru1702)

# recoding 
data_exp <- data_exp |>
  rename(refpol_moreimp = Soru1702) |>
  mutate(refpol_moreimp = case_when(
    refpol_moreimp == 99 ~ NA,
    TRUE ~ refpol_moreimp
  ))

table(data_exp$refpol_moreimp)


# Soru 1703 (Cultural similarity of Syrian refugees to oneself)

class(data_exp$Soru1703)
sum(is.na(data_exp$Soru1703))
unique(data_exp$Soru1703)
summary(data_exp$Soru1703)

# recoding 
data_exp <- data_exp |>
  rename(cultsim = Soru1703) |>
  mutate(cultsim = case_when(
    cultsim == 99 ~ NA,
    TRUE ~ cultsim
  ))

table(data_exp$cultsim)


# Soru 1704 (Religious similarity of Syrian refugees to oneself)

class(data_exp$Soru1704)
sum(is.na(data_exp$Soru1704))
unique(data_exp$Soru1704)
summary(data_exp$Soru1704)

  # recoding 
data_exp <- data_exp |>
  rename(relsim = Soru1704) |>
  mutate(relsim = case_when(
    relsim == 99 ~ NA,
    TRUE ~ relsim
  ))

table(data_exp$relsim)


# Soru 1705 (Syrian refugees are pious Muslims)

class(data_exp$Soru1705)
sum(is.na(data_exp$Soru1705))
unique(data_exp$Soru1705)
summary(data_exp$Soru1705)

  # recoding 
data_exp <- data_exp |>
  rename(syr_pious = Soru1705) |>
  mutate(syr_pious = case_when(
    syr_pious == 99 ~ NA,
    TRUE ~ syr_pious
  ))

table(data_exp$syr_pious)


# Soru 18 (Preference in refugee - general)

class(data_exp$Soru18)
sum(is.na(data_exp$Soru18))
unique(data_exp$Soru18)
summary(data_exp$Soru18)

  # recoding
data_exp <- data_exp |>
  mutate(Soru18 = case_when(
    Soru18 == 99 ~ NA,
    TRUE ~ Soru18
  )) |>
  rename(prefref_gen = Soru18) |>
  mutate(prefref_gen = factor(
    prefref_gen, levels = c(1, 2), 
    labels = c("Syrian", "Ukrainian")
  ))

table(data_exp$prefref_gen)


# Soru 19 (Relevance of ethnicity of Syrian refugee)

class(data_exp$Soru19)
sum(is.na(data_exp$Soru19))
unique(data_exp$Soru19)
summary(data_exp$Soru19)

# recoding
data_exp <- data_exp |>
  rename(syrref_eth = Soru19) |>
  mutate(syrref_eth = factor(
    syrref_eth, levels = c(1, 2, 3), 
    labels = c("Doesn't matter", "Prefer Arab", "Prefer Kurdish")
  ))

table(data_exp$syrref_eth)


# Soru 20 (Preference in refugee - educated and employed)

class(data_exp$Soru20)
sum(is.na(data_exp$Soru20))
unique(data_exp$Soru20)
summary(data_exp$Soru20)

  # recoding
data_exp <- data_exp |>
  mutate(Soru20 = case_when(
    Soru20 == 99 ~ NA,
    TRUE ~ Soru20
  )) |>
  rename(prefref_eduemp = Soru20) |>
  mutate(prefref_eduemp = factor(
    prefref_eduemp, levels = c(1, 2), 
    labels = c("Syrian", "Ukrainian")
  ))

table(data_exp$prefref_eduemp)

## treatment questions/item --------------------------------------

# Soru 2101 (Pious people used to be oppressed in Turkey)

class(data_exp$Soru2101)
sum(is.na(data_exp$Soru2101))
unique(data_exp$Soru2101)
summary(data_exp$Soru2101)

  # inspecting number of missing values
table(data_exp$Soru2101)

# recoding 
data_exp <- data_exp |>
  mutate(Soru2101 = case_when(
    Soru2101 == 99 ~ NA,
    TRUE ~ Soru2101
  ))

table(data_exp$Soru2101)


# Soru 2102 (Pious people are still oppressed in Turkey)

class(data_exp$Soru2102)
sum(is.na(data_exp$Soru2102))
unique(data_exp$Soru2102)
summary(data_exp$Soru2102)

  # inspecting number of missing values
table(data_exp$Soru2102)

  # recoding 
data_exp <- data_exp |>
  mutate(Soru2102 = case_when(
    Soru2102 == 99 ~ NA,
    TRUE ~ Soru2102
  ))

table(data_exp$Soru2102)


# Soru 2103 (Ethnic minorities used to be oppressed in Turkey)

class(data_exp$Soru2103)
sum(is.na(data_exp$Soru2103))
unique(data_exp$Soru2103)
summary(data_exp$Soru2103)

  # inspecting number of missing values
table(data_exp$Soru2103)

  # recoding 
data_exp <- data_exp |>
  mutate(Soru2103 = case_when(
    Soru2103 == 99 ~ NA,
    TRUE ~ Soru2103
  ))

table(data_exp$Soru2103)


# Soru 2104 (Ethnic minorities are still oppressed in Turkey)

class(data_exp$Soru2104)
sum(is.na(data_exp$Soru2104))
unique(data_exp$Soru2104)
summary(data_exp$Soru2104)

  # inspecting number of missing values
table(data_exp$Soru2104)

  # recoding 
data_exp <- data_exp |>
  mutate(Soru2104 = case_when(
    Soru2104 == 99 ~ NA,
    TRUE ~ Soru2104
  ))

table(data_exp$Soru2104)


# Soru 2105 (Turkey should embrace its ethnic minorities more)

class(data_exp$Soru2105)
sum(is.na(data_exp$Soru2105))
unique(data_exp$Soru2105)
summary(data_exp$Soru2105)

  # inspecting number of missing values
table(data_exp$Soru2105)

  # recoding 
data_exp <- data_exp |>
  mutate(Soru2105 = case_when(
    Soru2105 == 99 ~ NA,
    TRUE ~ Soru2105
  ))

table(data_exp$Soru2105)


# Soru 2106 (Turkey should embrace Islam more) 

class(data_exp$Soru2106)
sum(is.na(data_exp$Soru2106))
unique(data_exp$Soru2106)
summary(data_exp$Soru2106)

  # inspecting number of missing values
table(data_exp$Soru2106)

  # recoding 
data_exp <- data_exp |>
  mutate(Soru2106 = case_when(
    Soru2106 == 99 ~ NA,
    TRUE ~ Soru2106
  ))

table(data_exp$Soru2106)

# Soru 22 (Trigger item - no actual question)

class(data_exp$Soru22)
sum(is.na(data_exp$Soru22))
unique(data_exp$Soru22)
summary(data_exp$Soru22)

  # recoding
data_exp <- data_exp |>
  rename(trigger = Soru22) |>
  mutate(trigger = factor(
    trigger, levels = c(1, 2, 3, 4), 
    labels = c("Agree with them", "Don't agree with them", "Doesn't interest me", "Upsets me")
  ))

table(data_exp$trigger)


## treatment test questions -----------------------------------

# Soru 2301 (Syrians part of Turkish society - today)

class(data_exp$Soru2301)
sum(is.na(data_exp$Soru2301))
unique(data_exp$Soru2301)

  # recoding 
summary(data_exp$Soru2301)

data_exp <- data_exp |>
  rename(partsoc_today = Soru2301) |>
  mutate(partsoc_today = case_when(
    partsoc_today == 99 ~ NA,
    TRUE ~ partsoc_today
  ))

summary(data_exp$partsoc_today)
table(data_exp$partsoc_today)


# Soru 2302 (Syrians part of Turkish society - in the future)

class(data_exp$Soru2303)
sum(is.na(data_exp$Soru2303))
unique(data_exp$Soru2303)

  # recoding 
summary(data_exp$Soru2302)

data_exp <- data_exp |>
  rename(partsoc_tomo = Soru2302) |>
  mutate(partsoc_tomo = case_when(
    partsoc_tomo == 99 ~ NA,
    TRUE ~ partsoc_tomo
  ))

summary(data_exp$partsoc_tomo)
table(data_exp$partsoc_tomo)


# Soru 2303 (Syrians part of in Turkish society - should)

class(data_exp$Soru2303)
sum(is.na(data_exp$Soru2303))
unique(data_exp$Soru2303)

  # recoding 
summary(data_exp$Soru2303)

data_exp <- data_exp |>
  rename(partsoc_should = Soru2303) |>
  mutate(partsoc_should = case_when(
    partsoc_should == 99 ~ NA,
    TRUE ~ partsoc_should
  ))

summary(data_exp$partsoc_should)
table(data_exp$partsoc_should)


# Soru 2304 (Syrians should become Turkish citizens after integrating)

class(data_exp$Soru2304)
sum(is.na(data_exp$Soru2304))
unique(data_exp$Soru2304)

  # recoding 
summary(data_exp$Soru2304)

data_exp <- data_exp |>
  rename(citizenship = Soru2304) |>
  mutate(citizenship = case_when(
    citizenship == 99 ~ NA,
    TRUE ~ citizenship
  ))

summary(data_exp$citizenship)
table(data_exp$citizenship)


# Soru 24 (Syrian refugees as religious brothers/sisters)

class(data_exp$Soru24)
sum(is.na(data_exp$Soru24))
unique(data_exp$Soru24)
summary(data_exp$Soru24)

  # recoding
data_exp <- data_exp |>
  mutate(Soru24 = case_when(
    Soru24 == 1 ~ 1,
    Soru24 == 2 ~ 0,
    Soru24 == 99 ~ NA,
    TRUE ~ Soru24
  )) 

table(data_exp$Soru24)

data_exp <- data_exp |>
  rename(rel_bro = Soru24) |>
  mutate(rel_bro = factor(
    rel_bro, levels = c(0, 1), 
    labels = c("no", "yes")
  ))

table(data_exp$rel_bro)


# Soru 25 (Syrian refugees as Kurdish/Arab brothers/sisters)

class(data_exp$Soru25)
sum(is.na(data_exp$Soru25))
unique(data_exp$Soru25)
summary(data_exp$Soru25)

  # recoding
data_exp <- data_exp |>
  mutate(Soru25 = case_when(
    Soru25 == 1 ~ 1,
    Soru25 == 2 ~ 0,
    Soru25 == 99 ~ NA,
    TRUE ~ Soru25
  )) 

table(data_exp$Soru25)

data_exp <- data_exp |>
  rename(eth_bro = Soru25) |>
  mutate(eth_bro = factor(
    eth_bro, levels = c(0, 1), 
    labels = c("no", "yes")
  ))

table(data_exp$eth_bro)


# Soru 26 (Which description best fits the Syrian refugees?)

class(data_exp$Soru26)
sum(is.na(data_exp$Soru26))
unique(data_exp$Soru26)
summary(data_exp$Soru26)

  # recoding
data_exp <- data_exp |>
  mutate(Soru26 = case_when(
    Soru26 == 99 ~ NA,
    TRUE ~ Soru26
  )) 

data_exp <- data_exp |>
  rename(syr_descrip = Soru26) |>
  mutate(syr_descrip = factor(
    syr_descrip, levels = c(1, 2, 3, 4), 
    labels = c("Victim", "Opportunist", "Guest", "None of these")
  ))

table(data_exp$syr_descrip)
summary(data_exp$syr_descrip)


# Soru 27 (Is returning Syrian refugees to Syria at this point ethically acceptable?)

class(data_exp$Soru27)
sum(is.na(data_exp$Soru27))
unique(data_exp$Soru27)
summary(data_exp$Soru27)

  # recoding
data_exp <- data_exp |>
  mutate(Soru27 = case_when(
    Soru27 == 1 ~ 1,
    Soru27 == 2 ~ 0,
    Soru27 == 99 ~ NA,
    TRUE ~ Soru27
  )) 

table(data_exp$Soru27)

data_exp <- data_exp |>
  rename(return_ethical = Soru27) |>
  mutate(return_ethical = factor(
    return_ethical, levels = c(0, 1), 
    labels = c("no", "yes")
  ))

table(data_exp$return_ethical)


## manipulation check item -----------------------------------------

# Soru 28 (Manipulation check: While answering the questions above, 
# how much did you think about the views of other Turkish citizens?)

class(data_exp$Soru28)
sum(is.na(data_exp$Soru28))
unique(data_exp$Soru28)
summary(data_exp$Soru28)

  # recoding
data_exp <- data_exp |>
  mutate(Soru28 = case_when(
    Soru28 == 99 ~ NA,
    TRUE ~ Soru28
  )) 

data_exp <- data_exp |>
  rename(manip_check = Soru28) |>
  mutate(manip_check = factor(
    manip_check, levels = c(1, 2, 3, 4), 
    labels = c("Not at all", "A bit", "Quite a bit", "A lot")
  ))

table(data_exp$manip_check)
summary(data_exp$manip_check)


## other post-treatment items ----------------------------------

# Soru 29 (If the EU provided more economic and social support, would you accept that
# the Syrian refugees' longterm stay in Turkey?)

class(data_exp$Soru29)
sum(is.na(data_exp$Soru29))
unique(data_exp$Soru29)
summary(data_exp$Soru29)

  # recoding
data_exp <- data_exp |>
  mutate(Soru29 = case_when(
    Soru29 == 1 ~ 1,
    Soru29 == 2 ~ 0,
    Soru29 == 99 ~ NA,
    TRUE ~ Soru29
  )) 

table(data_exp$Soru29)

data_exp <- data_exp |>
  rename(longterm_stay = Soru29) |>
  mutate(longterm_stay = factor(
    longterm_stay, levels = c(0, 1), 
    labels = c("no", "yes")
  ))

table(data_exp$longterm_stay)


# Soru 30 (Primary language at home)

class(data_exp$Soru30)
sum(is.na(data_exp$Soru30))
unique(data_exp$Soru30)
summary(data_exp$Soru30)
table(data_exp$Soru30)

  # recoding
data_exp <- data_exp |>
  mutate(Soru30 = case_when(
    Soru30 == 99 ~ NA,
    TRUE ~ Soru30
  )) 

table(data_exp$Soru30)

data_exp <- data_exp |>
  rename(language = Soru30) |>
  mutate(language = factor(
    language, levels = c(1, 2, 3, 4, 5), 
    labels = c("Turkish", "Kurdish", "Zaza", "Arabic", "Other")
  ))

table(data_exp$language)


# Soru 31 (Vote in 14 May elections)

class(data_exp$Soru31)
sum(is.na(data_exp$Soru31))
unique(data_exp$Soru31)
summary(data_exp$Soru31)
table(data_exp$Soru31)

  # recoding
data_exp <- data_exp |>
  mutate(Soru31 = case_when(
    Soru31 == 99 ~ NA,
    TRUE ~ Soru31
  )) 

table(data_exp$Soru31)

data_exp <- data_exp |>
  rename(vote_may = Soru31) |>
  mutate(vote_may = factor(
    vote_may, levels = c(1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11, 12, 13, 14, 15, 16, 17, 18, 19), 
    labels = c("AKP", "MHP", "Yeniden Refah", "BBP", "CHP", "Iyi", "HDP/YSP", "TIP", "Memleket",
               "Zafer", "Adalet", "Vatan", "TKP", "TKH", "Other", "Independent", "Was underage", 
               "Didn't vote", "Voted blank")  
  ))

table(data_exp$vote_may)


# Soru 3101 (Vote in 14 May elections - grouped)

class(data_exp$Soru3101)
sum(is.na(data_exp$Soru3101))
unique(data_exp$Soru3101)
summary(data_exp$Soru3101)
table(data_exp$Soru3101)

  # recoding
data_exp <- data_exp |>
  mutate(Soru3101 = case_when(
    Soru3101 == 99 ~ NA,
    TRUE ~ Soru3101
  )) 

table(data_exp$Soru3101)

data_exp <- data_exp |>
  rename(vote_may_group = Soru3101) |>
  mutate(vote_may_group = factor(
    vote_may_group, levels = c(1, 2, 3, 4, 5, 6, 7, 8, 9, 10), 
    labels = c("AKP", "CHP", "MHP", "HDP/YSP", "Iyi", "Other", "Independent", "Was underage",
               "Didn't vote", "Voted blank")
  ))

table(data_exp$vote_may_group)


# Soru 3102 (Vote in 14 May elections - coalitions)

class(data_exp$Soru3102)
sum(is.na(data_exp$Soru3102))
unique(data_exp$Soru3102)
summary(data_exp$Soru3102)
table(data_exp$Soru3102)

  # recoding
data_exp <- data_exp |>
  mutate(Soru3102 = case_when(
    Soru3102 == 99 ~ NA,
    TRUE ~ Soru3102
  )) 

table(data_exp$Soru3102)

data_exp <- data_exp |>
  rename(vote_may_coal = Soru3102) |>
  mutate(vote_may_coal = factor(
    vote_may_coal, levels = c(1, 2, 3, 4, 5, 6, 7, 8, 9, 10), 
    labels = c("People", "Nation", "Bread and Freedom", "Memleket", "Ata", "Other", "Independent",
               "Was underage", "Didn't vote", "Voted blank")
  ))

table(data_exp$vote_may_coal)


# Soru 32 (Head covering - interviewee or partner)

class(data_exp$Soru32)
sum(is.na(data_exp$Soru32))
unique(data_exp$Soru32)
summary(data_exp$Soru32)
table(data_exp$Soru32)

  # recoding
data_exp <- data_exp |>
  mutate(Soru32 = case_when(
    Soru32 == 99 ~ NA,
    TRUE ~ Soru32
  )) 

table(data_exp$Soru32)

data_exp <- data_exp |>
  rename(head_cov = Soru32) |>
  mutate(head_cov = factor(
    head_cov, levels = c(1, 2, 3, 4, 5), 
    labels = c("None", "Headscarf", "Turban", "Chador/Niqab", "Interviewee single male")
  ))

table(data_exp$head_cov)


# Soru 33 (Ethnic self-identification)

class(data_exp$Soru33)
sum(is.na(data_exp$Soru33))
unique(data_exp$Soru33)
summary(data_exp$Soru33)
table(data_exp$Soru33)

  # recoding
data_exp <- data_exp |>
  mutate(Soru33 = case_when(
    Soru33 == 99 ~ NA,
    TRUE ~ Soru33
  )) 

table(data_exp$Soru33)

data_exp <- data_exp |>
  rename(eth_self = Soru33) |>
  mutate(eth_self = factor(
    eth_self, levels = c(1, 2, 3, 4, 5), 
    labels = c("Turkish", "Kurdish", "Zaza", "Arab", "Other")
  ))

table(data_exp$eth_self)


# Alevi (Recode of Soru 34)

data_exp$alevi <- data_exp$Soru34
table(data_exp$alevi)

  # recoding
data_exp <- data_exp |>
  mutate(alevi = case_when(
    alevi == 1 ~ 0,
    alevi == 2 ~ 1,
    alevi == 3 ~ 0,
    alevi == 99 ~ NA,
    TRUE ~ alevi
  )) 

table(data_exp$alevi)

data_exp <- data_exp |>
  mutate(alevi = factor(
    alevi, levels = c(0, 1), 
    labels = c("no", "yes")
  ))

table(data_exp$alevi)


# Soru 34 (Islamic sect)

class(data_exp$Soru34)
sum(is.na(data_exp$Soru34))
unique(data_exp$Soru34)
summary(data_exp$Soru34)
table(data_exp$Soru34)

  # recoding
data_exp <- data_exp |>
  mutate(Soru34 = case_when(
    Soru34 == 99 ~ NA,
    TRUE ~ Soru34
  )) 

table(data_exp$Soru34)

data_exp <- data_exp |>
  rename(sect = Soru34) |>
  mutate(sect = factor(
    sect, levels = c(1, 2, 3), 
    labels = c("Sunni", "Alevi", "Other")
  ))

table(data_exp$sect)


# Soru 35 (Religious self-identification)

class(data_exp$Soru35)
sum(is.na(data_exp$Soru35))
unique(data_exp$Soru35)
summary(data_exp$Soru35)
table(data_exp$Soru35)

  # recoding
data_exp <- data_exp |>
  mutate(Soru35 = case_when(
    Soru35 == 99 ~ NA,
    TRUE ~ Soru35
  )) 

table(data_exp$Soru35)

data_exp <- data_exp |>
  rename(rel_prac = Soru35) |>
  mutate(rel_prac = factor(
    rel_prac, levels = c(1, 2, 3, 4, 5), 
    labels = c("Non-believer", "No practice", "Some practice", "Practice", "Pious practice")
  ))

table(data_exp$rel_prac)


# Soru 3601 (Income - Grouping 1)

class(data_exp$Soru3601)
sum(is.na(data_exp$Soru3601))
unique(data_exp$Soru3601)
summary(data_exp$Soru3601)
table(data_exp$Soru3601)

  # recoding
data_exp <- data_exp |>
  mutate(Soru3601 = case_when(
    Soru3601 == 99 ~ NA,
    TRUE ~ Soru3601
  )) 

table(data_exp$Soru3601)

data_exp <- data_exp |>
  rename(income1 = Soru3601) |>
  mutate(income1 = factor(
    income1, levels = c(1, 2, 3, 4, 5, 6), 
    labels = c("Up to 5k", "5-8k", "8-10k", "10-15k", "15-20k", "20k+")
  ))

table(data_exp$income1)


# Soru 3602 (Income - Grouping 2)

class(data_exp$Soru3602)
sum(is.na(data_exp$Soru3602))
unique(data_exp$Soru3602)
summary(data_exp$Soru3602)
table(data_exp$Soru3602)

  # recoding
data_exp <- data_exp |>
  mutate(Soru3602 = case_when(
    Soru3602 == 99 ~ NA,
    TRUE ~ Soru3602
  )) 

table(data_exp$Soru3602)

data_exp <- data_exp |>
  rename(income2 = Soru3602) |>
  mutate(income2 = factor(
    income2, levels = c(1, 2, 3, 4, 5, 6), 
    labels = c("Up to 10k", "10-15k", "15-20k", "21-20k", "31-50k", "50k+")
  ))

table(data_exp$income2)


# Soru 38 (Type of house

class(data_exp$Soru38)
sum(is.na(data_exp$Soru38))
unique(data_exp$Soru38)
summary(data_exp$Soru38)
table(data_exp$Soru38)

  # recoding
data_exp <- data_exp |>
  mutate(Soru38 = case_when(
    Soru38 == 99 ~ NA,
    TRUE ~ Soru38
  )) 

table(data_exp$Soru38)

data_exp <- data_exp |>
  rename(house = Soru38) |>
  mutate(house = factor(
    house, levels = c(1, 2, 3, 4, 5), 
    labels = c("Gecekondu", "Traditional house", "Apartment", "Site", "Villa") 
  ))

table(data_exp$house)

## save dataset ------------------------------------------------

write_rds(data_exp, "02_data/03_data_clean.rds")

## clean environment -------------------------------------------

rm(list = ls())
