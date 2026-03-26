# Alexander F. Gazmararian
# agazmararian@gmail.com

library(tidyverse)
library(tidycensus)

#Load census variables 
acs_vars <- load_variables(2021, "acs5")
#sex by age by education
SexAgeEdu <- subset(acs_vars, concept == "SEX BY AGE BY EDUCATIONAL ATTAINMENT FOR THE POPULATION 18 YEARS AND OVER")
SexAgeEdu <- SexAgeEdu[str_count(SexAgeEdu$label, "!") == 8, ]
#household income
income <- subset(acs_vars, concept == "HOUSEHOLD INCOME IN THE PAST 12 MONTHS (IN 2021 INFLATION-ADJUSTED DOLLARS)")
income <- income[-1,]
#sex by age by white
RaceAgeSex <- subset(acs_vars, concept %in% c("SEX BY AGE (WHITE ALONE)",
                                              "SEX BY AGE (BLACK OR AFRICAN AMERICAN ALONE)",
                                              "SEX BY AGE (AMERICAN INDIAN AND ALASKA NATIVE ALONE)",
                                              "SEX BY AGE (ASIAN ALONE)",
                                              "SEX BY AGE (NATIVE HAWAIIAN AND OTHER PACIFIC ISLANDER ALONE)",
                                              "SEX BY AGE (SOME OTHER RACE ALONE)",
                                              "SEX BY AGE (TWO OR MORE RACES)"))
RaceAgeSex <- RaceAgeSex[stringr::str_count(RaceAgeSex$label, "!")==6,]
#Create vector of variables to download
getvars <- c(SexAgeEdu$name, income$name, RaceAgeSex$name)
#download 2021 5-Year ACS
acs_long <- get_acs("us", variables = getvars, year = 2021, sumfile = "acs5")
#create joint and marginal distributions
#age x sex x education
acs.SexAgeEdu <- subset(acs_long, variable %in% SexAgeEdu$name)
acs.SexAgeEdu <- merge(acs.SexAgeEdu, SexAgeEdu, by.x = "variable", by.y = "name")
joint <- separate(acs.SexAgeEdu, col = label, into = c("estimate2", "total", "gender", "age_acs", "edu"), sep = "!!")
joint <- subset(joint, select = -c(concept, estimate2, total, geography, NAME, GEOID))
joint <- joint %>%
  mutate(
    edu_acs = case_when(
      edu %in% c("Less than 9th grade", "9th to 12th grade, no diploma", "High school graduate (includes equivalency)") ~ "No college",
      edu %in% c("Associate's degree", "Some college, no degree") ~ "Some college",
      edu %in% c("Bachelor's degree", "Graduate or professional degree") ~ "College"
    )
  )
joint <- joint %>%
  group_by(gender, age_acs, edu_acs) %>%
  summarise(estimate = sum(estimate))
joint$Freq <- joint$estimate / sum(joint$estimate)
#Get share male
male <- joint %>% group_by(gender) %>% summarize(Freq = sum(Freq))
#Get share by age
age <- joint %>% group_by(age_acs) %>% summarize(Freq = sum(Freq))
#Get share education
edu <- joint %>% group_by(edu_acs) %>% summarize(Freq = sum(Freq))
#Joint distribution of race, age, and sex
totalpop <- subset(acs_long, variable == "B01003_001")
acs.RaceAgeSex <- subset(acs_long, variable %in% RaceAgeSex$name)
acs.RaceAgeSex <- merge(acs.RaceAgeSex, RaceAgeSex, by.x = "variable", by.y = "name")
race_dist <- separate(acs.RaceAgeSex, col = label, into = c("estimate2", "total", "gender", "age_acs"), sep = "!!") %>%
  dplyr::select(-c(variable, GEOID, NAME, moe, estimate2, total, geography))
#set up racial variables
race_dist$race_acs <- stringr::str_to_title(gsub("SEX BY AGE \\(|\\)", "", race_dist$concept))
race_dist$concept <- NULL
##adjust age
race_dist <- race_dist %>%
  mutate(
    age_acs = case_when(
      age_acs %in% c("18 and 19 years", "20 to 24 years") ~ "18 to 24 years",
      age_acs %in% c("25 to 29 years", "30 to 34 years") ~ "25 to 34 years",
      age_acs %in% c("35 to 44 years") ~ "35 to 44 years",
      age_acs %in% c("45 to 54 years", "55 to 64 years") ~ "45 to 64 years",
      age_acs %in% c("65 to 74 years", "75 to 84 years", "85 years and over") ~ "65 years and over",
      T ~ NA_character_
    )
  )
race_dist <- subset(race_dist, !is.na(age_acs))
race_dist <- race_dist %>%
  mutate(
    race_acs = case_when(
      race_acs %in% c("Native Hawaiian And Other Pacific Islander Alone",
                      "Some Other Race Alone", "Two Or More Races",
                      "American Indian And Alaska Native Alone",
                      "Native Hawaiian And Other Pacific Islander Alone") ~ "Other",
      race_acs == "Black Or African American Alone" ~ "Black or African American Alone",
      T ~ as.character(race_acs)
    )
  )
race_dist <- race_dist %>%
  group_by(gender, age_acs, race_acs) %>%
  summarise(estimate = sum(estimate))
race_dist$Freq <- race_dist$estimate / sum(race_dist$estimate)
race <- race_dist %>% group_by(race_acs) %>% summarize(Freq = sum(Freq))
#prepare income distribution
acs_income <- subset(acs_long, variable %in% income$name)
acs_income <- merge(acs_income, income, by.x = "variable", by.y = "name")
income_dist <- separate(acs_income, col = label, into = c("estimate2", "total", "income"), sep = "!!")
income_dist <- subset(income_dist, select = -c(concept, estimate2, total, geography, NAME, GEOID))
income_dist <- income_dist %>%
  mutate(
    income_acs = case_when(
      income %in% c("Less than $10,000", "$10,000 to $14,999", "$15,000 to $19,999", "$20,000 to $24,999", "$25,000 to $29,999") ~ "Q1",
      income %in% c("$30,000 to $34,999", "$35,000 to $39,999", "$40,000 to $44,999", "$45,000 to $49,999", "$50,000 to $59,999") ~ "Q2",
      income %in% c("$60,000 to $74,999", "$75,000 to $99,999") ~ "Q3",
      income %in% c("$100,000 to $124,999", "$125,000 to $149,999") ~ "Q4",
      income %in% c("$150,000 to $199,999", "$200,000 or more") ~ "Q5",
      T ~ as.character(income)
    )
  )
income_dist <- income_dist %>%
  group_by(income_acs) %>%
  summarise(estimate = sum(estimate))
income_dist$Freq <- income_dist$estimate / sum(income_dist$estimate)
