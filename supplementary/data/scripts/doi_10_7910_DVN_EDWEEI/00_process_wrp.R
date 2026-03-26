# Alexander F. Gazmararian
# agazmararian@gmail.com

library(here)
library(janitor)
library(readxl)
library(tidyverse)

g <- read_xlsx(here("Data", "input", "survey", "worldriskpoll", "2019", "data", "19_wrp_original.xlsx"))

# WRP later associated the original WRP with other variables collected by Gallup;
# but retained the original survey because not all of the variables in the supplement are also
# in the original, such as household size
gs <- read.csv(here("Data", "input", "survey", "worldriskpoll", "2019", "data", "19_wrp_supplement.csv"))

# Pull urban, occupation, and employment data
gs <- gs %>%
  dplyr::rename(
    urban = WP14,
    emp = EMP_2010,
    occ = EMP8B
  )
gs <- subset(gs, select = c(WPID_RANDOM, urban, emp, occ))
g <- dplyr::left_join(g, gs, by = "WPID_RANDOM")
g <- clean_names(g)

# Covariates----
# Age
g$age_year <- 2019 - g$age
g <- g %>%
  dplyr::mutate(
    age_gen = factor(
      case_when(
        age_year >= 1997 ~ "Gen Z",
        age_year >= 1981 & age_year < 1997 ~ "Millennial",
        age_year >= 1965 & age_year < 1981 ~ "Gen X",
        age_year >= 1946 & age_year < 1965 ~ "Boomer",
        age_year < 1946 ~ "Silent"
      )
    )
  )
summary(g$age_gen)

# Sex
g$female <- factor(ifelse(g$gender == 2, "Female", "Male"))
summary(g$female)

# Education
g <- g %>% dplyr::rename(edu = education)
g <- g %>%
  dplyr::mutate(
    edu = factor(
      case_when(
        edu == 1 ~ "Primary",
        edu == 2 ~ "Secondary",
        edu == 3 ~ "Tertiary",
        edu %in% c(4, 5) ~ "Other"
      )
    )
  )

# Urban
g$urbanicity <- NULL
g$urban <- case_when(g$urban %in% c("(DK)", "(Refused)") ~ "Other", T ~ g$urban)
g$urban <- factor(g$urban)
g$rural <- as.integer(g$urban == "A rural area or on a farm")

# Household size
g <- g %>%
  dplyr::rename(hhsize = household_size) %>%
  dplyr::mutate(
    hhsize = factor(
      case_when(
        hhsize == 1 ~ "1-2",
        hhsize == 2 ~ "3-4",
        hhsize == 3 ~ "5-9",
        hhsize == 4 ~ "10 or more"
      )
    )
  )
summary(g$hhsize)

summary(factor(g$children_in_household))

g <- g %>%
  dplyr::rename(kids = children_in_household) %>%
  dplyr::mutate(kids = factor(kids))
g <- g %>%
  dplyr::mutate(
    kids = factor(
      case_when(
        kids == 0 ~ "None",
        kids == 1 ~ "1",
        kids == 2 ~ "2",
        kids == 3 ~ "3",
        kids == 4 ~ "4",
        kids == 5 ~ "5+",
        kids == 9 ~ "DK/Refused",
        T ~ NA_character_
      )
    )
  )

# Impute kids for Rwanda based on household size
g <- g %>%
  mutate(
    kids = factor(
      case_when(
        country == "Rwanda" & hhsize == "1-2" ~ "None",
        country == "Rwanda" & hhsize == "3-4" ~ "1",
        country == "Rwanda" & hhsize == "5-9" ~ "3",
        country == "Rwanda" & hhsize == "10 or more" ~ "5+",
        T ~ kids
      )
    )
  )
summary(g$kids)
g <- g %>%
  dplyr::mutate(
    kids_bin = ifelse(kids %in% c("1", "2", "3", "4", "5+"), 1, 0),
    kidsn = case_when(
      kids == "1" ~ 1,
      kids == "2" ~ 2,
      kids == "3" ~ 3,
      kids == "4" ~ 4,
      kids == "5+" ~ 5,
      kids == "DK/Refused" | kids == "None" ~ 0
    )
  )

# Risk understanding
g <- g %>%
  dplyr::rename(riskund = l1) %>%
  dplyr::mutate(
    riskund = factor(
      case_when(
        riskund == 1 ~ "Opportunity",
        riskund == 2 ~ "Danger",
        riskund == 3 ~ "OpportunityAndDanger",
        riskund == 4 ~ "Neither",
        riskund %in% c(98, 99) ~ "other"
      )
    )
  )
summary(g$riskund)

# Internet access
g <- g %>%
  dplyr::rename(internet = l26) %>%
  dplyr::mutate(internet = ifelse(internet == 1, 1, 0))
summary(g$internet)

# Global region
g <- g %>%
  dplyr::rename(globalreg = global_region) %>%
  dplyr::mutate(
    globalreg = factor(
      case_when(
        globalreg == 1 ~ "Eastern Africa",
        globalreg == 2 ~ "Middle/Western Africa",
        globalreg == 3 ~ "Northern Africa",
        globalreg == 4 ~ "Southern Africa",
        globalreg == 5 ~ "Latin America & Caribbean",
        globalreg == 6 ~ "Northern America",
        globalreg == 7 ~ "Central Asia",
        globalreg == 8 ~ "Eastern Asia",
        globalreg == 9 ~ "South-eastern Asia",
        globalreg == 10 ~ "Southern Asia",
        globalreg == 11 ~ "Middle East",
        globalreg == 12 ~ "Eastern Europe",
        globalreg == 13 ~ "Northern/Western Europe",
        globalreg == 14 ~ "Southern Europe",
        globalreg == 15 ~ "Australia & New Zealand"
      )
    )
  )
summary(g$globalreg)

# WDI country income level
g <- g %>%
  dplyr::rename(dev_lvl = country_income_level) %>%
  dplyr::mutate(
    dev_lvl = factor(
      case_when(
        dev_lvl == 1 ~ "Low income",
        dev_lvl == 2 ~ "Lower middle income",
        dev_lvl == 3 ~ "Upper middle income",
        dev_lvl == 4 ~ "High income"
      )
    )
  )

# Income feeling
g <- g %>%
  dplyr::rename(incfeel = income_feelings) %>%
  dplyr::mutate(
    incfeel = factor(
      case_when(
        incfeel == 1 ~ "Comfortable",
        incfeel == 2 ~ "Getting by",
        incfeel == 3 ~ "Difficult",
        incfeel == 4 ~ "Very difficult",
        incfeel %in% c(5, 6) ~ "Other"
      )
    )
  )
summary(g$incfeel)

g <- g %>%
  dplyr::mutate(
    income_5 = factor(
      case_when(
        income_5 == 1 ~ "Poorest",
        income_5 == 2 ~ "Second",
        income_5 == 3 ~ "Middle",
        income_5 == 4 ~ "Fourth",
        income_5 == 5 ~ "Richest"
      )
    )
  )
summary(g$income_5)

# Occupation
g$occ_vuln <- as.integer(g$occ %in% c("Farmer/Farm worker/Fisherman/Other agricultural laborer"))
summary(g$occ_vuln)

# Risk aversion
g$riskaverse <- as.integer(g$l10 == "1")
summary(g$riskaverse)

# Outcomes----
g <- g %>%
  dplyr::rename(toprisk = l3_a) %>%
  dplyr::mutate(
    toprisk = case_when(
      toprisk == 1 ~ "Road-related accidents/injuries",
      toprisk == 2 ~ "Other transportation-related accidents/injuries",
      toprisk == 3 ~ "Crime/violence/terrorism",
      toprisk == 4 ~ "Cooking or other household accidents/injuries",
      toprisk == 5 ~ "Financial",
      toprisk == 6 ~ "Economy-related",
      toprisk == 7 ~ "Politics/political situation/corruption",
      toprisk == 8 ~ "Internet/technology",
      toprisk == 9 ~ "Health: personal health condition/illness",
      toprisk == 10 ~ "Health: drugs, alcohol, smoking",
      toprisk == 11 ~ "Water supply or drinking unclean water",
      toprisk == 12 ~ "Food-related",
      toprisk == 13 ~ "Pollution",
      toprisk == 14 ~ "Work-related accidents",
      toprisk == 15 ~ "Mental stress/exhaustion",
      toprisk == 16 ~ "Climate change, natural disasters or weather-related events",
      toprisk == 17 ~ "Drowning",
      toprisk == 18 ~ "Other",
      toprisk == 19 ~ "Nothing/No risks",
      toprisk == 98 ~ "Don't Know",
      toprisk == 99 ~ "Refused"
    ),
    toprisk = factor(toprisk)
  )
summary(g$toprisk)

g <- g %>%
  dplyr::rename(secondrisk = l3_b) %>%
  dplyr::mutate(
    secondrisk = case_when(
      secondrisk == 1 ~ "Road-related accidents/injuries",
      secondrisk == 2 ~ "Other transportation-related accidents/injuries",
      secondrisk == 3 ~ "Crime/violence/terrorism",
      secondrisk == 4 ~ "Cooking or other household accidents/injuries",
      secondrisk == 5 ~ "Financial",
      secondrisk == 6 ~ "Economy-related",
      secondrisk == 7 ~ "Politics/political situation/corruption",
      secondrisk == 8 ~ "Internet/technology",
      secondrisk == 9 ~ "Health: personal health condition/illness",
      secondrisk == 10 ~ "Health: drugs, alcohol, smoking",
      secondrisk == 11 ~ "Water supply or drinking unclean water",
      secondrisk == 12 ~ "Food-related",
      secondrisk == 13 ~ "Pollution",
      secondrisk == 14 ~ "Work-related accidents",
      secondrisk == 15 ~ "Mental stress/exhaustion",
      secondrisk == 16 ~ "Climate change, natural disasters or weather-related events",
      secondrisk == 17 ~ "Drowning",
      secondrisk == 18 ~ "Other",
      secondrisk == 19 ~ "Nothing/No risks",
      secondrisk == 98 ~ "Don't Know",
      secondrisk == 99 ~ "Refused"
    ),
    secondrisk = factor(secondrisk)
  )
summary(g$secondrisk)

g <- g %>%
  dplyr::rename(climaterisk = l5) %>%
  dplyr::mutate(
    climaterisk = case_when(
      climaterisk == 1 ~ "Very serious threat",
      climaterisk == 2 ~ "Somewhat serious threat",
      climaterisk == 3 ~ "Not a threat at all",
      climaterisk == 98 ~ "Don't know",
      climaterisk == 99 ~ "Refused"
    ),
    climaterisk = factor(climaterisk),
    climaterisk_scale = case_when(
      climaterisk %in% c("Not a threat all") ~ 0,
      climaterisk == "Somewhat serious threat" ~ 1,
      climaterisk == "Very serious threat" ~ 3
    )
  )
summary(g$climaterisk)

g <- g %>%
  dplyr::rename(weatherworry = l6d) %>%
  dplyr::mutate(
    weatherworry = case_when(
      weatherworry == 1 ~ "Very worried",
      weatherworry == 2 ~ "Somewhat worried",
      weatherworry == 3 ~ "Not worried",
      weatherworry == 98 ~ "Don't know",
      weatherworry == 99 ~ "Refused"
    ),
    weatherworry = factor(weatherworry)
  )

g <- g %>%
  dplyr::rename(weatherlikely = l7d) %>%
  dplyr::mutate(
    weatherlikely = case_when(
      weatherlikely == 1 ~ "Very likely",
      weatherlikely == 2 ~ "Somewhat likely",
      weatherlikely == 3 ~ "Not likely at all",
      weatherlikely == 98 ~ "Don't know",
      weatherlikely == 99 ~ "Refused"
    ),
    weatherlikely = factor(weatherlikely)
  )
summary(g$weatherlikely)

g <- g %>%
  dplyr::rename(exp_weather = l8d) %>%
  dplyr::mutate(
    exp_weather = case_when(
      exp_weather == 1 ~ "Yes",
      exp_weather == 2 ~ "No",
      exp_weather == 98 ~ "Don't know",
      exp_weather == 99 ~ "Refused"
    ),
    exp_weather = factor(exp_weather)
  )
summary(g$exp_weather)
g <- g %>%
  dplyr::mutate(
    exp_weather_i = case_when(
      exp_weather == "Yes" ~ 1,
      exp_weather %in% c("Don't know", "No") ~ 0,
      T ~ NA_real_
    )
  )


# other types of experience
g$exp_food <- as.integer(g$l8a == 1)
g$exp_water <- as.integer(g$l8b == 1)
g$exp_crime <- as.integer(g$l8c == 1)
g$exp_mental <- as.integer(g$l8g == 1)


g <- g %>%
  dplyr::mutate(
    weatherlikely_scale = case_when(
      weatherlikely == "Don't know" ~ 1,
      weatherlikely == "Not likely at all" ~ 2,
      weatherlikely == "Somewhat likely" ~ 3,
      weatherlikely == "Very likely" ~ 4,
      T ~ NA_real_
    ),
    weatherworry_scale = case_when(
      weatherworry == "Don't know" ~ 1,
      weatherworry == "Not worried" ~ 2,
      weatherworry == "Somewhat worried" ~ 3,
      weatherworry == "Very worried" ~ 4,
      T ~ NA_real_
    )
  )

g$weatherworry_i <- with(g, ifelse(weatherworry %in% c("Somewhat worried", "Very worried"), 1, 0))
g$weatherlikely_i <- with(g, ifelse(weatherlikely %in% c("Somewhat likely", "Very likely"), 1, 0))

# create a measure for if climate change is greatest risk or major concern
g$salient <- with(g, ifelse(toprisk %in% "Climate change, natural disasters or weather-related events", 1, 0))
g[g$salient == 0 & g$secondrisk %in% "Climate change, natural disasters or weather-related events", ]$salient <- 1
summary(g$salient)

# top threat
g$climatetop <- ifelse(g$toprisk == "Climate change, natural disasters or weather-related events", 1, 0)
g$climaterisk_i <- ifelse(g$climaterisk == "Very serious threat", 1, 0)

# code placebos
g$placebo_house <- with(g, ifelse(toprisk == "Cooking or other household accidents/injuries", 1, 0))
g$placebo_road <- with(g, ifelse(toprisk == "Road-related accidents/injuries", 1, 0))
g$placebo_work <- with(g, ifelse(toprisk == "Work-related accidents", 1, 0))
g$placebo_politics <- with(g, ifelse(toprisk == "Politics/political situation/corruption", 1, 0))
g$placebo_addiction <- as.integer(g$toprisk == "Health: drugs, alcohol, smoking")
g$placebo_tech <- as.integer(g$toprisk == "Internet/technology")

g$placebo_work2 <- as.integer(g$toprisk == "Work-related accidents")
g$placebo_work2[g$secondrisk == "Work-related accidents"] <- 1

g$placebo_tech2 <- as.integer(g$toprisk == "Internet/technology")
g$placebo_tech2[g$secondrisk == "Internet/technology"] <- 1

g$placebo_addiction2 <- as.integer(g$toprisk == "Health: drugs, alcohol, smoking")
g$placebo_addiction2[g$secondrisk == "Health: drugs, alcohol, smoking"] <- 1

g$placebo_politics2 <- as.integer(g$toprisk == "Politics/political situation/corruption")
g$placebo_politics2[g$secondrisk == "Politics/political situation/corruption"] <- 1

g$placebo_house2 <- as.integer(g$toprisk == "Cooking or other household accidents/injuries")
g$placebo_house2[g$secondrisk == "Cooking or other household accidents/injuries"] <- 1

g$placebo_drowning2 <- as.integer(g$toprisk == "Drowning")
g$placebo_drowning2[g$secondrisk == "Drowning"] <- 1

g$placebo_road2 <- as.integer(g$toprisk == "Road-related accidents/injuries")
g$placebo_road2[g$secondrisk == "Road-related accidents/injuries"] <- 1

g$placebo_transport2 <- as.integer(g$toprisk == "Other transportation-related accidents/injuries")
g$placebo_transport2[g$secondrisk == "Other transportation-related accidents/injuries"] <- 1

g$placebo_ai <- as.integer(g$l4c == 1)
g$placebo_powerline_worry <- as.integer(g$l6e == 1)
g$placebo_appliance_worry <- as.integer(g$l6f == 1)


# Belief that nuclear power will help
g$nuclear_good <- as.integer(g$l4b == "1")
summary(g$nuclear_good)

# Climate change knowledge measure
table(g$climaterisk)
g$cc_know <- as.integer(g$climaterisk %in% c("Don't know", "Refused"))

# Trust in in doctors/science
g$trustdocs <- as.integer(g$l13b == "1")
summary(g$trustdocs)

# Support for government regulations
g$publicgoods <- with(g, l16a + l16b + l16c)
summary(g$publicgoods) # No Saudi Arabia

# Social capital:
# Neighbor
g$sc_neighbor <- case_when(
  g$l17a == 1 ~ "Very likely", g$l17a == 2 ~ "Somewhat likely",
  g$l17a == 3 ~ "Not very likely", g$l17a %in% c(98, 99) ~ "DK/Refused"
)
# Stranger
g$sc_stranger <- case_when(
  g$l17b == 1 ~ "Very likely", g$l17b == 2 ~ "Somewhat likely",
  g$l17b == 3 ~ "Not very likely", g$l17b %in% c(98, 99) ~ "DK/Refused"
)
# Create index
g$socialcapital <- as.integer(g$l17a == 1) + as.integer(g$l17b == 1)
summary(g$socialcapital)

saveRDS(g, here("Data", "inter", "19_wrp", "wrp_processed.rds"))
