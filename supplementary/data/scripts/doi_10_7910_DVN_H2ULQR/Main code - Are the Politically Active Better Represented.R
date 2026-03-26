##### Are the Politically Active Better Represented? #####

### Read Me ###
# This code produces the findings from the paper "Are the Politically Active Better Represented?," and is structured as follows.
# (1) Initial Setup: Loads packages and data, and sets the working directory.
# (2) Data Preparation: Re-codes and organizes data from four different surveys/survey programmes.
# (3) Analyses in the Paper: Produces the findings in the paper.
# (4) Appendix Analyses: Produces the findings in the appendix.
# The appendix analyses are now in a separate script, but this script needs to be run first (at least parts (1) and (2)).

# To run the code, the reader will need to set a working directory, in which there needs to be a folder named "figures".
# There should also be a folder named "data" containing the data sets needed for the analyses.
# Some of the data, the ESS and ISSP files, must be downloaded separately.
# We have documented website and download date, for you to download the same data.

### (1) Initial Setup ###

# Set working directory
# setwd("")

# Load packages
library(rio)
library(lme4)
library(stargazer)
library(foreign)
library(haven)
library(psych)
library(car)
library(readxl)
library(tidyverse)
library(beepr)
library(ggrepel)
library(broom)
library(ggpubr)
library(writexl)
library(wesanderson)
library(boot)
library(lme4)

## Load data

# Load implementation data and question wording
impdata_slim <- read_excel("data/impdata_2025.xlsx")
questionwordings <- read_xlsx("data/questionwordings_2025.xlsx")

# ESS
ess <- import("data/ESS/ESS1e06_7-ESS2e03_6-ESS3e03_7-ESS4e04_6-ESS5e03_5-ESS7e02_3-ESS8e02_3.dta")
# Download date: 12 January, 2024
# URL: https://ess.sikt.no/en/data-builder/?tab=round_country&rounds=0+1+2+3+4+6+7&seriesVersion=775
# ess_original <- ess # Keep the original version so that it does not have to loaded again (if necessary to return to the original survey material)
# ess <- ess_original

# ISSP Role of Government 1996 
ISSPRoGIII <- import("data/ISSP/ISSP RoG III 1996/ZA2900.dta")
#Download date: 10 October, 2023
#URL: https://search.gesis.org/research_data/ZA2900

# ISSP CITIZEN I
ISSPCitizenI <- import("data/ISSP/ISSP Citizen 2004/ZA3950_v1-3-0.dta")
# Download date: 20 November, 2023
# URL: https://search.gesis.org/research_data/ZA3950

# ISSP CITIZEN II
ISSPCitizenII <- import("data/ISSP/ISSP Citizen 2014/ZA6670_v2-0-0.dta/ZA6670_v2-0-0.dta")
# Download date: 22 November, 2023
# URL: https://search.gesis.org/research_data/ZA6670

### End of Initial Setup ###

### (2) Data Preparation ###

#### ESS
{

## Socio-demographic control variables and political attitudes
# Recoding of variables
ess <- ess |>
  mutate(income1 = case_when(hinctnt == 1 ~ 900, hinctnt == 2 ~ 2700, hinctnt == 3 ~ 4800,
                             hinctnt == 4 ~ 9000, hinctnt == 5 ~ 15000, hinctnt == 6 ~ 21000,
                             hinctnt == 7 ~ 27000, hinctnt == 8 ~ 33000, hinctnt == 9 ~ 48000,
                             hinctnt == 10 ~ 75000, hinctnt == 11 ~ 105000, hinctnt == 12 ~ 120000,
                             TRUE ~ as.double(NA)),# changing income variable to actual real values (only first income variable), but see later transformation below
         woman = gndr - 1, # woman
         AgeCategories = case_when(agea < 30 ~ "Under 30", 
                                   agea >= 30 & agea <= 39 ~ "30 to 39",
                                   agea >= 40 & agea <= 49 ~ "40-49",
                                   agea >= 50 & agea <= 59 ~ "50-59",
                                   agea >= 60 & agea <= 69 ~ "60-69", 
                                   agea >= 70 ~ "70 and older",
                                   TRUE ~ NA), # age categories
         rural = case_when(domicil == 1 ~ 0,
                           domicil == 2 ~ 0,
                           domicil == 3 ~ 0,
                           domicil == 4 ~ 1,
                           domicil == 5 ~ 1,
                           TRUE ~ as.double(NA)), # rural
         religiosity = rlgdgr, # how religious is the individual
         attend_rel_onceamonth = case_when(rlgatnd == 1 ~ 1,
                                           rlgatnd == 2 ~ 1,
                                           rlgatnd == 3 ~ 1,
                                           rlgatnd == 4 ~ 1,
                                           rlgatnd == 5 ~ 0,
                                           rlgatnd == 6 ~ 0,
                                           rlgatnd == 7 ~ 0,
                                           TRUE ~ NA), # how often attend religious institution 2
         areligious = rlgblg - 1, # non-religious
         ethnic_minority = case_when(blgetmg == 1 ~ 1,
                                     blgetmg == 2 ~ 0,
                                     TRUE ~ as.double(NA)), # ethnic minority
         EducationLevel = case_when(eisced == 1 ~ "1 Lower Secondary or Less",
                                    eisced == 2 ~ "1 Lower Secondary or Less",
                                    eisced == 3 ~ "Upper Secondary or Vocational",
                                    eisced == 4 ~ "Upper Secondary or Vocational",
                                    eisced == 5 ~ "Upper Secondary or Vocational",
                                    eisced == 6 ~ "Tertiary",
                                    eisced == 7 ~ "Tertiary",
                                    TRUE ~ NA), # education level
         University = case_when(eisced == 1 ~ 0,
                                eisced == 2 ~ 0,
                                eisced == 3 ~ 0,
                                eisced == 4 ~ 0,
                                eisced == 5 ~ 0,
                                eisced == 6 ~ 1,
                                eisced == 7 ~ 1,
                                TRUE ~ NA), # university education
         lrself  = case_when(lrscale == 0 ~ 1,
                             lrscale == 1 ~ 1.4,
                             lrscale == 2 ~ 1.8,
                             lrscale == 3 ~ 2.2,
                             lrscale == 4 ~ 2.6,
                             lrscale == 5 ~ 3,
                             lrscale == 6 ~ 3.4,
                             lrscale == 7 ~ 3.8,
                             lrscale == 8 ~ 4.2,
                             lrscale == 9 ~ 4.6,
                             lrscale == 10 ~ 5,
                             TRUE ~ NA), # left-right
         Satisfied_w_Dem = case_when(stfdem == 1 ~ 0,
                                     stfdem == 2 ~ 0,
                                     stfdem == 3 ~ 0,
                                     stfdem == 4 ~ 0,
                                     stfdem == 5 ~ 0,
                                     stfdem == 6 ~ 1,
                                     stfdem == 7 ~ 1,
                                     stfdem == 8 ~ 1,
                                     stfdem == 9 ~ 1,
                                     stfdem == 10 ~ 1,
                                     TRUE ~ NA), # satisfaction with democracy
         Pol_efficacy1 = case_when(psppsgv == 0 ~ 0,
                                   psppsgv == 1 ~ 0,
                                   psppsgv == 2 ~ 0,
                                   psppsgv == 3 ~ 0,
                                   psppsgv == 4 ~ 0,
                                   psppsgv == 5 ~ 0,
                                   psppsgv == 6 ~ 1,
                                   psppsgv == 7 ~ 1,
                                   psppsgv == 8 ~ 1,
                                   psppsgv == 9 ~ 1,
                                   psppsgv == 10 ~ 1,
                                   TRUE ~ as.double(NA)),
         Pol_efficacy2 = case_when(psppsgva == 1 ~ 0,
                                   psppsgva == 2 ~ 0,
                                   psppsgva == 3 ~ 0,
                                   psppsgva == 4 ~ 1,
                                   psppsgva == 5 ~ 1,
                                   TRUE ~ as.double(NA)),
         Pol_efficacy = case_when(!is.na(Pol_efficacy1) ~ Pol_efficacy1,
                                  !is.na(Pol_efficacy2) ~ Pol_efficacy2,
                                  TRUE ~ as.double(NA)), # political efficacy
         PolInterested = case_when(polintr == 1 ~ 1,
                                   polintr == 2 ~ 1,
                                   polintr == 3 ~ 0,
                                   polintr == 4 ~ 0,
                                   TRUE ~ as.double(NA)), # interest in politics
         Pol_trust = case_when(trstplt == 0 ~ 0,
                               trstplt == 1 ~ 0,
                               trstplt == 2 ~ 0,
                               trstplt == 3 ~ 0,
                               trstplt == 4 ~ 0,
                               trstplt == 5 ~ 0,
                               trstplt == 6 ~ 1,
                               trstplt == 7 ~ 1,
                               trstplt == 8 ~ 1,
                               trstplt == 9 ~ 1,
                               trstplt == 10 ~ 1,
                               TRUE ~ as.double(NA)) # trust in politicians
  )

table(ess$lrself)

# Generate unified income variable
ess <- ess |> 
  group_by(cntry, essround) |> 
  mutate(income_a_rank = percent_rank(income1)) |> 
  ungroup() |>
  mutate(income_unified = case_when(!is.na(income1) ~ income_a_rank,
                                    !is.na(hinctnta) ~ (hinctnta-1)/9,
                                    TRUE ~ as.double(NA)))

## Political participation
# Recoding of variables

# Create a vector of political participation variables
partvar_vec <- c("contact",
                 "polwork",
                 "otherwork",
                 "wornbadge",
                 "petition",
                 "protest",
                 "boycott",
                 "ethcon",
                 "donated",
                 "illegalprotest")

policy_varname_vec <- c("disclaw", "anycrime", "seriouscrime", "yesrefugeefamily", 
                        "hatelaw", "noownschools", "supportrefapp", "refappcanwork", 
                        "ifunemployedleave", "overthrowdem", "keepterrorsuspect", "paidleavefamilysick", 
                        "higherearnershigherunemp", "higherearnershigherpen", "benefitsonlyforpoor",
                        "basicincomescheme", "banappliances", "fossilfueltaxincrease", "subrenew")

# Create new names/variables for political participation variables
ess <- ess |>
  rename(contact = contplt,
         polwork = wrkprty,
         otherwork = wrkorg,
         wornbadge = badge,
         petition = sgnptit,
         protest = pbldmn,
         boycott = bctprd,
         ethcon = bghtprd,
         donated = dntmny,
         illegalprotest = ilglpst)

# Recode political participation variables
ess <- ess %>%  
  mutate(across(.cols = partvar_vec, ~ case_when(.x == 2 ~ 0,
                                                 .x == 1 ~ 1,
                                                 TRUE ~ as.double(NA))))
# Code a summary variable for general participation
ess <- ess %>%  
  mutate(polparticipation = rowMeans(dplyr::select(., all_of(partvar_vec)), na.rm=TRUE),
         polparticipation_dum = case_when(polparticipation > 0 ~ 1,
                                          polparticipation == 0 ~ 0,
                                          TRUE ~ as.double(NA))) # create a general participation variable

# Code a vote variable
ess <- ess |> 
  mutate(voted = case_when(vote == 3 ~ 0,
                           vote == 2 ~ 0,
                           vote == 1 ~ 1,
                           TRUE ~ as.double(NA)))

# Check mean values of all variables
ess |>
  summarize(across(c(partvar_vec, polparticipation, polparticipation_dum, voted), ~ mean(.x, na.rm=TRUE)))

## Code support for policy questions
ess <- ess |>
  mutate(disclaw = lwdscwp,
         hatelaw = lwpeth,
         paidleavefamilysick = gvpdlwk,
         noownschools = 6-alwspsc,
         seriouscrime = 6-imscrlv,
         anycrime = 6-imacrlv,
         yesrefugeefamily = 6-rfgbfml,
         supportrefapp = 6-rfggvfn,
         refappcanwork = 6-rfgawrk,
         ifunemployedleave = 6-imunplv,
         overthrowdem = 6-prtyban,
         keepterrorsuspect = 6-trrprsn,
         fossilfueltaxincrease = 6-inctxff,
         subrenew = 6-sbsrnen,
         banappliances = 6-banhhap,
         benefitsonlyforpoor = bnlwinc,
         basicincomescheme = basinc,
         higherearnershigherpen = case_when(earnpen == 1 ~ 1,
                                            earnpen == 2 ~ 0.5,
                                            earnpen == 3 ~ 0,
                                            earnpen == 4 ~ as.double(NA),
                                            TRUE ~ as.double(NA)),
         higherearnershigherunemp = case_when(earnueb == 1 ~ 1,
                                              earnueb == 2 ~ 0.5,
                                              earnueb == 3 ~ 0,
                                              earnueb == 4 ~ as.double(NA),
                                              TRUE ~ as.double(NA)))


# Recode to binned variables for policy questions
ess <- ess |> 
  mutate(across(.cols = c(disclaw, # recode questions with 0-10 response options
                          hatelaw,
                          paidleavefamilysick),
                ~ case_when(.x > 5 ~ 1,
                            .x == 5 ~ 0.5,
                            .x < 5 ~ 0,
                            TRUE ~ as.double(NA))),
         across(.cols = c(noownschools, # recode questions with 1-5 response options
                          seriouscrime,
                          anycrime,
                          yesrefugeefamily,
                          supportrefapp,
                          refappcanwork,
                          ifunemployedleave,
                          overthrowdem,
                          keepterrorsuspect,
                          fossilfueltaxincrease,
                          subrenew,
                          banappliances),
                ~ case_when(.x > 3 ~ 1,
                            .x == 3 ~ 0.5,
                            .x < 3 ~ 0,
                            TRUE ~ as.double(NA))),
         across(.cols = c(benefitsonlyforpoor, # recode questions 1-4 response options
                          basicincomescheme),
                ~ case_when(.x >= 3 ~ 1,
                            .x <= 2 ~ 0,
                            TRUE ~ as.double(NA))))


# Create year variable
ess <- ess |> 
  mutate(year = case_match(essround,
                           1 ~ 2002,
                           2 ~ 2004,
                           3 ~ 2006,
                           4 ~ 2008,
                           5 ~ 2010,
                           7 ~ 2014,
                           8 ~ 2016))

# Create survey variable for later data matching
ess <- ess |> 
  mutate(survey = "ESS", # create survey variable for later data matching
         survey2 = "ESS") # this variable separates this survey from the other ones, and allows us to control for each specific survey

# Vector of policy questions
policy_varname_vec <- c("disclaw", "anycrime", "seriouscrime", "yesrefugeefamily", 
                        "hatelaw", "noownschools", "supportrefapp", "refappcanwork", 
                        "ifunemployedleave", "overthrowdem", "keepterrorsuspect", "paidleavefamilysick", 
                        "higherearnershigherunemp", "higherearnershigherpen", "benefitsonlyforpoor",
                        "basicincomescheme", "banappliances", "fossilfueltaxincrease", "subrenew")

# Create ESS with only policy questions
ess <- ess |> 
  mutate(own_id = seq(1, n())) # create id for respondents

ess_onlypolicy <- ess |> 
  dplyr::select(own_id, "country" = cntry, year, all_of(policy_varname_vec), survey, AgeCategories, 
                University, woman, rural, income_unified, PolInterested, Pol_efficacy, Pol_trust, Satisfied_w_Dem, 
                voted, protest, polparticipation, survey2, contact, petition, boycott, donated, polwork, 
                otherwork, wornbadge, ethcon, illegalprotest, ethnic_minority, lrself, religiosity, EducationLevel) # select only relevant variables

# # Include all NAs in analysis - uncomment for the analysis including NAs and don't know answers, which creates Table G1 instead of Table 1
# ess_onlypolicy <- ess_onlypolicy |>
#   mutate(across(.cols = policy_varname_vec, ~ case_when(is.na(.x) ~ 0,
#                                                         TRUE ~ .x)))

# Pivot longer to create a data frame where each policy is the observation (each individual is therefore included several times)
ess_onlypolicy_long <- ess_onlypolicy |> 
  pivot_longer(cols = all_of(policy_varname_vec), names_to = "policyvar")

# Create q_id variable (identification of each policy issue) - which matches with 'q_id' in impdata
ess_onlypolicy_long <- ess_onlypolicy_long |> 
  mutate(q_id = case_match(policyvar,
                           "disclaw"  ~ 107,
                           "anycrime" ~ 186,
                           "seriouscrime" ~ 193,
                           "yesrefugeefamily" ~ 194,
                           "hatelaw" ~ 136,
                           "noownschools" ~ 134,
                           "supportrefapp" ~ 200,
                           "refappcanwork" ~ 205,
                           "ifunemployedleave" ~ 210,
                           "overthrowdem" ~ 164,
                           "keepterrorsuspect" ~ 227,
                           "paidleavefamilysick" ~ 154,
                           "higherearnershigherunemp" ~ 250,
                           "higherearnershigherpen" ~ 239,
                           "benefitsonlyforpoor" ~ 177,
                           "basicincomescheme" ~ 42,
                           "banappliances" ~ 158,
                           "fossilfueltaxincrease" ~ 159,
                           "subrenew" ~ 178))
}


#### ISSP Role of Government 1996 
{

## Recode variables

# Country
ISSPRoGIII <- ISSPRoGIII |>
  mutate(country = case_match(v3, 1 ~ "AU",
                              2 ~ "DE",
                              3 ~ "DE",
                              4 ~ "GB",
                              5 ~ NA,
                              6 ~ "US",
                              7 ~ "AT",
                              8 ~ "HU",
                              9 ~ "IT", 
                              10 ~ "IE",
                              11 ~ "NL",
                              12 ~ "NO",
                              13 ~ "SE",
                              14 ~ "CZ",
                              15 ~ "SI",
                              16 ~ "PL",
                              17 ~ "BG",
                              18 ~ NA,
                              19 ~ "NZ",
                              20 ~ "CA",
                              21 ~ "PH",
                              22 ~ "IL",
                              23 ~ "IL",
                              24 ~ "JP",
                              25 ~ "ES",
                              26 ~ "LV",
                              27 ~ "FR",
                              28 ~ "CY",
                              30 ~ "CH"))

## Socio-demographic control variables
# Income
for (i in 1:length(unique(ISSPRoGIII$country))) { # to check the incomes reported in each country
  df <- subset(ISSPRoGIII, country == paste(unique(ISSPRoGIII$country)[i]))
  print(paste(unique(ISSPRoGIII$country)[i]))
}

ISSPRoGIII$v217 <- ifelse(ISSPRoGIII$country == "NO", # here we change observations that do not indicate actual income data - to numbers that do
                          ifelse(ISSPRoGIII$v217 == 999996, 1000000, ISSPRoGIII$v217),
                          ifelse(ISSPRoGIII$country == "SI",
                                 ifelse(ISSPRoGIII$v217 == 999996, 999000 , ISSPRoGIII$v217),
                                 ISSPRoGIII$v217))
ISSPRoGIII$v217 <- ifelse(ISSPRoGIII$v217 == 999997 | ISSPRoGIII$v217 == 999998 | 
                            ISSPRoGIII$v217 == 999999, NA, ISSPRoGIII$v217) # removing don't know answers and the like

ISSPRoGIII <- ISSPRoGIII %>% 
  group_by(country) %>% # an attempt to normalize incomes within each country
  mutate(income_old = ifelse(all(is.na(v217)), NA_real_,
      (v217 - min(v217, na.rm = TRUE)) / (max(v217, na.rm = TRUE) - min(v217, na.rm = TRUE))),
    income_unified = percent_rank(v217)) %>% # dividing respondents by their percentile in the income distribution within the survey population
  ungroup()

# Ethnicity
ethnic_counts <- ISSPRoGIII |> # count the number of people in each ethnic group by country
  group_by(country, v324) |>
  summarise(count = n()) |>
  ungroup()

majority_group <- ethnic_counts |> #check which is the majority group
  group_by(country) |>
  slice_max(order_by = count, n = 1, with_ties = FALSE) |>
  ungroup() |>
  select(country, v324) |>
  rename(majority_ethnic = v324)

ISSPRoGIII <- ISSPRoGIII |> # #count the number of people in each ethnic group by country
  left_join(majority_group, by = "country") |>
  mutate(ethnic_minority = ifelse(v324 != majority_ethnic, 1, 0)) |>
  select(-majority_ethnic)
table(ISSPRoGIII$ethnic_minority) #ethnic minority
table(ISSPRoGIII$ethnic_minority, ISSPRoGIII$country) #Note: We lose several countries when we include this variable

# Other socio-demographic variables, as well as political attitudes
table(ISSPRoGIII$v223)
ISSPRoGIII <- ISSPRoGIII |>
  mutate(woman = v200 - 1, # woman
         AgeCategories = case_when(v201 < 30 ~ "Under 30", 
                                   v201 >= 30 & v201 <= 39 ~ "30 to 39",
                                   v201 >= 40 & v201 <= 49 ~ "40-49",
                                   v201 >= 50 & v201 <= 59 ~ "50-59",
                                   v201 >= 60 & v201 <= 69 ~ "60-69", 
                                   v201 >= 70 ~ "70 and older",
                                   TRUE ~ NA), # age categories
         rural = case_when(v275 == 1 ~ 0,
                           v275 == 2 ~ 0,
                           v275 == 3 ~ 1,
                           TRUE ~ as.double(NA)), # rural
         attend_rel_onceamonth = case_when(v220 == 1 ~ 1,
                                           v220 == 2 ~ 1,
                                           v220 == 3 ~ 1,
                                           v220 == 4 ~ 0,
                                           v220 == 5 ~ 0,
                                           v220 == 6 ~ 0,
                                           TRUE ~ NA), # how often attend religious institution
         EducationLevel = case_when(v205 == 1 ~ "1 Low or still at school",
                                    v205 == 2 ~ "Intermediary secondary completed",
                                    v205 == 3 ~ "Entry requirement for universities",
                                    v205 == 4 ~ "More than higher secondary, less than university",
                                    v205 == 5 ~ "University degree",
                                    TRUE ~ NA), # level of education
         University = case_when(v205 == 1 ~ 0,
                                v205 == 2 ~ 0,
                                v205 == 3 ~ 0,
                                v205 == 4 ~ 0,
                                v205 == 5 ~ 1,
                                TRUE ~ NA), # university education
         lrself  = case_when(v223 == 1 ~ 1,
                             v223 == 2 ~ 2,
                             v223 == 3 ~ 3,
                             v223 == 4 ~ 4,
                             v223 == 5 ~ 5,
                             TRUE ~ NA), # left-right
         Satisfied_w_Dem = case_when(v55 == 1 ~ 1,
                                     v55 == 2 ~ 1,
                                     v55 == 3 ~ 0,
                                     v55 == 4 ~ 0,
                                    TRUE ~ NA), # satisfied with democracy
         Pol_trust = case_when(v53 == 1 ~ 1,
                               v53 == 2 ~ 1,
                               v53 == 3 ~ 0,
                               v53 == 4 ~ 0,
                               v53 == 5 ~ 0,
                               TRUE ~ NA), # trust in politicians
         Pol_efficacy = case_when(v47 == 1 ~ 0,
                                  v47 == 2 ~ 0,
                                  v47 == 3 ~ 0,
                                  v47 == 4 ~ 1,
                                  v47 == 5 ~ 1,
                               TRUE ~ NA), # political efficacy
         PolInterested = case_when(v46 == 1 ~ 1,
                                   v46 == 2 ~ 1,
                                   v46 == 3 ~ 1,
                                   v46 == 4 ~ 0,
                                   v46 == 5 ~ 0,
                                   TRUE ~ as.double(NA)) # interest in politics
  )

table(ISSPRoGIII$lrself)

### Political participation
## Recoding of variables

# Create a vector of political participation variables
partvar_vec <- c("meeting",
                 "protest")

# Create new names/variables for political participation variables
table(ISSPRoGIII$v10)
ISSPRoGIII <- ISSPRoGIII |>
  mutate(meeting = v10,
         protest = v11)

# Recode political participation variables
ISSPRoGIII <- ISSPRoGIII |> 
  mutate(across(.cols = all_of(partvar_vec), ~ case_when(.x == 3 ~ 1,
                                                         .x == 2 ~ 1,
                                                         .x == 1 ~ 0,
                                                         TRUE ~ as.double(NA))))

# Code summary variables for general participation
ISSPRoGIII <- ISSPRoGIII %>% 
  mutate(polparticipation = rowMeans(dplyr::select(., all_of(partvar_vec)), na.rm=TRUE),
         polparticipation_dum = case_when(polparticipation > 0 ~ 1,
                                          polparticipation == 0 ~ 0,
                                          TRUE ~ as.double(NA))) # create a general participation variable

#Vote variable
ISSPRoGIII <- ISSPRoGIII |> 
  mutate(voted = rowSums(across(paste0("v", 249:271), ~. %in% 1:96))) |>
  mutate(voted = if_else(voted == 0, NA_integer_, voted)) |> 
  mutate(voted = if_else(v272 %in% 1:97, 0, voted))

# It should be noted that the data does not allow us to distinguish between voters and non-voters in countries
# that lack this data. Since this part of the code constructs a new variable from other variables,
# we need to remove several countries before conducting analysis of voters and non-voters

table(ISSPRoGIII$voted)
ISSPRoGIII$voted <- ifelse(ISSPRoGIII$country == "AU" | ISSPRoGIII$country == "US" |
                             ISSPRoGIII$country ==  "RU" | ISSPRoGIII$country == "CY" | 
                             ISSPRoGIII$country == "TR", NA_integer_, ISSPRoGIII$voted)
table(ISSPRoGIII$voted)

### Code support for policy questions

# Create new names/variables for policy question variables
ISSPRoGIII <- ISSPRoGIII |>
  rename(cutspending = v19,
         govcreatejobs = v20,
         lessregulation = v21,
         reduceworkweek = v24,
         controlwages = v17,
         healthspending = v26,
         educationspending = v28,
         environmentspending = v25,
         oldagepensionsspending = v30,
         culturespending = v32,
         unemploymentspending = v31,
         militaryspending = v29,
         policespending = v27,
         banksprivate = v62,
         hospitalsprivate = v61,
         electricityprivate = v60)

# Recode support for policy questions - as dichotomous
ISSPRoGIII <- ISSPRoGIII |> 
  mutate(across(.cols = c(cutspending, # recode questions with 1-5 response options
                          govcreatejobs,
                          lessregulation,
                          reduceworkweek,
                          controlwages,
                          healthspending,
                          educationspending,
                          environmentspending,
                          oldagepensionsspending,
                          culturespending,
                          unemploymentspending,
                          militaryspending,
                          policespending),
                ~ case_when(.x == 1 ~ 1,
                            .x == 2 ~ 1,
                            .x == 3 ~ 0.5,
                            .x == 4 ~ 0,
                            .x == 5 ~ 0,
                            TRUE ~ as.double(NA))),
         across(.cols = c(banksprivate, # recode questions with 1-2 response options
                          hospitalsprivate,
                          electricityprivate),
                ~ case_when(.x == 1 ~ 1,
                            .x == 2 ~ 0,
                            TRUE ~ as.double(NA))))

#For later matching
ISSPRoGIII <- ISSPRoGIII |> 
  mutate(survey = "ISSP", # create survey variable for later data matching
         survey2 = "ISSP RoG", # this variable separates this ISSP survey from the other ones, and allows us to control for each specific survey
         year = 1996) # create year variable for later data matching
table(ISSPRoGIII$survey)

# Vector of policy questions
policy_varname_vec <- c("cutspending", "govcreatejobs", "lessregulation", "reduceworkweek", "controlwages",
                        "healthspending", "educationspending", "environmentspending", "oldagepensionsspending",
                        "culturespending", "unemploymentspending", "militaryspending", "policespending",
                        "banksprivate", "hospitalsprivate", "electricityprivate")

# Create ISSPRoGIII with only policy questions - one for each respondent
ISSPRoGIII <- ISSPRoGIII |> 
  mutate(own_id = seq(1, n())) # create id for respondents

ISSPRoGIII_onlypolicy <- ISSPRoGIII  |> 
  dplyr::select(own_id, country, year, all_of(policy_varname_vec), survey, AgeCategories, 
                University, woman, rural, income_unified,PolInterested, Pol_efficacy, Pol_trust, Satisfied_w_Dem, 
                voted, protest, polparticipation, country, survey2, meeting, ethnic_minority, lrself) # select relevant variables

# # Include all NAs in analysis - uncomment for the analysis including NAs and don't know answers, which creates Table G1 instead of Table 1
# ISSPRoGIII_onlypolicy <- ISSPRoGIII_onlypolicy |>
#   mutate(across(.cols = policy_varname_vec, ~ case_when(is.na(.x) ~ 0,
#                                                         TRUE ~ .x)))

# Pivot longer to create a data frame where each policy is the observation (each individual is therefore included several times)
ISSPRoGIII_onlypolicy_long <- ISSPRoGIII_onlypolicy |> 
  pivot_longer(cols = all_of(policy_varname_vec), names_to = "policyvar")

# Create q_id variable - which matches with q_id in impdata
ISSPRoGIII_onlypolicy_long <- ISSPRoGIII_onlypolicy_long |> 
  mutate(q_id = case_match(policyvar,
                           "cutspending" ~ 12,
                           "govcreatejobs" ~ 40,
                           "lessregulation" ~ 26,
                           "reduceworkweek" ~ 32,
                           "controlwages" ~ 184,
                           "healthspending" ~ 218,
                           "educationspending" ~ 221,
                           "environmentspending" ~ 222,
                           "oldagepensionsspending" ~ 224,
                           "culturespending" ~ 231,
                           "unemploymentspending" ~ 233,
                           "militaryspending" ~ 234,
                           "policespending" ~ 237,
                           "banksprivate" ~ 153,
                           "hospitalsprivate" ~ 157,
                           "electricityprivate" ~ 161))
}


#### ISSP CITIZEN I
{

# Country
ISSPCitizenI <- ISSPCitizenI |>
  mutate(country = case_match(COUNTRY, 1 ~ "AU",
                              2 ~ "DE",
                              3 ~ "DE",
                              4 ~ "GB",
                              6 ~ "US",
                              7 ~ "AT",
                              8 ~ "HU",
                              9 ~ "IT",
                              10 ~ "IE",
                              11 ~ "NL",
                              12 ~ "NO",
                              13 ~ "SE",
                              14 ~ "CZ",
                              15 ~ "SI",
                              16 ~ "PL",
                              17 ~ "BG",
                              18 ~ "RU",
                              19 ~ "NZ",
                              20 ~ "CA",
                              21 ~ "PH",
                              22 ~ "IL",
                              24 ~ "JP",
                              25 ~ "ES",
                              26 ~ "LV",
                              27 ~ "SK",
                              28 ~ "FR",
                              29 ~ "CY",
                              30 ~ "PT",
                              31 ~ "CL",
                              32 ~ "DK",
                              33 ~ "CH",
                              34 ~ "BE",
                              35 ~ "BR",
                              36 ~ "VE",
                              37 ~ "FI",
                              38 ~ "MX",
                              39 ~ "TW",
                              40 ~ "ZA",
                              41 ~ "KR",
                              42 ~ "UY"))

## Socio-demographic control variables and political attitudes
for (i in 1:length(unique(ISSPCitizenI$country))) { # to check the incomes reported in each country
  df <- subset(ISSPCitizenI, country == paste(unique(ISSPCitizenI$country)[i]))
  print(paste(unique(ISSPCitizenI$country)[i]))
}

ISSPCitizenI$v254 <- ifelse(ISSPCitizenI$country == "CL", # here we change two observations that do not indicate actual income data - to numbers that do
                            ifelse(ISSPCitizenI$v254 == 999996, 1000001, ISSPCitizenI$v254),
                            ifelse(ISSPCitizenI$country == "VE",
                                   ifelse(ISSPCitizenI$v254 == 999996, 1000000 , ISSPCitizenI$v254), ISSPCitizenI$v254))
ISSPCitizenI <- ISSPCitizenI |> 
  group_by(country) |> 
  mutate(income_unified = percent_rank(v254)) |> 
  ungroup()

#Ethnicity
ethnic_counts <- ISSPCitizenI |> # count the number of people in each ethnic group by country
  group_by(country, v379) |>
  summarise(count = n()) |>
  ungroup()

majority_group <- ethnic_counts |> #check which is the majority group
  group_by(country) |>
  slice_max(order_by = count, n = 1, with_ties = FALSE) |>
  ungroup() |>
  select(country, v379) |>
  rename(majority_ethnic = v379)

ISSPCitizenI <- ISSPCitizenI |> # #count the number of people in each ethnic group by country
  left_join(majority_group, by = "country") |>
  mutate(ethnic_minority = ifelse(v379 != majority_ethnic, 1, 0)) |>
  select(-majority_ethnic)
table(ISSPCitizenI$ethnic_minority) #ethnic minority

# Recode the variables
ISSPCitizenI <- ISSPCitizenI |>
  mutate(woman = v200 - 1, # woman
         AgeCategories = case_when(v201 < 30 ~ "Under 30", 
                                   v201 >= 30 & v201 <= 39 ~ "30 to 39",
                                   v201 >= 40 & v201 <= 49 ~ "40-49",
                                   v201 >= 50 & v201 <= 59 ~ "50-59",
                                   v201 >= 60 & v201 <= 69 ~ "60-69", 
                                   v201 >= 70 ~ "70 and older",
                                   TRUE ~ NA), # age categories
         rural = case_when(v378 == 1 ~ 0,
                           v378 == 2 ~ 0,
                           v378 == 3 ~ 0,
                           v378 == 4 ~ 1,
                           v378 == 5 ~ 1,
                           TRUE ~ as.double(NA)), # rural
         religiosity = case_when(v300 == 1 ~ "Once a month or more",
                                 v300 == 2 ~ "Once a month or more",
                                 v300 == 3 ~ "Once a month or more",
                                 v300 == 4 ~ "Once a month or more",
                                 v300 == 5 ~ "Less than once a month to once a year",
                                 v300 == 6 ~ "Less than once a month to once a year",
                                 v300 == 7 ~ "Less than once a year",
                                 v300 == 8 ~ "Less than once a year",
                                 TRUE ~ NA), # how often attend religious institution
         attend_rel_onceamonth = case_when(v300 == 1 ~ 1,
                                           v300 == 2 ~ 1,
                                           v300 == 3 ~ 1,
                                           v300 == 4 ~ 1,
                                           v300 == 5 ~ 0,
                                           v300 == 6 ~ 0,
                                           v300 == 7 ~ 0,
                                           v300 == 8 ~ 0,
                                           TRUE ~ as.double(NA)), # how often attend religious institution
         areligious = case_when(V27 == 1 ~ 0,
                                V27 == 2 ~ 0,
                                V27 == 3 ~ 1,
                                V27 == 4 ~ 1,
                                TRUE ~ as.double(NA)), # non-religious
         EducationLevel = case_when(v205 == 1 ~ "1 Low or still at school",
                                    v205 == 2 ~ "Intermediary secondary completed",
                                    v205 == 3 ~ "Entry requirement for universities",
                                    v205 == 4 ~ "More than higher secondary, less than university",
                                    v205 == 5 ~ "University degree",
                                    TRUE ~ NA), # level of education
         University = case_when(v205 == 1 ~ 0,
                                v205 == 2 ~ 0,
                                v205 == 3 ~ 0,
                                v205 == 4 ~ 0,
                                v205 == 5 ~ 1,
                                TRUE ~ as.double(NA)), # university education
         lrself = case_when(v258 == 1 ~ 1,
                            v258 == 2 ~ 2,
                            v258 == 3 ~ 3,
                            v258 == 4 ~ 4,
                            v258 == 5 ~ 5,
                            TRUE ~ as.double(NA)), # university education
         Satisfied_w_Dem = case_when(V60 == 1 ~ 0,
                                     V60 == 2 ~ 0,
                                     V60 == 3 ~ 0,
                                     V60 == 4 ~ 0,
                                     V60 == 5 ~ 0,
                                     V60 == 6 ~ 1,
                                     V60 == 7 ~ 1,
                                     V60 == 8 ~ 1,
                                     V60 == 9 ~ 1,
                                     V60 == 10 ~ 1,
                                     TRUE ~ as.double(NA)), # satisfaction with democracy
         Pol_efficacy = case_when(V36 == 1 ~ 0,
                                  V36 == 2 ~ 0,
                                  V36 == 3 ~ 0,
                                  V36 == 4 ~ 1,
                                  V36 == 5 ~ 1,
                                  TRUE ~ as.double(NA)), # political efficacy
         Pol_trust = case_when(V43 == 1 ~ 1,
                               V43 == 2 ~ 1,
                               V43 == 3 ~ 0,
                               V43 == 4 ~ 0,
                               V43 == 5 ~ 0,
                               TRUE ~ as.double(NA)), # political trust
         PolInterested = case_when(V42 == 1 ~ 1,
                                   V42 == 2 ~ 1,
                                   V42 == 3 ~ 0,
                                   V42 == 4 ~ 0,
                                   TRUE ~ as.double(NA)) # interest in politics
  )


## Political participation
# Recoding of variables

# Create a vector of political participation variables
partvar_vec <- c("contact",
                 "attendedmeeting",
                 "petition",
                 "protest",
                 "boycott",
                 "donated",
                 "internetforum",
                 "contactmedia")
meet_demon_vec <- c("attendedmeeting",
                    "protest")

# Create new names/variables for political participation variables
ISSPCitizenI <- ISSPCitizenI |>
  rename(contact = V21,
         attendedmeeting = V20,
         petition = V17,
         protest = V19,
         boycott = V18,
         donated = V22,
         internetforum = V24,
         contactmedia = V23)

# Recode political participation variables
ISSPCitizenI <- ISSPCitizenI |> 
  mutate(across(.cols = all_of(partvar_vec), ~ case_when(.x == 4 ~ 0,
                                                         .x == 3 ~ 0,
                                                         .x == 2 ~ 0,
                                                         .x == 1 ~ 1,
                                                         TRUE ~ as.double(NA))))

# Code summary variables for general participation
ISSPCitizenI <- ISSPCitizenI %>%
  mutate(polparticipation = rowMeans(dplyr::select(., all_of(partvar_vec)), na.rm=TRUE),
         polparticipation_dum = case_when(polparticipation > 0 ~ 1,
                                          polparticipation == 0 ~ 0,
                                          TRUE ~ as.double(NA)), # create a general participation variable
         meetorprotest = rowMeans(dplyr::select(., all_of(meet_demon_vec)), na.rm=TRUE),
         polparticipation_meetorprotest = case_when(meetorprotest > 0 ~ 1,
                                                    meetorprotest == 0 ~ 0,
                                                    TRUE ~ as.double(NA))) # create a protest or meeting participation variable


# Vote variable
ISSPCitizenI <- ISSPCitizenI |> 
  mutate(voted = case_when(v297 == 2 ~ 0,
                           v297 == 1 ~ 1,
                           TRUE ~ as.double(NA)))

## Code support for policy questions

# Create new names/variables for policy question variables
ISSPCitizenI <- ISSPCitizenI |>
  mutate(RacialPrejMeeting = V16,
         OverthrowGovMeeting = V15,
         ReligExtMeeting = V14)

# Recode support for policy questions - as dichotomous
ISSPCitizenI <- ISSPCitizenI |> 
  mutate(across(.cols = c(RacialPrejMeeting,
                          OverthrowGovMeeting,
                          ReligExtMeeting),
                ~ case_when(.x >= 3 ~ 0,
                            .x <= 2 ~ 1,
                            TRUE ~ as.double(NA))))

#For later matching
ISSPCitizenI <- ISSPCitizenI |> 
  mutate(survey = "ISSP",           # create survey variable for later data matching
         survey2 = "ISSP Citizen I", # this variable separates this ISSP survey from the other ones, and allows us to control for each specific survey
         year = 2004) # create year variable for later data matching

# Vector of policy questions
policy_varname_vec <- c("RacialPrejMeeting", "OverthrowGovMeeting", "ReligExtMeeting")

# Create ISSPCitizenI with only policy questions - one for each respondent
ISSPCitizenI <- ISSPCitizenI |> 
  mutate(own_id = seq(1, n())) # Create id for respondents

ISSPCitizenI_onlypolicy <- ISSPCitizenI  |>
  dplyr::select(own_id, country, year, all_of(policy_varname_vec), survey, AgeCategories, 
                University, woman, rural, income_unified, PolInterested, Pol_efficacy, Pol_trust, 
                Satisfied_w_Dem, protest, voted, polparticipation, country, survey2, contact, attendedmeeting, petition, 
                boycott, donated, internetforum, contactmedia, ethnic_minority, lrself) # select only relevant variables

# # Include all NAs in analysis - uncomment for the analysis including NAs and don't know answers, which creates Table G1 instead of Table 1
# ISSPCitizenI_onlypolicy <- ISSPCitizenI_onlypolicy |>
#   mutate(across(.cols = policy_varname_vec, ~ case_when(is.na(.x) ~ 0,
#                                                         TRUE ~ .x)))

# Pivot longer to create a data frame where each policy is the observation (each individual is therefore included several times)
ISSPCitizenI_onlypolicy_long <- ISSPCitizenI_onlypolicy |> 
  pivot_longer(cols = all_of(policy_varname_vec), names_to = "policyvar")

# Create q_id variable - which matches with q_id in impdata
ISSPCitizenI_onlypolicy_long <- ISSPCitizenI_onlypolicy_long |> 
  mutate(q_id = case_match(policyvar,
                           "RacialPrejMeeting"  ~ 84,
                           "OverthrowGovMeeting" ~ 85,
                           "ReligExtMeeting" ~ 103))
}


#### ISSP CITIZEN II
{

## Recode variables

# Country
ISSPCitizenII <- ISSPCitizenII |>
  mutate(country = case_when(C_ALPHAN == "GB-GBN" ~ "GB",
                             TRUE ~ C_ALPHAN))
# Income variables
variablelist <- c("AT_RINC", "AU_RINC", "BE_RINC", "CH_RINC", "CL_RINC", "CZ_RINC", "DE_RINC", "DK_RINC", 
                  "ES_RINC", "FI_RINC", "FR_RINC", "GB_RINC", "GE_RINC", "HR_RINC", "HU_RINC", "IL_RINC", 
                  "IN_RINC", "IS_RINC", "JP_RINC", "KR_RINC", "LT_RINC", "NL_RINC", "NO_RINC", "PH_RINC", 
                  "PL_RINC", "RU_RINC", "SE_RINC", "SI_RINC", "SK_RINC", "TR_RINC", "TW_RINC", "US_RINC", 
                  "VE_RINC", "ZA_RINC")

# Loop through each variable and replace values such as 999999 and 999990 (that indicate a "non-income answer") with NA
for (variable in variablelist) {
  ISSPCitizenII[[variable]][ISSPCitizenII[[variable]] == 999999 | ISSPCitizenII[[variable]] == 999990 | 
                              ISSPCitizenII[[variable]] == 999997 | ISSPCitizenII[[variable]] == 999998 | 
                              ISSPCitizenII[[variable]] == 9999990 | ISSPCitizenII[[variable]] == 9999998 | 
                              ISSPCitizenII[[variable]] == 9999999 | ISSPCitizenII[[variable]] == 9999997 | 
                              ISSPCitizenII[[variable]] == 99999990 | ISSPCitizenII[[variable]] == 99999998 | 
                              ISSPCitizenII[[variable]] == 99999999 | ISSPCitizenII[[variable]] == 99999997] <- NA
}


ISSPCitizenII <- ISSPCitizenII %>% 
  mutate(total_income = rowSums(dplyr::select(., all_of(variablelist)), na.rm = TRUE)) # sum across variables to get one column with all income data

# Group by country and normalize income within each country
ISSPCitizenII <- ISSPCitizenII |> 
  group_by(country) |> 
  mutate(income_unified = percent_rank(total_income)) |> 
  ungroup()

# Ethnicity
variablelist <- c("AT_ETHN1", "AU_ETHN1", "BE_ETHN1", "CH_ETHN1", "CH_ETHN1", "CZ_ETHN1",
                  "DE_ETHN1", "DK_ETHN1", "ES_ETHN1", "FI_ETHN1", "FR_ETHN1", "GB_ETHN1", "GE_ETHN1", 
                  "HR_ETHN1", "HU_ETHN1", "IL_ETHN1", "IN_ETHN1", "IS_ETHN1", "JP_ETHN1", "KR_ETHN1", 
                  "LT_ETHN1", "NL_ETHN1", "NO_ETHN1", "PH_ETHN1", "PL_ETHN1", "RU_ETHN1", "SE_ETHN1", 
                  "SI_ETHN1", "SK_ETHN1", "TR_ETHN1", "TW_ETHN1", "US_ETHN1", "VE_ETHN1", "ZA_ETHN1")

# Convert 0 values to NA for each variable in the variable list
ISSPCitizenII <- ISSPCitizenII |>
  mutate(across(all_of(variablelist), ~ na_if(., 0))) |>
  mutate(across(all_of(variablelist), ~ na_if(., 98))) |>
  mutate(across(all_of(variablelist), ~ na_if(., 99))) |>
  mutate(across(all_of(variablelist), ~ na_if(., 998))) |>
  mutate(across(all_of(variablelist), ~ na_if(., 9999)))

# Create a unified ethnic_group variable by coalescing the variables
ISSPCitizenII <- ISSPCitizenII |>
  mutate(ethnic_group = coalesce(!!!syms(variablelist)))
table(ISSPCitizenII$ethnic_group)

# Remove rows where ethnic_group is NA (if necessary)
ISSPCitizenII <- ISSPCitizenII |>
  filter(!is.na(ethnic_group))

ethnic_counts <- ISSPCitizenII |> #count the number of people in each ethnic group by country
  group_by(country, ethnic_group) |>
  summarise(count = n()) |>
  ungroup()

majority_group <- ethnic_counts |> #check which is the majority group
  group_by(country) |>
  slice_max(order_by = count, n = 1, with_ties = FALSE) |>
  ungroup() |>
  select(country, ethnic_group) |>
  rename(majority_ethnic = ethnic_group)

ISSPCitizenII <- ISSPCitizenII |> # #count the number of people in each ethnic group by country
  left_join(majority_group, by = "country") |>
  mutate(ethnic_minority = ifelse(ethnic_group != majority_ethnic, 1, 0)) |>
  select(-majority_ethnic)
table(ISSPCitizenII$ethnic_minority) #ethnic minority

# Recode other variables (sociodemographic and political attitudes)
table(ISSPCitizenII$V48)
ISSPCitizenII <- ISSPCitizenII |>
  mutate(woman = case_when(SEX == 1 ~ 0,
                           SEX == 2 ~ 1,
                           SEX == 9 ~ as.double(NA),
                           TRUE ~ as.double(NA)), # woman
         AgeCategories = case_when(AGE < 30 ~ "Under 30", 
                                   AGE >= 30 & AGE <= 39 ~ "30 to 39",
                                   AGE >= 40 & AGE <= 49 ~ "40-49",
                                   AGE >= 50 & AGE <= 59 ~ "50-59",
                                   AGE >= 60 & AGE <= 69 ~ "60-69", 
                                   AGE >= 70 & AGE < 126 ~ "70 and older",
                                   AGE > 125 ~ NA,
                                   TRUE ~ NA), # age categories
         rural = case_when(URBRURAL == 1 ~ 0,
                           URBRURAL == 2 ~ 0,
                           URBRURAL == 3 ~ 0,
                           URBRURAL == 4 ~ 1,
                           URBRURAL == 5 ~ 1,
                           URBRURAL == 7 ~ as.double(NA),
                           URBRURAL == 8 ~ as.double(NA),
                           URBRURAL == 9 ~ as.double(NA),
                           TRUE ~ as.double(NA)), # rural
         religiosity = case_when(ATTEND == 1 ~ "Once a month or more",
                                 ATTEND == 2 ~ "Once a month or more",
                                 ATTEND == 3 ~ "Once a month or more",
                                 ATTEND == 4 ~ "Once a month or more",
                                 ATTEND == 5 ~ "Once a month < Once a year",
                                 ATTEND == 6 ~ "Once a month < Once a year",
                                 ATTEND == 7 ~ "Less than once a year",
                                 ATTEND == 8 ~ "Less than once a year",
                                 ATTEND == 97 ~ NA,
                                 ATTEND == 97 ~ NA,
                                 ATTEND == 97 ~ NA,
                                 TRUE ~ NA), # how often attend religious institution
         attend_rel_onceamonth = case_when(ATTEND == 1 ~ 1,
                                           ATTEND == 2 ~ 1,
                                           ATTEND == 3 ~ 1,
                                           ATTEND == 4 ~ 1,
                                           ATTEND == 5 ~ 0,
                                           ATTEND == 6 ~ 0,
                                           ATTEND == 7 ~ 0,
                                           ATTEND == 8 ~ 0,
                                           TRUE ~ NA), # how often attend religious institution 2
         areligious = case_when(V29 == 1 ~ 0,
                                V29 == 2 ~ 0,
                                V29 == 3 ~ 1,
                                V29 == 4 ~ 1,
                                V29 == 8 ~ as.double(NA),
                                V29 == 9 ~ as.double(NA),
                                TRUE ~ as.double(NA)), # non-religious
         union = case_when(V28 == 1 ~ 1,
                           V28 == 2 ~ 1,
                           V28 == 3 ~ 0,
                           V28 == 4 ~ 0,
                           TRUE ~ as.double(NA)), # union member
         EducationLevel = case_when(DEGREE == 0 ~ "1 No education",
                                    DEGREE == 1 ~ "Primary school",
                                    DEGREE == 2 ~ "Secondary",
                                    DEGREE == 3 ~ "Secondary",
                                    DEGREE == 4 ~ "Post-secondary",
                                    DEGREE == 5 ~ "University",
                                    DEGREE == 6 ~ "University",
                                    DEGREE == 9 ~ NA,
                                    TRUE ~ NA), # level of education
         University = case_when(DEGREE == 0 ~ 0,
                                DEGREE == 1 ~ 0,
                                DEGREE == 2 ~ 0,
                                DEGREE == 3 ~ 0,
                                DEGREE == 4 ~ 0,
                                DEGREE == 5 ~ 1,
                                DEGREE == 6 ~ 1,
                                DEGREE == 9 ~ NA,
                                TRUE ~ NA), # university education
         lrself = case_when(V48 == 0 ~ 1,
                            V48 == 1 ~ 1.4,
                            V48 == 2 ~ 1.8,
                            V48 == 3 ~ 2.2,
                            V48 == 4 ~ 2.6,
                            V48 == 5 ~ 3,
                            V48 == 6 ~ 3.4,
                            V48 == 7 ~ 3.8,
                            V48 == 8 ~ 4.2,
                            V48 == 9 ~ 4.6,
                            V48 == 10 ~ 5,
                            TRUE ~ NA), # university education
         Satisfied_w_Dem = case_when(V62 == 1 ~ 0,
                                     V62 == 2 ~ 0,
                                     V62 == 3 ~ 0,
                                     V62 == 4 ~ 0,
                                     V62 == 5 ~ 0,
                                     V62 == 6 ~ 1,
                                     V62 == 7 ~ 1,
                                     V62 == 8 ~ 0,
                                     V62 == 9 ~ 1,
                                     V62 == 10 ~ 1,
                                     TRUE ~ NA), # satisfaction with democracy
         Pol_efficacy = case_when(V41 == 1 ~ 0,
                                  V41 == 2 ~ 0,
                                  V41 == 3 ~ 0,
                                  V41 == 4 ~ 1,
                                  V41 == 5 ~ 1,
                                  TRUE ~ as.double(NA)), # political efficacy
         Pol_trust = case_when(V49 == 1 ~ 1,
                               V49 == 2 ~ 1,
                               V49 == 3 ~ 0,
                               V49 == 4 ~ 0,
                               V49 == 5 ~ 0,
                               TRUE ~ as.double(NA)), # political trust
         PolInterested = case_when(V47 == 1 ~ 1,
                                   V47 == 2 ~ 1,
                                   V47 == 3 ~ 0,
                                   V47 == 4 ~ 0,
                                   V47 == 8 ~ as.double(NA),
                                   V47 == 9 ~ as.double(NA),
                                   TRUE ~ as.double(NA)) # interest in politics
  )


# Create new names/variables for political participation variables
ISSPCitizenII <- ISSPCitizenII |>
  rename(contact = V21,
         attendedmeeting = V20,
         petition = V17,
         protest = V19,
         boycott = V18,
         donated = V22,
         internetforum = V24,
         contactmedia = V23)

# Recode political participation variables
ISSPCitizenII <- ISSPCitizenII |> 
  mutate(across(.cols = all_of(partvar_vec), ~ case_when(.x == 4 ~ 0,
                                                         .x == 3 ~ 0,
                                                         .x == 2 ~ 0,
                                                         .x == 1 ~ 1,
                                                         TRUE ~ as.double(NA))))

# Code summary variables for general participation
ISSPCitizenII <- ISSPCitizenII %>% 
  mutate(polparticipation = rowMeans(dplyr::select(., all_of(partvar_vec)), na.rm=TRUE),
         polparticipation_dum = case_when(polparticipation > 0 ~ 1,
                                          polparticipation == 0 ~ 0,
                                          TRUE ~ as.double(NA)), # create a general participation variable
         meetorprotest = rowMeans(dplyr::select(., all_of(meet_demon_vec)), na.rm=TRUE),
         polparticipation_meetorprotest = case_when(meetorprotest > 0 ~ 1,
                                                    meetorprotest == 0 ~ 0,
                                                    TRUE ~ as.double(NA))) # create a protest or meeting participation variable

# Vote variable
table(ISSPCitizenII$VOTE_LE)
ISSPCitizenII <- ISSPCitizenII |> 
  mutate(voted = case_when(VOTE_LE == 2 ~ 0,
                           VOTE_LE == 1 ~ 1,
                           TRUE ~ as.double(NA)))

### Code support for policy questions
# Create new names/variables for policy question variables
ISSPCitizenII <- ISSPCitizenII |>
  rename(RacialPrejMeeting = V16,
         OverthrowGovMeeting = V15,
         ReligExtMeeting = V14,
         LongTermResVote = V38,
         RightToNotVote =  V39)

# Recode support for policy questions - as dichotomous
ISSPCitizenII <- ISSPCitizenII |> 
  mutate(across(.cols = c(RacialPrejMeeting,
                          OverthrowGovMeeting,
                          ReligExtMeeting),
                ~ case_when(between(., 3, 4) ~ 0,
                            . <= 2 ~ 1,
                            TRUE ~ as.double(NA))))

ISSPCitizenII <- ISSPCitizenII |> 
  mutate(across(.cols = c(LongTermResVote, RightToNotVote),
                ~ case_when(. <= 3 ~ 0,
                            . == 4 ~ 0.5,
                            between(., 5, 7) ~ 1,
                            TRUE ~ as.double(NA))))

#For later matching
ISSPCitizenII <- ISSPCitizenII |> 
  mutate(survey = "ISSP",            # create survey variable for later data matching
         survey2 = "ISSP Citizen II", # this variable separates this ISSP survey from the other ones, and allows us to control for each specific survey
         year = 2014) # create year variable for later data matching

# Vector of policy questions
policy_varname_vec <- c("RacialPrejMeeting", "OverthrowGovMeeting", "ReligExtMeeting",
                        "LongTermResVote", "RightToNotVote")


# Create ISSPCitizenII with only policy questions
ISSPCitizenII <- ISSPCitizenII |> 
  mutate(own_id = seq(1, n())) # Create id for respondents

ISSPCitizenII_onlypolicy <- ISSPCitizenII  |> 
  dplyr::select(own_id, country, year, all_of(policy_varname_vec), survey, AgeCategories, 
                University, woman, rural, income_unified, PolInterested, voted, polparticipation,
                Pol_efficacy, Pol_trust, Satisfied_w_Dem, protest, country, survey2, contact, attendedmeeting, 
                petition, boycott, donated, internetforum, contactmedia, ethnic_minority, lrself) # select only relevant variables

# # Include all NAs in analysis - uncomment for the analysis including NAs and don't know answers, which creates Table G1 instead of Table 1
# ISSPCitizenII_onlypolicy <- ISSPCitizenII_onlypolicy |>
#   mutate(across(.cols = policy_varname_vec, ~ case_when(is.na(.x) ~ 0,
#                                                         TRUE ~ .x)))

# Pivot longer to create a data frame where each policy is the observation (each individual is therefore included several times)
ISSPCitizenII_onlypolicy_long <- ISSPCitizenII_onlypolicy |> 
  pivot_longer(cols = all_of(policy_varname_vec), names_to = "policyvar")

# Create q_id variable - which matches with q_id in impdata
ISSPCitizenII_onlypolicy_long <- ISSPCitizenII_onlypolicy_long |> 
  mutate(q_id = case_match(policyvar,
                           "RacialPrejMeeting"  ~ 84,
                           "OverthrowGovMeeting" ~ 85,
                           "ReligExtMeeting" ~ 103,
                           "LongTermResVote" ~ 4,
                           "RightToNotVote" ~ 22))
}


### Combine all the data ###

# Combine the data
fulldata_long <- bind_rows(ess_onlypolicy_long, ISSPCitizenI_onlypolicy_long, ISSPCitizenII_onlypolicy_long, ISSPRoGIII_onlypolicy_long)

# Match on implementation data
fulldata_long <- fulldata_long %>% 
  left_join(., impdata_slim, by=c("country", "year", "q_id", "survey")) %>% 
  mutate(congruence = case_when(value - y3_imp == 0 ~ 1,
                                value - y3_imp == 1 ~ 0,
                                value - y3_imp == -1 ~ 0,
                                value == 0.5 & !is.na(y3_imp) ~ 0.5, # comment this line (#) to not include the indifferent in this measure
                                # is.na(value) & !is.na(y3_imp) ~ 0, # uncomment this line to examine the results when NA answers are included
                                TRUE ~as.double(NA)),
         supportsq = case_when(value==0 & q_switcher==0 ~ 1,
                               value==1 & q_switcher==1 ~ 1,
                               value==0 & q_switcher==1 ~ 0,
                               value==1 & q_switcher==0 ~ 0,
                               value==0.5 & !is.na(q_switcher) ~ 0.5,
                               TRUE ~ as.double(NA))) %>% 
  mutate(countryyear = paste0(country, "_", as.character(year))) %>% 
  #left_join(., dplyr::select(questionwordings, c(q_id, shortwording))) %>% 
  filter(!is.na(value))

# Individual-level analysis
fulldata_individual <- fulldata_long %>%  
  filter(!is.na(value), !is.na(y3_imp)) %>%
  group_by(survey2, country, year, own_id) %>%
  summarize(n_policyquestions = n(),
            meancong = mean(congruence, na.rm=TRUE),
            meansupport = mean(value, na.rm=TRUE),
            meanimp = mean(y3_imp, na.rm=TRUE),
            meansupportsq = mean(supportsq, na.rm=TRUE),
            across(.cols=c(AgeCategories, University, woman, rural, income_unified, PolInterested, ethnic_minority,
                           Pol_efficacy, Pol_trust, Satisfied_w_Dem, voted, protest, polparticipation, lrself, 
                           contact, attendedmeeting, petition, protest, boycott, donated, internetforum,
                           contactmedia, polwork, otherwork, wornbadge, ethcon, illegalprotest, meeting), ~ first(.x))) %>% 
  mutate(countrysurveyyear = paste0(country, "_", survey2, "_", as.character(year))) %>% # Create a variable for country-survey-year
  ungroup()

# Data for regressions
regdata <- fulldata_individual

### End of Data Preparation ###


### (3) Analyses in the Paper ###

# Create histogram over mean congruence at the individual level
congruence_histogram <- regdata |> 
  ggplot(aes(x=meancong, 
             weight = n_policyquestions,
             y = after_stat(count / sum(count)))) +
  geom_histogram(binwidth=0.05, boundary=0, fill= wes_palette("AsteroidCity3")[4], col="black", linewidth=0.5) +
  theme_classic() +
  scale_y_continuous(labels = scales::percent) +
  labs(x="Mean congruence", y="Percent")

# png("figures/congruence_histogram_2.png", width=4, height=3, units="in", res=300)
congruence_histogram
# dev.off()


# Descriptives on number of countries and years
regdata |>
  filter(!is.na(voted), !is.na(meancong), !is.na(protest)) |>
  count(year)


### First descriptive table - Table 1
# Basic table analysis - for first table in paper that just shows mean values
# Congruence first aggregated by individual -> then we take mean of all individuals

# Define the weighted mean function for bootstrapping - to calculate confidence intervals for weighted congruence
weighted_mean_func <- function(data, indices) {
  # Extract the bootstrap sample based on the given indices
  sample_data <- data[indices, ]
  
  # Calculate the weighted mean, removing NA values
  weighted_mean <- weighted.mean(sample_data$meancong, 
                                 w = sample_data$n_policyquestions, 
                                 na.rm = TRUE)
  return(weighted_mean)
}

# Perform the bootstrap
regdata_demons <- subset(regdata, protest == 1)
regdata_nondemons <- subset(regdata, protest == 0)
regdata_voters <- subset(regdata, voted == 1)
regdata_nonvoters <- subset(regdata, voted == 0)
regdata_lowinc <- subset(regdata, income_unified < 0.2)
regdata_highinc <- subset(regdata, income_unified > 0.8)
regdata_dem_vote <- subset(regdata, protest == 1 & voted == 1)
regdata_nodem_vote <- subset(regdata, protest == 0 & voted == 1)
regdata_dem_novote <- subset(regdata, protest == 1 & voted == 0)
regdata_nodem_novote <- subset(regdata, protest == 0 & voted == 0)
bootstrap_results_dem <- boot(data = regdata_demons, 
                          statistic = weighted_mean_func, 
                          R = 400)
bootstrap_results_nondem <- boot(data = regdata_nondemons, 
                              statistic = weighted_mean_func, 
                              R = 400)
bootstrap_results_vot <- boot(data = regdata_voters, 
                              statistic = weighted_mean_func, 
                              R = 400)
bootstrap_results_nonvot <- boot(data = regdata_nonvoters, 
                              statistic = weighted_mean_func, 
                              R = 400)
bootstrap_results_lowinc <- boot(data = regdata_lowinc, 
                                 statistic = weighted_mean_func, 
                                 R = 400)
bootstrap_results_highinc <- boot(data = regdata_highinc, 
                                  statistic = weighted_mean_func, 
                                  R = 400)
bootstrap_results_dem_vote <- boot(data = regdata_dem_vote, 
                                   statistic = weighted_mean_func, 
                                   R = 400)
bootstrap_results_nodem_vote <- boot(data = regdata_nodem_vote, 
                                     statistic = weighted_mean_func, 
                                     R = 400)
bootstrap_results_dem_novote <- boot(data = regdata_dem_novote, 
                                     statistic = weighted_mean_func, 
                                     R = 400)
bootstrap_results_nodem_novote <- boot(data = regdata_nodem_novote, 
                                        statistic = weighted_mean_func, 
                                        R = 400)
boot_ci_dem <- boot.ci(bootstrap_results_dem, type = "basic")
boot_ci_nondem <- boot.ci(bootstrap_results_nondem, type = "basic")
boot_ci_vot <- boot.ci(bootstrap_results_vot, type = "basic")
boot_ci_nonvot <- boot.ci(bootstrap_results_nonvot, type = "basic")
boot_ci_lowinc <- boot.ci(bootstrap_results_lowinc, type = "basic")
boot_ci_highinc <- boot.ci(bootstrap_results_highinc, type = "basic")
boot_ci_dem_vote <- boot.ci(bootstrap_results_dem_vote, type = "basic")
boot_ci_nodem_vote <- boot.ci(bootstrap_results_nodem_vote, type = "basic")
boot_ci_dem_novote <- boot.ci(bootstrap_results_dem_novote, type = "basic")
boot_ci_nodem_novote <- boot.ci(bootstrap_results_nodem_novote, type = "basic")

# Calculate mean congruence - voters vs nonvoters
votedmainresults <- regdata |>
  filter(!is.na(voted), !is.na(meancong)) |> 
  group_by(voted) |> 
  summarize(unweighted_cong = mean(meancong, na.rm=TRUE),
            unweighted_sd = sd(meancong, na.rm=TRUE),
            lowerCI_unw = t.test(meancong, conf.level = 0.95)$conf.int[1],
            upperCI_unw = t.test(meancong, conf.level = 0.95)$conf.int[2],
            lowerCI_w = NA,
            upperCI_w = NA,
            weighted_cong = weighted.mean(meancong, na.rm=TRUE, w = n_policyquestions),
            policyquestions = mean(n_policyquestions, na.rm=TRUE),
            n = n()) |>
  rename("Participant_type" = voted) |> 
  mutate(Participant_type = case_when(Participant_type == 1 ~ "Voter",
                                      Participant_type == 0 ~ "Non-Voter")) |> 
  ungroup()

# Add calculated (with bootstrap samples) confidence intervals for weighted congruence
votedmainresults$lowerCI_w[2] <- votedmainresults$weighted_cong[2] - ((boot_ci_vot$basic[5] - boot_ci_vot$basic[4])/2)
votedmainresults$upperCI_w[2] <- votedmainresults$weighted_cong[2] + ((boot_ci_vot$basic[5] - boot_ci_vot$basic[4])/2)
votedmainresults$lowerCI_w[1] <- votedmainresults$weighted_cong[1] - ((boot_ci_nonvot$basic[5] - boot_ci_nonvot$basic[4])/2)
votedmainresults$upperCI_w[1] <- votedmainresults$weighted_cong[1] + ((boot_ci_nonvot$basic[5] - boot_ci_nonvot$basic[4])/2)

# Calculate mean congruence - demonstrators vs non-demonstrators
demonstratedmainresults <- regdata |>
  filter(!is.na(protest), !is.na(meancong)) |> 
  group_by(protest) |> 
  summarize(unweighted_cong = mean(meancong, na.rm=TRUE),
            unweighted_sd = sd(meancong, na.rm=TRUE),
            lowerCI_unw = t.test(meancong, conf.level = 0.95)$conf.int[1],
            upperCI_unw = t.test(meancong, conf.level = 0.95)$conf.int[2],
            lowerCI_w = NA,
            upperCI_w = NA,
            weighted_cong = weighted.mean(meancong, na.rm=TRUE, w = n_policyquestions),
            policyquestions = mean(n_policyquestions, na.rm=TRUE),
            n = n()) |>
  rename("Participant_type" = protest) |> 
  mutate(Participant_type = case_when(Participant_type == 1 ~ "Demonstrator",
                                      Participant_type == 0 ~ "Non-Demonstrator")) |> 
  ungroup()

demonstratedmainresults$lowerCI_w[2] <- demonstratedmainresults$weighted_cong[2] - ((boot_ci_dem$basic[5] - boot_ci_dem$basic[4])/2)
demonstratedmainresults$upperCI_w[2] <- demonstratedmainresults$weighted_cong[2] + ((boot_ci_dem$basic[5] - boot_ci_dem$basic[4])/2)
demonstratedmainresults$lowerCI_w[1] <- demonstratedmainresults$weighted_cong[1] - ((boot_ci_nondem$basic[5] - boot_ci_nondem$basic[4])/2)
demonstratedmainresults$upperCI_w[1] <- demonstratedmainresults$weighted_cong[1] + ((boot_ci_nondem$basic[5] - boot_ci_nondem$basic[4])/2)

# Calculate mean congruence - high- vs low-income
incomemainresults <- regdata |>
  filter(income_unified < 0.2 | income_unified > 0.8, !is.na(meancong)) |> 
  mutate(income_cat = case_when(income_unified<0.2 ~ 1,
                                income_unified>0.8 ~ 2,
                                TRUE ~ as.double(NA))) |> 
  group_by(income_cat) |> 
  summarize(unweighted_cong = mean(meancong, na.rm=TRUE),
            unweighted_sd = sd(meancong, na.rm=TRUE),
            lowerCI_unw = t.test(meancong, conf.level = 0.95)$conf.int[1],
            upperCI_unw = t.test(meancong, conf.level = 0.95)$conf.int[2],
            lowerCI_w = NA,
            upperCI_w = NA,
            weighted_cong = weighted.mean(meancong, na.rm=TRUE, w = n_policyquestions),
            policyquestions = mean(n_policyquestions, na.rm=TRUE),
            n = n()) |>
  rename("Participant_type" = income_cat) |> 
  mutate(Participant_type = case_when(Participant_type == 1 ~ "Low-income",
                                      Participant_type == 2 ~ "High-income")) |> 
  ungroup()

incomemainresults$lowerCI_w[2] <- incomemainresults$weighted_cong[2] - ((boot_ci_highinc$basic[5] - boot_ci_highinc$basic[4])/2)
incomemainresults$upperCI_w[2] <- incomemainresults$weighted_cong[2] + ((boot_ci_highinc$basic[5] - boot_ci_highinc$basic[4])/2)
incomemainresults$lowerCI_w[1] <- incomemainresults$weighted_cong[1] - ((boot_ci_lowinc$basic[5] - boot_ci_lowinc$basic[4])/2)
incomemainresults$upperCI_w[1] <- incomemainresults$weighted_cong[1] + ((boot_ci_lowinc$basic[5] - boot_ci_lowinc$basic[4])/2)


mainresults <- rbind(votedmainresults, demonstratedmainresults, incomemainresults)
mainresults # Table 1 data (displays Table G1 if you have included NAs and don't know answers, see earlier comments)


### Robustness of this main result - first regression table in the paper - Table 2
m0 <- lm(meancong ~ as.factor(countrysurveyyear), data=filter(regdata, !is.na(voted) & !is.na(protest)), weights = n_policyquestions)
m1 <- lm(meancong ~ voted + protest, data=regdata, weights = n_policyquestions)
m2 <- lm(meancong ~ voted + protest + as.factor(country), data=regdata, weights = n_policyquestions)
m3 <- lm(meancong ~ voted + protest + as.factor(year), data=regdata, weights = n_policyquestions)
m4 <- lm(meancong ~ voted + as.factor(countrysurveyyear), data=filter(regdata, !is.na(voted) & !is.na(protest)), weights = n_policyquestions)
m5 <- lm(meancong ~ protest + as.factor(countrysurveyyear), data=filter(regdata, !is.na(voted) & !is.na(protest)), weights = n_policyquestions)
m6 <- lm(meancong ~ voted + protest + as.factor(countrysurveyyear), data=regdata, weights = n_policyquestions)

stargazer(m0, type="text", omit.stat=c("SER", "F"), omit=c("countrysurveyyear"))
stargazer(m1, m2, m3, m6, type="text", omit.stat=c("SER", "F"), omit=c("country", "year", "countrysurveyyear")) # Table 2


### Controlling for socio-economic factors - Table 3
b1 <- lm(meancong ~ voted + protest + as.factor(countrysurveyyear), data=regdata, weights = n_policyquestions)
b2 <- lm(meancong ~ voted + protest + income_unified + as.factor(countrysurveyyear), data=regdata, weights = n_policyquestions)
b3 <- lm(meancong ~ voted + protest + University + as.factor(countrysurveyyear), data=regdata, weights = n_policyquestions)

b4_0 <- lm(meancong ~ voted + protest + as.factor(countrysurveyyear),
         data=filter(regdata, !is.na(income_unified) & !is.na(University) & !is.na(woman) & !is.na(AgeCategories) & 
                       !is.na(rural)), weights = n_policyquestions) # running the first regression model with the observations of the fourth

b4 <- lm(meancong ~ voted + protest + income_unified + University + woman +
           eval(AgeCategories=="Under 30") + eval(AgeCategories=="60-69" | AgeCategories=="70 and older") +
           rural + as.factor(countrysurveyyear), data=regdata, weights = n_policyquestions)

stargazer(b4_0, type="text", omit.stat=c("SER", "F"), omit=c("country", "year", "countrysurveyyear"), star.cutoffs = c(0.05, 0.01, 0.001)) # Model b1 with sample of Model b4
stargazer(b1, b2, b3, b4, type="text", omit.stat=c("SER", "F"), 
          omit=c("country", "year", "countrysurveyyear"), star.cutoffs = c(0.05, 0.01, 0.001)) # Table 3 (displays Table G2 if you have included NAs and don't know answers, see earlier comments)



### Controlling for pro-democratic attitudes - Table 4 (and C1)

a1 <- lm(meancong ~ voted + protest + income_unified + University + woman + 
           eval(AgeCategories=="Under 30") + eval(AgeCategories=="60-69" | AgeCategories=="70 and older") +
           rural + as.factor(countrysurveyyear) + PolInterested, data=regdata, weights = n_policyquestions)
a2 <- lm(meancong ~ voted + protest + income_unified + University + woman + 
           eval(AgeCategories=="Under 30") + eval(AgeCategories=="60-69" | AgeCategories=="70 and older") +
           rural + as.factor(countrysurveyyear) + Pol_trust, data=regdata, weights = n_policyquestions)
a3 <- lm(meancong ~ voted + protest + income_unified + University + woman + 
           eval(AgeCategories=="Under 30") + eval(AgeCategories=="60-69" | AgeCategories=="70 and older") +
           rural + as.factor(countrysurveyyear) + Satisfied_w_Dem, data=regdata, weights = n_policyquestions)
a4 <- lm(meancong ~ voted + protest + income_unified + University + woman + 
           eval(AgeCategories=="Under 30") + eval(AgeCategories=="60-69" | AgeCategories=="70 and older") +
           rural + as.factor(countrysurveyyear) + Pol_efficacy, data=regdata, weights = n_policyquestions)
a5 <- lm(meancong ~ voted + protest + income_unified + University + woman + 
           eval(AgeCategories=="Under 30") + eval(AgeCategories=="60-69" | AgeCategories=="70 and older") +
           rural + as.factor(countrysurveyyear) + PolInterested + Pol_efficacy + Satisfied_w_Dem + Pol_trust, data=regdata, weights = n_policyquestions)

stargazer(a1, a2, a3, a4, a5, type="text", omit.stat=c("SER", "F"), omit=c("country", "year", "countrysurveyyear"), star.cutoffs = c(0.05, 0.01, 0.001)) # Table 4

# Checking for multicollinearity
vif(a1)
vif(a2)
vif(a3)
vif(a4)
vif(a5)

# Running the first four regression models with the same observations as in the fifth one - see note to Table 4
a1_0 <- lm(meancong ~ voted + protest + income_unified + University + woman + 
             eval(AgeCategories=="Under 30") + eval(AgeCategories=="60-69" | AgeCategories=="70 and older") +
             rural + as.factor(countrysurveyyear) + PolInterested, 
           data=filter(regdata, !is.na(Pol_efficacy) & !is.na(Pol_trust) & 
                         !is.na(Satisfied_w_Dem) & !is.na(PolInterested)), weights = n_policyquestions)
a2_0 <- lm(meancong ~ voted + protest + income_unified + University + woman + 
           eval(AgeCategories=="Under 30") + eval(AgeCategories=="60-69" | AgeCategories=="70 and older") +
           rural + as.factor(countrysurveyyear) + Pol_trust,
           data=filter(regdata, !is.na(Pol_efficacy) & !is.na(Pol_trust) & 
                         !is.na(Satisfied_w_Dem) & !is.na(PolInterested)), weights = n_policyquestions)
a3_0 <- lm(meancong ~ voted + protest + income_unified + University + woman + 
           eval(AgeCategories=="Under 30") + eval(AgeCategories=="60-69" | AgeCategories=="70 and older") +
           rural + as.factor(countrysurveyyear) + Satisfied_w_Dem, 
           data=filter(regdata, !is.na(Pol_efficacy) & !is.na(Pol_trust) & 
                         !is.na(Satisfied_w_Dem) & !is.na(PolInterested)), weights = n_policyquestions)
a4_0 <- lm(meancong ~ voted + protest + income_unified + University + woman + 
             eval(AgeCategories=="Under 30") + eval(AgeCategories=="60-69" | AgeCategories=="70 and older") +
             rural + as.factor(countrysurveyyear) + Pol_efficacy, 
           data=filter(regdata, !is.na(Pol_efficacy) & !is.na(Pol_trust) & 
                         !is.na(Satisfied_w_Dem) & !is.na(PolInterested)), weights = n_policyquestions)

stargazer(a1_0, a2_0, a3_0, a4_0, type="text", omit.stat=c("SER", "F"), 
          omit=c("country", "year", "countrysurveyyear", "income_unified", 
                 "University", "woman", "rural", "AgeCategories", "Pol_efficacy",
                 "Pol_trust", "Satisfied_w_Dem", "PolInterested"), star.cutoffs = c(0.05, 0.01, 0.001))

# Checking for multicollinearity
vif(a1_0)
vif(a2_0)
vif(a3_0)
vif(a4_0)

#####################


### ALL FORMS OF PARTICIPATION - Figure 6 and C.1

## Regressions without control variables
m1 <- lm(meancong ~ voted             + as.factor(countrysurveyyear), data=filter(regdata, !is.na(protest)))
m2 <- lm(meancong ~ protest           + as.factor(countrysurveyyear), data=regdata)
m3 <- lm(meancong ~ contact           + as.factor(countrysurveyyear), data=filter(regdata, !is.na(protest)))
m4 <- lm(meancong ~ attendedmeeting   + as.factor(countrysurveyyear), data=filter(regdata, !is.na(protest)))
m5 <- lm(meancong ~ petition          + as.factor(countrysurveyyear), data=filter(regdata, !is.na(protest)))
m6 <- lm(meancong ~ boycott           + as.factor(countrysurveyyear), data=filter(regdata, !is.na(protest)))
m7 <- lm(meancong ~ donated           + as.factor(countrysurveyyear), data=filter(regdata, !is.na(protest)))
m8 <- lm(meancong ~ internetforum     + as.factor(countrysurveyyear), data=filter(regdata, !is.na(protest)))
m9 <- lm(meancong ~ contactmedia      + as.factor(countrysurveyyear), data=filter(regdata, !is.na(protest)))
m10 <- lm(meancong ~ polwork          + as.factor(countrysurveyyear), data=filter(regdata, !is.na(protest)))
m11 <- lm(meancong ~ otherwork        + as.factor(countrysurveyyear), data=filter(regdata, !is.na(protest)))
m12 <- lm(meancong ~ wornbadge        + as.factor(countrysurveyyear), data=filter(regdata, !is.na(protest)))
m13 <- lm(meancong ~ ethcon           + as.factor(countrysurveyyear), data=filter(regdata, !is.na(protest)))
m14 <- lm(meancong ~ illegalprotest   + as.factor(countrysurveyyear), data=filter(regdata, !is.na(protest)))
m15 <- lm(meancong ~ meeting          + as.factor(countrysurveyyear), data=filter(regdata, !is.na(protest)))

stargazer(m1, m2, m3, m5, m6, type="text", omit.stat=c("SER", "F"), omit=c("countrysurveyyear"))
stargazer(m7, m4, m5, m8, m9, m10, m11, m12, m13, m14, m15, type="text", omit.stat=c("SER", "F"), omit=c("countrysurveyyear"))

tidy_m1 <- tidy(m1, conf.int = TRUE) |> filter(term=="voted")                  |> mutate(num_observations = nobs(m1))
tidy_m2 <- tidy(m2, conf.int = TRUE) |> filter(term=="protest")                |> mutate(num_observations = nobs(m2))
tidy_m3 <- tidy(m3, conf.int = TRUE) |> filter(term=="contact")                |> mutate(num_observations = nobs(m3))
tidy_m4 <- tidy(m4, conf.int = TRUE) |> filter(term=="attendedmeeting")        |> mutate(num_observations = nobs(m4))
tidy_m5 <- tidy(m5, conf.int = TRUE) |> filter(term=="petition")               |> mutate(num_observations = nobs(m5))
tidy_m6 <- tidy(m6, conf.int = TRUE) |> filter(term=="boycott")                |> mutate(num_observations = nobs(m6))
tidy_m7 <- tidy(m7, conf.int = TRUE) |> filter(term=="donated")                |> mutate(num_observations = nobs(m7))
tidy_m8 <- tidy(m8, conf.int = TRUE) |> filter(term=="internetforum")          |> mutate(num_observations = nobs(m8))
tidy_m9 <- tidy(m9, conf.int = TRUE) |> filter(term=="contactmedia")           |> mutate(num_observations = nobs(m9))
tidy_m10 <- tidy(m10, conf.int = TRUE) |> filter(term=="polwork")              |> mutate(num_observations = nobs(m10))
tidy_m11 <- tidy(m11, conf.int = TRUE) |> filter(term=="otherwork")            |> mutate(num_observations = nobs(m11))
tidy_m12 <- tidy(m12, conf.int = TRUE) |> filter(term=="wornbadge")            |> mutate(num_observations = nobs(m12))
tidy_m13 <- tidy(m13, conf.int = TRUE) |> filter(term=="ethcon")               |> mutate(num_observations = nobs(m13))
tidy_m14 <- tidy(m14, conf.int = TRUE) |> filter(term=="illegalprotest")       |> mutate(num_observations = nobs(m14))
tidy_m15 <- tidy(m15, conf.int = TRUE) |> filter(term=="meeting")              |> mutate(num_observations = nobs(m15))

# Separate the regressions to create Fig. 6 and Fig. C.1
results_nocontrol <- bind_rows(tidy_m1, tidy_m2, tidy_m3,
                               tidy_m5,tidy_m6) |>
  arrange(desc(estimate)) |>
  mutate(rank = seq(n(), 1))

results_nocontrol_2 <- bind_rows(tidy_m4, tidy_m7, tidy_m8, tidy_m9, tidy_m10,
                                 tidy_m11, tidy_m12, tidy_m13, tidy_m14, tidy_m15) |>
  arrange(desc(estimate)) |>
  mutate(rank = seq(n(), 1))

## With control variables
c1 <- lm(meancong ~ voted             + as.factor(countrysurveyyear) + income_unified + University + woman + eval(AgeCategories=="Under 30") + eval(AgeCategories=="60-69" | AgeCategories=="70 and older") + rural, data=filter(regdata, !is.na(protest)))
c2 <- lm(meancong ~ protest           + as.factor(countrysurveyyear) + income_unified + University + woman + eval(AgeCategories=="Under 30") + eval(AgeCategories=="60-69" | AgeCategories=="70 and older") + rural, data=regdata)
c3 <- lm(meancong ~ contact           + as.factor(countrysurveyyear) + income_unified + University + woman + eval(AgeCategories=="Under 30") + eval(AgeCategories=="60-69" | AgeCategories=="70 and older") + rural, data=filter(regdata, !is.na(protest)))
c4 <- lm(meancong ~ attendedmeeting   + as.factor(countrysurveyyear) + income_unified + University + woman + eval(AgeCategories=="Under 30") + eval(AgeCategories=="60-69" | AgeCategories=="70 and older") + rural, data=filter(regdata, !is.na(protest)))
c5 <- lm(meancong ~ petition          + as.factor(countrysurveyyear) + income_unified + University + woman + eval(AgeCategories=="Under 30") + eval(AgeCategories=="60-69" | AgeCategories=="70 and older") + rural, data=filter(regdata, !is.na(protest)))
c6 <- lm(meancong ~ boycott           + as.factor(countrysurveyyear) + income_unified + University + woman + eval(AgeCategories=="Under 30") + eval(AgeCategories=="60-69" | AgeCategories=="70 and older") + rural, data=filter(regdata, !is.na(protest)))
c7 <- lm(meancong ~ donated           + as.factor(countrysurveyyear) + income_unified + University + woman + eval(AgeCategories=="Under 30") + eval(AgeCategories=="60-69" | AgeCategories=="70 and older") + rural, data=filter(regdata, !is.na(protest)))
c8 <- lm(meancong ~ internetforum     + as.factor(countrysurveyyear) + income_unified + University + woman + eval(AgeCategories=="Under 30") + eval(AgeCategories=="60-69" | AgeCategories=="70 and older") + rural, data=filter(regdata, !is.na(protest)))
c9 <- lm(meancong ~ contactmedia      + as.factor(countrysurveyyear) + income_unified + University + woman + eval(AgeCategories=="Under 30") + eval(AgeCategories=="60-69" | AgeCategories=="70 and older") + rural, data=filter(regdata, !is.na(protest)))
c10 <- lm(meancong ~ polwork          + as.factor(countrysurveyyear) + income_unified + University + woman + eval(AgeCategories=="Under 30") + eval(AgeCategories=="60-69" | AgeCategories=="70 and older") + rural, data=filter(regdata, !is.na(protest)))
c11 <- lm(meancong ~ otherwork        + as.factor(countrysurveyyear) + income_unified + University + woman + eval(AgeCategories=="Under 30") + eval(AgeCategories=="60-69" | AgeCategories=="70 and older") + rural, data=filter(regdata, !is.na(protest)))
c12 <- lm(meancong ~ wornbadge        + as.factor(countrysurveyyear) + income_unified + University + woman + eval(AgeCategories=="Under 30") + eval(AgeCategories=="60-69" | AgeCategories=="70 and older") + rural, data=filter(regdata, !is.na(protest)))
c13 <- lm(meancong ~ ethcon           + as.factor(countrysurveyyear) + income_unified + University + woman + eval(AgeCategories=="Under 30") + eval(AgeCategories=="60-69" | AgeCategories=="70 and older") + rural, data=filter(regdata, !is.na(protest)))
c14 <- lm(meancong ~ illegalprotest   + as.factor(countrysurveyyear) + income_unified + University + woman + eval(AgeCategories=="Under 30") + eval(AgeCategories=="60-69" | AgeCategories=="70 and older") + rural, data=filter(regdata, !is.na(protest)))
c15 <- lm(meancong ~ meeting          + as.factor(countrysurveyyear) + income_unified + University + woman + eval(AgeCategories=="Under 30") + eval(AgeCategories=="60-69" | AgeCategories=="70 and older") + rural, data=filter(regdata, !is.na(protest)))

stargazer(c1, c2, c3,  c5, c6, type="text", omit.stat=c("SER", "F"), omit=c("countrysurveyyear"))
stargazer(c7, c4, c5, c8, c9, c10, c11, c12, c13, c14, c15, type="text", omit.stat=c("SER", "F"), omit=c("countrysurveyyear"))

tidy_c1 <- tidy(c1, conf.int = TRUE) |> filter(term=="voted") |>              mutate(num_observations = nobs(c1))
tidy_c2 <- tidy(c2, conf.int = TRUE) |> filter(term=="protest") |>            mutate(num_observations = nobs(c2))
tidy_c3 <- tidy(c3, conf.int = TRUE) |> filter(term=="contact") |>            mutate(num_observations = nobs(c3))
tidy_c4 <- tidy(c4, conf.int = TRUE) |> filter(term=="attendedmeeting") |>    mutate(num_observations = nobs(c4))
tidy_c5 <- tidy(c5, conf.int = TRUE) |> filter(term=="petition") |>           mutate(num_observations = nobs(c5))
tidy_c6 <- tidy(c6, conf.int = TRUE) |> filter(term=="boycott") |>            mutate(num_observations = nobs(c6))
tidy_c7 <- tidy(c7, conf.int = TRUE) |> filter(term=="donated") |>            mutate(num_observations = nobs(c7))
tidy_c8 <- tidy(c8, conf.int = TRUE) |> filter(term=="internetforum") |>      mutate(num_observations = nobs(c8))
tidy_c9 <- tidy(c9, conf.int = TRUE) |> filter(term=="contactmedia") |>       mutate(num_observations = nobs(c9))
tidy_c10 <- tidy(c10, conf.int = TRUE) |> filter(term=="polwork") |>          mutate(num_observations = nobs(c10))
tidy_c11 <- tidy(c11, conf.int = TRUE) |> filter(term=="otherwork") |>        mutate(num_observations = nobs(c11))
tidy_c12 <- tidy(c12, conf.int = TRUE) |> filter(term=="wornbadge") |>        mutate(num_observations = nobs(c12))
tidy_c13 <- tidy(c13, conf.int = TRUE) |> filter(term=="ethcon") |>           mutate(num_observations = nobs(c13))
tidy_c14 <- tidy(c14, conf.int = TRUE) |> filter(term=="illegalprotest") |>   mutate(num_observations = nobs(c14))
tidy_c15 <- tidy(c15, conf.int = TRUE) |> filter(term=="meeting") |>          mutate(num_observations = nobs(c15))


# Extract relevant rows for "voted" and "protest," etc.
results_control <- bind_rows(tidy_c1, tidy_c2, tidy_c3,
                             tidy_c5, tidy_c6) |>
  arrange(desc(estimate)) |>
  mutate(rank_control = seq(n(), 1)) |>
  rename("ctrl_estimate" = estimate,
         "ctrl_std.error" = std.error,
         "ctrl_statistic" = statistic,
         "ctrl_p.value" = p.value,
         "ctrl_conf.low" = conf.low,
         "ctrl_conf.high" = conf.high)

results_control_2 <- bind_rows(tidy_c4, tidy_c7, tidy_c8, tidy_c9, tidy_c10,
                               tidy_c11, tidy_c12, tidy_c13, tidy_c14, tidy_c15) |>
  arrange(desc(estimate)) |>
  mutate(rank_control = seq(n(), 1)) |>
  rename("ctrl_estimate" = estimate,
         "ctrl_std.error" = std.error,
         "ctrl_statistic" = statistic,
         "ctrl_p.value" = p.value,
         "ctrl_conf.low" = conf.low,
         "ctrl_conf.high" = conf.high)

## Estimate the coefficient for protest, with control variables - with the same data as for each variable of interest
c1_p <- lm(meancong ~ protest           + as.factor(countrysurveyyear) + income_unified + University + woman + eval(AgeCategories=="Under 30") + eval(AgeCategories=="60-69" | AgeCategories=="70 and older") + rural, data=filter(regdata, !is.na(voted)))
c2_p <- lm(meancong ~ protest           + as.factor(countrysurveyyear) + income_unified + University + woman + eval(AgeCategories=="Under 30") + eval(AgeCategories=="60-69" | AgeCategories=="70 and older") + rural, data=regdata)
c3_p <- lm(meancong ~ protest           + as.factor(countrysurveyyear) + income_unified + University + woman + eval(AgeCategories=="Under 30") + eval(AgeCategories=="60-69" | AgeCategories=="70 and older") + rural, data=filter(regdata, !is.na(contact)))
c4_p <- lm(meancong ~ protest           + as.factor(countrysurveyyear) + income_unified + University + woman + eval(AgeCategories=="Under 30") + eval(AgeCategories=="60-69" | AgeCategories=="70 and older") + rural, data=filter(regdata, !is.na(attendedmeeting)))
c5_p <- lm(meancong ~ protest           + as.factor(countrysurveyyear) + income_unified + University + woman + eval(AgeCategories=="Under 30") + eval(AgeCategories=="60-69" | AgeCategories=="70 and older") + rural, data=filter(regdata, !is.na(petition)))
c6_p <- lm(meancong ~ protest           + as.factor(countrysurveyyear) + income_unified + University + woman + eval(AgeCategories=="Under 30") + eval(AgeCategories=="60-69" | AgeCategories=="70 and older") + rural, data=filter(regdata, !is.na(boycott)))
c7_p <- lm(meancong ~ protest           + as.factor(countrysurveyyear) + income_unified + University + woman + eval(AgeCategories=="Under 30") + eval(AgeCategories=="60-69" | AgeCategories=="70 and older") + rural, data=filter(regdata, !is.na(donated)))
c8_p <- lm(meancong ~ protest           + as.factor(countrysurveyyear) + income_unified + University + woman + eval(AgeCategories=="Under 30") + eval(AgeCategories=="60-69" | AgeCategories=="70 and older") + rural, data=filter(regdata, !is.na(internetforum)))
c9_p <- lm(meancong ~ protest           + as.factor(countrysurveyyear) + income_unified + University + woman + eval(AgeCategories=="Under 30") + eval(AgeCategories=="60-69" | AgeCategories=="70 and older") + rural, data=filter(regdata, !is.na(contactmedia)))
c10_p <- lm(meancong ~ protest          + as.factor(countrysurveyyear) + income_unified + University + woman + eval(AgeCategories=="Under 30") + eval(AgeCategories=="60-69" | AgeCategories=="70 and older") + rural, data=filter(regdata, !is.na(polwork)))
c11_p <- lm(meancong ~ protest          + as.factor(countrysurveyyear) + income_unified + University + woman + eval(AgeCategories=="Under 30") + eval(AgeCategories=="60-69" | AgeCategories=="70 and older") + rural, data=filter(regdata, !is.na(otherwork)))
c12_p <- lm(meancong ~ protest          + as.factor(countrysurveyyear) + income_unified + University + woman + eval(AgeCategories=="Under 30") + eval(AgeCategories=="60-69" | AgeCategories=="70 and older") + rural, data=filter(regdata, !is.na(wornbadge)))
c13_p <- lm(meancong ~ protest          + as.factor(countrysurveyyear) + income_unified + University + woman + eval(AgeCategories=="Under 30") + eval(AgeCategories=="60-69" | AgeCategories=="70 and older") + rural, data=filter(regdata, !is.na(ethcon)))
c14_p <- lm(meancong ~ protest          + as.factor(countrysurveyyear) + income_unified + University + woman + eval(AgeCategories=="Under 30") + eval(AgeCategories=="60-69" | AgeCategories=="70 and older") + rural, data=filter(regdata, !is.na(illegalprotest)))
c15_p <- lm(meancong ~ protest          + as.factor(countrysurveyyear) + income_unified + University + woman + eval(AgeCategories=="Under 30") + eval(AgeCategories=="60-69" | AgeCategories=="70 and older") + rural, data=filter(regdata, !is.na(meeting)))

stargazer(c1_p, c2_p, c3_p, c5_p, c6_p, type="text", omit.stat=c("SER", "F"), omit=c("countrysurveyyear"))
stargazer(c7_p, c4_p, c5_p, c8_p, c9_p, c10_p, type="text", omit.stat=c("SER", "F"), omit=c("countrysurveyyear"))
stargazer(c11_p, c12_p, c13_p, c14_p, c15_p, type="text", omit.stat=c("SER", "F"), omit=c("countrysurveyyear"))

tidy_c1_p <- tidy(c1_p, conf.int = TRUE) |> filter(term=="protest")   |> mutate(num_observations = nobs(c1_p))
tidy_c2_p <- tidy(c2_p, conf.int = TRUE) |> filter(term=="protest")   |> mutate(num_observations = nobs(c2_p))
tidy_c3_p <- tidy(c3_p, conf.int = TRUE) |> filter(term=="protest")   |> mutate(num_observations = nobs(c3_p))
tidy_c4_p <- tidy(c4_p, conf.int = TRUE) |> filter(term=="protest")   |> mutate(num_observations = nobs(c4_p))
tidy_c5_p <- tidy(c5_p, conf.int = TRUE) |> filter(term=="protest")   |> mutate(num_observations = nobs(c5_p))
tidy_c6_p <- tidy(c6_p, conf.int = TRUE) |> filter(term=="protest")   |> mutate(num_observations = nobs(c6_p))
tidy_c7_p <- tidy(c7_p, conf.int = TRUE) |> filter(term=="protest")   |> mutate(num_observations = nobs(c7_p))
tidy_c8_p <- tidy(c8_p, conf.int = TRUE) |> filter(term=="protest")   |> mutate(num_observations = nobs(c8_p))
tidy_c9_p <- tidy(c9_p, conf.int = TRUE) |> filter(term=="protest")   |> mutate(num_observations = nobs(c9_p))
tidy_c10_p <- tidy(c10_p, conf.int = TRUE) |> filter(term=="protest") |> mutate(num_observations = nobs(c10_p))
tidy_c11_p <- tidy(c11_p, conf.int = TRUE) |> filter(term=="protest") |> mutate(num_observations = nobs(c11_p))
tidy_c12_p <- tidy(c12_p, conf.int = TRUE) |> filter(term=="protest") |> mutate(num_observations = nobs(c12_p))
tidy_c13_p <- tidy(c13_p, conf.int = TRUE) |> filter(term=="protest") |> mutate(num_observations = nobs(c13_p))
tidy_c14_p <- tidy(c14_p, conf.int = TRUE) |> filter(term=="protest") |> mutate(num_observations = nobs(c14_p))
tidy_c15_p <- tidy(c15_p, conf.int = TRUE) |> filter(term=="protest") |> mutate(num_observations = nobs(c15_p))

# Rename terms to match the variable for the graph
tidy_c1_p$term[tidy_c1_p$term == "protest"] <- "voted"
tidy_c3_p$term[tidy_c3_p$term == "protest"] <- "contact" 
tidy_c4_p$term[tidy_c4_p$term == "protest"] <- "attendedmeeting" 
tidy_c5_p$term[tidy_c5_p$term == "protest"] <- "petition" 
tidy_c6_p$term[tidy_c6_p$term == "protest"] <- "boycott"
tidy_c7_p$term[tidy_c7_p$term == "protest"] <- "donated"
tidy_c8_p$term[tidy_c8_p$term == "protest"] <- "internetforum"
tidy_c9_p$term[tidy_c9_p$term == "protest"] <- "contactmedia"
tidy_c10_p$term[tidy_c10_p$term == "protest"] <- "polwork"
tidy_c11_p$term[tidy_c11_p$term == "protest"] <- "otherwork"
tidy_c12_p$term[tidy_c12_p$term == "protest"] <- "wornbadge"
tidy_c13_p$term[tidy_c13_p$term == "protest"] <- "ethcon"
tidy_c14_p$term[tidy_c14_p$term == "protest"] <- "illegalprotest"
tidy_c15_p$term[tidy_c15_p$term == "protest"] <- "meeting"

# Add together the data and rename the variables
results_control_p <- bind_rows(tidy_c1_p, tidy_c2_p, tidy_c3_p,
                             tidy_c5_p, tidy_c6_p) |>
  arrange(desc(estimate)) |>
  mutate(rank = seq(n(), 1)) |>
  rename("ctrl_estimate_protest" = estimate,
         "ctrl_std.error_protest" = std.error,
         "ctrl_statistic_protest" = statistic,
         "ctrl_p.value_protest" = p.value,
         "ctrl_conf.low_protest" = conf.low,
         "ctrl_conf.high_protest" = conf.high)

results_control_p_2 <- bind_rows(tidy_c4_p, tidy_c7_p, tidy_c8_p, tidy_c9_p, tidy_c10_p,
                               tidy_c11_p, tidy_c12_p, tidy_c13_p, tidy_c14_p, tidy_c15_p) |>
  arrange(desc(estimate)) |>
  mutate(rank = seq(n(), 1)) |>
  rename("ctrl_estimate_protest" = estimate,
         "ctrl_std.error_protest" = std.error,
         "ctrl_statistic_protest" = statistic,
         "ctrl_p.value_protest" = p.value,
         "ctrl_conf.low_protest" = conf.low,
         "ctrl_conf.high_protest" = conf.high)

# Join data
results <- left_join(results_nocontrol, results_control, by="term")
results <- left_join(results, results_control_p, by="term") |>
  mutate(fullterm = case_match(term,
  "boycott" ~ "Participated in boycott",
  "protest" ~ "Demonstrated",
  "petition" ~ "Signed petition",
  "contact" ~ "Contacted politician",
  "voted" ~ "Voted"))

results_2 <- left_join(results_nocontrol_2, results_control_2,  by="term")
results_2 <- left_join(results_2, results_control_p_2, by="term") |>
  mutate(fullterm = case_match(term,
  "donated" ~ "Donated money",
  "internetforum" ~ "Joined internet forum",
  "attendedmeeting" ~ "Attended political meeting",
  "ethcon" ~ "Ethical consumption",
  "contactmedia" ~ "Contacted media",
  "polwork" ~ "Worked for political party",
  "wornbadge" ~ "Worn a badge",
  "otherwork" ~ "Worked for other organization",
  "meeting" ~ "Attended protest meeting",
  "illegalprotest" ~ "Illegal protest"
  ))

# Create Graph - Fig. 6 and B1

shapes <- c("Coefficient with controls" = 1, "Coefficient without controls" = 0, "Demonstrated coefficient\nwith control variables" = 4)

# Fig. 6
coefgraph <- results |>
  ggplot(aes(x=ctrl_estimate, y=rank_control)) +
  geom_vline(xintercept=0, linetype="solid", col="red") +
  geom_segment(aes(x=ctrl_conf.low, xend=ctrl_conf.high, yend=rank_control)) +
  geom_text(aes(y=rank_control+ 0.25, x = -0.018, label=paste0(fullterm, " \n(n = ", prettyNum(num_observations, big.mark=",",scientific=FALSE), ")")), hjust=0.5, vjust=1, size=4) +
  geom_point(aes(shape = "Coefficient with controls"), size=2) +
  geom_segment(aes(x=conf.low, xend=conf.high, y=rank_control + 0.15, yend=rank_control+0.15)) +
  geom_point(aes(x=estimate, y=rank_control+0.15, shape = "Coefficient without controls"), size=2) +
  geom_segment(data = results |> filter(row_number() != 3), aes(x=ctrl_conf.low_protest, xend=ctrl_conf.high_protest, y=rank_control + 0.30, yend=rank_control+0.30), col="gray") +
  geom_point(data = results |> filter(row_number() != 3), 
             aes(x=ctrl_estimate_protest, y=rank_control+0.30, shape = "Demonstrated coefficient\nwith control variables"), col="gray", size=2) +
  theme_minimal() +
  scale_shape_manual(values = shapes, name = "Coefficient Type") +
  labs(y = element_blank(),
       x = "Difference between participators and non-participators") +
  theme(axis.text.y = element_blank(), panel.grid.major.y=element_blank(), 
        panel.grid.minor.y=element_blank(), panel.grid.minor.x=element_blank(), panel.border=element_rect(fill=NA), 
        legend.position = c(0.782, 0.085), legend.title=element_text(size=15), 
        legend.text=element_text(size=12), legend.background = 
                    element_rect(color = "black",
                                 fill = "white", size = 0.3,
                                 linewidth = "solid")) +
  lims(x=c(-0.03, 0.08)) +
  # annotate("curve", curvature=0.5, x = 0.054, y=3.7, xend=0.033, yend= 4.15, arrow=arrow(length=unit(0.2, units="cm"))) +
  # annotate("text", x=0.054, y=3.69, hjust=0.5, vjust=1, label="Coefficient and confidence\ninterval without control variables", size=3) +
  annotate("curve", curvature=0.5, x = 0.046, y=2.9, xend=0.031, yend= 3.3, arrow=arrow(length=unit(0.2, units="cm")),
           col="gray") +
  annotate("text", x=0.046, y=2.89, hjust=0.5, vjust=1, 
           label="Coefficient for Demonstrated,\nestimated with same sample\nas the comparison variable", size=4)

# png("figures/coefgraph_1.png", width=6, height=8, units="in", res=300)
coefgraph
# dev.off()

# Fig. C.1
coefgraph_2 <- results_2 |>
  ggplot(aes(x=ctrl_estimate, y=rank_control)) +
  geom_vline(xintercept=0, linetype="solid", col="red") +
  geom_segment(aes(x=ctrl_conf.low, xend=ctrl_conf.high, yend=rank_control)) +
  geom_text(aes(y=rank_control+ 0.4, x = -0.025, label=paste0(fullterm, " \n(n = ", prettyNum(num_observations, big.mark=",",scientific=FALSE), ")")), hjust=0.5, vjust=1, size=3) +
  geom_point(aes(shape = "Coefficient with controls"), size=2) +
  geom_segment(aes(x=conf.low, xend=conf.high, y=rank_control + 0.15, yend=rank_control+0.15)) +
  geom_point(aes(x=estimate, y=rank_control+0.15, shape = "Coefficient without controls"), size=2) +
  geom_segment(aes(x=ctrl_conf.low_protest, xend=ctrl_conf.high_protest, y=rank_control + 0.30, yend=rank_control+0.30), col="gray") +
  geom_point(aes(x=ctrl_estimate_protest, y=rank_control+0.30, shape = "Demonstrated coefficient\nwith control variables"), col="gray", size=2) +
  theme_minimal() +
  scale_shape_manual(values = shapes, name = "Coefficient Type") +
  labs(y = element_blank(),
       x = "Difference between participators and non-participators") +
  theme(axis.text.y = element_blank(), panel.grid.major.y=element_blank(), panel.grid.minor.y=element_blank(), 
        panel.grid.minor.x=element_blank(), panel.border=element_rect(fill=NA), 
        legend.position = c(0.831, 0.074), legend.background = 
          element_rect(color = "black",
                       fill = "white", size = 0.3,
                       linewidth = "solid")) +
  lims(x=c(-0.04, 0.09)) +
  # annotate("curve", curvature=0.5, x = 0.032, y=2.6, xend=0.023, yend= 3.15, arrow=arrow(length=unit(0.2, units="cm"))) +
  # annotate("text", x=0.032, y=2.59, hjust=0.5, vjust=1, label="Coefficient and confidence\ninterval without control variables", size=3) +
  annotate("curve", curvature=0.5, x = 0.076, y=6.5, xend=0.06, yend= 7.3, arrow=arrow(length=unit(0.2, units="cm")),
           col="gray") +
  annotate("text", x=0.076, y=6.49, hjust=0.5, vjust=1, 
           label="Coefficient for Demonstrated,\nestimated with same sample\nas the comparison variable", size=3)

# png("figures/coefgraph_2.png", width=6, height=8, units="in", res=400)
coefgraph_2
# dev.off()



### Section to recreate analyses for each question at a time - Fig. 7

## First protest
qlist <- unique(fulldata_long$q_id)

fulldata_byq <- fulldata_long |> 
  mutate(countrysurveyyear = paste0(country, "_", survey2, "_", as.character(year)))

cur_variable <- "protest"

list_results <- list()
list_opinions <- list()
list_sq <- list()


# Run the regression for each question alone
vec_qid <- integer()
counter <- 1
for (i in qlist){
  print(i)
  current_data <- filter(fulldata_byq, q_id == i & 
                           !is.na(get(cur_variable)) & 
                           !is.na(congruence) &
                           #!is.na(meeting) &
                           !is.na(income_unified) &
                           !is.na(University) &
                           !is.na(woman) &
                           !is.na(eval(AgeCategories=="Under 30")) &
                           !is.na(eval(AgeCategories=="60-69"| AgeCategories=="70 and older")))
  
  # Check the number of levels in the factor variable
  num_levels <- nlevels(as.factor(current_data$countrysurveyyear))
  
  # Define the model formula based on the number of levels
  if (num_levels > 1) {
    list_results[[counter]] <- lm(congruence ~ get(cur_variable) + as.factor(countrysurveyyear) + income_unified + University + woman + eval(AgeCategories=="Under 30") + eval(AgeCategories=="60-69" | AgeCategories=="70 and older") + rural, data=current_data)
    list_opinions[[counter]] <- lm(value ~ get(cur_variable) + as.factor(countrysurveyyear) + income_unified + University + woman + eval(AgeCategories=="Under 30") + eval(AgeCategories=="60-69" | AgeCategories=="70 and older") + rural, data=current_data)
    list_sq[[counter]] <- lm(supportsq ~ get(cur_variable) + as.factor(countrysurveyyear) + income_unified + University + woman + eval(AgeCategories=="Under 30") + eval(AgeCategories=="60-69" | AgeCategories=="70 and older") + rural, data=current_data)
    vec_qid[counter] <- i
    counter <- counter+1
    
  }
}


extract_protest_info <- function(model) {
  tidy_model <- tidy(model, conf.int = TRUE)
  protest_info <- tidy_model |> 
    filter(term == "get(cur_variable)") |>
    select(estimate, conf.low, conf.high)
  return(protest_info)
}

# Use map_dfr to apply the function across all models in the list and combine the results into a tibble
results_tibble <- map_dfr(list_results, extract_protest_info) |> 
  mutate(q_id = vec_qid)
results_tibble_opinions <- map_dfr(list_opinions, extract_protest_info) |> 
  mutate(q_id = vec_qid) |> 
  rename("opinion_estimate" = estimate,
         "opinion_conf.low" = conf.low,
         "opinion_conf.high" = conf.high)

results_tibble_sq <- map_dfr(list_sq, extract_protest_info) |> 
  mutate(q_id = vec_qid) |> 
  rename("sq_estimate" = estimate,
         "sq_conf.low" = conf.low,
         "sq_conf.high" = conf.high)

results_tibble <- results_tibble %>% 
  left_join(., results_tibble_opinions, by="q_id") %>% 
  left_join(., results_tibble_sq, by="q_id") %>% 
  left_join(., dplyr::select(questionwordings, c(q_id, shortwording))) %>% 
  arrange(desc(estimate)) %>% 
  mutate(rank = seq(n(), 1))

# Combined graph with support in shapes and congruence on estimate
congruencegraph <- results_tibble |> 
  ggplot(aes(x=estimate, y=rank, col=opinion_estimate>0)) +
  geom_vline(xintercept=0, linetype="solid", col="red") +
  geom_segment(aes(x=conf.low, xend=conf.high, yend=rank)) +
  geom_point(aes(shape=opinion_estimate>0), size=2) +
  geom_text(aes(y=rank-0.15, label=shortwording), hjust=0.5, vjust=1, size=3, col="#333333") +
  theme_minimal() +
  labs(y = element_blank(),
       x = "Difference demonstrators/non-demonstrators",
       col = "Supported") +
  theme(axis.text.y = element_blank(), panel.grid.major.y=element_blank(), panel.grid.minor.y=element_blank(), panel.grid.minor.x=element_blank(),
        panel.grid.major.x = element_blank(), axis.ticks.x = element_line()) +
  scale_color_manual(values=c("orange", "blue")) +
  scale_shape_manual(values=c(4, 20)) +
  theme(legend.position = "none", aspect.ratio=2, panel.border = element_rect(fill=NA)) +
  lims(x=c(-0.3, 0.3))

# png("figures/congruence_questions_protest.png", height=10, width=5, units="in", res=300)
congruencegraph
# dev.off()

congruencegraph_protest <- congruencegraph


## Then voted
qlist <- unique(fulldata_long$q_id)

fulldata_byq <- fulldata_long |> 
  mutate(countrysurveyyear = paste0(country, "_", survey2, "_", as.character(year)))

cur_variable <- "voted"

list_results <- list()
list_opinions <- list()
list_sq <- list()

# Run the regression for each question alone
vec_qid <- integer()
counter <- 1
for (i in qlist){
  print(i)
  current_data <- filter(fulldata_byq, q_id == i & 
                           !is.na(get(cur_variable)) & 
                           !is.na(congruence) &
                           #!is.na(meeting) &
                           !is.na(income_unified) &
                           !is.na(University) &
                           !is.na(woman) &
                           !is.na(eval(AgeCategories=="Under 30")) &
                           !is.na(eval(AgeCategories=="60-69"| AgeCategories=="70 and older")))
  
  # Check the number of levels in the factor variable
  num_levels <- nlevels(as.factor(current_data$countrysurveyyear))
  
  # Define the model formula based on the number of levels
  if (num_levels > 1) {
    list_results[[counter]] <- lm(congruence ~ get(cur_variable) + as.factor(countrysurveyyear) + income_unified + University + woman + eval(AgeCategories=="Under 30") + eval(AgeCategories=="60-69" | AgeCategories=="70 and older") + rural + ethnic_minority, data=current_data)
    list_opinions[[counter]] <- lm(value ~ get(cur_variable) + as.factor(countrysurveyyear) + income_unified + University + woman + eval(AgeCategories=="Under 30") + eval(AgeCategories=="60-69" | AgeCategories=="70 and older") + rural + ethnic_minority, data=current_data)
    list_sq[[counter]] <- lm(supportsq ~ get(cur_variable) + as.factor(countrysurveyyear) + income_unified + University + woman + eval(AgeCategories=="Under 30") + eval(AgeCategories=="60-69" | AgeCategories=="70 and older") + rural + ethnic_minority, data=current_data)
    vec_qid[counter] <- i
    counter <- counter+1
    
  }
}


extract_voted_info <- function(model) {
  tidy_model <- tidy(model, conf.int = TRUE)
  voted_info <- tidy_model |> 
    filter(term == "get(cur_variable)") |>
    select(estimate, conf.low, conf.high)
  return(voted_info)
}

# Use map_dfr to apply the function across all models in the list and combine the results into a tibble
results_tibble <- map_dfr(list_results, extract_voted_info) |> 
  mutate(q_id = vec_qid)
results_tibble_opinions <- map_dfr(list_opinions, extract_voted_info) |> 
  mutate(q_id = vec_qid) |> 
  rename("opinion_estimate" = estimate,
         "opinion_conf.low" = conf.low,
         "opinion_conf.high" = conf.high)

results_tibble_sq <- map_dfr(list_sq, extract_voted_info) |> 
  mutate(q_id = vec_qid) |> 
  rename("sq_estimate" = estimate,
         "sq_conf.low" = conf.low,
         "sq_conf.high" = conf.high)

results_tibble <- results_tibble %>% 
  left_join(., results_tibble_opinions, by="q_id") %>% 
  left_join(., results_tibble_sq, by="q_id") %>% 
  left_join(., dplyr::select(questionwordings, c(q_id, shortwording))) %>% 
  arrange(desc(estimate)) %>% 
  mutate(rank = seq(n(), 1))

# Combined graph with support in shapes and congruence on estimate
congruencegraph <- results_tibble |> 
  ggplot(aes(x=estimate, y=rank, col=opinion_estimate>0)) +
  geom_vline(xintercept=0, linetype="solid", col="red") +
  geom_segment(aes(x=conf.low, xend=conf.high, yend=rank)) +
  geom_point(aes(shape=opinion_estimate>0), size=2) +
  geom_text(aes(y=rank-0.15, label=shortwording), hjust=0.5, vjust=1, size=3, col="#333333") +
  theme_minimal() +
  labs(y = element_blank(),
       x = "Difference voters/non-voters",
       col = "Supported") +
  theme(axis.text.y = element_blank(), panel.grid.major.y=element_blank(), panel.grid.minor.y=element_blank(), panel.grid.minor.x=element_blank(),
        panel.grid.major.x = element_blank(), axis.ticks.x = element_line()) +
  scale_color_manual(values=c("orange", "blue")) +
  scale_shape_manual(values=c(4, 20)) +
  theme(legend.position = "none", aspect.ratio=2, panel.border = element_rect(fill=NA)) +
  lims(x=c(-0.3, 0.3))

# png("figures/congruence_questions_voted.png", height=10, width=5, units="in", res=300)
congruencegraph
# dev.off()


congruencegraph_voted <- congruencegraph


# Create Fig. 7
# png("figures/congruence_questions_combined.png", height=8.2, width=8, units="in", res=300)
ggarrange(congruencegraph_voted, congruencegraph_protest, nrow=1, ncol=2)
# dev.off()

### End of Analyses in the Paper ###
