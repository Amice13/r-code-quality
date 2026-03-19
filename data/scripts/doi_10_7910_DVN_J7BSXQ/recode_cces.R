library(tidyverse)
library(foreign)
library(dataverse)
library(readstata13)


## create folders for generated data
if(!file.exists("./generated/")) dir.create("./generated")
if(!file.exists("./generated/data")) dir.create("./generated/data")


################################################################################
## Get the 2016 CCES data from dataverse
## located at `https://dataverse.harvard.edu/dataset.xhtml?persistentId=doi%3A10.7910/DVN/GDF6Z0`.
##
## Ansolabehere, Stephen; Schaffner, Brian F., 2017, "CCES Common Content, 2016", 
## https://doi.org/10.7910/DVN/GDF6Z0, Harvard Dataverse, V4, 
## UNF:6:WhtR8dNtMzReHC295hA4cg== [fileUNF]
## Download data `CCES16_Common_OUTPUT_Feb2018_VV.tab` as .dta
################################################################################
if(!file.exists("./generated/data/CCES16_Common_Content.dta")) {
  writeBin(get_file_by_name(
    filename = "CCES16_Common_OUTPUT_Feb2018_VV.tab",
    dataset  = "doi:10.7910/DVN/GDF6Z0",
    server   = "dataverse.harvard.edu"
  ), "./generated/data/CCES16_Common_Content.dta")
}

#### CCES Recoding

### region definitions
northeast_states = c("Massachusetts", "Pennsylvania", "Connecticut",
                     "New Hampshire", "New York", "New Jersey", "Maine",
                     "Rhode Island", "Vermont")
midwest_states = c("Minnesota", "Iowa", "Nebraska", "Ohio", "Indiana",
                   "Wisconsin", "Kansas", "Missouri", "Illinois", "Michigan",
                   "South Dakota", "North Dakota") 
south_states = c("Delaware", "Texas", "North Carolina", "Georgia", "Alabama",
                 "West Virginia", "Maryland", "District of Columbia",
                 "Kentucky", "South Carolina", "Tennessee", "Louisiana",
                 "Oklahoma", "Florida", "Arkansas", "Virginia", "Mississippi") 
west_states = c("California", "Oregon", "Washington", "Hawaii", "New Mexico",
                "Alaska", "Colorado", "Utah", "Wyoming", "Arizona", "Nevada",
                "Montana", "Idaho")

################################################################################
#### CCES #######################################################################
################################################################################

cces_env <- read.dta13("./generated/data/CCES16_Common_Content.dta")

# Common Content Weight -- commonweight_vv_post (post wave with vote validation)
cces_2016 <- cces_env %>% 
  dplyr::filter((CC16_401 == "I definitely voted in the General Election.") &
           !is.na(commonweight_vv_post))

## make outcome

# Horserace (post) question 410a
#this makes those that said they were not sure or did not vote to NA
cces_2016 <- cces_2016 %>%
  mutate( recode_vote_2016 = case_when(
    str_detect(CC16_410a, "Democrat") ~ "Democrat",
    str_detect(CC16_410a, "Republican") ~ "Republican",
    (as.numeric(CC16_410a) < 6 | as.numeric(CC16_410a) == 8) ~ "Other",
    #XXX this still includes 81 people who claim to have both definitly voted and on CC16_401 and didn't vote on CC16_410a; perhaps didnt vote for pres only
    #360: 229 = I'm not sure, 50 = NA, 81 = I didn't vote in this election
    TRUE ~ NA_character_) )

#what's missing in CCES? just a few party id's
lapply(cces_2016[, c("birthyr", "inputstate_post", "gender", "race", "pid3", "educ")], 
       function(x) (sum(is.na(x)) + sum(grepl("Skipped", x)) + sum(grepl("Not Asked", x))))

## demographic recodes
cces_2016$inputstate_post = droplevels(cces_2016$inputstate_post)

cces_2016 <- cces_2016 %>% mutate(
  recode_id = 1:nrow(cces_2016),
  
  # age
  recode_age = 2016 - as.numeric(as.character(birthyr)),
  recode_age_bucket = factor(case_when( recode_age <= 35 ~ "18 to 35",
                                        recode_age <= 50 ~ "36 to 50",
                                        recode_age < 65 ~ "51 to 64",
                                        !is.na(recode_age) ~ "65+"), 
                             levels = c("18 to 35", "36 to 50", "51 to 64", "65+")),
  
  ## three way age bucket; should be no NAs at this point
  recode_age_3way = case_when( recode_age <= 50 ~ "a_18to50",
                               recode_age < 65 ~ "b_51to64",
                               !is.na(recode_age) ~ "c_65"),
  
  # gender
  recode_female = case_when(gender == "Female" ~ "Female",
                            TRUE ~ "Male"),
  
  # race: this means Mixed, Asian, Other, Native American, Middle Eastern all in Other
  recode_race = ifelse(race %in% c("White", "Black", "Hispanic"), as.character(race), "Other"),
  
  # region: there should be no states that do not fall in one of these categories (no NAs)
  recode_region = case_when(inputstate_post %in% northeast_states ~ "Northeast",
                            inputstate_post %in% west_states ~ "West",
                            inputstate_post %in% midwest_states ~ "Midwest",
                            inputstate_post %in% south_states ~ "South",
                            TRUE ~ "South"),
  
  # party: note 13 missing on pid are categorized as Indep
  recode_pid_3way = case_when( str_detect(pid3, "Democrat") ~ "Dem",
                               str_detect(pid3, "Republican") ~ "Rep",
                               TRUE ~ "Ind"),
  
  # educ
  recode_educ = factor(educ, levels = c("No HS", "High school graduate", 
                                        "Some college", "2-year", "4-year", 
                                        "Post-grad")),
  
  recode_educ_3way = factor(case_when(
    recode_educ %in% c("No HS", "High school graduate", "Some college") ~ "No College",
    recode_educ %in% c("2-year", "4-year") ~ "College",
    TRUE ~ "Post-grad"), 
    levels = c("No College", "College", "Post-grad")),
  
  # state
  recode_inputstate = inputstate_post,
  
)

cces_2016$recode_inputstate <- droplevels(cces_2016$recode_inputstate)
cces_2016$recode_educ <- droplevels(cces_2016$recode_educ)

cces_2016 <- cces_2016 %>% mutate(
  #if responded don't know/refused 
  #unitarians are protestant (only 4 of these), Christains are protestant (269)
  recode_relig = factor(case_when(
    religpew == "Protestant" ~ "Protestant",
    religpew == "Jewish" ~ "Jewish",
    religpew == "Roman Catholic" ~ "Catholic",
    religpew == "Nothing in particular" ~ "Nothing in particular",
    religpew == "Agnostic" ~ "Agnostic",
    religpew == "Atheist" ~ "Atheist",
    religpew == "Buddhist" ~ "Buddhist",
    religpew == "Muslim" ~ "Muslim",
    religpew == "Mormon" ~ "Mormon",
    religpew == "Hindu" ~ "Hindu",
    religpew == "Eastern or Greek Orthodox" ~ "Orthodox",
    religpew == "Something else" ~ "Something else",
    #NAs remain NAs 38
    TRUE ~ NA_character_),
    levels = c("Protestant", "Jewish", "Catholic", "Nothing in particular", 
               "Agnostic", "Atheist", "Buddhist", "Muslim", "Mormon", "Hindu",
               "Orthodox", "Something else")),
  
  #22 NAs
  recode_born = case_when(pew_bornagain == "No" ~ "No",
                          pew_bornagain == "Yes" ~ "Yes", 
                          #NAs (22) assume to be not
                          TRUE ~ "No"),
  
  
  recode_income = factor(case_when(
    faminc == "Less than $10,000" ~ "<10k",
    faminc == "$10,000 - $19,999" ~ "10-20k",
    faminc == "$20,000 - $29,999" ~ "20-30k",
    faminc == "$30,000 - $39,999" ~ "30-40k",
    faminc == "$40,000 - $49,999" ~ "40-50k",
    faminc == "$50,000 - $59,999" ~ "50-100k",
    faminc == "$60,000 - $69,999" ~ "50-100k",
    faminc == "$70,000 - $79,999" ~ "50-100k",
    faminc == "$80,000 - $99,999" ~ "50-100k",
    faminc == "$100,000 - $119,999" ~ "100-150k",
    faminc == "$120,000 - $149,999" ~ "100-150k",
    faminc == "$150,000 - $199,999" ~ ">150k",
    faminc == "$200,000 - $249,999" ~ ">150k",
    faminc == "$250,000 - $349,999" ~ ">150k",
    faminc == "$350,000 - $499,999" ~ ">150k",
    faminc == "$500,000 or more" ~ ">150k",
    faminc == "$150,000 or more" ~ ">150k",
    #4788 do not say
    faminc == "Prefer not to say" ~ "Prefer not to say",
    #9 NAs say they are also prefer not to say
    TRUE ~ "Prefer not to say"),
    levels = c("<10k", "10-20k", "20-30k", "30-40k", "40-50k", "50-100k",
               "100-150k",">150k","Prefer not to say")),
  
  recode_income_5way = factor(case_when(
    faminc == "Less than $10,000" ~ "<20k",
    faminc == "$10,000 - $19,999" ~ "<20k",
    faminc == "$20,000 - $29,999" ~ "20-50k",
    faminc == "$30,000 - $39,999" ~ "20-50k",
    faminc == "$40,000 - $49,999" ~ "20-50k",
    faminc == "$50,000 - $59,999" ~ "50-100k",
    faminc == "$60,000 - $69,999" ~ "50-100k",
    faminc == "$70,000 - $79,999" ~ "50-100k",
    faminc == "$80,000 - $99,999" ~ "50-100k",
    faminc == "$100,000 - $119,999" ~ "100-150k",
    faminc == "$120,000 - $149,999" ~ "100-150k",
    faminc == "$150,000 - $199,999" ~ ">150k",
    faminc == "$200,000 - $249,999" ~ ">150k",
    faminc == "$250,000 - $349,999" ~ ">150k",
    faminc == "$350,000 - $499,999" ~ ">150k",
    faminc == "$500,000 or more" ~ ">150k",
    faminc == "$150,000 or more" ~ ">150k",
    #4788 do not say
    faminc == "Prefer not to say" ~ "Prefer not to say",
    #9 NAs
    TRUE ~ "Prefer not to say"),
    levels = c("<20k", "20-50k", "50-100k", "100-150k",">150k","Prefer not to say")),
  recode_income_4way = factor(case_when(
    faminc == "Less than $10,000" ~ "<50k",
    faminc == "Less than $10,000" ~ "<50k",
    faminc == "$10,000 - $19,999" ~ "<50k",
    faminc == "$20,000 - $29,999" ~ "<50k",
    faminc == "$30,000 - $39,999" ~ "<50k",
    faminc == "$40,000 - $49,999" ~ "<50k",
    faminc == "$50,000 - $59,999" ~ "50-100k",
    faminc == "$60,000 - $69,999" ~ "50-100k",
    faminc == "$70,000 - $79,999" ~ "50-100k",
    faminc == "$80,000 - $99,999" ~ "50-100k",
    faminc == "$100,000 - $119,999" ~ "100-150k",
    faminc == "$120,000 - $149,999" ~ "100-150k",
    faminc == "$150,000 - $199,999" ~ ">150k",
    faminc == "$200,000 - $249,999" ~ ">150k",
    faminc == "$250,000 - $349,999" ~ ">150k",
    faminc == "$350,000 - $499,999" ~ ">150k",
    faminc == "$500,000 or more" ~ ">150k",
    faminc == "$150,000 or more" ~ ">150k",
    #4788 do not say
    faminc == "Prefer not to say" ~ "Prefer not to say",
    #9 NAs
    TRUE ~ "Prefer not to say"),
    levels = c("<50k", "50-100k", "100-150k",">150k", "Prefer not to say")),
  
  recode_income_3way = factor(case_when(
    faminc == "Less than $10,000" ~ "<50k",
    faminc == "Less than $10,000" ~ "<50k",
    faminc == "$10,000 - $19,999" ~ "<50k",
    faminc == "$20,000 - $29,999" ~ "<50k",
    faminc == "$30,000 - $39,999" ~ "<50k",
    faminc == "$40,000 - $49,999" ~ "<50k",
    faminc == "$50,000 - $59,999" ~ "50-150k",
    faminc == "$60,000 - $69,999" ~ "50-150k",
    faminc == "$70,000 - $79,999" ~ "50-150k",
    faminc == "$80,000 - $99,999" ~ "50-150k",
    faminc == "$100,000 - $119,999" ~ "50-150k",
    faminc == "$120,000 - $149,999" ~ "50-150k",
    faminc == "$150,000 - $199,999" ~ ">150k",
    faminc == "$200,000 - $249,999" ~ ">150k",
    faminc == "$250,000 - $349,999" ~ ">150k",
    faminc == "$350,000 - $499,999" ~ ">150k",
    faminc == "$500,000 or more" ~ ">150k",
    faminc == "$150,000 or more" ~ ">150k",
    #4788 do not say
    faminc == "Prefer not to say" ~ "Prefer not to say",
    #9 NAs
    TRUE ~ "Prefer not to say"),
    levels = c("<50k", "50-150k",">150k","Prefer not to say"))
)


cces_2016 <- select(cces_2016,
                    "recode_age_bucket", "recode_female", "recode_race", 
                    "recode_region", "recode_pid_3way", "recode_educ", "recode_inputstate",
                    "recode_relig", "recode_born", "recode_income", "recode_income_5way", "recode_educ_3way",
                    "recode_income_3way", "recode_age_3way",
                    "recode_vote_2016", "commonweight_vv_post")

saveRDS(cces_2016, "./generated/cces.rds")
