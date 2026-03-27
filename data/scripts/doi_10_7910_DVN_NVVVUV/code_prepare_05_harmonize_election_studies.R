############################################################################
############################################################################
#### 
#### Constantine Boussalis, Travis G. Coan, Mirya R. Holman, Stefan Müller
#### Gender, Candidate Emotional Expression,
#### and Voter Reactions During Televised Debates 
####
#### American Political Science Review 
####
#### Note: this script is only required for data preparation 
### and does not need to be executed to reproduce any analyses reported
### in the paper or online appendix
############################################################################
############################################################################


library(dplyr)   # CRAN v1.0.5
library(tidyr)   # CRAN v1.1.3
library(foreign) # CRAN v0.8-81
library(forcats) # CRAN v0.5.1
library(stringr) # CRAN v1.4.0
library(vroom)   # CRAN v1.4.0
library(car)     # CRAN v3.0-10

# This script compares the RTR respondents
# with respondents from representative pre-election studies. 
# The pre-election studies are from the GESIS website.
# To reproduce this script, you need to download these raw datasets
# from the GESIS website after a free registration and put these files 
# into a folder called "data_dontshare".

# The RTR files are also 

# links to pre-election surveys
# 2017: https://dbk.gesis.org/dbksearch/sdesc2.asp?no=6800&db=e&doi=10.4232/1.13234
# 2013: https://dbk.gesis.org/dbksearch/sdesc2.asp?no=6800&db=e&doi=10.4232/1.13234
# 2009: https://dbk.gesis.org/dbksearch/sdesc2.asp?no=6800&db=e&doi=10.4232/1.13234
# 2005: https://doi.org/10.4232/1.11463

# set working directory

setwd("")

## 2017 ----

# load RTR samples


dat_2017_rtr <- vroom("data_for_analysis_2017.csv")

# select only relevant variables and keep one observation per respondent
dat_2017_rtr <- dat_2017_rtr %>% 
    select(respondent_id, gender, party_id, age, political_interest) %>% 
    unique() %>% 
    mutate(type = "RTR Sample")


## 2017: minor parties ----

# load RTR respondents
dat_2017_minor_rtr <- vroom("data_for_analysis_2017_minor.csv")


# select only relevant variables and keep one observation per respondent

dat_2017_minor_rtr <- dat_2017_minor_rtr %>% 
    select(respondent_id, gender, party_id, age, political_interest) %>% 
    unique() %>% 
    mutate(type = "RTR Sample")

table(dat_2017_minor_rtr$age)

table(dat_2017_minor_rtr$party_id)


# load 2017 survey 
# plese download the following dataset and copy it into a folder 
# called "data_dontshare"

# https://dbk.gesis.org/dbksearch/sdesc2.asp?no=6800&db=e&doi=10.4232/1.13234

dat_2017_survey <- read.dta("/Users/smueller/Dropbox/papers/german_debates_project/replication_materials_apsr/data_dontshare/ZA6800_de_v5-0-1.dta")

# select and rename relevant variables
dat_2017_survey_subset <- dat_2017_survey %>% 
    select(party_id_raw = q99b,
           gender = q1,
           political_interest = q3,
           year_birth = q2a) %>% 
    na.omit() %>% 
    mutate(age = 2017 - as.numeric(year_birth)) %>% 
    filter(age >= 18)


# recode variables

table(dat_2017_survey_subset$gender)

recode_gender17 <- c("'maennlich'='Male';
                   'weiblich'='Female'")

dat_2017_survey_subset <- dat_2017_survey_subset %>% 
    mutate(gender = car::recode(gender, recode_gender17)) %>% 
    mutate(respondent_id = paste0("respondent_", 1:n()))


table(dat_2017_survey_subset$political_interest)

recode_political_interest17 <- c("'ueberhaupt nicht'='0 no interest at all/no answer';
                               'weniger stark'='1 not very strong';
                               'mittelmaessig'='2 medium interest';
                               'stark'='3 strong';
                               'sehr stark'='4 very strong';
                               'keine Angabe'='0 no interest at all/no answer'")

dat_2017_survey_subset <- dat_2017_survey_subset %>% 
    mutate(political_interest = car::recode(political_interest, recode_political_interest17))

table(dat_2017_survey_subset$political_interest)

table(dat_2017_survey_subset$party_id_raw)

recode_party_id17 <- c("'keine Partei'='No party ID/No answer';
                     'andere Partei'='Other party';
                     'DIE LINKE'='Left Party';
                     'AfD'='AfD';
                     'GRUENE'='Greens';
                     'CDU'='CDU/CSU';
                     'CSU'='CDU/CSU';
                     'CDU/CSU'='CDU/CSU';
                     'FDP'='FDP';
                     'SPD'='SPD';
                     'weiss nicht'='No party ID/No answer';
                     'keine Angabe'='No party ID/No answer';
                     else='Other party'")

dat_2017_survey_subset <- dat_2017_survey_subset %>% 
    mutate(party_id = car::recode(party_id_raw, recode_party_id17)) %>% 
    mutate(type = "Representative Pre-Election Survey")

table(dat_2017_survey_subset$party_id)

dat_2017_survey_subset <- dat_2017_survey_subset %>% 
    mutate(age_cat = case_when(
        between(age, 18, 24) ~ "18-24",
        between(age, 25, 34) ~ "25-34",
        between(age, 35, 44) ~ "35-44",
        between(age, 45, 54) ~ "45-55",
        between(age, 55, 64) ~ "55-64",
        age > 64 ~ "65+"
    ))

table(dat_2017_survey_subset$age,dat_2017_survey_subset$age_cat, useNA = "always")

dat_2017_rtr <- dat_2017_rtr %>% 
    mutate(age_cat = case_when(
        between(age, 18, 24) ~ "18-24",
        between(age, 25, 34) ~ "25-34",
        between(age, 35, 44) ~ "35-44",
        between(age, 45, 54) ~ "45-55",
        between(age, 55, 64) ~ "55-64",
        age > 64 ~ "65+"
    ))


dat_2017_minor_rtr <- dat_2017_minor_rtr %>% 
    mutate(age_cat = case_when(
        between(age, 18, 24) ~ "18-24",
        between(age, 25, 34) ~ "25-34",
        between(age, 35, 44) ~ "35-44",
        between(age, 45, 54) ~ "45-55",
        between(age, 55, 64) ~ "55-64",
        age > 64 ~ "65+"
    ))


# change from wide to long format for plotting
dat_2017_survey_long_cats <- dat_2017_survey_subset %>% 
    select(type, party_id, gender, political_interest, age_cat) %>% 
    gather(variable, value, -type)

dat_2017_rtr_long_cats <- dat_2017_rtr %>% 
    select(type, party_id, gender, political_interest, age_cat) %>% 
    gather(variable, value, -type)


dat_2017_minor_rtr_long_cats <- dat_2017_minor_rtr %>% 
    dplyr::select(type, party_id, gender, political_interest, age_cat) %>% 
    gather(variable, value, -type)


# combine RTR data and pre-election study for 2017 and 2017 (minor parties)
dat_merged_2017_cats <- bind_rows(dat_2017_survey_long_cats,
                                  dat_2017_rtr_long_cats) 


dat_merged_2017_minor_cats <- bind_rows(dat_2017_survey_long_cats,
                                        dat_2017_minor_rtr_long_cats) 



# summarise measures for 2017 and 2017 (minor parties)

dat_merged_2017_sum <- dat_merged_2017_cats %>% 
    group_by(type, variable, value) %>% 
    summarise(n = n()) %>%
    mutate(freq = n / sum(n)) %>% 
    mutate(year = "2017")


dat_merged_2017_minor_sum <- dat_merged_2017_minor_cats %>% 
    group_by(type, variable, value) %>% 
    summarise(n = n()) %>%
    mutate(freq = n / sum(n)) %>% 
    mutate(year = "2017 (minor)")


## 2013 ----

# load RTR data
dat_2013_rtr <-  vroom("data_for_analysis_2013.csv")


# select only relevant variables and keep one observation per respondent
dat_2013_rtr <- dat_2013_rtr %>% 
    select(respondent_id, gender, party_id, age, political_interest) %>% 
    unique() %>% 
    mutate(type = "RTR Sample")


# please download the following dataset and put it into a folder called
# "data_dontshare"

# https://dbk.gesis.org/dbksearch/sdesc2.asp?no=6800&db=e&doi=10.4232/1.13234

dat_2013_survey <- read.dta("/Users/smueller/Dropbox/papers/german_debates_project/replication_materials_apsr/data_dontshare/ZA5700_de_v2-0-2.dta")

# select and rename relevant variables from survey
dat_2013_survey_subset <- dat_2013_survey %>% 
    select(party_id_raw = q119b,
           gender = q1,
           political_interest = q3,
           year_birth = q2c) %>% 
    na.omit() %>% 
    mutate(age = 2013 - as.numeric(year_birth)) %>% 
    filter(age >= 18) 


# recode variables
table(dat_2013_survey_subset$gender)


recode_gender13 <- c("'maennlich'='Male';
                   'weiblich'='Female'")


table(dat_2013_survey_subset$political_interest)

recode_political_interest13 <- c("'ueberhaupt nicht'='0 no interest at all/no answer';
                               'weniger stark'='1 not very strong';
                               'keine Angabe'='0 no interest at all/no answer';
                               'weiss nicht'='0 no interest at all/no answer';
                               'mittelmaessig'='2 medium interest';
                               'stark'='3 strong';
                               'sehr stark'='4 very strong'")


table(dat_2013_survey_subset$party_id)

recode_party_id13 <- c("'keine Partei keiner Partei'='No party ID/No answer';
                     'PIRATEN'='Other party';
                     'andere Partei'='Other party';
                     'CDU/CSU'='CDU/CSU';
                     'CDU'='CDU/CSU';
                     'CSU'='CDU/CSU';
                     'FDP'='FDP';
                     'SPD'='SPD';
                     'DIE LINKE'='Left Party';
                     'GRUENE'='Greens';
                     'weiss nicht'='No party ID/No answer';
                     'keine Angabe'='No party ID/No answer';
                     else='Other party'")


dat_2013_survey_subset <- dat_2013_survey_subset %>% 
    mutate(political_interest = car::recode(political_interest, recode_political_interest13)) %>% 
    mutate(gender = car::recode(gender, recode_gender13)) %>% 
    mutate(party_id_raw = str_replace_all(party_id_raw, ";", "")) %>% 
    mutate(party_id = car::recode(party_id_raw, recode_party_id13)) %>% 
    mutate(type = "Representative Pre-Election Survey")

table(dat_2013_survey_subset$political_interest)

table(dat_2013_survey_subset$party_id,
      dat_2013_survey_subset$party_id_raw)


# recode age into categories
dat_2013_survey_subset <- dat_2013_survey_subset %>% 
    mutate(age_cat = case_when(
        between(age, 18, 24) ~ "18-24",
        between(age, 25, 34) ~ "25-34",
        between(age, 35, 44) ~ "35-44",
        between(age, 45, 54) ~ "45-55",
        between(age, 55, 64) ~ "55-64",
        age > 64 ~ "65+"
    ))


# recode interest in RTR data

dat_2013_rtr <- dat_2013_rtr %>%
    mutate(political_interest = str_replace_all(political_interest,
                                                "at all", "at all/no answer"))


dat_2013_rtr <- dat_2013_rtr %>% 
    mutate(age_cat = case_when(
        between(age, 18, 24) ~ "18-24",
        between(age, 25, 34) ~ "25-34",
        between(age, 35, 44) ~ "35-44",
        between(age, 45, 54) ~ "45-55",
        between(age, 55, 64) ~ "55-64",
        age > 64 ~ "65+"
    ))

# reshape to long format for plot
dat_2013_survey_long_cats <- dat_2013_survey_subset %>% 
    select(type, party_id, gender, political_interest, age_cat) %>% 
    gather(variable, value, -type)

dat_2013_rtr_long_cats <- dat_2013_rtr %>% 
    ungroup() %>% 
    select(type, party_id, gender, political_interest, age_cat) %>% 
    gather(variable, value, -type)


# bind RTR and survey respondents
dat_merged_2013_cats <- bind_rows(dat_2013_survey_long_cats,
                                  dat_2013_rtr_long_cats) 


# summarise dataset
dat_merged_2013_sum <- dat_merged_2013_cats %>% 
    group_by(type, variable, value) %>% 
    summarise(n = n()) %>%
    mutate(freq = n / sum(n)) %>% 
    mutate(year = "2013")



## 2009 ----

dat_2009_rtr <- vroom("data_for_analysis_2009.csv")


dat_2009_rtr <- dat_2009_rtr %>% 
    select(respondent_id, gender, party_id, age, political_interest) %>% 
    unique() %>% 
    mutate(type = "RTR Sample")

table(dat_2009_rtr$gender)


# please download the following dataset and put it into a folder called
# "data_dontshare"

# https://dbk.gesis.org/dbksearch/sdesc2.asp?no=6800&db=e&doi=10.4232/1.13234

dat_2009_survey <- read.dta("/Users/smueller/Dropbox/papers/german_debates_project/replication_materials_apsr/data_dontshare/ZA5300_de_v5-0-2.dta")


summary(dat_2009_rtr)

dat_2009_survey_subset <- dat_2009_survey %>% 
    select(party_id_raw = q139b,
           gender = q1,
           political_interest = q2,
           age = d201rc) %>% 
    filter(!age %in% c(999, "keine Angabe")) %>% 
    #na.omit() %>% 
    mutate(age = as.numeric(age)) %>% 
    filter(age >= 18)


recode_gender09 <- c("'maennlich'='Male';
                   'weiblich'='Female'")


table(dat_2009_survey_subset$political_interest)

recode_political_interest09 <- c("'ueberhaupt nicht'='0 no interest at all/no answer';
                                 'keine Angabe'='0 no interest at all/no answer';
                                 'weiss nicht'='0 no interest at all/no answer';
                               'weniger stark'='1 not very strong';
                               'mittelmaessig'='2 medium interest';
                               'ziemlich stark'='3 strong';
                               'sehr stark'='4 very strong'")


table(dat_2009_survey_subset$party_id)

recode_party_id09 <- c("'keine Partei keiner Partei'='No party ID/No answer';
                     'PIRATEN'='Other party';
                     'andere Partei'='Other party';
                     'CDU/CSU'='CDU/CSU';
                     'CDU'='CDU/CSU';
                     'CSU'='CDU/CSU';
                     'FDP'='FDP';
                     'SPD'='SPD';
                     'DIE LINKE'='Left Party';
                     'GRUENE'='Greens';
                     'weiss nicht'='No party ID/No answer';
                     'keine Angabe'='No party ID/No answer';
                     else='Other party'")


dat_2009_survey_subset <- dat_2009_survey_subset %>% 
    mutate(political_interest = car::recode(political_interest, recode_political_interest09)) %>% 
    mutate(gender = car::recode(gender, recode_gender09)) %>% 
    mutate(party_id_raw = str_replace_all(party_id_raw, ";", "")) %>% 
    mutate(party_id = car::recode(party_id_raw, recode_party_id09)) %>% 
    mutate(type = "Representative Pre-Election Survey")

table(dat_2009_survey_subset$age)

# recode age into categories
dat_2009_survey_subset <- dat_2009_survey_subset %>% 
    mutate(age = as.numeric(age)) %>% 
    mutate(age_cat = case_when(
        between(age, 18, 24) ~ "18-24",
        between(age, 25, 34) ~ "25-34",
        between(age, 35, 44) ~ "35-44",
        between(age, 45, 54) ~ "45-55",
        between(age, 55, 64) ~ "55-64",
        age > 64 ~ "65+"
    ))


# recode interest

dat_2009_rtr <- dat_2009_rtr %>% 
    mutate(age_cat = case_when(
        between(age, 18, 24) ~ "18-24",
        between(age, 25, 34) ~ "25-34",
        between(age, 35, 44) ~ "35-44",
        between(age, 45, 54) ~ "45-55",
        between(age, 55, 64) ~ "55-64",
        age > 64 ~ "65+"
    ))


# change to long format
dat_2009_survey_long_cats <- dat_2009_survey_subset %>% 
    select(type, party_id, gender, political_interest, age_cat) %>% 
    gather(variable, value, -type)

dat_2009_rtr_long_cats <- dat_2009_rtr %>% 
    select(type, party_id, gender, political_interest, age_cat) %>% 
    gather(variable, value, -type)


# merge RTR and pre-election respondents
dat_merged_2009_cats <- bind_rows(dat_2009_survey_long_cats,
                                  dat_2009_rtr_long_cats) 


# summarise data
dat_merged_2009_sum <- dat_merged_2009_cats %>% 
    group_by(type, variable, value) %>% 
    summarise(n = n()) %>%
    mutate(freq = n / sum(n)) %>% 
    mutate(year = "2009")



## 2005 ----

dat_2005_rtr <- vroom("data_for_analysis_2005.csv")

# select only relevant variables from survey and recode variables

dat_2005_rtr <- dat_2005_rtr %>% 
    select(age, political_interest, party_id, gender) %>% 
    unique()


# please download the following dataset and put it into a folder called
# "data_dontshare"

# https://doi.org/10.4232/1.11463

dat_2005_survey <- read.dta("/Users/smueller/Dropbox/papers/german_debates_project/replication_materials_apsr/data_dontshare/ZA4332_v2-0-0.dta")

# select and rename relevant variables
dat_2005_survey_select <- dat_2005_survey %>% 
    select(year_birth = s02a,
           political_interest = f005,
           party_id_raw = s19, 
           gender = s01) %>% 
    mutate(year_birth = as.numeric(as.character(year_birth))) %>% 
    mutate(age = 2005 - as.numeric(year_birth)) %>% 
    na.omit()


# recode variables
table(dat_2005_survey_select$political_interest)

recode_political_interest05 <- c("'ueberhaupt nicht'='0 no interest at all/no answer';
                                 'keine Angabe'='0 no interest at all/no answer';
                                 'weiss nicht'='0 no interest at all/no answer';
                               'wenig'='1 not very strong';
                               'mittel'='2 medium interest';
                               'ziemlich stark'='3 strong';
                               'sehr stark'='4 very strong'")

dat_2005_survey_select <- dat_2005_survey_select %>% 
    mutate(political_interest = car::recode(political_interest, recode_political_interest05))

table(dat_2005_survey_select$political_interest)

recode_party_id05 <- c("'keine Partei'='No party ID/No answer';
                     'andere Partei'='Other party';
                     'CDU/CSU'='CDU/CSU';
                     'CDU'='CDU/CSU';
                     'CSU'='CDU/CSU';
                     'FDP'='FDP';
                     'SPD'='SPD';
                     'Linkspartei/PDS'='Left Party';
                     'Buendnis 90/Die Gruenen'='Greens';
                     'weiss nicht'='No party ID/No answer';
                     'keine Angabe'='No party ID/No answer';
                     else='Other party'")



dat_2005_survey_select <- dat_2005_survey_select %>% 
    mutate(party_id = car::recode(party_id_raw, recode_party_id05))

table(dat_2005_survey_select$party_id)

table(dat_2005_survey_select$gender)

dat_2005_survey_select <- dat_2005_survey_select %>% 
    mutate(gender = car::recode(gender, "'Mann'='Male';'Frau'='Female'"))



table(dat_2009_survey_subset$age)

# recode age into categories
dat_2005_survey_select <- dat_2005_survey_select %>% 
    mutate(age_cat = case_when(
        between(age, 18, 24) ~ "18-24",
        between(age, 25, 34) ~ "25-34",
        between(age, 35, 44) ~ "35-44",
        between(age, 45, 54) ~ "45-55",
        between(age, 55, 64) ~ "55-64",
        age > 64 ~ "65+"
    )) %>% 
    mutate(type = "Representative Pre-Election Survey")


# recode political interest

dat_2005_rtr <- dat_2005_rtr %>% 
    mutate(age_cat = case_when(
        between(age, 18, 24) ~ "18-24",
        between(age, 25, 34) ~ "25-34",
        between(age, 35, 44) ~ "35-44",
        between(age, 45, 54) ~ "45-55",
        between(age, 55, 64) ~ "55-64",
        age > 64 ~ "65+"
    )) %>% 
    mutate(type = "RTR Sample")


# transform to long format
dat_2005_survey_long_cats <- dat_2005_survey_select %>% 
    select(type, party_id, gender, political_interest, age_cat) %>% 
    gather(variable, value, -type)

dat_2005_rtr_long_cats <- dat_2005_rtr %>% 
    select(type, party_id, gender, political_interest, age_cat) %>% 
    gather(variable, value, -type)


# merge RTR and pre-selection respondents
dat_merged_2005_cats <- bind_rows(dat_2005_survey_long_cats,
                                  dat_2005_rtr_long_cats) 


# summarise data
dat_merged_2005_sum <- dat_merged_2005_cats %>% 
    group_by(type, variable, value) %>% 
    summarise(n = n()) %>%
    mutate(freq = n / sum(n)) %>% 
    mutate(year = "2005")



# merge summarised data from all years/debates
dat_merged_sum <- bind_rows(dat_merged_2017_sum,
                            dat_merged_2017_minor_sum,
                            dat_merged_2013_sum,
                            dat_merged_2009_sum,
                            dat_merged_2005_sum) %>% 
    mutate(variable = car::recode(variable, "'gender'='Gender';'party_id'='Party ID';
                                  'political_interest'='Pol. Interest';
                                  'age_cat'='Age'")) %>% 
    mutate(value = str_replace_all(value, "no answer", "NA"))


# create nicer variable for plot
dat_merged_sum <- dat_merged_sum %>% 
    mutate(year_var = paste0(year, ": ", variable))

# save this file as a csv file which will be used to create Figure A11
write.csv(dat_merged_sum, "data_compare_respondents.csv")
