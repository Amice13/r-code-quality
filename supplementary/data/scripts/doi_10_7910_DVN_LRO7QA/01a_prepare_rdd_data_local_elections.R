####################################################################
####################################################################
## Replication Material
##
## Michael Jankowski and Stefan Müller
##
## michael.jankowski@uol.de | stefan.mueller@ucd.ie
## 
## Incumbency Advantage in Lower-Order PR Elections:
## Evidence from the Irish Context, 1942-2019
## 
## Electoral Studies

## File: 01a_prepare_rdd_data_local_elections.R

## See 00_description_data_and_scripts.pdf for a detailed 
## overview of the required data and the outputs of this file.

####################################################################
####################################################################

library(dplyr)    # CRAN v1.0.4
library(car)      # CRAN v3.0-10
library(stringr)  # CRAN v1.4.0
library(quanteda) # CRAN v2.1.2

# load raw data of local election results
complete_raw <- readRDS("data_elections_local_raw.rds")

# load dataset with TDs
dta_dail_members <- read.csv("data_tds.csv", 
                             fileEncoding = "utf-8") %>% 
  mutate(td_type = "Elected in general election")


# check number of TDs per legislative period
dta_dail_members %>% 
  group_by(dail_number) %>% 
  count() %>% 
  head(20)


# create new variable for merging names
dta_dail_members <- dta_dail_members %>% 
  rename(candidate_dail = candidate) %>% 
  mutate(candidate_merge = str_to_lower(candidate_dail),
         candidate_merge = str_replace_all(candidate_merge, " ", ""),
         candidate_merge = str_replace_all(candidate_merge, "-", ""),
         candidate_merge = str_replace_all(candidate_merge, "í", "i"),
         candidate_merge = str_replace_all(candidate_merge, "é", "e"),
         candidate_merge = str_replace_all(candidate_merge, "á", "a"),
         candidate_merge = str_replace_all(candidate_merge, "ë", "e"),
         candidate_merge = str_replace_all(candidate_merge, "'", ""),
         candidate_merge = str_replace_all(candidate_merge, '"', ''),
         candidate_merge = str_replace_all(candidate_merge, "ó", 'o'),
         candidate_merge = str_replace_all(candidate_merge, "\\.", ""),
         candidate_merge = str_replace_all(candidate_merge, ",", ""))



# check remaining characters
dta_dail_members %>% 
  corpus(text_field = "candidate_merge") %>% 
  tokens(what = "character") %>% 
  dfm() %>% 
  topfeatures(n = 50)


# create new variable for merging of names
complete_raw <- complete_raw %>% 
  mutate(candidate_merge = str_to_lower(candidate),
         candidate_merge = str_replace_all(candidate_merge, " ", ""),
         candidate_merge = str_replace_all(candidate_merge, "-", ""),
         candidate_merge = str_replace_all(candidate_merge, "í", "i"),
         candidate_merge = str_replace_all(candidate_merge, "é", "e"),
         candidate_merge = str_replace_all(candidate_merge, "á", "a"),
         candidate_merge = str_replace_all(candidate_merge, "ë", "e"),
         candidate_merge = str_replace_all(candidate_merge, "'", ""),
         candidate_merge = str_replace_all(candidate_merge, '"', ''),
         candidate_merge = str_replace_all(candidate_merge, "ó", 'o'),
         candidate_merge = str_replace_all(candidate_merge, "\\.", ""),
         candidate_merge = str_replace_all(candidate_merge, ",", ""))




# check remaining characters
complete_raw %>% 
  corpus(text_field = "candidate_merge") %>% 
  tokens(what = "character") %>% 
  dfm() %>% 
  topfeatures(n = 50)


# load dates of general elections
dta_dail_dates <- read.csv("data_dates_general_elections.csv",
                           fileEncoding = "utf-8")

# merge TDs and dates of general elections
dta_dail_members <- left_join(dta_dail_members, 
                              dta_dail_dates, by = "dail_number")

# transform format of election date
dta_dail_members <- dta_dail_members %>% 
  mutate(election_date_dail = as.Date(election_date_dail, "%d/%m/%Y"))


# load dataset of by-elections
dta_bye_elections <- read.csv("data_dates_byeelections.csv",
                              fileEncoding = "utf-8")


dta_bye_elections <- dta_bye_elections %>% 
  rename(candidate_dail = name) %>% 
  mutate(candidate_merge = str_to_lower(candidate_dail),
         candidate_merge = str_replace_all(candidate_merge, " ", ""),
         candidate_merge = str_replace_all(candidate_merge, "-", ""),
         candidate_merge = str_replace_all(candidate_merge, "í", "i"),
         candidate_merge = str_replace_all(candidate_merge, "é", "e"),
         candidate_merge = str_replace_all(candidate_merge, "á", "a"),
         candidate_merge = str_replace_all(candidate_merge, "ë", "e"),
         candidate_merge = str_replace_all(candidate_merge, "'", ""),
         candidate_merge = str_replace_all(candidate_merge, '"', ''),
         candidate_merge = str_replace_all(candidate_merge, "ó", 'o'),
         candidate_merge = str_replace_all(candidate_merge, "\\.", ""),
         candidate_merge = str_replace_all(candidate_merge, ",", ""))




# check remaining characters
dta_bye_elections %>% 
  corpus(text_field = "candidate_merge") %>% 
  tokens(what = "character") %>% 
  dfm() %>% 
  topfeatures(n = 50)



# load dataset of local election dates
dta_election_dates_local <- read.csv("data_dates_local_elections.csv",
                                     fileEncoding = "utf-8") %>% 
  select(-date_certain) %>% 
  mutate(election_date = as.Date(election_date, "%d/%m/%Y"))


# merge TDs with local election dates by the D?il ID
# election_year indicates the date of a local election
# while election_date_dail indicates the date of the previous general election
dta_dail <- left_join(dta_dail_members, 
                      dta_election_dates_local, 
                      by = "dail_number") 


# make small changes for data on bye elections (rename variables and select only some variables)
dta_by_elections_relevant <- dta_bye_elections %>% 
  select(constituency, party, candidate_merge, candidate_dail, dail_number, year_next_local_election) %>% 
  rename(constituency_dail = constituency,
         party_dail = party, 
         election_year = year_next_local_election) %>% 
  mutate(td_type = "Elected in bye-election")


# combine elected TDs and TDs elected through by-election
# (and do not consider time after 2002 (no dual mandate anymore))
dta_dail_full <- bind_rows(dta_dail, dta_by_elections_relevant) %>%
  filter(election_year < 2002)

nrow(dta_dail_full)

## Rename party variables on Dail level to make it comparable to party names
# on constituency level
dta_dail_full$party_dail[dta_dail_full$party_dail=="Workers' Party"] <- "Workers Party"

dta_dail_full$party_dail_short <- car::recode(dta_dail_full$party_dail, "'Democratic Socialist Party'='DS';
                                              'Fianna Fail'='FF'; 'Fine Gael'='FG'; 'Socialist Party'='SP';
                                              'Green Party'='GP'; 'Labour Party'='LAB';
                                              'Progressive Democrats'='PD';
                                              'Sinn Fein'='SF'; 'FF'='FF';'FG'='FG';
                                              'Workers Party'='WP';'Democratic Left'='Democratic Left';
                                              'Independent'='IND';
                                              'SF'='SF';
                                              'Clann na Talmhan'='CnT';
                                              'Clann na Poblachta'='CnP';
                                              'Lab'='LAB';else='OTHER'") 



# remove some TDs who would be merged incorrectly in the next step 
# (same name in same Dail, but two different parties)

dta_dail_full_unique <- dta_dail_full %>%
  mutate(candidate_year_party = paste(candidate_dail, election_year, party_dail, sep = "_")) %>%
  filter(!candidate_year_party %in% c("John Browne_1991_Fine Gael",
                                      "John Browne_1999_Fine Gael",
                                      "Patrick Burke_1945_Fianna Fail"))

# write_csv(dta_dail_full_unique, "data/data_dail_clean.csv")

# rename dta_bye_elections district names to "council"
complete_raw <- complete_raw %>% 
  dplyr::rename(council_name = district_name,
                council_link = district_link)

# create ID for each list
complete_raw$list_id <- complete_raw$const_link %>% as.factor %>% as.numeric

# number of unique lists
complete_raw$list_id %>% unique %>% length

# create additional variables (mostly through regular expressions)
complete_raw$dm <- stringr::str_extract_all(complete_raw$dm, "[0-9]+") %>% unlist %>% as.numeric()
complete_raw$quota <- stringr::str_extract_all(complete_raw$quota, "[0-9]+") %>% unlist %>% as.numeric()
complete_raw$eligible <- gsub("Electorate: |Turnout.*", "", complete_raw$voter_info) %>% as.numeric
complete_raw$votes_cast <- gsub(".*Turnout: |Valid.*", "", complete_raw$voter_info) %>% as.numeric
complete_raw$valid_votes <- gsub(".*Valid: |Spoilt.*", "", complete_raw$voter_info) %>% as.numeric
complete_raw$spoilt_votes <- gsub(".*Spoilt: ", "", complete_raw$voter_info) %>% as.numeric
complete_raw$turnout <- 100*complete_raw$votes_cast/complete_raw$eligible
complete_raw$turnout[complete_raw$turnout < 1 | complete_raw$turnout>100] <- NA
complete_raw$eligible[complete_raw$turnout < 1 | complete_raw$turnout>100] <- NA

# extract IDs of consistuencies and elections
complete_raw$election_id <- stringr::str_extract_all(complete_raw$const_link, "elecid=[0-9]+") %>% unlist %>% gsub("elecid=", "", .) %>% as.numeric()
complete_raw$constit_id <- stringr::str_extract_all(complete_raw$const_link, "constitid=[0-9]+") %>% unlist %>% gsub("constitid=", "", .) %>% as.numeric()

table(complete_raw$election_id)

# create data frame of election years and the corresponding election ID from irelandelection.com
elections <- data.frame(election_id = c(237, 157, 175,174,173,171:162), 
                        election_year = c(2019, 2014, 2009, 2004, 1999, 1991, 1985, 1979, 1974, 1967, 1960, 1955, 1950, 1945, 1942))

# merge election IDs and election dates
complete_raw <- merge(complete_raw, elections, by = "election_id")

# reorder dataset by election year
complete_raw <- arrange(complete_raw, election_year)

table(complete_raw$election_year)

# recode some of the parties
complete_raw$party[complete_raw$party=="Cha O'Neill IND"] <- "IND"

complete_raw <- complete_raw %>% 
  mutate(party = car::recode(party, "'Cosai Fitzgerald FG'='FG';
                             'Coole Ryan FF'='FF';
                             'Chap Clere FF'='FF'; 'Boxer Moran IND'='IND';
                             'Scarteen Connor FG'='FG';
                             'Seffin FF'='FF';
                             'Gusher Gleeson IND'='IND';
                             'T.P. Niland IND'='IND';
                             'Spike Nolan FG'='FG';
                             'Mags Sheehan GP'='GP';
                             'Killyon FF'='FF';
                             'FF'='FF'; 'FG'='FG';'DS'='DS';
                             'LAB'='LAB';
                             'SF'='SF'; 'PD'='PD'; 'WP'='WP';
                             'IND'='IND'; 'CnP'='CnP';
                             'CnT'='CnT'; 'GP'='GP';else='OTHER'"))

# adjust Dáil parties accordingly
table(dta_dail_full$party_dail_short)
table(complete_raw$party)

setdiff(dta_dail_full$party_dail_short,
        complete_raw$party)

# recode Democratic Left and SP to "OTHER"
dta_dail_full <- dta_dail_full %>% 
  mutate(party_dail_short = ifelse(party_dail_short %in% c("Democratic Left", "SP"),
                                   "OTHER", party_dail_short))


# get overview of all parties included in the dataset
table(complete_raw$party)

table(dta_dail_full$party_dail_short)

# merge TDs to data frame of local election results
complete <- left_join(complete_raw,
                      dta_dail_full_unique, 
                      by = c("election_year", "candidate_merge"))

table(complete$elected, complete$election_year)

prop.table(table(complete$td_type))

## make sure that number of rows remained the same

stopifnot(nrow(complete_raw) == nrow(complete))

table(complete_raw$party)

# Democratic left in 1999 merged with Labour, 
# therefore recode these observations as Labour

# complete <- complete %>% 
#   mutate(party_dail_short = ifelse(election_year >= 1999 & 
#                                      party_dail_short == "Democratic Left", "LAB", party_dail_short))

# check whether party in D?il and party in local election are the same
complete <- complete %>% 
  mutate(party_same = ifelse(party_dail_short == party, TRUE, FALSE))

table(complete$party_same)

complete_party_different <- complete %>% 
  filter(party_same == FALSE)

cat(paste("For", nrow(complete_party_different), 
          "TDs, the party affiliation is not the same (based on the election results from the local election and the general election."))


# create dummy whether TD was in the vote margin 

# (only count candidates as TDs whose party affiliation is the same in the D?il and the local election)
complete <- complete %>%
  ungroup() %>%
  mutate(td_in_vote_margin = ifelse(!is.na(vote_margin) &
                                      !is.na(dail_number) &
                                      party_same == TRUE,
                                    TRUE, FALSE)) %>%
  mutate(td_status = ifelse(party_same == TRUE & !is.na(dail_number), "TD", "Not a TD"))

table(complete$td_status)

# create a dummy for TD status
complete$is_td <- ifelse(complete$td_status == "TD", 1, 0)


# 12 of the observations are wrong (errors in the raw datasets)
complete$data_error <- 0
complete$data_error[complete$vote_margin < 0.5 & complete$elected == 1] <- 1
complete$data_error[complete$vote_margin >= 0.5 & complete$elected == 0] <- 1

table(complete$data_error)

complete$sum_error <- ave(complete$data_error, 
                          list(complete$list_id), 
                          FUN = function(x) sum(x, na.rm = TRUE))

complete <- subset(complete, sum_error == 0)

# create outcomes for RDD analysis
complete <- complete %>% 
  arrange(candidate_merge, election_year) %>% 
  group_by(candidate_merge) %>% # <-- add here how candidates are merged
  mutate(reelected = lead(elected, order_by = election_year),
         first_round_vote_share_next = lead(first_round_vote_share, order_by = election_year),
         party_next = lead(party, order_by = election_year),
         council_name_next = lead(council_name, order_by = election_year)) %>%
  mutate(reelected = replace(reelected, is.na(reelected), 0),
         reran = ifelse(!is.na(party_next), 1, 0),
         reran = ifelse(reran == 1 & council_name_next == council_name, 1, 0), 
         reelected = ifelse(reelected == 1 & council_name_next == council_name, 1, 0)) %>%
  mutate(reelected_lag = lag(elected, order_by = election_year),
         party_lag = lag(party, order_by = election_year),
         council_name_lag = lag(council_name, order_by = election_year)) %>%
  mutate(reelected_lag = replace(reelected_lag, is.na(reelected_lag), 0),
         reran_lag = ifelse(!is.na(party_lag), 1, 0),
         reran_lag = ifelse(reran_lag == 1 & council_name_lag == council_name_lag, 1, 0), 
         reelected_lag = ifelse(reelected_lag == 1 & council_name_lag == council_name_lag, 1, 0)) %>%
  ungroup()

# check in how many constituencies a candidate ran in a given election
complete <- complete %>% 
  group_by(candidate_merge, election_year) %>% 
  mutate(frequency_run = n()) %>% 
  ungroup()

summary(complete$frequency_run)


table(complete$reelected,
      complete$election_year)

table(complete$reran,
      complete$election_year)

# create dummy whether TD ran in a constituency
complete <- complete %>% 
  group_by(election_year, constit_id) %>% 
  mutate(td_elected = ifelse(td_status == "TD" & elected == 1, TRUE, FALSE) %>% as.numeric) %>% 
  mutate(td_on_list = {if (any(td_status == "TD")) TRUE else FALSE} %>% as.numeric) %>% 
  mutate(td_on_list_elected = {if (any(td_elected == TRUE)) TRUE else FALSE} %>% as.numeric) %>%
  ungroup()

complete <- complete %>%
  group_by(candidate_merge) %>%
  arrange(candidate_merge, election_year) %>%
  mutate(td_elected_t1 = lead(td_elected),
         td_on_list_t1 = lead(td_on_list),
         td_on_list_elected_t1 = lead(td_on_list_elected)) %>%
  ungroup() %>%
  group_by(list_id) %>%
  mutate(td_elected_t1 = as.numeric(sum(td_elected_t1, na.rm = TRUE)>0),
         td_on_list_t1 = as.numeric(sum(td_on_list_t1, na.rm = TRUE)>0),
         td_on_list_elected_t1 = as.numeric(sum(td_on_list_elected_t1, na.rm = TRUE)>0))

table("t" = complete$td_on_list, "t1" = complete$td_on_list_t1)


complete$td_on_list_combo <- case_when(complete$td_on_list == 0 & complete$td_on_list_t1 == 0 ~ "0-0",
                                       complete$td_on_list == 1 & complete$td_on_list_t1 == 0 ~ "1-0",
                                       complete$td_on_list == 0 & complete$td_on_list_t1 == 1 ~ "0-1",
                                       complete$td_on_list == 1 & complete$td_on_list_t1 == 1 ~ "1-1")

table(complete$election_year, complete$td_on_list_elected)


# summarise dataset by election_year and constituency_id 
# and check whether td_in_constituecy is TRUE or FALSE

# afterwards create lead variable and merge back to dataset
# (not possible to do it directly as the numbers of candidates running differs per election)

complete_election_year_constit_id <- complete %>% 
  ungroup() %>% 
  select(election_year, constit_id, td_on_list_elected, td_on_list) %>% 
  unique() %>% 
  arrange(constit_id, election_year) %>% 
  group_by(constit_id) %>% 
  mutate(td_on_list_elected_lag = lag(td_on_list_elected)) %>%
  mutate(td_on_list_lag = lag(td_on_list)) %>%
  mutate(election_year_lag = lag(election_year),
         diff_years = election_year_lag - election_year) %>% 
  ungroup() %>% 
  mutate(diff_years = ifelse(is.na(diff_years), 0, diff_years)) # account for new constituencies

summary(complete_election_year_constit_id$diff_years)

# sometimes, the differences between "elections" is more than 10 years 
# (when a constituency ID was not included for elections in the meantime)
# thus, recode only those cases as td_on_list_elected_lag if diff_years < 8

complete_election_year_constit_id <- complete_election_year_constit_id %>% 
  mutate(td_on_list_elected_lag = ifelse(diff_years > 8 &  td_on_list_elected_lag == TRUE, FALSE, td_on_list_elected_lag)) %>% 
  mutate(td_on_list_lag = ifelse(diff_years > 8 &  td_on_list_lag == TRUE, FALSE, td_on_list_lag))


# Merge these constituency-level data to the complete data frame
complete <- left_join(complete, complete_election_year_constit_id, 
                      by = c("election_year", 
                             "constit_id", 
                             "td_on_list_elected", 
                             "td_on_list"))


# Change rerun and relected to 0 if difference between 
# elections larger than 8 years (the maximum difference between two election years)

table(complete$reran, useNA = "always")
table(complete$reelected, useNA = "always")

complete <- complete %>% 
  mutate(reran = if_else(diff_years > 8 & reran == 1, 0, reran),
         reelected = if_else(diff_years > 8 & reelected == 1, 0, reelected))

# summary statistics about TDs on list
sum_constituency_td <- complete %>% 
  filter(election_year < 2002) %>% 
  ungroup() %>% 
  select(election_year, constit_id, td_on_list) %>% 
  unique() %>% 
  group_by(constit_id) %>% 
  summarise(number_elections_constit = n(),
            td_participated_constit = sum(td_on_list),
            ratio_td_participated_constit = td_participated_constit / number_elections_constit) 

summary(sum_constituency_td$number_elections_constit)

# check how many TDs were participated in a council
sum_council_td <- complete %>% 
  filter(election_year < 2002) %>% 
  select(election_year, constit_id, council_name, td_on_list) %>% 
  unique() %>% 
  group_by(council_name) %>% 
  summarise(number_elections_council = n(),
            td_council = sum(td_on_list),
            ratio_td_council = td_council / number_elections_council) %>% 
  mutate(type = "TD participates")

# merge data frames on constituencies and councils and the presence of TDs to the main dataset
complete <- left_join(complete, sum_constituency_td, by = "constit_id")
complete <- left_join(complete, sum_council_td, by = "council_name")


# create dictionary with names

# load iebabynames file
load("iebabynames.rda")

# count sums of all male and female names
names_male_count <- iebabynames %>% 
  filter(sex == "Male") %>% 
  group_by(name) %>% 
  summarise(n_male = sum(n))

names_female_count <- iebabynames %>% 
  filter(sex == "Female") %>% 
  group_by(name) %>% 
  summarise(n_female = sum(n))

# merge data frames
names_full <- full_join(names_male_count,
                        names_female_count)


# use the gender that appears more often for each name

names_full <- names_full %>% 
  mutate(gender_dummy = ifelse(n_female > 0 & is.na(n_male), 
                               "female",
                               ifelse(n_male > 0 & is.na(n_female), "male",
                                      ifelse(n_female > n_male, "female", "male"))))

names_full_dict <- names_full %>% 
  rename(sentiment = gender_dummy, 
         word = name)

dict_names <- as.dictionary(names_full_dict)

corp_names <- corpus(complete, text_field = "candidate")

dict_applied <- corp_names %>% 
  tokens() %>% 
  tokens_lookup(dictionary = dict_names) %>% 
  dfm() %>% 
  convert(to = "data.frame") %>% 
  select(-doc_id) %>% 
  mutate(gender = ifelse(male + female == 0, "Not classified",
                         ifelse(male > female, "Male", "Female")))


# add gender analysis
complete <- bind_cols(complete, dict_applied)

table(complete$gender)

# change outcomes for most recent election in 2019 to NA (no reelection/reran values possible) 
complete$reran[complete$election_year == "2019"] <- NA
complete$reelected[complete$election_year == "2019"] <- NA


# estimate change in vote share
complete <- complete %>% 
  mutate(diff_vote_share_first_round = first_round_vote_share_next - first_round_vote_share)

# reorder some columns
complete <- complete %>% 
  select(election_year, council_name, const_name,
         candidate, candidate_merge,
         reelected, reran,
         reelected_lag, reran_lag,
         first_round_vote_share,
         first_round_vote_share_next,
         dail_number, election_date_dail,
         is_td, td_in_vote_margin,
         everything()) %>% 
  arrange(election_year, council_name, const_name)


# create new variable that checks whether marginal winner and marginal
# loser in constituency belong to the same party

complete <- complete %>% 
  mutate(party_winner_loser = ifelse(is.na(vote_margin), NA,
                                     party)) %>% 
  select(party_winner_loser, vote_margin, 
         election_year, 
         const_name, 
         council_name,
         everything()) %>% 
  arrange(election_year, council_name, const_name) %>% 
  group_by(election_year, council_name, const_name) %>% 
  mutate(party_same_winner_loser = n_distinct(party_winner_loser, na.rm = TRUE)) %>% 
  mutate(party_same_winner_loser = ifelse(party_same_winner_loser == 1, TRUE, FALSE)) %>% 
  select(party_same_winner_loser, everything())


# create aggregated party variable
complete <- complete %>% 
  mutate(party_aggregated = car::recode(party, 
                                        "'FF'='FF';
                                        'FG'='FG';
                                        'LAB'='LAB';
                                        'IND'='IND';
                                        else='OTHER'"))



# create variable that counts the number of 
# candidates running in constituency
complete <- complete %>% 
  group_by(election_year, council_name, const_name) %>% 
  mutate(n_candidates_const = n())

table(complete$party,
      complete$party_aggregated)

table(complete$party_same_winner_loser,
      complete$party_winner_loser)

complete <- ungroup(complete)


# save all local election data 
saveRDS(complete, file = "data_local_elections_complete.rds")

