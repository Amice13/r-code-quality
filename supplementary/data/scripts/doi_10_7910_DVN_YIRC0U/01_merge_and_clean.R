########
### Authors: James Cross, Derek Greene, Stefan Müller, and Martijn Schoonvelde 
### "Mapping Digital Campaign Strategies: 
### How Political Candidates Use Social Media to Communicate Constituency Connection and Policy Stance.
### Computational Communication Research.
### This script contains the code to clean and merge the datasets. 
### Due to Twitter's T&Cs not all datasets can be shared.
#######

## Load packages
library(tidyverse)          # CRAN v2.0.0
library(zoo)                # CRAN v1.8-14
library(slider)             # CRAN v0.3.2
library(texreg)             # CRAN v1.39.4
library(scales)             # CRAN v1.3.0
library(xtable)             # CRAN v1.8-4
library(xtable)             # CRAN v1.8-4
library(ggeffects)          # CRAN v2.3.0
library(quanteda)           # CRAN V4.3.1
library(quanteda.textstats) # CRAN v0.97.2
library(scales)             # CRAN v1.3.0
library(haven)              # CRAN v2.5.4


## 1. Load tweet data and filter relevant content -----


# load tweet dataset - cannot be shared due to privacy reasons
dat_scraped <- readRDS("data_dontshare/dat_candidates_parties_2020-03-14.rds")

# get followers at the time of scraping
dat_unique_followers <- dat_scraped %>%
    select(screen_name, followers_count, date_scraped) %>%
    unique() %>%
    arrange(screen_name, date_scraped) %>%
    group_by(screen_name) %>%
    mutate(screen_name_merge = str_to_lower(screen_name)) %>%
    filter(row_number() == 1) %>%
    rename(followers_date = date_scraped)

nrow(dat_unique_followers)

# load predictions for policy content
dat_policy <- read_csv("../replication_computational_communication_research_data_dontshare/unseen-policy-sbert_distilrobertav1_lr.csv")

table(dat_policy$screen_name)

# load predictions for electioneering content
dat_electioneering <- read_csv("../replication_computational_communication_research_data_dontshare/unseen-electioneering-sbert_distilrobertav1_lr.csv") %>%
    select(
        tweet_id, prediction_electioneering_lr,
        prob_electioneering_lr1
    )

# load predictions for policy content -- includes retweets
dat_all_with_retweets <- left_join(dat_policy, dat_electioneering,
    by = "tweet_id"
)

# exclude party tweets
party_accounts <- str_to_lower(c(
    "twitter_account",
    "fiannafailparty",
    "FineGael",
    "labour",
    "greenparty_ie",
    "RENUAIreland",
    "sinnfeinireland",
    "SocDems",
    "solidarityie",
    "pb4p",
    "AontuIE"
))
party_accounts

# get screen names of candidates for merging
dat_all_with_retweets <- dat_all_with_retweets %>%
    mutate(screen_name_merge = str_to_lower(screen_name))

# remove retweets from sample
dat_all <- filter(dat_all_with_retweets, is_retweet == FALSE)

# compare sample sizes
nrow(dat_all_with_retweets)
nrow(dat_all)

# remove party accounts
dat_all <- dat_all %>%
    filter(!screen_name_merge %in% party_accounts)

# check sample size again
nrow(dat_all)

# check start date
min(dat_all$created_at, na.rm = TRUE)

# select only relevant variables
dat_select <- dat_all %>%
    select(
        prediction_electioneering_lr,
        prediction_policy_lr,
        text,
        contains("prob_"),
        contains_media,
        n_media_tweet,
        screen_name, screen_name_merge,
        created_at,
        status_id, user_id,
        is_retweet,
        favorite_count,
        retweet_count
    )

nrow(dat_select)

## 2. Merge candidate-level data -----

# get metadata on candidates

dat_candidates <- read_csv("data_candidates_ge2020.csv")

# create variable for merging by Twitter handle
dat_candidates_check <- dat_candidates %>%
    mutate(screen_name_merge = str_to_lower(twitter_id))

# dataframe with candidates who have a Twitter account
dat_available <- dat_select %>%
    group_by(screen_name_merge) %>%
    count() |>
    ungroup() |>
    mutate(tweeted = 1)

nrow(dat_available)

# merge candidate data with Twitter data
dat_candidates_check <- left_join(dat_candidates_check, dat_available, by = "screen_name_merge")

# binary variable taking value 1 if candidate tweeted
dat_candidates_check <- dat_candidates_check %>%
    mutate(tweeted = ifelse(is.na(tweeted), 0, 1))


table(dat_candidates_check$tweeted)
# recode party names

dat_candidates_check <- dat_candidates_check %>%
    mutate(party_recoded = dplyr::recode(
        party,
        "Aon" = "Independent/Other",
        "FF" = "Fianna Fáil",
        "FG" = "Fine Gael",
        "GP" = "Green Party",
        "I4C" = "Independent/Other",
        "IFP" = "Independent/Other",
        "IND" = "Independent/Other",
        "LAB" = "Labour",
        "REN" = "Independent/Other",
        "SD" = "Social Democrats",
        "SF" = "Sinn Féin",
        "SOL-PBP" = "Solidarity-PBP",
        "WP" = "Independent/Other", .default = "Independent/Other"
    ))

table(dat_candidates_check$party_recoded)

table(dat_candidates_check$party_recoded,
dat_candidates_check$tweeted)

# get proportion and number of candidates with Twitter account
dat_prop_tweeted <- dat_candidates_check %>%
    group_by(party_recoded) %>%
    #filter(!is.na(n)) |>
    summarise(
        n_accounts = n(),
        tweet_accounts = sum(tweeted),
        prop_accounts = mean(tweeted)
    )




dat_candidates_check %>%
    summarise(
        n_accounts = n(),
        tweet_accounts = sum(tweeted),
        prop_accounts = mean(tweeted)
    )





# save data frame with proportions for plotting
write_csv(dat_prop_tweeted,
    "data_fig_02.csv"
)


# calcualte competitiveness meausure based on paddy power odds
dat_candidates <- dat_candidates %>%
    mutate(candidate = str_squish(candidate)) %>%
    mutate(paddy_power_odds = ifelse(paddy_power_odds == "EVS", "1/1",
        paddy_power_odds
    )) %>%
    separate(paddy_power_odds,
        into = c(
            "paddy_power_numerator",
            "paddy_power_denominator"
        ),
        sep = "/", remove = FALSE
    ) %>%
    mutate(
        paddy_power_numerator = as.numeric(paddy_power_numerator),
        paddy_power_denominator = as.numeric(paddy_power_denominator)
    ) %>%
    mutate(paddy_power_percentage = (1 / ((paddy_power_numerator / paddy_power_denominator) + 1)) * 100) %>%
    mutate(paddy_power_probs = paddy_power_percentage / 100)


dat_candidates <- dat_candidates %>%
    group_by(constituency_name) %>%
    mutate(candidates_running_const = n()) %>%
    mutate(paddy_power_prob_max = max(paddy_power_probs, na.rm = TRUE)) %>%
    mutate(competitiveness = 1 + paddy_power_probs - paddy_power_prob_max) %>%
    mutate(screen_name_merge = str_to_lower(twitter_id)) %>%
    filter(!is.na(screen_name_merge)) %>%
    group_by(screen_name_merge) %>%
    mutate(candidate_run_districts = n()) %>%
    filter(candidate != "Thomas Walsh") %>%
    mutate(const_candidate = paste(constituency_name, candidate, sep = "_")) %>%
    filter(const_candidate != "Dublin West_Peter Casey") %>%
    rename(party_raw = party)


# recode party names
dat_candidates <- dat_candidates %>%
    mutate(party = dplyr::recode(
        party_raw,
        "Aon" = "Aontú",
        "FF" = "Fianna Fáil",
        "FG" = "Fine Gael",
        "GP" = "Green Party",
        "I4C" = "Other/Ind",
        "IFP" = "Other/Ind",
        "IND" = "Other/Ind",
        "LAB" = "Labour",
        "REN" = "Other/Ind",
        "SD" = "Social Democrats",
        "SF" = "Sinn Féin",
        "SOL-PBP" = "Solidarity-PBP",
        "WP" = "Other/Ind", .default = "Other/Ind"
    ))



## 3. Predict success conditional on competitiveness -----

# predict if candidate got elected contitional on competitiveness
dat_candidates <- dat_candidates |>
    mutate(
        elected_num = ifelse(elected == TRUE, 1, 0),
        paddy_power_percentage = as.numeric(paddy_power_percentage)
    )

# save for analysis
dat_reg_competitive <- dat_candidates |>
    ungroup() |>
    select(elected_num, competitiveness, party)

write.csv(dat_reg_competitive,
    "data_reg_competitive.csv",
    fileEncoding = "utf-8",
    row.names = FALSE
)

# merge candidates with unique follower numbers
dat_candidates <- left_join(dat_candidates, dat_unique_followers,
    by = "screen_name_merge"
)


## 4. Merge candidate-level data with election-level data -----

# load election-level data
dat_elections_all_raw_full <- readRDS("data_ireland_general_elections.rds") %>%
    mutate(election_year_clean = substr(election_year, 1, 4)) %>%
    filter(election_year_clean >= 1960)


# recode party names again
dat_elections_all_raw <- dat_elections_all_raw_full %>%
    mutate(candidate = str_squish(candidate)) %>%
    mutate(party = dplyr::recode(
        party,
        "Aon" = "Aontú",
        "FF" = "Fianna Fáil",
        "FG" = "Fine Gael",
        "GP" = "Green Party",
        "I4C" = "Other/Ind",
        "IFP" = "Other/Ind",
        "IND" = "Other/Ind",
        "LAB" = "Labour",
        "REN" = "Other/Ind",
        "SD" = "Social Democrats",
        "SF" = "Sinn Féin",
        "SOL-PBP" = "Solidarity-PBP",
        "PBP" = "Solidarity-PBP",
        "AAA-PBP" = "Solidarity-PBP",
        "WP" = "Other/Ind", .default = "Other/Ind"
    )) %>% # remove "wrong Brendan Griffin"
    mutate(remove = ifelse(candidate == "Michael D'Arcy" & election_year_clean <= 2006, 1, 0)) %>%
    filter(remove == 0) %>%
    mutate(remove = ifelse(candidate == "Brendan Griffin" & str_detect(constituency_name, "Tipperary"), 1, 0)) %>%
    filter(remove == 0) %>%
    # different Michael White (from the 1980s)
    mutate(remove = ifelse(candidate == "Michael White" & str_detect(election_year, "198"), 1, 0)) %>%
    filter(remove == 0) %>%
    # Remove FG Peter Fitzpatrick and keep IND Peter Fitzpatrick
    mutate(remove = ifelse(candidate == "Peter Fitzpatrick" & str_detect(party, "Fine Gael"), 1, 0)) %>%
    filter(remove == 0) %>%
    # remove LAB John Lyons but keep S-PBP John Lyons
    mutate(remove = ifelse(candidate == "John Lyons" & str_detect(party, "Labour"), 1, 0)) %>%
    filter(remove == 0)



# count the number of times a candidate ran
dat_exp_all <- dat_elections_all_raw %>%
    arrange(candidate, party, election_year_clean) %>%
    select(candidate, party, election_year, constituency_name, election_year_clean, elected) %>%
    unique() %>%
    group_by(candidate) %>%
    mutate(running = 1) %>%
    mutate(sum_running = cumsum(running)) %>%
    mutate(sum_elected = cumsum(elected)) %>%
    mutate(experience_running_dummy = ifelse(sum_elected > 0, 1, 0)) %>%
    mutate(experience_elected_dummy = ifelse(sum_elected > 0, 1, 0)) %>%
    select(-c(elected, election_year_clean))


# merge back now (this avoided that candidates who ran
# in several constituencies are counted several times)
dat_elections_all <- left_join(dat_elections_all_raw, dat_exp_all,
    by = c(
        "candidate", "party", "election_year",
        "constituency_name"
    )
)

nrow(dat_elections_all_raw)
nrow(dat_elections_all)

# create variable on incumbency status
dat_elections_select <- dat_elections_all %>%
    group_by(candidate) %>%
    mutate(election_year_num = substr(election_year, 1, 4)) %>%
    filter(election_year_num == max(election_year_num)) %>%
    mutate(incumbent = ifelse(election_year_num == "2016" & elected == TRUE, 1, 0)) %>%
    select(candidate, incumbent, election_year_num, sum_running:experience_elected_dummy) %>%
    rename(year_prev = election_year_num) %>%
    mutate(candidate = str_squish(candidate)) %>%
    ungroup()


dat_candidates$candidate <- str_squish(dat_candidates$candidate)

setdiff(dat_candidates$candidate, dat_elections_select$candidate)

# remove old incumbency variable
dat_candidates <- select(dat_candidates, -incumbent)

# merge data on incumbency status
dat_candidates_all <- left_join(
    dat_candidates,
    dat_elections_select
) %>%
    mutate(incumbent = ifelse(is.na(incumbent), 0, incumbent))

nrow(dat_candidates_all)

# code winners of 2019 bye-elctions as incumbents
# https://en.wikipedia.org/wiki/List_of_Dáil_by-elections

table(dat_candidates_all$incumbent)

# for those candidates who did never compete before,
# set sum_running, sum_elected, experience_running_dummy, and experience_elected_dummy
# to 0

dat_candidates_all <- dat_candidates_all %>%
    mutate(sum_running = ifelse(is.na(sum_running), 0, sum_running)) %>%
    mutate(sum_elected = ifelse(is.na(sum_elected), 0, sum_elected)) %>%
    mutate(experience_running_dummy = ifelse(is.na(experience_running_dummy), 0, experience_running_dummy)) %>%
    mutate(experience_elected_dummy = ifelse(is.na(experience_elected_dummy), 0, experience_elected_dummy))


# winners of bye-elections
winners_byelections <- c(
    "Joe O'Brien",
    "Mark Ward",
    "Malcolm Byrne",
    "Pádraig O'Sullivan"
)

# code bye-election winners as incumbents
dat_candidates_all <- dat_candidates_all %>%
    mutate(incumbent = ifelse(candidate %in% winners_byelections, 1, incumbent))

# cross-table of incumbency status
table(dat_candidates_all$incumbent)

# merge datasets
dat_merged <- left_join(dat_select, dat_candidates_all)

dat_merged$status_id <- factor(dat_merged$status_id)

table(dat_merged$screen_name)

# create numeric variables for policy and electioneering
dat_merged <- dat_merged %>%
    mutate(electioneering_num = ifelse(prediction_electioneering_lr == "electioneering", 1, 0)) %>%
    mutate(policy_num = ifelse(prediction_policy_lr == "policy", 1, 0))

# arrange dataset by screen name and date+time of tweet
dat_merged <- arrange(dat_merged, screen_name, created_at)

# create variable indicating if tweet addresses electioneering, policy, both or neither
dat_merged <- dat_merged %>%
    mutate(dv_categories = ifelse(electioneering_num == 1 & policy_num == 0,
        "Electioneering",
        ifelse(electioneering_num == 0 & policy_num == 1,
            "Policy",
            ifelse(electioneering_num == 1 & policy_num == 1, "Both",
                ifelse(electioneering_num == 0 & policy_num == 0, "Neither", NA)
            )
        )
    ))

length(unique(dat_merged$gender))

# remove 21 candidates who are not listed in master spreadsheet (did not run)
dat_merged |>
    filter(is.na(gender)) |>
    dplyr::select(screen_name_merge, gender) |>
    distinct()

nrow(dat_merged)

dat_merged <- filter(dat_merged, !is.na(gender))

# number of observations in relevant sample
nrow(dat_merged)

# 5. Conduct keyness analysis across categories -----

toks_keyness <- dat_merged |>
    corpus(text_field = "text") |>
    tokens(
        remove_punct = TRUE, remove_symbols = TRUE,
        remove_numbers = TRUE, what = "word3" # specify tokenizer: v3
    ) |>
    tokens_compound(pattern = phrase(c(
        "fianna fáil*",
        "fine gael*",
        "sinn féin*"
    ))) |>
    tokens_remove(pattern = c(stopwords("en"), "amp")) |>
    tokens_remove(pattern = c("#*", "042-9751802")) |> # remove hashtags and meaningless features
    tokens_remove(pattern = c("http*", "www*")) # remove URLs


# get all categories and loop through them
# to run keyness analysis (always compare category vs all other categories)

categories_dv <- unique(dat_merged$dv_categories)

# empty data frame for keyness analysis
dat_keyness_words <- data.frame()

for (i in categories_dv) {
    cat("Keyness analysis for", i, "\n")

    # create dfm and group by measure i
    dfmat_grouped <- toks_keyness |>
        dfm() |>
        dfm_group(groups = toks_keyness$dv_categories)

    # run keyness analysis
    keyness <- textstat_keyness(dfmat_grouped, target = i)

    # create variable indicating the category
    keyness$category <- i

    # bind data frame
    dat_keyness_words <- bind_rows(keyness, dat_keyness_words)
}


# get the top-30 terms per measure
dat_keyness_words_max_30 <- dat_keyness_words |>
    group_by(category) |>
    mutate(rank = 1:n()) |>
    filter(rank <= 30)


# bind terms for each measure (unit of observation is not the measure, rather than the term)
dat_keyness_words_max_30_tab <- dat_keyness_words_max_30 |>
    rename(Category = category) |>
    group_by(Category) |>
    summarise(Words = paste(feature, collapse = ", ")) |>
    ungroup() |>
    mutate(Category = factor(
        Category,
        levels = c(
            "Electioneering",
            "Policy",
            "Both",
            "Neither"
        )
    )) |>
    arrange(Category)


## repeat keyness analysis for hashtags

toks_keyness_hashtags <- dat_merged |>
    corpus(text_field = "text") |>
    tokens(
        what = "word3",
        remove_punct = TRUE, remove_symbols = TRUE,
        remove_numbers = TRUE
    ) |>
    tokens_keep(pattern = "#*")


# empty data frame for keyness analysis
dat_keyness_hashtags <- data.frame()

for (i in categories_dv) {
    cat("Keyness analysis for", i, "\n")

    # create dfm and group by measure i
    dfmat_grouped_hashtags <- toks_keyness_hashtags |>
        dfm() |>
        dfm_group(groups = toks_keyness_hashtags$dv_categories)

    # run keyness analysis
    keyness_hashtags <- textstat_keyness(dfmat_grouped_hashtags, target = i)

    # create variable indicating the category
    keyness_hashtags$category <- i

    # bind data frame
    dat_keyness_hashtags <- bind_rows(keyness_hashtags, dat_keyness_hashtags)
}


# get the top-30 hashtags per measure
dat_keyness_hashtags_max_30 <- dat_keyness_hashtags |>
    group_by(category) |>
    mutate(rank = 1:n()) |>
    filter(rank <= 30)


# bind terms for each measure
dat_keyness_hashtags_max_30_tab <- dat_keyness_hashtags_max_30 |>
    rename(Category = category) |>
    group_by(Category) |>
    summarise(Hashtags = paste(feature, collapse = ", ")) |>
    ungroup() |>
    mutate(Category = factor(
        Category,
        levels = c(
            "Electioneering",
            "Policy",
            "Both",
            "Neither"
        )
    )) |>
    arrange(Category)


# combine words and hashtags
dat_keyness_combined <- left_join(dat_keyness_words_max_30_tab,
    dat_keyness_hashtags_max_30_tab,
    by = c("Category")
)

# print data frame
print(dat_keyness_combined)

# store terms as spreadheet
write.csv(dat_keyness_combined,
    "data_keyness_terms.csv",
    fileEncoding = "utf-8",
    row.names = FALSE
)


# set number of retweets to NA if tweet not about electioneering/policy
# and store in a new variable since we want to assess retweets on electioneering/policy tweets

dat_merged <- dat_merged %>%
    mutate(retweet_count_policy = ifelse(policy_num == 1, retweet_count, NA)) %>%
    mutate(retweet_count_electioneering = ifelse(electioneering_num == 1, retweet_count, NA))

# set sum of electioneering tweets, policy tweets, likes and retweets
dat_merged <- dat_merged %>%
    group_by(screen_name) %>%
    mutate(sum_electioneering_1_tweet = lag(slide_dbl(electioneering_num, ~ sum(.x, na.rm = TRUE),
        .before = 0
    ))) %>%
    mutate(sum_electioneering_5_tweet = lag(slide_dbl(electioneering_num, ~ sum(.x, na.rm = TRUE),
        .before = 4
    ))) %>%
    mutate(sum_electioneering_10_tweet = lag(slide_dbl(electioneering_num, ~ sum(.x, na.rm = TRUE),
        .before = 9
    ))) %>%
    mutate(sum_electioneering_20_tweet = lag(slide_dbl(electioneering_num, ~ sum(.x, na.rm = TRUE),
        .before = 19
    ))) %>%
    mutate(sum_policy_1_tweet = lag(slide_dbl(policy_num, ~ sum(.x, na.rm = TRUE),
        .before = 0
    ))) %>%
    mutate(sum_policy_5_tweet = lag(slide_dbl(policy_num, ~ sum(.x, na.rm = TRUE),
        .before = 4
    ))) %>%
    mutate(sum_policy_10_tweet = lag(slide_dbl(policy_num, ~ sum(.x, na.rm = TRUE),
        .before = 9
    ))) %>%
    mutate(sum_policy_20_tweet = lag(slide_dbl(policy_num, ~ sum(.x, na.rm = TRUE),
        .before = 19
    ))) %>%
    mutate(sum_likes_1_tweet = lag(slide_dbl(favorite_count, ~ sum(.x, na.rm = TRUE),
        .before = 0
    ))) %>%
    mutate(sum_likes_5_tweet = lag(slide_dbl(favorite_count, ~ sum(.x, na.rm = TRUE),
        .before = 4
    ))) %>%
    mutate(sum_likes_10_tweet = lag(slide_dbl(favorite_count, ~ sum(.x, na.rm = TRUE),
        .before = 9
    ))) %>%
    mutate(sum_retweets_1_tweet = lag(slide_dbl(retweet_count, ~ sum(.x, na.rm = TRUE),
        .before = 0
    ))) %>%
    mutate(sum_retweets_5_tweet = lag(slide_dbl(retweet_count, ~ sum(.x, na.rm = TRUE),
        .before = 4
    ))) %>%
    mutate(sum_retweets_10_tweet = lag(slide_dbl(retweet_count, ~ sum(.x, na.rm = TRUE),
        .before = 9
    )))


# get number of policy/electioneering tweets in the last XY tweets

# get sums and means of POLICY content

dat_merged_check <- select(
    dat_merged,
    screen_name, prediction_policy_lr,
    policy_num,
    starts_with("sum")
)


# get rolling averages of X retweets for Y latest policy/electioneering tweets

# This step is tricky. The MWE example below shows how to achieve it

# create data frame: tweet ID, and number of retweets for policy
# note: in original dataset, some values are NA (if tweet is not a policy tweet)
dat_ex <- data.frame(
    id = 1:10,
    n_retweets_policy = c(1, 2, 3, 4, 5, NA, 7, 8, 9, 10)
)

# only filter non-na values for policy
# to ensure that the last X POLICY tweets are used
dat_no_na_ex <- dat_ex |>
    filter(!is.na(n_retweets_policy)) |>
    mutate(rolmean_3 = lag(rollmeanr(n_retweets_policy,
        k = 3, fill = NA
    ))) |>
    select(-n_retweets_policy)

# merge back with full dataset
dat_all_ex <- left_join(dat_ex, dat_no_na_ex, by = c("id"))

# now fill empty observations with the PREVIOUS valid rolmean for policy tweets
dat_all_ex <- dat_all_ex |>
    mutate(rolmean_3_filled = na.locf(rolmean_3,
        na.rm = FALSE,
        fromLast = TRUE
    ))

dat_all_ex


# filter policy content only and get rolling average
# of retweets for last X policy tweets
dat_merged_policy <- dat_merged |>
    filter(prediction_policy_lr == "policy") |>
    group_by(screen_name) |>
    mutate(mean_retweets_policy_1 = lag(rollmeanr(retweet_count_policy,
        k = 1, fill = NA
    ))) |>
    mutate(mean_retweets_policy_5 = lag(rollmeanr(retweet_count_policy,
        k = 5, fill = NA
    ))) |>
    mutate(mean_retweets_policy_10 = lag(rollmeanr(retweet_count_policy,
        k = 10, fill = NA
    ))) |>
    mutate(mean_retweets_policy_20 = lag(rollmeanr(retweet_count_policy,
        k = 20, fill = NA
    ))) |>
    select(screen_name, status_id, starts_with("mean_retweets_policy")) |>
    ungroup()



# filter electioneering content only and get rolling average
# of retweets for last X electioneering tweets
dat_merged_electioneering <- dat_merged |>
    filter(prediction_electioneering_lr == "electioneering") |>
    group_by(screen_name) |>
    mutate(mean_retweets_electioneering_1 = lag(rollmeanr(retweet_count_electioneering,
        k = 1, fill = NA
    ))) |>
    mutate(mean_retweets_electioneering_5 = lag(rollmeanr(retweet_count_electioneering,
        k = 5, fill = NA
    ))) |>
    mutate(mean_retweets_electioneering_10 = lag(rollmeanr(retweet_count_electioneering,
        k = 10, fill = NA
    ))) |>
    mutate(mean_retweets_electioneering_20 = lag(rollmeanr(retweet_count_electioneering,
        k = 20, fill = NA
    ))) |>
    select(screen_name, status_id, starts_with("mean_retweets_elec")) |>
    ungroup()


# merge policy tweets to merged data
dat_merged_plus_policy <- left_join(dat_merged, dat_merged_policy,
    by = c("screen_name", "status_id")
)

dat_merged_plus_policy_and_elec <- left_join(dat_merged_plus_policy, dat_merged_electioneering,
    by = c("screen_name", "status_id")
)


dat_merged_plus_policy_and_elec <- dat_merged_plus_policy_and_elec |>
    select(starts_with("mean_retweet"), everything())

# fill NA columsn for mean_retweets... columns
summary(dat_merged_plus_policy$mean_retweets_policy_1)
summary(dat_merged_plus_policy_and_elec$mean_retweets_electioneering_1)


dat_merged_with_rolavgs <- dat_merged_plus_policy_and_elec |>
    mutate(mean_retweets_policy_1 = na.locf(mean_retweets_policy_1,
        na.rm = FALSE,
        fromLast = TRUE
    )) |>
    mutate(mean_retweets_policy_5 = na.locf(mean_retweets_policy_5,
        na.rm = FALSE,
        fromLast = TRUE
    )) |>
    mutate(mean_retweets_policy_10 = na.locf(mean_retweets_policy_10,
        na.rm = FALSE,
        fromLast = TRUE
    )) |>
    mutate(mean_retweets_policy_20 = na.locf(mean_retweets_policy_20,
        na.rm = FALSE,
        fromLast = TRUE
    )) |>
    mutate(mean_retweets_electioneering_1 = na.locf(mean_retweets_electioneering_1,
        na.rm = FALSE,
        fromLast = TRUE
    )) |>
    mutate(mean_retweets_electioneering_5 = na.locf(mean_retweets_electioneering_5,
        na.rm = FALSE,
        fromLast = TRUE
    )) |>
    mutate(mean_retweets_electioneering_10 = na.locf(mean_retweets_electioneering_10,
        na.rm = FALSE,
        fromLast = TRUE
    )) |>
    mutate(mean_retweets_electioneering_20 = na.locf(mean_retweets_electioneering_20,
        na.rm = FALSE,
        fromLast = TRUE
    ))


summary(dat_merged_with_rolavgs$mean_retweets_policy_1)
summary(dat_merged_with_rolavgs$mean_retweets_electioneering_1)

# create daily dataset
ts <- seq.Date(as.Date("2019-06-01"),
    as.Date("2020-02-08"),
    by = "day"
)

df_days <- data.frame(date = ts)

# empty data frame to expand it for all candidates
df_all <- data.frame()

# list of candidates (can be retrieved using screen_name column)
accounts <- unique(dat_merged_with_rolavgs$screen_name_merge)

# run in loop
for (i in accounts) {
    df_cand <- df_days

    df_cand$screen_name_merge <- i

    # bind data frames

    df_all <- bind_rows(df_all, df_cand)
}

nrow(df_all)

nrow(dat_merged_with_rolavgs)


# transform to date format
dat_merged_with_rolavgs$date <- lubridate::date(dat_merged_with_rolavgs$created_at)

df_all$date <- as.Date(df_all$date)

# join
data_full <- full_join(df_all, dat_merged_with_rolavgs, by = c("date", "screen_name_merge"))

summary(data_full$mean_retweets_electioneering_1)

nrow(data_full)
nrow(dat_merged_with_rolavgs)

# fill candidate names for "empty" dates
data_full <- data_full %>%
    group_by(candidate) %>%
    fill(candidate, .direction = "up") |>
    ungroup()


data_sum_daily <- data_full %>%
    filter(is_retweet == FALSE) %>% # exclude retweets
    dplyr::group_by(
        screen_name_merge, party, district_magnitude_man,
        constituency_name,
        candidate_run_districts,
        gender,
        twitter_account_opened, date
    ) %>%
    summarise(
        n_tweets_day = n(),
        n_likes_day = sum(favorite_count),
        n_electioneering_day = sum(electioneering_num, na.rm = TRUE),
        n_electioneering_retweets_day = sum(retweet_count_electioneering, na.rm = TRUE),
        n_policy_retweets_day = sum(retweet_count_policy, na.rm = TRUE),
        n_policy_day = sum(policy_num, na.rm = TRUE),
        n_retweets_day = sum(retweet_count)
    ) %>%
    mutate(
        n_likes_per_tweet_day = n_likes_day / n_tweets_day,
        n_retweets_per_tweet_day = n_retweets_day / n_tweets_day
    ) %>%
    group_by(screen_name_merge) %>%
    mutate(rolavg_ntweets_1 = lag(slide_dbl(n_tweets_day, ~ mean(.x, na.rm = TRUE),
        .before = 0
    ))) %>%
    mutate(rolavg_ntweets_3 = lag(slide_dbl(n_tweets_day, ~ mean(.x, na.rm = TRUE),
        .before = 2
    ))) %>%
    mutate(rolavg_ntweets_7 = lag(slide_dbl(n_tweets_day, ~ mean(.x, na.rm = TRUE),
        .before = 6
    ))) %>%
    mutate(rolavg_retweets_1 = lag(slide_dbl(n_retweets_per_tweet_day, ~ mean(.x, na.rm = TRUE),
        .before = 0
    ))) %>%
    mutate(rolavg_retweets_3 = lag(slide_dbl(n_retweets_per_tweet_day, ~ mean(.x, na.rm = TRUE),
        .before = 2
    ))) %>%
    mutate(rolavg_retweets_7 = lag(slide_dbl(n_retweets_per_tweet_day, ~ mean(.x, na.rm = TRUE),
        .before = 6
    ))) %>%
    mutate(rolavg_likes_1 = lag(slide_dbl(n_likes_per_tweet_day, ~ mean(.x, na.rm = TRUE),
        .before = 0
    ))) %>%
    mutate(rolavg_likes_3 = lag(slide_dbl(n_likes_per_tweet_day, ~ mean(.x, na.rm = TRUE),
        .before = 2
    ))) %>%
    mutate(rolavg_likes_7 = lag(slide_dbl(n_likes_per_tweet_day, ~ mean(.x, na.rm = TRUE),
        .before = 6
    ))) %>%
    mutate(sum_electioneering_1_days = lag(slide_dbl(n_electioneering_day, ~ sum(.x, na.rm = TRUE),
        .before = 0
    ))) %>%
    mutate(sum_electioneering_3_days = lag(slide_dbl(n_electioneering_day, ~ sum(.x, na.rm = TRUE),
        .before = 2
    ))) %>%
    mutate(sum_electioneering_7_days = lag(slide_dbl(n_electioneering_day, ~ sum(.x, na.rm = TRUE),
        .before = 6
    ))) %>%
    mutate(sum_policy_1_days = lag(slide_dbl(n_policy_day, ~ sum(.x, na.rm = TRUE),
        .before = 0
    ))) %>%
    mutate(sum_policy_3_days = lag(slide_dbl(n_policy_day, ~ sum(.x, na.rm = TRUE),
        .before = 2
    ))) %>%
    mutate(sum_policy_7_days = lag(slide_dbl(n_policy_day, ~ sum(.x, na.rm = TRUE),
        .before = 6
    ))) %>%
    # mutate(mean_retweets_policy_10 = lag(slide_dbl(retweet_count_policy, ~mean(.x, na.rm = TRUE),
    #                                                .before = 9))) %>%
    mutate(mean_retweets_policy_1_days = lag(slide_dbl(n_policy_retweets_day, ~ mean(.x, na.rm = TRUE),
        .before = 0
    ))) %>%
    mutate(mean_retweets_policy_3_days = lag(slide_dbl(n_policy_retweets_day, ~ mean(.x, na.rm = TRUE),
        .before = 2
    ))) %>%
    mutate(mean_retweets_policy_7_days = lag(slide_dbl(n_policy_retweets_day, ~ mean(.x, na.rm = TRUE),
        .before = 6
    ))) %>%
    mutate(mean_retweets_elec_1_days = lag(slide_dbl(n_electioneering_retweets_day, ~ mean(.x, na.rm = TRUE),
        .before = 0
    ))) %>%
    mutate(mean_retweets_elec_3_days = lag(slide_dbl(n_electioneering_retweets_day, ~ mean(.x, na.rm = TRUE),
        .before = 2
    ))) %>%
    mutate(mean_retweets_elec_7_days = lag(slide_dbl(n_electioneering_retweets_day, ~ mean(.x, na.rm = TRUE),
        .before = 6
    ))) %>%
    mutate(twitter_length = as.numeric(as.Date("2020-02-08") - lubridate::date(twitter_account_opened))) %>%
    mutate(time_until_election = as.numeric(as.Date("2020-02-08") - lubridate::date(date))) %>%
    mutate(campaign = ifelse(date >= as.Date("2020-01-14"), "Campaign Period", "Non-Campaign Period")) %>%
    ungroup() %>%
    rename(district_magnitude = district_magnitude_man)


table(data_sum_daily$constituency_name)

# identify rural and urban constituencies
data_sum_daily <- data_sum_daily %>%
    mutate(urban_rural = dplyr::case_when(
        str_detect(constituency_name, "Dublin") ~ "Urban",
        str_detect(constituency_name, "Dún La") ~ "Urban",
        constituency_name == "Cork North–Central" ~ "Urban",
        constituency_name == "Cork South-Central" ~ "Urban",
        TRUE ~ "Rural"
    ))

# merge variables back to full dataset
dat_merged_save <- left_join(data_full, data_sum_daily, by = join_by(
    date, screen_name_merge,
    constituency_name, twitter_account_opened, gender,
    candidate_run_districts, party
))


summary(dat_merged_save$mean_retweets_policy_1)

# remove observations without valid status_id
# (these are placeholder for days without tweets used to construct variables above)
dat_merged_save <- filter(dat_merged_save, !is.na(status_id))

summary(dat_merged_save$mean_retweets_elec_1_days)

nrow(dat_merged_save)

# remove unncessary variables
names(dat_merged_save)

dat_merged_save_select <- dat_merged_save %>%
    dplyr::select(date, screen_name_merge,
        created_at,
        starts_with("prediction"),
        starts_with("prob_"),
        starts_with("retweet_count"),
        starts_with("n_"),
        starts_with("rolavg_"),
        starts_with("mean_"),
        dv_categories,
        user_id,
        contains_media,
        n_media_tweet,
        ends_with("count"),
        constituency_name,
        candidates_running_const,
        candidate,
        party,
        gender,
        first_pref_share,
        district_magnitude_man,
        elected,
        competitiveness,
        twitter_length,
        time_until_election,
        followers_count,
        followers_date,
        campaign,
        urban_rural,
        incumbent,
        paddy_power_prob = paddy_power_probs,
        paddy_power_prob_max,
        ends_with("elected"),
        ends_with("running"),
        starts_with("sum_"),
        experience_running_dummy,
        experience_elected_dummy
    )


# now add data on YOB and seniority

dat_cand_legislator <- readRDS("data_legislators.rds") %>%
    rename(candidate_merge = candidate) %>%
    mutate(candidate_merge = str_replace_all(candidate_merge, "o'", "o "))

table(dat_cand_legislator$candidate_merge)


# keep most recent entry of each TD (to cover as many candidates as possible)
dat_legisator_subset <- dat_cand_legislator %>%
    arrange(candidate_merge) %>%
    group_by(candidate_merge) %>%
    slice(1) %>%
    ungroup() %>%
    filter(year_clean > 1960) # remove candiates who were last elected before 1960 - merging errors

table(dat_merged_save_select$candidate)

dat_merged_save_select_candidates <- dat_merged_save_select %>%
    mutate(candidate_merge = candidate) %>%
    mutate(
        candidate_merge = str_squish(candidate_merge),
        candidate_merge = str_to_lower(candidate_merge),
        candidate_merge = str_replace_all(candidate_merge, "o'", "o "),
        candidate_merge = str_replace_all(candidate_merge, "ó", "o"),
        candidate_merge = str_replace_all(candidate_merge, "é", "e"),
        candidate_merge = str_replace_all(candidate_merge, "è", "e"),
        candidate_merge = str_replace_all(candidate_merge, "ë", "e"),
        candidate_merge = str_replace_all(candidate_merge, "á", "a"),
        candidate_merge = str_replace_all(candidate_merge, "ú", "u"),
        candidate_merge = str_replace_all(candidate_merge, "p\\.", "p"),
        candidate_merge = str_replace_all(candidate_merge, "j\\.", "j"),
        candidate_merge = str_replace_all(candidate_merge, "í", "i")
    )

diff <- setdiff(
    dat_merged_save_select_candidates$candidate_merge,
    dat_legisator_subset$candidate_merge
)

dat_missing <- filter(
    dat_merged_save_select_candidates,
    candidate_merge %in% as.character(diff)
)

dat_legisator_subset$included_legislator <- TRUE

dat_legisator_subset <- dat_legisator_subset %>%
    select(-c(party:constituency_link))

dat_legisator_subset <- dat_legisator_subset %>%
    mutate(candidate_merge = dplyr::recode(
        candidate_merge,
        "charles flanagan" = "charlie flanagan",
        "francis noel duffy" = "francis duffy",
        "ged nash" = "gerald nash",
        "holly cairns" = "holly mckeever cairns"
    ))


dat_merged_final <- left_join(dat_merged_save_select_candidates,
    dat_legisator_subset,
    by = "candidate_merge"
)

nrow(dat_merged_final)

# save dataset for subsequent analyses
haven::write_dta(dat_merged_final, "data_analysis.dta")
