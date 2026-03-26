#' ---
#' title: "Legislating Landlords: Private Interests, Issue Emphasis, and Policy Positions (LSQ)"
#' subtitle: "01b_merge_metadata_texts.R"
#' author: "Authors: Stefan Mueller and Jihed Ncib"
#' date: "Note: Code compiled successfully on `r format(Sys.time(), '%d %B %Y')`"
#' ---

# script to merge and harmonise the text corpora and datasets on legislators
# and their interests

# If the code does not run, one or more packages may have been 
# updated, which may result in errors or conflicts. You can solve this issue
# by installing the package version listed above or by using the 
# groundhog package:
# after installing groundhog using install.packages("groundhog")
# change library(name_of_package) to
# groundhog::groundhog.library(name_of_package, date = "2024-05-06")
# Instead of adjusting the library() function for each package, 
# you can adjust them at all once using the
# the following syntax:
# groundhog.library("library('pkgA')
#                   library('pkgB')
#                   library('pkgC')", date = "2024-05-06")
# More details are available at: https://groundhogr.com/using/


# script to merge and harmonise the text corpora and datasets on legislators
# and their interests

# load packages

library(tidyverse)           # CRAN v2.0.0 
library(quanteda)            # CRAN v3.3.1
library(quanteda.textmodels) # CRAN v0.9.6
library(quanteda.textstats)  # CRAN v0.96.3
library(quanteda.textplots)  # CRAN v0.94.3
library(arrow)               # CRAN v14.0.0.2
library(stringr)             # CRAN v1.5.1


source("function_theme_base.R")

# load file with merged metadata

dat_meta_all <- readRDS("dat_meta_all.rds")

# load all tweets

dat_tweets_raw <- readRDS("data_dontshare/alltweets.rds")

# remove some unnecessary
table(dat_tweets_raw$year)

dat_tweets_raw <- select(dat_tweets_raw, -c(party,
                                            gender,
                                            constituency_name))


# remove replies

dat_tweets <- dat_tweets_raw |> 
    filter(is.na(in_reply_to_user_id))

table(dat_tweets$year)

dat_tweets$mpname <- dat_tweets$harmonised
dat_tweets$date <- as.Date(dat_tweets$fulldate)

max(dat_tweets$date, na.rm = TRUE)
min(dat_tweets$date, na.rm = TRUE)



# load questions
dat_questions_raw <- readRDS("writtenquestions.rds")

# remove some variables
names(dat_questions_raw)


dat_questions_raw <- dat_questions_raw |> 
    select(-c(gender, constituency_name, party))


dat_questions <- dat_questions_raw |> 
    mutate(year = as.integer(year)) |> 
    filter(between(year, 2013, 2022)) |> 
    mutate(mpname = str_squish(mpname)) |> 
    mutate(mpname = case_when(
        str_detect(mpname, "moran kevin") ~ "moran kevin",
        str_detect(mpname, "flanagan luke") ~ "flanagan luke",
        str_detect(mpname, "flanagan charl") ~ "flanagan charly",
        str_detect(mpname, "nash ge") ~ "nash gerald",
        str_detect(mpname, "duffy fran") ~ "duffy francis",
        str_detect(mpname, "corcoran") ~ "kennedy corcoran marcella",
        str_detect(mpname, "mahony margaret") ~ "o'mahony margaret murphy",
        str_detect(mpname, "o'brien j") ~ "o'brien jonathan",
        str_detect(mpname, "broughan t") ~ "broughan tommy",
        str_detect(mpname, "deering pat") ~ "deering patrick",
        str_detect(mpname, "barrett richard") ~ "boyd-barrett richard",
        TRUE ~ mpname))


nrow(dat_questions)

table(dat_questions$mpname)



nrow(dat_questions)


# recode names
dat_tweets <- dat_tweets |> 
    mutate(mpname = case_when(
        str_detect(mpname, "moran kevin") ~ "moran kevin",
        str_detect(mpname, "flanagan luke") ~ "flanagan luke",
        str_detect(mpname, "flanagan charl") ~ "flanagan charly",
        str_detect(mpname, "nash ge") ~ "nash gerald",
        str_detect(mpname, "duffy fran") ~ "duffy francis",
        str_detect(mpname, "corcoran") ~ "kennedy corcoran marcella",
        str_detect(mpname, "mahony margaret") ~ "o'mahony margaret murphy",
        str_detect(mpname, "o'brien j") ~ "o'brien jonathan",
        str_detect(mpname, "broughan t") ~ "broughan tommy",
        str_detect(mpname, "deering pat") ~ "deering patrick",
        str_detect(mpname, "barrett richard") ~ "boyd-barrett richard",
        TRUE ~ mpname))



dat_tweets_count_year <- dat_tweets |> 
    mutate(year = as.integer(year)) |> 
    filter(between(year, 2013, 2022)) |> 
    #mutate(mpname = harmonised) |> 
    group_by(year, harmonised) |> 
    count() |> 
    rename(n_tweets_year = n) |> 
    mutate(year = as.integer(year))


# get summary stats of tweeters/politicians who asked questions

dat_meta_all$year <- as.character(dat_meta_all$year)
dat_tweets_count_year$year <- as.character(dat_tweets_count_year$year)

dat_check_tweets_available <- left_join(dat_meta_all,
                                        dat_tweets_count_year,
                                        by = c("year", "harmonised")) |> 
    mutate(tweeted = ifelse(is.na(n_tweets_year), 0,
                            ifelse(n_tweets_year > 0, 1, 0)))


dat_tweets_props <- dat_check_tweets_available |> 
    group_by(year, gov_opp) |> 
    summarise(prop = mean(tweeted)) |> 
    mutate(type = "Twitter Accounts")


dat_tweets_props


dat_tweets_props_party <- dat_check_tweets_available |> 
    group_by(year, party_recoded) |> 
    summarise(prop_tweeters = mean(tweeted)) |> 
    mutate(type = "Twitter Accounts")


dat_questions_count_year <- dat_questions |> 
    filter(between(year, 2013, 2022)) |> 
    group_by(year, harmonised) |> 
    count() |> 
    rename(n_questions_year = n) |> 
    mutate(year = as.integer(year))

dat_meta_all$year <- as.character(dat_meta_all$year)
dat_questions_count_year$year <- as.character(dat_questions_count_year$year)

dat_check_questions_available <- left_join(dat_meta_all,
                                           dat_questions_count_year) |> 
    mutate(asked_question = ifelse(is.na(n_questions_year), 0,
                                   ifelse(n_questions_year > 0, 1, 0)))


dat_questions_props <- dat_check_questions_available |> 
    group_by(year, gov_opp) |> 
    summarise(prop = mean(asked_question)) |> 
    mutate(type = "Parliamentary Questions")


dat_props <- bind_rows(dat_questions_props,
                       dat_tweets_props)


dat_props$type <- relevel(factor(dat_props$type),
                          ref = "Twitter Accounts")

ggplot(dat_props, aes(x = factor(year), y = prop, 
                      fill = gov_opp)) +
    geom_bar(stat = "identity", position = position_dodge(width = 0.75),
             width = 0.7) +
    geom_text(aes(y = prop - 0.05, 
                  label = paste0(round(100 * prop, 0), "%")),
              colour = "white",
              size = 3,
              position = position_dodge(width = 0.75)) +
    scale_fill_manual(values = c("black", "grey60")) +
    facet_wrap(~type, nrow = 2, scales = "free_x") +
    scale_y_continuous(labels = scales::percent_format(accuracy = 1)) +
    labs(x = "Year", y = "Percentage of Legislators") +
    theme(legend.title = element_blank()) 
ggsave("fig_a03.pdf",
       width = 9.3, height = 6)
ggsave("fig_a03.eps",
       width = 9.3, height = 6)


# no differences anymore
setdiff(unique(dat_questions$harmonised),
        unique(dat_meta_all$harmonised))

setdiff(unique(dat_meta_all$harmonised),
        unique(dat_questions$harmonised))


## makes sense! these are government politicians

                             # [1] "coveney simon"  "deenihan jimmy" "hayes brian"   
                             # [4] "kenny enda"     "lynch kathleen" "reilly james"  
                             # [7] "varadkar leo"   "white alex"    

# check for tweets
setdiff(unique(dat_tweets$harmonised),
        unique(dat_meta_all$harmonised))


setdiff(unique(dat_meta_all$harmonised),
        unique(dat_tweets$harmonised))


dat_tweets$year <- as.character(dat_tweets$year)
dat_meta_all$year <- as.character(dat_meta_all$year)


# merge metadata

dat_tweets$year <- as.integer(dat_tweets$year)
dat_meta_all$year <- as.integer(dat_meta_all$year)

dat_tweets_merged <- left_join(
    select(dat_tweets, -mpname),
    dat_meta_all,
    by = c("harmonised", "year"))


dat_meta_all$year <- as.integer(dat_meta_all$year)

dat_questions_merged <- left_join(
    select(dat_questions, -mpname),
    dat_meta_all,
    by = c("harmonised", "year"))


# merge tweets and questions

dat_tweets_merged$type <- "Tweets"
dat_questions_merged$type <- "Parliamentary Questions"

dat_questions_merged <- dat_questions_merged |> 
    rename(text = quest)

min(dat_tweets_merged$date)
max(dat_tweets_merged$date)

# bind both datasets into a long dataset

dat_tweets_merged$year <- as.integer(dat_tweets_merged$year)
dat_questions_merged$year <- as.integer(dat_questions_merged$year)

dat_tweets_questions_all <- bind_rows(dat_tweets_merged,
                                  dat_questions_merged)


# stop in 2022
nrow(dat_tweets_questions_all)

dat_tweets_questions <- filter(dat_tweets_questions_all, year <= 2022)

nrow(dat_tweets_questions)

# remove hashtag symbols for better identification of keywords
dat_tweets_questions$text <- str_remove_all(dat_tweets_questions$text, "#")


# replace special characters and unicodes

dat_tweets_questions$text = str_replace_all(dat_tweets_questions$text, "\\<e1\\>", "a")
dat_tweets_questions$text = str_replace_all(dat_tweets_questions$text, "\\<ed\\>", "i")
dat_tweets_questions$text = str_replace_all(dat_tweets_questions$text, "\\<c9\\>", "e")
dat_tweets_questions$text = str_replace_all(dat_tweets_questions$text, "\\<fa\\>", "u")
dat_tweets_questions$text = str_replace_all(dat_tweets_questions$text, "\\<e9\\>", "e")
dat_tweets_questions$text = str_replace_all(dat_tweets_questions$text, "\\<f3\\>", "o")
dat_tweets_questions$text = str_replace_all(dat_tweets_questions$text, "\\<d3\\>", "o")

dat_tweets_questions$text = str_replace_all(dat_tweets_questions$text, "\\<U\\+0080\\>", "€")
dat_tweets_questions$text = str_replace_all(dat_tweets_questions$text, "\\<U\\+0092\\>", "'")
dat_tweets_questions$text = str_replace_all(dat_tweets_questions$text, "\\<U\\+00A0\\>", " ")
dat_tweets_questions$text = str_replace_all(dat_tweets_questions$text, "\\<U\\+0093\\>", "'")
dat_tweets_questions$text = str_replace_all(dat_tweets_questions$text, "\\<U\\+0094\\>", "'")
dat_tweets_questions$text = str_replace_all(dat_tweets_questions$text, "\\<U\\+0096\\>", "-")
dat_tweets_questions$text = str_replace_all(dat_tweets_questions$text, "\\<U\\+00A3\\>", "£")
dat_tweets_questions$text = str_replace_all(dat_tweets_questions$text, "\\<U\\+00B0\\>", "°")
dat_tweets_questions$text = str_replace_all(dat_tweets_questions$text, "\\<U\\+0097\\>", "—")
dat_tweets_questions$text = str_replace_all(dat_tweets_questions$text, "\\<U\\+00B7\\>", " ")
dat_tweets_questions$text = str_replace_all(dat_tweets_questions$text, "\\<U\\+009E\\>", "z")
dat_tweets_questions$text = str_replace_all(dat_tweets_questions$text, "\\<U\\+00B2\\>", "2")
dat_tweets_questions$text = str_replace_all(dat_tweets_questions$text, "\\<U\\+009A\\>", "s")


dat_tweets_questions$text = str_replace_all(dat_tweets_questions$text, "\\<U\\+0091\\>", "'")
dat_tweets_questions$text = str_replace_all(dat_tweets_questions$text, "\\<U\\+00AE\\>", "®")


str_replace_all("politics &amp; party", "&amp;", "&")

dat_tweets_questions$text = str_replace_all(dat_tweets_questions$text, "&amp;", "&")


table(dat_tweets_questions$year, 
      dat_tweets_questions$type)

dat_analysis <- dat_tweets_questions

table(dat_analysis$gov_opp,
      dat_analysis$party_recoded)


dat_analysis <- dat_analysis |> 
    mutate(party_broad = dplyr::recode(party_recoded,
                                       "AAA/S-PBP" = "Green Party and Left Bloc",
                                       "Green Party" = "Green Party and Left Bloc",
                                       "Labour" = "Green Party and Left Bloc",
                                       "Social Democrats" = "Green Party and Left Bloc"))


dat_analysis <- dat_analysis |> 
    mutate(party_broad = factor(party_broad, 
                                levels = c("Fine Gael",
                                           "Fianna Fáil",
                                           "Other/Independents",
                                           "Green Party and Left Bloc",
                                           "Sinn Féin")))


# reorder columns
dat_analysis <- dat_analysis |> 
    select(text, year, 
           fulldate,
           harmonised, 
           starts_with("ownership"),
           propnum,
           election_year, 
           birth, 
           seniority, 
           constituency_name,
           dublin_dummy,
           starts_with("party"),
           everything())

# remove the old birth column
dat_analysis = dat_analysis %>% dplyr::select(-birth)

# read file with birth years of all TDs
birth_years = read_parquet("birth_years_tds.parquet")

# merge birth years by harmonised
dat_analysis_newyears <- left_join(dat_analysis, birth_years, by = "harmonised")

# make sure merging did not change number of observations
stopifnot(nrow(dat_analysis_newyears) == nrow(dat_analysis))

# identify count of missing birth values
dat_analysis_newyears %>% filter(is.na(birth)) %>% count(birth)

# identify TDs with missing birth values
dat_analysis_newyears %>% filter(is.na(birth)) |> 
    select(harmonised) |> 
    unique()

# harmonised
# 1       berry cathal
# 353     leddin brian
# 869      o'brien joe
# 1490 o'rourke darren
# 2361    smyth ossian
# 2753   tully pauline
# 3120       ward mark
# 4382         daly pa

# create document_id variable
dat_analysis_newyears <- dat_analysis_newyears |> 
    group_by(harmonised, year, type) |> 
    mutate(document_id = paste(harmonised,  year, str_to_lower(type), 1:n(), sep = "_")) |> 
    ungroup() |> 
    select(document_id, everything())

# inspect how document_id looks like
dat_analysis_newyears %>% select(document_id) %>% head()

write_parquet(dat_analysis_newyears, "data_dontshare/data_analysis.parquet")


