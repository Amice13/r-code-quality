library(tidyverse)
library(stm)
library(quanteda)
library(gridExtra)
library(here)

setwd(here())
setwd("./twitter")

source("./code/Functions.R")

my_weight <- ""

tweets <- read_delim(
    "./data/top_friends_round3_add_other_china_refs.csv",
    delim=",",
    col_types = cols(
        .default = col_character(),
        created_at = col_date(format = "")
    ),
    comment=""
)

accounts = read_csv(
    "./data/CN_popular_account_list.csv",
    col_types = "cnccccncccncnnc"
)


tweets <- left_join(
    tweets,
    accounts %>% mutate(type = type_complete) %>%
    select(screen_name, type, followers_count, statuses_count),
    by=c("username" = "screen_name")
) %>% filter(
          type!="Porn"
      ) %>%
    mutate(
        state_media = type %in% c(
                                 "State Media or Chinese Officials"
                             ),
        politics = type %in% c(
                                 "Activists or US / Taiwan / Hong Kong Politics",
                                 "Citizen Journalists / Political Bloggers",
                                 "International News Agencies",
                                 "State Media or Chinese Officials"
                             ),
        politics_not_state_media = type %in% c(
                                                 "Activists or US / Taiwan / Hong Kong Politics",
                                                 "Citizen Journalists / Political Bloggers",
                                                 "International News Agencies"
                                             ),
    other = type %in% c(
                          "Non-Political Bloggers or Entertainment Accounts",
                          "Pornography Accounts"
                      ),
    state_media = type %in% "State Media or Chinese Officials"
    )


n_country_mentions <- tweets %>%
    mutate(
        tweet_source = case_when(
            politics_not_state_media == 1 ~ "politics_not_state_media",
            state_media == 1 ~ "state_media",
            TRUE ~ "other"
        )
    ) %>%
    group_by(tweet_source, created_at) %>%
    mutate(
        across(country_en:covid_anyLang, as.numeric)
    ) %>%
    mutate(
        wuhan_zh = as.numeric(grepl("武汉|武漢", text)),
        wuhan_en = as.numeric(grepl("wuhan", text, ignore.case=T))
    ) %>%
    summarise(
        china_zh_n = sum(china_zh),
        china_en_n = sum(china_en),
        china_provinces_zh_n = sum(china_provinces_zh),
        china_provinces_en_n = sum(china_provinces_en),
        wuhan_zh_n = sum(wuhan_zh),
        wuhan_en_n = sum(wuhan_en),
        country_zh_n = sum(country_zh),
        country_en_n = sum(country_en),
        any_n = n()
   )

save(
    n_country_mentions,
    file=str_glue(
        "./data/n_country_mentions{my_weight}_fixed_categories_ats_removed_add_woguo.RData"
        )
)




cleanAndModelTopics <- function(tweets_sub) {

CleanTweet = function(unclean_tweet) {
  clean_tweet = gsub("&amp", " ", unclean_tweet)
  clean_tweet = gsub("(RT|via)((?:\\b\\W*@\\w+)+)", " ", clean_tweet)
  clean_tweet = gsub("@\\w+", " ", clean_tweet)
  clean_tweet = gsub("[[:punct:]]", " ", clean_tweet)
  clean_tweet = gsub("[[:digit:]]", " ", clean_tweet)
  clean_tweet = gsub("http\\w+", " ", clean_tweet)
  clean_tweet = gsub("[ \t]{2,}", " ", clean_tweet)
  clean_tweet = gsub("^\\s+|\\s+$", " ", clean_tweet)
  clean_tweet = gsub(" *\\b[[:alpha:]]{1}\\b *", " ", clean_tweet)
  clean_tweet = gsub("http://t.co/[a-z,A-Z,0-9]*{8}", " ", clean_tweet)
  clean_tweet = gsub("http.*"," ", clean_tweet)
  clean_tweet = gsub("https.*"," ", clean_tweet)
  clean_tweet = gsub("[A-Za-z0-9]", " ", clean_tweet)
  clean_tweet = gsub("[^\\p{Han}+]", " ", clean_tweet, perl=TRUE)
  return(clean_tweet)
}

tweets_sub$tweet_clean = CleanTweet(tweets_sub$text)

tweet_toks = tweets_sub$tweet_clean %>%
  tokens(remove_punct = TRUE) %>%
  tokens_remove(pattern = stopwords("zh", source = "misc"))

tweets_dfm = dfm(tweet_toks)

tweet_out = convert(tweets_dfm, to="stm", docvars=tweets_sub)
tweet_out$meta$date = as.Date(tweet_out$meta$created_at, format="%Y-%m-%d")
tweet_out$meta$after = tweet_out$meta$created_at > as.Date("2020-01-23")

set.seed(987654321)
tweet_stm = stm(
    documents = tweet_out$documents,
    vocab = tweet_out$vocab,
    data = tweet_out$meta,
    K = 50, init.type="Spectral"
)

    return(
        list(
            topic_model=tweet_stm,
            tweets=tweet_out$meta
        )
    )
}

if (my_weight=="") {
    tweets$weight <- rep(1, nrow(tweets))
} else if (my_weight=="_follow_div_statuses") {
    tweets$weight <- with(
        tweets,
        followers_count / statuses_count
    )
}

set.seed(987654321)
tweets_for_model <- list(
    tweets_cn = tweets %>%
        filter(
            (china_zh==1 | china_provinces_zh==1) & created_at >= "2019-12-01"
            & politics_not_state_media
        ) %>%
        sample_n(10000, weight = weight),
    tweets_country = tweets %>%
        filter(
            country_zh==1 & created_at >= "2019-12-01"
            & politics_not_state_media
        ) %>%
        sample_n(10000, weight = weight),
    tweets_all = tweets %>%
        filter(
            created_at >= "2019-12-01"
            & politics_not_state_media
        ) %>%
        sample_n(10000, weight = weight)
)

model <- lapply(
    tweets_for_model,
    cleanAndModelTopics
)

save(
    model,
    file=str_glue(
        "./data/all_topic_estimates{my_weight}_fixed_categories_ats_removed_add_woguo_and_provinces.RData"
        )
)
