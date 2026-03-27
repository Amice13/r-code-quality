#!/usr/bin/Rscript

# Replication code for:
# Bailo, F., & Vromen, A. (2016). 
# Hybrid social and news media protest events: from #MarchinMarch to #BusttheBudget in Australia, 
# Information, Communication & Society, 1–20. https://doi.org/10.1080/1369118X.2016.1252410

# 07_create_twitter_friendlist_db.R
## Updated: 2016-11-11 17:09:49 AEDT

setwd('~/Documents/replication_packages/ics_2016')


load("twitter_stream_data_cleaned.RData")

require(DBI)

user_list <- character()
for (event in twt) {
  user_list <- c(user_list, event[['user']][['id']])
}

user_list <- unique(user_list)

# WARNING: If you write, you might overwrite existing database
con <- dbConnect(RSQLite::SQLite(), dbname = "twitter_user_friendlists.sqlite")
dbSendQuery(con,
            "CREATE TABLE log
            (twt_id CHAR PRIMARY KEY,
            n_friends INT,
            status INTEGER DEFAULT (0),
            timestamp DATETIME)")
for(twt_id in unique(user_list)) {
  dbSendQuery(con,
              paste0("INSERT INTO log (twt_id) VALUES ('", 
                     twt_id,"')"))
}
dbSendQuery(con,
            "CREATE TABLE friendlist
            ([from] CHAR NOT NULL,
            [to] CHAR NOT NULL,
            timestamp DATETIME,
            PRIMARY KEY ([from], [to]))")
dbSendQuery(con,
            "CREATE TABLE user
            (
            id CHAR PRIMARY KEY,
            name CHAR,
            screen_name CHAR,
            location CHAR,
            description CHAR,
            default_profile CHAR,
            default_profile_image CHAR,
            profile_image_url CHAR,
            profile_background_tile CHAR,
            profile_background_image_url CHAR,
            profile_banner_url CHAR,
            profile_sidebar_fill_color CHAR,
            profile_background_color CHAR,
            profile_link_color CHAR,
            profile_text_color CHAR,
            protected CHAR,
            utc_offset CHAR,
            time_zone CHAR,
            url CHAR,
            statuses_count CHAR,
            followers_count CHAR,
            friends_count CHAR,
            favourites_count CHAR,
            geo_enabled CHAR,
            verified CHAR,
            lang CHAR,
            notifications CHAR,
            contributors_enabled CHAR,
            created_at CHAR,
            listed_count CHAR,
            timestamp DATETIME)")
dbClearResult(dbListResults(con)[[1]])
dbDisconnect(con)