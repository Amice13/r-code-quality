#!/usr/bin/Rscript

# Replication code for:
# Bailo, F., & Vromen, A. (2016). 
# Hybrid social and news media protest events: from #MarchinMarch to #BusttheBudget in Australia, 
# Information, Communication & Society, 1–20. https://doi.org/10.1080/1369118X.2016.1252410

# 06_load_and_prepare_twitter_stream_data_2015.R
## Updated: 2016-11-11 15:34:17 AEDT

setwd('~/Documents/replication_packages/ics_2016')

sqLiteConnect <- function(database, table) {
  require(DBI)
  con <- dbConnect(RSQLite::SQLite(), dbname = database)
  query <- dbSendQuery(con, paste("SELECT * FROM ", table, ";", sep="")) 
  result <- fetch(query, n = -1)
  dbClearResult(query)
  dbDisconnect(con)
  return(result)
}

twtDate2EstTz <- function (twtdate) {
  
  dt.POSIXlt.gmt <- strptime(twtdate, "%a %b %d %H:%M:%S %z %Y", tz = "GMT")
  dt.POSIXlct.gmt  <- as.POSIXct(dt.POSIXlt.gmt, tz = "GMT")
  dt.char.est <- format(dt.POSIXlct.gmt, tz = "Australia/Sydney", usetz = TRUE)
  return(dt.char.est)
}

new_tweet1 <- sqLiteConnect("twitter_stream_data_2015a.sqlite","tweet")
new_tweet_user1 <- sqLiteConnect("twitter_stream_data_2015a.sqlite","user")

# Add posix column
new_tweet1$created_at.posix <- as.POSIXct(twtDate2EstTz(new_tweet1$created_at))

new_tweet2 <- sqLiteConnect("twitter_stream_data_2015b.sqlite","tweet")
new_tweet_user2 <- sqLiteConnect("twitter_stream_data_2015b.sqlite","user")

# Add posix column
new_tweet2$created_at.posix <- as.POSIXct(twtDate2EstTz(new_tweet2$created_at))

## CLEANING

# Add to old/cleaned data
load("twitter_stream_data_cleaned.RData")

# Define time windows and subset

events[['march_2015a']] <- list(lower_bound = as.POSIXct("2015-03-03 13:00:00", tz = "Australia/Sydney"),
                                event_start_at = as.POSIXct("2015-03-04 13:00:00", tz = "Australia/Sydney"),
                                upper_bound = as.POSIXct("2015-03-05 13:00:00", tz = "Australia/Sydney"))

events[['march_2015b']] <- list(lower_bound = as.POSIXct("2015-03-21 13:00:00", tz = "Australia/Sydney"),
                                event_start_at = as.POSIXct("2015-03-22 13:00:00", tz = "Australia/Sydney"),
                                upper_bound = as.POSIXct("2015-03-23 13:00:00", tz = "Australia/Sydney"))


# Subset TWT
twt[['march_2015a']] <- list(user = new_tweet_user1,
                             tweet = subset(new_tweet1,
                                            created_at.posix >= events$march_2015a$lower_bound & 
                                              created_at.posix <= events$march_2015a$upper_bound))

# Remove users non present in 'tweet'
twt[['march_2015a']]$user <- twt[['march_2015a']]$user[twt[['march_2015a']]$user$id %in% twt[['march_2015a']]$tweet$user_id,]

# Subset TWT
twt[['march_2015b']] <- list(user = new_tweet_user2,
                             tweet = subset(new_tweet2,
                                            created_at.posix >= events$march_2015b$lower_bound & 
                                              created_at.posix <= events$march_2015b$upper_bound))

# Remove users non present in 'tweet'
twt[['march_2015b']]$user <- twt[['march_2015b']]$user[twt[['march_2015b']]$user$id %in% twt[['march_2015b']]$tweet$user_id,]


save(events, twt, file="twitter_stream_data_cleaned.RData")
