#!/usr/bin/Rscript

# Replication code for:
# Bailo, F., & Vromen, A. (2016). 
# Hybrid social and news media protest events: from #MarchinMarch to #BusttheBudget in Australia, 
# Information, Communication & Society, 1–20. https://doi.org/10.1080/1369118X.2016.1252410

# 05_load_and_prepare_twitter_stream_data_2014.R
## Updated: 2016-11-11 15:34:17 AEDT

setwd('~/Documents/replication_packages/ics_2016')

# Functions
twtDate2EstTz.jul <- function (twtdate) {
  
  dt.POSIXlt.gmt <- strptime(twtdate, "%a %b %d %H:%M:%S %z %Y", tz = "GMT")
  dt.POSIXlct.gmt  <- as.POSIXct(dt.POSIXlt.gmt, tz = "GMT")
  dt.char.est <- format(dt.POSIXlct.gmt, tz = "Australia/Sydney", usetz = TRUE)
  return(dt.char.est)
}

twtDate2EstTz.mar <- function (twtdate) {
  
  dt.POSIXlt.gmt <- strptime(twtdate, "%Y-%m-%d %H:%M:%S", tz = "GMT")
  dt.POSIXlct.gmt  <- as.POSIXct(dt.POSIXlt.gmt, tz = "GMT")
  dt.char.est <- format(dt.POSIXlct.gmt, tz = "Australia/Sydney", usetz = TRUE)
  return(dt.char.est)
}

# Load data
load("twitter_stream_data_2014.RData")

# Add posix column
twt$march$tweet$created_at.posix <- as.POSIXct(twtDate2EstTz.mar(twt$march$tweet$created_at))
twt$july$tweet$created_at.posix <- as.POSIXct(twtDate2EstTz.jul(twt$july$tweet$created_at))
twt$august$tweet$created_at.posix <- as.POSIXct(twtDate2EstTz.jul(twt$august$tweet$created_at))


## CLEANING

# First round of cleaning
twt$march$user$lang[twt$march$user$lang=="https://about.twitter.com/products/tweetdeck"] <- "Unk"

# Remove all non-English speakers
twt_march_merged <- merge(twt$march$tweet, twt$march$user, by.x="user_id", by.y="id")
df <- twt_march_merged[grep("^en", twt_march_merged$lang), ]
ids <- df$id
user_ids <- df$user_id
n_tweet_exante <- nrow(twt$march$tweet)
n_user_exante <- nrow(twt$march$user)
twt$march$tweet <- twt$march$tweet[twt$march$tweet$id %in% ids, ]
twt$march$user <- twt$march$user[twt$march$user$id %in% user_ids, ]
twt$march$tweet.removed.step1 <- n_tweet_exante - nrow(twt$march$tweet)
twt$march$user.removed.step1 <- n_user_exante - nrow(twt$march$user)

rm(df, ids, user_ids, n_tweet_exante, n_user_exante)

# Remove tweets containing specific hashtags
hash2remove <- c("#18m", "#18may", "#venezuela", "#caracas", "#ufc171", "#lfc", "#somaliland", "#valencia", "#maracaibo", "#menosmarchasmasguarimbas", "#quierounavenezueladonde", "#liberenalosestudiantes", "#lucharhastavencer", "#barquisimeto", "#arsenal", "#sosvenezuela", "#elquesecansapierde", "#manutd", "#merida", "#ibrahimkaypakkaya", "#carabobo")
sanitizeHash <- function(x) {gsub("#","\\\\#",x)}
pattern <- paste(hash2remove, sep="", collapse="|")
df <- twt_march_merged
df <- df[!grepl(pattern, df$text, ignore.case = TRUE), ]
ids <- df$id
user_ids <- df$user_id
n_tweet_exante <- nrow(twt$march$tweet)
n_user_exante <- nrow(twt$march$user)
twt$march$tweet <- twt$march$tweet[twt$march$tweet$id %in% ids, ]
twt$march$user <- twt$march$user[twt$march$user$id %in% user_ids, ]
twt$march$tweet.removed.step2 <- n_tweet_exante - nrow(twt$march$tweet)
twt$march$user.removed.step2 <- n_user_exante - nrow(twt$march$user)

rm(df, ids, user_ids, n_tweet_exante, n_user_exante)

# END CLEANING

# Define time windows and subset

events <- list()

events[['march']] <- list(lower_bound = as.POSIXct("2014-03-15 12:00:00", tz = "Australia/Sydney"),
                          event_start_at = as.POSIXct("2014-03-16 12:00:00", tz = "Australia/Sydney"),
                          upper_bound = as.POSIXct("2014-03-17 12:00:00", tz = "Australia/Sydney"))
events[['may']] <- list(lower_bound = as.POSIXct("2014-05-17 12:00:00", tz = "Australia/Sydney"),
                        event_start_at = as.POSIXct("2014-05-18 12:00:00", tz = "Australia/Sydney"),
                        upper_bound = as.POSIXct("2014-05-19 12:00:00", tz = "Australia/Sydney"))
events[['july']] <- list(lower_bound = as.POSIXct("2014-07-05 13:00:00", tz = "Australia/Sydney"),
                         event_start_at = as.POSIXct("2014-07-06 13:00:00", tz = "Australia/Sydney"),
                         upper_bound = as.POSIXct("2014-07-07 13:00:00", tz = "Australia/Sydney"))
events[['august']] <- list(lower_bound = as.POSIXct("2014-08-30 13:00:00", tz = "Australia/Sydney"),
                           event_start_at = as.POSIXct("2014-08-31 13:00:00", tz = "Australia/Sydney"),
                           upper_bound = as.POSIXct("2014-09-01 13:00:00", tz = "Australia/Sydney"))

# Rename and define new empty variables
twt_raw <- twt
fb_raw <- fb

rm(fb)
rm(twt)

twt <- list()
fb <- list()

for (event in names(events)) {
  twt[[event]] <- list()
  fb[[event]] <- list()
}

# Subset TWT
twt[['march']] <- list(user = twt_raw$march$user,
                       tweet = subset(twt_raw$march$tweet,
                                      created_at.posix >= events$march$lower_bound & 
                                        created_at.posix <= events$march$upper_bound))
twt[['may']] <- list(user = twt_raw$march$user,
                     tweet = subset(twt_raw$march$tweet,
                                    created_at.posix >= events$may$lower_bound & 
                                      created_at.posix <= events$may$upper_bound))
twt[['july']] <- list(user = twt_raw$july$user,
                      tweet = subset(twt_raw$july$tweet,
                                     created_at.posix >= events$july$lower_bound & 
                                       created_at.posix <= events$july$upper_bound))
twt[['august']] <- list(user = twt_raw$august$user,
                        tweet = subset(twt_raw$august$tweet,
                                       created_at.posix >= events$august$lower_bound & 
                                         created_at.posix <= events$august$upper_bound))
# Remove users non present in 'tweet'
for (event in names(twt)) {
  twt[[event]]$user <- twt[[event]]$user[twt[[event]]$user$id %in% twt[[event]]$tweet$user_id,]
}

# WARNING: If you save, you might overwrite existing file
save(events, twt, file="twitter_stream_data_cleaned.RData")

