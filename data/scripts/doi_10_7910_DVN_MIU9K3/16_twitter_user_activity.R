#!/usr/bin/Rscript

# Replication code for:
# Bailo, F., & Vromen, A. (2016). 
# Hybrid social and news media protest events: from #MarchinMarch to #BusttheBudget in Australia, 
# Information, Communication & Society, 1–20. https://doi.org/10.1080/1369118X.2016.1252410

# 16_twitter_user_activity.R
## Updated: 2016-11-12 12:51:53 AEDT

setwd('~/Documents/replication_packages/ics_2016')

load("twitter_stream_data_cleaned.RData")

datasets <- names(twt)

# This table must contains all the attributes for the analysis
user_activity_attributes <- data.frame(user=character())

for (dataset in datasets) {
  user_activity_attributes <- rbind(user_activity_attributes,
                                    data.frame(user=as.character(twt[[dataset]]$user$id)))
}
user_activity_attributes <- unique(user_activity_attributes)

# Code tweet typology
codeTweet <- function(text){
  retweet <- grepl("^rt", tolower(text))
  if (retweet==TRUE) return("Retweet")
  
  reply <- grepl("^\\.?@[a-z0-9_]{1,15}", tolower(text))
  if (reply==TRUE) return("Direct reply")
  
  information <- grepl("https?:\\/\\/", tolower(text))
  if (information==TRUE) return("Link") 
  
  else return("Talk")
}

for (dataset in datasets) {
  twt[[dataset]]$tweet$coded_typology <- sapply(twt[[dataset]]$tweet$text, codeTweet)
}

# Number of tweets
tweets_mar14 <- data.frame(user=names(table(twt[['march']]$tweet$user_id)),
                           tweets_mar14=data.frame(table(twt[['march']]$tweet$user_id))$Freq)
tweets_may14 <- data.frame(user=names(table(twt[['may']]$tweet$user_id)),
                           tweets_may14=data.frame(table(twt[['may']]$tweet$user_id))$Freq)
tweets_jul14 <- data.frame(user=names(table(twt[['july']]$tweet$user_id)),
                           tweets_jul14=data.frame(table(twt[['july']]$tweet$user_id))$Freq)
tweets_aug14 <- data.frame(user=names(table(twt[['august']]$tweet$user_id)),
                           tweets_aug14=data.frame(table(twt[['august']]$tweet$user_id))$Freq)
tweets_mar15a <- data.frame(user=names(table(twt[['march_2015a']]$tweet$user_id)),
                            tweets_mar15a=data.frame(table(twt[['march_2015a']]$tweet$user_id))$Freq)
tweets_mar15b <- data.frame(user=names(table(twt[['march_2015b']]$tweet$user_id)),
                            tweets_mar15b=data.frame(table(twt[['march_2015b']]$tweet$user_id))$Freq)

# Number of Retweet
tweets_mar14_rt <- 
  data.frame(
    user=names(table(
      subset(twt[['march']]$tweet,coded_typology=='Retweet')$user_id)),
    tweets_mar14_rt=data.frame(table(
      subset(twt[['march']]$tweet,coded_typology=='Retweet')$user_id))$Freq)
tweets_may14_rt <- 
  data.frame(
    user=names(table(
      subset(twt[['may']]$tweet,coded_typology=='Retweet')$user_id)),
    tweets_may14_rt=data.frame(table(
      subset(twt[['may']]$tweet,coded_typology=='Retweet')$user_id))$Freq)
tweets_jul14_rt <- 
  data.frame(
    user=names(table(
      subset(twt[['july']]$tweet,coded_typology=='Retweet')$user_id)),
    tweets_jul14_rt=data.frame(table(
      subset(twt[['july']]$tweet,coded_typology=='Retweet')$user_id))$Freq)
tweets_aug14_rt <- 
  data.frame(
    user=names(table(
      subset(twt[['august']]$tweet,coded_typology=='Retweet')$user_id)),
    tweets_aug14_rt=data.frame(table(
      subset(twt[['august']]$tweet,coded_typology=='Retweet')$user_id))$Freq)
tweets_mar15a_rt <- 
  data.frame(
    user=names(table(
      subset(twt[['march_2015a']]$tweet,coded_typology=='Retweet')$user_id)),
    tweets_mar15a_rt=data.frame(table(
      subset(twt[['march_2015a']]$tweet,coded_typology=='Retweet')$user_id))$Freq)
tweets_mar15b_rt <- 
  data.frame(
    user=names(table(
      subset(twt[['march_2015b']]$tweet,coded_typology=='Retweet')$user_id)),
    tweets_mar15b_rt=data.frame(table(
      subset(twt[['march_2015b']]$tweet,coded_typology=='Retweet')$user_id))$Freq)


# Number of Direct reply
tweets_mar14_dr <- 
  data.frame(
    user=names(table(
      subset(twt[['march']]$tweet,coded_typology=='Direct reply')$user_id)),
    tweets_mar14_dr=data.frame(table(
      subset(twt[['march']]$tweet,coded_typology=='Direct reply')$user_id))$Freq)
tweets_may14_dr <- 
  data.frame(
    user=names(table(
      subset(twt[['may']]$tweet,coded_typology=='Direct reply')$user_id)),
    tweets_may14_dr=data.frame(table(
      subset(twt[['may']]$tweet,coded_typology=='Direct reply')$user_id))$Freq)
tweets_jul14_dr <- 
  data.frame(
    user=names(table(
      subset(twt[['july']]$tweet,coded_typology=='Direct reply')$user_id)),
    tweets_jul14_dr=data.frame(table(
      subset(twt[['july']]$tweet,coded_typology=='Direct reply')$user_id))$Freq)
tweets_aug14_dr <- 
  data.frame(
    user=names(table(
      subset(twt[['august']]$tweet,coded_typology=='Direct reply')$user_id)),
    tweets_aug14_dr=data.frame(table(
      subset(twt[['august']]$tweet,coded_typology=='Direct reply')$user_id))$Freq)
tweets_mar15a_dr <- 
  data.frame(
    user=names(table(
      subset(twt[['march_2015a']]$tweet,coded_typology=='Direct reply')$user_id)),
    tweets_mar15a_dr=data.frame(table(
      subset(twt[['march_2015a']]$tweet,coded_typology=='Direct reply')$user_id))$Freq)
tweets_mar15b_dr <- 
  data.frame(
    user=names(table(
      subset(twt[['march_2015b']]$tweet,coded_typology=='Direct reply')$user_id)),
    tweets_mar15b_dr=data.frame(table(
      subset(twt[['march_2015b']]$tweet,coded_typology=='Direct reply')$user_id))$Freq)

# Number of Link
tweets_mar14_link <- 
  data.frame(
    user=names(table(
      subset(twt[['march']]$tweet,coded_typology=='Link')$user_id)),
    tweets_mar14_link=data.frame(table(
      subset(twt[['march']]$tweet,coded_typology=='Link')$user_id))$Freq)
tweets_may14_link <- 
  data.frame(
    user=names(table(
      subset(twt[['may']]$tweet,coded_typology=='Link')$user_id)),
    tweets_may14_link=data.frame(table(
      subset(twt[['may']]$tweet,coded_typology=='Link')$user_id))$Freq)
tweets_jul14_link <- 
  data.frame(
    user=names(table(
      subset(twt[['july']]$tweet,coded_typology=='Link')$user_id)),
    tweets_jul14_link=data.frame(table(
      subset(twt[['july']]$tweet,coded_typology=='Link')$user_id))$Freq)
tweets_aug14_link <- 
  data.frame(
    user=names(table(
      subset(twt[['august']]$tweet,coded_typology=='Link')$user_id)),
    tweets_aug14_link=data.frame(table(
      subset(twt[['august']]$tweet,coded_typology=='Link')$user_id))$Freq)
tweets_mar15a_link <- 
  data.frame(
    user=names(table(
      subset(twt[['march_2015a']]$tweet,coded_typology=='Link')$user_id)),
    tweets_mar15a_link=data.frame(table(
      subset(twt[['march_2015a']]$tweet,coded_typology=='Link')$user_id))$Freq)
tweets_mar15b_link <- 
  data.frame(
    user=names(table(
      subset(twt[['march_2015b']]$tweet,coded_typology=='Link')$user_id)),
    tweets_mar15b_link=data.frame(table(
      subset(twt[['march_2015b']]$tweet,coded_typology=='Link')$user_id))$Freq)

# Number of Talk
tweets_mar14_talk <- 
  data.frame(
    user=names(table(
      subset(twt[['march']]$tweet,coded_typology=='Talk')$user_id)),
    tweets_mar14_talk=data.frame(table(
      subset(twt[['march']]$tweet,coded_typology=='Talk')$user_id))$Freq)
tweets_may14_talk <- 
  data.frame(
    user=names(table(
      subset(twt[['may']]$tweet,coded_typology=='Talk')$user_id)),
    tweets_may14_talk=data.frame(table(
      subset(twt[['may']]$tweet,coded_typology=='Talk')$user_id))$Freq)
tweets_jul14_talk <- 
  data.frame(
    user=names(table(
      subset(twt[['july']]$tweet,coded_typology=='Talk')$user_id)),
    tweets_jul14_talk=data.frame(table(
      subset(twt[['july']]$tweet,coded_typology=='Talk')$user_id))$Freq)
tweets_aug14_talk <- 
  data.frame(
    user=names(table(
      subset(twt[['august']]$tweet,coded_typology=='Talk')$user_id)),
    tweets_aug14_talk=data.frame(table(
      subset(twt[['august']]$tweet,coded_typology=='Talk')$user_id))$Freq)
tweets_mar15a_talk <- 
  data.frame(
    user=names(table(
      subset(twt[['march_2015a']]$tweet,coded_typology=='Talk')$user_id)),
    tweets_mar15a_talk=data.frame(table(
      subset(twt[['march_2015a']]$tweet,coded_typology=='Talk')$user_id))$Freq)
tweets_mar15b_talk <- 
  data.frame(
    user=names(table(
      subset(twt[['march_2015b']]$tweet,coded_typology=='Talk')$user_id)),
    tweets_mar15b_talk=data.frame(table(
      subset(twt[['march_2015b']]$tweet,coded_typology=='Talk')$user_id))$Freq)

attr_vector <- c("tweets_mar14",
                 "tweets_mar14_dr", "tweets_mar14_link", "tweets_mar14_rt", "tweets_mar14_talk",
                 "tweets_may14",
                 "tweets_may14_dr", "tweets_may14_link", "tweets_may14_rt", "tweets_may14_talk",
                 "tweets_jul14", 
                 "tweets_jul14_dr", "tweets_jul14_link", "tweets_jul14_rt", "tweets_jul14_talk",
                 "tweets_aug14", "tweets_aug14_dr", "tweets_aug14_link",
                 "tweets_aug14_rt", "tweets_aug14_talk",
                 "tweets_mar15a",
                 "tweets_mar15a_dr", "tweets_mar15a_link", "tweets_mar15a_rt", "tweets_mar15a_talk",
                 "tweets_mar15b",
                 "tweets_mar15b_dr", "tweets_mar15b_link", "tweets_mar15b_rt", "tweets_mar15b_talk"
)

for (attr in attr_vector) {
  # print(attr)
  user_activity_attributes <- merge(user_activity_attributes, get(attr), all.x=TRUE)
}
user_activity_attributes[is.na(user_activity_attributes)] <- 0

save(user_activity_attributes, file="twitter_user_activity.RData")

