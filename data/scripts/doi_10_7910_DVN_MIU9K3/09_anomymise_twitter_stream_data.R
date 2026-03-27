#!/usr/bin/Rscript

# Replication code for:
# Bailo, F., & Vromen, A. (2016). 
# Hybrid social and news media protest events: from #MarchinMarch to #BusttheBudget in Australia, 
# Information, Communication & Society, 1–20. https://doi.org/10.1080/1369118X.2016.1252410

# 09_anonymise_twitter_stream_data.R
## Updated: 2016-11-11 16:01:53 AEDT

setwd('~/Documents/replication_packages/ics_2016')

load('twitter_stream_data_cleaned.RData')

for (event in names(twt)) {
  user_cols_to_keep <- c('id','timestamp')
  tweet_cols_to_keep <- c('id', 'user_id','timestamp')
  twt[[event]]$user <- twt[[event]]$user[,user_cols_to_keep]
  twt[[event]]$tweet <- twt[[event]]$tweet[,tweet_cols_to_keep]
}

save(events, twt, file = 'twitter_stream_data_cleaned_and_anonymised.RData')
