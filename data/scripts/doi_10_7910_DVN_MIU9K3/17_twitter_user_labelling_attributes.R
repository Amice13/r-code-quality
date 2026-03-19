#!/usr/bin/Rscript

# Replication code for:
# Bailo, F., & Vromen, A. (2016). 
# Hybrid social and news media protest events: from #MarchinMarch to #BusttheBudget in Australia, 
# Information, Communication & Society, 1–20. https://doi.org/10.1080/1369118X.2016.1252410

# 17_twitter_user_labelling_attributes.R
## Updated: 2016-11-12 12:51:53 AEDT

setwd('~/Documents/replication_packages/ics_2016')


sqliteGetTable <- function(database, table) {
  require(DBI)
  require(RSQLite)
  con <- dbConnect(RSQLite::SQLite(), dbname = database)
  query <- dbSendQuery(con, paste("SELECT * FROM ", table, ";", sep="")) 
  result <- fetch(query, n = -1)
  dbClearResult(query)
  dbDisconnect(con)
  return(result)
}

sqliteListTables <- function(database, table) {
  require(DBI)
  require(RSQLite)
  con <- dbConnect(RSQLite::SQLite(), dbname = database)
  return(dbListTables(con))
}

friendlist_db <- "twitter_user_friendlists.sqlite"

load("twitter_stream_data_cleaned_and_anonymised.RData")

datasets <- names(twt)

# This table must contains all the attributes for the analysis
user_labelling_attributes <- data.frame(user=character())

for (dataset in datasets) {
  user_labelling_attributes <- rbind(user_labelling_attributes,
                                     data.frame(user=as.character(twt[[dataset]]$user$id)))
}
user_labelling_attributes <- unique(user_labelling_attributes)

load('twitter_public_lists.RData')

# USER

for (label in names(macro_sets)) {
  print(label)
  user_labelling_attributes <- 
    merge(user_labelling_attributes,
          data.frame(user=macro_sets[[label]],
                     tmp_label=rep(TRUE,length(macro_sets[[label]]))),
          by='user',
          all.x=TRUE
    )
  names(user_labelling_attributes)[names(user_labelling_attributes)=="tmp_label"] <- 
    paste0('user_', label)
}

for (label in names(micro_sets)) {
  print(label)
  user_labelling_attributes <- 
    merge(user_labelling_attributes,
          data.frame(user=micro_sets[[label]],
                     tmp_label=rep(TRUE,length(micro_sets[[label]]))),
          by='user',
          all.x=TRUE
    )
  names(user_labelling_attributes)[names(user_labelling_attributes)=="tmp_label"] <- 
    paste0('user_', label)
}

save(user_labelling_attributes, file="twitter_user_labelling_attributes.RData")

# FRIENDS
require(data.table)
log <- sqliteGetTable(friendlist_db, 'log')

user_labelling_attributes <- merge(user_labelling_attributes, 
                                   log[,c('twt_id','n_friends')], by.x='user', by.y='twt_id',
                                   all.x=TRUE)

friendlist <- sqliteGetTable(friendlist_db, 'friendlist')

friendlist <- data.table(friendlist, key='to')

for (label in names(macro_sets)) {
  print(label)
  friendlist <- 
    merge(friendlist,
          data.table(data.frame(to=macro_sets[[label]],
                                tmp_label=rep(TRUE,length(macro_sets[[label]]))),
                     key='to'),
          #by='to',
          all.x=TRUE
    )
  # names(friendlist)[names(friendlist)=="tmp_label"] <- 
  #   paste0('friends_', label)
  setnames(friendlist, 'tmp_label', paste0('friends_', label))
}
for (label in names(micro_sets)) {
  print(label)
  friendlist <- 
    merge(friendlist,
          data.table(data.frame(to=micro_sets[[label]],
                                tmp_label=rep(TRUE,length(micro_sets[[label]]))),
                     key='to'),
          #by='to',
          all.x=TRUE
    )
  # names(friendlist)[names(friendlist)=="tmp_label"] <- 
  #   paste0('friends_', label)
  setnames(friendlist, 'tmp_label', paste0('friends_', label))
}

attributes <- colnames(friendlist)[4:length(colnames(friendlist))]

for (label in attributes) {
  print(label)
  tmp_df <- as.data.frame(table(friendlist$from, friendlist[[label]]))
  tmp_df <- tmp_df[,c('Var1','Freq')]
  names(tmp_df)[names(tmp_df)=="Var1"] <- 'user'
  names(tmp_df)[names(tmp_df)=="Freq"] <- label
  user_labelling_attributes <- 
    merge(user_labelling_attributes, tmp_df, by='user', all.x=TRUE)
}

save(user_labelling_attributes, file="twitter_user_labelling_attributes.RData")
