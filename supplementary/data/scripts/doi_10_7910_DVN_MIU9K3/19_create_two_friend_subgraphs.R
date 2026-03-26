#!/usr/bin/Rscript

# Replication code for:
# Bailo, F., & Vromen, A. (2016). 
# Hybrid social and news media protest events: from #MarchinMarch to #BusttheBudget in Australia, 
# Information, Communication & Society, 1–20. https://doi.org/10.1080/1369118X.2016.1252410

# 19_create_two_friend_subgraphs.R
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


friendlist_db <- "twitter_user_friendlists.sqlite"
friendlist <- sqliteGetTable(friendlist_db, "friendList")

# Cleaning 
# There are 3 instances of users befriending themselves (API error?)
friendlist <- friendlist[-which(friendlist$from == friendlist$to),]

# Create user lists
load("twitter_stream_data_cleaned_and_anonymised.RData")

datasets <- names(twt)

# This table must contains all the attributes for the analysis
user_ego_net_attributes <- data.frame(user=character())

event_labels <- c('mar14', 'may14', 'jul14', 'aug14', 'mar15a', 'mar15b')

user_active_by_event <- list()

for (i in 1:length(datasets)) {
  user_active_by_event[[event_labels[i]]] <- as.character(twt[[datasets[i]]]$user$id)
  user_ego_net_attributes <- rbind(user_ego_net_attributes,
                                   data.frame(user=user_active_by_event[[event_labels[i]]]))
}

subgraphs <- list()

library(igraph)
for (i in 1:length(datasets)) {
  subgraphs[[paste0('g_', event_labels[i])]] <-
    graph.data.frame(subset(friendlist[,1:2], from %in% user_active_by_event[[i]] &
                              to %in% user_active_by_event[[i]]),
                     directed=TRUE, vertices=NULL)
}

for (label in names(subgraphs)) {
  subgraphs[[label]] <- 
    subgraphs[[label]] - E(subgraphs[[label]])[!is.mutual(subgraphs[[label]], E(subgraphs[[label]]))]
  subgraphs[[label]] <- subgraphs[[label]] - V(subgraphs[[label]])[degree(subgraphs[[label]])
                                                                   == 0]
}

for (label in names(subgraphs)) {
  subgraphs[[label]] <- as.directed(simplify(subgraphs[[label]]))
}

names(subgraphs)
subgraphs[['g_mar14']]
neigh <- graph.neighborhood(subgraphs[['g_mar14']],
                            order = 1,
                            mode = 'out',
                            V(subgraphs[['g_mar14']])$name==17384075)[[1]]
news_ego_mutual <- neigh -
  E(neigh)[!is.mutual(neigh, E(neigh))]
news_ego_mutual <- news_ego_mutual - V(news_ego_mutual)[degree(news_ego_mutual)==0]

neigh <- graph.neighborhood(subgraphs[['g_may14']],
                            order = 1,
                            mode = 'out',
                            V(subgraphs[['g_may14']])$name==158347224)[[1]]
nonews_ego_mutual <- neigh -
  E(neigh)[!is.mutual(neigh, E(neigh))]
nonews_ego_mutual <- nonews_ego_mutual - V(nonews_ego_mutual)[degree(nonews_ego_mutual)==0]


plot(nonews_ego_mutual)
save(news_ego_mutual, nonews_ego_mutual, file = "two_friend_subgraphs.RData")
