#!/usr/bin/Rscript

# Replication code for:
# Bailo, F., & Vromen, A. (2016). 
# Hybrid social and news media protest events: from #MarchinMarch to #BusttheBudget in Australia, 
# Information, Communication & Society, 1–20. https://doi.org/10.1080/1369118X.2016.1252410

# 09_create_twitter_friend_graphs.R
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

# Create user lists
load("twitter_stream_data_cleaned_and_anonymised.RData")

load("twitter_public_lists.RData")

event_labels <- c('mar14', 'may14', 'jul14', 'aug14', 'mar15a', 'mar15b')

datasets <- names(twt)

user_active_by_event <- list()

# Create list of active users 
for (i in 1:length(datasets)) {
  user_active_by_event[[event_labels[i]]] <- as.character(twt[[datasets[i]]]$user$id)
}

wide_graphs_wt_friends <- list()

for (i in 1:length(datasets)) {
  wide_graphs_wt_friends[[event_labels[i]]][['directed']] <-
    graph.data.frame(subset(friendlist[,1:2], from %in% user_active_by_event[[i]]),
                     directed=TRUE, vertices=NULL)
  
  wide_graphs_wt_friends[[event_labels[i]]][['undirected']] <-
    wide_graphs_wt_friends[[event_labels[i]]][['directed']] -
    E(wide_graphs_wt_friends[[event_labels[i]]][['directed']])[!is.mutual(wide_graphs_wt_friends[[event_labels[i]]][['directed']],
                                                                          E(wide_graphs_wt_friends[[event_labels[i]]][['directed']]))]
  
  wide_graphs_wt_friends[[event_labels[i]]][['undirected']] <- as.undirected(simplify(wide_graphs_wt_friends[[event_labels[i]]][['undirected']]))
}

save(wide_graphs_wt_friends, file="twitter_friend_graphs.RData")

for (event_label in event_labels) {
  print(event_label)
  for (net_label in c('undirected','directed')) {
    if (net_label=='directed') {
      print(net_label)
      V(wide_graphs_wt_friends[[event_label]][[net_label]])$indegree <- 
        degree(wide_graphs_wt_friends[[event_label]][[net_label]], mode='in')
      V(wide_graphs_wt_friends[[event_label]][[net_label]])$outdegree <-
        degree(wide_graphs_wt_friends[[event_label]][[net_label]], mode='out')
    } else {
      V(wide_graphs_wt_friends[[event_label]][[net_label]])$degree <- 
        degree(wide_graphs_wt_friends[[event_label]][[net_label]])
    }
  }
}

for (event_label in event_labels) {
  print(event_label)
  for (net_label in c('undirected','directed')) {
    print(net_label)
    #Sample
    V(wide_graphs_wt_friends[[event_label]][[net_label]])$closeness.sample <- 
      rep(NA, vcount(wide_graphs_wt_friends[[event_label]][[net_label]]))
    
    smpl <- sample(1:vcount(wide_graphs_wt_friends[[event_label]][[net_label]]), 10000)
    V(wide_graphs_wt_friends[[event_label]][[net_label]])$closeness.sample[smpl] <- 
      closeness(wide_graphs_wt_friends[[event_label]][[net_label]], 
                vids=V(wide_graphs_wt_friends[[event_label]][[net_label]])[smpl], mode='in')
    
    V(wide_graphs_wt_friends[[event_label]][[net_label]])$closeness.news <- 
      rep(NA, vcount(wide_graphs_wt_friends[[event_label]][[net_label]]))
    
    news_vert <- V(wide_graphs_wt_friends[[event_label]][[net_label]])$name %in% 
      macro_sets[['news']]

    V(wide_graphs_wt_friends[[event_label]][[net_label]])$closeness.news[news_vert] <- 
      closeness(wide_graphs_wt_friends[[event_label]][[net_label]], 
                vids=V(wide_graphs_wt_friends[[event_label]][[net_label]])[news_vert], mode='in')
  }
}

save(wide_graphs_wt_friends, file="twitter_friend_graphs.RData")


# Prepare statistics
sizes <- numeric()
clusters <- numeric()

for (event_label in event_labels) {
  print(event_label)
  for (net_label in c('directed')) {
    sizes <- c(sizes,
              vcount(wide_graphs_wt_friends[[event_label]][[net_label]]))
    clusters <- c(clusters, 
                  no.clusters(wide_graphs_wt_friends[[event_label]][[net_label]], mode='weak'))
  }
}

clusters_no_news <- numeric()
for (event_label in event_labels) {
  print(event_label)
  for (net_label in c('directed')) {
    # remove news
    g <- wide_graphs_wt_friends[[event_label]][[net_label]]
    g <- g - V(g)[V(g)$name %in% macro_sets[['news']]]
    clusters_no_news <- c(clusters_no_news, 
                          no.clusters(g, mode='weak'))
    
  }
}

closeness_news <- list()
closeness_sample <- list()
for (event_label in event_labels) {
  print(event_label)
  for (net_label in c('directed')) {
    # remove news
    closeness_news[[event_label]] <- 
      V(wide_graphs_wt_friends[[event_label]][[net_label]])$closeness.news 
    closeness_sample[[event_label]] <- 
      V(wide_graphs_wt_friends[[event_label]][[net_label]])$closeness.sample 
  }
}
save(sizes, clusters, clusters_no_news, closeness_news, closeness_sample, file="twitter_friend_graphs_sum_stats.RData")


