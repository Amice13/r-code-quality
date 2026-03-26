#!/usr/bin/Rscript

# Replication code for:
# Bailo, F., & Vromen, A. (2016). 
# Hybrid social and news media protest events: from #MarchinMarch to #BusttheBudget in Australia, 
# Information, Communication & Society, 1–20. https://doi.org/10.1080/1369118X.2016.1252410

# 18_twitter_user_ego_network.R
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
user_ego_net_attributes <- unique(user_ego_net_attributes)

# Subset to create 6 graphs one for each event including only active participants

subgraphs <- list()

for (i in 1:length(datasets)) {
  subgraphs[[paste0('g_', event_labels[i])]] <-
    graph.data.frame(subset(friendlist[,1:2], from %in% user_active_by_event[[i]] &
                              to %in% user_active_by_event[[i]]),
                     directed=TRUE, vertices=NULL)
}


# Extract egonetworks and foreach calculate (NA if user not in the dataset)
library(igraph)
for (i in 1:length(datasets)) {
  
  event_df <- data.frame()
  
  vids <- V(subgraphs[[i]])$name
  
  for (vid in vids) {
    #print(paste(event_labels[i], vid, sep="_"))
    ego_df <- data.frame(user = vid,
                         graph_entire_size = NA,
                         graph_entire_density = NA,
                         graph_entire_reciprocity = NA,
                         graph_entire_cluster_coeff = NA,
                         graph_mutual_size = NA,
                         graph_mutual_density = NA,
                         graph_mutual_reciprocity = NA, 
                         graph_mutual_cluster_coeff = NA)
    
    ego_entire_g <- graph.neighborhood(subgraphs[[i]], 1, vid, mode="out")[[1]]
    
    if (vcount(ego_entire_g)==1) {
      ego_df[1,'graph_entire_size'] <- vcount(ego_entire_g)
      event_df <- rbind(event_df, ego_df)
      next
    } else {
      # Fill dataframe
      ego_df[1,'graph_entire_size'] <- vcount(ego_entire_g)
      ego_df[1,'graph_entire_density'] <- graph.density(simplify(ego_entire_g))
      ego_df[1,'graph_entire_reciprocity'] <- reciprocity(ego_entire_g)
      ego_df[1,'graph_entire_cluster_coeff'] <- transitivity(simplify(ego_entire_g), type="global")

      ego_mutual_g <- ego_entire_g -
        E(ego_entire_g)[!is.mutual(ego_entire_g, E(ego_entire_g))]
      ego_mutual_g <- ego_mutual_g - V(ego_mutual_g)[degree(ego_mutual_g)
                                                     == 0]
      
      if (vcount(ego_mutual_g)==0) {
        ego_df[1,'graph_mutual_size'] <- vcount(ego_mutual_g)
        event_df <- rbind(event_df, ego_df)
        next
      } else {
        ego_df[1,'graph_mutual_size'] <- vcount(ego_mutual_g)
        ego_df[1,'graph_mutual_density'] <- graph.density(simplify(ego_mutual_g))
        ego_df[1,'graph_mutual_reciprocity'] <- reciprocity(ego_mutual_g)
        ego_df[1,'graph_mutual_cluster_coeff'] <- transitivity(simplify(ego_mutual_g), type="global")
      }
      
      
    }
    event_df <- rbind(event_df, ego_df)
  }
  names(event_df) <- paste(names(event_df), event_labels[i], sep='_') 
  user_ego_net_attributes <- merge(user_ego_net_attributes, event_df,
                                   by.x='user',
                                   by.y=paste('user', event_labels[i], sep='_'),
                                   all.x=TRUE)
}

save(user_ego_net_attributes, file="twitter_user_ego_network.RData")
