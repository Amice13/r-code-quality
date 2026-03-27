#!/usr/bin/Rscript

# Replication code for:
# Bailo, F., & Vromen, A. (2016). 
# Hybrid social and news media protest events: from #MarchinMarch to #BusttheBudget in Australia, 
# Information, Communication & Society, 1–20. https://doi.org/10.1080/1369118X.2016.1252410

# 12_create_twitter_rt_at_fship_graphs.R
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
load("twitter_stream_data_cleaned.RData")

datasets <- names(twt)

buildTwitterNetworkAT <- function (user, message) { 
  # Copyright 2010 cornelius
  # Downloaded http://blog.ynada.com/339
  
  require(igraph)
  
  # Get @-messages, senders, receivers
  ats <- grep("^\\.?@[a-z0-9_]{1,25}", tolower(message), perl=T, value=T);
  at.sender <- tolower(as.character(user[grep("^\\.?@[a-z0-9_]{1,25}", tolower(message), perl=F)]));
  at.receiver <- gsub("^\\.?@([a-z0-9_]{1,25})[^a-z0-9_]?.*$", "\\1", ats, perl=F);
  # print(paste(length(ats), " @-messages from ", length(unique(at.sender)), " senders and ", length(unique(at.receiver)), " receivers.", sep=""));
  
  # This is necessary to avoid problems with empty entries, usually caused by encoding issues in the source files
  at.sender[at.sender==""] <- "<NA>";
  at.receiver[at.receiver==""] <- "<NA>";
  
  # Create a data frame from the sender-receiver information
  ats.df <- data.frame(at.sender, at.receiver);
  
  # Transform data frame into a graph
  ats.g <- graph.data.frame(ats.df, directed=T);
  
  return(ats.g)
}

buildTwitterNetworkRT <- function (user, message) { 
  # Copyright 2010 cornelius
  # Downloaded http://blog.ynada.com/339
  
  require(igraph)
  
  # Get RTs, senders, receivers
  rts <- grep("^rt @[a-z0-9_]{1,25}", tolower(message), perl=T, value=T);
  rt.sender <- tolower(as.character(user[grep("^rt @[a-z0-9_]{1,25}", tolower(message), perl=T)]));
  rt.receiver <- gsub("^rt @([a-z0-9_]{1,25})[^a-z0-9_]?.*$", "\\1", rts, perl=F);
  # print(paste(length(rts), " RTs from ", length(unique(rt.sender)), " senders and ", length(unique(rt.receiver)), " receivers.", sep=""));
  
  # This is necessary to avoid problems with empty entries, usually caused by encoding issues in the source files
  rt.sender[rt.sender==""] <- "<NA>";
  rt.receiver[rt.receiver==""] <- "<NA>";
  
  # Create a data frame from the sender-receiver information
  rts.df <- data.frame(rt.sender, rt.receiver);
  
  # Transform data frame into a graph
  rts.g <- graph.data.frame(rts.df, directed=T);
  
  return(rts.g)
}


# This table must contains all the attributes for the analysis
user_wide_net_attributes <- data.frame(user=character())

event_labels <- c('mar14', 'may14', 'jul14', 'aug14', 'mar15a', 'mar15b')
net_labels <-  c('rt_graph', 'at_graph', 'fship_entire_graph', 'fship_mutual_graph')

user_active_by_event <- list()

# Create list of active users 
for (i in 1:length(datasets)) {
  user_active_by_event[[event_labels[i]]] <- as.character(twt[[datasets[i]]]$user$id)
  user_wide_net_attributes <- rbind(user_wide_net_attributes,
                                    data.frame(user=user_active_by_event[[event_labels[i]]]))
}
user_wide_net_attributes <- unique(user_wide_net_attributes)

wide_graphs <- list()


# Create the four subgraphs on for each event
for (i in 1:length(datasets)) {
  
  wide_graphs[[event_labels[i]]] <- list()
  
  tmp_user_tweet <- merge(twt[[datasets[i]]][['tweet']],
                          twt[[datasets[i]]][['user']][,c('id','screen_name')],
                          by.x='user_id',
                          by.y='id')
  
  wide_graphs[[event_labels[i]]][[net_labels[1]]] <-
    buildTwitterNetworkRT(tmp_user_tweet$screen_name, tmp_user_tweet$text)
  
  # Add user_id (numeric) since name is screen_name
  V(wide_graphs[[event_labels[i]]][[net_labels[1]]])$user_id <-
    tmp_user_tweet$user_id[match(V(wide_graphs[[event_labels[i]]][[net_labels[1]]])$name,
                                 tmp_user_tweet$screen_name)]
  
  wide_graphs[[event_labels[i]]][[net_labels[2]]] <-
    buildTwitterNetworkAT(tmp_user_tweet$screen_name, tmp_user_tweet$text)
  
  V(wide_graphs[[event_labels[i]]][[net_labels[2]]])$user_id <-
    tmp_user_tweet$user_id[match(V(wide_graphs[[event_labels[i]]][[net_labels[2]]])$name,
                                 tmp_user_tweet$screen_name)]
  
  wide_graphs[[event_labels[i]]][[net_labels[3]]] <-
    graph.data.frame(subset(friendlist[,1:2], from %in% user_active_by_event[[i]] &
                              to %in% user_active_by_event[[i]]),
                     directed=TRUE, vertices=NULL)
  
  wide_graphs[[event_labels[i]]][[net_labels[4]]] <-
    wide_graphs[[event_labels[i]]][[net_labels[3]]] -
    E(wide_graphs[[event_labels[i]]][[net_labels[3]]])[!is.mutual(wide_graphs[[event_labels[i]]][[net_labels[3]]],
                                                                  E(wide_graphs[[event_labels[i]]][[net_labels[3]]]))]
  
  wide_graphs[[event_labels[i]]][[net_labels[4]]] <- as.undirected(simplify(wide_graphs[[event_labels[i]]][[net_labels[4]]]))
}

for (event_label in event_labels) {
  print(event_label)
  for (net_label in net_labels) {
    print(net_label)
    V(wide_graphs[[event_label]][[net_label]])$indegree <- 
      degree(wide_graphs[[event_label]][[net_label]], mode='in')
    V(wide_graphs[[event_label]][[net_label]])$outdegree <-
      degree(wide_graphs[[event_label]][[net_label]], mode='out')
    V(wide_graphs[[event_label]][[net_label]])$closeness <-
      closeness(wide_graphs[[event_label]][[net_label]], mode='in', normalized=TRUE)
    V(wide_graphs[[event_label]][[net_label]])$betweenness <-
      betweenness(wide_graphs[[event_label]][[net_label]], directed=TRUE,
                  normalized=TRUE)
    V(wide_graphs[[event_label]][[net_label]])$pagerank <-
      page.rank(wide_graphs[[event_label]][[net_label]], directed=TRUE)[['vector']]
    
    if (net_label=='rt_graph' | net_label=='at_graph') {
      tmp_df <- data.frame(user = V(wide_graphs[[event_label]][[net_label]])$user_id,
                           indegree = V(wide_graphs[[event_label]][[net_label]])$indegree,
                           outdegree = V(wide_graphs[[event_label]][[net_label]])$outdegree,
                           closeness = V(wide_graphs[[event_label]][[net_label]])$closeness,
                           betweenness = V(wide_graphs[[event_label]][[net_label]])$betweenness,
                           pagerank = V(wide_graphs[[event_label]][[net_label]])$pagerank
      )
    } else {
      tmp_df <- data.frame(user = V(wide_graphs[[event_label]][[net_label]])$name,
                           indegree = V(wide_graphs[[event_label]][[net_label]])$indegree,
                           outdegree = V(wide_graphs[[event_label]][[net_label]])$outdegree,
                           closeness = V(wide_graphs[[event_label]][[net_label]])$closeness,
                           betweenness = V(wide_graphs[[event_label]][[net_label]])$betweenness,
                           pagerank = V(wide_graphs[[event_label]][[net_label]])$pagerank
      )
    }
    
    names(tmp_df) <- paste(names(tmp_df),event_label, net_label, sep='_')
    
    user_wide_net_attributes <- merge(user_wide_net_attributes, tmp_df,
                                      by.x='user',
                                      by.y=paste('user',event_label, net_label, sep='_'),
                                      all.x=TRUE)
    
  }
}

save(wide_graphs, user_wide_net_attributes, file="twitter_rt_at_fship_graphs.RData")



