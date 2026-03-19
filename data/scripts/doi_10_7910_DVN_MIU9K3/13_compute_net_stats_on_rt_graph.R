#!/usr/bin/Rscript

# Replication code for:
# Bailo, F., & Vromen, A. (2016). 
# Hybrid social and news media protest events: from #MarchinMarch to #BusttheBudget in Australia, 
# Information, Communication & Society, 1–20. https://doi.org/10.1080/1369118X.2016.1252410

# 13_compute_net_stats_on_rt_graph.R
## Updated: 2016-11-12 16:26:10 AEDT

setwd('~/Documents/replication_packages/ics_2016')

load('twitter_rt_at_fship_graphs.RData')
load("twitter_public_lists.RData")

# Retweet graph
# Prepare statistics
sizes_rt <- numeric()
clusters_rt <- numeric()

for (event_label in names(wide_graphs)) {
  print(event_label)
  sizes_rt <- c(sizes_rt,
                vcount(wide_graphs[[event_label]][['rt_graph']]))
  clusters_rt <- c(clusters_rt, 
                   no.clusters(wide_graphs[[event_label]][['rt_graph']], mode='weak'))
}

clusters_no_news_rt <- numeric()
for (event_label in names(wide_graphs)) {
  print(event_label)
  # remove news
  g <- wide_graphs[[event_label]][['rt_graph']]
  g <- g - V(g)[V(g)$user_id %in% macro_sets[['news']]]
  clusters_no_news_rt <- c(clusters_no_news_rt, 
                           no.clusters(g, mode='weak')) 
}

save(sizes_rt, clusters_rt, clusters_no_news_rt, file='net_stats_on_rt_graph.RData')
