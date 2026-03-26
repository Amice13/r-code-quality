#!/usr/bin/Rscript

# Replication code for:
# Bailo, F., & Vromen, A. (2016). 
# Hybrid social and news media protest events: from #MarchinMarch to #BusttheBudget in Australia, 
# Information, Communication & Society, 1–20. https://doi.org/10.1080/1369118X.2016.1252410

# 14_sim_v_remove_on_rt_graph.R
## Updated: 2016-11-12 16:26:10 AEDT

setwd('~/Documents/replication_packages/ics_2016')

load('twitter_rt_at_fship_graphs.RData')
load("twitter_public_lists.RData")

clusters_sim_rt<- list()
for (event_label in names(wide_graphs)) {
  clusters_sim_rt[[event_label]] <- numeric()
}

for (i in 1:1000){
  print(i)
  for (event_label in names(wide_graphs)) {
    # remove news
    g <- wide_graphs[[event_label]][['rt_graph']]
    n_edges_to_remove <- sum(V(g)$user_id %in% macro_sets[['news']])
    g <- g - sample(V(g), n_edges_to_remove)
    clusters_sim_rt[[event_label]] <- c(clusters_sim_rt[[event_label]], 
                                        no.clusters(g, mode='weak'))   
  }
}
save(clusters_sim_rt, file="sim_v_remove_on_rt_graph.RData")
