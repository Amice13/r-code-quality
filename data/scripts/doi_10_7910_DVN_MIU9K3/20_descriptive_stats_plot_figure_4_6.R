#!/usr/bin/Rscript

# Replication code for:
# Bailo, F., & Vromen, A. (2016). 
# Hybrid social and news media protest events: from #MarchinMarch to #BusttheBudget in Australia, 
# Information, Communication & Society, 1–20. https://doi.org/10.1080/1369118X.2016.1252410

# 19_plot_figure_4_and_6.R
## Updated: 2016-11-12 12:51:53 AEDT

setwd('~/Documents/replication_packages/ics_2016')

# Wide net activity
load("twitter_rt_at_fship_graphs.RData")
load("twitter_user_labelling_attributes.RData")
load("twitter_user_ego_network.RData")
load("two_friend_subgraphs.RData")
load("twitter_public_lists.RData")
load("twitter_stream_data_cleaned_and_anonymised.RData")

# Ratio tweet/retweet
for (event in names(twt)) {
  print(sum(grepl("^rt\\b", twt[[event]][['tweet']]$text, ignore.case = T)) / nrow(twt[[event]][['tweet']]))
}

# ratio entire network reciprocated network
col_sizes <- which(grepl("entire_size", names(user_ego_net_attributes)))

for (n in col_sizes) {
  print(mean((user_ego_net_attributes[[n+5]]-1) / (user_ego_net_attributes[[n]]-1), na.rm=T))
}


# Journalists characterstics
load("~/Desktop/r_work_directory/03_user_and_friends_labelling.RData")
journos <- subset(user_labelling_attributes, user_news == TRUE)$user
entire_size_cols <- 
  names(user_ego_net_attributes)[grepl("entire_size", names(user_ego_net_attributes))]
entire_size_mutual_cols <- 
  names(user_ego_net_attributes)[grepl("mutual_size", names(user_ego_net_attributes))]
median(as.matrix(user_ego_net_attributes[,entire_size_cols]), na.rm=TRUE)
# [1] 26
median(as.matrix(user_ego_net_attributes[user_ego_net_attributes$user %in% journos, entire_size_cols]), na.rm=TRUE)
# [1] 80

# Mutual
median(as.matrix(user_ego_net_attributes[,entire_size_mutual_cols]), na.rm=TRUE)
# [1] 26
median(as.matrix(user_ego_net_attributes[user_ego_net_attributes$user %in% journos, entire_size_mutual_cols]), na.rm=TRUE)
# [1] 78

t.test(as.numeric(as.matrix(user_ego_net_attributes[user_ego_net_attributes$user %in% journos, entire_size_mutual_cols])),
       as.numeric(as.matrix(user_ego_net_attributes[!(user_ego_net_attributes$user %in% journos), entire_size_mutual_cols])))

# Construct plot
mutual_density_cols <- 
  names(user_ego_net_attributes)[grepl("graph_mutual_density", names(user_ego_net_attributes))]
median(as.matrix(user_ego_net_attributes[,mutual_density_cols]), na.rm=TRUE)
# [1] 0.2976205
median(as.matrix(user_ego_net_attributes[user_ego_net_attributes$user %in% journos, mutual_density_cols]), na.rm=TRUE)
# [1] 0.1849271

mutual_size_cols <- 
  names(user_ego_net_attributes)[grepl("graph_mutual_size", names(user_ego_net_attributes))]
mutual_cluster_coeff_cols <- 
  names(user_ego_net_attributes)[grepl("mutual_cluster_coeff", names(user_ego_net_attributes))]
cor_regular <- 
  data.frame(size = as.numeric(as.matrix(user_ego_net_attributes[,mutual_size_cols])),
             density = as.numeric(as.matrix(user_ego_net_attributes[,mutual_density_cols])),
             cluster_coeff = as.numeric(as.matrix(user_ego_net_attributes[,mutual_cluster_coeff_cols])),
             user = rep(user_ego_net_attributes$user, 6),
             event = c(rep("1",26819), rep("2",26819), rep("3",26819), rep("4",26819), rep("5",26819),
                       rep("6",26819)))
cor_regular$news <- cor_regular$user %in% journos

# Find for sample
sample_regular <- subset(cor_regular, news == FALSE & size == 26 & 
                           density > 0.29 & density < 0.3)
sample_news <- subset(cor_regular, news == TRUE & size == 78 & 
                        density > 0.18 & density < 0.19)


# Create plots (news ego = 17384075, nonews ego = 158347224)
load("two_friend_subgraphs.RData")

weightCommunity <- function(row, membership, weigth_within, weight_between) {
  if(as.numeric(membership[which(names(membership)==row[1])]) == as.numeric(membership[which(names(membership)==row[2])])){
    weight=weigth_within
  }else{
    weight=weight_between
  }
  return(weight)
}

require(igraph)
news_example_g <- as.undirected(simplify(news_ego_mutual))
news_example_g <- news_example_g - V(news_example_g)[V(news_example_g)$name == 17384075]
# news_example_g_communities <- leading.eigenvector.community(news_example_g)
news_example_g_communities <- edge.betweenness.community(news_example_g)

graph.density(news_example_g)

E(news_example_g)$weight <- apply(get.edgelist(news_example_g), 1, 
                                  weightCommunity, membership(news_example_g_communities), 100, 1)
lo <- layout_with_fr(news_example_g, weights=E(news_example_g)$weight)

## FIGURE 4a (communities and layout might change due to stochastic function)
par(mar=c(0,1,0,1))
plot(news_example_g_communities, news_example_g,
     layout = lo,
     vertex.label = NA, vertex.size = 4, edge.width = .2)

nonews_example_g <- as.undirected(simplify(nonews_ego_mutual))
nonews_example_g <- nonews_example_g - V(nonews_example_g)[V(nonews_example_g)$name == 158347224]
nonews_example_g_communities <- edge.betweenness.community(nonews_example_g)
# nonews_example_g_communities <- infomap.community(nonews_example_g)



E(nonews_example_g)$weight <- apply(get.edgelist(nonews_example_g), 1, 
                                    weightCommunity, membership(nonews_example_g_communities), 100, 1)
lo <- layout_with_fr(nonews_example_g, weights=E(nonews_example_g)$weight)

## FIGURE 4b (communities and layout might change due to stochastic function)
par(mar=c(0,1,0,1))
plot(nonews_example_g_communities, nonews_example_g,
     layout = lo,
     vertex.label = NA, vertex.size = 4, edge.width = .2)


# Does jounrnalists have less dense networks?
mod <- lm(density ~ log10(size) + news, data = cor_regular)
predict(mod, data.frame(size = 50, news = FALSE), type = "response")

cor_news <- 
  data.frame(size = as.numeric(as.matrix(user_ego_net_attributes[user_ego_net_attributes$user %in% journos,mutual_size_cols])),
             density = as.numeric(as.matrix(user_ego_net_attributes[user_ego_net_attributes$user %in% journos,mutual_density_cols])))

ggplot() + 
  stat_smooth(data = cor_regular, aes(size, density), method='lm', colour = 'red') + 
  stat_smooth(data = cor_news, aes(size, density), method='lm', colour = 'blue') + 
  scale_x_log10()


require(ggplot2)
ggplot() + 
  geom_density(data = data.frame(x = as.numeric(as.matrix(user_ego_net_attributes[,entire_size_cols]))),
               aes(x), colour = 'red', adjust = 2) +
  geom_density(data = data.frame(x = as.numeric(as.matrix(user_ego_net_attributes[user_ego_net_attributes$user %in% journos, 
                                                                                  entire_size_cols]))),
               aes(x), colour = 'blue', adjust = 2) +
  scale_x_log10()
ggplot() + 
  geom_density(data = data.frame(x = as.numeric(as.matrix(user_ego_net_attributes[,entire_size_mutual_cols]))),
               aes(x), colour = 'red', adjust = 2) +
  geom_density(data = data.frame(x = as.numeric(as.matrix(user_ego_net_attributes[user_ego_net_attributes$user %in% journos, 
                                                                                  entire_size_mutual_cols]))),
               aes(x), colour = 'blue', adjust = 2) +
  scale_x_log10()


entire_density_cols <- 
  names(user_ego_net_attributes)[grepl("entire_density", names(user_ego_net_attributes))]
median(as.matrix(user_ego_net_attributes[,entire_density_cols]), na.rm=TRUE)
median(as.matrix(user_ego_net_attributes[user_ego_net_attributes$user %in% journos, entire_density_cols]), na.rm=TRUE)


# Cluster coefficient of ego-networks

mutual_cluster_coeff_cols_nonews <- 
  as.numeric(as.matrix(user_ego_net_attributes[!(user_ego_net_attributes$user %in% journos), mutual_cluster_coeff_cols]))

mutual_cluster_coeff_cols_news <-
  as.numeric(as.matrix(user_ego_net_attributes[user_ego_net_attributes$user %in% journos, mutual_cluster_coeff_cols]))


median(mutual_cluster_coeff_cols_nonews, na.rm=TRUE)
# [1] 0.48
mean(mutual_cluster_coeff_cols_nonews, na.rm=TRUE)
# [1] 0.4651543

median(mutual_cluster_coeff_cols_news, na.rm=TRUE)
# [1] 0.3828755
mean(mutual_cluster_coeff_cols_news, na.rm=TRUE)
# [1] 0.3903297


t.test(mutual_cluster_coeff_cols_nonews, mutual_cluster_coeff_cols_news)

mod <- lm(cluster_coeff ~ log10(size) + news, data = cor_regular)
predict(mod, data.frame(size = 50, news = TRUE), type = "response")
predict(mod, data.frame(size = 50, news = FALSE), type = "response")

##

# Compare journos, auspol, news, union
nets <- c("_rt_","_entire_")
stats <- c("indegree","closeness","pagerank")
events <- c("mar14","may14","jul14","aug14","mar15a","mar15b")

category_stats <- list()

for (event in events) {
  print(event)
  # category_stats[[event]][[net]] <- list()
  for (net in nets) {
    print(net)
    category_stats[[event]][[net]] <- list()
    for (stat in stats) {
      print(stat)
      col_names <- colnames(user_wide_net_attributes)[grepl(event, colnames(user_wide_net_attributes))]
      col_names <- col_names[grepl(net, col_names)]
      col_names <- col_names[grepl(stat, col_names)]
      
      news_df <- 
        data.frame(group = "news",
                   value = as.vector(as.matrix(subset(user_wide_net_attributes, select = col_names,
                                                      user %in% macro_sets[['news']]))),
                   event = event)
      auspol_df <- 
        data.frame(group = "auspol",
                   value = as.vector(as.matrix(subset(user_wide_net_attributes, select = col_names, 
                                                      user %in% macro_sets[['auspol']]))),
                   event = event)
      
      union_df <- 
        data.frame(group = "union",
                   value = as.vector(as.matrix(subset(user_wide_net_attributes, select = col_names, 
                                                      user %in% macro_sets[['union']]))),
                   event = event)
      
      socialmov_df <- 
        data.frame(group = "social-mov",
                   value = as.vector(as.matrix(subset(user_wide_net_attributes, select = col_names, 
                                                      user %in% macro_sets[['social-mov']]))),
                   event = event)
      
      category_stats[[event]][[net]][[stat]] <- rbind(news_df, auspol_df, union_df, socialmov_df)
      
    }
  }
}

rt_indegree <- data.frame()
for(event in events) {
  rt_indegree <- rbind(rt_indegree, category_stats[[event]][['_rt_']][['indegree']])
}
entire_indegree <- data.frame()
for(event in events) {
  entire_indegree <- rbind(entire_indegree, category_stats[[event]][['_entire_']][['indegree']])
}

library(ggplot2)
require(dplyr)
rt_indegree <-
  rt_indegree %>%
  dplyr::group_by(group, event) %>%
  dplyr::summarize(sum = sum(value, na.rm=T))
entire_indegree <-
  entire_indegree %>%
  dplyr::group_by(group, event) %>%
  dplyr::summarize(sum = sum(value, na.rm=T))

rt_indegree$event <- factor(rt_indegree$event, levels = events) 
entire_indegree$event <- factor(entire_indegree$event, levels = events) 

g1 <- ggplot(entire_indegree, aes(event, sum)) +   
  geom_bar(aes(fill = group), position = "dodge", stat="identity") + labs(x="Friendship indegree", y=NULL) + theme_bw() +
  theme(axis.text.y=element_blank(), axis.ticks.y=element_blank())
g2 <- ggplot(rt_indegree, aes(event, sum)) +   
  geom_bar(aes(fill = group), position = "dodge", stat="identity") + labs(x="Retweet", y="Indegree sum") + theme_bw()

require(reshape2)
tot_tweets <- data.frame()
for (event in names(twt)) {
  tot_tweets <- rbind(tot_tweets, data.frame(event = event, 
                                             tweets = nrow(twt[[event]][['tweet']]),
                                             profile = nrow(twt[[event]][['user']])))
}
tot_tweets$tweets <-  tot_tweets$tweets / sum(tot_tweets$tweets)
tot_tweets$profile <-  tot_tweets$profile / sum(tot_tweets$profile)
tot_tweets <- melt(tot_tweets)
tot_tweets$event <- factor(tot_tweets$event, levels = names(twt))

require(plyr)
tot_tweets$event <- mapvalues(tot_tweets$event, from = levels(tot_tweets$event), to = events)

g3 <- ggplot(tot_tweets, aes(event, value)) +   
  geom_bar(aes(fill = variable), position = "dodge", stat="identity") + labs(x = "Tweets and user presence", y = NULL) + theme_bw() +
  theme(axis.text.y=element_blank(), axis.ticks.y=element_blank()) +
  scale_fill_manual(values=c("black","gray"))

require(gridExtra)
grid.arrange(g3,g1,nrow=2)

rt_closeness <-
  category_stats[['_rt_']][['closeness']] %>%
  dplyr::group_by(group) %>%
  dplyr::summarize(sum = sum(value, na.rm=T))
entire_closeness <-
  category_stats[['_entire_']][['closeness']] %>%
  dplyr::group_by(group) %>%
  dplyr::summarize(sum = sum(value, na.rm=T))

ggplot(category_stats[['_rt_']][['indegree']], aes(x=group, y=value)) + 
  geom_bar(stat="sum")


plot(density(user_labelling_attributes$friends_news / user_labelling_attributes$n_friends, na.rm = T))

reciprocity_cols <- 
  names(user_ego_net_attributes)[grepl("entire_reciprocity", names(user_ego_net_attributes))]

reciprocity_df <-
  user_ego_net_attributes[,c("user",reciprocity_cols)]
require(reshape2)
reciprocity_df <- melt(reciprocity_df, id.vars = "user")
reciprocity_df <- reciprocity_df[complete.cases(reciprocity_df),]

require(ggplot2)
ggplot(reciprocity_df, aes(value, group = variable, colour = variable)) + geom_density() +
  theme_bw()

geodist_entire_cols <- 
  names(user_ego_net_attributes)[grepl("entire_geodist", names(user_ego_net_attributes))]
geodist_mutual_cols <- 
  names(user_ego_net_attributes)[grepl("mutual_geodist", names(user_ego_net_attributes))]

geodist_entire_df <-
  user_ego_net_attributes[,c("user",geodist_entire_cols)]
require(reshape2)
geodist_entire_df <- melt(geodist_entire_df, id.vars = "user")
geodist_entire_df <- geodist_entire_df[complete.cases(geodist_entire_df),]

ggplot(geodist_entire_df, aes(value, group = variable, colour = variable)) + geom_density() +
  theme_bw()

geodist_mutual_df <-
  user_ego_net_attributes[,c("user",geodist_mutual_cols)]
require(reshape2)
geodist_mutual_df <- melt(geodist_mutual_df, id.vars = "user")
geodist_mutual_df <- geodist_mutual_df[complete.cases(geodist_mutual_df),]

ggplot(geodist_mutual_df, aes(value, group = variable, colour = variable)) + geom_density() +
  theme_bw()

mean(geodist_entire_df$value) - mean(geodist_mutual_df$value)

for (n in names(twt)) {
  print(n)
  print(with(twt[[n]][['user']], screen_name[which.max(followers_count / mean(friends_count))]))
}

# Compare indegree across networks
indegree_col <- names(user_wide_net_attributes)[grepl("indegree",names(user_wide_net_attributes))]

cor.mtest <- function(mat, conf.level = 0.95) {
  mat <- as.matrix(mat)
  n <- ncol(mat)
  p.mat <- lowCI.mat <- uppCI.mat <- matrix(NA, n, n)
  diag(p.mat) <- 0
  diag(lowCI.mat) <- diag(uppCI.mat) <- 1
  for (i in 1:(n - 1)) {
    for (j in (i + 1):n) {
      tmp <- cor.test(mat[, i], mat[, j], conf.level = conf.level)
      p.mat[i, j] <- p.mat[j, i] <- tmp$p.value
      lowCI.mat[i, j] <- lowCI.mat[j, i] <- tmp$conf.int[1]
      uppCI.mat[i, j] <- uppCI.mat[j, i] <- tmp$conf.int[2]
    }
  }
  return(list(p.mat, lowCI.mat, uppCI.mat))
}

for (n in 0:5) {
  print(names(twt)[n+1])
  s <- c(1,3,4) + (n * 4)
  # print(n)
  #   print(cor.test(user_wide_net_attributes[[indegree_col[s[1]]]], 
  #                  user_wide_net_attributes[[indegree_col[s[2]]]]))
  print(cor.test(user_wide_net_attributes[[indegree_col[s[1]]]],
                 user_wide_net_attributes[[indegree_col[s[3]]]]))
  #   print(cor(user_wide_net_attributes[[indegree_col[s[1]]]],
  #             user_wide_net_attributes[[indegree_col[s[3]]]], use = "pairwise.complete.obs") /
  #           cor(user_wide_net_attributes[[indegree_col[s[1]]]], 
  #               user_wide_net_attributes[[indegree_col[s[2]]]], use = "pairwise.complete.obs"))
}


## Plot sample graphs
require(igraph)

missing_edges_to_density <- function(g, target_density) {
  v <- vcount(g)
  e <- ecount(g)
  efull <- (v*(v-1))/2
  current_density <- e / efull
  missing_e <- round(( target_density * efull ) - e, digits=0)
  return(missing_e)
}

regular_v <- c("ego", as.character(1:26))
regular_el <- data.frame(from = rep("ego",26),
                         to = as.character(1:26))
regular_g <- graph_from_data_frame(regular_el, directed = FALSE)

regular_missing_e <- missing_edges_to_density(regular_g, 0.2976205)

full_neigh <- t(combn(as.character(1:26), 2))
colnames(full_neigh) <- c("from","to")
regular_el <- rbind(regular_el, full_neigh[sample(1:nrow(full_neigh), regular_missing_e),])

regular_g <- graph_from_data_frame(regular_el, directed = FALSE)

news_v <- c("ego", as.character(1:78))
news_el <- data.frame(from = rep("ego",78),
                      to = as.character(1:78))
news_g <- graph_from_data_frame(news_el, directed = FALSE)

news_missing_e <- missing_edges_to_density(news_g, 0.1849271)

full_neigh <- t(combn(as.character(1:78), 2))
colnames(full_neigh) <- c("from","to")
news_el <- rbind(news_el, full_neigh[sample(1:nrow(full_neigh), news_missing_e),])
news_g <- graph_from_data_frame(news_el, directed = FALSE)

plot(news_g, layout = layout.fruchterman.reingold(news_g), 
     vertex.label = NA, vertex.size = 4)

# FIGURE 6

## Friendship
closeness_cols <- 
  names(user_wide_net_attributes)[grepl("closeness", names(user_wide_net_attributes)) &
                                    grepl("fship_entire", names(user_wide_net_attributes))]
betweenness_cols <- 
  names(user_wide_net_attributes)[grepl("betweenness", names(user_wide_net_attributes)) &
                                    grepl("fship_entire", names(user_wide_net_attributes))] 
pagerank_cols <- 
  names(user_wide_net_attributes)[grepl("pagerank", names(user_wide_net_attributes)) &
                                    grepl("fship_entire", names(user_wide_net_attributes))] 


net_enabled <- c('mar14','may14','aug14','mar15b')

org_enabled <- c('jul14','mar15a')

contains <- function(x, regex) {x[grepl(paste(regex, collapse="|"), x)]}

centrality_fship_df <- data.frame()

for (event in c(net_enabled, org_enabled)) {
  
  ratio <- mean(as.numeric(as.matrix(user_wide_net_attributes[user_wide_net_attributes$user 
                                                              %in% journos, contains(betweenness_cols, event)])), na.rm=T) / 
    mean(as.numeric(as.matrix(user_wide_net_attributes[, contains(betweenness_cols, event)])), na.rm=T)
  
  centrality_fship_df <- rbind(centrality_fship_df, 
                               data.frame(event = event,
                                          measure = "Betweenness",
                                          news_all_ratio = ratio))
  
  ratio <- mean(as.numeric(as.matrix(user_wide_net_attributes[user_wide_net_attributes$user 
                                                              %in% journos, contains(pagerank_cols, event)])), na.rm=T) / 
    mean(as.numeric(as.matrix(user_wide_net_attributes[, contains(pagerank_cols, event)])), na.rm=T)
  
  centrality_fship_df <- rbind(centrality_fship_df, 
                               data.frame(event = event,
                                          measure = "PageRank",
                                          news_all_ratio = ratio))
}

centrality_fship_df$event <- factor(centrality_fship_df$event, levels=c('mar14', 'may14',
                                                                        'jul14', 'aug14',
                                                                        'mar15a',
                                                                        'mar15b'))
require(dplyr)
centrality_fship_meanevent_df <- 
  centrality_fship_df %>%
  dplyr::group_by(event) %>%
  dplyr::summarize(mean_ratio = mean(news_all_ratio))

ggplot() +
    geom_bar(data = centrality_fship_df, aes(x=event, y=news_all_ratio, fill = measure), 
             position = "dodge", stat="identity") +
    geom_point(data = centrality_fship_meanevent_df,
               aes(x=event, y = mean_ratio)) +
    geom_line(data = centrality_fship_meanevent_df,
              aes(x=event, y = mean_ratio, group = 1)) +
    geom_hline(yintercept = 1, linetype = 'dashed') +
    scale_y_continuous(breaks = c(0,1,2.5,5,7.5,10,12.5)) +
    labs(x=NULL, y="Mean news node value / mean node value") + theme_bw()


## 

# t.test
for (event in c(net_enabled, org_enabled)) { 
  print(event)
  print(t.test(as.numeric(as.matrix(user_wide_net_attributes[user_wide_net_attributes$user 
                                                             %in% journos, contains(betweenness_cols, event)])),
               as.numeric(as.matrix(user_wide_net_attributes[!(user_wide_net_attributes$user 
                                                               %in% journos), contains(betweenness_cols, event)]))))
}
for (event in c(net_enabled, org_enabled)) { 
  print(event)
  print(t.test(as.numeric(as.matrix(user_wide_net_attributes[user_wide_net_attributes$user 
                                                             %in% journos, contains(pagerank_cols, event)])),
               as.numeric(as.matrix(user_wide_net_attributes[!(user_wide_net_attributes$user 
                                                               %in% journos), contains(pagerank_cols, event)]))))
}


# Retweet
closeness_cols <- 
  names(user_wide_net_attributes)[grepl("closeness", names(user_wide_net_attributes)) &
                                    grepl("rt_", names(user_wide_net_attributes))]
betweenness_cols <- 
  names(user_wide_net_attributes)[grepl("betweenness", names(user_wide_net_attributes)) &
                                    grepl("rt_", names(user_wide_net_attributes))] 
pagerank_cols <- 
  names(user_wide_net_attributes)[grepl("pagerank", names(user_wide_net_attributes)) &
                                    grepl("rt_", names(user_wide_net_attributes))] 


centrality_rt_df <- data.frame()

for (event in c(net_enabled, org_enabled)) {
  
  ratio <- mean(as.numeric(as.matrix(user_wide_net_attributes[user_wide_net_attributes$user 
                                                              %in% journos, contains(betweenness_cols, event)])), na.rm=T) / 
    mean(as.numeric(as.matrix(user_wide_net_attributes[, contains(betweenness_cols, event)])), na.rm=T)
  
  centrality_rt_df <- rbind(centrality_rt_df, 
                            data.frame(event = event,
                                       measure = "betweenness",
                                       news_all_ratio = ratio))
  
  ratio <- mean(as.numeric(as.matrix(user_wide_net_attributes[user_wide_net_attributes$user 
                                                              %in% journos, contains(pagerank_cols, event)])), na.rm=T) / 
    mean(as.numeric(as.matrix(user_wide_net_attributes[, contains(pagerank_cols, event)])), na.rm=T)
  
  centrality_rt_df <- rbind(centrality_rt_df, 
                            data.frame(event = event,
                                       measure = "PageRank",
                                       news_all_ratio = ratio))
}

centrality_rt_df$event <- factor(centrality_rt_df$event, levels=c('mar14', 'may14',
                                                                  'jul14', 'aug14',
                                                                  'mar15a',
                                                                  'mar15b'))

ggplot(centrality_rt_df, aes(x=event, y=news_all_ratio)) +
  geom_bar(aes(fill = measure), position = "dodge", stat="identity")
