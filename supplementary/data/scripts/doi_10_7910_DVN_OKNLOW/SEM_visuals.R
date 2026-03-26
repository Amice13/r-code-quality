library(tidyverse)
library(igraph)
library(intergraph)
library(scales)
library(statnet)
library(Matrix)
library(ggnetwork)
options(scipen=999)

setwd("~/Projects/phd/research/seo_update/")

rd_data_el <- './ora_backlinks.csv'
output_dir <- './'

## helper functions
addUnits <- function(n){
  # units on plot to 'k' or 'M'
  labels <- ifelse(n < 1000, n,
                   ifelse(n <1e6, paste0(round(n/1e3), 'k'),
                          ifelse(n<1e9, paste0(round(n/1e6), 'M'),
                                 'too big!')))
  return(labels)
}

min_overlap <- function(df){
  # creates minimum overlap matrix- see paper for details
  # did not implement this efficiently... you should rewrite
  # for larger networks
  web_adj <- df %>% 
    # convert edge list to adjacency matrix
    select(source, target, weight) %>% 
    pivot_wider(names_from = source, values_from=weight, 
                values_fn = list(weight= min)) %>% 
    mutate_all(~replace(., is.na(.), 0)) %>% 
    as.data.frame() %>% 
    column_to_rownames('target')
  
  # instantiate an empty matrix
  empty_mat <- matrix(0, nrow = length(colnames(web_adj)),
                      ncol=length(colnames(web_adj)))
  colnames(empty_mat) <- colnames(web_adj)
  rownames(empty_mat) <- colnames(web_adj)
  
  # calculate pairwise minimums and sum them to create
  # a co-amplificatin matrix
  
  for (i in seq_along(colnames(empty_mat))){
    for (j in seq_along(colnames(empty_mat))){
      if (i == j){
        empty_mat[i,j] <- 0
      } 
      else{
        # grab col i and j and take their mins
        col_i <- web_adj[,colnames(empty_mat)[i]]
        col_j <- web_adj[,colnames(empty_mat)[j]]
        empty_mat[i, j] <- sum(pmin(col_i, col_j))
      }
    }
  }
  return(empty_mat)
}

create_graph <- function(olapdf){
  # create sna network object
  G <- network(olapdf, directed = F, hyper = F, loops = F, multiple = F, 
               bipartite = F, ignore.eval=FALSE,
               names.eval='weight')
  G%v%'vertex_names' <- colnames(olapdf)
  return(G)
}

#### Read in Data ####
raw <- read_csv(rd_data_el)

# generate top-15 target node plot
top_targ <- raw %>% group_by(target) %>% summarize(weight = sum(links))
top_targ %>%
  arrange(desc(weight)) %>% 
  ungroup %>% 
  slice(1:15) %>% 
  ggplot(aes(x = weight, y = reorder(target, weight))) +
  geom_bar(stat = 'identity', fill = '#c41230') +
  expand_limits(x= c(0, NA), y = c(0, NA)) +
  scale_x_continuous(labels = addUnits) +
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        panel.background = element_blank(), axis.line=element_line(color='black'))+
  theme(text = element_text(size = 20),
        axis.title.y = element_text(size=10),
        axis.title.x = element_text(size=20)) +
  xlab('weighted degree')+
  ylab('')
ggsave(paste0(output_dir, 'top_target_rd.png'))

# generate top 15 source node plot:
top_source <- raw %>% group_by(source) %>% summarize(weight = sum(links))
top_source %>%
  arrange(desc(weight)) %>% 
  ungroup %>% 
  slice(1:15) %>% 
  ggplot(aes(x = weight, y = reorder(source, weight))) +
  geom_bar(stat = 'identity', fill = '#c41230') +
  expand_limits(x= c(0, NA), y = c(0, NA)) +
  scale_x_continuous(labels = addUnits) +
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        panel.background = element_blank(), axis.line=element_line(color='black'))+
  theme(text = element_text(size = 20),
        axis.title.y = element_text(size=10),
        axis.title.x = element_text(size=20)) +
  xlab('weighted degree')+
  ylab('')
ggsave(paste0(output_dir, 'top_source_rd.png'))

# create network by threshsholding the sum of the minimum pair-wise overlap
oraw = raw %>% rename(weight = links, source = target, target = source)
olapdf <- min_overlap(oraw)
filtered_matrix <- ifelse(olapdf >= 15000, olapdf, 0)
diag(filtered_matrix) <- 0
filtered_matrix <- as.matrix(filtered_matrix)

# create Graph object
G <- create_graph(filtered_matrix)
# drop isolate nodes (by extracting largest connective component)- 
# i.e., drop nodes without minimum overlap scores of >= 15000...
# This is a kludge-y way of doing it, but it works
ig <- intergraph::asIgraph(G)
gclust <- igraph::clusters(ig, mode = 'weak')
lcc <- igraph::induced.subgraph(ig, igraph::V(ig)[which(gclust$membership == which.max(gclust$csize))])
G <- intergraph::asNetwork(lcc)
vertex_names = G%v%'vertex.names'

# prep labels for the graph- need to color labels w/ ggnetwork
palette_df <- raw %>% 
  select(source, network = group) %>% 
  distinct() %>% 
  filter(source %in% vertex_names)
palette_df$orientation <- recode(palette_df$network, russia = 'Russia', us = "US", pseudo = 'Pseudo', eu = 'Europe')
palette = c('Russia' = "#00853e", 'US' = '#043673', 'Pseudo' = '#c41230', 'Europe' = '#FDB515')

# create the graph viz with nodes colored by class
G%v%'Orientation' <- ifelse(G%v%'vertex.names' %in% palette_df$source, palette_df$orientation, NA)
ggplot(G, aes(x, y, xend=xend, yend=yend)) +
  geom_edges(aes(size = weight), color = 'grey4', alpha = 0.5,
             curvature = 0) +
  scale_size_continuous(range= c(0.1,3))+
  geom_nodes(aes(color = Orientation), size = 7, alpha = 1) +
  scale_color_manual(values = palette) + 
  geom_nodelabel_repel(aes(label = vertex.names),
                       box.padding = unit(1, 'lines'),
                       size = 6,
                       data = function(x){x[x$vertex.names %in% palette_df$source, ]}) +
  theme_blank()+
  theme(legend.text=element_text(size = 12)) +
  coord_cartesian(xlim = c(0, 1.25))
ggsave(paste0(output_dir, '/co_amp_rd_over15k.png'), dpi = 300, height = 9, width = 11, units = 'in')



