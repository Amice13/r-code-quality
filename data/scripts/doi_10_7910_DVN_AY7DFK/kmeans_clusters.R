args = commandArgs(trailingOnly = TRUE)


library(tidyr)
library(igraph)
library(dplyr)
library(haven)

setwd(args[1])

df = read.csv("./labexnaf_prox.csv")

colnames(df) <- c("labex","ape","weight")

g = graph_from_data_frame(df, directed = TRUE,vertices = NULL)

adj.matrix = get.adjacency(g, sparse = FALSE, attr = "weight") 


k <- args[3]
km = kmeans(adj.matrix, centers = k, nstart = 25)

clustered = as.data.frame(km$cluster)
colnames(clustered) = "kmeans"
clustered = data.frame(labex=rownames(clustered),kmeans = clustered$kmeans)
clustered = merge(df,clustered, all.x = TRUE)

clustered = clustered %>% select(labex,ape,kmeans)
clustered = unique(clustered)

write.csv(clustered,args[2])
