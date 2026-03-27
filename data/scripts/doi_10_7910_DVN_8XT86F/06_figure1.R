##############################################################################
#
#                             Replication scripts
#                                Plot Figure 1
#         Battaglini M., Leone Sciabolazza V., Patacchini, E. (2019)
#                     "Effectiveness of connected legislators"
#                      American Journal of Political Science
#
##############################################################################

# load data
load("replication_data.RData")
load("layout_figure_1.rda")

# load libraries
library(igraph); library(scales)

# select 113th congress network
sel <- which(db$time == 5)
g <- cosponsorship_network[sel, sel]

# Select top connections
g <- ifelse(g >= quantile(g[g!=0], seq(0, 1, 0.05))[20], 1, 0)

# Transform g in an igraph object 
g <- graph_from_adjacency_matrix(adjmatrix = g)

# Set colors, size and shape
V(g)$color <- ifelse(unlist(subset(db, time == 5, select = party)) == 1, "blue", "red")
V(g)$shape <- ifelse(unlist(subset(db, time == 5, select = party)) == 1, "square", "circle")
V(g)$size <- rescale(unlist(subset(db, time == 5, select = les)), c(2, 7))

# print figure 1
plot(g, layout = l, vertex.color = V(g)$color, vertex.shape = V(g)$shape, 
     vertex.label = NA, vertex.size = V(g)$size, edge.arrow.size = 0, 
     edge.arrow.width = 0, edge.width = 0.5, edge.color = "grey70", asp = 0, 
     axes = FALSE)
