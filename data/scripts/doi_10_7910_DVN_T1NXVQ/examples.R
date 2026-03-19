#############################
# PLOT MONTE-CARLO EXAMPLES
# 
# 
# Part of:
# Shaping States into Nations: The Effects of Ethnic Geography on State Borders
# Müller-Crepon, Schvitz, Cederman
# Replication files
#
# Called from scripts/analysis/analysis_all.R
#
#############################
rm(list = ls())

# GLOBALS ####
source("scripts/analysis/analysis_globals.R")

# INIT #######
library(pspm)
library(reticulate)
library(raster)
library(plyr)
library(dplyr)
library(tidyr)
library(ggplot2)
library(gridExtra)
library(grid)
library(viridis)
library(MapColoring)


# PLOT EXAMPLE ###

# Setup 
N <- 8

# Simualate
set.seed(1)
py_set_seed(1, disable_hash_randomization = TRUE)
pspm <- generate_grid_data_mod(N_sqrd = N,
                                  beta0 = -1,
                                  beta = 1,
                                  temperatures = 1, swap_iter = 5,
                                  dep_structure = "hexagonal",
                                  sd = 1,
                                  burnin = 100, return_full = F)


# Make a graph
graph <- PSPM2igraph(pspm)


# Make coordinates
V(graph)$y <- rep(c(rev(seq_len(N)), rev(seq_len(N)) - .5), N/2)
V(graph)$x <- rep((seq_len(N)) * .75^.5, each = N)


# Plot Predictor
png(file.path(fig.path, "mc_example_pred.png"), 
    width = .5 + 3*.75^.5, height = .5 + 3, 
    res = 400, units = "in")
par(mar = c(0,0,0,0))
plot_spatial_graph(graph,
                   edge.width = 4,vertex.size = 1.5,
                   edge.color = rev(viridis(256))[as.numeric(cut(E(graph)$x1, 256))])
dev.off()


# Plot Partitioning
png(file.path(fig.path, "mc_example_part.png"), 
    width = .5 + 3*.75^.5, height = .5 + 3, 
    res = 400, units = "in")
par(mar = c(0,0,0,0))
set.seed(1)
plot_spatial_graph(graph,
                   vertex.color = make_opt_color(graph, var = "Y", 
                                                 colors = rainbow(n = 5)),
                   edge.width = 2,vertex.size = 1.5,
                   edge.color = ifelse(V(graph)$Y[ends(graph, E(graph), names = F)[,1]] != 
                                         V(graph)$Y[ends(graph,E(graph), names = F)[,2]], "lightgrey","black"))
dev.off()


