#############################
# ILLUSTRATION OF PSPM
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


# LOAD GLOBALS AND DATA ##
if(!exists("LOADED_GLOBALS")){
  source("scripts/analysis/analysis_globals.R")
}

# Make Lattice

## Make lattice
edge.mat <- data.frame(v1 = c(1,1,2,3),
                       v2 = c(2,3,4,4),
                       ethnic = c(1,0,0,1),
                       river = c(1,1,0,0))
vert.df <- data.frame(id = c(1:4),
                      outcome = rep(0, 4),
                      x = c(0,1,0,1),
                      y = c(1,1,0,0))

graph <- graph_from_data_frame(d = edge.mat, directed = F, 
                               vertices = vert.df)


## River and ethnic boundary
river.sp <- SpatialLines(LinesList = 
                           list(Lines(list(Line(cbind(c(.45, .3, .6, .45, -.25), 
                                                      c(1.25, 1, .75,  .5, .5)))), 
                                      ID = "river")))
river.sp <- smooth(river.sp, method = "chaikin")

ethnic.sp <- SpatialLines(LinesList = 
                           list(Lines(list(Line(cbind(c(.55, .4, .7, .55), 
                                                      c(1.25, 1, .75, -.25)))), 
                                      ID = "ethnic")))
ethnic.sp <- smooth(ethnic.sp, method = "chaikin")


# PSPM

## Make
pspm <- pspm::igraph2PSPM(graph, 
                                    outcome_name = "outcome",
                                    edge_pred_names = c("ethnic", "river"),
                                    vertex_coords = c("x", "y"))

## Set Beta
beta <- c(-1, 1, .5)
pspm$set_beta(beta)

# Get total log likelihood

## All partitionings

### All combinations
all.part <- expand.grid(rep(list(1L:4L), 4))

### Unique combinations 
all.part <- t(apply(all.part, 1, function(x){
  as.numeric(factor(x, levels = unique(x), ordered = T))
}))
all.part <- unique(all.part)

### Drop non-contiguous ones
non.contig <- apply(all.part, 1, function(x){
  (x[1] == x[4] & x[2] != x[1] & x[1] != x[3]) |
    (x[2] == x[3] & x[2] != x[1] & x[2] != x[4])
})
all.part <- all.part[!non.contig,]


## All energies
energies <- apply(all.part, 1, function(p){
  pspm$pm$set_partitioning(p)
  pspm$pm$get_total_energy()
})

## Probabilities
probabilities <- exp(-1 * energies) / sum(exp(-1*energies))


# Plots

## Selection
ex.vec <- c(4, 12,  2, 8, 7)


## Colors
river.col <- rgb(0,0,1, .6)
ethnic.col <- rgb(1,0,0, .6)


## Plot
for(i in seq_along(ex.vec)){
  
  ## Get index number
  this.idx <- ex.vec[i]
  
  ## Get partitioning
  this.part <- all.part[this.idx, ]
  
  ## Edge connections
  e.ends <- ends(graph, E(graph), names = F)
  edges <- this.part[e.ends[,1]] == this.part[e.ends[,2]]
  
  
  ## Plot
  
  png(file.path(fig.path, paste0("pspm_ex", i, ".png")), width = 1.2, height = 1.6, res = 400, units = "in")
  par(mar = c(0,0,0,0))
  plot(NULL, xlim = c(-.5, 1.5), ylim = c(-.5, 2),
       axes=F, bty = "n", xlab = "", ylab = "")
  lines(river.sp, col = river.col, lwd = 2)
  lines(ethnic.sp, col = ethnic.col, lwd = 2)
  plot_spatial_graph(graph,
                     vertex.color = c("red","blue","green","black")[this.part],
                     vertex.size = 2,
                     edge.lty = ifelse(edges, 1, 2),
                     edge.col = "black",
                     edge.width = 1, add = T)
  text(x = V(graph)$x, y = V(graph)$y,
       label = this.part, cex = .75,
       col = "white")
  text(x = -.5, y = 1.95, pos = 4,
       bquote(epsilon == .(signif(energies[this.idx], digits = 2))),
       cex = .75)
  text(x = -.5, y = 1.65, pos = 4,
       bquote("Pr" == .(signif(probabilities[this.idx], digits = 2))),
       cex = .75)
  if(i == 1){
    arrows(-.35, -.15, x1 = -.25, y1 = .45, length = 0.05, angle = 30,
           col = river.col)
    text(x =-.75, y = -.3,
         label = "River", cex = .75,
         col = river.col, pos = 4)
    
    text(x =.45, y = -.3,
         label = "Ethnic", cex = .75,
         col = ethnic.col, pos = 4)
    text(x =.45, y = -.5,
         label = "boundary", cex = .75,
         col = ethnic.col, pos = 4)
  }
  dev.off()
}

