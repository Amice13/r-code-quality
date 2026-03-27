#################################
# PLOT IGRAPH AS SPATIAL OBJECT
#################################


edges2lines <- function(g){
  e.vertices <- ends(g, E(g), names = F)
  pts <- cbind(vertex_attr(g, "x"),vertex_attr(g, "y"))
  cbind(pts[e.vertices[,1, drop = F],, drop = F], pts[e.vertices[,2, drop = F], , drop = F] )
}

plot_spatial_graph <- function(g, 
                               vertex.size = 0.25, 
                               vertex.color = "black", vertex.pch = 19,
                               edge.color = "grey", edge.width = .25,
                               axes=F, bty = "n", xlab = "", ylab = "",
                               add = F, edge.lty = 1,
                               ...){
  
  # Vertices to SpatialPoints
  pts <- SpatialPoints(cbind(vertex_attr(g, "x"),vertex_attr(g, "y")))
  
  # Edges to SpatialLines
  lines <- edges2lines(g)
  
  # Plot extent
  ext <- extent(pts)
  
  # Plot
  if(!add){
    plot(NULL, xlim = ext[1:2], ylim = ext[3:4], axes = axes, bty = bty, xlab = xlab, ylab = ylab, ...) 
  }
  if(!is.null(lines)){
    segments(lines[,1],lines[,2],lines[,3],lines[,4], lwd = edge.width, col = edge.color, lty = edge.lty)
  }
  points(pts, pch = vertex.pch, cex = vertex.size, col = vertex.color)
}


make_opt_color <- function(graph, var, colors = viridis(n = 10, option = "D")){
  require(igraph)
  require(MapColoring)
  
  Y <- as.numeric(as.factor(vertex_attr(graph, var)))
  Y.ngb <- unique(rbind(cbind(Y[ends(graph, E(graph), names = F)[,1]], 
                              Y[ends(graph, E(graph), names = F)[,2]]),
                        cbind(Y[ends(graph, E(graph), names = F)[,2]], 
                              Y[ends(graph, E(graph), names = F)[,1]])))
  adj.mat <- sapply(1:max(Y),function(i){
    sapply(1:max(Y), function(j){
      any(Y.ngb[,1] == i & Y.ngb[, 2] == j)
    })
  })
  diag(adj.mat) <- T
  
  ## Get Optimal contrast colors
  return(getOptimalContrast(x=adj.mat, col=colors)[Y])
}
