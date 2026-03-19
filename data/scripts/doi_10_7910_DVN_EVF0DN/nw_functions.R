
# (function): Make adjacency matrix #####
find.adj <- function(ncol, nrow, ngbs = 8){
  # Makes adjacency matrix listing all pairs of direct neighbors 
  # Conciders manhattan, queen, and knight moves
  cell.ids <- c(1:(ncol*nrow))
  # Manhattan
  man.mat <- cbind(c(cell.ids[-seq(ncol, ncol*nrow, by = ncol)], cell.ids),
                   c(cell.ids[-seq(ncol, ncol*nrow, by = ncol)]+1, cell.ids+ncol))
  ngb.mat <- man.mat[man.mat[,2]>0 & man.mat[,2]<= ncol*nrow,]
  # Queen
  if(ngbs > 4){
    que.mat <- cbind(c(cell.ids[-seq(ncol, ncol*nrow, by = ncol)], cell.ids[-seq(1, ncol*nrow, by = ncol)]),
                     c(cell.ids[-seq(ncol, ncol*nrow, by = ncol)]+1+ncol, cell.ids[-seq(1, ncol*nrow, by = ncol)]-1+ncol))
    que.mat <- que.mat[que.mat[,2]>0 & que.mat[,2]<= ncol*nrow,]
    
    ngb.mat <- rbind(ngb.mat,que.mat)
  }
  
  
  # Knight
  if(ngbs == 16){
    kni.mat1 <- cbind(c(cell.ids[-seq(ncol, ncol*nrow, by = ncol)], cell.ids[-seq(1, ncol*nrow, by = ncol)]),
                      c(cell.ids[-seq(ncol, ncol*nrow, by = ncol)]+1+2*ncol,
                        cell.ids[-seq(1, ncol*nrow, by = ncol)]-1+2*ncol))
    kni.mat2 <- cbind(c(cell.ids[-c(seq(ncol, ncol*nrow, by = ncol),seq(ncol-1, ncol*nrow, by = ncol))],
                        cell.ids[-c(seq(1, ncol*nrow, by = ncol),seq(2, ncol*nrow, by = ncol))]),
                      c(cell.ids[-c(seq(ncol, ncol*nrow, by = ncol),seq(ncol-1, ncol*nrow, by = ncol))]+2+ncol,
                        cell.ids[-c(seq(1, ncol*nrow, by = ncol),seq(2, ncol*nrow, by = ncol))]-2+ncol))
    kni.mat <- rbind(kni.mat1,kni.mat2)
    kni.mat <- kni.mat[kni.mat[,2]>0 & kni.mat[,2]<= ncol*nrow,]
    
    ngb.mat <- rbind(ngb.mat,kni.mat)
  }
  
  
  return(ngb.mat)
}


# Empty network
make_graph <- function(raster.path, factor, extent, noisy = T){
  ## Population grid
  raster <- velox(raster.path)
  
  ## Crop
  raster$crop(extent)
  
  ## Aggregate
  raster$aggregate(factor = FACTOR, aggtype = "sum")
  
  ## To Raster
  raster <- raster$as.RasterLayer()
  
  # To Grid
  rast.coords <- as.data.frame(raster,  xy=T, na.rm = F)
  adj.mat <- find.adj(ncol(raster), nrow(raster), ngb = 8)
  lines.ls <- lapply(c(1:nrow(adj.mat)), function(x){Lines(Line(rast.coords[adj.mat[x,], c("x","y")]), ID = x)})
  base.edge.sp <- SpatialLines(lines.ls)
  base.edge.spdf <- SpatialLinesDataFrame(base.edge.sp, 
                                          data = data.frame(id = paste0("base.", c(1:length(base.edge.sp))),
                                                            type = rep("base", length(base.edge.sp)), 
                                                            stringsAsFactors = F),
                                          match.ID = F)
  
  
  grid.bl <- BoostLines(base.edge.spdf) ## BoostLines::bGrid(raster, n8 = TRUE)
  
  # Node
  print("node lines")
  node.ls <- bNode.BoostLines(grid.bl)
  
  # Fake Data
  comb.df <- data.frame(year = 0, type = "base",  stringsAsFactors = F)[rep(1, length(node.ls[[2]])),]
  
  # Make igraph
  print("boost lines to graph")
  spl.nw <- boost2graph(x = node.ls[[1]], 
                         df = comb.df, 
                         lonlat = TRUE, 
                         plot.result = F)
  
  vert.df <- data.frame(x = V(spl.nw)$x, y = V(spl.nw)$y)
  raster.df <- as.data.frame(raster,  xy=T)
  raster.df[,c("x","y")]  <- apply(raster.df[,c("x","y")] , 2, round_any , accuracy = 1e-6)
  vert.df[,c("x","y")]  <- apply(vert.df[,c("x","y")] , 2, round_any , accuracy = 1e-6)
  raster.df$base.vertex <- 1 
  
  vert.df <- plyr::join(vert.df, raster.df, type = "left", by = c("x","y"))
  vert.df$base.vertex[is.na(vert.df$base.vertex)] <- 0
  
  ## Include columns
  for(var in colnames(vert.df)[!colnames(vert.df) %in% c("x","y")]){
    vertex_attr(spl.nw, var) <- vert.df[,var]
  }
  
  # Return data
  return(spl.nw)
  
}

#################################
# PLOT IGRAPH AS SPATIAL OBJECT
#################################


edges2lines <- function(g){
  e.vertices <- ends(g, E(g), names = F)
  pts <- cbind(vertex_attr(g, "x"),vertex_attr(g, "y"))
  cbind(pts[e.vertices[,1, drop = F],, drop = F], pts[e.vertices[,2, drop = F], , drop = F] )
}


# (function): Vertices of g within polygon or extent #
vertex_within <- function(g, x){
  # Convert x to polygon if not already a polygon
  if(!grepl("SpatialPolygon",class(x))){
    x <- as(raster::extent(x), "SpatialPolygons")
  }
  # Verteces to Spatial Points
  vertex.pts <- SpatialPoints(cbind(V(g)$x, V(g)$y))
  proj4string(vertex.pts) <- proj4string(x)
  
  # Within area
  return(apply(gContains(x,vertex.pts,  byid = T), 1, any))
}

# (function): Crop graph geographically #
crop_graph <- function(g, x, delete.islands = F){
  require(igraph)
  require(raster)
  require(rgeos)
  
  
  # Subset graph
  v.within <- which(!vertex_within(g, x))
  g.sub = delete.vertices(g, v.within)
  
  # Delete islands
  if(delete.islands){
    g.components <- components(g.sub)$membership
    g.sub <- induced_subgraph(g.sub,g.components == which.max(table(g.components)))
  }
  
  # Return
  return(g.sub)
}

delete_graph_islands <- function(g){
  g.components <- components(g)$membership
  induced_subgraph(g,g.components == which.max(table(g.components)))
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



# (function): Wrapper around bNode
node_bNode <- function(x){
  # In particular: Deal with 0-length lines returned by bNode
  # Send to bNode
  boost <- BoostLines(SpatialLines(x@lines))
  
  # Send to bNode
  noded <- bNode(boost)
  
  # Unboost
  noded.spl <- unboost(noded[[1]])
  ids <- noded[[2]]
  
  # Data
  noded.data <- x@data[ids,]
  
  
  # Round to 1e-8
  noded.spl@lines <- lapply(c(1:length(noded.spl@lines)),
                            function(l){Lines(Line(round_any(noded.spl@lines[[l]]@Lines[[1]]@coords,
                                                             1e-6)), ID = l)})
  # Check for points
  point.line <- unlist(lapply(noded.spl@lines,
                              function(l){all(l@Lines[[1]]@coords[1,] ==
                                                l@Lines[[1]]@coords[2,])}))
  noded.spl <- noded.spl[!point.line,]
  noded.data <- noded.data[!point.line,]
  
  # Spatial Dataframe
  sldf.noded.sldf <- SpatialLinesDataFrame(noded.spl, 
                                           data = noded.data,
                                           match.ID = F)
  # Return
  return(sldf.noded.sldf)
}
