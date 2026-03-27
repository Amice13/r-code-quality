##############################
# CROP GRAPH WITH POLYGON
# Carl MC, 20.9.2019
##############################

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

