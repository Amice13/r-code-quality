
albers_grid <- function(shape,
                        structure,
                        edge_length = 100,
                        shift_x = 0,
                        shift_y = 0,
                        exp_factor = 1){
  if(structure == "hexagonal"){
    albers_hexgrid(shape = shape,
                   edge_length = edge_length,
                   shift_x = shift_x,
                   shift_y = shift_y,
                   exp_factor = exp_factor)
  } else if(structure == "quad4"){
    albers_quadgrid(shape = shape,
                    edge_length = edge_length,
                    shift_x = shift_x,
                    shift_y = shift_y,
                    exp_factor = exp_factor)
  } else if(structure == "triangular"){
    albers_trigrid(shape = shape,
                   edge_length = edge_length,
                   shift_x = shift_x,
                   shift_y = shift_y,
                   exp_factor = exp_factor)
  } else if(structure == "random"){
    albers_randgrid(shape = shape,
                    edge_length = edge_length,
                    shift_x = shift_x,
                    shift_y = shift_y,
                    exp_factor = exp_factor)
  } else {
    stop("Invalid structure.")
  }
}

albers_sample <- function(shape, structure, edge_length, shift_x = 0, 
                          shift_y = 0, 
                          exp_factor = 1){
  # Extent of grid
  ext <- extent(shape)
  ext <- ext + c(-shift_x, 0, -shift_y, 0)
  
  # Compute number of points in grid
  ##   Area of hexagon = A = 2 * r^2 * 3^.5
  if(structure == "triangular"){
    target.area <- 2 * 3^.5 * edge_length^2 / 2
  } else if (structure == "quadratic"){
    target.area <- edge_length^2
  } else if (structure == "hexagonal"){
    target.area <- 2 * (edge_length/(1.3 * 2))^2 * 3^.5
  } else if (structure == "random"){
    target.area <- 2 * (edge_length/(1.3 * 2))^2 * 3^.5
  } else {
    stop("Invalid structure")
  }
  ext.area <- geosphere::areaPolygon(as(ext, "SpatialPolygons")) / 1e6
  N <- max(c(1, round(ext.area / target.area)))
  
  # Transformation to Albers
  proj4string = "+proj=aea +lat_1=29.5 +lat_2=42.5"
  if(is.null(proj4string(shape)) | is.na(proj4string(shape))){
    warning("Setting projection to WGS84")
    proj4string(shape) <- "+proj=longlat +ellps=WGS84 +no_defs"
  }
  old.proj4string <- proj4string(shape)
  if(old.proj4string != proj4string){
    shape <- spTransform(shape, CRS(proj4string))
  }
  
  # Sample Points
  sample.exp <- extent(shape)
  if(exp_factor < 1){
    sample.exp <- extent(c(sample.exp[2] + (sample.exp[1:2] - sample.exp[2]) / exp_factor,
                           sample.exp[4] + (sample.exp[3:4] - sample.exp[4]) / exp_factor))
  }
  pts <- spsample(as(sample.exp, "SpatialPolygons"), 
                  n = N,
                  type = ifelse(structure == "quadratic", "regular", structure))
  
  # Expansion
  if(exp_factor != 1){
    max <- c(max(pts@coords[,1]), max(pts@coords[,2]))
    pts <- SpatialPoints(cbind(max[1] + (pts@coords[,1] - max[1]) * exp_factor,
                               max[2] + (pts@coords[,2] - max[2]) * exp_factor))
  }
  
  # Crop to shape
  pts <- raster::crop(pts, shape)
  proj4string(pts) <- proj4string
  
  # Transform back
  pts <- spTransform(pts, CRS(old.proj4string))
  
  # Return
  return(pts)
}


albers_trigrid <- function(shape, edge_length, shift_x = 0, 
                           shift_y = 0, 
                           exp_factor = 1){
  # Extent of grid
  ext <- extent(shape)
  ext <- ext + c(-shift_x, 0, -shift_y, 0)
  
  # Compute number of points in grid
  ##   Area of hexagon = A = 2 * r^2 * 3^.5
  target.area <- 2 * 3^.5 * edge_length^2 / 2
  ext.area <- geosphere::areaPolygon(as(ext, "SpatialPolygons")) / 1e6
  N <- max(c(1, round(ext.area / target.area)))
  
  # Transformation to Albers
  proj4string = "+proj=aea +lat_1=29.5 +lat_2=42.5"
  if(is.null(proj4string(shape)) | is.na(proj4string(shape))){
    warning("Setting projection to WGS84")
    proj4string(shape) <- "+proj=longlat +ellps=WGS84 +no_defs"
  }
  old.proj4string <- proj4string(shape)
  if(old.proj4string != proj4string){
    shape <- spTransform(shape, CRS(proj4string))
  }
  
  # Sample Points
  sample.exp <- extent(shape)
  if(exp_factor < 1){
    sample.exp <- extent(c(sample.exp[2] + (sample.exp[1:2] - sample.exp[2]) / exp_factor,
                           sample.exp[4] + (sample.exp[3:4] - sample.exp[4]) / exp_factor))
  }
  pts <- spsample(as(sample.exp, "SpatialPolygons"), 
                  n = N,
                  type = "hexagonal")
  
  # Expansion
  if(exp_factor != 1){
    max <- c(max(pts@coords[,1]), max(pts@coords[,2]))
    pts <- SpatialPoints(cbind(max[1] + (pts@coords[,1] - max[1]) * exp_factor,
                               max[2] + (pts@coords[,2] - max[2]) * exp_factor))
  }
  
  # Cropper if too large
  cropper <-  as(extent(shape), "SpatialPolygons")
  pts <- raster::crop(pts, cropper)
  
  # Draw Network
  graph <- pts2graph(pts)
  
  # Triangles
  triangles <- triangles(graph)
  
  # Average point per triangle
  pts <- SpatialPoints(cbind(V(graph)$x, V(graph)$y))
  new.pts <- do.call(rbind, lapply(seq(1, length(triangles)-2, by = 3), 
                                   function(t){
                                     apply(pts@coords[triangles[t:(t+2)],], 2, mean)
                                   }))
  new.pts <- SpatialPoints(new.pts)
  
  # Make new graph
  graph <- pts2graph(new.pts)
  
  # Delete long edges
  new.pts <- SpatialPoints(cbind(V(graph)$x, V(graph)$y))
  E(graph)$length <- apply(ends(graph, E(graph), names = F), 1,
                           function(e){
                             dist(new.pts@coords[e,])
                           })
  graph <- delete.edges(graph, 
                        which(E(graph)$length > min(E(graph)$length) + .01*min(E(graph)$length)))
  v.degree <- degree(graph)
  graph <- delete.vertices(graph, which(v.degree > 3 | v.degree == 0))
  
  # Check
  if(length(V(graph)) == 0){
    warning("No vertices in graph")
    return(NULL)
  }
  
  
  # Subset network
  graph <- crop_graph(graph, shape)
  
  # Transform network
  g.pts <- SpatialPoints(cbind(V(graph)$x, V(graph)$y),
                         proj4string = CRS(proj4string))
  g.pts <- spTransform(g.pts, CRS(old.proj4string))
  V(graph)$x <- g.pts@coords[,1]
  V(graph)$y <- g.pts@coords[,2]
  
  return(graph)
}



# Hexagonal grid with roughly equal edge lengths
albers_quadgrid <- function(shape, edge_length, 
                           shift_x = 0, shift_y = 0, 
                           exp_factor = 1){
  # Extent of grid
  ext <- extent(shape)
  ext <- ext + c(-shift_x, 0, -shift_y, 0)
  
  # Compute number of points in grid
  target.area <- edge_length^2 
  ext.area <- geosphere::areaPolygon(as(ext, "SpatialPolygons")) / 1e6
  N <- max(c(1, round(ext.area / target.area)))
  
  # Transformation to Albers
  proj4string = "+proj=aea +lat_1=29.5 +lat_2=42.5"
  if(is.null(proj4string(shape)) | is.na(proj4string(shape))){
    warning("Setting projection to WGS84")
    proj4string(shape) <- "+proj=longlat +ellps=WGS84 +no_defs"
  }
  old.proj4string <- proj4string(shape)
  if(old.proj4string != proj4string){
    shape <- spTransform(shape, CRS(proj4string))
  }
  
  # Sample Points
  sample.exp <- extent(shape)
  if(exp_factor < 1){
    sample.exp <- extent(c(sample.exp[2] + (sample.exp[1:2] - sample.exp[2]) / exp_factor,
                           sample.exp[4] + (sample.exp[3:4] - sample.exp[4]) / exp_factor))
  }
  pts <- spsample(as(sample.exp, "SpatialPolygons"), 
                  n = N,
                  type = "regular")
  
  # Expansion
  if(exp_factor != 1){
    max <- c(max(pts@coords[,1]), max(pts@coords[,2]))
    pts <- SpatialPoints(cbind(max[1] + (pts@coords[,1] - max[1]) * exp_factor,
                               max[2] + (pts@coords[,2] - max[2]) * exp_factor))
  }
  
  # Cropper if too large
  cropper <-  as(extent(shape), "SpatialPolygons")
  pts <- raster::crop(pts, cropper)
  
  # Draw Network
  graph <- pts2graph(pts)
  
  # Delete long edges
  new.pts <- SpatialPoints(cbind(V(graph)$x, V(graph)$y))
  E(graph)$length <- apply(ends(graph, E(graph), names = F), 1,
                           function(e){
                             dist(new.pts@coords[e,])
                           })
  graph <- delete.edges(graph, 
                        which(E(graph)$length > min(E(graph)$length) + .01*min(E(graph)$length)))
  v.degree <- degree(graph)
  graph <- delete.vertices(graph, which(v.degree > 4 | v.degree == 0))
  
  # Check
  if(length(V(graph)) == 0){
    warning("No vertices in graph")
    return(NULL)
  }
  
  # Subset network
  graph <- crop_graph(graph, shape)
  
  # Transform network
  g.pts <- SpatialPoints(cbind(V(graph)$x, V(graph)$y),
                         proj4string = CRS(proj4string))
  g.pts <- spTransform(g.pts, CRS(old.proj4string))
  V(graph)$x <- g.pts@coords[,1]
  V(graph)$y <- g.pts@coords[,2]
  
  # Return
  return(graph)
}

# Hexagonal grid with roughly equal edge lengths
albers_hexgrid <- function(shape, edge_length, 
                           shift_x = 0, shift_y = 0, 
                           exp_factor = 1){
  # Extent of grid
  ext <- extent(shape)
  ext <- ext + c(-shift_x, 0, -shift_y, 0)
  
  # Compute number of points in grid
  ##   Area of hexagon = A = 2 * r^2 * 3^.5
  target.area <- 2 * (edge_length/(1.3 * 2))^2 * 3^.5
  # target.area <- (edge_length/2)^2 * 3^.5
  ext.area <- geosphere::areaPolygon(as(ext, "SpatialPolygons")) / 1e6
  N <- max(c(1, round(ext.area / target.area)))
  
  # Transformation to Albers
  proj4string = "+proj=aea +lat_1=29.5 +lat_2=42.5"
  if(is.null(proj4string(shape)) | is.na(proj4string(shape))){
    warning("Setting projection to WGS84")
    proj4string(shape) <- "+proj=longlat +ellps=WGS84 +no_defs"
  }
  old.proj4string <- proj4string(shape)
  if(old.proj4string != proj4string){
    shape <- spTransform(shape, CRS(proj4string))
  }
  
  # Sample Points
  sample.exp <- extent(shape)
  if(exp_factor < 1){
    sample.exp <- extent(c(sample.exp[2] + (sample.exp[1:2] - sample.exp[2]) / exp_factor,
                    sample.exp[4] + (sample.exp[3:4] - sample.exp[4]) / exp_factor))
  }
  pts <- spsample(as(sample.exp, "SpatialPolygons"), 
                  n = N,
                  type = "hexagonal")
  
  # Expansion
  if(exp_factor != 1){
    max <- c(max(pts@coords[,1]), max(pts@coords[,2]))
    pts <- SpatialPoints(cbind(max[1] + (pts@coords[,1] - max[1]) * exp_factor,
                               max[2] + (pts@coords[,2] - max[2]) * exp_factor))
  }
  
  
  # Cropper
  cropper <-  as(extent(shape), "SpatialPolygons")
  pts <- raster::crop(pts, cropper)
  

  # # Make Cells
  # cells <- voronoi_poly(pts, ext = extent(shape))
  
  # Draw Network
  graph <- pts2graph(pts)
  
  # Delete long edges
  new.pts <- SpatialPoints(cbind(V(graph)$x, V(graph)$y))
  E(graph)$length <- apply(ends(graph, E(graph), names = F), 1,
                           function(e){
                             dist(new.pts@coords[e,])
                           })
  graph <- delete.edges(graph, 
                        which(E(graph)$length > min(E(graph)$length) + .01*min(E(graph)$length)))
  v.degree <- degree(graph)
  graph <- delete.vertices(graph, which(v.degree > 6 | v.degree == 0))
  
  # Check
  if(length(V(graph)) == 0){
    warning("No vertices in graph")
    return(NULL)
  }
  
  
  # Subset network
  graph <- crop_graph(graph, shape)
  
  # Transform network
  g.pts <- SpatialPoints(cbind(V(graph)$x, V(graph)$y),
                         proj4string = CRS(proj4string))
  g.pts <- spTransform(g.pts, CRS(old.proj4string))
  V(graph)$x <- g.pts@coords[,1]
  V(graph)$y <- g.pts@coords[,2]
  
  # Return
  return(graph)
}


# Hexagonal grid with roughly equal edge lengths
albers_randgrid <- function(shape, edge_length, 
                           shift_x = 0, shift_y = 0, 
                           exp_factor = 1){
  # Extent of grid
  ext <- extent(shape)
  ext <- ext + c(-shift_x, 0, -shift_y, 0)
  
  # Compute number of points in grid
  ##   Area of hexagon = A = 2 * r^2 * 3^.5
  target.area <- 2 * (edge_length/(1.3 * 2))^2 * 3^.5
  # target.area <- (edge_length/2)^2 * 3^.5
  ext.area <- geosphere::areaPolygon(as(ext, "SpatialPolygons")) / 1e6
  N <- max(c(1, round(ext.area / target.area)))
  
  # Transformation to Albers
  proj4string = "+proj=aea +lat_1=29.5 +lat_2=42.5"
  if(is.null(proj4string(shape)) | is.na(proj4string(shape))){
    warning("Setting projection to WGS84")
    proj4string(shape) <- "+proj=longlat +ellps=WGS84 +no_defs"
  }
  old.proj4string <- proj4string(shape)
  if(old.proj4string != proj4string){
    shape <- spTransform(shape, CRS(proj4string))
  }
  
  # Sample Points
  sample.exp <- extent(shape)
  if(exp_factor < 1){
    sample.exp <- extent(c(sample.exp[2] + (sample.exp[1:2] - sample.exp[2]) / exp_factor,
                           sample.exp[4] + (sample.exp[3:4] - sample.exp[4]) / exp_factor))
  }
  pts <- spsample(as(sample.exp, "SpatialPolygons"), 
                  n = N,
                  type = "random")
  
  # Expansion
  if(exp_factor != 1){
    max <- c(max(pts@coords[,1]), max(pts@coords[,2]))
    pts <- SpatialPoints(cbind(max[1] + (pts@coords[,1] - max[1]) * exp_factor,
                               max[2] + (pts@coords[,2] - max[2]) * exp_factor))
  }
  
  # Cropper
  cropper <-  as(extent(shape), "SpatialPolygons")
  pts <- raster::crop(pts, cropper)
  
  
  # Draw Network
  graph <- pts2graph(pts)
  
  # Check
  if(length(V(graph)) == 0){
    warning("No vertices in graph")
    return(NULL)
  }
  
  
  # Subset network
  graph <- crop_graph(graph, shape)
  
  # Transform network
  g.pts <- SpatialPoints(cbind(V(graph)$x, V(graph)$y),
                         proj4string = CRS(proj4string))
  g.pts <- spTransform(g.pts, CRS(old.proj4string))
  V(graph)$x <- g.pts@coords[,1]
  V(graph)$y <- g.pts@coords[,2]
  
  # Return
  return(graph)
}

optimize_albers_grid <- function(shape, edge_length, maxit, grid.type,
                                 interval = c(.5, 2), tol = .5){
  ## Make network
  if(grid.type == "quad4"){
    g <- albers_quadgrid(shape, 
                         edge_length = edge_length,
                         shift_x = 0, shift_y = 0,
                         exp_factor = 1) ## res in km
  } else if(grid.type == "hexagonal"){
    g <- albers_hexgrid(shape = shape, 
                        edge_length = edge_length,
                        shift_x = 0, shift_y = 0,
                        exp_factor = .7) ## res in km
  } else if(grid.type == "triangular"){
    g <- albers_trigrid(shape = shape, 
                        edge_length = edge_length,
                        shift_x = 0, shift_y = 0,
                        exp_factor = 1) ## res in km
  } else if(grid.type == "random"){
    set.seed(1)
    g <- albers_randgrid(shape = shape, 
                         edge_length = edge_length,
                         shift_x = 0, shift_y = 0,
                         exp_factor = 1) ## res in km
  } else {
    stop("Please enter valid grid type.")
  }
  ## Get average edge length
  e.ends <- ends(g, es = E(g), names = F)
  
  # Pts
  pts <- SpatialPoints(cbind(V(g)$x, V(g)$y))
  proj4string(pts) <- "+proj=longlat +ellps=WGS84 +no_defs"
  
  # Cropper
  proj4string = "+proj=aea +lat_1=29.5 +lat_2=42.5"
  if(is.null(proj4string(shape)) | is.na(proj4string(shape))){
    warning("Setting projection to WGS84")
    proj4string(shape) <- "+proj=longlat +ellps=WGS84 +no_defs"
  }
  old.proj4string <- proj4string(shape)
  if(old.proj4string != proj4string){
    shape <- spTransform(shape, CRS(proj4string))
  }
  
  # Transform to Albers
  proj4string = "+proj=aea +lat_1=29.5 +lat_2=42.5"
  pts <- spTransform(pts, CRS(proj4string))

  ## Optimization function
  opt_fun <- function(x){
    print(x)
    # Loacl copy
    new.pts <- pts
    
    # Expansion
    max <- c(max(new.pts@coords[,1]), max(new.pts@coords[,2]))
    new.pts <- SpatialPoints(cbind(max[1] + (new.pts@coords[,1] - max[1]) * x,
                                 max[2] + (new.pts@coords[,2] - max[2]) * x))
    proj4string(new.pts) <- proj4string
    inside <- which(gContains(shape, new.pts, byid = T)[,1])
    
    # Transform back to WGS84
    new.pts <- new.pts[inside, ]
    new.pts <- try(spTransform(new.pts, CRS("+proj=longlat +ellps=WGS84 +no_defs")))
    if(class(new.pts) == "try-error"){
      warning("No finite transformation")
      return(edge_length)
    }
    
    # Distanc
    these.ends <- e.ends[e.ends[,1] %in% inside  & e.ends[,2] %in% inside,]
    length <- apply(these.ends,1, function(i){
      distHaversine(new.pts[which(inside == i[1]),],
                    new.pts[which(inside == i[2]),])/1000
    })
    
    ## Return squared difference
    return(abs(edge_length - mean(length)))
  }
  
  ## Optimize
  res <- optim(par = edge_length, fn = opt_fun, 
                  lower = interval[1], upper = interval[2],
                  method = "Brent",
                  control = list(maxit = maxit, abstol = tol))
  
  ## Return
  return(res)
}

