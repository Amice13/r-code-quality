#############################
# MAIN FUNCTIONS FOR NETWORK CREATION
# 
# 
# Part of:
# Shaping States into Nations: The Effects of Ethnic Geography on State Borders
# Müller-Crepon, Schvitz, Cederman
# Replication files
#
# Called from scripts/data_prep/prepare_networks.R
#
#############################

# MAKE REAL NETWORK WRAPPER FUNCTION ### 
make_realnw_wrapper <- function(coverage = NULL,
                                keep.cowcode = c(200:400), drop.cowcode = NULL, extend.cov = F, ### Spatial Coverage parameters
                                resolution = 1, grid.type = c("quad4", "quad8", "quad24", "hexagonal", "triagonal", "random"),
                                predictors = c("rivers","watersheds", "elevation", "GREG"),
                                min.river.size = 6, watershed_level = "02",
                                na.rm = F,
                                predictor.data.path = "data/", 
                                shift_x = 0, shift_y = 0,exp_factor = 1,
                                # degree = 1,
                                continent = "eu"
){
  # packages
  require(velox)
  require(rgdal)
  require(rgeos)
  
  # Get sopatial coverage
  print("Make graph")
  if(is.null(coverage)){
    coverage <- get_spatial_cov(keep.cowcode = keep.cowcode, drop.cowcode = drop.cowcode, extend = extend.cov)
  } 
  
  # Make Graph
  if(any(grepl("SpatialPolygons", class(coverage)))){
    if(grid.type == "quad4"){
      # g <- make_quad_nw(coverage, res = resolution, ngb = 4)
      g <- albers_quadgrid(coverage, 
                          edge_length = resolution,
                          shift_x, shift_y, exp_factor = exp_factor) ## res in km
    } else if(grid.type == "quad8"){
      g <- make_quad_nw(coverage, res = resolution, ngb = 8)
    } else if(grid.type == "quad24"){
      g <- make_quad_nw(coverage, res = resolution, ngb = 24)
    } else if(grid.type == "hexagonal"){
      # g <- make_hex_nw(coverage, res = resolution * 114) ## res in km
      g <- albers_hexgrid(coverage, 
                          edge_length = resolution,
                          shift_x, shift_y, exp_factor = exp_factor) ## res in km
    } else if(grid.type == "triangular"){
      g <- albers_trigrid(coverage, 
                          edge_length = resolution,
                          shift_x, shift_y, exp_factor = exp_factor) ## res in km
    }  else if(grid.type == "random"){
      g <- albers_randgrid(coverage, 
                          edge_length = resolution,
                          shift_x, shift_y, exp_factor = exp_factor) ## res in km
    } else {
      stop("Please enter valid grid type.")
    }
  } else if(any(grepl("SpatialPoints", class(coverage)))) {
    
    g <- pts2graph(points = coverage, ngb.order = 1)
    
  } else {
    stop("Invalid coverage class.")
    
  }
  
  # # Add degrees of connectivity
  # if(degree > 1){
  #   E(g)$degree <- 1
  #   for(d in 2:degree){
  #     # Find pars
  #     pairs <- ego(g, order = d, mindist = d, mode = "all")
  #     
  #     # To edges
  #     add.edges <- do.call(cbind, 
  #                          lapply(seq_along(pairs), function(i){
  #                            rbind(rep(i, length(pairs[[i]])),
  #                                  pairs[[i]])
  #                          }))
  #     
  #     # Add
  #     g <- add_edges(g, as.vector(add.edges))
  #     
  #     # Name degree
  #     E(g)$degree[is.na(E(g)$degree)] <- d
  #   }
  # } else {
  #   E(g)$degree <- 1
  # }

  
  # Add predictors
  print("Add predictors")
  ## Edges as Spatial Lines
  sl <- edges2sl(g)
  
  ## Rivers
  if(any(grepl("river", predictors))){
    E(g)$river <- line2rivers(sl = sl, min.river.size = min.river.size, predictor.data.path = predictor.data.path)
    
    ### Recode
    E(g)$any_river <- ifelse(edge_attr(g, name = "river") == 0, 0, 1)
    stopifnot(all(!is.na(edge_attr(g, name = "any_river"))))
    
  }
  
  ## Basins
  if(any(grepl("watershed", predictors))){
    g <- graph2watershed(g, watershed_level = watershed_level, predictor.data.path = predictor.data.path, 
                         continent = continent, output = c("graph"))
    
  }
  
  
  ## GREG
  if(any(grepl("GREG", predictors))){
    greg.onedge <- graph2GREG(g, predictor.data.path = predictor.data.path, output = c("both"))
    V(g)$greg.id <- greg.onedge$vertex
    E(g)$same_greg <- greg.onedge$edge[,"same_greg"]
    E(g)$diff_greg <- greg.onedge$edge[,"diff_greg"]
    E(g)$mean_greg <- greg.onedge$edge[,"mean_greg"]
  }
  
  
  
  ## Elevation profile
  if(any(grepl("elevation", predictors))){
    ### Sample elevation on edges
    edge.elev.ls <- line2elevprofile(sl = sl, elev_rast_path = file.path(predictor.data.path, "geography/elevation/globe_30sec.tif"),
                                     sample_res = 2 * .00833333 ## 2 x resolution of elevation raster
    )
    
    ### Encode 0s instead of missings
    edge.elev.ls <- lapply(edge.elev.ls, function(x){
      x[is.na(x)] <- 0
      x
    })
    
    ### Calculate reasonable measure: sum of absolute differences, setting missings (i.e. water) to 0
    sum_abs_diff <- function(x){
      sum(abs(diff(x)))
    }
    E(g)$elevdiff <- unlist(lapply(edge.elev.ls, sum_abs_diff))
    E(g)$elevmean <- unlist(lapply(edge.elev.ls, mean, na.rm = T))
    E(g)$elevsd <- unlist(lapply(edge.elev.ls, sd, na.rm = T))
    E(g)$elevmax <- unlist(lapply(edge.elev.ls, max, na.rm = T))
    E(g)$elevmin <- unlist(lapply(edge.elev.ls, min, na.rm = T))
  }
  
  
  ## Population
  if(any(grepl("population", predictors))){
    ## Lcoal 
    pop.yrs <-  seq(1880, 2000, by = 10)
    
    ## Load raster
    pop.vx <- velox(stack(file.path(predictor.data.path, "geography", "pop_density_hist", paste0("popc_", pop.yrs, "AD.asc"))))
    
    ## Get vertex attr.
    vert.pop <- pop.vx$extract_points(SpatialPoints(cbind(V(g)$x, V(g)$y)))
    colnames(vert.pop) <- paste0("pop", pop.yrs)
    
    ## Store in vertex attr table
    for(v in colnames(vert.pop)){
      vertex_attr(g, v) <- vert.pop[,v]
    }
    
    ## Create edge measures
    fun.ls <- list(max = max, min = min, mean = mean, diff = function(x){abs(x[1] - x[2])})
    e.ends <- ends(g, E(g), names = F)
    for(v in colnames(vert.pop)){
      for(f in 1:length(fun.ls)){
        edge_attr(g, paste0(v,"_", names(fun.ls)[f])) <- 
          as.vector(apply(cbind(vert.pop[e.ends[,1], v], vert.pop[e.ends[,2], v]), 1, fun.ls[[f]]))
      }
    }
  }
  
  ## Latitude and longitude
  e.ends <- ends(g, E(g), names = F)
  E(g)$x <- (V(g)$x[e.ends[,1]] + V(g)$x[e.ends[,2]]) / 2
  E(g)$y <- (V(g)$y[e.ends[,1]] + V(g)$y[e.ends[,2]]) / 2
  
  
  # Delete Verteces and Edges with missings
  if(na.rm){
    for(v in vertex_attr_names(g)){
      g <- induced_subgraph(g, vids = which(!is.na(vertex_attr(g, name = v))))
    } 
    for(v in edge_attr_names(g)){
      g <- delete.edges(g, which(is.na(edge_attr(g, name = v))))
    } 
  }
  
  # Return graph
  return(g)
}


# Points to spatial graph
pts2graph <- function(points, ngb.order = 1){
  library(rtree)
  
  # Remove duplicate points
  dupl <- duplicated(points@coords)
  if(any(dupl)){
    print(paste("Removing" , sum(dupl), "duplicate points. "))
    points <- points[!dupl,]
  }
  
  # Delauney Triangulation
  del.tri = rgeos::gDelaunayTriangulation(points, onlyEdges = T)
  
  # remove multilines
  del.tri <- BoostLines::remove_multilines(del.tri)
  
  # To BoostLines
  bl <- BoostLines(del.tri)
  
  # Lines to igraph
  g <- boost2graph(bl, df = data.frame(lid = 1:length(del.tri)),
                   lonlat = T, plot = F)
  
  # Check
  stopifnot(length(V(g)) == length(points))
  
  # Add vertex info
  lat.df <- cbind(x = V(g)$x, y = V(g)$y)
  rt <- RTree(lat.df)
  ngb <- unlist(rtree::knn(rt, y = points@coords, k = as.integer(1)))
  stopifnot(length(ngb) == length(unique(ngb)))
  
  if(.hasSlot(points, "data")){
    for(v in colnames(points@data)[!colnames(points@data) %in% c("x","y")]){
      vertex_attr(g, name = v, index = ngb) <- points@data[,v]
    }
  }
  
  
  # Return
  return(g)
}


# Graph to GREG
graph2GREG <- function(g, predictor.data.path = "data", output = c("edge", "vertex", "both")){
  ### Load
  greg.shp <- readOGR(file.path(predictor.data.path, "geography/GREG"), "GREG")
  
  ### Create Group polygons
  grp.ids <- as.matrix(greg.shp@data[, c("G1ID","G2ID","G3ID")])
  grp.ids <- apply(grp.ids, 1, function(x){x[x!=0]})
  
  
  ### Get to Verteces
  over.mat <- gContains(greg.shp, SpatialPoints(cbind(V(g)$x, V(g)$y)), byid = T)
  V(g)$greg.poly.id <- unlist(apply(over.mat, 1, function(x){ifelse(any(x), which(x)[1], NA)}))
  
  
  ### Encode on Edges
  same_greg <- as.numeric(unlist(apply(ends(g, E(g)), 1, function(v){
    gid <- vertex_attr(g, name = "greg.poly.id", index = v)
    ifelse(any(grp.ids[[gid[1]]] %in% grp.ids[[gid[2]]]), 1, 0)
  })))
  diff_greg <- as.numeric(unlist(apply(ends(g, E(g)), 1, function(v){
    gid <- vertex_attr(g, name = "greg.poly.id", index = v)
    ifelse(any(!grp.ids[[gid[1]]] %in% grp.ids[[gid[2]]]) | any(!grp.ids[[gid[2]]] %in% grp.ids[[gid[1]]]), 1, 0)
  })))
  mean_greg <- as.numeric(unlist(apply(ends(g, E(g)), 1, function(v){
    gid <- vertex_attr(g, name = "greg.poly.id", index = v)
    v1 = grp.ids[[gid[1]]]
    v2 = grp.ids[[gid[2]]]
    max(c(mean(!v1 %in% v2), mean(!v2 %in% v1)))
  })))
  
  # Return
  if(output == "edge"){
    cbind(same_greg = same_greg, diff_greg = diff_greg, mean_greg = mean_greg)
  } else if(output == "vertex"){
    V(g)$greg.poly.id
  } else {
    list(edge = cbind(same_greg = same_greg, diff_greg = diff_greg, mean_greg = mean_greg),
         vertex = V(g)$greg.poly.id)
  }
}

# Graph to watershed
graph2watershed <- function(g, watershed_level = "04", predictor.data.path = "data", continent = "eu", output = c("edge", "vertex", "both")){
  ### Load
  basin.shp <- NULL
  this.l <- as.numeric(watershed_level)
  while(is.null(basin.shp) & this.l > 0){
    basin.shp <- try(load_hydroshed_polys(type = "standard", continent = continent, 
                                      level = ifelse(this.l > 9, 
                                                     as.character(this.l),
                                                     paste0(0, this.l)), 
                                      hydro.path = file.path(predictor.data.path, "geography/hydrosheds")))
    if(class(basin.shp) == "try-error"){
      basin.shp <- NULL
      this.l <- this.l - 1
      print(paste("Moving down to watershed level ", this.l))
    }
  }
  

  
  ### Get to Verteces
  over.mat <- gContains(basin.shp, SpatialPoints(cbind(V(g)$x, V(g)$y)), byid = T)
  V(g)$basin.id <- basin.shp$PFAF_ID[unlist(apply(over.mat, 1, function(x){ifelse(any(x), which(x)[1], NA)}))]
  
  ### Encode on Edges
  g.ends <- ends(g, E(g), names = F)
  edge_attr(g, name = "watershed_diff") <- 0 
  for(l in as.numeric(this.l):1){
    v.b.id <- substr(V(g)$basin.id, 1, l)
    edge_attr(g, name = "watershed_diff") <- ifelse(v.b.id[g.ends[,1]] != v.b.id[g.ends[,2]],
                                                    edge_attr(g, name = "watershed_diff") + 1, 
                                                    edge_attr(g, name = "watershed_diff"))
  }
  edge_attr(g, name = "any_watershed") <- ifelse(edge_attr(g, name = "watershed_diff") > 0, 1, 0)
  
  if(output == "edge"){
    edge_attr(g, name = "watershed_diff")
  } else if(output == "vertex"){
    vertex_attr(g, name = "basin.id")
  } else if(output == "graph"){
    g
  } else {
    list(edge = edge_attr(g, name = "watershed_diff"),
         vertex = vertex_attr(g, name = "basin.id"))
  }
  
}

# Rivers to line
line2rivers <- function(sl, min.river.size = 0, predictor.data.path = "data"){
  ### Load
  rivers.shp <- readOGR(file.path(predictor.data.path, "geography/Rivers"),
                        "ne_10m_rivers_lake_centerlines_scale_rank")
  
  
  ### Subset to Coverage
  riv.cov.int <- gIntersects(rivers.shp, 
                             as(extent(sl), "SpatialPolygons"), 
                             byid = T, returnDense =  T)
  rivers.shp <- rivers.shp[riv.cov.int[1,],]
  
  ### Union of rivers by name
  new.df <- rivers.shp@data[!duplicated(rivers.shp$rivernum),]
  new.df <- new.df[order(new.df$rivernum),]
  rownames(new.df) <- as.character(new.df$rivernum)
  rivers.shp <- SpatialLinesDataFrame(gLineMerge(rivers.shp, byid = T, id  = as.numeric(as.character(rivers.shp@data$rivernum))),
                                      new.df)
  row.names(rivers.shp) <- as.character(1:length(rivers.shp))
  
  ### Recode scalerank to numeric
  rivers.shp$size <- 11 - as.numeric(as.character(rivers.shp$scalerank))
  table(rivers.shp$size)
  
  ### Keep only big rivers
  rivers.shp <- rivers.shp[rivers.shp$size >= min.river.size,]
  
  ### Intersect with lines
  max.river <- intersect_lines(sl = sl, sl2 = rivers.shp, aggvar = "size", 
                               aggfun = function(x){max(x)}, na.val = 0, only.uneven = T)
  
  ### Return
  return(max.river)
}

# Elevation Profile along Line
line2elevprofile <- function(sl, elev_rast_path = "geography/elevation/globe_30sec.tif",
                             sample_res = 2 * .00833333 ## Defaults to 2 x resolution of raster
){ 
  # Load Raster
  elev.vx <- velox(elev_rast_path)
  
  # Crop raster
  elev.vx$crop(extent(sl))
  
  # Sample points regularly on sl
  llength <- gLength(sl, byid = T)
  if(length(unique(llength)) == 1){
    npts <- round(unique(llength)/sample_res) 
    sl.pts <- spsample(sl, npts * length(sl), type = "regular")
    pts.line.id <- rep(1: length(sl), each = npts)
  } else {
    npts <- round(llength / sample_res)
    npts[npts == 0] <- 1
    sl.pts <- do.call(rbind, lapply(1:length(sl), function(l){
      spsample(sl[l,], npts[l], type = "regular")
    }))
    pts.line.id <- rep(1: length(sl), npts)
  }
  
  # Extract elevation 
  pts.elev <- elev.vx$extract_points(sl.pts)
  
  # Split by line
  sl.evel.ls <- split(pts.elev, pts.line.id)
  
  # Clean
  rm(elev.vx)
  
  # Return
  return(sl.evel.ls)
}

# GET SPATIAL COVERAGE ##########
get_spatial_cov <- function(keep.cowcode = c(200:400), drop.cowcode = c(600:626,651, 365), extend = T){
  require(cshapes)
  ## Load
  coverage.shp <- cshp(date = as.Date("2016-01-01"))
  
  ## Subset to Core-coverage
  coverage.core.shp <- coverage.shp[coverage.shp$COWCODE %in% keep.cowcode &
                                      !coverage.shp$COWCODE %in% drop.cowcode,]
  
  ## Crop to extent Russia 
  if(extend){
    coverage.shp <- raster::crop(coverage.shp, as(extent(coverage.core.shp), "SpatialPolygons"))
    
    ## Drop Unwanted countries
    coverage.shp <- coverage.shp[!coverage.shp$COWCODE %in% drop.cowcode,]
  } else {
    coverage.shp <- coverage.core.shp
  }
  
  ## Country codes 1:length(coverage.shp)
  coverage.shp$id <- 1:length(coverage.shp)
  
  ## Return coverage
  return(coverage.shp)
}




# QUADRATIC GRIDS ###############
make_quad_nw <- function(coverage, res, ngb = 4){
  require(velox)
  require(rgeos)
  require(BoostLines)
  require(raster)
  require(igraph)
  
  # Make raster of coverage
  r <- raster(ext = extent(coverage), resolution = res)
  r <- setValues(r, 0)
  vx <- velox(r)
  coverage$id <- 1:length(coverage)
  vx$rasterize(coverage, field = "id", background = NA)
  r <- vx$as.RasterLayer()
  names(r) <- "cover.id"
  
  
  # Trim graph if ngb = 8 (get rid of intersections between main nodes)
  if(ngb != 4){
    adj.mat <- find.adj(ncol = ncol(r), nrow = nrow(r), ngb = ngb)
    rast.coords <- as.data.frame(r, na.rm = F, xy = T)[,c("x","y")]
    sl <- SpatialLines(lapply(c(1:nrow(adj.mat)), function(x){Lines(Line(rast.coords[adj.mat[x,],]), ID = x)}))
    bl <- BoostLines(sl)
  } else {
    # Raster to Boost Lines
    bl <- bGrid(x = r[[1]], n8 = FALSE)
    # SpatialLines
    sl <- unboost(bl)
  }
  
  # All to igraph
  g <- boost2graph(bl, df = data.frame(lid = 1:length(sl)),
                   lonlat = T, plot = F)
  
  # Get Vertex attributes
  vertex_attr(g, name = "cover.id") <-  vx$extract_points(SpatialPoints(cbind(vertex_attr(g, "x"),vertex_attr(g, "y"))))
  
  # Delete network outside coverage
  g <- induced_subgraph(g, vids = which(!is.na(vertex_attr(g, name = "cover.id"))))
  
  # Return Graph
  return(g)
}



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
  if(ngbs >= 16){
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
  
  # 24 neighbourhood
  if(ngbs == 24){
    man2.mat <- cbind(c(cell.ids[-seq(ncol, ncol*nrow, by = ncol)], cell.ids),
                      c(cell.ids[-seq(ncol, ncol*nrow, by = ncol)]+2, cell.ids+2*ncol))
    man2.mat <- man2.mat[man2.mat[,2]>0 & man2.mat[,2]<= ncol*nrow,]
    que2.mat <- cbind(c(cell.ids[-seq(ncol, ncol*nrow, by = ncol)],  cell.ids[-seq(1, ncol*nrow, by = ncol)]),
                      c(cell.ids[-seq(ncol, ncol*nrow, by = ncol)] + 2 + 2*ncol, cell.ids[-seq(1, ncol*nrow, by = ncol)] - 2 + 2*ncol))
    que2.mat <- que2.mat[que2.mat[,2]>0 & que2.mat[,2]<= ncol*nrow,]
    ngb.mat <- rbind(ngb.mat,man2.mat,que2.mat )
  }
  
  return(ngb.mat)
}



# HEXAGONAL GRIDS ###############
make_hex_nw <- function(coverage, res){
  require(sp)
  require(dggridR)
  require(rgeos)
  require(igraph)
  
  # Make grid for coverage
  
  ## Generate a dggs specifying an intercell spacing of ~25 miles
  dggs <- dgconstruct(spacing = res, metric = T, resround='nearest')
  
  ## Clip to coverage's extent
  cov_grid   <- dgrectgrid(dggs, minlat = extent(coverage)[3], minlon = extent(coverage)[1], 
                           maxlat = extent(coverage)[4], maxlon = extent(coverage)[2])
  
  ## Get as shapefile
  cov_grid.shp <- df_to_SpatialPolygons(df = cov_grid, keys = "cell", coords = c("long","lat"),
                                        proj = CRS(proj4string(coverage)))
  
  
  ## Get Centroids
  cov_centr.shp <- gCentroid(cov_grid.shp, byid = T)
  
  ## Subset grid to Coverage
  int.mat <- gContains(coverage, cov_centr.shp, byid = T)
  in.cov <- apply(int.mat,1, any)
  cov_grid.shp <- cov_grid.shp[in.cov,]
  cov_centr.shp <- cov_centr.shp[in.cov,]
  if(length(coverage) > 1){
    cover.id = apply(int.mat[in.cov,], 1, which)
  } else {
    cover.id <- rep(1, length(cov_grid.shp))
  }
  cov_grid.shp <- SpatialPolygonsDataFrame(cov_grid.shp,
                                           data.frame(cover.id = cover.id),
                                           match.ID = F)
  
  # Make graph
  g <- poly2graph(poly = cov_grid.shp, pts = cov_centr.shp, plot.result = F)
  
  # Return
  return(g)
}





# Dataframe with coordinates to Spatial Polygons
df_to_SpatialPolygons <- function(df,keys,coords,proj) {
  require(sp)
  
  ## Basic checks
  if(!is(df,"data.frame")) stop("df needs to be a data frame")
  if(!is(keys,"character")) stop("keys needs to be of class character")
  if(!is(coords,"character")) stop("coords needs to be of class character")
  if(!all(keys %in% names(df))) stop("All keys needs to be labels in data frame")
  if(!all(coords %in% names(df))) stop("All coordinate labels needs to be labels in data frame")
  if(!is(proj,"CRS")) stop("proj needs to be of class CRS")
  
  ## dfun takes a data frame with coordinates for 1 polygon, and makes one POLYGON object from it
  ## with a UID from the polygon key
  dfun <- function(d) {
    Polygons(list(Polygon(d[coords])),
             as.character(unique(d[keys])))
  }
  
  ## Now apply dfun to all polygons in data frame
  df_poly <- plyr::dlply(df,keys,dfun)
  
  ## Form a SpatialPolygons object from all the returned Polygons
  Sr <- SpatialPolygons(df_poly,             # Polygons
                        1:length(df_poly),   # plotting order
                        proj4string=proj)    # CRS
}




## fDataFrame to Polygons; from https://rdrr.io/cran/FRK/src/R/geometryfns.R
poly2graph <- function(poly, pts = NULL, plot.result = F) {
  require(igraph)
  require(rgeos)
  require(Matrix)
  
  # Use centroids if no points given
  if(is.null(pts)){
    pts <- gCentroid(poly, byid = T)
  }
  
  # Neighborhood matrix
  # Get neighbour list
  ngb.ls <- gTouches(poly, byid = T, returnDense = F)
  
  # Make sparse matrix
  ngb.mat <- sparseMatrix(i = rep(1:length(ngb.ls), unlist(lapply(ngb.ls, length))),
                          j = unlist(ngb.ls),
                          x = T)
  
  # Make Graph From ngb matrix
  graph <- graph_from_adjacency_matrix(ngb.mat, mode = "undirected", weighted = NULL, 
                                       diag = F)
  
  # Add vertex attributes
  vertex_attr(graph, name = "x") <- coordinates(pts)[,1]
  vertex_attr(graph, name = "y") <- coordinates(pts)[,2]
  
  # Add other data
  if(class(poly) == "SpatialPolygonsDataFrame"){
    for(v in colnames(poly@data)){
      vertex_attr(graph, name = v) <- poly@data[,v]
    }
  }
  
  # Plot
  if(plot.result){
    asp <- (max( coordinates(pts)[, 2]) - min( coordinates(pts)[, 
                                                                2]))/(max( coordinates(pts)[, 1]) - min( coordinates(pts)[, 
                                                                                                                          1]))
    plot(graph, vertex.size = 0.25, vertex.label = NA, 
         layout = cbind(vertex_attr(graph, name = "x"), vertex_attr(graph, name = "y")), 
         asp = asp)
  }
  
  # Return
  return(graph)
}

# Edges to SpaTialLines
edges2sl <- function(g){
  end.mat <- ends(g, E(g))
  SpatialLines(lapply(1:nrow(end.mat), function(e){
    Lines(list(Line(cbind(vertex_attr(g, name = "x", index = end.mat[e,]),
                          vertex_attr(g, name = "y", index = end.mat[e,])))), ID = as.character(e))
  }))
}

# Intersect Lines with rivers, etc.
intersect_lines <- function(sl, sl2, aggvar, aggfun = function(x){max(x)}, na.val = 0, only.uneven = T){
  # Get decent row.names to be able to work with intersection points
  row.names(sl) <- as.character(1:length(sl))
  row.names(sl2) <- as.character(1:length(sl2))
  
  ### Intersect
  sl.int <- gIntersects(sl, sl2, byid = T, returnDense =  F)
  
  ### Count number of intersections by line x river combination
  sl.int.pts <- gIntersection(sl2, sl, byid = T)
  sl.int.pts.id <- apply(do.call(rbind, strsplit(row.names(sl.int.pts), " ")), 2, as.numeric)
  
  ### Generate aggregate of variable of sl2
  agg.vec <- unlist(lapply(1:length(sl), function(i){
    r.i <- sl.int[[i]]
    if(length(r.i) == 0){ ## No single crossing
      na.val
    } else { ## Keep only those with an *uneven* number of crossings
      r.ids <- unlist(lapply(r.i, function(j){
        if(only.uneven & sum(sl.int.pts.id[,1] == j & sl.int.pts.id[,2] == i) %% 2 == 0){
          NULL
        } else {
          j
        }
      }))
      if(length(r.ids) == 0){
        na.val
      } else {
        aggfun(sl2@data[r.ids, aggvar])
      }
    }
  }))
  
  # Return
  return(agg.vec)
}


graph_from_adjacency_list <- function (adjlist,
                                       mode = c("directed", "undirected", "max", 
                                                "min", "upper", "lower", "plus"), 
                                       weighted = NULL, diag = TRUE, 
                                       add.colnames = NULL, add.rownames = NA) 
{
  if (inherits(adjmatrix, "Matrix")) {
    res <- graph.adjacency.sparse(adjmatrix, mode = mode, 
                                  weighted = weighted, diag = diag)
  }
  else {
    res <- graph.adjacency.dense(adjmatrix, mode = mode, 
                                 weighted = weighted, diag = diag)
  }
  if (is.null(add.colnames)) {
    if (!is.null(colnames(adjmatrix))) {
      add.colnames <- "name"
    }
    else {
      add.colnames <- NA
    }
  }
  else if (!is.na(add.colnames)) {
    if (is.null(colnames(adjmatrix))) {
      warning("No column names to add")
      add.colnames <- NA
    }
  }
  if (is.null(add.rownames)) {
    if (!is.null(rownames(adjmatrix))) {
      add.rownames <- "name"
    }
    else {
      add.colnames <- NA
    }
  }
  else if (!is.na(add.rownames)) {
    if (is.null(rownames(adjmatrix))) {
      warning("No row names to add")
      add.rownames <- NA
    }
  }
  if (!is.na(add.rownames) && !is.na(add.colnames) && add.rownames == 
      add.colnames) {
    warning("Same attribute for columns and rows, row names are ignored")
    add.rownames <- NA
  }
  if (!is.na(add.colnames)) {
    res <- set_vertex_attr(res, add.colnames, value = colnames(adjmatrix))
  }
  if (!is.na(add.rownames)) {
    res <- set_vertex_attr(res, add.rownames, value = rownames(adjmatrix))
  }
  res
}
