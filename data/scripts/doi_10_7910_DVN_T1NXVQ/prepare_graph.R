#######################################
# PREPARE GRAPH FOR ANALYSIS FUNCTION
#######################################
prepare_graph <- function(graph, 
                          save = NULL, ## Save graph and some additional files as RData?
                          language_dist = FALSE, ## Compute linguistic distances?
                          keep_all_map_data = FALSE, ## Keep all data?
                          cut_yrs = seq(1886, 2017, by = 25)
                          ){
  
  ## Delete Northern areas, which produce to errors due to limited coverage in some data
  graph <- delete_vertices(graph, which(V(graph)$y > 71.6))
  
  ## Load continents
  continent.shp <- readOGR("data/geography/continents", "continents")
  europe.shp <- continent.shp[continent.shp$continent == "Europe",]
  
  ## Load cshapes
  cshp.all <- cshp(date = as.Date("2000-01-01"))
  cshp.all <- as(cshp.all, "Spatial")
  cshp <- cshp.all[cshp.all$gwcode %in% 200:395 & !cshp.all$gwcode %in% c(365, 371:373),]
  
  ## Correct watersheds for missings
  E(graph)$watershed_diff[is.na(E(graph)$watershed_diff)] <- 0
  E(graph)$any_watershed <- ifelse(E(graph)$watershed_diff > 3, 1 , 0)
  
  ## Set river dummy
  E(graph)$any_river <- ifelse(E(graph)$river > 5, 1 , 0)
  
  ## Latitude and logitude differences
  e.ends <- ends(graph, E(graph), names = F)
  E(graph)$latdiff <- abs(V(graph)$y[e.ends[,1]] - V(graph)$y[e.ends[,2]])
  E(graph)$londiff <- abs(V(graph)$x[e.ends[,1]] - V(graph)$x[e.ends[,2]])
  
  ## Correct population for missing (mean among neighbours)
  fun.ls <- list(max = max, min = min, mean = mean, diff = function(x, na.rm = F){r <- abs(x[1] - x[2]); if(!na.rm | !is.na(r)){r} else {0}})
  e.ends <- ends(graph, E(graph), names = F)
  pop.vars <- paste0("pop", seq(1880, 2000, by = 10))
  vert.pop <- do.call(cbind, lapply(pop.vars, function(v){vertex_attr(graph, v)}))
  colnames(vert.pop) <- pop.vars
  for(v in pop.vars){
    for(f in 1:length(fun.ls)){
      edge_attr(graph, paste0(v,"_", names(fun.ls)[f])) <- 
        as.vector(apply(cbind(vert.pop[e.ends[,1], v], vert.pop[e.ends[,2], v]), 1, fun.ls[[f]], na.rm = T))
      edge_attr(graph, paste0(v,"_", names(fun.ls)[f]))[is.na(edge_attr(graph, paste0(v,"_", names(fun.ls)[f]))) | 
                                                          is.infinite(edge_attr(graph, paste0(v,"_", names(fun.ls)[f])))] <- 0
    }
  }
  
  
  ## Add Abramson States
  vert.pts <- SpatialPoints(cbind(V(graph)$x, V(graph)$y))
  abram.yrs <- c(seq(1100, 1700, by = 100), 1790)
  abram.path <- "/nastac/data/abramsonmax"
  
  ### Get ID of vertices
  v.abram.mat <- do.call(cbind, lapply(abram.yrs, function(y){
    print(y)
    if(file.exists(file.path(abram.path, paste0(y, "_poly.shp")))){
      shp <- readOGR(abram.path,
                     paste0(y, "_poly"))
    } else {
      shp <- readOGR(abram.path,
                     paste0("poly_", y))
    }
    e.ls <- gWithin( vert.pts, shp,byid = T, returnDense = F)
    sapply(e.ls, function(x){
      if(is.null(x)){
        NA
      } else {
        x[1]
      }
    })
  }))
  
  ### Transfer to edge
  e.abram.mat <- apply(v.abram.mat, 2, function(v){
    as.numeric(v[e.ends[,1]] != v[e.ends[,2]])
  })
  
  ### Encode on graph
  for(y in abram.yrs){
    edge_attr(graph, paste0("abram_", y)) <- e.abram.mat[, which(abram.yrs == y)]
  }
  edge_attr(graph, paste0("abram_", "sum")) <- rowSums(e.abram.mat, na.rm = T) / 
    apply(e.abram.mat, 1, function(x){sum(!is.na(x))})
  
  
  ## Add Euratlas 1st level divisions
  vert.pts <- SpatialPoints(cbind(V(graph)$x, V(graph)$y))
  euratlas.yrs <- seq(1800, 1900, by = 100)
  euratlas.path <- "/nastac/data/euratlas"
  
  ### Get ID of vertices
  v.euratlas.mat <- do.call(cbind, lapply(euratlas.yrs, function(y){
    print(y)
    shp <- readOGR(euratlas.path,
                   paste0("EURATLAS_2ND_LEVEL_DIV_", y))
    shp <- spTransform(shp,  "+proj=longlat +datum=WGS84 +no_defs +ellps=WGS84 +towgs84=0,0,0")
    e.ls <- gWithin( vert.pts, shp,byid = T, returnDense = F)
    sapply(e.ls, function(x){
      if(is.null(x)){
        NA
      } else {
        x[1]
      }
    })
  }))
  
  ### Transfer to edge
  e.euratlas.mat <- apply(v.euratlas.mat, 2, function(v){
    as.numeric(v[e.ends[,1]] != v[e.ends[,2]])
  })
  
  ### Encode on graph
  for(y in euratlas.yrs){
    edge_attr(graph, paste0("eurat2lev_", y)) <- e.euratlas.mat[, which(euratlas.yrs == y)]
  }

  
  ## Add cshapes
  vert.pts <- SpatialPoints(cbind(V(graph)$x, V(graph)$y))
  cshapes.yrs <- sort(unique(c(1886, 1910, 1920, 1930, 1946, 1950, 1989, 2000, 2017,
                               cut_yrs)))
  cshapes.shp <- readOGR("data/geography/cshapes_2_shapefile/cshapes_2_cow.geojson",
                         paste0("cshapes_2_cow"))
  cshapes.shp$startdate <- as.Date(cshapes.shp$start)
  cshapes.shp$enddate <- as.Date(cshapes.shp$end)
  
  ### Get ID of vertices
  v.cshapes.mat <- do.call(cbind, lapply(cshapes.yrs, function(y){
    print(y)
    shp <- cshapes.shp[cshapes.shp$startdate <= as.Date(paste0(y, "-01-01")) & 
                         cshapes.shp$enddate >= as.Date(paste0(y, "-01-01")), ]
    shp <- spTransform(shp,  "+proj=longlat +datum=WGS84 +no_defs +ellps=WGS84 +towgs84=0,0,0")
    e.ls <- gWithin(vert.pts, shp,byid = T, returnDense = F)
    res <- sapply(e.ls, function(x){
      if(is.null(x)){
        NA
      } else {
        x[1]
      }
    })
    shp$cowcode[res]
  }))
  
  ### Transfer to edge
  e.cshapes.mat <- apply(v.cshapes.mat, 2, function(v){
    as.numeric(v[e.ends[,1]] != v[e.ends[,2]])
  })
  
  ### Encode on graph
  for(y in cshapes.yrs){
    vertex_attr(graph, paste0("cshp_", y)) <- v.cshapes.mat[, which(cshapes.yrs == y)]
    edge_attr(graph, paste0("cshp_", y)) <- e.cshapes.mat[, which(cshapes.yrs == y)]
  }
  

  ## Add ethnic maps
  
  ### Load language tree if needed
  if(language_dist){
    require(LEDA)
    leda <- readRDS("data/geography/HEG/leda.rds")
  }
  
  ### Load all HEG
  eth.meta.df <- read.csv("data/geography/HEG/heg_meta.csv")
  heg.shp <- readOGR("data/geography/HEG/heg_full.geojson",
                     paste0("heg_full"))
  
  ### Add to Edges
  e.ethmap.mat <- NULL
  e.lingdist.mat <- NULL
  for(m in eth.meta.df$file_name){
    print(m)
    # Subset
    shp <- heg.shp[heg.shp$file_name == m, ]
    
    # Buffer to avoid errors
    shp <- gBuffer(shp, width = 0, byid = T)
    
    # Intersect
    e.ls <- gWithin(vert.pts, shp,byid = T, returnDense = F)
    
    # Project onto edges
    e.ethmap.mat <- cbind(e.ethmap.mat, sapply(1:nrow(e.ends), function(x){
      v1 = unique(shp$group[e.ls[[e.ends[x,1]]]])
      v2 = unique(shp$group[e.ls[[e.ends[x,2]]]])
      if(length(v1) == 0 | length(v2) == 0 ){
        NA
      } else {
        max(c(mean(!v1 %in% v2), mean(!v2 %in% v1)))
      }
    }))
    
    ## Linguistic distance
    if(language_dist){
      dist <- leda$ling_distance(lists.a = list(type = "NASTAC", map = m),
                                 lists.b = list(type = "NASTAC", map = m), 
                                 by.country = FALSE, level = "dialect",
                                 add_listmetadata = FALSE)
      e.lingdist.mat <- cbind(e.lingdist.mat, sapply(1:nrow(e.ends), function(x){
        v1 = unique(shp$group[e.ls[[e.ends[x,1]]]])
        v2 = unique(shp$group[e.ls[[e.ends[x,2]]]])
        comb.mat <- expand.grid(a.group = v1, 
                                b.group = v2)
        submat <- dist[dist$a.group %in% v1 &
                         dist$b.group %in% v2, ]
        if(nrow(submat) == 0 ){
          NA
        } else {
          max(c(mean(aggregate(submat$distance,
                               list(submat$a.group), FUN = min)$x),
                mean(aggregate(submat$distance,
                               list(submat$b.group), FUN = min)$x)))
        }
      }))
    }
  }
  colnames(e.ethmap.mat) <- eth.meta.df$file_name
  if(language_dist){
    colnames(e.lingdist.mat) <- eth.meta.df$file_name
  }
  
  ### Make year buckets:
  cut.buck.ls <- lapply(cut_yrs, function(y){
    c((y-50):(y-1))
  })
  names(cut.buck.ls) <- paste0("cuteth", cut_yrs)
  year.buck.ls <- cut.buck.ls
  
  
  ### Add bucket aggregates to Edges and Vertices
  for(i in seq_along(year.buck.ls)){
    ## Maps
    these.ids <- which(eth.meta.df$year_data_end %in% year.buck.ls[[i]])
    these.maps <- eth.meta.df$file_name[these.ids]
    ## mean by Edge
    edge_attr(graph, names(year.buck.ls)[i]) <- apply(e.ethmap.mat[,these.maps, drop = F], 1, mean, na.rm = T)
    
    ## Language distance
    if(language_dist){
      edge_attr(graph, paste0(names(year.buck.ls)[i], "_ld")) <- 
                  apply(e.lingdist.mat[,these.maps, drop = F], 1, mean, na.rm = T)

    }
  }
  
  ## Keep all ethnic maps
  if(keep_all_map_data){
    for(m in eth.meta.df$file_name){
      ## Dummy by Edge
      edge_attr(graph, paste0("ethmap_", m)) <- e.ethmap.mat[,m]
    }
  }
  
  ## Edge Length in km
  library(geosphere)
  lines <- edges2sl(graph)
  E(graph)$length <- geosphere::lengthLine(lines) / 1e3
  
  ## Normalize Variables
  norm.vars <- c("length", "river", "watershed_diff", "elevmean",
                 "londiff", "latdiff", "pop1880_mean", "elevdiff", "elevsd")
  for(v in norm.vars){
    edge_attr(graph, paste0(v, ".norm")) <- (edge_attr(graph, v) - min(edge_attr(graph, v), na.rm = T))
    edge_attr(graph, paste0(v, ".norm")) <- edge_attr(graph, paste0(v, ".norm")) / 
      max(edge_attr(graph, paste0(v, ".norm")), na.rm = T)
  }
  
  ## Crop graph
  
  ### Geographic definition of Europe
  graph <- crop_graph(g = graph, x = europe.shp, delete.islands = F)
  
  ## Continents for plotting
  plot.europe.shp <- raster::crop(europe.shp, 
                                  extent(SpatialPoints(cbind(vertex_attr(graph, "x"),vertex_attr(graph, "y")))))
  
  plot.europe.diff.shp <- gDifference(as(extent(plot.europe.shp), "SpatialPolygons"),
                                      plot.europe.shp)
  
  # Drop vertices outside cshapes
  graph <- delete_vertices(graph,
                           which(rowSums(do.call(cbind, lapply(cshapes.yrs, 
                                                               function(y){is.na(vertex_attr(graph, paste0("cshp_", y)))}))) > 0))
  
  
  
  # Save
  if(!is.null(save)){
    save(graph, europe.shp, plot.europe.shp, plot.europe.diff.shp, cshp,
         file = save)
  }
  
  # Return
  return(graph)
  
}



# Simplified, continent-wide version for continent graphs
prepare_continent_graph <- function(graph){
  
  ## Delete Northern areas out of coverage and produces errors
  graph <- delete_vertices(graph, which(V(graph)$y > 71.6))
  
  ## Correct watersheds for missings
  E(graph)$watershed_diff[is.na(E(graph)$watershed_diff)] <- 0
  E(graph)$any_watershed <- ifelse(E(graph)$watershed_diff > 3, 1 , 0)
  
  ## Set river dummy
  E(graph)$any_river <- ifelse(E(graph)$river > 5, 1 , 0)
  
  ## Latitude and logitude differences
  e.ends <- ends(graph, E(graph), names = F)
  E(graph)$latdiff <- abs(V(graph)$y[e.ends[,1]] - V(graph)$y[e.ends[,2]])
  E(graph)$londiff <- abs(V(graph)$x[e.ends[,1]] - V(graph)$x[e.ends[,2]])
  
  ## Correct population for missing (mean among neighbours)
  fun.ls <- list(max = max, min = min, mean = mean, diff = function(x, na.rm = F){
    r <- abs(x[1] - x[2]); if(!na.rm | !is.na(r)){r} else {0}
  })
  e.ends <- ends(graph, E(graph), names = F)
  pop.vars <- paste0("pop", seq(1880, 2000, by = 10))
  vert.pop <- do.call(cbind, lapply(pop.vars, function(v){vertex_attr(graph, v)}))
  colnames(vert.pop) <- pop.vars
  for(v in pop.vars){
    for(f in 1:length(fun.ls)){
      edge_attr(graph, paste0(v,"_", names(fun.ls)[f])) <- 
        as.vector(apply(cbind(vert.pop[e.ends[,1], v], vert.pop[e.ends[,2], v]), 1, fun.ls[[f]], na.rm = T))
      edge_attr(graph, paste0(v,"_", names(fun.ls)[f]))[is.na(edge_attr(graph, paste0(v,"_", names(fun.ls)[f]))) | 
                                                          is.infinite(edge_attr(graph, paste0(v,"_", names(fun.ls)[f])))] <- 0
    }
  }
  

  ## Add cshapes
  vert.pts <- SpatialPoints(cbind(V(graph)$x, V(graph)$y))
  cshapes.yrs <- sort(unique(c(1964, 2017)))
  cshapes.shp <- readOGR("data/geography/cshapes_2_shapefile/cshapes_2_cow.geojson",
                         paste0("cshapes_2_cow"))
  cshapes.shp$startdate <- as.Date(cshapes.shp$start)
  cshapes.shp$enddate <- as.Date(cshapes.shp$end)
  
  
  ### Get ID of vertices
  v.cshapes.mat <- do.call(cbind, lapply(cshapes.yrs, function(y){
    print(y)
    shp <- cshapes.shp[cshapes.shp$startdate <= as.Date(paste0(y, "-01-01")) & 
                         cshapes.shp$enddate >= as.Date(paste0(y, "-01-01")), ]
    shp <- spTransform(shp,  "+proj=longlat +datum=WGS84 +no_defs +ellps=WGS84 +towgs84=0,0,0")
    e.ls <- gWithin(vert.pts, shp,byid = T, returnDense = F)
    res <- sapply(e.ls, function(x){
      if(is.null(x)){
        NA
      } else {
        x[1]
      }
    })
    shp$cowcode[res]
  }))
  
  ### Transfer to edge
  e.cshapes.mat <- apply(v.cshapes.mat, 2, function(v){
    as.numeric(v[e.ends[,1]] != v[e.ends[,2]])
  })
  
  ### Encode on graph
  for(y in cshapes.yrs){
    vertex_attr(graph, paste0("cshp_", y)) <- v.cshapes.mat[, which(cshapes.yrs == y)]
    edge_attr(graph, paste0("cshp_", y)) <- e.cshapes.mat[, which(cshapes.yrs == y)]
  }
  
  
  ## Edge Length if not there yet
  if(!"length" %in% edge_attr_names(graph)){
    library(geosphere)
    lines <- edges2sl(graph)
    E(graph)$length <- geosphere::lengthLine(lines) / 1e3
  }
  
  ## Normalize Variables
  norm.vars <- c("length", "river", "watershed_diff", "elevmean",
                 "londiff", "latdiff", "pop1880_mean", "elevdiff", "elevsd")
  for(v in norm.vars){
    edge_attr(graph, paste0(v, ".norm")) <- (edge_attr(graph, v) - min(edge_attr(graph, v), na.rm = T))
    edge_attr(graph, paste0(v, ".norm")) <- edge_attr(graph, paste0(v, ".norm")) / 
      max(edge_attr(graph, paste0(v, ".norm")), na.rm = T)
  }
  
  # Return
  return(graph)
  
}
