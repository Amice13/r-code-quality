#############################
# FINAL DATA PREPARATION
# 
# This file compiles the final data used in the PSPM analysis and 
# secession analysis. 
# 
# Part of:
# Shaping States into Nations: The Effects of Ethnic Geography on State Borders
# Müller-Crepon, Schvitz, Cederman
# Replication files
#
# Called from scripts/data_prep/prepare_all.R.R
#
#############################
rm(list = ls())


# LOCALS ########################

# Network resolutions
resolutions <- seq(50, 200, by = 25)

# Network structures
structures <- c("hexagonal", "quad4", "triangular", "random")

# Continents
continents <- c("eu","as","af","na","sa")

# Periodicity of respective robustness checks
periodicity <- c(5, 15, 25, 35, 45, 55, 65)

# PACKAGES ######################
library(foreach)
library(doParallel)
library(pspm)
library(reticulate)
library(raster)
library(plyr)
library(dplyr)
library(tidyr)
library(igraph)
library(ggplot2)
library(rgeos)
library(rgdal)
library(abind)
library(cshapes)
library(BoostLines)

# FUNCTIONS #####################

# Functions
source(file.path("scripts/data_prep/", "make_realnw_functions.R"))

# Wrappers around SCCRF functions
path <- "scripts/functions"
for(f in list.files(path, recursive = T)){
  source(file.path(path, f))
}

# CLUSTER #######################

## Setup cluster
cl <- makeCluster(getOption("cl.cores", 7))
registerDoParallel(cl)
clusterExport(cl, c("resolutions", "structures", "continents", "periodicity"))

## Init Cluster
init.ls <- clusterEvalQ(cl = cl, expr = {
  # Packages 
  library(reticulate)
  Sys.setenv(RETICULATE_PYTHON = '/usr/bin/python3')
  library(pspm)
  library(raster)
  library(velox)
  library(plyr)
  library(sp)
  library(rgdal)
  library(rgeos)
  library(BoostLines)
  library(igraph)
  library(dplyr)
  library(abind)
  library(cshapes)
  library(RPostgreSQL)
  
  # Functions
  source(file.path("scripts/data_prep/", "make_realnw_functions.R"))
  path <- "scripts/functions"
  for(f in list.files(path, recursive = T)){
    source(file.path(path, f))
  }
})

# PARTITION ANALYSIS ############

# All resolutions
data.prep <- parLapply(cl, resolutions, function(res){
  ## Load raw graph
  graph <- readRDS(file.path("data/analysis_data",
                             paste0("nw_hexagonal_",res,".rds")))

  ## Save for full analysis data
  if(res == 100){
    save.path = file.path("data/analysis_data/", "maindata.Rdata")
    cut_yrs <- sort(unique(unlist(lapply(periodicity,
                                         function(p){seq(1886, 2017, by = p)}))))
  } else {
    save.path = NULL
    cut_yrs <- seq(1886, 2017, by = 25)
  }

  ## Prepare graph
  graph <- prepare_graph(graph,
                         save = save.path,
                         language_dist = (res == 100),
                         keep_all_map_data = (res == 100),
                         cut_yrs = cut_yrs)

  ## Save for simple graph
  saveRDS(graph, file.path("data/analysis_data/",
                           paste0("graph_", res, ".rds")))

  ## Return TRUE
  return(TRUE)
})

# All network structures
data.prep <- parLapply(cl, structures, function(struct){
  ## Load raw graph
  graph <- readRDS(file.path("data/analysis_data",
                             paste0("nw_", struct, "_",100,".rds")))


  ## Prepare graph
  graph <- prepare_graph(graph,
                         save = NULL,
                         language_dist = FALSE,
                         keep_all_map_data = FALSE)

  ## Save for simple graph
  saveRDS(graph, file.path("data/analysis_data/",
                           paste0("graph_", struct, ".rds")))

  ## Return TRUE
  return(TRUE)
})

# All continents
data.prep <- parLapply(cl, continents, function(cont){
  ## Load raw graph
  graph <- readRDS(file.path("data/analysis_data",
                             paste0("nw_hexagonal_",cont,".rds")))


  ## Prepare graph
  graph <- prepare_continent_graph(graph)

  ## Save for simple graph
  saveRDS(graph, file.path("data/analysis_data/",
                           paste0("graph_", cont, ".rds")))

  ## Return TRUE
  return(TRUE)
})

# AUSTRIA HUNGARY ###############

# Load Census
ah.shp <- readOGR("data/geography/census_austhung","Final_df_complete")

# Make 0 shares
eth.vars <- c("German", "Bhm_M_S", "Polish", "Ruthenn", "Slovenn", "Srb_Crt",
              "Italian", "Romanin", "Hungarn", "Other")
ah.shp@data[, eth.vars][is.na(ah.shp@data[, eth.vars])] <- 0

# Long -> Wide
for(y in c(1900, 1910)){
  e.df <- ah.shp@data[ah.shp$Year == y, c("Dstrc_N", "Land_Nm", eth.vars)]
  colnames(e.df)[-c(1:2)] <- paste0(colnames(e.df)[-c(1:2)], "_", y)
  ah.shp@data <- join(ah.shp@data, e.df, by = c("Dstrc_N", "Land_Nm"),
                      match = "first", type = "left")
}
ah.shp@data[, eth.vars] <- NULL

# Keep districts from 1900 only // i.e. only one set
ah.shp <- ah.shp[ah.shp$Year == 1900, ]

# Change to WGS 84
ah.shp <- spTransform(ah.shp, CRS("+proj=longlat +ellps=WGS84 +no_defs"))

# ID
ah.shp$ID <- 1:length(ah.shp)

# Get points
pts <- SpatialPointsDataFrame(gCentroid(ah.shp, byid = TRUE),
                              data = ah.shp@data[,c("ID"), drop = F],
                              match.ID = F)

# Make a network
ah.graph <- make_realnw_wrapper(coverage = pts,
                                keep.cowcode = NULL, drop.cowcode = NULL, extend.cov = T, ### Spatial Coverage parameters
                                resolution = NULL,
                                grid.type = NULL,
                                predictors = c("rivers","watersheds", "elevation", "population"),
                                min.river.size = 1, watershed_level = "07",
                                exp_factor = 1,
                                na.rm = F,
                                predictor.data.path = file.path("data"))

# Delete edges > 100km (exterior)
ah.graph <- delete_edges(ah.graph, which(E(ah.graph)$length > 100000))

# Add compositional distance

## Hellinger Distance
hellinger_dist <- function(x, y){
  # Normalize to probabilities
  x <- x / sum(x)
  y <- y / sum(y)

  # Merge if names
  if(!is.null(names(x)) & !is.null(names(y))){
    all.names <- unique(c(names(x), names(y)))
    x <- x[all.names]
    x[is.na(x)] <- 0
    y <- y[all.names]
    y[is.na(y)] <- 0
  } else {
    stopifnot(length(x) == length(y))
  }

  # Compute distance
  1/sqrt(2) * sqrt(sum((sqrt(x) - sqrt(y))^2))
}

## Matrix
end.mat <- ends(ah.graph, E(ah.graph), names = F)
end.mat <- cbind(V(ah.graph)$ID[end.mat[,1]],
                 V(ah.graph)$ID[end.mat[,2]])
for(y in c(1900, 1910)){
  igraph::edge_attr(ah.graph, paste0("hell.dist.", y)) <- sapply(seq_len(nrow(end.mat)), function(x){
    hellinger_dist(ah.shp@data[end.mat[x,1], paste0(eth.vars, "_", y)],
                   ah.shp@data[end.mat[x,2], paste0(eth.vars, "_", y)])
  })
  igraph::edge_attr(ah.graph, paste0("largest.diff.", y)) <- sapply(seq_len(nrow(end.mat)), function(x){
    x1 <- paste0(eth.vars, "_", y)[which.max(ah.shp@data[end.mat[x,1], paste0(eth.vars, "_", y)])[1]]
    x2 <- paste0(eth.vars, "_", y)[which.max(ah.shp@data[end.mat[x,2], paste0(eth.vars, "_", y)])[1]]
    mean(c(abs(ah.shp@data[end.mat[x,1], x1] - ah.shp@data[end.mat[x,2], x1]),
           abs(ah.shp@data[end.mat[x,2], x2] - ah.shp@data[end.mat[x,1], x2])))
  })
}
E(ah.graph)$land.diff <- as.numeric(ah.shp@data$Land_Nm[end.mat[,1]] ==
                                      ah.shp@data$Land_Nm[end.mat[,2]])


# Final Preparation
ah.graph <- prepare_graph(ah.graph,
                          save = NULL,
                          language_dist = FALSE, keep_all_map_data = FALSE,
                          cut_yrs = seq(1886, 2017, by = 25))

# Save
saveRDS(ah.graph,"data/analysis_data/graph_austhung.rds")




# SELF-DETERMINATION ############


# All resolutions
data.prep <- parLapply(cl, resolutions, function(res){
  # Load Graph
  graph <- readRDS(file.path("data/analysis_data/", 
                             paste0("graph_", res, ".rds")))
  
  # Make points
  points.df <- do.call(cbind, lapply(vertex_attr_names(graph), function(v){
    data.frame(as.matrix(vertex_attr(graph, v), ncol = 1))
  }))
  colnames(points.df) <- vertex_attr_names(graph)
  points.df$pts.id <- 1:nrow(points.df)
  
  # Prepare data
  points.yrs.df <- prep_sdm_analysis(points.df = points.df)
  
  # Save
  saveRDS(points.yrs.df, file.path("data/analysis_data/", 
                                   paste0("sdm_", res, ".rds")))
  
  ## Return TRUE
  return(TRUE)
})

# All network structures
data.prep <- parLapply(cl, structures, function(struct){
  ## Load Graph
  graph <- readRDS(file.path("data/analysis_data/", 
                             paste0("graph_", struct, ".rds")))
  
  # Make points
  points.df <- do.call(cbind, lapply(vertex_attr_names(graph), function(v){
    data.frame(as.matrix(vertex_attr(graph, v), ncol = 1))
  }))
  colnames(points.df) <- vertex_attr_names(graph)
  points.df$pts.id <- 1:nrow(points.df)
  
  # Prepare data
  points.yrs.df <- prep_sdm_analysis(points.df = points.df)
  
  # Save
  saveRDS(points.yrs.df, file.path("data/analysis_data/", 
                                   paste0("sdm_", struct, ".rds")))
  
  ## Return TRUE
  return(TRUE)
})

# Stop Cluster 
stopCluster(cl)
