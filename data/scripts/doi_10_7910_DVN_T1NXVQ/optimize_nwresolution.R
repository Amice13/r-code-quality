#############################
# NETWORK RESOLUTION
# 
# This file optimizes the network resolution to get a given average edge-length
#
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

# PACKAGES ###
library(foreach)
library(doParallel)
library(reticulate)
library(raster)
library(cshapes)
library(velox)
library(rgdal)
library(rgeos)
library(BoostLines)
library(igraph)
library(dplyr)
library(dggridR)

# FUNCTIONS ##
source(file.path("scripts/data_prep", "make_realnw_functions.R"))
path <- "scripts/functions"
for(f in list.files(path, recursive = T)){
  source(file.path(path, f))
}

# PARAMETERS ####
param.df <- rbind(data.frame(grid.type = "hexagonal",
                       edge_length = 100,
                       continent = c("Europe","Asia","Africa","North_America","South_America")),
                  data.frame(grid.type = "hexagonal",
                             edge_length = seq(50, 200, by = 25),
                             continent = "Europe"),
                  data.frame(grid.type = c("quad4", "triangular", "random"),
                             edge_length = 100,
                             continent = "Europe"))
param.df <- unique(param.df)                 
param.df$continent <- as.character(param.df$continent)

# Coverage

## Continents
coverage <- readOGR(file.path("data", "geography/continents"),
                    "continents")

## Crop Asia at -180
asia <-
  raster::crop(coverage[coverage$continent == "Asia",],
               as(extent(0, 180, -180, 180), "SpatialPolygons"))
coverage <- rbind(coverage[coverage$continent != "Asia",],
                  asia)
coverage$continent <- as.character(coverage$continent)

# Cluster
cl <- make_cluster(ncore = nrow(param.df))

# Optimize
res <- foreach(i = seq_len(nrow(param.df)), 
               .options.multicore =  list(preschedule = FALSE),
               .packages = c("geosphere")) %dopar% {
                 set.seed(1)
                 optimize_albers_grid(shape = coverage[coverage$continent == param.df$continent[i],], 
                                      edge_length = param.df$edge_length[i], 
                                      maxit = 10, 
                                      grid.type = param.df$grid.type[i], 
                                      tol = .05)
               }

## Save
param.df$exp_factor <- sapply(res, function(x){x$par})
param.df$diff.val <- sapply(res, function(x){x$val})

## Write to disk
saveRDS(param.df, file.path("data/analysis_data", "res_optim.rds"))


## Close Cluster
stopCluster(cl)
