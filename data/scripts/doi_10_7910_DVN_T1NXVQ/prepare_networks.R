#############################
# PREPARE MAIN SPATIAL LATTICES
# 
# This file generates the main baseline lattices for the analysis, with 
# different resolutions, coverage, and structures. 
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
#' Run as
#' cd Projects/state_tess
#' nohup Rscript scripts/data_prep/prepare_networks.R  >logs/prepnw_20230302.log>&1 &

# Remove everything
rm(list = ls())

# GLOBALS ##
save_dir <- "data/analysis_data/new"
param_ls <- list(
  list(nwtype = "hexagonal", res = 50),
  list(nwtype = "hexagonal", res = 75),
  list(nwtype = "hexagonal", res = 100),
  list(nwtype = "triangular", res = 100),
  list(nwtype = "quad4", res = 100),
  list(nwtype = "random", res = 100),
  list(nwtype = "hexagonal", res = 125),
  list(nwtype = "hexagonal", res = 150),
  list(nwtype = "hexagonal", res = 175),
  list(nwtype = "hexagonal", res = 200)
)

cont_param_ls <- list(
  list(nwtype = "hexagonal", res = 100,
       cont_long = "Europe", cont_short = "eu"),
  list(nwtype = "hexagonal", res = 100,
       cont_long = "Asia", cont_short = "as"),
  list(nwtype = "hexagonal", res = 100,
       cont_long = "Africa", cont_short = "af"),
  list(nwtype = "hexagonal", res = 100,
       cont_long = "South_America", cont_short = "sa"),
  list(nwtype = "hexagonal", res = 100,
       cont_long = "North_America", cont_short = "na")
)


ncore = length(param_ls)

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

# CLUSTER ###

## Setup cluster
cluster.report <- file.path(save_dir, "cluster_progress.txt")
if(file.exists(cluster.report)){
  unlink(cluster.report)
}
cl <- makeCluster(getOption("cl.cores", ncore), 
                  outfile =  cluster.report)
registerDoParallel(cl)

## Init Cluster
init.ls <- clusterEvalQ(cl = cl, expr = {
 
  # Packages 
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
  
  source(file.path("scripts/data_prep", "make_realnw_functions.R"))
  source(file.path("scripts/functions/data_mgt/load_hydrobasins.R"))
  source(file.path("scripts/functions/data_mgt/load_fao.R"))
  
  ## Expansion factors
  exp_factor.df <- readRDS(file.path("data/analysis_data", "res_optim.rds"))
  
  NULL
})


## Load network coverage
print("MAIN NETWORKS EUROPE")

### Continents
coverage <- readOGR(file.path("data", "geography/continents"),
                    "continents")

### Subset to EU
coverage <- coverage[coverage$continent == "Europe",]

## Export objects
export.ls <- c("param_ls", "save_dir", "coverage")
clusterExport(cl, export.ls)

# PREPARE NETWORKS ####
result <- foreach(i = 1:length(param_ls),
                  # .errorhandling = "remove",
                  .noexport = export.ls,
                  .options.multicore =  list(preschedule = FALSE)) %dopar% {
                    print(i)
                    ## Set random seed
                    set.seed(1)
                    
                    ## Get expansion factor
                    exp_factor <- exp_factor.df$exp_factor[exp_factor.df$continent == "Europe" &
                                                             exp_factor.df$edge_length == param_ls[[i]][["res"]] &
                                                             exp_factor.df$grid.type == param_ls[[i]][["nwtype"]]]
                    
                    ## Make netowrk
                    g <- make_realnw_wrapper(coverage = coverage,
                                             keep.cowcode = NULL, drop.cowcode = NULL, extend.cov = T, ### Spatial Coverage parameters
                                             resolution = param_ls[[i]][["res"]],
                                             grid.type = param_ls[[i]][["nwtype"]], #c("quad4", "quad8", "hexagonal"),
                                             predictors = c("rivers","watersheds", "elevation", "population"),
                                             min.river.size = 1, watershed_level = "07",
                                             exp_factor = exp_factor,
                                             na.rm = F,
                                             predictor.data.path = file.path("data"))


                    saveRDS(g, file = file.path(save_dir,
                                                paste0("nw_",
                                                       param_ls[[i]][["nwtype"]], "_",
                                                       param_ls[[i]][["res"]],
                                                       ".rds")))
                    TRUE

                  }




# Networks by Continent
print("MAIN NETWORKS REST OF THE WORLD ")

## Continents
coverage <- readOGR(file.path("data", "geography/continents"),
                    "continents")

## Crop Asia at -180
asia <-
  raster::crop(coverage[coverage$continent == "Asia",],
               as(extent(0, 180, -180, 180), "SpatialPolygons"))
coverage <- rbind(coverage[coverage$continent != "Asia",],
                  asia)


## Export objects
export.ls <- c("cont_param_ls", "save_dir", "coverage")
clusterExport(cl, export.ls)

## Prepare
result <- foreach(i = 1:length(cont_param_ls), 
                  .noexport = export.ls,
                  .options.multicore =  list(preschedule = FALSE)) %dopar% {
                    print(i)
                    ## Get expansion factor
                    exp_factor <- exp_factor.df$exp_factor[exp_factor.df$continent == cont_param_ls[[i]][["cont_long"]] &
                                                             exp_factor.df$edge_length == cont_param_ls[[i]][["res"]] &
                                                             exp_factor.df$grid.type == cont_param_ls[[i]][["nwtype"]]]
                    
                    ## Make network
                    g <- make_realnw_wrapper(coverage = coverage[coverage$continent == cont_param_ls[[i]][["cont_long"]],], 
                                             keep.cowcode = NULL, drop.cowcode = NULL, extend.cov = T, ### Spatial Coverage parameters
                                             resolution = cont_param_ls[[i]][["res"]], 
                                             grid.type = cont_param_ls[[i]][["nwtype"]], #c("quad4", "quad8", "hexagonal"),
                                             predictors = c("rivers","watersheds", "elevation", "population", "GREG"),
                                             min.river.size = 1, watershed_level = "07",
                                             exp_factor = exp_factor,
                                             na.rm = F,
                                             continent = cont_param_ls[[i]][["cont_short"]],
                                             predictor.data.path = file.path("data"))
                    
                    
                    saveRDS(g, file = file.path(save_dir, 
                                                paste0("nw_", 
                                                       cont_param_ls[[i]][["nwtype"]], "_", 
                                                       cont_param_ls[[i]][["cont_short"]], 
                                                       ".rds")))
                    TRUE
                    
                  }



stopCluster(cl)




