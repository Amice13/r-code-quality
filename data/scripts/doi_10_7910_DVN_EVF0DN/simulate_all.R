#################################
# RAILROAD SIMULATION
#' cd Projects/rshapes
#' nohup Rscript scripts/simulation/simulate_all.R  >simulate_all.log>&1 &
#' 
#' Forced termination of running MC, e.g. via: pkill -f slaveRSOCK
#################################


# Globals

## Variables
DEBUG <- FALSE
NCORE <- 10
RES <- 1
FACTOR <- 4
YRS <- 1834:1921
BUDGET_DEFLATE <- 2
SPEEDUP <- 10 ## from 6 to 60km/h

## Paths

### Cshapes data
cshp.path <- "analysis_data/cshapes_1816_2017.rds"

### Rshapes shapefile
rs.path <- "analysis_data/RShapes.geojson"

### Baseraster
popr.path <- "analysis_data/popc_1830AD.asc"

### Path for saving
out.path <- "analysis_data/simulation"
log.path <- file.path(out.path)

# INIT ################
library(sf)
library(sp)
library(rgeos)
library(rgdal)
library(geosphere)
library(BoostGraph) ## See package under scripts/simulation
library(parallel)
library(foreach)
library(doParallel)
library(velox) ### devtools::install_github("hunzikp/velox")
library(igraph)
library(raster)
library(BoostLines) ## install_github("lucgirardin/BoostLines")
library(plyr)
library(R6)
library(hlc) ## See package under scripts/simulation

# Functions
source("scripts/simulation/nw_functions.R")
source("scripts/simulation/simulator_class.R")

# CLUSTER #############
# Make cluster
cl <- makeCluster(getOption("cl.cores", NCORE), outfile = "error.txt")

# Load libraries
loaded <- clusterEvalQ(cl, {
  library(BoostGraph)
  library(hlc)
  library(R6)
  library(igraph)
  library(raster)
  library(sp)
  library(sf)
  library(rtree)
  library(rgeos)
  library(rgdal)
})


# Register doParallel
registerDoParallel(cl)

# Rail budget 
source("scripts/simulation/rail_budget.R")

# plot_spatial_graph(graph)

# Simulate
source("scripts/simulation/simulate.R")
