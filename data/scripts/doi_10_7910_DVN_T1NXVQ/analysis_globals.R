#############################
# SET GLOBALS
# This file is run at the beginning of every analysis file. 
# 
# Part of:
# Shaping States into Nations: The Effects of Ethnic Geography on State Borders
# Müller-Crepon, Schvitz, Cederman
# Replication files
#############################

if(!exists("LOADED_GLOBALS")){
  # GLOBALS ####
  
  # Paths
  fig.path <- "results/figures"
  tab.path <- "results/tables"
  num.path <- "results/numbers"
  tem_res.path <- "data/models"
  
  
  # CREATE RESULTS FOLDERS #######
  if(!dir.exists(fig.path)){
    dir.create(fig.path, recursive = T)
  }
  if(!dir.exists(tab.path)){
    dir.create(tab.path, recursive = T)
  }
  if(!dir.exists(num.path)){
    dir.create(num.path, recursive = T)
  }
  
  # Main graph
  graph.res <- 100
  graph_path <- "data/analysis_data"

  # Frequency for temporal cuts 
  cut_freq <- 25
  cut_yrs <- seq(1886, 2017, by = cut_freq)
  
  # Number of bootstrap iterations
  n_boot_iter <- 120
  
  # Max segment size for analysis
  
  # Number of cores
  ncore <- 40
  
  # Burnin rate
  burnin = 100
  
  # Main covariate specification
  main_cov_spec <- "length.norm + river.norm + watershed_diff.norm + elevmean.norm"
  
  # INIT #######
  if(!dir.exists(tem_res.path)){
    dir.create(tem_res.path)
  }
  Sys.setenv(RETICULATE_PYTHON = '/usr/bin/python3')
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
  library(scico) 
  library(maxLik)
  library(abind)
  library(gridExtra)
  library(grid)
  library(cshapes)
  library(ggridges)
  library(survival)
  library(MASS)
  library(fasterize)
  library(sf)
  library(viridis)
  library(tidyverse)
  library(xtable)
  library(smoothr)
  library(stargazer)
  library(ggplot2)
  library(survival)
  library(MASS)

  # Functions
  source(file.path("scripts/data_prep/", "make_realnw_functions.R"))
  source(file.path("scripts/functions/data_mgt/load_hydrobasins.R"))
  source(file.path("scripts/functions/data_mgt/load_fao.R"))
  
  # Wrappers around SCCRF functions
  path <- "scripts/functions"
  for(f in list.files(path, recursive = T)){
    source(file.path(path, f))
  }
  
  # Function to make cluster
  make_cluster <- function(ncore){
    
    # CLUSTER ###
    
    ## Setup cluster
    cluster.report <- file.path("logs", "cluster_log.txt")
    if(file.exists(cluster.report)){
      unlink(cluster.report)
    }
    cl <- makeCluster(getOption("cl.cores", ncore), 
                      outfile =  file.path(cluster.report))
    registerDoParallel(cl)
    
    ## Init Cluster
    init.ls <- clusterEvalQ(cl = cl, expr = {
      # Packages 
      library(reticulate)
      Sys.setenv(RETICULATE_PYTHON = '/usr/bin/python3')
      library(pspm)
      library(raster)
      library(plyr)
      library(sp)
      library(rgdal)
      library(rgeos)
      library(igraph)
      library(dplyr)
      library(survival)
      library(MASS)
      library(lfe)
      library(ggplot2)
      library(maxLik)
      library(abind)
      library(cshapes)
      library(fasterize)
      library(sf)
      
      # Functions
      path <- "scripts/functions"
      for(f in list.files(path, recursive = T)){
        source(file.path(path, f))
      }
    })
    
    # Print
    print("Made Cluster")
    return(cl)
  }
  
  # Function to load all data
  load_data <- function(reload = FALSE, graph_path = "data/analysis_data"){
    if(!exists("LOADED_DATA", envir = .GlobalEnv) | reload){
      load(file.path(graph_path, "maindata.Rdata"), envir = .GlobalEnv)
      LOADED_DATA <<- TRUE
      print("Loaded data")
    }
  }
  
  # Aspect ratio of graphs
  get_asp_ratio <- function(g){
    endcoord.mat <-  cbind(vertex_attr(g, "x"),vertex_attr(g, "y"))
    (max(endcoord.mat[, 2]) - min(endcoord.mat[, 2]))/ (max(endcoord.mat[, 1]) - min(endcoord.mat[, 1]))
  }
  
  
  # Save we did this
  LOADED_GLOBALS <- TRUE
}
