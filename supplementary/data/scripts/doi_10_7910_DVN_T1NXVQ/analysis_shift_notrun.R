##################################
# PSPM WITH SHIFTING LATTICE
#
# ### DO NOT RUN THIS FILE - IT NEEDS "DEEP" INPUT DATA NOT INCLUDED IN THE 
# ### REPLICATION -- RESULTS ARE SAVED AS file.path(tem_res.path, "shiftcheck_model.ls.rds")
# 
# Part of:
# Shaping States into Nations: The Effects of Ethnic Geography on State Borders
# Müller-Crepon, Schvitz, Cederman
# Replication files
#
# Called from scripts/analysis/analysis_all.R
#
#############################


# LOAD GLOBALS AND DATA ##
if(!exists("LOADED_GLOBALS")){
  source("scripts/analysis/analysis_globals.R")
}
load_data(reload = F)

## Make cluster
if(!exists("cl")){
  cl <- make_cluster(ncore)
  stop.cl <- T
} else {
  stop.cl <- F
}

clusterEvalQ(cl, expr = {
  # Load Globals
  source("scripts/analysis/analysis_globals.R")
  
  # Load Continents
  coverage <- readOGR(file.path("data", "geography/continents"),
                      "continents")
  coverage <- coverage[coverage$continent == "Europe",]
  
  ## Expansion factors
  exp_factor.df <- readRDS(file.path("data/analysis_data", "res_optim.rds"))
  
  # Return
  NULL
})


# ESTIMATE MODEL #########


# Models
form.ls <- list(pool_ldv = as.formula(paste("cshp",
                                            " ~ ", main_cov_spec, " + abram_sum + ",
                                            "cshp_lag + cuteth_lag")),
                pool_xsec = as.formula(paste("cshp",
                                             " ~ ", main_cov_spec, " + ",
                                             "cuteth")))
clusterExport(cl, c("form.ls"))

# Load Data and Fit Model for each specification
print(paste("MODEL ESTIMATION"))
model.ls <- parLapply(cl, 1:100, function(i){
  print("Prepare Data")
  print(Sys.time())
  
  # Make Graph with shift
  set.seed(i)
  
  ## Shift in dec. degrees
  shift_x = runif(1, min = 0, max = 10)
  shift_y = runif(1, min = 0, max = 10)
  
  ## Get expansion factor
  exp_factor <- exp_factor.df$exp_factor[exp_factor.df$continent == "Europe" &
                                           exp_factor.df$edge_length == 100 &
                                           exp_factor.df$grid.type == "hexagonal"]

  
  ## Make network
  this.g <-  make_realnw_wrapper(coverage = coverage, 
                                 keep.cowcode = NULL, drop.cowcode = NULL, extend.cov = T, 
                                 resolution = graph.res, 
                                 grid.type = "hexagonal",
                                 predictors = c("rivers","watersheds", "elevation", "population"),
                                 min.river.size = 1, watershed_level = "07",
                                 exp_factor = exp_factor,
                                 na.rm = F,
                                 predictor.data.path = file.path("data"),
                                 shift_x = shift_x,
                                 shift_y = shift_y)
  
  # Prepare graph
  this.g <- prepare_graph(this.g)
  
  # Setup pooed structure (~ wide to long)
  graph.ls <- lapply(cut_yrs, function(y){
    ## rename treatment and outcome
    V(this.g)$cshp <- vertex_attr(this.g, paste0("cshp_", y))
    E(this.g)$cuteth <- edge_attr(this.g, paste0("cuteth", cut_yrs[which(cut_yrs == y)]))
    if(y != cut_yrs[1]){
      E(this.g)$cshp_lag <- edge_attr(this.g, paste0("cshp_", cut_yrs[which(cut_yrs == y) -1]))
      E(this.g)$cuteth_lag <- edge_attr(this.g, paste0("cuteth", cut_yrs[which(cut_yrs == y) -1]))
    }
    
    ## Return
    this.g
  })
  
  ## Drop first entry in ldv model
  m.list <- lapply(form.ls, function(f){
    if(any(grepl("_lag", as.character(f), fixed = T))){
      graph.ls[[1]] <- NULL
    }
    
    # Estimate
    set.seed(1)
    print("Estimate")
    print(Sys.time())
    m <-  fit_pspm_model(form = f, 
                            g_ls = graph.ls,
                            model_type = "composite_log_likelihood",
                            sigma2 = 10, 
                            force_contiguous = TRUE,
                            min_component_size = 2,
                            na.rm = T,
                            return_pspm = F,
                            return_g_ls = F)
    
    # Save shift
    m$shift_x = shift_x
    m$shift_y = shift_y
    
    # Reduce result to bare minimum
    res <- list(estimate = m$estimate,
         se = stdEr(m),
         shift_x = m$shift_x,
         shift_y = m$shift_y
    )
    
    # Return
    return(res)
  })
  
  
  # Return
  m.list
})          
saveRDS(model.ls, file = file.path(tem_res.path, "shiftcheck_model.ls.rds"))


# STOP CLUSTER
if(stop.cl){
  stopCluster(cl)
  rm(cl)
}


