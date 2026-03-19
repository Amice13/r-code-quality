#############################
# PSPM WITH VARYING NETWORK STRUCTURES
# 
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
  source("scripts/analysis/analysis_globals.R")
})

# ESTIMATE MODEL #########

# Structures
structures <- c("hexagonal", "quad4", "triangular", "random")

# Models
form.ls <- list(pool_ldv = as.formula(paste("cshp",
                                            " ~ ", main_cov_spec, " + abram_sum + ",
                                            "cshp_lag + cuteth_lag")),
                pool_xsec = as.formula(paste("cshp",
                                             " ~ ", main_cov_spec, " + ",
                                             "cuteth")))
spec.ls <- unlist(lapply(form.ls, function(f){
  lapply(structures, function(s){
    list(structure = s, fun = f)
  })
}), recursive = F)


# Load Data and Fit Model for each specification
print(paste("MODEL ESTIMATION"))
model.ls <- parLapply(cl, spec.ls, function(s){
  print("Prepare Data")
  print(Sys.time())
  
  # Load Graph
  this.graph.path <- file.path("data/analysis_data",
                               paste0("graph_", s[["structure"]], ".rds"))
  this.g <- readRDS(this.graph.path)
  
  # Setup pooled structure (~ wide to long)
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
  if(any(grepl("_lag", as.character(s[["fun"]]), fixed = T))){
    graph.ls[[1]] <- NULL
  }
  
  # Estimate
  set.seed(1)
  print("Estimate")
  print(Sys.time())
  m <-  fit_pspm_model(form = s[["fun"]], 
                          g_ls = graph.ls,
                          model_type = "composite_log_likelihood",
                          sigma2 = 10, 
                          force_contiguous = TRUE,
                          min_component_size = 2,
                          na.rm = T,
                          return_pspm = T)
  print(s[["res"]])
  print(summary(m))
  print(Sys.time())
  
  # Return
  m
})          
saveRDS(model.ls, file = file.path(tem_res.path, "structcheck_model.ls.rds"))
# model.ls <- readRDS(file.path(tem_res.path, "structcheck_model.ls.rds"))

# Reset Models
for(m in seq_along(model.ls)){
  model.ls[[m]]$learn_obj <- reset_pspmlearn(model.ls[[m]]$learn_obj)
}

# Bootstrapped standard errors
print(Sys.time())
print(paste("BOOTSTRAP SEs"))
se.ls <- list()

for( i in seq_along(model.ls)){
  print("next")
  print(Sys.time())
  se.ls[[i]] <- model.ls[[i]]$learn_obj$par_bootstrap_composite_log_likelihood(
    cl = cl, 
    n_boot_iter = n_boot_iter, 
    burnin = burnin,
    ci_level = 0.95,
    return_sims = T, 
    cache_samples = F
  )
}

saveRDS(se.ls, file = file.path(tem_res.path, "structcheck_se.ls.rds"))

# Clean up model.ls to save space
model.ls <- lapply(model.ls, function(m){
  m$pspm_ls  <- NULL
  m$learn_obj <- NULL
  m$g_ls <- NULL
  m
})
saveRDS(model.ls, file = file.path(tem_res.path, "structcheck_model.ls.rds"))
# se.ls <- readRDS(file.path(tem_res.path, "structcheck_se.ls.rds"))


# STOP CLUSTER
if(stop.cl){
  stopCluster(cl)
  rm(cl)
}


