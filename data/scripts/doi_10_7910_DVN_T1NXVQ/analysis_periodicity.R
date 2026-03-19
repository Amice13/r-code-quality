#############################
# PSPM WITH VARYING TEMPORAL STRUCTURES
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



# ESTIMATE MODEL #########

# Periodicity
periodicity <- c(5, 15, 25, 35, 45, 55, 65)

# Models
form.ls <- list(pool_ldv = as.formula(paste("cshp",
                                            " ~ ", main_cov_spec, " + abram_sum + ",
                                            "cshp_lag + cuteth_lag")),
                pool_xsec = as.formula(paste("cshp",
                                             " ~ ", main_cov_spec, " + ",
                                             "cuteth")))

## Specifications
spec.ls <- unlist(lapply(form.ls, function(f){
  lapply(periodicity, function(p){
    list(periodicity = p, fun = f)
  })
}), recursive = F)

# Export stuff to cluster
clusterExport_fast(cl, c("graph", "spec.ls"), envir = globalenv())

# Load Data and Fit Model for each specification
print(paste("MODEL ESTIMATION"))
model.ls <- parLapply(cl, seq_along(spec.ls), function(i){
  print("Prepare Data")
  print(Sys.time())
  
  # Make periods
  p <- spec.ls[[i]]$periodicity
  t.cut_freq <- p
  t.cut_yrs <- seq(1886, 2017, by = t.cut_freq)
 
  
  # Setup pooed structure (~ wide to long)
  graph.ls <- lapply(t.cut_yrs, function(y){
    ## rename treatment and outcome
    V(graph)$cshp <- vertex_attr(graph, paste0("cshp_", y))
    E(graph)$cuteth <- edge_attr(graph, paste0("cuteth", t.cut_yrs[which(t.cut_yrs == y)]))
    if(y != t.cut_yrs[1]){
      E(graph)$cshp_lag <- edge_attr(graph, paste0("cshp_", t.cut_yrs[which(t.cut_yrs == y) -1]))
      E(graph)$cuteth_lag <- edge_attr(graph, paste0("cuteth", t.cut_yrs[which(t.cut_yrs == y) -1]))
    }
    
    ## Return
    graph
  })
  
  ## Drop first entry in ldv model
  f <- spec.ls[[i]]$f
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
                          return_pspm = T,
                          return_g_ls = F)
  
  # Save shift
  m$spec <- f
  m$cut_freq = t.cut_freq
  m$cut_yrs = t.cut_yrs
  
  
  # Return
  m
})         
saveRDS(model.ls, file = file.path(tem_res.path, "periodcheck_model.ls.rds"))
# model.ls <- readRDS(file.path(tem_res.path, "periodcheck_model.ls.rds"))

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
  se.ls[[i]] <- model.ls[[i]]$learn_obj$par_bootstrap_composite_log_likelihood(cl = cl, 
                                                                               n_boot_iter = n_boot_iter, burnin = burnin,
                                                                               ci_level = 0.95,
                                                                               return_sims = T, cache_samples = F)
}

saveRDS(se.ls, file = file.path(tem_res.path, "periodcheck_se.ls.rds"))

# Clean up model.ls to save space
model.ls <- lapply(model.ls, function(m){
  m$pspm_ls  <- NULL
  m$learn_obj <- NULL
  m$g_ls <- NULL
  m
})
saveRDS(model.ls, file = file.path(tem_res.path, "periodcheck_model.ls.rds"))


# STOP CLUSTER
if(stop.cl){
  stopCluster(cl)
  rm(cl)
}


