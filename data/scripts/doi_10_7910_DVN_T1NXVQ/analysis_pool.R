#############################
# POOLED PARTITION ANALYSIS
# 
# 
# Part of:
# Shaping States into Nations: The Effects of Ethnic Geography on State Borders
# Müller-Crepon, Schvitz, Cederman
# Replication files
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

## Export stuff to cluster
clusterExport_fast(cl, c("graph", "cut_yrs"), envir = globalenv())


# ESTIMATE MODEL #########

# Models
form.ls <- list(pool_ldv = as.formula(paste("cshp",
                                 " ~ ", main_cov_spec, " + abram_sum + ",
                                 "cshp_lag + cuteth_lag")),
                pool_xsec = as.formula(paste("cshp",
                                 " ~ ", main_cov_spec, " + ",
                                 "cuteth")),
                pool_ldv_long = as.formula(paste("cshp",
                                 " ~ ", main_cov_spec, " + abram_sum + ",
                                 paste0("cshp_lag + cuteth", cut_yrs[1]))),
                pool_xsec_long = as.formula(paste("cshp",
                                 " ~ ", main_cov_spec, " + ",
                                 paste0("cuteth", cut_yrs[1]))))
  


# Fit Pseudo MLE: short, only physical geography
print(Sys.time())
print(paste("MODEL ESTIMATION"))
model.ls <- parLapply(cl, form.ls, function(f){
  set.seed(1)
  print("Estimate")
  print(Sys.time())
  
  ## Make Graph list (~ wide to long)
  graph.ls <- lapply(cut_yrs, function(y){
    ## rename treatment and outcome
    V(graph)$cshp <- vertex_attr(graph, paste0("cshp_", y))
    E(graph)$cuteth <- edge_attr(graph, paste0("cuteth", cut_yrs[which(cut_yrs == y)]))
    if(y != cut_yrs[1]){
      E(graph)$cshp_lag <- edge_attr(graph, paste0("cshp_", cut_yrs[which(cut_yrs == y) -1]))
      E(graph)$cuteth_lag <- edge_attr(graph, paste0("cuteth", cut_yrs[which(cut_yrs == y) -1]))
    }
    
    ## Return
    graph
  })
  
  ## Drop first entry in ldv model
  if(any(grepl("_lag", as.character(f), fixed = T))){
    graph.ls[[1]] <- NULL
  }
  
  ## Estimate model
  m <- fit_pspm_model(form = f, 
                         g_ls = graph.ls,
                         model_type = "composite_log_likelihood",
                         sigma2 = 10, 
                         force_contiguous = TRUE,
                         na.rm = T,
                         min_component_size = 2,
                         vertex_coords = c("x", "y"),
                         return_pspm = T, return_g_ls = T)
  print(summary(m))
  print(Sys.time())
  m
})          
saveRDS(model.ls, file = file.path(tem_res.path, "pool_model.ls.rds"))
# model.ls <- readRDS(file.path(tem_res.path, "pool_model.ls.rds"))

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
                                      return_sims = TRUE, cache_samples = T)
}

saveRDS(se.ls, file = file.path(tem_res.path, "pool_se.ls.rds"))
saveRDS(model.ls, file = file.path(tem_res.path, "pool_model.ls.rds"))

  
# STOP CLUSTER
if(stop.cl){
  stopCluster(cl)
  rm(cl)
}


