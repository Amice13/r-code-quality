#############################
# PSPM WITH EXPANDED CONTROLS
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

# Add controls
add.contr <- "+ river.norm.2 + any_river + londiff.norm + latdiff.norm + pop1880_mean.norm + elevdiff.norm + elevsd.norm "

# Additional control varialbes
if(!"latdiff" %in% edge_attr_names(graph)){
  e.ends <- ends(graph, E(graph), names = F)
  E(graph)$latdiff <- abs(V(graph)$y[e.ends[,1]] - V(graph)$y[e.ends[,2]])
  E(graph)$londiff <- abs(V(graph)$x[e.ends[,1]] - V(graph)$x[e.ends[,2]])
}

# Export stuff to cluster
clusterExport_fast(cl, c("graph", "cut_yrs"), envir = globalenv())

# Models
form.ls <- list(controls_ldv_nocontr = as.formula(paste("cshp",
                                 " ~ ", " + abram_sum + ",
                                 "cshp_lag + cuteth_lag")),
                controls_xsec_nocontr = as.formula(paste("cshp",
                                 " ~ ",
                                 "cuteth")),
                controls_ldv_addcontr = as.formula(paste("cshp",
                                 " ~ ", main_cov_spec, add.contr, " + abram_sum + ",
                                 "cshp_lag + cuteth_lag")),
                controls_xsec_addcontr = as.formula(paste("cshp",
                                 " ~ ", main_cov_spec, add.contr, " + ",
                                 "cuteth")))
  


# Fit Pseudo MLE: short, only physical geography
print(Sys.time())
print(paste("MODEL ESTIMATION"))
model.ls <- parLapply(cl, form.ls, function(f){
  set.seed(1)
  print("Estimate")
  print(Sys.time())
  
  ## Make Graph list (~ wide to long)
  graph.ls <- lapply(cut_yrs, function(y){
    print(y)
    ## rename treatment and outcome
    V(graph)$cshp <- vertex_attr(graph, paste0("cshp_", y))
    E(graph)$river.norm.2 <- E(graph)$river.norm^2
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
saveRDS(model.ls, file = file.path(tem_res.path, "controls_model.ls.rds"))
# model.ls <- readRDS(file.path(tem_res.path, "controls_model.ls.rds"))

# Reset Models
for(m in seq_along(model.ls)){
  for(s in seq_along(model.ls[[m]]$pspm_ls)){
    model.ls[[m]]$pspm_ls[[s]] <- reset_pspm(model.ls[[m]]$pspm_ls[[s]])
  }
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

saveRDS(se.ls, file = file.path(tem_res.path, "controls_se.ls.rds"))

# Clean up model.ls to save space
model.ls <- lapply(model.ls, function(m){
  m$pspm_ls  <- NULL
  m$learn_obj <- NULL
  m$g_ls <- NULL
  m
})
saveRDS(model.ls, file = file.path(tem_res.path, "controls_model.ls.rds"))
# se.ls <- readRDS(file.path(tem_res.path, "controls_se.ls.rds"))

  
# STOP CLUSTER
if(stop.cl){
  stopCluster(cl)
  rm(cl)
}


