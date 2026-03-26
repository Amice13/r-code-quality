#############################
# PSPM OF AUSTRIA-HUNGARY CENSUS DATA
# 
# 
# Part of:
# Shaping States into Nations: The Effects of Ethnic Geography on State Borders
# Müller-Crepon, Schvitz, Cederman
# Replication files
#############################

# LOAD GLOBALS  ##
if(!exists("LOADED_GLOBALS")){
  source("scripts/analysis/analysis_globals.R")
}


## Make cluster
if(!exists("cl")){
  cl <- make_cluster(ncore)
  stop.cl <- T
} else {
  stop.cl <- F
}

ah.graph <- readRDS("data/analysis_data/graph_austhung.rds")

## Export stuff to cluster
clusterExport_fast(cl, c("ah.graph", "cut_yrs"), envir = globalenv())


# ESTIMATE MODEL #########

# Models
form.ls <- unlist(lapply(c(1900, 1910), function(y){
  list(austhung_ldv_hd = as.formula(paste0("cshp",
                                           " ~ ", main_cov_spec, " + land.diff + abram_sum + ",
                                           "cshp_lag + hell.dist.", y)),
       austhung_xsec_hd = as.formula(paste0("cshp",
                                            " ~ ", main_cov_spec, " + land.diff + ",
                                            "hell.dist.", y)),
       austhung_ldv_ld = as.formula(paste0("cshp",
                                           " ~ ", main_cov_spec, " + land.diff + abram_sum + ",
                                           "cshp_lag + largest.diff.", y)),
       austhung_xsec_ld = as.formula(paste0("cshp",
                                            " ~ ", main_cov_spec, " + land.diff + ",
                                            "largest.diff.", y)),
       austhung_ldv_contr = as.formula(paste0("cshp",
                                              " ~ ", main_cov_spec, " + land.diff + abram_sum + ",
                                              "cshp_lag + cuteth1911")),
       austhung_xsec_contr = as.formula(paste0("cshp_2011",
                                               " ~ ", main_cov_spec, " + land.diff + ",
                                               "cuteth1911")))
}), recursive = F)


# Fit Pseudo MLE: short, only physical geography
print(Sys.time())
print(paste("MODEL ESTIMATION"))
model.ls <- parLapply(cl, form.ls, function(f){
  set.seed(1)
  print("Estimate")
  print(Sys.time())
  
  ## Make Graph list (~ wide to long)
  graph.ls <- lapply(cut_yrs[-1], function(y){
    print(y)
    ## rename treatment and outcome
    V(ah.graph)$cshp <- vertex_attr(ah.graph, paste0("cshp_", y))
    if(y != cut_yrs[2]){
      E(ah.graph)$cshp_lag <- edge_attr(ah.graph, paste0("cshp_", cut_yrs[which(cut_yrs == y) -1]))
    }
    
    ## Return
    ah.graph
  })
  
  ## Drop first entry in ldv models
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
saveRDS(model.ls, file = file.path(tem_res.path, "austhung_model.ls.rds"))
# model.ls <- readRDS(file.path(tem_res.path, "austhung_model.ls.rds"))

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

saveRDS(se.ls, file = file.path(tem_res.path, "austhung_se.ls.rds"))

# Clean up model.ls to save space
model.ls <- lapply(model.ls, function(m){
  m$pspm_ls  <- NULL
  m$learn_obj <- NULL
  m$g_ls <- NULL
  m
})
saveRDS(model.ls, file = file.path(tem_res.path, "austhung_model.ls.rds"))

  
# STOP CLUSTER
if(stop.cl){
  stopCluster(cl)
  rm(cl)
}


