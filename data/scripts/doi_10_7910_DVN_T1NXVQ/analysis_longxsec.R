#############################
# CROSS-SECTIONAL PSPM WITH PRE-1886 ETHNICITY DATA
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

## Export stuff to cluster
clusterExport_fast(cl, c("graph"), envir = globalenv())


# ESTIMATE MODEL #########

# Models
form.ls <- lapply(cut_yrs, function(y){
  as.formula(paste(paste0("cshp_", y),
                   " ~ ", main_cov_spec, " + ",
                   paste0("cuteth", cut_yrs[1])))
})

# Fit Pseudo MLE: short, only physical geography
print(Sys.time())
print(paste("MODEL ESTIMATION"))
model.ls <- parLapply(cl, form.ls, function(f){
  set.seed(1)
  print("Estimate")
  print(Sys.time())
  m <- fit_pspm_model(form = f, 
                         g_ls = list(graph),
                         model_type = "composite_log_likelihood",
                         sigma2 = 10, 
                         force_contiguous = TRUE,
                         min_component_size = 2,
                         na.rm = T,
                         return_pspm = T)
  print(summary(m))
  print(Sys.time())
  m
})          
saveRDS(model.ls, file = file.path(tem_res.path, "longxsec_model.ls.rds"))
# model.ls <- readRDS(file.path(tem_res.path, "longxsec_model.ls.rds"))

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

saveRDS(se.ls, file = file.path(tem_res.path, "longxsec_se.ls.rds"))

# Clean up model.ls to save space
model.ls <- lapply(model.ls, function(m){
  m$pspm_ls  <- NULL
  m$learn_obj <- NULL
  m$g_ls <- NULL
  m
})
saveRDS(model.ls, file = file.path(tem_res.path, "longxsec_model.ls.rds"))

  
# STOP CLUSTER
if(stop.cl){
  stopCluster(cl)
  rm(cl)
}


