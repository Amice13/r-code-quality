#############################
# PSPM FOR EACH CONTINENT
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

# Continents
cont.vec <- c("eu","as","af","na","sa")

# Models
form.ls <- list(pool_ldv = as.formula(paste("cshp_2017",
                                            " ~ ", main_cov_spec, " + ",
                                            "cshp_1964 + mean_greg")),
                pool_xsec = as.formula(paste("cshp_2017",
                                             " ~ ", main_cov_spec, " + ",
                                             "mean_greg")))
spec.ls <- unlist(lapply(form.ls, function(f){
  lapply(cont.vec, function(c){
    list(continent = c, fun = f)
  })
}), recursive = F)


# Load Data and Fit Model for each specification
print(paste("MODEL ESTIMATION"))
model.ls <- parLapply(cl, spec.ls, function(s){
  print("Prepare Data")
  print(Sys.time())
  
  # Load Graph
  this.graph.path <- file.path("data/analysis_data",
                               paste0("graph_",s[["continent"]],".rds"))
  this.g <- readRDS(this.graph.path)
 
  # Estimate
  set.seed(1)
  print("Estimate")
  print(Sys.time())
  m <-  fit_pspm_model(form = s[["fun"]], 
                          g_ls = list(this.g),
                          model_type = "composite_log_likelihood",
                          sigma2 = 10, 
                          force_contiguous = TRUE,
                          min_component_size = 2,
                          na.rm = T,
                          return_pspm = T)
  print(s[["continent"]])
  print(summary(m))
  print(Sys.time())
  
  # Return
  m
})          
saveRDS(model.ls, file = file.path(tem_res.path, "continents_model.ls.rds"))
# model.ls <- readRDS(file.path(tem_res.path, "continents_model.ls.rds"))

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

saveRDS(se.ls, file = file.path(tem_res.path, "continents_se.ls.rds"))

# Clean up model.ls to save space
model.ls <- lapply(model.ls, function(m){
  m$pspm_ls  <- NULL
  m$learn_obj <- NULL
  m$g_ls <- NULL
  m
})
saveRDS(model.ls, file = file.path(tem_res.path, "continents_model.ls.rds"))
# se.ls <- readRDS(file.path(tem_res.path, "continents_se.ls.rds"))



# STOP CLUSTER
if(stop.cl){
  stopCluster(cl)
  rm(cl)
}


