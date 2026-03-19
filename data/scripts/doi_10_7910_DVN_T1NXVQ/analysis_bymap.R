#############################
# PSPM BY HEG MAP 
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
clusterExport_fast(cl, c("graph", "cut_yrs"), envir = globalenv())

# HEG maps meta data
eth.meta.df <- read.csv("data/geography/HEG/heg_meta.csv")


# ESTIMATE MODEL #########

# Models
form.ls <- c(lapply(eth.meta.df$file_name, function(m){
  starts <- c(1800, cut_yrs[-length(cut_yrs)])
  l <- which(sapply(seq_along(starts), function(s){
    (eth.meta.df$year_data_end[eth.meta.df$file_name == m] %in% (starts[s]:(cut_yrs[s]-1)))
  }))
  as.formula(paste0("cshp_2011",
                   " ~ ", main_cov_spec, " + abram_sum + ",
                   "cshp_", cut_yrs[l], " + ethmap_", m))
}), 
lapply(eth.meta.df$file_name, function(m){
  as.formula(paste0("cshp_2011",
                   " ~ ", main_cov_spec, " + ethmap_", m))
}))
names(form.ls) <- c(paste0(eth.meta.df$file_name, "_ldv"),
                    paste0(eth.meta.df$file_name, "_xsec"))
  


# Fit Pseudo MLE: short, only physical geography
print(Sys.time())
print(paste("MODEL ESTIMATION"))
model.ls <- parLapply(cl, form.ls, function(f){
  set.seed(1)
  print("Estimate")
  print(Sys.time())
  
  ## Make Graph list (~ wide to long)
  graph.ls <- list(graph)
  
  ## Drop first entry in ldv model
  if(any(grepl("_lag", as.character(f), fixed = T))){
    graph.ls[[1]] <- NULL
  }
  
  ## Estimate model
  m <- try(fit_pspm_model(form = f, 
                         g_ls = graph.ls,
                         model_type = "composite_log_likelihood",
                         sigma2 = 10, 
                         force_contiguous = TRUE,
                         na.rm = T,
                         min_component_size = 2,
                         vertex_coords = c("x", "y"),
                         return_pspm = T, return_g_ls = F))
  if(any(grepl("error", class(m)))){
    print("ERRROR")
    return(NULL)
  } else {
    print(summary(m))
    print(Sys.time())
    return(m)
  }
})        
names(model.ls) <- c(paste0(eth.meta.df$file_name, "_ldv"),
                    paste0(eth.meta.df$file_name, "_xsec"))
saveRDS(model.ls, file = file.path(tem_res.path, "bymap_model.ls.rds"))
# model.ls <- readRDS(file.path(tem_res.path, "bymap_model.ls.rds"))

# Reset Models
for(m in seq_along(model.ls)){
  if(!is.null(model.ls[[m]])){
    model.ls[[m]]$learn_obj <- reset_pspmlearn(model.ls[[m]]$learn_obj)
  } else {
    next
  }
}

# Bootstrapped standard errors
print(Sys.time())
print(paste("BOOTSTRAP SEs"))
se.ls <- list()

for( i in seq_along(model.ls)){
  print("next")
  print(Sys.time())
  if(!is.null(model.ls[[i]])){
    se.ls[[i]] <- model.ls[[i]]$learn_obj$par_bootstrap_composite_log_likelihood(cl = cl, 
                                                                                 n_boot_iter = n_boot_iter, burnin = burnin,
                                                                                 ci_level = 0.95,
                                                                                 return_sims = TRUE, cache_samples = T)
  } else {
    se.ls[[i]] <- NULL
  }
}

saveRDS(se.ls, file = file.path(tem_res.path, "bymap_se.ls.rds"))

# Clean up model.ls to save space
model.ls <- lapply(model.ls, function(m){
  m$pspm_ls  <- NULL
  m$learn_obj <- NULL
  m$g_ls <- NULL
  m
})
saveRDS(model.ls, file = file.path(tem_res.path, "bymap_model.ls.rds"))

  
# STOP CLUSTER
if(stop.cl){
  stopCluster(cl)
  rm(cl)
}


