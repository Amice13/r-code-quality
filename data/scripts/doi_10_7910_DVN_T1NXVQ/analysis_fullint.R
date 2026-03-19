#############################
# FULLY INTERACTED PSPM
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
cov <- strsplit(main_cov_spec, " + ", fixed = T)[[1]]
form.ls <- list(int1 = as.formula(paste("cshp",
                                        " ~ ", main_cov_spec, 
                                        "+", paste(paste("I(", c(cov, "abram_sum"), "*cshp_lag)"), collapse = "+"),
                                        # "+", paste(paste("I(", cov, "*abram_sum)"), collapse = "+"),
                                        " + abram_sum + ",
                                        "cshp_lag + cuteth_lag")),
                int2 = as.formula(paste("cshp",
                                        " ~ ", main_cov_spec,
                                        " + abram_sum + ",
                                        "cshp_lag + cuteth_lag + I(cshp_lag*cuteth_lag)")),
                int3 = as.formula(paste("cshp",
                                        " ~ ", main_cov_spec, 
                                        "+", paste(paste("I(", c(cov, "abram_sum"), "*cshp_lag)"), collapse = "+"),
                                        # "+", paste(paste("I(", cov, "*abram_sum)"), collapse = "+"),
                                        " + abram_sum + ",
                                        "cshp_lag + cuteth_lag + I(cshp_lag*cuteth_lag)")),
                dur1 = as.formula(paste("cshp",
                                        " ~ ", main_cov_spec, "+", paste(paste0("duration", 1:3), collapse = "+"), 
                                        "+", paste(paste("I(", paste0("duration", 1:3), "*cshp_lag)"), collapse = "+"),
                                        # "+", paste(paste("I(", cov, "*abram_sum)"), collapse = "+"),
                                        " + abram_sum + ",
                                        "cshp_lag + cuteth_lag")),
                full = as.formula(paste("cshp",
                                        " ~ ", main_cov_spec, "+", paste(paste0("duration", 1:3), collapse = "+"), 
                                        "+", paste(paste("I(", c(cov, "abram_sum"), "*cshp_lag)"), collapse = "+"), 
                                        "+", paste(paste("I(", paste0("duration", 1:3), "*cshp_lag)"), collapse = "+"),
                                        # "+", paste(paste("I(", cov, "*abram_sum)"), collapse = "+"),
                                        " + abram_sum + ",
                                        "cshp_lag + cuteth_lag + I(cshp_lag*cuteth_lag)")))
  


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
    E(graph)$cuteth <- edge_attr(graph, paste0("cuteth", cut_yrs[which(cut_yrs == y)]))
    if(y != cut_yrs[1]){
      E(graph)$cshp_lag <- edge_attr(graph, paste0("cshp_", cut_yrs[which(cut_yrs == y) -1]))
      E(graph)$cuteth_lag <- edge_attr(graph, paste0("cuteth", cut_yrs[which(cut_yrs == y) -1]))
      
      ## Duration
      d.yrs <- c(seq(1100, 1700, by = 100), 1790, cut_yrs[cut_yrs < cut_yrs[which(cut_yrs == y) - 1]])
      d.vars <- paste0(ifelse(d.yrs < 1800, "abram_", "cshp_"), d.yrs)
      dur.mat <- do.call(cbind, lapply(d.vars, function(v){
        edge_attr(graph, v)
      }))
      dur.mat[is.na(dur.mat)] <- 0 ## set missings to 0, since no state there in Abrams. and dropped otherwise anyways later. 
      change.mat <- apply(dur.mat, 2, function(x){
        x != edge_attr(graph, paste0("cshp_", cut_yrs[which(cut_yrs == y) - 1]))
      })
      change.vec <- sapply(apply(change.mat, 1, which), max)
      change.vec[is.infinite(change.vec)] <- 1
      E(graph)$duration1 <- ((cut_yrs[which(cut_yrs == y) - 1] - d.yrs[change.vec])/100)^1
      E(graph)$duration2 <- ((cut_yrs[which(cut_yrs == y) - 1] - d.yrs[change.vec])/100)^2
      E(graph)$duration3 <- ((cut_yrs[which(cut_yrs == y) - 1] - d.yrs[change.vec])/100)^3
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
                         return_pspm = T, return_g_ls = F)
  print(summary(m))
  print(Sys.time())
  m
})          
saveRDS(model.ls, file = file.path(tem_res.path, "fullint_model.ls.rds"))
# model.ls <- readRDS(file.path(tem_res.path, "fullint_model.ls.rds"))

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

saveRDS(se.ls, file = file.path(tem_res.path, "fullint_se.ls.rds"))

# Clean up model.ls to save space
model.ls <- lapply(model.ls, function(m){
  m$pspm_ls  <- NULL
  m$learn_obj <- NULL
  m$g_ls <- NULL
  m
})
saveRDS(model.ls, file = file.path(tem_res.path, "fullint_model.ls.rds"))

  
# STOP CLUSTER
if(stop.cl){
  stopCluster(cl)
  rm(cl)
}


