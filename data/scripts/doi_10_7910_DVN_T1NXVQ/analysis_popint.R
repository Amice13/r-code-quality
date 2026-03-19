#############################
# GLOBAL PSPMS WITH POPULATION INTERACTION
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

# Continents
cont.vec <- c("eu","as","af","na","sa")

# Load Data and Fit Model for each specification
print(paste("MAKE GRAPHS"))
c.graph.ls <- parLapply(cl, cont.vec, function(c){
  print("Prepare Data")
  print(Sys.time())
  
  # Load Graph
  this.graph.path <- file.path("data/analysis_data",
                               paste0("graph_",c,".rds"))
  this.g <- readRDS(this.graph.path)

  # Return
  this.g
})          
names(c.graph.ls) <- cont.vec

## Add graphs
c.graph.ls[["world"]] <- c.graph.ls

## Drop Americas, only LDV
c.graph.ls[["na"]] <- NULL
c.graph.ls[["sa"]] <- NULL
c.graph.ls[sapply(c.graph.ls, is.null)] <- NULL
# c.graph.ls[["eu_full"]] <- graph


# ESTIMATE MODEL #########


# Models

## Covariates (unnormalized)
cov <- gsub(".norm", "", 
            strsplit(main_cov_spec, " + ", fixed = T)[[1]],
            fixed = T)

## Specs
spec.ls <- lapply(names(c.graph.ls), function(n){
  if(n == "eu_full"){
    list(ldv = as.formula(paste("cshp",
                                " ~ ", paste(c(cov, "abram_sum", "pop1880l"), collapse = " + "), " + ",
                                paste(paste("I(pop1880l*", c(cov, "abram_sum"), ")"), collapse = " + "), 
                                "+ cshp_lag + cuteth_lag + ",
                                paste(paste("I(pop1880l*", c("cshp_lag", "cuteth_lag"), ")"), collapse = " + ")))
         )
  } else {
    list(ldv = as.formula(paste("cshp_2017",
                                     " ~ ", paste(c(cov, "pop1880l"), collapse = " + "), " + ",
                                     # paste(paste("I(pop1880l*", c(cov), ")"), collapse = " + "), "+",
                                     "cshp_1964 + mean_greg + ",
                                     paste(paste("I(pop1880l*", c("cshp_1964",  "mean_greg"), ")"), collapse = " + ")))
         )
  }
})
names(spec.ls) <- names(c.graph.ls)
spec.ls <- unlist(spec.ls, recursive = F)


## Export stuff to cluster
clusterExport_fast(cl, c("c.graph.ls", "cut_yrs", "spec.ls"), 
                   envir = globalenv())

# Fit Pseudo MLE
print(Sys.time())
print(paste("MODEL ESTIMATION"))
model.ls <- parLapply(cl, names(spec.ls), function(n){
  set.seed(1)
  print("Estimate")
  print(Sys.time())
  
  ## Continent and graph
  cont <- strsplit(n, "\\.")[[1]][1]
  graph <- c.graph.ls[[cont]]
  fun <- spec.ls[[n]]
  
  ## Make Graph list (~ wide to long)
  if(grepl("eu_full", n, fixed = T)){
    graph.ls <- lapply(cut_yrs, function(y){
      print(y)
      
      ## rename treatment and outcome
      V(graph)$cshp <- vertex_attr(graph, paste0("cshp_", y))
      E(graph)$cuteth <- edge_attr(graph, paste0("cuteth", cut_yrs[which(cut_yrs == y)]))
      if(y != cut_yrs[1]){
        E(graph)$cshp_lag <- edge_attr(graph, paste0("cshp_", cut_yrs[which(cut_yrs == y) -1]))
        E(graph)$cuteth_lag <- edge_attr(graph, paste0("cuteth", cut_yrs[which(cut_yrs == y) -1]))
      }
      graph
    })
  } else if(!grepl("world", n, fixed = T)){
    graph.ls <- list(graph)
  } else {
    graph.ls <- graph
  }
  
  ## Log popultion
  for(i in seq_along(graph.ls)){
    E(graph.ls[[i]])$pop1880l <- log(1 + E(graph.ls[[i]])$pop1880_mean)
  }
  
  ## Drop first entry in ldv model
  if(any(grepl("_lag", as.character(fun), fixed = T))){
    graph.ls[[1]] <- NULL
  }
  
  ## Estimate model
  m <- fit_pspm_model(form = fun, 
                         g_ls = graph.ls,
                         model_type = "composite_log_likelihood",
                         sigma2 = 10, 
                         force_contiguous = TRUE,
                         na.rm = T,
                         min_component_size = 2,
                         vertex_coords = c("x", "y"),
                         return_pspm = T, return_g_ls = F)
  print(n)
  print(summary(m))
  print(Sys.time())
  m
})          
saveRDS(model.ls, file = file.path(tem_res.path, "popint_model.ls.rds"))
# model.ls <- readRDS(file.path(tem_res.path, "popint_model.ls.rds"))

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

saveRDS(se.ls, file = file.path(tem_res.path, "popint_se.ls.rds"))

# Clean up model.ls to save space
model.ls <- lapply(model.ls, function(m){
  # m$pspm_ls  <- NULL ## We need this to plot marginal effects later
  m$learn_obj <- NULL
  m$g_ls <- NULL
  m
})
saveRDS(model.ls, file = file.path(tem_res.path, "popint_model.ls.rds"))
  
# STOP CLUSTER
if(stop.cl){
  stopCluster(cl)
  rm(cl)
}


