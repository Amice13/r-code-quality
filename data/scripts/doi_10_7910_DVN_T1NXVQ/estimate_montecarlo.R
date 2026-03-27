#########################################
# ESTIMATE MONTE CARLO SIMULATIONS
# Wrapper around Monte Carlo Simulations
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

# Main function ####
estimate_montecarlo <- function(mc_param_df, output.dir, ncore = 45, patch.size = 40){
  if(grepl("carlvs", getwd())){
    Sys.setenv(RETICULATE_PYTHON = '/usr/bin/python3')
  }
  library(pspm)
  library(reticulate)
  library(raster)
  library(dplyr)
  library(plyr)
  library(ggplot2)
  library(foreach)
  library(doParallel)
  library(maxLik)
  
  # Print stuff for Logfile ###
  
  # Setup directory ##
  
  ## Create
  dir.create(output.dir)
  
  ## Files to save
  output.params <- file.path(output.dir, "parameters.rds")
  output.results <- file.path(output.dir, "results.rds")
  
  # Setup Logfile ##
  
  ## make log
  logfile <- file.path(output.dir, "logfile.txt")
  sink(logfile)
  
  ## Print overview
  print(Sys.time())
  print(paste("Total of ", nrow(mc_param_df), "runs with the following parameters:."))
  for(v in colnames(mc_param_df)){
    print(paste("### ", v, ":"))
    print(paste(unique(as.vector(mc_param_df[,v])), collapse = "; "))
  }
  
  
  # Make cluster ##
  
  ## Make
  cluster.report <- file.path(output.dir, "cluster_progress.txt")
  if(file.exists(cluster.report)){
    unlink(cluster.report)
  }
  cl <- makeCluster(getOption("cl.cores", ncore), 
                    outfile =  cluster.report)
  registerDoParallel(cl)
  
  ## Init Cluster
  init.ls <- clusterEvalQ(cl = cl, expr = {
    if(grepl("carlvs", getwd())){
      Sys.setenv(RETICULATE_PYTHON = '/usr/bin/python3')
    }
    # Packages 
    library(pspm)
    library(reticulate)
    library(raster)
    library(ggplot2)
    library(foreach)
    library(doParallel)
    library(maxLik)
    
    # Source this code for grid making function
    source("scripts/functions/monte_carlos/estimate_montecarlo.R")
    
  })
  

  # Simulate ##
  
  
  # Get unique training data parameters
  data_param_df <- unique(mc_param_df[,!colnames(mc_param_df) %in% c("num_instances", "burnin")])
  data_param_df$data.id <- 1:nrow(data_param_df)
  
  ## Split into patches à patch.size * ncore
  data_param_df$patch.id <- rep(1:ceiling(nrow(data_param_df)/(ncore*patch.size)), 
                                each = ncore*patch.size)[1:nrow(data_param_df)]
  
  ## Join with entire parameter space
  mc_param_df <- join(data_param_df, mc_param_df,
                      by = colnames(mc_param_df)[!colnames(mc_param_df) %in% c("num_instances", "burnin")], 
                      type = "left", match = "all")
  
  ## Init simulation ID
  mc_param_df$sim.id <- 1:nrow(mc_param_df)
  
  ## Save parameter space
  saveRDS(mc_param_df, output.params)
  
  ## Make tmeporary directors
  dir.create(file.path(output.dir, "temp"))
  
  
  ## Run loop in parallel
  for(patch in unique(mc_param_df$patch.id[mc_param_df$patch.id > 0])){
    
    # Some Logging
    print(Sys.time())
    print(paste0("Patch: ", patch, " of ", length(unique(mc_param_df$patch.id))))
    
    # Compute
    this_mc_result.df <- foreach(i = unique(mc_param_df$data.id[mc_param_df$patch.id == patch]), .combine = rbind, 
                                 # .errorhandling = "remove",
                                 .options.multicore =  list(preschedule = FALSE)) %dopar% {
                                   print(i)
                                   
                                   ## Get simulations to work on
                                   this_mc_param_df <- mc_param_df[mc_param_df$data.id == i, , drop = F]
                                   
                                   ## Set local parameters
                                   beta0 <- unique(this_mc_param_df$beta0)
                                   beta1 <- unique(this_mc_param_df$beta1)
                                   
                                   N_sqrd <- unique(this_mc_param_df$N_sqrd)
                                   seed <- unique( this_mc_param_df$seed)
                                   burnin <- unique(this_mc_param_df$burnin)
                                   num_instances <- unique(this_mc_param_df$num_instances)
                                   bootse <- unique(this_mc_param_df$bootse)
                                   
                                   partemp <- unique(this_mc_param_df$partemp)
                                   
                                   sd <- unique(this_mc_param_df$sd)
                                   if(is.null(sd)){
                                     sd <- 0
                                   }
                                   
                                   ## Check
                                   stopifnot(nrow(unique(this_mc_param_df[,!colnames(this_mc_param_df) %in% 
                                                                            c("num_instances", "burnin", "sim.id")])) == 1)
                                   
                                   ## Set local seed
                                   set.seed(seed)
                                   py_set_seed(seed, disable_hash_randomization = TRUE)
                                   
                                   ## Simulate outcome data with known betas
                                   pspm_ls <- vector('list', max(num_instances))
                                   for (j in 1:max(num_instances)) {
                                     if(partemp == 0){
                                       pspm_ls[[j]] <- generate_grid_data_mod(N_sqrd = N_sqrd, 
                                                                                 beta0 = beta0, 
                                                                                 beta = beta1,
                                                                                 burnin = max(burnin),
                                                                                 temperatures = 1, swap_iter = 1,
                                                                                 dep_structure = "hexagonal",
                                                                                 sd = sd,
                                                                                 return_full = any(burnin != max(burnin)))
                                     } else {
                                       pspm_ls[[j]] <- generate_grid_data_mod(N_sqrd = N_sqrd, 
                                                                                 beta0 = beta0, 
                                                                                 beta = beta1,
                                                                                 burnin = max(burnin),
                                                                                 temperatures = c(1,2,4,8), swap_iter = 1,
                                                                                 dep_structure = "hexagonal",
                                                                                 sd = sd,
                                                                                 return_full = any(burnin != max(burnin)))
                                     }
                                     
                                   }
                                   
                                   ## Estimate models
                                   result.df <- do.call(rbind, lapply(burnin, function(bi){
                                     do.call(rbind, lapply(num_instances, function(inst){
                                       ## Subset number of instances
                                       sub_this_mc_param_df <- this_mc_param_df[this_mc_param_df$num_instances == inst & 
                                                                                  this_mc_param_df$burnin == bi, ]
                                       
                                       ## Init learning object with reset betas
                                       learn_obj <- PSPMLearn$new(lapply(1:inst, function(x){
                                         ## Get PSPM Object
                                         if(any(burnin != max(burnin))){
                                           s <- pspm_ls[[x]]$sl$clone()
                                           
                                           ## Reset partitioning
                                           s$pm$set_partitioning(pspm_ls[[x]]$samples[, bi+1])
                                           s$pm$partitioning
                                         } else {
                                           s <- pspm_ls[[x]]
                                         }
                                         
                                         
                                         ## Return
                                         s
                                       }), sigma2 = 10)
                                       
                                       
                                       ## Reset Beta
                                       learn_obj$reset_beta()
                                       
                                       ## Learn
                                       beta_init <- rep(0, 2)
                                       tm <- system.time({
                                         llfit <- learn_obj$train_composite_log_likelihood(beta = beta_init) 
                                       })
                                       if(bootse){
                                         boot.se <- learn_obj$bootstrap_composite_log_likelihood(n_boot_iter = 100, 
                                                                                                 burnin = 1e2,
                                                                                                 ci_level = 0.95,
                                                                                                 return_sims = F, 
                                                                                                 report_every = 1000)
                                         boot.ci.basic <- as.vector(boot.se$ci_basic)
                                         boot.ci.perc <- as.vector(boot.se$ci_percentile)
                                       } else {
                                         boot.ci.basic <- rep(NA, 4)
                                         boot.ci.perc <- rep(NA, 4)
                                       }
                                       se <- stdEr(llfit)
                                       beta_est <- llfit$estimate
                                       
                                       ## Return results
                                       res.df <- data.frame(b0 = beta_est[1], b = beta_est[2], 
                                                            b0.se = se[1], b.se = se[2],
                                                            b0.bootci.basic.low =  boot.ci.basic[1],
                                                            b0.bootci.basic.high =  boot.ci.basic[2],
                                                            b1.bootci.basic.low =  boot.ci.basic[3],
                                                            b1.bootci.basic.high =  boot.ci.basic[4],
                                                            b0.bootci.perc.low =  boot.ci.perc[1],
                                                            b0.bootci.perc.high =  boot.ci.perc[2],
                                                            b1.bootci.perc.low =  boot.ci.perc[3],
                                                            b1.bootci.perc.high =  boot.ci.perc[4],
                                                            time = tm[3],
                                                            stringsAsFactors = F)
                                       
                                       ## Save sim.id
                                       res.df$sim.id <- sub_this_mc_param_df$sim.id
                                       
                                       ## Return
                                       return(res.df)
                                     }))
                                   }))
                                   
                                   
                                   ## return
                                   result.df
                                 }
    
    ## Save results
    saveRDS(this_mc_result.df, file.path(output.dir, "temp", paste0(patch, ".rds")))
    
  }
  
  ## Load all results
  print(Sys.time())
  print("Load all and save --- FINSIHED!!")
  mc_result.df <- do.call(rbind, lapply(unique(mc_param_df$patch.id), function(p){
    readRDS(file.path(output.dir, "temp", paste0(p, ".rds")))
  }))
  
  ## Sort Results
  mc_result.df <- mc_result.df[order(mc_result.df$sim.id),]
  
  ## Save results
  saveRDS(mc_result.df, output.results)
  
  ## Delete temp folder
  unlink(file.path(output.dir, "temp"), recursive = T)
  
  ## Print error messages
  if(!all(mc_param_df$sim.id %in% mc_result.df$sim.id)){
    print("We've got errors")
    print(paste("Look at ids", paste(mc_param_df$sim.id[!mc_param_df$sim.id %in% mc_result.df$sim.id],collapse = "; ")))
    print(as_tibble(mc_param_df[!mc_param_df$sim.id %in% mc_result.df$sim.id,]))
  }
  
  
  ## CLEAN UP ####
  stopCluster(cl)
  rm(cl)
  
  
  # Exit ##
  sink()
  return(TRUE)
}



# (function) Generate grid data function // Modified from PSPM to return full samples
generate_grid_data_mod <- function(N_sqrd,
                                   beta0,
                                   beta = NULL,
                                   temperatures = 1, swap_iter = 5,
                                   dep_structure = c("von_neumann", "moore", "hexagonal"),
                                   burnin = 10, return_full = F, sd = 0) {
  dep_structure <- match.arg(dep_structure)
  
  # Make NxN dependency matrix
  N <- N_sqrd^2
  pairs <- expand.grid(1:N, 1:N)
  p1 <- pairs[,1]
  p2 <- pairs[,2]
  A <- generate_square_grid_lattice(N_sqrd, dep_structure = dep_structure)
  
  # Generate distance covariates (column separators, one for each beta)
  if (!is.null(beta)) {
    L <- length(beta)
    # stopifnot(L < N_sqrd - 1)
    stopifnot(L == 1)
    separating_cols <- seq(1, N_sqrd-1, 2)
    X.ls <- vector('list', L)
    for (l in 1:L) {
      
      # Predictor
      is_distant <- mapply(function(a, b) {
        any(sapply(separating_cols, function(c){pspm:::is_separated_by_col(a, b, N_sqrd, c)}))
      }, p1, p2)
      
      # Noise (same across all simulations)
      if(sd > 0){
        oldSeed <- if (exists('.Random.seed', envir = .GlobalEnv, inherits = FALSE)) {
          get('.Random.seed', envir = .GlobalEnv, inherits = FALSE)
        }
        set.seed(1)
        noise <- rnorm(N*N, mean = 0, sd = sd)
        if(!is.null(oldSeed)) {
          assign('.Random.seed', oldSeed, envir = .GlobalEnv)
        } else {
          set.seed(NULL)
        }
      } else {
        noise = 0
      }
      
      # Put together
      D <- A*matrix(is_distant + noise, N, N)*1
      X.ls[[l]] <- D
    }
  } else {
    X.ls <- list()
  }
  
  # Placeholder Y
  Y <- 1:N
  
  # Create the pspm object
  sl <- PSPM$new(Y = Y, Z = X.ls, X =  matrix(Y, ncol = 1), 
                    A = A, force_contiguous=TRUE, verbose = FALSE)
  sl$set_beta(c(beta0, beta))
  
  # Sample a new lattice partitioning
  if(length(temperatures) > 1 & any(temperatures == 1)){
    if(return_full){
      samples <- sl$sample_partemp(burnin = burnin,
                                   temperatures = temperatures, 
                                   swap_iter = swap_iter,
                                   return_full = T)
      return(list(sl = sl, samples = samples))
    } else {
      sl$sample_partemp(burnin = burnin,
                        temperatures = temperatures, swap_iter = swap_iter)
      return(sl)
    }
  } else {
    if(return_full){
      samples <- sl$sample(burnin = burnin, return_full = T)
      return(list(sl = sl, samples = samples))
    } else {
      sl$sample(burnin = burnin)
      return(sl)
    }
  }
}
