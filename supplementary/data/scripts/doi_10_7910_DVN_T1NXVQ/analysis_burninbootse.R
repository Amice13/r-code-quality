#############################
# PSPM WITH VARYING BURN-IN PERIOD FOR BOOTSTRAPPED SEs
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

## Burnin length
burnin.vec <- c(1, 5, 10, 50, 100, 500, 1000)

## Export stuff to cluster
clusterExport_fast(cl, c("graph", "cut_yrs", "burnin.vec"), 
                   envir = globalenv())

# ESTIMATE MODEL #########

# Models
form.ls <- list(pool_ldv = as.formula(paste("cshp",
                                            " ~ ", main_cov_spec, " + abram_sum + ",
                                            "cshp_lag + cuteth_lag")),
                pool_xsec = as.formula(paste("cshp",
                                             " ~ ", main_cov_spec, " + ",
                                             "cuteth")))

# Load pooled model
model.ls <- readRDS(file.path(tem_res.path, "pool_model.ls.rds"))
model.ls <- lapply(1:2, function(m){
  model.ls[[m]]
})

# Reset Models
for(m in seq_along(model.ls)){
  model.ls[[m]]$learn_obj <- reset_pspmlearn(model.ls[[m]]$learn_obj)
}

# Get samples for 1...burnin sample periods
print(Sys.time())
print(paste("SAMPLE"))
sample.ls <- vector(mode = "list",
                    length = length(model.ls))

for(s in seq_along(model.ls)){
  print(s)

  ## Spatlat
  pspm.ls <- model.ls[[s]]$learn_obj$pspm_ls
  
  ## Coefficients
  coefs <- model.ls[[s]]$estimate
  
  ## Prepare cluster
  clusterExport_fast(cl, c("pspm.ls", "burnin.vec", "coefs"), 
                     envir = environment())
  
  ## Reset pspm object
  reset_pspm <- clusterEvalQ(cl, expr = {
    for(s in seq_along(pspm.ls)){
      pspm.ls[[s]] <- reset_pspm(pspm.ls[[s]])
    }
    TRUE
  })
  
  ## Draw samples
  sample.ls[[s]] <- foreach(i = seq_len(n_boot_iter), 
                            .noexport = c("pspm.ls", "coefs","burnin")) %dopar% { 
                              lapply(seq_along(pspm.ls), function(s){
                                # Set seeds
                                set.seed(i)
                                reticulate::py_set_seed(i, disable_hash_randomization = TRUE)
                                
                                # This pspm object
                                this.pspm <- pspm.ls[[s]]
                                
                                # Sample new one
                                this.pspm$set_beta(coefs)
                                new.part <- this.pspm$sample(burnin = max(burnin.vec), 
                                                                return_full = TRUE)
                                
                                # Reset original
                                this.pspm$pm$set_partitioning(1:this.pspm$N)
                                this.pspm$Y <- 1:this.pspm$N
                                
                                # Return
                                new.part
                              })
                            }
  
}
saveRDS(sample.ls, file = file.path(tem_res.path, "burnin_sample.ls.rds"))
# sample.ls <- readRDS(file.path(tem_res.path, "burnin_sample.ls.rds"))

# Estimate distribution of estimates for 1 ... burnin periods

## init SE ls
all.se.ls <- vector(mode = "list",
                    length = length(model.ls))
  
  

## Get estimates
for(m in seq_along(model.ls)){
  ## Init
  all.se.ls[[m]] <- vector(mode = "list",
                           length(burnin.vec))
  
  ## Point estimates
  beta_point <- coef(model.ls[[m]])
  
  ## Spatlat
  pspm.ls <- model.ls[[m]]$learn_obj$pspm_ls
  
  
  ## Prepare cluster
  clusterExport_fast(cl, c("pspm.ls"), 
                     envir = environment())
  
  ## Reset pspm object
  reset_pspm <- clusterEvalQ(cl, expr = {
    for(s in seq_along(pspm.ls)){
      pspm.ls[[s]] <- reset_pspm(pspm.ls[[s]])
    }
    TRUE
  })
  
  ## Bootstrap estimates
  for(b in burnin.vec){
    ## Prepare sampled partitionings
    these.samples <- lapply(sample.ls[[m]], function(s){
      lapply(s, function(j){
        j[,b+1]
      })
    })
    
    ## Export
    clusterExport(cl, c("these.samples", "b"), 
                  envir = environment())
    
    ## Estimate model
    beta_boot_mat <- foreach(i = seq_len(n_boot_iter), 
                        .combine = rbind,
                        .noexport = c("pspm.ls", "these.samples", "b", "coefs")) %dopar% { 
                          # Set seeds
                          set.seed(i)
                          reticulate::py_set_seed(i, disable_hash_randomization = TRUE)
                          
                          # Set partitioning
                          pspm.ls <- lapply(seq_along(pspm.ls), function(s){
                            # This pspm object
                            this.pspm <- pspm.ls[[s]]
                            
                            # Set partitioning
                            this.pspm$pm$set_partitioning(these.samples[[i]][[s]])
                            this.pspm$Y <- these.samples[[i]][[s]]
                            
                            # Return
                            this.pspm
                          })
                          
                          # Make learn pspm.ls
                          learn <- PSPMLearn$new(pspm_ls = pspm.ls, 
                                                    sigma2 = 10)
                          learn$reset_beta()
                          
                          # Fit
                          llfit <- learn$fit_composite_log_likelihood()
                          
                          # Return coefs
                          coef(llfit)
                        }
    
    ## Get CIs
    ci_basic_mat <- pspm:::get_basic_ci(beta_boot_mat, 
                                           beta_point, 
                                           alpha = .05)
    ci_perc_mat <- pspm:::get_percentile_ci(beta_boot_mat, 
                                               alpha = .05)
    
    ## Save
    all.se.ls[[m]][[which(burnin.vec == b)]] <- list(beta_boot = beta_boot_mat,
                                ci_basic = ci_basic_mat,
                                ci_percentile = ci_perc_mat)
  }
}
saveRDS(all.se.ls, file = file.path(tem_res.path, "burnin_all.se.ls.rds"))
# all.se.ls <- readRDS(file.path(tem_res.path, "burnin_all.se.ls.rds"))


# Plot ###


## Prepare main
plot.df <- do.call(rbind, lapply(seq_along(burnin.vec), function(b){
  do.call(rbind, lapply(1:2, function(i){
    p <- length(model.ls[[i]]$estimate)
    data.frame(beta = model.ls[[i]]$estimate[p],
               se = stdEr(model.ls[[i]])[p],
               bcilow = all.se.ls[[i]][[b]]$ci_percentile[1,p],
               bciup = all.se.ls[[i]][[b]]$ci_percentile[2,p],
               variable = names(model.ls[[i]]$estimate)[p],
               model = c("Lagged Dependent Variable Model", "Baseline Model")[i],
               burnin = burnin.vec[b], stringsAsFactors = F)
  }))
}))
plot.df$burnin <- factor(plot.df$burnin, 
                       levels = burnin.vec, 
                       ordered = T)


## Bootstrapped estimates
bs.plot.df <- do.call(rbind, lapply(seq_along(burnin.vec), function(b){
  do.call(rbind, lapply(1:2, function(i){
    p <- length(model.ls[[i]]$estimate)
    data.frame(beta = as.vector(all.se.ls[[i]][[b]]$beta_boot[,p]),
               variable = names(model.ls[[i]]$estimate)[p],
               model = c("Lagged Dependent Variable Model", "Baseline Model")[i],
               burnin = burnin.vec[b], stringsAsFactors = F)
  }))
}))
bs.plot.df$burnin <- factor(bs.plot.df$burnin, 
                            levels = burnin.vec, 
                            ordered = T)

## Plot
g <- ggplot(plot.df, aes(x = burnin, y = beta)) +
  geom_hline(yintercept = 0, lty = 2, col = "darkgrey") +
  geom_violin(data = bs.plot.df, aes(group = burnin), fill = "lightgrey", color = "grey") +
  geom_point() +
  geom_errorbar(aes(ymin = bcilow, ymax = bciup), width = 0) +
  facet_wrap(~ model, nrow = 1) + theme_minimal() +
  scale_y_continuous(breaks = scales::pretty_breaks(n = 4)) +
  theme(axis.text.x = element_text(angle = 90, vjust = .5, hjust = 1),
        panel.spacing = unit(2, "lines"),
        panel.grid.minor.x = element_blank()) +
  xlab("Length of burn-in period") + 
  ylab("Effect of ethnic boundaries\nw/ uncertainty estimate") +
  NULL

png(file.path(fig.path, "robcheck_burninbootse.png"), width = 6, height = 3, res = 400, units = "in")
par(mar = c(0,0,0,0))
print(g)
dev.off()



# STOP CLUSTER
if(stop.cl){
  stopCluster(cl)
  rm(cl)
}
