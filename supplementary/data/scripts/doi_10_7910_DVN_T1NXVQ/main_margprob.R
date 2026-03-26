#############################
# COMPUTE MARGINAL PROBABILITIES
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
library(viridis)
library(sp)
library(rgeos)
library(pracma)

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

# MAIN RESULTS ###

# Load models 

## Pooled
pool.model.ls <- readRDS(file.path(tem_res.path, "pool_model.ls.rds"))

# Samples with ethnic difference
samples.ls <- lapply(1:2, function(s){
  do.call(cbind, lapply(pool.model.ls[[s]]$learn_obj$samples_cache, function(x){
    x[[length(x)]]
  }))
})

# Sample w/ ethnic difference = 0

## Init
samples.noeth.ls <- vector(mode = "list", 
                           length = length(samples.ls))

## Sample
for(s in 1:2){
  print(s)
  
  ## Main model
  model <- pool.model.ls[[s]]
  
  ## Spatlat
  this.pspm <- model$learn_obj$pspm_ls[[length(model$learn_obj$pspm_ls)]]
  
  ## Coefficients
  coefs <- model$estimate
  
  ## Set ethnic to 0
  coefs[length(coefs)] <- 0
  
  ## Prepare cluster
  clusterExport_fast(cl, c("this.pspm", "burnin", "coefs"), 
                     envir = environment())
  
  ## Reset pspm object
  reset_pspm <- clusterEvalQ(cl, expr = {
    this.pspm <- reset_pspm(this.pspm)
    TRUE
  })
  
  ## Draw samples
  samples.noeth.ls[[s]] <- foreach(i = seq_len(n_boot_iter), .combine = cbind, 
                                   .noexport = c("this.pspm", "coefs","burnin")) %dopar% { #parLapply(cl, seq_len(nsample), function(i){
                                     set.seed(i)
                                     reticulate::py_set_seed(i, disable_hash_randomization = TRUE)
                                     
                                     # Sample new one
                                     this.pspm$set_beta(coefs)
                                     this.pspm$sample(burnin = burnin, return_full = FALSE)
                                     new.part <- this.pspm$Y
                                     
                                     # Reset original
                                     this.pspm$pm$set_partitioning(1:this.pspm$N)
                                     this.pspm$Y <- 1:this.pspm$N
                                     
                                     # Return
                                     new.part
                                   }
  
}



# Plot ###

# Prepare

## Edges to data.frame
edge.df <- NULL
for(i in seq_along(samples.ls)){
  g <- pool.model.ls[[i]]$g_ls[[length(pool.model.ls[[i]]$g_ls)]]
  Y <- pool.model.ls[[i]]$pspm_ls[[length(pool.model.ls[[i]]$pspm_ls)]]$Y
  e.ends <- ends(g, E(g), names = F)
  edge.df <- rbind(edge.df, 
                   data.frame(type = c("Lagged Dependent Variable Model", "Baseline Model")[i],
                              ethnic = E(g)$cuteth_lag == 1,
                              state = Y[e.ends[,1]] == Y[e.ends[,2]],
                              P_base = rowMeans(apply(samples.ls[[i]], 2, function(y_hat){
                                y_hat[e.ends[,1]] == y_hat[e.ends[,2]]
                              })),
                              P_noeth = rowMeans(apply(samples.noeth.ls[[i]], 2, function(y_hat){
                                y_hat[e.ends[,1]] == y_hat[e.ends[,2]]
                              })),stringsAsFactors = F))
}

## Difference in probability
edge.df$P_diff <- edge.df$P_noeth - edge.df$P_base

## Prediction vs. observes
edge.df$Y_Yhat_base <- edge.df$state - edge.df$P_base
edge.df$Y_Yhat_noeth <- edge.df$state - edge.df$P_noeth

## Save
saveRDS(edge.df, file = file.path(tem_res.path, "marg_prob_df.rds"))
edge.df <- readRDS(file.path(tem_res.path, "marg_prob_df.rds"))

## Plot data
plot.df <- rbind(cbind(sample = "Edges w/ ethnic boundary (1986)", 
                       edge.df[edge.df$ethnic & !is.na(edge.df$ethnic), ]))

## Cshapes
cshapes.shp <- readOGR("data/geography/cshapes_2_shapefile/cshapes_2_cow.geojson",
                       paste0("cshapes_2_cow"))
cshapes.shp$startdate <- as.Date(cshapes.shp$start)
cshapes.shp$enddate <- as.Date(cshapes.shp$end)
cshapes.all <- cshapes.shp
cshapes.shp <- raster::crop(cshapes.shp[cshapes.shp$startdate <= as.Date("2011-01-01") &
                                          cshapes.shp$enddate > as.Date("2011-01-01"), ], 
                            plot.europe.shp)

## Means
quant.df <- cbind(do.call(rbind, apply(unique(plot.df[, c("type","sample")]), 1, function(x){
  dens_at(plot.df[plot.df$type == x["type"] & plot.df$sample == x["sample"], "P_diff"],
          fun = mean)
})), unique(plot.df[,c("type", "sample")]))
writeLines(as.character(signif(quant.df$mean[quant.df$type == "Baseline Model"] * 100, 2)),
           file.path(num.path, "margprob_base.tex"))
writeLines(as.character(signif(quant.df$mean[quant.df$type == "Lagged Dependent Variable Model"] * 100, 2)),
           file.path(num.path, "margprob_ldv.tex"))

# Plot

## Make
g <- ggplot(plot.df, aes(x = P_diff)) +
  geom_vline(xintercept = 0, col = "darkgrey", lty = 2) +
  geom_line(stat = "density", position = "identity") +
  geom_segment(data = quant.df, aes(x = mean, xend = mean, y = 0, yend = est.dens)) +
  # geom_line(aes(x = P_base, col = "red"), stat = "density") +
  facet_wrap( ~ type, nrow = 1) +
  theme_minimal() +
  theme(panel.spacing = unit(2, "lines")) +
  scale_x_continuous(breaks = seq(0, 1, by = .25)) +
  xlab("Effect of ethnic boundaries on edges' border probability (2011)") +
  ylab("Density (across edges)")

## Save
png(file.path(fig.path, "marg_prob_change.png"), width = 6, height = 3, res = 400, units = "in")
par(mar = c(0,0,0,0))
plot(g)
dev.off()




# Plot Map
for(m in c(1:2)){
  print(m)
  
  # Plot country borders? 
  cshp <- FALSE 
  
  # Loop over plot types
  for(v in c("example","P_base", "P_noeth")){
    ## Graph
    this.graph <- pool.model.ls[[m]]$g_ls[[length(pool.model.ls[[m]]$g_ls)]]
    
    ## Make base raster
    base.rs <- raster(extent(plot.europe.shp), res = 0.05, crs = "+proj=longlat +datum=WGS84")
    base.rs <- setValues(base.rs, rep(0, ncell(base.rs)))
    
    ## Edge Values
    if(v == "example"){
      e.ends <- ends(this.graph, E(this.graph), names= F)
      y <- pool.model.ls[[m]]$learn_obj$samples_cache
      y <- y[[length(y)]][[1]]
      val.vec <- y[e.ends[,1]] == y[e.ends[,2]]
      col.vec <- viridis(256, direction = 1)[
        1 + round(val.vec * 255)
      ]
    } else if(v %in% c("Y_Yhat_base", "Y_Yhat_noeth")){
      val.vec <- edge.df[edge.df$type == c("Lagged Dependent Variable Model", "Baseline Model")[m],
                         v]
      col.vec <- viridis(256, direction = 1)[
        1 + round((1 + val.vec)/2 * 255)
      ]
    } else {
      val.vec <- edge.df[edge.df$type == c("Lagged Dependent Variable Model", "Baseline Model")[m], 
                         v]
      col.vec <- viridis(256, direction = 1)[
        1 + round(val.vec * 255)
      ]
    }
    stopifnot(length(val.vec) == length(E(this.graph)))
    
    ## Plot
    png(file.path(fig.path, paste0("mapmarg_", v, ifelse(cshp, "_cshp", ""),
                                   ifelse(m == 1, "_ldv", ""), ".png")), 
        width = 6, height = 3, res = 400, units = "in")
    par(mar = c(0,0,2,0))
    
    plot(plot.europe.shp, border = "lightgrey",
         lwd = .4, asp = 1)
    
    plot_spatial_graph(graph, vertex.size = 0.025,
                       edge.color = "lightgrey",
                       vertex.color = "grey",
                       edge.width = .75,
                       add = T)
    plot_spatial_graph(this.graph, vertex.size = 0.025,
                       edge.color = col.vec,
                       edge.width = 1.25,
                       add = T)
    if(cshp){
      plot(cshapes.shp, border = "red",
           lwd = 1, asp = 1, add = T)
      plot(plot.europe.shp, border = "lightgrey",
           lwd = 1, asp = 1, add = T)
    }
    if(!v %in% c("example", "Y_Yhat_base", "Y_Yhat_noeth")){
      raster::plot(base.rs, zlim = c(0, 1), col = viridis(256, direction = -1),
                   legend.only = T, add = T)
    } else if(v %in% c("Y_Yhat_base", "Y_Yhat_noeth")){
      raster::plot(base.rs, zlim = c(-1, 1), 
                   col = viridis(256, direction = -1),
                   legend.only = T, add = T)
    }
    dev.off()
  }
  
}


# MODEL FIT


## Function
simple_roc <- function(labels, scores){
  labels <- labels[order(scores, decreasing=TRUE)]
  data.frame(TPR=cumsum(labels)/sum(labels), 
             FPR=cumsum(!labels)/sum(!labels), labels)
}

## Compute ROC for model
roc.df <-  do.call(rbind, lapply(c("P_base", "P_noeth"), function(i){
  this.graph <- pool.model.ls[[2]]$g_ls[[length(pool.model.ls[[2]]$g_ls)]]
  val.vec <- edge.df[edge.df$type == "Baseline Model", i]
  cbind(simple_roc(labels = E(this.graph)$cshp_2011 == 0, scores = val.vec),
        model = i)
}))


## AUC
writeLines(as.character(signif(trapz(roc.df$FPR[roc.df$model == "P_base"], roc.df$TPR[roc.df$model == "P_base"]), 2)*100),
           file.path(num.path, "auc_base.tex"))
writeLines(as.character(signif(trapz(roc.df$FPR[roc.df$model == "P_noeth"], roc.df$TPR[roc.df$model == "P_noeth"]), 2)*100),
           file.path(num.path, "auc_noeth.tex"))


## ACC
acc <-  sapply(c("P_base", "P_noeth"), function(i){
  this.graph <- pool.model.ls[[2]]$g_ls[[length(pool.model.ls[[2]]$g_ls)]]
  val.vec <- edge.df[edge.df$type == "Baseline Model", i]
  TP = sum((1-val.vec)[(E(this.graph)$cshp_2011 == 1)])
  TN = sum(val.vec[(E(this.graph)$cshp_2011 == 0)])
  (TP + TN) / length(val.vec)
})

## Plot
png(file.path(fig.path, "baseline_roc.png"), width = 3.5, height = 2.5, res = 400, units = "in")
g <- ggplot(roc.df, aes(x = FPR, y = TPR, 
                        group = as.factor(model), col =  as.factor(model), 
                        lty =  as.factor(model))) +
  geom_abline(intercept = 0, slope = 1, color = "darkgrey") + 
  geom_step() +
  ylab("True positive rate") + xlab("False positive rate") +
  theme_minimal() + 
  theme(legend.position = c(0.82, 0.25),
        legend.direction = "vertical",
        legend.spacing.y = unit(.01, 'cm')
  ) +
  labs(color = "Model", lty = "Model")
print(g)
dev.off()


# STOP CLUSTER
if(stop.cl){
  stopCluster(cl)
  rm(cl)
}
