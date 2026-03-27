#############################
# PLOTTING OF MONTE-CARLO RESULTS: RE INFERENCE
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


########################
# BURN IN   ############
########################



# LOAD MC RESULTS #####
mc_path <- file.path("data/monte_carlo_res/mc_burnin")

## Read parameters and results
mc_param_tb <- as_tibble(readRDS(file.path(mc_path, "parameters.rds")))
mc_results_tb <- as_tibble(readRDS(file.path(mc_path, "results.rds")))

## Check 
stopifnot(nrow(mc_param_tb) == nrow(mc_results_tb))


## PARAMETER INFERENCE ####

## Compute errors
mc_eval_tb <- join(mc_results_tb, mc_param_tb, by = c("sim.id"), 
                   type = "left", match = "first")
mc_eval_tb$b0_err <- mc_eval_tb$b0 - mc_eval_tb$beta0
mc_eval_tb$b1_err <- mc_eval_tb$b - mc_eval_tb$beta1

plot_tb <- mc_eval_tb

plot_tb$beta1 <- factor(plot_tb$beta1, levels = c(2,1,0), ordered = T)


# VIOLINS: Plot beta0 and beta1 bias

## Combined plot
g_ls <- lapply(c(0,1), function(b){
  # plot_tb <- plot_tb[plot_tb$beta0>= -1 & plot_tb$beta1 <= 1, ]
  if(b == 0){
    plot_tb$dv <- plot_tb$b0_err
  } else {
    plot_tb$dv <- plot_tb$b1_err
  }
  g <- ggplot(plot_tb, aes(as.numeric(factor(burnin)),  dv)) + 
    geom_hline(yintercept = 0, lty = 2, col = "darkgrey") +
    geom_violin(aes(group = factor(burnin)), fill = "grey", color = "darkgrey") + 
    stat_summary( fun.y='mean', geom='line') +
    stat_summary(aes(group = factor(burnin)), fun.y='mean', geom='point', size = 0.5) +
    stat_summary(aes(group = factor(burnin)), fun.y='mean', geom='point', size = 0.1, col = "white")  +
    theme_minimal() + scale_x_continuous(name = "Burn-in rate", 
                                         breaks = 1:length(unique(plot_tb$burnin)),
                                         labels = unique(plot_tb$burnin))+
    ylim(range(c(plot_tb$b0_err, plot_tb$b1_err))) +
    theme(axis.text.x = element_text(angle = 90, hjust = 1, vjust = .5, size = rel(1)))
  if(b == 1){
    
    g <- g  +
      ggtitle("Beta 1 Bias")+
      facet_grid(beta1 ~ beta0, labeller = facet_labs) +
      theme(axis.title.y=element_blank(),
            axis.text.y=element_blank(),
            axis.ticks.y=element_blank()) +
      NULL
  } else {
    g <- g  +
      ylab(paste0("Bias")) +
      ggtitle("Beta 0 Bias")+
      facet_grid(beta1 ~ beta0, 
                 labeller = function(x){facet_labs(x, dict = c(beta0 = "Beta 0", beta1 = NA))}) +
      NULL
  }
  g
})

# Save
png(file.path(fig.path, "inf_mc_burnin.png"), width = 6, height = fig.height, 
    res = 400, units = "in")
par(mar = c(0,0,0,0), mfrow = c(3,1))
grid.arrange(g_ls[[1]], g_ls[[2]], nrow = 1, widths = c(.55,.45))
dev.off()



########################
# SIZE   ###############
########################



# LOAD MC RESULTS #####
mc_path <- file.path("data/monte_carlo_res/mc_size")

## Read parameters and results
mc_param_tb <- as_tibble(readRDS(file.path(mc_path, "parameters.rds")))
mc_results_tb <- as_tibble(readRDS(file.path(mc_path, "results.rds")))

## Check 
stopifnot(nrow(mc_param_tb) == nrow(mc_results_tb))

## PARAMETER INFERENCE ####

## Compute errors
mc_eval_tb <- join(mc_results_tb, mc_param_tb, by = c("sim.id"), 
                   type = "left", match = "first")
mc_eval_tb$b0_err <- mc_eval_tb$b0 - mc_eval_tb$beta0
mc_eval_tb$b1_err <- mc_eval_tb$b - mc_eval_tb$beta1

plot_tb <- mc_eval_tb

plot_tb$beta1 <- factor(plot_tb$beta1, levels = c(2,1,0), ordered = T)

plot_tb$size <- plot_tb$N ^2

## VIOLINS: Plot beta0 and beta1 bias


## Combined plot

### Beta 1 and 2
g_ls <- lapply(c(0,1), function(b){
  if(b == 0){
    plot_tb$dv <- plot_tb$b0_err
  } else {
    plot_tb$dv <- plot_tb$b1_err
  }
  g <- ggplot(plot_tb, aes(as.numeric(factor(size)),  dv)) + 
    geom_hline(yintercept = 0, lty = 2, col = "darkgrey") +
    geom_violin(aes(group = factor(size)), fill = "grey", color = "darkgrey") + 
    stat_summary(fun.y='mean', geom='line') +
    stat_summary(aes(group = factor(size)), fun.y='mean', geom='point', size = 0.5) +
    stat_summary(aes(group = factor(size)), fun.y='mean', geom='point', size = 0.1, col = "white")  +
    theme_minimal() + scale_x_continuous(name = "Network size", 
                                         breaks = 1:length(unique(plot_tb$size)),
                                         labels = unique(plot_tb$size))+
    ylim(range(c(plot_tb$b0_err, plot_tb$b1_err))) +
    theme(axis.text.x = element_text(angle = 90, hjust = 1, vjust = .5, size = rel(1)))
  if(b == 1){
    
    g <- g  +
      ggtitle("Beta 1 Bias")+
      facet_grid(beta1 ~ beta0, labeller = facet_labs) +
      theme(axis.title.y=element_blank(),
            axis.text.y=element_blank(),
            axis.ticks.y=element_blank()) +
      NULL
  } else {
    g <- g  +
      ylab(paste0("Bias")) +
      ggtitle("Beta 0 Bias")+
      facet_grid(beta1 ~ beta0, 
                 labeller = function(x){facet_labs(x, dict = c(beta0 = "Beta 0", beta1 = NA))}) +
      NULL
  }
  g
})

# Save
png(file.path(fig.path, "inf_mc_size.png"),width = 6, height = fig.height, 
    res = 400, units = "in")
par(mar = c(0,0,0,0), mfrow = c(3,1))
grid.arrange(g_ls[[1]], g_ls[[2]], nrow = 1, widths = c(.55,.45))
dev.off()




########################
# INSTANCES   ##########
########################



# LOAD MC RESULTS #####
mc_path <- file.path("data/monte_carlo_res/mc_instances")

## Read parameters and results
mc_param_tb <- as_tibble(readRDS(file.path(mc_path, "parameters.rds")))
mc_results_tb <- as_tibble(readRDS(file.path(mc_path, "results.rds")))

## Check 
stopifnot(nrow(mc_param_tb) == nrow(mc_results_tb))

## PARAMETER INFERENCE ####

## Compute errors
mc_eval_tb <- join(mc_results_tb, mc_param_tb, by = c("sim.id"), 
                   type = "left", match = "first")
mc_eval_tb$b0_err <- mc_eval_tb$b0 - mc_eval_tb$beta0
mc_eval_tb$b1_err <- mc_eval_tb$b - mc_eval_tb$beta1

plot_tb <- mc_eval_tb

plot_tb$beta1 <- factor(plot_tb$beta1, levels = c(2,1,0), ordered = T)



## VIOLINS: Plot beta0 and beta1 bias


## Combined plot

### Beta 1 and 2
g_ls <- lapply(c(0,1), function(b){
  if(b == 0){
    plot_tb$dv <- plot_tb$b0_err
  } else {
    plot_tb$dv <- plot_tb$b1_err
  }
  g <- ggplot(plot_tb, aes(as.numeric(factor(num_instances)),  dv)) + 
    geom_hline(yintercept = 0, lty = 2, col = "darkgrey") +
    geom_violin(aes(group = factor(num_instances)), fill = "grey", color = "darkgrey") + 
    stat_summary(fun.y='mean', geom='line') +
    stat_summary(aes(group = factor(num_instances)), fun.y='mean', geom='point', size = 0.5) +
    stat_summary(aes(group = factor(num_instances)), fun.y='mean', geom='point', size = 0.1, col = "white")  +
    theme_minimal() + scale_x_continuous(name = "Number of instances", 
                                         breaks = 1:length(unique(plot_tb$num_instances)),
                                         labels = unique(plot_tb$num_instances))+
    ylim(range(c(plot_tb$b0_err, plot_tb$b1_err))) +
    theme(axis.text.x = element_text(angle = 90, hjust = 1, vjust = .5, size = rel(1)))
  if(b == 1){
    
    g <- g  +
      ggtitle("Beta 1 Bias")+
      facet_grid(beta1 ~ beta0, labeller = facet_labs) +
      theme(axis.title.y=element_blank(),
            axis.text.y=element_blank(),
            axis.ticks.y=element_blank()) +
      NULL
  } else {
    g <- g  +
      ylab(paste0("Bias")) +
      ggtitle("Beta 0 Bias")+
      facet_grid(beta1 ~ beta0, 
                 labeller = function(x){facet_labs(x, dict = c(beta0 = "Beta 0", beta1 = NA))}) +
      NULL
  }
  g
})

# Save
png(file.path(fig.path, "inf_mc_instances.png"), width = 6, height = fig.height, 
    res = 400, units = "in")
par(mar = c(0,0,0,0), mfrow = c(3,1))
grid.arrange(g_ls[[1]], g_ls[[2]], nrow = 1, widths = c(.55,.45))
dev.off()



