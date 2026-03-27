#############################
# SELF-DETERMINATION ANALYSES: SHIFTING THE NETWORK
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
rm(list = ls())

# Load Globals
source("scripts/analysis/analysis_globals.R")

# Variables
treat <- c( "diff.capeth.tv" )
contr.vars <- c("log(cap.dist)","log(border.dist)",
                "log(pop +1)", "median.altitude" , "median.slope",
                "diff.river", "abramstate.hist", "watershed.diff", "elevmean")



# ROBUSTNESS CHECK: SHIFT #####

# Load
model.ls <- readRDS(file.path(tem_res.path, "shiftcheck_sdmcox.rds"))
main.model.ls <- readRDS(file.path(tem_res.path, "mainmodel_sdmcox.rds"))

# Extract coefs
coef.df <- do.call(rbind, lapply(seq_len(length(model.ls) + 1), function(i){
  if(i == 1){
    ml <- main.model.ls
  } else {
    ml <- model.ls[[i - 1]]
  }
  do.call(rbind, lapply(seq_along(ml), function(j){
    pos <- which(names(ml[[j]]$coefficients) == treat)
    data.frame(sim = i,
               model = j,
               coef = ml[[j]]$coefficients[pos],
               se = ml[[j]]$var[pos, pos]^.5,
               stringsAsFactors = F)
  }))
}))

## Prepare plot
plot.df <- coef.df
plot.df$outcome <- rep(c("Secessionist Claim", "Secessionist Civil War", "Secession"), 
                       each = 2)
plot.df$outcome <- factor(plot.df$outcome , levels = unique(plot.df$outcome),
                          ordered = T)
plot.df$spec <- c("Baseline", "Stratified by\ncountry-year")


## Mean
mean.df <- cbind(do.call(rbind, apply(unique(plot.df[, c("outcome", "spec"), drop = F]), 1, function(x){
  dens_at(plot.df[plot.df$outcome == x["outcome"] & 
                    plot.df$spec == x["spec"] & 
                    plot.df$sim != 1, "coef"],
          fun = mean)
})), unique(plot.df[,c("outcome", "spec")]))


### Plot
g <- ggplot(plot.df[plot.df$sim != 1,],
            aes(x = coef)) +
  geom_line(stat = "density", aes( x = coef + 1.96*se),
            lty = 2, col = "darkgrey") +
  geom_line(stat = "density", aes( x = coef - 1.96*se),
            lty = 2, col = "darkgrey") +
  geom_line(stat = "density") +
  geom_segment(data = mean.df, 
               aes(x = mean, xend = mean, y = 0, yend = est.dens)) +
  theme_minimal() +
  facet_grid(spec ~ outcome) +
  geom_point(data = plot.df[plot.df$sim == 1,], aes(y = 3), 
             col = "red") +
  geom_segment(data = plot.df[plot.df$sim == 1,], 
               aes(x = coef + 1.96*se, xend = coef - 1.96*se, y = 3, yend = 3), 
               col = "red") +
  ylab("Density") + xlab("Estimate of effect of non-coethnic capital") +
  # xlim(c(0,1.5)) +
  theme(panel.spacing = unit(2, "lines")) +
  geom_vline(xintercept = 0, col = "darkgrey", lty = 2)



### Save
png(file.path(fig.path, "survival_shift.png"), width = 6.5, height = 2.5, 
    unit = "in", res = 400)
print(g)
dev.off()





