#############################
# SELF-DETERMINATION ANALYSES: VARYING SPATIAL STRUCTURES
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

## Make cluster
if(!exists("cl")){
  ncore = 4
  cl <- make_cluster(ncore)
  stop.cl <- T
} else {
  stop.cl <- F
}

clusterExport(cl, list("treat", "contr.vars"))

# ESTIMATE MODEL #########

# Load Data and Fit Model for each specification
print(paste("RESOLUTION MODEL ESTIMATION"))

# Structures
structures <- c("hexagonal", "quad4", "triangular", "random")

# Make data and estimate
model.ls <- parLapply(cl, structures, function(struct){
  # Load data
  points.yrs.df <- readRDS(file.path("data/analysis_data/", paste0("sdm_", struct,".rds")))
  
  # Globals

  ## Survival outcomes
  surv.outcomes <- c("sdm.onset", "tco.onset", "breaks.away")

  ## Estimate
  model.ls <- unlist(lapply(surv.outcomes, function(o){
    f.vec <- paste0("Surv(yrs.since.start, yrs.since.end, ", o, ")", " ~  ",
                    paste(c(treat,contr.vars), collapse = "+"),
                    c("", " + strata(cow.yrs)"),
                    " + cluster(cluster.id)")
    lapply(f.vec, function(f) coxph(as.formula(f),points.yrs.df))
  }), recursive = F)

  # Clean models shift
  model.ls <- lapply(model.ls, function(m){
    m$structure <- struct
    m
  })


  # Return
  model.ls
})
saveRDS(model.ls, file = file.path(tem_res.path, "struct_sdmcox.rds"))


# PLOT #####

# Load
# model.ls <- readRDS(file.path(tem_res.path, "struct_sdmcox.rds"))

# Extract coefs
coef.df <- do.call(rbind, lapply(seq_len(length(model.ls)), function(i){
  ml <- model.ls[[i]]
  do.call(rbind, lapply(seq_along(ml), function(j){
    pos <- which(names(ml[[j]]$coefficients) == treat)
    data.frame(sim = i,
               model = j,
               structure = ml[[j]]$structure,
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
plot.df$structure <- factor(plot.df$structure, 
                  levels = unique(plot.df$structure),
                  labels = c("hex.", "quad.", "triang.", "rand."),
                  ordered = T)

### Plot
g <- ggplot(plot.df,
            aes(y = coef, x = structure)) + 
  theme_minimal() +
  facet_grid(spec ~ outcome) +
  geom_point() +
  geom_segment(aes(y = coef + 1.96*se, yend = coef - 1.96*se, 
                   x = structure, xend = structure)) +
  xlab("Point sampling structure") + 
  ylab("Estimate of effect\nof non-coethnic capital") +
  theme(axis.text.x = element_text(angle = 90, vjust = .5, hjust = 1),
        panel.spacing = unit(1, "lines")) +
  geom_hline(yintercept = 0, col = "darkgrey", lty = 2)



### Save
png(file.path(fig.path, "survival_struct.png"), width = 6.5, height = 2.5, 
    unit = "in", res = 400)
print(g)
dev.off()



# STOP CLUSTER
if(stop.cl){
  stopCluster(cl)
  rm(cl)
}



