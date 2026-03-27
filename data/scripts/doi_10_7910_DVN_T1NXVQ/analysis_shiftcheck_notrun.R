#############################
# SELF-DETERMINATION ANALYSES: SHIFTING THE NETWORK
# 
# ### DO NOT RUN THIS FILE - IT NEEDS "DEEP" INPUT DATA NOT INCLUDED IN THE 
# ### REPLICATION -- RESULTS ARE SAVED AS file.path(tem_res.path, "shiftcheck_sdmcox.rds")
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
  cl <- make_cluster(ncore)
  stop.cl <- T
} else {
  stop.cl <- F
}

## Set up cluster
res <- clusterEvalQ(cl, expr = {
  # Load Globals
  source("scripts/analysis/analysis_globals.R")

  # Load Continents
  coverage <- readOGR(file.path("data", "geography/continents"),
                      "continents")
  coverage <- coverage[coverage$continent == "Europe",]

  # Expansion factors
  exp_factor.df <- readRDS(file.path("data/analysis_data", "res_optim.rds"))

  # NULL
  NULL
})
clusterExport(cl, list("treat", "contr.vars"))


# Make Data and Fit Model for each specification
print(paste("SHIFT MODEL ESTIMATION"))
model.ls <- parLapply(cl, 1:100, function(i){
  print("Prepare Data")
  print(Sys.time())

  # Make Graph with shift
  set.seed(i)

  ## Shift
  shift_x = runif(1, min = 0, max = 10)
  shift_y = runif(1, min = 0, max = 10)

  ## Select the right expansion factor
  exp_factor <-  exp_factor.df$exp_factor[exp_factor.df$continent == "Europe" &
                                            exp_factor.df$edge_length == 100 &
                                            exp_factor.df$grid.type == "hexagonal"]

  ## Make network
  graph <- albers_hexgrid(coverage,
                          edge_length = 100,
                          shift_x = shift_x,
                          shift_y = shift_y,
                          exp_factor = exp_factor)

  # Make points
  points.df <- do.call(cbind, lapply(vertex_attr_names(graph), function(v){
    data.frame(as.matrix(vertex_attr(graph, v), ncol = 1))
  }))

  colnames(points.df) <- vertex_attr_names(graph)
  points.df$pts.id <- 1:nrow(points.df)

  # Prepare data
  points.yrs.df <- prep_sdm_analysis(points.df = points.df)

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
    m$shift_x = shift_x
    m$shift_y = shift_y

    m
  })


  # Return
  model.ls
})
saveRDS(model.ls, file = file.path(tem_res.path, "shiftcheck_sdmcox.rds"))



# STOP CLUSTER
if(stop.cl){
  stopCluster(cl)
  rm(cl)
}



