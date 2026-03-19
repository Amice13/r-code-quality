source("Carter/Carter_DGP.R")


# ---------------------------------------------------------------------
#  experimental factors
k_set <- c(10, 30, 60, 100)							# number of studies in each MA
delta_set <- c(0, .2, .5, .8)						# true mean of effect sizes
qrpEnv_Set <- c("none", "med", "high")	# QRP environment
censor_set <- c("none", "med", "high")	# publication bias
tau_set <- c(0, .2, .4)									# heterogeneity; assumed to follow a normal distribution

# params stores all possible combinations of experimental factors
paramsONE <- expand.grid(k=k_set, delta=delta_set, qrpEnv=qrpEnv_Set, censor=censor_set, tau=tau_set)


k_set <- c(200, 400, 800)							  # number of studies in each MA
delta_set <- c(0, .2, .5, .8)						# true mean of effect sizes
qrpEnv_Set <- c("none", "med", "high")	# QRP environment
censor_set <- c("none", "med", "high")	# publication bias
tau_set <- c(0, .2, .4)									# heterogeneity; assumed to follow a normal distribution

# params stores all possible combinations of experimental factors
paramsTWO <- expand.grid(k=k_set, delta=delta_set, qrpEnv=qrpEnv_Set, censor=censor_set, tau=tau_set)
params <- rbind(paramsONE, paramsTWO)
params$Condition <- c(1:nrow(params))
rownames(params) <- NULL
print(paste0(nrow(params), " fully crossed experimental conditions have been generated."))


## ======================================================================
## THE SIMULATION
## ======================================================================
for (j in beginN:endN) {
  log1 <- paste0(Sys.time(), ", NEW CONDITION: computing condition ", params$Condition[j], "/", nrow(params))
  print(log1)
    # res stores the results
    res <- data.frame()
    for (i in 1:SimulationLength) {
      log2 <- paste0(Sys.time(),  ": computing condition ", params$Condition[j], "/", nrow(params), "; rep = ", i)
      print(log2)
      MA1 <- simMA(k=params[j, "k"], delta=params[j, "delta"], tau=params[j, "tau"], censorFunc=as.character(params[j, "censor"]), qrpEnv=as.character(params[j, "qrpEnv"]))
      # remove rownames (otherwise cbind complains)
      rownames(MA1) <- NULL
      p <- params[j, ]
      rownames(p) <- NULL
      # combine sim settings and results
      res0 <- cbind(
        replication	= i, 
        condition	= params$Condition[j],
        # settings of the condition
        p,
        # results of the computation
        as.matrix(MA1))
      res <- rbind(res, res0)
    } 
  save(res, file=paste0("Carter/simData/simData_condition_", params$Condition[j], ".RData"), compress="gzip")
} 

