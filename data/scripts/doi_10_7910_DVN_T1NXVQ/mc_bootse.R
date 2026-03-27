######################
# MONTE CARLO EXPERIMENT
#
# BOOTSTRAPPED SEs
#
#' Run via commandline:
#'  cd /home/carlvs/Projects/state_tess
#'  nohup Rscript scripts/monte_carlos/mc_bootse.R  >logs/mc_bootse.log>&1 &
#'  
#'  Forced termination of running MC, e.g. via: pkill -f slaveRSOCK
#######################
rm(list = ls())

# Wrappers around SCCRF functions
path <- "scripts/functions"
for(f in list.files(path, recursive = T)){
  source(file.path(path, f))
}

# Size of Network ##

# Directory
output.dir <- "data/monte_carlo_res/mc_bootse"

# Parameters
mc_param_df <- expand.grid(seed = 1:100,  # Increase number of sims
                           beta0 = seq(-2, 0, by = 1),  # fine-grained sweep
                           beta1 = seq(2, 0, by = -1),     # Dito
                           N_sqrd = 32L,  # Vary # of obs
                           burnin = as.integer(100),  # Increase burnin rate
                           num_instances = c(1L), # Number of instances
                           stringsAsFactors = FALSE, 
                           partemp = c(0),
                           sd = 1,
                           bootse = T)


# Do it
estimate_montecarlo(mc_param_df = mc_param_df, 
                    output.dir = output.dir, 
                    ncore = 45, patch.size = 40)



