######################
# MONTE CARLO EXPERIMENT
#
# SIZE AND NUMBER OF INSTANCES
#
#' Run via commandline:
#'  cd /home/carlvs/Projects/state_tess
#'  nohup Rscript scripts/monte_carlos/mc_size.R  >logs/mc_size.log>&1 &
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
output.dir <- "data/monte_carlo_res/mc_size"

# Parameters
mc_param_df <- expand.grid(seed = 1:100,  # Increase number of sims
                           beta0 = seq(-2, 0, by = 1),  # fine-grained sweep
                           beta1 = seq(2, 0, by = -1),     # Dito
                           N_sqrd = c(4L, 8L, 16L, 32L, 64L),  # Vary # of obs
                           burnin = as.integer(100),  # Increase burnin rate
                           num_instances = c(1L), # Number of instances
                           stringsAsFactors = FALSE, 
                           partemp = c(0),
                           sd = 1,
                           bootse = F)


# Do it
estimate_montecarlo(mc_param_df = mc_param_df, 
                    output.dir = output.dir, 
                    ncore = 45, patch.size = 40)





# Number of Instances ##

# Directory
output.dir <- "data/monte_carlo_res/mc_instances"

# Parameters
mc_param_df <- expand.grid(seed = 1:100,  # Increase number of sims
                           beta0 = seq(-2, 0, by = 1),  # fine-grained sweep
                           beta1 = seq(2, 0, by = -1),     # Dito
                           N_sqrd = c(16L),  # Vary # of obs
                           burnin = as.integer(100),  # Increase burnin rate
                           num_instances = c(1L, 2L, 4L, 8L, 16L), # Number of instances
                           stringsAsFactors = FALSE, 
                           partemp = c(0),
                           sd = 1,
                           bootse = F)


# Do it
estimate_montecarlo(mc_param_df = mc_param_df, 
                    output.dir = output.dir, 
                    ncore = 45, patch.size = 40)



