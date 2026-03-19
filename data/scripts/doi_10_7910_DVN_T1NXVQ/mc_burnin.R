######################
# MONTE CARLO EXPERIMENT
#
# GIBBS-SAMPLIG // BURNIN RATE
#
#' Run via commandline:
#'  cd /home/carlvs/Projects/state_tess
#'  nohup Rscript scripts/monte_carlos/mc_burnin_3.R  >logs/mc_burnin_3.log>&1 &
#'  
#'  Forced termination of running MC, e.g. via: pkill -f slaveRSOCK
#######################
rm(list = ls())

# Wrappers around SCCRF functions
path <- "scripts/functions"
for(f in list.files(path, recursive = T)){
  source(file.path(path, f))
}



# Directory
output.dir <- "data/monte_carlo_res/mc_burnin"

# Parameters
mc_param_df <- expand.grid(seed = 1:100,  # Increase number of sims
                           beta0 = seq(-2, 0, by = 1),  # fine-grained sweep
                           beta1 = seq(2, 0, by = -1),     # Dito
                           N_sqrd = c(32L),  # Vary # of obs
                           burnin = as.integer(c(1, 5, 10, 50, 100, 500, 1000)),  # Increase burnin rate 
                           num_instances = c(1L), # Number of instances
                           partemp = c(0),
                           sd = 1,
                           bootse = F,
                           stringsAsFactors = FALSE)


# Do it
estimate_montecarlo(mc_param_df = mc_param_df, 
                    output.dir = output.dir, 
                    ncore = 45, patch.size = 40)
  
  