# This script runs in order each script necessary to produce all figures in the manuscript and supporting information.

# N.B. Running all scripts takes approximately 7 hours.

# Timings for Apple M1 Pro with 16GB memory:
# code/hk-cleaning.R: success (0.00 minutes)
# code/hk-analysis.R: success (41.55 minutes)
# code/hk-figures.R: success (0.01 minutes)
# code/hk-prior-predictive.R: success (5.10 minutes)
# code/hk-replication-priors.R: success (25.58 minutes)
# code/hk-replication-plot2.R: success (0.01 minutes)
# code/hk-replication-plot3.R: success (0.00 minutes)
# code/gibbs-assumptions-simulation.R: success (129.69 minutes)
# code/gibbs-covariate-simulation-betas.R: success (115.64 minutes)
# code/gibbs-covariate-simulation-psis.R: success (117.83 minutes)
# code/sim-visualizations.R: success (0.02 minutes)

# load packages

library(remotes)
# remotes::install_github('mattblackwell/prepost') #install.package
library(prepost)

# for seed setting
RNGkind("L'Ecuyer-CMRG")

# Create subdirectory to store output

if(!dir.exists('output')){
  dir.create(('output'))
}

if(!dir.exists('figures')){
  dir.create(('figures'))
}


# Collect results
results <- list()

# Scripts to run (in order)
scripts <- c(
  'code/hk-cleaning.R',
  'code/hk-analysis.R',
  'code/hk-figures.R',
  'code/hk-prior-predictive.R',
  'code/hk-replication-priors.R',
  'code/hk-replication-plot2.R',
  'code/hk-replication-plot3.R',
  'code/gibbs-assumptions-simulation.R',
  'code/gibbs-covariate-simulation-betas.R',
  'code/gibbs-covariate-simulation-psis.R',
  'code/sim-visualizations.R'
)


for (script_name in scripts) {  
  cat(sprintf("\nStarting %s at %s\n", script_name, format(Sys.time(), "%Y-%m-%d %H:%M:%S")))
  
  # Clear environment except for our key variables
  keep_vars <- c("scripts", "results", "script_name")  
  rm(list = setdiff(ls(), keep_vars))
  gc()
  
  p.time <- proc.time()
  
  
  tryCatch({
    source(script_name)
    runtime <- (proc.time() - p.time)[3]/60  # Convert to minutes
    results[[script_name]] <- list(
      status = "success",
      runtime = runtime,
      error = NULL
    )
    cat(sprintf("Successfully completed in %.2f minutes\n", runtime))
    
  }, error = function(e) {
    runtime <- (proc.time() - p.time)[3]/60
    results[[script_name]] <- list(
      status = "error",
      runtime = runtime,
      error = as.character(e)
    )
    cat(sprintf("Failed after %.2f minutes with error: %s\n", runtime, as.character(e)))
  })
}

# Print summary at the end
cat("\nSummary of all runs:\n")
for (script_name in names(results)) {
  cat(sprintf("%s: %s (%.2f minutes)\n", 
              script_name, 
              results[[script_name]]$status, 
              results[[script_name]]$runtime))
}
