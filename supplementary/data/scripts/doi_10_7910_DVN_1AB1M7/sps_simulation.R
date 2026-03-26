library(metafor)
require(parallel)
require(doSNOW)
library(spsR)

# ##################################################
# Simulation Design (Parameters)
# ##################################################

source("functions.R")

OUT_DIR  <- paste0('../', "Output")
SIM_SIZE <- 100 
data_type <- "data"
ver_use  <- 1
testing  <- FALSE
HPC <- TRUE

load("../Data/Simulation/dgp_naumann.rdata")
for_use <- as.formula(~ GDP + Immigration + Unemp. + Age + Immi_Support + I(GDP^2) + I(Immi_Support^2))

# Simulation Design
X_cov <- dgp_naumann$X_cov
dgp_coef <- dgp_naumann$coef
bet_se <- dgp_naumann$bet_se
within_se <- dgp_naumann$within_se

design <- expand.grid("num_var" = c(3, 4, 5, 7, 9, 12, 15, 20, 25, 30),
                     "N_s" = c(3, 6, 9),
                     "coef_mul" = c(1),
                     "se_mul" = c(1),
                     "N" = c(20))

total_sim_size <- SIM_SIZE * nrow(design)

## #####################
## run on HPC 
## ######################

# Read job array info
job_idx <- as.numeric(Sys.getenv("SLURM_ARRAY_TASK_ID"))
n_jobs  <- as.numeric(Sys.getenv("SLURM_ARRAY_TASK_COUNT"))

if(testing == TRUE){
  job_idx <- 1
  n_jobs  <- 1
}

# Timing
start_time <- Sys.time()
run_ts <- paste0(strftime(start_time, "%y%m%d"), "_ver_", ver_use)

# total simulation size
total_sim_size <- SIM_SIZE * nrow(design)

# Map from job_idx to sim_num
n_sims  <- total_sim_size
n_tasks <- floor(n_sims/n_jobs)
remainder <- n_sims %% n_jobs
jobs <- (n_tasks*(job_idx-1)+1):(n_tasks*(job_idx))
if (job_idx<=remainder) {
  jobs <- c(jobs, n_jobs*n_tasks+job_idx)
}

if(testing == TRUE){
  jobs <- 1:3
}

# Run
for (idx in jobs){
  out <- tryCatch(
    {sps_simulation(s = idx,
                    total_sim_size = total_sim_size,
                    design = design,
                    HPC = HPC)},
    error=function(e){
      message(paste0("ERROR: ", e))
      return(NULL)},
    warning=function(w){
      message(paste0("WARNING: ", w))
      return(NULL)}
  )
}
