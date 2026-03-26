#!/usr/bin/env Rscript

source("Code/fun/project_utils.R")

# Set working directory to project root
tryCatch({
  setwd(find_project_root())
}, error = function(e) {
  cat("Warning: Could not find project root. Using current directory:", getwd(), "\n")
})

# Clear and recreate all output directories for clean replication
# (only if not being run from master script)
if (!exists("RUNNING_FROM_MASTER") || !RUNNING_FROM_MASTER) {
  cat("\n")
  cat("Clearing output directories for fresh replication...\n")
  clear_output_dirs()
  ensure_project_dirs()
  cat("✓ Project directories ready\n\n")
} else {
  ensure_project_dirs()
}

# Setup logging (only if not being run from master script)
if (!exists("RUNNING_FROM_MASTER") || !RUNNING_FROM_MASTER) {
  log <- setup_logging("ces")
  log_system_info("CES Analysis Pipeline")
}

# Define script execution order
script_paths <- c(
  "Code/ces/01_process_hsiang_damage.R",
  "Code/ces/02_usa_damage_map.R",
  "Code/ces/01_process_fema.R",
  "Code/ces/01a_process_ces.R",
  "Code/ces/01_process_temp.R",
  "Code/ces/01b_calc_temp_shocks.R",
  "Code/ces/01c_merge_ces.R",
  "Code/ces/02_analyze_ces.R",
  "Code/ces/02_plot_shocks_fig7.R"
)

# Execute all scripts
results <- execute_scripts(script_paths)

# Clean up logging (only if not being run from master script)
if (!exists("RUNNING_FROM_MASTER") || !RUNNING_FROM_MASTER) {
  cleanup_logging(log$connection)
}