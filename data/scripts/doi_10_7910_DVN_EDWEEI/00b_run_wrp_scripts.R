# Load shared utilities
source("Code/fun/project_utils.R")

# Set working directory to project root
tryCatch({
  setwd(find_project_root())
}, error = function(e) {
  cat("Warning: Could not find project root. Using current directory:", getwd(), "\n")
})

# Note: This script assumes Phase 1 has already been run and Data/inter/19_wrp exists
# It does NOT clear directories since it depends on Phase 1 outputs

# Ensure all required directories exist
cat("Setting up project directories...\n")
ensure_project_dirs()
cat("✓ Project directories ready\n\n")

# Setup logging (only if not being run from master script)
if (!exists("RUNNING_FROM_MASTER") || !RUNNING_FROM_MASTER) {
  log <- setup_logging("wrp_2")
  log_system_info("WRP Analysis Pipeline (Phase 2)")
}

# Configure terra options (suppress progress bars globally)
if (requireNamespace("terra", quietly = TRUE)) {
  terra::terraOptions(progress = 0)
}

# Define script execution order
script_paths <- c(
  "Code/19_wrp/01_process/05_create_weighted_data.R",
  "Code/19_wrp/02_analysis/estimate_shock_effect_models.R",
  "Code/19_wrp/02_analysis/fig_1_data_ex_map.R",
  "Code/19_wrp/02_analysis/fig_2_vuln_and_sample.R",
  "Code/19_wrp/02_analysis/tab_b1_summary_stats.R"
)

# Execute all scripts
results <- execute_scripts(script_paths)

# Clean up logging (only if not being run from master script)
if (!exists("RUNNING_FROM_MASTER") || !RUNNING_FROM_MASTER) {
  cleanup_logging(log$connection)
}
