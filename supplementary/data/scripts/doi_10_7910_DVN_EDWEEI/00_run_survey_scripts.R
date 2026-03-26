# Load shared utilities
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
  log <- setup_logging("survey")
  log_system_info("Survey Jan24 Analysis Pipeline")
}

# Define script execution order
script_paths <- c(
  "Code/survey_jan24/01_clean_survey.R",
  "Code/survey_jan24/02_vulnerability_correlates.R",
  "Code/survey_jan24/03_sample_desc.R"
)

# Execute all scripts
results <- execute_scripts(script_paths)

# Clean up logging (only if not being run from master script)
if (!exists("RUNNING_FROM_MASTER") || !RUNNING_FROM_MASTER) {
  cleanup_logging(log$connection)
}
