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
  log <- setup_logging("wrp_1")
  log_system_info("WRP Processing Pipeline (Phase 1)")
}

# Configure terra options (suppress progress bars globally)
terra::terraOptions(progress = 0)

# Define script execution order
script_paths <- c(
  "Code/19_wrp/01_process/00_process_wrp.R",
  "Code/19_wrp/01_process/01a_process_damage/01_clean_coord.R",
  "Code/19_wrp/01_process/01a_process_damage/02_clean_cell.R",
  "Code/19_wrp/01_process/01a_process_damage/03_process_tempdamage.R",
  "Code/19_wrp/01_process/01b_process_survey_zones/01_prepare_zone_ids.R",
  "Code/19_wrp/01_process/01b_process_survey_zones/02_process_botswana.R",
  "Code/19_wrp/01_process/01b_process_survey_zones/02_process_surveyzones.R",
  "Code/19_wrp/01_process/01b_process_survey_zones/03_create_zone_neighbors.R",
  "Code/19_wrp/01_process/01b_process_survey_zones/04_merge_zone_and_survey.R",
  "Code/19_wrp/01_process/01c_extract_survey_dates.R",
  "Code/19_wrp/01_process/01d_get_included_countries.R",
  "Code/19_wrp/01_process/01f_process_vdem.R",
  "Code/19_wrp/01_process/02_estimate_zonal_stats/01_process_pop.R",
  "Code/19_wrp/01_process/02_estimate_zonal_stats/01_calculate_admin_area.R",
  "Code/19_wrp/01_process/02_estimate_zonal_stats/02_process_zonal.R",
  "Code/19_wrp/01_process/02_process_country_data.R",
  "Code/19_wrp/01_process/03_create_unweighted_data.R"
)

# Execute all scripts
results <- execute_scripts(script_paths)

# Clean up logging (only if not being run from master script)
if (!exists("RUNNING_FROM_MASTER") || !RUNNING_FROM_MASTER) {
  cleanup_logging(log$connection)
}