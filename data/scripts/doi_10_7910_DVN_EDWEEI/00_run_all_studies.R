#!/usr/bin/env Rscript
#
# Master Replication Script
# 
# This script runs all three studies in the replication package sequentially:
# 1. World Risk Poll (19_wrp) - Phase 1 (processing)
# 2. World Risk Poll (19_wrp) - Phase 2 (analysis, requires cluster weights)
# 3. Cooperative Election Study (ces)
# 4. Survey January 2024 (survey_jan24)
#
# Usage from terminal:
#   MacOS:   caffeinate -i Rscript 00_run_all_studies.R
#   Windows: Rscript 00_run_all_studies.R
#
# Usage from R:
#   source("00_run_all_studies.R")

# Load shared utilities
source("Code/fun/project_utils.R")

# Set working directory to project root
tryCatch({
  setwd(find_project_root())
}, error = function(e) {
  cat("Warning: Could not find project root. Using current directory:", getwd(), "\n")
})

# Clear and recreate all output directories for clean replication
cat("\n")
cat(paste(rep("=", 80), collapse = ""), "\n")
cat("CLEANING OUTPUT DIRECTORIES FOR FRESH REPLICATION\n")
cat(paste(rep("=", 80), collapse = ""), "\n\n")
clear_output_dirs()
ensure_project_dirs()
cat("✓ Project directories ready\n\n")

# Setup logging
log <- setup_logging("master_replication")
log_system_info("Master Replication Pipeline - All Studies")

# Set flag to indicate we're running from master script
# This prevents individual study scripts from clearing directories
RUNNING_FROM_MASTER <- TRUE

# Configure terra options (suppress progress bars globally)
if (requireNamespace("terra", quietly = TRUE)) {
  terra::terraOptions(progress = 0)
}

# Track overall timing
master_start_time <- Sys.time()

cat(paste(rep("=", 80), collapse = ""), "\n")
cat("MASTER REPLICATION PIPELINE\n")
cat(paste(rep("=", 80), collapse = ""), "\n")
cat("This script will run all three studies in sequence:\n")
cat("  1. World Risk Poll - Phase 1 (Data Processing)\n")
cat("  2. World Risk Poll - Phase 2 (Analysis with CBPS weights)\n")
cat("  3. Cooperative Election Study Panel\n")
cat("  4. Survey January 2024\n")
cat(paste(rep("=", 80), collapse = ""), "\n\n")

# Initialize results tracking
all_results <- list()
study_times <- list()

# =============================================================================
# STUDY 1: WORLD RISK POLL - PHASE 1 (Processing)
# =============================================================================

cat("\n")
cat(paste(rep("=", 80), collapse = ""), "\n")
cat("STUDY 1A: WORLD RISK POLL - DATA PROCESSING (Phase 1)\n")
cat(paste(rep("=", 80), collapse = ""), "\n\n")

phase1_start <- Sys.time()
tryCatch({
  source("Code/19_wrp/00a_run_wrp_scripts.R", echo = FALSE)
  phase1_end <- Sys.time()
  phase1_elapsed <- difftime(phase1_end, phase1_start, units = "mins")
  study_times[["wrp_phase1"]] <- as.numeric(phase1_elapsed)
  all_results[["wrp_phase1"]] <- "SUCCESS"
  cat("\n✓ WRP Phase 1 completed successfully in", 
      round(as.numeric(phase1_elapsed), 1), "minutes\n")
}, error = function(e) {
  phase1_end <- Sys.time()
  phase1_elapsed <- difftime(phase1_end, phase1_start, units = "mins")
  study_times[["wrp_phase1"]] <- as.numeric(phase1_elapsed)
  all_results[["wrp_phase1"]] <- paste("ERROR:", e$message)
  cat("\n✗ ERROR in WRP Phase 1:", e$message, "\n")
  cat("Elapsed time:", round(as.numeric(phase1_elapsed), 1), "minutes\n")
})

# =============================================================================
# CLUSTER STEP NOTICE
# =============================================================================

cat("\n")
cat(paste(rep("=", 80), collapse = ""), "\n")
cat("CLUSTER PROCESSING STEP\n")
cat(paste(rep("=", 80), collapse = ""), "\n\n")

# Check if pre-computed weights exist
weights_file <- "Data/cluster/19_wrp_weights.rds"
if (file.exists(weights_file)) {
  cat("✓ Pre-computed CBPS weights found at:", weights_file, "\n")
  cat("  Continuing with Phase 2...\n\n")
  skip_phase2 <- FALSE
} else {
  cat("WARNING: CBPS weights not found at:", weights_file, "\n\n")
  cat("REQUIRED ACTION:\n")
  cat("  You must estimate CBPS weights on a computing cluster before continuing.\n")
  cat("  See README.md 'Study 1: World Risk Poll' section for instructions.\n\n")
  cat("Steps:\n")
  cat("  1. Transfer Code/19_wrp/01_process/04_estimate_cbps/ to computing cluster\n")
  cat("  2. Run estimate_cbps.R (requires ~240GB RAM, ~1.5 hours)\n")
  cat("  3. Download results to Data/cluster/19_wrp_weights.rds\n")
  cat("  4. Re-run this script to continue\n\n")
  cat("Alternatively:\n")
  cat("  If pre-computed weights were provided, ensure they are at:\n")
  cat("  ", weights_file, "\n\n")
  
  skip_phase2 <- TRUE
  all_results[["wrp_phase2"]] <- "SKIPPED - Missing CBPS weights"
}

# =============================================================================
# STUDY 1: WORLD RISK POLL - PHASE 2 (Analysis)
# =============================================================================

if (!skip_phase2) {
  cat("\n")
  cat(paste(rep("=", 80), collapse = ""), "\n")
  cat("STUDY 1B: WORLD RISK POLL - ANALYSIS (Phase 2)\n")
  cat(paste(rep("=", 80), collapse = ""), "\n\n")
  
  phase2_start <- Sys.time()
  tryCatch({
    source("Code/19_wrp/00b_run_wrp_scripts.R", echo = FALSE)
    phase2_end <- Sys.time()
    phase2_elapsed <- difftime(phase2_end, phase2_start, units = "mins")
    study_times[["wrp_phase2"]] <- as.numeric(phase2_elapsed)
    all_results[["wrp_phase2"]] <- "SUCCESS"
    cat("\n✓ WRP Phase 2 completed successfully in", 
        round(as.numeric(phase2_elapsed), 1), "minutes\n")
  }, error = function(e) {
    phase2_end <- Sys.time()
    phase2_elapsed <- difftime(phase2_end, phase2_start, units = "mins")
    study_times[["wrp_phase2"]] <- as.numeric(phase2_elapsed)
    all_results[["wrp_phase2"]] <- paste("ERROR:", e$message)
    cat("\n✗ ERROR in WRP Phase 2:", e$message, "\n")
    cat("Elapsed time:", round(as.numeric(phase2_elapsed), 1), "minutes\n")
  })
}

# =============================================================================
# STUDY 2: COOPERATIVE ELECTION STUDY
# =============================================================================

cat("\n")
cat(paste(rep("=", 80), collapse = ""), "\n")
cat("STUDY 2: COOPERATIVE ELECTION STUDY PANEL (2010-2014)\n")
cat(paste(rep("=", 80), collapse = ""), "\n\n")

ces_start <- Sys.time()
tryCatch({
  source("Code/ces/00_run_ces_scripts.R", echo = FALSE)
  ces_end <- Sys.time()
  ces_elapsed <- difftime(ces_end, ces_start, units = "mins")
  study_times[["ces"]] <- as.numeric(ces_elapsed)
  all_results[["ces"]] <- "SUCCESS"
  cat("\n✓ CES analysis completed successfully in", 
      round(as.numeric(ces_elapsed), 1), "minutes\n")
}, error = function(e) {
  ces_end <- Sys.time()
  ces_elapsed <- difftime(ces_end, ces_start, units = "mins")
  study_times[["ces"]] <- as.numeric(ces_elapsed)
  all_results[["ces"]] <- paste("ERROR:", e$message)
  cat("\n✗ ERROR in CES analysis:", e$message, "\n")
  cat("Elapsed time:", round(as.numeric(ces_elapsed), 1), "minutes\n")
})

# =============================================================================
# STUDY 3: SURVEY JANUARY 2024
# =============================================================================

cat("\n")
cat(paste(rep("=", 80), collapse = ""), "\n")
cat("STUDY 3: CLIMATE VULNERABILITY PERCEPTIONS SURVEY (January 2024)\n")
cat(paste(rep("=", 80), collapse = ""), "\n\n")

survey_start <- Sys.time()
tryCatch({
  source("Code/survey_jan24/00_run_survey_scripts.R", echo = FALSE)
  survey_end <- Sys.time()
  survey_elapsed <- difftime(survey_end, survey_start, units = "mins")
  study_times[["survey"]] <- as.numeric(survey_elapsed)
  all_results[["survey"]] <- "SUCCESS"
  cat("\n✓ Survey analysis completed successfully in", 
      round(as.numeric(survey_elapsed), 1), "minutes\n")
}, error = function(e) {
  survey_end <- Sys.time()
  survey_elapsed <- difftime(survey_end, survey_start, units = "mins")
  study_times[["survey"]] <- as.numeric(survey_elapsed)
  all_results[["survey"]] <- paste("ERROR:", e$message)
  cat("\n✗ ERROR in Survey analysis:", e$message, "\n")
  cat("Elapsed time:", round(as.numeric(survey_elapsed), 1), "minutes\n")
})

# =============================================================================
# FINAL SUMMARY
# =============================================================================

master_end_time <- Sys.time()
total_elapsed <- difftime(master_end_time, master_start_time, units = "mins")

cat("\n\n")
cat(paste(rep("=", 80), collapse = ""), "\n")
cat("MASTER REPLICATION PIPELINE - FINAL SUMMARY\n")
cat(paste(rep("=", 80), collapse = ""), "\n\n")

cat("Study Results:\n")
cat(paste(rep("-", 80), collapse = ""), "\n")
for (study in names(all_results)) {
  status_symbol <- if (grepl("SUCCESS", all_results[[study]])) "✓" else 
                   if (grepl("SKIPPED", all_results[[study]])) "⊘" else "✗"
  time_str <- if (!is.null(study_times[[study]])) {
    sprintf(" (%.1f min)", study_times[[study]])
  } else {
    ""
  }
  cat(sprintf("%s %-20s: %s%s\n", 
              status_symbol, 
              study, 
              all_results[[study]],
              time_str))
}

cat("\n")
cat("Timing Summary:\n")
cat(paste(rep("-", 80), collapse = ""), "\n")
if (!is.null(study_times[["wrp_phase1"]])) {
  cat(sprintf("  WRP Phase 1 (Processing):  %6.1f minutes\n", study_times[["wrp_phase1"]]))
}
if (!is.null(study_times[["wrp_phase2"]])) {
  cat(sprintf("  WRP Phase 2 (Analysis):    %6.1f minutes\n", study_times[["wrp_phase2"]]))
}
if (!is.null(study_times[["ces"]])) {
  cat(sprintf("  CES Panel:                 %6.1f minutes\n", study_times[["ces"]]))
}
if (!is.null(study_times[["survey"]])) {
  cat(sprintf("  Survey Jan24:              %6.1f minutes\n", study_times[["survey"]]))
}
cat(sprintf("  %-27s %6.1f minutes\n", "TOTAL ELAPSED:", as.numeric(total_elapsed)))

cat("\n")
cat(paste(rep("=", 80), collapse = ""), "\n")

# Count successes and failures
success_count <- sum(grepl("SUCCESS", unlist(all_results)))
error_count <- sum(grepl("ERROR", unlist(all_results)))
skip_count <- sum(grepl("SKIPPED", unlist(all_results)))

if (error_count > 0) {
  cat(sprintf("WARNING: Pipeline completed with %d error(s) and %d skipped.\n", 
              error_count, skip_count))
  cat("         Check log for details.\n")
} else if (skip_count > 0) {
  cat(sprintf("NOTICE: Pipeline completed with %d study/studies skipped.\n", skip_count))
  cat("        See messages above for required actions.\n")
} else {
  cat("SUCCESS: All studies completed successfully!\n")
}

cat("\nCompleted at:", format(master_end_time, "%Y-%m-%d %H:%M:%S"), "\n")
cat("Log file:", log$file, "\n")
cat(paste(rep("=", 80), collapse = ""), "\n\n")

# Clean up logging
cleanup_logging(log$connection)

# Exit with appropriate code
if (error_count > 0) {
  quit(status = 1, save = "no")
} else {
  quit(status = 0, save = "no")
}

