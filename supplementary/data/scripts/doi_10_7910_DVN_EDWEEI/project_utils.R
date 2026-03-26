#' Project Utility Functions
#' 
#' Shared utility functions for consistent project setup and file management

#' Find project root by looking for .here file or cleavages.Rproj file
#' 
#' @return Character path to project root directory
find_project_root <- function() {
  current_dir <- getwd()
  # First look for .here file (preferred for individual replication directory)
  while (!file.exists(file.path(current_dir, ".here"))) {
    parent_dir <- dirname(current_dir)
    if (parent_dir == current_dir) {
      # If .here not found, fall back to looking for cleavages.Rproj
      current_dir <- getwd()
      while (!file.exists(file.path(current_dir, "cleavages.Rproj"))) {
        parent_dir <- dirname(current_dir)
        if (parent_dir == current_dir) {
          stop("Could not find .here or cleavages.Rproj file. Please run from within the project directory.")
        }
        current_dir <- parent_dir
      }
      break
    }
    current_dir <- parent_dir
  }
  return(current_dir)
}

#' Ensure directory exists, create if necessary
#' 
#' @param dir_path Character path to directory
#' @param recursive Logical, create parent directories if needed
ensure_dir <- function(dir_path, recursive = TRUE) {
  if (!dir.exists(dir_path)) {
    dir.create(dir_path, recursive = recursive)
  }
}

#' Clear output directories for replication
#' 
#' Removes all contents from Data/inter and Data/output directories
#' to ensure a clean replication run
clear_output_dirs <- function() {
  cat("Clearing Data/inter and Data/output directories...\n")
  
  # Clear Data/inter
  if (dir.exists("Data/inter")) {
    unlink("Data/inter", recursive = TRUE)
    cat("  ✓ Cleared Data/inter\n")
  }
  
  # Clear Data/output
  if (dir.exists("Data/output")) {
    unlink("Data/output", recursive = TRUE)
    cat("  ✓ Cleared Data/output\n")
  }
  
  cat("✓ Output directories cleared\n\n")
}

#' Ensure all required project directories exist
#' 
#' Creates the standard Data/inter and Data/output directory structure if missing
ensure_project_dirs <- function() {
  # Data directories
  ensure_dir("Data/inter")
  ensure_dir("Data/inter/country")
  ensure_dir("Data/inter/grid")
  ensure_dir("Data/output")
  
  # WRP specific directories
  ensure_dir("Data/inter/19_wrp")
  ensure_dir("Data/inter/19_wrp/world_admin_boundaries")
  ensure_dir("Data/inter/19_wrp/zonal_merged")
  ensure_dir("Data/inter/19_wrp/zonal_output")
  ensure_dir("Data/output/19_wrp")
  
  # CES specific directories
  ensure_dir("Data/inter/ces")
  ensure_dir("Data/inter/usa")
  ensure_dir("Data/output/ces")
  
  # Survey specific directories
  ensure_dir("Data/inter/survey")
  ensure_dir("Data/inter/survey/lucid")
  
  # Output directories
  ensure_dir("Output")
  ensure_dir("Output/figures")
  ensure_dir("Output/log")
  ensure_dir("Output/tables")
}

#' Set up logging for script runners
#' 
#' @param log_name Character name for log file (e.g., "wrp", "ces")
#' @return List with log connection and file path
setup_logging <- function(log_name) {
  logdir <- file.path(getwd(), "Output", "log")
  ensure_dir(logdir)
  
  logfile <- file.path(
    logdir,
    paste0(format(Sys.time(), "%Y-%m-%d_%H-%M-%S"), "_", log_name, "_log.txt")
  )
  log_con <- file(logfile, open = "wt")
  
  sink(log_con, split = TRUE)
  sink(log_con, type = "message")
  
  return(list(connection = log_con, file = logfile))
}

#' Log system information
log_system_info <- function(pipeline_name) {
  cat("### Starting", pipeline_name, "###\n")
  cat("Working directory:", getwd(), "\n")
  cat("R version:", R.version.string, "\n")
  cat("Platform:", R.version$platform, "\n")
  cat("OS type:", .Platform$OS.type, "\n")
  cat("Path separator:", .Platform$path.sep, "\n")
  print(Sys.Date())
  print(sessionInfo())
  cat("\n")
}

#' Execute scripts with validation, timing, and error handling
#' 
#' @param script_paths Character vector of script file paths
#' @return List with execution results
execute_scripts <- function(script_paths) {
  # Validate all scripts exist before starting
  cat("Validating script paths...\n")
  for (script in script_paths) {
    if (!file.exists(script)) {
      stop("Required script not found: ", script)
    }
  }
  cat("✓ All scripts found\n\n")
  
  # Initialize counters
  total_scripts <- length(script_paths)
  success_count <- 0
  error_count <- 0
  start_time <- Sys.time()
  
  cat(sprintf("Executing %d scripts...\n\n", total_scripts))
  
  for (i in seq_along(script_paths)) {
    script <- script_paths[i]
    cat(sprintf("[%d/%d] Running %s …\n", i, total_scripts, script))
    
    script_start <- Sys.time()
    result <- tryCatch(
      { 
        source(script, echo = FALSE)
        script_end <- Sys.time()
        elapsed <- round(as.numeric(difftime(script_end, script_start, units = "secs")), 1)
        cat(sprintf("✓ Successfully completed %s (%.1f seconds)\n\n", script, elapsed))
        "success"
      },
      error = function(e) {
        script_end <- Sys.time()
        elapsed <- round(as.numeric(difftime(script_end, script_start, units = "secs")), 1)
        cat("✗ ERROR in", script, "(", elapsed, "seconds):", e$message, "\n\n")
        "error"
      }
    )
    
    # Update counts based on result (fixes scoping issue with tryCatch)
    if (result == "success") {
      success_count <- success_count + 1
    } else {
      error_count <- error_count + 1
    }
    gc()
  }
  
  # Summary
  end_time <- Sys.time()
  total_elapsed <- round(as.numeric(difftime(end_time, start_time, units = "mins")), 1)
  
  cat(paste(rep("=", 60), collapse = ""), "\n")
  cat("PIPELINE SUMMARY\n")
  cat(paste(rep("=", 60), collapse = ""), "\n")
  cat("Total scripts:", total_scripts, "\n")
  cat("✓ Successful:", success_count, "\n")
  cat("✗ Failed:", error_count, "\n")
  cat("Total runtime:", total_elapsed, "minutes\n")
  cat("Completed at:", format(end_time, "%Y-%m-%d %H:%M:%S"), "\n")
  
  if (error_count > 0) {
    cat("\n⚠️  Pipeline completed with", error_count, "error(s). Check log for details.\n")
  } else {
    cat("\nAll scripts completed successfully!\n")
  }
  
  return(list(
    success = success_count,
    errors = error_count,
    total_time = total_elapsed
  ))
}

#' Clean up logging
#' 
#' @param log_connection Connection object from setup_logging
cleanup_logging <- function(log_connection) {
  sink(type = "message")
  sink()
  
  # Check if connection is valid before trying to close it
  tryCatch({
    if (isOpen(log_connection)) {
      close(log_connection)
    }
  }, error = function(e) {
    # Connection may already be closed or invalid - just warn
    cat("Note: Log connection cleanup issue (likely already closed):", e$message, "\n")
  })
  
  if (file.exists("Rplots.pdf")) file.remove("Rplots.pdf")
} 

# Function to run script safely with timing
run_script_safe <- function(script_path) {
  script_name <- basename(script_path)
  base_dir <- dirname(script_path)
  
  # Check dependencies first
  if (!check_dependencies(script_name, base_dir)) {
    return(paste("Skipped (waiting for dependencies):", script_path))
  }
  
  if (!needs_processing(script_path)) {
    return(paste("Skipped (up to date):", script_path))
  }
  
  start_time <- Sys.time()
  result <- tryCatch({
    setwd(here::here())
    source(script_path, local = TRUE)
    end_time <- Sys.time()
    duration <- round(as.numeric(difftime(end_time, start_time, units = "secs")), 1)
    paste("Success:", script_path, sprintf("(%.1f seconds)", duration))
  }, error = function(e) {
    end_time <- Sys.time()
    duration <- round(as.numeric(difftime(end_time, start_time, units = "secs")), 1)
    paste("Error in", script_path, ":", e$message, sprintf("(%.1f seconds)", duration))
  })
  return(result)
}

# Add check_dependencies function if not already present
check_dependencies <- function(script_name, base_dir) {
  # If no dependencies defined, return TRUE
  if (!exists("dependencies")) return(TRUE)
  
  # Get dependencies for this script
  script_deps <- dependencies[sapply(dependencies, function(x) x$target == script_name)]
  
  if (length(script_deps) == 0) return(TRUE)
  
  # Check each dependency
  all(sapply(script_deps, function(dep) {
    dep_path <- file.path(base_dir, dep$source)
    if (!file.exists(dep_path)) return(FALSE)
    TRUE
  }))
} 