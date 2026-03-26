# Utility functions for zonal processing

# Detect if running in RStudio
is_rstudio <- function() {
  Sys.getenv("RSTUDIO") == "1" || !is.null(getOption("rstudio.notebook.executing"))
}

# Map of script names to their actual output files
# This mapping reflects where each script ACTUALLY saves its output
script_output_map <- list(
  # merge_zonal scripts
  "temp_best_daily.R" = "Data/inter/19_wrp/zonal_merged/berkeley_tmax_daily_zonal.rds",
  "temp_cpc_daily.R" = "Data/inter/19_wrp/zonal_merged/noaa_cpc_tmax_daily_zonal.rds",
  "fires_modis.R" = "Data/inter/19_wrp/zonal_merged/modis_burnarea_zonal.rds",
  "gdp.R" = "Data/inter/19_wrp/zonal_output/subregion_gdp.rds",
  "pop.R" = "Data/inter/19_wrp/zonal_output/subregion_pop.rds",
  "crops.R" = "Data/inter/19_wrp/zonal_output/subregion_crop.rds",
  "emissions.R" = "Data/inter/19_wrp/zonal_output/subregion_emissions.rds",
  "urban.R" = "Data/inter/19_wrp/zonal_output/subregion_urban.rds",
  "dpi.R" = "Data/inter/19_wrp/zonal_output/subregion_ff.rds",
  "cr_gw_projections.R" = "Data/inter/19_wrp/zonal_output/subregion_damage.rds",
  "pop_centroids.R" = "Data/inter/19_wrp/zonal_merged/subregion_sf_centroids.rds",
  # calc_stats scripts
  "best_daily_stats.R" = "Data/inter/19_wrp/zonal_output/best_daily_stats.rds",
  "cpc_daily_stats.R" = "Data/inter/19_wrp/zonal_output/cpc_daily_stats.rds",
  "fires_modis_stats.R" = "Data/inter/19_wrp/zonal_output/fires_modis_stats.rds"
)

# Function to get output file path for a script
get_output_path <- function(script_path) {
  script_name <- basename(script_path)
  
  # Use the explicit mapping if available
  if (script_name %in% names(script_output_map)) {
    return(script_output_map[[script_name]])
  }
  
  # Fallback for unknown scripts (shouldn't happen)
  return(NULL)
}

# Function to check dependencies for a script
check_dependencies <- function(script_name, base_dir = NULL, stop_on_missing = TRUE) {
  if (!exists("dependencies") || !script_name %in% names(dependencies)) return(TRUE)
  
  dep <- dependencies[[script_name]]
  input_file <- file.path("Data/inter/19_wrp/zonal_merged", dep$input)
  
  # Check if input file exists and is not empty
  if (!file.exists(input_file) || file.size(input_file) == 0) {
    error_msg <- sprintf("ERROR: Missing dependency %s for script %s\n", dep$input, script_name)
    cat(error_msg)
    if (stop_on_missing) {
      stop(error_msg)
    }
    return(FALSE)
  }
  
  # If base_dir is provided, check if source file exists
  if (!is.null(base_dir)) {
    # Handle relative paths (starting with ../)
    if (grepl("^\\.\\.?/", dep$source)) {
      # Resolve the relative path from base_dir
      source_path <- normalizePath(file.path(base_dir, dep$source), mustWork = FALSE)
    } else {
      source_path <- file.path(base_dir, dep$source)
    }
    
    if (!file.exists(source_path)) {
      error_msg <- sprintf("ERROR: Source file %s not found at %s for script %s\n", dep$source, source_path, script_name)
      cat(error_msg)
      if (stop_on_missing) {
        stop(error_msg)
      }
      return(FALSE)
    }
  }
  
  return(TRUE)
}

# Function to check if processing is needed
needs_processing <- function(script_path) {
  output_path <- get_output_path(script_path)
  if (is.null(output_path)) return(TRUE)
  
  if (!file.exists(output_path)) return(TRUE)
  
  # Check if script is newer than output
  script_mtime <- file.mtime(script_path)
  output_mtime <- file.mtime(output_path)
  
  # If this is a stats script, also check if dependency is newer than output
  script_name <- basename(script_path)
  if (exists("dependencies") && script_name %in% names(dependencies)) {
    dep <- dependencies[[script_name]]
    input_file <- file.path("Data/inter/19_wrp/zonal_merged", dep$input)
    if (file.exists(input_file)) {
      input_mtime <- file.mtime(input_file)
      if (input_mtime > output_mtime) return(TRUE)
    }
  }
  
  return(script_mtime > output_mtime)
}

# Function to run script safely with timing
run_script_safe <- function(script_path) {
  script_name <- basename(script_path)
  base_dir <- dirname(script_path)
  
  # Check dependencies first - this will stop execution if dependencies are missing
  check_dependencies(script_name, base_dir, stop_on_missing = TRUE)
  
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