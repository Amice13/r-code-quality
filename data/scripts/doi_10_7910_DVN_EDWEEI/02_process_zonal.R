# Alexander F. Gazmararian
# agazmararian@gmail.com

library(here)
library(lubridate)

# Source utility functions
source(here("Code", "19_wrp", "fun", "zonal_processing_utils.R"))

# Define dependencies between files with correct paths
dependencies <- list(
  "fires_modis_stats.R" = list(
    input = "modis_burnarea_zonal.rds",
    source = "../merge_zonal/fires_modis.R"
  ),
  "cpc_daily_stats.R" = list(
    input = "noaa_cpc_tmax_daily_zonal.rds",
    source = "../merge_zonal/temp_cpc_daily.R"
  ),
  "best_daily_stats.R" = list(
    input = "berkeley_tmax_daily_zonal.rds",
    source = "../merge_zonal/temp_best_daily.R"
  )
)

# Function to process scripts SEQUENTIALLY
# Note: Parallel processing was causing silent failures on Ubuntu/Windows because:
# 1. PSOCK workers don't have terra/sf/exactextractr loaded properly
# 2. Large raster objects can't be serialized across workers reliably
# 3. Errors in workers were being swallowed and not reported
# Sequential processing is more reliable for these large raster operations
process_scripts <- function(base_dir, files) {
  cat("\nProcessing", length(files), "files from", base_dir, "\n")
  
  # Sort files to ensure dependencies are processed first
  if (grepl("merge_zonal", base_dir)) {
    # Move dependency sources to the front
    dep_sources <- unique(sapply(dependencies, function(x) x$source))
    files <- c(
      files[files %in% dep_sources],
      files[!files %in% dep_sources]
    )
  }
  
  # Always process sequentially for reliability with large raster operations
  # Each script sources load_zonal_df.R which loads terra rasters - 
  # these can't be reliably shared across PSOCK workers
  start_time <- Sys.time()
  success_count <- 0
  error_count <- 0
  
  for (i in seq_along(files)) {
    file <- files[i]
    script_path <- file.path(base_dir, file)
    
    cat(sprintf("[%d/%d] Running %s...\n", i, length(files), file))
    
    # Run the script with proper error handling
    script_start <- Sys.time()
    result <- tryCatch({
      # Check dependencies first
      if (file %in% names(dependencies)) {
        check_dependencies(file, base_dir, stop_on_missing = TRUE)
      }
      
      # Source the script in the global environment so it has access to everything
      source(script_path, local = FALSE)
      
      script_end <- Sys.time()
      duration <- round(as.numeric(difftime(script_end, script_start, units = "secs")), 1)
      cat(sprintf("  ✓ Completed in %.1f seconds\n", duration))
      success_count <<- success_count + 1
      "success"
    }, error = function(e) {
      script_end <- Sys.time()
      duration <- round(as.numeric(difftime(script_end, script_start, units = "secs")), 1)
      cat(sprintf("  ✗ ERROR in %s (%.1f seconds): %s\n", file, duration, e$message))
      error_count <<- error_count + 1
      e$message
    })
    
    # Force garbage collection between scripts to free memory
    gc(verbose = FALSE)
  }
  
  elapsed <- difftime(Sys.time(), start_time, units = "mins")
  cat(sprintf("\nCompleted %d/%d scripts in %.1f minutes", 
              success_count, length(files), as.numeric(elapsed)))
  if (error_count > 0) {
    cat(sprintf(" (%d errors)", error_count))
  }
  cat("\n")
}

# Create output directories if they don't exist
dir.create(
  file.path("Data/inter/19_wrp/zonal_merged"),
  recursive = TRUE,
  showWarnings = FALSE
)
dir.create(
  file.path("Data/inter/19_wrp/zonal_output"),
  recursive = TRUE,
  showWarnings = FALSE
)

# First process merge_zonal scripts to create required dependencies
cat("\nStarting merge_zonal processing...\n")
base_dir <- "Code/19_wrp/01_process/02_estimate_zonal_stats/merge_zonal"
files <- list.files(path = base_dir, pattern = "\\.R$")
process_scripts(base_dir, files)

# Then process calc_stats scripts
cat("\nStarting calc_stats processing...\n")
base_dir <- "Code/19_wrp/01_process/02_estimate_zonal_stats/calc_stats"
files <- list.files(path = base_dir, pattern = "\\.R$")
process_scripts(base_dir, files)

cat("\nAll processing completed!\n")
