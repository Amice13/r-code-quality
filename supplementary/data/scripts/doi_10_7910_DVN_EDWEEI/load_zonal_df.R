# Load required spatial data and setup variables

# Suppress progress bars (especially for Windows logs)
if (requireNamespace("terra", quietly = TRUE)) {
  terra::terraOptions(progress = 0)
}

# Increase exactextractr memory limit for faster zonal stats
# Default is 3e7; dynamically set based on available system memory
# Note: Replication requires minimum 53GB RAM per README specifications
get_safe_max_cells <- function() {
  bytes_per_cell <- 16  # 8 bytes raster + 8 bytes weights
  default_cells <- 1e9  # fallback: 1 billion cells (~16 GB)
  
  # Try to detect available memory (platform-specific)
  available_bytes <- tryCatch({
    if (.Platform$OS.type == "unix") {
      if (Sys.info()["sysname"] == "Darwin") {
        # macOS: get free + inactive memory
        vm <- system("vm_stat", intern = TRUE)
        page_size <- 16384  # Apple Silicon default, safe fallback
        free_pages <- as.numeric(gsub("\\D", "", vm[grep("Pages free", vm)]))
        inactive_pages <- as.numeric(gsub("\\D", "", vm[grep("Pages inactive", vm)]))
        (free_pages + inactive_pages) * page_size
      } else {
        # Linux: read available memory from /proc/meminfo
        meminfo <- readLines("/proc/meminfo", n = 10)
        mem_available <- meminfo[grep("MemAvailable", meminfo)]
        as.numeric(gsub("\\D", "", mem_available)) * 1024  # KB to bytes
      }
    } else {
      # Windows: use wmic
      mem_output <- system("wmic OS get FreePhysicalMemory /value", intern = TRUE)
      mem_line <- mem_output[grep("FreePhysicalMemory", mem_output)]
      as.numeric(gsub("\\D", "", mem_line)) * 1024  # KB to bytes
    }
  }, error = function(e) NULL)
  
  if (is.null(available_bytes) || is.na(available_bytes) || available_bytes <= 0) {
    return(default_cells)
  }
  
  # Use 50% of available memory (no cap - replication specs require 53+ GB RAM)
  target_bytes <- available_bytes * 0.5
  target_cells <- floor(target_bytes / bytes_per_cell)
  max(target_cells, default_cells)
}

options(exactextractr.max_cells_in_memory = get_safe_max_cells())

load(file = here("Data", "inter", "19_wrp", "subregion_sf.rda"))
load(file = here("Data", "inter", "19_wrp", "subregion_neighbors.rda"))
# Use file.path() to ensure proper path separator on all platforms
# Note: here() strips trailing slashes, so we store just the directory
save_path <- here("Data", "inter", "19_wrp", "zonal_merged")
focal_window <- 9

# Ensure sf_data is available from subregion_sf.rda
if (!exists("sf_data")) {
  stop("sf_data not found in subregion_sf.rda")
}

# Load population data if needed for calculations
if (!exists("calc")) {
  pop <- rast(here("Data", "inter", "grid", "pop_proj.tif"))
}
