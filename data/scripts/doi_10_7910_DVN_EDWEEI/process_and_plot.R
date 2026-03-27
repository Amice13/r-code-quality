#' Process and Visualize Administrative Boundary Data
#'
#' Validates, aggregates, and visualizes administrative boundaries by region ID,
#' saving both plot and processed data.
#'
#' @param x sf object. Spatial dataframe with administrative boundaries
#' @param name Character. Country/region identifier for file naming
#' @param plot Logical. Whether to create and save plot (default: FALSE)
#'
#' @return sf object. Processed spatial data
#'
#' @details
#' Function checks geometry validity and applies st_make_valid() if needed.
#' Aggregates boundaries by REG_ID and GID_0 using st_union().
#' Optionally saves SVG plot to Output/figures/world_subregions_4.1/
#' Saves RDS data to Data/inter/19_wrp/world_admin_boundaries/
#'
#' @note Creates output files automatically
process_and_plot <- function(x, name, plot = FALSE) {
  require(sf)
  require(dplyr)
  
  # Set output directories
  output_dir_data <- "Data/inter/19_wrp/world_admin_boundaries"
  
  # Process geometries efficiently
  if (any(!st_is_valid(x))) {
    x <- x %>%
      st_make_valid() %>%
      st_simplify(preserveTopology = TRUE)
  }
  
  # Get geometry column name
  geom_col <- attr(x, "sf_column")
  if (is.null(geom_col)) geom_col <- "geom"
  
  # Aggregate using dplyr operations
  x_agg <- x %>%
    group_by(REG_ID, GID_0) %>%
    summarise(!!geom_col := st_union(!!sym(geom_col)), .groups = "drop") %>%
    ungroup()
  
  # Optional plotting
  if (plot) {
    require(ggplot2)
    output_dir_fig <- "Output/figures/world_subregions_4.1"
    dir.create(output_dir_fig, showWarnings = FALSE, recursive = TRUE)
    
    p <- ggplot(x_agg) +
      geom_sf(aes(fill = factor(REG_ID)), color = "black") +
      theme_minimal()
    
    ggsave(
      p,
      filename = file.path(output_dir_fig, paste0(name, ".pdf")),
      width = 6,
      height = 3,
      scale = 1.5
    )
  }
  
  # Save processed data
  dir.create(output_dir_data, showWarnings = FALSE, recursive = TRUE)
  saveRDS(x_agg, file = file.path(output_dir_data, paste0(name, ".rds")))
  
  # Return processed data
  return(x_agg)
}
