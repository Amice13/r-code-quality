#################################################################################
### load needed packages
#################################################################################

require(sf)
require(tigris)
require(tidyverse)

#################################################################################
### define function
#################################################################################

## download official census tract boundaries by county
get_polygons <- function(
          x, # character vector of census tract GEOIDs
          yr, # vintage for TIGER/LINE shapefiles
          epsg = 2163, # coordinate reference system;
                       # defaults to US National Atlas Equal Area
          as_sp = TRUE, # convert polygons from sf to sp objects for output?
          check_match = TRUE # confirm match btwn x and returned polygon IDs?
) {
     cnty0 <- unique(substr(x, 1, 5))
     states <- unique(substr(cnty0, 1, 2))
     cnty <- map(states, ~ substr(cnty0[substr(cnty0, 1, 2) == .], 3, 5))
     geo <- map2_dfr(
          states, cnty,
          ~ tigris::tracts(.x, .y, year = yr, progress_bar = FALSE)
     ) %>%
          dplyr::select(starts_with("GEOID")) %>%
          set_names(c("ct_id", "geometry")) %>%
          arrange(ct_id) %>%
          st_transform(epsg)
     if (check_match) {
          # confirm exact match between requested and returned GEOIDs
          if (!identical(sort(x), geo$ct_id)) {
               stop("ID mismatch with spatial file.")
          }
     }
     out <- if (as_sp) {as_Spatial(geo)} else {geo}
     return(out)
}