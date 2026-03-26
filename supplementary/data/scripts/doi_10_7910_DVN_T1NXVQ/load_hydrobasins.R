####################
# LOAD HYDROBASINS Function
#
# Carl MC, 8.8.2019
####################

get_hydroshed_continents <- function(hydro.path ="data/geography/hydrosheds"){
  list.files(file.path(hydro.path, "standard"))
}




load_hydroshed_polys <- function(type = "standard", continent = get_hydroshed_continents(), 
                                 level, hydro.path ="data/geography/hydrosheds"){
  # Reuqired packages
  require(rgdal)
  
  # Check
  stopifnot(dir.exists(hydro.path))
  
  # Load
  if(dir.exists(file.path(hydro.path, paste0("hydrosheds_standard_lev", level)))){
    hybas.shp <- readOGR(file.path(hydro.path),
                         paste0("hydrosheds_standard_lev", level))
  } else {
    hybas.shp <- readOGR(file.path(hydro.path, "standard", continent, paste0("hybas_", continent, "_lev", level,"_v1c")),
                         paste0("hybas_", continent, "_lev", level,"_v1c"))
  }
  
  
  # Return
  return(hybas.shp)
}


load_hydroshed_lines <- function(type = "standard", continent = get_hydroshed_continents(), 
                                 level, hydro.path ="data/geography/hydrosheds", clip.coast = T){
  # Reuqired packages
  require(rgdal)
  
  # Check
  stopifnot(dir.exists(hydro.path))
  
  # Load hydrosheds
  hybas.shp <- load_hydroshed_polys(type , continent, level, hydro.path)
  
  # To lines
  hybas.shp <- as(hybas.shp, "SpatialLinesDataFrame")
  
  # Load Coasts
  if(clip.coast){
    coast.shp <- do.call(rbind, lapply(continent, function(c){
      readOGR(file.path(hydro.path, type, c, paste0("hybas_", c, "_lev", "01", "_v1c")),
              paste0("hybas_", c, "_lev", "01", "_v1c"))
    }))
    if(length(coast.shp) > 1){
      coast.shp <- gUnaryUnion(coast.shp, id = rep(1, length(coast.shp)))
    }
    coast.shp <- as(coast.shp, "SpatialLines")
    hybas.shp <- raster::crop(hybas.shp, gBuffer(coast.shp, .1, byid = T))
  }
  
  # Return
  return(hybas.shp)
}



