# function to compute distances between objects and kilns - only for points
closestKiln <- function(object){
    distances <- st_distance(object[[1]], kilns)
    dist2closestKiln <- apply(distances, MARGIN = 1, min)
    dist2closestKiln <- set_units(dist2closestKiln, "m")
    dist2closestKiln <- set_units(dist2closestKiln, "km")
    closest_kiln <- as.data.frame(dist2closestKiln)
    closest_kiln$type <- object[[2]]
    
    return(closest_kiln)
}
