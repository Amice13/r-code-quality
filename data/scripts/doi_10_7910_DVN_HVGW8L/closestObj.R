closestObj<- function(object, kilnList = kilns){
    distances <- st_distance(kilnList, object[[1]])
    dist2closestKiln <- apply(distances, MARGIN = 1, min)
    dist2closestKiln <- set_units(dist2closestKiln, "m")
    dist2closestKiln <- set_units(dist2closestKiln, "km")
    closest_kiln <- as.data.frame(dist2closestKiln)
    closest_kiln$type <- object[[2]]
    
    return(closest_kiln)
}
