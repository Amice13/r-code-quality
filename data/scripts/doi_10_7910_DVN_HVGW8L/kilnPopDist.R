kilnPopDist <- function(point, kilns){
    # calculates distance from each raster centroid to each kiln
    # sums the number of kilns in different distances
    # finds closest kiln
    # combines everything into a df with n = size of raster data
    
    dist <- sf::st_distance(point, kilns)
    closestKiln <-  apply(dist, 1, min)
    kilnWithn1km <- apply(dist, 1, function(x) x <= 1000)
    kilnCount1km <- sum(kilnWithn1km == T)
    
    kilnWithn2km <- apply(dist, 1, function(x) x <= 2000)
    kilnCount2km <- sum(kilnWithn2km == T)
    
    kilnWithn5km <- apply(dist, 1, function(x) x <= 5000)
    kilnCount5km <- sum(kilnWithn5km == T)
    
    kilnWithn7km <- apply(dist, 1, function(x) x <= 7000)
    kilnCount7km <- sum(kilnWithn7km == T)
    
    kilnWithn10km <- apply(dist, 1, function(x) x <= 10000)
    kilnCount10km <- sum(kilnWithn10km == T)
    
    
    results <- data.frame("id" = point$id,
                          "pop" = point$pop,
                          "closestKiln" = closestKiln,
                          "kilnCount1km" =  kilnCount1km,
                          "kilnCount2km" =  kilnCount2km,
                          "kilnCount5km" =  kilnCount5km,
                          "kilnCount7km" =  kilnCount7km,
                          "kilnCount10km" =  kilnCount10km)
    return(results)
}
