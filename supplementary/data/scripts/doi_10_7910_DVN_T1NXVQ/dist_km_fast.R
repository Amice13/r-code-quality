# FAST DISTANCE IN KM

# (function) Distance point -- point/line, in km (with projection)
dist_km <- function(points, target, sample.res = .001, ncore = 1, seed = 1){
  require(sp)
  require(geosphere)
  require(rtree)
  
  # Return NA is length of target or points == 0
  if(length(target) == 0){
    return(rep(NA, length(points)))
  }
  if(length(points) == 0){
    return(rep(NA, 0))
  }
  
  # Sample points on lines / polygon boundaries
  set.seed(seed)
  if(class(target) %in% c("SpatialPolygons", "SpatialPolygonsDataFrame")){
    target.poly <- target
    target <- as(target, "SpatialLines")
  }
  if(class(target) %in% c("SpatialLines", "SpatialLinesDataFrame")){
    target <- spsample(target, n = max(1, round(gLength(target) / sample.res)), type = "regular")
  } 
  
  # Get nearest neighbor
  
  # ... to rTree
  target.rt <- RTree(as.matrix(target@coords))
  
  # ... get closest neighbors
  closest.ngb <- unlist(rtree::knn(target.rt, as.matrix(points@coords), k = as.integer(1)))
  
  # ... check
  stopifnot(length(closest.ngb) == length(points))
  
  # Calculate distance
  if(ncore == 1){
    dist.vec <- unlist(lapply(1:length(points), function(x){
      distGeo(target[closest.ngb[x], ],
              points[x,])
    }))
  } else {
    # Load packages
    library(snow)
    library(parallel)
    library(foreach)
    library(doParallel)
    
    # Make cluster
    cl <- makeCluster(getOption("cl.cores", ncore))
    clusterExport(cl, c("target","closest.ngb","points"),
                  envir = environment())
    registerDoParallel(cl)
    
    # Foreach loop
    dist.vec <- foreach(x = unique(closest.ngb), .packages = c("geosphere"),
                        .noexport = c("target","closest.ngb","points"),
                        .options.multicore = list(preschedule = FALSE)) %dopar% {
                          pts.id <- which(closest.ngb == x)
                          data.frame(pts.id = pts.id, distance = geosphere::distm(target[x, ],
                                                                                  points[pts.id,])[1,])
                          # geosphere::distm(target[closest.ngb[x], ],
                          #         points[x,])
                        }
    # dist.vec <- unlist(dist.vec)
    dist.vec <- do.call(rbind, dist.vec)
    dist.vec <- dist.vec$distance[order(dist.vec$pts.id)]
    
    # Close cluster
    stopCluster(cl)
    rm(cl)
  }
  
  # meter --> km
  dist.vec <- dist.vec / 1000
  
  # Return
  return(dist.vec)
}
