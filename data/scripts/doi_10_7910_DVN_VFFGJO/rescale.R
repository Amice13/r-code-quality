rescale <- function(x, a, b){
  (b - a) * (x - min(x, na.rm=T)) / (max(x, na.rm=T) - min(x, na.rm=T)) + a
}