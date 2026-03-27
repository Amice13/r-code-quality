# NOT in ------------------------------------------------------------------
'%!in%' <- function(x,y)!('%in%'(x,y))


# Rescore 0-1 -------------------------------------------------------------
scale01 <- function(var, print_min = FALSE){
  if(print_min == T){
    print(min(var, na.rm = T))
  }
  v <- var-min(var, na.rm =T)
  v <- v/max(v, na.rm =T)
}

