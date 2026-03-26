sem <- function(x) {
    X <- na.omit(x)
    sqrt(var(X)/length(X))
}
