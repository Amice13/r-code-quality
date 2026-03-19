#=============================================
# FUNCTION TO DETERMINE IF ONE VARIABLE IS BINARY (EXCLUDING NAs)
#=============================================
binary_check <- function(v) {
  x <- unique(v)
  length(x) - sum(is.na(x)) == 2L
}
