# FUNCTION TO COUNT NUMBER OF BINARY VARIABLES IN DATA (EXCLUDING NAs)
binary_count <- function(df) {
  map_lgl(df, binary_check)  %>% sum()
}
