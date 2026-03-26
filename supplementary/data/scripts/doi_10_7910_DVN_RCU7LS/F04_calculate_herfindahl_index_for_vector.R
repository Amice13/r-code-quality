# Calculate Herfindahl Index for vector
calc_herfindahl_index <- function(vector_with_freqencies) {
  return(sum((vector_with_freqencies/sum(vector_with_freqencies))^2))
}