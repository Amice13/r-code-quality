# Is top three color setup for stacked bar graph

is_top_three <- function(df, col) {
  output_matrix <- matrix("grey", nrow=nrow(df), ncol = ncol(df))
  for(i in 1:nrow(df)) {
    output_matrix[i,order(df[i,], decreasing = T)[1:3]] <- col[i]
  }
  return(output_matrix)
}