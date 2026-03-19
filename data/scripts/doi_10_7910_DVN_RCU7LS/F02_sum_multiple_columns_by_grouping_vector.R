# Sum multiple columns by a grouping vector

sum_multiple_cols_by_group <- function(df_with_variables, grouping_vector) {
  my_groups <- sort(unique(grouping_vector))
  output_df <- data.frame(matrix(ncol=ncol(df_with_variables), nrow=length(my_groups)), row.names = my_groups)
  colnames(output_df) <- colnames(df_with_variables)
  for (i in 1:ncol(df_with_variables)) {
    output_df[,i] <- aggregate(df_with_variables[,i]~grouping_vector, FUN=sum)[,2]
  }
  return(output_df)
}