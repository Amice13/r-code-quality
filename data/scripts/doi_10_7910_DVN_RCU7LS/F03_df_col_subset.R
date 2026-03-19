# select all columns between the first and second one specified

df_col_subset <- function(data, first_col, second_col) {
  if(!first_col %in% colnames(data) | !second_col %in% colnames(data)) {stop("Columns not in data frame!")}
  return(data[,which(colnames(data)==first_col) : which(colnames(data)==second_col)])
}
