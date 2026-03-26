# cregg::cj does not allow for duplicate factor labels across variables. This function renames them.
replace_duplicate_factor_labels_across_variables <- function(d) {
  return_levels <- sapply(d, levels)
  d_levels <- unlist(return_levels)
  d_dub <- d_levels[duplicated(d_levels)]
  for(i in 1:length(d_dub)) {
    select_var <- names(d_dub)[i]
    select_var <- str_sub(select_var, end = -2)
    levels(d[,c(select_var)])[which(levels(d[,c(select_var)])==d_dub[i])] <- paste0(d_dub[i], i)
  }
  return(list(d, return_levels))
}