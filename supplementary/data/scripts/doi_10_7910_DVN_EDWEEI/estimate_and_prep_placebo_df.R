estimate_and_prep_placebo_df <- function(placebo_list) {
  ame <- lapply(shocks, est_placebo, outcome = placebo_list)
  ame_df <- do.call(rbind, ame)
  ame_df$placebo <- placebo_list
  
  return(ame_df)
}
