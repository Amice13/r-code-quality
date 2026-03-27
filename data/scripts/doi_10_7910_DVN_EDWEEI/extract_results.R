#' Extract and Combine Results from Nested Model Lists
#'
#' Processes nested list structure from est_models() to extract
#' average marginal effects and hypothesis tests.
#'
#' @param nested_list List. Nested list containing model results from est_models()
#'
#' @return List with two elements:
#'   \item{first}{Data.frame of combined average marginal effects across methods}
#'   \item{second}{Data.frame of combined hypothesis tests across methods}
#'
#' @details
#' Function assumes nested_list has structure: list[[shock]][[method]][[1 or 2]]
#' where [1] contains AME results and [2] contains hypothesis tests.
extract_results <- function(nested_list) {
  first_dfs <- list()
  second_dfs <- list()
  
  for (i in seq_along(nested_list)) {
    sublist <- nested_list[[i]]
    
    for (method in names(sublist)) {
      first_dfs <- c(first_dfs, list(data.frame(sublist[[method]][[1]], model = method)))
      
      l2 <- data.frame(sublist[[method]][[2]], model = method)
      l2$term <- sublist[[method]][[1]]$term[[1]]
      
      second_dfs <- c(second_dfs, list(l2))
    }
  }
  
  first_combined <- do.call(rbind, first_dfs)
  second_combined <- do.call(rbind, second_dfs)
  
  list(first = first_combined, second = second_combined)
}