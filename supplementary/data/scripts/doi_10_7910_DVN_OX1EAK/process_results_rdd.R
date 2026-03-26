
process_results <- function(result) {
  coef_rounded <- round(result$coef, 3)
  ci_rounded <- round(result$ci, 3)
  p_rounded <- round(result$pv, 3)
  
  formatted_results <- lapply(1:nrow(coef_rounded), function(i) {
    coef_value <- coef_rounded[i, 1]
    ci_lower <- ci_rounded[i, 1]
    ci_upper <- ci_rounded[i, 2]
    p_value <- p_rounded[i, 1]
    
    formatted_coef <- sprintf("%.3f", coef_value)
    formatted_ci <- sprintf("[%.3f, %.3f]", ci_lower, ci_upper)
    formatted_p <- sprintf("%.3f", p_value)
    
    c(formatted_coef, formatted_ci, formatted_p)
  })
  
  do.call(cbind, formatted_results)
}