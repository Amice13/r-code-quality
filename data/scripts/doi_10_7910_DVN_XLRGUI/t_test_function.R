
t_test_function <- function(df, var) {
  control_values <- control_group %>% mutate(no_response = calculate_no_response(., var)) %>% pull(no_response)
  treatment_values <- treatment_group %>% mutate(no_response = calculate_no_response(., var)) %>% pull(no_response)
  t_test_result <- t.test(control_values, treatment_values)
  return(c(mean(control_values, na.rm = TRUE), 
           mean(treatment_values, na.rm = TRUE), 
           t_test_result$statistic, 
           t_test_result$p.value))
}