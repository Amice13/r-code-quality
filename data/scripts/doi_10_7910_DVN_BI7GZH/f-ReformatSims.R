#######################################################################################################
# Reformat simulations in long format
#######################################################################################################

ReformatSims <- function(sims_df, agecats_nm) {
  # Args: 
  # sims_df: simulations in wide format (data frame)
  # agecats_nm: names of age categories
  # Returns: data frame of simulations, in long format
  
  # Add variables for total no of infections
  for(i in 1:length(agecats_nm)) {
    sims_df[[paste0("CItot_", i)]] <- sims_df[[paste0("CI1_", i)]] +  sims_df[[paste0("CI2_", i)]] +  sims_df[[paste0("CI3_", i)]] +  sims_df[[paste0("CI4_", i)]] +  sims_df[[paste0("CI5_", i)]]
  }
  
  # Pivot in longer format
  out <- sims_df %>% 
    pivot_longer(cols = -c(".id", "time"), names_to = "var", values_to = "n") %>% 
    separate(col = "var", into = c("var_nm", "age_no"), "_", remove = T) %>% 
    mutate(var_type =  ifelse(var_nm %in% c("CI1", "CI2", "CI3", "CI4", "CI5", "CIobs", "CItot"), "obs", "state"), 
           age_no = as.integer(age_no),
           age_yr = ifelse(age_no == 1, 0, ifelse(age_no == 2, 0.17, ifelse(age_no == 3, 1.5, age_no - 2))),
           age_no = factor(age_no, levels = 1:length(agecats_nm)),
           age_nm = factor(age_no, levels = 1:length(agecats_nm), labels = agecats_nm), 
           age_nm2 = cut(x = age_yr, breaks = c(0, 0.17, 1.5, 5, 10, 20, 40, 60, 75), include.lowest = T, right = F)) %>% 
    group_by(.id, time, age_no, age_yr, age_nm, age_nm2) %>% 
    mutate(N_age = sum(n[var_type == "state"])) %>% 
    ungroup()
  out
}