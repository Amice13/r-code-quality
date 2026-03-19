library(dplyr)

# load data 
platforms <- readRDS("policy_platforms.rds")

# aggregate data, preserving auxiliary variables
platforms_agg <- platforms %>%
  arrange(candidate_webname, state_postal, cd, cand_party, year, statement_id) %>%
  group_by(candidate_webname, state_postal, cd, cand_party, year) %>%
  summarise(
    platform_text = paste(paste(issue_header, issue_text, sep = ": "), collapse = " "),
    across(-c(issue_header, issue_text, policy_code, statement_id), first)  
  ) %>%
  ungroup()

saveRDS(platforms_agg, "policy_platforms_aggregated.rds")
