# We create the expert level dataset.

expert_dataset <- df_list %>% 
  map(~ .x %>% 
        pivot_longer(cols = -response_id, names_to = c(".value", "party"), 
                     names_pattern = "^([^_]+)_\\w+_(\\w+)",
                     names_transform = list(party = as.factor))) %>%
  list_rbind(names_to = "country")


# We create the number of experts per country, per party, per item.
# we can then use this to create NA's if there are not enough experts. 

# We create a new column for each variable with the following suffix: _n_experts
# We sum the number of experts that answered the question, per party per item. 
# We then use this later to filter based on the number of experts. 

expert_dataset <- expert_dataset %>%        
  group_by(country, party) %>%
  mutate(across(c(manichean:compromise), ~ sum(!is.na(.x)), .names = "{.col}_n_experts")) %>%
  ungroup()  


# We first create a expert level dataset and we export it. 

expert_dataset <- dplyr::left_join(expert_dataset,
                                   df_list_experts %>% 
                                     list_rbind(names_to = "country") %>% 
                                     select(-country),           
                                   by = c("response_id")) %>% 
  dplyr::left_join(read.csv("original_data/party_codes.csv", sep = ",") %>%
                     rename(partyfacts_id = party_facts_id) %>% 
                     mutate(country = stringr::str_to_title(country)),
                   by = c("country", "party")) %>%                                                   
  dplyr::left_join(., readxl::read_excel("original_data/POPPA_List_of_parties_2022.xlsx") %>%
                     select(country_short = ISO3, poppa_id = party_id, party_short = Abbreviation,
                            party_name_original = `Party name National`, party_name_english = `Party name English`),
                   by = "poppa_id")  %>% 
  select(response_id, country, country_short, party_short, party_name_english, party_name_original, poppa_id, everything(), -party)

# We export the expert dataset.

saveRDS(object = expert_dataset, file = "final_data/poppa2_expert.RDS")

# We continue with building the expert dataset. We do not export this. This will be used to create the party level dataset.
# The purpose is to filter out columns that do not have enough expert responses: this is done per party per item. 
# We now use the column per item per party per item to filter based on the number of experts. 
# The vector n sets the number of valid responses necessary to receive a score.
# Our recommended default is 4.

n <- 4

# List of columns to process
columns <- c("manichean", "indivisible", "generalwill", "peoplecentrism", "antielitism", "peopleimm", 
             "peoplewealthy", "elitepol", "elitemedia", "eliteecon", "elitecult", "lrecon", "salienceecon", 
             "immigration", "nativism", "lifestyle", "laworder", "saliencecult", "lroverall", "eu", 
             "climatepolicy", "climatescience", "democracy", "positionchange", "compromise")

# Corresponding _n_experts columns
n_experts_columns <- paste0(columns, "_n_experts")

# Loop over each column pair
for (i in seq_along(columns)) {
  column <- columns[i]
  n_experts_column <- n_experts_columns[i]
  
  expert_dataset <- expert_dataset %>%
    mutate(!!sym(column) := case_when(
      .data[[n_experts_column]] < n ~ NA_real_,
      .default = .data[[column]]
    ))
}


rm(list = setdiff(ls(), c("run_start", "run_start_total", "expert_dataset")))

