# We now do some further data cleaning. 
# We share from this document publicly since the data has been anonymized. 

# We import that anonymized df_list_v2 file.

df_list_v2 <- readRDS("original_data_v2/df_list_v2.rds")

# Set all "dont know" responses to NA.

df_list_v2 <-  map(df_list_v2, ~.x %>% 
                  replace_with_na_all(condition = ~.x %in% c(-99, -88)) ) 

# We relocate the data so response_id is the first column. 

df_list_v2 <- map(df_list_v2, ~.x %>% 
                 relocate(response_id, .before = start_date))

# We remove non-relevant data, such as time stamps and respondent information.

# We split the data into two lists. One we will use for the experts later (df_list_experts_v2) and the other we use for the party level dataset (df_list_v2).
# We first make a list with just the response_id, gender, self_leftright, and party_pref from the df_list_experts_v2. 

df_list_experts_v2 <- map(df_list_v2, ~.x %>%
                         select(response_id,
                                starts_with("gender"), 
                                starts_with("self_leftright"), 
                                starts_with("party_pref")))

# We then remove these columns from the df_list_v2 since we do not need them for the party dataset.

df_list_v2 <- map(df_list_v2, ~.x %>%
                 select(-start_date,          
                         -end_date,                
                         -status,                 
                         -ip_address,              
                         -progress,                
                         -duration_in_seconds,     
                         -finished,              
                         -recorded_date,           
                         -external_reference,      
                         -location_latitude,       
                         -location_longitude,     
                         -distribution_channel,  
                         -user_language,          
                         -informed_consent))   

               
df_list_v2 <- map(df_list_v2, ~.x %>%
                 select(-starts_with("gender"), 
                        -starts_with("self_leftright"), 
                        -starts_with("party_pref")))


# The first function removes the underscore (from general_will to climate_policy columns).
# The second function converts the gender, self_leftright and party_pref columns.
# These changes are needed to fix some typos from the column names from the original Qualtrics survey. 

changes_underscores <- function(x) {
  x <-  sub("general_will", "generalwill", x)           
  x <-  sub("people_wealthy", "peoplewealthy", x)       
  x <-  sub("elite_pol", "elitepol", x)
  x <-  sub("elite_media", "elitemedia", x)
  x <-  sub("elite_econ", "eliteecon", x)
  x <-  sub("elite_cult", "elitecult", x)
  x <-  sub("salience_econ", "salienceecon", x)
  x <-  sub("salience_cult", "saliencecult", x)
  x <-  sub("climate_policy", "climatepolicy", x)
  return(x)
  
}


convert_columns <- function(x) {
  x <- sub("^gender_.*", "gender", x)                  # Converts columns starting with "gender_" to "gender"
  x <- sub("^self_leftright_.*", "selfleftright", x)   # Converts columns starting with "self_leftright_" to "selfleftright"
  x <- sub("^party_pref_.*", "partypref", x)           # Converts columns starting with "party_pref_" to "partypref"
  
  return(x)
}

# We use the function changes_underscores to fix the columns for df_list_v2.
# We use the function convert_columns to fix the columns for df_list_experts_v2

df_list_v2 <-  map(df_list_v2, ~.x %>% rename_with(changes_underscores))
df_list_experts_v2 <- map(df_list_experts_v2, ~.x %>% rename_with(convert_columns))

