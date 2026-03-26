# We now create the integrated dataset: with POPPA1 and POPPA2

if(!file.exists("./final_data/poppa1.rds")){
  
  poppa1 <- dplyr::left_join(get_dataframe_by_name(
                             filename = "party_means.tab",
                             dataset = "10.7910/DVN/8NEL7B", 
                             server = "dataverse.harvard.edu") %>% 
                             rename(poppa_id = party_id),
                           readxl::read_excel("original_data/POPPA_List_of_parties_2018.xlsx", sheet = 2) %>% 
                             select(country_short = ISO3, country_id = Country_id, poppa_id = party_id, party_short = Abbreviation,
                                    party_name_original = `Party name National`, party_name_english = `Party name English`),
                           by = c("country_id", "poppa_id")) %>% 
  select(-party, -country_id, -cee, -populism) %>% 
  mutate(country = countrycode::countrycode(country_short, origin = "iso3c", destination = "country.name"),
         wave = "Wave 1 - 2018")

saveRDS(object = poppa1, file = "final_data/poppa1.rds")
} else{
  poppa1 <- readRDS(file = "final_data/poppa1.rds")
}

poppa1 <- poppa1 %>% 
  rename(indivisible = indivisble,
         n_experts = nr_experts,
         mean_n_experts = mean_expert_response,
         manifesto_id = cmp_id) %>% 
  mutate(country = forcats::fct_recode(country,
                                       Czech = "Czechia")) %>%  
  select(-family, -manifesto_id, -ches_id, -parlgov_id)
  
poppa2 <- readRDS("final_data/poppa2.RDS") %>% 
  select(-populism)

combined <- plyr::rbind.fill(poppa2, poppa1)

combined <- dplyr::left_join(combined %>% 
                                dplyr::select(-party_short, -party_name_english, -party_name_original), 
                              combined %>% 
                                dplyr::select(poppa_id, country_short, party_short, party_name_english, party_name_original, wave) %>% 
                                group_by(poppa_id, country_short) %>% 
                                dplyr::top_n(1, wave) %>% 
                                dplyr::select(-wave),
                              by = c("poppa_id", "country_short")) %>% 
  dplyr::select(country, country_short, party_short, party_name_english,
                party_name_original, everything()) %>% 
  mutate(populism =  rowMeans(select(., "manichean", "indivisible", "generalwill", "peoplecentrism", "antielitism"))) %>% 
  relocate(populism, .before = manichean) %>% 
  relocate(wave, .after = party_short) %>% 
  arrange(poppa_id, wave) %>% 
  dplyr::left_join(., read.csv("original_data/party_family_labels.csv") %>% 
                     select(poppa_id, wave, family = family_combined, family_label = family_combined_names),
                   by = c("poppa_id", "wave")) %>% 
  relocate(family, .before = poppa_id) %>% 
  relocate(family_label, .before = poppa_id) %>% 
  group_by(poppa_id) %>% 
  fill(partyfacts_id, .direction = "downup") %>%
  ungroup()

# We do some smaller changes to the dataset.

# We eliminate those rows that have all missing values.

combined <- combined %>% 
  dplyr::filter(!if_all(populism:personalised, ~ is.na(.) | is.nan(.)))

# We rename Czech to the Czech Republic 

combined <- combined %>% 
  mutate(country = forcats::fct_recode(country,
                                       "Czech Republic" = "Czech")) 



# The following steps are outlined more fully in our paper that introduces the data: "The State of Populism: Introducing the 2023 Wave of the Populism and Political Parties Expert Survey"
# We create the populism variable from the CFA model. 
# We then attach the regression scores.
# We provide the original scale and a rescaled version of the populism variable.
# We rescale the variable from 0 - 10. 


cfa_combined <- '

populism_cfa =~ manichean + generalwill + peoplecentrism + antielitism + indivisible

generalwill ~~ indivisible
peoplecentrism ~~ antielitism
manichean ~~  antielitism

'

# We fit the model to the data.

fit_combined <- cfa(model = cfa_combined, data = combined)


# We attach the regression/ factor scores to the full dataset.

idx <- lavInspect(fit_combined, "case.idx")
fscores <- lavPredict(fit_combined)
## loop over factors
for (fs in colnames(fscores)) {
  combined[idx, fs] <- fscores[ , fs]
}


# We rescale the variable from 0 to 10. 

combined$populism_cfa_rescaled <- scales::rescale(combined$populism_cfa, to = c(0, 10))


# We rename the populism mean variable to `populism_mean` and we move the `populism_mean` variable so that all the populism variables appear at the end of the datasest. 

combined <- combined %>% 
  dplyr::rename(populism_mean = populism) %>% 
  dplyr::relocate(populism_mean, .before = populism_cfa)


# We filter the variables that we are still withholding. 


combined <- combined %>% 
   select(-c(peopleimm,
             peoplewealthy,
             elitepol,
             elitemedia,
             eliteecon,
             climatepolicy,
             climatescience,
             democracy,
             positionchange,
             compromise))


# We save the final dataset. 
                   
saveRDS(object = combined, file = "final_data/poppa_integrated.rds")
haven::write_dta(combined, here("non_shareable/poppa_integrated_unlabelled.dta"))
readr::write_csv(combined, here("final_data/poppa_integrated.csv"))

