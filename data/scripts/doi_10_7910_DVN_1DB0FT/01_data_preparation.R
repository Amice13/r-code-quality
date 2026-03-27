########################
### DATA PREPARATION ###
########################

# Load raw data
load("bureaucratic_attention_data.RData")
load("cabinets_data.RData")
load("cap_manifesto_data.RData")
load("mp_cap_crosswalk.RData")
load("lists_capcodes.RData")

DE <- DE %>%
  group_by(cabinet, ministry, office_id) %>%
  mutate(office_weight = 1,
         function_weight = 1/n())

DK <-DK %>%
  group_by(cabinet, ministry, agency) %>%
  mutate(function_weight = 1 / n())

NL <- NL %>%
  group_by(cabinet, ministry, office_id) %>%
  mutate(office_weight = 1,
         function_weight = 1/n())

DE <- DE %>%
  left_join(DE_cab %>%
              select(cabinet, ministry, party, minister_from) %>%
              arrange(cabinet, ministry, minister_from) %>%
              group_by(cabinet, ministry) %>%
              summarise(party = first(party)),
            by = c("cabinet" = "cabinet", "ministry" = "ministry")) %>%

  # Collapse the German CDU and CSU
  mutate(party = if_else(party == "CSU", "CDU", party))


DK <- DK %>%
  # Join via a matching table that contains ministries (DK) and ministries (DK_cab)
  left_join(expand.grid(pull(DK %>% ungroup() %>%
                               select(ministry_id, ministry) %>% distinct()),
                        pull(DK_cab %>% select(ministry) %>% distinct())) %>%
              rename(ministry_o = Var1, ministry_c = Var2) %>%
              group_by(ministry_o) %>%
              mutate(similarity = str_similarity(ministry_o, ministry_c)) %>%
              filter(similarity == min(similarity)) %>%
              ungroup() %>%
              mutate(ministry_o = as.character(ministry_o),
                     ministry_c = as.character(ministry_c)) %>%
              select(-similarity),
            by = c("ministry" = "ministry_o")) %>% 
  # Match party information from DK_cab
  left_join(DK_cab %>%
              select(cabinet, party, ministry, minister_from) %>%
              group_by(cabinet, ministry) %>%
              filter(minister_from == min(minister_from)) %>%
              select(cabinet, party, ministry),
            by = c("cabinet" = "cabinet", "ministry_c" = "ministry")) %>%
  select(-ministry_c)


NL <- NL %>%
  left_join(NL_cab %>%
              group_by(cabinet_name, ministry_name) %>%
              rename(party = minister_party) %>%
              filter(minister_from == min(minister_from)) %>%
              select(cabinet_name, ministry_name, party) %>%
              ungroup() %>%
              mutate(cabinet_name = str_remove(cabinet_name, "Kabinet "),
                     ministry_name = if_else(
                       ministry_name == "Veiligheid en Justitie",
                       "Justitie en Veiligheid",
                       ministry_name),
                     ministry_name = case_when(
                       str_detect(ministry_name, "^minister voor") ~ ministry_name,
                       TRUE ~ str_c("Ministerie van", ministry_name, sep = " "))),
            by = c("cabinet" = "cabinet_name")) %>%
  mutate(strdist = stringdist(ministry, ministry_name)) %>%
  group_by(unique_id) %>%
  filter(strdist == min(strdist)) %>% 
  select(-strdist, -ministry_name) 


# Use Manifesto Data to calculate issue salience
DE_attention <-
  DE_attention %>%
  rename(election = Election, party = Party, national_minor = Subtopic) %>%
  filter(election > 1998) %>%
  mutate(cabinet = recode(election,
                          `2002` = "Schröder II", `2005`= "Merkel I",
                          `2009` = "Merkel II", `2013` = "Merkel III"),
         party = if_else(party == "Grünen", "Grüne", party),
         national_minor = if_else(national_minor == 9900,
                                  as.integer(9999),
                                  as.integer(national_minor))) %>%
  
  # Filter incumbent parties
  mutate(keep = case_when(cabinet == "Schröder II" & party %in% c("SPD", "Grüne") ~ TRUE,
                          cabinet %in% c("Merkel I", "Merkel III") &
                            party %in% c("CDU", "SPD") ~ TRUE,
                          cabinet == "Merkel II" & party %in% c("CDU", "FDP") ~ TRUE,
                          TRUE ~ FALSE)) %>%
  filter(keep) %>% select(-keep) %>%
  
  # Calculate attention
  group_by(cabinet, party) %>%
  mutate(sentences = n()) %>%
  group_by(cabinet, party, national_minor) %>%
  summarise(attention = unique(n() / sentences)) %>%
  filter(!is.na(national_minor)) %>%
  
  # Set attention non-mentioned policy issues to 0
  spread(key = national_minor, value = attention, fill = 0) %>%
  gather(key = "national_minor", value = "attention", -cabinet, -party) %>%
  mutate(national_minor = as.integer(national_minor),
         country = "DE")

DK_attention <-
  DK_attention %>%
  rename(national_minor = dk_new_code) %>%
  filter(year >= 1994) %>%
  mutate(cabinet = recode(year,
                          `1994` = "Nyrup Rasmussen III", `1998` = "Nyrup Rasmussen IV",
                          `2001` = "Fogh Rasmussen I", `2005` = "Fogh Rasmussen II",
                          `2007` = "Fogh Rasmussen III"),
         party = recode(party,
                        `1` = "A", `2` = "B", `3` = "C", `11` = "V",
                        .default = "other")) %>%
  
  # Filter incumbent parties
  mutate(keep = case_when(cabinet == "Nyrup Rasmussen III" & party %in% c("A", "B") ~ TRUE,
                          cabinet == "Nyrup Rasmussen IV" & party %in% c("A", "B") ~ TRUE,
                          cabinet == "Fogh Rasmussen I" & party %in% c("V", "C") ~ TRUE,
                          cabinet == "Fogh Rasmussen II" & party %in% c("V", "C") ~ TRUE,
                          cabinet == "Fogh Rasmussen III" & party %in% c("V", "C") ~ TRUE,
                          TRUE ~ FALSE)) %>%
  filter(keep) %>% select(-keep) %>%
  
  # Calculate attention
  group_by(cabinet, party) %>%
  mutate(sentences = n()) %>%
  group_by(cabinet, party, national_minor) %>%
  summarise(attention = unique(n() / sentences)) %>%
  
  # Set attention non-mentioned policy issues to 0
  spread(key = national_minor, value = attention, fill = 0) %>%
  gather(key = "national_minor", value = "attention", -cabinet, -party) %>%
  mutate(national_minor = as.integer(national_minor),
         country = "DK")

NL_attention <-
  NL_attention %>%
  select(Partynumber, Year, code) %>%
  rename(party = Partynumber, year = Year, national_minor = code) %>%
  
  filter(year >= 1994, national_minor != 99, national_minor > 0) %>%
  mutate(cabinet = recode(year, `1994` = "Kok I", `1998`= "Kok II", `2002` = "Balkenende I",
                          `2003` = "Balkenende II", `2006` = "Balkenende IV",
                          `2010` = "Rutte I", `2012` = "Rutte II"),
         party = recode(party, "CU Christian" = "christenunie", "LPF Fortuijn" = "lpf"),
         party = tolower(party)) %>%
  filter(party %in% unique(NL$party)) %>%
  
  # Filter incumbent parties
  right_join(NL_cab %>%
               select(cabinet_name, minister_party) %>%
               rename(cabinet = cabinet_name, party = minister_party) %>%
               distinct() %>%
               mutate(cabinet = str_remove(cabinet, "Kabinet ")),
             by = c("cabinet", "party")) %>%
  
  # Calculate attention
  group_by(cabinet, party) %>%
  mutate(sentences = n()) %>%
  group_by(cabinet, party, national_minor) %>%
  summarise(attention = unique(n() / sentences)) %>%
  filter(cabinet != "Balkenende III") %>%
  
  # Set attention non-mentioned policy issues to 0
  spread(key = national_minor, value = attention, fill = 0) %>%
  gather(key = "national_minor", value = "attention", -cabinet, -party) %>%
  mutate(national_minor = as.integer(national_minor),
         country = "NL")
  
Attention <- 
  bind_rows(DE_attention, DK_attention, NL_attention)
rm(DE_attention, DK_attention, NL_attention)


# Party Positions
mp_cap_recoding <- list(
  "409" = "budget_left",
  "414" = "budget_right",
  "503" = "welfare_left",
  "504" = "welfare_left",
  "606" = "welfare_left",
  "505" = "welfare_right",
  "301" = "decentralisation_left",
  "302" = "decentralisation_right",
  "506" = "education_left",
  "507" = "education_right",
  "105" = "defence_left",
  "104" = "defence_right",
  "405" = "labour_left",
  "701" = "labour_left",
  "702" = "labour_right",
  "103" = "internationalism_left",
  "106" = "internationalism_left",
  "107" = "internationalism_left",
  "109" = "internationalism_right",
  "108" = "eu_left",
  "110" = "eu_right",
  "602" = "immigration_left",
  "607" = "immigration_left",
  "601" = "immigration_right",
  "608" = "immigration_right",
  "416" = "environment_left",
  "501" = "environment_left",
  "410" = "environment_right",
  "403" = "economy_left",
  "404" = "economy_left",
  "412" = "economy_left",
  "413" = "economy_left",
  "401" = "economy_right",
  "402" = "economy_right",
  "407" = "agriculture_left",
  "406" = "agriculture_right",
  "703" = "agriculture_right",
  "201" = "civilrights_left",
  "202" = "civilrights_left",
  "604" = "civilrights_left",
  "705" = "civilrights_left",
  "706" = "civilrights_left",
  "603" = "civilrights_right",
  "605" = "civilrights_right"
)

options(warn = -1)
Positions <-
  read_dta("MPDataset_MPDS2019a_stata14.dta") %>%
  filter(countryname %in% c("Germany", "Denmark", "Netherlands"),
         edate >= "1994-01-01", edate < "2014-01-01") %>% 
  mutate(partyabbrev = if_else(partyabbrev == "", partyname, partyabbrev)) %>%
  select(countryname, edate, partyabbrev, absseat, totseats,
         starts_with("per"), -pervote, -peruncod) %>%
  rename(country = countryname, party = partyabbrev, year = edate) %>%
  mutate(year = as.numeric(format(year,'%Y'))) %>% 
  gather(key = "issue", value = "salience",
         -country, -year, -party, -absseat, -totseats) %>%
  mutate(issue = str_remove(issue, "per"),
         salience = if_else(is.na(salience), 0, salience),
         direction = recode(issue, !!!mp_cap_recoding, .default = "")) %>%
  filter(direction != "") %>% 
  ungroup() %>%
  mutate(cluster = str_extract(direction, ".*_"),
         cluster = str_remove(cluster, "_"),
         direction = if_else(str_detect(direction, "_ri"), "right", "left")) %>%
  group_by(country, year, party, cluster, direction) %>%
  summarise(absseat = first(absseat), totseats = first(totseats),
            salience = sum(salience)) %>%
  group_by(country, year, party, cluster) %>%
  spread("direction", "salience") %>%
  mutate(cluster_pos = log((right + 0.5) / (left + 0.5) )) %>%
  ungroup() %>%
  mutate(cabinet = case_when(country == "Germany" & year == 2002 ~ "Schröder II",
                             country == "Germany" & year == 2005 ~ "Merkel I",
                             country == "Germany" & year == 2009 ~ "Merkel II",
                             country == "Germany" & year == 2013 ~ "Merkel III",
                             
                             country == "Denmark" & year == 1994 ~ "Nyrup Rasmussen III",
                             country == "Denmark" & year == 1998 ~ "Nyrup Rasmussen IV",
                             country == "Denmark" & year == 2001 ~ "Fogh Rasmussen I",
                             country == "Denmark" & year == 2005 ~ "Fogh Rasmussen II",
                             country == "Denmark" & year == 2007 ~ "Fogh Rasmussen III",
                             
                             country == "Netherlands" & year == 1994 ~ "Kok I",
                             country == "Netherlands" & year == 1998 ~ "Kok II",
                             country == "Netherlands" & year == 2002 ~ "Balkenende I",
                             country == "Netherlands" & year == 2003 ~ "Balkenende II",
                             country == "Netherlands" & year == 2006 ~ "Balkenende IV",
                             country == "Netherlands" & year == 2010 ~ "Rutte I",
                             country == "Netherlands" & year == 2012 ~ "Rutte II",
                             
                             TRUE ~ NA_character_),
         party = recode(party,
                        "SD" = "A", "KF" = "C", "RV" = "B", "V" = "V",
                        "SPD" = "SPD","90/Greens" = "Grüne", "CDU/CSU" = "CDU",
                        "FDP" = "FDP",
                        "CDA"= "cda", "CU" = "christenunie", "D’66" = "d66",
                        "LPF" = "lpf", "PvdA" = "pvda", "VVD" = "vvd",
                        .default = NA_character_),
         country = recode(country,
                          "Germany" = "DE", "Denmark" = "DK", "Netherlands" = "NL")) %>%
  filter(!is.na(cabinet), !is.na(party)) %>%
  select(country, year,cabinet, party, cluster, absseat, totseats, cluster_pos) %>%
  arrange(country, year, cluster, party) %>%
  
  # Subset to cabinet parties
  left_join(bind_rows(DE, DK, NL) %>% group_by(cabinet) %>% distinct(party),
            .,
            by = c("cabinet", "party")) %>%
  arrange(country, year, cluster, party)

rm(mp_cap_recoding)
options(warn = defaultW)


# Count number of ministries per cabinet
DE_ministries <-
  DE %>%
  ungroup() %>%
  select(cabinet, party, ministry) %>%
  group_by(cabinet, party) %>%
  summarise(n_ministries = n_distinct(ministry)) %>%
  mutate(country = "DE")

DK_ministries <-
  DK %>%
  ungroup() %>%
  select(cabinet, party, ministry) %>%
  group_by(cabinet, party) %>%
  summarise(n_ministries = n_distinct(ministry)) %>%
  mutate(country = "DK")

NL_ministries <-
  NL %>%
  ungroup() %>%
  select(cabinet, party, ministry) %>%
  group_by(cabinet, party) %>%
  summarise(n_ministries = n_distinct(ministry)) %>%
  mutate(country = "NL")

NumberMinistries <- bind_rows(DE_ministries, DK_ministries, NL_ministries) %>%
  group_by(country, cabinet) %>%
  mutate(rel_ministries = n_ministries / sum(n_ministries))

rm(DE_ministries, DK_ministries, NL_ministries)


# Calculate bureaucratic attention
DE_party <-
  DE %>%
  ungroup() %>%
  select(cabinet, ministry, agency, agency_admin,
         function_weight, party, national_minor) %>%
  filter(agency_admin == FALSE, ministry != "BK") %>%
  
  group_by(cabinet) %>%
  mutate(n_parties_cabinet = n_distinct(party),
         n_ministries_cabinet = n_distinct(ministry),
         bureau_attention_total = sum(function_weight)) %>%
  group_by(cabinet, national_minor) %>%
  mutate(n_parties = n_distinct(party),
         n_ministries = n_distinct(ministry)) %>%
  group_by(cabinet, national_minor, party) %>%
  summarise(n_parties_cabinet = first(n_parties_cabinet),
            n_parties = first(n_parties),
            n_ministries_cabinet = first(n_ministries_cabinet),
            n_ministries = first(n_ministries),
            bureau_attention = sum(function_weight) / first(bureau_attention_total)) %>%
  group_by(cabinet, national_minor) %>%
  mutate(bureau_responsibility = bureau_attention / sum(bureau_attention)) %>%
  
  arrange(cabinet, national_minor, party)

DK_party <-
  DK %>%
  ungroup() %>%
  select(cabinet, ministry, agency, agency_admin,
         function_weight, party, national_minor) %>%
  filter(agency_admin == FALSE, ministry != "Statsministeriet") %>%
  
  group_by(cabinet) %>%
  mutate(n_parties_cabinet = n_distinct(party),
         n_ministries_cabinet = n_distinct(ministry),
         bureau_attention_total = sum(function_weight)) %>%
  group_by(cabinet, national_minor) %>%
  mutate(n_parties = n_distinct(party),
         n_ministries = n_distinct(ministry)) %>%
  group_by(cabinet, national_minor, party) %>%
  summarise(n_parties_cabinet = first(n_parties_cabinet),
            n_parties = first(n_parties),
            n_ministries_cabinet = first(n_ministries_cabinet),
            n_ministries = first(n_ministries),
            bureau_attention = sum(function_weight) / first(bureau_attention_total)) %>% 
  
  group_by(cabinet, national_minor) %>%
  mutate(bureau_responsibility = bureau_attention / sum(bureau_attention)) %>%
  
  arrange(cabinet, national_minor, party)

NL_party <-
  NL %>%
  ungroup() %>%
  select(cabinet, ministry, agency, agency_admin,
         function_weight, party, national_minor) %>%
  filter(agency_admin == FALSE, ministry != "Ministerie van Algemene Zaken") %>%
  
  # party level: attention
  group_by(cabinet) %>%
  mutate(n_parties_cabinet = n_distinct(party),
         n_ministries_cabinet = n_distinct(ministry),
         bureau_attention_total = sum(function_weight)) %>%
  group_by(cabinet, national_minor) %>%
  mutate(n_parties = n_distinct(party),
         n_ministries = n_distinct(ministry)) %>%
  group_by(cabinet, national_minor, party) %>%
  summarise(n_parties_cabinet = first(n_parties_cabinet),
            n_parties = first(n_parties),
            n_ministries_cabinet = first(n_ministries_cabinet),
            n_ministries = first(n_ministries),
            bureau_attention = sum(function_weight) / first(bureau_attention_total)) %>% 
  
  group_by(cabinet, national_minor) %>%
  mutate(bureau_responsibility = bureau_attention / sum(bureau_attention)) %>%
  
  arrange(cabinet, national_minor, party)
  
Bureaucatic_Attention <-
  bind_rows(mutate(DE_party, country = "DE"),
            mutate(DK_party, country = "DK"),
            mutate(NL_party, country = "NL"))

rm(DE_party, DK_party, NL_party)


# Merge everything
Data_Party <- bind_rows(
  create_complete_data(country = "DE",
                       cabinet_data = get_cabinet_data(DE),
                       cap_codes = DE_codes$deu_minor),
  create_complete_data(country = "DK",
                       cabinet_data = get_cabinet_data(DK),
                       cap_codes = DK_codes$dnk_minor),
  create_complete_data(country = "NL",
                       cabinet_data = get_cabinet_data(NL),
                       cap_codes = NL_codes$nl_minor)
) %>%
  left_join(mp_cap_match %>%
              filter(!is.na(cluster)) %>%
              select(cluster, minor) %>%
              rename(national_minor = minor),
            by = ("national_minor")) %>%
  
  # merge meta data
  left_join(select(Positions, country, cabinet, party, year, absseat, totseats) %>%
              distinct(),
            by = c("country", "cabinet", "party_x" = "party"),
            suffix = c("_x", "_y")) %>%
  left_join(select(Positions, country, cabinet, party, absseat) %>%
              distinct(),
            by = c("country", "cabinet", "party_y" = "party"),
            suffix = c("_x", "_y")) %>% 
  
  # merge bureaucratic attention
  left_join(select(Bureaucatic_Attention, -starts_with("n_")),
            by = c("country", "cabinet", "national_minor", "party_x" = "party"),
            suffix = c("_x", "_y")) %>%
  left_join(select(Bureaucatic_Attention, -starts_with("n_")),
            by = c("country", "cabinet", "national_minor", "party_y" = "party"),
            suffix = c("_x", "_y")) %>%
  mutate(bureau_attention_total = sum_na(bureau_attention_x, bureau_attention_y)) %>%
  
  # merge attention
  left_join(Attention,
            by = c("country", "cabinet", "national_minor", "party_x" = "party"),
            suffix = c("_x", "_y")) %>%
  left_join(Attention,
            by = c("country", "cabinet", "national_minor", "party_y" = "party"),
            suffix = c("_x", "_y")) %>%
  mutate(attention_total = sum_na(attention_x, attention_y)) %>%
  
  # merge position
  left_join(select(Positions, -year, -totseats, -absseat),
            by = c("country", "cabinet", "cluster", "party_x" = "party"),
            suffix = c("_x", "_y")) %>%
  left_join(select(Positions, -year, -totseats, -absseat),
            by = c("country", "cabinet", "cluster", "party_y" = "party"),
            suffix = c("_x", "_y")) %>%
  
  # merge number of ministries
  left_join(select(NumberMinistries, -n_ministries),
            by = c("country", "cabinet", "party_x" = "party"),
            suffix = c("_x", "_y")) %>%
  left_join(select(NumberMinistries, -n_ministries),
            by = c("country", "cabinet", "party_y" = "party"),
            suffix = c("_x", "_y")) %>%
  
  # Impute missing attention with 0
  mutate(across(.cols = contains("attention"),
                function(x) if_else(is.na(x), 0, x)),
         across(.cols = contains("responsibility"),
                function(x) if_else(is.na(x), 0, x)))


# Calculate party differences
Data_Party <- Data_Party %>%
  mutate(bureau_attention_diff = abs(bureau_attention_x - bureau_attention_y),
         bureau_responsibility_diff = abs(bureau_responsibility_x - bureau_responsibility_y),
         bureau_responsibility_diff = if_else(is.nan(bureau_responsibility_diff), 0,
                                              bureau_responsibility_diff),
         sharedness = 1 - bureau_responsibility_diff,
         attention_diff = abs(attention_x - attention_y),
         position_diff = abs(cluster_pos_x - cluster_pos_y),
         seat_diff = abs(absseat_x/totseats - absseat_y/totseats),
         ministries_diff = abs(rel_ministries_x - rel_ministries_y),
         ministries_total = rel_ministries_x + rel_ministries_y) %>%
  group_by(country) %>%
  mutate(sharedness_infl = inflate(sharedness, n()))


# Translate major policy areas
Data_Party <- Data_Party %>%
  mutate(major = as.character(as.integer(national_minor/100)),
         major = factor(major,
                        levels = c("1", "2", "3", "4", "5", "6", "7", "8", "9", "10", "12", "13", "14", "15", "16", "17", "18", "19",
                                   "20", "21", "23", "24", "25"),
                        labels = c("Macroeconomics",
                                   "Civil Rights",
                                   "Health",
                                   "Agriculture",
                                   "Labor",
                                   "Education",
                                   "Environment",
                                   "Energy",
                                   "Immigration",
                                   "Transportation",
                                   "Law and Crime",
                                   "Social Welfare",
                                   "Housing",
                                   "Domestic Commerce",
                                   "Defense",
                                   "Technology",
                                   "Foreign Trade",
                                   "International Affairs",
                                   "Government Operations",
                                   "Public Lands",
                                   "Culture",
                                   "State and Local Government Administration",
                                   "German Reunification"
                        )))


# Drop policy issues which no ministry attends to
Data_Party_Pos <- subset(Data_Party, bureau_attention_total > 0)
