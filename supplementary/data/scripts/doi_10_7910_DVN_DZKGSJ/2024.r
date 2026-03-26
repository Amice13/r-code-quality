##-----------------------------2024 Analysis-------------------------------##
##----------------------------Authors: Rose Nafa and Liza Gordon-Rogers----##
##-----------------------------Created:  9/30/25---------------------------##
##-----------------------------Last Modified: 2/6/2026--------------------##

#Load required packages
options(scipen = 999)

pacman::p_load(tidyverse,
               #data manipulation
               dplyr,
               #data manipulation 
               tidycensus,
               #census (survey) API
               tigris,
               #census (mapping) API
               sf,
               #shape files
               readr,
               #read txt files
               readxl,
               #read excel files
               geomander,
               #geoaggregation 
               lme4,
               #multilevel models
               MuMIn, 
               #mixed effects fit statistics
               install = FALSE)

#------------------------Census Data-------------------------------------------
#read in census from bg-block conversion
full_census_blocks <- st_read("../DATA/2023_census/b_24_est.shp") %>%
  transmute(
    GEOID = GEOID20,
    block,
    bg,
    tract,
    county,
    state,
    tot_cvap = est_cvp_t,
    white_cvap = est_cvp_w,
    hisp_cvap = est_cvp_h,
    black_cvap = est_cvp_b,
    asian_cvap = est_cvp_s,
    pacific_cvap = est_cvp_p,
    native_cvap = est_cvp_n
  )

maricopa_census <- full_census_blocks %>%
  filter(county == "Maricopa County" & state == "Arizona")

fulton_census <- full_census_blocks %>%
  filter(county == "Fulton County" & state == "Georgia")

wayne_census <- full_census_blocks  %>%
  filter(county == "Wayne County" & state == "Michigan")

durham_census <- full_census_blocks %>%
  filter(county == "Durham County" & state == "North Carolina")

mecklenburg_census <- full_census_blocks %>%
  filter(county == "Mecklenburg County" & state == "North Carolina")

columbus_census <- full_census_blocks %>%
  filter(county == "Columbus County" & state == "North Carolina")

cuyahoga_census <- full_census_blocks %>%
  filter(county == "Cuyahoga County" & state == "Ohio")

lorain_census <- full_census_blocks %>%
  filter(county == "Lorain County" & state == "Ohio")

allegheny_census <- full_census_blocks %>%
  filter(county == "Allegheny County" & state == "Pennsylvania")

philadelphia_census <- full_census_blocks %>%
  filter(county == "Philadelphia County" & state == "Pennsylvania")

milwaukee_census <- full_census_blocks  %>%
  filter(county == "Milwaukee County" & state == "Wisconsin")

#---------------------------------Maricopa------------------------------------
# 04013 Maricopa AZ

##------------------------------vf-------------------------------------------

maricopa_vf_raw <- read.delim(
  "../DATA/2024_election_results/maricopa.txt",
  sep = "\t",
  header = TRUE,
  stringsAsFactors = FALSE
) %>%
  filter(ContestName == "Presidential Electors") %>%
  mutate(
    candidate_group = case_when(
      CandidateName == "TRUMP / VANCE" ~ "Donald J. Trump",
      CandidateName == "HARRIS / WALZ" ~ "Kamala D. Harris",
      CandidateName == "Write-in" ~ "other",
      CandidateName %in% c(
        "*NOT QUALIFIED*",
        "AYYADURAI / ELLIS",
        "BODDIE / STONEHAM",
        "DE LA CRUZ / GARCIA",
        "OLIVER / TER MAAT",
        "SHELTON / SERRA",
        "SKOUSEN / COMBS",
        "STEIN / WARE"
      ) ~ "other",
      TRUE ~ "other"
    )
  )

maricopa_candidate_totals <- maricopa_vf_raw %>%
  group_by(PrecinctName, candidate_group) %>%
  summarise(total_votes = sum(Votes, na.rm = TRUE),
            .groups = "drop") %>%
  pivot_wider(names_from = candidate_group,
              values_from = total_votes,
              values_fill = 0) %>%
  left_join(
    maricopa_vf_raw %>%
      select(
        PrecinctName,
        PrecinctRegistered,
        PrecinctTurnout,
        PrecinctTurnoutPerc
      ) %>%
      distinct(),
    by = "PrecinctName"
  )

maricopa_over_under_totals <- maricopa_vf_raw %>%
  group_by(PrecinctName) %>%
  summarise(
    over_vote = sum(Overvotes, na.rm = TRUE),
    under_vote = sum(Undervotes, na.rm = TRUE),
    .groups = "drop"
  )

maricopa_method_totals <- maricopa_vf_raw %>%
  group_by(PrecinctName) %>%
  summarise(
    EARLY_VOTE = sum(Votes_EARLY.VOTE, na.rm = TRUE),
    ELECTION_DAY = sum(Votes_ELECTION.DAY, na.rm = TRUE),
    PROVISIONAL = sum(Votes_PROVISIONAL, na.rm = TRUE),
    .groups = "drop"
  )

# Merge all summaries into a single data frame
maricopa_vf <- maricopa_candidate_totals %>%
  left_join(maricopa_over_under_totals, by = "PrecinctName") %>%
  left_join(maricopa_method_totals, by = "PrecinctName") %>%
  mutate(PrecinctName = str_trim(str_remove(PrecinctName, "\\d+")))

maricopa_shp <- st_read(
  "../DATA/2024_shapefiles/Maricopa_County_Voting_Precincts_(2022-)_2760828096242529363/VotingPrecincts2022.shp"
)

maricopa_vf_shp <- maricopa_shp %>%
  left_join(maricopa_vf, by = c("BdName" = "PrecinctName"))

##----------------------------geomander----------------------------------------

maricopa_census <- st_transform(maricopa_census, st_crs(maricopa_vf_shp))

maricopa_matches <-
  geo_match(from = maricopa_census, to = maricopa_vf_shp, method = "centroid")

maricopa_prec <- block2prec(block_table = maricopa_census, matches = maricopa_matches)

maricopa_block_full <- bind_cols(maricopa_vf_shp, maricopa_prec) %>%
  transmute(
    GEOID = BdVal,
    NAME = BdName,
    county,
    state,
    CTY_ID = 013,
    PREC_ID1 = PctNum,
    G24PRERTRU = `Donald J. Trump`,
    G24PREDHAR = `Kamala D. Harris`,
    G24PREO = other,
    G24votes = PrecinctTurnout,
    voter_reg = PrecinctRegistered,
    prov = PROVISIONAL,
    tot_cvap,
    voter_tnt_cvap = (G24votes / tot_cvap) * 100, 
    voter_tnt_cvap = if_else(voter_tnt_cvap >= 99.9 | voter_tnt_cvap == 0, NA_real_, voter_tnt_cvap),
    white_pct = ifelse(white_cvap > 0, trunc(white_cvap / tot_cvap * 100), 0),
    hisp_pct = ifelse(hisp_cvap > 0, trunc(hisp_cvap / tot_cvap * 100), 0),
    black_pct = ifelse(black_cvap > 0, trunc(black_cvap / tot_cvap * 100), 0),
    api_pct = ifelse((asian_cvap + pacific_cvap) > 0, trunc((
      asian_cvap + pacific_cvap
    ) / tot_cvap * 100), 0),
    native_pct = ifelse(native_cvap > 0, trunc(native_cvap / tot_cvap * 100), 0),
    maj_race = case_when(
      tot_cvap == 0 ~ "No-Pop",
      white_pct > 49 ~ "White",
      black_pct > 49 ~ "Black",
      hisp_pct > 49 ~ "Hispanic",
      api_pct > 49 ~ "Asian/Pacific Islander",
      native_pct > 49 ~ "Native",
      TRUE ~ "Other"
    ),
    pv_rej = NA_real_,
    ballots_cast = G24votes,
    rej_pct = NA_real_,
    rej_binned = NA_real_,
    voter_tnt = (ballots_cast / voter_reg) * 100,
    voter_tnt = case_when(
      voter_tnt > 100 ~ NA_real_,
      voter_tnt == 0 ~ NA_real_,
      TRUE ~ voter_tnt
    )
  ) %>%
  select(
    county,
    state,
    CTY_ID,
    PREC_ID1,
    NAME,
    G24votes,
    G24PRERTRU,
    G24PREDHAR,
    voter_tnt, 
    voter_tnt_cvap,
    prov,
    pv_rej,
    ballots_cast,
    rej_pct,
    rej_binned,
    maj_race,
    tot_cvap,
    white_pct,
    black_pct, 
    hisp_pct,
    api_pct,
    native_pct,
    voter_reg
  )


maricopa_2020 <- read_csv("../DATA/full_datasets/AZfull_20.csv") %>%
  select(GEOID, PREC_ID1, NAME, maj_race20)

maricopa_full <- left_join(maricopa_block_full, maricopa_2020, by = "NAME") %>%
  mutate(
    maj_race = case_when(is.na(maj_race) ~ maj_race20, TRUE ~ maj_race),
    PREC_ID1 = PREC_ID1.x
  ) %>%
  select(-PREC_ID1.y, -maj_race20) %>%
  select(
    GEOID,
    county,
    state,
    CTY_ID,
    PREC_ID1,
    NAME,
    G24votes,
    G24PRERTRU,
    G24PREDHAR,
    voter_tnt,
    voter_tnt_cvap,
    prov,
    pv_rej,
    ballots_cast,
    rej_pct,
    rej_binned,
    maj_race,
    tot_cvap,
    white_pct,
    black_pct, 
    hisp_pct,
    api_pct,
    native_pct,
    voter_reg
  )

AZ_full <- maricopa_full

#st_write(maricopa_full,
  #"../DATA/full_datasets/AZfull_24.shp", append = FALSE)

st_write(AZ_full,
"../DATA/full_datasets/AZfull_2024.shp", append = FALSE)


#----------------------------------Fulton-------------------------------------
# 13121 Fulton GA
##------------------------------vf-------------------------------------------

fulton_vf <-
  read_csv(
    "../DATA/2024_election_results/trimmed_Fulton_Nov_05_2024_StatementOfVotesCastRPT-2-_1_.csv",
    col_names = FALSE
  ) %>%
  rename(raw_text = X1) %>%
  filter(!is.na(raw_text), raw_text != "") %>%
  filter(
    !str_detect(
      raw_text,
      "Page:|Registered|Precinct|Voters|County|Statement|General|Official"
    )
  ) %>%
  separate(
    raw_text,
    into = c(
      "Precinct",
      "voter_reg",
      "cards_cast",
      "total_votes",
      "turnout"
    ),
    sep = "\\s{2,}",
    remove = FALSE
  ) %>%
  transmute(
    Precinct,
    voter_reg = as.integer(gsub(",", "", voter_reg)),
    cards_cast = as.integer(gsub(",", "", cards_cast)),
    total_votes = as.integer(gsub(",", "", total_votes)),
    turnout = as.numeric(gsub("%", "", turnout))
  )


fulton_shp <- st_read("../DATA/2024_shapefiles/Fulton/Voting_Precincts.shp") %>%
  select(OBJECTID, VoterDist, geometry)

fulton_vf_shp <- fulton_shp %>%
  left_join(fulton_vf, by = c("VoterDist" = "Precinct")) 


##----------------------------geomander----------------------------------------

fulton_census <- st_transform(fulton_census, st_crs(fulton_vf_shp))

fulton_matches <-
  geo_match(from = fulton_census, to = fulton_vf_shp, method = "centroid")

fulton_prec <- block2prec(block_table = fulton_census, matches = fulton_matches)

fulton_full <- bind_cols(fulton_vf_shp, fulton_prec) %>%
  mutate(
    white_pct = trunc(white_cvap  / tot_cvap * 100),
    hisp_pct =  trunc(hisp_cvap  / tot_cvap  * 100),
    black_pct = trunc(black_cvap  / tot_cvap  * 100),
    api_pct = trunc((asian_cvap  + pacific_cvap) / tot_cvap * 100),
    native_pct = trunc(native_cvap  / tot_cvap * 100),
    maj_race = case_when(
      white_pct > 49 ~ "White",
      black_pct > 49 ~ "Black",
      hisp_pct > 49 ~ "Hispanic",
      api_pct > 49 ~ "Asian/Pacific Islander",
      native_pct > 49 ~ "Native",
      is.na(tot_cvap) ~ NA,
      TRUE ~ "Other"
    ),
    GEOID = as.character(OBJECTID),
    county = "Fulton",
    state = "GA",
    CTY_ID = 121,
    NAME = VoterDist,
    PREC_ID1 = VoterDist,
    G24votes = total_votes,
    G24PRERTRU = NA_real_,
    G24PREDHAR = NA_real_,
    voter_reg = voter_reg,
    voter_tnt = turnout,
    tot_cvap,
    voter_tnt_cvap = (G24votes / tot_cvap) * 100, 
    voter_tnt_cvap = if_else(voter_tnt_cvap >= 99.9 | voter_tnt_cvap == 0, NA_real_, voter_tnt_cvap),
    prov = NA_real_,
    pv_rej = NA_real_,
    ballots_cast = cards_cast,
    rej_pct = NA_real_,
    rej_binned = NA_real_,
    voter_tnt = (G24votes / voter_reg) * 100,
    voter_tnt = case_when(
      voter_tnt > 100 ~ NA_real_,
      voter_tnt == 0 ~ NA_real_,
      TRUE ~ voter_tnt
    )
  ) %>%
  select(
    GEOID,
    county,
    state,
    CTY_ID,
    PREC_ID1,
    NAME,
    G24votes,
    G24PRERTRU,
    G24PREDHAR,
    voter_tnt,
    voter_tnt_cvap,
    prov,
    pv_rej,
    ballots_cast,
    rej_pct,
    rej_binned,
    maj_race,
    tot_cvap,
    white_pct,
    black_pct, 
    hisp_pct,
    api_pct,
    native_pct,
    voter_reg
  )

GA_full <- fulton_full

st_write(fulton_full, "../DATA/full_datasets/GAfull_2024.shp", append = FALSE)

#------------------------------------Wayne-------------------------------------
# 26163 Wayne MI

mi_reg_raw <- read_csv("../DATA/2024_election_results/MI_EntireStateVoters_24.csv") 

mi_voters <- mi_reg_raw %>% 
  filter(COUNTY_NAME == "WAYNE") %>% 
  select(VOTER_IDENTIFICATION_NUMBER, PRECINCT, JURISDICTION_NAME)

mi_precinct_reg <- mi_reg_raw %>% 
  filter(COUNTY_NAME == "WAYNE") %>% 
  group_by(PRECINCT, JURISDICTION_NAME) %>% 
  summarise(voter_reg = n()) 

mi_voterhist_1 <- read_csv("../DATA/2024_election_results/MI_January Entire State Voter History 2025 Redacted Part 1.csv")

mi_voterhist_2 <- read_csv("../DATA/2024_election_results/MI_January Entire State Voter History 2025 Redacted Part 2.csv")

mi_voterhist_full <- bind_rows(mi_voterhist_1, mi_voterhist_2) %>% 
  mutate(ELECTION_DATE = as.Date(ELECTION_DATE)) %>% #read dates without timestamp
  filter(ELECTION_DATE == "2024-11-05"& COUNTY_NAME == "WAYNE") #keep only 2024 election from wayne county 

mi_reg_votes <- left_join(mi_voterhist_full, mi_voters, by = c("VOTER_IDENTIFICATION_NUMBER", "JURISDICTION_NAME"))%>% 
  group_by(JURISDICTION_NAME, PRECINCT) %>% 
  summarise(voter_tnt = n()) %>% 
  left_join(mi_precinct_reg, by = c("JURISDICTION_NAME", "PRECINCT")) %>% 
  separate(PRECINCT, into = c("first2", "last3"), sep = 2) %>% 
  transmute(
    NAME =  str_to_upper(JURISDICTION_NAME) %>%
      str_remove("CITY") %>%
      str_remove("CHARTER TOWNSHIP") %>%
      str_remove("TOWNSHIP OF ") %>% 
      str_remove("TOWNSHIP") %>% 
      str_squish(),
    PREC_ID1 = as.character(last3),
    voter_reg,
    G24votes = voter_tnt) %>% 
  drop_na(PREC_ID1)
  
  wayne_shp <- st_read("../DATA/2024_shapefiles/Wayne (all of MI)/2024_Voting_Precincts.shp") %>%
  filter(COUNTYFIPS == 163) %>%
  separate(Precinct_L, into = c("NAME", "PREC_ID1"), sep = ", ") %>% 
  transmute(
    PREC_ID1 = as.character(str_pad(str_remove(PREC_ID1, "Precinct "), pad = 0, width = 3)),
    NAME = str_to_upper(NAME) %>%
           str_remove("CITY OF ") %>%
           str_remove("CHARTER TOWNSHIP OF ") %>%
           str_remove("TOWNSHIP OF ") %>% 
           str_remove("TOWNSHIP") %>% 
      str_squish(),
    geometry) %>% 
    drop_na(PREC_ID1)

wayne_vf_shp <- mi_reg_votes %>% 
  left_join(wayne_shp, by = c("NAME", "PREC_ID1")) %>% 
  transmute(
    GEOID = NAME,
    county = "Wayne",
    state = "MI",
    CTY_ID = 163,
    PREC_ID1,
    NAME,
    G24votes,
    G24PRERTRU = NA_real_,
    G24PREDHAR = NA_real_,
    voter_reg,
    voter_tnt = (G24votes / voter_reg) * 100,
    voter_tnt = case_when(
      voter_tnt > 100 ~ NA_real_,
      voter_tnt == 0 ~ NA_real_,
      TRUE ~ voter_tnt
    ),
    prov = NA_real_,
    pv_rej = NA_real_,
    rej_denom = NA_real_,
    ballots_cast = G24votes,
    rej_pct = NA_real_,
    rej_binned = NA_real_,
    geometry) %>%
  st_as_sf()

##-------------------------------geomander--------------------------------------

wayne_census <- st_transform(wayne_census, st_crs(wayne_vf_shp))

wayne_prec <- st_intersection(
  wayne_census,
  wayne_vf_shp
) %>%
  mutate(
    intersection_area = st_area(.)) %>%
  filter(as.numeric(intersection_area) > 0) %>%
  left_join(
    st_drop_geometry(wayne_census) %>%
      mutate(block_area = st_area(wayne_census)) %>%
      select(GEOID, block_area),
    by = "GEOID"
  ) %>%
  mutate(
    area_fraction = as.numeric(intersection_area / block_area),
    tot_cvap = tot_cvap * area_fraction,
    white_cvap = white_cvap * area_fraction,
    hisp_cvap = hisp_cvap * area_fraction,
    black_cvap = black_cvap * area_fraction,
    asian_cvap = asian_cvap * area_fraction,
    pacific_cvap = pacific_cvap * area_fraction,
    native_cvap = native_cvap * area_fraction
  ) %>%
  group_by(NAME, PREC_ID1) %>%  
  summarise(tot_cvap = sum(tot_cvap, na.rm = TRUE),
            white_cvap = sum(white_cvap, na.rm = TRUE),
            hisp_cvap = sum(hisp_cvap, na.rm = TRUE),
            black_cvap = sum(black_cvap, na.rm = TRUE),
            asian_cvap = sum(asian_cvap, na.rm = TRUE),
            pacific_cvap = sum(pacific_cvap, na.rm = TRUE),
            native_cvap = sum(native_cvap, na.rm = TRUE)
  ) %>% 
  st_drop_geometry()

wayne_block_full <- wayne_prec %>% 
  left_join(wayne_vf_shp, by = c("NAME", "PREC_ID1"))


wayne_block_full2 <- wayne_block_full %>%
  mutate(
    white_pct = ifelse(white_cvap > 0, trunc(white_cvap / tot_cvap * 100), 0),
    hisp_pct = ifelse(hisp_cvap > 0, trunc(hisp_cvap / tot_cvap * 100), 0),
    black_pct = ifelse(black_cvap > 0, trunc(black_cvap / tot_cvap * 100), 0),
    api_pct = ifelse((asian_cvap + pacific_cvap) > 0, trunc((
      asian_cvap + pacific_cvap
    ) / tot_cvap * 100), 0),
    native_pct = ifelse(native_cvap > 0, trunc(native_cvap / tot_cvap * 100), 0),
    maj_race = case_when(
      tot_cvap == 0 ~ "No-Pop",
      white_pct > 49 ~ "White",
      black_pct > 49 ~ "Black",
      hisp_pct > 49 ~ "Hispanic",
      api_pct > 49 ~ "Asian/Pacific Islander",
      native_pct > 49 ~ "Native",
      is.na(tot_cvap) ~ "NA",
      TRUE ~ "Plurality"
    )
  ) %>%
  select(
    GEOID,
    county,
    state,
    CTY_ID,
    PREC_ID1,
    NAME,
    G24PRERTRU,
    G24PREDHAR,
    G24votes,
    voter_tnt,
    prov,
    pv_rej,
    ballots_cast,
    rej_pct,
    rej_binned,
    rej_denom,
    maj_race,
    tot_cvap,
    white_pct,
    black_pct, 
    hisp_pct,
    api_pct,
    native_pct,
    voter_reg
  ) %>%
  separate(NAME, into = c("NAME", "prec_num"), sep = ", ") %>% 
  mutate( #these nas were hand coded using https://bestneighborhood.org/ racial maps and precinct maps from city websites
    maj_race = case_when(
      NAME == "Charter Township of Huron" ~ "White",
      NAME == "City of Inkster" ~ "Black",
      NAME == "Charter Township of Northville" ~ "White",
      NAME == "Township of Sumpter" ~ "White",
      NAME == "Charter Township of Brownstown" ~ "White",
      NAME == "The Village of Grosse Pointe Shores" ~ "White",
      TRUE ~ maj_race
    ),
    maj_race = case_when(
      PREC_ID1 == "City of Inkster, District 4 Precinct 1" ~ "White",
      TRUE ~ maj_race
    ),
    voter_tnt_cvap = if_else(is.na(pv_rej), 
                             (G24votes / tot_cvap) * 100, 
                             (ballots_cast / tot_cvap) * 100),
    voter_tnt_cvap = if_else(voter_tnt_cvap >= 99.9 | voter_tnt_cvap == 0, NA_real_, voter_tnt_cvap)) %>% 
  unite(NAME, prec_num, col = "NAME", sep = ", ")

wayne_full_temp <- wayne_block_full2 %>%
#  left_join(wayne_rej, by = "PREC_ID1") %>%
  mutate(
    pv_rej = NA_real_,
    ballots_cast = G24votes,
    rej_pct = NA_real_,
    rej_binned = NA_real_) %>% 
  separate(NAME, into = c("NAME", "drop"), sep = ",") %>% 
  select(-drop)

wayne_precs <- wayne_shp %>% 
  select(NAME, PREC_ID1)


wayne_full <- left_join(wayne_precs, wayne_full_temp, by = c("NAME", "PREC_ID1")) %>% 
  unique()


MI_full <- wayne_full %>% 
  select(
    GEOID,
    county,
    state,
    CTY_ID,
    PREC_ID1,
    NAME,
    G24votes,
    G24PRERTRU,
    G24PREDHAR,
    voter_tnt,
    voter_tnt_cvap,
    prov,
    pv_rej,
    ballots_cast,
    rej_pct,
    rej_binned,
    maj_race,
    tot_cvap,
    white_pct,
    black_pct, 
    hisp_pct,
    api_pct,
    native_pct,
    voter_reg
  )

#write_csv(MI_full, "../DATA/full_datasets/mi_full_temp_revised.csv")

st_write(MI_full, "../DATA/full_datasets/MIfull_2024.shp", append = FALSE)

st_write(wayne_full, "../DATA/full_datasets/waynefull_2024.shp", append = FALSE)


#----------------------------------NC-------------------------------------------

##-----------------------------------Durham-------------------------------------
# 37063 Durham

durham_vf_temp <- read_delim(file = "../DATA/2024_election_results/durham_handfix.txt", delim = "\t") %>%
  mutate(
    candidate_group = case_when(
      candidate_name == "Donald J. Trump" ~ "DJT",
      candidate_name == "Kamala D. Harris" ~ "KDH",
      TRUE ~ "other"
    ),
    vote_type = case_when(group_name == "PROVISIONAL" ~ "prov", TRUE ~ "notprov"),
    vote_group = paste(candidate_group, vote_type, sep = "_")
  ) %>%
  group_by(precinct_code, precinct_name, vote_group) %>%
  summarize(votes = sum(vote_ct), .groups = "drop") %>%
  pivot_wider(names_from = vote_group,
              values_from = votes,
              values_fill = 0) %>%
  transmute(
    PREC_ID1 = precinct_code,
    NAME = precinct_name,
    state = "NC",
    county = "Durham",
    CTY_ID = 063,
    G24votes = DJT_prov + DJT_notprov + KDH_prov + KDH_notprov + other_prov + other_notprov,
    prov = DJT_prov + KDH_prov + other_prov,
    G24PRERTRU = DJT_prov + DJT_notprov,
    G24PREDHAR = KDH_prov + KDH_notprov,
    G24PREO = other_prov + other_notprov
  )

durham_reg <- read_delim(file = "../DATA/2024_election_results/nc_reg.txt", delim = "\t") %>%
  filter(county_desc == "DURHAM") %>%
  group_by(precinct_abbrv) %>%
  summarize(voter_reg = sum(total_voters))

durham_vf <- right_join(durham_vf_temp, durham_reg, by = c("PREC_ID1" = "precinct_abbrv")) %>%
  drop_na(G24votes)

durham_shp <- st_read("../DATA/2024_shapefiles/All of NC/Voting_Precincts.shp") %>%
  select(OBJECTID, PRECINCT, geometry)

durham_vf_shp <- durham_shp %>%
  left_join(durham_vf, by = c("PRECINCT" = "PREC_ID1")) %>%
  rename(PREC_ID1 = PRECINCT)

###-------------------------------geomander------------------------------------

durham_census <- st_transform(durham_census, st_crs(durham_vf_shp))

durham_matches <-
  geo_match(from = durham_census, to = durham_vf_shp, method = "centroid")

durham_prec <- block2prec(block_table = durham_census, matches = durham_matches)

durham_full <- bind_cols(durham_vf_shp, durham_prec) %>%
  mutate(
    county = "Durham",
    state = "NC",
    white_pct = trunc(white_cvap  / tot_cvap * 100),
    hisp_pct =  trunc(hisp_cvap  / tot_cvap  * 100),
    black_pct = trunc(black_cvap  / tot_cvap  * 100),
    api_pct = trunc((asian_cvap  + pacific_cvap) / tot_cvap * 100),
    native_pct = trunc(native_cvap  / tot_cvap * 100),
    maj_race = case_when(
      white_pct > 49 ~ "White",
      black_pct > 49 ~ "Black",
      hisp_pct > 49 ~ "Hispanic",
      api_pct > 49 ~ "Asian/Pacific Islander",
      native_pct > 49 ~ "Native",
      is.na(tot_cvap) ~ NA,
      TRUE ~ "Other"
    ),
    GEOID = as.character(OBJECTID),
    voter_tnt = (G24votes / voter_reg) * 100,
    voter_tnt = case_when(
      voter_tnt > 100 ~ NA_real_,
      voter_tnt == 0 ~ NA_real_,
      TRUE ~ voter_tnt
    ),
    tot_cvap,
    voter_tnt_cvap = (G24votes / tot_cvap) * 100, 
    voter_tnt_cvap = if_else(voter_tnt_cvap >= 99.9 | voter_tnt_cvap == 0, NA_real_, voter_tnt_cvap),
  )



##--------------------------------Mecklenburg-----------------------------------
# 37119 Mecklenburg NC
#Mecklenburg split into two docs to fix dimension mismatch

meck_vf_temp <- read_delim(file = "../DATA/2024_election_results/meck_handfix.txt", delim = "\t") %>%
  mutate(
    candidate_group = case_when(
      candidate_name == "Donald J. Trump" ~ "DJT",
      candidate_name == "Kamala D. Harris" ~ "KDH",
      TRUE ~ "other"
    ),
    vote_type = case_when(group_name == "PROVISIONAL" ~ "prov", TRUE ~ "notprov"),
    vote_group = paste(candidate_group, vote_type, sep = "_")
  ) %>%
  group_by(precinct_code, precinct_name, vote_group) %>%
  summarize(votes = sum(vote_ct), .groups = "drop") %>%
  pivot_wider(names_from = vote_group,
              values_from = votes,
              values_fill = 0) %>%
  transmute(
    PREC_ID1 = precinct_code,
    NAME = precinct_name,
    state = "NC",
    county = "Mecklenburg",
    CTY_ID = 119,
    #Meck doesn't have any provisional ballots in the file, so this calc is a little different than the others in NC on purpose
    G24votes =DJT_notprov + KDH_notprov + other_notprov,
    prov = NA_real_,
    G24PRERTRU = DJT_notprov,
    G24PREDHAR = KDH_notprov,
    G24PREO = other_notprov
  )

meck_reg <- read_delim(file = "../DATA/2024_election_results/nc_reg.txt", delim = "\t") %>%
  filter(county_desc == "MECKLENBURG") %>%
  group_by(precinct_abbrv) %>%
  summarize(voter_reg = sum(total_voters))

meck_vf <- right_join(meck_vf_temp, meck_reg, by = c("PREC_ID1" = "precinct_abbrv")) %>%
  drop_na(G24votes)

meck_shp <- st_read("../DATA/2024_shapefiles/All of NC/SBE_PRECINCTS_20240723.shp") %>% 
  filter(county_nam == "MECKLENBURG") %>% 
  select(id, prec_id, geometry)

meck_vf_shp <- meck_shp %>% 
  right_join(meck_vf, by = c("prec_id" = "PREC_ID1"))




###-------------------------------geomander------------------------------------

meck_census <- st_transform(mecklenburg_census, st_crs(meck_vf_shp))

meck_matches <-
  geo_match(from = meck_census, to = meck_vf_shp, method = "centroid")

meck_prec <- block2prec(block_table = meck_census, matches = meck_matches)

meck_full <- bind_cols(meck_vf_shp, meck_prec) %>%
  mutate(
    county = "Mecklenburg",
    state = "NC",
    PREC_ID1 = prec_id,
    white_pct = trunc(white_cvap  / tot_cvap * 100),
    hisp_pct =  trunc(hisp_cvap  / tot_cvap  * 100),
    black_pct = trunc(black_cvap  / tot_cvap  * 100),
    api_pct = trunc((asian_cvap  + pacific_cvap) / tot_cvap * 100),
    native_pct = trunc(native_cvap  / tot_cvap * 100),
    maj_race = case_when(
      white_pct > 49 ~ "White",
      black_pct > 49 ~ "Black",
      hisp_pct > 49 ~ "Hispanic",
      api_pct > 49 ~ "Asian/Pacific Islander",
      native_pct > 49 ~ "Native",
      is.na(tot_cvap) ~ NA,
      TRUE ~ "Other"
    ),
    GEOID = as.character(id),
    voter_tnt = (G24votes / voter_reg) * 100,
    voter_tnt = case_when(
      voter_tnt > 100 ~ NA_real_,
      voter_tnt == 0 ~ NA_real_,
      TRUE ~ voter_tnt
    ),
    tot_cvap,
    voter_tnt_cvap = (G24votes / tot_cvap) * 100, 
    voter_tnt_cvap = if_else(voter_tnt_cvap >= 99.9 | voter_tnt_cvap == 0, NA_real_, voter_tnt_cvap),
  )



##----------------------------------Columbus------------------------------------
# 37047 Columbus NC

columbus_vf_temp <- read_delim(file = "../DATA/2024_election_results/columbus_handfix.txt", delim = "\t") %>%
  mutate(
    candidate_group = case_when(
      candidate_name == "Donald J. Trump" ~ "DJT",
      candidate_name == "Kamala D. Harris" ~ "KDH",
      TRUE ~ "other"
    ),
    vote_type = case_when(group_name == "PROVISIONAL" ~ "prov", TRUE ~ "notprov"),
    vote_group = paste(candidate_group, vote_type, sep = "_")
  ) %>%
  group_by(precinct_code, precinct_name, vote_group) %>%
  summarize(votes = sum(vote_ct), .groups = "drop") %>%
  pivot_wider(names_from = vote_group,
              values_from = votes,
              values_fill = 0) %>%
  transmute(
    PREC_ID1 = precinct_code,
    NAME = precinct_name,
    state = "NC",
    county = "Columbus",
    CTY_ID = 047,
    G24votes = DJT_prov + DJT_notprov + KDH_prov + KDH_notprov + other_prov + other_notprov,
    prov = DJT_prov + KDH_prov + other_prov,
    G24PRERTRU = DJT_prov + DJT_notprov,
    G24PREDHAR = KDH_prov + KDH_notprov,
    G24PREO = other_prov + other_notprov
  )


columbus_reg <- read_delim(file = "../DATA/2024_election_results/nc_reg.txt", delim = "\t") %>%
  filter(county_desc == "COLUMBUS") %>%
  group_by(precinct_abbrv) %>%
  summarize(voter_reg = sum(total_voters))

columbus_vf <- right_join(columbus_vf_temp,
                          columbus_reg,
                          by = c("PREC_ID1" = "precinct_abbrv")) %>%
  drop_na(G24votes)

columbus_shp <- st_read("../DATA/2024_shapefiles/All of NC/SBE_PRECINCTS_20240723.shp") %>% 
  filter(county_nam == "COLUMBUS") %>% 
  select(id, prec_id, geometry)

columbus_vf_shp <- columbus_shp %>% 
  right_join(columbus_vf, by = c("prec_id" = "PREC_ID1"))

###-------------------------------geomander------------------------------------

columbus_census <- st_transform(columbus_census, st_crs(columbus_vf_shp))

columbus_matches <-
  geo_match(from = columbus_census, to = columbus_vf_shp, method = "centroid")

columbus_prec <- block2prec(block_table = columbus_census, matches = columbus_matches)

columbus_full <- bind_cols(columbus_vf_shp, columbus_prec) %>%
  mutate(
    county = "Columbus",
    state = "NC",
    white_pct = trunc(white_cvap  / tot_cvap * 100),
    hisp_pct =  trunc(hisp_cvap  / tot_cvap  * 100),
    black_pct = trunc(black_cvap  / tot_cvap  * 100),
    api_pct = trunc((asian_cvap  + pacific_cvap) / tot_cvap * 100),
    native_pct = trunc(native_cvap  / tot_cvap * 100),
    maj_race = case_when(
      white_pct > 49 ~ "White",
      black_pct > 49 ~ "Black",
      hisp_pct > 49 ~ "Hispanic",
      api_pct > 49 ~ "Asian/Pacific Islander",
      native_pct > 49 ~ "Native",
      is.na(tot_cvap) ~ NA,
      TRUE ~ "Other"
    ),
    GEOID = as.character(id),
    voter_tnt = (G24votes / voter_reg) * 100,
    voter_tnt = case_when(
      voter_tnt > 100 ~ NA_real_,
      voter_tnt == 0 ~ NA_real_,
      TRUE ~ voter_tnt
    )
  )


NC_full <- bind_rows(durham_full, meck_full, columbus_full) %>% 
  mutate(
    GEOID,
    county = case_when(
      !is.na(`county...5`) ~ `county...5`,
      TRUE ~ `county...21`),
    county = case_when(
      county == "Durham County" ~ "Durham",
      TRUE ~ county
    ),
    state = "NC",
    CTY_ID,
    PREC_ID1,
    NAME,
    voter_reg,
    G24votes,
    G24PRERTRU,
    G24PREDHAR,
    voter_tnt,
    prov,
    pv_rej = NA_real_,
    ballots_cast = NA_real_,
    rej_pct = NA_real_,
    rej_binned = NA_real_,
    maj_race,
    tot_cvap,
    voter_tnt_cvap = (G24votes / tot_cvap) * 100,
    voter_tnt_cvap = if_else(voter_tnt_cvap >= 99.9 | voter_tnt_cvap == 0, NA_real_, voter_tnt_cvap),
  ) %>%
  select(
    GEOID,
    county,
    state,
    CTY_ID,
    PREC_ID1,
    NAME,
    G24votes,
    G24PRERTRU,
    G24PREDHAR,
    voter_tnt,
    voter_tnt_cvap,
    prov,
    pv_rej,
    ballots_cast,
    rej_pct,
    rej_binned,
    maj_race,
    tot_cvap,
    white_pct,
    black_pct, 
    hisp_pct,
    api_pct,
    native_pct,
    voter_reg
  )



#st_write(NC_full, "../DATA/full_datasets/NCfull_2024.shp")
#----------------------------------Cuyahoga------------------------------------
# 39035 Cuyahoga OH

##------------------------------vf-------------------------------------------

cuyahoga_votes <-
  read_excel("../DATA/2024_election_results/Cuyahoga_NOV24GEN - Official Results(1).xlsx") %>%
  pivot_wider(names_from = "Group", values_from = "Total") %>%
  transmute(
    Name = Precinct,
    total = rowSums(across(4:9), na.rm = TRUE),
    prov = Provisional,
    rej_denom = Provisional + `Vote By Mail` + `Post VBM`
  )

cuyahoga_rej_raw <-
  read_excel("../DATA/2024_election_results/Cuyahoga_NOV2024GEN Rejected Provisionals.xls") 

cuyahoga_rej <- cuyahoga_rej_raw %>%
  group_by(consolidation_name_actual, failsafe_adjudication) %>%
  summarise(tot_rej = n(), .groups = "drop") %>%
  group_by(consolidation_name_actual) %>%
  slice_max(tot_rej, with_ties = FALSE) %>%
  rename(max_reason = failsafe_adjudication)

cuyahoga_rej_reasons <- cuyahoga_rej_raw %>%
  count(reason = failsafe_adjudication) %>%
  mutate(
    `% Rej` = round((n / sum(n)) * 100, 1)
  ) %>%
  rename(`# Rej` = n) %>%
  arrange(desc(`# Rej`))%>%
  bind_rows(
    summarise(
      .,
      reason = "Total",
      `# Rej` = sum(`# Rej`, na.rm = TRUE),
      `% Rej` = 100
    )
  )

cuyahoga_vf <- left_join(cuyahoga_votes, cuyahoga_rej, by = c("Name" = "consolidation_name_actual")) %>%
  replace_na(list(tot_rej = 0)) %>%
  mutate(Name = str_remove_all(Name, " "))

cuyahoga_shp <- read_sf("../DATA/2024_shapefiles/Cuyahoga/Voting_Precincts_region.shp") %>%
  st_make_valid() %>%
  mutate(Name = str_remove_all(Name, " ")) %>%
  select(Name, geometry) %>%
  unique() %>%
  drop_na(Name)


cuyahoga_vf_temp <- cuyahoga_shp %>%
  left_join(cuyahoga_vf, by = "Name") %>%
  drop_na(total) %>%
  unique()

cuyahoga_reg <- read_csv("../Data/2024_election_results/ohio_voter_reg.csv") %>%
  mutate(date_col = mdy(`Last Registration Date`)) %>%
  filter(date_col <= as.Date("2025-10-07")) %>%
  unite(city,
        ward,
        col = "temp",
        sep = "-",
        remove = FALSE) %>%
  unite(temp,
        pct,
        col = "PREC_ID1",
        sep = "-",
        remove = FALSE) %>%
  group_by(PREC_ID1) %>%
  summarise(voter_reg = n()) %>%
  mutate(
    PREC_ID1 = PREC_ID1 %>%
      str_replace("\\bPEPPER PIKE\\b", "PEPPERPIKE") %>%
      str_replace("\\bRICHMOND HTS\\b", "RICHMONDHEIGHTS") %>%
      str_replace("\\bROCKY RIVER\\b", "ROCKYRIVER") %>%
      str_replace("\\bSOUTH EUCLID\\b", "SOUTHEUCLID") %>%
      str_replace("\\bSHAKER HTS\\b", "SHAKERHEIGHTS") %>%
      str_replace("\\bSEVEN HILLS\\b", "SEVENHILLS") %>%
      str_replace("\\bUNIVERSITY HTS\\b", "UNIVERSITYHEIGHTS") %>%
      str_replace("\\bVALLEY VIEW\\b", "VALLEYVIEW") %>%
      str_replace("\\bWARRENSVILLE HTS\\b", "WARRENSVILLEHTS") %>%
      str_replace("\\bCLEVELAND HTS\\b", "CLEVELANDHEIGHTS") %>%
      str_replace("\\bEAST CLEVELAND\\b", "EASTCLEVELAND") %>%
      str_replace("\\bWALTON HILLS\\b", "WALTONHILLS") %>%
      str_replace("\\bFAIRVIEW PARK\\b", "FAIRVIEWPARK") %>%
      str_replace("\\bGARFIELD HTS\\b", "GARFIELDHEIGHTS") %>%
      str_replace("\\bGATES MILLS\\b", "GATESMILLS") %>%
      str_replace("\\bHIGHLAND HILLS\\b", "HIGHLANDHILLS") %>%
      str_replace("\\bBAY VILLAGE\\b", "BAYVILLAGE") %>%
      str_replace("\\bBEDFORD HTS\\b", "BEDFORDHEIGHTS") %>%
      str_replace("\\bBROADVIEW HTS\\b", "BROADVIEWHEIGHTS") %>%
      str_replace("\\bBROOK PARK\\b", "BROOKPARK") %>%
      str_replace("\\bCHAGRIN FALLS\\b", "CHAGRINFALLS") %>%
      str_replace("\\bCHAGRIN FALLS TWP\\b", "CHAGRINFALLSTWP") %>%
      str_replace("\\bCUYAHOGA HTS\\b", "CUYAHOGAHEIGHTS") %>%
      str_replace("\\bHIGHLAND HTS\\b", "HIGHLANDHEIGHTS") %>%
      str_replace("\\bHUNTING VALLEY\\b", "HUNTINGVALLEY") %>%
      str_replace("\\bMAYFEILD HEIGHTS\\b", "MAYFEILDHEIGHTS") %>%
      str_replace("\\bCHAGRINFALLS TWP\\b", "CHAGRINFALLSTWP") %>%
      str_replace("\\bMAPLE HTS\\b", "MAPLEHEIGHTS") %>%
      str_replace("\\bMAYFIELD HTS\\b", "MAYFIELDHEIGHTS") %>%
      str_replace("\\bMAYFIELD VILLAGE\\b", "MAYFIELDVILLAGE") %>%
      str_replace("\\bMIDDLEBURG HTS\\b", "MIDDLEBURGHEIGHTS") %>%
      str_replace("\\bMORELAND HILLS\\b", "MORELANDHILLS") %>%
      str_replace("\\bNORTH OLMSTED\\b", "NORTHOLMSTED") %>%
      str_replace("\\bNORTH ROYALTON\\b", "NORTHROYALTON") %>%
      str_replace("\\bOLMSTED FALLS\\b", "OLMSTEDFALLS") %>%
      str_replace("\\bOLMSTED TWP\\b", "OLMSTEDTOWNSHIP") %>%
      str_replace("\\bPARMA HTS\\b", "PARMAHEIGHTS") %>%
      str_replace("\\bBROOKLYN HTS\\b", "BROOKLYNHEIGHTS") %>%
      str_replace("\\bNEWBURGH HTS\\b", "NEWBURGHHEIGHTS") %>%
      str_replace("\\bNORTH RANDALL\\b", "NORTHRANDALL")
  ) %>%
  unique()

cuyahoga_vf_shp <- cuyahoga_vf_temp %>%
  left_join(cuyahoga_reg, by = c("Name" = "PREC_ID1"))

##----------------------------geomander----------------------------------------


cuyahoga_census <- st_transform(cuyahoga_census, st_crs(cuyahoga_vf_shp))

cuyahoga_matches <-
  geo_match(from = cuyahoga_census, to = cuyahoga_vf_shp, method = "centroid")

cuyahoga_prec <- block2prec(block_table = cuyahoga_census, matches = cuyahoga_matches)

cuyahoga_full_temp <- bind_cols(cuyahoga_vf_shp, cuyahoga_prec) %>%
  mutate(
    GEOID = NA_real_,
    county = "Cuyahoga",
    state = "OH",
    CTY_ID = 035,
    PREC_ID1 = NA_real_,
    NAME = Name,
    G24PRERTRU = NA_real_,
    G24PREDHAR = NA_real_,
    G24PREO = NA_real_,
    G24votes = total,
    pv_rej = tot_rej,
    voter_reg,
    prov = NA_real_,
    ballots_cast = total,
    rej_pct = (pv_rej / rej_denom) * 100,
    rej_binned = factor(
      ntile(rej_pct, 3),
      levels = 1:3,
      labels = c("Lower 3rd", "Middle 3rd", "Upper 3rd")
    ),
    voter_tnt = (ballots_cast / voter_reg) * 100,
    voter_tnt = case_when(
      voter_tnt > 100 ~ NA_real_,
      voter_tnt == 0 ~ NA_real_,
      TRUE ~ voter_tnt
    ),
    tot_cvap,
    voter_tnt_cvap = (G24votes / tot_cvap) * 100, 
    voter_tnt_cvap = if_else(voter_tnt_cvap >= 99.9 | voter_tnt_cvap == 0, NA_real_, voter_tnt_cvap),
    white_pct = trunc(white_cvap  / tot_cvap * 100),
    hisp_pct =  trunc(hisp_cvap  / tot_cvap  * 100),
    black_pct = trunc(black_cvap  / tot_cvap  * 100),
    api_pct = trunc((asian_cvap  + pacific_cvap) / tot_cvap * 100),
    native_pct = trunc(native_cvap  / tot_cvap * 100),
    maj_race = case_when(
      white_pct > 49 ~ "White",
      black_pct > 49 ~ "Black",
      hisp_pct > 49 ~ "Hispanic",
      api_pct > 49 ~ "Asian/Pacific Islander",
      native_pct > 49 ~ "Native",
      is.na(tot_cvap) ~ NA,
      TRUE ~ "Other"
    ),
    PREC_ID1 = as.character(PREC_ID1)
  )  


cuyahoga_full <- cuyahoga_full_temp %>%
  select(
    GEOID,
    county,
    state,
    CTY_ID,
    PREC_ID1,
    NAME,
    G24votes,
    G24PRERTRU,
    G24PREDHAR,
    voter_tnt,
    voter_tnt_cvap,
    prov,
    pv_rej,
    ballots_cast,
    rej_pct,
    rej_binned,
    maj_race,
    tot_cvap,
    white_pct,
    black_pct, 
    hisp_pct,
    api_pct,
    native_pct,
    voter_reg
  )

#write_csv(cuyahoga_full, "../DATA/full_datasets/cuyahoga_full_temp.csv")
#-----------------------------------Lorain-------------------------------------
# 39093 Lorain OH


##------------------------------vf-------------------------------------------

lorain_ev <- read_csv("../DATA/2024_election_results/lorrain_24G Group Detail Report Early Voting.csv") %>%
  filter(!is.na(`Lorain County`) &
           !is.na(...2) & !is.na(...3) & !is.na(...4)) %>%
  transmute(
    Precinct = `Lorain County`,
    voter_reg = ...2,
    early_votes = ...3,
    et_percentage = ...4,
    early_turnout = as.numeric(str_remove(...4, "%")) / 100
  )

lorain_absentee <- read_csv("../DATA/2024_election_results/lorrain_24G-Group-Detail-Report-Absentee_1.csv") %>%
  separate(
    `Lorain County                                                                            OFFICIAL RESULTS`,
    into = c("Precinct", "voter_reg", "absentee_votes", "absentee_turnout"),
    sep = "\\s{2,}",
    extra = "merge",
    fill = "right"
  ) %>%
  drop_na(Precinct, absentee_votes) %>% 
  filter(Precinct != "Totals") %>% 
  mutate(absentee_percentage = absentee_turnout,
         absentee_turnout = as.numeric(str_remove(absentee_turnout, "%")) / 100)

lorain_prov <- read_csv("../DATA/2024_election_results/lorrain_24G Group Detail Report Provisional.csv") %>%
  filter(!is.na(`Lorain County`) &
           !is.na(...2) & !is.na(...3) & !is.na(...4)) %>%
  transmute(
    Precinct = `Lorain County`,
    voter_reg = ...2,
    prov_votes = ...3,
    prov_percentage = ...4,
    prov_prop = as.numeric(str_remove(...4, "%")) / 100
  )

lorain_uocava <- read_csv("../DATA/2024_election_results/lorrain_24G-Group-Detail-Report-UOCAVA.csv") %>%
  separate(
    `Lorain County                                                                           OFFICIAL RESULTS`,
    into = c("Precinct", "voter_reg", "uocava_votes", "uocava_turnout"),
    sep = "\\s{2,}",
    extra = "merge",
    fill = "right"
  ) %>%
  drop_na(Precinct, uocava_votes) %>% 
  filter(Precinct != "Totals") %>% 
  mutate(uocava_percentage = uocava_turnout,
         uocava_turnout = as.numeric(str_remove(uocava_turnout, "%")) / 100)

lorain_ed <- read_csv(
  "../DATA/2024_election_results/lorrain_24G-Group-Detail-Report-Election-Day_1.csv"
) %>%
  separate(
    `Lorain County                                                                            OFFICIAL RESULTS`,
    into = c("Precinct", "voter_reg", "ed_votes", "ed_turnout"),
    sep = "\\s{2,}",
    extra = "merge",
    fill = "right"
  ) %>%
  drop_na(Precinct, ed_votes) %>% 
  filter(Precinct != "Totals") %>% 
  mutate(ed_percentage = ed_turnout,
         ed_turnout = as.numeric(str_remove(ed_turnout, "%")) / 100)

lorain_vf_full <- lorain_ev %>%
  full_join(lorain_absentee, by = c("Precinct", "voter_reg")) %>%
  full_join(lorain_prov, by = c("Precinct", "voter_reg")) %>%
  full_join(lorain_uocava, by = c("Precinct", "voter_reg")) %>%
  full_join(lorain_ed, by = c("Precinct", "voter_reg")) %>%
  replace_na(
    list(
      voter_reg = 0,
      early_votes = 0,
      early_turnout = 0,
      et_percentage = "0.00%",
      absentee_votes = "0",
      absentee_turnout = 0,
      absentee_percentage = "0.00%",
      prov_votes = 0,
      prov_turnout = 0,
      prov_percentage = "0.00%",
      uocava_votes = "0",
      uocava_turnout = 0,
      uocava_percentage = "0.00%",
      ed_votes = "0",
      ed_turnout = 0,
      ed_percentage = "0.00%"
    )
  ) %>%
  transmute(
    GEOID = NA_real_,
    county = "Lorain",
    state = "OH",
    CTY_ID = 093,
    Precinct,
    NAME = Precinct,
    G24PRERTRU = NA_real_,
    G24PREDHAR = NA_real_,
    G24PREO = NA_real_,
    G24votes =
      ( as.numeric(early_votes) +
          as.numeric(absentee_votes) +
          as.numeric(prov_votes) +
          as.numeric(uocava_votes) +
          as.numeric(ed_votes)),
    prov = as.numeric(prov_votes), 
    voter_reg = parse_number(voter_reg),
    voter_tnt = (G24votes / voter_reg) * 100,
    voter_tnt = case_when(
      voter_tnt > 100 ~ NA_real_,
      voter_tnt == 0 ~ NA_real_,
      TRUE ~ voter_tnt
    ),
    pv_rej = NA_real_,
    ballots_cast = NA_real_,
    rej_pct = NA_real_,
    rej_binned = NA_real_,
    maj_race = NA_real_
  )


lorain_shp <- st_read("../DATA/2024_shapefiles/Lorain/2024 County Precincts/Precincts.shp") %>%
  select(PRECINCT, geometry)


lorain_cross <- read_csv("../DATA/2024_shapefiles/Lorain/2024 County Precincts/lorain_crosswalk.csv")

lorain_vf_shp <- lorain_shp %>%
  left_join(lorain_cross, by = "PRECINCT") %>%
  left_join(lorain_vf_full, by = "Precinct") %>% 
  rename(PREC_ID1 = Precinct)


##----------------------------geomander----------------------------------------


lorain_census <- st_transform(lorain_census, st_crs(lorain_vf_shp))

lorain_matches <-
  geo_match(from = lorain_census, to = lorain_vf_shp, method = "centroid")

lorain_prec <- block2prec(block_table = lorain_census, matches = lorain_matches) %>% 
  bind_rows(
    slice(., 1:2) %>% mutate(across(everything(), ~ NA))
  )


lorain_prec <- lorain_prec[-c(208: 209),] 

lorain_full <- bind_cols(lorain_vf_shp, lorain_prec) %>% 
  mutate(
    county = `county...4`,
    state = `state...5`,
    white_pct = trunc(white_cvap  / tot_cvap * 100),
    hisp_pct =  trunc(hisp_cvap  / tot_cvap  * 100),
    black_pct = trunc(black_cvap  / tot_cvap  * 100),
    api_pct = trunc((asian_cvap  + pacific_cvap) / tot_cvap * 100),
    native_pct = trunc(native_cvap  / tot_cvap * 100),
    maj_race = case_when(
      white_pct > 49 ~ "White",
      black_pct > 49 ~ "Black",
      hisp_pct > 49 ~ "Hispanic",
      api_pct > 49 ~ "Asian/Pacific Islander",
      native_pct > 49 ~ "Native",
      is.na(tot_cvap) ~ NA,
      TRUE ~ "Other"
    ),
    rej_binned = as_factor(rej_binned),
    tot_cvap,
    voter_tnt_cvap = (G24votes / tot_cvap) * 100, 
    voter_tnt_cvap = if_else(voter_tnt_cvap >= 99.9 | voter_tnt_cvap == 0, NA_real_, voter_tnt_cvap),
  )  %>%
  select(
    GEOID,
    county,
    state,
    CTY_ID,
    PREC_ID1,
    NAME,
    G24votes,
    G24PRERTRU,
    G24PREDHAR,
    voter_tnt,
    voter_tnt_cvap,
    prov,
    pv_rej,
    ballots_cast,
    rej_pct,
    rej_binned,
    maj_race,
    tot_cvap,
    white_pct,
    black_pct, 
    hisp_pct,
    api_pct,
    native_pct,
    voter_reg
  )

lorain_full <- st_transform(lorain_full, st_crs(cuyahoga_full))

OH_full <- bind_rows(lorain_full, cuyahoga_full)

#st_write(OH_full, "../DATA/full_datasets/OHfull_2024.shp", append = FALSE)

#----------------------------------Allegheny-----------------------------------
# 42003 Allegheny PA

##------------------------------vf-------------------------------------------

allegheny_vf <- read_excel("../DATA/2024_election_results/Allegheny_24.xlsx", sheet = 2)

allegheny_shp <-
  st_read(
    "../DATA/2024_shapefiles/Allegheny_County_Election_Districts_7010957069832547247/Voting_Districts.shp"
  )

allegheny_vf_shp <-
  left_join(allegheny_vf, allegheny_shp, by = c("Precinct" = "Muni_War_1")) %>%
  transmute(
    Name = Precinct,
    registered = `Registered Voters`,
    election_day = `Election Day`,
    absentee = `Absentee`,
    prov = Provisional,
    total_votes = `Ballots Cast`,
    voter_turnout = `Voter Turnout`,
    rej_denom = absentee + prov,
    geometry
  ) %>%
  st_as_sf() %>%
  filter(Name != "Total:")


##----------------------------geomander----------------------------------------

allegheny_census <- st_transform(allegheny_census, st_crs(allegheny_vf_shp))

allegheny_matches <-
  geo_match(from = allegheny_census, to = allegheny_vf_shp, method = "centroid")

allegheny_prec <- block2prec(block_table = allegheny_census, matches = allegheny_matches)

allegheny_block_full <- bind_cols(allegheny_vf_shp, allegheny_prec) %>%
  transmute(
    NAME = Name,
    county = "Allegheny",
    state = "PA",
    voter_reg = registered,
    G24votes = total_votes,
    prov,
    election_day,
    absentee,
    vaptnt = trunc(G24votes / voter_reg),
    vap_tnt_pct = as.numeric(str_remove(voter_turnout, "%")),
    white_pct = trunc(white_cvap  / tot_cvap * 100),
    hisp_pct =  trunc(hisp_cvap  / tot_cvap  * 100),
    black_pct = trunc(black_cvap  / tot_cvap  * 100),
    api_pct = trunc((asian_cvap  + pacific_cvap) / tot_cvap * 100),
    native_pct = trunc(native_cvap  / tot_cvap * 100),
    maj_race20 = case_when(
      white_pct > 49 ~ "White",
      black_pct > 49 ~ "Black",
      hisp_pct > 49 ~ "Hispanic",
      api_pct > 49 ~ "Asian/Pacific Islander",
      native_pct > 49 ~ "Native",
      is.na(tot_cvap) ~ NA,
      TRUE ~ "Other"
    ),
    tot_cvap,
    voter_tnt_cvap = (G24votes / tot_cvap) * 100, 
    voter_tnt_cvap = if_else(voter_tnt_cvap >= 99.9 | voter_tnt_cvap == 0, NA_real_, voter_tnt_cvap)
  )

allegheny_2020 <- read_csv("../DATA/full_datasets/PAfull_20.csv") %>%
  filter(CTY_ID == "003") %>%
  select(GEOID, PREC_ID1, NAME, maj_race) %>%
  mutate(
    NAME = tools::toTitleCase(tolower(NAME)),
    NAME = NAME %>%
      str_replace("\\bDistrict\\b", "Dist") %>%
      str_replace("\\bMccandless\\b", "McCandless") %>%
      str_replace("\\bMckeesport\\b", "McKeesport") %>%
      str_replace("\\bMckees\\b", "McKees") %>%
      str_replace("\\bEast\\b", "E") %>%
      str_replace("\\bO'hara\\b", "O'Hara") %>%
      str_replace("\\bSpringdale\\b", "Springdale Br") %>%
      str_replace_all("\\b0(\\d)\\b", "\\1")
  )


allegheny_full <- left_join(allegheny_block_full, allegheny_2020, by = "NAME") %>%
  mutate(
    maj_race = case_when(is.na(maj_race20) ~ maj_race, TRUE ~ maj_race),
    maj_race = case_when(NAME == "McDonald" ~ "White", TRUE ~ maj_race),
    rej_pct = NA,
    rej_binned = NA,
    pv_rej = NA,
    CTY_ID = 42003,
    G24PRERTRU = NA_real_,
    G24PREDHAR = NA_real_,
    ballots_cast = NA_real_,
    voter_tnt = (G24votes / voter_reg) * 100
  ) %>%
  select(
    GEOID,
    county,
    state,
    CTY_ID,
    PREC_ID1,
    NAME,
    G24votes,
    G24PRERTRU,
    G24PREDHAR,
    voter_tnt,
    voter_tnt_cvap,
    prov,
    pv_rej,
    ballots_cast,
    rej_pct,
    rej_binned,
    maj_race,
    tot_cvap,
    white_pct,
    black_pct, 
    hisp_pct,
    api_pct,
    native_pct,
    voter_reg
  )


#st_write(allegheny_full,
 #"../DATA/full_datasets/allegheny_24.shp",
 #append = FALSE)

#--------------------------------Philadelphia----------------------------------
# 42101 Philadelphia PA

##------------------------------vf-------------------------------------------

phili_prov_temp <-
  read_csv("../DATA/2024_election_results/phili_prov.csv") %>%
  mutate(across(everything(), ~ replace_na(.x, 0))) %>%
  transmute(
    Precinct = substr(Precinct, 1, 4),
    pv_rej = Rejected
  )

phili_abs <- read_csv("../DATA/2024_election_results/2024G Vote Method Totals_Provisional Report.xlsx - Votes by Method.csv") %>% 
  transmute(
    Precinct = str_remove(Precinct, "-"), 
    abs = `Mail Votes`,
    prov = `Provisional`,
    pv_denom = abs + prov
  )

phili_prov <- left_join(phili_prov_temp, phili_abs, by = "Precinct")

phili_rej <- read_csv("../DATA/2024_election_results/phili_rej.csv") %>%
  mutate(across(everything(), ~ replace_na(.x, 0)),
         tot_rej = `Grand Total`,
         Precinct = substr(Precinct, 1, 4)) %>%
  mutate(
    max_reason = {
      rej_subset <- select(., -Precinct, -`Grand Total`, -`tot_rej`)
      rej_names <- colnames(rej_subset)
      max_idx <- max.col(rej_subset, ties.method = "first")
      rej_names[max_idx]
    }
  )

phili_rej_reasons <- phili_rej %>%
  select(-`Grand Total`,-max_reason) %>% 
  summarise(across(-Precinct, sum, na.rm = TRUE)) %>%
  pivot_longer(
    cols = -`tot_rej`,
    names_to = "reason",
    values_to = "# Rej"
  ) %>%
  mutate(`% Rej` = round((`# Rej` / tot_rej) * 100, 2)) %>%
  select(reason, `# Rej`, `% Rej`) %>% 
  arrange(desc(`# Rej`))


phili_reg <- read_csv("../DATA/2024_election_results/phili_reg.csv") %>%
  filter(election_name == "2024 General") %>%
  select(precinct_code, total_votes, registered_voters, turnout) 


phili_vf <- full_join(phili_prov, phili_rej, by = "Precinct") %>% 
  left_join(phili_reg, by = c("Precinct" = "precinct_code"))
  

phili_shp <- st_read("../DATA/2024_shapefiles/Philadelphia/Political_Divisions.shp")

phili_vf_shp <- phili_shp %>%
  left_join(phili_vf, by = c("DIVISION_N" = "Precinct"))

##----------------------------geomander----------------------------------------

phili_census <- st_transform(philadelphia_census, st_crs(phili_vf_shp))

phili_matches <-
  geo_match(from = phili_census, to = phili_vf_shp, method = "centroid")

phili_prec <- block2prec(block_table = phili_census, matches = phili_matches)

phili_full_temp <- bind_cols(phili_vf_shp, phili_prec) %>%
  mutate(
    white_pct = trunc(white_cvap  / tot_cvap * 100),
    hisp_pct =  trunc(hisp_cvap  / tot_cvap  * 100),
    black_pct = trunc(black_cvap  / tot_cvap  * 100),
    api_pct = trunc((asian_cvap  + pacific_cvap) / tot_cvap * 100),
    native_pct = trunc(native_cvap  / tot_cvap * 100),
    maj_race = case_when(
      white_pct > 49 ~ "White",
      black_pct > 49 ~ "Black",
      hisp_pct > 49 ~ "Hispanic",
      api_pct > 49 ~ "Asian/Pacific Islander",
      native_pct > 49 ~ "Native",
      is.na(tot_cvap) ~ NA,
      TRUE ~ "Other"
    ),
    GEOID = as.character(OBJECTID),
    county = "Philadelphia",
    state = "PA",
    CTY_ID = 101,
    NAME = DIVISION_N,
    PREC_ID1 = DIVISION_N,
    G24votes = total_votes,
    G24PRERTRU = NA_real_,
    G24PREDHAR = NA_real_,
    voter_tnt = turnout * 100,
    voter_reg = registered_voters,
    prov,
    pv_rej,
    ballots_cast = G24votes + pv_rej,
    rej_pct = (pv_rej / pv_denom) * 100,
    rej_binned = factor(
      ntile(rej_pct, 3),
      levels = 1:3,
      labels = c("Lower 3rd", "Middle 3rd", "Upper 3rd")
    ),
    voter_tnt = (G24votes / voter_reg) * 100,
    voter_tnt = case_when(
      voter_tnt > 100 ~ NA_real_,
      voter_tnt == 0 ~ NA_real_,
      TRUE ~ voter_tnt
    ),
    tot_cvap,
    voter_tnt_cvap = (G24votes / tot_cvap) * 100, 
    voter_tnt_cvap = if_else(voter_tnt_cvap >= 99.9 | voter_tnt_cvap == 0, NA_real_, voter_tnt_cvap)
  ) 


phili_full <- phili_full_temp %>%
  select(
    GEOID,
    county,
    state,
    CTY_ID,
    PREC_ID1,
    NAME,
    G24votes,
    G24PRERTRU,
    G24PREDHAR,
    voter_tnt,
    voter_tnt_cvap,
    prov,
    pv_rej,
    ballots_cast,
    rej_pct,
    rej_binned,
    maj_race,
    tot_cvap,
    white_pct,
    black_pct, 
    hisp_pct,
    api_pct,
    native_pct,
    voter_reg
  ) 




PA_full <- bind_rows(allegheny_full, phili_full)


#transform one object so crs match without dropping geometry 
philly_transform <- st_transform(phili_full, st_crs(allegheny_full)) 

PA_full <- bind_rows(allegheny_full, philly_transform) 

#then st_write 
#st_write(PA_full, "../DATA/full_datasets/PA_2024_shp.shp")



#----------------------------------Milwaukee-----------------------------------
# 55079 Milwaukee WI

#data never received

#----------------------------------Analysis-------------------------------------

#crs don't match, get the crs for everything
crs <- lapply(list(PA_full, NC_full, MI_full, GA_full, OH_full, AZ_full), st_crs)

harmonized_crs <- lapply(
  list(PA_full, NC_full, MI_full, GA_full, OH_full, AZ_full),
  function(obj) st_transform(st_zm(obj), crs = 4326)
) %>% 
  lapply(st_make_valid)

names(harmonized_crs) <- c("PA_full", "NC_full",  "MI_full", "GA_full", "OH_full", "AZ_full")

list2env(harmonized_crs, envir = .GlobalEnv)


full_24 <- rbind(PA_full, NC_full, MI_full, GA_full, OH_full, AZ_full)%>% 
  mutate(
    voter_tnt = if_else(voter_tnt >= 100 | voter_tnt == 0, NA_real_, voter_tnt),
    rej_pct = case_when(
      is.na(pv_rej) ~ NA_real_,
      is.na(ballots_cast) ~ NA_real_,
      TRUE ~ (pv_rej/ballots_cast)*100
    )
  ) %>%
  select(
    GEOID,
    county,
    state,
    CTY_ID,
    PREC_ID1,
    NAME,
    G24votes,
    G24PRERTRU,
    G24PREDHAR,
    voter_tnt,
    voter_tnt_cvap,
    prov,
    pv_rej,
    ballots_cast,
    rej_pct,
    rej_binned,
    maj_race,
    tot_cvap,
    white_pct,
    black_pct, 
    hisp_pct,
    api_pct,
    native_pct,
    voter_reg
  ) %>% 
  mutate(
    voter_tnt = (case_when(
      voter_tnt > 100 ~ 100,
      TRUE ~ voter_tnt)),
    maj_race = factor(
      case_when(
        maj_race == "White" ~ 1,
        maj_race == "Black" ~ 2,
        maj_race == "Hispanic" ~ 3,
        maj_race == "Asian/Pacific Islander" ~ 4,
        maj_race == "Native" ~ 5,
        maj_race == "Other" ~ 6
      ),
      levels = 1:6,
      labels = c(
        "White",
        "Black",
        "Hispanic",
        "Asian/Pacific Islander",
        "Native",
        "Plurality"
      )
    )
  ) %>% 
  drop_na(county)


##CVAP Standardization 

#add county-level cvap 
full_24 <- full_24 %>%
  mutate(county_cvap_tnt=case_when(
    county=="Mecklenburg" ~ 76.40, 
    county=="Durham" ~ 79.80, 
    county=="Columbus" ~ 61.60, 
    county=="Maricopa County" ~ 68.60, 
    county=="Fulton" ~ 69.70, 
    county=="Wayne" ~ 68.30, 
    county=="Cuyahoga" ~ 65.70, 
    county=="Lorain" ~ 66.20, 
    county=="Allegheny" ~ 75.30, 
    county=="Philadelphia" ~ 65.00, 
    TRUE ~ NA
  ))


#standardization process 
full_24_std <- full_24 %>%
  group_by(county) %>%
  mutate(
    precinct_cvap_mean=sum(voter_tnt_cvap, na.rm=TRUE)/n(), 
    scale_factor=county_cvap_tnt/precinct_cvap_mean, 
    precinct_cvap_std=voter_tnt_cvap * scale_factor) %>%
  ungroup() 

#Adjustment process 
full_24 <- full_24_std %>%
  mutate(#adjustment=pmax(10, 0.02 * tot_cvap), 
         cvap_adj= ifelse(precinct_cvap_std > 100, 
                          tot_cvap + 10, tot_cvap), 
         precinct_cvap_std_adj= 100 * G24votes/cvap_adj) %>% 
  select(-county_cvap_tnt, -scale_factor, -precinct_cvap_mean, cvap_adj) 


write_sf(full_24, "../DATA/full_datasets/full_2024_revised.shp", append = FALSE)

#split shapefile 
full_24 %>%
  group_by(state) %>%
  group_split() %>%
  walk(~st_write(., paste0("../DATA/full_datasets/", unique(.$state, ".shp")))) 


out_dir <- "/DATA/full_datasets/states"
dir.create(out_dir, showWarnings = FALSE) 

full_24 %>%
  split(state) %>%
  lapply(function(x) {
    st_write(
      x,
      file.path(out_dir, paste0("state_", unique(x$state), ".shp")),
      delete_layer = TRUE,
      quiet = TRUE
    )
  })


shapefile_path <- "../DATA/full_datasets/full_2024_revised.shp"

# Output directory
out_dir <- "/DATA/full_datasets/states"
dir.create(out_dir, showWarnings = FALSE, recursive = TRUE)

# Read shapefile
shp <- st_read(shapefile_path, quiet = TRUE)

# Optional: make geometries valid
shp <- st_make_valid(shp)

# Ensure 'state' column is character
shp$state <- as.character(shp$state)

# Split shapefile by state and write each state to separate shapefile
shp %>%
  split(.$state) %>%
  lapply(function(x) {
    out_path <- file.path(out_dir, paste0("state_", unique(x$state), ".shp"))
    st_write(x, out_path, delete_layer = TRUE, quiet = TRUE)
    message("Saved ", out_path)
  })


full_24_csv <- full_24 %>% st_drop_geometry()

write_csv(full_24_csv, "../DATA/full_datasets/full_2024_revised.csv")

split_states <- split(full_24, full_24$state)

list2env(split_states, envir = .GlobalEnv)

purrr::walk2(split_states,
             names(split_states),
             ~ write_sf(.x, paste0(.y, "_2024_revised.shp"), append = FALSE))

##-------------------------------models and tables----------------------------

full_24 <- read_csv("../DATA/full_datasets/full_2024_revised.csv") %>% 
  mutate(
    maj_race = factor(
      case_when(
        maj_race == "White" ~ 1,
        maj_race == "Black" ~ 2,
        maj_race == "Hispanic" ~ 3,
        maj_race == "Asian/Pacific Islander" ~ 4,
        maj_race == "Native" ~ 5,
        maj_race == "Plurality" ~ 6
      ),
      levels = 1:6,
      labels = c(
        "White",
        "Black",
        "Hispanic",
        "Asian/Pacific Islander",
        "Native",
        "Plurality"
      )
    )
  )

race_tnt_24 <- lm(voter_tnt ~ maj_race, data = full_24)

race_tnt_mixed_24 <- lmer(voter_tnt ~ maj_race + factor(state) + (1 | CTY_ID), data = full_24)

#calculate marginal and conditional r2 (not reported by lmer
r.squaredGLMM(race_tnt_mixed_24)


race_tnt_cvap_24 <- lm(precinct_cvap_std_adj ~ maj_race, data = full_24)


race_tnt_cvap_mixed_24 <- lmer(precinct_cvap_std_adj ~ maj_race + (1 | CTY_ID), data = full_24)


#calculate marginal and conditional r2 (not reported by lmer) 
r.squaredGLMM(race_tnt_cvap_mixed_24)


clean_rej_24 <- full_24 %>%
  filter(is.finite(rej_pct), !is.na(maj_race)) 

race_rej_24 <- lm(rej_pct ~ maj_race, data = clean_rej_24, na.action = na.omit)

race_rej_mixed_24 <- lmer(rej_pct ~ maj_race + (1 | CTY_ID), data = clean_rej_24)

r.squaredGLMM(race_rej_mixed_24)

modelsummary::modelsummary(
  list("Model 1" = race_tnt_24, 
       "Model 2" = race_tnt_mixed_24,
       "Model 1 (CVAP)" = race_tnt_cvap_24,
       "Model 2 (CVAP)" = race_tnt_cvap_24),
  output = "../Output/tnt_24_models_revised.html",
  coef_rename = c(
    "(Intercept)" = "Intercept",
    "maj_raceBlack" = "Majority Black",
    "maj_raceHispanic" = "Majority Hispanic",
    "maj_raceAsian/Pacific Islander" = "Majority API",
    "maj_raceNative" = "Majority Native American",
    "maj_racePlurality" = "Racial Plurality",
    "factor(state)GA" = "Georgia",
    "factor(state)NC" = "North Carolina",
    "factor(state)MI" = "Michigan",
    "factor(state)OH" = "Ohio",
    "factor(state)PA" = "Pennsylvania"
  ),
  stars = TRUE,  # Adds significance stars
  gof_map = c("r.squared", "adj.r.squared", "nobs"),  # Include R-squared and observations
  title = "Regression Models Predicting vaptnt",
  notes = "Significance levels: ***p<0.01, **p<0.05, *p<0.1"
)


modelsummary::modelsummary(
  list("OLS" = race_rej_24, "Mixed Effects" = race_rej_mixed_24),
  output = "../Output/rej_24_models_revised.html",
  coef_rename = c(
    "(Intercept)" = "Intercept",
    "maj_raceBlack" = "Majority Black",
    "maj_raceHispanic" = "Majority Hispanic",
    "maj_raceAsian/Pacific Islander" = "Majority API",
    "maj_raceNative" = "Majority Native American",
    "maj_racePlurality" = "Racial Plurality"
  ),
  stars = TRUE,  # Adds significance stars
  gof_map = c("r.squared", "adj.r.squared", "nobs"),  # Include R-squared and observations
  title = "Regression Models Predicting Ballot Rejections",
  notes = "Significance levels: ***p<0.01, **p<0.05, *p<0.1"
)

# 26163 Wayne MI

##----------------------------------plots--------------------------------------

race_tnt_plot <- full_24 %>% drop_na(precinct_cvap_std_adj, maj_race) %>% 
  filter(maj_race != "NA") %>% 
  ggplot(aes(x = maj_race, y = precinct_cvap_std_adj)) +
  geom_jitter(width = 0.2, alpha = 0.3, color = "blue") +  
  geom_boxplot(alpha = 0.5, outlier.shape = NA) +  
  theme_minimal() +
  labs(y = "% CVAP Turnout", 
       x = "Precinct Racial Majority") 



ggsave(race_tnt_plot, file = "../Output/racial_tnt_2024_final.jpg", width = 8, height = 6, units = "in")

race_binned <- clean_rej_24 %>%
  filter(!is.na(rej_binned)) %>% #there's not actually na values according to table call, but it is making them up if this is left out
  ggplot(aes(x = rej_binned, y = precinct_cvap_std_adj, color = rej_binned)) +
  
  geom_jitter(width = 0.2, alpha = 0.3) +
  geom_boxplot(alpha = 0.5, outlier.shape = NA) +
  theme_minimal() +
  labs(y = "% CVAP Turnout", 
       x = "Precinct Ballot Rejections",
       color="Rejection Category")+
  scale_color_manual(values = c("Lower 3rd" = "green",
                                "Middle 3rd" = "yellow",
                                "Upper 3rd" = "blue")) +
   theme_dark(base_size = 12)  
 

ggsave(race_binned, file = "../Output/tercile_scatter_24_final.jpg", width = 8, height = 6, units = "in")

race_bar <- clean_rej_24 %>%
  filter(maj_race != "NA") %>% 
  ggplot(aes(x = maj_race, fill = rej_binned)) +
  geom_bar(position = "fill") +
  labs(y = "Proportion of Precincts",
       x = "Precinct Racial Majority",
       fill = "Ballot Rejections") +
  scale_fill_manual(values = c("Lower 3rd" = "green",
                               "Middle 3rd" = "yellow",
                               "Upper 3rd" = "blue"))  +
  theme_dark(base_size = 12)+
  theme(
    axis.text.x = element_text(
      size = 8,
      angle = 45,        # rotate labels 45 degrees
      hjust = 1,         # right-justify
      vjust = 1          # vertically align
    )
  )



ggsave(race_bar, file = "../Output/stacked_bar_24_final.jpg", width = 8, height = 6, units = "in")

#---------------------------------tables v2-------------------------------

race_means <- full_24 %>% 
  group_by(maj_race) %>% 
  summarise(
    n = n(),
    mean_tnt = mean(voter_tnt, na.rm = TRUE),
    med_tnt = median(voter_tnt, na.rm = TRUE),
    n_na = sum(is.na(voter_tnt)),
    mean_tnt_cvap = mean(precinct_cvap_std_adj, na.rm = TRUE),
    med_tnt_cvap = median(precinct_cvap_std_adj, na.rm = TRUE),
    n_na_cvap = sum(is.na(precinct_cvap_std_adj)
  )%>% 
  st_drop_geometry()
  )

race_means_html <- knitr::kable(race_means, format = "html")

writeLines(race_means_html, "../Output/race_means_24_revised.html")

county_tnt_means <- full_24 %>% 
  group_by(county, state) %>% 
  summarise(
    n = n(),
    mean_tnt = mean(voter_tnt, na.rm = TRUE),
    med_tnt = median(voter_tnt, na.rm = TRUE),
    n_na = sum(is.na(voter_tnt)),
    mean_tnt_cvap = mean(precinct_cvap_std_adj, na.rm = TRUE),
    med_tnt_cvap = median(precinct_cvap_std_adj, na.rm = TRUE),
    n_na_cvap = sum(is.na(precinct_cvap_std_adj)),
    .groups = "drop"
  ) %>% 
  arrange(state, county) %>% 
  st_drop_geometry()
  

county_tnt_means_html <- knitr::kable(county_tnt_means, format = "html")

writeLines(county_tnt_means_html, "../Output/county_tnt_means_24_revised.html")

county_rej_means <- full_24 %>% 
  group_by(county, state) %>% 
  summarise(
    n = n(),
    mean_rej = mean(rej_pct, na.rm = TRUE),
    med_rej = median(rej_pct, na.rm = TRUE),
    n_na = sum(is.na(rej_pct)),
    .groups = "drop"
  ) %>% 
  arrange(desc(mean_rej)) %>% 
  st_drop_geometry()

county_rej_means_html <- knitr::kable(county_rej_means, format = "html")

writeLines(county_rej_means_html, "../Output/county_rej_means_24_revised.html")

