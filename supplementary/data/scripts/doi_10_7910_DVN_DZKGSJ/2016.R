##-----------------------------2016 Analysis---------------------------------##
##---------------------------- Authors: Rose Nafa and Liza Gordon-Rogers-----##
##-----------------------------Created:  9/30/2025---------------------------##
##-----------------------------Last Modified: 2/4/2026-----------------------##

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

#State-County FIPS codes
# 04013 Maricopa AZ
# 13121 Fulton GA
# 26163 Wayne MI
# 37063 Durham NC
# 37119 Mecklenberg NC
# 37047 Columbus NC
# 39035 Cuyahoga OH
# 39093 Lorain OH
# 42003 Allegheny PA
# 42101 Philadelphia PA
# 55079 Milwaukee WI


#---------------------------------Arizona---------------------------------------
# 04013 Maricopa AZ

#Arizona shapefile and election data (VEST)
az_shp <- st_read("../DATA/dataverse_files_2016_VDT_VEST/az_vest_16/az_vest_16.shp") %>%
  filter(CDE_COUNTY == "MC") %>% 
  mutate(CTY_ID = as.character(CDE_COUNTY),
         PREC_ID1 = as.character(PCTNUM),
         NAME = PRECINCTNA,
         G16PREO = G16PRELJOH + G16PREGSTE,
         G16votes = G16PRERTRU + G16PREDCLI + G16PREO) %>%
  select(CTY_ID,
         CDE_COUNTY,
         PREC_ID1,
         NAME,
         G16votes,
         G16PRERTRU,
         G16PREDCLI,
         G16PREO,
         geometry)

##Voter Registration Data 
maricopa <- read_tsv("../DATA/maricopa_results_2016.txt")

maricopa <- maricopa %>%
  distinct(PRECINCT_NAME, .keep_all=TRUE) %>%
  select(
    PRECINCT_NAME, 
    CONTEST_TOTAL
  ) %>%
  rename(voter_reg=CONTEST_TOTAL)


maricopa <- maricopa %>%
  separate(col=PRECINCT_NAME, 
           into=c("PREC_ID", "NAME"), 
           sep=" ") %>%
  mutate(PREC_ID= str_c("MC", as.character(PREC_ID)))

#join shp and registration data 
az_shp <- 
  left_join(az_shp, 
            maricopa, 
            by=c("PREC_ID1" = "PREC_ID")) %>%
  select( 
    -NAME.y 
  ) %>%
  rename(NAME=NAME.x) 

##Election Data (Dave's Redistricting Hub)
election_data_AZ <-
  read.csv(
    "../DATA/Election_Data/Election_Data_AZ.v05/election_data_AZ.v05.csv",
    stringsAsFactors = TRUE
  ) %>% 
  select(
    GEOID20, Name, E_16_PRES_Total, E_16_PRES_Dem, E_16_PRES_Rep)


#population increase from 2010 to 2016 by X amount. Therefore 2016 would be ((x/10)*6)+2010 numbers
demographic_data_AZ <-
  read.csv(
    "../DATA/Demographic_Data/Demographic_Data_AZ.v04/demographic_data_AZ.v04.csv",
    stringsAsFactors = TRUE
  ) %>% 
  transmute(
    GEOID20,
    Name,
    tot_pop = T_10_CENS_Total + ((T_20_CENS_Total - T_10_CENS_Total) / 10) * 6,
    white_pop = T_10_CENS_White + ((T_20_CENS_White - T_10_CENS_White) / 10) * 6,
    hisp_pop = T_10_CENS_Hispanic + ((T_20_CENS_Hispanic - T_10_CENS_Hispanic) / 10) * 6,
    black_pop = T_10_CENS_Black + ((T_20_CENS_Black - T_10_CENS_Black) / 10) * 6,
    asian_pop = T_10_CENS_Asian + ((T_20_CENS_Asian - T_10_CENS_Asian) / 10) * 6,
    native_pop = T_10_CENS_Native + ((T_20_CENS_Native - T_10_CENS_Native) / 10) * 6,
    pacific_pop = T_10_CENS_Pacific + ((T_20_CENS_Pacific - T_10_CENS_Pacific) / 10) * 6,
    tot_cvap = V_10_VAP_Total + ((V_20_VAP_Total - V_10_VAP_Total) / 10) * 6,
    white_cvap = V_10_VAP_White + ((V_20_VAP_White - V_10_VAP_White) / 10) * 6,
    hisp_cvap = V_10_VAP_Hispanic + ((V_20_VAP_Hispanic - V_10_VAP_Hispanic) / 10) * 6,
    black_cvap = V_10_VAP_Black + ((V_20_VAP_Black - V_10_VAP_Black) / 10) * 6,
    asian_cvap = V_10_VAP_Asian + ((V_20_VAP_Asian - V_10_VAP_Asian) / 10) * 6,
    native_cvap = V_10_VAP_Native + ((V_20_VAP_Native - V_10_VAP_Native) / 10) * 6,
    pacific_cvap = V_10_VAP_Pacific + ((V_20_VAP_Pacific - V_10_VAP_Pacific) / 10) * 6
  )


DRA_az <-
  full_join(
    election_data_AZ,
    demographic_data_AZ,
    by = c("GEOID20", "Name")
  ) 


VTDRA_az <- left_join(az_shp, DRA_az, by = c("NAME" = "Name")) %>% 
  mutate(
    GEOID = GEOID20,
    vaptnt=(G16votes/((tot_pop + tot_cvap)/2)* 100), #this uses VEST turnout rather than RDH 
    white_pct = trunc(white_cvap  / tot_cvap * 100),
    hisp_pct =  trunc(hisp_cvap  / tot_cvap  * 100),
    black_pct = trunc(black_cvap  / tot_cvap  * 100),
    api_pct = trunc((asian_cvap  + pacific_cvap ) / tot_cvap * 100),
    native_pct = trunc(native_cvap  / tot_cvap * 100),
    maj_race = case_when(
      white_pct > 49 ~ "White",
      black_pct > 49 ~ "Black",
      hisp_pct > 49 ~ "Hispanic",
      api_pct > 49 ~ "Asian/Pacific Islander",
      native_pct > 49 ~ "Native",
      TRUE ~ "Other"
    )
  )%>%
  select(
    GEOID,
    CTY_ID,
    PREC_ID1,
    NAME,
    G16PRERTRU,
    G16PREDCLI,
    G16PREO,
    G16votes,
    vaptnt,
    tot_pop,
    tot_cvap,
    white_pop,
    white_cvap,
    white_pct,
    hisp_pop,
    hisp_cvap,
    hisp_pct,
    black_pop,
    black_cvap,
    black_pct,
    asian_pop,
    asian_cvap,
    pacific_pop,
    pacific_cvap,
    api_pct,
    native_pop,
    native_cvap,
    native_pct,
    maj_race, 
    voter_reg
  ) 


#Rank rejections
az_VTDRA_full <-VTDRA_az %>%
  unique() %>% 
  filter(!is.na(geometry)) %>% 
  mutate(vaptnt = replace(vaptnt, vaptnt > 100, 100),
         state = "AZ") 


##-----------------------------------provisional votes------------------------------------------

# 04013 Maricopa AZ
az_pv_raw <- read_excel("../DATA/2016_20_pv_abs_data/AZ_Mar_2016_pv.xlsx")
az_abs_raw <- read_excel("../DATA/2016_20_pv_abs_data/AZ_Mar_2016_abs_cleaned.xlsx") 

az_disag <- read_delim("../DATA/2016_20_pv_abs_data/Maricopa_16_disag.txt") %>% 
  filter(CANDIDATE_FULL_NAME == "Early Voting Paper Turnout") %>% 
  separate(PRECINCT_NAME, into = c("Precinct", "Precinct_Name"), sep = " ", extra = "merge", remove = FALSE) %>% 
  transmute(
    precinct = str_pad(as.character(Precinct), width = 4, pad = "0"),
    precinct = paste0("MC", precinct),
    Precinct_Name,
    early_tot = TOTAL
  )

colnames(az_abs_raw) <- c("Precinct", "Precinct_Name", "Reason", "Number_Rejected")

az_abs_temp <- az_abs_raw %>% 
  fill(Precinct, Precinct_Name, .direction = "down") %>% 
  pivot_wider(names_from = "Reason", values_from = "Number_Rejected") %>% 
  transmute(
    precinct = as.numeric(Precinct),
    Precinct_Name,
    bad_sig = as.numeric(`BAD SIGNATURE`),
    ret_late = as.numeric(`RETURNED LATE`),
    no_sig = as.numeric(`NO SIGNATURE`)
  ) %>% 
  replace_na(list(bad_sig = 0, ret_late = 0, no_sig = 0)) %>% 
  mutate(precinct = str_pad(as.character(precinct), width = 4, pad = "0"),
         precinct = paste0("MC", precinct))

az_abs <- left_join(az_abs_temp, az_disag, by = c("precinct", "Precinct_Name"))

all_precincts <- tibble(precinct = 1:724) %>%
  mutate(
    precinct = str_pad(as.character(precinct), width = 4, pad = "0"),
    precinct = paste0("MC", precinct)
  ) 

missing_precincts <- all_precincts %>%
  anti_join(az_abs, by = "precinct")



az_pv_reasons <- az_pv_raw %>%
  filter(!is.na(BV10B)) %>%
  #identify which rows are which
  mutate(
    precinct_row = str_detect(BV10B, "^PRECINCT"),
    rejected_row = str_detect(BV10B, "^[0-9]+\\s"),
    precinct = as.numeric(if_else(precinct_row, str_extract(BV10B, "\\d{4}"), NA_character_))
  ) %>%
  fill(precinct, .direction = "down") %>%
  filter(rejected_row) %>%
  separate(BV10B, into = c("num_rejected", "rejected_reason"), sep = "\\s+", extra = "merge") %>%
  separate(rejected_reason, into = c("rej_code", "rej_reason"), sep = " ", extra = "merge") %>% 
  select(precinct, num_rejected, rej_code, rej_reason) %>%
  mutate(num_rejected = as.numeric(num_rejected),
         accepted = str_starts(rej_code, "A")) %>% 
  filter(rej_code != "MARICOPA" & !accepted)


az_pv <- az_pv_raw %>%
  filter(!is.na(BV10B)) %>%
  #identify which rows are which
  mutate(
    precinct_row = str_detect(BV10B, "^PRECINCT"), 
    precinct_row2 =str_detect(BV10B, "[0-9]{1,2}"),
    rejected_row = str_detect(BV10B, "^[0-9]+\\s"),
    precinct_either= precinct_row | precinct_row2, 
    precinct = as.numeric(if_else(precinct_either, str_extract(BV10B, "\\d{4}"), NA_character_))
  ) %>%
  fill(precinct, .direction = "down") %>%
  filter(rejected_row) %>%
  separate(BV10B, into = c("num_rejected", "rejected_reason"), sep = "\\s+", extra = "merge") %>%
  separate(rejected_reason, into = c("rej_code", "rej_reason"), sep = " ", extra = "merge") %>% 
  select(precinct, num_rejected, rej_code, rej_reason) %>%
  mutate(num_rejected = as.numeric(num_rejected),
         accepted = str_starts(rej_code, "A"),
         rejected = !accepted & rej_code != "MARICOPA") %>% 
  group_by(precinct) %>%
  summarise(
    prov = sum(num_rejected[accepted | rejected], na.rm = TRUE),
    pv_rej = sum(num_rejected[rejected], na.rm = TRUE)
  ) %>%
  mutate(rej_pct = (pv_rej/prov), #* 100,
         precinct = str_pad(as.character(precinct), width = 4, pad = "0"),
         precinct = paste0("MC", precinct))



az_reasons_raw <- left_join(az_pv, az_abs, by = "precinct") %>% 
  mutate(rej_denom = prov + early_tot, 
         early_rej= bad_sig + ret_late + no_sig) 



az_pv_temp <- az_reasons_raw %>%
  select(precinct, Precinct_Name, where(is.numeric))

all_az <- tibble(precinct = as.numeric(sprintf("%04d", 1:724))) %>% 
  mutate(
    precinct = str_pad(as.character(precinct), width = 4, pad = "0"),
    precinct = paste0("MC", precinct)
  ) 

missing_az_pv <- all_az %>%
  anti_join(az_pv_temp, by = "precinct") %>%
  mutate(total_rej = 0)

# Add missing precincts to az_pv_temp
az_pv_full16 <- az_pv_temp %>%
  bind_rows(missing_precincts) %>%
  arrange(precinct)%>% 
  drop_na(pv_rej) %>% 
  select(precinct, pv_rej, rej_denom, early_tot, early_rej, prov)

az_full <- az_VTDRA_full %>% 
  left_join(az_pv_full16, by = c("PREC_ID1" = "precinct")) %>%
  left_join(az_pv, by = c("PREC_ID1" = "precinct")) %>% 
  rename(pv_rej = pv_rej.x) %>% 
  select(-pv_rej.y)


az_full_temp <- az_full %>%
  mutate(voter_tnt = (G16votes/voter_reg) * 100,
         county = "Maricopa, AZ",
         ballots_cast = pv_rej + G16votes,
         rej_tot = coalesce(pv_rej, 0) + coalesce(early_rej, 0), 
         rej_pct = rej_tot/rej_denom * 100,
         rej_binned = factor(ntile(rej_pct, 3),
                             levels = 1:3,
                             labels = c("Lower 3rd",
                                        "Middle 3rd",
                                        "Upper 3rd"))) %>% 
  select(
    GEOID,
    county,
    state,
    CTY_ID,
    PREC_ID1,
    NAME,
    G16PRERTRU,
    G16PREDCLI,
    G16PREO,
    G16votes,
    voter_tnt,
    vaptnt,
    pv_rej,
    early_rej,
    early_tot, 
    rej_tot,
    rej_denom, 
    ballots_cast,
    rej_pct,
    rej_binned,
    maj_race,
    voter_reg,
    tot_cvap, 
  )


az_tnt <- az_full_temp


#----------------------------------Georgia-------------------------------------

# 13121 Fulton GA

#GA shapefile/VEST data 
ga_shp <-
  st_read("../DATA/dataverse_files_2016_VDT_VEST/ga_vest_16/ga_vest_16.shp") %>%
  filter(CTYNAME == "FULTON") %>%
  mutate(
    CTY_ID = as.character(CTYNUMBER),
    PREC_ID1 = as.character(PRECINCT_I),
    NAME = PRECINCT_N,
    G16PREO = G16PRELJOH,
    G16votes = G16PRERTRU + G16PREDCLI + G16PREO
  ) %>%
  select(CTY_ID,
         PREC_ID1,
         NAME,
         G16votes,
         G16PRERTRU,
         G16PREDCLI,
         G16PREO,
         geometry)

##RDH data 
election_data_GA <- read.csv("../DATA/Election_Data/Election_Data_GA.v04/election_data_GA.v04.csv", stringsAsFactors = TRUE) %>% 
  select(
    GEOID20, Name, E_16_PRES_Total, E_16_PRES_Dem, E_16_PRES_Rep
  )

demographic_data_GA <- read.csv("../DATA/Demographic_Data/Demographic_Data_GA.v04/demographic_data_GA.v04.csv", stringsAsFactors = TRUE) %>%
  transmute(
    GEOID20,
    Name,
    tot_pop = T_10_CENS_Total + ((T_20_CENS_Total - T_10_CENS_Total) / 10) * 6,
    white_pop = T_10_CENS_White + ((T_20_CENS_White - T_10_CENS_White) / 10) * 6,
    hisp_pop = T_10_CENS_Hispanic + ((T_20_CENS_Hispanic - T_10_CENS_Hispanic) / 10) * 6,
    black_pop = T_10_CENS_Black + ((T_20_CENS_Black - T_10_CENS_Black) / 10) * 6,
    asian_pop = T_10_CENS_Asian + ((T_20_CENS_Asian - T_10_CENS_Asian) / 10) * 6,
    native_pop = T_10_CENS_Native + ((T_20_CENS_Native - T_10_CENS_Native) / 10) * 6,
    pacific_pop = T_10_CENS_Pacific + ((T_20_CENS_Pacific - T_10_CENS_Pacific) / 10) * 6,
    tot_cvap = V_10_VAP_Total + ((V_20_VAP_Total - V_10_VAP_Total) / 10) * 6,
    white_cvap = V_10_VAP_White + ((V_20_VAP_White - V_10_VAP_White) / 10) * 6,
    hisp_cvap = V_10_VAP_Hispanic + ((V_20_VAP_Hispanic - V_10_VAP_Hispanic) / 10) * 6,
    black_cvap = V_10_VAP_Black + ((V_20_VAP_Black - V_10_VAP_Black) / 10) * 6,
    asian_cvap = V_10_VAP_Asian + ((V_20_VAP_Asian - V_10_VAP_Asian) / 10) * 6,
    native_cvap = V_10_VAP_Native + ((V_20_VAP_Native - V_10_VAP_Native) / 10) * 6,
    pacific_cvap = V_10_VAP_Pacific + ((V_20_VAP_Pacific - V_10_VAP_Pacific) / 10) * 6
  )

DRA_ga <- full_join(election_data_GA, demographic_data_GA, by = c("GEOID20", "Name")) %>%
  rename(NAME = Name)


VTDRA_ga <- left_join(ga_shp, DRA_ga, by = "NAME") %>% 
  mutate(
    GEOID = GEOID20,
    vaptnt = (G16votes / ((tot_pop  + tot_cvap ) / 2) * 100),
    white_pct = trunc(white_cvap  / tot_cvap * 100),
    hisp_pct =  trunc(hisp_cvap  / tot_cvap  * 100),
    black_pct = trunc(black_cvap  / tot_cvap  * 100),
    api_pct = trunc((asian_cvap  + pacific_cvap ) / tot_cvap * 100),
    native_pct = trunc(native_cvap  / tot_cvap * 100),
    maj_race = case_when(
      white_pct > 49 ~ "White",
      black_pct > 49 ~ "Black",
      hisp_pct > 49 ~ "Hispanic",
      api_pct > 49 ~ "Asian/Pacific Islander",
      native_pct > 49 ~ "Native",
      TRUE ~ "Other"
    )
  )%>%
  select(
    GEOID,
    CTY_ID,
    PREC_ID1,
    NAME,
    G16PRERTRU,
    G16PREDCLI,
    G16PREO,
    G16votes,
    vaptnt,
    tot_pop,
    tot_cvap,
    white_pop,
    white_cvap,
    white_pct,
    hisp_pop,
    hisp_cvap,
    hisp_pct,
    black_pop,
    black_cvap,
    black_pct,
    asian_pop,
    asian_cvap,
    pacific_pop,
    pacific_cvap,
    api_pct,
    native_pop,
    native_cvap,
    native_pct,
    maj_race
  )

ga_full <- VTDRA_ga %>%
  unique() %>% 
  filter(!is.na(geometry)) %>% 
  mutate(vaptnt = replace(vaptnt, vaptnt > 100, 100),
         state = "GA") 

##-----------------------------------pv------------------------------------------

# 13121 Fulton GA
ga_vf_raw <- read_csv("../DATA/2016_20_pv_abs_data/GA_FULTON_2016_vf.csv") 


stat_16 <- ga_vf_raw %>%
  group_by(`Ballot Status`) %>%
  summarise(cnt = n())


#side quest for reasons breakdown
ga_reasons_sums <- ga_vf_raw %>%
  group_by(`Status Reason`) %>% 
  summarise(total = n()) %>% 
  drop_na(`Status Reason`)%>% 
  mutate(percentage = round(total / sum(total)*100, 4)) %>% 
  arrange(desc(total))

ga_reasons_by_precinct <- ga_vf_raw %>%
  filter(!is.na(`Status Reason`)) %>%
  group_by(`County Precinct`, `Status Reason`) %>%
  summarise(total = n(), .groups = "drop")

ga_max_reason <- ga_reasons_by_precinct %>%
  group_by(`County Precinct`) %>%
  slice_max(order_by = total, n = 1, with_ties = FALSE) %>%
  ungroup() %>%
  select(`County Precinct`, max_reason = `Status Reason`)

ga_pvr_16 <- ga_vf_raw %>% 
  group_by(`County Precinct`, `Ballot Status`) %>% 
  summarise(
    n = n(),
    .groups = "drop"
  ) %>% 
  pivot_wider(names_from = `Ballot Status`, values_from = n) %>% 
  mutate(across(everything(), ~ replace_na(.x, 0))) %>% 
  transmute(
    PREC_ID = `County Precinct`,
    pv_rej = R, 
    pv = PROV
  ) %>%
  left_join(ga_max_reason, by = c("PREC_ID" = "County Precinct"))

ga_pv_full_16 <- left_join(ga_full, ga_pvr_16, by = c("PREC_ID1" = "PREC_ID")) %>% 
  replace_na(list(pv_rej = 0, pv = 0))

ga_tnt_temp <- read_csv("../DATA/Election_Data/fulton_tnt.csv") %>% 
  filter(`Election Name` == "General Election" & Race == "President of the United States" & `Election Date` == "2016 November 08") %>% 
  group_by(Precinct) %>% 
  summarize(voter_reg = mean(`Registered Voters`, na.rm = TRUE), 
            tot_votes = mean(`Ballots Cast`, na.rm = TRUE), 
            turnout_pct = mean(`Voter Turnout`, na.rm = TRUE), 
            abs_cast= sum(`Absentee by Mail`), 
            prov_cast=sum(`Provisional`), 
            rej_cast=sum(abs_cast + prov_cast)) %>% 
  mutate(PREC_ID1 = Precinct, 
         turnout_pct = case_when(
           turnout_pct > 100 ~ 100,
           TRUE ~ turnout_pct
         )
  )



ga_tnt <- left_join(ga_pv_full_16, ga_tnt_temp, by = "PREC_ID1") %>% 
  select(-pv, -Precinct) %>% 
  mutate(county = "Fulton, GA",
         ballots_cast=tot_votes, 
         rej_pct = (pv_rej/rej_cast)*100,
         rej_binned = factor(ntile(rej_pct, 3),
                             levels = 1:3,
                             labels = c("Lower 3rd",
                                        "Middle 3rd",
                                        "Upper 3rd")),
         vaptnt = (ballots_cast / ((tot_pop  + tot_cvap ) / 2) * 100),
         voter_tnt=(ballots_cast/voter_reg) * 100) %>%

  select(
    GEOID,
    county,
    state,
    CTY_ID,
    PREC_ID1,
    NAME,
    G16PRERTRU,
    G16PREDCLI,
    G16PREO,
    G16votes,
    ballots_cast, 
    voter_tnt,
    pv_rej,
    ballots_cast,
    rej_pct,
    rej_binned,
    maj_race,
    voter_reg,
    tot_cvap, 
    vaptnt
  )
#--------------------------------Michigan------------------------------------------------

# 26163 Wayne MI

#MI shapefile/VEST data 
mi_shp_temp <- st_read("../DATA/dataverse_files_2016_VDT_VEST/mi_vest_16/mi_vest_16.shp")


mi_shp_temp2 <- st_read("../DATA/dataverse_files_2016_VDT_VEST/mi_vest_16/mi_vest_16.shp")
#Michigan 2016 shape file has all precincts but no counties, load tidyverse shape file 
#of Wayne county and spatial merge in to add missing columns. 

counties <- st_read("../DATA/2016_prec_shapes/tl_2016_us_county/tl_2016_us_county.shp") %>% 
  st_transform(crs = st_crs(mi_shp_temp))

counties <- st_make_valid(counties)

mi_shp_temp <- st_make_valid(mi_shp_temp)

mi_merge <- st_join(mi_shp_temp, counties, join = st_intersects) 


mi_shp <- mi_merge %>%
  filter(COUNTYFP == "163") %>%
  mutate(
    CTY_ID = as.character(COUNTYFP),
    PREC_ID1 = str_trim(VTD2016),
    GEOID = VTD2016,
    NAME = Label,
    G16PREO = G16PREGSte + G16PRELJoh + G16PREOth,
    G16votes = G16PRERTru + G16PREDCli + G16PREO
  ) %>%
  select(CTY_ID,
         PREC_ID1,
         NAME,
         G16votes,
         G16PRERTru,
         G16PREDCli,
         G16PREO,
         geometry) %>% 
  rename(
    G16PRERTRU = G16PRERTru,
    G16PREDCLI = G16PREDCli
  )

#MI RDH data 
election_data_MI <- read.csv("../DATA/Election_Data/Election_Data_MI.v05/election_data_MI.v05.csv", stringsAsFactors = TRUE) %>% 
  select(
    GEOID20, Name, E_16_PRES_Total, E_16_PRES_Dem, E_16_PRES_Rep
  )

demographic_data_MI <- read.csv("../DATA/Demographic_Data/Demographic_Data_MI.v04/demographic_data_MI.v04.csv", stringsAsFactors = TRUE) %>%
  transmute(
    GEOID20,
    Name,
    tot_pop = T_10_CENS_Total + ((T_20_CENS_Total - T_10_CENS_Total) / 10) * 6,
    white_pop = T_10_CENS_White + ((T_20_CENS_White - T_10_CENS_White) / 10) * 6,
    hisp_pop = T_10_CENS_Hispanic + ((T_20_CENS_Hispanic - T_10_CENS_Hispanic) / 10) * 6,
    black_pop = T_10_CENS_Black + ((T_20_CENS_Black - T_10_CENS_Black) / 10) * 6,
    asian_pop = T_10_CENS_Asian + ((T_20_CENS_Asian - T_10_CENS_Asian) / 10) * 6,
    native_pop = T_10_CENS_Native + ((T_20_CENS_Native - T_10_CENS_Native) / 10) * 6,
    pacific_pop = T_10_CENS_Pacific + ((T_20_CENS_Pacific - T_10_CENS_Pacific) / 10) * 6,
    tot_cvap = V_10_VAP_Total + ((V_20_VAP_Total - V_10_VAP_Total) / 10) * 6,
    white_cvap = V_10_VAP_White + ((V_20_VAP_White - V_10_VAP_White) / 10) * 6,
    hisp_cvap = V_10_VAP_Hispanic + ((V_20_VAP_Hispanic - V_10_VAP_Hispanic) / 10) * 6,
    black_cvap = V_10_VAP_Black + ((V_20_VAP_Black - V_10_VAP_Black) / 10) * 6,
    asian_cvap = V_10_VAP_Asian + ((V_20_VAP_Asian - V_10_VAP_Asian) / 10) * 6,
    native_cvap = V_10_VAP_Native + ((V_20_VAP_Native - V_10_VAP_Native) / 10) * 6,
    pacific_cvap = V_10_VAP_Pacific + ((V_20_VAP_Pacific - V_10_VAP_Pacific) / 10) * 6
  )

DRA_mi <- full_join(election_data_MI, demographic_data_MI, by = c("GEOID20", "Name")) %>%
  mutate(NAME = as.character(str_trim(Name)))

VTDRA_mi <- left_join(mi_shp, DRA_mi, by = c("PREC_ID1" = "NAME")) %>% 
  mutate(
    GEOID = GEOID20,
    vaptnt = (G16votes / ((tot_pop  + tot_cvap ) / 2) * 100),
    white_pct = trunc(white_cvap  / tot_cvap * 100),
    hisp_pct =  trunc(hisp_cvap  / tot_cvap  * 100),
    black_pct = trunc(black_cvap  / tot_cvap  * 100),
    api_pct = trunc((asian_cvap  + pacific_cvap ) / tot_cvap * 100),
    native_pct = trunc(native_cvap  / tot_cvap * 100),
    maj_race = case_when(
      white_pct > 49 ~ "White",
      black_pct > 49 ~ "Black",
      hisp_pct > 49 ~ "Hispanic",
      api_pct > 49 ~ "Asian/Pacific Islander",
      native_pct > 49 ~ "Native",
      TRUE ~ "Other"
    ),
    CTY_ID = as.character(CTY_ID)
  )%>%
  select(
    GEOID,
    CTY_ID,
    PREC_ID1,
    NAME,
    G16PRERTRU,
    G16PREDCLI,
    G16PREO,
    G16votes,
    vaptnt,
    tot_pop,
    tot_cvap,
    white_pop,
    white_cvap,
    white_pct,
    hisp_pop,
    hisp_cvap,
    hisp_pct,
    black_pop,
    black_cvap,
    black_pct,
    asian_pop,
    asian_cvap,
    pacific_pop,
    pacific_cvap,
    api_pct,
    native_pop,
    native_cvap,
    native_pct,
    maj_race
  )

VTDRA_sf <- st_as_sf(VTDRA_mi)

mi_full <- VTDRA_sf %>%
  unique() %>% 
  filter(!is.na(geometry)) %>% 
  mutate(vaptnt = replace(vaptnt, vaptnt > 100, 100),
         state = "MI",
         county = "Wayne, MI") 


##----------------------------------pv--------------------------------------------
# 26163 Wayne MI
mi_pv <- read_excel("../DATA/2016_20_pv_abs_data/MI_2016_pv.xlsx") %>% 
  unite("JURISDICTION NAME", "PRECINCT NAME", col = "precinct", sep = " ")


mi_pv_16 <- mi_pv %>%  
transmute(PREC_ID = precinct,
            pv_rej = as.numeric(`C) Envelope Ballots (Rejected / Not Counted)`),
            prov= `Total Provisional Ballots Issued on Election Day`, 
            precinct_words = str_extract(precinct, "^[A-Za-z ]+"),
            precinct_numbers = str_pad(str_extract(precinct, "\\d+$"), width = 3, pad = "0")) %>% 
  select(-PREC_ID)

mi_tnt <- mi_full %>% 
  mutate(
    precinct_numbers = str_sub(PREC_ID1, -3),
    precinct_words = case_when(
      str_detect(NAME, "of ") ~ str_extract(NAME, "(?<=of ).*"),  # Extract after "of "
      str_detect(NAME, "Township") ~ str_remove(NAME, " Township"),  # Remove " Township"
      TRUE ~ NA_character_  # Default case, if no match
    )
  ) %>% 
  mutate(
    precinct_numbers = case_when(precinct_words == "Inkster" ~ str_sub(PREC_ID1, -4), 
                                 TRUE ~ precinct_numbers)
  )

#confirm no trailing whitespace
mi_tnt$precinct_numbers <- str_trim(mi_tnt$precinct_numbers)
mi_tnt$precinct_words <- str_trim(mi_tnt$precinct_words)
mi_pv_16$precinct_numbers <- str_trim(mi_pv_16$precinct_numbers)
mi_pv_16$precinct_words <- str_trim(mi_pv_16$precinct_words)


MI_full_16 <- read_csv("../Data/full_datasets/full_16.csv") %>% 
  filter(CTY_ID == 163)
# 

#---------------------------------- North Carolina-----------------------------------------

# 37063 Durham NC
# 37119 Mecklenberg NC
# 37047 Columbus NC

#NC shapefile/VEST data
nc_shp <- st_read("../DATA/dataverse_files_2016_VDT_VEST/nc_vest_16/nc_vest_16.shp") %>%
  filter(COUNTY_NAM %in% c("DURHAM", "MECKLENBURG", "COLUMBUS")) %>%
  mutate(
    NAME = str_remove(ENR_DESC, "^[^_]*_"),
    CTY_ID = as.character(COUNTY_ID),
    PREC_ID1 = PREC_ID,
    G16PREO = G16PRELJOH + G16PREOWRI,
    G16votes = G16PRERTRU + G16PREDCLI + G16PREO
  ) %>%
  select(CTY_ID, PREC_ID1, NAME, G16votes, G16PRERTRU, G16PREDCLI, G16PREO, geometry)

#NC RDH data 
election_data_nc <- read.csv("../DATA/Election_Data/Election_Data_nc.v05/election_data_nc.v05.csv", stringsAsFactors = TRUE) %>% 
  select(
    GEOID20, Name, E_16_PRES_Total, E_16_PRES_Dem, E_16_PRES_Rep
  )

demographic_data_nc <- read.csv("../DATA/Demographic_Data/Demographic_Data_nc.v04/demographic_data_nc.v04.csv", stringsAsFactors = TRUE) %>%
  transmute(
    GEOID20,
    Name,
    tot_pop = T_10_CENS_Total + ((T_20_CENS_Total - T_10_CENS_Total) / 10) * 6,
    white_pop = T_10_CENS_White + ((T_20_CENS_White - T_10_CENS_White) / 10) * 6,
    hisp_pop = T_10_CENS_Hispanic + ((T_20_CENS_Hispanic - T_10_CENS_Hispanic) / 10) * 6,
    black_pop = T_10_CENS_Black + ((T_20_CENS_Black - T_10_CENS_Black) / 10) * 6,
    asian_pop = T_10_CENS_Asian + ((T_20_CENS_Asian - T_10_CENS_Asian) / 10) * 6,
    native_pop = T_10_CENS_Native + ((T_20_CENS_Native - T_10_CENS_Native) / 10) * 6,
    pacific_pop = T_10_CENS_Pacific + ((T_20_CENS_Pacific - T_10_CENS_Pacific) / 10) * 6,
    tot_cvap = V_10_VAP_Total + ((V_20_VAP_Total - V_10_VAP_Total) / 10) * 6,
    white_cvap = V_10_VAP_White + ((V_20_VAP_White - V_10_VAP_White) / 10) * 6,
    hisp_cvap = V_10_VAP_Hispanic + ((V_20_VAP_Hispanic - V_10_VAP_Hispanic) / 10) * 6,
    black_cvap = V_10_VAP_Black + ((V_20_VAP_Black - V_10_VAP_Black) / 10) * 6,
    asian_cvap = V_10_VAP_Asian + ((V_20_VAP_Asian - V_10_VAP_Asian) / 10) * 6,
    native_cvap = V_10_VAP_Native + ((V_20_VAP_Native - V_10_VAP_Native) / 10) * 6,
    pacific_cvap = V_10_VAP_Pacific + ((V_20_VAP_Pacific - V_10_VAP_Pacific) / 10) * 6
  )

DRA_nc <- full_join(election_data_nc, demographic_data_nc, by = c("GEOID20", "Name")) %>%
  rename(NAME = Name)

VTDRA_nc <- left_join(nc_shp, DRA_nc, by = "NAME") %>% 
  mutate(
    GEOID = GEOID20,
    vaptnt=(G16votes/((tot_pop + tot_cvap)/2) * 100), #this is VEST instead of RDH 
    white_pct = trunc(white_cvap  / tot_cvap * 100),
    hisp_pct =  trunc(hisp_cvap  / tot_cvap  * 100),
    black_pct = trunc(black_cvap  / tot_cvap  * 100),
    api_pct = trunc((asian_cvap  + pacific_cvap ) / tot_cvap * 100),
    native_pct = trunc(native_cvap  / tot_cvap * 100),
    maj_race = case_when(
      white_pct > 49 ~ "White",
      black_pct > 49 ~ "Black",
      hisp_pct > 49 ~ "Hispanic",
      api_pct > 49 ~ "Asian/Pacific Islander",
      native_pct > 49 ~ "Native",
      TRUE ~ "Other"
    )
  )%>%
  select(
    GEOID,
    CTY_ID,
    PREC_ID1,
    NAME,
    G16PRERTRU,
    G16PREDCLI,
    G16PREO,
    G16votes,
    vaptnt,
    tot_pop,
    tot_cvap,
    white_pop,
    white_cvap,
    white_pct,
    hisp_pop,
    hisp_cvap,
    hisp_pct,
    black_pop,
    black_cvap,
    black_pct,
    asian_pop,
    asian_cvap,
    pacific_pop,
    pacific_cvap,
    api_pct,
    native_pop,
    native_cvap,
    native_pct,
    maj_race
  )


nc_full <- VTDRA_nc %>%
  unique() %>% 
  filter(!is.na(geometry)) %>% 
  mutate(vaptnt = replace(vaptnt, vaptnt > 100, 100),
         state = "NC") 


##-------------------------------provisional vote--------------------------------------##

#Start w/ pv, from voter files
NC_2016_prov_vf_dcm <-
  read.csv(
    "../DATA/2016_20_pv_abs_data/NC_2016_prov_vf.csv"
  ) 

nc_vf_16 <- read_delim("../DATA/2016_20_pv_abs_data/nc_vf_16.txt") %>% 
  filter(county_desc %in% c("MECKLENBURG", "COLUMBUS", "DURHAM")) %>% 
  group_by(county_desc, precinct_abbrv, voting_method_desc) %>% 
  summarise(votes = n()) %>% 
  pivot_wider(names_from = voting_method_desc, values_from = votes) %>% 
  mutate(
    across(everything(), ~replace_na(., 0)),
    abs = `ABSENTEE BY MAIL` + `ABSENTEE CURBSIDE` + `ABSENTEE ONESTOP`,
    prov = PROVISIONAL,
    rej_denom = abs + prov
  )

#PV reasons for rejection
nc_reason_sums <- NC_2016_prov_vf_dcm %>% 
  filter(pv_status == "NOT COUNTED" & county_name %in% c("MECKLENBURG", "COLUMBUS", "DURHAM")) %>% 
  group_by(county_name, pv_voted_reason) %>% 
  summarise(total = n())%>% 
  mutate(percentage = round(total / sum(total)*100, 2)) %>% 
  arrange(desc(total), .by_group = TRUE)

nc_max_reason_df <- NC_2016_prov_vf_dcm %>%
  filter(county_name %in% c("MECKLENBURG", "COLUMBUS", "DURHAM")) %>%
  filter(!is.na(not_counted_reason)) %>%
  group_by(voted_pct_lbl, not_counted_reason) %>%
  summarise(n = n(), .groups = "drop") %>%
  group_by(voted_pct_lbl) %>%
  slice_max(n, n = 1, with_ties = FALSE) %>%
  ungroup() %>% 
  transmute(
    PREC_ID1 = voted_pct_lbl,
    max_reason = case_when(
      not_counted_reason == "INELIGIBLE TO VOTE" ~ "INELIGIBLE TO VOTE",
      not_counted_reason == "MOVED OUT OF COUNTY MORE THAN 30 DAYS" ~ "MOVED OUT OF COUNTY MORE THAN 30 DAYS",
      not_counted_reason == "NOT REGISTERED" ~ "NOT REGISTERED",
      not_counted_reason == "REGISTRATION AFTER DEADLINE" ~ "REGISTRATION AFTER DEADLINE",
      not_counted_reason == "REMOVED" ~ "REMOVED",
      not_counted_reason == "VOTING OUT OF PRECINCT" ~ "VOTING OUT OF PRECINCT",
      TRUE ~ "Blank"
    )
  )
#####
nc_pv_16 <- NC_2016_prov_vf_dcm %>% 
  filter(county_name %in% c("MECKLENBURG", "COLUMBUS", "DURHAM")) %>% 
  group_by(voted_pct_lbl, pv_status) %>% 
  summarise(n = n(), .groups = "drop") %>% 
  pivot_wider(names_from = pv_status, values_from = n) %>% 
  mutate(across(everything(), ~ replace_na(.x, 0))) %>%
  transmute(
    PREC_ID1 = voted_pct_lbl, 
    pv_rej = `NOT COUNTED`
  ) %>% 
  left_join(nc_vf_16, by = c("PREC_ID1" = "precinct_abbrv")) %>% 
  mutate(
    rej_pct = pv_rej/rej_denom *100
  )

nc_pv_full_16 <- left_join(nc_full, nc_pv_16, by = c("PREC_ID1"))


columbus_tnt_temp <- read.delim("../DATA/Election_Data/columbus_tnt.txt") %>%
  filter(voter_status_desc %in% c("ACTIVE", "INACTIVE", "TEMPORARY")) %>%
  group_by(precinct_desc, precinct_abbrv) %>%
  summarize(
    voter_reg = n()
  ) %>%
  transmute(
    precinct_desc,
    PREC_ID1 = as.character(precinct_abbrv),
    voter_reg,
    county = "COLUMBUS",
    CTY_ID = "24"
  ) 

durham_tnt_temp <- read.delim("../DATA/Election_Data/durham_tnt.txt") %>%
  filter(voter_status_desc %in% c("ACTIVE", "INACTIVE", "TEMPORARY")) %>%
  group_by(precinct_desc, precinct_abbrv) %>%
  summarize(
    voter_reg = n()
  )%>%
  transmute(
    precinct_desc,
    PREC_ID1 = as.character(precinct_abbrv),
    voter_reg,
    county = "DURHAM",
    CTY_ID = "32"
  )

meck_tnt_temp <- read.delim("../DATA/Election_Data/meck_tnt.txt") %>%
  filter(voter_status_desc %in% c("ACTIVE", "INACTIVE", "TEMPORARY")) %>%
  group_by(precinct_desc, precinct_abbrv) %>%
  summarize(
    voter_reg = n()
  )%>%
  transmute(
    precinct_desc,
    PREC_ID1 = as.character(str_pad(as.character(precinct_abbrv), width = 3, pad = 0)),
    voter_reg,
    county = "MECKLENBURG",
    CTY_ID = "60"
  )

nc_tnt_temp <- bind_rows(columbus_tnt_temp, durham_tnt_temp, meck_tnt_temp)

nc_tnt <- left_join(nc_pv_full_16, nc_tnt_temp, by = c("CTY_ID", "PREC_ID1")) %>% 
mutate(
    voter_tnt = (G16votes/voter_reg) * 100,
    ballots_cast = pv_rej + G16votes,
    rej_pct = (pv_rej/rej_denom)*100,
    rej_binned = factor(ntile(rej_pct, 3),
                        levels = 1:3,
                        labels = c("Lower 3rd",
                                   "Middle 3rd",
                                   "Upper 3rd"))
  ) %>% 
  select(
    GEOID,
    county,
    state,
    CTY_ID,
    PREC_ID1,
    NAME,
    G16PRERTRU,
    G16PREDCLI,
    G16PREO,
    G16votes,
    voter_tnt,
    vaptnt, 
    pv_rej,
    rej_denom,
    ballots_cast,
    rej_pct,
    rej_binned,
    maj_race,
    voter_reg,
    tot_cvap
  )



columbus_tnt_temp2 <- nc_tnt %>% 
  filter(county == "COLUMBUS")%>% 
  mutate(ballots_cast = pv_rej + G16votes,
         rej_pct = (pv_rej/ballots_cast)*100)

durham_tnt_temp2 <- nc_tnt %>% 
  filter(county == "DURHAM")%>% 
  mutate(ballots_cast = pv_rej + G16votes,
         rej_pct = (pv_rej/ballots_cast)*100) 

meck_tnt_temp2 <- nc_tnt %>% 
  filter(county == "MECKLENBURG")%>% 
  mutate(ballots_cast = pv_rej + G16votes,
         rej_pct = (pv_rej/ballots_cast)*100) 


#------------------------------------------Ohio---------------------------------------

# 39035 Cuyahoga OH
# 39093 Lorain OH

#Shapefile/VEST data 
oh_shp <- st_read("../DATA/dataverse_files_2016_VDT_VEST/oh_vest_16/oh_vest_16.shp") %>%
  filter(COUNTYFP16 %in% c("035", "093")) %>%
  mutate(
    CTY_ID = as.character(COUNTYFP16),
    PREC_ID1 = PRECINCT16,
    NAME = NAME16,
    G16PREO = G16PRELJOH + G16PREGSTE + G16PREIDUN,
    G16votes = G16PRERTRU + G16PREDCLI + G16PREO
  ) %>%
  select(CTY_ID, PREC_ID1, NAME, G16votes, G16PRERTRU, G16PREDCLI, G16PREO, geometry)

#RDH data
election_data_oh <- read.csv("../DATA/Election_Data/Election_Data_oh.v04/election_data_oh.v04.csv", stringsAsFactors = TRUE) %>% 
  select(
    GEOID20, Name, E_16_PRES_Total, E_16_PRES_Dem, E_16_PRES_Rep
  )

demographic_data_oh <- read.csv("../DATA/Demographic_Data/Demographic_Data_oh.v04/demographic_data_oh.v04.csv", stringsAsFactors = TRUE) %>%
  transmute(
    GEOID20,
    Name,
    tot_pop = T_10_CENS_Total + ((T_20_CENS_Total - T_10_CENS_Total) / 10) * 6,
    white_pop = T_10_CENS_White + ((T_20_CENS_White - T_10_CENS_White) / 10) * 6,
    hisp_pop = T_10_CENS_Hispanic + ((T_20_CENS_Hispanic - T_10_CENS_Hispanic) / 10) * 6,
    black_pop = T_10_CENS_Black + ((T_20_CENS_Black - T_10_CENS_Black) / 10) * 6,
    asian_pop = T_10_CENS_Asian + ((T_20_CENS_Asian - T_10_CENS_Asian) / 10) * 6,
    native_pop = T_10_CENS_Native + ((T_20_CENS_Native - T_10_CENS_Native) / 10) * 6,
    pacific_pop = T_10_CENS_Pacific + ((T_20_CENS_Pacific - T_10_CENS_Pacific) / 10) * 6,
    tot_cvap = V_10_VAP_Total + ((V_20_VAP_Total - V_10_VAP_Total) / 10) * 6,
    white_cvap = V_10_VAP_White + ((V_20_VAP_White - V_10_VAP_White) / 10) * 6,
    hisp_cvap = V_10_VAP_Hispanic + ((V_20_VAP_Hispanic - V_10_VAP_Hispanic) / 10) * 6,
    black_cvap = V_10_VAP_Black + ((V_20_VAP_Black - V_10_VAP_Black) / 10) * 6,
    asian_cvap = V_10_VAP_Asian + ((V_20_VAP_Asian - V_10_VAP_Asian) / 10) * 6,
    native_cvap = V_10_VAP_Native + ((V_20_VAP_Native - V_10_VAP_Native) / 10) * 6,
    pacific_cvap = V_10_VAP_Pacific + ((V_20_VAP_Pacific - V_10_VAP_Pacific) / 10) * 6
  )

DRA_oh <- full_join(election_data_oh, demographic_data_oh, by = c("GEOID20", "Name")) %>%
  rename(NAME = Name) 

VTDRA_oh <- left_join(oh_shp, DRA_oh, by = "NAME") %>% 
  mutate(
    GEOID = GEOID20,
    vaptnt = (G16votes / ((tot_pop  + tot_cvap ) / 2) * 100),
    white_pct = trunc(white_cvap  / tot_cvap * 100),
    hisp_pct =  trunc(hisp_cvap  / tot_cvap  * 100),
    black_pct = trunc(black_cvap  / tot_cvap  * 100),
    api_pct = trunc((asian_cvap  + pacific_cvap ) / tot_cvap * 100),
    native_pct = trunc(native_cvap  / tot_cvap * 100),
    maj_race = case_when(
      white_pct > 49 ~ "White",
      black_pct > 49 ~ "Black",
      hisp_pct > 49 ~ "Hispanic",
      api_pct > 49 ~ "Asian/Pacific Islander",
      native_pct > 49 ~ "Native",
      TRUE ~ "Other"
    )
  )%>%
  select(
    GEOID,
    CTY_ID,
    PREC_ID1,
    NAME,
    G16PRERTRU,
    G16PREDCLI,
    G16PREO,
    G16votes,
    vaptnt,
    tot_pop,
    tot_cvap,
    white_pop,
    white_cvap,
    white_pct,
    hisp_pop,
    hisp_cvap,
    hisp_pct,
    black_pop,
    black_cvap,
    black_pct,
    asian_pop,
    asian_cvap,
    pacific_pop,
    pacific_cvap,
    api_pct,
    native_pop,
    native_cvap,
    native_pct,
    maj_race
  )

oh_full <- VTDRA_oh %>%
  unique() %>% 
  filter(!is.na(geometry)) %>% 
  mutate(vaptnt = replace(vaptnt, vaptnt > 100, 100),
         state = "OH") 

lorain <- oh_full %>% filter(CTY_ID == "093")

lorain_tnt_temp1 <- read.delim("../DATA/Election_Data/lorain_tnt.txt",
                               sep = "", 
                               header = FALSE, 
                               stringsAsFactors = FALSE,
                               skip = 1)

colnames(lorain_tnt_temp1) <- as.character(lorain_tnt_temp1[1, ])  

lorain_tnt_temp2 <- lorain_tnt_temp1[-1, ]

lorain_tnt_temp3 <- lorain_tnt_temp2 %>% 
  transmute(
    V1 = REPORTING,
    V2 = DISTRICT,
    V3 = Reg.,
    V4 = Voters,
    V5 = Turnout,
    v6 = Percent,
    just_numbers = str_detect(V3, "^[0-9]+$"),
    letters_and_numbers = str_detect(V3, "^(?=.*[A-Za-z])(?=.*[0-9])"),
    reporting_district = case_when(
      letters_and_numbers ~ paste(V1, V2, V3, sep = " "),  # Letters & numbers in V3
      TRUE ~ paste(V1, V2, sep = " ")  # Default: just V1 and V2
    ),
    blank = !just_numbers & !letters_and_numbers,
    city = ifelse(blank, reporting_district, NA)
  ) %>% 
  fill(city, .direction = "down") %>% 
  filter(V1 != "Total" & blank != "TRUE") 

lorain_cut_1 <- lorain_tnt_temp3 %>% 
  filter(just_numbers) %>% 
  transmute(
    city,
    reporting_district,
    voter_reg = as.numeric(V3),
    voter_tnt_raw = as.numeric(V4),
    tnt_pct = as.character(V5)
  )

lorain_cut_2 <- lorain_tnt_temp3 %>% 
  filter(letters_and_numbers) %>% 
  transmute(
    city,
    reporting_district,
    voter_reg = as.numeric(V4),
    voter_tnt_raw = as.numeric(V5),
    tnt_pct = as.character(v6)
  )

lorain_tnt_temp <- bind_rows(lorain_cut_1, lorain_cut_2) %>% 
  mutate(
    NAME = reporting_district %>% 
      str_replace("\\bAMHERST\\b", "AMHERST CITY") %>% 
      str_replace("\\bAVON LAKE\\b", "AVON LAKE CITY") %>% 
      str_replace("\\bCARLISLE\\b", "CARLISLE TWP") %>% 
      str_replace("\\bCOLUMBIA\\b", "COLUMBIA TWP") %>% 
      str_replace("\\bEATON\\b", "EATON TWP") %>% 
      str_replace("\\bN RIDGEVILLE\\b", "N. RIDGEVILLE") %>% 
      str_replace("\\bSHEFFIELD LAKE\\b", "SHEF. LAKE CITY") %>% 
      str_replace("\\bVERMILION\\b", "VERMILION CITY"),
    NAME = case_when(
      NAME == "OBERLIN 1" ~ "OBERLIN CITY #1",
      NAME == "OBERLIN 2" ~ "OBERLIN CITY #2",
      NAME == "OBERLIN 3" ~ "OBERLIN CITY #3",
      NAME == "OBERLIN 4" ~ "OBERLIN CITY #4",
      NAME == "OBERLIN 5" ~ "OBERLIN CITY #5",
      NAME == "OBERLIN 6" ~ "OBERLIN CITY #6",
      NAME == "OBERLIN 7" ~ "OBERLIN CITY #7",
      NAME == "OBERLIN 8" ~ "OBERLIN CITY #8",
      NAME == "LAGRANGE VIL" ~ "LAGRANGE VILL",
      TRUE ~ NAME
    )
  ) %>% 
  filter(NAME != "VOTING DISTRICTS NOT DEFINED")


lorain_tnt <- left_join(lorain, lorain_tnt_temp, by = "NAME") %>% 
  mutate(voter_reg = case_when(
    NAME == "AMHERST TWP #1" ~ 1038,
    NAME == "AMHERST TWP #2" ~ 1085,
    NAME == "AMHERST TWP #3" ~ 1023,
    NAME == "AMHERST TWP #4" ~ 814,
    NAME == "CAMDEN TWP/KIPTON VILL" ~ 1105,
    NAME == "ELYRIA TWP #1" ~ 1106,
    NAME == "ELYRIA TWP #2" ~ 1146,
    NAME == "GRAFTON TWP #1" ~ 1076,
    NAME == "GRAFTON TWP #2" ~ 978,
    NAME == "GRAFTON VILL #1/#2" ~ 786,
    NAME == "GRAFTON VILL #3/#4" ~ 957,
    NAME == "LAGRANGE TWP #1" ~ 1286,
    NAME == "LAGRANGE TWP #2" ~ 1446,
    NAME == "NEW RUSSIA TWP" ~ 1354,
    NAME == "ROCHESTER TWP & VILL" ~ 544,
    NAME == "S AMHERST VILL" ~ 1059,
    NAME == "SHEFFIELD TWP EAST" ~ 675,
    NAME == "SHEFFIELD TWP NORTH" ~ 727,
    NAME == "SHEFFIELD TWP WEST" ~ 897,
    NAME == "SHEFFIELD VILL #1" ~ 837,
    NAME == "SHEFFIELD VILL #2" ~ 908,
    NAME == "SHEFFIELD VILL #3" ~ 1012,
    NAME == "WELLINGTON VILL NORTH" ~ 1394,
    NAME == "WELLINGTON VILL SOUTH" ~ 1757,
    TRUE ~ voter_reg
  ),
  voter_tnt_raw = case_when(
    NAME == "AMHERST TWP #1" ~ 782,
    NAME == "AMHERST TWP #2" ~ 831,
    NAME == "AMHERST TWP #3" ~ 683,
    NAME == "AMHERST TWP #4" ~ 637,
    NAME == "CAMDEN TWP/KIPTON VILL" ~ 853,
    NAME == "ELYRIA TWP #1" ~ 815,
    NAME == "ELYRIA TWP #2" ~ 863,
    NAME == "GRAFTON TWP #1" ~ 843,
    NAME == "GRAFTON TWP #2" ~ 734,
    NAME == "GRAFTON VILL #1/#2" ~ 550,
    NAME == "GRAFTON VILL #3/#4" ~ 674,
    NAME == "LAGRANGE TWP #1" ~ 880,
    NAME == "LAGRANGE TWP #2" ~ 1122,
    NAME == "NEW RUSSIA TWP" ~ 983,
    NAME == "ROCHESTER TWP & VILL" ~ 427,
    NAME == "S AMHERST VILL" ~ 800,
    NAME == "SHEFFIELD TWP EAST" ~ 417,
    NAME == "SHEFFIELD TWP NORTH" ~ 464,
    NAME == "SHEFFIELD TWP WEST" ~ 491,
    NAME == "SHEFFIELD VILL #1" ~ 665,
    NAME == "SHEFFIELD VILL #2" ~ 742,
    NAME == "SHEFFIELD VILL #3" ~ 791,
    NAME == "WELLINGTON VILL NORTH" ~ 867,
    NAME == "WELLINGTON VILL SOUTH" ~ 1098,
    TRUE ~ voter_tnt_raw
  ),
  tnt_pct = case_when(
    NAME == "AMHERST TWP #1" ~ "75.34%",
    NAME == "AMHERST TWP #2" ~ "76.59%",
    NAME == "AMHERST TWP #3" ~ "66.76%",
    NAME == "AMHERST TWP #4" ~ "78.26",
    NAME == "CAMDEN TWP/KIPTON VILL" ~ "77.19%",
    NAME == "ELYRIA TWP #1" ~ "73.69%",
    NAME == "ELYRIA TWP #2" ~ "75.31%",
    NAME == "GRAFTON TWP #1" ~ "78.35%",
    NAME == "GRAFTON TWP #2" ~ "75.05%",
    NAME == "GRAFTON VILL #1/#2" ~ "69.97%",
    NAME == "GRAFTON VILL #3/#4" ~ "70.43%",
    NAME == "LAGRANGE TWP #1" ~ "68.43%",
    NAME == "LAGRANGE TWP #2" ~ "77.59%",
    NAME == "NEW RUSSIA TWP" ~ "72.60%",
    NAME == "ROCHESTER TWP & VILL" ~ "78.49%",
    NAME == "S AMHERST VILL" ~ "75.54%",
    NAME == "SHEFFIELD TWP EAST" ~ "61.78",
    NAME == "SHEFFIELD TWP NORTH" ~ "63.82%",
    NAME == "SHEFFIELD TWP WEST" ~ "54.74%",
    NAME == "SHEFFIELD VILL #1" ~ "79.45%",
    NAME == "SHEFFIELD VILL #2" ~ "81.72%",
    NAME == "SHEFFIELD VILL #3" ~ "78.16%",
    NAME == "WELLINGTON VILL NORTH" ~ "62.20%",
    NAME == "WELLINGTON VILL SOUTH" ~ "62.49%",
    TRUE ~ tnt_pct
  ),
  voter_tnt = G16votes/voter_reg * 100,
  county = "Lorain") %>% 
  filter(NAME != "VOTING DISTRICTS NOT DEFINED") %>% 
  select(-city, -reporting_district, -tnt_pct)

cuyahoga <- oh_full %>% filter(CTY_ID == "035")

cuyahoga_tnt_temp <- read_excel("../DATA/Election_Data/cuyahoga_tnt.xlsx",
                                skip = 1) %>% 
  filter(`County Name` == "Cuyahoga") %>% 
  transmute(NAME = `Precinct Name`,
            PREC_ID1 = `Precinct Code`,
            voter_reg = `Registered Voters`,
            voter_tnt_raw = `Total Voters`,
            tnt_pct = `Turnout Percentage`
  ) %>% 
  select(-tnt_pct)

cuyahoga_tnt <- left_join(cuyahoga, cuyahoga_tnt_temp, by = c("NAME", "PREC_ID1")) %>% 
  mutate(voter_tnt = (G16votes/voter_reg) * 100,
         county = "Cuyahoga")



oh_tnt <- bind_rows(lorain_tnt, cuyahoga_tnt) %>% 
  mutate(
    ballots_cast = NA_real_,
    rej_pct = NA_real_,
    rej_binned = as.factor(NA_real_),
    pv_rej = NA_real_)  %>% 
  select(
    GEOID,
    county,
    state,
    CTY_ID,
    PREC_ID1,
    NAME,
    G16PRERTRU,
    G16PREDCLI,
    G16PREO,
    G16votes,
    voter_tnt,
    vaptnt, 
    pv_rej,
    ballots_cast,
    rej_pct,
    rej_binned,
    maj_race,
    voter_reg,
    tot_cvap
  )


#--------------------------------------Pennsylvania------------------------------------

# 42003 Allegheny PA
# 42101 Philadelphia PA

#PA shapefile/VEST data 
pa_shp <- st_read("../DATA/dataverse_files_2016_VDT_VEST/pa_vest_16/pa_vest_16.shp") %>%
  filter(COUNTYFP %in% c("003", "101")) %>%
  mutate(
    CTY_ID = as.character(COUNTYFP),
    PREC_ID1 = VTDST,
    G16PREO = G16PRELJOH + G16PREGSTE + G16PRECCAS,
    G16votes = G16PRERTRU + G16PREDCLI + G16PREO
  ) %>%
  select(CTY_ID, PREC_ID1, NAME, G16votes, G16PRERTRU, G16PREDCLI, G16PREO, geometry)


#RDH data 
election_data_pa <- read.csv("../DATA/Election_Data/Election_Data_pa.v05/election_data_pa.v05.csv", stringsAsFactors = TRUE) %>% 
  select(
    GEOID20, Name, E_16_PRES_Total, E_16_PRES_Dem, E_16_PRES_Rep
  )

demographic_data_pa <- read.csv("../DATA/Demographic_Data/Demographic_Data_pa.v04/demographic_data_pa.v04.csv", stringsAsFactors = TRUE) %>%
  transmute(
    GEOID20,
    Name,
    tot_pop = T_10_CENS_Total + ((T_20_CENS_Total - T_10_CENS_Total) / 10) * 6,
    white_pop = T_10_CENS_White + ((T_20_CENS_White - T_10_CENS_White) / 10) * 6,
    hisp_pop = T_10_CENS_Hispanic + ((T_20_CENS_Hispanic - T_10_CENS_Hispanic) / 10) * 6,
    black_pop = T_10_CENS_Black + ((T_20_CENS_Black - T_10_CENS_Black) / 10) * 6,
    asian_pop = T_10_CENS_Asian + ((T_20_CENS_Asian - T_10_CENS_Asian) / 10) * 6,
    native_pop = T_10_CENS_Native + ((T_20_CENS_Native - T_10_CENS_Native) / 10) * 6,
    pacific_pop = T_10_CENS_Pacific + ((T_20_CENS_Pacific - T_10_CENS_Pacific) / 10) * 6,
    tot_cvap = V_10_VAP_Total + ((V_20_VAP_Total - V_10_VAP_Total) / 10) * 6,
    white_cvap = V_10_VAP_White + ((V_20_VAP_White - V_10_VAP_White) / 10) * 6,
    hisp_cvap = V_10_VAP_Hispanic + ((V_20_VAP_Hispanic - V_10_VAP_Hispanic) / 10) * 6,
    black_cvap = V_10_VAP_Black + ((V_20_VAP_Black - V_10_VAP_Black) / 10) * 6,
    asian_cvap = V_10_VAP_Asian + ((V_20_VAP_Asian - V_10_VAP_Asian) / 10) * 6,
    native_cvap = V_10_VAP_Native + ((V_20_VAP_Native - V_10_VAP_Native) / 10) * 6,
    pacific_cvap = V_10_VAP_Pacific + ((V_20_VAP_Pacific - V_10_VAP_Pacific) / 10) * 6
  )

DRA_pa <- full_join(election_data_pa, demographic_data_pa, by = c("GEOID20", "Name")) %>%
  rename(NAME = Name) 

duplicates <- DRA_pa %>% group_by(NAME) %>% filter(n() > 1)


DRA_pa_unique <- DRA_pa %>% distinct(NAME, .keep_all = TRUE)


VTDRA_pa <- left_join(pa_shp, DRA_pa_unique, by = "NAME")%>% 
  mutate(
    GEOID = GEOID20,
    vaptnt =(G16votes / ((tot_pop  + tot_cvap ) / 2) * 100),
    white_pct = trunc(white_cvap  / tot_cvap * 100),
    hisp_pct =  trunc(hisp_cvap  / tot_cvap  * 100),
    black_pct = trunc(black_cvap  / tot_cvap  * 100),
    api_pct = trunc((asian_cvap  + pacific_cvap ) / tot_cvap * 100),
    native_pct = trunc(native_cvap  / tot_cvap * 100),
    maj_race = case_when(
      white_pct > 49 ~ "White",
      black_pct > 49 ~ "Black",
      hisp_pct > 49 ~ "Hispanic",
      api_pct > 49 ~ "Asian/Pacific Islander",
      native_pct > 49 ~ "Native",
      TRUE ~ "Other"
    )
  )%>%
  select(
    GEOID,
    CTY_ID,
    PREC_ID1,
    NAME,
    G16PRERTRU,
    G16PREDCLI,
    G16PREO,
    G16votes,
    vaptnt,
    tot_pop,
    tot_cvap,
    white_pop,
    white_cvap,
    white_pct,
    hisp_pop,
    hisp_cvap,
    hisp_pct,
    black_pop,
    black_cvap,
    black_pct,
    asian_pop,
    asian_cvap,
    pacific_pop,
    pacific_cvap,
    api_pct,
    native_pop,
    native_cvap,
    native_pct,
    maj_race
  )

pa_full <- VTDRA_pa %>%
  unique() %>% 
  filter(!is.na(geometry)) %>% 
  mutate(vaptnt = replace(vaptnt, vaptnt > 100, 100),
         state = "PA") 

pa_full <- st_as_sf(pa_full)

pa_full <- st_transform(st_zm(pa_full), crs = 4326)

names(pa_full) <- abbreviate(names(pa_full), minlength = 10)


allegheny <- pa_full %>% filter(CTY_ID == "003")

allegheny_tnt_temp <- read_csv("../DATA/Election_Data/allegheny_turnout.csv") %>% 
  mutate(NAME = Precinct %>%
           str_trim() %>%
           str_replace("DIST(RICT)? ", "DISTRICT ") %>% 
           str_replace("BALDWIN BR", "BALDWIN") %>% 
           str_replace("\\bHT\\b", "HEIGHTS") %>%
           str_replace("\\bHL\\b", "HILLS") %>% 
           str_replace("\\bCASL\\b", "CASTLE") %>% 
           str_replace("\\bE\\b", "EAST") %>% 
           str_replace("\\bN\\b", "NORTH") %>% 
           str_replace("\\bS\\b", "SOUTH") %>% 
           str_replace("\\bW\\b", "WEST") %>% 
           str_replace("\\bPK\\b", "PARK") %>% 
           str_replace("\\bWD\\b", "WARD") %>% 
           str_replace("OHARA", "O'HARA") %>% 
           str_replace("\\bUP\\b", "UPPER") %>% 
           str_replace("\\bST\\b", "ST.") %>%
           str_replace("\\bWRD\\b", "WARD") %>%
           str_replace("\\bMT\\b", "MOUNT") %>%
           str_replace("\\bCOLLIER DISTRICT\\b", "COLLIER WARD") %>%
           str_replace("\\bRANKIN WARD\\b", "RANKIN DISTRICT") %>%
           str_replace("\\bELIZABETH TP\\b", "ELIZABETH") %>%
           str_replace("\\bSPRINGDAL BR\\b", "SPRINGDALE") %>%
           str_replace_all("\\bWARD (\\d)\\b", "WARD 0\\1") %>%
           str_replace_all("\\bDISTRICT (\\d)\\b", "DISTRICT 0\\1"),
         NAME = case_when(
           NAME == "BLAWNOX" ~ "BLAWNOX DISTRICT 01",
           NAME == "CHALFANT DISTRICT 01" ~ "CHALFANT",
           NAME == "ELIZABETH BR" ~ "ELIZABETH",
           NAME == "FRAZER" ~ "FRAZER DISTRICT 01",
           NAME == "HEIDELBERG" ~ "HEIDELBERG DISTRICT 01",
           NAME == "SPRINGDALE TWP" ~ "SPRINGDALE",
           NAME == "LEETSDALE" ~ "LEETSDALE DISTRICT 01",
           NAME == "ROSSLYN FARM" ~ "ROSSLYN FARMS",
           NAME == "TRAFFORD" ~ "TRAFFORD DISTRICT 01",
           NAME == "MCDONALD" ~ "MCDONALD DISTRICT 05",
           NAME == "PENNSBURY VILL" ~ "PENNSBURY VILLAGE",
           NAME == "THORNBURG" ~ "THORNBURG DISTRICT 01",
           NAME == "SEWICKLEY HTS" ~ "SEWICKLEY HEIGHTS",
           TRUE ~ NAME
         )
  )

allegheny_tnt <- left_join(allegheny, allegheny_tnt_temp, by = "NAME") %>% 
  mutate(voter_tnt = as.numeric(str_remove(`Voter Turnout`, " %")),  #/100,
         voter_reg = `Registered Voters`,
         county = "Allegheny") %>% 
  select(-`Ballots Cast`, -`Voter Turnout`, -Precinct, -`Registered Voters`)


philly <- pa_full %>% filter(CTY_ID == "101")

philly_tnt_temp <- read_csv("../DATA/Election_Data/philly_turnout.csv") %>% 
  transmute(PREC_ID1 = str_pad(precinct, width = 6, pad = "0"), 
            voter_reg = total)

philly_pv <- read_csv("../DATA/2016_20_pv_abs_data/Philly_16_pv.csv")%>%
  slice(-1:-4) %>% 
  janitor::row_to_names(row_number = 1) %>% 
  filter(!is.na(`Row Labels`), `Row Labels` != "(blank)") %>% 
  column_to_rownames("Row Labels") %>%
  t() %>%
  as.data.frame() %>% 
  transmute(PREC_ID1 = as.character(row_number()),
            abs = A,
            machine = M,
            prov = P,
            tnt_pvdata = `Grand Total`)

philly_tnt <- left_join(philly, philly_tnt_temp, by = "PREC_ID1") %>% 
  # left_join(philly_pv, by = "PREC_ID1") %>% 
  mutate(voter_tnt = (G16votes/voter_reg) * 100,
         county = "Philadelphia")

pa_tnt <- bind_rows(allegheny_tnt, philly_tnt) %>% 
  mutate(
    ballots_cast = NA_real_,
    rej_pct = NA_real_,
    rej_binned = as.factor(NA_real_),
    pv_rej = NA_real_)  %>% 
  select(
    GEOID,
    county,
    state,
    CTY_ID,
    PREC_ID1,
    NAME,
    G16PRERTRU,
    G16PREDCLI,
    G16PREO,
    G16votes,
    voter_tnt,
    vaptnt, 
    pv_rej,
    ballots_cast,
    rej_pct,
    rej_binned,
    maj_race,
    voter_reg,
    tot_cvap
  )
#----------------------------------------Wisconsin-----------------------------------

# 55079 Milwaukee WI

#WI shapefile/VEST data 
wi_shp <- st_read("../DATA/dataverse_files_2016_VDT_VEST/wi_vest_16/wi_vest_16.shp") %>%
  filter(CNTY_FIPS == "55079") %>%
  transmute(
    GEOID,
    NAME,
    CTY_ID = as.character(CNTY_FIPS),
    PREC_ID1 = OBJECTID,
    G16PRERTRU, 
    G16PREDCLI,
    G16PREO = G16PRELJOH + G16PREGSTE + G16PRECCAS,
    G16votes = G16PRERTRU + G16PREDCLI + G16PREO,
    tot_pop = PERSONS,
    tot_cvap = PERSONS18,
    white_pop = WHITE,
    white_cvap = WHITE18,
    black_pop = BLACK,
    black_cvap = BLACK18,
    hisp_pop = HISPANIC,
    hisp_cvap = HISPANIC18,
    asian_pop = ASIAN,
    asian_cvap = ASIAN18,
    native_pop = AMINDIAN,
    native_cvap = AMINDIAN18,
    pacific_pop = PISLAND,
    pacific_cvap = PISLAND18
  )


VTDRA_wi <- wi_shp %>% 
  mutate(
    PREC_ID1 = as.character(PREC_ID1),
    vaptnt = (G16votes / ((tot_pop + tot_cvap) / 2) * 100),
    white_pct = trunc(white_cvap / tot_cvap * 100),
    hisp_pct = trunc(hisp_cvap / tot_cvap * 100),
    black_pct = trunc(black_cvap / tot_cvap * 100),
    api_pct = trunc((asian_cvap + pacific_cvap) / tot_cvap * 100),
    native_pct = trunc(native_cvap / tot_cvap * 100),
    maj_race = case_when(
      white_pct > 49 ~ "White",
      black_pct > 49 ~ "Black",
      hisp_pct > 49 ~ "Hispanic",
      api_pct > 49 ~ "Asian/Pacific Islander",
      native_pct > 49 ~ "Native",
      TRUE ~ "Other"
    )
  )

wi_full <- VTDRA_wi %>%
  unique() %>% 
  filter(!is.na(geometry)) %>% 
  mutate(vaptnt = replace(vaptnt, vaptnt > 100, 100),
         state = "WI") 

names(wi_full) <- abbreviate(names(wi_full), minlength = 10)


wi_tnt_temp <- st_read("../DATA/dataverse_files_2016_VDT_VEST/wi_2016/wi_2016.shp") %>% 
  filter(CNTY_NAME == "MILWAUKEE") %>% 
  transmute(voter_reg = RV, geometry,
            G16voteswi = G16PRERTRU + G16PREDCLI + G16PRECCAS + G16PRELJOH +
              G16PREGSTE + G16PREWMOO + G16PREADEL + G16PREOFOX + G16PREOMCM +
              G16PREOMAT + G16PREOSCH + G16PREOKEN + G16PREOKOT + G16PREOHOE + 
              G16PREOMAL + G16PREOSOL + G16PREOWRI)

wi_full <- st_as_sf(wi_full) %>% 
  select(-G16votes)

wi_tnt_temp <- st_as_sf(wi_tnt_temp)

wi_tnt_temp <- wi_tnt_temp %>% st_transform(crs = st_crs(wi_full))

wi_tnt <- st_join(wi_full, wi_tnt_temp) %>% 
  mutate(G16votes = G16voteswi,
         voter_tnt = (G16votes/voter_reg) * 100,
         ballots_cast = NA_real_,
         rej_pct = NA_real_,
         rej_binned = as.factor(NA_real_),
         county = "Milwaukee",
         pv_rej = NA_real_) %>%
  select(-G16voteswi) %>% 
  select(
    GEOID,
    county,
    state,
    CTY_ID,
    PREC_ID1,
    NAME,
    G16PRERTRU,
    G16PREDCLI,
    G16PREO,
    G16votes,
    voter_tnt,
    pv_rej,
    ballots_cast,
    rej_pct,
    rej_binned,
    maj_race,
    voter_reg,
    tot_cvap, 
    vaptnt
  )
#---------------------------------------Analysis--------------------------------

#crs dont match, get the crs for everything
crs <- lapply(list(az_tnt, ga_tnt, mi_tnt, nc_tnt, oh_tnt, pa_tnt, wi_tnt), st_crs)

harmonized_crs <- lapply(
  list(az_tnt, ga_tnt, mi_tnt, nc_tnt, oh_tnt, pa_tnt, wi_tnt),
  function(obj) st_transform(st_as_sf(obj), crs = 4326)
) %>%
  lapply(st_make_valid)

names(harmonized_crs) <- c("az_tnt", "ga_tnt", "mi_tnt", "nc_tnt", "oh_tnt", "pa_tnt", "wi_tnt")

list2env(harmonized_crs, envir = .GlobalEnv)

full_df <- bind_rows(az_tnt, ga_tnt, mi_tnt, nc_tnt, oh_tnt, pa_tnt, wi_tnt) %>%
  st_drop_geometry()



full_tnt <- bind_rows(az_tnt, ga_tnt, mi_tnt, nc_tnt, oh_tnt, pa_tnt, wi_tnt) %>% 
  mutate(
    voter_tnt = if_else(voter_tnt >= 99.9 | voter_tnt == 0, NA_real_, voter_tnt),
    voter_tnt_cvap = (G16votes / tot_cvap) * 100, 
    voter_tnt_cvap = if_else(voter_tnt_cvap >= 99.9 | voter_tnt_cvap == 0, NA_real_, voter_tnt_cvap),
    rej_pct = case_when(rej_pct == Inf ~ NA_real_,
                        TRUE ~ rej_pct)) %>% 
  select(GEOID,
         CTY_ID,
         county,
         PREC_ID1,
         state,
         NAME,
         maj_race,
         G16votes,
         voter_reg,
         voter_tnt,
         voter_tnt_cvap,
         pv_rej,
         ballots_cast,
         tot_cvap, 
         rej_pct,
         rej_binned
  ) %>% 
  mutate(
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
  )

##CVAP Standardization 
full_tnt <- full_tnt %>%
  mutate(county_cvap_tnt=case_when(
    CTY_ID==60 ~ 71.50, 
    CTY_ID==32 ~ 71.70, 
    CTY_ID==24 ~ 54.50, 
    CTY_ID=="MC" ~ 46.30, 
    CTY_ID==121 ~ 60.60, 
    CTY_ID==163 ~ 59.90, 
    CTY_ID=="035" ~ 61.00, 
    CTY_ID=="093" ~ 59.70, 
    CTY_ID=="003" ~ 66.90, 
    CTY_ID==101 ~ 61.20, 
    CTY_ID==55079 ~ 64.60,
    TRUE ~ NA
  ))


#standardization process 
full_tnt_std <- full_tnt %>%
  group_by(county) %>%
  mutate(
    precinct_cvap_mean=sum(voter_tnt_cvap, na.rm=TRUE)/n(), 
    scale_factor=county_cvap_tnt/precinct_cvap_mean, 
    precinct_cvap_std=voter_tnt_cvap * scale_factor) %>%
  ungroup() 



#Adjustment process 
full_tnt <- full_tnt_std %>%
  mutate(#adjustment=pmax(10, 0.02 * tot_cvap), 
         cvap_adj= ifelse(precinct_cvap_std > 100, 
                          tot_cvap + 10, tot_cvap), 
         precinct_cvap_std_adj= 100 * G16votes/cvap_adj) %>% 
  select(-county_cvap_tnt, -scale_factor, -precinct_cvap_mean, cvap_adj) 





write_sf(full_tnt, "../DATA/full_datasets/full_2024_revised.shp", append = FALSE)

full_16_csv <- full_tnt %>% st_drop_geometry()

write_csv(full_16_csv, "../DATA/full_datasets/full_2016_revised.csv")

split_states <- split(full_tnt, full_tnt$state)

list2env(split_states, envir = .GlobalEnv)

purrr::walk2(split_states,
             names(split_states),
             ~ write_sf(.x, paste0(.y, "_2016_revised.shp"), append = FALSE))


##-------------------------------models and tables----------------------------

race_tnt_16 <- lm(voter_tnt ~ maj_race, data = full_tnt)

race_tnt_mixed_16 <- lmer(voter_tnt ~ maj_race + factor(state) + (1 | CTY_ID), data = full_tnt)

#calculate marginal and conditional r2 (not reported by lmer)
r.squaredGLMM(race_tnt_mixed_16)


race_tnt_cvap_16 <- lm(precinct_cvap_std_adj ~ maj_race, data = full_tnt)


race_tnt_mixed_cvap_16 <- lmer(precinct_cvap_std_adj ~ maj_race + factor(state) + (1 | CTY_ID), data = full_tnt)


#calculate marginal and conditional r2 (not reported by lmer)
r.squaredGLMM(race_tnt_mixed_cvap_16)

race_rej_16 <- lm(rej_pct ~ maj_race, data = full_tnt)

race_rej_mixed_16 <- lmer(rej_pct ~ maj_race + (1 | CTY_ID), data = full_tnt)

r.squaredGLMM(race_rej_mixed_16)

modelsummary::modelsummary(
  list("Model 1" = race_tnt_16, 
       "Model 2" = race_tnt_mixed_16, 
       "Model 1 (CVAP)" = race_tnt_cvap_16, 
       "Model 2 (CVAP)" = race_tnt_mixed_cvap_16),
  output = "../Output/tnt_16_models_revised.html",
  coef_rename = c(
    "(Intercept)" = "Intercept",
    "maj_raceBlack" = "Majority Black",
    "maj_raceHispanic" = "Majority Hispanic",
    "maj_raceAsian/Pacific Islander" = "Majority API",
    "maj_raceNative" = "Majority Native American",
    "maj_racePlurality" = "Racial Plurality",
    "factor(state)GA" = "Georgia",
    "factor(state)NC" = "North Carolina",
    "factor(state)OH" = "Ohio",
    "factor(state)PA" = "Pennsylvania",
    "factor(state)WI" = "Wisconsin"
  ),
  stars = TRUE,  # Adds significance stars
  gof_map = c("r.squared", "adj.r.squared", "nobs"),  # Include R-squared and observations
  title = "Regression Models Predicting vaptnt",
  notes = "Significance levels: ***p<0.01, **p<0.05, *p<0.1"
)


modelsummary::modelsummary(
  list("Model 1" = race_rej_16, "Model 2" = race_rej_mixed_16),
  output = "../Output/rej_16_models_revised.html",
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
  title = "Regression Models Predicting vaptnt",
  notes = "Significance levels: ***p<0.01, **p<0.05, *p<0.1"
)

race_means <- full_tnt %>% 
  st_drop_geometry() %>% 
  #filter(CTY_ID != 163) %>% 
  group_by(maj_race) %>% 
  summarise(
    n = n(),
    mean_tnt = round(mean(voter_tnt, na.rm = TRUE), 2),
    med_tnt = round(median(voter_tnt, na.rm = TRUE), 2),
    na_tnt = sum(is.na(voter_tnt)),
    mean_tnt_cvap = round(mean(precinct_cvap_std_adj, na.rm = TRUE), 2),
    med_tnt_cvap = round(median(precinct_cvap_std_adj, na.rm = TRUE), 2),
    na_tnt_cvap = sum(is.na(precinct_cvap_std_adj))
  )

race_means_html <- knitr::kable(race_means, format = "html")

writeLines(race_means_html, "../Output/race_means_16_revised.html")

tnt_means <- full_tnt %>% 
  st_drop_geometry() %>% 
  group_by(state, CTY_ID) %>% 
  summarise(
    n = n(),
    mean_tnt = round(mean(voter_tnt, na.rm = TRUE), 2),
    med_tnt = round(median(voter_tnt, na.rm = TRUE), 2),
    na_tnt = sum(is.na(voter_tnt)),
    mean_tnt_cvap = round(mean(precinct_cvap_std_adj, na.rm = TRUE), 2),
    med_tnt_cvap = round(median(precinct_cvap_std_adj, na.rm = TRUE), 2),
    na_tnt_cvap = sum(is.na(precinct_cvap_std_adj))
  )

tnt_means_html <- knitr::kable(tnt_means, format = "html")

writeLines(tnt_means_html, "../Output/tnt_means_16_revised.html")

rej_means <- full_tnt %>% 
  st_drop_geometry() %>% 
  filter(!state %in% c("MI", "OH", "PA", "WI")) %>% 
  group_by(state, CTY_ID) %>% 
  summarise(
    n = n(),
    mean_tnt = round(mean(rej_pct, na.rm = TRUE), 2),
    med_tnt = round(median(rej_pct, na.rm = TRUE), 2),
    n_na = sum(is.na(rej_pct))
  )

rej_means_html <- knitr::kable(rej_means, format = "html")

writeLines(rej_means_html, "../Output/rej_means_16_revised.html")

# 26163 Wayne MI

##----------------------------------plots--------------------------------------


racial_tnt_2016 <- ggplot(full_tnt, aes(x = maj_race, y = precinct_cvap_std_adj)) +
  geom_jitter(width = 0.2, alpha = 0.3, color = "blue") +  
  geom_boxplot(alpha = 0.5, outlier.shape = NA) +  
  theme_minimal() +
      labs(y="% CVAP Turnout", 
        x = "Precinct Racial Majority") 


ggsave(racial_tnt_2016, filename = "../output/racial_tnt_2016.jpeg", width = 10, height = 6, units = "in")


       tercile_scatter_16 <- full_tnt %>%
         filter(!is.na(rej_binned)) %>% #no na values according to table call, but its adds them without the inclusion of this line
         ggplot(aes(x=rej_binned, y=precinct_cvap_std_adj, color=rej_binned)) +    
         geom_jitter(width = 0.2, alpha = 0.3) +
         geom_boxplot(alpha = 0.5, outlier.shape = NA) +
         theme_minimal() +
         labs(
              y= "% CVAP Turnout",
              x = "Precinct Rejections", 
              color="Rejection Category") +
         scale_color_manual(values = c("Lower 3rd" = "green",
                                       "Middle 3rd" = "yellow",
                                       "Upper 3rd" = "blue")) + 
          theme_dark(base_size = 12) 
     
       
       ggsave(tercile_scatter_16, filename = "../output/tercile_scatter_16.jpeg", width = 8, height = 6, units = "in")
    
       
       stacked_bar_16 <- full_tnt %>%
         filter(!is.na(rej_binned)) %>%
         ggplot(aes(x = maj_race, fill = rej_binned)) +
         geom_bar(position = "fill") +
         labs(y = "Proportion of Precincts",
              x = "Precinct Racial Majority",
              fill = "Ballot Rejections") +
         scale_fill_manual(values = c("Lower 3rd" = "green",
                                      "Middle 3rd" = "yellow",
                                      "Upper 3rd" = "blue")) +
         theme_dark(base_size = 12) +
         theme(
           axis.text.x = element_text(
             size = 8,
             angle = 45,        # rotate labels 45 degrees
             hjust = 1,         # right-justify
             vjust = 1          # vertically align
           )
         )
       
       ggsave(stacked_bar_16, filename = "../output/stacked_bar_16.jpeg", width = 8, height = 6, units = "in")
       
       