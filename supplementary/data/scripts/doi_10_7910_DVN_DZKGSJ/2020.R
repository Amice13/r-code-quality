##-----------------------------2020 PAP Analysis----------------------------##
##-----------------------------Authors: Rose Nafa & Liza Gordon-Rogers------##
##-----------------------------Created:  9/30/2025--------------------------##
##-----------------------------Last Modified: 1/7/2026--------------------##
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

##---------------------------------------Michigan------------------------



#MI shapefile/VEST data 
mi_2020_VTDw_temp <-
  st_read(dsn = "../DATA/dataverse_files_2020_VTD_VEST/mi_2020/mi_2020.shp") %>%
  subset(COUNTYFIPS %in% c(163)) %>%
  mutate(
    CTY_ID = COUNTYFIPS,
    PREC_ID = PRECINCT, 
    PREC_ID1 = PRECINCTID,
    PREC_ID2 = PRECINCT,
    PREC_ID3 = MCD,
    PREC_ID4 = NAME,
    NAME = MCD,
    PREC_ID5 = PRECINCT_S,
    G20PREO = G20PRELJOR + G20PREUBLA + G20PREGHAW + G20PRENDEL,
    G20votes = G20PRERTRU + G20PREDBID + G20PREO
  ) %>%
  select(
    CTY_ID,
    WARD,
    PREC_ID,
    NAME,
    PREC_ID1,
    PREC_ID2,
    PREC_ID3,
    PREC_ID4,
    PREC_ID5,
    G20votes,
    G20PRERTRU,
    G20PREDBID,
    G20PREO,
    geometry
  )

mi_2020_VTDw_temp2 <- mi_2020_VTDw_temp
    
   

mi_2020_VTDw <- mi_2020_VTDw_temp2 %>%
  mutate(
    ballots_cast = G20votes
  )


#Pull RDH VTD and demographic files

election_data_MI.v05 <-
  read.csv(
    "../DATA/Election_Data/Election_Data_MI.v05/election_data_MI.v05.csv",
    stringsAsFactors = TRUE
  )
demographic_data_MI.v04 <-
  read.csv(
    "../DATA/Demographic_Data/Demographic_Data_MI.v04/demographic_data_MI.v04.csv",
    stringsAsFactors = TRUE
  ) %>% 
  transmute(
    GEOID20,
    Name,
    tot_pop20 = T_20_CENS_Total,
    white_pop20 = T_20_CENS_White,
    hisp_pop20 = T_20_CENS_Hispanic,
    black_pop20 = T_20_CENS_Black,
    asian_pop20 = T_20_CENS_Asian,
    native_pop20 = T_20_CENS_Native,
    pacific_pop20 = T_20_CENS_Pacific,
    tot_cvap20 = V_20_VAP_Total,
    white_cvap20 = V_20_VAP_White,
    hisp_cvap20 = V_20_VAP_Hispanic,
    black_cvap20 = V_20_VAP_Black,
    asian_cvap20 = V_20_VAP_Asian,
    native_cvap20 = V_20_VAP_Native,
    pacific_cvap20 = V_20_VAP_Pacific
  )

#Join DRA then DRA VTD
DRA <-
  full_join(
    election_data_MI.v05,
    demographic_data_MI.v04,
    by = c("GEOID20", "Name")
  )%>%
  rename(NAME = Name)
DRA <- filter(DRA, NAME != "Voting Districts Not Defined") 

#join
VTDRAr_mi <- left_join(mi_2020_VTDw, DRA, by=c("PREC_ID4"= "NAME")) %>%
  mutate(
    GEOID = GEOID20,
    white_pct20 = trunc(white_cvap20  / tot_cvap20 * 100),
    hisp_pct20 =  trunc(hisp_cvap20  / tot_cvap20  * 100),
    black_pct20 = trunc(black_cvap20  / tot_cvap20  * 100),
    api_pct20 = trunc((asian_cvap20  + pacific_cvap20 ) / tot_cvap20 * 100),
    native_pct20 = trunc(native_cvap20  / tot_cvap20 * 100),
    maj_race = case_when(
      white_pct20 > 49 ~ "White",
      black_pct20 > 49 ~ "Black",
      hisp_pct20 > 49 ~ "Hispanic",
      api_pct20 > 49 ~ "Asian/Pacific Islander",
      native_pct20 > 49 ~ "Native",
      TRUE ~ "Other"
    )
  ) %>%
  select(
    GEOID,
    PREC_ID,
    NAME,
    G20PRERTRU,
    G20PREDBID,
    G20PREO,
    G20votes,
    maj_race,
    tot_cvap20,
    ballots_cast

  ) %>%
  rename(PREC_ID1 = PREC_ID)

# #Pull census block cvap shapefiles
mi_2020_CVAP_blockr <-
  st_read(dsn = "../DATA/2020 CVAP/mi_cvap_2020_2020_b/mi_cvap_2020_2020_b.shp") %>%
  subset(COUNTYFP20 %in% c("163"))%>%
  select(COUNTYFP20, GEOID20)

# #Pull L2 turnout block files
mi_l2_turnout_stats_block20r <-
  read_csv(
    "../DATA/l2_turnout_2020blockAgg/MI_l2_turnout_2020blockAgg/MI_l2_turnout_stats_block20.csv",col_types = cols(geoid20 = col_character())
  )%>%
  rename(GEOID20 = geoid20) %>%
  select(
    GEOID20,
    total_reg
  )


#Join L2 and census block files
blkl2 <-
  left_join(
    mi_2020_CVAP_blockr,
    mi_l2_turnout_stats_block20r,
    by = "GEOID20" 
  )



#geomatch
#From CK
matches <- geo_match(from = blkl2, to = VTDRAr_mi, method = "centroid")


prec <- block2prec(block_table = blkl2, matches = matches)

fill_zero_rows <- lapply(names(prec)[-c(1:2)], function(x)
  0) |>
  setNames(names(prec)[-c(1:2)])

prec <- complete(prec,
                 matches_id = seq_len(nrow(VTDRAr_mi)),
                 fill = fill_zero_rows)

MIfull <- bind_cols(VTDRAr_mi, prec) %>% 
  mutate(voter_reg = total_reg,
         voter_tnt = (G20votes/voter_reg)*100,
         voter_tnt = case_when(
           voter_tnt > 100 ~ NA_real_,
           voter_tnt == 0 ~NA_real_,
           TRUE ~ voter_tnt
         ),
         voter_tnt_cvap = (G20votes / tot_cvap20) * 100, 
         voter_tnt_cvap = if_else(voter_tnt_cvap >= 99.9 | voter_tnt_cvap == 0, NA_real_, voter_tnt_cvap),
         county = "Wayne",
         state="MI",
         CTY_ID=63, 
         pv_rej = NA_real_,
         rej_pct = NA_real_,
         rej_binned = NA_real_, 
         tot_cvap=tot_cvap20) %>%
  select(
    GEOID,
    county,
    state, 
    CTY_ID, 
    PREC_ID1,
    NAME,
    G20PRERTRU,
    G20PREDBID,
    G20PREO,
    G20votes,
    voter_tnt,
    voter_tnt_cvap,
    pv_rej,
    ballots_cast,
    tot_cvap, 
    rej_pct,
    rej_binned,
    maj_race,
    voter_reg
  )
  


#st_write(MIfull, "../Data/full_datasets/MI_2020_data.shp", append=FALSE)

#create shapefile
# #st_write(
#   MIfull,
#   "../DATA/Storymapfiles/MI_2020_shp.shp")


##--------------------------------Arizona---------------------------------------

##AZ shapefile/VEST Data
az_2020_VTDm <-
  st_read(dsn = "../DATA/dataverse_files_2020_VTD_VEST/az_2020/az_2020.shp") %>% 
  subset(CDE_COUNTY %in% c("MC"))

az_2020_VTDm <- az_2020_VTDm %>%
  mutate(CTY_ID = CDE_COUNTY,
         PREC_ID1 = PCTNUM,
         NAME = PRECINCTNA,
         G20PREO = G20PRELJOR,
         G20votes = G20PRERTRU + G20PREDBID + G20PREO) %>%
  select(CTY_ID,
         PREC_ID1,
         NAME,
         G20votes,
         G20PRERTRU,
         G20PREDBID,
         G20PREO,
         geometry)

#Pull RDA VTD and demographic files
election_data_AZ.v05 <-
  read.csv(
    "../DATA/Election_Data/Election_Data_AZ.v05/election_data_AZ.v05.csv",
    stringsAsFactors = TRUE
  )
demographic_data_AZ.v04 <-
  read.csv(
    "../DATA/Demographic_Data/Demographic_Data_AZ.v04/demographic_data_AZ.v04.csv",
    stringsAsFactors = TRUE
  ) %>% 
  transmute(
    GEOID20,
    Name,
    tot_pop20 = T_20_CENS_Total,
    white_pop20 = T_20_CENS_White,
    hisp_pop20 = T_20_CENS_Hispanic,
    black_pop20 = T_20_CENS_Black,
    asian_pop20 = T_20_CENS_Asian,
    native_pop20 = T_20_CENS_Native,
    pacific_pop20 = T_20_CENS_Pacific,
    tot_cvap20 = V_20_VAP_Total,
    white_cvap20 = V_20_VAP_White,
    hisp_cvap20 = V_20_VAP_Hispanic,
    black_cvap20 = V_20_VAP_Black,
    asian_cvap20 = V_20_VAP_Asian,
    native_cvap20 = V_20_VAP_Native,
    pacific_cvap20 = V_20_VAP_Pacific
  )

#Join DRA then DRA VTD
DRA <-
  full_join(
    election_data_AZ.v05,
    demographic_data_AZ.v04,
    by = c("GEOID20", "Name")
  ) %>%
  rename(NAME = Name)

VTDRAr_az <- left_join(az_2020_VTDm, DRA, by = "NAME") %>% 
  filter(str_detect(GEOID20, "04013")) %>% 
  mutate(
    GEOID = GEOID20,
    vaptnt20 = trunc(E_20_PRES_Total / ((tot_pop20  + tot_cvap20 ) / 2) * 100),
    white_pct20 = trunc(white_cvap20  / tot_cvap20 * 100),
    hisp_pct20 =  trunc(hisp_cvap20  / tot_cvap20  * 100),
    black_pct20 = trunc(black_cvap20  / tot_cvap20  * 100),
    api_pct20 = trunc((asian_cvap20  + pacific_cvap20 ) / tot_cvap20 * 100),
    native_pct20 = trunc(native_cvap20  / tot_cvap20 * 100),
    maj_race = case_when(
      white_pct20 > 49 ~ "White",
      black_pct20 > 49 ~ "Black",
      hisp_pct20 > 49 ~ "Hispanic",
      api_pct20 > 49 ~ "Asian/Pacific Islander",
      native_pct20 > 49 ~ "Native",
      TRUE ~ "Other"
    ),
    state = "AZ") %>% 
  select(
    GEOID,
    CTY_ID,
    PREC_ID1,
    NAME,
    state,
    G20PRERTRU,
    G20PREDBID,
    G20PREO,
    G20votes,
    tot_cvap20,
    maj_race
  )



#Pull census block cvap shapefiles
az_2020_CVAP_blockr <-
  st_read(dsn = "../DATA/2020 CVAP/az_cvap_2020_2020_b/az_cvap_2020_2020_b.shp") %>%
  filter(COUNTYFP20 == "13") %>%
  select(COUNTYFP20, GEOID20)

#Pull L2 turnout block files
az_l2_turnout_stats_block20r <-
  read_csv(
    "../DATA/l2_turnout_2020blockAgg/AZ_l2_turnout_2020blockAgg/AZ_l2_turnout_stats_block20.csv",
    col_types = cols(geoid20 = col_character())
  ) %>%
  mutate(GEOID20 = str_pad(
    string = geoid20,
    width = 15,
    pad = "0"
  )) %>%
  filter(str_detect(GEOID20, "04013")) %>%
  select(
    GEOID20,
    total_reg
  )

#Join L2 and census block files
blkl2 <-
  left_join(
    az_2020_CVAP_blockr,
    az_l2_turnout_stats_block20r,
    by = "GEOID20"
  )


#geomatch
#From CK
matches <- geo_match(from = blkl2, to = VTDRAr_az, method = 'centroid')

prec <- block2prec(block_table = blkl2, matches = matches)

fill_zero_rows <- lapply(names(prec)[-c(1:2)], function(x)
  0) |>
  setNames(names(prec)[-c(1:2)])

prec <- complete(prec,
                 matches_id = seq_len(nrow(VTDRAr_az)),
                 fill = fill_zero_rows)


full_az <- bind_cols(VTDRAr_az, prec)

AZfull_temp <- full_az# %>% st_drop_geometry()

az_pv_raw <- read_excel("../DATA/2016_20_pv_abs_data/AZ_Mar_2020_pv.xlsx")

az_pv_clean <- az_pv_raw %>%
  # Extract precinct numbers from the third column
  mutate(precinct = ifelse(grepl("^PRECINCT #\\d+", ...3), ...3, NA)) %>%
  fill(precinct, .direction = "down") %>%
  filter(!grepl("Reason Code|Total|PRECINCT #\\d+ TOTAL|Description", ...3, ignore.case = TRUE)) %>%
  fill(`Provisional Totals by Precinct - Canvass`, .direction = "down") %>%
  # Remove rows where precinct data is NA
  filter(!is.na(...3) & !is.na(...4)) %>%
  rename(
    Counted = `Provisional Totals by Precinct - Canvass`,
    Reason = ...2,
    Description = ...3,
    Count = ...4
  ) %>%
  mutate(precinct = str_extract(precinct, "\\d+")) %>%
  mutate(Count = as.numeric(Count)) %>% 
  select(-...5)

az_pv_full20 <- az_pv_clean %>%
  group_by(precinct) %>%
  summarize(
    pv_total = sum(Count, na.rm = TRUE),
    pv_rej = sum(Count[Counted == "No"], na.rm = TRUE),
    .groups = "drop"
  ) %>%
  mutate(
    rej_pct = pv_rej / pv_total,
    precinct = str_pad(as.character(precinct), width = 4, pad = "0"),
    precinct = paste0("MC", precinct)
  )


AZfull <- left_join(AZfull_temp, az_pv_full20, by = c("PREC_ID1" = "precinct")) %>% 
  mutate(county = "Maricopa",
         rej_binned = factor(
           ntile(rej_pct, 3),
           levels = 1:3,
           labels = c("Lower 3rd", "Middle 3rd", "Upper 3rd")
         ),
         ballots_cast = NA_real_,
         voter_reg = total_reg,
         voter_tnt = (G20votes / voter_reg) * 100,
         voter_tnt = case_when(
           voter_tnt > 100 ~ NA_real_,
           voter_tnt == 0 ~ NA_real_,
           TRUE ~ voter_tnt
         ),
         tot_cvap20,
         voter_tnt_cvap = (G20votes / tot_cvap20) * 100, 
         voter_tnt_cvap = if_else(voter_tnt_cvap >= 99.9 | voter_tnt_cvap == 0, NA_real_, voter_tnt_cvap),
         state = "AZ", 
         tot_cvap= tot_cvap20
  ) %>% 
  select(
    GEOID,
    county,
    state,
    CTY_ID,
    PREC_ID1,
    NAME,
    G20PRERTRU,
    G20PREDBID,
    G20PREO,
    G20votes,
    voter_tnt,
    voter_tnt_cvap,
    pv_rej,
    ballots_cast,
    tot_cvap, 
    rej_pct,
    rej_binned,
    maj_race,
    voter_reg
  )
# 
# #create shapefile
 st_write(
   AZfull,
  "../DATA/Storymapfiles/AZ_2020_shp.shp")


##------------------------------------Georgia-----------------------------------


#Pull state VTD shp files (from VEST w G20 data)
ga_2020_VTD <-
  st_read(dsn = "../DATA/dataverse_files_2020_VTD_VEST/ga_2020/ga_2020.shp") %>%
  subset(CTYNAME %in% c("FULTON"))


#Pull ballot rejection files (and join counties/files if necessary)
GA_FULTON_2020_vf <-
  read.csv(
    "../DATA/2016_20_pv_abs_data/GA_FULTON_2020_vf.csv"
  )


stat <- GA_FULTON_2020_vf %>%
  group_by(Ballot.Status) %>%
  summarise(cnt = n())



temp <- GA_FULTON_2020_vf %>% 
  mutate(
    Ballot.Status = case_when(
      Ballot.Status == "A" ~ "A",
      Ballot.Status == "C" ~ "C",
      Ballot.Status == "PROV" ~ "PROV",
      Ballot.Status == "R" ~ "R",
      Ballot.Status == "S" ~ "S",
      TRUE ~ "Blank"
    )
  )



ga_2020_VTDf_temp <- st_read("../DATA/dataverse_files_2020_VTD_VEST/ga_2020/ga_2020_VTDf.shp") %>% 
  transmute(
    CTY_N = CTYNAME,
    CTY_ID = FIPS2,
    PREC_ID1 = PRECINCT_I,
    PREC_ID2 = CTYSOSID,
    G20PRERTRU,
    G20PREDBID,
    G20PREO = G20PRELJOR,
    G20votes = G20PRERTRU + G20PREDBID + G20PREO,
    geometry)

#convert to columns
status <-
  reshape2::dcast(GA_FULTON_2020_vf, County.Precinct ~ Ballot.Status, value.var = "Ballot.Status")
st_reason <-
  reshape2::dcast(GA_FULTON_2020_vf, County.Precinct ~ Status.Reason, value.var = "Status.Reason")
b_style <-
  reshape2::dcast(GA_FULTON_2020_vf, County.Precinct ~ Ballot.Style, value.var = "Ballot.Style")
chlng <-
  reshape2::dcast(GA_FULTON_2020_vf,
                  County.Precinct ~ Challenged.Provisional,
                  value.var = "Challenged.Provisional")
ballots <- list(b_style, status, st_reason, chlng)
purrr::reduce(.x = ballots,
              merge,
              by = c('County.Precinct'),
              all = T)
ballots <- as.data.frame(ballots)

status_all <- reshape2::dcast(
  GA_FULTON_2020_vf, 
  County.Precinct ~ Ballot.Status, 
  value.var = "Ballot.Status", 
  fun.aggregate = length, # count how many times each status appears per precinct
  drop = FALSE
)

GA_FULTON_2020_vf_R <- GA_FULTON_2020_vf %>% 
  filter(Ballot.Status == "R")

st_reason_R <- reshape2::dcast(
  GA_FULTON_2020_vf, 
  County.Precinct ~ Status.Reason, 
  value.var = "Status.Reason",
  fun.aggregate = length, 
  drop = FALSE
)

reasons_summary <- st_reason_R %>%
  select(-County.Precinct) %>%                
  summarise(across(everything(), ~ sum(.x, na.rm = TRUE))) %>%  
  pivot_longer(cols = everything(), names_to = "reason", values_to = "total") %>%
  mutate(
    pct = round(total / sum(total) * 100, 2) 
  ) %>%
  arrange(desc(total))


ga_2020_VTDf_x <- 
  left_join(
    ga_2020_VTDf_temp,
    status_all,
    by = c("PREC_ID1" = "County.Precinct")
  ) %>% 
  mutate(
    pv_rej = R,
    rej_denom = A + PROV,
    ballots_cast = G20votes,
    rej_pct = (pv_rej/rej_denom)*100,
    rej_binned = factor(ntile(rej_pct, 3),
                        levels = 1:3,
                        labels = c("Lower 3rd",
                                   "Middle 3rd",
                                   "Upper 3rd"))
    
  ) %>% 
  select(
    PREC_ID1,
    PREC_ID2,
    CTY_ID,
    G20votes,
    G20PREDBID,
    G20PRERTRU,
    G20PREO,
    pv_rej,
    ballots_cast,
    rej_pct,
    rej_binned
  )



#Pull RDA VTD and demographic files
demographic_data_GA_v04 <- read_csv("../DATA/Demographic_Data/Demographic_Data_GA.v04/GA.v04.csv") %>% 
  transmute(
    GEOID20,
    Name,
    tot_pop20 = T_20_CENS_Total,
    white_pop20 = T_20_CENS_White,
    hisp_pop20 = T_20_CENS_Hispanic,
    black_pop20 = T_20_CENS_Black,
    asian_pop20 = T_20_CENS_Asian,
    native_pop20 = T_20_CENS_Native,
    pacific_pop20 = T_20_CENS_Pacific,
    tot_cvap20 = V_20_VAP_Total,
    white_cvap20 = V_20_VAP_White,
    hisp_cvap20 = V_20_VAP_Hispanic,
    black_cvap20 = V_20_VAP_Black,
    asian_cvap20 = V_20_VAP_Asian,
    native_cvap20 = V_20_VAP_Native,
    pacific_cvap20 = V_20_VAP_Pacific
  )

election_data_GA_v04 <- read_csv("../DATA/Election_Data/Election_Data_GA.v04/election_data_GA.v04.csv")



DRA <- left_join(demographic_data_GA_v04, election_data_GA_v04, by=c("GEOID20", "Name"))%>%
  mutate(NAME=Name,
         GEODRA = trimws(str_sub(GEOID20, 3, -1)),
  )

ga_2020_VTDf <- ga_2020_VTDf_x %>% 
  mutate(
    G1 = str_pad(PREC_ID1, 6, pad = "0"),
    GEODRA = trimws(paste(CTY_ID, G1, sep = "", collapse=NULL))
  )


matched <- intersect(ga_2020_VTDf$GEODRA, DRA$GEODRA)
all <-  union(ga_2020_VTDf$GEODRA, DRA$GEODRA)
non.matched <- all[!all %in% matched]
#write.csv(DRA, "~/Downloads/DRA.csv")
gamatch<-ga_2020_VTDf$GEODRA

#join
VTDRAr_ga <- left_join(ga_2020_VTDf, DRA, by="GEODRA") %>% 
  mutate(
    GEOID = GEOID20,
    vaptnt20 = trunc(E_20_PRES_Total / ((tot_pop20  + tot_cvap20 ) / 2) * 100),
    white_pct20 = trunc(white_cvap20  / tot_cvap20 * 100),
    hisp_pct20 =  trunc(hisp_cvap20  / tot_cvap20  * 100),
    black_pct20 = trunc(black_cvap20  / tot_cvap20  * 100),
    api_pct20 = trunc((asian_cvap20  + pacific_cvap20 ) / tot_cvap20 * 100),
    native_pct20 = trunc(native_cvap20  / tot_cvap20 * 100),
    maj_race = case_when(
      white_pct20 > 49 ~ "White",
      black_pct20 > 49 ~ "Black",
      hisp_pct20 > 49 ~ "Hispanic",
      api_pct20 > 49 ~ "Asian/Pacific Islander",
      native_pct20 > 49 ~ "Native",
      TRUE ~ "Other"
    ),
    state = "GA") %>% 
  select(
    GEOID,
    CTY_ID,
    PREC_ID1,
    NAME,
    state,
    G20PRERTRU,
    G20PREDBID,
    G20PREO,
    G20votes,
    maj_race,
    tot_cvap20,
    pv_rej,
    ballots_cast,
    rej_pct,
    rej_binned
  )


# #Pull census block cvap shapefiles
 ga_2020_CVAP_block <-
   st_read(dsn = "../DATA/2020 CVAP/ga_cvap_2020_2020_b/ga_cvap_2020_2020_b.shp") %>% 
   subset(COUNTYFP20 %in% c("121")) %>% 
   st_zm()
 

#Pull L2 turnout block files
GA_l2_turnout_stats_block20r <-
  read_csv(
    "../DATA/l2_turnout_2020blockAgg/GA_l2_turnout_2020blockAgg/GA_l2_turnout_stats_block20.csv", col_types = cols(geoid20 = col_character())
  ) %>%
  rename(GEOID20 = geoid20) %>%
  select(
    GEOID20,
    total_reg
  )
# 
# #Join L2 and census block files
blkl2 <-
  left_join(ga_2020_CVAP_block,
            GA_l2_turnout_stats_block20r,
            by = "GEOID20") %>%
  select(
    GEOID20,
    total_reg
  )


#geomatch
#From CK
matches <-
  geo_match(from = blkl2, to = VTDRAr_ga, method = 'centroid')

prec <- block2prec(block_table = blkl2, matches = matches)

fill_zero_rows <- lapply(names(prec)[-c(1:2)], function(x)
  0) |>
  setNames(names(prec)[-c(1:2)])

prec <- complete(prec,
                 matches_id = seq_len(nrow(VTDRAr_ga)),
                 fill = fill_zero_rows)


VTDRAr_ga_temp <- bind_cols(VTDRAr_ga, prec) %>% 
  mutate(voter_reg = total_reg,
         voter_tnt = (G20votes/voter_reg)*100,
         voter_tnt = case_when(
           voter_tnt > 100 ~ NA_real_,
           voter_tnt == 0 ~NA_real_,
           TRUE ~ voter_tnt
         ),
         voter_tnt_cvap = (G20votes / tot_cvap20) * 100, 
         voter_tnt_cvap = if_else(voter_tnt_cvap >= 99.9 | voter_tnt_cvap == 0, NA_real_, voter_tnt_cvap),
         state = "GA",
         county = "Fulton",
         tot_cvap=tot_cvap20) %>% 
  select(
    GEOID,
    county,
    state,
    CTY_ID,
    PREC_ID1,
    NAME,
    G20PRERTRU,
    G20PREDBID,
    G20PREO,
    G20votes,
    voter_tnt,
    voter_tnt_cvap,
    pv_rej,
    ballots_cast,
    tot_cvap, 
    rej_pct,
    rej_binned,
    maj_race,
    voter_reg
  )


GAfull <- VTDRAr_ga_temp #%>% st_drop_geometry()

# #create shapefile
 st_write(
   GAfull,
   "../DATA/Storymapfiles/GA_2020_shp.shp")


##---------------------------North Carolina------------------------------------

#NC shapefile/VEST data 
nc_shp <-
  st_read(dsn = "../DATA/dataverse_files_2020_VTD_VEST/nc_2020/nc_2020.shp") %>%
  subset(COUNTY_NAM %in% c("MECKLENBURG", "COLUMBUS", "DURHAM")) %>%
  mutate(
    CTY_N = COUNTY_NAM,
    CTY_ID = COUNTY_ID,
    G20PREO = G20PRELJOR + G20PREGHAW + G20PRECBLA + G20PREOWRI,
    G20votes = G20PRERTRU + G20PREDBID + G20PREO
  ) %>%
  select(CTY_N,
         CTY_ID,
         PREC_ID,
         ENR_DESC,
         G20votes,
         G20PRERTRU,
         G20PREDBID,
         G20PREO)
         #geometry)


#read in registered voters by election date
ncreg <- read_delim("../DATA/Election_Data/voter_stats_20201103.txt") %>%
  filter(county_desc %in% c("MECKLENBURG", "COLUMBUS", "DURHAM")) %>% 
  group_by(precinct_abbrv) %>%
  mutate(voter_reg=sum(total_voters),  
         CTY_N=county_desc, 
         PREC_ID=precinct_abbrv) %>%
  select(
    CTY_N, 
    PREC_ID, 
    voter_reg
  )


data <- left_join(nc_shp, ncreg, by=c("CTY_N", "PREC_ID"))
                  

data <- unique(data)


#Pull RDA VTD and demographic files

demographic_data_NC.v04 <-
  read.csv(
    "../DATA/Demographic_Data/Demographic_Data_NC.v04/demographic_data_NC.v04.csv"
  ) %>% 
  transmute(
    GEOID20,
    Name,
    tot_pop20 = T_20_CENS_Total,
    white_pop20 = T_20_CENS_White,
    hisp_pop20 = T_20_CENS_Hispanic,
    black_pop20 = T_20_CENS_Black,
    asian_pop20 = T_20_CENS_Asian,
    native_pop20 = T_20_CENS_Native,
    pacific_pop20 = T_20_CENS_Pacific,
    tot_cvap20 = V_20_VAP_Total,
    white_cvap20 = V_20_VAP_White,
    hisp_cvap20 = V_20_VAP_Hispanic,
    black_cvap20 = V_20_VAP_Black,
    asian_cvap20 = V_20_VAP_Asian,
    native_cvap20 = V_20_VAP_Native,
    pacific_cvap20 = V_20_VAP_Pacific
  )

election_data_NC.v05 <-
  read.csv(
    "../DATA/Election_Data/Election_Data_NC.v05/election_data_NC.v05.csv")


#Join DRA then DRA VTD
DRA <-
  merge(
    election_data_NC.v05,
    demographic_data_NC.v04,
    by = c("GEOID20", "Name"),
    all = TRUE
  ) %>% rename(NAME = Name)


VTDRA <-
  left_join(data,
            DRA,
            by = c("ENR_DESC" = "NAME"))


#Reduce columns
VTDRAr_nc <- VTDRA %>%
  mutate(
    GEOID = GEOID20,
    white_pct20 = trunc(white_cvap20  / tot_cvap20 * 100),
    hisp_pct20 =  trunc(hisp_cvap20  / tot_cvap20  * 100),
    black_pct20 = trunc(black_cvap20  / tot_cvap20  * 100),
    api_pct20 = trunc((asian_cvap20  + pacific_cvap20 ) / tot_cvap20 * 100),
    native_pct20 = trunc(native_cvap20  / tot_cvap20 * 100),
    maj_race = case_when(
      white_pct20 > 49 ~ "White",
      black_pct20 > 49 ~ "Black",
      hisp_pct20 > 49 ~ "Hispanic",
      api_pct20 > 49 ~ "Asian/Pacific Islander",
      native_pct20 > 49 ~ "Native",
      TRUE ~ "Other"
    ),
    state = "NC") %>% 
  rename(
    PREC_ID1 = PREC_ID,
    NAME = ENR_DESC,
    county = CTY_N
  ) %>% 
  select(
    GEOID,
    CTY_ID,
    county,
    PREC_ID1,
    NAME,
    state,
    G20PRERTRU,
    G20PREDBID,
    G20PREO,
    G20votes,
    maj_race,
    tot_cvap20, 
    voter_reg, 
    geometry
  )


#Pull census block cvap shapefiles
nc_2020_CVAP_blockr <-
  st_read(dsn = "../DATA/2020 CVAP/nc_cvap_2020_2020_b/nc_cvap_2020_2020_b.shp") %>%
  subset(COUNTYFP20 %in% c("47", "63", "119")) %>%
  select(COUNTYFP20, GEOID20) %>%
  mutate(GEOID=GEOID20)

#Pull L2 turnout block files
NC_l2_turnout_stats_block20 <-
  read_csv(
    "../DATA/l2_turnout_2020blockAgg/NC_l2_turnout_2020blockAgg/NC_l2_turnout_stats_block20.csv", col_types = cols(geoid20 = col_character())
  )%>%
  rename(GEOID20 = geoid20) %>% 
  select(
    GEOID20,
    total_reg
  )


#Join L2 and census block files
blkl2 <-
  merge(
    nc_2020_CVAP_blockr,
    NC_l2_turnout_stats_block20,
    by = "GEOID20",
    all.x = TRUE
  )


#geomatch
#From CK
matches <- geo_match(from = blkl2, to = VTDRAr_nc, method = 'centroid')

prec <- block2prec(block_table = blkl2, matches = matches)

fill_zero_rows <- lapply(names(prec)[-c(1:2)], function(x)
  0) |>
  setNames(names(prec)[-c(1:2)])

prec <- complete(prec,
                 matches_id = seq_len(nrow(VTDRAr_nc)),
                 fill = fill_zero_rows)

NCfull_temp1 <- bind_cols(VTDRAr_nc, prec) %>% 
  select(-matches_id, - total_reg)


NC_2020_prov_vf_dcm <- read.csv("../DATA/2016_20_pv_abs_data/NC_2020_prov_vf.csv")

nc_vf_20 <- read_delim("../DATA/2016_20_pv_abs_data/nc_vf_2020.txt") %>% 
  filter(county_desc %in% c("MECKLENBURG", "COLUMBUS", "DURHAM")) %>% 
  group_by(county_desc, precinct_abbrv, voting_method_desc) %>% 
  summarise(votes = n()) %>% 
  pivot_wider(names_from = voting_method_desc, values_from = votes)

nc_max_reason_df <- NC_2020_prov_vf_dcm %>%
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
      not_counted_reason == "ID NOT PROVIDED" ~ "ID NOT PROVIDED",
      not_counted_reason == "MOVED OUT OF COUNTY MORE THAN 30 DAYS" ~ "MOVED OUT OF COUNTY MORE THAN 30 DAYS",
      not_counted_reason == "NOT ELIGIBLE TO VOTE IN CURRENT ELECTION" ~ "NOT ELIGIBLE TO VOTE IN CURRENT ELECTION",
      not_counted_reason == "NOT REGISTERED" ~ "NOT REGISTERED",
      not_counted_reason == "PROVISIONAL APPLICATION INCOMPLETE/ILLEGIBLE" ~ "PROVISIONAL APPLICATION INCOMPLETE/ILLEGIBLE",
      not_counted_reason == "REGISTRATION AFTER DEADLINE" ~ "REGISTRATION AFTER DEADLINE",
      not_counted_reason == "REMOVED" ~ "REMOVED",
      not_counted_reason == "VOTER ALREADY VOTED" ~ "VOTER ALREADY VOTED",
      not_counted_reason == "VOTING OUT OF PRECINCT" ~ "VOTING OUT OF PRECINCT",
      TRUE ~ "Blank"
    )
  )

nc_pv_20 <- NC_2020_prov_vf_dcm %>% 
  filter(county_name %in% c("MECKLENBURG", "COLUMBUS", "DURHAM")) %>% 
  group_by(county_name, voted_pct_lbl, pv_status) %>% 
  summarise(n = n(), .groups = "drop") %>% 
  pivot_wider(names_from = pv_status, values_from = n) %>% 
  mutate(across(everything(), ~ replace_na(.x, 0))) %>%
  left_join(nc_vf_20, by = c("county_name" = "county_desc", "voted_pct_lbl" = "precinct_abbrv")) %>% 
  transmute(
    county = county_name,
    PREC_ID1 = voted_pct_lbl,
    pv_rej = `NOT COUNTED`,
    rej_denom = `ABSENTEE BY MAIL` + `ABSENTEE CURBSIDE` + `ABSENTEE ONESTOP` + `PROVISIONAL`,
    rej_pct = (pv_rej/rej_denom)*100
  )

nc_reasons_sums <- NC_2020_prov_vf_dcm %>% 
  subset(county_name %in% c("MECKLENBURG", "COLUMBUS", "DURHAM")) %>%
  group_by(county_name, not_counted_reason) %>% 
  summarise(total = n())%>% 
  mutate(percentage = round(total / sum(total)*100, 2)) %>% 
  arrange(desc(total), .by_group = TRUE)

NCfull_temp2 <- left_join(NCfull_temp1, nc_pv_20, by = c("county","PREC_ID1"))%>% 
  mutate(ballots_cast = pv_rej + G20votes,
         rej_binned = factor(ntile(rej_pct, 3),
                             levels = 1:3,
                             labels = c("Lower 3rd",
                                        "Middle 3rd",
                                        "Upper 3rd")),
         voter_tnt = (G20votes/voter_reg)*100,
         voter_tnt = case_when(
           voter_tnt > 100 ~ NA_real_, voter_tnt == 0 ~ NA_real_, TRUE ~ voter_tnt ),
         voter_tnt_cvap = (G20votes / tot_cvap20) * 100, 
         tot_cvap=tot_cvap20, 
         voter_tnt_cvap = if_else(voter_tnt_cvap >= 99.9 | voter_tnt_cvap == 0, NA_real_, voter_tnt_cvap),
         max_reason = NA_real_)%>% 
  select(
    GEOID,
    county,
    state,
    CTY_ID,
    PREC_ID1,
    NAME,
    G20PRERTRU,
    G20PREDBID,
    G20PREO,
    G20votes,
    voter_tnt,
    voter_tnt_cvap,
    pv_rej,
    ballots_cast,
    tot_cvap,  
    rej_pct,
    rej_binned,
    maj_race,
    voter_reg,
    max_reason
  )

NCfull <- NCfull_temp2 %>% 
  select(
    GEOID,
    county,
    state,
    CTY_ID,
    PREC_ID1,
    NAME,
    G20PRERTRU,
    G20PREDBID,
    G20PREO,
    G20votes,
    voter_tnt,
    voter_tnt_cvap,
    pv_rej,
    ballots_cast,
    tot_cvap, 
    rej_pct,
    rej_binned,
    maj_race,
    voter_reg
  )

#write_csv(NCfull, "Data/full_datasets/NCfull_20.csv")

# #create shapefile
st_write(
  NCfull,
  "../DATA/Storymapfiles/NC_2020_shp.shp")
##-------------------------------------Ohio-----------------------------------

#shape files
oh_2020_vtd_raw <-
  st_read("../DATA/dataverse_files_2020_VTD_VEST/oh_2020/oh_2020.shp")

oh_2020_vtd <- oh_2020_vtd_raw %>%
  filter(COUNTYFP20 %in% c("035", "093")) %>% 
  mutate(
    CTY_ID = COUNTYFP20,
    PREC_ID1 = VTDST20,
    PREC_ID2 = PRECINCT20,
    PREC_ID3 = GEOID20,
    PREC_ID4 = NAME20,
    G20PRERTRU,
    G20PREDBID,
    G20PREO = G20PRELJOR + G20PREGHAW,
    G20votes = G20PRERTRU + G20PREDBID + G20PREO,
    geometry
  )

#ballot rejection files
#Lorain county
oh_2020_lorain_pv <-
  read_csv("../DATA/2016_20_pv_abs_data/OH_2020_Lorain_pv.csv") %>%
  rename(PREC_ID4 = ...6) %>%
  mutate(#match outcome variables for other county
    pv_rej = numunc,
    prov = numapps,
    county = "Lorain") %>%
  select(county, PREC_ID4, pv_rej, prov)

#Cuyahoga county
oh_2020_cuypv <-
  read.csv("../DATA/2016_20_pv_abs_data/OH_2020_cuypv.csv", stringsAsFactors = TRUE) %>%
  mutate(
    pv_rej = TOT..REJ,
    prov = TOTAL,
    county = "Cuyahoga"
  ) %>%
  select(county,PREC_ID4, pv_rej, prov)

oh_2020_clpv <- bind_rows(oh_2020_lorain_pv, oh_2020_cuypv)

#Join VTD shp and ballot rejection
oh_2020_vdt_cl <-
  merge(oh_2020_vtd,
        oh_2020_clpv,
        by = c("PREC_ID4"),
        all.x = TRUE)

#Pull RDA VTD and demographic files
demographic_OH_v04 <-
  read_csv("../DATA/Demographic_Data/Demographic_Data_OH.v04/demographic_data_OH.v04.csv") %>% 
  transmute(
    GEOID20,
    Name,
    tot_pop20 = T_20_CENS_Total,
    white_pop20 = T_20_CENS_White,
    hisp_pop20 = T_20_CENS_Hispanic,
    black_pop20 = T_20_CENS_Black,
    asian_pop20 = T_20_CENS_Asian,
    native_pop20 = T_20_CENS_Native,
    pacific_pop20 = T_20_CENS_Pacific,
    tot_cvap20 = V_20_VAP_Total,
    white_cvap20 = V_20_VAP_White,
    hisp_cvap20 = V_20_VAP_Hispanic,
    black_cvap20 = V_20_VAP_Black,
    asian_cvap20 = V_20_VAP_Asian,
    native_cvap20 = V_20_VAP_Native,
    pacific_cvap20 = V_20_VAP_Pacific
  )

election_OH.v04 <-
  read.csv("../DATA/Election_Data/Election_Data_OH.v04/election_data_OH.v04.csv",
           stringsAsFactors = TRUE)

#Join DRA then DRA VTD
DRA <-
  merge(demographic_OH_v04,
        election_OH.v04,
        by = "GEOID20",
        all = TRUE)
VTDRA <-
  merge(oh_2020_vdt_cl,
        DRA,
        by.x = "PREC_ID3",
        by.y = "GEOID20",
        all.x = TRUE)

VTDRA_oh <- VTDRA %>%
  mutate(
    GEOID = GEOID20,
    vaptnt20 = trunc(E_20_PRES_Total / ((tot_pop20  + tot_cvap20 ) / 2) * 100),
    white_pct20 = trunc(white_cvap20  / tot_cvap20 * 100),
    hisp_pct20 =  trunc(hisp_cvap20  / tot_cvap20  * 100),
    black_pct20 = trunc(black_cvap20  / tot_cvap20  * 100),
    api_pct20 = trunc((asian_cvap20  + pacific_cvap20 ) / tot_cvap20 * 100),
    native_pct20 = trunc(native_cvap20  / tot_cvap20 * 100),
    maj_race = case_when(
      white_pct20 > 49 ~ "White",
      black_pct20 > 49 ~ "Black",
      hisp_pct20 > 49 ~ "Hispanic",
      api_pct20 > 49 ~ "Asian/Pacific Islander",
      native_pct20 > 49 ~ "Native",
      TRUE ~ "Other"
    ),
    state = "OH") %>% 
  rename(NAME = PREC_ID4) %>% 
  select(
    GEOID,
    CTY_ID,
    county,
    PREC_ID1,
    NAME,
    state,
    G20PRERTRU,
    G20PREDBID,
    G20PREO,
    G20votes,
    maj_race,
    pv_rej,
    prov, 
    tot_cvap20
  )
# 
#Pull census block cvap shapefiles
oh_2020_CVAP_block<-st_read(dsn = "../DATA/2020 CVAP/oh_cvap_2020_2020_b/oh_cvap_2020_2020_b.shp") %>%
  subset(COUNTYFP20 %in% c("35","93"))
#Reduce
oh_2020_CVAP_blockr<-select(oh_2020_CVAP_block, GEOID20)

#Pull L2 turnout block files
OH_l2_turnout_stats_block20 <- read_csv("../DATA/l2_turnout_2020blockAgg/OH_l2_turnout_2020blockAgg/OH_l2_turnout_stats_block20.csv")%>%
  rename(GEOID20 = geoid20) %>%
  mutate(GEOID20 = as.character(GEOID20)) %>%
  select(
    GEOID20,
    total_reg
  )


#Join L2 and census block files
blkl2<-left_join(oh_2020_CVAP_blockr, OH_l2_turnout_stats_block20, by="GEOID20")

#Geomatch
matches <- geo_match(from = blkl2, to = VTDRA_oh, method = 'centroid')
#Then we aggregate this up to the precinct level with block2prec.
prec <- block2prec(block_table = blkl2, matches = matches)
#the data is arranged by the order of the second argument (to/precinct), so we can column bind them to have a full dataset for analysis purposes.
OHfull_temp <- bind_cols(VTDRA_oh, prec) %>% 
  mutate(
    voter_reg = total_reg,
    ballots_cast = G20votes,
    rej_pct = (pv_rej/prov)*100,
    rej_binned = factor(ntile(rej_pct, 3),
                        levels = 1:3,
                        labels = c("Lower 3rd",
                                   "Middle 3rd",
                                   "Upper 3rd")),
    voter_tnt = (G20votes/voter_reg)*100,
    voter_tnt = case_when(
      voter_tnt > 100 ~ NA_real_,
      voter_tnt == 0 ~NA_real_,
      TRUE ~ voter_tnt
    ),
    voter_tnt_cvap = (G20votes / tot_cvap20) * 100, 
    voter_tnt_cvap = if_else(voter_tnt_cvap >= 99.9 | voter_tnt_cvap == 0, NA_real_, voter_tnt_cvap), 
    tot_cvap=tot_cvap20) %>% 
  select(
    GEOID,
    county,
    state,
    CTY_ID,
    PREC_ID1,
    NAME,
    G20PRERTRU,
    G20PREDBID,
    G20PREO,
    G20votes,
    voter_tnt,
    voter_tnt_cvap,
    pv_rej,
    ballots_cast,
    tot_cvap, 
    rej_pct,
    rej_binned,
    maj_race,
    voter_reg
  ) %>% 
  filter(PREC_ID1 != "ZZZZZZ") %>% 
  mutate(county = case_when(
    PREC_ID1 == "BEACHWOOD-00-C" ~ "Cuyahoga",
    PREC_ID1 == "LORAIN CITY 2-F" ~ "Lorain",
    PREC_ID1 == "ROCHESTER TWP & VILL" ~ "Lorain",
    PREC_ID1 == "SHEFFIELD TWP EAST" ~ "Lorain",
    PREC_ID1 == "SHEFFIELD TWP NORTH" ~ "Lorain",
    PREC_ID1 == "SHEFFIELD TWP WEST" ~ "Lorain",
    PREC_ID1 == "SHEFFIELD VILL #1" ~ "Lorain",
    PREC_ID1 == "SHEFFIELD VILL #2" ~ "Lorain",
    PREC_ID1 == "SHEFFIELD VILL #3" ~ "Lorain",
    PREC_ID1 == "WELLINGTON TWP" ~ "Lorain",
    PREC_ID1 == "WELLINGTON VILL NORTH"~ "Lorain",
    PREC_ID1 == "WELLINGTON VILL SOUTH"~ "Lorain",
    PREC_ID1 == "N. RIDGEVILLE 3-H"~ "Lorain",
    TRUE ~ county
  ))



erie <- st_read("../DATA/Lake_erie/hydro_p_LakeErie.shp") %>%
  st_transform(., crs = 4269)


OHfull <- st_join(OHfull_temp, erie) %>%
  filter(is.na(NAMEEN))%>% 
  select(
    GEOID,
    county,
    state,
    CTY_ID,
    PREC_ID1,
    NAME,
    G20PRERTRU,
    G20PREDBID,
    G20PREO,
    G20votes,
    voter_tnt,
    voter_tnt_cvap,
    pv_rej,
    ballots_cast,
    tot_cvap, 
    rej_pct,
    rej_binned,
    maj_race,
    voter_reg
  )

#create shapefile
st_write(
  OHfull,
  "../DATA/Storymapfiles/OH_2020_shp.shp")

##----------------------------------Pennsylvania-------------------------------


###PA DOS 2020 voter registration data 

pa_reg_data <- read.table("../DATA/2020_results_data/PA_2020_voter_registration.txt", header=FALSE, sep=",") %>%
  mutate(county=V3, 
         prec_id=V4, 
         reg_dems=V7, 
         reg_reps=V10, 
         reg_other=V13, 
         VTD=V35) %>%
  group_by(prec_id, county) %>% 
  mutate(total_reg=reg_dems + reg_reps + reg_other, 
         VTD=paste0("00", VTD)) %>% 
  select(
   county,
   VTD, 
   prec_id, 
   total_reg
 )
  
pa_reg_prec <- pa_reg_data %>%
  mutate(county_fips=ifelse(county=="2", "003", "101")) %>%
  filter(county=="2" | county=="51") %>% 
  mutate(county_name=ifelse(county=="2", "Allegheny", "Philadelphia")) %>% 
  select(
    county_fips,
    county,
    county_name,
    VTD,
    prec_id,
    total_reg
  )



#Pull state VTD shp files (from VEST w G20 data)
pa_2020_VTD <-
  st_read("../DATA/dataverse_files_2020_VTD_VEST/pa_2020/pa_2020.shp") %>%
  st_zm() 

pa_2020_VTDap_temp <-
  subset(pa_2020_VTD, COUNTYFP %in% c("003", "101")) %>%
  mutate(G20PREO = G20PRELJOR,
         G20votes = G20PREDBID + G20PRERTRU + G20PREO,
         county = case_when(
           COUNTYFP == "003" ~ "Allegheny",
           COUNTYFP == "101" ~ "Philadelphia"
         )) %>% 
  select(COUNTYFP, county,VTDST, NAME, G20PREDBID, G20PRERTRU, G20PREO, G20votes)


pa_2020_VTDap_temp1 <- 
  left_join(pa_2020_VTDap_temp, pa_reg_prec, by=c("VTDST"="VTD", "COUNTYFP"="county_fips")) %>%
  select(-county.y, -county.x) 


#There are duplicate names in the VTD file
# duplicated_rows <- pa_2020_VTDap |>
#   group_by(NAME) |>
#   filter(n() > 1) |>
#   ungroup()

#only two need to be changed, Baldwin Districts 1 and 2
pa_2020_VTDap_temp1$NAME[pa_2020_VTDap_temp1$VTDST == '000151'] <-
  "BALDWIN TP DIST 1"
pa_2020_VTDap_temp1$NAME[pa_2020_VTDap_temp1$VTDST == '000161'] <-
  "BALDWIN TP DIST 2"

# Pull ballot rejection files (and join counties/files if necessary)
#pa_2020_part <-
  #read.csv("../DATA/2016_20_pv_abs_data/pa_2020_part.csv", stringsAsFactors = TRUE)
pa_2020_rejections <-
  read.csv("../DATA/2016_20_pv_abs_data/pa_2020_rejections.csv", stringsAsFactors = TRUE) %>% 
  #mutate(pv_rej = Grand.Total)
  mutate(pv_rej=Grand.Total) 

#total absentee cast
pa_2020_cast <- 
  read_excel("../DATA/2016_20_pv_abs_data/pa_2020_general.xlsx")

names(pa_2020_cast)<-str_replace_all(names(pa_2020_cast), c(" " = "." , "," = "", "-"="." ))
names(pa_2020_cast)[8] <- "Biden"
names(pa_2020_cast)[11] <- "Writein"



pa_2020_cast <- pa_2020_cast %>%
  rowwise() %>%
  mutate(
    ab_cast = ifelse(
      TYPE=="M", 
      sum(c_across(`Biden`:`Writein`)), 0
    )
  )%>%
  ungroup()



pa_reasons_sums <- pa_2020_rejections %>%
  select(-Division, -Grand.Total) %>%
  summarise(across(everything(), ~ sum(.x, na.rm = TRUE))) %>% 
  pivot_longer(
    everything(),
    names_to = "Rejection_Reason",
    values_to = "total"
  ) %>% 
  mutate(percentage = round(total / sum(total)*100, 2),
         Rejection_Reason = str_replace_all(Rejection_Reason, "\\.", " ")) %>%
  arrange(desc(total), .by_group = TRUE)


pa_2020_ab <- pa_2020_cast[pa_2020_cast$TYPE=="M",] 



#Calculate rejection rates and number rejected ballots
pa_2020_pv_raw <- pa_2020_rejections %>%
  full_join(pa_2020_ab, by = c("Division"="DIV")) %>%
  mutate(rej_denom = ab_cast)

pa_2020_pv_temp <-
  pa_2020_rejections %>%
  mutate_all( ~ replace_na(., 0)) %>%
  mutate(
    VTDST = str_pad(
      str_replace_all(Division, "-", ""),
      width = 6,
      pad = "0"
    ),
    COUNTYFP = "101"
  ) %>%
  select(COUNTYFP, VTDST, pv_rej) 

pa_2020_pv_temp <- pa_2020_pv_raw %>%
  rowwise() %>%
  mutate(
    max_reason = NA_real_
  ) %>%
  ungroup() %>%
  mutate(
    VTDST = str_pad(
      str_replace_all(Division, "-", ""),
      width = 6,
      pad = "0"
    ),
    COUNTYFP = "101"
  ) %>%
  select(COUNTYFP, VTDST, pv_rej, rej_denom, max_reason) 


pa_2020ap_pv_temp <-
  read.csv("../DATA/2016_20_pv_abs_data/pa_2020ap_pv.csv", stringsAsFactors = TRUE) %>%
  mutate(COUNTYFP = case_when(
    COUNTYFP == 3 ~ "003",
    COUNTYFP == 101 ~ "101"
  )) %>% 
  select(COUNTYFP, VTDST, NAME) 



pa_2020ap_pv_temp2 <- left_join(pa_2020ap_pv_temp, pa_2020_pv_temp, by = c("COUNTYFP", "VTDST")) 


pa_2020_VTDap_temp2 <-
  merge(
    pa_2020_VTDap_temp1,
    pa_2020ap_pv_temp2,
    by = c("COUNTYFP", "VTDST"),
    all.x = TRUE
  ) %>%
  mutate(NAME=NAME.x) %>%
  select(-NAME.x)


#Pull RDA VTD and demographic files
demographic_data_PA.v04 <-
  read.csv("../DATA/Demographic_Data/Demographic_Data_PA.v04/demographic_data_PA.v04.csv",
           stringsAsFactors = TRUE) %>% 
  transmute(
    GEOID20,
    Name,
    tot_pop20 = T_20_CENS_Total,
    white_pop20 = T_20_CENS_White,
    hisp_pop20 = T_20_CENS_Hispanic,
    black_pop20 = T_20_CENS_Black,
    asian_pop20 = T_20_CENS_Asian,
    native_pop20 = T_20_CENS_Native,
    pacific_pop20 = T_20_CENS_Pacific,
    tot_cvap20 = V_20_VAP_Total,
    white_cvap20 = V_20_VAP_White,
    hisp_cvap20 = V_20_VAP_Hispanic,
    black_cvap20 = V_20_VAP_Black,
    asian_cvap20 = V_20_VAP_Asian,
    native_cvap20 = V_20_VAP_Native,
    pacific_cvap20 = V_20_VAP_Pacific
  )

election_data_PA.v05 <-
  read.csv("../DATA/Election_Data/Election_Data_PA.v05/election_data_PA.v05.csv",
           stringsAsFactors = TRUE)

#Join DRA then DRA VTD
DRA <-
  full_join(demographic_data_PA.v04,
            election_data_PA.v05,
            by = c("GEOID20", "Name")) %>%
  rename(NAME = Name)



geojoin <-
  read.csv("../DATA/Election_Data/Election_Data_PA.v05/election_data_PA.v05tojoin.csv",
           stringsAsFactors = TRUE)
geojoin <- geojoin %>%
  rename(NAME = Name)

#has rej_denom col and looks right 
pa_2020_VTDap <- left_join(pa_2020_VTDap_temp2, geojoin, by = "NAME")

#Join
#Let's see if we stick with 3,028 entries
VTDRA <- left_join(pa_2020_VTDap, DRA, by = "GEOID20")%>%
  rename(NAME=NAME.x)
VTDRA <- VTDRA[!is.na(VTDRA$GEOID20), ]
VTDRA <- VTDRA[!duplicated(VTDRA[, 1]),]

VTDRAr_pa <- VTDRA %>%
  mutate(
    GEOID = GEOID20,
    vaptnt20 = trunc(E_20_PRES_Total / ((tot_pop20  + tot_cvap20 ) / 2) * 100),
    white_pct20 = trunc(white_cvap20  / tot_cvap20 * 100),
    hisp_pct20 =  trunc(hisp_cvap20  / tot_cvap20  * 100),
    black_pct20 = trunc(black_cvap20  / tot_cvap20  * 100),
    api_pct20 = trunc((asian_cvap20  + pacific_cvap20 ) / tot_cvap20 * 100),
    native_pct20 = trunc(native_cvap20  / tot_cvap20 * 100),
    maj_race = case_when(
      white_pct20 > 49 ~ "White",
      black_pct20 > 49 ~ "Black",
      hisp_pct20 > 49 ~ "Hispanic",
      api_pct20 > 49 ~ "Asian/Pacific Islander",
      native_pct20 > 49 ~ "Native",
      TRUE ~ "Other"
    ),
    state = "PA", 
    county=if_else(COUNTYFP==101, "Philadelphia", "Allegheny")) %>% 
  rename(CTY_ID = COUNTYFP,
         PREC_ID1 = VTDST) %>% 
  select(
    GEOID,
    CTY_ID,
    county, 
    PREC_ID1,
    NAME,
    state,
    G20PRERTRU,
    G20PREDBID,
    G20PREO,
    G20votes,
    maj_race,
    tot_cvap20,
    pv_rej,
    rej_denom,
    max_reason, 
    total_reg
  )




#Pull census block cvap shapefiles
pa_2020_CVAP_block <-
  st_read(dsn = "../DATA/2020 CVAP/pa_cvap_2020_2020_b/pa_cvap_2020_2020_b.shp") %>%
  subset(COUNTYFP20 %in% c("3", "101"))

#Reduce
pa_2020_CVAP_blockr <- pa_2020_CVAP_block %>%
  select(COUNTYFP20, GEOID20) %>%
  st_zm(pa_2020_CVAP_blockr)
#Pull L2 turnout block files

PA_l2_turnout_stats_block20r <-
  read_csv("../DATA/l2_turnout_2020blockAgg/PA_l2_turnout_2020blockAgg/PA_l2_turnout_stats_block20.csv") %>%
  mutate(
    GEOID20 = as.character(geoid20)
  ) %>%
  select(
    GEOID20,
    #total_reg
  )

#Join L2 and census block files
blkl2 <-
  left_join(
    pa_2020_CVAP_blockr,
    PA_l2_turnout_stats_block20r,
    by = "GEOID20"
  )

#geomatch
#From CK
matches <- geo_match(from = blkl2, to = VTDRAr_pa, method = 'centroid')

prec <- block2prec(block_table = blkl2, matches = matches)

fill_zero_rows <- lapply(names(prec)[-c(1:2)], function(x)
  0) |>
  setNames(names(prec)[-c(1:2)])

prec <- complete(prec,
                 matches_id = seq_len(nrow(VTDRAr_pa)),
                 fill = fill_zero_rows)


VTDRAr_pa_temp <- bind_cols(VTDRAr_pa, prec) %>% 
  mutate(voter_reg = total_reg,
         ballots_cast = pv_rej + G20votes,
         voter_tnt = (G20votes/voter_reg)*100,
         voter_tnt = case_when(
           voter_tnt > 100 ~ NA_real_,
           voter_tnt == 0 ~NA_real_,
           TRUE ~ voter_tnt
         ),
         tot_cvap=tot_cvap20, 
         voter_tnt_cvap = (G20votes / tot_cvap20) * 100, 
         voter_tnt_cvap = if_else(voter_tnt_cvap >= 99.9 | voter_tnt_cvap == 0, NA_real_, voter_tnt_cvap),
         rej_pct = (pv_rej/rej_denom) * 100,
         rej_binned = factor(ntile(rej_pct, 3),
                             levels = 1:3,
                             labels = c("Lower 3rd",
                                        "Middle 3rd",
                                        "Upper 3rd")
         ))%>% 
  select(
    GEOID,
    county,
    state,
    CTY_ID,
    PREC_ID1,
    NAME,
    G20PRERTRU,
    G20PREDBID,
    G20PREO,
    G20votes,
    voter_tnt,
    voter_tnt_cvap,
    pv_rej,
    rej_denom, 
    ballots_cast,
    tot_cvap, 
    rej_pct,
    rej_binned,
    maj_race,
    voter_reg,
    max_reason
  )



PAfull <- VTDRAr_pa_temp %>% 
  select(
    GEOID,
    county,
    state,
    CTY_ID,
    PREC_ID1,
    NAME,
    G20PRERTRU,
    G20PREDBID,
    G20PREO,
    G20votes,
    voter_tnt,
    voter_tnt_cvap,
    pv_rej,
    ballots_cast,
    tot_cvap,
    rej_pct,
    rej_binned,
    maj_race,
    voter_reg
  )

#
#write_csv(PAfull, "../DATA/full_datasets/PAfull_20.csv")


#Create shapefile
# st_write(
#  PAfull,
# "../DATA/Storymapfiles/PA_2020_shp.shp")
##---------------------------------Wisconsin------------------------------------

wi_2020_VTDm <-
  st_read(dsn = "../DATA/dataverse_files_2020_VTD_VEST/wi_2020/wi_2020.shp") %>%  
  subset(CNTY_NAME %in% c("MILWAUKEE"))


wi_shp <- wi_2020_VTDm %>%
  mutate(
    NAME = LABEL,
    CTY_ID = CNTY_FIPS,
    CTY_NAME = CNTY_NAME,
    PREC_ID1 = WARDID,
    NAME = LABEL,
    G20PREO = G20PRELJOR + G20PRECBLA + G20PREOWRI,
    G20votes = G20PRERTRU + G20PREDBID + G20PREO
  ) %>%
  select(CTY_ID,
         CTY_NAME,
         PREC_ID1,
         NAME,
         RV,
         G20votes,
         G20PRERTRU,
         G20PREDBID,
         G20PREO,
         geometry)%>%
  mutate(NAME = case_when(
    NAME == "Fontana-On-Geneva Lake - V 0001" ~ "FontanaOnGenevaLake - V 0001",
    NAME == "Fontana-On-Geneva Lake - V 0002" ~ "FontanaOnGenevaLake - V 0002",
    NAME == "Fontana-On-Geneva Lake - V 0003" ~ "FontanaOnGenevaLake - V 0003",
    TRUE ~ NAME
  )) %>% 
  separate(NAME, into = c("NAME", "TEMP"), sep = "-") %>% 
  mutate(PREC_ID1.2 = str_extract(TEMP, "\\d+$")) %>% 
  select(-TEMP)

#Pull RDA VTD and demographic files
election_data_WI_v05 <-
  read.csv(
    "../DATA/Election_Data/Election_Data_WI.v05/election_data_WI.v05.csv",
    stringsAsFactors = TRUE
  )
demographic_data_WI_v04 <-
  read.csv(
    "../DATA/Demographic_Data/Demographic_Data_WI.v04/demographic_data_WI.v04.csv",
    stringsAsFactors = TRUE
  ) %>% 
  transmute(
    GEOID20,
    Name,
    tot_pop20 = T_20_CENS_Total,
    white_pop20 = T_20_CENS_White,
    hisp_pop20 = T_20_CENS_Hispanic,
    black_pop20 = T_20_CENS_Black,
    asian_pop20 = T_20_CENS_Asian,
    native_pop20 = T_20_CENS_Native,
    pacific_pop20 = T_20_CENS_Pacific,
    tot_cvap20 = V_20_VAP_Total,
    white_cvap20 = V_20_VAP_White,
    hisp_cvap20 = V_20_VAP_Hispanic,
    black_cvap20 = V_20_VAP_Black,
    asian_cvap20 = V_20_VAP_Asian,
    native_cvap20 = V_20_VAP_Native,
    pacific_cvap20 = V_20_VAP_Pacific
  )

#Join DRA then DRA VTD
DRA <-
  full_join(
    election_data_WI_v05,
    demographic_data_WI_v04,
    by = c("GEOID20", "Name")
  )%>%
  mutate(Name = case_when(
    Name == "Fontana-On-Geneva Lake - V 0001" ~ "FontanaOnGenevaLake - V 0001",
    Name == "Fontana-On-Geneva Lake - V 0002" ~ "FontanaOnGenevaLake - V 0002",
    Name == "Fontana-On-Geneva Lake - V 0003" ~ "FontanaOnGenevaLake - V 0003",
    TRUE ~ Name
  )) %>% 
  separate(Name, into = c("NAME", "TEMP"), sep = "-") %>% 
  mutate(PREC_ID1 = str_extract(TEMP, "\\d+$"),
         NAME = toupper(NAME)) %>% 
  select(-TEMP)


VTDRAr_wi <- left_join(wi_shp, DRA, by = c("NAME", "PREC_ID1")) %>% 
  mutate(
    GEOID = as.character(GEOID20),
    vaptnt20 = trunc(E_20_PRES_Total / ((tot_pop20  + tot_cvap20 ) / 2) * 100),
    white_pct20 = trunc(white_cvap20  / tot_cvap20 * 100),
    hisp_pct20 =  trunc(hisp_cvap20  / tot_cvap20  * 100),
    black_pct20 = trunc(black_cvap20  / tot_cvap20  * 100),
    api_pct20 = trunc((asian_cvap20  + pacific_cvap20 ) / tot_cvap20 * 100),
    native_pct20 = trunc(native_cvap20  / tot_cvap20 * 100),
    maj_race = case_when(
      white_pct20 > 49 ~ "White",
      black_pct20 > 49 ~ "Black",
      hisp_pct20 > 49 ~ "Hispanic",
      api_pct20 > 49 ~ "Asian/Pacific Islander",
      native_pct20 > 49 ~ "Native",
      TRUE ~ "Other"
    ),
    state = "WI") %>% 
  select(
    GEOID,
    CTY_ID,
    NAME,
    PREC_ID1,
    state,
    G20PRERTRU,
    G20PREDBID,
    G20PREO,
    G20votes,
    tot_cvap20,
    maj_race,
    RV
  )
#View(VTDRAr)
# ggplot(VTDRAr)+
# #  geom_sf()

#Pull census block cvap shapefiles
wi_2020_CVAP_blockr <-
  st_read(dsn = "../DATA/2020 CVAP/wi_cvap_2020_2020_b/wi_cvap_2020_2020_b.shp") %>%
  subset(COUNTYFP20 %in% c("79")) %>%
  select(COUNTYFP20, GEOID20)

#Pull L2 turnout block files
WI_l2_turnout_stats_block20 <-
  read_csv(
    "../DATA/l2_turnout_2020blockAgg/WI_l2_turnout_2020blockAgg/WI_l2_turnout_stats_block20.csv", col_types = cols(geoid20 = col_character())
  ) %>%
  rename(GEOID20 = geoid20)

WI_l2_turnout_stats_block20 <-
  WI_l2_turnout_stats_block20[grepl("55079", WI_l2_turnout_stats_block20$GEOID20), ]


#Reduce
wi_l2_turnout_stats_block20r <-
  select(
    WI_l2_turnout_stats_block20,
    GEOID20,
    total_reg
  )

#Join L2 and census block files
blkl2 <-
  left_join(
    wi_2020_CVAP_blockr,
    wi_l2_turnout_stats_block20r,
    by = "GEOID20"
  )


#geomatch
#From CK
matches <- geo_match(from = blkl2, to = VTDRAr_wi, method = 'centroid')

prec <- block2prec(block_table = blkl2, matches = matches)

fill_zero_rows <- lapply(names(prec)[-c(1:2)], function(x)
  0) |>
  setNames(names(prec)[-c(1:2)])

prec <- complete(prec,
                 matches_id = seq_len(nrow(VTDRAr_wi)),
                 fill = fill_zero_rows)

full_wi <- bind_cols(VTDRAr_wi, prec)


WIfull <- full_wi %>% 
  mutate(county = "Milwaukee",
         pv_rej = NA_real_,
         rej_pct = NA_real_,
         rej_binned = NA_real_,
         ballots_cast = NA_real_,
         tot_cvap=tot_cvap20, 
         voter_reg= RV, 
         #voter_reg = total_reg,
         voter_tnt = (G20votes / voter_reg) * 100,
         voter_tnt = case_when(
           voter_tnt > 100 ~ NA_real_,
           voter_tnt == 0 ~ NA_real_,
           TRUE ~ voter_tnt
         ),
         voter_tnt_cvap = (G20votes / tot_cvap20) * 100, 
         voter_tnt_cvap = if_else(voter_tnt_cvap >= 99.9 | voter_tnt_cvap == 0, NA_real_, voter_tnt_cvap)) %>% 
  select(
    GEOID,
    county,
    state,
    CTY_ID,
    PREC_ID1,
    NAME,
    G20PRERTRU,
    G20PREDBID,
    G20PREO,
    G20votes,
    voter_tnt,
    voter_tnt_cvap,
    pv_rej,
    tot_cvap, 
    ballots_cast,
    rej_pct,
    rej_binned,
    maj_race,
    voter_reg
  )

#write_csv(WIfull, "Data/full_datasets/WIfull_20.csv")
# 
# #create shapefile
# st_write(
#   WIfull,
#   "../DATA/Storymapfiles/WI_2020_shp.shp")


##-------------------------------------Analysis------------------------------

#crs dont match, get the crs for everything
crs <- lapply(list(PAfull, NCfull, MIfull, GAfull, OHfull, WIfull, AZfull), st_crs)

harmonized_crs <- lapply(
  list(PAfull, NCfull, MIfull, GAfull, OHfull, WIfull, AZfull),
  function(obj) st_transform(st_zm(obj), crs = 4326)
) %>% 
  lapply(st_make_valid)

names(harmonized_crs) <- c("PAfull", "NCfull", "MIfull", "GAfull", "OHfull", "WIfull", "AZfull")

list2env(harmonized_crs, envir = .GlobalEnv)


full_20 <- rbind(PAfull, NCfull, MIfull, GAfull, OHfull, WIfull, AZfull) %>% 
  mutate(
    voter_tnt = if_else(is.na(pv_rej),
                        (G20votes / voter_reg) * 100,
                        ((G20votes + pv_rej) / voter_reg) * 100),
    voter_tnt = if_else(voter_tnt >= 99.9 | voter_tnt == 0, NA_real_, voter_tnt),
    voter_tnt_cvap = if_else(voter_tnt_cvap >= 99.9 | voter_tnt_cvap == 0, NA_real_, voter_tnt_cvap),
    rej_binned = factor(
      ntile(rej_pct, 3),
      levels = 1:3,
      labels = c("Lower 3rd", "Middle 3rd", "Upper 3rd")
    )
  ) %>% 
  select(GEOID,
         CTY_ID,
         county,
         PREC_ID1,
         state,
         NAME,
         maj_race,
         G20votes,
         voter_reg,
         voter_tnt,
         voter_tnt_cvap,
         pv_rej,
         tot_cvap, 
         ballots_cast,
         rej_pct,
         rej_binned
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
    ),
    county = case_when(
      PREC_ID1 == "BEACHWOOD-00-C" ~ "Cuyahoga",
      is.na(county) ~ "Lorain",
      TRUE ~ county
    )
  )

##CVAP Standardization 

#add county-level cvap 
full_20 <- full_20 %>%
  mutate(county_cvap_tnt=case_when(
    county=="MECKLENBURG" ~ 66.23, 
    county=="DURHAM" ~ 69.56, 
    county=="COLUMBUS" ~ 65.76, 
    county=="Maricopa" ~ 61.34, 
    county=="Fulton" ~ 62.98, 
    county=="Wayne" ~ 64.50, 
    county=="Cuyahoga" ~ 63.21, 
    county=="Lorain" ~ 64.75, 
    county=="Allegheny" ~ 71.98, 
    county=="Philadelphia" ~ 59.92, 
    county=="Milwaukee" ~ 63.66,   #source cvap for Milwaukee was way off--.2498%. Used ours. 
    TRUE ~ NA
  ))



#standardization process 
full_20_std <- full_20 %>%
  group_by(county) %>%
  mutate(
    precinct_cvap_mean=sum(voter_tnt_cvap, na.rm=TRUE)/n(), 
    scale_factor=county_cvap_tnt/precinct_cvap_mean, 
    precinct_cvap_std=voter_tnt_cvap * scale_factor) %>%
  ungroup() 


#Adjustment process 
full_20 <- full_20_std %>%
  mutate( 
         cvap_adj= ifelse(precinct_cvap_std > 100, 
                          tot_cvap + 10, tot_cvap), 
         precinct_cvap_std_adj= 100 * G20votes/cvap_adj) %>% 
  select(-county_cvap_tnt, -scale_factor, -precinct_cvap_mean, cvap_adj) 


write_sf(full_20, "../DATA/full_datasets/full_2020_revised.shp")


shapefile_path <- "../DATA/full_datasets/full_2020_revised.shp"


# Output directory
out_dir <- "../DATA/full_datasets/states20"
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


write_sf(full_20, "../DATA/full_datasets/full_2020_revised.shp")

full_20_csv <- full_20 %>% st_drop_geometry()

write_csv(full_20_csv, "../DATA/full_datasets/full_2020_revised.csv")

split_states <- split(full_20, full_20$state)

list2env(split_states, envir = .GlobalEnv)

purrr::walk2(split_states,
      names(split_states),
      ~ write_sf(.x, paste0(.y, "_2020__revised.shp"), append = FALSE))

##-------------------------------models and tables----------------------------
full_20 <- read_csv("../DATA/full_datasets/full_2020_revised.csv") %>% 
  mutate(maj_race = factor(
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
  ))

race_tnt_20 <- lm(voter_tnt ~ maj_race, data = full_20)

race_tnt_mixed_20 <- lmer(voter_tnt ~ maj_race + factor(state) + (1 | CTY_ID), data = full_20)


#calculate marginal and conditional r2 (not reported by lmer)
r.squaredGLMM(race_tnt_mixed_20)


race_tnt_cvap_20 <- lm(precinct_cvap_std_adj ~ maj_race, data = full_20)



race_tnt_cvap_mixed_20 <- lmer(precinct_cvap_std_adj ~ maj_race + factor(state) + (1 | CTY_ID), data = full_20)




#calculate marginal and conditional r2 (not reported by lmer)
r.squaredGLMM(race_tnt_cvap_mixed_20)


clean_reg_20 <- full_20 %>%
  filter(is.finite(rej_pct), !is.na(maj_race)) 

race_rej_20 <- lm(rej_pct ~ maj_race, data = clean_reg_20, na.action = na.omit)

race_rej_mixed_20 <- lmer(rej_pct ~ maj_race + (1 | CTY_ID), data = clean_reg_20)

r.squaredGLMM(race_rej_mixed_20)

modelsummary::modelsummary(
  list("Model 1" = race_tnt_20, 
       "Model 2" = race_tnt_mixed_20, 
       "Model 1 CVAP" = race_tnt_cvap_20,
       "Mpdel 2 CVAP" = race_tnt_cvap_mixed_20),
  output = "../Output/tnt_20_models_cvap.html",
  coef_rename = c(
    "(Intercept)" = "Intercept",
    "maj_raceBlack" = "Majority Black",
    "maj_raceHispanic" = "Majority Hispanic",
    "maj_raceAsian/Pacific Islander" = "Majority API",
    "maj_raceNative" = "Majority Native American",
    "maj_raceOther" = "Majority Other Race",
    "factor(state)GA" = "Georgia",
    "factor(state)MI" = "Michigan",
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
  list("OLS" = race_rej_20, "Mixed Effects" = race_rej_mixed_20),
  output = "../Output/rej_20_models_revised.html",
  coef_rename = c(
    "(Intercept)" = "Intercept",
    "maj_raceBlack" = "Majority Black",
    "maj_raceHispanic" = "Majority Hispanic",
    "maj_raceAsian/Pacific Islander" = "Majority API",
    "maj_raceNative" = "Majority Native American",
    "maj_raceOther" = "Majority Other Race"
  ),
  stars = TRUE,  # Adds significance stars
  gof_map = c("r.squared", "adj.r.squared", "nobs"),  # Include R-squared and observations
  title = "Regression Models Predicting Ballot Rejections",
  notes = "Significance levels: ***p<0.01, **p<0.05, *p<0.1"
)


race_means <- full_20 %>% 
  group_by(maj_race) %>% 
  summarise(
    n = n(),
    mean_tnt = round(mean(voter_tnt, na.rm = TRUE), digits = 2),
    med_tnt = round(median(voter_tnt, na.rm = TRUE),digits = 2),
    n_na = sum(is.na(voter_tnt)),
    #mean_tnt_cvap = round(mean(voter_tnt_cvap, na.rm = TRUE), digits = 2),
    mean_tnt_cvap = round(mean(precinct_cvap_std_adj, na.rm = TRUE), digits = 2),
    #med_tnt_cvap = round(median(voter_tnt_cvap, na.rm = TRUE), digits = 2),
    med_tnt_cvap = round(median(precinct_cvap_std_adj, na.rm = TRUE), digits = 2),
    #n_na_cvap = sum(is.na(voter_tnt_cvap))
    n_na_cvap = sum(is.na(precinct_cvap_std_adj))
    
  )%>% 
  st_drop_geometry()

race_means_html <- knitr::kable(race_means, format = "html")

writeLines(race_means_html, "../Output/race_means_20_revised.html")

county_tnt_means <- full_20 %>% 
  group_by(county, state) %>% 
  summarise(
    n = n(),
    mean_tnt = round(mean(voter_tnt, na.rm = TRUE), digits = 2),
    med_tnt = round(median(voter_tnt, na.rm = TRUE),digits = 2),
    n_na = sum(is.na(voter_tnt)),
    #mean_tnt_cvap = round(mean(voter_tnt_cvap, na.rm = TRUE), digits = 2),
    mean_tnt_cvap = round(mean(precinct_cvap_std_adj, na.rm = TRUE), digits = 2),
    #med_tnt_cvap = round(median(voter_tnt_cvap, na.rm = TRUE), digits = 2),
    med_tnt_cvap = round(median(precinct_cvap_std_adj, na.rm = TRUE), digits = 2),
    #n_na_cvap = sum(is.na(voter_tnt_cvap)),
    n_na_cvap = sum(is.na(precinct_cvap_std_adj)),
    .groups = "drop"
  ) %>% 
  arrange(state, county) %>% 
  st_drop_geometry()

county_tnt_means_html <- knitr::kable(county_tnt_means, format = "html")

writeLines(county_tnt_means_html, "../Output/county_tnt_means_20_revised.html")

county_rej_means <- full_20 %>% 
  group_by(county, state) %>% 
  summarise(
    n = n(),
    mean_rej = round(mean(rej_pct, na.rm = TRUE), digits = 2),
    med_rej = round(median(rej_pct, na.rm = TRUE), digits = 2),
    n_na = sum(is.na(rej_pct)),
    .groups = "drop"
  ) %>% 
  arrange(desc(mean_rej)) %>% 
  st_drop_geometry()

county_rej_means_html <- knitr::kable(county_rej_means, format = "html")

writeLines(county_rej_means_html, "../Output/county_rej_means_20_revised.html")

# 26163 Wayne MI

##----------------------------------plots--------------------------------------


race_tnt <- ggplot(full_20, aes(x = maj_race, y = precinct_cvap_std_adj)) +

  geom_jitter(width = 0.2, alpha = 0.3, color = "blue") +  
  geom_boxplot(alpha = 0.5, outlier.shape = NA) +  
  theme_minimal() +
  labs(y = "% CVAP Turnout", 
       x = "Precinct Racial Majority") 

ggsave(race_tnt, file = "../Output/racial_tnt_2020_revised.jpg", width = 8, height = 6, units = "in")

race_binned <- full_20 %>%
  filter(!is.na(rej_binned)) %>% #there's not actually na values according to my table call, but its making them up if i leave this out
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

ggsave(race_binned, file = "../Output/tercile_scatter_20_revised.jpg", width = 8, height = 6, units = "in")

race_bar <- full_20 %>%
  filter(!is.na(rej_binned)) %>%
  ggplot(aes(x = maj_race, fill = rej_binned)) +
  geom_bar(position = "fill") +
  labs(y = "Proportion of Precincts",
       x = "Precinct Racial Majority",
       fill = "Ballot Rejections") +
  scale_fill_manual(values = c("Lower 3rd" = "green",
                               "Middle 3rd" = "yellow",
                               "Upper 3rd" = "blue"))  +
  theme_dark(base_size = 12) + 
  theme(
    axis.text.x = element_text(
      size = 8,
      angle = 45,        # rotate labels 45 degrees
      hjust = 1,         # right-justify
      vjust = 1          # vertically align
    )
  )

ggsave(race_bar, file = "../Output/stacked_bar_20_revised.jpg", width = 8, height = 6, units = "in")

