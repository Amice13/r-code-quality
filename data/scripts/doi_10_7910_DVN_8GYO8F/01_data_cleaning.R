# install the required libraries if they are not already installed --------

# if (!require("rio")) install.packages("rio")
# if (!require("sjlabelled")) install.packages("sjlabelled")
# if (!require("tidyverse")) install.packages("tidyverse")

# load the required libraries ---------------------------------------------

library(rio) # A swiss-army knife for data i/o, cran 0.5.26
library(sjlabelled) # labelled data utility functions, cran 1.1.8
library(tidyverse) # easily install and load the 'tidyverse', cran 1.3.1

# create a sub-directory for data files to be generated, called data ------

if (!dir.exists("data")) dir.create(file.path(getwd(), "data"))

# generate three separate r data files ------------------------------------
# 1. deu_tidy.Rdata
# 2. gbr_tidy.Rdata
# 3. che_tidy.Rdata

##########################################################################
# soep v35 ###############################################################
##########################################################################

# get household data ------------------------------------------------------

household_a <- import("soepv35/hl.dta") %>%
  filter(syear >= 2007) %>%
  select(hid,                                  # current wave hh number
         syear,                                # survey year
         hlf0035,                              # dwelling with solar panels
         hlf0532,                              # dwelling with photovoltaic (fit)
         hlf0537,                              # dwelling with solarthermal (heating only)
         hlk0044                               # children under 16 Yr in household
  ) %>%
  sjlabelled::remove_all_labels()

household_b <- import("soepv35/hgen.dta") %>%
  filter(syear >= 2007) %>%
  select(hid,                                  # current wave hh number
         syear,                                # survey year
         hgnuts1,                              # nuts-systematic-1 (federal state)
         hghinc,                               # monthly household net income (EUR)
         hgowner                               # tenant or owner
  ) %>%
  sjlabelled::remove_all_labels()

df_household <- left_join(household_a, household_b, by = c("hid" = "hid", "syear" = "syear"))

# get personal data -------------------------------------------------------

personal_a <- import("soepv35/pl.dta") %>%
  filter(syear >= 2007) %>%
  select(pid,                                  # never changing person id
         hid,                                  # current wave hh number
         syear,                                # survey year
         plh0007,                              # interest in politics
         plh0004,                              # left-right position
         plh0011_h,                            # supports political party (yes / no)
         plh0013_h,                            # amount of support for political party
         plh0012_h,                            # political party, supported
         plh0036,                              # worried about environment 
         plh0037,                              # worried about the consequences from climate change
         plb0022_h                             # employment status        
  ) %>%
  sjlabelled::remove_all_labels()

personal_b <- import("soepv35/pgen.dta") %>%
  filter(syear >= 2007) %>%
  select(pid,                                  # never changing person id
         syear,                                # survey year
         pgisced97,                            # isced
         pgstib                                # occupational status
  ) %>%
  sjlabelled::remove_all_labels()

personal_c <- import("soepv35/pbrutto.dta") %>%
  filter(syear >= 2007) %>%
  select(pid,                                  # never changing person id
         syear,                                # survey year
         stell_h                               # relationship to the head of household
  ) %>%
  sjlabelled::remove_all_labels()

df_personal <- left_join(personal_a, personal_b, by = c("pid" = "pid", "syear" = "syear")) %>% 
               left_join(., personal_c, by = c("pid" = "pid", "syear" = "syear"))

# merge the two -----------------------------------------------------------

df_merged <- left_join(df_personal, df_household, by = c("hid" = "hid", "syear" = "syear"))

# clean the dataset -------------------------------------------------------

df_clean <- df_merged %>% 
  mutate(across(.cols = everything(), .fns = ~replace(., which(. < 0), NA_real_))) %>% 
  mutate(p_id = pid, 
         h_id = hid,
         wave = syear,
         region = recode(hgnuts1,
                         `1` = "Baden-Wuerttemberg",
                         `2` = "Bayern",
                         `3` = "Berlin",
                         `4` = "Brandenburg",
                         `5` = "Bremen",
                         `6` = "Hamburg",
                         `7` = "Hessen",
                         `8` = "Mecklenburg-Vorpommern",
                         `9` = "Niedersachsen",
                         `10` = "Nordrhein-Westfalen",
                         `11` = "Rheinland-Pfalz",
                         `12` = "Saarland",
                         `13` = "Sachsen",
                         `14` = "Sachsen-Anhalt",
                         `15` = "Schleswig-Holstein",
                         `16` = "Thueringen"),
         
         # dependent variables
         interest_politics = recode(plh0007, `4` = 1, `3` = 2, `2` = 3, `1` = 4),  
         left_right = plh0004,
         support_party = recode(plh0011_h, `2` = 0, `1` = 1),                           
         support_amount = recode(plh0013_h, `5` = 2, `4` = 3, `3` = 4, `2` = 5, `1` = 6), 
         support_amount = replace(support_amount, support_party == 0, 1),
         support_green = case_when(plh0012_h %in% c(5, 9, 15, 16, 18, 23) ~ 1,
                                   is.na(plh0012_h) ~ NA_real_, support_party == 0 ~ 0, TRUE ~ 0),
                  
         # additional dependent variables for the appendix
         worry_environment = recode(plh0036, `3` = 1, `2` = 2, `1` = 3),
         worry_climate = recode(plh0037, `3` = 1, `2` = 2, `1` = 3),
         
         # independent variable
         solar_panel = recode(hlf0035, `2` = 0, `1` = 1),
         
         # additional independent variables for the appendix
         solar_pv = recode(hlf0532, `2` = 0, `1` = 1),
         solar_st = recode(hlf0537, `2` = 0, `1` = 1),
         
         # control variables
         education = pgisced97,
         head = case_when(stell_h == 0 ~ 1, is.na(stell_h) ~ NA_real_, TRUE ~ 0),
         unemployed = case_when(pgstib == 12 ~ 1, is.na(pgstib) ~ NA_real_, TRUE ~ 0),
         income = hghinc,
         children = case_when(hlk0044 == 1 ~ 1, hlk0044 == 2 ~ 0, is.na(hlk0044) ~ NA_real_),
         owner = recode(hgowner, `5` = 0, `4` = 0, `3` = 0, `2` = 0, `1` = 1)) %>% 
  select(p_id, h_id, wave, region, 
         interest_politics, left_right, support_party, support_amount, support_green,
         worry_environment, worry_climate,
         solar_panel, 
         solar_pv, solar_st,
         education, head, unemployed, income, children, owner)
   
# save the clean dataset --------------------------------------------------

export(df_clean, "data/deu_tidy.Rdata")

##########################################################################
# ukhls 10th edition #####################################################
##########################################################################

# get household data ------------------------------------------------------

w_18h <- import("stata/stata13/bhps_w18/br_hhresp.dta") %>% 
  select(br_hidp,                         # household identifier
         br_gor_dv,                       # government office region
         br_solar1,                       # solar panels for electricity
         br_solar2,                       # solar water heating
         br_hhyneti,                      # annual household net income
         br_nkids_dv,                     # number of children in household
         br_hsownd_bh                     # house owned or rented
  ) %>%
  sjlabelled::remove_all_labels() %>% 
  rename(hidp = br_hidp, gor = br_gor_dv,
         solar1 = br_solar1, solar2 = br_solar2, 
         nkids = br_nkids_dv, hsownd = br_hsownd_bh) %>% 
  mutate(fihhmnnet1 = case_when(br_hhyneti < 0 ~ br_hhyneti,
                                br_hhyneti == 0 ~ 0,
                                br_hhyneti > 0 ~ br_hhyneti / 12),
         hrpid = NA_real_,
         wave = 2008) %>% 
  select(-br_hhyneti)

w_1h <- import("stata/stata13/ukhls_w1/a_hhresp.dta") %>% 
  select(a_hidp,                          # household identifier
         a_hrpid,                         # reference person id
         a_gor_dv,                        # government office region
         a_solar1,                        # solar panels for electricity
         a_solar2,                        # solar water heating
         a_fihhmnnet1_dv,                 # monthly household net income
         a_nkids_dv,                      # number of children in household
         a_hsownd                         # house owned or rented
  ) %>%
  sjlabelled::remove_all_labels() %>% 
  rename(hidp = a_hidp, gor = a_gor_dv,
         solar1 = a_solar1, solar2 = a_solar2, 
         fihhmnnet1 = a_fihhmnnet1_dv, nkids = a_nkids_dv, hsownd = a_hsownd,
         hrpid = a_hrpid) %>% 
  mutate(wave = 2009)

w_4h <- import("stata/stata13/ukhls_w4/d_hhresp.dta") %>% 
  select(d_hidp,                          # household identifier
         d_hrpid,                         # reference person id
         d_gor_dv,                        # government office region
         d_solar1,                        # solar panels for electricity
         d_solar2,                        # solar water heating
         d_fihhmnnet1_dv,                 # monthly household net income
         d_nkids_dv,                      # number of children in household
         d_hsownd                         # house owned or rented
  ) %>%
  sjlabelled::remove_all_labels() %>% 
  rename(hidp = d_hidp, gor = d_gor_dv, 
         solar1 = d_solar1, solar2 = d_solar2, 
         fihhmnnet1 = d_fihhmnnet1_dv, nkids = d_nkids_dv, hsownd = d_hsownd,
         hrpid = d_hrpid) %>% 
  mutate(wave = 2012)

df_household <- rbind(w_18h, w_1h, w_4h)

# get personal data -------------------------------------------------------

w_18i <- import("stata/stata13/bhps_w18/br_indresp.dta") %>% 
  select(pidp,                             # personal identifier
         br_hidp,                          # household identifier
         br_vote6,                         # level of interest in politics
         br_vote2,                         # closer to a party             
         br_vote4,                         # which political party closest to
         br_vote1,                         # supports a particular political party
         br_vote5,                         # strength of support for stated party
         br_hiqualb_dv,                    # highest qualification
         br_hoh,                           # head of household
         br_jbstat                         # job status
  ) %>%
  sjlabelled::remove_all_labels() %>%
  rename(hidp = br_hidp, 
         vote6 = br_vote6, vote2 = br_vote2, vote4 = br_vote4, vote1 = br_vote1, vote5 = br_vote5, 
         hiqual = br_hiqualb_dv, hoh = br_hoh, jbstat = br_jbstat) %>% 
  mutate(wave = 2008)

w_1i <- import("stata/stata13/ukhls_w1/a_indresp.dta") %>% 
  select(pidp,                             # personal identifier
         a_hidp,                           # household identifier
         a_vote6,                          # level of interest in politics
         a_vote2,                          # closer to a party  
         a_vote4,                          # which political party closest to
         a_vote1,                          # supports a particular political party
         a_vote5,                          # strength of support for stated party
         a_hiqual_dv,                      # highest qualification
         a_jbstat                          # job status
  ) %>%
  sjlabelled::remove_all_labels() %>%
  rename(hidp = a_hidp, 
         vote6 = a_vote6, vote2 = a_vote2, vote4 = a_vote4, vote1 = a_vote1, vote5 = a_vote5,
         hiqual = a_hiqual_dv, jbstat = a_jbstat) %>% 
  mutate(hoh = NA_real_,
         wave = 2009)

w_4i <- import("stata/stata13/ukhls_w4/d_indresp.dta") %>% 
  select(pidp,
         d_hidp,                           # household identifier
         d_vote6,                          # level of interest in politics
         d_vote2,                          # closer to a party  
         d_vote4,                          # which political party closest to
         d_vote1,                          # supports a particular political party
         d_vote5,                          # strength of support for stated party
         d_hiqual_dv,                      # highest qualification
         d_jbstat                          # job status
  ) %>%
  sjlabelled::remove_all_labels() %>% 
  rename(hidp = d_hidp, 
         vote6 = d_vote6, vote2 = d_vote2, vote4 = d_vote4, vote1 = d_vote1, vote5 = d_vote5, 
         hiqual = d_hiqual_dv, jbstat = d_jbstat) %>% 
  mutate(hoh = NA_real_,
         wave = 2012)

df_personal <- rbind(w_18i, w_1i, w_4i)

# merge the two -----------------------------------------------------------

df_merged <- left_join(df_personal, df_household, by = c("hidp" = "hidp", "wave" = "wave")) 

# clean the dataset -------------------------------------------------------

df_clean <- df_merged %>% 
  mutate(across(.cols = everything(), .fns = ~replace(., which(. < 0), NA_real_))) %>% 
  mutate(p_id = pidp, 
         h_id = hidp,
         region = recode(gor,
                         `1` = "North East",
                         `2` = "North West",
                         `3` = "Yorkshire and the Humber",
                         `4` = "East Midlands",
                         `5` = "West Midlands",
                         `6` = "East of England",
                         `7` = "London",
                         `8` = "South East",
                         `9` = "South West",
                         `10` = "Wales",
                         `11` = "Scotland",
                         `12` = "Northern Ireland"),
         
         # dependent variables
         interest_politics = recode(vote6, `4` = 1, `3` = 2, `2` = 3, `1` = 4),      
         
         support_party = case_when(vote1 == 1 ~ 1, vote1 == 2 ~ 0,
                                   is.na(vote1) ~ NA_real_),                           
         support_amount = recode(vote5, `3` = 2, `2` = 3, `1` = 4),    
         support_amount = replace(support_amount, is.na(support_party), NA_real_),
         support_amount = replace(support_amount, support_party == 0, 1),
         support_green = case_when(vote4 == 6 ~ 1, support_party == 0 ~ 0,
                                   is.na(vote4) ~ NA_real_, vote4 == 96 ~ NA_real_, 
                                   TRUE ~ 0),
         
         # independent variable
         solar_panel = case_when(solar1 == 1 | solar2 == 1 ~ 1, solar1 > 1 & solar2 > 1 ~ 0),
         
         # additional independent variables for the appendix
         solar_pv = case_when(solar1 == 1 ~ 1, solar1 > 1 ~ 0),
         solar_st = case_when(solar2 == 1 ~ 1, solar2 > 1 ~ 0),

         # control variables
         education = recode(hiqual,
                            `1` = 6,
                            `2` = 5, 
                            `3` = 4, 
                            `4` = 3,
                            `5` = 2,
                            `9` = 1),
         head = case_when(wave == 2008 & hoh == 1 ~ 1, 
                          wave == 2008 & hoh == 2 ~ 0,
                          wave > 2008 & is.na(hrpid) ~ NA_real_,
                          wave > 2008 & hrpid == pidp ~ 1, 
                          wave > 2008 & hrpid != pidp ~ 0),
         unemployed = case_when(jbstat == 3 ~ 1, is.na(jbstat) ~ NA_real_, TRUE ~ 0),
         income = fihhmnnet1,
         children = case_when(nkids > 0 ~ 1, nkids == 0 ~ 0, is.na(nkids) ~ NA_real_),
         owner = case_when(wave == 2008 & hsownd %in% c(1, 2) ~ 1, 
                           wave == 2008 & hsownd %in% c(3, 4, 5) ~ 0,
                           wave > 2008 & hsownd %in% c(1, 2, 3) ~ 1, 
                           wave > 2008 & hsownd %in% c(4, 5, 97) ~ 0)) %>% 
  select(p_id, h_id, wave, region,
         interest_politics, support_party, support_amount, support_green,
         solar_panel, 
         solar_pv, solar_st,
         education, head, unemployed, income, children, owner)

# save the clean dataset --------------------------------------------------

export(df_clean, "data/gbr_tidy.Rdata")

##########################################################################
# shp v6097-5 ############################################################
##########################################################################

# get household data ------------------------------------------------------

w20_h <- import("Data_STATA/SHP-DATA-W1-W20-STATA/W20_2018/shp18_h_user.dta") %>% 
  select(idhous18,                      # identification number of the household
         region18,                      # region of residence
         hhmove18,                      # moved since last interview
         h18h44,                        # solar panels
         i18htyn,                       # yearly household income net
         nbkid18,                       # number of children in household
         h18h29                         # tenant or owner
  ) %>%
  sjlabelled::remove_all_labels() %>% 
  rename(idhous = idhous18, region = region18, hhmove = hhmove18, 
         h44 = h18h44,
         htyn = i18htyn, n_kid =  nbkid18, h29 = h18h29) %>% 
  mutate(syear = 2018)

w19_h <- import("Data_STATA/SHP-DATA-W1-W20-STATA/W19_2017/shp17_h_user.dta") %>% 
  select(idhous17,                      # identification number of the household
         region17,                      # region of residence
         hhmove17,                      # moved since last interview
         h17h44,                        # solar panels         
         i17htyn,                       # yearly household income net         
         nbkid17,                       # number of children in household
         h17h29                         # tenant or owner
  ) %>%
  sjlabelled::remove_all_labels() %>% 
  rename(idhous = idhous17, region = region17, hhmove = hhmove17, 
         h44 = h17h44,
         htyn = i17htyn, n_kid = nbkid17, h29 = h17h29) %>% 
  mutate(syear = 2017)

w18_h <- import("Data_STATA/SHP-DATA-W1-W20-STATA/W18_2016/shp16_h_user.dta") %>% 
  select(idhous16,                      # identification number of the household
         region16,                      # region of residence
         hhmove16,                      # moved since last interview
         h16h44,                        # solar panels
         i16htyn,                       # yearly household income net         
         nbkid16,                       # number of children in household
         h16h29                         # tenant or owner
  ) %>%
  sjlabelled::remove_all_labels() %>% 
  rename(idhous = idhous16, region = region16, hhmove = hhmove16, 
         h44 = h16h44,
         htyn = i16htyn, n_kid =  nbkid16, h29 = h16h29) %>% 
  mutate(syear = 2016)

w17_h <- import("Data_STATA/SHP-DATA-W1-W20-STATA/W17_2015/shp15_h_user.dta") %>% 
  select(idhous15,                      # identification number of the household
         region15,                      # region of residence
         hhmove15,                      # moved since last interview
         h15h44,                        # solar panels
         i15htyn,                       # yearly household income net
         nbkid15,                       # number of children in household
         h15h29                         # tenant or owner
  ) %>%
  sjlabelled::remove_all_labels() %>% 
  rename(idhous = idhous15, region = region15, hhmove = hhmove15,
         h44 = h15h44,
         htyn = i15htyn, n_kid =  nbkid15, h29 = h15h29, ) %>% 
  mutate(syear = 2015)

w16_h <- import("Data_STATA/SHP-DATA-W1-W20-STATA/W16_2014/shp14_h_user.dta") %>% 
  select(idhous14,                      # identification number of the household
         region14,                      # region of residence
         hhmove14,                      # moved since last interview
         h14h44,                        # solar panels
         i14htyn,                       # yearly household income net
         nbkid14,                       # number of children in household
         h14h29                         # tenant or owner
  ) %>%
  sjlabelled::remove_all_labels() %>% 
  rename(idhous = idhous14, region = region14, hhmove = hhmove14, 
         h44 = h14h44,
         h29 = h14h29, n_kid =  nbkid14, htyn = i14htyn) %>% 
  mutate(syear = 2014)

w15_h <- import("Data_STATA/SHP-DATA-W1-W20-STATA/W15_2013/shp13_h_user.dta") %>% 
  select(idhous13,                      # identification number of the household
         region13,                      # region of residence
         hhmove13,                      # moved since last interview
         h13h44,                        # solar panels
         i13htyn,                       # yearly household income net
         nbkid13,                       # number of children in household
         h13h29                         # tenant or owner
  ) %>%
  sjlabelled::remove_all_labels() %>% 
  rename(idhous = idhous13, region = region13, hhmove = hhmove13, 
         h44 = h13h44,
         h29 = h13h29, n_kid =  nbkid13, htyn = i13htyn) %>% 
  mutate(syear = 2013)

merged_household <- rbind(w15_h, w16_h, w17_h, w18_h, w19_h, w20_h) 

df_moves <-  merged.household %>% 
  mutate(moves = case_when(syear > 2013 & hhmove == 1 ~ 1, TRUE ~ 0)) %>% 
  group_by(idhous) %>% 
  summarise(sum_moves = sum(moves)) %>% 
  ungroup()

df_household <- left_join(merged.household, df_moves, by = "idhous") %>% 
  filter(sum_moves == 0) %>%
  group_by(idhous) %>% arrange(syear) %>% 
  mutate(solar_ren = case_when(h44 == 1 ~ 1, TRUE ~ 0),
         solar_rens = cumsum(solar_ren)) %>% 
  select(-sum_moves, -solar_ren) %>% 
  ungroup()

# get personal data -------------------------------------------------------

w20_p <- import("Data_STATA/SHP-DATA-W1-W20-STATA/W20_2018/shp18_p_user.dta") %>% 
  select(idhous18,                      # identification number of the household
         idpers,                        # identification number of the person
         p18p01,                        # interest in politics
         p18p10,                        # political position: left, right
         p18p19,                        # party choice in case of elections tomorrow
         isced18,                       # international standard classification of education
         relarp18,                      # relation with person of reference
         wstat18                        # working status
  ) %>%
  sjlabelled::remove_all_labels() %>% 
  rename(idhous = idhous18, 
         p01 = p18p01, p10 = p18p10, p19 = p18p19,
         isced = isced18, relarp = relarp18, wstat = wstat18) %>% 
  mutate(syear = 2018,
         p04 = NA_real_, p66 = NA_real_, p68 = NA_real_, p67 = NA_real_)

w19_p <- import("Data_STATA/SHP-DATA-W1-W20-STATA/W19_2017/shp17_p_user.dta") %>% 
  select(idhous17,                      # identification number of the household
         idpers,                        # identification number of the person
         p17p01,                        # interest in politics
         p17p10,                        # political position: left, right
         p17p04,                        # trust in federal government
         p17p66,                        # party identification: yes-no
         p17p68,                        # party identification: proximity
         p17p67,                        # party identification: party
         p17p19,                        # party choice in case of elections tomorrow
         isced17,                       # international standard classification of education
         relarp17,                      # relation with person of reference
         wstat17                        # working status
  ) %>%
  sjlabelled::remove_all_labels() %>% 
  rename(idhous = idhous17, 
         p01 = p17p01, p10 = p17p10, p04 = p17p04, p66 = p17p66, p68 = p17p68, p67 = p17p67, p19 = p17p19,
         isced = isced17, relarp = relarp17, wstat = wstat17) %>% 
  mutate(syear = 2017)

w18_p <- import("Data_STATA/SHP-DATA-W1-W20-STATA/W18_2016/shp16_p_user.dta") %>% 
  select(idhous16,                      # identification number of the household
         idpers,                        # identification number of the person
         p16p01,                        # interest in politics
         p16p10,                        # political position: left, right
         p16p19,                        # party choice in case of elections tomorrow         
         isced16,                       # international standard classification of education
         relarp16,                      # relation with person of reference
         wstat16                        # working status
  ) %>%
  sjlabelled::remove_all_labels() %>% 
  rename(idhous = idhous16,
         p01 = p16p01, p10 = p16p10, p19 = p16p19,
         isced = isced16, relarp = relarp16, wstat = wstat16) %>% 
  mutate(syear = 2016, 
         p04 = NA_real_, p66 = NA_real_, p68 = NA_real_, p67 = NA_real_)

w17_p <- import("Data_STATA/SHP-DATA-W1-W20-STATA/W17_2015/shp15_p_user.dta") %>% 
  select(idhous15,                      # identification number of the household
         idpers,                        # identification number of the person
         p15p01,                        # interest in politics
         p15p10,                        # political position: left, right
         p15p19,                        # party choice in case of elections tomorrow
         isced15,                       # international standard classification of education
         relarp15,                      # relation with person of reference         
         wstat15                        # working status
  ) %>%
  sjlabelled::remove_all_labels() %>% 
  rename(idhous = idhous15,         
         p01 = p15p01, p10 = p15p10, p19 = p15p19,
         isced = isced15, relarp = relarp15, wstat = wstat15) %>% 
  mutate(syear = 2015,
         p04 = NA_real_, p66 = NA_real_, p68 = NA_real_, p67 = NA_real_)

w16_p <- import("Data_STATA/SHP-DATA-W1-W20-STATA/W16_2014/shp14_p_user.dta") %>% 
  select(idhous14,                      # identification number of the household
         idpers,                        # identification number of the person
         p14p01,                        # interest in politics
         p14p10,                        # political position: left, right
         p14p04,                        # trust in federal government
         p14p66,                        # party identification: yes-no
         p14p68,                        # party identification: proximity
         p14p67,                        # party identification: party
         p14p19,                        # party choice in case of elections tomorrow
         isced14,                       # international standard classification of education
         relarp14,                      # relation with person of reference
         wstat14                        # working status
  ) %>%
  sjlabelled::remove_all_labels() %>% 
  rename(idhous = idhous14,          
         p01 = p14p01, p10 = p14p10, p04 = p14p04, p66 = p14p66, p68 = p14p68, p67 = p14p67, p19 = p14p19,
         isced = isced14, relarp = relarp14, wstat = wstat14) %>% 
  mutate(syear = 2014)

w15_p <- import("Data_STATA/SHP-DATA-W1-W20-STATA/W15_2013/shp13_p_user.dta") %>% 
  select(idhous13,                      # identification number of the household
         idpers,                        # identification number of the person
         p13p01,                        # interest in politics
         p13p10,                        # political position: left, right
         p13p19,                        # party choice in case of elections tomorrow
         isced13,                       # international standard classification of education
         relarp13,                      # relation with person of reference
         wstat13                        # working status
  ) %>%
  sjlabelled::remove_all_labels() %>% 
  rename(idhous = idhous13,            
         p01 = p13p01, p10 = p13p10, p19 = p13p19,
         isced = isced13, relarp = relarp13, wstat = wstat13) %>% 
  mutate(syear = 2013,
         p04 = NA_real_, p66 = NA_real_, p68 = NA_real_, p67 = NA_real_)

df_personal <- rbind(w15_p, w16_p, w17_p, w18_p, w19_p, w20_p) %>% 
  left_join(., df_moves, by = "idhous") %>% 
  filter(sum_moves == 0) %>% 
  select(-sum_moves)

# merge the two -----------------------------------------------------------

df_merged <- left_join(df_personal, df_household, by = c("idhous" = "idhous", "syear" = "syear")) 

# clean the dataset -------------------------------------------------------

df_clean <- df_merged %>%
  mutate(across(.cols = everything(), .fns = ~replace(., which(. < 0), NA_real_))) %>% 
  mutate(p_id = idpers, 
         h_id = idhous,
         wave = syear,
         region = recode(region,
                         `1` = "Lake Geneva",
                         `2` = "Middleland",
                         `3` = "North-west Switzerland",
                         `4` = "Zurich",
                         `5` = "East Switzerland",
                         `6` = "Central Switzerland",
                         `7` = "Ticino"),
         
         # dependent variables
         interest_politics = scales::rescale(p01,
                                             to = c(1, 4),
                                             from = range(p01, na.rm = TRUE)),
         left_right = p10,
         trust_government = p04,
         support_party = recode(p66, `2` = 0, `1` = 1),                           
         support_amount = recode(p68, `3` = 2, `2` = 3, `1` = 4),
         support_amount = replace(support_amount, support_party == 0, 1),
         support_green = case_when(p67 %in% c(10, 11, 20) ~ 1,
                                   is.na(p67) ~ NA_real_, support_party == 0 ~ 0, TRUE ~ 0),
         
         # additional dependent variables for the appendix
         vote_green = case_when(p19 %in% c(10, 11, 20) ~ 1,
                                is.na(p19) ~ NA_real_, TRUE ~ 0),
         vote_turnout = case_when(p19 %in% c(51, 52) ~ 0,
                                  is.na(p19) ~ NA_real_, TRUE ~ 1),
         
         # independent variable
         solar_panel = case_when(solar_rens > 0 ~ 1, solar_rens == 0 ~ 0, is.na(solar_rens) ~ NA_real_),
         
         # control variables
         education = recode(isced,
                            `0` = 0,
                            `10` = 1,
                            `20` = 2, 
                            `31` = 3, 
                            `32` = 4,
                            `33` = 5,
                            `41` = 6,
                            `51` = 7,
                            `52` = 8,
                            `60` = 9),
         head = case_when(relarp == 1 ~ 1, is.na(relarp) ~ NA_real_, TRUE ~ 0),
         unemployed = case_when(wstat == 2 ~ 1, is.na(wstat) ~ NA_real_, TRUE ~ 0),
         income = htyn / 12,
         children = case_when(n_kid > 0 ~ 1, n_kid == 0 ~ 0, is.na(n_kid) ~ NA_real_),
         owner = recode(h29, `3` = 0, `2` = 1, `1` = 0)) %>% 
  select(p_id, h_id, wave, region, 
         interest_politics, left_right, trust_government, support_party, support_amount, support_green,
         vote_turnout, vote_green,
         solar_panel,
         education, head, unemployed, income, children, owner)

# save the clean dataset --------------------------------------------------

export(df_clean, "data/che_tidy.Rdata")