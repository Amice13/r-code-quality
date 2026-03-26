#=====================================================================================================================================#
#                                                                                                                                     #
# Title: "Americans’ Responses to COVID-19 and the Conditional Role of Dispositional Needs for Security: A Replication and Extension" #
# Authors: Adam R. Panish, Trent Ollerenshaw, & Joseph A. Vitriol                                                                     #
# Date: 9/21/2025                                                                                                                     #
#                                                                                                                                     #
#################################### Code for cleaning datasets and creating latent NSC variables #####################################
#                                                                                                                                     #
#=====================================================================================================================================#

#================================================================#
#                          Load Packages                         #
#================================================================#

library(haven)
library(readxl)
library(dplyr)
library(crfsuite)
library(writexl)
library(lavaan)

#================================================================#
#                          Load Datasets                         #
#================================================================#

Bovitz2020 <- read_sav("Bovitz2020.sav")
MTurk2020 <- read_sav("MTurk2020.sav")
Lucid2020 <- read_xlsx("Lucid2020.xlsx")
Lucid2021 <- read_xlsx("Lucid2021.xlsx")
ANESGSS2020 <- read_dta("ANESGSS2020.dta")
VSG <- read_dta("VSG.dta")

#================================================================#
#                         Bovitz Recoding                        #
#================================================================#

Bovitz2020 <- 
  Bovitz2020 %>%
  transmute(
    
    ## Demographics
    sample = "Bovitz",
    year = 2020,
    attn_chk1 = ifelse(is.na(Q56), NA, ifelse(Q56 == 1, "Passed", "Failed")),
    attn_chk2 = ifelse(is.na(Q58), NA, ifelse(Q58 == 3, "Passed", "Failed")),
    age_ = (age - min(age, na.rm = T)) / (max(age, na.rm = T) - min(age, na.rm = T)),
    male = ifelse(is.na(gender), NA, ifelse(as.numeric(gender) == 1, 1, 0)),
    white = ifelse(is.na(ethnicity) | ethnicity %in% c('', "99"), NA, ifelse(as.numeric(ethnicity) == 1 & hispanic == 2, 1, 0)),
    black = ifelse(is.na(ethnicity) | ethnicity %in% c('', "99"), NA, ifelse(as.numeric(ethnicity) == 2 & hispanic == 2, 1, 0)),
    hispanic_ = ifelse(is.na(hispanic), NA, ifelse(hispanic == 1, 1, 0)),
    otherrace = ifelse(
      ethnicity %in% c('', "99"), NA, ifelse( # Multiracial respondents categorized as Other Race
        ethnicity %in% c("1;2", "1;2;3", "1;2;3;4;5;98", "1;2;3;5", "1;2;5", "1;2;5;98", "1;2;98", "1;3", "1;3;98", "1;4", "1;5", "1;5;98", "1;98",
                         "2;3", "2;3;98", "2;4", "2;5", "2;98", 
                         "3", "3;5", "3;98", 
                         "4", "4;98", 
                         "5", 
                         "98", "98;99"), 1, 0
        )
      ),
    race_ = relevel(factor(ifelse(white == 1, "White", ifelse(black == 1, "Black", ifelse(hispanic_ == 1, "Hispanic", ifelse(otherrace == 1, "Other", NA))))), "White"),
    income_ = ifelse(income == 99, NA, (income - 1) / 10),
    education_ = car::recode(as.numeric(education), "1=0; 2=0.25; 3:5=0.5; 6=0.75; 7:9=1; NA=NA"),

    ## Location
    ipaddress = as.numeric(gsub("\\.", "", IPAddress)),
    zip_ = txt_sprintf("%05d", zip),
    state = car::recode(
      as.numeric(zip_),
      "35000:36999 = 'AL';
      99500:99999 = 'AK';
      85000:86999 = 'AZ';
      71600:72999 = 'AR';
      90000:96699 = 'CA';
      80000:81999 = 'CO';
      6000:6999 = 'CT';
      19700:19999 = 'DE';
      32000:34999 = 'FL';
      30000:31999 = 'GA';
      96700:96999 = 'HI';
      83200:83999 = 'ID';
      60000:62999 = 'IL';
      46000:47999 = 'IN';
      50000:52999 = 'IA';
      66000:67999 = 'KS';
      40000:42999 = 'KY';
      70000:71599 = 'LA';
      3900:4999 = 'ME';
      20600:21999 = 'MD';
      1000:2799 = 'MA';
      48000:49999 = 'MI';
      55000:56999 = 'MN';
      38600:39999 = 'MS';
      63000:65999 = 'MO';
      59000:59999 = 'MT';
      27000:28999 = 'NC';
      58000:58999 = 'ND';
      68000:69999 = 'NE';
      88900:89999 = 'NV';
      3000:3899 = 'NH';
      7000:8999 = 'NJ';
      87000:88499 = 'NM';
      10000:14999 = 'NY';
      43000:45999 = 'OH';
      73000:74999 = 'OK';
      97000:97999 = 'OR';
      15000:19699 = 'PA';
      300:999 = 'PR';
      2800:2999 = 'RI';
      29000:29999 = 'SC';
      57000:57999 = 'SD';
      37000:38599 = 'TN';
      75000:79999 = 'TX';
      88500:88599 = 'TX';
      84000:84999 = 'UT';
      5000:5999 = 'VT';
      22000:24699 = 'VA';
      20000:20599 = 'DC';
      98000:99499 = 'WA';
      24700:26999 = 'WV';
      53000:54999 = 'WI';
      82000:83199 = 'WY';
      NA = NA"
      ),
    region_ = case_when(
      state %in% c("CT", "NH", "RI", "ME", "MA", "VT") ~ 1,
      state %in% c("NY", "PA", "NJ") ~ 2,
      state %in% c("MI", "OH", "IN", "WI", "IL") ~ 3,
      state %in% c("IA", "NE", "MO", "ND", "MN", "SD", "KS") ~ 4,
      state %in% c("NC", "FL", "DC", "VA", "GA", "SC", "WV", "MD", "DE") ~ 5,
      state %in% c("TN", "AL", "KY", "MS") ~ 6,
      state %in% c("TX", "LA", "OK", "AR") ~ 7,
      state %in% c("CO", "AZ", "UT", "NM", "ID", "NV", "MT", "WY") ~ 8,
      state %in% c("WA", "CA", "OR", "AK", "HI") ~ 9,
      T ~ NA
      ),
    south_ = ifelse(region_ %in% 5:7, 1, 0),
    
    ## Partisanship and Ideology (higher values more Republican/conservative)
    # Partisan Identification
    pid = ifelse(
      Q145 %in% 3:4, car::recode(Q148, "1=0.333; 2=0.667; 5=0.5; NA=NA"), ifelse(
        Q145 == 1, car::recode(Q146, "1=0; 2=0.167; NA=NA"), ifelse(
          Q145 == 2, car::recode(Q147, "1=1; 2=0.833; NA=NA"), NA
        )
      )
    ),
    # Symbolic Ideology
    ideo = (Ideo1 - 1) / 6,
    
    ## Psychological Predispositions
    # Authoritarianism (For authoritarianism items, "I’m not sure", "Both are equally important", and "Neither is very important" are assigned to the midpoint)
    auth1 = as.numeric(car::recode(AUT1, "1=0; 2=1; 3:5=0.5; NA=NA")), # Independence or Respect for Elders
    auth2 = as.numeric(car::recode(AUT2, "1=1; 2=0; 3:5=0.5; NA=NA")), # Obedience or Self-reliance
    auth3 = as.numeric(car::recode(AUT3, "1=0; 2=1; 3:5=0.5; NA=NA")), # Curiosity or Good Manners
    auth4 = as.numeric(car::recode(AUT4, "1=0; 2=1; 3:5=0.5; NA=NA")), # Being Considerate or Well-behaved
    auth5 = NA,
    auth6 = NA,
    auth7 = NA,
    auth8 = NA,
    auth_scale = rowMeans(matrix(c(auth1, auth2, auth3, auth4), ncol = 4), na.rm = T),
    # Big 5 Personality (Conscientiousness and Openness)
    consc1 = ifelse(Q231_3 %in% 20:24, (24 - Q231_3) / 4, NA),
    consc2 = ifelse(Q231_8 %in% 20:24, (24 - Q231_8) / 4, NA),
    consc3 = ifelse(Q231_13 %in% 20:24, (Q231_13 - 20) / 4, NA),
    consc_scale = rowMeans(matrix(c(consc1, consc2, consc3), ncol = 3), na.rm = T),
    open1 = ifelse(Q231_5 %in% 20:24, (Q231_5 - 20) / 4, NA),
    open2 = ifelse(Q231_10 %in% 20:24, (24 - Q231_10) / 4, NA),
    open3 = ifelse(Q231_15 %in% 20:24, (Q231_15 - 20) / 4, NA),
    open4 = ifelse(Q231_16 %in% 20:24, (24 - Q231_16) / 4, NA),
    open5 = ifelse(Q231_17 %in% 20:24, (Q231_17 - 20) / 4, NA),
    open6 = ifelse(Q231_18 %in% 20:24, (24 - Q231_18) / 4, NA),
    open_scale = rowMeans(matrix(c(open1, open2, open3, open4, open5, open6), ncol = 6), na.rm = T),
    # Right-Wing Authoritarianism
    rwa1 = ifelse(Q210_1 %in% 1:5, (Q210_1 - 1) / 4, NA),
    rwa2 = ifelse(Q210_2 %in% 1:5, (5 - Q210_2) / 4, NA),
    rwa3 = ifelse(Q210_3 %in% 1:5, (Q210_3 - 1) / 4, NA),
    rwa4 = ifelse(Q210_4 %in% 1:5, (5 - Q210_4) / 4, NA),
    rwa5 = ifelse(Q210_5 %in% 1:5, (5 - Q210_5) / 4, NA),
    rwa6 = ifelse(Q210_6 %in% 1:5, (Q210_6 - 1) / 4, NA),
    rwa7 = ifelse(Q210_7 %in% 1:5, (Q210_7 - 1) / 4, NA),
    rwa8 = ifelse(Q210_8 %in% 1:5, (5 - Q210_8) / 4, NA),
    rwa9 = ifelse(Q210_9 %in% 1:5, (5 - Q210_9) / 4, NA),
    rwa10 = ifelse(Q210_10 %in% 1:5, (Q210_10 - 1) / 4, NA),
    rwa11 = ifelse(Q210_11 %in% 1:5, (5 - Q210_11) / 4, NA),
    rwa12 = ifelse(Q210_12 %in% 1:5, (Q210_12 - 1) / 4, NA),
    rwa13 = ifelse(Q210_13 %in% 1:5, (Q210_13 - 1) / 4, NA),
    rwa14 = ifelse(Q210_14 %in% 1:5, (5 - Q210_14) / 4, NA),
    rwa15 = ifelse(Q210_15 %in% 1:5, (Q210_15 - 1) / 4, NA),
    rwa16 = ifelse(Q210_16 %in% 1:5, (5 - Q210_16) / 4, NA),
    rwa17 = ifelse(Q210_17 %in% 1:5, (Q210_17 - 1) / 4, NA),
    rwa18 = ifelse(Q210_18 %in% 1:5, (5 - Q210_18) / 4, NA),
    rwa_scale = rowMeans(matrix(c(rwa1, rwa2, rwa3, rwa4, rwa5, rwa6, rwa7, rwa8, rwa9, rwa10,
                                  rwa11, rwa12, rwa13, rwa14, rwa15, rwa16, rwa17, rwa18), ncol = 18), na.rm = T),
    # Moral Foundations
    authority = ifelse(Q233_5 %in% 7:12, (Q233_5 - 7) / 5, NA), 
    purity = ifelse(Q233_2 %in% 7:12, (Q233_2 - 7) / 5, NA),
    loyalty = ifelse(Q233_1 %in% 7:12, (Q233_1 - 7) / 5, NA),
    loyal2 = NA, # MTurk survey has additional MFQ items not lisked in codebook. Bovitz does not.
    care = ifelse(Q233_3 %in% 7:12, (Q233_3 - 7) / 5, NA),
    care2 = NA,
    fairness = ifelse(Q233_4 %in% 7:12, (Q233_4 - 7) / 5, NA),
    fair2 = NA,
    individualizing = rowMeans(matrix(c(care, fairness), ncol = 2), na.rm = T),
    binding = rowMeans(matrix(c(purity, loyalty, authority), ncol = 3), na.rm = T),
    # Need for Structure
    nfs1 = NA,
    nfs2 = NA,
    nfs3 = NA,
    nfs4 = NA,
    nfs5 = NA,
    nfs6 = NA,
    nfs7 = NA,
    nfs8 = NA,
    nfs9 = NA,
    nfs10 = NA,
    nfs11 = NA,
    nfs12 = NA,
    nfs_scale = NA,
    
    ## Political Engagement
    # Political Interest
    polint1 = (4 - PI1) / 3, # Would you say you follow what’s going on in government and public affairs
    polint2 = (PI2 - 1) / 6, # My political attitudes and beliefs are an important reflection of who I am
    polint3 = (PI3 - 1) / 6, # In general, my political attitudes and beliefs are an important part of my self-image
    polint4 = NA,
    polint5 = NA,
    polint6 = NA,
    polint_scale = rowMeans(matrix(c(polint1, polint2, polint3), ncol = 3), na.rm = T),
    # Political Knowledge
    polknow1 = ifelse(PK1 == 5, 1, 0), # What job or political office does John Roberts currently hold?
    polknow2 = ifelse(PK2 == 2, 1, 0), # What job or political office does Mike Pence currently hold?
    polknow3 = ifelse(PK3 == 2, 1, 0), # What job or political office does Boris Johnson currently hold?
    polknow4 = ifelse(PK4 == 4, 1, 0), # What job or political office does Nancy Pelosi currently hold?
    polknow5 = ifelse(PK5 == 2, 1, 0), # Which political party currently has the most members in the Senate in Washington?
    polknow6 = ifelse(PK6 == 4, 1, 0), # How long is the term of office for a U.S. senator?
    polknow7 = ifelse(PK7 == 1, 1, 0), # Which political party currently has the most members in the House of Representatives in Washington?
    polknow8 = ifelse(PK8 == 1, 1, 0), # Whose responsibility is it to nominate judges to the Federal Courts — the President, the Congress, or the Supreme Court?
    polknow_scale = rowMeans(matrix(c(polknow1, polknow2, polknow3, polknow4, polknow5, polknow6, polknow7, polknow8), ncol = 8), na.rm = T),
    # Overall Political Engagement
    polengage_scale = rowMeans(matrix(c(polint_scale, polknow_scale), ncol = 2), na.rm = T),
    
    ## COVID Behaviors and Attitudes
    # COVID-Mitigation Behaviors ("Have you done any of the following to prevent the spread of coronavirus?")
    dv_travel = ifelse(Q109_1 %in% 1:2, 2 - Q109_1, NA), # Changed travel plans
    dv_work_home = ifelse(Q109_2 %in% 1:2, 2 - Q109_2, NA), # Worked from home
    dv_wash_hands = ifelse(Q109_3 %in% 1:2, 2 - Q109_3, NA), # Regularly washed your hands
    dv_avoid_dine = ifelse(Q109_4 %in% 1:2, 2 - Q109_4, NA), # Avoided dining-in at restaurants or bars
    dv_cancel_event = ifelse(Q109_5 %in% 1:2, 2 - Q109_5, NA), # Canceled social engagements
    dv_sanitize = ifelse(Q109_7 %in% 1:2, 2 - Q109_7, NA), # Sanitized your home or workspace with disinfectant
    dv_distancing1 = NA,
    dv_distancing2 = NA,
    dv_distancing = ifelse(Q109_6 %in% 1:2, 2 - Q109_6, NA), # Stayed at least three feet away from others in public places
    dv_masking1 = NA,
    dv_masking2 = NA,
    dv_masking = ifelse(Q109_8 %in% 1:2, 2 - Q109_8, NA), # Wore a mask in public
    dv_vaxstatus = NA,
    dv_behavior = rowMeans(matrix(c(dv_travel, dv_work_home, dv_wash_hands, dv_avoid_dine, dv_cancel_event, dv_sanitize, dv_distancing, dv_masking), ncol = 8), na.rm = T),
    # COVID Concern
    dv_concern = ifelse(Q118 %in% 1:6, (6 - Q118) / 5, NA), # Coded so that 0 = "Not at all worried" and 1 = "Extremely worried"
    # Public Health Measures
    dv_businesses = NA,
    dv_tracking = NA,
    dv_lockdowns = NA,
    dv_mmindoors = NA,
    dv_mmoutdoors = NA,
    dv_maskmandates = NA,
    dv_vaxmandate = NA,
    dv_staterestrict = NA,
    dv_cancelevents = NA,
    dv_schools = NA,
    dv_workhome = NA,
    dv_travelrest = NA,
    dv_stayhome = NA,
    dv_testingreq = NA,
    dv_restrictions = NA,
    
    ## COVID variables not included in PAP
    # Vaccine Confidence
    vax1 = ifelse(Vax1_1 %in% 1:7, (Vax1_1 - 1) / 6, NA), # I am completely confident that vaccines are safe
    vax2 = ifelse(Vax1_2 %in% 1:7, (7 - Vax1_2) / 6, NA), # Vaccination is unnecessary because vaccine-preventable diseases are not common anymore.
    vax3 = ifelse(Vax1_3 %in% 1:7, (7 - Vax1_3) / 6, NA), # Everyday stress prevents me from being vaccinated.
    vax4 = ifelse(Vax1_6 %in% 1:7, (Vax1_6 - 1) / 6, NA), # When I think about being vaccinated, I weigh its benefits and risks to make the best decision possible.
    vax5 = ifelse(Vax1_7 %in% 1:7, (7 - Vax1_7) / 6, NA), # When everyone else is vaccinated, I don’t have to be vaccinated, too.
    vax_scale = rowMeans(matrix(c(vax1, vax2, vax3, vax4, vax5), ncol = 5), na.rm = T),
    # Infection ("Have you or anyone close to you been infected by the coronavirus (COVID-19)?")
    infected = car::recode(as.numeric(Q117), "1='Yes'; 2='No'; 3='I Dont Know'")
  )

names(Bovitz2020) <- gsub("_$", "", names(Bovitz2020)) # Remove trailing _ from variable names
Bovitz2020 <- Bovitz2020[Bovitz2020$attn_chk1 == "Passed" & Bovitz2020$attn_chk2 == "Passed" & !is.na(Bovitz2020$attn_chk1),] # Drop respondents who failed either attention check
Bovitz2020 <- Bovitz2020[-c(3:4)] # Remove attention check variables

write_xlsx(Bovitz2020, "Bovitz2020_coded.xlsx")


#================================================================#
#                          MTurk Recoding                        #
#================================================================#

MTurk2020 <- 
  MTurk2020 %>%
  transmute(
    
    ## Demographics
    sample = "MTurk",
    year = 2020,
    attn_chk1 = ifelse(is.na(Q56), NA, ifelse(Q56 == 1, "Passed", "Failed")),
    attn_chk2 = ifelse(is.na(Q58), NA, ifelse(Q58 == 3, "Passed", "Failed")),
    age = (Q174 - min(Q174, na.rm = T)) / (max(Q174, na.rm = T) - min(Q174, na.rm = T)),
    male = ifelse(is.na(Q176), NA, ifelse(Q176 == 1, 1, 0)),
    white = ifelse(!is.na(Q178_4) & is.na(Q178_1) & is.na(Q178_2) & is.na(Q178_3) & is.na(Q178_5) & is.na(Q178_6), 1, 0),
    black = ifelse(!is.na(Q178_2) & is.na(Q178_1) & is.na(Q178_3) & is.na(Q178_4) & is.na(Q178_5) & is.na(Q178_6), 1, 0),
    hispanic = ifelse(!is.na(Q178_1) & is.na(Q178_2) & is.na(Q178_3) & is.na(Q178_4) & is.na(Q178_5) & is.na(Q178_6), 1, 0),
    otherrace = ifelse(!is.na(Q178_3) | !is.na(Q178_5) | !is.na(Q178_6) | 
                         (rowSums(!is.na(dplyr::select(., Q178_1, Q178_2, Q178_3, Q178_4, Q178_5, Q178_6))) > 1), 1, 0), # Multiracial respondents categorized as Other Race
    race = relevel(factor(ifelse(white == 1, "White", ifelse(black == 1, "Black", ifelse(hispanic == 1, "Hispanic", ifelse(otherrace == 1, "Other", NA))))), "White"),
    income = (Q184 - 1) / 10,
    education = car::recode(as.numeric(Q186), "1:2=0; 3=0.25; 4:5=0.5; 6=0.75; 7:8=1; NA=NA"),

    ## Location
    ipaddress = as.numeric(gsub("\\.", "", IPAddress)),
    zip = ifelse(
      Q187 == "48006-3709", "48006", 
      ifelse(
        Q187 %in% c('', "123456"), NA, Q187
        )
      ),
    state = ifelse(
      is.na(zip), NA, 
      car::recode( 
      as.numeric(zip),
      "35000:36999 = 'AL';
      99500:99999 = 'AK';
      85000:86999 = 'AZ';
      71600:72999 = 'AR';
      90000:96699 = 'CA';
      80000:81999 = 'CO';
      6000:6999 = 'CT';
      19700:19999 = 'DE';
      32000:34999 = 'FL';
      30000:31999 = 'GA';
      96700:96999 = 'HI';
      83200:83999 = 'ID';
      60000:62999 = 'IL';
      46000:47999 = 'IN';
      50000:52999 = 'IA';
      66000:67999 = 'KS';
      40000:42999 = 'KY';
      70000:71599 = 'LA';
      3900:4999 = 'ME';
      20600:21999 = 'MD';
      1000:2799 = 'MA';
      48000:49999 = 'MI';
      55000:56999 = 'MN';
      38600:39999 = 'MS';
      63000:65999 = 'MO';
      59000:59999 = 'MT';
      27000:28999 = 'NC';
      58000:58999 = 'ND';
      68000:69999 = 'NE';
      88900:89999 = 'NV';
      3000:3899 = 'NH';
      7000:8999 = 'NJ';
      87000:88499 = 'NM';
      10000:14999 = 'NY';
      43000:45999 = 'OH';
      73000:74999 = 'OK';
      97000:97999 = 'OR';
      15000:19699 = 'PA';
      300:999 = 'PR';
      2800:2999 = 'RI';
      29000:29999 = 'SC';
      57000:57999 = 'SD';
      37000:38599 = 'TN';
      75000:79999 = 'TX';
      88500:88599 = 'TX';
      84000:84999 = 'UT';
      5000:5999 = 'VT';
      22000:24699 = 'VA';
      20000:20599 = 'DC';
      98000:99499 = 'WA';
      24700:26999 = 'WV';
      53000:54999 = 'WI';
      82000:83199 = 'WY';
      NA = NA"
      )
    ),
    region = case_when(
      state %in% c("CT", "NH", "RI", "ME", "MA", "VT") ~ 1,
      state %in% c("NY", "PA", "NJ") ~ 2,
      state %in% c("MI", "OH", "IN", "WI", "IL") ~ 3,
      state %in% c("IA", "NE", "MO", "ND", "MN", "SD", "KS") ~ 4,
      state %in% c("NC", "FL", "DC", "VA", "GA", "SC", "WV", "MD", "DE") ~ 5,
      state %in% c("TN", "AL", "KY", "MS") ~ 6,
      state %in% c("TX", "LA", "OK", "AR") ~ 7,
      state %in% c("CO", "AZ", "UT", "NM", "ID", "NV", "MT", "WY") ~ 8,
      state %in% c("WA", "CA", "OR", "AK", "HI") ~ 9,
      T ~ NA
    ),
    south = ifelse(region %in% 5:7, 1, 0),

    ## Partisanship and Ideology (higher values more Republican/conservative)
    # Partisan Identification
    pid = ifelse(
      Q145 %in% 3:4, car::recode(Q148, "1=0.333; 2=0.667; 5=0.5; NA=NA"), ifelse(
        Q145 == 1, car::recode(Q146, "1=0; 2=0.167; NA=NA"), ifelse(
          Q145 == 2, car::recode(Q147, "1=1; 2=0.833; NA=NA"), NA
        )
      )
    ),
    # Symbolic Ideology
    ideo = (Ideo1 - 1) / 6,

    ## Psychological Predispositions
    # Authoritarianism (For authoritarianism items, "I’m not sure", "Both are equally important", and "Neither is very important" are assigned to the midpoint)
    auth1 = as.numeric(car::recode(AUT1, "1=0; 2=1; 3:5=0.5; NA=NA")), # Independence or Respect for Elders
    auth2 = as.numeric(car::recode(AUT2, "1=1; 2=0; 3:5=0.5; NA=NA")), # Obedience or Self-reliance
    auth3 = as.numeric(car::recode(AUT3, "1=0; 2=1; 3:5=0.5; NA=NA")), # Curiosity or Good Manners
    auth4 = as.numeric(car::recode(AUT4, "1=0; 2=1; 3:5=0.5; NA=NA")), # Being Considerate or Well-behaved
    auth5 = NA,
    auth6 = NA,
    auth7 = NA,
    auth8 = NA,
    auth_scale = rowMeans(matrix(c(auth1, auth2, auth3, auth4), ncol = 4), na.rm = T),
    # Big 5 Personality (Conscientiousness and Openness)
    consc1 = ifelse(Q231_3 %in% 1:5, (5 - Q231_3) / 4, NA),
    consc2 = ifelse(Q231_8 %in% 1:5, (5 - Q231_8) / 4, NA),
    consc3 = ifelse(Q231_13 %in% 1:5, (Q231_13 - 1) / 4, NA),
    consc_scale = rowMeans(matrix(c(consc1, consc2, consc3), ncol = 3), na.rm = T),
    open1 = ifelse(Q231_5 %in% 1:5, (Q231_5 - 1) / 4, NA),
    open2 = ifelse(Q231_10 %in% 1:5, (5 - Q231_10) / 4, NA),
    open3 = ifelse(Q231_15 %in% 1:5, (Q231_15 - 1) / 4, NA),
    open4 = ifelse(Q231_16 %in% 1:5, (5 - Q231_16) / 4, NA),
    open5 = ifelse(Q231_17 %in% 1:5, (Q231_17 - 1) / 4, NA),
    open6 = ifelse(Q231_18 %in% 1:5, (5 - Q231_18) / 4, NA),
    open_scale = rowMeans(matrix(c(open1, open2, open3, open4, open5, open6), ncol = 6), na.rm = T),
    # Right-Wing Authoritarianism
    rwa1 = ifelse(Q210_1 %in% 1:5, (Q210_1 - 1) / 4, NA),
    rwa2 = ifelse(Q210_2 %in% 1:5, (5 - Q210_2) / 4, NA),
    rwa3 = ifelse(Q210_3 %in% 1:5, (Q210_3 - 1) / 4, NA),
    rwa4 = ifelse(Q210_4 %in% 1:5, (5 - Q210_4) / 4, NA),
    rwa5 = ifelse(Q210_5 %in% 1:5, (5 - Q210_5) / 4, NA),
    rwa6 = ifelse(Q210_6 %in% 1:5, (Q210_6 - 1) / 4, NA),
    rwa7 = ifelse(Q210_7 %in% 1:5, (Q210_7 - 1) / 4, NA),
    rwa8 = ifelse(Q210_8 %in% 1:5, (5 - Q210_8) / 4, NA),
    rwa9 = ifelse(Q210_9 %in% 1:5, (5 - Q210_9) / 4, NA),
    rwa10 = ifelse(Q210_10 %in% 1:5, (Q210_10 - 1) / 4, NA),
    rwa11 = ifelse(Q210_11 %in% 1:5, (5 - Q210_11) / 4, NA),
    rwa12 = ifelse(Q210_12 %in% 1:5, (Q210_12 - 1) / 4, NA),
    rwa13 = ifelse(Q210_13 %in% 1:5, (Q210_13 - 1) / 4, NA),
    rwa14 = ifelse(Q210_14 %in% 1:5, (5 - Q210_14) / 4, NA),
    rwa15 = ifelse(Q210_15 %in% 1:5, (Q210_15 - 1) / 4, NA),
    rwa16 = ifelse(Q210_16 %in% 1:5, (5 - Q210_16) / 4, NA),
    rwa17 = ifelse(Q210_17 %in% 1:5, (Q210_17 - 1) / 4, NA),
    rwa18 = ifelse(Q210_18 %in% 1:5, (5 - Q210_18) / 4, NA),
    rwa_scale = rowMeans(matrix(c(rwa1, rwa2, rwa3, rwa4, rwa5, rwa6, rwa7, rwa8, rwa9, rwa10,
                                  rwa11, rwa12, rwa13, rwa14, rwa15, rwa16, rwa17, rwa18), ncol = 18), na.rm = T),
    # Moral Foundations 
    authority = ifelse(Q233_5 %in% 7:12, (Q233_5 - 7) / 5, NA), # MTurk survey has additional MFQ items not lisked in codebook. Bovitz does not.
    purity = ifelse(Q233_2 %in% 7:12, (Q233_2 - 7) / 5, NA),    # Items with 2 appended are the extras
    loyalty = ifelse(Q233_1 %in% 7:12, (Q233_1 - 7) / 5, NA),
    loyal2 = ifelse(Q233_8 %in% 7:12, (Q233_8 - 7) / 5, NA),
    care = ifelse(Q233_3 %in% 7:12, (Q233_3 - 7) / 5, NA), 
    care2 = ifelse(Q233_6 %in% 7:12, (Q233_6 - 7) / 5, NA), 
    fairness = ifelse(Q233_4 %in% 7:12, (Q233_4 - 7) / 5, NA),
    fair2 = ifelse(Q233_7 %in% 7:12, (Q233_7 - 7) / 5, NA),
    individualizing = rowMeans(matrix(c(care, fairness), ncol = 2), na.rm = T),
    binding = rowMeans(matrix(c(purity, loyalty, authority), ncol = 3), na.rm = T),
    # Need for Structure
    nfs1 = ifelse(Q291_1 %in% 47:53, (Q291_1 - 47) / 6, NA),
    nfs2 = ifelse(Q291_2 %in% 47:53, (Q291_2 - 47) / 6, NA),
    nfs3 = ifelse(Q291_3 %in% 47:53, (53 - Q291_3) / 6, NA),
    nfs4 = ifelse(Q291_4 %in% 47:53, (Q291_4 - 47) / 6, NA),
    nfs5 = ifelse(Q291_5 %in% 47:53, (53 - Q291_5) / 6, NA),
    nfs6 = ifelse(Q291_6 %in% 47:53, (Q291_6 - 47) / 6, NA),
    nfs7 = ifelse(Q291_7 %in% 47:53, (53 - Q291_7) / 6, NA),
    nfs8 = ifelse(Q291_8 %in% 47:53, (Q291_8 - 47) / 6, NA),
    nfs9 = ifelse(Q291_9 %in% 47:53, (Q291_9 - 47) / 6, NA),
    nfs10 = ifelse(Q291_10 %in% 47:53, (Q291_10 - 47) / 6, NA),
    nfs11 = ifelse(Q291_11 %in% 47:53, (Q291_11 - 47) / 6, NA),
    nfs12 = ifelse(Q291_12 %in% 47:53, (53 - Q291_12) / 6, NA),
    nfs_scale = rowMeans(matrix(c(nfs1, nfs2, nfs3, nfs4, nfs5, nfs6, nfs7, nfs8, nfs9, nfs10, nfs11, nfs12), ncol = 12), na.rm = T),
    
    ## Political Engagement
    # Political Interest
    polint1 = (4 - PI1) / 3, # Would you say you follow what’s going on in government and public affairs
    polint2 = (PI2 - 1) / 6, # My political attitudes and beliefs are an important reflection of who I am
    polint3 = (PI3 - 1) / 6, # In general, my political attitudes and beliefs are an important part of my self-image
    polint4 = NA,
    polint5 = NA,
    polint6 = NA,
    polint_scale = rowMeans(matrix(c(polint1, polint2, polint3), ncol = 3), na.rm = T),
    # Political Knowledge
    polknow1 = ifelse(PK1 == 5, 1, 0), # What job or political office does John Roberts currently hold?
    polknow2 = ifelse(PK2 == 2, 1, 0), # What job or political office does Mike Pence currently hold?
    polknow3 = ifelse(PK3 == 2, 1, 0), # What job or political office does Boris Johnson currently hold?
    polknow4 = ifelse(PK4 == 4, 1, 0), # What job or political office does Nancy Pelosi currently hold?
    polknow5 = ifelse(PK5 == 2, 1, 0), # Which political party currently has the most members in the Senate in Washington?
    polknow6 = ifelse(PK6 == 4, 1, 0), # How long is the term of office for a U.S. senator?
    polknow7 = ifelse(PK7 == 1, 1, 0), # Which political party currently has the most members in the House of Representatives in Washington?
    polknow8 = ifelse(PK8 == 1, 1, 0), # Whose responsibility is it to nominate judges to the Federal Courts — the President, the Congress, or the Supreme Court?
    polknow_scale = rowMeans(matrix(c(polknow1, polknow2, polknow3, polknow4, polknow5, polknow6, polknow7, polknow8), ncol = 8), na.rm = T),
    # Overall Political Engagement
    polengage_scale = rowMeans(matrix(c(polint_scale, polknow_scale), ncol = 2), na.rm = T),
    
    ## COVID Behaviors and Attitudes
    # COVID-Mitigation Behaviors ("Have you done any of the following to prevent the spread of coronavirus?")
    dv_travel = ifelse(Q109_1 %in% 1:2, 2 - Q109_1, NA), # Changed travel plans
    dv_work_home = ifelse(Q109_2 %in% 1:2, 2 - Q109_2, NA), # Worked from home
    dv_wash_hands = ifelse(Q109_3 %in% 1:2, 2 - Q109_3, NA), # Regularly washed your hands
    dv_avoid_dine = ifelse(Q109_4 %in% 1:2, 2 - Q109_4, NA), # Avoided dining-in at restaurants or bars
    dv_cancel_event = ifelse(Q109_5 %in% 1:2, 2 - Q109_5, NA), # Canceled social engagements
    dv_sanitize = ifelse(Q109_7 %in% 1:2, 2 - Q109_7, NA), # Sanitized your home or workspace with disinfectant
    dv_distancing1 = NA,
    dv_distancing2 = NA,
    dv_distancing = ifelse(Q109_6 %in% 1:2, 2 - Q109_6, NA), # Stayed at least three feet away from others in public places
    dv_masking1 = NA,
    dv_masking2 = NA,
    dv_masking = ifelse(Q109_8 %in% 1:2, 2 - Q109_8, NA), # Wore a mask in public
    dv_vaxstatus = NA,
    dv_behavior = rowMeans(matrix(c(dv_travel, dv_work_home, dv_wash_hands, dv_avoid_dine, dv_cancel_event, dv_sanitize, dv_distancing, dv_masking), ncol = 8), na.rm = T),
    # COVID Concern
    dv_concern = ifelse(Q118 %in% 1:6, (6 - Q118) / 5, NA), # Coded so that 0 = "Not at all worried" and 1 = "Extremely worried"
    # Public Health Measures
    dv_businesses = NA,
    dv_tracking = NA,
    dv_lockdowns = NA,
    dv_mmindoors = NA,
    dv_mmoutdoors = NA,
    dv_maskmandates = NA,
    dv_vaxmandate = NA,
    dv_staterestrict = NA,
    dv_cancelevents = NA,
    dv_schools = NA,
    dv_workhome = NA,
    dv_travelrest = NA,
    dv_stayhome = NA,
    dv_testingreq = NA,
    dv_restrictions = NA,
    
    ## COVID variables not included in PAP
    # Vaccine Confidence
    vax1 = ifelse(Vax1_1 %in% 1:7, (Vax1_1 - 1) / 6, NA), # I am completely confident that vaccines are safe
    vax2 = ifelse(Vax1_2 %in% 1:7, (7 - Vax1_2) / 6, NA), # Vaccination is unnecessary because vaccine-preventable diseases are not common anymore.
    vax3 = ifelse(Vax1_3 %in% 1:7, (7 - Vax1_3) / 6, NA), # Everyday stress prevents me from being vaccinated.
    vax4 = ifelse(Vax1_6 %in% 1:7, (Vax1_6 - 1) / 6, NA), # When I think about being vaccinated, I weigh its benefits and risks to make the best decision possible.
    vax5 = ifelse(Vax1_7 %in% 1:7, (7 - Vax1_7) / 6, NA), # When everyone else is vaccinated, I don’t have to be vaccinated, too.
    vax_scale = rowMeans(matrix(c(vax1, vax2, vax3, vax4, vax5), ncol = 5), na.rm = T),
    # Infection ("Have you or anyone close to you been infected by the coronavirus (COVID-19)?")
    infected = car::recode(as.numeric(Q117), "1='Yes'; 2='No'; 3='I Dont Know'")
  )

MTurk2020 <- MTurk2020[MTurk2020$attn_chk1 == "Passed" & MTurk2020$attn_chk2 == "Passed",] # Drop respondents who failed either attention check
MTurk2020 <- MTurk2020[-c(3:4)] # Remove attention check variables

write_xlsx(MTurk2020, "MTurk2020_coded.xlsx")


#================================================================#
#                        Lucid 2020 Recoding                     #
#================================================================#

# Drop Respondents who failed attention check
Lucid2020 <- Lucid2020[!is.na(Lucid2020$ResponseId) & Lucid2020$attn == "2,3" & Lucid2020$bot == "5,6" & Lucid2020$IC == 4,]

Lucid2020 <- 
  Lucid2020 %>%
  transmute(
    
    ## Demographics
    sample = "Lucid2020",
    year = 2020,
    age_ = (age - min(na.omit(age))) / max(na.omit((age - min(na.omit(age))))),
    male = ifelse(gender %in% 1:3, ifelse(gender == 1, 1, 0), NA),
    white = ifelse(ethnicity_lucid == "1" & hispanic_lucid == "1", 1, 0),
    black = ifelse(ethnicity_lucid == "2" & hispanic_lucid == "1", 1, 0),
    hispanic = ifelse(hispanic_lucid != "1", 1, 0),
    otherrace = ifelse(white == 0 & black == 0 & hispanic == 0, 1, 0),
    race_ = relevel(factor(ifelse(white == 1, "White", ifelse(black == 1, "Black", ifelse(hispanic == 1, "Hispanic", ifelse(otherrace == 1, "Other", NA))))), "White"),
    income_ = (income - 1) / 12,
    education =
      ifelse(
        is.na(educ),
        case_when(
          education_lucid == "1" ~ 0,
          education_lucid == "2" ~ 0.2,
          education_lucid %in% c("3", "4") ~ 0.4,
          education_lucid == "5" ~ 0.6,
          education_lucid == "6" ~ 0.8,
          education_lucid %in% c("7", "8") ~ 1
          ),
        (educ - 1) / 5),

    ## Location
    ipaddress = as.numeric(gsub("\\.", "", IPAddress)),
    zip = zip_lucid,
    state = ifelse(
      is.na(zip), NA, 
      car::recode( 
        as.numeric(zip),
        "35000:36999 = 'AL';
              100 = 'AK';
      99500:99999 = 'AK';
      85000:86999 = 'AZ';
      71600:72999 = 'AR';
      90000:96699 = 'CA';
      80000:81999 = 'CO';
      6000:6999 = 'CT';
      19700:19999 = 'DE';
      32000:34999 = 'FL';
      30000:31999 = 'GA';
      96700:96999 = 'HI';
      83200:83999 = 'ID';
      60000:62999 = 'IL';
      46000:47999 = 'IN';
      50000:52999 = 'IA';
      66000:67999 = 'KS';
      40000:42999 = 'KY';
      70000:71599 = 'LA';
      3900:4999 = 'ME';
      20600:21999 = 'MD';
      1000:2799 = 'MA';
      48000:49999 = 'MI';
      55000:56999 = 'MN';
      38600:39999 = 'MS';
      63000:65999 = 'MO';
      59000:59999 = 'MT';
      27000:28999 = 'NC';
      58000:58999 = 'ND';
      68000:69999 = 'NE';
      88900:89999 = 'NV';
      3000:3899 = 'NH';
      7000:8999 = 'NJ';
      87000:88499 = 'NM';
      10000:14999 = 'NY';
      43000:45999 = 'OH';
      73000:74999 = 'OK';
      97000:97999 = 'OR';
      15000:19699 = 'PA';
      300:999 = 'PR';
      2800:2999 = 'RI';
      29000:29999 = 'SC';
      57000:57999 = 'SD';
      37000:38599 = 'TN';
      75000:79999 = 'TX';
      88500:88599 = 'TX';
      84000:84999 = 'UT';
      5000:5999 = 'VT';
      22000:24699 = 'VA';
      20000:20599 = 'DC';
      98000:99499 = 'WA';
      24700:26999 = 'WV';
      53000:54999 = 'WI';
      82000:83199 = 'WY';
      NA = NA"
      )
    ),
    region = case_when(
      state %in% c("CT", "NH", "RI", "ME", "MA", "VT") ~ 1,
      state %in% c("NY", "PA", "NJ") ~ 2,
      state %in% c("MI", "OH", "IN", "WI", "IL") ~ 3,
      state %in% c("IA", "NE", "MO", "ND", "MN", "SD", "KS") ~ 4,
      state %in% c("NC", "FL", "DC", "VA", "GA", "SC", "WV", "MD", "DE") ~ 5,
      state %in% c("TN", "AL", "KY", "MS") ~ 6,
      state %in% c("TX", "LA", "OK", "AR") ~ 7,
      state %in% c("CO", "AZ", "UT", "NM", "ID", "NV", "MT", "WY") ~ 8,
      state %in% c("WA", "CA", "OR", "AK", "HI") ~ 9,
      T ~ NA
    ),
    south = ifelse(region %in% 5:7, 1, 0),
    
    ## Partisanship and Ideology (higher values more Republican/conservative)
    # Partisan Identification
    pid = case_when(
      dstr == 1 ~ 0,
      dstr == 2 ~ 1,
      lean == 3 ~ 2,
      lean == 4 ~ 3,
      lean == 5 ~ 4,
      rstr == 6 ~ 5,
      rstr == 7 ~ 6,
      T ~ NA) / 6,
    # Symbolic Ideology
    ideo_ = (ideo - 1) / 6,
    
    ## Psychological Predispositions
    # Authoritarianism
    auth1_ = ifelse(!is.na(auth1), auth1 - 1, NA), 
    auth2_ = ifelse(!is.na(auth2), auth2 - 1, NA), 
    auth3_ = ifelse(!is.na(auth3), 2 - auth3, NA),
    auth4_ = ifelse(!is.na(auth4), auth4 - 1, NA),
    auth5_ = ifelse(!is.na(auth5), 2 - auth5, NA),
    auth6 = NA,
    auth7 = NA,
    auth8 = NA,
    auth_scale = rowMeans(matrix(c(auth1_, auth2_, auth3_, auth4_, auth5_), ncol = 5), na.rm = T),
    # Big 5 Personality (Conscientiousness and Openness)
    consc1 = NA,
    consc2 = NA,
    consc3 = NA,
    consc_scale = NA,
    open1 = NA,
    open2 = NA,
    open3 = NA,
    open4 = NA,
    open5 = NA,
    open6 = NA,
    open_scale = NA,
    # Right-Wing Authoritarianism
    rwa1 = NA,
    rwa2 = NA,
    rwa3 = NA,
    rwa4 = NA,
    rwa5 = NA,
    rwa6 = NA,
    rwa7 = NA,
    rwa8 = NA,
    rwa9 = NA,
    rwa10 = NA,
    rwa11 = NA,
    rwa12 = NA,
    rwa13 = NA,
    rwa14 = NA,
    rwa15 = NA,
    rwa16 = NA,
    rwa17 = NA,
    rwa18 = NA,
    rwa_scale = NA,
    # Moral Foundations
    authority = NA,
    purity = NA,
    loyalty = NA,
    loyal2 = NA,
    care = NA,
    care2 = NA,
    fairness = NA,
    fair2 = NA,
    individualizing = NA,
    binding = NA,
    # Need for Structure
    nfs1 = NA,
    nfs2 = NA,
    nfs3 = NA,
    nfs4 = NA,
    nfs5 = NA,
    nfs6 = NA,
    nfs7 = NA,
    nfs8 = NA,
    nfs9 = NA,
    nfs10 = NA,
    nfs11 = NA,
    nfs12 = NA,
    nfs_scale = NA,
    
    ## Political Engagement
    # Political Interest
    polint1 = (attention - 1) / 4,
    polint2 = news / 7,
    polint3 = NA,
    polint4 = NA,
    polint5 = NA,
    polint6 = NA,
    polint_scale = rowMeans(matrix(c(polint1, polint2), ncol = 2), na.rm = T),
    # Political Knowledge
    polknow1 = ifelse(!is.na(kn1), ifelse(kn1 == 1, 1, 0), NA),
    polknow2 = ifelse(!is.na(kn2), ifelse(kn2 == 3, 1, 0), NA),
    polknow3 = ifelse(!is.na(kn3), ifelse(kn3 == 3, 1, 0), NA),
    polknow4 = ifelse(!is.na(kn4), ifelse(kn4 == 2, 1, 0), NA),
    polknow5 = ifelse(!is.na(kn5), ifelse(kn5 == 2, 1, 0), NA),
    polknow6 = ifelse(!is.na(kn6), ifelse(kn6 == 1, 1, 0), NA),
    polknow7 = NA,
    polknow8 = NA,
    polknow_scale = rowMeans(matrix(c(polknow1, polknow2, polknow3, polknow4, polknow5, polknow6), ncol = 6), na.rm = T),
    # Overall Political Engagement
    polengage_scale = rowMeans(matrix(c(polint_scale, polknow_scale), ncol = 2), na.rm = T),
    
    ## COVID Behaviors and Attitudes
    # COVID-Mitigation Behaviors ("Have you done any of the following to prevent the spread of coronavirus?")
    dv_travel = NA,
    dv_work_home = NA,
    dv_wash_hands = NA,
    dv_avoid_dine = NA,
    dv_cancel_event = NA,
    dv_sanitize = NA,
    dv_distancing1 = NA,
    dv_distancing2 = NA,
    dv_distancing = (15 - distancing) / 4,
    dv_masking1 = NA,
    dv_masking2 = NA,
    dv_masking = (5 - maskwearing) / 4,
    dv_vaxstatus = NA,
    dv_behavior = rowMeans(matrix(c(dv_distancing, dv_masking), ncol = 2), na.rm = T),
    # COVID Concern
    dv_concern = NA,
    # Public Health Measures
    dv_businesses_ = (7 - dv_businesses) / 6,
    dv_tracking_ = (7 - dv_tracking) / 6,
    dv_lockdowns_ = (7 - dv_lockdowns) / 6,
    dv_mmindoors = NA,
    dv_mmoutdoors = NA,
    dv_maskmandates = (7 - dv_masks) / 6,
    dv_vaxmandate = NA,
    dv_staterestrict = NA,
    dv_cancelevents = NA,
    dv_schools = NA,
    dv_workhome = NA,
    dv_travelrest = NA,
    dv_stayhome = NA,
    dv_testingreq = NA,
    dv_restrictions = rowMeans(matrix(c(dv_businesses_, dv_tracking_, dv_lockdowns_, dv_maskmandates), ncol = 4), na.rm = T),
    
    ## COVID variables not included in PAP
    # Vaccine Confidence
    vax1 = NA,
    vax2 = NA,
    vax3 = NA,
    vax4 = NA,
    vax5 = NA,
    vax_scale = NA,
    # Infection
    infected = NA
  )

names(Lucid2020) <- gsub("_$", "", names(Lucid2020)) # Remove trailing _ from variable names

write_xlsx(Lucid2020, "Lucid2020_coded.xlsx")


#================================================================#
#                        Lucid 2021 Recoding                     #
#================================================================#

# Drop Respondents who failed attention check
Lucid2021 <- Lucid2021[!is.na(Lucid2021$ResponseID) & Lucid2021$attn == "2,3" & Lucid2021$bot == "5,6" & Lucid2021$IC != 2 & Lucid2021$citizen != 2 & Lucid2021$adult != 2,]

Lucid2021 <- 
  Lucid2021 %>%
  transmute(
    
    ## Demographics
    sample = "Lucid2021",
    year = 2021,
    age_ = (age - min(na.omit(age))) / max(na.omit((age - min(na.omit(age))))),
    male = ifelse(gender %in% 1:3, ifelse(gender == 1, 1, 0), NA),
    white = ifelse(ethnicity_lucid == "1" & hispanic_lucid == "1", 1, 0),
    black = ifelse(ethnicity_lucid == "2" & hispanic_lucid == "1", 1, 0),
    hispanic = ifelse(hispanic_lucid != "1", 1, 0),
    otherrace = ifelse(white == 0 & black == 0 & hispanic == 0, 1, 0),
    race_ = relevel(factor(ifelse(white == 1, "White", ifelse(black == 1, "Black", ifelse(hispanic == 1, "Hispanic", ifelse(otherrace == 1, "Other", NA))))), "White"),
    income_ = (income - 1) / 12,
    education =
      ifelse(
        is.na(educ),
        case_when(
          education_lucid == "1" ~ 0,
          education_lucid == "2" ~ 0.2,
          education_lucid %in% c("3", "4") ~ 0.4,
          education_lucid == "5" ~ 0.6,
          education_lucid == "6" ~ 0.8,
          education_lucid %in% c("7", "8") ~ 1
        ),
        (educ - 1) / 5),
    
    ## Location
    ipaddress = as.numeric(gsub("\\.", "", IPAddress)),
    zip = zip_lucid,
    state = ifelse(
      is.na(zip), NA, 
      car::recode( 
        as.numeric(zip),
        "35000:36999 = 'AL';
              100 = 'AK';
      99500:99999 = 'AK';
      85000:86999 = 'AZ';
      71600:72999 = 'AR';
      90000:96699 = 'CA';
      80000:81999 = 'CO';
      6000:6999 = 'CT';
      19700:19999 = 'DE';
      32000:34999 = 'FL';
      30000:31999 = 'GA';
      96700:96999 = 'HI';
      83200:83999 = 'ID';
      60000:62999 = 'IL';
      46000:47999 = 'IN';
      50000:52999 = 'IA';
      66000:67999 = 'KS';
      40000:42999 = 'KY';
      70000:71599 = 'LA';
      3900:4999 = 'ME';
      20600:21999 = 'MD';
      1000:2799 = 'MA';
      48000:49999 = 'MI';
      55000:56999 = 'MN';
      38600:39999 = 'MS';
      63000:65999 = 'MO';
      59000:59999 = 'MT';
      27000:28999 = 'NC';
      58000:58999 = 'ND';
      68000:69999 = 'NE';
      88900:89999 = 'NV';
      3000:3899 = 'NH';
      7000:8999 = 'NJ';
      87000:88499 = 'NM';
      10000:14999 = 'NY';
      43000:45999 = 'OH';
      73000:74999 = 'OK';
      97000:97999 = 'OR';
      15000:19699 = 'PA';
      300:999 = 'PR';
      2800:2999 = 'RI';
      29000:29999 = 'SC';
      57000:57999 = 'SD';
      37000:38599 = 'TN';
      75000:79999 = 'TX';
      88500:88599 = 'TX';
      84000:84999 = 'UT';
      5000:5999 = 'VT';
      22000:24699 = 'VA';
      20000:20599 = 'DC';
      98000:99499 = 'WA';
      24700:26999 = 'WV';
      53000:54999 = 'WI';
      82000:83199 = 'WY';
      NA = NA"
      )
    ),
    region = case_when(
      state %in% c("CT", "NH", "RI", "ME", "MA", "VT") ~ 1,
      state %in% c("NY", "PA", "NJ") ~ 2,
      state %in% c("MI", "OH", "IN", "WI", "IL") ~ 3,
      state %in% c("IA", "NE", "MO", "ND", "MN", "SD", "KS") ~ 4,
      state %in% c("NC", "FL", "DC", "VA", "GA", "SC", "WV", "MD", "DE") ~ 5,
      state %in% c("TN", "AL", "KY", "MS") ~ 6,
      state %in% c("TX", "LA", "OK", "AR") ~ 7,
      state %in% c("CO", "AZ", "UT", "NM", "ID", "NV", "MT", "WY") ~ 8,
      state %in% c("WA", "CA", "OR", "AK", "HI") ~ 9,
      T ~ NA
    ),
    south = ifelse(region %in% 5:7, 1, 0),
    
    ## Partisanship and Ideology (higher values more Republican/conservative)
    # Partisan Identification
    pid = case_when(
      dstr == 1 ~ 0,
      dstr == 2 ~ 1,
      lean == 3 ~ 2,
      lean == 4 ~ 3,
      lean == 5 ~ 4,
      rstr == 6 ~ 5,
      rstr == 7 ~ 6,
      T ~ NA) / 6,
    # Symbolic Ideology
    ideo_ = (ideo - 1) / 6,
    
    ## Psychological Predispositions
    # Authoritarianism
    auth1_ = ifelse(!is.na(auth1), auth1 - 1, NA), 
    auth2_ = ifelse(!is.na(auth2), auth2 - 1, NA), 
    auth3_ = ifelse(!is.na(auth3), 2 - auth3, NA),
    auth4_ = ifelse(!is.na(auth4), auth4 - 1, NA),
    auth5_ = ifelse(!is.na(auth5), 2 - auth5, NA),
    auth6_ = ifelse(!is.na(auth6), auth6 - 1, NA),
    auth7_ = ifelse(!is.na(auth7), 2 - auth7, NA),
    auth8_ = ifelse(!is.na(auth8), 2 - auth8, NA),
    auth_scale = rowMeans(matrix(c(auth1_, auth2_, auth3_, auth4_, auth5_, auth6_, auth7_, auth8_), ncol = 8), na.rm = T),
    # Big 5 Personality (Conscientiousness and Openness)
    consc1 = NA,
    consc2 = NA,
    consc3 = NA,
    consc_scale = NA,
    open1 = NA,
    open2 = NA,
    open3 = NA,
    open4 = NA,
    open5 = NA,
    open6 = NA,
    open_scale = NA,
    # Right-Wing Authoritarianism
    rwa1 = NA,
    rwa2 = NA,
    rwa3 = NA,
    rwa4 = NA,
    rwa5 = NA,
    rwa6 = NA,
    rwa7 = NA,
    rwa8 = NA,
    rwa9 = NA,
    rwa10 = NA,
    rwa11 = NA,
    rwa12 = NA,
    rwa13 = NA,
    rwa14 = NA,
    rwa15 = NA,
    rwa16 = NA,
    rwa17 = NA,
    rwa18 = NA,
    rwa_scale = NA,
    # Moral Foundations
    authority = NA,
    purity = NA,
    loyalty = NA,
    loyal2 = NA,
    care = NA,
    care2 = NA,
    fairness = NA,
    fair2 = NA,
    individualizing = NA,
    binding = NA,
    # Need for Structure
    nfs1 = NA,
    nfs2 = NA,
    nfs3 = NA,
    nfs4 = NA,
    nfs5 = NA,
    nfs6 = NA,
    nfs7 = NA,
    nfs8 = NA,
    nfs9 = NA,
    nfs10 = NA,
    nfs11 = NA,
    nfs12 = NA,
    nfs_scale = NA,
    
    ## Political Engagement
    # Political Interest
    polint1 = (attention - 1) / 4,
    polint2 = news / 7,
    polint3 = NA,
    polint4 = NA,
    polint5 = NA,
    polint6 = NA,
    polint_scale = rowMeans(matrix(c(polint1, polint2), ncol = 2), na.rm = T),
    # Political Knowledge
    polknow1 = ifelse(!is.na(kn1), ifelse(kn1 == 1, 1, 0), NA),
    polknow2 = ifelse(!is.na(kn2), ifelse(kn2 == 3, 1, 0), NA),
    polknow3 = ifelse(!is.na(kn3), ifelse(kn3 == 3, 1, 0), NA),
    polknow4 = ifelse(!is.na(kn4), ifelse(kn4 == 1, 1, 0), NA),
    polknow5 = ifelse(!is.na(kn5), ifelse(kn5 == 4, 1, 0), NA),
    polknow6 = NA,
    polknow7 = NA,
    polknow8 = NA,
    polknow_scale = rowMeans(matrix(c(polknow1, polknow2, polknow3, polknow4, polknow5), ncol = 5), na.rm = T),
    # Overall Political Engagement
    polengage_scale = rowMeans(matrix(c(polint_scale, polknow_scale), ncol = 2), na.rm = T),
    
    ## COVID Behaviors and Attitudes
    # COVID-Mitigation Behaviors ("Have you done any of the following to prevent the spread of coronavirus?")
    dv_travel = NA,
    dv_work_home = NA,
    dv_wash_hands = NA,
    dv_avoid_dine = NA,
    dv_cancel_event = NA,
    dv_sanitize = NA,
    dv_distancing1_ = (15 - dv_distancing1) / 4,
    dv_distancing2_ = (15 - dv_distancing2) / 4,
    dv_distancing_ = rowMeans(matrix(c(dv_distancing1_, dv_distancing2_), ncol = 2), na.rm = T),
    dv_masking1 = (5 - dv_maskindoors) / 4,
    dv_masking2 = (5 - dv_maskoutdoors) / 4,
    dv_masking_ = rowMeans(matrix(c(dv_masking1, dv_masking2), ncol = 2), na.rm = T),
    dv_vaxstatus_ = (3 - dv_vaxstatus) / 2,
    dv_behavior = rowMeans(matrix(c(dv_distancing_, dv_masking_, dv_vaxstatus_), ncol = 3), na.rm = T),
    # COVID Concern
    dv_concern = NA,
    # Public Health Measures
    dv_businesses_ = (7 - dv_businesses) / 6,
    dv_tracking_ = (7 - dv_tracking) / 6,
    dv_lockdowns_ = (7 - dv_lockdowns) / 6,
    dv_mmindoors_ = (7 - dv_mmindoors) / 6,
    dv_mmoutdoors_ = (7 - dv_mmoutdoors) / 6,
    dv_maskmandates = rowMeans(matrix(c(dv_mmindoors_, dv_mmoutdoors_), ncol = 2), na.rm = T),
    dv_vaxmandate_ = (7 - dv_vaxmandate) / 6,
    dv_staterestrict = NA,
    dv_cancelevents = NA,
    dv_schools = NA,
    dv_workhome = NA,
    dv_travelrest = NA,
    dv_stayhome = NA,
    dv_testingreq = NA,
    dv_restrictions = rowMeans(matrix(c(dv_businesses_, dv_tracking_, dv_lockdowns_, dv_maskmandates, dv_vaxmandate_), ncol = 5), na.rm = T),
  
    ## COVID variables not included in PAP
    # Vaccine Confidence
    vax1 = NA,
    vax2 = NA,
    vax3 = NA,
    vax4 = NA,
    vax5 = NA,
    vax_scale = NA,
    # Infection
    infected = NA
  )

names(Lucid2021) <- gsub("_$", "", names(Lucid2021)) # Remove trailing _ from variable names

write_xlsx(Lucid2021, "Lucid2021_coded.xlsx")


#================================================================#
#                         ANES-GSS Recoding                      #
#================================================================#

ANESGSS2020 <- 
  ANESGSS2020 %>%
  transmute(
    
    ## Demographics
    sample = sample,
    year = year,
    age_ = age,
    male_ = male,
    white_ = white,
    black_ = black,
    hispanic_ = hispanic,
    otherrace_ = otherrace, 
    race_ = relevel(factor(ifelse(white_ == 1, "White", ifelse(black_ == 1, "Black", ifelse(hispanic_ == 1, "Hispanic", ifelse(otherrace_ == 1, "Other", NA))))), "White"),
    income = income01,
    education_ = education,
    
    ## Location
    ipaddress = NA,
    zip_ = NA,
    state_ = NA,
    region_ = region,
    south_ = ifelse(region_ %in% 5:7, 1, 0),
    
    ## Partisanship and Ideology (higher values more Republican/conservative)
    # Partisan Identification
    pid = pid01,
    # Symbolic Ideology
    ideo = ideo01,
    
    ## Psychological Predispositions
    # Authoritarianism
    auth1_ = auth1, 
    auth2_ = auth2, 
    auth3_ = auth3,
    auth4_ = auth4,
    auth5 = NA,
    auth6 = NA,
    auth7 = NA,
    auth8 = NA,
    auth_scale_ = rowMeans(matrix(c(auth1_, auth2_, auth3_, auth4_), ncol = 4), na.rm = T),
    # Big 5 Personality (Conscientiousness and Openness)
    consc1 = NA,
    consc2 = NA,
    consc3 = NA,
    consc_scale = NA,
    open1 = NA,
    open2 = NA,
    open3 = NA,
    open4 = NA,
    open5 = NA,
    open6 = NA,
    open_scale = NA,
    # Right-Wing Authoritarianism
    rwa1 = NA,
    rwa2 = NA,
    rwa3 = NA,
    rwa4 = NA,
    rwa5 = NA,
    rwa6 = NA,
    rwa7 = NA,
    rwa8 = NA,
    rwa9 = NA,
    rwa10 = NA,
    rwa11 = NA,
    rwa12 = NA,
    rwa13 = NA,
    rwa14 = NA,
    rwa15 = NA,
    rwa16 = NA,
    rwa17 = NA,
    rwa18 = NA,
    rwa_scale = NA,
    # Moral Foundations
    authority = NA,
    purity = NA,
    loyalty = NA,
    loyal2 = NA,
    care = NA,
    care2 = NA,
    fairness = NA,
    fair2 = NA,
    individualizing = NA,
    binding = NA,
    # Need for Structure
    nfs1 = NA,
    nfs2 = NA,
    nfs3 = NA,
    nfs4 = NA,
    nfs5 = NA,
    nfs6 = NA,
    nfs7 = NA,
    nfs8 = NA,
    nfs9 = NA,
    nfs10 = NA,
    nfs11 = NA,
    nfs12 = NA,
    nfs_scale = NA,
    
    ## Political Engagement
    # Political Interest
    polint1 = NA,
    polint2 = NA,
    polint3 = NA,
    polint4 = NA,
    polint5 = NA,
    polint6 = NA,
    polint_scale = NA,
    # Political Knowledge
    polknow1 = NA,
    polknow2 = NA,
    polknow3 = NA,
    polknow4 = know4,
    polknow5 = know5,
    polknow6 = know6,
    polknow7 = know7,
    polknow8 = know8,
    polknow_scale_ = rowMeans(matrix(c(polknow4, polknow5, polknow6, polknow7, polknow8), ncol = 5), na.rm = T),
    # Overall Political Engagement
    polengage_scale = rowMeans(matrix(c(polint_scale, polknow_scale_), ncol = 2), na.rm = T),
    
    ## COVID Behaviors and Attitudes
    # COVID-Mitigation Behaviors ("Have you done any of the following to prevent the spread of coronavirus?")
    dv_travel = NA,
    dv_work_home = NA,
    dv_wash_hands = NA,
    dv_avoid_dine = NA,
    dv_cancel_event = NA,
    dv_sanitize = NA,
    dv_distancing1 = NA,
    dv_distancing2 = NA,
    dv_distancing = NA,
    dv_masking1 = NA,
    dv_masking2 = NA,
    dv_masking = NA,
    dv_vaxstatus = NA,
    dv_behavior = NA,
    # COVID Concern
    dv_concern_ = dv_concern,
    # Public Health Measures
    dv_businesses = NA,
    dv_tracking = NA,
    dv_lockdowns = NA,
    dv_mmindoors = NA,
    dv_mmoutdoors = NA,
    dv_maskmandates = NA,
    dv_vaxmandate = NA,
    dv_staterestrict = NA,
    dv_cancelevents = NA,
    dv_schools = NA,
    dv_workhome = NA,
    dv_travelrest = NA,
    dv_stayhome = NA,
    dv_testingreq = NA,
    dv_restrictions = NA,
    
    ## COVID variables not included in PAP
    # Vaccine Confidence
    vax1 = NA,
    vax2 = NA,
    vax3 = NA,
    vax4 = NA,
    vax5 = NA,
    vax_scale = NA,
    # Infection
    infected = NA
  )

names(ANESGSS2020) <- gsub("_$", "", names(ANESGSS2020)) # Remove trailing _ from variable names

write_xlsx(ANESGSS2020, "ANESGSS2020_coded.xlsx")


#================================================================#
#                           VSG Recoding                         #
#================================================================#

VSG <- 
  VSG %>%
  transmute(
    
    ## Demographics
    sample = sample,
    year = 2020,
    age_ = age,
    male_ = male,
    white_ = white,
    black_ = black,
    hispanic_ = hispanic,
    otherrace_ = otherrace, 
    race_ = relevel(factor(ifelse(white_ == 1, "White", ifelse(black_ == 1, "Black", ifelse(hispanic_ == 1, "Hispanic", ifelse(otherrace_ == 1, "Other", NA))))), "White"),
    income = income01,
    education_ = education,
    
    ## Location
    ipaddress = NA,
    zip_ = zip,
    state = car::recode(
      state_full,
      "'Alabama' = 'AL';
       'Alaska' = 'AK';
       'Arizona' = 'AZ';
       'Arkansas' = 'AR';
       'California' = 'CA';
       'Colorado' = 'CO';
       'Connecticut' = 'CT';
       'Delaware' = 'DE';
       'Florida' = 'FL';
       'Georgia' = 'GA';
       'Hawaii' = 'HI';
       'Idaho' = 'ID';
       'Illinois' = 'IL';
       'Indiana' = 'IN';
       'Iowa' = 'IA';
       'Kansas' = 'KS';
       'Kentucky' = 'KY';
       'Louisiana' = 'LA';
       'Maine' = 'ME';
       'Maryland' = 'MD';
       'Massachusetts' = 'MA';
       'Michigan' = 'MI';
       'Minnesota' = 'MN';
       'Mississippi' = 'MS';
       'Missouri' = 'MO';
       'Montana' = 'MT';
       'North Carolina' = 'NC';
       'North Dakota' = 'ND';
       'Nebraska' = 'NE';
       'Nevada' = 'NV';
       'New Hampshire' = 'NH';
       'New Jersey' = 'NJ';
       'New Mexico' = 'NM';
       'New York' = 'NY';
       'Ohio' = 'OH';
       'Oklahoma' = 'OK';
       'Oregon' = 'OR';
       'Pennsylvania' = 'PA';
       'Puerto Rico' = 'PR';
       'Rhode Island' = 'RI';
       'South Carolina' = 'SC';
       'South Dakota' = 'SD';
       'Tennessee' = 'TN';
       'Texas' = 'TX';
       'Utah' = 'UT';
       'Vermont' = 'VT';
       'Virginia' = 'VA';
       'District of Columbia' = 'DC';
       'Washington' = 'WA';
       'West Virginia' = 'WV';
       'Wisconsin' = 'WI';
       'Wyoming' = 'WY';
        NA = NA"
      ),
    region_ = region,
    south_ = ifelse(region_ %in% 5:7, 1, 0),
    
    ## Partisanship and Ideology (higher values more Republican/conservative)
    # Partisan Identification
    pid = pid01,
    # Symbolic Ideology
    ideo = ideo01,
    
    ## Psychological Predispositions
    # Authoritarianism
    auth1_ = auth1, 
    auth2_ = auth2, 
    auth3_ = auth3,
    auth4_ = auth4,
    auth5 = NA,
    auth6 = NA,
    auth7 = NA,
    auth8 = NA,
    auth_scale_ = rowMeans(matrix(c(auth1_, auth2_, auth3_, auth4_), ncol = 4), na.rm = T),
    # Big 5 Personality (Conscientiousness and Openness)
    consc1 = NA,
    consc2 = NA,
    consc3 = NA,
    consc_scale = NA,
    open1 = NA,
    open2 = NA,
    open3 = NA,
    open4 = NA,
    open5 = NA,
    open6 = NA,
    open_scale = NA,
    # Right-Wing Authoritarianism
    rwa1 = NA,
    rwa2 = NA,
    rwa3 = NA,
    rwa4 = NA,
    rwa5 = NA,
    rwa6 = NA,
    rwa7 = NA,
    rwa8 = NA,
    rwa9 = NA,
    rwa10 = NA,
    rwa11 = NA,
    rwa12 = NA,
    rwa13 = NA,
    rwa14 = NA,
    rwa15 = NA,
    rwa16 = NA,
    rwa17 = NA,
    rwa18 = NA,
    rwa_scale = NA,
    # Moral Foundations
    authority = NA,
    purity = NA,
    loyalty = NA,
    loyal2 = NA,
    care = NA,
    care2 = NA,
    fairness = NA,
    fair2 = NA,
    individualizing = NA,
    binding = NA,
    # Need for Structure
    nfs1 = NA,
    nfs2 = NA,
    nfs3 = NA,
    nfs4 = NA,
    nfs5 = NA,
    nfs6 = NA,
    nfs7 = NA,
    nfs8 = NA,
    nfs9 = NA,
    nfs10 = NA,
    nfs11 = NA,
    nfs12 = NA,
    nfs_scale = NA,
    
    ## Political Engagement
    # Political Interest
    polint1 = interest_2016,
    polint2 = interest_2017,
    polint3 = interest_2018,
    polint4 = interest_2019Nov,
    polint5 = interest_2019Jan,
    polint6 = interest_2020Sep,
    polint_scale = rowMeans(matrix(c(polint1, polint2, polint3, polint4, polint5, polint6), ncol = 6), na.rm = T),
    # Political Knowledge
    polknow1 = know1,
    polknow2 = know2,
    polknow3 = know3,
    polknow4 = know4,
    polknow5 = know5,
    polknow6 = know6,
    polknow7 = know7,
    polknow8 = NA,
    polknow_scale_ = rowMeans(matrix(c(polknow1, polknow2, polknow3, polknow4, polknow5, polknow6, polknow7), ncol = 7), na.rm = T),
    # Overall Political Engagement
    polengage_scale = rowMeans(matrix(c(polint_scale, polknow_scale_), ncol = 2), na.rm = T),
    
    ## COVID Behaviors and Attitudes
    # COVID-Mitigation Behaviors ("Have you done any of the following to prevent the spread of coronavirus?")
    dv_travel = NA,
    dv_work_home = NA,
    dv_wash_hands = NA,
    dv_avoid_dine = NA,
    dv_cancel_event = NA,
    dv_sanitize = NA,
    dv_distancing1 = NA,
    dv_distancing2 = NA,
    dv_distancing = NA,
    dv_masking1 = NA,
    dv_masking2 = NA,
    dv_masking = NA,
    dv_vaxstatus = NA,
    dv_behavior = NA,
    # COVID Concern
    dv_concern_ = dv_concern,
    # Public Health Measures
    dv_businesses = businessclosures,
    dv_tracking = NA,
    dv_lockdowns = NA,
    dv_mmindoors = NA,
    dv_mmoutdoors = NA,
    dv_maskmandates = NA,
    dv_vaxmandate = NA,
    dv_staterestrict = staterestrictions,
    dv_cancelevents = cancelevents,
    dv_schools = schoolclosures,
    dv_workhome = workfromhome,
    dv_travelrest = travelrestrictions,
    dv_stayhome = stayhome,
    dv_testingreq = testingreq,
    dv_restrictions_ = rowMeans(matrix(c(dv_businesses, dv_staterestrict, dv_cancelevents, dv_schools, dv_workhome, 
                                         dv_travelrest, dv_stayhome, dv_testingreq), ncol = 8), na.rm = T),
    
    ## COVID variables not included in PAP
    # Vaccine Confidence
    vax1 = NA,
    vax2 = NA,
    vax3 = NA,
    vax4 = NA,
    vax5 = NA,
    vax_scale = NA,
    # Infection
    infected = NA
  )

names(VSG) <- gsub("_$", "", names(VSG)) # Remove trailing _ from variable names

write_xlsx(VSG, "VSG_coded.xlsx")


#================================================================#
#       Estimating Latent Needs for Security and Certainty       #
#================================================================#

# Combine MTurk and Forthright datasets
BovTurk <- rbind(Bovitz2020, MTurk2020)

# Pre-registered model without covariances between reverse-coded items
model1 <- '
  authoritarianism =~ auth1 + auth2 + auth3 + auth4
  openness_to_experience =~ open1 + open2 + open3 + open4 + open5 + open6
  moral_foundations =~ authority + loyalty + purity
  need_for_structure =~ nfs1 + nfs2 + nfs3 + nfs4 + nfs5 + nfs6 + nfs7 + nfs8 + nfs9 + nfs10 + nfs11 + nfs12

  latent_nsc =~ authoritarianism + openness_to_experience + moral_foundations + need_for_structure
'

fit1 <- cfa(model1, data = BovTurk, estimator = "MLR", missing = "fiml")

summary(fit1, fit.measures = T) # RMSEA and SRMR are satisfactory; CFI and TLI are low but not terrible

# Deviation to model covariances between reverse-coded items
model2 <- '
  authoritarianism =~ auth1 + auth2 + auth3 + auth4
  openness_to_experience =~ open1 + open2 + open3 + open4 + open5 + open6
  moral_foundations =~ authority + loyalty + purity
  need_for_structure =~ nfs1 + nfs2 + nfs3 + nfs4 + nfs5 + nfs6 + nfs7 + nfs8 + nfs9 + nfs10 + nfs11 + nfs12

  latent_nsc =~ authoritarianism + openness_to_experience + moral_foundations + need_for_structure

  # Covariances for reversed items
  open2 ~~ open4 + open6
  open4 ~~ open6

  nfs3 ~~ nfs5 + nfs7 + nfs11
  nfs5 ~~ nfs7 + nfs11
  nfs7 ~~ nfs11
'
fit2 <- cfa(model2, data=BovTurk, estimator = "MLR", missing = "fiml")

summary(fit2, fit.measures = T) # RMSEA and SRMR are satisfactory; CFI and TLI are low but not terrible

# Generate predicted latent NSC scores
scores1 <- lavPredict(fit1, type = "lv")
scores2 <- lavPredict(fit2, type = "lv")

# Add these scores back to the combined dataset
BovTurk$latentnsc1 <- scores1[,"latent_nsc"]
Lucid2020$latentnsc1 <- NA
Lucid2021$latentnsc1 <- NA
ANESGSS2020$latentnsc1 <- NA
VSG$latentnsc1 <- NA

BovTurk$latentnsc2 <- scores2[,"latent_nsc"]
Lucid2020$latentnsc2 <- NA
Lucid2021$latentnsc2 <- NA
ANESGSS2020$latentnsc2 <- NA
VSG$latentnsc2 <- NA

pooled_data <- rbind(BovTurk, Lucid2020, Lucid2021, ANESGSS2020, VSG)

pooled_data <- pooled_data %>%
  mutate(
    latentnsc1 = (latentnsc1 - min(latentnsc1, na.rm = T)) / (max(latentnsc1, na.rm = T) - min(latentnsc1, na.rm = T)),
    latentnsc2 = (latentnsc2 - min(latentnsc2, na.rm = T)) / (max(latentnsc2, na.rm = T) - min(latentnsc2, na.rm = T))
  )

#================================================================#
#                     Recoding Race Variable                     #
#================================================================#

race_levels <- sort(unique(pooled_data$race))
pooled_data <- pooled_data %>%
  mutate(
    race_num = as.integer(factor(race, levels = race_levels)),
    race = ifelse(race_num == 4, 0L, race)
  )

#================================================================#
#              Division-Based Fractional Imputation              #
#================================================================#

sample_levels <- sort(unique(pooled_data$sample))
state_levels  <- sort(unique(pooled_data$state))

pooled_data <- pooled_data %>%
  mutate(
    sample_num = as.integer(factor(sample, levels = sample_levels)),
    state_num  = as.integer(factor(state,  levels = state_levels))
  )

n_states <- 51L
f_state  <- factor(pooled_data$state_num, levels = seq_len(n_states))
mf <- model.frame(~ f_state, na.action = na.pass)
mm <- model.matrix(~ f_state - 1, data = mf)
colnames(mm) <- sub("^f_state", "state_", colnames(mm))
mm[is.na(f_state), ] <- 0
state_df <- as.data.frame(mm)

pooled_data <- bind_cols(pooled_data, state_df)

set_wt <- function(df, col, val, mask) {
  df[[col]][mask] <- val
  df
}

# New England (Division 1)
mask <- is.na(pooled_data$state_num) & pooled_data$region == 1
pooled_data <- set_wt(pooled_data, "state_7",  0.2365, mask)
pooled_data <- set_wt(pooled_data, "state_22", 0.0901, mask)
pooled_data <- set_wt(pooled_data, "state_20", 0.4653, mask)
pooled_data <- set_wt(pooled_data, "state_31", 0.0911, mask)
pooled_data <- set_wt(pooled_data, "state_40", 0.0726, mask)
pooled_data <- set_wt(pooled_data, "state_47", 0.0425, mask)

# Middle Atlantic (Division 2)
mask <- is.na(pooled_data$state_num) & pooled_data$region == 2
pooled_data <- set_wt(pooled_data, "state_32", 0.2186, mask)
pooled_data <- set_wt(pooled_data, "state_35", 0.4754, mask)
pooled_data <- set_wt(pooled_data, "state_39", 0.3059, mask)

# East North Central (Division 3)
mask <- is.na(pooled_data$state_num) & pooled_data$region == 3
pooled_data <- set_wt(pooled_data, "state_15", 0.2704, mask)
pooled_data <- set_wt(pooled_data, "state_16", 0.1432, mask)
pooled_data <- set_wt(pooled_data, "state_23", 0.2131, mask)
pooled_data <- set_wt(pooled_data, "state_36", 0.2490, mask)
pooled_data <- set_wt(pooled_data, "state_49", 0.1244, mask)

# West North Central (Division 4)
mask <- is.na(pooled_data$state_num) & pooled_data$region == 4
pooled_data <- set_wt(pooled_data, "state_13", 0.1476, mask)
pooled_data <- set_wt(pooled_data, "state_17", 0.1359, mask)
pooled_data <- set_wt(pooled_data, "state_24", 0.2640, mask)
pooled_data <- set_wt(pooled_data, "state_25", 0.2847, mask)
pooled_data <- set_wt(pooled_data, "state_30", 0.0907, mask)
pooled_data <- set_wt(pooled_data, "state_29", 0.0360, mask)
pooled_data <- set_wt(pooled_data, "state_42", 0.0410, mask)

# South Atlantic (Division 5)
mask <- is.na(pooled_data$state_num) & pooled_data$region == 5
pooled_data <- set_wt(pooled_data, "state_9",  0.0150, mask)
pooled_data <- set_wt(pooled_data, "state_10", 0.3259, mask)
pooled_data <- set_wt(pooled_data, "state_11", 0.1621, mask)
pooled_data <- set_wt(pooled_data, "state_21", 0.0935, mask)
pooled_data <- set_wt(pooled_data, "state_28", 0.1580, mask)
pooled_data <- set_wt(pooled_data, "state_41", 0.0774, mask)
pooled_data <- set_wt(pooled_data, "state_46", 0.1306, mask)
pooled_data <- set_wt(pooled_data, "state_8",  0.0104, mask)
pooled_data <- set_wt(pooled_data, "state_50", 0.0271, mask)

# East South Central (Division 6)
mask <- is.na(pooled_data$state_num) & pooled_data$region == 6
pooled_data <- set_wt(pooled_data, "state_2",  0.2592, mask)
pooled_data <- set_wt(pooled_data, "state_18", 0.2323, mask)
pooled_data <- set_wt(pooled_data, "state_26", 0.1526, mask)
pooled_data <- set_wt(pooled_data, "state_43", 0.3561, mask)

# West South Central (Division 7)
mask <- is.na(pooled_data$state_num) & pooled_data$region == 7
pooled_data <- set_wt(pooled_data, "state_3",  0.0739, mask)
pooled_data <- set_wt(pooled_data, "state_19", 0.1142, mask)
pooled_data <- set_wt(pooled_data, "state_37", 0.0971, mask)
pooled_data <- set_wt(pooled_data, "state_44", 0.7148, mask)

# Mountain (Division 8)
mask <- is.na(pooled_data$state_num) & pooled_data$region == 8
pooled_data <- set_wt(pooled_data, "state_4",  0.2870, mask)
pooled_data <- set_wt(pooled_data, "state_6",  0.2317, mask)
pooled_data <- set_wt(pooled_data, "state_14", 0.0738, mask)
pooled_data <- set_wt(pooled_data, "state_27", 0.0435, mask)
pooled_data <- set_wt(pooled_data, "state_33", 0.1246, mask)
pooled_data <- set_wt(pooled_data, "state_34", 0.0850, mask)
pooled_data <- set_wt(pooled_data, "state_45", 0.1313, mask)
pooled_data <- set_wt(pooled_data, "state_51", 0.0231, mask)

# Pacific (Division 9)
mask <- is.na(pooled_data$state_num) & pooled_data$region == 9
pooled_data <- set_wt(pooled_data, "state_1",  0.0137, mask)
pooled_data <- set_wt(pooled_data, "state_5",  0.7369, mask)
pooled_data <- set_wt(pooled_data, "state_12", 0.0271, mask)
pooled_data <- set_wt(pooled_data, "state_38", 0.0790, mask)
pooled_data <- set_wt(pooled_data, "state_48", 0.1434, mask)


#================================================================#
#                       Save Pooled Dataset                      #
#================================================================#                     

write_xlsx(pooled_data, "pooled_data.xlsx")


