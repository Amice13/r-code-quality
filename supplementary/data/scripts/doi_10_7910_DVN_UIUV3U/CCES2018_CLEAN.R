# *****************************************************************
# OVERVIEW ####
# *****************************************************************
# CCES18_CLEAN.R
# Clean CES 2018 Data
# Jennifer Lin and Kristin Lunz Trujillo
# Created On: 2021 08 11

# *** The following script loads and cleans the CCES 2018 data
#   used in the paper. This results in a cleaned form of data 
#   and a survey object needed for the analyses. ***

# ** Before running this file, ensure that you have ran
#   - Functions.R
#   - Load_RUCA.R
#  or components of this code will result in an error. See 
#  00_GENERAL.R for more details **

# *****************************************************************
# LOAD RAW DATA ####
# *****************************************************************

# To load the data, there are 2 options:
#  1. Download and read in data files from source (Raw data can 
#     be found here: https://cces.gov.harvard.edu/) or
#  2. Leverage the Dataverse API and R package and read files 
#     directly.

# We load data directly from dataverse, but if this fails,
#   downlad the data from source and read it in by hand. The stata
#   version is recommended.
CCES_2018 <- get_dataframe_by_name(
  filename = "cces18_common_vv.dta",
  dataset = "10.7910/DVN/ZSBZ7K",
  original = TRUE,
  .f = haven::read_dta,
  server = "dataverse.harvard.edu"
) %>% zap_labels()

# *****************************************************************
# CLEAN DATA ####
# *****************************************************************

# Before cleaning the data, we need to get the RUCA codes for 
#   each respondent. Here, we combine the data from dataverse
#   with the RUCA codes that are available in Load_RUCA.R and in
#   Aux_Data/. 
CCES_2018_zip <- left_join(
  CCES_2018,
  RUCA,
  by = c("lookupzip" = "zipcode")
) %>% 
  distinct(caseid, .keep_all = TRUE)

# The following code cleans the various variables of
#   interest using the merged data with RUCA codes from above
CES_18_clean <- CCES_2018_zip %>% 
  mutate(
    # Self-identified place of residence
    residence      = case_when(
      urbancity == 1 ~ "City",
      urbancity == 2 ~ "Suburb",
      urbancity == 3 ~ "Town",
      urbancity == 4 ~ "Rural"
    ),
    residence = factor(
      residence,
      levels = c("City", "Suburb", "Town", "Rural")
    ),
    # Education
    educ            = clean_CCES(educ),
    # Income
    income          = recode(
      faminc_new, `97` = NaN, .default = faminc_new),
    # Gender where Female == 1
    FEMALE          = case_when(
      gender == 2 ~ TRUE,
      TRUE ~ FALSE
    ),
    # Race
    race           = clean_CCES(race),
    # Party ID
    pid7           = clean_CCES(pid7),
    # Ideology
    ideo5          = clean_CCES(ideo5),
    # Church Attendance 
    CHURCH         = clean_CCES(pew_churatd),
    # Age and age category
    age            = 2018 - birthyr,
    AGE            = case_when(
      age %in% c(18:24) ~ "18 - 24",
      age %in% c(25:44) ~ "25 - 44",
      age %in% c(45:64) ~ "45 - 64",
      age %in% c(65:80) ~ "65+"
    ),
    # Racial Resentment 
    rr_work        = clean_CCES(CC18_422e),
    rr_slavery     = clean_CCES(CC18_422f),
    rr_less        = clean_CCES(CC18_422g),
    rr_harder      = clean_CCES(CC18_422h),
    # Political Participation in the Past 12 months
    vote         = clean_CCES(CC18_401),
    # Voted in the election
    p_VOTED        = case_when(
      vote == 5 ~ 1,
      TRUE ~ 0
    ),
    # Attend a meeting
    p_meeting      = code_Oppose(CC18_417a_1),
    # Yard sign
    p_sign         = code_Oppose(CC18_417a_2),
    # Work in campaign
    p_work_campaign= code_Oppose(CC18_417a_3),
    # Attend a protest
    p_protest      = code_Oppose(CC18_417a_4),
    # Contact elected official
    p_contact      = code_Oppose(CC18_417a_5),
    # Donate Money
    p_donate_money = code_Oppose(CC18_417a_6),
    # Donate Blood
    p_donate_blood = code_Oppose(CC18_417a_7),
    # Participate in none of the above (NOTA)
    p_NOTA         = code_Oppose(CC18_417a_8),
    # Additive Scale for behavior items
    behavior_series= p_VOTED + p_meeting + p_sign + 
      p_work_campaign + p_protest + p_contact +
      p_donate_money + p_donate_blood,
    # Post about politics on social media
    sm_post        = code_Oppose(CC18_300d_1),
    # Make a comment
    sm_comment     = code_Oppose(CC18_300d_2),
    # Read story about politics
    sm_read        = code_Oppose(CC18_300d_3),
    # Followed political event
    sm_event       = code_Oppose(CC18_300d_4),
    # Forwarded political content
    sm_forward     = code_Oppose(CC18_300d_5),
    # Additive scale for social media participation
    social_series  = sm_post + sm_comment + sm_read +
      sm_event + sm_forward,
    # Recode RUCA measure to categorical
    RUCAx = case_when(
      RUCA2 == 1.0 ~ "1",
      RUCA2 == 1.1 ~ "1",
      RUCA2 == 2.0 ~ "1",
      RUCA2 == 2.1 ~ "1",
      RUCA2 == 3.0 ~ "1",
      RUCA2 == 4.1 ~ "1",
      RUCA2 == 5.1 ~ "1",
      RUCA2 == 7.1 ~ "1",
      RUCA2 == 8.1 ~ "1",
      RUCA2 == 10.1 ~ "1",
      RUCA2 == 4.0 ~ "2",
      RUCA2 == 4.2 ~ "2", 
      RUCA2 == 5.0 ~ "2",
      RUCA2 == 5.2 ~ "2",
      RUCA2 == 6.0 ~ "2",
      RUCA2 == 6.1 ~ "2",
      RUCA2 == 7.0 ~ "3",
      RUCA2 == 7.1 ~ "3",
      RUCA2 == 7.2 ~ "3",
      RUCA2 == 7.3 ~ "3",
      RUCA2 == 7.4 ~ "3",
      RUCA2 == 8.0 ~ "3",
      RUCA2 == 8.2 ~ "3",
      RUCA2 == 8.3 ~ "3",
      RUCA2 == 8.4 ~ "3",
      RUCA2 == 9.0 ~ "3",
      RUCA2 == 9.1 ~ "3",
      RUCA2 == 9.2 ~ "3",
      RUCA2 == 10.0 ~ "4",
      RUCA2 == 10.2 ~ "4",
      RUCA2 == 10.3 ~ "4",
      RUCA2 == 10.4 ~ "4",
      RUCA2 == 10.5 ~ "4",
      RUCA2 == 10.6 ~ "4"
    ),
    RUCAx = as.factor(RUCAx)
  )

# Declare the cleaned version of the data a survey object,
#   using the provided commonweight variable as the weight.
CCES_18_survey <- CES_18_clean %>% 
  srvyr::as_survey(weight = commonweight)

# Clean up the output
rm("CCES_2018_zip", "CCES_2018")
