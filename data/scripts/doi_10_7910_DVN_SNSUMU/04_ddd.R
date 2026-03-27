# Replication Files for Sacred Speech: Analyzing the Influence of Congressional Leadership on Religious Rhetoric

# 04_ddd
# This file takes the "speeches" and "dcinbox" CSVs 
# It includes code to replicate the difference-in-difference-in-differences  
# regressions (Table 2) in the paper and Table 5 in the Appendix.
# It also includes code to replicate the DDDs done for each separate dictionary
# as included in Tables 6 and 7 in the Appendix.

# Last updated July 18, 2024

# Initial settings -------------------------------------------------------------
my_packages <- c("clusterSEs", "stargazer", "dplyr", "lmtest", "sandwich") 
lapply(my_packages, require, character.only = TRUE) 

# Upload the data --------------------------------------------------------------
{df <- read.csv("R Data/speeches.csv")
df_dc <- read.csv("R Data/dcinbox.csv")}


# Setting up data --------------------------------------------------------------

{# Setting up Dates (2 months before/after Oct 25, 2023 and Oct 25, 2022)
  df$full_dates <- as.Date(df$date, format = "%m/%d/%y")
  start_date_23 <- as.Date("8/25/23", format = "%m/%d/%y")
  stop_date_23 <- as.Date("12/25/23", format = "%m/%d/%y")
  start_date_22 <- as.Date("8/25/22", format = "%m/%d/%y")
  stop_date_22 <- as.Date("12/25/22", format = "%m/%d/%y")
}

{# Speeches
  # limit data to the correct time period
  did_data_23 = df[df$full_dates >= start_date_23 & df$full_dates <= stop_date_23 ,]
  did_data_22 = df[df$full_dates >= start_date_22 & df$full_dates <= stop_date_22 ,]
  # make the j_speech variables the same
  did_data_23$j_speech_1 <- did_data_23$j_speech
  did_data_22$j_speech_1 <- did_data_22$j_speech_22
  # Merge 2022 and 2023 datasets together
  did_data = rbind(did_data_23,did_data_22)
  # Remove Johnson's speech
  did_data <- did_data[!(did_data$date == "10/25/23" & did_data$bioguide_id=="J000299"),]
  # Change year variable to be binary
  did_data$Year <- ifelse(did_data$Year == 2023, 1, 0)
  
  # Newsletters
  # limit data to the correct time period
  did_data_23_dc = df_dc[df_dc$full_dates >= start_date_23 & df_dc$full_dates <= stop_date_23 ,]
  did_data_22_dc = df_dc[df_dc$full_dates >= start_date_22 & df_dc$full_dates <= stop_date_22 ,]
  # Making the j_speech variables the same
  did_data_23_dc$j_speech_1 <- did_data_23_dc$j_speech
  did_data_22_dc$j_speech_1 <- did_data_22_dc$j_speech_22
  # Merge 2022 and 2023 datasets together
  did_data_dc = rbind(did_data_23_dc,did_data_22_dc)
  did_data_dc$bioguide_id <- did_data_dc$BioGuide.ID
  # Change year variable to be binary
  did_data_dc$Year <- ifelse(did_data_dc$Year == 2023, 1, 0)
}


# Paper Table 2 and Appendix Table 5: DDD --------------------------------------

# Religious Terms in Speeches
ddd_sp_ter <- lm(all_terms_count ~ rep * j_speech_1 * Year + factor(bioguide_id), 
                         data = did_data)
ddd_sp_ter2 <- coeftest(ddd_sp_ter, vcov = vcovCL, cluster = ~bioguide_id)
# Religious Speeches
ddd_sp_bin <- lm(all_terms_binary ~ rep * j_speech_1 * Year + factor(bioguide_id), 
                          data = did_data)
ddd_sp_bin2 <- coeftest(ddd_sp_bin, vcov = vcovCL, cluster = ~bioguide_id)

# Religious Terms in Newsletters
ddd_nl_ter <- lm(all_terms_count ~ rep * j_speech_1 * Year + factor(BioGuide.ID), 
                            data = did_data_dc)
ddd_nl_ter2 <- coeftest(ddd_nl_ter, vcov = vcovCL, cluster = ~BioGuide.ID)
# Religious Newsletters
ddd_nl_bin <- lm(all_terms_binary ~ rep * j_speech_1 * Year + factor(BioGuide.ID), 
                             data = did_data_dc)
ddd_nl_bin2 <- coeftest(ddd_nl_bin, vcov = vcovCL, cluster = ~BioGuide.ID)

# Creating the table
stargazer(ddd_sp_ter2, ddd_sp_bin2, ddd_nl_ter2, ddd_nl_bin2, 
          omit = c("bioguide_id","BioGuide.ID"))
# Creating the bottom part of the table (observations, R^2, etc.)
stargazer(ddd_sp_ter, ddd_sp_bin, ddd_nl_ter, ddd_nl_bin, 
          omit = c("bioguide_id","BioGuide.ID"))


# Appendix Table 6: DDD Dictionaries, Speeches  --------------------------------
# Running the initial DDD regressions (with fixed effects for legislators)
did_god <- lm(god_terms_count ~ rep * j_speech_1 * Year + factor(bioguide_id), 
                 data = did_data)
did_bless <- lm(blessings_terms_count ~ rep * j_speech_1 * Year + factor(bioguide_id), 
                   data = did_data)
did_scr <- lm(scripture_terms_count ~ rep * j_speech_1 * Year + factor(bioguide_id), 
                       data = did_data)
did_rel <- lm(religion_terms_count ~ rep * j_speech_1 * Year + factor(bioguide_id), 
                      data = did_data)
did_poli <- lm(political_terms_count ~ rep * j_speech_1 * Year + factor(bioguide_id), 
                       data = did_data)
did_wh <- lm(whistles_terms_count ~ rep * j_speech_1 * Year + factor(bioguide_id), 
                     data = did_data)

# Running regression with clustered standard errors
did_god2 <- coeftest(did_god, vcov = vcovCL, cluster = ~bioguide_id)
did_bless2 <- coeftest(did_bless, vcov = vcovCL, cluster = ~bioguide_id)
did_scr2 <- coeftest(did_scr, vcov = vcovCL, cluster = ~bioguide_id)
did_rel2 <- coeftest(did_rel, vcov = vcovCL, cluster = ~bioguide_id)
did_poli2 <- coeftest(did_poli, vcov = vcovCL, cluster = ~bioguide_id)
did_wh2 <- coeftest(did_wh, vcov = vcovCL, cluster = ~bioguide_id)

# Making the table
stargazer(did_god2, did_bless2, did_scr2, did_rel2, did_poli2, did_wh2,
          omit = c("bioguide_id"))
# Making the bottom of the table (observations, R^2, etc.)
stargazer(did_god, did_bless, did_scr, did_rel, did_poli, did_wh,
          omit = c("bioguide_id"))


# Appendix Table 7: DDD Dictionaries, Newsletters ------------------------------
# Running the initial DDD regressions (with fixed effects for legislators)
ndid_god <- lm(god_terms_count ~ rep * j_speech_1 * Year + factor(bioguide_id), 
                 data = did_data_dc)
ndid_bl <- lm(blessings_terms_count ~ rep * j_speech_1 * Year + factor(bioguide_id), 
                   data = did_data_dc)
ndid_scr <- lm(scripture_terms_count ~ rep * j_speech_1 * Year + factor(bioguide_id), 
                       data = did_data_dc)
ndid_rel <- lm(religion_terms_count ~ rep * j_speech_1 * Year + factor(bioguide_id), 
                      data = did_data_dc)
ndid_pol <- lm(political_terms_count ~ rep * j_speech_1 * Year + factor(bioguide_id), 
                       data = did_data_dc)
ndid_wh <- lm(whistles_terms_count ~ rep * j_speech_1 * Year + factor(bioguide_id), 
                     data = did_data_dc)

# Running regression with clustered standard errors
ndid_god2 <- coeftest(ndid_god, vcov = vcovCL, cluster = ~bioguide_id)
ndid_bl2 <- coeftest(ndid_bl, vcov = vcovCL, cluster = ~bioguide_id)
ndid_scr2 <- coeftest(ndid_scr, vcov = vcovCL, cluster = ~bioguide_id)
ndid_rel2 <- coeftest(ndid_rel, vcov = vcovCL, cluster = ~bioguide_id)
ndid_pol2 <- coeftest(ndid_pol, vcov = vcovCL, cluster = ~bioguide_id)
ndid_wh2 <- coeftest(ndid_wh, vcov = vcovCL, cluster = ~bioguide_id)

# Making the table
stargazer(ndid_god2, ndid_bl2, ndid_scr2, 
          ndid_rel2, ndid_pol2, ndid_wh2,
          omit = c("bioguide_id"))
# Making the bottom of the table (observations, R^2, etc.)
stargazer(ndid_god, ndid_bl, ndid_scr, 
          ndid_rel, ndid_pol, ndid_wh,
          omit = c("bioguide_id"))

