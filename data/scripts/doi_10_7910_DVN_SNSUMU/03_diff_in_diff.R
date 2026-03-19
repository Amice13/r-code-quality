# Replication Files for Sacred Speech: Analyzing the Influence of Congressional Leadership on Religious Rhetoric

# 03_diff_in_diff
# This file takes the "speeches" and "dcinbox" CSVs 
# It includes code to replicate the difference-in-difference regression 
# (Table 2) in the Appendix.
# It also includes code to replicate the regression discontinuity referenced in
# the Difference in Difference section and included as Tables 3 & 4 in the Appendix.

# Last updated July 18, 2024


# Initial settings -------------------------------------------------------------
my_packages <- c("clusterSEs", "stargazer", "dplyr", "lmtest", "sandwich") 
lapply(my_packages, require, character.only = TRUE) 


# Upload the data --------------------------------------------------------------
{df <- read.csv("R Data/speeches.csv")
df_dc <- read.csv("R Data/dcinbox.csv")}

# Data Set Up ------------------------------------------------------------------

{# Setting the start and stop dates (2 months before/after Oct 25, 2023)
  start_date <- as.Date("8/25/23", format = "%m/%d/%y")
  stop_date <- as.Date("12/25/23", format = "%m/%d/%y")
  
  # House Speeches
  # Subsetting by start and stop dates
  df$full_dates <- as.Date(df$date, format = "%m/%d/%y")
  did_data = df[df$full_dates >= start_date & df$full_dates <= stop_date ,]
  # Remove Johnson's inaugural speech as Speaker
  did_data <- did_data[!(did_data$date == "10/25/23" & did_data$bioguide_id=="J000299"),]
  
  # Newsletters - subset by start and stop dates
  did_data_dc = df_dc[df_dc$full_dates >= start_date & df_dc$full_dates <= stop_date ,]
}


# Appendix Table 2: DiD Regression ------------------------------------------------------
# Note this includes fixed effects and clustered S.E.s by legislators (bioguide_id)

# Religious Terms in Speeches
did_sp <- lm(all_terms_count ~ rep * j_speech + factor(bioguide_id), 
                         data = did_data)
did_sp2 <- coeftest(did_sp, vcov = vcovCL, cluster = ~bioguide_id)
# Religious Speeches
did_sp_b <- lm(all_terms_binary ~ rep * j_speech + factor(bioguide_id), 
                          data = did_data)
did_sp_b2 <- coeftest(did_sp_b, vcov = vcovCL, cluster = ~bioguide_id)

# Religious Terms in Newsletters
did_n <- lm(all_terms_count ~ rep * j_speech + factor(BioGuide.ID), 
                            data = did_data_dc)
did_n2 <- coeftest(did_n, vcov = vcovCL, cluster = ~BioGuide.ID)
# Religious Newsletters
did_n_b <- lm(all_terms_binary ~ rep * j_speech + factor(BioGuide.ID), 
                             data = did_data_dc)
did_n_b2 <- coeftest(did_n_b, vcov = vcovCL, cluster = ~BioGuide.ID)

# Creating the table using regressions with clustered standard errors
stargazer(did_sp2,did_sp_b2, did_n2, did_n_b2, omit = c("bioguide_id","BioGuide.ID"))

# Creating the end of the table (observations, R^2, etc.)
# Note: while this doesn't use the clustered S.E. regressions, the data included
# (observations, R^2) is accurate for both clustered and unclustered regressions
stargazer(did_sp,did_sp_b, did_n, did_n_b, omit = c("bioguide_id","BioGuide.ID"))



# RDD (Appendix Tables 3 & 4) -----------------------------------------------------------

{# Setting up data
  # Limit data to 2 months around Oct 25, 2023 & Oct 25, 2022
  start_date_2023 <- as.Date("8/25/23", format = "%m/%d/%y")
  stop_date_2023 <- as.Date("12/25/23", format = "%m/%d/%y")
  start_date_2022 <- as.Date("8/25/22", format = "%m/%d/%y")
  stop_date_2022 <- as.Date("12/25/22", format = "%m/%d/%y")
  
  df_2023 = df[df$full_dates >= start_date_2023 & df$full_dates <= stop_date_2023 ,]
  df_2022 = df[df$full_dates >= start_date_2022 & df$full_dates <= stop_date_2022 ,]
  
  # Convert dates to Date format
  df_2023$date <- as.Date(df_2023$full_dates)
  df_2022$date <- as.Date(df_2022$full_dates)
  
  # Set up days before/after j_speech (October 25)
  df_2023$j_speech_days <- df_2023$date - as.Date("10/25/23", format = "%m/%d/%y")
  df_2022$j_speech_days <- df_2022$date - as.Date("10/25/22", format = "%m/%d/%y")
  
  # Newspapers
  # Limit data to 2 months around Oct 25, 2023 & Oct 25, 2022
  dc_df_2023 = df_dc[df_dc$full_dates >= start_date_2023 & df_dc$full_dates <= stop_date_2023 ,]
  dc_df_2022 = df_dc[df_dc$full_dates >= start_date_2022 & df_dc$full_dates <= stop_date_2022 ,]
  
  # Convert dates to Date format
  dc_df_2023$date <- as.Date(dc_df_2023$full_dates)
  dc_df_2022$date <- as.Date(dc_df_2022$full_dates)
  
  # Set up days before/after j_speech
  dc_df_2023$j_speech_days <- dc_df_2023$date - as.Date("10/25/23", format = "%m/%d/%y")
  dc_df_2022$j_speech_days <- dc_df_2022$date - as.Date("10/25/22", format = "%m/%d/%y")
}


# Appendix Table 3: RDD Regression, Speeches --------------------------------------------
# Number of religious terms in speeches
sp_ter <- lm(all_terms_count ~ j_speech + I(j_speech_days) + 
                       j_speech:I(j_speech_days) + factor(bioguide_id), 
                     data = df_2023)
sp_ter2 <- coeftest(sp_ter, vcov = vcovCL, cluster = ~bioguide_id)
# Number of religious speeches
sp_bin <- lm(all_terms_binary ~ j_speech + I(j_speech_days) +
                        j_speech:I(j_speech_days) + factor(bioguide_id), 
                      data = df_2023)
sp_bin2 <- coeftest(sp_bin, vcov = vcovCL, cluster = ~bioguide_id)

# Number of religious terms in speeches: Republicans only
sp_ter_r <- lm(all_terms_count ~ j_speech + I(j_speech_days) +
                         j_speech:I(j_speech_days) + factor(bioguide_id), 
                       data = df_2023[df_2023$rep == 1, ])
sp_ter_r2 <- coeftest(sp_ter_r, vcov = vcovCL, cluster = ~bioguide_id)
# Number of religious speeches: Republicans only
sp_bin_r <- lm(all_terms_binary ~ j_speech + I(j_speech_days) +
                          j_speech:I(j_speech_days) + factor(bioguide_id), 
                        data = df_2023[df_2023$rep == 1, ])
sp_bin_r2 <- coeftest(sp_bin_r, vcov = vcovCL, cluster = ~bioguide_id)

# Number of religious terms in speeches: Democrats only
sp_ter_d <- lm(all_terms_count ~ j_speech + I(j_speech_days) +
                         j_speech:I(j_speech_days) + factor(bioguide_id), 
                       data = df_2023[df_2023$rep == 0, ])
sp_ter_d2 <- coeftest(sp_ter_d, vcov = vcovCL, cluster = ~bioguide_id)
# Number of religious speeches: Democrats only
sp_bin_d <- lm(all_terms_binary ~ j_speech + I(j_speech_days) +
                          j_speech:I(j_speech_days) + factor(bioguide_id), 
                        data = df_2023[df_2023$rep == 0, ])
sp_bin_d2 <- coeftest(sp_bin_d, vcov = vcovCL, cluster = ~bioguide_id)

# Creating the table
stargazer(sp_ter2, sp_bin2, 
          sp_ter_r2, sp_bin_r2, 
          sp_ter_d2, sp_bin_d2, 
          omit = "bioguide_id")

# Creating the bottom of the table (observations, R^2, etc.)
stargazer(sp_ter, sp_bin, 
          sp_ter_r, sp_bin_r, 
          sp_ter_d, sp_bin_d, 
          omit = "bioguide_id")



# Appendix Table 4: RDD Regression, Newsletters -----------------------------------------

# Number of religious terms in newsletters
nl_ter <- lm(all_terms_count ~ j_speech + I(j_speech_days) + 
                       j_speech:I(j_speech_days) + factor(bioguide_id), 
                     data = dc_df_2023)
nl_ter2 <- coeftest(nl_ter, vcov = vcovCL, cluster = ~bioguide_id)
# Number of religious speeches
nl_bin <- lm(all_terms_binary ~ j_speech + I(j_speech_days) +
                        j_speech:I(j_speech_days) + factor(bioguide_id), 
                      data = dc_df_2023)
nl_bin2 <- coeftest(nl_bin, vcov = vcovCL, cluster = ~bioguide_id)

# Number of religious terms in newsletters; Republicans only
nl_ter_r <- lm(all_terms_count ~ j_speech + I(j_speech_days) +
                         j_speech:I(j_speech_days) + factor(bioguide_id), 
                       data = dc_df_2023[dc_df_2023$rep == 1, ])
nl_ter_r2 <- coeftest(nl_ter_r, vcov = vcovCL, cluster = ~bioguide_id)
# Number of religious speeches; Republicans only
nl_bin_r <- lm(all_terms_binary ~ j_speech + I(j_speech_days) +
                          j_speech:I(j_speech_days) + factor(bioguide_id), 
                        data = dc_df_2023[dc_df_2023$rep == 1, ])
nl_bin_r2 <- coeftest(nl_bin_r, vcov = vcovCL, cluster = ~bioguide_id)

# Number of religious terms in newsletters; Democrats only
nl_ter_d <- lm(all_terms_count ~ j_speech + I(j_speech_days) +
                         j_speech:I(j_speech_days) + factor(bioguide_id), 
                       data = dc_df_2023[dc_df_2023$rep == 0, ])
nl_ter_d2 <- coeftest(nl_ter_d, vcov = vcovCL, cluster = ~bioguide_id)
# Number of religious speeches; Democrats only
nl_bin_d <- lm(all_terms_binary ~ j_speech + I(j_speech_days) +
                          j_speech:I(j_speech_days) + factor(bioguide_id), 
                        data = dc_df_2023[dc_df_2023$rep == 0, ])
nl_bin_d2 <- coeftest(nl_bin_d, vcov = vcovCL, cluster = ~bioguide_id)

# Creating the table
stargazer(nl_ter2, nl_bin2, 
          nl_ter_r2, nl_bin_r2, 
          nl_ter_d2, nl_bin_d2, 
          omit = "bioguide_id")

# Creating the bottom of the table (observations, R^2, etc.)
stargazer(nl_ter, nl_bin, 
          nl_ter_r, nl_bin_r, 
          nl_ter_d, nl_bin_d, 
          omit = "bioguide_id")






