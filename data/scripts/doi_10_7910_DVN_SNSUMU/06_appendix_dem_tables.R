# Replication Files for Sacred Speech: Analyzing the Influence of Congressional Leadership on Religious Rhetoric

# 06_appendix_dem_tables
# This file takes the "speeches" and "dcinbox" CSVs 
# It includes code to replicate the DiD and DiDiD regressions 
# (Tables 8-10) in the Appendix.

# Last updated March 20, 2024


# Initial settings -------------------------------------------------------------
my_packages <- c("clusterSEs", "stargazer", "dplyr", "lmtest", "sandwich") 
lapply(my_packages, require, character.only = TRUE) 


# Upload the data --------------------------------------------------------------
{df <- read.csv("R Data/speeches.csv")
df_dc <- read.csv("R Data/dcinbox.csv")}


# Table 8 ----------------------------------------------------------------------

# Data Set Up

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
  
  # Subsetting the data to only Democrats
  did_data_d <- did_data[did_data$party == "D",]
  did_data_dc_d <- did_data_dc[did_data_dc$Party == "Democrat",]
}


# Religious Terms in Speeches
sp1 <- lm(all_terms_count ~ black * j_speech + factor(bioguide_id), 
                         data = did_data_d)
sp1_se <- coeftest(sp1, vcov = vcovCL, cluster = ~bioguide_id)
# Religious Speeches
sp2 <- lm(all_terms_binary ~ black * j_speech + factor(bioguide_id), 
                          data = did_data_d)
sp2_se <- coeftest(sp2, vcov = vcovCL, cluster = ~bioguide_id)

# Religious Terms in Newsletters
news1 <- lm(all_terms_count ~ black * j_speech + factor(BioGuide.ID), 
                            data = did_data_dc_d)
news1_se <- coeftest(news1, vcov = vcovCL, cluster = ~BioGuide.ID)
# Religious Newsletters
news2 <- lm(all_terms_binary ~ black * j_speech + factor(BioGuide.ID), 
                             data = did_data_dc_d)
news2_se <- coeftest(news2, vcov = vcovCL, cluster = ~BioGuide.ID)


# Creating the table using regressions with clustered standard errors
stargazer(sp1_se,sp2_se,news1_se, news2_se, omit = c("bioguide_id","BioGuide.ID"))

# Creating the end of the table (observations, R^2, etc.)
# Note: while this doesn't use the clustered S.E. regressions, the data included
# (observations, R^2) is accurate for both clustered and unclustered regressions
stargazer(sp1,sp2,news1, news2, omit = c("bioguide_id","BioGuide.ID"))



# Table 9 ----------------------------------------------------------------------

# Data Set Up

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
  
  # Limit to just Democrats
  did_data <- did_data[did_data$party == "D",]
  did_data_dc <- did_data_dc[did_data_dc$Party == "Democrat",]
}

# Making a new binary variable (rel_person) which is 1 if the legislator used
# 3 or more religious references in speeches or newsletters and 0 otherwise
did_data <- did_data %>%
  group_by(bioguide_id) %>%
  mutate(rel_speaker = ifelse(sum(all_terms_count) >= 3, 1, 0)) %>%
  ungroup()

did_data_dc <- did_data_dc %>%
  group_by(bioguide_id) %>%
  mutate(rel_writer = ifelse(sum(all_terms_count) >= 3, 1, 0)) %>%
  ungroup()

did_data2 <- did_data %>%
  left_join(did_data_dc %>% select(bioguide_id, rel_writer), by = "bioguide_id")
did_data_dc <- did_data_dc %>%
  left_join(did_data %>% select(bioguide_id, rel_speaker), by = "bioguide_id")
did_data <- did_data2

did_data$rel_person <- ifelse(did_data$rel_speaker == 1 | did_data$rel_writer == 1,
                              1,0)
did_data_dc$rel_person <- ifelse(did_data_dc$rel_speaker == 1 | did_data_dc$rel_writer == 1,
                                 1,0)



# DiD Regression ---------------------------------------------------------------

# Religious Terms in Speeches
sp1 <- lm(all_terms_count ~ rel_person * j_speech + factor(bioguide_id), 
                         data = did_data)
sp1_se <- coeftest(sp1, vcov = vcovCL, cluster = ~bioguide_id)
# Religious Speeches
sp2 <- lm(all_terms_binary ~ rel_person * j_speech + factor(bioguide_id), 
                          data = did_data)
sp2_se <- coeftest(sp2, vcov = vcovCL, cluster = ~bioguide_id)

# Religious Terms in Newsletters
news1 <- lm(all_terms_count ~ rel_person * j_speech + factor(BioGuide.ID), 
                            data = did_data_dc)
news1_se <- coeftest(news1, vcov = vcovCL, cluster = ~BioGuide.ID)
# Religious Newsletters
news2 <- lm(all_terms_binary ~ rel_person * j_speech + factor(BioGuide.ID), 
                             data = did_data_dc)
news2_se <- coeftest(news2, vcov = vcovCL, cluster = ~BioGuide.ID)

# Creating the table using regressions with clustered standard errors
stargazer(sp1_se,sp2_se,news1_se, news2_se, omit = c("bioguide_id","BioGuide.ID"))

# Creating the end of the table (observations, R^2, etc.)
# Note: while this doesn't use the clustered S.E. regressions, the data included
# (observations, R^2) is accurate for both clustered and unclustered regressions
stargazer(sp1,sp2,news1, news2, omit = c("bioguide_id","BioGuide.ID"))




# Table 10: DiDiD --------------------------------------------------------------

{df <- read.csv("R Data/speeches.csv")
df_dc <- read.csv("R Data/dcinbox.csv")}

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
  
  # Making them only for Democrats
  did_data <- did_data[did_data$party == "D",]
  did_data_dc <- did_data_dc[did_data_dc$Party == "Democrat",]
}

# Add in our religious indicator
did_data <- did_data %>%
  group_by(bioguide_id) %>%
  mutate(rel_speaker = ifelse(sum(all_terms_count) >= 3, 1, 0)) %>%
  ungroup()

did_data_dc <- did_data_dc %>%
  group_by(bioguide_id) %>%
  mutate(rel_writer = ifelse(sum(all_terms_count) >= 3, 1, 0)) %>%
  ungroup()

did_data2 <- did_data %>%
  left_join(did_data_dc %>% select(bioguide_id, rel_writer), by = "bioguide_id")
did_data_dc <- did_data_dc %>%
  left_join(did_data %>% select(bioguide_id, rel_speaker), by = "bioguide_id")

did_data <- did_data2

did_data$rel_person <- ifelse(did_data$rel_speaker == 1 | did_data$rel_writer == 1,
                              1,0)
did_data_dc$rel_person <- ifelse(did_data_dc$rel_speaker == 1 | did_data_dc$rel_writer == 1,
                                 1,0)




# Table 2: DDD -----------------------------------------------------------------

# Religious Terms in Speeches
sp1 <- lm(all_terms_count ~ rel_person * j_speech_1 * Year + factor(bioguide_id), 
                         data = did_data)
sp1_se <- coeftest(sp1, vcov = vcovCL, cluster = ~bioguide_id)
# Religious Speeches
sp2 <- lm(all_terms_binary ~ rel_person * j_speech_1 * Year + factor(bioguide_id), 
                          data = did_data)
sp2_se <- coeftest(sp2, vcov = vcovCL, cluster = ~bioguide_id)

# Religious Terms in Newsletters
news1 <- lm(all_terms_count ~ rel_person * j_speech_1 * Year + factor(BioGuide.ID), 
                            data = did_data_dc)
news1_se <- coeftest(news1, vcov = vcovCL, cluster = ~BioGuide.ID)
# Religious Newsletters
news2 <- lm(all_terms_binary ~ rel_person * j_speech_1 * Year + factor(BioGuide.ID), 
                             data = did_data_dc)
news2_se <- coeftest(news2, vcov = vcovCL, cluster = ~BioGuide.ID)

# Creating the table
stargazer(sp1_se,sp2_se,news1_se, news2_se, omit = c("bioguide_id","BioGuide.ID"))

# Creating the bottom part of the table (observations, R^2, etc.)
stargazer(sp1,sp2,news1, news2, omit = c("bioguide_id","BioGuide.ID"))





