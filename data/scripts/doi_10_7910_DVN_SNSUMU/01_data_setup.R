# Replication Files for Sacred Speech: Analyzing the Influence of Congressional Leadership on Religious Rhetoric

# 01_data_setup
# This file takes "df_dict", "DCinbox_dict", and "CongressID" CSVs, cleans the data,
# and creates the "speeches" and "dcinbox" CSVs which are used for all analyses
# It then saves the data in a "R Data" file.

# Last updated July 18, 2024

# Initial settings -------------------------------------------------------------
library(dplyr)

# Upload data ------------------------------------------------------------------
{df <- read.csv("R Data/df_dict.csv")
congressID <- read.csv("R Data/CongressID.csv")
dc_df <- read.csv("R Data/DCinbox_dict.csv")
}

# Data Cleaning ----------------------------------------------------------------

# House speeches

# Merging in the Congress members' information by bioguide ID
df <- merge(df, congressID,by="bioguide_id")

{
  # Dates: correcting format & adding in Year variable
  df$full_dates <- as.Date(df$date, format = "%m/%d/%y")
  df$Year <- substr(df$full_dates, 1,4)
  
  # Creating binary variable for when Johnson became Speaker
  # 0 if before October 25, 1 if on or after Oct 25. Includes 2023 and 2022.
  johnson_date <- as.Date("10/25/23", format = "%m/%d/%y")
  johnson_date_22 <- as.Date("10/25/22", format = "%m/%d/%y")
  
  df$j_speech <- ifelse(df$full_dates < johnson_date, 0,
                        ifelse(df$full_dates >= johnson_date, 1, NA))
  df$j_speech_22 <- ifelse(df$full_dates < johnson_date_22, 0,
                           ifelse(df$full_dates >= johnson_date_22, 1, NA))

  # Drop speeches if they weren't given in the House
  df = df[df$chamber == "House",]
  
  # Drop speeches from American Samoa, Guam, Mariana Islands, Puerto Rico, Virgin Islands
  df <- df[!df$state %in% c("AS", "GU", "Guam", "MP", "PR", "VI"), ]
  
  # Add in binary variable for south or nonsouth
  df$south <- ifelse(df$state %in% c("DE","DC","FL","GA","MD","NC","SC","VA",
                                     "WV","AL","KY","MS","TN","AK","LA","OK","TX"), 1, 0)
  
  # Creating binary variable for whether legislator is Republican or Democrat
  df$rep = ifelse(df$party == "R", 1, ifelse(df$party == "D", 0, NA))
}

# DCInbox data
{
  # Dates: correcting format & adding in Year variable
  dc_df$Unix.Timestamp <- as.numeric(dc_df$Unix.Timestamp)
  dc_df$full_dates <- as.Date(as.POSIXct(dc_df$Unix.Timestamp/1000, origin="1970-01-01"))
  dc_df$Year <- substr(dc_df$full_dates, 1,4)
  
  # make bioguide variable with same variable name as Speech data
  dc_df$bioguide_id <- dc_df$BioGuide.ID

  # Creating binary variable for when Johnson became Speaker
  # 0 if before October 25, 1 if on or after Oct 25. Includes 2023 and 2022.
  dc_df$j_speech <- ifelse(dc_df$full_dates < johnson_date, 0,
                          ifelse(dc_df$full_dates >= johnson_date, 1, NA))
  dc_df$j_speech_22 <- ifelse(dc_df$full_dates < johnson_date_22, 0,
                              ifelse(dc_df$full_dates >= johnson_date_22, 1, NA))
  
  # Drop newsletters from American Samoa, Guam, Mariana Islands, Puerto Rico, Virgin Islands
  dc_df <- dc_df[!dc_df$State %in% c("AS", "GU", "MP", "PR", "VI"), ]
  
  # Add in binary variable for south or nonsouth
  dc_df$south <- ifelse(dc_df$State %in% c("DE","DC","FL","GA","MD","NC","SC","VA",
                                           "WV","AL","KY","MS","TN","AK","LA","OK","TX"), 1, 0)
  
  # Creating binary variable for whether legislator is Republican or Democrat
  dc_df$rep = ifelse(dc_df$Party == "Republican", 1, 
                     ifelse(dc_df$Party == "Democrat", 0, NA))
  
  # Add in binary variable for if legislator is black
  dc_df <- dc_df %>%
    left_join(congressID %>% select(bioguide_id, black), by = "bioguide_id")
}

# Saving the Data --------------------------------------------------------------
write.csv(df, "R Data/speeches.csv")
write.csv(dc_df, "R Data/dcinbox.csv")

