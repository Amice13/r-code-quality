#############################
# Load packages
#############################
library(tidyverse)

#############################
# Load data 
#############################
rm(list=setdiff(ls(), c('script', 'scripts', 'log_file')))

# Load candidate data
election <- readRDS('descriptive_df.rds')
# Load general election dates
general <- read_csv('general-dates.csv')
# Load primary dates
primary <- readRDS('primary_dates_long.rds')
colnames(primary) <- c('state_postal', 'date', 'year')

# Merge data
election <- left_join(election, primary, by = c('state_postal', 'year'))
election <- left_join(election, general, by = 'year')
election <- subset(election, year >= 2010)

#############################
# Figure A3: Proportion of first donation within various fundraising windows
#############################

# Determine if first donation fell within itemized donation window
election$first_capture <- ifelse(election$window_start_date_first >= 
                                   election$window_start_date_first &
                                   election$window_start_date_first <= 
                                   election$window_end_date_first, 1, 0)

# Print 
table(election$first_capture)

# Determine if first donation fell within FEC registration window
election$filing_capture <- ifelse(election$window_start_date_first >=
                                    election$window_start_date_registration_90 &
                                    election$window_start_date_first <=
                                    election$window_end_date_registration_90, 1, 0)

# Print
table(election$filing_capture)

# Determine if first donation fell within FEC quarterly report window
election$quarter_capture <- ifelse(election$window_start_date_first >=
                                     election$window_start_date_first &
                                     election$window_start_date_first <=
                                     election$window_end_date_quarter_90, 1, 0)

# Print
table(election$quarter_capture)

# Create and label empty table
table.data <- data.frame(matrix(ncol = 4, nrow = 5))
colnames(table.data) <- c('Candidates', 'First Donation', 
                          'FEC Registration', 'FEC First Report')

# Calculate proportion for general election winners
table.data[1,] <- c('General Election Winners', 
                    colSums(subset(election, wingen == 1)[,(ncol(election)-2):ncol(election)], 
                            na.rm=TRUE)/nrow(subset(election, wingen == 1)))

# Calculate proportion for all candidates
table.data[2,] <- c('All Primary Candidates', 
                    colSums(election[,(ncol(election)-2):ncol(election)], 
                            na.rm=TRUE)/nrow(election))

# Calculate proportion for incumbents 
table.data[3,] <- c('Incumbents',
                    colSums(subset(election, inc == 1)[,(ncol(election)-2):ncol(election)], 
                            na.rm=TRUE)/nrow(subset(election, inc == 1)))

# Calculate proportion for all non-incumbents 
table.data[4,] <- c('Primary Challengers', 
                    colSums(subset(election, inc == 0)[,(ncol(election)-2):ncol(election)], 
                            na.rm=TRUE)/nrow(subset(election, inc == 0)))

# Calculate proportion for all quality candidates 
table.data[5,] <- c('Experienced Primary Challengers', 
                    colSums(subset(election, qual_chall == 1)[,(ncol(election)-2):ncol(election)], 
                            na.rm=TRUE)/nrow(subset(election, qual_chall == 1)))

# Rounding values 
table.data$`First Donation` <- round(as.numeric(table.data$`First Donation`), 
                                     digits = 2)
table.data$`FEC Registration` <- round(as.numeric(table.data$`FEC Registration`), 
                                       digits = 2)
table.data$`FEC First Report` <- round(as.numeric(table.data$`FEC First Report`), 
                                       digits = 2)

# Print
table.data

stargazer(table.data, summary = FALSE, out = 'tableA3.txt')

