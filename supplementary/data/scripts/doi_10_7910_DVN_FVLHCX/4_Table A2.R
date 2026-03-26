#############################
# Load packages
#############################
library(tidyverse)
library(stargazer)

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
# Figure A2: Candidate first itemized receipt timing 
#############################

# Convert first contribution dates to days before the election
election$first_timing <-  as.numeric(as.Date(mdy(election$date)) - 
                                       as.Date(election$window_start_date_first))

# Classify first donation into categories for "months before the election"
election$twelve_months <- ifelse(election$first_timing >=365 - 0*(365/12), 1, 0)
election$eleven_months <- ifelse(election$first_timing >=365 - 1*(365/12), 1, 0)
election$ten_months <-  ifelse(election$first_timing >=365 - 2*(365/12), 1, 0)
election$nine_months <- ifelse(election$first_timing >=365 - 3*(365/12), 1, 0)
election$eight_months <- ifelse(election$first_timing >=365 - 4*(365/12), 1, 0)
election$seven_months <- ifelse(election$first_timing >=365 - 5*(365/12), 1, 0)
election$six_months <- ifelse(election$first_timing >=365 - 6*(365/12), 1, 0)
election$five_months <- ifelse(election$first_timing >=365 - 7*(365/12), 1, 0)
election$four_months <- ifelse(election$first_timing >=365 - 8*(365/12), 1, 0)
election$three_months <- ifelse(election$first_timing >=365 - 9*(365/12), 1, 0)
election$two_months <- ifelse(election$first_timing >=365 - 10*(365/12), 1, 0)
election$one_months <- ifelse(election$first_timing >=365 - 11*(365/12), 1, 0)

# Calculate cumulative proportion of fundraising time, for general election winners  
winners <- colSums(subset(election, wingen ==1)[,(ncol(election)-11):ncol(election)], 
                   na.rm = TRUE)/nrow(subset(election, wingen ==1 & !is.na(first_timing)))

# Calculate cumulative proportion of fundraising time, for non-inc. election winners  
noincwinners <- colSums(subset(election, wingen ==1 & 
                                 inc == 0)[,(ncol(election)-11):ncol(election)], 
                        na.rm = TRUE)/nrow(subset(election, wingen ==1 & 
                                                    !is.na(first_timing) & inc == 0))

# Calculate cumulative proportion of fundraising time, for quality candidates
quality <- colSums(subset(election, qual_chall ==1)[,(ncol(election)-11):ncol(election)], 
                   na.rm = TRUE)/nrow(subset(election, qual_chall ==1 & !is.na(first_timing)))

# Collapse totals into a table
table <- as.data.frame(t(rbind(winners, noincwinners, quality)))
colnames(table) <- c('All General Election Winners', 
                     'Non-Incumbent General Election Winners', 
                     'Quality Challengers')

# Print fundraising timing table 
stargazer(table, summary = FALSE, out = 'tableA2.txt')
