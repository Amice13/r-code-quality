#############################
# Load packages
#############################
library(tidyverse)
library(lubridate)
library(stringr)
library(directlabels)

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
# Figure A2: Average fundraising window length, across measures
#############################

# Select relevant columns 
timing.primary <- subset(election, select = c('window_start_date_first', 
                                              'window_end_date_first',
                                              'window_start_date_quarter_90',
                                              'window_end_date_quarter_90',
                                              'window_start_date_registration_90',
                                              'window_end_date_registration_90',
                                              'window_start_date_nine_months_preelection',
                                              'window_end_date_nine_months_preelection',
                                              'window_start_date_twelve_months_prior',
                                              'window_end_date_twelve_months_prior',
                                              'date', 'general_date', 'inc'))

# Find length of window, for all candidates for all measures 
for(i in seq(1, 10, by = 2)){
  name.start <- str_remove(colnames(timing.primary)[i], 'window_')
  name.start <- str_remove(name.start, 'date_')
  
  name.end <- str_remove(colnames(timing.primary)[i+1], 'window_')
  name.end <- str_remove(name.end, 'date_')
  
  timing.primary$x <- as.Date(timing.primary[,i]) - as.Date(mdy(timing.primary$date))
  colnames(timing.primary)[ncol(timing.primary)] <- name.start 
  
  timing.primary$x <- as.Date(timing.primary[,i+1]) - as.Date(mdy(timing.primary$date))
  colnames(timing.primary)[ncol(timing.primary)] <- name.end 
}

# Remove objects 
rm(i, name.end, name.start)

# Select relevant columns 
timing.primary <- timing.primary[,13:ncol(timing.primary)]

# Create empty dataframes for plotting 
all.primary <- data.frame(matrix(ncol = 3, nrow = 5))
colnames(all.primary) <- c('measure', 'start', 'end')
incum.primary <- data.frame(matrix(ncol = 3, nrow = 5)) 
colnames(incum.primary) <- c('measure', 'start', 'end')
nonincum.primary <- data.frame(matrix(ncol = 3, nrow = 5))
colnames(nonincum.primary) <- c('measure', 'start', 'end')

# Produce avergae window length plot for each candidate type 
for(i in seq(2, 10, by = 2)){
  name <- str_remove(colnames(timing.primary)[i], 'start_')
  # All Primary 
  all.primary$measure[i/2] <- name
  all.primary$start[i/2] <- mean(timing.primary[,i], na.rm = TRUE)
  all.primary$end[i/2] <- mean(timing.primary[,i+1], na.rm = TRUE)
  
  # Incumbents
  incum.primary$measure[i/2] <- name
  incum.primary$start[i/2] <- mean(subset(timing.primary, inc == 1)[,i], na.rm = TRUE)
  incum.primary$end[i/2] <- mean(subset(timing.primary, inc == 1)[,i+1], na.rm = TRUE)
  
  # Non Incumbents
  nonincum.primary$measure[i/2] <- name
  nonincum.primary$start[i/2] <- mean(subset(timing.primary, inc == 0)[,i], 
                                      na.rm = TRUE)
  nonincum.primary$end[i/2] <- mean(subset(timing.primary, inc == 0)[,i+1], 
                                    na.rm = TRUE)
}

# Convert measure column to factor, make other changes 
incum.primary$measure <- factor(incum.primary$measure, 
                                levels = c('first', 'quarter_90', 
                                           'registration_90', 
                                           'twelve_months_prior', 
                                           'nine_months_preelection'))
incum.primary$start <- incum.primary$start*-1
incum.primary$end <- incum.primary$end*-1

# Produce incumbent window length plot
incumbents <- ggplot(incum.primary, aes(y=measure)) +
  geom_segment(aes(x=start, xend=end, y=measure, yend=measure), size=10) +
  theme_classic() +
  xlim(0, 533.1) + 
  theme(axis.text =element_text(size  = 16),
        axis.title = element_text(size = 24)) +
  geom_vline(xintercept = 0, color = 'black') +
  labs(x = 'Days Until Primary', y = 'Measure') +
  scale_x_reverse() +
  scale_y_discrete(labels = c('Candidate-Centered\nFirst Itemized', 
                              'Candidate-Centered\nFEC First Report',
                              'Candidate-Centered\nCampaign Registration', 
                              'Election-Centered\n9 Months',  
                              'Election-Centered\n12 Months'))

# Print incumbent window length plot 
incumbents
ggsave('FigureA2a.png', incumbents, width = 14, height = 10)


# Convert measure column to factor, make other changes 
nonincum.primary$measure <- factor(nonincum.primary$measure, 
                                   levels = c('first', 'quarter_90', 
                                              'registration_90', 
                                              'twelve_months_prior', 
                                              'nine_months_preelection'))
nonincum.primary$start <- nonincum.primary$start*-1
nonincum.primary$end <- nonincum.primary$end*-1

# Produce non- incumbent window length plot
nonincumbents <- ggplot(nonincum.primary, aes(y=measure)) +
  geom_segment(aes(x=start, xend=end, y=measure, yend=measure), size=10) +
  theme_classic() +
  xlim(0, 533.1) + 
  theme(axis.text =element_text(size  = 16),
        axis.title = element_text(size = 24)) +
  geom_vline(xintercept = 0, color = 'black') +
  labs(x = 'Days Until Primary', y = 'Measure') +
  scale_x_reverse() +
  scale_y_discrete(labels = c('Candidate-Centered\nFirst Itemized', 
                              'Candidate-Centered\nFEC First Report',
                              'Candidate-Centered\nCampaign Registration', 
                              'Election-Centered\n9 Months',  
                              'Election-Centered\n12 Months'))

# Print non-incumbent window length plot
nonincumbents
ggsave('FigureA2b.png', nonincumbents, width = 14, height = 10)

