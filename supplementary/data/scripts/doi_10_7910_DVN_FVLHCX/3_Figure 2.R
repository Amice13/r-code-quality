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
# Figure 2: Example from NY 2020 race
#############################

# Subset data to example candidates 
figure2 <- subset(election, year == 2020 & (candidate == 'Patel, Suraj' | 
                                              candidate == 'Maloney, Carolyn B.'))

# Calculate window length for first itemized measure 
figure2$first_start <- as.Date(figure2$window_start_date_first) - 
  as.Date(mdy(figure2$date))
figure2$first_end <- as.Date(figure2$window_end_date_first) - 
  as.Date(mdy(figure2$date))

# Calculate window length for quarterly measure 
figure2$quarter_start <- as.Date(figure2$window_start_date_quarter_90) - 
  as.Date(mdy(figure2$date))
figure2$quarter_end <- as.Date(figure2$window_end_date_quarter_90) - 
  as.Date(mdy(figure2$date))

# Calculate window length for registration date measure 
figure2$registration_start <- as.Date(figure2$window_start_date_registration_90) - 
  as.Date(mdy(figure2$date))
figure2$registration_end <- as.Date(figure2$window_end_date_registration_90) - 
  as.Date(mdy(figure2$date))

# Calculate window length for calendar year measure 
figure2$twelve_start <- as.Date(figure2$window_start_date_twelve_months_prior) - 
  as.Date(mdy(figure2$date))
figure2$twelve_end <- as.Date(figure2$window_end_date_twelve_months_prior) - 
  as.Date(mdy(figure2$date))

# Select relevant columns 
figure2 <- subset(figure2, select = c('candidate','first_start', 'first_end',
                                      'fundraising_amount_first',
                                      'quarter_start', 'quarter_end',
                                      'fundraising_amount_quarter_90',
                                      'registration_start', 'registration_end',
                                      'fundraising_amount_registration_90',
                                      'twelve_start', 'twelve_end',
                                      'fundraising_amount_twelve_months_prior'))

# Create measure DF
figure2_plotdata <- data.frame(matrix(ncol = 5, nrow = 8))
colnames(figure2_plotdata) <- c('candidate', 'measure', 'start', 'end', 'amount')

# Calculate fundraising total for each candidate, for each measure
for(j in 0:1){
  candidate <- figure2$candidate[j + 1]
  for(i in seq(2, 13, by = 3)){
    name <- str_remove(colnames(figure2)[i], '_start')
    figure2_plotdata$candidate[4*j + (i+1)/3] <- candidate
    figure2_plotdata$measure[4*j + (i+1)/3] <- name
    figure2_plotdata$start[4*j + (i+1)/3] <-figure2[j+1,i]
    figure2_plotdata$end[4*j + (i+1)/3] <- figure2[j+1,i+1]
    figure2_plotdata$amount[4*j + (i+1)/3] <- figure2[j+1,i+2]
  }
}

# Convert "measure" to factor variable, make other changes 
figure2_plotdata$measure <- factor(figure2_plotdata$measure, 
                                   levels = c('first', 'quarter', 
                                              'registration', 'twelve'))
figure2_plotdata$start <- figure2_plotdata$start*-1
figure2_plotdata$end <- figure2_plotdata$end*-1
figure2_plotdata$amount <- paste('$', 
                                 formatC(round(as.numeric(figure2_plotdata$amount), 0), 
                                         format = 'd', big.mark = ','), sep = '')

last.bumpup <- list('last.points', 'bumpup')

# Produce Patel plot for length of window, fundraising  
patel <- ggplot(subset(figure2_plotdata, 
                       candidate == 'Patel, Suraj'), 
                aes(y=measure)) +
  geom_segment(aes(x=start, xend=end, y=measure, yend=measure), size=10) +
  geom_dl(aes(x = end, y = measure, label = amount), 
          method = list(dl.trans(x = x + .1), 
                        "last.bumpup", cex = 1.3, 
                        colour = "black")) +
  theme_classic() +
  xlim(0, 550) +
  geom_vline(xintercept = 0, color = 'black') +
  theme(axis.text =element_text(size  = 16),
        axis.title = element_text(size = 24)) +
  labs(x = '\nDays Until Primary', y = 'Measure\n') +
  scale_y_discrete(labels = c('Candidate-Centered\nFirst Itemized', 
                              'Candidate-Centered\nFEC First Report',
                              'Candidate-Centered\nCampaign Registration', 
                              'Election-Centered\n12 Months')) +
  scale_x_reverse()

# Print Patel plot 
patel
ggsave('Figure2_patel.png', patel, width = 14, height = 10)


# Produce Maloney plot for length of window, fundraising  
maloney <- ggplot(subset(figure2_plotdata, 
                         candidate == 'Maloney, Carolyn B.'), 
                  aes(y=measure)) +
  geom_segment(aes(x=start, xend=end, y=measure, yend=measure), size=10) +
  geom_dl(aes(x = end, y = measure, label = amount), 
          method = list(dl.trans(x = x + .1), "last.bumpup", cex = 1.3, 
                        colour = "black")) +
  theme_classic() +
  xlim(0, 550) +
  geom_vline(xintercept = 0, color = 'black') +
  theme(axis.text =element_text(size  = 16),
        axis.title = element_text(size = 24)) +
  labs(x = '\nDays Until Primary', y = 'Measure\n') +
  scale_y_discrete(labels = c('Candidate-Centered\nFirst Itemized', 
                              'Candidate-Centered\nFEC First Report',
                              'Candidate-Centered\nCampaign Registration', 
                              'Election-Centered\n12 Months')) +
  scale_x_reverse()

# Print Maloney plot 
maloney
ggsave('Figure2_maloney.png', maloney, width = 14, height = 10)
