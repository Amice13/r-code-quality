#############################
# Load packages
#############################
library(tidyverse)
library(ggcorrplot)

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
# Figure 1: Correlation plot, incumbents and non-incumbents
#############################

# Subset to non-incumbent and incumbent
nonincum <- subset(election, quality_cand != 2 & year >= 2010)
incum <- subset(election, quality_cand == 2 & year >= 2010)

# Produce non-incumbent correlations
nonincum_correlation <- nonincum[,c('fundraising_amount_first', 
                                    'fundraising_amount_quarter_90',
                                    'fundraising_amount_registration_90', 
                                    'fundraising_amount_twelve_months_prior')]
# Remove nas
nonincum_correlation <- na.omit(nonincum_correlation)
# Rounding
corr <- round(cor(nonincum_correlation), 2)

# Produce non-incumbent plot
correlation_nonincum <- ggcorrplot(corr, type = "lower",
                                   lab = TRUE,
                                   lab_col = "white", 
                                   lab_size = 19) +
  scale_fill_gradient2(breaks=c(0, 1), limit=c(0, 1), low = 'white', 
                       high = 'black',name = 'Correlation') +
  geom_vline(xintercept=1:ncol(nonincum_correlation)-0.5, colour="white", size=1) +
  geom_hline(yintercept=1:ncol(nonincum_correlation)-0.5, colour="white", size=1) +
  theme(axis.text.x =element_text(size  = 32),
        axis.text.y = element_text(size = 32),
        legend.position = 'none') +
  scale_y_discrete(labels = c('Candidate-Centered\nFirst Itemized', 
                              'Candidate-Centered\nFEC First Report', 
                              'Candidate-Centered\nCampaign Registration')) +
  scale_x_discrete(labels = c('Candidate-Centered\nFEC First Report', 
                              'Candidate-Centered\nCampaign Registration', 
                              'Election-Centered\n12 Months'))

# Print non-incumbent plot
correlation_nonincum

# Save figure
ggsave('Figure1a.png', correlation_nonincum, width = 10, height = 10)

# Produce incumbent correlations
incum_correlation <- incum[,c('fundraising_amount_first', 
                              'fundraising_amount_quarter_90',
                              'fundraising_amount_registration_90', 
                              'fundraising_amount_twelve_months_prior')]
# Remove nas
incum_correlation <- na.omit(incum_correlation)
# Rounding
corr <- round(cor(incum_correlation), 2)

# Produce incumbent plot
correlation_incum <- ggcorrplot(corr, type = "lower",
                                lab = TRUE,
                                lab_col = "white", 
                                lab_size = 19) +
  scale_fill_gradient2(breaks=c(0, 1), limit=c(0, 1), low = 'white', 
                       high = 'black', name = 'Correlation') +
  geom_vline(xintercept=1:ncol(incum_correlation)-0.5, colour="white", size=1) +
  geom_hline(yintercept=1:ncol(incum_correlation)-0.5, colour="white", size=1) +
  theme(axis.text.x =element_text(size  = 32),
        axis.text.y = element_text(size = 32),
        legend.position = "none") +
  scale_y_discrete(labels = c('Candidate-Centered\nFirst Itemized', 
                              'Candidate-Centered\nFEC First Report', 
                              'Candidate-Centered\nCampaign Registration')) +
  scale_x_discrete(labels = c('Candidate-Centered\nFEC First Report', 
                              'Candidate-Centered\nCampaign Registration', 
                              'Election-Centered\n12 Months'))

# Print incumbent plot
correlation_incum

ggsave('Figure1b.png', correlation_incum, width = 10, height = 10)

