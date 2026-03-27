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
# Figure A1: Correlation plots, by seat type
#############################

# Subset to non-incumbent and incumbent
nonincum_open <- subset(election, quality_cand != 2 & year >= 2010 & 
                          openseat == 'Open')
nonincum_notopen <- subset(election, quality_cand != 2 & year >= 2010 & 
                             openseat == 'Not Open')

# Subset to non-incumbent, open seats 
nonincum_open_correlation <- nonincum_open[,c('fundraising_amount_first', 
                                    'fundraising_amount_quarter_90',
                                    'fundraising_amount_registration_90', 
                                    'fundraising_amount_twelve_months_prior')]
# Remove nas
nonincum_open_correlation <- na.omit(nonincum_open_correlation)
# Rounding
corr <- round(cor(nonincum_open_correlation), 2)

# Produce non-incumbent correlation plot, open seat
correlation_nonincum_open <- ggcorrplot(corr, type = "lower",
                                        lab = TRUE,
                                        lab_col = "white", 
                                        lab_size = 19) +
  scale_fill_gradient2(breaks=c(0, 1), limit=c(0, 1), low = 'white', 
                       high = 'black', name = 'Correlation') +
  geom_vline(xintercept=1:ncol(nonincum_open_correlation)-0.5, colour="white", size=1) +
  geom_hline(yintercept=1:ncol(nonincum_open_correlation)-0.5, colour="white", size=1) +
  theme(axis.text.x =element_text(size  = 32),
        axis.text.y = element_text(size = 32),
        legend.position = 'none') +
  scale_y_discrete(labels = c('Candidate-Centered\nFirst Itemized', 
                              'Candidate-Centered\nFEC First Report', 
                              'Candidate-Centered\nCampaign Registration')) +
  scale_x_discrete(labels = c('Candidate-Centered\nFEC First Report', 
                              'Candidate-Centered\nCampaign Registration', 
                              'Election-Centered\n12 Months'))

# Print non-incumbent plot, open seat
correlation_nonincum_open

ggsave('FigureA1a.png', correlation_nonincum_open, width = 10, height = 10)


# Subset to non-incumbent, incumbent-held seat
nonincum_notopen_correlation <- nonincum_notopen[,c('fundraising_amount_first', 
                                                 'fundraising_amount_quarter_90',
                                                 'fundraising_amount_registration_90', 
                                                 'fundraising_amount_twelve_months_prior')]
# Remove nas
nonincum_notopen_correlation <- na.omit(nonincum_notopen_correlation)
# Rounding
corr <- round(cor(nonincum_notopen_correlation), 2)

# Produce non-incumbent correlation plot, incumbent-held seat
correlation_nonincum_notopen <- ggcorrplot(corr, type = "lower",
                                           lab = TRUE,
                                           lab_col = "white", 
                                           lab_size = 19) +
  scale_fill_gradient2(breaks=c(0, 1), limit=c(0, 1), low = 'white', 
                       high = 'black', name = 'Correlation') +
  geom_vline(xintercept=1:ncol(nonincum_notopen_correlation)-0.5, 
             colour="white", size=1) +
  geom_hline(yintercept=1:ncol(nonincum_notopen_correlation)-0.5, 
             colour="white", size=1) +
  theme(axis.text.x =element_text(size  = 32),
        axis.text.y = element_text(size = 32),
        legend.position = 'none') +
  scale_y_discrete(labels = c('Candidate-Centered\nFirst Itemized', 
                              'Candidate-Centered\nFEC First Report', 
                              'Candidate-Centered\nCampaign Registration')) +
  scale_x_discrete(labels = c('Candidate-Centered\nFEC First Report', 
                              'Candidate-Centered\nCampaign Registration', 
                              'Election-Centered\n12 Months'))

# Print non-incumbent plot, incumbent-held seat
correlation_nonincum_notopen

ggsave('FigureA1b.png', correlation_nonincum_notopen, width = 10, height = 10)

