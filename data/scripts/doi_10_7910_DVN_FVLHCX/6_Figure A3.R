#############################
# Load packages
#############################
library(tidyverse)
library(ggridges)

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
# Figure A3: Distribution of first itemized contributions, relative to primary
#############################

# Convert first contribution dates to days before the election 
election$first_timing <-  as.numeric(as.Date(mdy(election$date)) - 
                                       as.Date(election$window_start_date_first))

# Calculate length of FEC measure window 
election$fec_window_length <-  as.Date(election$window_end_date_quarter_90, 
                                       format = '%Y-%m-%d') -
  as.Date(election$window_start_date_first, format="%Y-%m-%d")

# Set window length to NA if first contribution came after first quarterly report
election$fec_window_length <- ifelse(election$fec_window_length < 0, NA, 
                                     election$fec_window_length)

# Calculate prop. of non-incs. who began fundraising less than 90 days b4 primary
nrow(subset(election, first_timing <= 90 & inc == 0))/
  nrow(subset(election, inc == 0))

# Calculate prop. of incs. who began fundraising more than one year b4 primary
nrow(subset(election, first_timing >= 365  & inc == 1))/
  nrow(subset(election, inc == 1))

# Subset data for non-inc. individual plots  
primary_challengers <- subset(election, inc == 0)
primary_challengers$group <- 'Primary Challengers'

incumbents <- subset(election, inc == 1)
incumbents$group <- 'Incumbents'

quality <- subset(election, qual_chall == 1)
quality$group <- 'Experienced Challengers'

noninc_winners <- subset(election, inc == 0 & wingen == 1)
noninc_winners$group <- 'Non-Incumbent General \nElection Winners'

# Bind plot data 
plot_data <- rbind(primary_challengers, noninc_winners, quality, incumbents)

# Drop observations where fundraising began after first quarterly report 
plot_data <- subset(plot_data, !is.na(fec_window_length))
# Keep observations where length of quarterly report was less than 175 days
plot_data <- subset(plot_data, fec_window_length <= 175)

# Produce first itemized contribution plot
first_timing <- ggplot(plot_data, aes(x = first_timing, y = group, 
                                      group = group)) + 
  geom_density_ridges(alpha = 0.4) +
  theme_bw() +
  ylab('Candidate Type\n') +
  xlab('\nDays Before Primary Election') +
  geom_vline(xintercept = c(0,90), linetype="dotted", 
             color = "black", size=1.5) +
  theme(axis.text.x =element_text(size  = 22),
        axis.text.y = element_text(size = 22),
        axis.title = element_text(size = 24)) +
  scale_x_reverse()

# Print first itemized contribution plot
first_timing
ggsave('FigureA3.png', first_timing, width = 14, height = 10)

