#############################
# Load packages
#############################
library(tidyverse)
library(lubridate)
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
# Figure A7: Effective FEC window length, based on first contribution
#############################

# Calculate length of FEC first quarterly report window
election$fec_window_length <-  as.Date(election$window_end_date_quarter_90, 
                                       format = '%Y-%m-%d') -
  as.Date(election$window_start_date_first, 
          format="%Y-%m-%d")

# Setting window length to na if less than 0 days
election$fec_window_length <- ifelse(election$fec_window_length < 0, NA, 
                                     election$fec_window_length)

# Calculating median window length 
median(election$fec_window_length, na.rm = TRUE)

# Calculating number of obs. that are longer than standard length
election$quarter_less <- ifelse(election$window_start_date_quarter_90 <
                                  election$window_start_date_first, 1, 0)
table(election$quarter_less)

# Calculating number of obs. that are shorter than standard length
election$quarter_more <- ifelse(election$window_start_date_quarter_90 >
                                  election$window_start_date_first, 1, 0)
table(election$quarter_more)

# Calculating proportion of obs. with window shorter than 45 days 
nrow(subset(election, fec_window_length <= 45 ))/
  nrow(subset(election, !is.na(fec_window_length)))

# Calculating same proportion, for non-incumbent, general election winners 
nrow(subset(election, fec_window_length <= 45 & wingen == 1 & inc ==0))/
  nrow(subset(election, !is.na(fec_window_length) & wingen == 1 & inc == 0))

# Create subset dfs for plots
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

# Drop obs. without window length, window length more than 175 days
plot_data <- subset(plot_data, !is.na(fec_window_length))
plot_data <- subset(plot_data, fec_window_length <= 175)

# Produce distribution plots for window lengths 
fecwindow <- ggplot(plot_data, aes(x = fec_window_length, 
                                   y = group, group = group)) + 
  geom_density_ridges(alpha = 0.4) +
  theme_bw() +
  xlim(0, 120) +
  ylab('Candidate Type\n') +
  xlab('\nEffective FEC Window Length') +
  geom_vline(xintercept = 90, linetype="dotted", 
             color = "black", size=1.5) +
  theme(axis.text.x =element_text(size  = 22),
        axis.text.y = element_text(size = 22),
        axis.title = element_text(size = 24))

# Print distribution plots for window lengths 
fecwindow

ggsave('FigureA7.png', fecwindow, width = 14, height = 10)
