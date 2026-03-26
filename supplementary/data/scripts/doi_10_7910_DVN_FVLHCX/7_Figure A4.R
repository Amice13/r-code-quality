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
# Figure A4: Timing of primary elections, by month
#############################

# Selecting relevant columns 
race <- subset(election, select = c('raceid', 'window_end_date_pre_primary'))

# Formatting date of primary to months
race$month <- format(as.Date(race$window_end_date_pre_primary, 
                             format="%Y-%m-%d"),"%m")

# Keeping only unique observations
race <- unique(subset(race, select = c('raceid', 'month')))
race$election <- 1

# Calculating proportion of primaries, by month
plot_df <- race %>%
  group_by(month) %>%
  summarise(total_elections = sum(election))

# Producing plot of proportion of primaries 
calander <- ggplot(plot_df, aes(x=month, y=total_elections)) +
  geom_bar(stat="identity", color = 'black', fill = 'gray') +
  theme_bw() +
  labs(x = 'Primary Month', y = 'Number of Primary Races') +
  theme(axis.text.x =element_text(size  = 19),
        axis.text.y = element_text(size = 19),
        axis.title = element_text(size = 24)) +
  scale_x_discrete(labels = c('February', 'March', 'April', 'May',
                              'June', 'July', 'August', 'September'))

# Printing plot of proportion of primaries 
calander
ggsave('FigureA4.png', calander, width = 14, height = 10)
