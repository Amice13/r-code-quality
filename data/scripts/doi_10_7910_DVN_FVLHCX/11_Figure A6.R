#############################
# Load packages
#############################
library(tidyverse)
library(lubridate)

#############################
# Load data 
#############################
rm(list=setdiff(ls(), c('script', 'scripts', 'log_file')))

# Load all itemized contributions
combined <- readRDS('all_itemized_contributions.rds')
combined <- combined[which(as.numeric(combined$Amount) > 0),]

# Remove keep only donations for 2010 election and beyond
combined <- subset(combined, year >= 2010)

#############################
# Figure A6: Timing of donations versus FEC deadlines
#############################

# For calculating proportion
combined$donation <- 1

# Reformat dates
combined$year_end <- as.Date(paste("12/31", combined$year, sep = "/"), 
                             format="%m/%d/%Y")

# Set dates relative to election date 
combined$difference <- as.numeric(combined$year_end - combined$Date)

# Calculate daily totals
plot_df <- combined %>%
  group_by(difference) %>%
  summarise(total_donations = sum(donation))

# Keeping totals within bounds of election cycle
plot_df <- subset(plot_df, difference >= 92 & difference <= 730)

# Calculate daily proportions
plot_df$percent_donations <- plot_df$total_donations/nrow(combined)

# Creating plot labels 
custom_labels <- c("-730" = "Jan 1\n(Year Prior)", 
                   "-640" = "Mar 31\n(Year Prior)", 
                   '-549' = 'Jun 30\n(Year Prior)',
                   '-457' = 'Sep 30\n(Year Prior)', 
                   '-365' = 'Dec 31\n(Year Prior)',
                   '-270' = 'Mar 31', 
                   '-184' = 'Jun 30', 
                   '-92' = 'Sep 30')

# Producing plot for daily campaign contributions
electioncycle <- ggplot(plot_df, aes(x = difference*-1, y = percent_donations))+
  geom_point() + 
  theme_bw() +
  xlab('\nTimeline of Election Cycle') +
  ylab('Daily Proportion of Donations\n') +
  theme(axis.text.x =element_text(size  = 26),
        axis.text.y = element_text(size = 26),
        axis.title = element_text(size = 32)) +
  scale_x_continuous(breaks = c(-730, -640, -549, -457, 
                                -365, -270, -184, -96), 
                     labels = custom_labels)

# Printing plot for daily campaign contributions
electioncycle

ggsave('FigureA6.png', electioncycle, width = 14, height = 10)

