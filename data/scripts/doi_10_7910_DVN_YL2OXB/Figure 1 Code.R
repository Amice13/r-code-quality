rm(list = ls())

library(readr)
library(tidyverse)
library(ggplot2)
library(reshape)
library(ggpubr)
library(wru)
library(weights)
library(stringr)
library(facetscales)
library(grid)

# constants
gridPts <- 50
useLog <- FALSE
saveImage <- FALSE

# source the legend function
setwd("Replication Code")         # set to the directory containing the code
source("Shift Legend Function.R")

################################################
##               data wrangling               ##
################################################

# read in the Census data
load('surnames2010.RData')

# read in our data and normalize it appropriately
load("last_nameRaceProbs.rData")
lastNames.ros <- last_nameRaceProbs
lastNames.ros$last_name <- toupper(lastNames.ros$name)

load("last_raceNameProbs.rData")
countData <- last_raceNameProbs
rawCounts <- rowSums(countData[,-1] * 
                       matrix(1/apply(countData[,-1], 2, FUN = function(x) {min(x[x > 0])}), 
                              ncol = 5, nrow = nrow(countData), byrow = TRUE))
countData$last_name <- toupper(countData$name)
countData$count <- rawCounts/7

lastNames.ros <- merge(lastNames.ros, countData[,c('last_name', 'count')], by = 'last_name')

# join the datasets
mergedData <- merge(lastNames.census, lastNames.ros[,-2], by.x = 'surname', by.y = 'last_name')

# create the long version of the data
data <- melt(mergedData, id.vars = c('surname', 'count'))
data$source <- ifelse(grepl('p_', data$variable), 'rawCensus', 'rawRosenman')
data$race <- ifelse(data$variable %in% c('p_whi', 'whi'), 'White',
                    ifelse(data$variable %in% c('p_bla', 'bla'), 'Black',
                           ifelse(data$variable %in% c('p_his', 'his'), 'Hispanic',
                                  ifelse(data$variable %in% c('p_asi', 'asi'), 'Asian', 'Other'))))

# final pivot 
data <- data %>% 
  pivot_wider(id_cols = c(surname, race, count),
              names_from = source,
              values_from = value)
data$race <- factor(data$race, levels = c('White', 'Black', 'Hispanic', 'Asian', 'Other'))

# use the version of the data we want
if(useLog) {
  data$Census <- log(data$rawCensus + 1)
  data$Rosenman <- log(data$rawRosenman + 1)
} else {
  data$Census <- data$rawCensus
  data$Rosenman <- data$rawRosenman
}

#################################################
##       computations for visualization        ##
#################################################

# preliminaries
raceGroups <- unique(data$race)

# coarsen the data
data$id <- NA
for(r in raceGroups) {
  
  # to generate square grids, use identical breaks for each variable
  breaks <- seq(from = 0, to = 1, length = gridPts + 1)
  data$id[data$race == r] <- paste(r, 
                                   cut(data$Census[data$race == r], breaks, include.lowest = TRUE),
                                   cut(data$Rosenman[data$race == r], breaks, include.lowest = TRUE))
}

# build the grid
grid <- data.frame(id = unique(data$id))
grid$race <- str_match(grid$id, "(^\\S+)")[,1]
grid$Census <- sapply(str_split(gsub("\\(|\\[|\\]", "", do.call(rbind, strsplit(grid$id, " "))[,2]), ","), FUN = function(x) {
  mean(as.numeric(x))
})
grid$Rosenman <- sapply(str_split(gsub("\\(|\\[|\\]", "", do.call(rbind, strsplit(grid$id, " "))[,3]), ","), FUN = function(x) {
  mean(as.numeric(x))
})
  
# populate the grid 
grid <- merge(grid, data[,c('id', 'count', 'race')], by = c('id', 'race')) %>%
  group_by(id, race, Census, Rosenman) %>%
  summarise(tally = sum(count))

#################################################
##           make the visualization            ##
#################################################

# order the factor
grid$race <- factor(grid$race, levels = c('White', 'Black', 'Hispanic', 'Asian', 'Other'))

# get the weighted correlations
cors <- sapply(unique(data$race), FUN = function(r) {
  cor(x = data$rawCensus[data$race == r], y = data$rawRosenman[data$race == r])[1]
})
wtd.cors <- sapply(unique(data$race), FUN = function(r) {
  wtd.cor(x = data$rawCensus[data$race == r], y = data$rawRosenman[data$race == r],
          weight = data$count[data$race == r])[1]
})
labels <- data.frame(Census = tapply(grid$Census, grid$race, max), 
                     Rosenman = tapply(grid$Rosenman, grid$race, max), 
                     race = unique(data$race), wtdCor = wtd.cors)
labels$lab <- paste("Wtd. Cor. =", format(round(labels$wtdCor, 2), nsmall = 2))

# make the plot
options(scipen = 10000000)
p <- ggplot(grid, 
       aes(x = Rosenman, y = Census)) + 
  geom_point(aes(alpha = tally)) +
  scale_alpha_continuous(trans = 'sqrt', limits = c(0, 1e5), breaks = c(25, 1000, 10000, 100000)) + 
#  scale_alpha_continuous(breaks = 10^c(1,3,5), range = c(1, 1e5)) + 
  facet_wrap(~race) + theme_bw() + xlim(c(0, 1)) + ylim(c(0, 1)) +
  ggtitle('P(race | surname) Distributions: Voter Files vs. Census') +
  geom_label(size = 4, data = labels, mapping = aes(x = 0.25*Rosenman, y = 0.95*Census, label = lab)) + 
  labs(alpha = "# of Appearances\non Voter File") + 
  theme(strip.text = element_text(size = 14)) + 
  theme(plot.title = element_text(size = 16)) + 
  geom_smooth(data, mapping = aes(weight = count), lwd = 1.5) + 
  xlab('Voter File Distribution of P(race | surname)') + 
  ylab('Census Distribution of P(race | surname)') + 
  geom_abline(intercept = 0, slope = 1, color = 'deeppink', lty = 2, lwd = 1.5) +
  theme(aspect.ratio=1) 

# move the legend and save it
p2 <- shift_legend(p)
if(saveImage) {
  ggsave('census_raceGivenName.png', plot = p2, width = 10, height = 7, units = 'in')
  ggsave('census_raceGivenName.pdf', plot = p2, width = 10, height = 7, units = 'in')
  
} else {
  plot(p2)
}
