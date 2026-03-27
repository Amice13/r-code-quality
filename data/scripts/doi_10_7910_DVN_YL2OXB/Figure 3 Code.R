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

# read in the Tzioumis data
firstNames.tzi <- read_csv("firstnames.csv")

# read in our data and normalize it appropriately
load("first_nameRaceProbs.rData")
firstNames.ros <- first_nameRaceProbs
firstNames.ros$first_name <- toupper(firstNames.ros$name)

load("first_raceNameProbs.rData")
countData <- first_raceNameProbs
rawCounts <- rowSums(countData[,-1] * 
                       matrix(1/apply(countData[,-1], 2, FUN = function(x) {min(x[x > 0])}), 
                              ncol = 5, nrow = nrow(countData), byrow = TRUE))
countData$first_name <- toupper(countData$name)
countData$count <- rawCounts/7

firstNames.ros <- merge(firstNames.ros, countData[,c('first_name', 'count')], by = 'first_name')

# join the datasets
mergedData <- merge(firstNames.tzi, firstNames.ros[,-2], by.x = 'firstname', by.y = 'first_name')
mergedData$pctoth <- mergedData$pctaian + mergedData$pct2prace
mergedData <- mergedData[, !(names(mergedData) %in% c('obs', 'pctaian', 'pct2prace'))]

# create the long version of the data
data <- melt(mergedData, id.vars = c('firstname', 'count'))
data$source <- ifelse(grepl('pct', data$variable), 'rawTzioumis', 'rawRosenman')
data$race <- ifelse(data$variable %in% c('pctwhite', 'whi'), 'White',
                    ifelse(data$variable %in% c('pctblack', 'bla'), 'Black',
                           ifelse(data$variable %in% c('pcthispanic', 'his'), 'Hispanic',
                                  ifelse(data$variable %in% c('pctapi', 'asi'), 'Asian', 'Other'))))

# final pivot 
data <- data %>% 
  pivot_wider(id_cols = c(firstname, race, count),
              names_from = source,
              values_from = value)
data$race <- factor(data$race, levels = c('White', 'Black', 'Hispanic', 'Asian', 'Other'))

# use the version of the data we want
if(useLog) {
  data$Tzioumis <- log(data$rawTzioumis/100 + 1)
  data$Rosenman <- log(data$rawRosenman + 1)
} else {
  data$Tzioumis <- data$rawTzioumis/100
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
                                   cut(data$Tzioumis[data$race == r], breaks, include.lowest = TRUE),
                                   cut(data$Rosenman[data$race == r], breaks, include.lowest = TRUE))
}

# build the grid
grid <- data.frame(id = unique(data$id))
grid$race <- str_match(grid$id, "(^\\S+)")[,1]
grid$Tzioumis <- sapply(str_split(gsub("\\(|\\[|\\]", "", do.call(rbind, strsplit(grid$id, " "))[,2]), ","), FUN = function(x) {
  mean(as.numeric(x))
})
grid$Rosenman <- sapply(str_split(gsub("\\(|\\[|\\]", "", do.call(rbind, strsplit(grid$id, " "))[,3]), ","), FUN = function(x) {
  mean(as.numeric(x))
})
  
# populate the grid 
grid <- merge(grid, data[,c('id', 'count', 'race')], by = c('id', 'race')) %>%
  group_by(id, race, Tzioumis, Rosenman) %>%
  summarise(tally = sum(count))

#################################################
##           make the visualization            ##
#################################################

# order the factor
grid$race <- factor(grid$race, levels = c('White', 'Black', 'Hispanic', 'Asian', 'Other'))

# get the weighted correlations
wtd.cors <- sapply(unique(data$race), FUN = function(r) {
  wtd.cor(x = data$Tzioumis[data$race == r], y = data$Rosenman[data$race == r],
          weight = data$count[data$race == r])[1]
})
labels <- data.frame(Tzioumis = 1, Rosenman = 1, 
                     race = unique(data$race), wtdCor = wtd.cors)
labels$lab <- paste("Wtd. Cor. =", format(round(labels$wtdCor, 2), nsmall = 2))

# make the plot
options(scipen = 1000000)
p <- ggplot(grid, 
       aes(x = Rosenman, y = Tzioumis)) + 
  geom_point(aes(alpha = tally)) +
  scale_alpha_continuous(trans = 'sqrt', limits = c(0, 1e5), breaks = c(25, 1000, 10000, 100000)) + 
  facet_wrap(~race) + theme_bw() + xlim(c(0, 1)) + ylim(c(0, 1)) +
  ggtitle('P(race | first name) Distributions: Voter Files vs. Mortgage Application Data') +
  geom_label(size = 4, data = labels, mapping = aes(x = 0.25*Rosenman, y = 0.95*Tzioumis, label = lab)) + 
  labs(alpha = "# of Appearances\non Voter File") + 
  theme(strip.text = element_text(size = 14)) + 
  theme(plot.title = element_text(size = 16)) + 
  geom_smooth(data, mapping = aes(weight = count), lwd = 1.5) + 
  xlab('Voter File Distribution of P(race | first name)') + 
  ylab('Mortgage Application Data Distribution of P(race | first name)') + 
  geom_abline(intercept = 0, slope = 1, color = 'deeppink', lty = 2, lwd = 1.5) +
  theme(aspect.ratio=1) 

# move the legend and save it
p2 <- shift_legend(p)
if(saveImage) {
  ggsave('tzioumis_raceGivenName.png', plot = p2, width = 10, height = 7, units = 'in')
  ggsave('tzioumis_raceGivenName.pdf', plot = p2, width = 10, height = 7, units = 'in')
  
} else {
  plot(p2)
}
