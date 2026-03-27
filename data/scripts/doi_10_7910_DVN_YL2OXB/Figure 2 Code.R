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
library(ggh4x)

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
last <- read_csv("censusSurnames.csv")
lastNames.census <- data.frame(surname = toupper(last$surname))
lastNames.census$p_whi <- last$whi.last
lastNames.census$p_bla <- last$bla.last
lastNames.census$p_his <- last$his.last
lastNames.census$p_asi <- last$asi.last
lastNames.census$p_oth <- last$oth.last

# read in our data and get the counts (divide by 7 b/c there are 7 voter files)
load("last_raceNameProbs.rData")
lastNames.ros <- last_raceNameProbs
rawCounts <- rowSums(lastNames.ros[,-1] * 
                       matrix(1/apply(lastNames.ros[,-1], 2, FUN = function(x) {min(x[x > 0])}), 
                              ncol = 5, nrow = nrow(lastNames.ros), byrow = TRUE))
lastNames.ros$last_name <- toupper(lastNames.ros$name)
lastNames.ros$count <- rawCounts/7

# join the datasets
mergedData <- merge(lastNames.census, lastNames.ros[,-1], by.x = 'surname', by.y = 'last_name')

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
raceMaxima <- sapply(raceGroups, FUN = function(r) {
  max(data[data$race == r, c("Census", "Rosenman")])})

# coarsen the data
data$id <- NA
for(r in raceGroups) {
  
  # to generate square grids, use identical breaks for each variable
  breaks <- seq(from = 0, to = raceMaxima[raceGroups == r], length = gridPts + 1)
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

# add pseudo rows to ensure square grid
for(r in raceGroups) {
  
  # get the maximum entry for each dataset
  censusMax <- max(grid[grid$race == r,]$Census)
  rosenmanMax <- max(grid[grid$race == r,]$Rosenman)
    
  # create a pseudo entry to scale plots appropriately
  if(censusMax != rosenmanMax) {
      
    # add the row to the dataframe
    grid[nrow(grid) + 1,] <- list(id = 'pseudoEntry',
                                  race = r,
                                  Census = max(censusMax, rosenmanMax),
                                  Rosenman = max(censusMax, rosenmanMax),
                                  tally = 0)
  }
}

#################################################
##           make the visualization            ##
#################################################

# order the factor
grid$race <- factor(grid$race, levels = c('White', 'Black', 'Hispanic', 'Asian', 'Other'))

# get the weighted correlations
cors <- sapply(sort(unique(data$race)), FUN = function(r) {
  cor(x = data$rawCensus[data$race == r], y = data$rawRosenman[data$race == r])[1]
})
wtd.cors <- sapply(sort(unique(data$race)), FUN = function(r) {
  wtd.cor(x = data$rawCensus[data$race == r], y = data$rawRosenman[data$race == r],
          weight = data$count[data$race == r])[1]
})
labels <- data.frame(Census = tapply(grid$Census, grid$race, max), 
                     Rosenman = tapply(grid$Rosenman, grid$race, max), 
                     race = sort(unique(data$race)), wtdCor = wtd.cors)
labels$lab <- paste("Wtd. Cor. =", format(round(labels$wtdCor, 2), nsmall = 2))

# make the plot
options(scipen = 10000000)
p <- ggplot(grid, 
       aes(x = Rosenman, y = Census)) + 
  geom_point(aes(alpha = tally)) +
  scale_alpha_continuous(trans = 'sqrt', limits = c(0, 1e5), breaks = c(25, 1000, 10000, 100000)) + 
#  scale_alpha_continuous(trans = 'pseudo_log', breaks = 10^c(1,3,5,7)) + 
  facet_wrap(~race, scales = 'free') + theme_bw() +
  ggtitle('P(surname | race) Distributions: Voter Files vs. Census') +
  labs(alpha = "# of Appearances\non Voter File") + 
  theme(strip.text = element_text(size = 14)) + 
  theme(plot.title = element_text(size = 16)) + 
  xlab('Voter File Distribution of P(surname | race)') + 
  ylab('Census Distribution of P(surname | race)') + 
  geom_abline(intercept = 0, slope = 1, color = 'deeppink', lty = 2, lwd = 1.5) +
  theme(aspect.ratio=1) +
  geom_smooth(data, mapping = aes(weight = count), lwd = 1.5) +
  geom_label(size = 4, data = labels, mapping = aes(x = 0.25*Rosenman, y = 0.95*Census, label = lab)) + 
  facetted_pos_scales(
    y = list(
      race == "Hispanic" ~ scale_y_continuous(limits = c(0, labels$Rosenman[rownames(labels) == "Hispanic"])),
      race == "Other" ~ scale_y_continuous(limits = c(0, labels$Rosenman[rownames(labels) == "Other"]))
    ))

# move the legend and save it
p2 <- shift_legend(p)
if(saveImage) {
  ggsave('census_nameGivenRace.png', plot = p2, width = 10, height = 7, units = 'in')
  ggsave('census_nameGivenRace.pdf', plot = p2, width = 10, height = 7, units = 'in')
  
} else {
  plot(p2)
}

