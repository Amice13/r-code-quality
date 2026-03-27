rm(list = ls())

# set working directory to YOUR PATH TO REPLICATION MATERIALS
setwd('~/PATH TO REPLICATION MATERIALS/gz_replication_materials/')

# load packages
library(data.table)
library(ggplot2)
library(stringr)

## load data
mentions_region <- readRDS('./data/media/mentions_candidate_subregion.RDS')
sentiment_summary <- readRDS('./data/media/mentions_candidate_sentiment.RDS')

mentions_region[, sub_region:=str_to_title(sub_region)]
### Figure D2a. Distribution of mentions by a candidate's subregion of origin
ggplot(mentions_region, aes(x=reorder(sub_region, -mentions), y=mentions)) +
  geom_bar(stat="identity") +
  geom_text(aes(label = mentions), vjust=-0.5, size=3.5) +
  geom_text(aes(label = candidates), vjust=1.2, colour='#FF7F50') +
  xlab(NULL) +
  ylab('Number of mentions') +
  theme(legend.position="bottom", legend.title = element_blank(),
        legend.text = element_text(size=12),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        panel.background = element_blank(), axis.line = element_line(colour = "black"),
        axis.title.x = element_text(size =14), axis.title.y = element_text(size =14),
        axis.text.x = element_text(size=12, angle = 90, vjust = 0.5, hjust=1),
        axis.text.y = element_text(size = 12),
        plot.title = element_text(size=18, lineheight=3))
ggsave('./output/figures/figureD2a.pdf', width=6, height=4.85)

### Figure D2b. Distribution of mentions by candidate and sentiment 
ggplot(sentiment_summary[mentions<100], aes(x = mentions, color = category)) +
  geom_histogram(fill="white", position="dodge", bins=20) +
  geom_vline(data = sentiment_summary[,.(category_median = median(mentions)), by=.(category)], aes(xintercept=category_median, color=category),
             linetype="dashed") +
  xlab('Number of mentions') +
  ylab('Number of candidates') +
  theme(legend.position="bottom", legend.title = element_blank(),
        legend.text = element_text(size=12),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        panel.background = element_blank(), axis.line = element_line(colour = "black"),
        axis.title.x = element_text(size =14), axis.title.y = element_text(size =14),
        axis.text.x = element_text(size=12, angle = 90, vjust = 0.5, hjust=1),
        axis.text.y = element_text(size = 12),
        plot.title = element_text(size=18, lineheight=3))
ggsave('./output/figures/figureD2b.pdf', width=6, height=4.85)
