rm(list = ls())

# set working directory to YOUR PATH TO REPLICATION MATERIALS
setwd('~/PATH TO REPLICATION MATERIALS/gz_replication_materials/')

# *NOTE*: install rdrobust version 2.0.3 as indicated below
# packageurl <- "https://cran.r-project.org/src/contrib/Archive/rdrobust/rdrobust_2.0.3.tar.gz"
# install.packages(packageurl, repos=NULL, type="source")

## load required packages
library(data.table)
library(tidyr)
library(rdrobust)
library(rddensity)
library(ggplot2)

## load data
load(file='./data/attitudes/attitudes.rds')
bes_rdd <- bes_rdd[grepl('^(E|W)', ons_id) & white==1]
bes_rdd_unique <- unique(bes_rdd[, victory_margin, by=.(ons_id, election)])

## Figure I2a
outden <- rddensity(bes_rdd_unique[,victory_margin], p=1)

pden <- rdplotdensity(outden, bes_rdd_unique[,victory_margin],
                      hist = FALSE,
                      lcol = '#0000FF', CIcol = 1, xlabel = "Margin of victory",
                      ylabel = 'Density of respondents', title = 'Local linear approximation')

pden$Estplot + 
  geom_vline(aes(xintercept=0),
             linetype='solid', colour='#666666') +
  annotate(geom="text", x=-32, y=0.0005, label=paste(paste("Density continuity test", "p-value = ", sep='\n'),round(outden$test$p_jk, 2), sep='\n')) +
  scale_x_continuous(breaks=seq(-80,80,20)) +
  theme(plot.title = element_text(size=14, lineheight=3),
        panel.grid.major = element_blank(), panel.grid.minor = element_blank(), panel.border = element_blank(),
        panel.background = element_blank(), axis.line = element_line(colour = "black"),
        axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1, size = 12),
        axis.title.x = element_text(size =12), axis.title.y = element_text(size =12),
        axis.text.y = element_text(size = 12))
ggsave('./output/figures/figureI2a.pdf', width=6, height=4.85)

## Figure I2b
outden <- rddensity(bes_rdd_unique[,victory_margin], p=2)

pden <- rdplotdensity(outden, bes_rdd_unique[,victory_margin],
                      hist = FALSE,
                      lcol = '#0000FF', CIcol = 1, xlabel = "Margin of victory",
                      ylabel = 'Density of respondents', title = 'Local quadratic approximation')
pden$Estplot + 
  geom_vline(aes(xintercept=0),
             linetype='solid', colour='#666666') +
  annotate(geom="text", x=-32, y=0.0005, label=paste(paste("Density continuity test", "p-value = ", sep='\n'),round(outden$test$p_jk, 2), sep='\n')) +
  scale_x_continuous(breaks=seq(-80,80,20)) +
  theme(plot.title = element_text(size=14, lineheight=3),
        panel.grid.major = element_blank(), panel.grid.minor = element_blank(), panel.border = element_blank(),
        panel.background = element_blank(), axis.line = element_line(colour = "black"),
        axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1, size = 12),
        axis.title.x = element_text(size =12), axis.title.y = element_text(size =12),
        axis.text.y = element_text(size = 12))
ggsave('./output/figures/figureI2b.pdf', width=6, height=4.85)


## Figure I2c
outden <- rddensity(bes_rdd_unique[,victory_margin], p=3)

pden <- rdplotdensity(outden, bes_rdd_unique[,victory_margin],
                      hist = FALSE,
                      lcol = '#0000FF', CIcol = 1, xlabel = "Margin of victory",
                      ylabel = 'Density of respondents', title = 'Local cubic approximation')
pden$Estplot + 
  geom_vline(aes(xintercept=0),
             linetype='solid', colour='#666666') +
  annotate(geom="text", x=-32, y=-0.005, label=paste(paste("Density continuity test", "p-value = ", sep='\n'),round(outden$test$p_jk, 2), sep='\n')) +
  scale_x_continuous(breaks=seq(-80,80,20)) +
  theme(plot.title = element_text(size=14, lineheight=3),
        panel.grid.major = element_blank(), panel.grid.minor = element_blank(), panel.border = element_blank(),
        panel.background = element_blank(), axis.line = element_line(colour = "black"),
        axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1, size = 12),
        axis.title.x = element_text(size =12), axis.title.y = element_text(size =12),
        axis.text.y = element_text(size = 12))
ggsave('./output/figures/figureI2c.pdf', width=6, height=4.85)

