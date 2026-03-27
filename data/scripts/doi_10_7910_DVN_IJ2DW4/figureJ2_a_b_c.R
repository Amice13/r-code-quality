rm(list = ls())

# set working directory to YOUR PATH TO REPLICATION MATERIALS
setwd('~/PATH TO REPLICATION MATERIALS/gz_replication_materials/')

# *NOTE*: install rdrobust version 2.0.3 as indicated below
# packageurl <- "https://cran.r-project.org/src/contrib/Archive/rdrobust/rdrobust_2.0.3.tar.gz"
# install.packages(packageurl, repos=NULL, type="source")

## load required packages
library(data.table)
library(tidyr)
library(dplyr)
library(rdrobust)
library(rddensity)
library(ggplot2)

## load data
sentiment <- readRDS('./data/media/media.RDS')

### Figure J.2 Density of the running variable
# figure a
outden <- rddensity(sentiment[(region_name!='Scotland' & as.numeric(as.character(period))==1), victory_margin], p=1)

pden <- rdplotdensity(outden, sentiment[(region_name!='Scotland' & as.numeric(as.character(period))==1), victory_margin],
                      hist = FALSE,
                      lcol = '#0000FF', CIcol = 1, xlabel = "Margin of victory",
                      ylabel = 'Density of candidates', title = 'Local linear approximation')
pden$Estplot + 
  geom_vline(aes(xintercept=0),
             linetype='solid', colour='#666666') +
  annotate(geom="text", x=-20, y=0.0025, label=paste(paste("Density continuity test", "p-value = ", sep='\n'),round(outden$test$p_jk, 2), sep='\n')) +
  scale_x_continuous(breaks=seq(-30,30,10)) +
  theme(plot.title = element_text(size=14, lineheight=3),
        panel.grid.major = element_blank(), panel.grid.minor = element_blank(), panel.border = element_blank(),
        panel.background = element_blank(), axis.line = element_line(colour = "black"),
        axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1, size = 12),
        axis.title.x = element_text(size =12), axis.title.y = element_text(size =12),
        axis.text.y = element_text(size = 12))
ggsave('./output/figures/figureJ2a.pdf', width=6, height=4.85)

# figure b
outden <- rddensity(sentiment[(region_name!='Scotland' & as.numeric(as.character(period))==1), victory_margin], p=2)

pden <- rdplotdensity(outden, sentiment[(region_name!='Scotland' & as.numeric(as.character(period))==1), victory_margin],
                      hist = FALSE,
                      lcol = '#0000FF', CIcol = 1, xlabel = "Margin of victory",
                      ylabel = 'Density of candidates', title = 'Local quadratic approximation')
pden$Estplot + 
  geom_vline(aes(xintercept=0),
             linetype='solid', colour='#666666') +
  annotate(geom="text", x=-30, y=0.0025, label=paste(paste("Density continuity test", "p-value = ", sep='\n'),round(outden$test$p_jk, 2), sep='\n')) +
  scale_x_continuous(breaks=seq(-50,60,10)) +
  theme(plot.title = element_text(size=14, lineheight=3),
        panel.grid.major = element_blank(), panel.grid.minor = element_blank(), panel.border = element_blank(),
        panel.background = element_blank(), axis.line = element_line(colour = "black"),
        axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1, size = 12),
        axis.title.x = element_text(size =12), axis.title.y = element_text(size =12),
        axis.text.y = element_text(size = 12))
ggsave('./output/figures/figureJ2b.pdf', width=6, height=4.85)


# figure c
outden <- rddensity(sentiment[(region_name!='Scotland' & as.numeric(as.character(period))==1), victory_margin], p=3)

pden <- rdplotdensity(outden, sentiment[(region_name!='Scotland' & as.numeric(as.character(period))==1), victory_margin],
                      hist = FALSE,
                      lcol = '#0000FF', CIcol = 1, xlabel = "Margin of victory",
                      ylabel = 'Density of candidates', title = 'Local cubic approximation')
pden$Estplot + 
  geom_vline(aes(xintercept=0),
             linetype='solid', colour='#666666') +
  annotate(geom="text", x=-45, y=-0.005, label=paste(paste("Density continuity test", "p-value = ", sep='\n'),round(outden$test$p_jk, 2), sep='\n')) +
  scale_x_continuous(breaks=seq(-80,60,10)) +
  theme(plot.title = element_text(size=14, lineheight=3),
        panel.grid.major = element_blank(), panel.grid.minor = element_blank(), panel.border = element_blank(),
        panel.background = element_blank(), axis.line = element_line(colour = "black"),
        axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1, size = 12),
        axis.title.x = element_text(size =12), axis.title.y = element_text(size =12),
        axis.text.y = element_text(size = 12))
ggsave('./output/figures/figureJ2c.pdf', width=6, height=4.85)
