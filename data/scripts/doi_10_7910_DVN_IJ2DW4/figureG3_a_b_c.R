rm(list = ls())

# set working directory to YOUR PATH TO REPLICATION MATERIALS
setwd('~/PATH TO REPLICATION MATERIALS/gz_replication_materials/')

# *NOTE*: install rdrobust version 2.0.3 as indicated below
# packageurl <- "https://cran.r-project.org/src/contrib/Archive/rdrobust/rdrobust_2.0.3.tar.gz"
# install.packages(packageurl, repos=NULL, type="source")

## load required packages
library(data.table)
library(rdrobust)
library(rddensity)
library(dplyr)
library(tidyr)
library(ggplot2)


## load data
load('./data/crime/hate_crime.RDS')
hate_crimes_unique <- unique(hate_crimes_w_at_1m, by=c('PCON17CD', 'election'))

### Figure G3a: Continuity in the density of candidates around the cutoff (p = 1)
outden <- rddensity(hate_crimes_unique$victory_margin, p=1)

pden <- rdplotdensity(outden, hate_crimes_unique$victory_margin,
                      hist = FALSE,
                      lcol = '#0000FF', CIcol = 1, xlabel = "Margin of victory",
                      ylabel = 'Density of candidates', title = 'Local linear approximation')
pden$Estplot + 
  geom_vline(aes(xintercept=0),
             linetype='solid', colour='#666666') +
  annotate(geom="text", x=-25, y=0.0025, label=paste(paste("Density continuity test", "p-value = ", sep='\n'),round(outden$test$p_jk, 2), sep='\n')) +
  scale_x_continuous(breaks=seq(-40,30,10)) +
  theme(plot.title = element_text(size=14, lineheight=3),
        panel.grid.major = element_blank(), panel.grid.minor = element_blank(), panel.border = element_blank(),
        panel.background = element_blank(), axis.line = element_line(colour = "black"),
        axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1, size = 12),
        axis.title.x = element_text(size =12), axis.title.y = element_text(size =12),
        axis.text.y = element_text(size = 12))
ggsave('./output/figures/figureG3a.pdf', width=6, height=4.85)


### Figure G3b: Continuity in the density of candidates around the cutoff (p = 2)
outden <- rddensity(hate_crimes_unique$victory_margin, p=2)

pden <- rdplotdensity(outden, hate_crimes_unique$victory_margin,
                      hist = FALSE,
                      lcol = '#0000FF', CIcol = 1, xlabel = "Margin of victory",
                      ylabel = 'Density of candidates', title = 'Local quadratic approximation')
pden$Estplot + 
  geom_vline(aes(xintercept=0),
             linetype='solid', colour='#666666') +
  annotate(geom="text", x=-20, y=0.0005, label=paste(paste("Density continuity test", "p-value = ", sep='\n'),round(outden$test$p_jk, 2), sep='\n')) +
  scale_x_continuous(breaks=seq(-30,40,10)) +
  theme(plot.title = element_text(size=14, lineheight=3),
        panel.grid.major = element_blank(), panel.grid.minor = element_blank(), panel.border = element_blank(),
        panel.background = element_blank(), axis.line = element_line(colour = "black"),
        axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1, size = 12),
        axis.title.x = element_text(size =12), axis.title.y = element_text(size =12),
        axis.text.y = element_text(size = 12))
ggsave('./output/figures/figureG3b.pdf', width=6, height=4.85)


### Figure G3c: Continuity in the density of candidates around the cutoff (p = 3)
outden <- rddensity(hate_crimes_unique$victory_margin, p=3)

pden <- rdplotdensity(outden, hate_crimes_unique$victory_margin,
                      hist = FALSE,
                      lcol = '#0000FF', CIcol = 1, xlabel = "Margin of victory",
                      ylabel = 'Density of candidates', title = 'Local cubic approximation')
pden$Estplot + 
  geom_vline(aes(xintercept=0),
             linetype='solid', colour='#666666') +
  annotate(geom="text", x=-50, y=0.000, label=paste(paste("Density continuity test", "p-value = ", sep='\n'),round(outden$test$p_jk, 2), sep='\n')) +
  scale_x_continuous(breaks=seq(-80,80,20)) +
  theme(plot.title = element_text(size=14, lineheight=3),
        panel.grid.major = element_blank(), panel.grid.minor = element_blank(), panel.border = element_blank(),
        panel.background = element_blank(), axis.line = element_line(colour = "black"),
        axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1, size = 12),
        axis.title.x = element_text(size =12), axis.title.y = element_text(size =12),
        axis.text.y = element_text(size = 12))
ggsave('./output/figures/figureG3c.pdf', width=6, height=4.85)
