rm(list = ls())

# set working directory to YOUR PATH TO REPLICATION MATERIALS
setwd('~/PATH TO REPLICATION MATERIALS/gz_replication_materials/')

## load required packages
library(data.table)
library(ggplot2)

## load data
lad_pop <- readRDS('./data/crime/lad_to_constituency_validation.RDS')

### Figure B1 Validation of hate crime assignment from LAD to constituency
## Figure B1a
cor_eth <- cor(lad_pop$ethnicity_minority, lad_pop$inferred_ethnicity)
ggplot(lad_pop, aes(ethnicity_minority, inferred_ethnicity)) +
  geom_point(color = '#666666', size=0.8) + geom_smooth(method = lm, color = '#333333') +
  xlab('Observed proportion non-white population') + ylab('Inferred proportion non-white population') +
  annotate("text", label = paste("Pearson correlation =",round(cor_eth,2),sep='\n'), x = 60, y = 15, size = 4, colour = '#666666') +
  theme(panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        panel.background = element_blank(), axis.line = element_line(colour = "black"
        ),axis.title.y = element_text(size = rel(1.2)), axis.title.x = element_text(size = rel(1.2)),
        axis.text.x=element_text(size=12),axis.text.y=element_text(size=12))
ggsave('./output/figures/figureB1a.pdf', width=6, height=4.85)

## Figure B1b
cor_rel <- cor(lad_pop$religion_minority, lad_pop$inferred_religion)
ggplot(lad_pop, aes((religion_minority)*100, (inferred_religion)*100)) +
  geom_point(color = '#666666', size=0.8) + geom_smooth(method = lm, color = '#333333') +
  xlab('Observed proportion non-dominant religion') + ylab('Inferred proportion non-dominant religion') +
  annotate("text", label = paste("Pearson correlation =",round(cor_rel,2),sep='\n'), x = 40, y = 10, size = 4, colour = '#666666') +
  theme(panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        panel.background = element_blank(), axis.line = element_line(colour = "black"
        ),axis.title.y = element_text(size = rel(1.2)), axis.title.x = element_text(size = rel(1.2)),
        axis.text.x=element_text(size=12),axis.text.y=element_text(size=12))
ggsave('./output/figures/figureB1b.pdf', width=6, height=4.85)
