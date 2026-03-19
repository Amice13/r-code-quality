rm(list = ls())

# set working directory to YOUR PATH TO REPLICATION MATERIALS
setwd('~/PATH TO REPLICATION MATERIALS/gz_replication_materials/')

# *NOTE*: install rdrobust version 2.0.3 as indicated below
# packageurl <- "https://cran.r-project.org/src/contrib/Archive/rdrobust/rdrobust_2.0.3.tar.gz"
# install.packages(packageurl, repos=NULL, type="source")

## load required packages
library(data.table)
library(rdrobust)
library(ggplot2)

## load data
load('./data/crime/hate_crime.RDS')
religion <- readRDS('./data/candidates/candidates_assigned_religion.RDS')
covars_crime <- readRDS('./data/crime/covars_crime.RDS')

### Figure G11a: Subgroup effects on hate crime by candidate's religion
data <- merge(religion,
              hate_crimes_w_at_3m,
              by=c('PCON17CD', 'election'),
              all.x = TRUE)

out_muslim <- rdrobust(data[religion_assigned=='muslim', crime_rate],
                       data[religion_assigned=='muslim', victory_margin],
                       kernel = "triangular", p = 1,
                       nnmatch = 4,
                       cluster = data[religion_assigned=='muslim', cluster]) 

data_muslim <- cbind.data.frame(out_muslim$coef[1], out_muslim$ci[3,1], out_muslim$ci[3,2], out_muslim$se[1], paste('Muslim', 'background', sep='\n'))
colnames(data_muslim) <- c('coef', 'ci_l', 'ci_u', 'se', 'religious_background')

out_not_muslim <- rdrobust(data[religion_assigned!='muslim', crime_rate],
                           data[religion_assigned!='muslim', victory_margin],
                           kernel = "triangular", p = 1,
                           nnmatch = 4,
                           cluster = data[religion_assigned!='muslim', cluster]) 

data_not_muslim <- cbind.data.frame(out_not_muslim$coef[1], out_not_muslim$ci[3,1], out_not_muslim$ci[3,2], out_not_muslim$se[1],paste('Not muslim', 'background', sep='\n'))
colnames(data_not_muslim) <- c('coef', 'ci_l', 'ci_u', 'se', 'religious_background')

data_religion <- rbind(data_muslim, data_not_muslim)

ggplot(data_religion,
       aes(x=religious_background, y=coef)) + geom_point() +
  geom_errorbar(aes(ymin=ci_l, ymax=ci_u), width=.1) +
  geom_hline(yintercept=0, linetype='longdash', size=0.2) +
  xlab(NULL) +
  ylab(paste('Minority victory effects on',
             "hate crimes per 1000 residents",
             '3 months after general election',
             sep='\n')) +
  scale_color_grey(end = 0.6) +
  theme(legend.position="bottom", legend.title = element_blank(),
        legend.text = element_text(size=12),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        panel.background = element_blank(), axis.line = element_line(colour = "black"),
        axis.title.x = element_text(size =14), axis.title.y = element_text(size =14),
        axis.text.x = element_text(size=12), axis.text.y = element_text(size = 12),
        plot.title = element_text(size=18, lineheight=3))
ggsave(file = './output/figures/figureG11a.pdf', width=6, height=4.85)




### Figure G11b: Subgroup effects on hate crime by candidate's gender
data <- hate_crimes_w_at_3m
out_female <- rdrobust(data[female==1, crime_rate],
                       data[female==1, victory_margin],
                       covs = data[female==1 , covars_crime, with=FALSE],
                       kernel = "triangular", p = 1,
                       nnmatch = 4,
                       cluster = data[female==1, cluster]) 

data_female <- cbind.data.frame(out_female$coef[1], out_female$ci[3,1], out_female$ci[3,2], out_female$se[1], paste('Female', 'candidate', sep='\n'))
colnames(data_female) <- c('coef', 'ci_l', 'ci_u', 'se', 'candidate_gender')

out_not_female <- rdrobust(data[female==0, crime_rate],
                           data[female==0, victory_margin],
                           covs = data[female==0 , covars_crime, with=FALSE],
                           kernel = "triangular", p = 1,
                           nnmatch = 4,
                           cluster = data[female==0, cluster]) 

data_not_female <- cbind.data.frame(out_not_female$coef[1], out_not_female$ci[3,1], out_not_female$ci[3,2], out_not_female$se[1],paste('Male', 'candidate', sep='\n'))
colnames(data_not_female) <- c('coef', 'ci_l', 'ci_u', 'se', 'candidate_gender')

data_gender <- rbind(data_female, data_not_female)

ggplot(data_gender,
       aes(x=candidate_gender, y=coef)) + geom_point() +
  geom_errorbar(aes(ymin=ci_l, ymax=ci_u), width=.1) +
  geom_hline(yintercept=0, linetype='longdash', size=0.2) +
  xlab(NULL) +
  ylab(paste('Minority victory effects on',
             "hate crimes per 1000 residents",
             '3 months after general election',
             sep='\n')) +
  scale_color_grey(end = 0.6) +
  theme(legend.position="bottom", legend.title = element_blank(),
        legend.text = element_text(size=12),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        panel.background = element_blank(), axis.line = element_line(colour = "black"),
        axis.title.x = element_text(size =14), axis.title.y = element_text(size =14),
        axis.text.x = element_text(size=12), axis.text.y = element_text(size = 12),
        plot.title = element_text(size=18, lineheight=3))
ggsave(file = './output/figures/figureG11b.pdf', width=6, height=4.85)
