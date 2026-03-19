rm(list = ls())

# set working directory to YOUR PATH TO REPLICATION MATERIALS
setwd('~/PATH TO REPLICATION MATERIALS/gz_replication_materials/')

# *NOTE*: install rdrobust version 2.0.3 as indicated below
# packageurl <- "https://cran.r-project.org/src/contrib/Archive/rdrobust/rdrobust_2.0.3.tar.gz"
# install.packages(packageurl, repos=NULL, type="source")


## load required packages
library(data.table)
library(rdrobust)
library(dplyr)
library(tidyr)
library(ggplot2)


## load data
load('./data/crime/hate_crime.RDS')
covars_crime <- readRDS('./data/crime/covars_crime.RDS')

### Figure G4a:Sensitivity to bandwidth size; MSE bandwidth
mse <- rdrobust(hate_crimes_w_at_3m[, crime_rate],
                hate_crimes_w_at_3m[, victory_margin],
                covs = hate_crimes_w_at_3m[, covars_crime, with=F],
                kernel = "triangular", p = 1, bwselect = "mserd",
                nnmatch = 4,
                cluster = hate_crimes_w_at_3m[, cluster],
                c=0)
data_mse <- cbind('MSE' ,mse$bws[1], mse$coef[1], mse$ci[3,1], mse$ci[3,2])

mse1.25 <- rdrobust(hate_crimes_w_at_3m[, crime_rate],
                    hate_crimes_w_at_3m[, victory_margin],
                    covs = hate_crimes_w_at_3m[, covars_crime, with=F],
                    kernel = "triangular", p = 1, h=1.25*mse$bws[1],
                    nnmatch = 4,
                    cluster = hate_crimes_w_at_3m[, cluster],
                    c=0)
data_mse1.25 <- cbind('1.25*MSE', mse1.25$bws[1], mse1.25$coef[1], mse1.25$ci[3,1], mse1.25$ci[3,2])

mse1.5 <- rdrobust(hate_crimes_w_at_3m[, crime_rate],
                   hate_crimes_w_at_3m[, victory_margin],
                   covs = hate_crimes_w_at_3m[, covars_crime, with=F],
                   kernel = "triangular", p = 1, h=1.5*mse$bws[1],
                   nnmatch = 4,
                   cluster = hate_crimes_w_at_3m[, cluster],
                   c=0)
data_mse1.5 <- cbind('1.5*MSE', mse1.5$bws[1], mse1.5$coef[1], mse1.5$ci[3,1], mse1.5$ci[3,2])

mse.75 <- rdrobust(hate_crimes_w_at_3m[, crime_rate],
                   hate_crimes_w_at_3m[, victory_margin],
                   covs = hate_crimes_w_at_3m[, covars_crime, with=F],
                   kernel = "triangular", p = 1, h=.75*mse$bws[1],
                   nnmatch = 4,
                   cluster = hate_crimes_w_at_3m[, cluster],
                   c=0)
data_mse.75 <- cbind('0.75*MSE', mse.75$bws[1], mse.75$coef[1], mse.75$ci[3,1], mse.75$ci[3,2])

mse.5 <- rdrobust(hate_crimes_w_at_3m[, crime_rate],
                  hate_crimes_w_at_3m[, victory_margin],
                  covs = hate_crimes_w_at_3m[, covars_crime ,with=F],
                  kernel = "triangular", p = 1, h=0.5*mse$bws[1],
                  nnmatch = 4,
                  cluster = hate_crimes_w_at_3m[,c(cluster)],
                  c=0)
data_mse.5 <- cbind('0.5*MSE', mse.5$bws[1], mse.5$coef[1], mse.5$ci[3,1], mse.5$ci[3,2])

data_mse <- as.data.frame(rbind(data_mse.5, data_mse.75, data_mse, data_mse1.25, data_mse1.5))
colnames(data_mse) <- c('type', 'bandwidth', 'coef', 'ci_l', 'ci_u')

# plot
ggplot(data_mse) +
  aes(x=as.numeric(as.character(bandwidth)), y=as.numeric(as.character(coef))) +
  geom_point() +
  geom_errorbar(aes(ymin=as.numeric(as.character(ci_l)), ymax=as.numeric(as.character(ci_u))), alpha=0.4) +
  scale_x_continuous(breaks=c(mse.5$bws[1], mse.75$bws[1], mse$bws[1], mse1.25$bws[1], mse1.5$bws[1]),
                     labels = c(paste('0.5*MSE',round(mse.5$bws[1],1),sep='\n'),
                                paste('0.75*MSE',round(mse.75$bws[1],1),sep='\n'),
                                paste('MSE',round(mse$bws[1],1),sep='\n'),
                                paste('1.25*MSE',round(mse1.25$bws[1],1),sep='\n'),
                                paste('1.5*MSE',round(mse1.5$bws[1],1),sep='\n'))) +
  geom_hline(aes(yintercept=0),
             linetype='dashed', colour='#666666') +
  ggtitle("MSE Bandwidth") +
  theme(plot.title = element_text(size=14, lineheight=3),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        panel.background = element_blank(), axis.line = element_line(colour = "black"),
        axis.title.x = element_text(size =14), axis.title.y = element_text(size =14),
        axis.text.x = element_text(size=10, angle = 90), axis.text.y = element_text(size = 10)) +
  xlab(NULL) +
  ylab(paste('RD effect on hate crimes per 1000 residents', '3 months after election', sep = '\n'))
ggsave('./output/figures/figureG4a.pdf', width=6, height=4.85)


### Figure G4b:Sensitivity to bandwidth size; CER bandwidth
cer <- rdrobust(hate_crimes_w_at_3m[, crime_rate],
                hate_crimes_w_at_3m[, victory_margin],
                covs = hate_crimes_w_at_3m[, covars_crime, with=F],
                kernel = "triangular", p = 1, bwselect = 'cerrd',
                nnmatch = 4,
                cluster = hate_crimes_w_at_3m[, cluster],
                c=0)
data_cer <- cbind('CER', cer$bws[1], cer$coef[1], cer$ci[3,1], cer$ci[3,2])

cer1.25 <- rdrobust(hate_crimes_w_at_3m[, crime_rate],
                    hate_crimes_w_at_3m[, victory_margin],
                    covs = hate_crimes_w_at_3m[,covars_crime,with=F],
                    kernel = "triangular", p = 1, h=1.25*cer$bws[1],
                    nnmatch = 4,
                    cluster = hate_crimes_w_at_3m[, cluster],
                    c=0) 

data_cer1.25 <- cbind('1.25*CER', cer1.25$bws[1], cer1.25$coef[1], cer1.25$ci[3,1], cer1.25$ci[3,2])

cer1.5 <- rdrobust(hate_crimes_w_at_3m[, crime_rate],
                   hate_crimes_w_at_3m[, victory_margin],
                   covs = hate_crimes_w_at_3m[,covars_crime,with=F],
                   kernel = "triangular", p = 1, h=1.5*cer$bws[1],
                   nnmatch = 4,
                   cluster = hate_crimes_w_at_3m[, cluster],
                   c=0) 

data_cer1.5 <- cbind('1.5*CER', cer1.5$bws[1], cer1.5$coef[1], cer1.5$ci[3,1], cer1.5$ci[3,2])

cer.75 <- rdrobust(hate_crimes_w_at_3m[, crime_rate],
                   hate_crimes_w_at_3m[, victory_margin],
                   covs = hate_crimes_w_at_3m[,covars_crime,with=F],
                   kernel = "triangular", p = 1, h=.75*cer$bws[1],
                   nnmatch = 4,
                   cluster = hate_crimes_w_at_3m[, cluster],
                   c=0) 

data_cer.75 <- cbind('0.75*CER', cer.75$bws[1], cer.75$coef[1], cer.75$ci[3,1], cer.75$ci[3,2])

cer.5 <- rdrobust(hate_crimes_w_at_3m[, crime_rate],
                  hate_crimes_w_at_3m[, victory_margin],
                  covs = hate_crimes_w_at_3m[, covars_crime ,with=F],
                  kernel = "triangular", p = 1, h=0.5*cer$bws[1],
                  nnmatch = 4,
                  cluster = hate_crimes_w_at_3m[, cluster],
                  c=0) 

data_cer.5 <- cbind('0.5*CER', cer.5$bws[1], cer.5$coef[1], cer.5$ci[3,1], cer.5$ci[3,2])
data_cer <- as.data.frame(rbind(data_cer.5, data_cer.75, data_cer, data_cer1.25, data_cer1.5))
colnames(data_cer) <- c('type', 'bandwidth', 'coef', 'ci_l', 'ci_u')

# plot
ggplot(data_cer) +
  aes(x=as.numeric(as.character(bandwidth)), y=as.numeric(as.character(coef))) +
  geom_point() +
  geom_errorbar(aes(ymin=as.numeric(as.character(ci_l)), ymax=as.numeric(as.character(ci_u))), alpha=0.4) +
  scale_x_continuous(breaks=c(cer.5$bws[1], cer.75$bws[1], cer$bws[1], cer1.25$bws[1], cer1.5$bws[1]),
                     labels = c(paste('0.5*CER',round(cer.5$bws[1],1),sep='\n'),
                                paste('0.75*CER',round(cer.75$bws[1],1),sep='\n'),
                                paste('CER',round(cer$bws[1],1),sep='\n'),
                                paste('1.25*CER',round(cer1.25$bws[1],1),sep='\n'),
                                paste('1.5*CER',round(cer1.5$bws[1],1),sep='\n'))) +
  geom_hline(aes(yintercept=0),
             linetype='dashed', colour='#666666') +
  ggtitle("CER Bandwidth") +
  theme(plot.title = element_text(size=14, lineheight=3),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        panel.background = element_blank(), axis.line = element_line(colour = "black"),
        axis.title.x = element_text(size =14), axis.title.y = element_text(size =14),
        axis.text.x = element_text(size=10, angle = 90), axis.text.y = element_text(size = 10)) +
  xlab(NULL) +
  ylab(paste('RD effect on hate crimes per 1000 residents', '3 months after election', sep = '\n'))
ggsave('./output/figures/figureG4b.pdf', width=6, height=4.85)
