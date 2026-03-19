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
load(file='./data/attitudes/attitudes.rds')
bes_rdd <- bes_rdd[grepl('^(E|W)', ons_id) & white==1]
covars_attitudes <- readRDS('./data/attitudes/covars_attitudes.RDS')

### Figure I4a: Sensitivity to the choice of bandwidth (MSE)
mse <- rdrobust(bes_rdd[, j05],
                bes_rdd[,victory_margin],
                covs = bes_rdd[, covars_attitudes, with=F],
                kernel = "triangular", p = 1, bwselect = "mserd",
                cluster = bes_rdd[,cluster])
data_mse <- cbind('MSE' ,mse$bws[1], mse$coef[1], mse$ci[3,1], mse$ci[3,2])

mse1.25 <- rdrobust(bes_rdd[, j05],
                    bes_rdd[,victory_margin],
                    covs = bes_rdd[, covars_attitudes, with=F],
                    kernel = "triangular", p = 1, bwselect = "mserd", h=1.25*mse$bws[1],
                    cluster = bes_rdd[,cluster])
data_mse1.25 <- cbind('1.25*MSE', mse1.25$bws[1], mse1.25$coef[1], mse1.25$ci[3,1], mse1.25$ci[3,2])


mse1.5 <- rdrobust(bes_rdd[, j05],
                   bes_rdd[,victory_margin],
                   covs = bes_rdd[, covars_attitudes, with=F],
                   kernel = "triangular", p = 1, bwselect = "mserd", h=1.5*mse$bws[1],
                   cluster = bes_rdd[,cluster])

data_mse1.5 <- cbind('1.5*MSE', mse1.5$bws[1], mse1.5$coef[1], mse1.5$ci[3,1], mse1.5$ci[3,2])

mse.75 <- rdrobust(bes_rdd[, j05],
                   bes_rdd[,victory_margin],
                   covs = bes_rdd[, covars_attitudes, with=F],
                   kernel = "triangular", p = 1, bwselect = "mserd", h=.75*mse$bws[1],
                   cluster = bes_rdd[,cluster])

data_mse.75 <- cbind('.75*MSE', mse.75$bws[1], mse.75$coef[1], mse.75$ci[1,1], mse.75$ci[1,2])

mse.5 <- rdrobust(bes_rdd[, j05],
                  bes_rdd[,victory_margin],
                  covs = bes_rdd[, covars_attitudes, with=F],
                  kernel = "triangular", p = 1, bwselect = "mserd", h=0.5*mse$bws[1],
                  cluster = bes_rdd[,cluster])
data_mse.5 <- cbind('0.5*MSE', mse.5$bws[1], mse.5$coef[1], mse.5$ci[1,1], mse.5$ci[1,2])

data_mse <- as.data.frame(rbind(data_mse.5, data_mse.75, data_mse, data_mse1.25, data_mse1.5))
colnames(data_mse) <- c('type', 'bandwidth', 'coef', 'ci_l', 'ci_u')

# graph sensitivity to choice of bandwidth
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
  ylab(paste('RD effect on post-election', 'immigrant inclusionary attitudes', sep = '\n'))
ggsave(file = './output/figures/figureI4a.pdf', width=6, height=4.85)



### Figure I4b: Sensitivity to the choice of bandwidth (CER)
cer <- rdrobust(bes_rdd[, j05],
                bes_rdd[,victory_margin],
                covs = bes_rdd[, covars_attitudes, with=F],
                kernel = "triangular", p = 1, bwselect = "cerrd",
                cluster = bes_rdd[,cluster])
data_cer <- cbind('CER', cer$bws[1], cer$coef[1], cer$ci[3,1], cer$ci[3,2])

cer1.25 <- rdrobust(bes_rdd[, j05],
                    bes_rdd[,victory_margin],
                    covs = bes_rdd[, covars_attitudes, with=F],
                    kernel = "triangular", p = 1, bwselect = "cerrd", h=1.25*cer$bws[1],
                    cluster = bes_rdd[,cluster])

data_cer1.25 <- cbind('1.25*CER', cer1.25$bws[1], cer1.25$coef[1], cer1.25$ci[3,1], cer1.25$ci[3,2])

cer1.5 <- rdrobust(bes_rdd[, j05],
                   bes_rdd[,victory_margin],
                   covs = bes_rdd[, covars_attitudes, with=F],
                   kernel = "triangular", p = 1, bwselect = "cerrd", h=1.5*cer$bws[1],
                   cluster = bes_rdd[,cluster])

data_cer1.5 <- cbind('1.5*CER', cer1.5$bws[1], cer1.5$coef[1], cer1.5$ci[3,1], cer1.5$ci[3,2])

cer.75 <- rdrobust(bes_rdd[, j05],
                   bes_rdd[,victory_margin],
                   covs = bes_rdd[, covars_attitudes, with=F],
                   kernel = "triangular", p = 1, bwselect = "cerrd", h=0.75*cer$bws[1],
                   cluster = bes_rdd[,cluster]) 

data_cer.75 <- cbind('0.75*CER', cer.75$bws[1], cer.75$coef[1], cer.75$ci[1,1], cer.75$ci[1,2])

cer.5 <- rdrobust(bes_rdd[, j05],
                  bes_rdd[,victory_margin],
                  covs = bes_rdd[, covars_attitudes, with=F],
                  kernel = "triangular", p = 1, bwselect = "cerrd", h=0.5*cer$bws[1],
                  cluster = bes_rdd[,cluster]) 

data_cer.5 <- cbind('0.5*CER', cer.5$bws[1], cer.5$coef[1], cer.5$ci[1,1], cer.5$ci[1,2])

data_cer <- as.data.frame(rbind(data_cer.5, data_cer.75, data_cer, data_cer1.25, data_cer1.5))
colnames(data_cer) <- c('type', 'bandwidth', 'coef', 'ci_l', 'ci_u')

# graph sensitivity to choice of bandwidth
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
  ylab(paste('RD effect on post-election', 'immigrant inclusionary attitudes', sep = '\n'))
ggsave(file = './output/figures/figureI4b.pdf', width=6, height=4.85)
