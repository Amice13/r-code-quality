rm(list = ls())

# set working directory to YOUR PATH TO REPLICATION MATERIALS
setwd('~/PATH TO REPLICATION MATERIALS/gz_replication_materials/')

# *NOTE*: install rdrobust version 2.0.3 as indicated below
# packageurl <- "https://cran.r-project.org/src/contrib/Archive/rdrobust/rdrobust_2.0.3.tar.gz"
# install.packages(packageurl, repos=NULL, type="source")

## load required packages
library(data.table)
library(rdrobust)
library(estimatr)
library(xtable)

## load data
sentiment <- readRDS('./data/media/media.RDS')
covars_media <- readRDS('./data/media/covars_media.RDS')

### Table 3
# compute RD estimates within different bandwidths: MSE-optimal, 0.5 MSE-optimal, fixed at 10pp
mse <- rdrobust(sentiment[(region_name!='Scotland' & as.numeric(as.character(period))<=3), prop_negative],
                sentiment[(region_name!='Scotland' & as.numeric(as.character(period))<=3), victory_margin],
                covs = sentiment[(region_name!='Scotland' & as.numeric(as.character(period))<=3), covars_media, with = FALSE],
                p = 1,
                kernel = "triangular", bwselect = "mserd", nnmatch = 3,
                cluster = sentiment[(region_name!='Scotland' & as.numeric(as.character(period))<=3), cluster])

mse.5 <- rdrobust(sentiment[(region_name!='Scotland' & as.numeric(as.character(period))<=3), prop_negative],
                  sentiment[(region_name!='Scotland' & as.numeric(as.character(period))<=3), victory_margin],
                  covs = sentiment[(region_name!='Scotland' & as.numeric(as.character(period))<=3), covars_media, with = FALSE],
                  p = 1,
                  kernel = "triangular", h=0.5*mse$bws[1], nnmatch = 3,
                  cluster = sentiment[(region_name!='Scotland' & as.numeric(as.character(period))<=3), cluster])

bws10 <- rdrobust(sentiment[(region_name!='Scotland' & as.numeric(as.character(period))<=3), prop_negative],
                  sentiment[(region_name!='Scotland' & as.numeric(as.character(period))<=3), victory_margin],
                  covs = sentiment[(region_name!='Scotland' & as.numeric(as.character(period))<=3), covars_media, with = FALSE],
                  p = 1,
                  kernel = "triangular", h=10, nnmatch = 3,
                  cluster = sentiment[(region_name!='Scotland' & as.numeric(as.character(period))<=3), cluster])

# get r-squared of models
data <- sentiment[(abs(victory_margin)<=mse$bws[1] & region_name!='Scotland' & as.numeric(as.character(period))<=3)]
data[,weight:= (1-abs((victory_margin/mse$bws[1])))]
data[,treat:=ifelse(victory_margin>0,1,0)]
covs <- data[,covars_media, with=FALSE]

out_mse <- lm_robust(data[,prop_negative] ~ data[,treat]*data[,victory_margin] + as.matrix(covs),     
                               clusters = data[, cluster],
                               weights = data[,weight])

data <- sentiment[(abs(victory_margin)<=mse.5$bws[1] & region_name!='Scotland' & as.numeric(as.character(period))<=3)]
data[, weight:= (1-abs((victory_margin/mse.5$bws[1])))]
data[, treat:=ifelse(victory_margin>0,1,0)]
covs <- data[,covars_media, with=FALSE]

out_mse.5 <- lm_robust(data[,prop_negative] ~ data[,treat]*data[,victory_margin] + as.matrix(covs),     
                                 clusters = data[, cluster],
                                 weights = data[, weight])

data <- sentiment[(abs(victory_margin)<=10 & region_name!='Scotland' & as.numeric(as.character(period))<=3)]
data[, weight:= (1-abs((victory_margin/10)))]
data[, treat:=ifelse(victory_margin>0,1,0)]
covs <- data[,covars_media, with=FALSE]

out_bws10 <- lm_robust(data[,prop_negative] ~ data[,treat]*data[,victory_margin] + as.matrix(covs),     
                                 clusters = data[, cluster],
                                 weights = data[, weight])

# store estimates as data frame
data_main <- as.data.frame(rbind(cbind(mse$coef[1], mse.5$coef[1], bws10$coef[1]),
                                 cbind(mse$se[3], mse.5$se[3], bws10$se[3]),
                                 cbind(mse$beta_p_l[1], mse.5$beta_p_l[1], bws10$beta_p_l[1]),
                                 cbind(out_mse$r.squared, out_mse.5$r.squared, out_bws10$r.squared),
                                 cbind(mse$N_h[1]+mse$N_h[2], mse.5$N_h[1]+mse.5$N_h[2], bws10$N_h[1]+bws10$N_h[2]),
                                 cbind(mse$N[1]+mse$N[2], mse.5$N[1]+mse.5$N[2], bws10$N[1]+bws10$N[2]),
                                 cbind(mse$M[1]+mse$M[2], mse$M[1]+mse$M[2], mse$M[1]+mse$M[2]),
                                 cbind(mse$bws[1], mse.5$bws[1], bws10$bws[1]),
                                 cbind(mse$bws[1], mse$bws[1], mse$bws[1])))

# create row and column names for table
table3 <- cbind(data.frame(labels=c('RD Estimate', ' ', 'Mean DV control', 'R2', 'Num. eff. obs.', 'Num. obs.', 'N Clusters', 'Using bandwidth', 'MSE-optimal bandwidth')),
                data_main)
colnames(table3) <- c('', '(1)', '(2)', '(3)')

# create footnotes for table
comment <- list(pos = list(0), command = NULL)
comment$pos[[1]] <- c(nrow(table3))
comment$command <- c(paste("\\hline\n",
                           "{\\footnotesize Notes: The dependent variable is monthly share of negative mentions in news articles about a candidate's ethnic group 3 months after election. Average treatment effect at cutoff estimated with local linear regression with triangular kernel and MSE-optimal bandwidth in (1), half MSE-optimal in (2) and fixed at 10pp in (3). In parenthesis robust bias-corrected standard errors and clustered by constituency-election. Models control for predetermined covariates.}\n", sep = ""))

# print table
print(xtable(table3,
             align = "llccc",       
             caption = "Ethnic Minority Victory Effects on Media Tone About Migrant Groups",
             label = "table:table_3",
             digits = c(0,0,3,3,3)),
      include.rownames=FALSE,
      caption.placement = "top",
      add.to.row = comment,
      timestamp = NULL,
      file='./output/tables/table3.tex')
