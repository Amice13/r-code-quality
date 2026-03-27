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

## load and subset data to white respondents in England, Wales
load(file='./data/attitudes/attitudes.rds')
bes_rdd <- bes_rdd[grepl('^(E|W)', ons_id) & white==1]
covars_attitudes <- readRDS('./data/attitudes/covars_attitudes.RDS')

### Table 2
# compute RD estimates within different bandwidths: MSE-optimal, 0.5 MSE-optimal, fixed at 10pp
mse <- rdrobust(bes_rdd[, j05],
                bes_rdd[,victory_margin],
                covs = bes_rdd[, covars_attitudes, with=F],
                kernel = "triangular", p = 1, bwselect = "mserd",
                cluster = bes_rdd[,cluster])

mse.5 <- rdrobust(bes_rdd[, j05],
                  bes_rdd[,victory_margin],
                  covs = bes_rdd[, covars_attitudes, with=F],
                  kernel = "triangular", p = 1, bwselect = "mserd", h=0.5*mse$bws[1],
                  cluster = bes_rdd[,cluster])

bws10 <- rdrobust(bes_rdd[, j05],
                  bes_rdd[,victory_margin],
                  covs = bes_rdd[, covars_attitudes, with=F],
                  kernel = "triangular", p = 1, bwselect = "mserd", h=10,
                  cluster = bes_rdd[,cluster])

# get r-squared of models
data <- bes_rdd[(abs(victory_margin)<=mse$bws[1])]
data[,weight:= (1-abs((victory_margin/mse$bws[1])))]
data[,treat:=ifelse(victory_margin>0,1,0)]
covs <- data[, covars_attitudes, with=FALSE]

out_mse <- lm_robust(data[,j05] ~ data[,treat]*data[,victory_margin] + as.matrix(covs),     
                               clusters = data[, cluster],
                               weights = data[,weight])

data <- bes_rdd[(abs(victory_margin)<=mse.5$bws[1])]
data[,weight:= (1-abs((victory_margin/mse.5$bws[1])))]
data[,treat:=ifelse(victory_margin>0,1,0)]
covs <- data[, covars_attitudes, with=FALSE]

out_mse.5 <- lm_robust(data[,j05] ~ data[,treat]*data[,victory_margin] + as.matrix(covs),     
                                 clusters = data[, cluster],
                                 weights = data[,weight])

data <- bes_rdd[(abs(victory_margin)<=bws10$bws[1])]
data[,weight:= (1-abs((victory_margin/bws10$bws[1])))]
data[,treat:=ifelse(victory_margin>0,1,0)]
covs <- data[, covars_attitudes, with=FALSE]

out_bws10 <- lm_robust(data[,j05] ~ data[,treat]*data[,victory_margin] + as.matrix(covs),     
                                 clusters = data[, cluster],
                                 weights = data[,weight])

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
table2 <- cbind(data.frame(labels=c('RD Estimate', ' ', 'Mean DV control', 'R2', 'Num. eff. obs.', 'Num. obs.', 'N Clusters', 'Using bandwidth', 'MSE-optimal bandwidth')),
                data_main)
colnames(table2) <- c('', '(1)', '(2)', '(3)')

# create footnotes for table
comment <- list(pos = list(0), command = NULL)
comment$pos[[1]] <- c(nrow(table2))
comment$command <- c(paste("\\hline\n",
                           "{\\footnotesize Notes: The dependent variable indicates whether survey respondents do not thinks that \"too many immigrants have been let into the country\". Average treatment effect at cutoff estimated with local linear regression with triangular kernel and MSE-optimal bandwidth in (1), half MSE-optimal in (2) and fixed at 10pp in (3). In parenthesis standard errors robust bias-corrected and clustered by constituency-election. Models control for predetermined covariates.}\n", sep = ""))

# print table
print(xtable(table2,
             align = "llccc",       
             caption = "Ethnic Minority Victory Effects on Attitudes Towards Immigrants",
             label = "table:table_2",
             digits = c(0,0,3,3,3)),
      include.rownames=FALSE,
      caption.placement = "top",
      add.to.row = comment,
      timestamp = NULL,
      file='./output/tables/table2.tex')
