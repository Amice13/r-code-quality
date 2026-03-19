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
library(ggplot2)
library(xtable)

## load data
load(file='./data/attitudes/attitudes.rds')
bes_rdd <- bes_rdd[grepl('^(E|W)', ons_id) & white==1]
covars_attitudes <- readRDS('./data/attitudes/covars_attitudes.RDS')


### Table I.1: Ethnic minority victory effects on mass inclusionary attitudes towards immigrants: Sensitivity to Polynomial Order
outcomes <- c('j05')
y_labels <- c('not too many immigrants')
bes_rdd_complete <- bes_rdd[complete.cases(bes_rdd[, covars_attitudes, with = FALSE])]


## Linear Polynomial
coef <- c()
se <- c()
p_value <- c()
cil <- c()
cir <- c()
mean_control <- c()
std_effect <- c()
std_cil <- c()
std_cir <- c()
bw <-  c()
nl <- c()
nr <- c()
Nl <- c()
Nr <- c()

for (i in outcomes) {
  
  main <- rdrobust(bes_rdd[[i]],
                   bes_rdd$victory_margin,
                   kernel = "triangular", p = 1, bwselect = "mserd",
                   cluster = bes_rdd$cluster)
  
  main_complete <- rdrobust(bes_rdd_complete[[i]],
                            bes_rdd_complete$victory_margin,
                            kernel = "triangular", p = 1, bwselect = "mserd",
                            cluster = bes_rdd_complete$cluster)
  
  main_covar <- rdrobust(bes_rdd[[i]],
                         bes_rdd$victory_margin,
                         covs = bes_rdd[, covars_attitudes, with=F],
                         kernel = "triangular", p = 1, bwselect = "mserd",
                         cluster = bes_rdd$cluster)
  
  
  coef <- c(coef, main$coef[1,1])
  coef <- c(coef, main_complete$coef[1,1])
  coef <- c(coef, main_covar$coef[1,1])
  se <- c(se, main$se[1,1])
  se <- c(se, main_complete$se[1,1])
  se <- c(se, main_covar$se[1,1])
  p_value <- c(p_value, main$pv[3,1])
  p_value <- c(p_value, main_complete$pv[3,1])
  p_value <- c(p_value, main_covar$pv[3,1])
  cil <- c(cil, main$ci[3,1])
  cil <- c(cil, main_complete$ci[3,1])
  cil <- c(cil, main_covar$ci[3,1])
  cir <- c(cir, main$ci[3,2])
  cir <- c(cir, main_complete$ci[3,2])
  cir <- c(cir, main_covar$ci[3,2])
  mean_control <- c(mean_control, main$beta_p_l[1])
  mean_control <- c(mean_control, main_complete$beta_p_l[1])
  mean_control <- c(mean_control, main_covar$beta_p_l[1])
  
  std_effect <- c(std_effect, main$coef[1,1]/(bes_rdd[((victory_margin >= -main$bws[1,1]) & (victory_margin < 0)),
                                                      sd(get(i), na.rm = TRUE)]))
  std_effect <- c(std_effect, main_complete$coef[1,1]/(bes_rdd_complete[((victory_margin >= -main_complete$bws[1,1]) & (victory_margin < 0)),
                                                                        sd(get(i), na.rm = TRUE)]))
  
  std_effect <- c(std_effect, main_covar$coef[1,1]/(bes_rdd_complete[((victory_margin >= -main_covar$bws[1,1]) & (victory_margin < 0)),
                                                                     sd(get(i), na.rm = TRUE)]))
  std_cil <- c(std_cil, main$ci[3,1]/(bes_rdd[((victory_margin >= -main$bws[1,1]) & (victory_margin < 0)),
                                              sd(get(i), na.rm = TRUE)]))
  std_cil <- c(std_cil, main_complete$ci[3,1]/(bes_rdd_complete[((victory_margin >= -main_complete$bws[1,1]) & (victory_margin < 0)),
                                                                sd(get(i), na.rm = TRUE)]))
  
  std_cil <- c(std_cil, main_covar$ci[3,1]/(bes_rdd_complete[((victory_margin >= -main_covar$bws[1,1]) & (victory_margin < 0)),
                                                             sd(get(i), na.rm = TRUE)]))
  
  std_cir <- c(std_cir, main$ci[3,2]/(bes_rdd[((victory_margin >= -main$bws[1,1]) & (victory_margin < 0)),
                                              sd(get(i), na.rm = TRUE)]))
  std_cir <- c(std_cir, main_complete$ci[3,2]/(bes_rdd_complete[((victory_margin >= -main_complete$bws[1,1]) & (victory_margin < 0)),
                                                                sd(get(i), na.rm = TRUE)]))
  std_cir <- c(std_cir, main_covar$ci[3,2]/(bes_rdd_complete[((victory_margin >= -main_covar$bws[1,1]) & (victory_margin < 0)),
                                                             sd(get(i), na.rm = TRUE)]))
  
  
  bw <-  c(bw, main$bws[1])
  bw <-  c(bw, main_complete$bws[1])
  bw <-  c(bw, main_covar$bws[1])
  nl <- c(nl, main$N_h[1])
  nl <- c(nl, main_complete$N_h[1])
  nl <- c(nl, main_covar$N_h[1])
  nr <- c(nr, main$N_h[2])
  nr <- c(nr, main_complete$N_h[2])
  nr <- c(nr, main_covar$N_h[2])
  Nl <- c(Nl, main$N[1])
  Nl <- c(Nl, main_complete$N[1])
  Nl <- c(Nl, main_covar$N[1])
  Nr <- c(Nr, main$N[2])
  Nr <- c(Nr, main_complete$N[2])
  Nr <- c(Nr, main_covar$N[2])
  
}

estimates_linear <- data.frame(coef=coef,se=se,p_value=p_value,cil=cil,cir=cir,mean_control=mean_control,
                                  std_effect=std_effect,bw=bw,n=nl+nr,N=Nl+Nr,
                                  outcome = rep(y_labels,3),
                                  cov = rep(c('no','no','yes'),1),
                                  sample = rep(c('full', 'complete cases', 'complete cases'),1),
                                  polynomial = rep('linear', 3))



## Quadratic Polynomial
coef <- c()
se <- c()
p_value <- c()
cil <- c()
cir <- c()
mean_control <- c()
std_effect <- c()
std_cil <- c()
std_cir <- c()
bw <-  c()
nl <- c()
nr <- c()
Nl <- c()
Nr <- c()

for (i in outcomes) {
  
  main <- rdrobust(bes_rdd[[i]],
                   bes_rdd$victory_margin,
                   kernel = "triangular", p = 2, bwselect = "mserd",
                   cluster = bes_rdd$cluster)
  
  main_complete <- rdrobust(bes_rdd_complete[[i]],
                            bes_rdd_complete$victory_margin,
                            kernel = "triangular", p = 2, bwselect = "mserd",
                            cluster = bes_rdd_complete$cluster)
  
  main_covar <- rdrobust(bes_rdd[[i]],
                         bes_rdd$victory_margin,
                         covs = bes_rdd[, covars_attitudes, with=F],
                         kernel = "triangular", p = 2, bwselect = "mserd",
                         cluster = bes_rdd$cluster)
  
  
  coef <- c(coef, main$coef[1,1])
  coef <- c(coef, main_complete$coef[1,1])
  coef <- c(coef, main_covar$coef[1,1])
  se <- c(se, main$se[1,1])
  se <- c(se, main_complete$se[1,1])
  se <- c(se, main_covar$se[1,1])
  p_value <- c(p_value, main$pv[3,1])
  p_value <- c(p_value, main_complete$pv[3,1])
  p_value <- c(p_value, main_covar$pv[3,1])
  cil <- c(cil, main$ci[3,1])
  cil <- c(cil, main_complete$ci[3,1])
  cil <- c(cil, main_covar$ci[3,1])
  cir <- c(cir, main$ci[3,2])
  cir <- c(cir, main_complete$ci[3,2])
  cir <- c(cir, main_covar$ci[3,2])
  mean_control <- c(mean_control, main$beta_p_l[1])
  mean_control <- c(mean_control, main_complete$beta_p_l[1])
  mean_control <- c(mean_control, main_covar$beta_p_l[1])
  
  std_effect <- c(std_effect, main$coef[1,1]/(bes_rdd[((victory_margin >= -main$bws[1,1]) & (victory_margin < 0)),
                                                      sd(get(i), na.rm = TRUE)]))
  std_effect <- c(std_effect, main_complete$coef[1,1]/(bes_rdd_complete[((victory_margin >= -main_complete$bws[1,1]) & (victory_margin < 0)),
                                                                        sd(get(i), na.rm = TRUE)]))
  
  std_effect <- c(std_effect, main_covar$coef[1,1]/(bes_rdd_complete[((victory_margin >= -main_covar$bws[1,1]) & (victory_margin < 0)),
                                                                     sd(get(i), na.rm = TRUE)]))
  std_cil <- c(std_cil, main$ci[3,1]/(bes_rdd[((victory_margin >= -main$bws[1,1]) & (victory_margin < 0)),
                                              sd(get(i), na.rm = TRUE)]))
  std_cil <- c(std_cil, main_complete$ci[3,1]/(bes_rdd_complete[((victory_margin >= -main_complete$bws[1,1]) & (victory_margin < 0)),
                                                                sd(get(i), na.rm = TRUE)]))
  
  std_cil <- c(std_cil, main_covar$ci[3,1]/(bes_rdd_complete[((victory_margin >= -main_covar$bws[1,1]) & (victory_margin < 0)),
                                                             sd(get(i), na.rm = TRUE)]))
  
  std_cir <- c(std_cir, main$ci[3,2]/(bes_rdd[((victory_margin >= -main$bws[1,1]) & (victory_margin < 0)),
                                              sd(get(i), na.rm = TRUE)]))
  std_cir <- c(std_cir, main_complete$ci[3,2]/(bes_rdd_complete[((victory_margin >= -main_complete$bws[1,1]) & (victory_margin < 0)),
                                                                sd(get(i), na.rm = TRUE)]))
  std_cir <- c(std_cir, main_covar$ci[3,2]/(bes_rdd_complete[((victory_margin >= -main_covar$bws[1,1]) & (victory_margin < 0)),
                                                             sd(get(i), na.rm = TRUE)]))
  
  
  bw <-  c(bw, main$bws[1])
  bw <-  c(bw, main_complete$bws[1])
  bw <-  c(bw, main_covar$bws[1])
  nl <- c(nl, main$N_h[1])
  nl <- c(nl, main_complete$N_h[1])
  nl <- c(nl, main_covar$N_h[1])
  nr <- c(nr, main$N_h[2])
  nr <- c(nr, main_complete$N_h[2])
  nr <- c(nr, main_covar$N_h[2])
  Nl <- c(Nl, main$N[1])
  Nl <- c(Nl, main_complete$N[1])
  Nl <- c(Nl, main_covar$N[1])
  Nr <- c(Nr, main$N[2])
  Nr <- c(Nr, main_complete$N[2])
  Nr <- c(Nr, main_covar$N[2])
  
}

estimates_quadratic <- data.frame(coef=coef,se=se,p_value=p_value,cil=cil,cir=cir,mean_control=mean_control,
                        std_effect=std_effect,bw=bw,n=nl+nr,N=Nl+Nr,
                        outcome = rep(y_labels,3),
                        cov = rep(c('no','no','yes'),1),
                        sample = rep(c('full', 'complete cases', 'complete cases'),1),
                        polynomial = rep('quadratic', 3))


estimates <- rbind(estimates_linear, estimates_quadratic)

# in-text number
estimates_linear <- data.table(estimates_linear)
estimates_linear[cov=='yes' & sample=='complete cases', round(std_effect,2)]
estimates_linear[cov=='yes' & sample=='complete cases', round((coef/mean_control),2)*100]
estimates_linear[cov=='yes' & sample=='complete cases', round(coef,1)]

estimates <- data.table(estimates)
estimates <- cbind(estimates, cbind(paste0('[', round(estimates$cil,3), ',', round(estimates$cir,3), ']')))
setnames(estimates, 'V1', '95% CI')

order_col <- c('coef', 'se', 'p_value', '95% CI', 'mean_control', 'std_effect', 'bw', 'n', 'N', 'cov', 'sample', 'polynomial')
tablei1 <- estimates[, ..order_col]

# add column names to table
names(tablei1) <- c('RD Estimate', 'se', 'p-value', '95% CI', 'mean control', 'sd effect', 'MSE-opt bw', 'eff. N', 'N', 'cov', 'sample', 'polynomial')

# create footnotes for table
comment <- list(pos = list(0), command = NULL)
comment$pos[[1]] <- c(nrow(tablei1))
comment$command <- c(paste("\\hline\n",
                           "{\\footnotesize Notes: The dependent variable is a dummy indicating whether a survey respondent \\emph{do not} thinks that too many immigrants have been let into the country. \\emph{RD estimate} is computed with local-linear regression within a symmetric MSE-optimal bandwidth when \\emph{polynomial} is \\emph{linear}, and with a quadratic polynomial when \\emph{polynomial} is \\emph{quadratic}. \\emph{se} is the conventional standard error, \\emph{p-value} and \\emph{95\\% CI} are robust bias-corrected. \\emph{mean control} indicates the average dependent variable value in constituencies where ethnic minorities barely lose. \\emph{sd effect} presents the RD estimate in standard deviations, \\emph{MSE-opt bw} is the MSE-optimal bandwidth of vote-share winning margin around the victory threshold, \\emph{eff. N} is the sample size within the MSE-optimal bandwidth and \\emph{N} is the sample size. \\emph{cov} includes predetermined covariates in the model specification. \\emph{smpl} is the used sample: \\emph{f} stands for full sample and \\emph{c} for a complete cases sample with no missing values for respondent's predetermined variables. Standard errors are clustered by constituency-election. Survey data are from the British Election Study, ethnic background of candidates is constructed by the authors, and constituency characteristics from 2001 and 2011 UK Decennial Census.}\n", sep = ""))

# print table
print(xtable(tablei1,
             align = "lcccccccccccc",       
             caption = "Ethnic minority victory effects on mass inclusionary attitudes towards immigrants",
             label = "table:table_i1",
             digits = c(0,3,3,3,3,3,3,2,0,0,0,0,0)),
      include.rownames=FALSE,
      caption.placement = "top",
      add.to.row = comment,
      timestamp = NULL,
      file='./output/tables/tableI1.tex')





