rm(list = ls())

# set working directory to YOUR PATH TO REPLICATION MATERIALS
setwd('~/PATH TO REPLICATION MATERIALS/gz_replication_materials/')

# *NOTE*: install rdrobust version 2.0.3 as indicated below
# packageurl <- "https://cran.r-project.org/src/contrib/Archive/rdrobust/rdrobust_2.0.3.tar.gz"
# install.packages(packageurl, repos=NULL, type="source")

## load required packages
library(data.table)
library(rdrobust)
library(xtable)

## load data
load('./data/crime/hate_crime_violent.RDS')
covars_crime <- readRDS('./data/crime/covars_crime.RDS')

### Table G.3: Ethnic minority victory effects on violent hate crimes
dataset <- list(hate_crimes_w_at_1m, hate_crimes_w_at_2m, hate_crimes_w_at_3m, hate_crimes_w_at_4m,
                hate_crimes_w_at_5m, hate_crimes_w_at_6m, hate_crimes_w_at_7m, hate_crimes_w_at_8m,
                hate_crimes_w_at_9m)
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
month <- c()

for (i in seq(1,length(dataset),1)) {
  
  if (i <= 2) {
    nnmatch = 3
  } else {
    nnmatch = i+1
  }
  
  data <- dataset[[i]]
  
  main <- rdrobust(data[, crime_rate],
                   data[, victory_margin], p = 1,
                   kernel = "triangular", bwselect = "mserd", nnmatch = nnmatch,
                   cluster = data[, cluster])
  
  main_covar <- rdrobust(data[, crime_rate],
                         data[, victory_margin],
                         covs = data[, covars_crime, with = FALSE],
                         p = 1,
                         kernel = "triangular", bwselect = "mserd", nnmatch = nnmatch,
                         cluster = data[, cluster])
  
  # estimates
  coef <- c(coef, main$coef[1,1])
  coef <- c(coef, main_covar$coef[1,1])
  se <- c(se, main$se[1,1])
  se <- c(se, main_covar$se[1,1])
  p_value <- c(p_value, main$pv[3,1])
  p_value <- c(p_value, main_covar$pv[3,1])
  cil <- c(cil, main$ci[3,1])
  cil <- c(cil, main_covar$ci[3,1])
  cir <- c(cir, main$ci[3,2])
  cir <- c(cir, main_covar$ci[3,2])
  mean_control <- c(mean_control, main$beta_p_l[1])
  mean_control <- c(mean_control, main_covar$beta_p_l[1])
  std_effect <- c(std_effect, main$coef[1,1]/(data[((victory_margin >= -main$bws[1,1]) & (victory_margin < 0)),
                                                   sd(crime_rate, na.rm = TRUE)]))
  std_effect <- c(std_effect, main_covar$coef[1,1]/(data[((victory_margin >= -main_covar$bws[1,1]) & (victory_margin < 0)),
                                                         sd(crime_rate, na.rm = TRUE)]))
  std_cil <- c(std_cil, main$ci[3,1]/(data[((victory_margin >= -main$bws[1,1]) & (victory_margin < 0)),
                                           sd(crime_rate, na.rm = TRUE)]))
  std_cil <- c(std_cil, main_covar$ci[3,1]/(data[((victory_margin >= -main_covar$bws[1,1]) & (victory_margin < 0)),
                                                 sd(crime_rate, na.rm = TRUE)]))
  
  std_cir <- c(std_cir, main$ci[3,2]/(data[((victory_margin >= -main$bws[1,1]) & (victory_margin < 0)),
                                           sd(crime_rate, na.rm = TRUE)]))
  std_cir <- c(std_cir, main_covar$ci[3,2]/(data[((victory_margin >= -main_covar$bws[1,1]) & (victory_margin < 0)),
                                                 sd(crime_rate, na.rm = TRUE)]))
  bw <-  c(bw, main$bws[1])
  bw <-  c(bw, main_covar$bws[1])
  nl <- c(nl, main$N_h[1])
  nl <- c(nl, main_covar$N_h[1])
  nr <- c(nr, main$N_h[2])
  nr <- c(nr, main_covar$N_h[2])
  Nl <- c(Nl, main$N[1])
  Nl <- c(Nl, main_covar$N[1])
  Nr <- c(Nr, main$N[2])
  Nr <- c(Nr, main_covar$N[2])
  month <- c(month, rep(i,2))
  
} 

estimates <- data.frame(coef=coef,se=se,p_value=p_value,cil=cil,cir=cir,mean_control=mean_control,
                        std_effect=std_effect,bw=bw,n=nl+nr,N=Nl+Nr,
                        cov = rep(c('no', 'yes'), length(coef)/2),
                        month = month)
estimates <- data.table(estimates)
estimates <- cbind(estimates, cbind(paste0('[', round(estimates$cil,3), ',', round(estimates$cir,3), ']')))
setnames(estimates, 'V1', '95% CI')
order_col <- c('coef', 'se', 'p_value', '95% CI', 'mean_control', 'std_effect', 'bw', 'n', 'N', 'month')
tableg3 <- estimates[cov=='yes', ..order_col]

# add column names to table
names(tableg3) <- c('RD Estimate', 'se', 'p-value', '95% CI', 'mean control', 'sd effect', 'MSE-opt bw', 'eff. N', 'N', 'month')

# create footnotes for table
comment <- list(pos = list(0), command = NULL)
comment$pos[[1]] <- c(nrow(tableg3))
comment$command <- c(paste("\\hline\n",
                           "{\\footnotesize Notes: The dependent variable is monthly hate crimes within the category of 'violence against the person with injury' per 1000 residents in a constituency. \\emph{RD estimate} is computed with local-linear regression within a symmetric MSE-optimal bandwidth. \\emph{se} is the conventional standard error, \\emph{p-value} and \\emph{95\\% CI} are robust bias-corrected. \\emph{mean control} indicates the average monthly hate crime rate in constituencies where ethnic minorities barely lose, \\emph{sd effect} presents the RD estimate in standard deviations, \\emph{MSE-opt bw} is the MSE-optimal bandwidth of vote-share winning margin around the victory threshold, \\emph{eff. N} is the sample size within the MSE-optimal bandwidth and \\emph{N} is the sample size. The model specification includes controls. Standard errors are clustered by constituency-election. Hate crime data are from Home Office, ethnic background of candidates is constructed by the authors, and constituency characteristics from 2011 UK Decennial Census.}\n", sep = ""))

# print table
print(xtable(tableg3,
             align = "lcccccccccc",       
             caption = "Ethnic Minority Victory Effects on Violent Hate Crime",
             label = "table:table_g3",
             digits = c(0,4,3,3,3,3,3,2,0,0,0)),
      include.rownames=FALSE,
      caption.placement = "top",
      add.to.row = comment,
      timestamp = NULL,
      file='./output/tables/tableG3.tex')





