rm(list = ls())

# set working directory to YOUR PATH TO REPLICATION MATERIALS
setwd('~/PATH TO REPLICATION MATERIALS/gz_replication_materials/')

## load required packages
library(data.table)
library(xtable)

### Table G1: Ethnic minority victory effects on hate crimes
estimates <- readRDS(file = './output/estimates/crime_estimates.RDS')
estimates_table <- cbind(estimates, cbind(paste0('[', round(estimates$cil,3), ',', round(estimates$cir,3), ']')))
setnames(estimates_table, 'V1', '95% CI')
order_col <- c('coef', 'se', 'p_value', '95% CI', 'mean_control', 'std_effect', 'bw', 'n', 'N', 'cov', 'month')

tableg1 <- estimates_table[, ..order_col]
names(tableg1) <- c('RD Estimate', 'se', 'p-value', '95% CI', 'mean control', 'sd effect', 'MSE-opt bw', 'eff. N', 'N', 'controls', 'month')

# create footnotes for table
comment <- list(pos = list(0), command = NULL)
comment$pos[[1]] <- c(nrow(tableg1))
comment$command <- c(paste("\\hline\n",
                           "{\\footnotesize Notes: The dependent variable is monthly hate crimes per 1000 residents in a constituency. \\emph{RD estimate} is computed with local-linear regression within a symmetric MSE-optimal bandwidth. \\emph{se} is the conventional standard error, \\emph{p-value} and \\emph{95\\% CI} are robust bias-corrected. \\emph{mean control} indicates the average monthly hate crime rate in constituencies where ethnic minorities barely lose, \\emph{sd effect} presents the RD estimate in standard deviations, \\emph{MSE-opt bw} is the MSE-optimal bandwidth of vote-share winning margin around the victory threshold, \\emph{eff. N} is the sample size within the MSE-optimal bandwidth and \\emph{N} is the sample size. \\emph{controls} include an indicator of whether the candidate is the incumbent, constituency vote share for UKIP and BNP in the previous election, constituency share that is ethnic minority, young population, single, with social grade DE, unemployed, population density, and share of households with 3 or more deprivations, and in social tenure. Standard errors are clustered by constituency-election. Hate crime data are from Home Office, ethnic background of candidates is constructed by the authors, and constituency characteristics from 2011 UK Decennial Census.}\n", sep = ""))

# print table -- Table G1
print(xtable(tableg1,
             align = "lccccccccccc",       
             caption = "Ethnic Minority Victory Effects on Hate Crimes",
             label = "table:table_G1",
             digits = c(0,3,3,3,3,3,3,3,3,3,3,3)),
      include.rownames=FALSE,
      caption.placement = "top",
      add.to.row = comment,
      timestamp = NULL,
      file='./output/tables/tableG1.tex')
