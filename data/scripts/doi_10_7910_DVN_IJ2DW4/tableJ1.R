rm(list = ls())

# set working directory to YOUR PATH TO REPLICATION MATERIALS
setwd('~/PATH TO REPLICATION MATERIALS/gz_replication_materials/')


## load required packages
library(data.table)
library(xtable)


## load estimates computed in Figure 3b
estimates <- readRDS('./output/estimates/media_estimates.RDS')
estimates[,c('n', 'N'):=.(nl+nr, Nl+Nr)]
estimates <- cbind(estimates, cbind(paste0('[', round(estimates$cil,3), ',', round(estimates$cir,3), ']')))
setnames(estimates, 'V1', '95% CI')

order_col <- c('coef', 'se', 'p_value', '95% CI', 'mean_control', 'std_effect', 'bw', 'n', 'N', 'cov', 'month')
tablej1 <- estimates[outcome == 'prop_negative', ..order_col]

names(tablej1) <- c('RD Estimate', 'se', 'p-value', '95% CI', 'mean control', 'sd effect', 'MSE-opt bw', 'eff. N', 'N', 'cov', 'month')

# create footnotes for table
comment <- list(pos = list(0), command = NULL)
comment$pos[[1]] <- c(nrow(tablej1))
comment$command <- c(paste("\\hline\n",
                           "{\\footnotesize Notes: The dependent variable is the monthly proportion of negative mentions in news articles about a candidate's ethnic group. \\emph{RD estimate} is computed with local-linear regression within a symmetric MSE-optimal bandwidth. \\emph{se} is the conventional standard error, \\emph{p-value} and \\emph{95\\% CI} are robust bias-corrected. \\emph{mean control} indicates the average proportion of negative news article mentions about the barely losing candidate's ethnic group. \\emph{sd effect} presents the RD estimate in standard deviations, \\emph{MSE-opt bw} is the MSE-optimal bandwidth of vote-share winning margin around the victory threshold, \\emph{eff. N} is the sample size within the MSE-optimal bandwidth and \\emph{N} is the sample size. \\emph{cov} is a vector of controls including whether the candidate is the incumbent, from a left-leaning party, a woman, a first-generation immigrant, the constituency vote share for UKIP and BNP in the previous election, constituency share that shares the candidate's ethnic background, shares of foreign born, with a minority religion, young population, single, with level 1 qualifications, with social grade DE, unemployed, and share of households with 4 or more deprivations, and in social tenure. Standard errors are clustered by constituency-election. News articles were extracted from Common Crawl, ethnic background of candidates is constructed by the authors, and constituency characteristics from 2001 and 2011 UK Decennial Census.}\n", sep = ""))

# print table
print(xtable(tablej1,
             align = "lccccccccccc",       
             caption = "Ethnic minority victory effects on media tone about migrant groups",
             label = "table:table_i1",
             digits = c(0,3,3,3,3,3,3,2,0,0,0,0)),
      include.rownames=FALSE,
      caption.placement = "top",
      add.to.row = comment,
      timestamp = NULL,
      file='./output/tables/tableJ1.tex')
