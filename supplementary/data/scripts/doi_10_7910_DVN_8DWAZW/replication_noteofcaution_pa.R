# R (3.5.1) code to replicate
# How to assess power law behavior using stochastic process methods: A note of caution
# matthias.fatke@uni-konstanz.de
# 15.05.2019

# Please note:
# The code is adapted from vignettes of the poweRlaw package by Gillespie (2015) 
# available at https://rdrr.io/cran/poweRlaw/
# and from the replication material by Breunig and Jones (2011)
# available at https://www.polver.uni-konstanz.de/breunig/team/breunig/
# Credit belongs to them.

##################################################################################################


# loading packages
library(poweRlaw)
library(tidyverse)
library(MASS)

set.seed(42)

# loading data
  ## Available from Christian Breunig's website at
  ## https://www.polver.uni-konstanz.de/typo3temp/secure_downloads/75510/0/42923788b4733eabb6e2e9024ad610c1542fb94c/Replication-ES2011.zip
dat_budget<-read.csv("budget.csv", na.strings = "NA", header=TRUE)

# Please note:
# To obtain the results in the paper using a version of R later than 3.6 it is necessary to revert to
# the old version of the sample function. Running an analysis for your own purpose, I strongly recommend
# using the most current version of R (and the sample function).
RNGkind(sample.kind="Rounding")

# creating power law objects for outlays, domestic, defense
pl_out <- c(NA,abs(diff(log(dat_budget$outlays)))+1) %>% na.omit()%>% conpl$new()
  (pl_out$setXmin(estimate_xmin(pl_out)))
  (pl_out$setPars(estimate_pars(pl_out)))

pl_dom <- c(NA,abs(diff(log(dat_budget$realdom)))+1) %>% na.omit()%>% conpl$new()
  (pl_dom$setXmin(estimate_xmin(pl_dom)))
  (pl_dom$setPars(estimate_pars(pl_dom)))

pl_def <- c(NA,abs(diff(log(dat_budget$realdef)))+1) %>% na.omit()%>% conpl$new()
  (pl_def$setXmin(estimate_xmin(pl_def)))
  (pl_def$setPars(estimate_pars(pl_def)))

# estimating uncertainty of power law parameters via bootstrap
bs_out   = bootstrap(pl_out, no_of_sims=1000, threads=7)
bs_p_out = bootstrap_p(pl_out, no_of_sims=1000, threads=7)

bs_dom   = bootstrap(pl_dom, no_of_sims=1000, threads=7)
bs_p_dom = bootstrap_p(pl_dom, no_of_sims=1000, threads=7)

bs_def   = bootstrap(pl_def, no_of_sims=1000, threads=7)
bs_p_def = bootstrap_p(pl_def, no_of_sims=1000, threads=7)

# Table 1: Statistics and parameter estimates for budget changes
tab1 = cbind(c("", "Outlays", "Domestic", "Defense"),
             c("n",      length(pl_out$dat), length(pl_dom$dat), length(pl_def$dat)),
             c("Median", round(median(pl_out$dat), 3), round(median(pl_dom$dat), 3), round(median(pl_def$dat), 3)),
             c("_sigma", round(sd(pl_out$dat), 3),     round(sd(pl_dom$dat), 3),     round(sd(pl_def$dat), 3)),
             c("X_max",  round(max(pl_out$dat), 3),    round(max(pl_dom$dat), 3),    round(max(pl_def$dat), 3)),
             c("x_min",  round(pl_out$getXmin(), 3),   round(pl_dom$getXmin(), 3),   round(pl_def$getXmin(), 3)),
             c("", paste0("(",round(sd(bs_out$bootstraps[,2]), 3),")"), paste0("(",round(sd(bs_dom$bootstraps[,2]), 3),")"), paste0("(",round(sd(bs_def$bootstraps[,2]), 3),")")),
             c("_alpha",  round(pl_out$getPars(), 3),   round(pl_dom$getPars(), 3),   round(pl_def$getPars(), 3)),
             c("", paste0("(",round(sd(bs_out$bootstraps[,3]), 3),")"), paste0("(",round(sd(bs_dom$bootstraps[,3]), 3),")"), paste0("(",round(sd(bs_def$bootstraps[,3]), 3),")")),
             c("n_tail",  round(get_ntail(pl_out), 3),   round(get_ntail(pl_dom), 3),   round(get_ntail(pl_def), 3)),
             c("", paste0("(",round(sd(bs_out$bootstraps[,4]), 0),")"), paste0("(",round(sd(bs_dom$bootstraps[,4]), 0),")"), paste0("(",round(sd(bs_def$bootstraps[,4]), 0),")")),
             c("_p",  round(bs_p_out$p, 3),   round(bs_p_dom$p, 3),   round(bs_p_def$p, 3)) ); tab1
write.table(tab1, "table1.csv", sep = ",", col.names=FALSE, row.names = FALSE)

# comparing to alternative heavy-tailed distributions

  ## Exponential 
ep_out = c(NA,abs(diff(log(dat_budget$outlays)))+1) %>% na.omit() %>% conexp$new()
  (ep_out$setXmin(pl_out$getXmin()))
  (ep_out$setPars(estimate_pars(ep_out)))

ep_dom = c(NA,abs(diff(log(dat_budget$realdom)))+1) %>% na.omit() %>% conexp$new()
  (ep_dom$setXmin(pl_dom$getXmin()))
  (ep_dom$setPars(estimate_pars(ep_dom)))

ep_def = c(NA,abs(diff(log(dat_budget$realdef)))+1) %>% na.omit() %>% conexp$new()
  (ep_def$setXmin(pl_def$getXmin()))
  (ep_def$setPars(estimate_pars(ep_def)))

  ## Log normal
ln_out = c(NA,abs(diff(log(dat_budget$outlays)))+1) %>% na.omit() %>% conlnorm$new()
  (ln_out$setXmin(pl_out$getXmin()))
  (ln_out$setPars(estimate_pars(ln_out)))

ln_dom = c(NA,abs(diff(log(dat_budget$realdom)))+1) %>% na.omit() %>% conlnorm$new()
  (ln_dom$setXmin(pl_dom$getXmin()))
  (ln_dom$setPars(estimate_pars(ln_dom)))

ln_def = c(NA,abs(diff(log(dat_budget$realdef)))+1) %>% na.omit() %>% conlnorm$new()
  (ln_def$setXmin(pl_def$getXmin()))
  (ln_def$setPars(estimate_pars(ln_def)))

  ## Weibull 
we_out = c(NA,abs(diff(log(dat_budget$outlays)))+1) %>% na.omit() %>% conweibull$new()
  (we_out$setXmin(pl_out$getXmin()))
  (we_out$setPars(estimate_pars(we_out)))

we_dom = c(NA,abs(diff(log(dat_budget$realdom)))+1) %>% na.omit() %>% conweibull$new()
  (we_dom$setXmin(pl_dom$getXmin()))
  (we_dom$setPars(estimate_pars(we_dom)))

we_def = c(NA,abs(diff(log(dat_budget$realdef)))+1) %>% na.omit() %>% conweibull$new()
  (we_def$setXmin(pl_def$getXmin()))
  (we_def$setPars(estimate_pars(we_def)))


# Figure 1: CDFs of budget changes and corresponding fits
pdf("figure1.pdf", width=12, height=3.3)
par(mai=c(.5 , .75 , .5 , .25 ), mgp = c(2.5, .75 ,0), mfrow=c(1,3))

  ## (a) Outlays
plot(pl_out, ylab="CDF", xlab="", main="")
  lines(pl_out, col="#e41a1c", lwd=2)
  lines(ep_out, col="#377eb8", lwd=2, lty=2)
  lines(ln_out, col="#4daf4a", lwd=2, lty=3)
  lines(we_out, col="#984ea3", lwd=2, lty=4)
  legend("topright", 1, c("Power law", "Exponential","Log normal", "Weibull"),
         lty=c(1, 2, 3, 4), lwd=2, col=c("#e41a1c", "#377eb8", "#4daf4a", "#984ea3"), bty="n") 

  ## (b) Domestic
plot(pl_dom, ylab="CDF", xlab="", main="")
  lines(pl_dom, col="#e41a1c", lwd=2)
  lines(ep_dom, col="#377eb8", lwd=2, lty=2)
  lines(ln_dom, col="#4daf4a", lwd=2, lty=3)
  lines(we_dom, col="#984ea3", lwd=2, lty=4)
  legend("topright", 1, c("Power law", "Exponential","Log normal", "Weibull"),
         lty=c(1, 2, 3, 4), lwd=2, col=c("#e41a1c", "#377eb8", "#4daf4a", "#984ea3"), bty="n")

  ## (c) Defense
plot(pl_def, ylab="CDF", xlab="", main="")
  lines(pl_def, col="#e41a1c", lwd=2)
  lines(ep_def, col="#377eb8", lwd=2, lty=2)
  lines(ln_def, col="#4daf4a", lwd=2, lty=3)
  lines(we_def, col="#984ea3", lwd=2, lty=4)
  legend("topright", 1, c("Power law", "Exponential","Log normal", "Weibull"),
         lty=c(1, 2, 3, 4), lwd=2, col=c("#e41a1c", "#377eb8", "#4daf4a", "#984ea3"), bty="n") 
dev.off()

# Table 2:	Log-likelihood ratios and p values for budget change data compared to power law fits
tab2 = cbind(c("", "Exponential", "Log-normal", "Weibull"),
             c("Outlays", round(compare_distributions(pl_out, ep_out)$test_statistic,3),
               round(compare_distributions(pl_out, ln_out)$test_statistic,3),
               round(compare_distributions(pl_out, we_out)$test_statistic,3)),
             c("", paste0("(",round(compare_distributions(pl_out, ep_out)$p_two_sided,3),")"),
               paste0("(",round(compare_distributions(pl_out, ln_out)$p_two_sided,3),")"),
               paste0("(",round(compare_distributions(pl_out, we_out)$p_two_sided,3),")")),
             c("Domestic",  round(compare_distributions(pl_dom, ep_dom)$test_statistic,3),
               round(compare_distributions(pl_dom, ln_dom)$test_statistic,3),
               round(compare_distributions(pl_dom, we_dom)$test_statistic,3)),
             c("", paste0("(",round(compare_distributions(pl_dom, ep_dom)$p_two_sided,3),")"),
               paste0("(",round(compare_distributions(pl_dom, ln_dom)$p_two_sided,3),")"),
               paste0("(",round(compare_distributions(pl_dom, we_dom)$p_two_sided,3),")")),
             c("Defense",  round(compare_distributions(pl_def, ep_def)$test_statistic,3),
               round(compare_distributions(pl_def, ln_def)$test_statistic,3),
               round(compare_distributions(pl_def, we_def)$test_statistic,3)),
             c("", paste0("(",round(compare_distributions(pl_def, ep_def)$p_two_sided,3),")"),
               paste0("(",round(compare_distributions(pl_def, ln_def)$p_two_sided,3),")"),
               paste0("(",round(compare_distributions(pl_def, we_def)$p_two_sided,3),")")) ); tab2
write.table(tab2, "table2.csv", sep = ",", col.names=FALSE, row.names = FALSE)

##################################################################################################
# APPENDIX

# Exponent estimates using "Rank - 1/2"

  ## Outlays
rank_out = c(NA,abs(diff(log(dat_budget$outlays)))+1) %>% na.omit()
rank_out = rank_out[rank_out>pl_out$getXmin()]  %>% sort(., decreasing = TRUE)
lm_out   = lm(log(1:length(rank_out)-.5)~log(rank_out)); lm_out

pl_out_r = pl_out
  pl_out_r$setPars(abs(coef(lm_out)[2]))
  bs_out_r   = bootstrap(pl_out_r, no_of_sims=1000, threads=7)
  bs_p_out_r = bootstrap_p(pl_out_r, no_of_sims=1000, threads=7)
  
  ## Domestic
rank_dom = c(NA,abs(diff(log(dat_budget$realdom)))+1) %>% na.omit()
rank_dom = rank_dom[rank_dom>pl_dom$getXmin()]  %>% sort(., decreasing = TRUE)
lm_dom   = lm(log(1:length(rank_dom)-.5)~log(rank_dom)); lm_dom

pl_dom_r = pl_dom
  pl_dom_r$setPars(abs(coef(lm_dom)[2]))
  bs_dom_r   = bootstrap(pl_dom_r, no_of_sims=1000, threads=7)
  bs_p_dom_r = bootstrap_p(pl_dom_r, no_of_sims=1000, threads=7)
  
  ## Defense
rank_def = c(NA,abs(diff(log(dat_budget$realdef)))+1) %>% na.omit()
rank_def = rank_def[rank_def>pl_def$getXmin()]  %>% sort(., decreasing = TRUE)
lm_def   = lm(log(1:length(rank_def)-.5)~log(rank_def)); lm_def

pl_def_r = pl_def
  pl_def_r$setPars(abs(coef(lm_def)[2]))
  bs_def_r   = bootstrap(pl_def_r, no_of_sims=1000, threads=7)
  bs_p_def_r = bootstrap_p(pl_def_r, no_of_sims=1000, threads=7)


  ## Table A1: Exponent estimates using "Rank - 1/2", p values for goodness-of-fit, log-likelihood ratios and p values compared to power law fits
tabA1 = cbind(c("", "_alpha", "_p", "Exponential", "Log-normal", "Weibull"),
              c("Outlays", round(pl_out_r$getPars(), 3), round(bs_p_out_r$p, 3),
                round(compare_distributions(pl_out_r, ep_out)$test_statistic,3),
                round(compare_distributions(pl_out_r, ln_out)$test_statistic,3),
                round(compare_distributions(pl_out_r, we_out)$test_statistic,3)),
              c("", "", "",
                paste0("(",round(compare_distributions(pl_out_r, ep_out)$p_two_sided,3),")"),
                paste0("(",round(compare_distributions(pl_out_r, ln_out)$p_two_sided,3),")"),
                paste0("(",round(compare_distributions(pl_out_r, we_out)$p_two_sided,3),")")),
              c("Domestic", round(pl_dom_r$getPars(), 3), round(bs_p_dom_r$p, 3),
                round(compare_distributions(pl_dom_r, ep_dom)$test_statistic,3),
                round(compare_distributions(pl_dom_r, ln_dom)$test_statistic,3),
                round(compare_distributions(pl_dom_r, we_dom)$test_statistic,3)),
              c("", "", "",
                paste0("(",round(compare_distributions(pl_dom_r, ep_dom)$p_two_sided,3),")"),
                paste0("(",round(compare_distributions(pl_dom_r, ln_dom)$p_two_sided,3),")"),
                paste0("(",round(compare_distributions(pl_dom_r, we_dom)$p_two_sided,3),")")),
              c("Defense", round(pl_def_r$getPars(), 3), round(bs_p_def_r$p, 3),
                round(compare_distributions(pl_def_r, ep_def)$test_statistic,3),
                round(compare_distributions(pl_def_r, ln_def)$test_statistic,3),
                round(compare_distributions(pl_def_r, we_def)$test_statistic,3)),
              c("", "", "",
                paste0("(",round(compare_distributions(pl_def_r, ep_def)$p_two_sided,3),")"),
                paste0("(",round(compare_distributions(pl_def_r, ln_def)$p_two_sided,3),")"),
                paste0("(",round(compare_distributions(pl_def_r, we_def)$p_two_sided,3),")"))
              ); tabA1
write.table(tabA1, "tableA1.csv", sep = ",", col.names=FALSE, row.names = FALSE)

  ## Figure A1: CDFs of budget changes and corresponding fits
pdf("figureA1.pdf", width=12, height=3.3)
par(mai=c(.5 , .75 , .5 , .25 ), mgp = c(2.5, .75 ,0), mfrow=c(1,3))

    ### (a) Outlays
plot(pl_out_r, ylab="CDF", xlab="", main="")
  lines(pl_out, col="#e41a1c", lwd=2)
  lines(ep_out, col="#377eb8", lwd=2, lty=2)
  lines(ln_out, col="#4daf4a", lwd=2, lty=3)
  lines(we_out, col="#984ea3", lwd=2, lty=4)
  legend("topright", c("Power law", "Exponential","Log normal", "Weibull"),
         lty=c(1, 2, 3, 4), lwd=2, col=c("#e41a1c", "#377eb8", "#4daf4a", "#984ea3"), bty="n") 

   ### (b) Domestic
plot(pl_dom_r, ylab="CDF", xlab="", main="")
  lines(pl_dom, col="#e41a1c", lwd=2)
  lines(ep_dom, col="#377eb8", lwd=2, lty=2)
  lines(ln_dom, col="#4daf4a", lwd=2, lty=3)
  lines(we_dom, col="#984ea3", lwd=2, lty=4)
  legend("topright", c("Power law", "Exponential","Log normal", "Weibull"),
         lty=c(1, 2, 3, 4), lwd=2, col=c("#e41a1c", "#377eb8", "#4daf4a", "#984ea3"), bty="n")

   ### (c) Defense
plot(pl_def_r, ylab="CDF", xlab="", main="")
  lines(pl_def, col="#e41a1c", lwd=2)
  lines(ep_def, col="#377eb8", lwd=2, lty=2)
  lines(ln_def, col="#4daf4a", lwd=2, lty=3)
  lines(we_def, col="#984ea3", lwd=2, lty=4)
  legend("topright", c("Power law", "Exponential","Log normal", "Weibull"),
         lty=c(1, 2, 3, 4), lwd=2, col=c("#e41a1c", "#377eb8", "#4daf4a", "#984ea3"), bty="n") 
dev.off()

# Positive and negative values separately

pl_out_p <- c(NA,abs(diff(log(dat_budget$outlays))[which(diff(log(dat_budget$outlays)) >= 0)])+1) %>% na.omit()%>% conpl$new()
  (pl_out_p$setXmin(estimate_xmin(pl_out_p)))
  (pl_out_p$setPars(estimate_pars(pl_out_p)))

  bs_out_p   = bootstrap(pl_out_p, no_of_sims=1000, threads=7)
  bs_p_out_p = bootstrap_p(pl_out_p, no_of_sims=1000, threads=7)

pl_out_n <- c(NA,abs(diff(log(dat_budget$outlays))[which(diff(log(dat_budget$outlays)) <= 0)])+1) %>% na.omit()%>% conpl$new()
  (pl_out_n$setXmin(estimate_xmin(pl_out_n)))
  (pl_out_n$setPars(estimate_pars(pl_out_n)))

  bs_out_n   = bootstrap(pl_out_n, no_of_sims=1000, threads=7)
  bs_p_out_n = bootstrap_p(pl_out_n, no_of_sims=1000, threads=7)

pl_dom_p <- c(NA,abs(diff(log(dat_budget$realdom))[which(diff(log(dat_budget$realdom)) >= 0)])+1) %>% na.omit()%>% conpl$new()
  (pl_dom_p$setXmin(estimate_xmin(pl_dom_p)))
  (pl_dom_p$setPars(estimate_pars(pl_dom_p)))

  bs_dom_p   = bootstrap(pl_dom_p, no_of_sims=1000, threads=7)
  bs_p_dom_p = bootstrap_p(pl_dom_p, no_of_sims=1000, threads=7)

pl_dom_n <- c(NA,abs(diff(log(dat_budget$realdom))[which(diff(log(dat_budget$realdom)) <= 0)])+1) %>% na.omit()%>% conpl$new()
  (pl_dom_n$setXmin(estimate_xmin(pl_dom_n)))
  (pl_dom_n$setPars(estimate_pars(pl_dom_n)))

  bs_dom_n   = bootstrap(pl_dom_n, no_of_sims=1000, threads=7)
  bs_p_dom_n = bootstrap_p(pl_dom_n, no_of_sims=1000, threads=7)

pl_def_p <- c(NA,abs(diff(log(dat_budget$realdef))[which(diff(log(dat_budget$realdef)) >= 0)])+1) %>% na.omit()%>% conpl$new()
  (pl_def_p$setXmin(estimate_xmin(pl_def_p)))
  (pl_def_p$setPars(estimate_pars(pl_def_p)))

  bs_def_p   = bootstrap(pl_def_p, no_of_sims=1000, threads=7)
  bs_p_def_p = bootstrap_p(pl_def_p, no_of_sims=1000, threads=7)

pl_def_n <- c(NA,abs(diff(log(dat_budget$realdef))[which(diff(log(dat_budget$realdef)) <= 0)])+1) %>% na.omit()%>% conpl$new()
  (pl_def_n$setXmin(estimate_xmin(pl_def_n)))
  (pl_def_n$setPars(estimate_pars(pl_def_n)))

  bs_def_n   = bootstrap(pl_def_n, no_of_sims=1000, threads=7)
  bs_p_def_n = bootstrap_p(pl_def_n, no_of_sims=1000, threads=7)

  
  ## Table A2:  Statistics and parameter estimates for positive and negative values of budget changes
tabA2 = cbind(c("", "Outlays (positive)", "Outlays (negative)", "Domestic (positive)", "Domestic (negative)", "Defense (positive)", "Defense (negative)"),
              c("n",      length(pl_out_p$dat), length(pl_out_n$dat), length(pl_dom_p$dat), length(pl_dom_n$dat), length(pl_def_p$dat), length(pl_def_n$dat)),
              c("Median", round(median(pl_out_p$dat), 3), round(median(pl_out_n$dat), 3),
                round(median(pl_dom_p$dat), 3), round(median(pl_dom_n$dat), 3),
                round(median(pl_def_p$dat), 3), round(median(pl_def_n$dat), 3)),
              c("_sigma", round(sd(pl_out_p$dat), 3), round(sd(pl_out_n$dat), 3),
                round(sd(pl_dom_p$dat), 3), round(sd(pl_dom_n$dat), 3),
                round(sd(pl_def_p$dat), 3), round(sd(pl_def_n$dat), 3)),
              c("X_max",  round(max(pl_out_p$dat), 3), round(max(pl_out_n$dat), 3),
                round(max(pl_dom_p$dat), 3), round(max(pl_dom_n$dat), 3),
                round(max(pl_def_p$dat), 3), round(max(pl_def_n$dat), 3)),
              c("x_min",  round(pl_out_p$getXmin(), 3), round(pl_out_n$getXmin(), 3),
                round(pl_dom_p$getXmin(), 3), round(pl_dom_n$getXmin(), 3),
                round(pl_def_p$getXmin(), 3), round(pl_def_n$getXmin(), 3)),
              c("", paste0("(",round(sd(bs_out_p$bootstraps[,2]), 3),")"), paste0("(",round(sd(bs_out_n$bootstraps[,2]), 3),")"),
                paste0("(",round(sd(bs_dom_p$bootstraps[,2]), 3),")"), paste0("(",round(sd(bs_dom_n$bootstraps[,2]), 3),")"),
                paste0("(",round(sd(bs_def_p$bootstraps[,2]), 3),")"), paste0("(",round(sd(bs_def_n$bootstraps[,2]), 3),")")),
              c("_alpha", round(pl_out_p$getPars(), 3), round(pl_out_n$getPars(), 3),
                round(pl_dom_p$getPars(), 3), round(pl_dom_n$getPars(), 3),
                round(pl_def_p$getPars(), 3), round(pl_def_n$getPars(), 3)),
              c("", paste0("(",round(sd(bs_out_p$bootstraps[,3]), 3),")"), paste0("(",round(sd(bs_out_n$bootstraps[,3]), 3),")"),
                paste0("(",round(sd(bs_dom_p$bootstraps[,3]), 3),")"), paste0("(",round(sd(bs_dom_n$bootstraps[,3]), 3),")"),
                paste0("(",round(sd(bs_def_p$bootstraps[,3]), 3),")"), paste0("(",round(sd(bs_def_n$bootstraps[,3]), 3),")")),
              c("n_tail", round(get_ntail(pl_out_p), 3), round(get_ntail(pl_out_n), 3),
                round(get_ntail(pl_dom_p), 3), round(get_ntail(pl_dom_n), 3),
                round(get_ntail(pl_def_p), 3), round(get_ntail(pl_def_n), 3)),
              c("", paste0("(",round(sd(bs_out_p$bootstraps[,4]), 0),")"), paste0("(",round(sd(bs_out_n$bootstraps[,4]), 0),")"),
                paste0("(",round(sd(bs_dom_p$bootstraps[,4]), 0),")"), paste0("(",round(sd(bs_dom_n$bootstraps[,4]), 0),")"),
                paste0("(",round(sd(bs_def_p$bootstraps[,4]), 0),")"), paste0("(",round(sd(bs_def_n$bootstraps[,4]), 0),")")),
              c("_p", round(bs_p_out_p$p, 3), round(bs_p_out_n$p, 3),
                round(bs_p_dom_p$p, 3), round(bs_p_dom_n$p, 3),
                round(bs_p_def_p$p, 3), round(bs_p_def_n$p, 3)) ); tabA2
write.table(tabA2, "tableA2.csv", sep = ",", col.names=FALSE, row.names = FALSE)


  ## Exponential 
    ### Outlays
ep_out_p = c(NA,abs(diff(log(dat_budget$outlays))[which(diff(log(dat_budget$outlays)) >= 0)])+1) %>% na.omit() %>% conexp$new()
  (ep_out_p$setXmin(pl_out_p$getXmin()))
  (ep_out_p$setPars(estimate_pars(ep_out_p)))
ep_out_n = c(NA,abs(diff(log(dat_budget$outlays))[which(diff(log(dat_budget$outlays)) <= 0)])+1) %>% na.omit() %>% conexp$new()
  (ep_out_n$setXmin(pl_out_n$getXmin()))
  (ep_out_n$setPars(estimate_pars(ep_out_n)))

    ### Domestic
ep_dom_p = c(NA,abs(diff(log(dat_budget$realdom))[which(diff(log(dat_budget$realdom)) >= 0)])+1) %>% na.omit() %>% conexp$new()
  (ep_dom_p$setXmin(pl_dom_p$getXmin()))
  (ep_dom_p$setPars(estimate_pars(ep_dom_p)))
ep_dom_n = c(NA,abs(diff(log(dat_budget$realdom))[which(diff(log(dat_budget$realdom)) <= 0)])+1) %>% na.omit() %>% conexp$new()
  (ep_dom_n$setXmin(pl_dom_n$getXmin()))
  (ep_dom_n$setPars(estimate_pars(ep_dom_n)))

    ### Defense
ep_def_p = c(NA,abs(diff(log(dat_budget$realdef))[which(diff(log(dat_budget$realdef)) >= 0)])+1) %>% na.omit() %>% conexp$new()
  (ep_def_p$setXmin(pl_def_p$getXmin()))
  (ep_def_p$setPars(estimate_pars(ep_def_p)))
ep_def_n = c(NA,abs(diff(log(dat_budget$realdef))[which(diff(log(dat_budget$realdef)) <= 0)])+1) %>% na.omit() %>% conexp$new()
  (ep_def_n$setXmin(pl_def_n$getXmin()))
  (ep_def_n$setPars(estimate_pars(ep_def_n)))

  ## Log normal
    ### Outlays
ln_out_p = c(NA,abs(diff(log(dat_budget$outlays))[which(diff(log(dat_budget$outlays)) >= 0)])+1) %>% na.omit() %>% conlnorm$new()
  (ln_out_p$setXmin(pl_out_p$getXmin()))
  (ln_out_p$setPars(estimate_pars(ln_out_p)))
ln_out_n = c(NA,abs(diff(log(dat_budget$outlays))[which(diff(log(dat_budget$outlays)) <= 0)])+1) %>% na.omit() %>% conlnorm$new()
  (ln_out_n$setXmin(pl_out_n$getXmin()))
  (ln_out_n$setPars(estimate_pars(ln_out_n)))

    ### Domestic
ln_dom_p = c(NA,abs(diff(log(dat_budget$realdom))[which(diff(log(dat_budget$realdom)) >= 0)])+1) %>% na.omit() %>% conlnorm$new()
  (ln_dom_p$setXmin(pl_dom_p$getXmin()))
  (ln_dom_p$setPars(estimate_pars(ln_dom_p)))
ln_dom_n = c(NA,abs(diff(log(dat_budget$realdom))[which(diff(log(dat_budget$realdom)) <= 0)])+1) %>% na.omit() %>% conlnorm$new()
  (ln_dom_n$setXmin(pl_dom_n$getXmin()))
  (ln_dom_n$setPars(estimate_pars(ln_dom_n)))

    ### Defense
ln_def_p = c(NA,abs(diff(log(dat_budget$realdef))[which(diff(log(dat_budget$realdef)) >= 0)])+1) %>% na.omit() %>% conlnorm$new()
  (ln_def_p$setXmin(pl_def_p$getXmin()))
  (ln_def_p$setPars(estimate_pars(ln_def_p)))
ln_def_n = c(NA,abs(diff(log(dat_budget$realdef))[which(diff(log(dat_budget$realdef)) <= 0)])+1) %>% na.omit() %>% conlnorm$new()
  (ln_def_n$setXmin(pl_def_n$getXmin()))
  (ln_def_n$setPars(estimate_pars(ln_def_n)))

  ## Weibull 
    ### Outlays
we_out_p = c(NA,abs(diff(log(dat_budget$outlays))[which(diff(log(dat_budget$outlays)) >= 0)])+1) %>% na.omit() %>% conweibull$new()
  (we_out_p$setXmin(pl_out_p$getXmin()))
  (we_out_p$setPars(estimate_pars(we_out_p)))
we_out_n = c(NA,abs(diff(log(dat_budget$outlays))[which(diff(log(dat_budget$outlays)) <= 0)])+1) %>% na.omit() %>% conweibull$new()
  (we_out_n$setXmin(pl_out_n$getXmin()))
  (we_out_n$setPars(estimate_pars(we_out_n)))

    ### Domestic
we_dom_p = c(NA,abs(diff(log(dat_budget$realdom))[which(diff(log(dat_budget$realdom)) >= 0)])+1) %>% na.omit() %>% conweibull$new()
  (we_dom_p$setXmin(pl_dom_p$getXmin()))
  (we_dom_p$setPars(estimate_pars(we_dom_p)))
we_dom_n = c(NA,abs(diff(log(dat_budget$realdom))[which(diff(log(dat_budget$realdom)) <= 0)])+1) %>% na.omit() %>% conweibull$new()
  (we_dom_n$setXmin(pl_dom_n$getXmin()))
  (we_dom_n$setPars(estimate_pars(we_dom_n)))

    ### Defense
we_def_p = c(NA,abs(diff(log(dat_budget$realdef))[which(diff(log(dat_budget$realdef)) >= 0)])+1) %>% na.omit() %>% conweibull$new()
  (we_def_p$setXmin(pl_def_p$getXmin()))
  (we_def_p$setPars(estimate_pars(we_def_p)))
we_def_n = c(NA,abs(diff(log(dat_budget$realdef))[which(diff(log(dat_budget$realdef)) <= 0)])+1) %>% na.omit() %>% conweibull$new()
  (we_def_n$setXmin(pl_def_n$getXmin()))
  (we_def_n$setPars(estimate_pars(we_def_n)))

  ## Table A3: Log-likelihood ratios and p values for budget changes compared to power law fits
tabA3 = cbind(c("", "Exponential", "Log-normal", "Weibull"),
              c("Outlays (positive)", round(compare_distributions(pl_out_p, ep_out_p)$test_statistic,3),
                round(compare_distributions(pl_out_p, ln_out_p)$test_statistic,3),
                round(compare_distributions(pl_out_p, we_out_p)$test_statistic,3)),
              c("", paste0("(",round(compare_distributions(pl_out_p, ep_out_p)$p_two_sided,3),")"),
                paste0("(",round(compare_distributions(pl_out_p, ln_out_p)$p_two_sided,3),")"),
                paste0("(",round(compare_distributions(pl_out_p, we_out_p)$p_two_sided,3),")")),
              c("Outlays (negative)", round(compare_distributions(pl_out_n, ep_out_n)$test_statistic,3),
                round(compare_distributions(pl_out_n, ln_out_n)$test_statistic,3),
                round(compare_distributions(pl_out_n, we_out_n)$test_statistic,3)),
              c("", paste0("(",round(compare_distributions(pl_out_n, ep_out_n)$p_two_sided,3),")"),
                paste0("(",round(compare_distributions(pl_out_n, ln_out_n)$p_two_sided,3),")"),
                paste0("(",round(compare_distributions(pl_out_n, we_out_n)$p_two_sided,3),")")),
              c("Domestic (positive)",  round(compare_distributions(pl_dom_p, ep_dom_p)$test_statistic,3),
                round(compare_distributions(pl_dom_p, ln_dom_p)$test_statistic,3),
                round(compare_distributions(pl_dom_p, we_dom_p)$test_statistic,3)),
              c("", paste0("(",round(compare_distributions(pl_dom_p, ep_dom_p)$p_two_sided,3),")"),
                paste0("(",round(compare_distributions(pl_dom_p, ln_dom_p)$p_two_sided,3),")"),
                paste0("(",round(compare_distributions(pl_dom_p, we_dom_p)$p_two_sided,3),")")),
              c("Domestic (negative)",  round(compare_distributions(pl_dom_n, ep_dom_n)$test_statistic,3),
                round(compare_distributions(pl_dom_n, ln_dom_n)$test_statistic,3),
                round(compare_distributions(pl_dom_n, we_dom_n)$test_statistic,3)),
              c("", paste0("(",round(compare_distributions(pl_dom_n, ep_dom_n)$p_two_sided,3),")"),
                paste0("(",round(compare_distributions(pl_dom_n, ln_dom_n)$p_two_sided,3),")"),
                paste0("(",round(compare_distributions(pl_dom_n, we_dom_n)$p_two_sided,3),")")),
              c("Defense (positive)",  round(compare_distributions(pl_def_p, ep_def_p)$test_statistic,3),
                round(compare_distributions(pl_def_p, ln_def_p)$test_statistic,3),
                round(compare_distributions(pl_def_p, we_def_p)$test_statistic,3)),
              c("", paste0("(",round(compare_distributions(pl_def_p, ep_def_p)$p_two_sided,3),")"),
                paste0("(",round(compare_distributions(pl_def_p, ln_def_p)$p_two_sided,3),")"),
                paste0("(",round(compare_distributions(pl_def_p, we_def_p)$p_two_sided,3),")")),
              c("Defense (negative)",  round(compare_distributions(pl_def_n, ep_def_n)$test_statistic,3),
                round(compare_distributions(pl_def_n, ln_def_n)$test_statistic,3),
                round(compare_distributions(pl_def_n, we_def_n)$test_statistic,3)),
              c("", paste0("(",round(compare_distributions(pl_def_n, ep_def_n)$p_two_sided,3),")"),
                paste0("(",round(compare_distributions(pl_def_n, ln_def_n)$p_two_sided,3),")"),
                paste0("(",round(compare_distributions(pl_def_n, we_def_n)$p_two_sided,3),")"))
              ); tabA3
write.table(tabA3, "tableA3.csv", sep = ",", col.names=FALSE, row.names = FALSE)


  ## Figure A2: CDFs of budget changes and corresponding fits
pdf("figureA2.pdf", width=8, height=10)
par(mai=c(.5 , .75 , .5 , .25 ), mgp = c(2.5, .75 ,0), mfrow=c(3,2))

    ### (a): Outlays (positive)
plot(pl_out_p, ylab="CDF", xlab="", main="")
  lines(pl_out_p, col="#e41a1c", lwd=2)
  lines(ep_out_p, col="#377eb8", lwd=2, lty=2)
  lines(ln_out_p, col="#4daf4a", lwd=2, lty=3)
  lines(we_out_p, col="#984ea3", lwd=2, lty=4)
  legend("topright", c("Power law", "Exponential","Log normal", "Weibull"),
         lty=c(1, 2, 3, 4), lwd=2, col=c("#e41a1c", "#377eb8", "#4daf4a", "#984ea3"), bty="n") 

    ### (b): Outlays (negative)
plot(pl_out_n, ylab="CDF", xlab="", main="")
  lines(pl_out_n, col="#e41a1c", lwd=2)
  lines(ep_out_n, col="#377eb8", lwd=2, lty=2)
  lines(ln_out_n, col="#4daf4a", lwd=2, lty=3)
  lines(we_out_n, col="#984ea3", lwd=2, lty=4)
  legend("topright", c("Power law", "Exponential","Log normal", "Weibull"),
         lty=c(1, 2, 3, 4), lwd=2, col=c("#e41a1c", "#377eb8", "#4daf4a", "#984ea3"), bty="n") 

    ### (c): (positive)
plot(pl_dom_p, ylab="CDF", xlab="", main="")
  lines(pl_dom_p, col="#e41a1c", lwd=2)
  lines(ep_dom_p, col="#377eb8", lwd=2, lty=2)
  lines(ln_dom_p, col="#4daf4a", lwd=2, lty=3)
  lines(we_dom_p, col="#984ea3", lwd=2, lty=4)
  legend("topright", c("Power law", "Exponential","Log normal", "Weibull"),
         lty=c(1, 2, 3, 4), lwd=2, col=c("#e41a1c", "#377eb8", "#4daf4a", "#984ea3"), bty="n")

    ### (d): (negative)
plot(pl_dom_n, ylab="CDF", xlab="", main="")
  lines(pl_dom_n, col="#e41a1c", lwd=2)
  lines(ep_dom_n, col="#377eb8", lwd=2, lty=2)
  lines(ln_dom_n, col="#4daf4a", lwd=2, lty=3)
  lines(we_dom_n, col="#984ea3", lwd=2, lty=4)
  legend("topright", c("Power law", "Exponential","Log normal", "Weibull"),
         lty=c(1, 2, 3, 4), lwd=2, col=c("#e41a1c", "#377eb8", "#4daf4a", "#984ea3"), bty="n")

    ### (e): Defense (positive)
plot(pl_def_p, ylab="CDF", xlab="", main="")
  lines(pl_def_p, col="#e41a1c", lwd=2)
  lines(ep_def_p, col="#377eb8", lwd=2, lty=2)
  lines(ln_def_p, col="#4daf4a", lwd=2, lty=3)
  lines(we_def_p, col="#984ea3", lwd=2, lty=4)
  legend("topright", c("Power law", "Exponential","Log normal", "Weibull"),
         lty=c(1, 2, 3, 4), lwd=2, col=c("#e41a1c", "#377eb8", "#4daf4a", "#984ea3"), bty="n")

    ### (f): Defense (negative)
plot(pl_def_n, ylab="CDF", xlab="", main="")
  lines(pl_def_n, col="#e41a1c", lwd=2)
  lines(ep_def_n, col="#377eb8", lwd=2, lty=2)
  lines(ln_def_n, col="#4daf4a", lwd=2, lty=3)
  lines(we_def_n, col="#984ea3", lwd=2, lty=4)
  legend("topright", c("Power law", "Exponential","Log normal", "Weibull"),
         lty=c(1, 2, 3, 4), lwd=2, col=c("#e41a1c", "#377eb8", "#4daf4a", "#984ea3"), bty="n") 
dev.off()

# Simulating data for graphical inference

btot <-c(NA, diff(log(dat_budget$outlays))) %>% na.omit()
ln_out2 = ln_out
ln_out2$setPars(c(-5.11425 , 1.070844)) # equivalent form with non-negative Pars

h_pl <- NULL
h_ex <- NULL
h_ln <- NULL
h_we <- NULL

set.seed(-42)
for(i in 1:4){
  pl_rns <- dist_rand(pl_out, 60)
  ep_rns <- dist_rand(ep_out, 60)
  ln_rns <- dist_rand(ln_out2, 60)
  we_rns <- dist_rand(we_out, 60)
  
  btot_pl <- btot[btot>-0.11847  & btot< 0.11847]  %>% c(., (pl_rns-1)*sample(c(-1,1), replace = T, size=length(pl_rns)))
  btot_ex <- btot[btot>-0.11847  & btot< 0.11847]  %>% c(., (ep_rns-1)*sample(c(-1,1), replace = T, size=length(ep_rns)))
  btot_ln <- btot[btot>-0.11847  & btot< 0.11847]  %>% c(., (ln_rns-1)*sample(c(-1,1), replace = T, size=length(ln_rns)))
  btot_we <- btot[btot>-0.11847  & btot< 0.11847]  %>% c(., (we_rns-1)*sample(c(-1,1), replace = T, size=length(we_rns)))
  
  h_pl[[i]] <- hist(btot_pl, nclass=(max(btot_pl)-min(btot_pl))/width.SJ(btot_pl), plot=F)
  h_ex[[i]] <- hist(btot_ex, nclass=(max(btot_ex)-min(btot_ex))/width.SJ(btot_ex), plot=F)
  h_ln[[i]] <- hist(btot_ln, nclass=(max(btot_ln)-min(btot_ln))/width.SJ(btot_ln), plot=F)
  h_we[[i]] <- hist(btot_we, nclass=(max(btot_we)-min(btot_we))/width.SJ(btot_we), plot=F)
}

h_pl[[1]] <- hist(btot, nclass=(max(btot)-min(btot))/width.SJ(btot), plot=F)  # Original outlays data
h_tot <- c(h_pl, h_ex, h_ln, h_we)  # Combine in one list

  ## Figure A3: Histograms of budget changes in outlays and 15 randomly generated data sets
set.seed(42)
ran_order <- sample(1:16, 16, replace = F)

    ### With titles
# pdf("figureA3title.pdf", width=12, height=12)
par(mai=c(.5, .5 , .25, .25), mgp = c(0, 1, 0), mfrow=c(4,4))
set.seed(42)
for(i in 1:length(ran_order)){
  j <- ran_order[i]; j
  plot(h_tot[[j]], col="grey", freq=F, main = h_tot[[j]]$xname, xlab = "Growth Rate", ylab = "Probability Density",xlim=c(-2.5,2.5),ylim=c(0,4))
}
# dev.off()

    ### Without titles
pdf("figureA3.pdf", width=12, height=12)
par(mai=c(.5, .5 , .25, .25), mgp = c(0, 1, 0), mfrow=c(4,4))
set.seed(42)
for(i in 1:length(ran_order)){
  j <- ran_order[i]; j
  plot(h_tot[[j]], col="grey", freq=F, main = "Outlays", xlab = "Growth Rate", ylab = "Probability Density",xlim=c(-2.5,2.5),ylim=c(0,4))
}
dev.off()