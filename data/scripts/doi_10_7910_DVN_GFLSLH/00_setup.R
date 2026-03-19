#######################################################################################################
##  R AND STATA DEPENDENCIES and PACKAGES                                                            ##
# See Readme.md for instructions for installing dependencies                                          # 
## In R:                                                                                             ## 
# The only package from a non-standard repository is VizTest                                          #
# which can be installed from the author's GitHub with:                                               #
# remotes::install_github("davidaarmstrong/VizTest", ref="a717dd2")                                   #
## IN STATA                                                                                          ##
# net install viztest, from("https://raw.githubusercontent.com/davidaarmstrong/viztest_stata/main/")  #
# ssc install matsort                                                                                 #
#######################################################################################################

library(ggplot2)
library(dplyr)
library(tidyr)
library(VizTest)
library(rio)
library(survey)
library(effects)
library(multcomp)
library(ggeffects)
library(gridExtra)
library(tidybayes) 
library(posterior)
# rstan is required for file code/12_Gill_Retail_Sales_Analysis.R, 
# code/13_FigureA7.R, code/16_FigureA10.R
library(rstan) 
library(BaM)
library(xtable)
library(remotes)
select <- function(...)dplyr::select(...)
filter <- function(...)dplyr::filter(...)

#### Function for letter plots ####
letter_plot <- function(fits, letters){
  if(!(all(c("x", "predicted", "conf.low", "conf.high") %in% names(fits))))stop("x, predicted, conf.low and conf.high need to be variables in the 'fits' data frame.")
  lmat <- letters
  g1 <- ggplot(fits, aes(y=.data[["x"]])) +
    geom_errorbarh(aes(xmin=.data[["conf.low"]], xmax=.data[["conf.high"]]),
                   height=0) +
    geom_point(aes(x=.data[["predicted"]]))
  p <- ggplot_build(g1)
  rgx <- p$layout$panel_params[[1]]$x.range
  diffrg <- diff(rgx)
  prty <- pretty(rgx, 4)
  if(prty[length(prty)] > rgx[2]){
    prty <- prty[-length(prty)]
  }
  labs <- as.character(prty)
  diffrg <- diff(range(c(rgx, prty)))
  firstlet <- max(c(max(prty), rgx[2])) + .075*diffrg
  vl <- max(rgx) + .0375*diffrg
  letbrk <- firstlet + (0:(ncol(lmat)-1))*.05*diffrg
  prty <- c(prty, letbrk)
  labs <- c(labs, LETTERS[1:ncol(lmat)])
  lmat <- t(apply(lmat, 1, function(x)x*letbrk))
  if(any(lmat == 0)){
    lmat[which(lmat == 0, arr.ind=TRUE)] <- NA
  }
  ldat <- as_tibble(lmat, rownames="x")
  dat <- left_join(fits, ldat)
  dat$x <- fits$x
  out <- ggplot(dat, aes(y=.data[["x"]])) +
    geom_errorbarh(aes(xmin=.data[["conf.low"]], xmax=.data[["conf.high"]]), height=0) +
    geom_point(aes(x=.data[["predicted"]]))
  obs_lets <- colnames(lmat)
  for(i in 1:length(obs_lets)){
    out <- out + geom_point(mapping=aes(x=.data[[obs_lets[i]]]), size=2.5)
  }
  out <- out + geom_vline(xintercept=vl, lty=2)+
    scale_x_continuous(breaks=prty,
                       labels=labs) +
    theme_classic() +
    coord_cartesian(clip='off') +
    ylab("")
  out
}
