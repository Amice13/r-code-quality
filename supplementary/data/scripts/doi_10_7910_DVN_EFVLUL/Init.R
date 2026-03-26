################################################################################
## Init File
##   Loads packages
##

rm(list=ls())

setwd(getSrcDirectory(function(){})[1])

## Package names
packages_data = c('broom', 'countrycode', 'data.table', 'dplyr', 'fredr', 'fst',
                  'latex2exp', 'lubridate', 'readxl', 'purrr', 'stringr', 'zoo',
                  'magrittr', 'tidyr')
packages_stat = c('dlm', 'fixest', 'GenSA', 'modelsummary', 'lfe', 'texreg',
                  'YieldCurve', 'lmtest', 'plm', 'stargazer')
packages_figs = c('cowplot', 'ggplot2', 'ggrepel', 'kableExtra', 'RColorBrewer',
                  'viridis', 'ggpubr', 'ggthemes', 'ggpmisc')
packages_par  = c('doFuture', 'doParallel', 'future')

packages = c(packages_data, packages_stat, packages_figs, packages_par)

## Install packages not yet installed
installed_packages <- packages %in% rownames(installed.packages())
if (any(installed_packages == FALSE)) {
  install.packages(packages[!installed_packages])
}

## Packages loading
invisible(lapply(packages, library, character.only = TRUE))

source('HelperFunctions.R')
source('stargazermod.R')

## Definitions
options(repr.plot.width =8, repr.plot.height = 6)
"%ni%"=Negate("%in%")
"%nlk%"=Negate("%like%")
CCYG10 = c("AUD","CAD","CHF","EUR","GBP","JPY","NOK","NZD","SEK","USD")
CCYG9  = c("AUD","CAD","CHF","EUR","GBP","JPY","NOK","NZD","SEK")

FRED_API_KEY <- 'f0b5b56f7e86d60da9b61dd263e9563d'
fredr_set_key(FRED_API_KEY)

dtbegin_covid = ymd(20200201)
dtend_covid   = ymd(20200313)

dtbegin_gfc   = ymd(20080901)
dtend_gfc     = ymd(20081202)
dtend2_gfc    = ymd(20081001)

dtbegin_eur   = ymd(20110701)
dtend_eur     = ymd(20111129)
dtend2_eur    = ymd(20110811)

dtend_dol     = ymd(20171231)

## Produce exhibits?

export = TRUE
