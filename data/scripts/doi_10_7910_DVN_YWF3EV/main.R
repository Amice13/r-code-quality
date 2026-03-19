


# change this filepath to be the root file path
setwd('/Users/shivram/Documents/Projects/research/gov2001_replication')
getwd()


# filepaths

data <- file.path(getwd(), "data")
code <- file.path(getwd(), "code")
output <- file.path(getwd(), "output")


# set up packages
packages <- c('tidyverse','readxl', 'stats','reshape2', 'ggplot2', 'cowplot', 'utils',
  'stringr', 'haven', 'stargazer', 'broom', 'data.table', 'dtplyr',
  'stargazer', 'xtable', 'jtools', 'ggrepel', 'gghighlight', 'foreign',
  'lubridate', 'ggthemes', 'kableExtra', 'sf', 'modelsummary', 'margins',
  'modeest', 'here', 'easypackages', 'clusterSEs', 'fixest', 'FactoMineR',
  'readstata13', 'RColorBrewer')

if(length(which(!packages %in% installed.packages())) > 0) {
  install.packages(packages[!packages %in% installed.packages()])
}

library(easypackages)
libraries(packages)



# WVS Measures ------------------------------------------------------------

source(file.path(code, 'wvs/construct_measures.R'))




# Afrobarometer Measures --------------------------------------------------

source(file.path(code, 'afrobarometer/1_cleaning.R'))

source(file.path(code, 'afrobarometer/2_creatingmeasures.R'))



# Outputs -----------------------------------------------------------------

source(file.path(code, 'afrobarometer/3_creatingoutput.R'))

source(file.path(code, 'other_outputs.R'))



