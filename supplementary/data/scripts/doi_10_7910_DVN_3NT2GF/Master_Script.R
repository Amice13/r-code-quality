################################################################################
#####################Master Script##############################################
################################################################################

#####Setups#####

#Set Working Directory to where the data are saved#

#setwd()

#Install and Load Packages#

packages <- c('manifestoR', 'tidyverse', 'rio', 'Hmisc', 'lubridate', 'ggrepel',
              'plm', 'stargazer', 'lme4', 'lattice', 'texreg', 'sjPlot',
              'gridExtra')

#install.packages(packages)

lapply(packages, library, character.only = T)

#Set seed for replication of any random processes#

set.seed(19052022)

#####Calculate Issue salience divergence for all parties in the main dataset#####

source('Code/issue_salience_divergence.R')

#####Calculate polarization on economic and non-economic issues#####

source('Code/polarization.R')

#####Import data for institutional factors#####

source('Code/institutional_factors.R')

#####Identify first time a party enters government####

source('Code/gov_partic_first.R')

#####merge election-level outcomes and predictors together####

source('Code/election_country_merging.R')

####Party-level Analyses#####

source('Code/Party_figures_and_models.R')

####Code for UK Example####

source('Code/uk_example.R')
