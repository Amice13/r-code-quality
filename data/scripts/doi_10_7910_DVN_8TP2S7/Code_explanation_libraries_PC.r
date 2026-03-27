
##########################

#R scripts for analyses asscoiated with the paper by Currie et al.
#"Duration of agriculture and distance from the steppe predict the evolution of large-scale human societies in Afro-Eurasia"

#this file calls the main libraries used and the various sub-components of the analyses


########################################
###         LIBRARIES                ###

#the following packages are used across multiple analysis files
#some other packages (particularly relating to creation of figures) are called within scripts

library(lme4)
library(MASS)
library(AICcmodavg)
library(devtools)
#the package nlmehaversine is used to incorporate spatial distance based on great circle distance
#the package is available at https://github.com/toph-allen/nlmehaversine and is used instead of the regular nlme (although it otherwise functions in the same way)
devtools::load_all(*ADD PATH TO RELEVANT FOLDER*) # load nlmehaversine package w/o installing


########################################
### Choosing Geographical Structure  ###

#examine spatial autocorrelation in the data and select an appropriate model for subsequent analyses

source("Geographical_error_structure.r")




########################################
###       Main Analyses              ###


#correlation matrix

#main spatial GLS analyses

source("Main_analyses.r")



########################################
###      change across time          ###


#examine change in strength of predictors across the time frame considered in these analyses

source("change_over_time.r")


########################################
###    confirmatory analyses         ###


#account for uncertainty in the data by examining the effect of using different versions of predictor and outcome variables

source("confirmatory_analyses.r")



########################################
###        creating figures          ###

#plot change in parameter estimates over time (Fig 2, Fig S5)
#data from published analyses provided but can be run using output from "change_over_time.r"
source("time_series_figure.r")

#plot maps based on main dataset, and associated violin plots, and scatterplots
source("map_scatterplot_figures.r")



########################################
###   data files                     ###


#MAIN DATA USED IN ANALYSES ABOVE
#Gridded_dataset_April_2020.csv


#OUTPUT FROM MAIN ANALYSES
#Main_analyses_all_AICs.csv
#Main_analyses_all_coefficients.csv


#OUTPUT FROM TIME SERIES ANALYSES USED TO MAKE TIME SERIES FIGURES
#joint_time_series_ALL_GLS_with_interaction.txt
#joint_time_series_FE_ALL_GLS_test.txt













