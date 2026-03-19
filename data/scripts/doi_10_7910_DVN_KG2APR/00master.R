## - If you're working on a Mac, you may have to install Command Line Tools and XQuartz (https://www.xquartz.org) if you have not yet done so or have them pre-installed
## packages needed for replication 
## (may take a while, see a_Software_Info.txt for information on package versions)
packages <- c("tm","data.table","xtable","zoo", "dga", "chron", "rgeos",
              "tidyr","dplyr","reshape","plyr","digest","filehash",
              "ggplot2","maptools","rgdal","gridExtra","scales","MASS",
              "sandwich","lmtest","texreg", "randomForest", "quanteda",
              "xgboost", "remotes", "tree")
install.packages(packages) # if prompted to install dependencies: type/click: Yes

## adapt working directory path to where you've saved the replication folder
# setwd("..")

#######################################################################
##### Classification of targeted and untargeted violence ######
#######################################################################


### xgboost Classification
source("01-classify-xgboost.R")
##  input:
    ## trainingset.csv - hand coded details of death
    ## merged-records-details.RData - full sample of documented violence
## sources code from:
  ## 02-xgboost_functions.R
## output:
    ## xgboost-performance.Rdata - performance of classifier
    ## classified_data-xgboost.RData - full sample of documented violence + xgboost classifications


#######################################################################
##### Cleaning and aggregating Internet access ######
#######################################################################
source("03-import-access.R")
## input: access.csv
## output: date2w-access.csv


#######################################################################
##### Cleaning and aggregating Internet access ######
#######################################################################
source("04-aggregate-control.R")
## input: terr_control.csv
## output: date2w-control.csv


#######################################################################
##### Estimate unreported violence:  ######
#######################################################################

### Step 1: import data from classification
source("05-import-viol-for-est.R")
## input: classified_data-xgboost.RData
## output: classified_data.RData

### Step 2: build strata and estimate
source("06-estimate_r.R")
## input: classified_data.RData 
## output: estimates-dga-date2w.Rdata


#######################################################################
##### Concat all data needed for analysis:  ######
#######################################################################
source("07-merge-data.R")
## input: estimates-dga-date2w.Rdata - violence estimates
## input: date2w-control.Rdata - territorial control/ armed group presence
## input: date2w-access.Rdata - internet accessibility
## input: EPR-Syria-bygov.csv - Ethnic groups
## input: syr_pop_2011.csv - Population Data
## input: SY_unemployment.csv - Unemployment
## output: analysis-data.Rdata - see Codebook.pdf for full description of all variables

#######################################################################
##### Descriptive graphs, maps, and tables :  ######
#######################################################################
source("08-descriptives.R")
## input: analysis-data.Rdata - see Codebook.pdf for full description of all variables
## input: classified_data-xgboost.RData
## output: Figures 1, 2, APPENDIX: A5, A8, A9, A10, A11

source("09-control-maps.R")
## input: terr_control.csv
## input: SYR_adm1*
## output: APPENDIX: Figure A4

#######################################################################
##### Multivariate Analyses :  ######
#######################################################################
## Note that all tables generated with the following two files were manually edited to include an additional row
## of information that indicates whether a model included temporal fixed effects, or not.
## The latex code manually added is a check mark (\checkmark),
## The \checkmark code works through the latex package amssymb (\usepackage{amssymb})
## In Tables 1, A3, A4, A5 all except the first model include temporal FEs
## In Tables A1, A2, A6, A7  all models include temporal FEs

source("10-main-analysis.R")
## input: analysis-data.Rdata - see Codebook.pdf for full description of all variables
## output: Tables 1, APPENDIX: A2, A3, A4, A5, A6, A7;
## output: Figures 3, 4, APPENDIX: A3

### APPENDIX: Tables A1, Figures A1, A2, A9, A10, A11

source("11-count-analysis.R")
## input: analysis-data.Rdata - see Codebook.pdf for full description of all variables
## output: APPENDIX: Tables A1, Figures A1, A2


source("12-make_oosperformance_table.R")
## input:
## xgboost-performance.Rdata - xgboost performance metrics
## classified_data-xgboost.RData
## output:
## Tables A9, A10, A11
