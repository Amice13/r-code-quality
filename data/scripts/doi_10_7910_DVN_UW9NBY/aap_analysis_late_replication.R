#-------------------------------------------------------------------------------
# Please uncomment these lines such that you have all the packages needed in the following scripts
#-------------------------------------------------------------------------------
# Install required packages 
# install.packages("remotes")
# library(remotes)
# install_version("reshape2", version = "1.4.4");install_version("dplyr", version = "1.1.4")
# install_version("haven", version = "2.5.4");install_version("stargazer", version = "5.2.3")
# install_version("AER", version = "1.2-12");install_version("sandwich", version = "3.1-0")
# install_version("parallel", version = "4.3.3");install_version("randomizr", version = "1.0.0")
# install_version("ggplot2", version = "3.4.4");install_version("xtable", version = "1.8-4")
# install_version("lfe", version = "2.9-0");install_version("estimatr", version = "1.0.2")
# install_version("texreg", version = "1.39.3");install_version("car", version = "3.1-2")
# install_version("sjmisc", version = "2.8.9");install_version("MASS", version = "7.3-60.0.1")
# install_version("doSNOW", version = "1.0.20");install_version("doRNG", version = "1.8.6")
# install_version("data.table", version = "1.15.0"); install_version("tibble", version = "3.2.1")
# install_version("tidyr", version = "1.3.0")


# Set working directory
wd = ""
setwd( wd )
# set where is located your libraries
# generally you will need to uncomment the next line to set where the libraries are located
# lib = .libPaths()[1]
# if it does not work please write where is located your packages
# it should seems like "/home/ar8787/R/x86_64-redhat-linux-gnu-library/4.3"
lib = ""

# Run 'data_merge.R'
source("data_merge.R")

# Run 'var_create.R'
source("var_create.R")

# # Run 'aap_analysis_late.R'
source("aap_analysis_late.R")
 
# # Run 'aap_analysis_late_excluded.R'
source("aap_analysis_late_excluded.R")
 
# # Run 'aap_analysis_late_skills.R'
source("aap_analysis_late_skills.R")

# Run 'aap_analysis_late_recall_survey.R'
source("aap_analysis_late_recall_survey.R")

# Run 'aap_analysis_interaction.R'
source("aap_analysis_interaction.R")

# Run 'aap_analysis_late_coef_plot.R'
source("aap_analysis_late_coef_plot.R")

# Run 'aap_analysis_simulation.R'
source("aap_analysis_simulation.R")

# Run 'compliance_calc.R'
source("compliance_calc.R")

# Run 'DescriptiveStatsPlot.R'
source("DescriptiveStatsPlot.R")
