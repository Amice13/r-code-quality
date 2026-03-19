# ReadMe ----------------------------------------------------------------------------
# Project - Covid19 WB info campaign
# Author  - XY
# Purpose - Master File

# Setup packages --------------------------------------------------------------------

rm(list = ls())
list.of.packages <- c("tidyverse","estimatr","fixest","broom","modelsummary",
                      "DescTools","ggdist","haven","multcomp","scales","patchwork",
                      "texreg","modelsummary","kableExtra","webshot","pastecs",
                      "car","janitor")

new.packages <- list.of.packages[!(list.of.packages %in% installed.packages()[,"Package"])]
if(length(new.packages)) install.packages(new.packages)

# Setup paths -----------------------------------------------------------------------

current_user = Sys.getenv("USERNAME")
main_folder = "Dropbox/West Bengal Information Campaign/AER_I/for_submission"
DRIVE = paste("/Users", current_user, main_folder, sep="/")
R_SCRPITS = paste(DRIVE,"code/2_R_files",sep="/")

# -----------------------------------------------------------------------------------
# Run all these R files in the correct sequence as they are ordered -----------------

# Creates Figure 3 for the main paper and supplementary table A8 and A9 in the Online Appendix
source(paste(R_SCRPITS,"01_GP_figures_effects_by_content.R",sep="/"))

# Creates supplementary table A10 in the Online Appendix
source(paste(R_SCRPITS,"02_GP_tables_effects_by_content.R",sep="/"))

# Creates supplementary table A21 in the Online Appendix
source(paste(R_SCRPITS,"03_GP_Attrition_TableA21.R",sep="/"))

# Creates supplementary table A22 in the Online Appendix
source(paste(R_SCRPITS,"04_GP_Balance_TableA22.R",sep="/"))

# Process Facebook mobility data
source(paste(R_SCRPITS,"05_facebook_setup.R",sep="/"))




