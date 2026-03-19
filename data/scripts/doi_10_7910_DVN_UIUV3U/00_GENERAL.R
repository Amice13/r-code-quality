# *****************************************************************
# OVERVIEW ####
# *****************************************************************
# 00_GENDERAL.R
# Replication Code for:
# Urban-Rural Differences in Non-Voting Political Behaviors
# Political Research Quarterly, 2022
# Authors: 
#   Jennifer Lin: jenniferlin2025@u.northwestern.edu
#   Kristin Lunz Trujillo: ktrujillo@hks.harvard.edu
# Created On: 2021 08 11

# ***The following script is intended to be an overview of all
#   the script files present in thsi directory and is intended for 
#   use with quick results. For more in-depth code explainations,
#   consult the respective code files.***

# *****************************************************************
# PACKAGES AND FUNCTIONS ####
# *****************************************************************

# Read in functions file for all the packages and 
# functions for the analyses with the CCES Data
source("Functions.R")

# Load Auxillary Data -- RUCA and RUCC Codes
source("Load_RUCA.R")

# *****************************************************************
# CES 2020 ####
# *****************************************************************

# The following section details script files and the respective
#   figures and tables that it is designed to replicate. Details on
#   the specific dataset can be found in the _CLEAN.R file.

# Load and clean data
source("CES2020_CLEAN.R")

# Descriptive Statistics: The following file contains code
#   to replicate the following figures and tables:
#  1. Main Paper
#     A. Figure 1a and 1b
#     B. Figure 2a and 2b
#  2. Supplemental Appendix
#     A. C.1 Tables
#     B. Figure 1a and 1b
#     C. D Table 1
source("CES2020_DESCRIPTIVE.R")

# Regression Analysis: The following file contains code
#   to replicate the following figures and tables:
#  1. Main Paper
#     A. Figure 5
#     B. Figure 6
#  2. Supplemental Appendix
#     A. E Table 2
#     B. E Table 3
#     C. F Table 6
source("CES2020_REGRESSION.R")

# Clear environment if continuing on in this file.
rm(list = ls())

# *****************************************************************
# CCES 2018 ####
# *****************************************************************

# The following source commands are included in the event
#   that you are running the code from top to bottom.
#   The above remove code clears the workspace with 2020
#   analyses but since we will need the functions, we will
#   reload them again here.
source("Functions.R")
source("Load_RUCA.R")

# The following section details script files and the respective
#   figures and tables that it is designed to replicate. Details on
#   the specific dataset can be found in the _CLEAN.R file.

# Load and clean data
source("CCES2018_CLEAN.R")

# Descriptive Statistics: The following file contains code
#   to replicate the following figures and tables:
#  1. Main Paper
#     A. Figure 3a and 3b
#     B. Figure 4a and 4b
#  2. Supplemental Appendix
#     A. C.2 Tables
#     B. Figure 2a and 2b
#     C. D Table 1
source("CCES2018_DESCRIPTIVE.R")

# Regression Analysis: The following file contains code
#   to replicate the following figures and tables:
#  1. Main Paper
#     A. Figure 7
#     B. Figure 8
#  2. Supplemental Appendix
#     A. E Table 4
#     B. E Table 5
#     C. F Table 7
source("CCES2018_REGRESSION.R")

# Clear environment if continuing on in this file.
rm(list = ls())

# *****************************************************************
# PROTESTS BY COUNTY ####
# *****************************************************************

# This section uses data from ACLED. We provide a simplified 
#   version of the data for replication purposes, which includes
#   an event ID that is provided by ACLED and the census GEOID that
#   we appended ourselves. If you desire a more complex version of
#   the data, you may acquire it for free using the steps below.

# To get the original data -- 
#  1. Visit https://acleddata.com/#/dashboard
#  2. Register for an API Key: https://developer.acleddata.com/
#  3. Use the data export tool and apply filter for the data
#     A. Use: https://acleddata.com/data-export-tool/
#     B. Restrict date ranges from 1/1 to 12/31 for year of 
#        interest
#     C. Keep all Filters except
#        I. Restrict Region to NORTH AMERICA
#        II. Restrict Country to UNITED STATES

# Load in the functions for the ACLED componet of the analyses
source("ACLED_FUNCTIONS.R")

# Included again for reference. If environment is cleared from 
#   above, load again.
source("Load_RUCA.R")

# The code in the following script generates Figure 9a
#   in the main paper
source("ACLED_MAP2020.R")

# The code in the following script generates Figure 9b
#   in the main paper
source("ACLED_MAP2021.R")

# The code in the following script generates the map
#   in Supplemental Appendix G
source("RUCC_MAP.R")

