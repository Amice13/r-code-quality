# *****************************************************************
# OVERVIEW ####
# *****************************************************************
# Load_RUCA.R
# Loading Auxillary Data 
# RUCA Codes
# Jennifer Lin and Kristin Lunz Trujillo
# Created On: 2021 08 11

# *** This script includes directions for loading the Rural Urban 
#   codes used in this paper. See the Aux_Data/ folder for 
#   information on files used. *** 

# *****************************************************************
# RUCA ####
# *****************************************************************

# Load the RUCA dataset
RUCA <- read.csv("Aux_Data/RUCA.csv")

# Fix the RUCA data so that the ZIP code matche the standard
#   5 digit layout and that RUCA is a facotr variable.
RUCA <- RUCA %>% 
  mutate(
    RUCA2 = as.factor(RUCA2.0),
    zipcode = sprintf("%05d", ZIPA)
  )


# *****************************************************************
# RUCC ####
# *****************************************************************

# We are loading the RUCC data from source. If the URL does not 
#   work, load the ruralurbancodes2013.xls file in the Aux_Data/
#   folder.

# Source: US ERS

url <- "https://www.ers.usda.gov/webdocs/DataFiles/"
spec <- "53251/ruralurbancodes2013.xls?v=3013.7"
rucc <- paste0(url, spec)
rucc <- rio::import(rucc)
