################################################################################
# Author: Pietryka
# Creation Date:  2018-03-26
# Purpose: Subset anes restricted data to case id + psu, save as csv
# Data Input:
#  - ANES-restricted/ANES_RDA_2012TimeSeriesGeocodes.xlsx
#  - ANES-restricted/ANES_RDA_2016TimeSeriesGeocodes.xlsx
# Data Output:
#  - Data/Source/anes_rds_2012.csv
#  - Data/Source/anes_rds_2016.csv
# Contact: mpietryka@fsu.edu
# NOTES: The 2016 public data did not provide PSUs so we needed to access the
#        restricted data file. We securly deleted the input data after this file
#        was run (as specified in our IRB protocol)
################################################################################


# LOAD PACKAGES
library(readxl)
library(readr)
library(dplyr)

# READ IN DATA
raw_12 <- read_excel(
  "C:/Users/mtp/Downloads/ANES-restricted/ANES_RDA_2012TimeSeriesGeocodes.xlsx"
  )

raw_16 <- read_excel(
  "C:/Users/mtp/Downloads/ANES-restricted/ANES_RDA_2016TimeSeriesGeocodes.xlsx"
)

# SUBSET
sub_12 <-  select(raw_12, case_id = Caseid, PSUcode)
sub_16 <-  select(raw_16, case_id = CaseID, PSUcode)

# SAVE AS CSVs
write_csv(sub_12, "D:/gdrive/Projects/AP - ANES Preregistration/Work/Data/Source/anes_rds_2012.csv")
write_csv(sub_16, "D:/gdrive/Projects/AP - ANES Preregistration/Work/Data/Source/anes_rds_2016.csv")
