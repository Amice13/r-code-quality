####################################
# Script Name: Malawi and Zambia Census Demographics
#
# Purpose: 
#
# Author: Erica Ann Metheney
#
# Contact: data@gld.gu.se
#
# Notes: 
#        1) Change the working directory on line 19 to point to the Replication Materials folder
#
#
#
######################################

# SET WORKING DIRECTORY

setwd("C:/Users/xmeter/University of Gothenburg/GLD Projects - Documents/Papers/Stereotypes Paper/")

# DATA IMPORT
MA <- read.csv("Replication Materials/Data/malawi2008_v2.csv")
ZA <- read.csv("Replication Materials/Data/zambia2010_v2.csv")

# DEMOGRAPHIC VARIABLES - gender, age, education

#...Gender
round(prop.table(table(MA$SEX)),digits = 2)
round(prop.table(table(ZA$SEX)),digits = 2)

#...Age
MA$AGE[MA$AGE == 999]<- NA
MA$AGE[MA$AGE < 18]<- NA
MA$AGE_bin = cut(MA$AGE,breaks = c(18,35,55,98),right = FALSE)
round(prop.table(table(MA$AGE_bin)),digits = 2)

ZA$AGE[ZA$AGE == 999]<- NA
ZA$AGE[ZA$AGE < 18]<- NA
ZA$AGE_bin = cut(ZA$AGE,breaks = c(18,35,55,98),right = FALSE)
round(prop.table(table(ZA$AGE_bin)),digits = 2)

#...Education
MA$EDATTAIN[MA$EDATTAIN==0]<- NA
round(prop.table(table(MA$EDATTAIN)),digits = 4)

ZA$EDATTAIN[ZA$EDATTAIN==0]<- NA
round(prop.table(table(ZA$EDATTAIN)),digits = 4)
