#### Replication Code for "Who Paid Los Angeles' Minimum Wage? A Side-by-Side Minimum Wage Experiment in Los Angeles County"
# Journal of Human Resources
# For questions, please contact Chris Esposito at christopher.esposito@anderson.ucla.edu


# This code reproduces all figures and tables that use our price survey dataset in the article. 
# Because the code for many of the figures and tables are near-duplicates (i.e. the same code is used to separately produce results for restaurants in high-income and low-income census tracts), in numerous locations you will need to change the data subsets to arrive at the near-duplicate versions of each table
# The comments in the code indicate each instance where the subset needs to be changed (usually this involves flipping a great-than sign to a less-than sign)

require(data.table)
require(doBy)
require(plm)
require(dplyr)
require(lfe)
require(zoo)
require(ggplot2)
require(reshape2)
require(fixest)
require(pwr)

####################################################################################
#################################### Prepare data
####################################################################################
### Set your working directory to where you have the files stored
setwd("/Users/cresposito/Library/CloudStorage/Dropbox/Wage Research/Revisions/ReplicationPackage")

### Read in all data
# The main price survey dataset
DATA_A <- as.data.frame(fread("PriceSurvey.csv"))
# Data for Rounds 0 and 1, used for pre-trends
DATA_0_1 <- fread("DATA_0_1.csv")
# BLS data on CPI and PPI
BLS <- fread("BLS_CPI.csv")
# Attributes on distances to closed competitors
ATTRIBUTES_NEAR_CLOSURES <- fread("ATTRIBUTES_NEAR_CLOSURES.csv") 
# Features at the aggregate level of each wage strata
STRATA <- fread("STRATA.csv")
# Year-specific median household income data for census tracts
MHHI_TRACT_YEAR <- fread(file="MHHI_TRACT_YEAR.csv")
# The full population of LA County Restaurants
ALL_RESTAURANTS <- fread("RESTAURANT_POPULATION.csv")

# Convert the main data to a pdata frame. PDATA is a panel data frame format used by the PLM package. It allows for time indexing  based on item unique IDS (RESITEM)
DATA_A <- pdata.frame(DATA_A, index=c("RESITEM", "ROUND"))


################################################################################################################################
################################################################################################################################
################################################## Main Text tables and figures ################################################
################################################################################################################################
################################################################################################################################

############### Table 1 
# Get number of observations by month and starata for ROUND 1
# The ALLMATCH subset makes sure we are using the balanced panel
summaryBy(HASPRICE  ~ CITY_WAGE*BORDER*YEAR*MONTH, FUN="sum", 
          data=DATA_A[which(DATA_A$ROUND %in% 1 & is.na(DATA_A$IPRICE)==F & DATA_A$ALLMATCH==1),], na.rm=T)
# Get number of observations by month and strata for ROUND 7
summaryBy(HASPRICE  ~ CITY_WAGE*BORDER*YEAR*MONTH, FUN="sum", 
          data=DATA_A[which(DATA_A$ROUND %in% 7  & is.na(DATA_A$IPRICE)==F & DATA_A$ALLMATCH==1),], na.rm=T)

############### Table 2
# Mean price changes by strata and market income segment
# HI_MED is used to subset above-and-below median household income census tracts, based on the distribution of census tract
## median household income in 2015 for item menu items with ALLMATCH==1. The median value for  59807
median(DATA_A$HI_MED[which(DATA_A$ALLMATCH==1)], na.rm=T)
# The variable RPRICE_MED_R1 can be used to subset the high base price restaurants
# The median RPRICE_MED_R1 value was $6.245. This condition can be set to $0 to not perform a subset
summaryBy(PD6PRICE ~ CITY_WAGE*BORDER, FUN="mean", na.rm=T, data=DATA_A[which(DATA_A$ROUND == 7 & DATA_A$ALLMATCH==1 &
                                                                                DATA_A$HI_MED > 0 & DATA_A$RPRICE_MED_R1 > 0),])
# Standard errors for the first column of the table, taken as the standard deviation divided by the square root of n
with(DATA_A[which(DATA_A$ROUND == 7 & DATA_A$ALLMATCH==T & DATA_A$HI_MED >= 0  & DATA_A$CITY_WAGE== 0 & DATA_A$BORDER==0 & DATA_A$RPRICE_MED_R1 > 0),],
     sd(PD6PRICE) / length(PD6PRICE)^.5)
# Standard errors of the differences, taken as the square of the sum of the variances divided by n
with(DATA_A[which(DATA_A$ROUND == 7 & DATA_A$ALLMATCH==T & DATA_A$HI_MED >= 0  & DATA_A$CITY_WAGE== 0 & DATA_A$BORDER==0 & DATA_A$RPRICE_MED_R1 > 0),],
     sd(PD6PRICE)^2 / (length(PD6PRICE))) +
  with(DATA_A[which(DATA_A$ROUND == 7 & DATA_A$ALLMATCH==T & DATA_A$HI_MED >= 0  & DATA_A$CITY_WAGE== 1 & DATA_A$BORDER==0 & DATA_A$RPRICE_MED_R1 > 0),],
       sd(PD6PRICE)^2 / (length(PD6PRICE)))
# Compute mean min wage change by strata for table
with(DATA_A[which(DATA_A$ROUND == 7  & DATA_A$CITY_WAGE== 0 & DATA_A$BORDER==0 & DATA_A$ALLMATCH==1 & DATA_A$HI_MED > 0 ),], 
     mean(PD6MINWAGE, na.rm=T))
# Compute number of observations by strata and income segment
with(DATA_A[which(DATA_A$ROUND == 7 & DATA_A$CITY_WAGE== 1 & DATA_A$BORDER==0 & DATA_A$ALLMATCH==1 & DATA_A$HI_MED > 0 & DATA_A$RPRICE_MED_R1 > 0),], 
     length(PD6PRICE))


############### Table 3
# Mean price change by strata with number of cross-border competitors
# First do low-Income Census Tracts (Row 1 of the table)
# The first set of codes computes the mean price changes
# State Wage Low-Income Census Tracts,  Not Exposed to x-border competition
with(DATA_A[which(DATA_A$ROUND == 7 & DATA_A$CITY_WAGE== 0 & DATA_A$COMP_CW_LI==0 & DATA_A$ALLMATCH==1 & DATA_A$HI_MED < 59807),], 
     mean(PD6PRICE, na.rm=T))
# State Wage Low-Income Census Tracts,  Exposed to x-border competition
with(DATA_A[which(DATA_A$ROUND == 7 & DATA_A$CITY_WAGE== 0 & DATA_A$COMP_CW_LI > 1 & DATA_A$ALLMATCH==1 & DATA_A$HI_MED < 59807),], 
     mean(PD6PRICE, na.rm=T))
# City Wage Low-Income Census Tracts,  Exposed to x-border competition
with(DATA_A[which(DATA_A$ROUND == 7 & DATA_A$CITY_WAGE== 1 & DATA_A$COMP_SW_LI > 1 & DATA_A$ALLMATCH==1 & DATA_A$HI_MED < 59807),], 
     mean(PD6PRICE, na.rm=T))
# City Wage Low-Income Census Tracts,  Not Exposed to x-border competition
with(DATA_A[which(DATA_A$ROUND == 7 & DATA_A$CITY_WAGE== 1 & DATA_A$COMP_SW_LI == 0 & DATA_A$ALLMATCH==1 & DATA_A$HI_MED < 59807),], 
     mean(PD6PRICE, na.rm=T))
## High-Income Census Tracts (Row 2)
# State Wage Low-Income Census Tracts,  Not Exposed to x-border competition
with(DATA_A[which(DATA_A$ROUND == 7 & DATA_A$CITY_WAGE== 0 & DATA_A$COMP_CW_HI==0 & DATA_A$ALLMATCH==1 & DATA_A$HI_MED >= 59807),], 
     mean(PD6PRICE, na.rm=T))
# State Wage Low-Income Census Tracts,  Exposed to x-border competition
with(DATA_A[which(DATA_A$ROUND == 7 & DATA_A$CITY_WAGE== 0 & DATA_A$COMP_CW_HI > 1 & DATA_A$ALLMATCH==1 & DATA_A$HI_MED >=  59807),], 
     mean(PD6PRICE, na.rm=T))
# City Wage Low-Income Census Tracts,  Exposed to x-border competition
with(DATA_A[which(DATA_A$ROUND == 7 & DATA_A$CITY_WAGE== 1 & DATA_A$COMP_SW_HI > 1 & DATA_A$ALLMATCH==1 & DATA_A$HI_MED >= 59807),], 
     mean(PD6PRICE, na.rm=T))
# City Wage Low-Income Census Tracts,  Not Exposed to x-border competition
with(DATA_A[which(DATA_A$ROUND == 7 & DATA_A$CITY_WAGE== 1 & DATA_A$COMP_SW_HI == 0 & DATA_A$ALLMATCH==1 & DATA_A$HI_MED >= 59807),], 
     mean(PD6PRICE, na.rm=T))
######## The following gives NOBS for each group
# State Wage Low-Income Census Tracts,  Not Exposed to x-border competition
with(DATA_A[which(DATA_A$ROUND == 7 & DATA_A$CITY_WAGE== 0 & DATA_A$COMP_CW_LI==0 & DATA_A$ALLMATCH==1 & DATA_A$HI_MED < 59807),], 
     length(PD6PRICE))
# State Wage Low-Income Census Tracts,  Exposed to x-border competition
with(DATA_A[which(DATA_A$ROUND == 7 & DATA_A$CITY_WAGE== 0 & DATA_A$COMP_CW_LI > 1 & DATA_A$ALLMATCH==1 & DATA_A$HI_MED < 59807),], 
     length(PD6PRICE))
# City Wage Low-Income Census Tracts,  Exposed to x-border competition
with(DATA_A[which(DATA_A$ROUND == 7 & DATA_A$CITY_WAGE== 1 & DATA_A$COMP_SW_LI > 1 & DATA_A$ALLMATCH==1 & DATA_A$HI_MED < 59807),], 
     length(PD6PRICE))
# City Wage Low-Income Census Tracts,  Not Exposed to x-border competition
with(DATA_A[which(DATA_A$ROUND == 7 & DATA_A$CITY_WAGE== 1 & DATA_A$COMP_SW_LI == 0 & DATA_A$ALLMATCH==1 & DATA_A$HI_MED < 59807),], 
     length(PD6PRICE))
## High-Income Census Tracts (Row 2)
# State Wage High-Income Census Tracts,  Not Exposed to x-border competition
with(DATA_A[which(DATA_A$ROUND == 7 & DATA_A$CITY_WAGE== 0 & DATA_A$COMP_CW_HI==0 & DATA_A$ALLMATCH==1 & DATA_A$HI_MED >= 59807),], 
     length(PD6PRICE))
# State Wage High-Income Census Tracts,  Exposed to x-border competition
with(DATA_A[which(DATA_A$ROUND == 7 & DATA_A$CITY_WAGE== 0 & DATA_A$COMP_CW_HI > 1 & DATA_A$ALLMATCH==1 & DATA_A$HI_MED >=  59807),], 
     length(PD6PRICE))
# City Wage High-Income Census Tracts,  Exposed to x-border competition
with(DATA_A[which(DATA_A$ROUND == 7 & DATA_A$CITY_WAGE== 1 & DATA_A$COMP_SW_HI > 1 & DATA_A$ALLMATCH==1 & DATA_A$HI_MED >= 59807),], 
     length(PD6PRICE))
# City Wage High-Income Census Tracts,  Not Exposed to x-border competition
with(DATA_A[which(DATA_A$ROUND == 7 & DATA_A$CITY_WAGE== 1 & DATA_A$COMP_SW_HI == 0 & DATA_A$ALLMATCH==1 & DATA_A$HI_MED >= 59807),], 
     length(PD6PRICE))
########  These final codes give standard errors for for each difference
# State Wage Low-Income Census Tracts, exposed to x-border competition
((with(DATA_A[which(DATA_A$ROUND == 7 & DATA_A$CITY_WAGE== 0 & DATA_A$COMP_CW_LI==0 & DATA_A$ALLMATCH==1 & DATA_A$HI_MED < 59807),], 
       sd(PD6PRICE))^2 / 
    with(DATA_A[which(DATA_A$ROUND == 7 & DATA_A$CITY_WAGE== 0 & DATA_A$COMP_CW_LI == 0 & DATA_A$ALLMATCH==1 & DATA_A$HI_MED < 59807),], 
         length(PD6PRICE))) +
    (with(DATA_A[which(DATA_A$ROUND == 7 & DATA_A$CITY_WAGE== 0 & DATA_A$COMP_CW_LI>1 & DATA_A$ALLMATCH==1 & DATA_A$HI_MED < 59807),], 
          sd(PD6PRICE))^2 / 
       with(DATA_A[which(DATA_A$ROUND == 7 & DATA_A$CITY_WAGE== 0 & DATA_A$COMP_CW_LI > 1 & DATA_A$ALLMATCH==1 & DATA_A$HI_MED < 59807),], 
            length(PD6PRICE))))^.5
# City Wage Low-Income Census Tracts, exposed to x-border competition
((with(DATA_A[which(DATA_A$ROUND == 7 & DATA_A$CITY_WAGE== 0 & DATA_A$COMP_CW_LI==0 & DATA_A$ALLMATCH==1 & DATA_A$HI_MED < 59807),], 
       sd(PD6PRICE))^2 / 
    with(DATA_A[which(DATA_A$ROUND == 7 & DATA_A$CITY_WAGE== 0 & DATA_A$COMP_CW_LI == 0 & DATA_A$ALLMATCH==1 & DATA_A$HI_MED < 59807),], 
         length(PD6PRICE))) +
    (with(DATA_A[which(DATA_A$ROUND == 7 & DATA_A$CITY_WAGE== 1 & DATA_A$COMP_SW_LI>1 & DATA_A$ALLMATCH==1 & DATA_A$HI_MED < 59807),], 
          sd(PD6PRICE))^2 / 
       with(DATA_A[which(DATA_A$ROUND == 7 & DATA_A$CITY_WAGE== 1 & DATA_A$COMP_SW_LI > 1 & DATA_A$ALLMATCH==1 & DATA_A$HI_MED < 59807),], 
            length(PD6PRICE))))^.5
# City Wage Low-Income Census Tracts, not exposed to x-border competition
((with(DATA_A[which(DATA_A$ROUND == 7 & DATA_A$CITY_WAGE== 0 & DATA_A$COMP_CW_LI==0 & DATA_A$ALLMATCH==1 & DATA_A$HI_MED < 59807),], 
       sd(PD6PRICE))^2 / 
    with(DATA_A[which(DATA_A$ROUND == 7 & DATA_A$CITY_WAGE== 0 & DATA_A$COMP_CW_LI == 0 & DATA_A$ALLMATCH==1 & DATA_A$HI_MED < 59807),], 
         length(PD6PRICE))) +
    (with(DATA_A[which(DATA_A$ROUND == 7 & DATA_A$CITY_WAGE== 1 & DATA_A$COMP_SW_LI==0 & DATA_A$ALLMATCH==1 & DATA_A$HI_MED < 59807),], 
          sd(PD6PRICE))^2 / 
       with(DATA_A[which(DATA_A$ROUND == 7 & DATA_A$CITY_WAGE== 1 & DATA_A$COMP_SW_LI==0 & DATA_A$ALLMATCH==1 & DATA_A$HI_MED < 59807),], 
            length(PD6PRICE))))^.5
########  High-income census tracts
# State Wage High-Income Census Tracts, exposed to x-border competition
((with(DATA_A[which(DATA_A$ROUND == 7 & DATA_A$CITY_WAGE== 0 & DATA_A$COMP_CW_HI==0 & DATA_A$ALLMATCH==1 & DATA_A$HI_MED >= 59807),], 
       sd(PD6PRICE))^2 / 
    with(DATA_A[which(DATA_A$ROUND == 7 & DATA_A$CITY_WAGE== 0 & DATA_A$COMP_CW_HI == 0 & DATA_A$ALLMATCH==1 & DATA_A$HI_MED >= 59807),], 
         length(PD6PRICE))) +
    (with(DATA_A[which(DATA_A$ROUND == 7 & DATA_A$CITY_WAGE== 0 & DATA_A$COMP_CW_HI>1 & DATA_A$ALLMATCH==1 & DATA_A$HI_MED >= 59807),], 
          sd(PD6PRICE))^2 / 
       with(DATA_A[which(DATA_A$ROUND == 7 & DATA_A$CITY_WAGE== 0 & DATA_A$COMP_CW_HI > 1 & DATA_A$ALLMATCH==1 & DATA_A$HI_MED >= 59807),], 
            length(PD6PRICE))))^.5
# City Wage High-Income Census Tracts, exposed to x-border competition
((with(DATA_A[which(DATA_A$ROUND == 7 & DATA_A$CITY_WAGE== 0 & DATA_A$COMP_CW_HI==0 & DATA_A$ALLMATCH==1 & DATA_A$HI_MED >= 59807),], 
       sd(PD6PRICE))^2 / 
    with(DATA_A[which(DATA_A$ROUND == 7 & DATA_A$CITY_WAGE== 0 & DATA_A$COMP_CW_HI == 0 & DATA_A$ALLMATCH==1 & DATA_A$HI_MED >= 59807),], 
         length(PD6PRICE))) +
    (with(DATA_A[which(DATA_A$ROUND == 7 & DATA_A$CITY_WAGE== 1 & DATA_A$COMP_SW_HI > 1 & DATA_A$ALLMATCH==1 & DATA_A$HI_MED >= 59807),], 
          sd(PD6PRICE))^2 / 
       with(DATA_A[which(DATA_A$ROUND == 7 & DATA_A$CITY_WAGE== 1 & DATA_A$COMP_SW_HI > 1 & DATA_A$ALLMATCH==1 & DATA_A$HI_MED >= 59807),], 
            length(PD6PRICE))))^.5
# City Wage High-Income Census Tracts, not exposed to x-border competition
((with(DATA_A[which(DATA_A$ROUND == 7 & DATA_A$CITY_WAGE== 0 & DATA_A$COMP_CW_HI==0 & DATA_A$ALLMATCH==1 & DATA_A$HI_MED >= 59807),], 
       sd(PD6PRICE))^2 / 
    with(DATA_A[which(DATA_A$ROUND == 7 & DATA_A$CITY_WAGE== 0 & DATA_A$COMP_CW_HI == 0 & DATA_A$ALLMATCH==1 & DATA_A$HI_MED >= 59807),], 
         length(PD6PRICE))) +
    (with(DATA_A[which(DATA_A$ROUND == 7 & DATA_A$CITY_WAGE== 1 & DATA_A$COMP_SW_HI==0 & DATA_A$ALLMATCH==1 & DATA_A$HI_MED >= 59807),], 
          sd(PD6PRICE))^2 / 
       with(DATA_A[which(DATA_A$ROUND == 7 & DATA_A$CITY_WAGE== 1 & DATA_A$COMP_SW_HI==0 & DATA_A$ALLMATCH==1 & DATA_A$HI_MED >= 59807),], 
            length(PD6PRICE))))^.5


##### Table 4 #####
# Regression results
# The regression model requires the calculation of two new variables: the change in the alternative minimum wage, and the spillover variable
# First, we find that the mean change in the alternative minimum wage for State Wage restaurants is 52%
mean(DATA_A$PD6MINWAGE[which(DATA_A$CITY_WAGE==0)], na.rm=T)
# Both variables are defined in the paper's text. They are computed for the dataset below
# Compute alternative (ALT) minimum wage change
DATA_A$PD6MINWAGE_ALT <- NA
DATA_A$PD6MINWAGE_ALT[which(DATA_A$CITY_WAGE==0)] <- 52.06669
DATA_A$PD6MINWAGE_ALT[which(DATA_A$CITY_WAGE==1)] <- 29.63002
# Create spillover variable
DATA_A$SPILLOVER <- ((1-DATA_A$BORDER)*DATA_A$PD6MINWAGE) + 
  (DATA_A$BORDER*DATA_A$PD6MINWAGE_ALT)
# Low-income census tracts
summary(feols(PD6PRICE ~  PD6MINWAGE + SPILLOVER + PD6PPI + POP_DENSITY + RESTAURANT_DENSITY + RESTAURANTS_PER_CAPITA + SHARE_FULL_SERVICE + PD6MHHI
             | HGRADE + BM + ICLASS + MONTH, data=DATA_A[which(DATA_A$ROUND %in% 7 & DATA_A$ALLMATCH==1 & DATA_A$HI_MED < 59807 ),]), cluster="RID")
# High-income census tracts
summary(feols(PD6PRICE ~  PD6MINWAGE + SPILLOVER + PD6PPI + POP_DENSITY + RESTAURANT_DENSITY + RESTAURANTS_PER_CAPITA + SHARE_FULL_SERVICE + PD6MHHI
             | HGRADE + BM + ICLASS + MONTH , data=DATA_A[which(DATA_A$ROUND %in% 7 & DATA_A$ALLMATCH==1 & DATA_A$HI_MED > 59807 ),]), cluster="RID")


##### Table 5 ######
# To compute Table 5, we manually input the values of theta and beta from the regressions above
# we then multiply them by the mean difference in the minimum wage 
# First, restaurants in low-income census tracts
BETA <- 0.0411
THETA <- -0.0364
# State Border
THETA * (52.06669-29.63002)
# City Border
BETA * (52.06669-29.63002)
# City Non-Border
(THETA + BETA) * (52.06669-29.63002)
# Second, restaurants in high-income census tracts
BETA <- 0.0414
THETA <- 0.234
# State Border
THETA * (52.06669-29.63002)
# City Border
BETA * (52.06669-29.63002)
# City Non-Border
(THETA + BETA) * (52.06669-29.63002)


########## Table 6 (pretrends)
DATA_0_1 <- DATA_0_1[which(DATA_0_1$ROUND %in% c(0,1)),]
DATA_0_1$RESITEM <- paste(DATA_0_1$RID, DATA_0_1$ICLASS, sep="_")
DATA_0_1 <- pdata.frame(DATA_0_1, index=c("RESITEM", "ROUND"))
# Prepare variables
DATA_0_1$YEAR <- as.numeric(DATA_0_1$YEAR)
DATA_0_1$MONTH <- as.numeric(DATA_0_1$MONTH)
DATA_0_1$IPRICE <- as.numeric(DATA_0_1$IPRICE)
DATA_0_1$LAGIPRICE <- plm::lag(DATA_0_1$IPRICE, k=1)
DATA_0_1$LAGYEAR <- plm::lag(DATA_0_1$YEAR, k=1)
DATA_0_1$LAGMONTH <- plm::lag(DATA_0_1$MONTH, k=1)
DATA_0_1$DYEARS <- DATA_0_1$YEAR - DATA_0_1$LAGYEAR
DATA_0_1$DMONTHS <- DATA_0_1$MONTH - DATA_0_1$LAGMONTH
DATA_0_1$DTIME <- (DATA_0_1$DYEARS*12) + DATA_0_1$DMONTHS
DATA_0_1$HIGH_INCOME <- 0
DATA_0_1$HIGH_INCOME[which(DATA_0_1$HI_MED > 59807)] <- 1 # 59807 is the median value from the full dataset; we use it here for consistency
DATA_0_1$PDIPRICE <- (DATA_0_1$IPRICE - DATA_0_1$LAGIPRICE) / DATA_0_1$LAGIPRICE # Compute percent change in prices
DATA_0_1$STRATA <- paste(DATA_0_1$CITY_WAGE, DATA_0_1$BORDER, sep="_")
DATA_0_1$STRATA[which(DATA_0_1$STRATA=="0_0")] <- "State Non-Border"
DATA_0_1$STRATA[which(DATA_0_1$STRATA=="0_1")] <- "State Border"
DATA_0_1$STRATA[which(DATA_0_1$STRATA=="1_1")] <- "City Border"
DATA_0_1$STRATA[which(DATA_0_1$STRATA=="1_0")] <- "City Non-Border"
DATA_0_1$STRATA <- factor(DATA_0_1$STRATA, levels=c("State Non-Border","State Border","City Border","City Non-Border"))
# Subset just the observations for which we observe a change in price and a change in time (omits observations with no time stamp)
DATA_0_1 <- DATA_0_1[which(is.na(DATA_0_1$PDIPRICE)==F & DATA_0_1$DTIME > 0),]
# Regression models (the first is for no fixed effects; the second has observation month and year FEs). Change the HIGH_INCOME==1 subset for market segment
summary(feols(I(PDIPRICE/(DTIME/12)) ~ STRATA | 0, data=DATA_0_1[which(DATA_0_1$HIGH_INCOME == 1 ),]), cluster="RID") 
summary(feols(I(PDIPRICE/(DTIME/12)) ~ STRATA | LAGMONTH + LAGYEAR + MONTH + YEAR, data=DATA_0_1[which(DATA_0_1$HIGH_INCOME == 1 ),]), cluster="RID") 


########## Table 7
###### Note that we will also produce Table A11 in this same block of code, because it will use information from the Table 8 computation
###### Note that this block of code also produces Tables A15 and A16 by changing the border distance threshold, as set below
DATA_A <- DATA_A %>% group_by(RID,ROUND) %>% mutate(ALIVE = sum(HASPRICE)) %>% as.data.frame()
DATA_A$ALIVE[DATA_A$ALIVE > 0] <- 1
DATA_A$DEAD <- 0
DATA_A$DEAD[DATA_A$ALIVE !=1 ] <- 1
# Now we aggregate up to the restuarnt*round level by ignoring individual items at same restaurant
RID_ROUND <- DATA_A[which(DATA_A$ICLASS==11),]
RID_ROUND$ICLASS <- NULL
RID_ROUND$INAME <- NULL
RID_ROUND$IPRICE <- NULL
RID_ROUND$RESITEM <- NULL
RID_ROUND$HASPRICE <- NULL
RID_ROUND <- pdata.frame(RID_ROUND, index=c("RID", "ROUND"))
# Now we make ALWAYSALIVE, a variable that recods whether the restaurant is open all 7 rounds
RID_ROUND <- RID_ROUND %>% group_by(RID) %>% mutate(ALWAYSALIVE = sum(ALIVE, na.rm=T))
RID_ROUND$ALWAYSALIVE[which(RID_ROUND$ALWAYSALIVE < 7)] <- 0
RID_ROUND$ALWAYSALIVE[which(RID_ROUND$ALWAYSALIVE == 7)] <- 1
# Next we construct a variable WASDEAD, which records if a restaurant ever previously closed
# Such closures may occur when a restaurant temporarially closes and then reopens
RID_ROUND <- pdata.frame(RID_ROUND, index=c("RID", "ROUND"))
RID_ROUND$DEAD1 <- plm::lag(RID_ROUND$DEAD, k=1)
RID_ROUND$DEAD2 <- plm::lag(RID_ROUND$DEAD, k=2)
RID_ROUND$DEAD3 <- plm::lag(RID_ROUND$DEAD, k=3)
RID_ROUND$DEAD4 <- plm::lag(RID_ROUND$DEAD, k=4)
RID_ROUND$DEAD5 <- plm::lag(RID_ROUND$DEAD, k=5)
RID_ROUND$DEAD6 <- plm::lag(RID_ROUND$DEAD, k=6)
RID_ROUND$DEAD7 <- plm::lag(RID_ROUND$DEAD, k=7)
# Next we identify whether a restaurant ever was temporarily closed (WASDEAD)
RID_ROUND <- RID_ROUND %>% rowwise() %>% mutate(WASDEAD = sum(DEAD1, DEAD2, DEAD3, DEAD4, DEAD5, DEAD6, DEAD7, na.rm=T))
# We only want to include restaurants first observed in the first round of observations (FIRST_ROUND_OF_OBS)
# Thus we compute the first round of observation of each restaurant
RID_ROUND <- RID_ROUND %>% group_by(RID) %>% mutate(FIRST_ROUND_OF_OBS = dplyr::first(ROUND)) %>% as.data.frame()
# Finally, we can create a vector of restaurant IDs that never close nor close and reopen
RIDS_THAT_NEVER_DIE <- as.character(unique(RID_ROUND$RID[which(RID_ROUND$ALWAYSALIVE == 1 & RID_ROUND$WASDEAD==0 &
                                                                 RID_ROUND$FIRST_ROUND_OF_OBS == 1 & RID_ROUND$HI_MED>0)]))
# With this vector calculated, we can now subset items at just the restaurants that never close
ITEM_DEATHS <- DATA_A[which(DATA_A$RID %in% RIDS_THAT_NEVER_DIE),]
# Take only the items that were observed in Round 1
ITEM_DEATHS <- ITEM_DEATHS %>% group_by(RESITEM) %>% mutate(HASPRICE_R1 = sum(HASPRICE[which(ROUND==1)])) %>% as.data.frame()
ITEM_DEATHS <- ITEM_DEATHS[which(ITEM_DEATHS$HASPRICE_R1 == 1),]
# Compute for each item whether its price is observed in each of the 7 rounds
ITEM_DEATHS <- ITEM_DEATHS %>% group_by(RESITEM) %>% mutate(ITEM_ALWAYS_ALIVE = sum(HASPRICE)) %>% as.data.frame()
ITEM_DEATHS$ITEM_ALWAYS_ALIVE[which(ITEM_DEATHS$ITEM_ALWAYS_ALIVE < 7)] <- 0
ITEM_DEATHS$ITEM_ALWAYS_ALIVE[which(ITEM_DEATHS$ITEM_ALWAYS_ALIVE > 0)] <- 1
ITEM_DEATHS <- ITEM_DEATHS[which(ITEM_DEATHS$ROUND==1),]
# For each restaurant, identify whether a single item dies at the restaurant
ITEM_DEATHS <- ITEM_DEATHS %>% group_by(RID) %>% mutate(RES_ALL_ITEMS_ALWAYS_ALIVE = min(ITEM_ALWAYS_ALIVE)) %>% as.data.frame()
RES_ITEM_DEATHS <- ITEM_DEATHS[which(ITEM_DEATHS$ICLASS == 11 & ITEM_DEATHS$HI_MED > 0),]
RES_ITEM_DEATHS$ITEMS_DIE <- 1- RES_ITEM_DEATHS$RES_ALL_ITEMS_ALWAYS_ALIVE
RES_ITEM_DEATHS$HIGH_INCOME <- NA
RES_ITEM_DEATHS$HIGH_INCOME[which(RES_ITEM_DEATHS$HI_MED >= 59807)] <- 1
RES_ITEM_DEATHS$HIGH_INCOME[which(RES_ITEM_DEATHS$HI_MED < 59807)] <- 0
########## Tables 15 ans 16: Establish border definition (change this for narrow/wide borders by setting 1600 to 1400 or 1800)
RES_ITEM_DEATHS$BORDER[which(RES_ITEM_DEATHS$BDIST <= 1600)] <- 1
RES_ITEM_DEATHS$BORDER[which(RES_ITEM_DEATHS$BDIST > 1600)] <- 0
#### First we summarize menu changes by strata, for ALL census tracts (both high and low income)
MENUCHANGES <- round(summaryBy(ITEMS_DIE ~ CITY_WAGE*BORDER, FUN="mean", data=RES_ITEM_DEATHS),3)
# Compute number of observations
MENUHCANGESOBS <- round(cbind(summaryBy(HASPRICE ~ CITY_WAGE*BORDER, FUN="sum", data=RES_ITEM_DEATHS)),3)
names(MENUHCANGESOBS) <- c("CITY_WAGE","BORDER","NOBS")
# SDs and Standard errors 
MENUHCANGESDs <- cbind(MENUCHANGES[,1:2], (MENUCHANGES[,3]*(1-MENUCHANGES[,3]))^.5)
MENUHCANGESEs <- cbind(MENUCHANGES[,1:2], ( ((MENUCHANGES[,3]*(1-MENUCHANGES[,3])) / MENUHCANGESOBS[,3])^.5))
names(MENUHCANGESDs) <- c("CITY_WAGE","BORDER","SD")
names(MENUHCANGESEs) <- c("CITY_WAGE","BORDER","SE")
# Means of differences
MENUCHANGES_MEANS_DIFFS <- (MENUCHANGES[2:4,3] - MENUCHANGES[1,3])
# SEs of differences
MENUCHANGE_SE_DIFFS <- (MENUHCANGESEs[1,3]^2 + MENUHCANGESEs[2:4,3]^2)^.5
################  Table 7, first row:
MENUHCANGESOBS
MENUHCANGESEs
MENUCHANGES_MEANS_DIFFS
MENUCHANGE_SE_DIFFS
################  Table A11 first row
SDs <- MENUHCANGESDs %>% select(SD) %>% unlist() %>% combn(m=2)
SDs <- SDs[, c(1,3,2)]
NOBs <- MENUHCANGESOBS %>% select(NOBS) %>% unlist() %>% combn(m=2)
NOBs <- NOBs[, c(1,3,2)]
SDs_POOLED <- ((((NOBs[1,]-1)*(SDs[1,]^2)) + ((NOBs[2,]-1)*(SDs[2,]^2))) / (NOBs[1,] + NOBs[2,] -2) )^.5
d <- c()
for(i in 1:3){
  d <- append(d, pwr.t2n.test(n1 = NOBs[1,i], n2=NOBs[2,i], d = NULL, sig.level = 0.05, power=.95)$d)
}
# Effect size (percentage points) required to achieve 95% power (reject null 95% of the time)
d * SDs_POOLED
#######
##### Now we summarize menu changes by strata AND income level
MENUCHANGES <- round(summaryBy(ITEMS_DIE ~ CITY_WAGE*BORDER*HIGH_INCOME, FUN="mean", data=RES_ITEM_DEATHS),3)
# Compute number of observations
MENUHCANGESOBS <- round(cbind(summaryBy(HASPRICE ~ CITY_WAGE*BORDER*HIGH_INCOME, FUN="sum", data=RES_ITEM_DEATHS)),3)
names(MENUHCANGESOBS) <- c("CITY_WAGE","BORDER","HIGH_INCOME","NOBS")
# Standard errors 
MENUHCANGESDs <- cbind(MENUCHANGES[,1:3], (MENUCHANGES[,4]*(1-MENUCHANGES[,4]))^.5)
MENUHCANGESEs <- cbind(MENUCHANGES[,1:3], ( ((MENUCHANGES[,4]*(1-MENUCHANGES[,4])) / MENUHCANGESOBS[,4])^.5))
names(MENUHCANGESDs) <- c("CITY_WAGE","BORDER","HIGH_INCOME","SD")
names(MENUHCANGESEs) <- c("CITY_WAGE","BORDER","HIGH_INCOME","SE")
# Means of differences
MENUCHANGE_LOW_INCOME_MEANS_DIFFS <- (MENUCHANGES[seq(3, 7, 2),4] -MENUCHANGES[1,4] )
MENUCHANGE_HIGH_INCOME_MEANS_DIFFS <- (MENUCHANGES[seq(4, 8, 2),4] -MENUCHANGES[2,4] )
# SEs of differences
MENUCHANGE_LOW_INCOME_SE_DIFFS <- (MENUHCANGESEs[1,4]^2 + MENUHCANGESEs[seq(3, 7, 2),4]^2)^.5
MENUCHANGE_HIGH_INCOME_SE_DIFFS <- (MENUHCANGESEs[2,4]^2 + MENUHCANGESEs[seq(4, 8, 2),4]^2)^.5
############################### Table 7, second and third rows
MENUHCANGESOBS
MENUHCANGESEs
MENUCHANGE_LOW_INCOME_MEANS_DIFFS
MENUCHANGE_LOW_INCOME_SE_DIFFS
MENUCHANGE_HIGH_INCOME_MEANS_DIFFS
MENUCHANGE_HIGH_INCOME_SE_DIFFS
###### Table A11 second and third row (change HIGH_INCOME==0 to HIGH_INCOME==1 to get third row)
SDs <- MENUHCANGESDs %>% filter(HIGH_INCOME ==0) %>% select(SD) %>% unlist() %>% combn(m=2)
SDs <- SDs[, c(1,3,2)]
NOBs <- MENUHCANGESOBS %>% filter(HIGH_INCOME ==0) %>% select(NOBS) %>% unlist() %>% combn(m=2)
NOBs <- NOBs[, c(1,3,2)]
SDs_POOLED <- ((((NOBs[1,]-1)*(SDs[1,]^2)) + ((NOBs[2,]-1)*(SDs[2,]^2))) / (NOBs[1,] + NOBs[2,] -2) )^.5
d <- c()
for(i in 1:3){
  d <- append(d, pwr.t2n.test(n1 = NOBs[1,i], n2=NOBs[2,i], d = NULL, sig.level = 0.05, power=.95)$d)
}
# Effect size (percentage points) required to achieve 95% power (reject null 95% of the time)
d * SDs_POOLED


############################### Table 8
###### Note that we will also produce Table A12 in this same block of code, because it will use information from the Table 8 computation
###### Note that this block of code also produces Tables A17 and A18 by changing the border distance threshold, as set below
RID_ROUND$HIGH_INCOME <- NA
RID_ROUND$HIGH_INCOME[which(RID_ROUND$HI_MED >= 59807)] <- 1
RID_ROUND$HIGH_INCOME[which(RID_ROUND$HI_MED < 59807)] <- 0
# Change bordeer distance threshold (set to 1600 for main text, 1400 or 1800 for appendicies robustness checks)
RID_ROUND$BORDER[which(RID_ROUND$BDIST <= 1600)] <- 1
RID_ROUND$BORDER[which(RID_ROUND$BDIST > 1600)] <- 0
RID_ROUND$DEATH <- RID_ROUND$DEAD - RID_ROUND$DEAD1 
DEATHS_BY_ROUND <- RID_ROUND[which(RID_ROUND$WASDEAD == 0 & RID_ROUND$ALWAYSALIVE==0 & RID_ROUND$FIRST_ROUND_OF_OBS ==1 & RID_ROUND$HI_MED >0 & RID_ROUND$CITY_WAGE==1 &
                                     RID_ROUND$BORDER==1),]
DEATHS_BY_ROUND <- DEATHS_BY_ROUND[,c(3,5,17,18,57)] #YEAR, 
DEATHS_BY_ROUND$ROUND <- as.numeric(DEATHS_BY_ROUND$ROUND)
DEATHS_BY_ROUND <- DEATHS_BY_ROUND %>% group_by(RID, LAT, LONG) %>% summarize(DEATH_ROUND = max(ROUND)) %>% as.data.frame()
# First Row
ALIVE_ROUND1 <- summaryBy(ALIVE ~ CITY_WAGE*BORDER, FUN="sum", 
                          data=RID_ROUND[which(RID_ROUND$WASDEAD==0 & RID_ROUND$FIRST_ROUND_OF_OBS==1 
                                               & RID_ROUND$HI_MED  > 0  & RID_ROUND$ROUND==1),])
ALIVE_ROUND7 <- summaryBy(ALIVE ~ CITY_WAGE*BORDER, FUN="sum", 
                          data=RID_ROUND[which(RID_ROUND$WASDEAD==0 & RID_ROUND$FIRST_ROUND_OF_OBS==1 
                                               & RID_ROUND$HI_MED  > 0  & RID_ROUND$ROUND==7),])
CLOSURE_RATES <- cbind(ALIVE_ROUND1, 1-(ALIVE_ROUND7[,3] /ALIVE_ROUND1[,3]))
names(CLOSURE_RATES) <- c("CITY_WAGE","BORDER","Initial_Restaurants","Closure_Rate")
REL_CLOSURE_RATES <- cbind(CLOSURE_RATES[,1:3], c(CLOSURE_RATES[1,4], CLOSURE_RATES[2:4,4] - CLOSURE_RATES[1,4]))
names(REL_CLOSURE_RATES) <- c("CITY_WAGE","BORDER","Initial_Restaurants","Closure_Rate_Relative")
REL_CLOSURE_RATES
# Standard errors 
CLOSURES_SES <- cbind(CLOSURE_RATES[,1:4], ( ((CLOSURE_RATES[,4]*(1-CLOSURE_RATES[,4])) / CLOSURE_RATES[,3])^.5))
CLOSURES_SDS <- cbind(CLOSURE_RATES[,1:4], ((CLOSURE_RATES[,4]*(1-CLOSURE_RATES[,4]))^.5))
names(CLOSURES_SES) <- c("CITY_WAGE","BORDER","Initial_Restaurants","Closure_Rate", "SEs")
names(CLOSURES_SDS) <- c("CITY_WAGE","BORDER","Initial_Restaurants","Closure_Rate", "SD")
# SEs of differences
CLOSURES_SE_DIFFS <- (CLOSURES_SES[1,5]^2 + CLOSURES_SES[2:4,5]^2)^.5
CLOSURES_SE_DIFFS
################  Table A12 first row
SDs <- CLOSURES_SDS  %>% select(SD) %>% unlist() %>% combn(m=2)
SDs <- SDs[, c(1,3,2)]
NOBs <- CLOSURE_RATES  %>% select(Initial_Restaurants) %>% unlist() %>% combn(m=2)
NOBs <- NOBs[, c(1,3,2)]
SDs_POOLED <- ((((NOBs[1,]-1)*(SDs[1,]^2)) + ((NOBs[2,]-1)*(SDs[2,]^2))) / (NOBs[1,] + NOBs[2,] -2) )^.5
d <- c()
for(i in 1:3){
  d <- append(d, pwr.t2n.test(n1 = NOBs[1,i], n2=NOBs[2,i], d = NULL, sig.level = 0.05, power=.95)$d)
}
d * SDs_POOLED
###################### Table 8 Bottom 2 rows
ALIVE_ROUND1 <- summaryBy(ALIVE ~ CITY_WAGE*BORDER*HIGH_INCOME, FUN="sum", 
                          data=RID_ROUND[which(RID_ROUND$WASDEAD==0 & RID_ROUND$FIRST_ROUND_OF_OBS==1 
                                               & RID_ROUND$HI_MED  > 0  & RID_ROUND$ROUND==1),])
ALIVE_ROUND7 <- summaryBy(ALIVE ~ CITY_WAGE*BORDER*HIGH_INCOME, FUN="sum", 
                          data=RID_ROUND[which(RID_ROUND$WASDEAD==0 & RID_ROUND$FIRST_ROUND_OF_OBS==1 
                                               & RID_ROUND$HI_MED  > 0  & RID_ROUND$ROUND==7),])
CLOSURE_RATES <- cbind(ALIVE_ROUND1, 1-(ALIVE_ROUND7[,4] /ALIVE_ROUND1[,4]))
names(CLOSURE_RATES) <- c("CITY_WAGE","BORDER","HIGH_INCOME","Initial_Restaurants","Closure_Rate")
REL_CLOSURE_RATES <- cbind(CLOSURE_RATES[,1:4], c(CLOSURE_RATES[1:2,5], CLOSURE_RATES[3:8,5] - CLOSURE_RATES[1:2,5]))
names(REL_CLOSURE_RATES) <- c("CITY_WAGE","BORDER","HIGH_INCOME","Initial_Restaurants","Closure_Rate_Relative")
REL_CLOSURE_RATES
# Standard errors 
CLOSURES_SES <- cbind(CLOSURE_RATES[,1:5], ( ((CLOSURE_RATES[,5]*(1-CLOSURE_RATES[,5])) / CLOSURE_RATES[,4])^.5))
CLOSURES_SDS <- cbind(CLOSURE_RATES[,1:5], ((CLOSURE_RATES[,5]*(1-CLOSURE_RATES[,5]))^.5))
names(CLOSURES_SES) <- c("CITY_WAGE","BORDER","HIGH_INCOME","Initial_Restaurants","Closure_Rate", "SEs")
names(CLOSURES_SDS) <- c("CITY_WAGE","BORDER","HIGH_INCOME","Initial_Restaurants","Closure_Rate", "SD")
# SEs of differences
CLOSURES_LOW_INCOME_SE_DIFFS <- (CLOSURES_SES[1,6]^2 + CLOSURES_SES[seq(3, 7, 2),6]^2)^.5
CLOSURES_HIGH_INCOME_SE_DIFFS <- (CLOSURES_SES[2,6]^2 + CLOSURES_SES[seq(4, 8, 2),6]^2)^.5
###### Table A12 second and third row (change HIGH_INCOME==0 to HIGH_INCOME==1 to get third row)
SDs <- CLOSURES_SDS %>%  filter(HIGH_INCOME==0) %>% select(SD) %>% unlist() %>% combn(m=2)
SDs <- SDs[, c(1,3,2)]
NOBs <- CLOSURE_RATES %>% filter(HIGH_INCOME==0) %>% select(Initial_Restaurants) %>% unlist() %>% combn(m=2)
NOBs <- NOBs[, c(1,3,2)]
SDs_POOLED <- ((((NOBs[1,]-1)*(SDs[1,]^2)) + ((NOBs[2,]-1)*(SDs[2,]^2))) / (NOBs[1,] + NOBs[2,] -2) )^.5
d <- c()
for(i in 1:3){
  d <- append(d, pwr.t2n.test(n1 = NOBs[1,i], n2=NOBs[2,i], d = NULL, sig.level = 0.05, power=.95)$d)
}
d * SDs_POOLED



############################### Table 9
# Compute closure rates by strata and competition type
CR_SNE <- with(RID_ROUND[which(RID_ROUND$ROUND == 1 & RID_ROUND$WASDEAD==0 & RID_ROUND$FIRST_ROUND_OF_OBS==1 &
                                 RID_ROUND$CITY_WAGE ==0 & RID_ROUND$COMP_CW_LI == 0 & RID_ROUND$HI_MED < 59807),], mean(1-ALWAYSALIVE, na.rm=T))
# State Wage Low-Income Census Tracts,  Exposed to x-border competition
CR_SE <- with(RID_ROUND[which(RID_ROUND$ROUND == 1 & RID_ROUND$WASDEAD==0 & RID_ROUND$FIRST_ROUND_OF_OBS==1 &
                                RID_ROUND$CITY_WAGE ==0 & RID_ROUND$COMP_CW_LI > 0 & RID_ROUND$HI_MED < 59807),], mean(1-ALWAYSALIVE, na.rm=T))
# City Wage Low-Income Census Tracts,  Exposed to x-border competition
CR_CE <- with(RID_ROUND[which(RID_ROUND$ROUND == 1 & RID_ROUND$WASDEAD==0 & RID_ROUND$FIRST_ROUND_OF_OBS==1 &
                                RID_ROUND$CITY_WAGE ==1 & RID_ROUND$COMP_SW_LI > 0 & RID_ROUND$HI_MED < 59807),], mean(1-ALWAYSALIVE, na.rm=T))
# City Wage Low-Income Census Tracts,  Not Exposed to x-border competition
CR_CNE <- with(RID_ROUND[which(RID_ROUND$ROUND == 1 & RID_ROUND$WASDEAD==0 & RID_ROUND$FIRST_ROUND_OF_OBS==1 &
                                 RID_ROUND$CITY_WAGE ==1 & RID_ROUND$COMP_SW_LI == 0 & RID_ROUND$HI_MED < 59807),], mean(1-ALWAYSALIVE, na.rm=T))
## High-Income Census Tracts (Row 2)
# State Wage Low-Income Census Tracts,  Not Exposed to x-border competition
CR_SNE <- with(RID_ROUND[which(RID_ROUND$ROUND == 1 & RID_ROUND$WASDEAD==0 & RID_ROUND$FIRST_ROUND_OF_OBS==1 &
                                 RID_ROUND$CITY_WAGE ==0 & RID_ROUND$COMP_CW_HI == 0 & RID_ROUND$HI_MED >= 59807),], mean(1-ALWAYSALIVE, na.rm=T))
# State Wage Low-Income Census Tracts,  Exposed to x-border competition
CR_SE <- with(RID_ROUND[which(RID_ROUND$ROUND == 1 & RID_ROUND$WASDEAD==0 & RID_ROUND$FIRST_ROUND_OF_OBS==1 &
                                RID_ROUND$CITY_WAGE ==0 & RID_ROUND$COMP_CW_HI > 0 & RID_ROUND$HI_MED >= 59807),], mean(1-ALWAYSALIVE, na.rm=T))
# City Wage Low-Income Census Tracts,  Exposed to x-border competition
CR_CE <- with(RID_ROUND[which(RID_ROUND$ROUND == 1 & RID_ROUND$WASDEAD==0 & RID_ROUND$FIRST_ROUND_OF_OBS==1 &
                                RID_ROUND$CITY_WAGE ==1 & RID_ROUND$COMP_SW_HI > 0 & RID_ROUND$HI_MED >= 59807),], mean(1-ALWAYSALIVE, na.rm=T))
# City Wage Low-Income Census Tracts,  Not Exposed to x-border competition
CR_CNE <- with(RID_ROUND[which(RID_ROUND$ROUND == 1 & RID_ROUND$WASDEAD==0 & RID_ROUND$FIRST_ROUND_OF_OBS==1 &
                                 RID_ROUND$CITY_WAGE ==1 & RID_ROUND$COMP_SW_HI == 0 & RID_ROUND$HI_MED >= 59807),], mean(1-ALWAYSALIVE, na.rm=T))
########## NOBS for each group ######
# State Wage Low-Income Census Tracts,  Not Exposed to x-border competition
NOBS_SNE <- with(RID_ROUND[which(RID_ROUND$ROUND == 1 & RID_ROUND$WASDEAD==0 & RID_ROUND$FIRST_ROUND_OF_OBS==1 &
                                   RID_ROUND$CITY_WAGE ==0 & RID_ROUND$COMP_CW_LI == 0 & RID_ROUND$HI_MED < 59807),], length(ALWAYSALIVE))
# State Wage Low-Income Census Tracts,  Exposed to x-border competition
NOBS_SE <- with(RID_ROUND[which(RID_ROUND$ROUND == 1 & RID_ROUND$WASDEAD==0 & RID_ROUND$FIRST_ROUND_OF_OBS==1 &
                                  RID_ROUND$CITY_WAGE ==0 & RID_ROUND$COMP_CW_LI > 0 & RID_ROUND$HI_MED < 59807),], length(ALWAYSALIVE))
# City Wage Low-Income Census Tracts,  Exposed to x-border competition
NOBS_CE <- with(RID_ROUND[which(RID_ROUND$ROUND == 1 & RID_ROUND$WASDEAD==0 & RID_ROUND$FIRST_ROUND_OF_OBS==1 &
                                  RID_ROUND$CITY_WAGE ==1 & RID_ROUND$COMP_SW_LI > 0 & RID_ROUND$HI_MED < 59807),], length(ALWAYSALIVE))
# City Wage Low-Income Census Tracts,  Not Exposed to x-border competition
NOBS_CNE <- with(RID_ROUND[which(RID_ROUND$ROUND == 1 & RID_ROUND$WASDEAD==0 & RID_ROUND$FIRST_ROUND_OF_OBS==1 &
                                   RID_ROUND$CITY_WAGE ==1 & RID_ROUND$COMP_SW_LI == 0 & RID_ROUND$HI_MED < 59807),], length(ALWAYSALIVE))
### High-Income Census Tracts (Row 2)
# State Wage Low-Income Census Tracts,  Not Exposed to x-border competition
NOBS_SNE <- with(RID_ROUND[which(RID_ROUND$ROUND == 1 & RID_ROUND$WASDEAD==0 & RID_ROUND$FIRST_ROUND_OF_OBS==1 &
                                   RID_ROUND$CITY_WAGE ==0 & RID_ROUND$COMP_CW_HI == 0 & RID_ROUND$HI_MED >= 59807),], length(ALWAYSALIVE))
# State Wage Low-Income Census Tracts,  Exposed to x-border competition
NOBS_SE <- with(RID_ROUND[which(RID_ROUND$ROUND == 1 & RID_ROUND$WASDEAD==0 & RID_ROUND$FIRST_ROUND_OF_OBS==1 &
                                  RID_ROUND$CITY_WAGE ==0 & RID_ROUND$COMP_CW_HI > 0 & RID_ROUND$HI_MED >= 59807),], length(ALWAYSALIVE))
# City Wage Low-Income Census Tracts,  Exposed to x-border competition
NOBS_CE <- with(RID_ROUND[which(RID_ROUND$ROUND == 1 & RID_ROUND$WASDEAD==0 & RID_ROUND$FIRST_ROUND_OF_OBS==1 &
                                  RID_ROUND$CITY_WAGE ==1 & RID_ROUND$COMP_SW_HI > 0 & RID_ROUND$HI_MED >= 59807),], length(ALWAYSALIVE))
# City Wage Low-Income Census Tracts,  Not Exposed to x-border competition
NOBS_CNE <- with(RID_ROUND[which(RID_ROUND$ROUND == 1 & RID_ROUND$WASDEAD==0 & RID_ROUND$FIRST_ROUND_OF_OBS==1 &
                                   RID_ROUND$CITY_WAGE ==1 & RID_ROUND$COMP_SW_HI == 0 & RID_ROUND$HI_MED >= 59807),], length(ALWAYSALIVE))
### Table 9 standard errors
## Low-income census tracts
# State Wage Low-Income Census Tracts,  Not Exposed to x-border competition
(CR_SNE*(1-CR_SNE)) / (NOBS_SNE^.5)
# SEs for the differences
# State Wage exposed to x-border competition
(((CR_SNE*(1-CR_SNE)) / (NOBS_SNE^.5))^2 + ((CR_SE*(1-CR_SE)) / (NOBS_SE^.5))^2)^.5
# City Wage exposed to x-border competition
(((CR_SNE*(1-CR_SNE)) / (NOBS_SNE^.5))^2 + ((CR_CE*(1-CR_CE)) / (NOBS_CE^.5))^2)^.5
# City Wage not exposed to x-border competition
(((CR_SNE*(1-CR_SNE)) / (NOBS_SNE^.5))^2 + ((CR_CNE*(1-CR_CNE)) / (NOBS_CNE^.5))^2)^.5



################################################################################################################################
################################################################################################################################
################################################## Appendix tables and figures #################################################
################################################################################################################################
################################################################################################################################


###########  Table A1
STRATA <- STRATA[,c(1,5)]
names(STRATA) <- c("GEOID10","STRATA")
MHHI_TRACT_YEAR <- merge(MHHI_TRACT_YEAR, STRATA, by="GEOID10", all.x=T)
MHHI_ALLTRACTS_2015 <- MHHI_TRACT_YEAR[which(MHHI_TRACT_YEAR$YEAR==2015),]
ALL_RESTAURANTS$RID <-  as.numeric(gsub(",","",ALL_RESTAURANTS$rid))
names(ALL_RESTAURANTS)[which(names(ALL_RESTAURANTS)=="CT")] <- "GEOID10"
MHHI_ALLTRACTS_2015$YEAR <- NULL
ALL_RESTAURANTS <- merge(ALL_RESTAURANTS, MHHI_ALLTRACTS_2015, by="GEOID10", all.x=T)
SURVEY_RESTAURANTS <- DATA_A[which(DATA_A$HASPRICE==1 & DATA_A$ROUND==1 & is.na(DATA_A$MHHI)==F),]
SURVEY_RESTAURANTS <- SURVEY_RESTAURANTS[duplicated(SURVEY_RESTAURANTS$RID)==F,]
# Strata breakdown for survey and population 
round(table(SURVEY_RESTAURANTS$STRATA)*100 / sum(table(SURVEY_RESTAURANTS$STRATA)), 2)
round(table(ALL_RESTAURANTS$STRATA)*100 / sum(table(ALL_RESTAURANTS$STRATA)), 2)
# Median income of restaurants for survey and population 
median(SURVEY_RESTAURANTS$HI_MED, na.rm=T) # Note that HI_MED in the SURVEY_RESTAURANTS dataframe is 2015 median household income
median(ALL_RESTAURANTS$MHHI, na.rm=T)
# Percent full service for survey and population 
RESTAURANT_TYPE <- ALL_RESTAURANTS[,c(14,8)]
SURVEY_RESTAURANTS <- merge(SURVEY_RESTAURANTS, RESTAURANT_TYPE, by="RID")
round(table(SURVEY_RESTAURANTS$foodtype)*100 / sum(table(SURVEY_RESTAURANTS$foodtype)), 2)
round(table(ALL_RESTAURANTS$foodtype)*100 / sum(table(ALL_RESTAURANTS$foodtype)), 2)


##############  Table A2: Average pop density and restaurant density by strata 
SUMMARIES <- DATA_A %>% group_by(HIGH_INCOME,STRATA) %>% filter(ROUND %in% 7 & is.na(IPRICE)==F & ALLMATCH==1 & ICLASS==11 & CT_POP > 100) %>% 
  summarize(Pop_Density = round(mean(CT_POP / CT_AREA_MI, na.rm=T), 0),
            Restaurant_Density = round(mean(NR_TOTAL / CT_AREA_MI), 0),
            Restaurants_Per_Capita = (round(mean(NR_TOTAL / CT_POP, na.rm=T),4)*1000),
            Share_Full_Service = round(mean(NR_FULL/NR_TOTAL),2),
            Median_Household_Income = round(mean(HI_MED, na.rm=T), 0))
SUMMARIES <- as.data.frame(rbind(SUMMARIES[which(SUMMARIES$HIGH_INCOME==0),], SUMMARIES[which(SUMMARIES$HIGH_INCOME==1),]))
SUMMARIES


############## Table A3
### This code block also produces Tables A13 and A14; just replace the STRATA explanatory variable with STRATA1400 or STRATA1800
M1 <- felm(PD6PRICE ~ STRATA  | 0|0| RID
           ,data=DATA_A[which(DATA_A$ROUND == 7 & DATA_A$ALLMATCH==1 & 
                                DATA_A$HI_MED < 59807 & DATA_A$RPRICE_MED_R1 > 0),])
M2 <- felm(PD6PRICE ~ STRATA + POP_DENSITY + RESTAURANT_DENSITY | 0|0| RID
           ,data=DATA_A[which(DATA_A$ROUND == 7 & DATA_A$ALLMATCH==1 & 
                                DATA_A$HI_MED < 59807 & DATA_A$RPRICE_MED_R1 > 0),])
M3 <- felm(PD6PRICE ~ STRATA + POP_DENSITY + RESTAURANT_DENSITY + RESTAURANTS_PER_CAPITA + SHARE_FULL_SERVICE | 0|0| RID
           ,data=DATA_A[which(DATA_A$ROUND == 7 & DATA_A$ALLMATCH==1 & 
                                DATA_A$HI_MED < 59807 & DATA_A$RPRICE_MED_R1 > 0),])
M4 <- felm(PD6PRICE ~ STRATA + POP_DENSITY + RESTAURANT_DENSITY + RESTAURANTS_PER_CAPITA + SHARE_FULL_SERVICE + PD6MHHI | 0|0| RID
           ,data=DATA_A[which(DATA_A$ROUND == 7 & DATA_A$ALLMATCH==1 & 
                                DATA_A$HI_MED < 59807 & DATA_A$RPRICE_MED_R1 > 0),])
M5 <- felm(PD6PRICE ~ STRATA | 0|0| RID
           ,data=DATA_A[which(DATA_A$ROUND == 7 & DATA_A$ALLMATCH==1 & 
                                DATA_A$HI_MED > 59807 & DATA_A$RPRICE_MED_R1 > 0),])
M6 <- felm(PD6PRICE ~ STRATA + POP_DENSITY + RESTAURANT_DENSITY | 0|0| RID
           ,data=DATA_A[which(DATA_A$ROUND == 7 & DATA_A$ALLMATCH==1 & 
                                DATA_A$HI_MED > 59807 & DATA_A$RPRICE_MED_R1 > 0),])
M7 <- felm(PD6PRICE ~ STRATA + POP_DENSITY + RESTAURANT_DENSITY + RESTAURANTS_PER_CAPITA + SHARE_FULL_SERVICE | 0|0| RID
           ,data=DATA_A[which(DATA_A$ROUND == 7 & DATA_A$ALLMATCH==1 & 
                                DATA_A$HI_MED > 59807 & DATA_A$RPRICE_MED_R1 > 0),])
M8 <- felm(PD6PRICE ~ STRATA + POP_DENSITY + RESTAURANT_DENSITY + RESTAURANTS_PER_CAPITA + SHARE_FULL_SERVICE + PD6MHHI| 0|0| RID
           ,data=DATA_A[which(DATA_A$ROUND == 7 & DATA_A$ALLMATCH==1 & 
                                DATA_A$HI_MED > 59807 & DATA_A$RPRICE_MED_R1 > 0),])


############## Table A4  (PRETRENDS)
### Number of observations by strata and income segment 
DATA_0_1 %>%  group_by(STRATA, HIGH_INCOME) %>% summarize(n()) %>% as.data.frame() #group_by(STRATA, HIGH_INCOME) %>%


############## Figure A4  (PRETRENDS)
# Histogram of year of Round 0 observations
hist(DATA_0_1$LAGYEAR, main="Round 0 Observations by Year", xlab="Year",ylab="Number of Items")


############## Figure A5  (PRETRENDS)
# Histogram of time lag between Round 0 and Round 1 observations
hist(DATA_0_1$DTIME, main="Duration Between Round 0 and Round 1 Observations ", xlab="Months Between Observations",ylab="Number of Items")


############## Table A5
########## Matched observations by round
summaryBy(HASPRICE ~ CITY_WAGE*BORDER*YEAR*MONTH,
          FUN="sum", data=DATA_A[which(DATA_A$ROUND %in% 2:7 & is.na(DATA_A$PDPRICE)==F  & DATA_A$IPRICE > 0),], na.rm=T)


############## Table A6
# The mean price changes are taken over 6 months apart, so they are divided by the number of years elapsed between observations to annualize them
# In the below code, the first summaryBy command gets the ID info (i.e. wage and border).
# The second summaryBy gets raw mean price changes
# The third summaryBy gets the number of years (# of Months/12) elapsed between observations to annualize the mean price changes
cbind(summaryBy(PDPRICE ~ CITY_WAGE*BORDER*ROUND,
                FUN="mean", data=DATA_A[which(DATA_A$ROUND %in% 2:7 & is.na(DATA_A$PDPRICE)==F  ),], na.rm=T)[,1:3],
      summaryBy(PDPRICE ~ CITY_WAGE*BORDER*ROUND,
                FUN="mean", data=DATA_A[which(DATA_A$ROUND %in% 2:7 & is.na(DATA_A$PDPRICE)==F  ),], na.rm=T)[,4] /
        (summaryBy(DTIME ~ CITY_WAGE*BORDER*ROUND,
                   FUN="mean", data=DATA_A[which(DATA_A$ROUND %in% 2:7 & is.na(DATA_A$PDPRICE)==F  ),], na.rm=T)[,4]/12))
## Do same for right-most column of Table A2
cbind(summaryBy(PDPRICE ~ ROUND,
                FUN="mean", data=DATA_A[which(DATA_A$ROUND %in% 2:7 & is.na(DATA_A$PDPRICE)==F  ),], na.rm=T)[,1],
      summaryBy(PDPRICE ~ ROUND,
                FUN="mean", data=DATA_A[which(DATA_A$ROUND %in% 2:7 & is.na(DATA_A$PDPRICE)==F  ),], na.rm=T)[,2] /
        (summaryBy(DTIME ~ ROUND,
                   FUN="mean", data=DATA_A[which(DATA_A$ROUND %in% 2:7 & is.na(DATA_A$PDPRICE)==F  ),], na.rm=T)[,2]/12))

############## Table A7
## Mean price change by strata from balanced panel (ALLMATCH==1) broken down by census tract income
# HI_MED allows you to subset for high-income and low-income census tracts. The median census tract income level for observations
# From the UNBALANCED item panel was 57500 (see first line below)
with(DATA_A[which(DATA_A$HASPRICE==1 ),], median(HI_MED, na.rm=T))
## Compute the mean values for Table A2. HI_MED subset allows for computation of above-and-below median census tract income levels
cbind(summaryBy(PDPRICE ~ CITY_WAGE*BORDER*ROUND,
                FUN="mean", data=DATA_A[which(DATA_A$HI_MED >= 57500 & DATA_A$ROUND %in% 2:7 & is.na(DATA_A$PDPRICE)==F),], na.rm=T)[,1:3],
      summaryBy(PDPRICE ~ CITY_WAGE*BORDER*ROUND,
                FUN="mean", data=DATA_A[which(DATA_A$HI_MED >= 57500 & DATA_A$ROUND %in% 2:7 & is.na(DATA_A$PDPRICE)==F),], na.rm=T)[,4] /
        (summaryBy(DTIME ~ CITY_WAGE*BORDER*ROUND,
                   FUN="mean", data=DATA_A[which(DATA_A$HI_MED >= 57500 & DATA_A$ROUND %in% 2:7 & is.na(DATA_A$PDPRICE)==F),], na.rm=T)[,4]/12))


############## Figure A6
# Compare CPI Food Away From Home index from the BLS with our price survey price changes
# First we summarize between-round price changes by aggregating them to the YEAR*MONTH level and taking means
FIPI <- summaryBy(PDPRICE ~ paste(YEAR,MONTH), data=DATA_A[which(is.na(DATA_A$PDPRICE) != T),])
names(FIPI) <- c("Year","Month","PriceChange")
# Now we re-arrange that table and compute differences
FIPI <- FIPI[c(1:36),]
FIPI_SEMI_ANNUAL <- FIPI[-c(1:6),] 
# Plot it with clean margins
par(mar=c(3,4,.8,1))
plot(rollapply(I(FIPI_SEMI_ANNUAL$PriceChange),3, "mean"), ylim=c(0, 5), pch=20, col="blue", ylab="6-Month Price Change (%)", xaxt="n")
lines(rollapply(I(FIPI_SEMI_ANNUAL$PriceChange),3, "mean"), col="blue")
# Now we manipulate the BLS Food away from home for LA Metro data
CPI <- BLS[,c(1,2,5)]
# We don't use the first 2 years of this dataset because it pre-dates our study
CPI <- CPI[-c(1:24),]
# Now te dataset just needs to be re-arranged and have differences taken
CPI_SEMI_ANNUAL <- (CPI$CPI[7:36] - CPI$CPI[1:30]) / CPI$CPI[1:30]
points(rollapply(I(CPI_SEMI_ANNUAL*100),3, FUN="mean"), pch=20, col="darkgreen")
lines(rollapply(I(CPI_SEMI_ANNUAL*100),3, FUN="mean"), pch=20, col="darkgreen")
legend(x=1, y=5, 
       c("Price Survey Food Items","CPI Food Away from Home"), col=c("blue","darkgreen"), pch=20, cex=.8, pt.cex=1.2)
axis(1, at=seq(1,30,6), labels=c("Jul. '17","Jan. '18","Jul. '18","Jan. '19","Jul. '19"), cex.axis=1.2)
#dev.off()


############## Figure A7
# Create descriptive stats for CPI and PPI
BLS$Date <- paste(BLS$Month, "15", BLS$Year, sep="/")
BLS_Dates <- as.Date(BLS$Date, format="%m/%d/%Y")
# Normalize BLS to Jan 2015 values
BLS_NORM <- BLS[,3:5]
BLS_NORM <-  data.frame(lapply(BLS_NORM, function(X) X/X[1]))
BLS_NORM <- cbind(BLS_NORM, BLS_Dates)
names(BLS_NORM) <- c("PPICereals","PPIProtein","CPI","Date")
# Rearrange the dataframe so that it can be easily plotted
BLS_NORM <- melt(BLS_NORM, id="Date")
BLS_NORM$variable <- factor(BLS_NORM$variable, levels = c("CPI","PPICereals","PPIProtein"))
ggplot(data=BLS_NORM, aes(x=Date, y=value, colour=variable)) +
  geom_line() +  geom_point() + scale_color_manual(values=c("darkgreen", "orange","darkgrey")) + 
  theme(text = element_text(size=12), panel.grid.major = element_blank(),
                                      panel.grid.minor = element_blank(), legend.title=element_blank(),
                                      strip.background = element_blank(), legend.position = c(.2, .85),
                                      panel.border = element_rect(colour = "black", fill=NA),
                                      legend.box.background = element_rect(colour = "black"),
                                      legend.spacing.y = unit(0, "mm")) + 
  labs(x="Year", y="Value Relative to Jan. 2015") 


############## Figure A8
# Loess regression of between-round price increases
#### Run multiple times for each round, by changing the subset four ROUND from 7 to lower vallues in the ggplot(...) function
# Make BDIST_CONTINUOUS, which is BDIST but with state wage restaurants recorded with negative BDIST values
DATA_A$BDIST_CONTINUOUS <- DATA_A$BDIST
DATA_A$BDIST_CONTINUOUS[which(DATA_A$CITY_WAGE==0)] <- DATA_A$BDIST_CONTINUOUS[which(DATA_A$CITY_WAGE==0)]*-1
DATA_A$BDIST_CONTINUOUS_MILES <- DATA_A$BDIST_CONTINUOUS / 1609
DATA_A$WageSchedule <- NA
DATA_A$WageSchedule[which(DATA_A$CITY_WAGE==1)] <- "City Wage"
DATA_A$WageSchedule[which(DATA_A$CITY_WAGE==0)] <- "State Wage"
DATA_A$WageSchedule <- factor(DATA_A$WageSchedule, levels=c("State Wage","City Wage"))
# Make BDIST plots. These can be made separately for each round using the DATA_A$ROUND %in% c(7) subset
ggplot(DATA_A[which(is.na(DATA_A$PDPRICE)==F  & DATA_A$ROUND %in% c(7) ),],
       aes(x = BDIST_CONTINUOUS_MILES , y = PDPRICE, colour=WageSchedule)) + geom_point(size=.5) + 
  stat_smooth(span=1, size=2, alpha=.5, method="loess", level=.95)  + scale_color_manual(values=(c("blue", "red"))) +
  labs(x="Border Distance (Miles)", y="% Change in Food Prices", title="") +
  coord_cartesian(ylim=c(0, 15), xlim=c(-5, 5)) +  geom_hline(yintercept=0) + geom_vline(xintercept=0) 


############## Table A8
## Closures by strata and round of observation
# Count up the number of alive restaurants each round, ignoring all restaurants not observed during the first round and ignoring all restaurants that
# ever closed at any point and subsequently reopened, in addition to restaurants in any census tract that do not have a median household income (HI_MED >0)
ALIVE_BY_ROUND <- summaryBy(ALIVE ~ CITY_WAGE*BORDER*ROUND, FUN="sum", 
                            data=RID_ROUND[which(RID_ROUND$FIRST_ROUND_OF_OBS==1 & RID_ROUND$WASDEAD == 0 & RID_ROUND$HI_MED  > 0  ),])
# The ALIVE_BY_ROUND table just needs to be arranged and have the deaths computed from it to produce Table A4
ALIVE_BY_ROUND_W <- cbind(ALIVE_BY_ROUND[1:7,3:4],ALIVE_BY_ROUND[8:14,4],ALIVE_BY_ROUND[22:28,4],ALIVE_BY_ROUND[15:21,4])
names(ALIVE_BY_ROUND_W) <- c("ROUND","S_NB","S_B","C_B","C_NB")
ALIVE_BY_ROUND_W$S_NB_D <- c(0, (ALIVE_BY_ROUND_W$S_NB[2:7] - ALIVE_BY_ROUND_W$S_NB[1:6]))
ALIVE_BY_ROUND_W$S_B_D <- c(0, (ALIVE_BY_ROUND_W$S_B[2:7] - ALIVE_BY_ROUND_W$S_B[1:6]))
ALIVE_BY_ROUND_W$C_B_D <- c(0, (ALIVE_BY_ROUND_W$C_B[2:7] - ALIVE_BY_ROUND_W$C_B[1:6]))
ALIVE_BY_ROUND_W$C_NB_D <- c(0, (ALIVE_BY_ROUND_W$C_NB[2:7] - ALIVE_BY_ROUND_W$C_NB[1:6]))
ALIVE_BY_ROUND_W$S_NB_PD <- round(c(0, (ALIVE_BY_ROUND_W$S_NB_D[2:7] / ALIVE_BY_ROUND_W$S_NB[1:6]))*-1, 4)*100
ALIVE_BY_ROUND_W$S_B_PD <- round(c(0, (ALIVE_BY_ROUND_W$S_B_D[2:7] / ALIVE_BY_ROUND_W$S_B[1:6]))*-1, 4)*100
ALIVE_BY_ROUND_W$C_B_PD <- round(c(0, (ALIVE_BY_ROUND_W$C_B_D[2:7] / ALIVE_BY_ROUND_W$C_B[1:6]))*-1, 4)*100
ALIVE_BY_ROUND_W$C_NB_PD <- round(c(0, (ALIVE_BY_ROUND_W$C_NB_D[2:7] / ALIVE_BY_ROUND_W$C_NB[1:6]))*-1, 4)*100
ALIVE_BY_ROUND_W


############## Figure A9
### Did State Border restaurants near City Border closures increase prices more?
##### Run twice for high and low-income census tracts, with the HI_MED > 59817 subset in the feols(...) function
DATA_B <- DATA_A[,-c(10:16)]
DATA_B <- merge(DATA_B, ATTRIBUTES_NEAR_CLOSURES, by="RID", all.x=T)
DATA_B$ROUNDS_AFTER_CITYCOMP_CLOSES <- as.numeric(DATA_B$ROUND) - DATA_B$ROUND_CITYCOMPCLOSES
MOD1 <- summary(feols(PDPRICE ~ relevel(factor(ROUNDS_AFTER_CITYCOMP_CLOSES), ref="0")    | ROUND 
                      ,data=DATA_B[which(DATA_B$ROUND %in% 3:7 & DATA_B$ALLMATCH==1 & 
                                           DATA_B$HI_MED >59817 & DATA_B$RPRICE_MED_R1 > 0 & DATA_B$DIST_CITYCOMP_THATCLOSES <1),]), cluster="RID") # Change the DATA_B$HI_MED >59817 subset for low-income (left panel)
MOD1 <- cbind.data.frame(names(MOD1$coefficients), MOD1$coefficients, MOD1$se)
names(MOD1) <- c("VAR","COEFF","SE")
MOD1 <- MOD1[which(grepl("ROUNDS_AFTER_CITYCOMP_CLOSES", MOD1$VAR)),]
MOD1$ROUNDS_TO_TREATMENT <- c(-4:-1, 1:6)
MOD1 <- rbind(MOD1, c(0,0,0,0))
ggplot(MOD1, aes(x=ROUNDS_TO_TREATMENT, y=COEFF)) + 
  geom_line() + geom_point(size=2.5) + geom_ribbon(aes(ymin=(COEFF)-(1.96*SE), ymax=(COEFF)+(1.96*SE)), alpha=.07) +
  ylim(-10, 10) + labs(title="", x="Rounds to Competitor Closure", y = "% Change in Prices") +
  theme_classic(base_size = 12) + geom_hline(yintercept=0) + geom_vline(xintercept=0)


##############  Figure A10
SUBSET <- DATA_A[which(DATA_A$ALLMATCH==1),]
SUBSET <- SUBSET[!duplicated(SUBSET$RID),]
plot(SUBSET$RPRICE_MED_R1 ~ SUBSET$HI_MED, ylab="Average Entree Prices ($, in Round 1)", xlab="Census Tract Median Income ($, 2015 ACS)", cex.lab=1.2)
abline(lm(RPRICE_MED_R1 ~ HI_MED, data = SUBSET), col = "black", lwd=3)


############## Table A9
summary(felm(PD6PRICE ~ STRATA | 0|0| RID
             ,data=DATA_A[which(DATA_A$ROUND == 7 & DATA_A$ALLMATCH==1 & 
                                  DATA_A$HI_MED > 59807 & DATA_A$RPRICE_MED_R1 > 0),]))
summary(felm(PD6PRICE ~ STRATA | 0|0| RID
             ,data=DATA_A[which(DATA_A$ROUND == 7 & DATA_A$ALLMATCH==1 & 
                                  DATA_A$HI_MED > 82794 & DATA_A$RPRICE_MED_R1 > 0),]))
summary(felm(PD6PRICE ~ STRATA | 0|0| RID
             ,data=DATA_A[which(DATA_A$ROUND == 7 & DATA_A$ALLMATCH==1 & 
                                  DATA_A$HI_MED > 0 & DATA_A$RPRICE_MED_R1 > 6.245),]))
summary(felm(PD6PRICE ~ STRATA | 0|0| RID
             ,data=DATA_A[which(DATA_A$ROUND == 7 & DATA_A$ALLMATCH==1 & 
                                  DATA_A$HI_MED > 0 & DATA_A$RPRICE_MED_R1 > 8.49),]))


############## Table A10
# The following code produces each line of Table A10 individually. Change the subset in the below line to make each line
# The values are MED_INC: 59807, RPRICE_MED_RI: 6.245. Change these values for each row of each table
SUBSET <- DATA_A[which(DATA_A$ROUND == 7 & DATA_A$ALLMATCH==T & DATA_A$HI_MED >= 59807 & DATA_A$RPRICE_MED_R1 >=6.245),]
SDs <- SUBSET %>% group_by(CITY_WAGE, BORDER) %>% summarize(sd(PD6PRICE, na.rm=T))  %>% as.data.frame() %>% select(3) %>% unlist() %>% combn(m=2)
SDs <- SDs[, c(1,3,2)]
NOBs <- SUBSET %>% group_by(CITY_WAGE, BORDER) %>% summarize(n())  %>% as.data.frame() %>% select(3) %>% unlist() %>% combn(m=2)
NOBs <- NOBs[, c(1,3,2)]
SDs_POOLED <- ((((NOBs[1,]-1)*(SDs[1,]^2)) + ((NOBs[2,]-1)*(SDs[2,]^2))) / (NOBs[1,] + NOBs[2,] -2) )^.5
d <- c()
for(i in 1:3){
  d <- append(d, pwr.t2n.test(n1 = NOBs[1,i], n2=NOBs[2,i], d = NULL, sig.level = 0.05, power=.95)$d)
}
# Effect size (percentage points) required to achieve 95% power (reject null 95% of the time)
d * SDs_POOLED


############## Table A11
######### This table was produced above in the same code block that produces Table 7


############## Table A12
######### This table was produced above in the same code block that produces Table 8


############## Tables A13 and A14
######### These tables were produced above in the code block that produces Table A3; re-run that code using STRATA1400 or STRATA1800 instead of STRATA


############## Tables A15 and A16
######### These tables were produced above in the code block that produces Table 7; re-run that code but change the BDIST cutoff from 1600 to 1400 or 1800


############## Tables A17 and A18
######### These tables were produced above in the code block that produces Table 8; re-run that code but change the BDIST cutoff from 1600 to 1400 or 1800

