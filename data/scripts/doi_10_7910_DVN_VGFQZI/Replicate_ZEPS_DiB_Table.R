#------------------------------------------
# File Name: Code to Make Table in ZEPS DiB Article
#
# File Purpose: Provide replication materials for ZEPS DiB Article
#
# Author: Governance and Local Development Institute
#
# Contact: data@gld.gu.se
#
# Last Updated: November 16, 2022
#
# Instructions for Use: set working directory
#                       on line 26 to point to 
#                       folder containing 
#                       ZEPS_DiB_Dataset.rds
#
#------------------------------------------


#---Set Working Directory
setwd("C:/Users/xmeter/University of Gothenburg/GLD Projects - GLD Datasets/DataInBrief_Info/GLD_DiB/ZEPS/Final Materials/Data")


#---Data Import
zepsData <- readRDS("ZEPS_DiB_Dataset.rds")


#---Global Vars

# Age Brackets
age0 = 18
age1 = 25
age2 = 45
age3 = 65

yLabels <- c("Total","Previous Respondents","New Respondents","Not Sure", "Men", "Women", "Central", "Copper Belt", "Eastern", "Luapula", "Lusaka", "Muchinga", "North-Western", "Northern", "Southern", "Western", "DK/RA Region", paste("Age ",as.character(age0)," - ", as.character(age1) ), paste(as.character(age1+1), " - ", as.character(age2)), paste(as.character(age2+1), " - ", as.character(age3)), paste("Older than ",as.character(age3)))

#---Create Table

r1 <- as.numeric(unlist(list(sum(table(zepsData$Q_19_R1)), # Total
                             table(zepsData$Q_19_R1), # Prev, new, unsure
                             table(zepsData$Q_16_R1)[1:2], # Male, Female
                             table(zepsData$regionListR1)[c(1:2,4:11,3)], # Regions
                             hist(as.numeric(zepsData$Q_14_R1),breaks=c(age0-0.005,age1+0.005,age2+0.005,age3+0.005,120))[2] # Ages
)))

r2 <- as.numeric(unlist(list(sum(table(zepsData$demo_q6_R2)), # Total
                             table(zepsData$demo_q6_R2), # Prev, new, unsure
                             table(zepsData$demo_q4_R2)[1:2], # Male, Female
                             table(zepsData$regionListR2)[c(1:2,4:11,3)], # Regions
                             hist(as.numeric(zepsData$demo_q3_R2),breaks=c(age0-0.005,age1+0.005,age2+0.005,age3+0.005,120))[2] # Ages
)))

r3 <- as.numeric(unlist(list(sum(table(zepsData$demo_q6_R3)), # Total
                             table(zepsData$demo_q6_R3), # Prev, new, unsure
                             table(zepsData$demo_q4_R3)[1:2], # Male, Female
                             table(zepsData$regionListR3)[c(1:2,4:11,3)], # Regions
                             hist(as.numeric(zepsData$demo_q3_R3),breaks=c(age0-0.005,age1+0.005,age2+0.005,age3+0.005,120))[2] # Ages
)))

dataTable = data.frame(
  Labels <- yLabels,
  r1tot <- r1,
  r2tot <- r2,
  r3tot <- r3
)

