# Code to generate new childhood asthma incidence

# Please change the following lines:
# Line 7 -- working directory
# Line 19 -- reading population data (see below instructions for downloading it)
# Line 23 -- reading inputs provided with code
# Line 99 -- saving output files

# Setting working directory
setwd("Marginal_Damages_Model/New_Preprocessing_Data/")

# Reading Population data from the U.S. CDC
# Available at: https://www.cdc.gov/nchs/nvss/bridged_race/data_documentation.htm#Vintage2020
# Used data for July 1, 2019 -- Vintage 2020
# Direct data link https://www.cdc.gov/nchs/nvss/bridged_race/pcen_v2020_y19_txt.zip
# Downloaded July 13, 2023 (Released 9/22/2021)
# Downloaded dataset: pcen_v2020_y19.txt.zip
# Downloaded .zip file is 13 MB; unzipped is 116 MB
CDC.Pop <- read.csv("Inputs/pcen_v2020_y19.txt", header=F)

# Reading the list of counties included in our model (i.e., for the contiguous U.S.)
# This file is provided with the data deposit
STCOU.List <- read.csv("Inputs/STCOUList.csv")
names(STCOU.List) <- "STCOU"

# Extracting population by age group data based on CDC's documentation: 
# Available at: https://www.cdc.gov/nchs/data/nvss/bridged_race/Documentation-Bridged-PostcenV2020.pdf
summary(nchar(CDC.Pop$V1))
CDC.Pop$Vintage <- as.numeric(substr(CDC.Pop$V1,1,4))
CDC.Pop$Year <- as.numeric(substr(CDC.Pop$V1,5,8))
CDC.Pop$Month <- as.numeric(substr(CDC.Pop$V1,9,9))
CDC.Pop$ST <- as.numeric(substr(CDC.Pop$V1,10,11))
CDC.Pop$COUNTY <- as.numeric(substr(CDC.Pop$V1,12,14))
CDC.Pop$Age <- as.numeric(substr(CDC.Pop$V1,15,16))
CDC.Pop$Race.Sex <- as.numeric(substr(CDC.Pop$V1,17,17))
CDC.Pop$Hispanic <- as.numeric(substr(CDC.Pop$V1,18,18))
CDC.Pop$Pop <- as.numeric(substr(CDC.Pop$V1,19,26))

CDC.Pop$STCOU <- 1000*CDC.Pop$ST + CDC.Pop$COUNTY

Pop04yr <- CDC.Pop[CDC.Pop$Age %in% c(0:4),]
Pop511yr <- CDC.Pop[CDC.Pop$Age %in% c(5:11),]
Pop1217yr <- CDC.Pop[CDC.Pop$Age %in% c(12:17),]

SumPop04 <- aggregate(Pop04yr$Pop, by=list(Pop04yr$STCOU), FUN=sum)
names(SumPop04) <- c("STCOU","Pop04")

SumPop511 <- aggregate(Pop511yr$Pop, by=list(Pop511yr$STCOU), FUN=sum)
names(SumPop511) <- c("STCOU","Pop511")

SumPop1217 <- aggregate(Pop1217yr$Pop, by=list(Pop1217yr$STCOU), FUN=sum)
names(SumPop1217) <- c("STCOU","Pop1217")

Child.Pop <- merge(x=SumPop04,y=SumPop511,by="STCOU",all=TRUE)
Child.Pop <- merge(x=Child.Pop,y=SumPop1217, by="STCOU",all=TRUE)


Child.Pop <- Child.Pop[Child.Pop$STCOU %in% STCOU.List$STCOU,]
Child.Pop <- Child.Pop[order(Child.Pop$STCOU,decreasing = FALSE),]

sum(Child.Pop$STCOU == STCOU.List$STCOU)
sum(Child.Pop$STCOU != STCOU.List$STCOU)


#Asthma Incidence (Winer et al., 2012)
# R. A. Winer, X. Qin, T. Harrington, J. Moorman, H. Zahran
# Asthma Incidence among Children and Adults: Findings from the Behavioral Risk Factor Surveillance System Asthma Call-back Survey—United States, 2006–2008. 
# Journal of Asthma 49, 16-22 (2012).
# Incidence:
# 0-4: 23.4/1000
# 5-11: 11.1/1000
# 12-17: 4.4/1000

# Asthma prevalence from the U.S> CDC
# https://www.cdc.gov/asthma/most_recent_national_asthma_data.htm
# Accessed July 13, 2023
# Using data from 2019
# https://www.cdc.gov/asthma/nhis/2019/table4-1.htm
# 0-4: 2.6%
# 5-14: 9.1%
# 15-19: 7.4%
# Hence we assume 5-11 will be 9.1% and 12-17 will be (9.1+7.4)/2 = 8.25%

Child.Pop$Asthma04 <- (Child.Pop$Pop04)*(1-0.026)*0.0234
Child.Pop$Asthma511 <- (Child.Pop$Pop511)*(1-0.091)*0.0111
Child.Pop$Asthma1217 <- (Child.Pop$Pop1217)*(1-0.0825)*0.0044

Model_Inputs_Asthma04_2019 <- subset(Child.Pop,select=c("STCOU","Asthma04"))
Model_Inputs_Asthma511_2019 <- subset(Child.Pop,select=c("STCOU","Asthma511"))
Model_Inputs_Asthma1217_2019 <- subset(Child.Pop,select=c("STCOU","Asthma1217"))

Model_Inputs_Asthma_2019 <- subset(Child.Pop,select=c("STCOU","Asthma04","Asthma511","Asthma1217"))

# write.csv(Model_Inputs_Asthma04_2019, file="Outputs/Model_Inputs_Asthma04_2019.csv",row.names=FALSE)
# write.csv(Model_Inputs_Asthma511_2019, file="Outputs/Model_Inputs_Asthma511_2019.csv",row.names=FALSE)
# write.csv(Model_Inputs_Asthma1217_2019, file="Outputs/Model_Inputs_Asthma1217_2019.csv",row.names=FALSE)
# write.csv(Model_Inputs_Asthma_2019, file="Outputs/Model_Inputs_Asthma2019.csv",row.names=FALSE)

save(Model_Inputs_Asthma04_2019,
     Model_Inputs_Asthma511_2019,
     Model_Inputs_Asthma1217_2019,
     Model_Inputs_Asthma_2019,
     file="Outputs/Model_Inputs_Asthma.RData")

