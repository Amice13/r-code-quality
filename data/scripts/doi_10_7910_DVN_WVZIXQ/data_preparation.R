# Title -------------------------------------------------------------------

# Replication script for "How to distinguish human error from election fraud: Evidence from the 2019 Malawi election"   
# Authors Johan Ahlback and Ryan Jablonski               


# Description -------------------------------------------------------------

# Use this script to prepare data for replication of tables and figures presented in sections 4-6 and online appendix
# The files needed for this replication file include: edits.anon.csv, pstream.anon.csv and ethnic_groups.csv


# Library packages --------------------------------------------------------

library(readr)
library(ggplot2)
library(stargazer)
library(lfe)
library(rdrobust)
library(rddensity)
library(gridExtra)
library(xtable)
library(plyr)
library(MCMCpack)
library(tidyverse)
library(dotwhisker)
library(modelsummary)


# Load data ---------------------------------------------------------------

# set working directory accordingly

edits <- read_csv("edits.anon.csv")
pstream2 <- read_csv("pstream.anon.csv")
ethnic_groups=read.csv("ethnic_groups.csv") #Ethnic groups from 2018 census https://malawi.unfpa.org/sites/default/files/resource-pdf/2018%20Malawi%20Population%20and%20Housing%20Census%20Main%20Report%20(1).pdf

# Prepare data ------------------------------------------------------------

# prepare some variables (in 0-100)
edits$turnout2014 <- as.numeric(edits$turnout2014)
edits$dpp_share2014 <- as.numeric(edits$dpp_share2014)
edits$female <- 100*edits$female
edits$youth <- 100*edits$youth
edits$pop_density14a <- 100*(edits$pop_density14 - min(edits$pop_density14, na.rm = T)) / 
  (max(edits$pop_density14, na.rm = T) - min(edits$pop_density14, na.rm = T)) # convert to 0-100
edits$poverty11a <- 100*(edits$poverty11 - min(edits$poverty11, na.rm = T)) / 
  (max(edits$poverty11, na.rm = T) - min(edits$poverty11, na.rm = T)) # convert to 0-100
edits$elevation100 <- 100*(edits$elevation - min(edits$elevation, na.rm = T)) / 
  (max(edits$elevation, na.rm = T) - min(edits$elevation, na.rm = T)) # convert to 0-100
edits$dep_ratio100 <- 100*(edits$dep_ratio - min(edits$dep_ratio, na.rm = T)) / 
  (max(edits$dep_ratio, na.rm = T) - min(edits$dep_ratio, na.rm = T)) # convert to 0-100
edits$nightlights_2014a <- 100*(edits$nightlights_2014 - min(edits$nightlights_2014, na.rm = T)) / 
  (max(edits$nightlights_2014, na.rm = T) - min(edits$nightlights_2014, na.rm = T)) # convert to 0-100


# create a variable for edits in candidate figures (cand_alt)
edits$cand_alt <- ifelse((edits$pres_1 + edits$pres_2 + edits$pres_3 + edits$pres_4 + edits$pres_5 + edits$pres_6 + edits$pres_7)>0,1,0)

# prepare ethnic groups data
ethnic_groups$location=ifelse(ethnic_groups$location=="Chikwawa", "Chikhwawa", ethnic_groups$location)
ethnic_groups$location=ifelse(ethnic_groups$location=="Nkhata Bay", "Nkhatabay", ethnic_groups$location)
for(i in c(2:15)){
  ethnic_groups[,i]=as.numeric(gsub(",","",ethnic_groups[,i]))
}
ethnic_groups$group_max=apply(ethnic_groups[,3:15],1,max,na.rm=TRUE)
ethnic_groups$largest_ethnic_group=NA
for(this.group in c(names(ethnic_groups)[3:15])){
  ethnic_groups$largest_ethnic_group=ifelse(ethnic_groups$group_max==ethnic_groups[,this.group], this.group, ethnic_groups$largest_ethnic_group)
}

edits$largest_ethnic_group=ethnic_groups[match(edits$district, ethnic_groups$location), "largest_ethnic_group"]


#Add district level results from 2014 election
#From https://www.eeas.europa.eu/node/23926_en and https://mec.org.mw/2014-tripartite-elections/

edits$district_2014_DPP=NA
edits$district_2014_MCP=NA
edits$district_2014_PP=NA
edits$district_2014_UDF=NA

edits$district_2014_winner=NA
i=1
edits$district_2014_DP=ifelse(edits$district_code==i,0.39,edits$district_2014_DPP)
edits$district_2014_MCP=ifelse(edits$district_code==i,0.10,edits$district_2014_MCP)
edits$district_2014_PP=ifelse(edits$district_code==i,0.45,edits$district_2014_PP)
edits$district_2014_UDF=ifelse(edits$district_code==i,0.02,edits$district_2014_UDF)
edits$district_2014_winner=ifelse(edits$district_code==i,"PP",edits$district_2014_winner)

i=2
edits$district_2014_DPP=ifelse(edits$district_code==i,0.22,edits$district_2014_DPP)
edits$district_2014_MCP=ifelse(edits$district_code==i,0.22,edits$district_2014_MCP)
edits$district_2014_PP=ifelse(edits$district_code==i,0.52,edits$district_2014_PP)
edits$district_2014_UDF=ifelse(edits$district_code==i,0.03,edits$district_2014_UDF)
edits$district_2014_winner=ifelse(edits$district_code==i,"PP",edits$district_2014_winner)

i=3
edits$district_2014_DPP=ifelse(edits$district_code==i,0.08,edits$district_2014_DPP)
edits$district_2014_MCP=ifelse(edits$district_code==i,0.14,edits$district_2014_MCP)
edits$district_2014_PP=ifelse(edits$district_code==i,0.74,edits$district_2014_PP)
edits$district_2014_UDF=ifelse(edits$district_code==i,0.03,edits$district_2014_UDF)
edits$district_2014_winner=ifelse(edits$district_code==i,"PP",edits$district_2014_winner)

i=4
edits$district_2014_DPP=ifelse(edits$district_code==i,0.25,edits$district_2014_DPP)
edits$district_2014_MCP=ifelse(edits$district_code==i,0.07,edits$district_2014_MCP)
edits$district_2014_PP=ifelse(edits$district_code==i,0.64,edits$district_2014_PP)
edits$district_2014_UDF=ifelse(edits$district_code==i,0.03,edits$district_2014_UDF)
edits$district_2014_winner=ifelse(edits$district_code==i,"PP",edits$district_2014_winner)

i=5
edits$district_2014_DPP=ifelse(edits$district_code==i,0.42,edits$district_2014_DPP)
edits$district_2014_MCP=ifelse(edits$district_code==i,0.20,edits$district_2014_MCP)
edits$district_2014_PP=ifelse(edits$district_code==i,0.27,edits$district_2014_PP)
edits$district_2014_UDF=ifelse(edits$district_code==i,0.10,edits$district_2014_UDF)
edits$district_2014_winner=ifelse(edits$district_code==i,"DPP",edits$district_2014_winner)

i=6
edits$district_2014_DPP=ifelse(edits$district_code==i,0.20,edits$district_2014_DPP)
edits$district_2014_MCP=ifelse(edits$district_code==i,0.20,edits$district_2014_MCP)
edits$district_2014_PP=ifelse(edits$district_code==i,0.55,edits$district_2014_PP)
edits$district_2014_UDF=ifelse(edits$district_code==i,0.03,edits$district_2014_UDF)
edits$district_2014_winner=ifelse(edits$district_code==i,"PP",edits$district_2014_winner)

i=7
edits$district_2014_DPP=ifelse(edits$district_code==i,0.16,edits$district_2014_DPP)
edits$district_2014_MCP=ifelse(edits$district_code==i,0.60,edits$district_2014_MCP)
edits$district_2014_PP=ifelse(edits$district_code==i,0.20,edits$district_2014_PP)
edits$district_2014_UDF=ifelse(edits$district_code==i,0.03,edits$district_2014_UDF)
edits$district_2014_winner=ifelse(edits$district_code==i,"MCP",edits$district_2014_winner)

i=8
edits$district_2014_DPP=ifelse(edits$district_code==i,0.26,edits$district_2014_DPP)
edits$district_2014_MCP=ifelse(edits$district_code==i,0.33,edits$district_2014_MCP)
edits$district_2014_PP=ifelse(edits$district_code==i,0.25,edits$district_2014_PP)
edits$district_2014_UDF=ifelse(edits$district_code==i,0.14,edits$district_2014_UDF)
edits$district_2014_winner=ifelse(edits$district_code==i,"MCP",edits$district_2014_winner)

i=9
edits$district_2014_DPP=ifelse(edits$district_code==i,0.18,edits$district_2014_DPP)
edits$district_2014_MCP=ifelse(edits$district_code==i,0.60,edits$district_2014_MCP)
edits$district_2014_PP=ifelse(edits$district_code==i,0.18,edits$district_2014_PP)
edits$district_2014_UDF=ifelse(edits$district_code==i,0.01,edits$district_2014_UDF)
edits$district_2014_winner=ifelse(edits$district_code==i,"MCP",edits$district_2014_winner)

i=10
edits$district_2014_DPP=ifelse(edits$district_code==i,0.08,edits$district_2014_DPP)
edits$district_2014_MCP=ifelse(edits$district_code==i,0.79,edits$district_2014_MCP)
edits$district_2014_PP=ifelse(edits$district_code==i,0.10,edits$district_2014_PP)
edits$district_2014_UDF=ifelse(edits$district_code==i,0.02,edits$district_2014_UDF)
edits$district_2014_winner=ifelse(edits$district_code==i,"MCP",edits$district_2014_winner)

i=11
edits$district_2014_DPP=ifelse(edits$district_code==i,0.17,edits$district_2014_DPP)
edits$district_2014_MCP=ifelse(edits$district_code==i,0.38,edits$district_2014_MCP)
edits$district_2014_PP=ifelse(edits$district_code==i,0.23,edits$district_2014_PP)
edits$district_2014_UDF=ifelse(edits$district_code==i,0.20,edits$district_2014_UDF)
edits$district_2014_winner=ifelse(edits$district_code==i,"MCP",edits$district_2014_winner)

i=12
edits$district_2014_DPP=ifelse(edits$district_code==i,0.12,edits$district_2014_DPP)
edits$district_2014_MCP=ifelse(edits$district_code==i,0.70,edits$district_2014_MCP)
edits$district_2014_PP=ifelse(edits$district_code==i,0.13,edits$district_2014_PP)
edits$district_2014_UDF=ifelse(edits$district_code==i,0.04,edits$district_2014_UDF)
edits$district_2014_winner=ifelse(edits$district_code==i,"MCP",edits$district_2014_winner)

i=13
edits$district_2014_DPP=ifelse(edits$district_code==i,0.18,edits$district_2014_DPP)
edits$district_2014_MCP=ifelse(edits$district_code==i,0.66,edits$district_2014_MCP)
edits$district_2014_PP=ifelse(edits$district_code==i,0.07,edits$district_2014_PP)
edits$district_2014_UDF=ifelse(edits$district_code==i,0.08,edits$district_2014_UDF)
edits$district_2014_winner=ifelse(edits$district_code==i,"MCP",edits$district_2014_winner)

i=14
edits$district_2014_DPP=ifelse(edits$district_code==i,0.10,edits$district_2014_DPP)
edits$district_2014_MCP=ifelse(edits$district_code==i,0.66,edits$district_2014_MCP)
edits$district_2014_PP=ifelse(edits$district_code==i,0.13,edits$district_2014_PP)
edits$district_2014_UDF=ifelse(edits$district_code==i,0.10,edits$district_2014_UDF)
edits$district_2014_winner=ifelse(edits$district_code==i,"MCP",edits$district_2014_winner)

i=15
edits$district_2014_DPP=ifelse(edits$district_code==i,0.69,edits$district_2014_DPP)
edits$district_2014_MCP=ifelse(edits$district_code==i,0.09,edits$district_2014_MCP)
edits$district_2014_PP=ifelse(edits$district_code==i,0.10,edits$district_2014_PP)
edits$district_2014_UDF=ifelse(edits$district_code==i,0.09,edits$district_2014_UDF)
edits$district_2014_winner=ifelse(edits$district_code==i,"DPP",edits$district_2014_winner)

i=16
edits$district_2014_DPP=ifelse(edits$district_code==i,0.18,edits$district_2014_DPP)
edits$district_2014_MCP=ifelse(edits$district_code==i,0.02,edits$district_2014_MCP)
edits$district_2014_PP=ifelse(edits$district_code==i,0.14,edits$district_2014_PP)
edits$district_2014_UDF=ifelse(edits$district_code==i,0.63,edits$district_2014_UDF)
edits$district_2014_winner=ifelse(edits$district_code==i,"UDF",edits$district_2014_winner)

i=17
edits$district_2014_DPP=ifelse(edits$district_code==i,0.43,edits$district_2014_DPP)
edits$district_2014_MCP=ifelse(edits$district_code==i,0.02,edits$district_2014_MCP)
edits$district_2014_PP=ifelse(edits$district_code==i,0.16,edits$district_2014_PP)
edits$district_2014_UDF=ifelse(edits$district_code==i,0.38,edits$district_2014_UDF)
edits$district_2014_winner=ifelse(edits$district_code==i,"DPP",edits$district_2014_winner)

i=18
edits$district_2014_DPP=ifelse(edits$district_code==i,0.19,edits$district_2014_DPP)
edits$district_2014_MCP=ifelse(edits$district_code==i,0.01,edits$district_2014_MCP)
edits$district_2014_PP=ifelse(edits$district_code==i,0.26,edits$district_2014_PP)
edits$district_2014_UDF=ifelse(edits$district_code==i,0.51,edits$district_2014_UDF)
edits$district_2014_winner=ifelse(edits$district_code==i,"UDF",edits$district_2014_winner)

i=19
edits$district_2014_DPP=ifelse(edits$district_code==i,0.48,edits$district_2014_DPP)
edits$district_2014_MCP=ifelse(edits$district_code==i,0.04,edits$district_2014_MCP)
edits$district_2014_PP=ifelse(edits$district_code==i,0.28,edits$district_2014_PP)
edits$district_2014_UDF=ifelse(edits$district_code==i,0.20,edits$district_2014_UDF)
edits$district_2014_winner=ifelse(edits$district_code==i,"UDF",edits$district_2014_winner)

#obtained from https://mec.org.mw/2014-tripartite-elections/ as missing in the EO report
i=20
edits$district_2014_DPP=ifelse(edits$district_code==i,0.74,edits$district_2014_DPP)
edits$district_2014_MCP=ifelse(edits$district_code==i,0.02,edits$district_2014_MCP)
edits$district_2014_PP=ifelse(edits$district_code==i,0.08,edits$district_2014_PP)
edits$district_2014_UDF=ifelse(edits$district_code==i,0.14,edits$district_2014_UDF)
edits$district_2014_winner=ifelse(edits$district_code==i,"DPP",edits$district_2014_winner)

#obtained from https://mec.org.mw/2014-tripartite-elections/ as erroneous in the EO report
i=21
edits$district_2014_DPP=ifelse(edits$district_code==i,0.61,edits$district_2014_DPP)
edits$district_2014_MCP=ifelse(edits$district_code==i,0.07,edits$district_2014_MCP)
edits$district_2014_PP=ifelse(edits$district_code==i,0.11,edits$district_2014_PP)
edits$district_2014_UDF=ifelse(edits$district_code==i,0.19,edits$district_2014_UDF)
edits$district_2014_winner=ifelse(edits$district_code==i,"DPP",edits$district_2014_winner)

i=22
edits$district_2014_DPP=ifelse(edits$district_code==i,0.60,edits$district_2014_DPP)
edits$district_2014_MCP=ifelse(edits$district_code==i,0.03,edits$district_2014_MCP)
edits$district_2014_PP=ifelse(edits$district_code==i,0.22,edits$district_2014_PP)
edits$district_2014_UDF=ifelse(edits$district_code==i,0.08,edits$district_2014_UDF)
edits$district_2014_winner=ifelse(edits$district_code==i,"DPP",edits$district_2014_winner)

i=23
edits$district_2014_DPP=ifelse(edits$district_code==i,0.68,edits$district_2014_DPP)
edits$district_2014_MCP=ifelse(edits$district_code==i,0.03,edits$district_2014_MCP)
edits$district_2014_PP=ifelse(edits$district_code==i,0.18,edits$district_2014_PP)
edits$district_2014_UDF=ifelse(edits$district_code==i,0.05,edits$district_2014_UDF)
edits$district_2014_winner=ifelse(edits$district_code==i,"DPP",edits$district_2014_winner)

i=24
edits$district_2014_DPP=ifelse(edits$district_code==i,0.91,edits$district_2014_DPP)
edits$district_2014_MCP=ifelse(edits$district_code==i,0.01,edits$district_2014_MCP)
edits$district_2014_PP=ifelse(edits$district_code==i,0.09,edits$district_2014_PP)
edits$district_2014_UDF=ifelse(edits$district_code==i,0.06,edits$district_2014_UDF)
edits$district_2014_winner=ifelse(edits$district_code==i,"DPP",edits$district_2014_winner)

i=25
edits$district_2014_DPP=ifelse(edits$district_code==i,0.79,edits$district_2014_DPP)
edits$district_2014_MCP=ifelse(edits$district_code==i,0.01,edits$district_2014_MCP)
edits$district_2014_PP=ifelse(edits$district_code==i,0.11,edits$district_2014_PP)
edits$district_2014_UDF=ifelse(edits$district_code==i,0.07,edits$district_2014_UDF)
edits$district_2014_winner=ifelse(edits$district_code==i,"DPP",edits$district_2014_winner)

i=26
edits$district_2014_DPP=ifelse(edits$district_code==i,0.82,edits$district_2014_DPP)
edits$district_2014_MCP=ifelse(edits$district_code==i,0.01,edits$district_2014_MCP)
edits$district_2014_PP=ifelse(edits$district_code==i,0.09,edits$district_2014_PP)
edits$district_2014_UDF=ifelse(edits$district_code==i,0.06,edits$district_2014_UDF)
edits$district_2014_winner=ifelse(edits$district_code==i,"DPP",edits$district_2014_winner)

i=27
edits$district_2014_DPP=ifelse(edits$district_code==i,0.69,edits$district_2014_DPP)
edits$district_2014_MCP=ifelse(edits$district_code==i,0.03,edits$district_2014_MCP)
edits$district_2014_PP=ifelse(edits$district_code==i,0.16,edits$district_2014_PP)
edits$district_2014_UDF=ifelse(edits$district_code==i,0.09,edits$district_2014_UDF)
edits$district_2014_winner=ifelse(edits$district_code==i,"DPP",edits$district_2014_winner)

i=28
edits$district_2014_DPP=ifelse(edits$district_code==i,0.65,edits$district_2014_DPP)
edits$district_2014_MCP=ifelse(edits$district_code==i,0.03,edits$district_2014_MCP)
edits$district_2014_PP=ifelse(edits$district_code==i,0.18,edits$district_2014_PP)
edits$district_2014_UDF=ifelse(edits$district_code==i,0.11,edits$district_2014_UDF)
edits$district_2014_winner=ifelse(edits$district_code==i,"DPP",edits$district_2014_winner)

edits$dpp14=ifelse(edits$district_2014_winner=="DPP",1, 0)
edits$mcp14=ifelse(edits$district_2014_winner=="MCP",1, 0)



# MEC and NICe data
edits$nice_chakwera <- as.numeric(edits$nice_chakwera)
edits$nice_chilima <- as.numeric(edits$nice_chilima)
edits$nice_chisi <- as.numeric(edits$nice_chisi)
edits$nice_kaliya <- as.numeric(edits$nice_kaliya)
edits$nice_kuwani <- as.numeric(edits$nice_kuwani)
edits$nice_muluzi <- as.numeric(edits$nice_muluzi)
edits$nice_mutharika <- as.numeric(edits$nice_mutharika)

edits$nice_votes=rowSums(edits[,c("nice_chakwera", "nice_mutharika",  "nice_chilima", "nice_chisi", "nice_kaliya", "nice_kuwani", "nice_muluzi")], na.rm=T)
edits$nice_votes=ifelse(edits$nice_votes<1, NA, edits$nice_votes)

# MEC and NICE data: Raw vote differences
edits$mutharika_difference <- edits$mec_mutharika - edits$nice_mutharika
edits$chakwera_difference <- edits$mec_chakwera - edits$nice_chakwera
edits$chilima_difference <- edits$mec_chilima - edits$nice_chilima

# MEC and NICE data: Differences in vote-shares
edits$nice_mutharika_percent_mec = (edits$nice_mutharika/edits$mec_valid)*100
edits$nice_chakwera_percent_mec = (edits$nice_chakwera/edits$mec_valid)*100
edits$nice_chilima_percent_mec = (edits$nice_chilima/edits$mec_valid)*100

edits$mec_mutharika_percent_mec = (edits$mec_mutharika/edits$mec_valid)*100
edits$mec_chakwera_percent_mec = (edits$mec_chakwera/edits$mec_valid)*100
edits$mec_chilima_percent_mec = (edits$mec_chilima/edits$mec_valid)*100


edits$nice_mutharika_percent = (edits$nice_mutharika/edits$nice_votes)*100
edits$nice_chakwera_percent = (edits$nice_chakwera/edits$nice_votes)*100
edits$nice_chilima_percent = (edits$nice_chilima/edits$nice_votes)*100

edits$mec_mutharika_percent = (edits$mec_mutharika/edits$nice_votes)*100
edits$mec_chakwera_percent = (edits$mec_chakwera/edits$nice_votes)*100
edits$mec_chilima_percent = (edits$mec_chilima/edits$nice_votes)*100

edits$mutharika_difference_percent <- edits$mec_mutharika_percent - edits$nice_mutharika_percent
edits$chakwera_difference_percent <- edits$mec_chakwera_percent - edits$nice_chakwera_percent
edits$chilima_difference_percent <- edits$mec_chilima_percent - edits$nice_chilima_percent

# Turnout and invalidation

edits$turnout <- 100*(edits$mec_total / edits$registered)
edits$turnout[edits$turnout>100]<-NA # remove in case above 100 (1 observation)
edits$invalidation <- 100*(edits$mec_null_void / edits$mec_total) 
edits$invalidation[edits$invalidation>100]<-NA # remove in case above 100 (3 observations)


### comparison with 2020 votes

# Dependent varialbes
edits$mutharika_2020_difference <- edits$mutharika_2020 - edits$mec_mutharika
edits$chakwera_2020_difference <- edits$chakwera_2020 - edits$mec_chakwera
edits$joint_2020_difference <- edits$chakwera_2020 - (edits$mec_chakwera + edits$mec_chilima)


