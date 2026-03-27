## install.packages

library(foreign)
#install.packages("readstata13")
library(readstata13)
library(tidyverse)
library(haven)
library(dplyr)
library(car)

# Download data from http://discover.ukdataservice.ac.uk/series/?sn=200001 and extract data into the data subfolder.
## save data in folder Data on your device

# Data selection, merging and cleaning
######################################################################################
### code for birth - 0 years old - 1970 Birth
######################################################################################
## please load data from your data folder
setwd("~Data/sweep 1 birth/2666stata9_1d80ccb2df549a0194d755980e48d1f3/UKDA-2666-stata9/stata9")

d0 <- read.dta13("bcs1derived.dta")

df0age <- d0 %>%
		select(BCSID, BD1AGEFB, BD1PSOC, BD1REGN) %>%
		rename(bcsid = BCSID,
			mathernal_age_birth = BD1AGEFB, 
			social_class_parent_birth = BD1PSOC, 
			region_birth = BD1REGN) 

df0age$mathernal_age_birth <- as.numeric(df0age$mathernal_age_birth)

df0age <- df0age %>%
			mutate(maternal_age_birth = recode(mathernal_age_birth, `-8` = NaN))

d0a <- read.dta13("bcs7072a.dta")

df0ageA <- d0a %>%
		select(bcsid, a0012, a0255, a0009) %>%
		rename(married_birth = a0012, 
				  sex_birth = a0255, 
				  education_age_mother = a0009)

df0All <- merge(df0age, df0ageA, by = "bcsid", all = TRUE)

######################################################################################
### code for age 5 - 1975 age 5
######################################################################################
setwd("~Data/sweep 2 subset")
# this data file is merged from f699a.dta and f699c.dta which is sweep 2 at age 5. 
# DOI: 10.5255/UKDA-SN-2699-4
# Study Number SN: 2699
df5 <- read.dta("f699a.dta")

df5$female5 <- as.numeric(df5$d003)

df5age <- df5 %>%
		select(bcsid, female5) %>%
		mutate(female5 = recode(female5, `5` = "0",
										                 `6` = "1")) 

dfNonVerbal <- read.dta("f699c.dta")

dfNonVer <- dfNonVerbal %>%
		select(bcsid, f122) %>%
		rename(copyDesignStd = f122) 

dfNonVer$copyDesignStd <- ifelse(dfNonVer$copyDesignStd < -4.9999999, NA, dfNonVer$copyDesignStd)

######################################################################################
### code for age 26 - 1996 age 26
######################################################################################
setwd("~/Data/sweep 26/3833stata11_4dcf81f7ef82b24e1c6d4105d065e390/UKDA-3833-stata11/stata11")

df26 <- read_dta("bcs96x.dta")

myvars <- c("bcsid", "hqual26")
df26 <- df26[myvars]

df26$educ26 <- df26$hqual26
df26$educ26F <- ifelse(df26$educ26 > 2, 1,
			          ifelse(df26$educ26 == -1, 2,
			          ifelse(df26$educ26 < 3, 0, df26$educ26)))

df26$edu26 <- as.numeric(df26$educ26F)

######################################################################################
### code for age 30 - 2000 age 30
######################################################################################
setwd("~Data/sweep 30/5558stata11_se_055d3bb8b0a8713d18e9bed96b747f63/UKDA-5558-stata11_se/stata11_se")

df30 <- read_dta("bcs2000.dta")

myvars <- c("bcsid", "vote97", "politint", "unionmem", "mal01", "mal02", "mal03", "mal04", "mal05", "mal06", "mal07", "mal08", "mal09", "mal10", "mal11", "mal12", "mal13", "mal14", "mal15", "mal16", "mal17", "mal18", "mal19", "mal20", "mal21", "mal22", "mal23", "mal24", "sc", "finnow")
df30 <- df30[myvars]

df30$vote97 <- as.numeric(df30$vote97)
df30$vote97_age30 <- ifelse(df30$vote97 == 2, 0, ifelse(df30$vote97 > 2, NA, df30$vote97))

df30$trade_union30 <- ifelse(df30$unionmem < 4, 1,
						          ifelse(df30$unionmem == 4, 0,
						          ifelse(df30$unionmem > 4, NA, df30$unionmem)))

df30$mal01 <- ifelse(df30$mal01 == 2, 0, ifelse(df30$mal01 > 3, NA, df30$mal01)) 
df30$mal02 <- ifelse(df30$mal02 == 2, 0, ifelse(df30$mal02 > 3, NA, df30$mal02)) 
df30$mal03 <- ifelse(df30$mal03 == 2, 0, ifelse(df30$mal03 > 3, NA, df30$mal03)) 
df30$mal04 <- ifelse(df30$mal04 == 2, 0, ifelse(df30$mal04 > 3, NA, df30$mal04)) 
df30$mal05 <- ifelse(df30$mal05 == 2, 0, ifelse(df30$mal05 > 3, NA, df30$mal05)) 
df30$mal06 <- ifelse(df30$mal06 == 2, 0, ifelse(df30$mal06 > 3, NA, df30$mal06)) 
df30$mal07 <- ifelse(df30$mal07 == 2, 0, ifelse(df30$mal07 > 3, NA, df30$mal07)) 
df30$mal08 <- ifelse(df30$mal08 == 2, 0, ifelse(df30$mal08 > 3, NA, df30$mal08)) 
df30$mal09 <- ifelse(df30$mal09 == 2, 0, ifelse(df30$mal09 > 3, NA, df30$mal09)) 
df30$mal10 <- ifelse(df30$mal10 == 2, 0, ifelse(df30$mal10 > 3, NA, df30$mal10)) 
df30$mal11 <- ifelse(df30$mal11 == 2, 0, ifelse(df30$mal11 > 3, NA, df30$mal11)) 
df30$mal12 <- ifelse(df30$mal12 == 2, 0, ifelse(df30$mal12 > 3, NA, df30$mal12)) 
df30$mal13 <- ifelse(df30$mal13 == 2, 0, ifelse(df30$mal13 > 3, NA, df30$mal13)) 
df30$mal14 <- ifelse(df30$mal14 == 2, 0, ifelse(df30$mal14 > 3, NA, df30$mal14)) 
df30$mal15 <- ifelse(df30$mal15 == 2, 0, ifelse(df30$mal15 > 3, NA, df30$mal15)) 
df30$mal16 <- ifelse(df30$mal16 == 2, 0, ifelse(df30$mal16 > 3, NA, df30$mal16)) 
df30$mal17 <- ifelse(df30$mal17 == 2, 0, ifelse(df30$mal17 > 3, NA, df30$mal17)) 
df30$mal18 <- ifelse(df30$mal18 == 2, 0, ifelse(df30$mal18 > 3, NA, df30$mal18)) 
df30$mal19 <- ifelse(df30$mal19 == 2, 0, ifelse(df30$mal19 > 3, NA, df30$mal19)) 
df30$mal20 <- ifelse(df30$mal20 == 2, 0, ifelse(df30$mal20 > 3, NA, df30$mal20)) 
df30$mal21 <- ifelse(df30$mal21 == 2, 0, ifelse(df30$mal21 > 3, NA, df30$mal21)) 
df30$mal22 <- ifelse(df30$mal22 == 2, 0, ifelse(df30$mal22 > 3, NA, df30$mal22)) 
df30$mal23 <- ifelse(df30$mal23 == 2, 0, ifelse(df30$mal23 > 3, NA, df30$mal23)) 
df30$mal24 <- ifelse(df30$mal24 == 2, 0, ifelse(df30$mal24 > 3, NA, df30$mal24)) 

df30$malAll <- (df30$mal01 + df30$mal02 + df30$mal03 + df30$mal04 + df30$mal05 + df30$mal06 + df30$mal07 + df30$mal08 + df30$mal09 + df30$mal10 + df30$mal11 + df30$mal12 + df30$mal13 + df30$mal14 + df30$mal15 + df30$mal16 + df30$mal17 + df30$mal18 + df30$mal19 + df30$mal20 + df30$mal21 + df30$mal22 + df30$mal23 + df30$mal24)


df30$malaise30 <- ifelse(df30$malAll < 8, 0, 1)

df30$politint <- as.numeric(df30$politint)
df30$pol_int_30 <- recode(df30$politint, 	'1' = "1",
										                    	'2' = "1",
											                    '3' = "2",
											                    '4' = "2",
											                    '8' = "NA",
											                    '9' = "NA")	

df30$finnow30 <- df30$finnow
########################################################################################################
### code for 42 year old  -2012 age 42
########################################################################################################
setwd("~Data/sweep 42/7473stata11_f2070467b969740fead63b4238a3844a/UKDA-7473-stata11/stata11")

d42 <- read.dta("bcs70_2012_derived.dta")
f42 <- read.dta("bcs70_2012_flatfile.dta")

d2 <- select(d42, BCSID, BD9HACHQ)
f2 <- select(f42, BCSID, B9SCQ4, B9SCQ6, B9CLASS, B9SCQ8B)

df42 <- merge(d2, f2, by = "BCSID")

## how interested are you in politics 1-not at all and 4 very interested
df42$polInt1 <- as.numeric(df42$B9SCQ4)
df42$polInt1 <- car::recode(df42$polInt1, "'1' = 'NA'; '2' = 'NA'; '3' = 'NA'; '4' = '4'; '5' = '3'; '6' = '2'; '7' = '1'")
df42$polInt <- as.numeric(df42$polInt1)
df42$polInt1 <- NULL

## vote in 2010 elections
df42$vote42 <- car::recode(df42$B9SCQ6, "'Not Stated' = 'NA'; 'Multicode' = 'NA'; 'Paper self completion not received' = 'NA'; 'Not living in UK' = 'NA'")      

## Social Class
df42$class1 <- as.numeric(df42$B9CLASS)
df42$class1 <- car::recode(df42$class1, "'1' = 'NA'; '2' = 'NA'; '3' = 'NA'; '4' = '1'; '5' = '2'; '6' = '3'; '7' = '4'; '8' = '5'; '9' = '6'")
df42$class <- as.numeric(df42$class1)
df42$class1 <- NULL

df42$social_class42 <- df42$class

## member of trade union
df42$TradeUnion1 <- as.numeric(df42$B9SCQ8B)
df42$TradeUnion <- car::recode(df42$TradeUnion1, "'1' = 'NA'; '2' = 'NA'; '3' = '0'; '4' = '1'")
df42$TradeUnion <- as.numeric(df42$TradeUnion)
df42$TradeUnion1 <- NULL

## Education 2010 to 2012 mentioned derived
df42$education <- df42$BD9HACHQ 

## age 42 drop duplicate variabls
df42$BD9HACHQ <- NULL
df42$B9SCQ4 <- NULL
df42$B9SCQ6 <- NULL
df42$B9CLASS <- NULL
df42$B9SCQ8B <- NULL

######################################################################################
### code for 46 years old - 2016 age 46
######################################################################################

setwd("~Data/sweep 46/UKDA-8547-stata/stata11")

d46 <- read.dta13("bcs_age46_main.dta")

df46 <- select(d46, BCSID, B10CMSEX, B10VOTE01, B10VOTEWO1, B10VOTE02, B10VOTEWO2, B10VOTEFLAG, B10INCAMT)

## B10CMSEX recode to Female 1, Male 0
df46$female1 <- as.numeric(df46$B10CMSEX)
df46$female1 <- car::recode(df46$female1, "'4' = '0'; '5' = '1'")
df46$female <- as.numeric(df46$female1)
df46$female1 <- NULL

## B10VOTE01 Turnout 2015 elections - everyone asked before October 
df46$t_out51 <- as.numeric(df46$B10VOTE01)
df46$t_out51 <- car::recode(df46$t_out51, "'1' = 'NA'; '2' = 'NA'; '3' = 'NA'; '4' = '1'; '5' = '0'")
df46$t_out5 <- as.numeric(df46$t_out51)
df46$t_out51 <- NULL

## B10VOTE02 Turnout 2017 elections - everyone asked after October 
df46$t_out71 <- as.numeric(df46$B10VOTE02)
df46$t_out71 <- car::recode(df46$t_out71, "'1' = 'NA'; '2' = 'NA'; '3' = 'NA'; '4' = 'NA'; '5' = '1'; '6' = '0'")
df46$t_out7 <- as.numeric(df46$t_out71)
df46$t_out71 <- NULL

df46$vote15 <- df46$B10VOTEWO1
df46$vote17 <- df46$B10VOTEWO2

## B10VOTEFLAG people who responded on the old survey after the elections of 2017 were held 
df46$B10VOTEFLAG1 <- as.numeric(df46$B10VOTEFLAG)
df46$B10VOTEFLAG1 <- car::recode(df46$B10VOTEFLAG1, "'4' = '0'; '5' = '1'")
df46$B10VOTEFLAG <- as.numeric(df46$B10VOTEFLAG1)
df46$B10VOTEFLAG1 <- NULL

## negative vlaues are NA - not applicable - not known - refused 
df46$incomeTotal <- ifelse(df46$B10INCAMT < 0, NA, df46$B10INCAMT)


## age 46 drop duplicate variabels 
df46$B10CMSEX <- NULL
df46$B10VOTE01 <- NULL
df46$B10VOTEWO1 <- NULL
df46$B10VOTE02 <- NULL
df46$B10VOTEWO2 <- NULL
df46$B10INCAMT <- NULL

###############################################################
### Mergeing all files 
###############################################################

df4642 <- merge(df46, df42, by = "BCSID", all = TRUE)

df4642$bcsid <- df4642$BCSID

setwd("~Data")

##############################################################
## Traj file see READ ME.txt this is caluclated from STATA and the produced dataset is merged here
#############################################################
## This dataset is for conduct problems calucalted in STATA and the output merged with the full dataset. This dataset comes from traj_estimation.do file
dfcp <- read.dta13("df_traj_cond_estimated_rep.dta") 

dfcp$cond <- dfcp$"_traj_Group"

myvars2 <- c("bcsid", "cond")
cp <- dfcp[myvars2]

# change groups so it fits by level = group 1 is middle line; group 2 is the lowest line and group 3 is the highest line
cp$cond <- car::recode(cp$cond, "'1'='2'; '2' = '1'; '3' = '3'")

df <- merge(df4642, cp, by = "bcsid", all = TRUE)

dfAll5 <- merge(df, df5age, by = "bcsid", all = TRUE)

dfAll <- merge(dfAll5, df0All, by = "bcsid", all = TRUE)

dfAllNV <- merge(dfAll, dfNonVer, by = "bcsid", all = TRUE)

dfAll26 <- merge(dfAllNV, df26, by = "bcsid", all = TRUE)

dfAll30 <- merge(dfAll26, df30, by = "bcsid", all = TRUE)

df <- dfAll30

##############################################################
## Cleaning variables 
##############################################################

# Recode turnout to merge 2015 and 2017

df$t_out1 <- ifelse(is.na(df$t_out7), df$t_out5, df$t_out7)

# recode sex_birth to binary with 1 female and 0 male
df$sex_birth1 <- as.numeric(df$sex_birth)
df$sex_birth <- ifelse(df$sex_birth1 == 4, 0, ifelse(df$sex_birth1 == 5, 1, NA)) 
df$sex_birth1 <- NULL

df$sex <- ifelse(is.na(df$sex_birth), df$female, df$sex_birth)

# recode married_birth into categories where married is category one and single, widowed, 
# divorced, separated is 0. 

df$married1 <- as.numeric(df$married_birth)
df$marriedN <- ifelse(df$married1 == 1, NA, ifelse(df$married1 == 3, 1, 0))
df$married1 <- NULL

df$married2 <- as.factor(df$marriedN)

df <- df %>%
      mutate(married = case_when(
                married2 == '1' ~ "Married",
                married2 == '0' ~ "Single"                
      ))
df$married <- as.factor(df$married)

# maternal_age_at_birth -8 should be NA
df$maternal_age_birth1 <- ifelse(df$maternal_age_birth == -8, NA, df$maternal_age_birth)

# missing values of mother_education_age transered to NA
df$education_age_mother <- ifelse(df$education_age_mother == -2, NA, 
                           ifelse(df$education_age_mother == -3, NA, 
                           ifelse(df$education_age_mother == 97, NA, df$education_age_mother)))

# categorizing social_class_parent_birth to merge similar categories
df <- df %>%
    mutate(social_class_parent_birthCat = case_when(
            social_class_parent_birth == 'nk/ns' ~ "Not working/Other",
            social_class_parent_birth == 'Other category' ~ "Not working/Other",
            social_class_parent_birth == 'single parent - not working' ~ "Not working/Other",
            social_class_parent_birth == 'V unskilled' ~ "Unskilled/partly-skilled", 
            social_class_parent_birth == 'IV partly-skilled' ~ "Unskilled/partly-skilled",
            social_class_parent_birth == 'III manual' ~ "Non-manual/manual",
            social_class_parent_birth == 'III non manual' ~ "Non-manual/manual",
            social_class_parent_birth == 'II managerial and Technical' ~ "Managerial/Professional",
            social_class_parent_birth == 'I professional' ~ "Managerial/Professional"))
df$social_class_parent_birthCat <- ifelse(is.na(df$social_class_parent_birthCat), 
                                                  "Not working/Other", df$social_class_parent_birthCat)
df$social_class_parent_birthCat <- as.factor(df$social_class_parent_birthCat)
df$social_class_parent_birthCat <- factor(df$social_class_parent_birthCat, 
                                levels = c("Managerial/Professional", "Non-manual/manual", 
                                           "Unskilled/partly-skilled", "Not working/Other"))

# categorizing region at birth
df <- df %>%
    mutate(region_birthCat = case_when(
            region_birth == 'North' ~ "England",
            region_birth == 'Yorks and Humberside' ~ "England",
            region_birth == 'East Midlands' ~ "England",
            region_birth == 'East Anglia' ~ "England", 
            region_birth == 'South East' ~ "England",
            region_birth == 'South West' ~ "England",
            region_birth == 'West Midlands' ~ "England",
            region_birth == 'North West' ~ "England", 
            region_birth == 'Wales' ~ "Wales", 
            region_birth == 'Scotland' ~ "Scotland",
            region_birth == 'Northern Ireland' ~ "Northern Ireland"
            ))
df$region_birthCat <- ifelse(is.na(df$region_birthCat), 
                      "Missing Info", df$region_birthCat)
df$region_birthCat <- as.factor(df$region_birthCat)
df$region_birthCat <- factor(df$region_birthCat, 
                                levels = c("England", "Wales", "Scotland", 
                                           "Missing Info"))


# Armed Services|Unknown|North|Yorks and Humberside|East Midlands 
# 0              0       1023  1486                 1036 
# East Anglia|South East|South West|West Midlands|North West 
# 539         5022       1051       1745          2170 
# Wales|Scotland|Northern Ireland|Overseas 
# 879   1617     0              0 


df$education <- as.character(df$education)

# include NAs as a category of 'Not enough information'
df$education1 <- ifelse(is.na(df$education), "Not enough information", df$education)  

df$educationCat1[df$education1 == 'Not enough information'] <- "Non respondents"
df$educationCat1[df$education1 == 'no academic qualification'] <- "low"
df$educationCat1[df$education1 == 'gcse d-e'] <- "low"
df$educationCat1[df$education1 == 'cses2-5, other scottish quals'] <- "low"
df$educationCat1[df$education1 == 'gcse a-c, good o levels scot standards'] <- "low"
df$educationCat1[df$education1 == 'as levels or 1 a level'] <- "high"
df$educationCat1[df$education1 == '2+ a levels, scot higher/6th'] <- "high"
df$educationCat1[df$education1 == 'diploma'] <- "high"
df$educationCat1[df$education1 == 'degree level'] <- "high"
df$educationCat1[df$education1 == 'higher degree'] <- "high"

df$educationCat1 <- factor(df$educationCat1, levels = c("low", "high", "Non respondents"))

#Not enough information   not applicable                no academic qualification 
#                     0                0                                     2895 
#gcse d-e   cses2-5, other scottish quals   gcse a-c, good o levels scot standards 
#      45                             606                                     2433 
#as levels or 1 a level   2+ a levels, scot higher/6th                     diploma 
#                   184                            356                         825 
#                          degree level                          higher degree 
#                                  2018                                    479 

df <- df %>%
    mutate(polIntCat = case_when(polInt == 1 ~ "Not Interested",
                                 polInt == 2 ~ "Not Interested",
                                 polInt == 3 ~ "Interested",
                                 polInt == 4 ~ "Interested"))
df$polIntCat <- ifelse(is.na(df$polIntCat), "Info Missing", df$polIntCat)
df$polIntCat <- as.factor(df$polIntCat)
df$polIntCat <- factor(df$polIntCat, levels = c("Not Interested","Interested", "Info Missing"))


df <- df %>%
  mutate(TradeUnion1 = case_when(TradeUnion == 1 ~ "Member",
                                 TradeUnion == 0 ~ "Not a Member")) # Change values to characters 

# Add missing values as a category
df$TradeUnion1 <- ifelse(is.na(df$TradeUnion1), "Info Missing", df$TradeUnion1)  

# Transfer to factors, so you can assign levels in order that better fits for analysis
df$TradeUnion1 <- as.factor(df$TradeUnion1) 
df$TradeUnion1 <- factor(df$TradeUnion1, levels = c("Not a Member", "Member", "Info Missing"))

df <- df %>%
    mutate(vote42a = case_when(vote42 == "British National Party/BNP" ~ "1",
                                 vote42 == "Christian Party" ~ "1",
                                 vote42 == "Conservative" ~ "1",
                                 vote42 == "Defaced/spoilt ballot paper" ~ "1", 
                                 vote42 == "Did not vote" ~ "0", 
                                 vote42 == "Do not wish to disclose/answer" ~ "NA", 
                                 vote42 == "Don't know/Can't remember" ~ "NA",
                                 vote42 == "Green Party" ~ "1",
                                 vote42 == "Labour" ~ "1", 
                                 vote42 == "Liberal Democrats" ~ "1", 
                                 vote42 == " Local/Independent candidate" ~ "1", 
                                 vote42 == "NA" ~ "NA",
                                 vote42 == "Other" ~ "1", 
                                 vote42 == "Plaid Cymru" ~ "1", 
                                 vote42 == "Respect" ~ "1", 
                                 vote42 == "Scottish National Party" ~ "1",
                                 vote42 == "Scottish Socialist" ~ "1", 
                                 vote42 == "UK Independence Party" ~ "1"
                                ))
df$vote42b <- as.numeric(df$vote42a)

## Variables to be used in Analysis.do

dfVars <- c("t_out1", "t_out5", "vote42b", "vote97_age30", "cond", "sex", "educationCat1", "TradeUnion1", "polIntCat", "malaise30", "incomeTotal", "maternal_age_birth1", "married", "region_birthCat", "copyDesignStd", "education_age_mother", "social_class_parent_birthCat", "social_class42", "edu26", "trade_union30", "pol_int_30", "finnow30")

df_rep <- df[dfVars]

## Setting work directory in new folder Replication files where data will be stored 
setwd("~/Replication files")

## data for analysis
write.dta(df_rep, "df_rep.dta")



