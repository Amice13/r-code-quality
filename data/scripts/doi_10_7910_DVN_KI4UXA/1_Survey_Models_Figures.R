# This R script contains the code to replicate the CCES survey-level analysis of the PRQ Publication: "The Role of State & National Institutional Evaluations in Fostering Collective Accountability Across the U.S. States"

# This R replication file reproduces: 

# Figure 6: Relationship Between Institutional Evaluations & State Legislative Choice: (a) Presidential Approval

# Figure 6: Relationship Between Institutional Evaluations & State Legislative Choice: (b) Gubernatorial Approval

# Figure 6: Relationship Between Institutional Evaluations & State Legislative Choice: (c) Congressional Approval

# Figure 6: Relationship Between Institutional Evaluations & State Legislative Choice: (d) State Legislative Approval

# Figure 7: Distribution of Significant Executive Effects on State Legislative Vote

##### Load Relevant Packages #####

library(readstata13)
library(dplyr)
library(plyr)
library(xlsx)
library(descr)
library(data.table)
library(ggplot2)
library(haven)

setwd("") # Set the working directory on local machine with the appropriate source data files

options(scipen=999)

##### 1) Import the Spreadsheet with the Variables of Interest & Raw CCES Data #####

# Import the spreadsheet with the variables of interest.

cces_variables <- read.csv("cces_variables_individual_level.csv")

for(i in 1:ncol(cces_variables)){
  cces_variables[,i] <- trimws(cces_variables[,i])
  cces_variables[,i] <- ifelse(  cces_variables[,i] %in% "",NA,  cces_variables[,i])
}

# Import the CCES Cross-Sectional Surveys

cces_datasets <- list()
for(i in c(2006,2007,2008,2009,2010,2011,2012,2013,2014,2015,2016,2017,2018,2019,2020)){
  cces <- read.dta13(paste("CCES",i,".dta",sep=""),convert.factors = T, convert.underscore = FALSE, encoding ="UTF-8")
  cces$year <- i
  cces_datasets[[i]] <- cces
  assign(paste("cces", i, sep = "_"), cces_datasets[[i]])
  rm(cces)
}
rm(cces_2006,cces_2007,cces_2008,cces_2009,cces_2010,cces_2011,cces_2012,cces_2013,cces_2014,cces_2015,cces_2016,cces_2017,cces_2018,cces_2019,cces_2020)

cces_variables$state_leg_approval[cces_variables$state_leg_approval == ""] <- NA

# Subset the data based on the specified variables in the spreadsheet
cces_subsets <- list()
for(i in c(2006,2007,2008,2009,2010,2011,2012,2013,2014,2015,2016,2017,2018,2019,2020)){
  cces_vars <- subset(cces_variables,cces_variables$cces_dataset == paste("cces",i,sep=""))
  cces_vars$cces_dataset <- NULL
  cces_vars$year <- "year"
  cces_vars <- cces_vars[,!sapply(cces_vars,function(x) any(is.na(x)))]
  cces_vars_cols <- colnames(cces_vars)
  cces_vars <- as.character(cces_vars[1,])
  cces <- cces_datasets[[i]]
  names.use <- names(cces)[(names(cces) %in% cces_vars)]
  cces <- cces[, cces_vars]
  colnames(cces) <- cces_vars_cols
  #cces %>% mutate_if(is.factor, as.character) -> cces  #Convert factors to characters
  cces_subsets[[i]] <- cces
  assign(paste("cces", i, sep = "_"), cces_subsets[[i]])
}
cces_subsets2 <- list()
for(i in c(2006,2007,2008,2009,2010,2011,2012,2013,2014,2015,2016,2017,2018,2019,2020)){
  cces <- cces_subsets[[i]]
  cces %>% mutate_if(is.numeric, as.character) -> cces  #Convert numeric to characters
  cces_subsets2[[i]] <- cces
  assign(paste("cces", i, sep = "_"), cces_subsets2[[i]])
}

cces <- bind_rows(cces_2006,cces_2007,cces_2008,cces_2009,cces_2010,cces_2011,cces_2012,cces_2013,cces_2014,cces_2015,cces_2016,cces_2017,cces_2018,cces_2019,cces_2020)

rm(cces_datasets,cces_subsets,cces_subsets2)

##### 2) Clean Up CCES State & District Indicators #####

# Fix the states

cces$state[cces$state=="Alabama"] <- "AL"
cces$state[cces$state=="alabama"] <- "AL"
cces$state[cces$state=="Alaska"] <- "AK"
cces$state[cces$state=="alaska"] <- "AK"
cces$state[cces$state=="Arizona"] <- "AZ"
cces$state[cces$state=="arizona"] <- "AZ"
cces$state[cces$state=="Arkansas"] <- "AR"
cces$state[cces$state=="arkansas"] <- "AR"
cces$state[cces$state=="California"] <- "CA"
cces$state[cces$state=="california"] <- "CA"
cces$state[cces$state=="Colorado"] <- "CO"
cces$state[cces$state=="colorado"] <- "CO"
cces$state[cces$state=="Connecticut"] <- "CT"
cces$state[cces$state=="connecticut"] <- "CT"
cces$state[cces$state=="Delaware"] <- "DE"
cces$state[cces$state=="delaware"] <- "DE"
cces$state[cces$state=="Florida"] <- "FL"
cces$state[cces$state=="florida"] <- "FL"
cces$state[cces$state=="Georgia"] <- "GA"
cces$state[cces$state=="georgia"] <- "GA"
cces$state[cces$state=="Hawaii"] <- "HI"
cces$state[cces$state=="hawaii"] <- "HI"
cces$state[cces$state=="Idaho"] <- "ID"
cces$state[cces$state=="idaho"] <- "ID"
cces$state[cces$state=="Illinois"] <- "IL"
cces$state[cces$state=="illinois"] <- "IL"
cces$state[cces$state=="Indiana"] <- "IN"
cces$state[cces$state=="indiana"] <- "IN"
cces$state[cces$state=="Iowa"] <- "IA"
cces$state[cces$state=="iowa"] <- "IA"
cces$state[cces$state=="Kansas"] <- "KS"
cces$state[cces$state=="kansas"] <- "KS"
cces$state[cces$state=="Kentucky"] <- "KY"
cces$state[cces$state=="kentucky"] <- "KY"
cces$state[cces$state=="Louisiana"] <- "LA"
cces$state[cces$state=="louisiana"] <- "LA"
cces$state[cces$state=="Maine"] <- "ME"
cces$state[cces$state=="maine"] <- "ME"
cces$state[cces$state=="Maryland"] <- "MD"
cces$state[cces$state=="maryland"] <- "MD"
cces$state[cces$state=="Massachusetts"] <- "MA"
cces$state[cces$state=="massachusetts"] <- "MA"
cces$state[cces$state=="Michigan"] <- "MI"
cces$state[cces$state=="michigan"] <- "MI"
cces$state[cces$state=="Minnesota"] <- "MN"
cces$state[cces$state=="minnesota"] <- "MN"
cces$state[cces$state=="Mississippi"] <- "MS"
cces$state[cces$state=="mississippi"] <- "MS"
cces$state[cces$state=="Missouri"] <- "MO"
cces$state[cces$state=="missouri"] <- "MO"
cces$state[cces$state=="Montana"] <- "MT"
cces$state[cces$state=="montana"] <- "MT"
cces$state[cces$state=="Nebraska"] <- "NE"
cces$state[cces$state=="nebraska"] <- "NE"
cces$state[cces$state=="Nevada"] <- "NV"
cces$state[cces$state=="nevada"] <- "NV"
cces$state[cces$state=="New Hampshire"] <- "NH"
cces$state[cces$state=="new hampshire"] <- "NH"
cces$state[cces$state=="New Jersey"] <- "NJ"
cces$state[cces$state=="new jersey"] <- "NJ"
cces$state[cces$state=="New Mexico"] <- "NM"
cces$state[cces$state=="new mexico"] <- "NM"
cces$state[cces$state=="New York"] <- "NY"
cces$state[cces$state=="new york"] <- "NY"
cces$state[cces$state=="North Carolina"] <- "NC"
cces$state[cces$state=="north carolina"] <- "NC"
cces$state[cces$state=="North Dakota"] <- "ND"
cces$state[cces$state=="north dakota"] <- "ND"
cces$state[cces$state=="Ohio"] <- "OH"
cces$state[cces$state=="ohio"] <- "OH"
cces$state[cces$state=="Oklahoma"] <- "OK"
cces$state[cces$state=="oklahoma"] <- "OK"
cces$state[cces$state=="Oregon"] <- "OR"
cces$state[cces$state=="oregon"] <- "OR"
cces$state[cces$state=="Pennsylvania"] <- "PA"
cces$state[cces$state=="pennsylvania"] <- "PA"
cces$state[cces$state=="Rhode Island"] <- "RI"
cces$state[cces$state=="rhode island"] <- "RI"
cces$state[cces$state=="South Carolina"] <- "SC"
cces$state[cces$state=="south carolina"] <- "SC"
cces$state[cces$state=="South Dakota"] <- "SD"
cces$state[cces$state=="south dakota"] <- "SD"
cces$state[cces$state=="Tennessee"] <- "TN"
cces$state[cces$state=="tennessee"] <- "TN"
cces$state[cces$state=="Texas"] <- "TX"
cces$state[cces$state=="texas"] <- "TX"
cces$state[cces$state=="Utah"] <- "UT"
cces$state[cces$state=="utah"] <- "UT"
cces$state[cces$state=="Vermont"] <- "VT"
cces$state[cces$state=="vermont"] <- "VT"
cces$state[cces$state=="Virginia"] <- "VA"
cces$state[cces$state=="virginia"] <- "VA"
cces$state[cces$state=="Washington"] <- "WA"
cces$state[cces$state=="washington"] <- "WA"
cces$state[cces$state=="West Virginia"] <- "WV"
cces$state[cces$state=="west virginia"] <- "WV"
cces$state[cces$state=="Wisconsin"] <- "WI"
cces$state[cces$state=="wisconsin"] <- "WI"
cces$state[cces$state=="Wyoming"] <- "WY"
cces$state[cces$state=="wyoming"] <- "WY"
cces$state[cces$state=="district of columbia"] <- "DC"
cces$state[cces$state=="District of Columbia"] <- "DC"

# Fix the districts (note: district 00/0 indicates at-large district)

cces$district <- ifelse(cces$district == "01", "1",ifelse(cces$district == "02", "2",ifelse(cces$district == "03", "3",ifelse(cces$district == "04", "4", ifelse(cces$district == "05", "5",ifelse(cces$district == "06", "6",ifelse(cces$district == "07", "7",ifelse(cces$district == "08", "8",ifelse(cces$district == "09", "9", ifelse(cces$district == "00", "1", ifelse(cces$district == "0", "1", cces$district)))))))))))
cces$district <- as.numeric(cces$district)
cces$district <- paste(cces$state,cces$district,sep="")

##### 3) Clean Up the Individual-Level Regression Covariates #####

# Congressional Approval

cces$cong_approval <- tolower(cces$cong_approval)
na <- data.frame(freq(cces$cong_approval))
na$values <- rownames(na)
rownames(na) <- NULL

cces$cong_approval_binary <- NA # Binary Dichotomous Measure
cces$cong_approval_binary[cces$cong_approval %in% as.character(na[c(1,2,6,12,14),4])] <- 1
cces$cong_approval_binary[cces$cong_approval %in% as.character(na[c(3,4,7,13,15),4])] <- 0

for(i in seq(2007,2020,1)){
  print(i)
  print(freq(cces$cong_approval[cces$year == i]))
  print(freq(cces$cong_approval_binary[cces$year == i]))
}

# Presidential Approval

cces$pres_approve <- tolower(cces$pres_approve)
na <- data.frame(freq(cces$pres_approve))
na$values <- rownames(na)
rownames(na) <- NULL

for(i in seq(2006,2020,1)){
  print(i)
  print(table(cces$pres_approve[cces$year == i]))
}

cces$pres_approval <- NA # Binary Dichotomous Measure
cces$pres_approval[cces$pres_approve %in% as.character(na[c(1,2,6,11,13),4])] <- 1
cces$pres_approval[cces$pres_approve %in% as.character(na[c(3,4,7,12,14),4])] <- 0

for(i in seq(2006,2020,1)){
  print(i)
  print(freq(cces$pres_approve[cces$year == i]))
  print(freq(cces$pres_approval[cces$year == i]))
}

# Respondent Gender

cces$gender <- tolower(cces$gender)
na <- data.frame(freq(cces$gender))
na$values <- rownames(na)
rownames(na) <- NULL

cces$male <- NA
cces$male[cces$gender %in% as.character(na[c(1,4),3])] <- "male"
cces$male[cces$gender %in% as.character(na[c(2,3),3])] <- "female"

# State Legislative Approval

cces$state_leg_approve <- tolower(cces$state_leg_approval) # convert 2007 "neither approve nor disapprove" to not sure to match other year's coding
cces$state_leg_approve <- ifelse(cces$state_leg_approve == "neither approve nor disapprove","not sure",cces$state_leg_approve)

na <- data.frame(freq(cces$state_leg_approve))
na$values <- rownames(na)
rownames(na) <- NULL

for(i in c(2007,2008,2010:2020)){
  print(i)
  print(table(cces$state_leg_approve[cces$year == i]))
}

cces$state_leg_approval <- NA # Binary Dichotomous Measure
cces$state_leg_approval[cces$state_leg_approve %in% as.character(na[c(1,2,8,13,15),4])] <- 1
cces$state_leg_approval[cces$state_leg_approve %in% as.character(na[c(3,4,9,14,16),4])] <- 0

for(i in c(2007,2008,2010:2020)){
  print(i)
  print(freq(cces$state_leg_approve[cces$year == i]))
  print(freq(cces$state_leg_approval[cces$year == i]))
}

# State Governor Approval

cces$gov_approve <- tolower(cces$gov_approval)
na <- data.frame(freq(cces$gov_approve))
na$values <- rownames(na)
rownames(na) <- NULL

for(i in seq(2006,2020,1)){
  print(i)
  print(table(cces$gov_approve[cces$year == i]))
}

cces$gov_approval <- NA # Binary Dichotomous Measure
cces$gov_approval[cces$gov_approve %in% as.character(na[c(1,2,6,12,14),4])] <- 1
cces$gov_approval[cces$gov_approve %in% as.character(na[c(3,4,7,13,15),4])] <- 0

for(i in c(2006:2020)){
  print(i)
  print(freq(cces$gov_approval[cces$year == i]))
  print(freq(cces$gov_approve[cces$year == i]))
}

# Respondent Education

cces$educ <- tolower(cces$education)

na <- data.frame(freq(cces$educ))
na$values <- rownames(na)
rownames(na) <- NULL

cces$education <- NA
cces$education[cces$educ %in% as.character(na[c(1,10),4])] <- "no hs"
cces$education[cces$educ %in% as.character(na[c(2,9),4])] <- "high school graduate"
cces$education[cces$educ %in% as.character(na[c(4,3),4])] <- "2-year"
cces$education[cces$educ %in% as.character(na[c(5,12),4])] <- "some college"
cces$education[cces$educ %in% as.character(na[c(6,7),4])] <- "4-year"
cces$education[cces$educ %in% as.character(na[c(8,11),4])] <- "post-grad"

for(i in seq(2006,2020,1)){
  print(i)
  print(table(cces$educ[cces$year == i]))
  print(table(cces$education[cces$year == i]))
}

cces$education <- ifelse(cces$education %in% "no hs","no hs",ifelse(cces$education %in% c("high school graduate"),"hs",ifelse(cces$education %in% c("2-year","some college"),"some college",ifelse(cces$education %in% c("4-year","post-grad"),"college",NA))))

# Respondent Race

cces$race <- tolower(cces$race)
na <- data.frame(freq(cces$race))
na$values <- rownames(na)
rownames(na) <- NULL

for(i in seq(2006,2020,1)){
  print(i)
  print(table(cces$race[cces$year == i]))
}

cces$race3 <- "other"
cces$race3[cces$race %in% as.character(na[c(1,18),3])] <- "white"
cces$race3[cces$race %in% as.character(na[c(2,10),3])] <- "black"

for(i in seq(2006,2020,1)){
  print(i)
  print(table(cces$race[cces$year == i]))
  print(table(cces$race3[cces$year == i]))
}

cces$source <- paste("CCES",cces$year)
cces$year <- as.numeric(cces$year)
cces$weight <- as.numeric(cces$weight)

# State Representative, vote coded as 1 Democratic, 0 Republican

cces$state_lower_vote <- tolower(cces$state_lower_vote)
na <- data.frame(freq(cces$state_lower_vote))
na$values <- rownames(na)
rownames(na) <- NULL

for(i in c(2006:2008,2010,2012,2014,2016,2018,2020)){
  print(i)
  print(table(cces$state_lower_vote[cces$year == i]))
}

cces$state_lower_leg_vote <- NA # Binary Dichotomous Measure
cces$state_lower_leg_vote[cces$state_lower_vote %in% as.character(na[c(1,6,7),4])] <- 1
cces$state_lower_leg_vote[cces$state_lower_vote %in% as.character(na[c(2,15,16),4])] <- 0

for(i in  c(2006:2008,2010,2012,2014,2016,2018,2020)){
  print(i)
  print(freq(cces$state_lower_leg_vote[cces$year == i]))
  print(freq(cces$state_lower_vote[cces$year == i]))
}

# State Senator 

cces$state_upper_vote <- tolower(cces$state_upper_vote)
na <- data.frame(freq(cces$state_upper_vote))
na$values <- rownames(na)
rownames(na) <- NULL

for(i in c(2006:2008,2010,2012,2014,2016,2018,2020)){
  print(i)
  print(table(cces$state_upper_vote[cces$year == i]))
}

cces$state_upper_leg_vote <- NA # Binary Dichotomous Measure
cces$state_upper_leg_vote[cces$state_upper_vote %in% as.character(na[c(1,6,7),4])] <- 1
cces$state_upper_leg_vote[cces$state_upper_vote %in% as.character(na[c(2,15,16),4])] <- 0

for(i in  c(2006:2008,2010,2012,2014,2016,2018,2020)){
  print(i)
  print(freq(cces$state_upper_leg_vote[cces$year == i]))
  print(freq(cces$state_upper_vote[cces$year == i]))
}

rm(cces_2006,cces_2007,cces_2008,cces_2009,cces_2010,cces_2011,cces_2012,cces_2013,cces_2014,cces_2015,cces_2016,cces_2017,cces_2018,cces_2019,cces_2020,na,cces_vars,cces_vars_cols,i,names.use)

# CCES PID 7

cces$pid7 <- tolower(cces$pid7)

cces$pid3 <- ifelse(cces$pid7 %in% c("1","2","3","lean democrat","not very strong democrat","strong democrat","weak democrat"),"Democrat",ifelse(cces$pid7 %in% c("5","6","7","lean republican","not very strong republican","strong republican","weak republican"),"Republican",ifelse(cces$pid7 %in% c("4","independent"),"Independent",NA)))

table(cces$pid3,cces$pid7)

# Contextuals

contextuals <- read.dta13("cces_contextuals_2006_2020.dta")
contextuals$state <- NULL

cces$row_id <- as.numeric(rownames(cces))

cces <- merge(cces,contextuals,by=c("year","district"),all=T)
cces <- subset(cces,!is.na(cces$row_id))
nrow(cces)==max(cces$row_id)

cces$dem_president <- ifelse(cces$year %in% c(2009:2016),1,0)

# President Party Vote

cces$dem_governor <- ifelse(cces$gov_party == "Republican",0,ifelse(cces$gov_party == "Democratic",1,NA))

cces$state_upper_vote_inc_party <- ifelse(cces$state_upper_leg_vote == cces$dem_governor,1,0)

cces$state_lower_vote_inc_party <- ifelse(cces$state_lower_leg_vote == cces$dem_governor,1,0)

cces$governor_pres_party <- ifelse(cces$gov_party == "Republican" & cces$dem_president == 0,1,ifelse(cces$gov_party == "Democratic" & cces$dem_president == 1,1,0))

library(fixest)

cces$pid3_inc_party <- ifelse(cces$pid3 == "Democrat" & cces$dem_governor == 1,"Pres Party", ifelse(cces$pid3 == "Republican" & cces$dem_governor == 0,"Pres Party",ifelse(cces$pid3 == "Independent","Independent","Out Party")))

cces$gov_approve_coded <- ifelse(cces$gov_approve %in% c("1","strongly approve"),4,ifelse(cces$gov_approve %in% c("2","somewhat approve","approve"),3,ifelse(cces$gov_approve %in% c("3","somewhat disapprove","disapprove"),2,ifelse(cces$gov_approve %in% c("4","strongly disapprove"),1,NA))))

cces$pres_approve_coded <- ifelse(cces$pres_approve %in% c("1","strongly approve"),4,ifelse(cces$pres_approve %in% c("2","somewhat approve","approve"),3,ifelse(cces$pres_approve %in% c("3","somewhat disapprove","disapprove"),2,ifelse(cces$pres_approve %in% c("4","strongly disapprove"),1,NA))))

cces$lower_leg_pres_party <- ifelse(cces$state_house_party == "Republican" & cces$dem_governor == 0,1,ifelse(cces$state_house_party == "Democratic" & cces$dem_governor == 1,1,0))

cces$upper_leg_pres_party <- ifelse(cces$state_senate_party == "Republican" & cces$dem_governor == 0,1,ifelse(cces$state_senate_party == "Democratic" & cces$dem_governor == 1,1,0))

cces$state_leg_approve_coded <- ifelse(cces$state_leg_approve %in% c("1","strongly approve"),4,ifelse(cces$state_leg_approve %in% c("2","somewhat approve","approve"),3,ifelse(cces$state_leg_approve %in% c("3","somewhat disapprove","disapprove"),2,ifelse(cces$state_leg_approve %in% c("4","strongly disapprove"),1,NA))))

cces$cong_approval_coded <- ifelse(cces$cong_approval %in% c("1","strongly approve"),4,ifelse(cces$cong_approval %in% c("2","somewhat approve","approve"),3,ifelse(cces$cong_approval %in% c("3","somewhat disapprove","disapprove"),2,ifelse(cces$cong_approval %in% c("4","strongly disapprove"),1,NA))))

cces$dem_congress <- ifelse(cces$year %in% seq(2007,2010,1),"Democratic",ifelse(cces$year %in% c(2006,2015:2018),"Republican","Split"))
cces$dem_congress <- factor(cces$dem_congress,levels=c("Republican","Split","Democratic"))

cces$congress_inc_party <- ifelse(cces$dem_congress==cces$dem_governor,1,0)

# Household income attainment cross-sectional consistent conding (use cumulative file for consistent coding and merge based on caseid and year of survey; note this is necessary given variation in household income reporting cross-sectionally across surveys)

x <- haven::read_dta("cumulative_2006-2020.dta")
x <- subset(x,select=c(year,case_id,faminc))
x <- data.frame(x)
x$faminc <- ifelse(x$faminc > 12,NA,x$faminc)

x$faminc_bin <- NA
for(i in seq(2006,2020,1)){
  x$faminc_bin <- ifelse(x$year == i,as.numeric(factor(cut_number(x$faminc[x$year==i],n=3))),x$faminc_bin)
  print(table(x$faminc_bin[x$year == i]))
}
x$faminc_bin <- factor(x$faminc_bin,levels=c(1,2,3),labels=c("L","M","H"))

cces <- merge(cces,x,by=c("year","case_id"),all=T)
cces <- subset(cces,!is.na(cces$row_id))
nrow(cces)==max(cces$row_id)

cces$retro_econ_eval <- tolower(cces$retro_econ_eval)

cces$retro_econ_evals <- ifelse(cces$retro_econ_eval %in% c("6","8","not sure","skipped"),NA, ifelse(cces$retro_econ_eval %in% c("gotten much better","much better"),1,ifelse(cces$retro_econ_eval %in% c("gotten better","gotten somewhat better","somewhat better"),2,ifelse(cces$retro_econ_eval %in% c("stayed about the same","about the same"),3,ifelse(cces$retro_econ_eval %in% c("gotten worse","gotten somewhat worse","somewhat worse"),4,ifelse(cces$retro_econ_eval %in% c("gotten much worse","much worse"),5,cces$retro_econ_eval))))))

for(i in 2006:2020){
  print(i)
  print(table(cces$retro_econ_eval[cces$year == i]))
  print(table(cces$retro_econ_evals[cces$year == i]))
}

bam <- cces[,c(1,3,50,19:21,30,33:38)]
x <- subset(bam,bam$year %in% 2006:2008)
bam <- subset(bam,!(bam$year %in% 2006:2008))

for(i in c(4:ncol(x))){
  x[,i] <- as.numeric(x[,i])
  x[,i] <- ifelse(x[,i] < 14.28572, "Very Liberal", ifelse(x[,i] >= 14.28572 &   x[,i]  < (14.28572 * 2), "Liberal", ifelse(x[,i] >= (14.28572 * 2) & x[,i] <   (14.28572 * 3), "Somewhat Liberal", ifelse(x[,i] >= (14.28572 * 3) & x[,i] <  (14.28572 * 4), "Middle of the Road", ifelse(x[,i] >= (14.28572 * 4) & x[,i] <  (14.28572 * 5), "Somewhat Conservative", ifelse(x[,i] >= (14.28572 * 5) & x[,i] <  (14.28572 * 6), "Conservative", ifelse(x[,i] >= (14.28572 * 6), "Very Conservative", x[,i])))))))  
  print(table(x[,i]))
}

bam <- rbind(bam,x)

#Convert characters to numeric
for (i in 4:ncol(bam)){
  bam[,i] <- as.character(bam[,i])
}  

bam <- bam[order(bam$row_id),]

bam[bam == "Very Liberal"] <- 1
bam[bam == "very liberal"] <- 1
bam[bam == "Liberal"] <- 2
bam[bam == "liberal"] <- 2
bam[bam == "Somewhat Liberal"] <- 3
bam[bam == "somewhat liberal"] <- 3
bam[bam == "Middle of the Road"] <- 4
bam[bam == "middle of the road"] <- 4
bam[bam == "Somewhat Conservative"] <- 5
bam[bam == "somewhat conservative"] <- 5
bam[bam == "Conservative"] <- 6
bam[bam == "conservative"] <- 6
bam[bam == "Very Conservative"] <- 7
bam[bam == "very conservative"] <- 7
bam[bam == "Not Sure"] <- NA
bam[bam == "Not sure"] <- NA
bam[bam == "not sure"] <- NA
bam[bam == "Skipped"] <- NA
bam[bam == "skipped"] <- NA
bam[bam == "Not Asked"] <- NA
bam[bam == "Not asked"] <- NA
bam[bam == "not asked"] <- NA

#Convert characters to numeric
for (i in 4:ncol(bam)){
  bam[,i] <- as.numeric(bam[,i])
}  
# Check the stimuli
for (i in 4:ncol(bam)){
  bam[,i] <- ifelse(bam[,i] > 7,NA,bam[,i])
  print(colnames(bam)[[i]])
  print(freq(bam[,i], y.axis = "percent"))
}

library(basicspace)

mle_ideal_pts <- list()
stimuli <- list()
for(i in seq(2006,2020,1)){
  x <- subset(bam,bam$year == i)
  c <- x$row_id
  x$case_id <- NULL
  x$year <- NULL
  rownames(x) <- x$row_id
  x$district <- NULL
  x$row_id <- NULL
  x <- x[,colSums(x,na.rm=T) > 0]
  x[is.na(x)] <- 999
  MLE_result <- aldmck(x, polarity=2, respondent=1, missing=c(999), verbose=FALSE) # Standard ALDMCK scaling technique
  y <- data.frame(MLE_result$respondents,row_id=c)
  x <- MLE_result$stimuli
  mle_ideal_pts[[i]] <- y
  s <- data.frame(MLE_result$stimuli)
  s$stimuli <- rownames(s)
  s$year <- i
  stimuli[[i]] <- s
}
mle_ideal_pts <- ldply(mle_ideal_pts,data.frame)
stimuli <- ldply(stimuli,data.frame)

rm(c,i,y,s,MLE_result)

bam <- merge(bam,mle_ideal_pts,by=c("row_id"))

colnames(stimuli)[2] <- "scaled_stimuli"
bush <- subset(stimuli,stimuli$scaled_stimuli == "ideo_bush")
parties <- subset(stimuli,stimuli$scaled_stimuli %in% c("ideo_gop","ideo_dem"))
obama <- subset(stimuli,stimuli$scaled_stimuli %in% c("ideo_obama"))
trump <- subset(stimuli,stimuli$scaled_stimuli %in% c("ideo_trump"))

x1 <- subset(obama,obama$year > 2008)
x2 <- subset(trump,trump$year > 2016)

presidents <- rbind(bush,x1,x2)
rm(x,x1,x2,obama,bush,parties,trump)

bam <- bam[,c(1,14:18)]
colnames(bam)[3] <- "bam_weight"

cces <- merge(cces,bam,by=c("row_id"))
colnames(presidents)[1:3] <- c("president_ideal_pt","president","year")

cces <- merge(cces,presidents,by=c("year"),all=T)

cces$absolute_proximity_president <- abs(cces$president_ideal_pt-cces$idealpt)

# Knowledge

# Knowledge Materials

x <- subset(cces,select=c(year,state,house_maj_knowledge,senate_maj_knowledge,knowledge_recall_house_rep,knowledge_recall_governor,knowledge_recall_senator1,knowledge_recall_senator2,state_house_knowledge,state_senate_knowledge,state_house_party,state_senate_party,gov_party,senator1_party,senator2_party,house_seat_party))

for(i in 2006:2020){
  print(i)
  print(table(x$house_maj_knowledge[x$year == i]))
}
x[x=="the democrats"] <- "Democrats"
x[x=="democrat"] <- "Democrats"
x[x=="the republicans"] <- "Republicans"
x[x=="republican"] <- "Republicans"

x$house_maj_knowledge <- ifelse(x$year == 2017 & x$house_maj_knowledge == "1","Republicans",ifelse(x$year == 2017 & x$house_maj_knowledge == "2","Democrats",ifelse(x$year == 2017 & x$house_maj_knowledge == "3","Neither",ifelse(x$year == 2017 & x$house_maj_knowledge == "4","Not sure",x$house_maj_knowledge))))
x$house_maj_knowledge <- ifelse(x$year == 2018 & x$house_maj_knowledge == "1","Republicans",ifelse(x$year == 2018 & x$house_maj_knowledge == "2","Democrats",ifelse(x$year == 2018 & x$house_maj_knowledge == "3","Neither",ifelse(x$year == 2018 & x$house_maj_knowledge == "4","Not sure",x$house_maj_knowledge))))

for(i in 2006:2020){
  print(i)
  print(table(x$senate_maj_knowledge[x$year == i]))
}

x$senate_maj_knowledge <- ifelse(x$year == 2017 & x$senate_maj_knowledge == "1","Republicans",ifelse(x$year == 2017 & x$senate_maj_knowledge == "2","Democrats",ifelse(x$year == 2017 & x$senate_maj_knowledge == "3","Neither",ifelse(x$year == 2017 & x$senate_maj_knowledge == "4","Not sure",x$senate_maj_knowledge))))
x$senate_maj_knowledge <- ifelse(x$year == 2018 & x$senate_maj_knowledge == "1","Republicans",ifelse(x$year == 2018 & x$senate_maj_knowledge == "2","Democrats",ifelse(x$year == 2018 & x$senate_maj_knowledge == "3","Neither",ifelse(x$year == 2018 & x$senate_maj_knowledge == "4","Not sure",x$senate_maj_knowledge))))                         

for(i in 2006:2020){
  print(i)
  print(table(x$knowledge_recall_house_rep[x$year == i]))
}
x$knowledge_recall_house_rep <- ifelse(x$year == 2006 & x$state == "VT" & x$knowledge_recall_house_rep == "Independent","democrat",x$knowledge_recall_house_rep)

x$knowledge_recall_house_rep <- ifelse(x$year == 2017 & x$knowledge_recall_house_rep == "2","Republicans",ifelse(x$year == 2017 & x$knowledge_recall_house_rep == "3","Democrats",ifelse(x$year == 2017 & x$knowledge_recall_house_rep == "1","Never heard of this person",ifelse(x$year == 2017 & x$knowledge_recall_house_rep == "4","Other Party / Independent",ifelse(x$year == 2017 & x$knowledge_recall_house_rep == "5","Not sure",x$knowledge_recall_house_rep)))))
x$knowledge_recall_house_rep <- ifelse(x$year == 2018 & x$knowledge_recall_house_rep == "2","Republicans",ifelse(x$year == 2018 & x$knowledge_recall_house_rep == "3","Democrats",ifelse(x$year == 2018 & x$knowledge_recall_house_rep == "1","Never heard of this person",ifelse(x$year == 2018 & x$knowledge_recall_house_rep == "4","Other Party / Independent",ifelse(x$year == 2018 & x$knowledge_recall_house_rep == "5","Not sure",x$knowledge_recall_house_rep)))))

for(i in 2006:2020){
  print(i)
  print(table(x$knowledge_recall_senator1[x$year == i]))
}

x$knowledge_recall_senator1 <- ifelse(x$year == 2017 & x$knowledge_recall_senator1 == "2","Republicans",ifelse(x$year == 2017 & x$knowledge_recall_senator1 == "3","Democrats",ifelse(x$year == 2017 & x$knowledge_recall_senator1 == "1","Never heard of this person",ifelse(x$year == 2017 & x$knowledge_recall_senator1 == "4","Other Party / Independent",ifelse(x$year == 2017 & x$knowledge_recall_senator1 == "5","Not sure",x$knowledge_recall_senator1)))))
x$knowledge_recall_senator1 <- ifelse(x$year == 2018 & x$knowledge_recall_senator1 == "2","Republicans",ifelse(x$year == 2018 & x$knowledge_recall_senator1 == "3","Democrats",ifelse(x$year == 2018 & x$knowledge_recall_senator1 == "1","Never heard of this person",ifelse(x$year == 2018 & x$knowledge_recall_senator1 == "4","Other Party / Independent",ifelse(x$year == 2018 & x$knowledge_recall_senator1 == "5","Not sure",x$knowledge_recall_senator1)))))

x$knowledge_recall_senator1[x$year == 2010 & x$state == "VT" & x$knowledge_recall_senator1 == "Other Party / Independent"] <- "Democrats"
x$knowledge_recall_senator1[x$year == 2012 & x$state == "VT" & x$knowledge_recall_senator1 == "Other Party / Independent"] <- "Democrats"
x$knowledge_recall_senator1[x$year == 2013 & x$state == "VT" & x$knowledge_recall_senator1 == "Other Party / Independent"] <- "Democrats"
x$knowledge_recall_senator1[x$year == 2017 & x$state == "VT" & x$knowledge_recall_senator1 == "Other Party / Independent"] <- "Democrats"
x$knowledge_recall_senator1[x$year == 2018 & x$state == "VT" & x$knowledge_recall_senator1 == "Other Party / Independent"] <- "Democrats"

x$knowledge_recall_senator1[x$year == 2017 & x$state == "ME" & x$knowledge_recall_senator1 == "Other Party / Independent"] <- "Democrats"
x$knowledge_recall_senator1[x$year == 2018 & x$state == "ME" & x$knowledge_recall_senator1 == "Other Party / Independent"] <- "Democrats"


for(i in 2006:2020){
  print(i)
  print(table(x$knowledge_recall_senator2[x$year == i]))
}

x$knowledge_recall_senator2 <- ifelse(x$year == 2017 & x$knowledge_recall_senator2 == "2","Republicans",ifelse(x$year == 2017 & x$knowledge_recall_senator2 == "3","Democrats",ifelse(x$year == 2017 & x$knowledge_recall_senator2 == "1","Never heard of this person",ifelse(x$year == 2017 & x$knowledge_recall_senator2 == "4","Other Party / Independent",ifelse(x$year == 2017 & x$knowledge_recall_senator2 == "5","Not sure",x$knowledge_recall_senator2)))))
x$knowledge_recall_senator2 <- ifelse(x$year == 2018 & x$knowledge_recall_senator2 == "2","Republicans",ifelse(x$year == 2018 & x$knowledge_recall_senator2 == "3","Democrats",ifelse(x$year == 2018 & x$knowledge_recall_senator2 == "1","Never heard of this person",ifelse(x$year == 2018 & x$knowledge_recall_senator2 == "4","Other Party / Independent",ifelse(x$year == 2018 & x$knowledge_recall_senator2 == "5","Not sure",x$knowledge_recall_senator2)))))

x$knowledge_recall_senator2[x$year == 2006 & x$state == "VT" & x$knowledge_recall_senator2 == "Other Party / Independent"] <- "Democrats"
x$knowledge_recall_senator2[x$year == 2007 & x$state == "VT" & x$knowledge_recall_senator2 == "Other Party / Independent"] <- "Democrats"
x$knowledge_recall_senator2[x$year == 2008 & x$state == "VT" & x$knowledge_recall_senator2 == "Other Party / Independent"] <- "Democrats"
x$knowledge_recall_senator2[x$year == 2009 & x$state == "VT" & x$knowledge_recall_senator2 == "Other Party / Independent"] <- "Democrats"
x$knowledge_recall_senator2[x$year == 2011 & x$state == "VT" & x$knowledge_recall_senator2 == "Other Party / Independent"] <- "Democrats"
x$knowledge_recall_senator2[x$year == 2014 & x$state == "VT" & x$knowledge_recall_senator2 == "Other Party / Independent"] <- "Democrats"
x$knowledge_recall_senator2[x$year == 2015 & x$state == "VT" & x$knowledge_recall_senator2 == "Other Party / Independent"] <- "Democrats"
x$knowledge_recall_senator2[x$year == 2016 & x$state == "VT" & x$knowledge_recall_senator2 == "Other Party / Independent"] <- "Democrats"
x$knowledge_recall_senator2[x$year == 2019 & x$state == "VT" & x$knowledge_recall_senator2 == "Other Party / Independent"] <- "Democrats"
x$knowledge_recall_senator2[x$year == 2020 & x$state == "VT" & x$knowledge_recall_senator2 == "Other Party / Independent"] <- "Democrats"

x$knowledge_recall_senator2[x$year == 2013 & x$state == "ME" & x$knowledge_recall_senator2 == "Other Party / Independent"] <- "Democrats"
x$knowledge_recall_senator2[x$year == 2014 & x$state == "ME" & x$knowledge_recall_senator2 == "Other Party / Independent"] <- "Democrats"
x$knowledge_recall_senator2[x$year == 2015 & x$state == "ME" & x$knowledge_recall_senator2 == "Other Party / Independent"] <- "Democrats"
x$knowledge_recall_senator2[x$year == 2016 & x$state == "ME" & x$knowledge_recall_senator2 == "Other Party / Independent"] <- "Democrats"
x$knowledge_recall_senator2[x$year == 2019 & x$state == "ME" & x$knowledge_recall_senator2 == "Other Party / Independent"] <- "Democrats"
x$knowledge_recall_senator2[x$year == 2020 & x$state == "ME" & x$knowledge_recall_senator2 == "Other Party / Independent"] <- "Democrats"

x$gov_party <- ifelse(x$state %in% "RI" & x$year %in% c(2011,2012),"Democrats",x$gov_party)

x$knowledge_recall_governor[x$year == 2011 & x$state == "RI" & x$knowledge_recall_governor == "Other Party / Independent"] <- "Democrats"
x$knowledge_recall_governor[x$year == 2012 & x$state == "RI" & x$knowledge_recall_governor == "Other Party / Independent"] <- "Democrats"

x$gov_party <- ifelse(x$state %in% "AK" & x$year %in% c(2015,2016,2017,2018),"Independent",x$gov_party)
x$knowledge_recall_governor[x$year == 2017 & x$state == "AK" & x$knowledge_recall_governor == "4"] <- "Independent"
x$knowledge_recall_governor[x$year == 2018 & x$state == "AK" & x$knowledge_recall_governor == "4"] <- "Independent"

for(i in 2006:2020){
  print(i)
  print(table(x$knowledge_recall_governor[x$year == i]))
}

x$knowledge_recall_governor <- ifelse(x$year == 2017 & x$knowledge_recall_governor == "2","Republicans",ifelse(x$year == 2017 & x$knowledge_recall_governor == "3","Democrats",ifelse(x$year == 2017 & x$knowledge_recall_governor == "1","Never heard of this person",ifelse(x$year == 2017 & x$knowledge_recall_governor == "4","Independent",ifelse(x$year == 2017 & x$knowledge_recall_governor == "5","Not sure",x$knowledge_recall_governor)))))

x$knowledge_recall_governor <- ifelse(x$year == 2018 & x$knowledge_recall_governor == "2","Republicans",ifelse(x$year == 2018 & x$knowledge_recall_governor == "3","Democrats",ifelse(x$year == 2018 & x$knowledge_recall_governor == "1","Never heard of this person",ifelse(x$year == 2018 & x$knowledge_recall_governor == "4","Independent",ifelse(x$year == 2018 & x$knowledge_recall_governor == "5","Not sure",x$knowledge_recall_governor)))))

for(i in 2006:2020){
  print(i)
  print(table(x$state_house_knowledge[x$year == i]))
}
x$state_house_knowledge <- ifelse(x$year %in% c(2017,2018) & x$state_house_knowledge == "1", "Republicans",ifelse(x$year %in% c(2017,2018) & x$state_house_knowledge == "2", "Democrats",ifelse(x$year %in% c(2017,2018) & x$state_house_knowledge == "3", "Neither",ifelse(x$year %in% c(2017,2018) & x$state_house_knowledge == "4", "Not sure",x$state_house_knowledge))))

for(i in 2006:2020){
  print(i)
  print(table(x$state_senate_knowledge[x$year == i]))
}
x$state_senate_knowledge <- ifelse(x$year %in% c(2017,2018) & x$state_senate_knowledge == "1", "Republicans",ifelse(x$year %in% c(2017,2018) & x$state_senate_knowledge == "2", "Democrats",ifelse(x$year %in% c(2017,2018) & x$state_senate_knowledge == "3", "Neither",ifelse(x$year %in% c(2017,2018) & x$state_senate_knowledge == "4", "Not sure",x$state_senate_knowledge))))

for(i in 3:10){
  x[,i] <- toupper(x[,i])
  print(colnames(x)[i])
  print(table(x[,i]))
}
x[x=="DEMOCRAT"] <- "DEMOCRATIC PARTY"
x[x=="DEMOCRATIC"] <- "DEMOCRATIC PARTY"
x[x=="DEMOCRATS"] <- "DEMOCRATIC PARTY"
x[x=="REPUBLICANS"] <- "REPUBLICAN PARTY"
x[x=="REPUBLICAN"] <- "REPUBLICAN PARTY"
x[x=="OTHER PARTY / INDEPENDENT"] <- "INDEPENDENT"
x[x=="OTHER PARTY/INDEPENDENT"] <- "INDEPENDENT"
x[x=="TIED"] <- "SPLIT"
x[x=="NEITHER"] <- "SPLIT"
x[x=="THERE WILL BE 50 DEMOCRATS AND 50 REPUBLICANS"] <- "SPLIT"
x[x=="8"] <- "SKIPPED"
x[x=="9"] <- "NOT ASKED"
x$knowledge_recall_senator1 <- ifelse(x$knowledge_recall_senator1 %in% c("1","2","3","4"),"SKIPPED",x$knowledge_recall_senator1)
for(i in 3:10){
  print(colnames(x)[i])
  print(table(x[,i]))
}

for(i in 2006:2020){
  print(i)
  print(freq(x$house_maj_knowledge[x$year == i]))
}
for(i in 2006:2020){
  print(i)
  print(table(x$senate_maj_knowledge[x$year == i]))
}
for(i in 2006:2020){
  print(i)
  print(table(x$knowledge_recall_house_rep[x$year == i]))
}
for(i in 2006:2020){
  print(i)
  print(table(x$knowledge_recall_senator1[x$year == i]))
}
for(i in 2006:2020){
  print(i)
  print(table(x$knowledge_recall_senator2[x$year == i]))
}
for(i in 2006:2020){
  print(i)
  print(table(x$knowledge_recall_governor[x$year == i]))
}
for(i in 2006:2020){
  print(i)
  print(table(x$state_house_knowledge[x$year == i]))
}
for(i in 2006:2020){
  print(i)
  print(table(x$state_senate_knowledge[x$year == i]))
}

# Create the "Correct variables"

x[x=="DEMOCRATS"] <-"DEMOCRATIC PARTY"
x[x=="DEMOCRATIC"] <-"DEMOCRATIC PARTY"
x[x=="REPUBLICAN"] <-"REPUBLICAN PARTY"
x[x=="NON-PARTISAN/GOP INFORMAL"] <-"REPUBLICAN PARTY"

for(i in 11:16){
  print(colnames(x)[i])
  x[,i] <- toupper(x[,i])
  print(table(x[,i]))
}
x$state_house_party <- ifelse(x$state == "NE",NA,x$state_house_party)
x$house_seat_party <- ifelse(x$house_seat_party == "",NA,x$house_seat_party)

x$knowledge_recall_senator1[x$year == 2010 & x$state == "CT" & x$knowledge_recall_senator1 == "INDEPENDENT"] <- "DEMOCRATIC PARTY"
x$knowledge_recall_senator1[x$year == 2011 & x$state == "CT" & x$knowledge_recall_senator1 == "INDEPENDENT"] <- "DEMOCRATIC PARTY"
x$knowledge_recall_senator1[x$year == 2012 & x$state == "CT" & x$knowledge_recall_senator1 == "INDEPENDENT"] <- "DEMOCRATIC PARTY"

x$knowledge_recall_senator2[x$year == 2006 & x$state == "CT" & x$knowledge_recall_senator2 == "INDEPENDENT"] <- "DEMOCRATIC PARTY"
x$knowledge_recall_senator2[x$year == 2007 & x$state == "CT" & x$knowledge_recall_senator2 == "INDEPENDENT"] <- "DEMOCRATIC PARTY"
x$knowledge_recall_senator2[x$year == 2008 & x$state == "CT" & x$knowledge_recall_senator2 == "INDEPENDENT"] <- "DEMOCRATIC PARTY"
x$knowledge_recall_senator2[x$year == 2009 & x$state == "CT" & x$knowledge_recall_senator2 == "INDEPENDENT"] <- "DEMOCRATIC PARTY"

# Correct state_house_party, state_senate_party, gov_party, senator1_party, senator2_party, house_seat_party, house_maj_knowledge, senate_maj_knowledge

x[x=="DEMOCRATIC"] <- "DEMOCRATIC PARTY"
x[x=="REPUBLICAN"] <- "REPUBLICAN PARTY"
x[x=="DEMOCRATS"] <- "DEMOCRATIC PARTY"
x[x=="DEMOCRATS"] <- "DEMOCRATIC PARTY"

x$correct_state_house <- ifelse(x$state_house_knowledge==x$state_house_party,1,0)
x$correct_state_senate <- ifelse(x$state_senate_knowledge==x$state_senate_party,1,0)
x$correct_senator1 <- ifelse(x$knowledge_recall_senator1==x$senator1_party,1,0)
x$correct_senator2 <- ifelse(x$knowledge_recall_senator2==x$senator2_party,1,0)
x$correct_mc <- ifelse(x$knowledge_recall_house_rep==x$house_seat_party,1,0)
x$correct_gov <- ifelse(x$knowledge_recall_governor==x$gov_party,1,0)

x$correct_house_maj <- ifelse(x$house_maj_knowledge %in% "DEMOCRATIC PARTY" & x$year %in% c(2006,2007,2008,2009,2010,2019,2020),1,ifelse(x$house_maj_knowledge %in% "REPUBLICAN PARTY" & x$year %in% c(2006,2011,2012,2013,2014,2015,2016,2017,2018),1,0)) # Note in 2006 it's prospective given last election results

x$correct_senate_maj <- ifelse(x$senate_maj_knowledge %in% "DEMOCRATIC PARTY" & x$year %in% c(2006,2007,2008,2009,2010,2011,2012,2013,2014),1,ifelse(x$senate_maj_knowledge %in% "REPUBLICAN PARTY" & x$year %in% c(2006,2015,2016,2017,2018,2019,2020),1,0)) # Note in 2006 it's prospective given last election results

for(i in 17:ncol(x)){
  print(colnames(x)[i])
  print(freq(x[,i]))
}

knowledge <- x
x <- knowledge[,c(17:24)]
x$knowledge_mean <- rowMeans(x,na.rm=T)
x$knowledge_sum <- rowSums(x,na.rm=T)

x$knowledge_mean_bin <- factor(cut_number(x$knowledge_mean,n=3),labels=c("L","M","H"))

cces <- cbind(cces,x)
cces$retro_econ_evals <- as.numeric(cces$retro_econ_evals)

##### 4) Model Estimation #####

# cces$state_lower_leg_vote, 1 = Dem, 0 = GOP
# cces$state_upper_leg_vote, 1 = Dem, 0 = GOP

cces$dem_state_house <- ifelse(cces$state_house_party == "Democratic",1,ifelse(cces$state_house_party == "Republican",0,NA))

cces$dem_state_senate <-  ifelse(cces$state_senate_party == "Democratic",1,ifelse(cces$state_senate_party == "Republican",0,NA))
cces$pid3 <- factor(cces$pid3,levels=c("Republican","Independent","Democrat"))

analysis_data <- cces

analysis_data <- subset(analysis_data,analysis_data$year %in% c(2007,2008,2010,2012,2014,2016,2018,2020))

analysis_data$year <- factor(analysis_data$year)
analysis_data$state <- factor(analysis_data$state)

analysis_data$dem_legislature <- ifelse(analysis_data$state_house_party == "Democratic" & analysis_data$state_senate_party == "Democratic","Democratic Legislature",ifelse(analysis_data$state_house_party == "Republican" & analysis_data$state_senate_party == "Republican","Republican Legislature","Split Legislature"))

analysis_data$dem_legislature  <- factor(analysis_data$dem_legislature)

# Main Models--Lower Chambers

full_model_lower <- feols(state_lower_leg_vote ~ i(dem_president,pres_approve_coded) + i(dem_governor,gov_approve_coded) + i(dem_legislature,state_leg_approve_coded) + i(dem_congress,cong_approval_coded) + pid3 + i(dem_president,absolute_proximity_president) + retro_econ_evals + knowledge_mean_bin + faminc_bin + male + race3 + education | year + state,data=analysis_data)
summary(full_model_lower, cluster = c("year","state"))

# 2007

x <- subset(analysis_data,analysis_data$year == 2007)

full_model_lower_2007 <- feols(state_lower_leg_vote ~ i(dem_president,pres_approve_coded) + i(dem_governor,gov_approve_coded) + i(dem_legislature,state_leg_approve_coded) + i(dem_congress,cong_approval_coded) + pid3 + i(dem_president,absolute_proximity_president) + retro_econ_evals + knowledge_mean_bin + faminc_bin + male + race3 + education | state,data=x)
summary(full_model_lower, cluster = c("state"))

# 2008

x <- subset(analysis_data,analysis_data$year == 2008)

full_model_lower_2008 <- feols(state_lower_leg_vote ~ i(dem_president,pres_approve_coded) + i(dem_governor,gov_approve_coded) + i(dem_legislature,state_leg_approve_coded) + i(dem_congress,cong_approval_coded) + pid3 + i(dem_president,absolute_proximity_president) + retro_econ_evals + knowledge_mean_bin + faminc_bin + male + race3 + education | state,data=x)
summary(full_model_lower_2008, cluster = c("state"))

# 2010

x <- subset(analysis_data,analysis_data$year == 2010)

full_model_lower_2010 <- feols(state_lower_leg_vote ~ i(dem_president,pres_approve_coded) + i(dem_governor,gov_approve_coded) + i(dem_legislature,state_leg_approve_coded) + i(dem_congress,cong_approval_coded) + pid3 + i(dem_president,absolute_proximity_president) + retro_econ_evals + knowledge_mean_bin + faminc_bin + male + race3 + education | state,data=x)
summary(full_model_lower_2010, cluster = c("state"))

# 2012

x <- subset(analysis_data,analysis_data$year == 2012)

full_model_lower_2012 <- feols(state_lower_leg_vote ~ i(dem_president,pres_approve_coded) + i(dem_governor,gov_approve_coded) + i(dem_legislature,state_leg_approve_coded) + i(dem_congress,cong_approval_coded) + pid3 + i(dem_president,absolute_proximity_president) + retro_econ_evals + knowledge_mean_bin + faminc_bin + male + race3 + education | state,data=x)
summary(full_model_lower_2012, cluster = c("state"))

# 2014

x <- subset(analysis_data,analysis_data$year == 2014)

full_model_lower_2014 <- feols(state_lower_leg_vote ~ i(dem_president,pres_approve_coded) + i(dem_governor,gov_approve_coded) + i(dem_legislature,state_leg_approve_coded) + i(dem_congress,cong_approval_coded) + pid3 + i(dem_president,absolute_proximity_president) + retro_econ_evals + knowledge_mean_bin + faminc_bin + male + race3 + education | state,data=x)
summary(full_model_lower_2014, cluster = c("state"))

# 2016

x <- subset(analysis_data,analysis_data$year == 2016)

full_model_lower_2016 <- feols(state_lower_leg_vote ~ i(dem_president,pres_approve_coded) + i(dem_governor,gov_approve_coded) + i(dem_legislature,state_leg_approve_coded) + i(dem_congress,cong_approval_coded) + pid3 + i(dem_president,absolute_proximity_president) + retro_econ_evals + knowledge_mean_bin + faminc_bin + male + race3 + education | state,data=x)
summary(full_model_lower_2016, cluster = c("state"))

# 2018

x <- subset(analysis_data,analysis_data$year == 2018)

full_model_lower_2018 <- feols(state_lower_leg_vote ~ i(dem_president,pres_approve_coded) + i(dem_governor,gov_approve_coded) + i(dem_legislature,state_leg_approve_coded) + i(dem_congress,cong_approval_coded) + pid3 + i(dem_president,absolute_proximity_president) + retro_econ_evals + knowledge_mean_bin + faminc_bin + male + race3 + education | state,data=x)
summary(full_model_lower_2018, cluster = c("state"))


# 2020

x <- subset(analysis_data,analysis_data$year == 2020)

full_model_lower_2020 <- feols(state_lower_leg_vote ~ i(dem_president,pres_approve_coded) + i(dem_governor,gov_approve_coded) + i(dem_legislature,state_leg_approve_coded) + i(dem_congress,cong_approval_coded) + pid3 + i(dem_president,absolute_proximity_president) + retro_econ_evals + knowledge_mean_bin + faminc_bin + male + race3 + education | state,data=x)
summary(full_model_lower_2020, cluster = c("state"))

# Main Models--Upper Chambers 

full_model_upper <- feols(state_upper_leg_vote ~ i(dem_president,pres_approve_coded) + i(dem_governor,gov_approve_coded) + i(dem_legislature,state_leg_approve_coded) + i(dem_congress,cong_approval_coded) + pid3 + i(dem_president,absolute_proximity_president) + retro_econ_evals + knowledge_mean_bin + faminc_bin + male + race3 + education | year + state,data=analysis_data)
summary(full_model_upper, cluster = c("year","state"))

# 2007

x <- subset(analysis_data,analysis_data$year == 2007)

full_model_upper_2007 <- feols(state_upper_leg_vote ~ i(dem_president,pres_approve_coded) + i(dem_governor,gov_approve_coded) + i(dem_legislature,state_leg_approve_coded) + i(dem_congress,cong_approval_coded) + pid3 + i(dem_president,absolute_proximity_president) + retro_econ_evals + knowledge_mean_bin + faminc_bin + male + race3 + education | state,data=x)
summary(full_model_upper, cluster = c("state"))

# 2008

x <- subset(analysis_data,analysis_data$year == 2008)

full_model_upper_2008 <- feols(state_upper_leg_vote ~ i(dem_president,pres_approve_coded) + i(dem_governor,gov_approve_coded) + i(dem_legislature,state_leg_approve_coded) + i(dem_congress,cong_approval_coded) + pid3 + i(dem_president,absolute_proximity_president) + retro_econ_evals + knowledge_mean_bin + faminc_bin + male + race3 + education | state,data=x)
summary(full_model_upper_2008, cluster = c("state"))

# 2010

x <- subset(analysis_data,analysis_data$year == 2010)

full_model_upper_2010 <- feols(state_upper_leg_vote ~ i(dem_president,pres_approve_coded) + i(dem_governor,gov_approve_coded) + i(dem_legislature,state_leg_approve_coded) + i(dem_congress,cong_approval_coded) + pid3 + i(dem_president,absolute_proximity_president) + retro_econ_evals + knowledge_mean_bin + faminc_bin + male + race3 + education | state,data=x)
summary(full_model_upper_2010, cluster = c("state"))

# 2012

x <- subset(analysis_data,analysis_data$year == 2012)

full_model_upper_2012 <- feols(state_upper_leg_vote ~ i(dem_president,pres_approve_coded) + i(dem_governor,gov_approve_coded) + i(dem_legislature,state_leg_approve_coded) + i(dem_congress,cong_approval_coded) + pid3 + i(dem_president,absolute_proximity_president) + retro_econ_evals + knowledge_mean_bin + faminc_bin + male + race3 + education | state,data=x)
summary(full_model_upper_2012, cluster = c("state"))

# 2014

x <- subset(analysis_data,analysis_data$year == 2014)

full_model_upper_2014 <- feols(state_upper_leg_vote ~ i(dem_president,pres_approve_coded) + i(dem_governor,gov_approve_coded) + i(dem_legislature,state_leg_approve_coded) + i(dem_congress,cong_approval_coded) + pid3 + i(dem_president,absolute_proximity_president) + retro_econ_evals + knowledge_mean_bin + faminc_bin + male + race3 + education | state,data=x)
summary(full_model_upper_2014, cluster = c("state"))

# 2016

x <- subset(analysis_data,analysis_data$year == 2016)

full_model_upper_2016 <- feols(state_upper_leg_vote ~ i(dem_president,pres_approve_coded) + i(dem_governor,gov_approve_coded) + i(dem_legislature,state_leg_approve_coded) + i(dem_congress,cong_approval_coded) + pid3 + i(dem_president,absolute_proximity_president) + retro_econ_evals + knowledge_mean_bin + faminc_bin + male + race3 + education | state,data=x)
summary(full_model_upper_2016, cluster = c("state"))

# 2018

x <- subset(analysis_data,analysis_data$year == 2018)

full_model_upper_2018 <- feols(state_upper_leg_vote ~ i(dem_president,pres_approve_coded) + i(dem_governor,gov_approve_coded) + i(dem_legislature,state_leg_approve_coded) + i(dem_congress,cong_approval_coded) + pid3 + i(dem_president,absolute_proximity_president) + retro_econ_evals + knowledge_mean_bin + faminc_bin + male + race3 + education | state,data=x)
summary(full_model_upper_2018, cluster = c("state"))


# 2020

x <- subset(analysis_data,analysis_data$year == 2020)

full_model_upper_2020 <- feols(state_upper_leg_vote ~ i(dem_president,pres_approve_coded) + i(dem_governor,gov_approve_coded) + i(dem_legislature,state_leg_approve_coded) + i(dem_congress,cong_approval_coded) + pid3 + i(dem_president,absolute_proximity_president) + retro_econ_evals + knowledge_mean_bin + faminc_bin + male + race3 + education | state,data=x)
summary(full_model_upper_2020, cluster = c("state"))

# Main Models--all Chambers 

x <- subset(analysis_data,select=c(year,state,district,state_lower_leg_vote,dem_legislature,pid3,pres_approve_coded,governor_pres_party,gov_approve_coded,state_leg_approve_coded,dem_congress,cong_approval_coded,knowledge_mean_bin,faminc_bin,male,race3,education,weight,retro_econ_evals,absolute_proximity_president,idealpt,dem_president,dem_governor))
x <- subset(x,x$year %in% c(2007,2008,2010,2012,2014,2016,2018,2020))

x1 <- subset(analysis_data,select=c(year,state,district,state_upper_leg_vote,dem_legislature,pid3,pres_approve_coded,governor_pres_party,gov_approve_coded,state_leg_approve_coded,dem_congress,cong_approval_coded,knowledge_mean_bin,faminc_bin,male,race3,education,weight,retro_econ_evals,absolute_proximity_president,idealpt,dem_president,dem_governor))
x1 <- subset(x1,x1$year %in% c(2007,2008,2010,2012,2014,2016,2018,2020))


x$chamber <- "Lower Chamber"
x1$chamber <- "Upper Chamber"

colnames(x)[4] <- "state_all_vote_inc_party"
colnames(x1)[4] <- "state_all_vote_inc_party"

colnames(x)[5] <- "all_leg_pres_party"
colnames(x1)[5] <- "all_leg_pres_party"

analysis_data_full <- rbind(x,x1)
analysis_data_full$chamber_state_fe <- paste(analysis_data_full$state,analysis_data_full$chamber)
analysis_data_full$chamber_state_fe <- factor(analysis_data_full$chamber_state_fe)

full_model_all <- feols(state_all_vote_inc_party ~ i(dem_president,pres_approve_coded) + i(dem_governor,gov_approve_coded) + i(all_leg_pres_party,state_leg_approve_coded) + i(dem_congress,cong_approval_coded) + pid3 + i(dem_president,absolute_proximity_president) + retro_econ_evals + knowledge_mean_bin + faminc_bin + male + race3 + education | year + chamber_state_fe,data=analysis_data_full)
summary(full_model_all, cluster = c("year","chamber_state_fe"))

# 2007

x <- subset(analysis_data_full,analysis_data_full$year == 2007)

full_model_all_2007 <- feols(state_all_vote_inc_party ~ i(dem_president,pres_approve_coded) + i(dem_governor,gov_approve_coded) + i(all_leg_pres_party,state_leg_approve_coded) + i(dem_congress,cong_approval_coded) + pid3 + i(dem_president,absolute_proximity_president) + retro_econ_evals + knowledge_mean_bin + faminc_bin + male + race3 + education | chamber_state_fe,data=x)
summary(full_model_all_2007, cluster = c("chamber_state_fe"))

# 2008

x <- subset(analysis_data_full,analysis_data_full$year == 2008)

full_model_all_2008 <- feols(state_all_vote_inc_party ~ i(dem_president,pres_approve_coded) + i(dem_governor,gov_approve_coded) + i(all_leg_pres_party,state_leg_approve_coded) + i(dem_congress,cong_approval_coded) + pid3 + i(dem_president,absolute_proximity_president) + retro_econ_evals + knowledge_mean_bin + faminc_bin + male + race3 + education | chamber_state_fe,data=x)
summary(full_model_all_2008, cluster = c("chamber_state_fe"))

# 2010

x <- subset(analysis_data_full,analysis_data_full$year == 2010)

full_model_all_2010 <- feols(state_all_vote_inc_party ~ i(dem_president,pres_approve_coded) + i(dem_governor,gov_approve_coded) + i(all_leg_pres_party,state_leg_approve_coded) + i(dem_congress,cong_approval_coded) + pid3 + i(dem_president,absolute_proximity_president) + retro_econ_evals + knowledge_mean_bin + faminc_bin + male + race3 + education | chamber_state_fe,data=x)
summary(full_model_all_2010, cluster = c("chamber_state_fe"))

# 2012

x <- subset(analysis_data_full,analysis_data_full$year == 2012)

full_model_all_2012 <- feols(state_all_vote_inc_party ~ i(dem_president,pres_approve_coded) + i(dem_governor,gov_approve_coded) + i(all_leg_pres_party,state_leg_approve_coded) + i(dem_congress,cong_approval_coded) + pid3 + i(dem_president,absolute_proximity_president) + retro_econ_evals + knowledge_mean_bin + faminc_bin + male + race3 + education | chamber_state_fe,data=x)
summary(full_model_all_2012, cluster = c("chamber_state_fe"))

# 2014

x <- subset(analysis_data_full,analysis_data_full$year == 2014)

full_model_all_2014 <- feols(state_all_vote_inc_party ~ i(dem_president,pres_approve_coded) + i(dem_governor,gov_approve_coded) + i(all_leg_pres_party,state_leg_approve_coded) + i(dem_congress,cong_approval_coded) + pid3 + i(dem_president,absolute_proximity_president) + retro_econ_evals + knowledge_mean_bin + faminc_bin + male + race3 + education | chamber_state_fe,data=x)
summary(full_model_all_2014, cluster = c("chamber_state_fe"))

# 2016

x <- subset(analysis_data_full,analysis_data_full$year == 2016)

full_model_all_2016 <- feols(state_all_vote_inc_party ~ i(dem_president,pres_approve_coded) + i(dem_governor,gov_approve_coded) + i(all_leg_pres_party,state_leg_approve_coded) + i(dem_congress,cong_approval_coded) + pid3 + i(dem_president,absolute_proximity_president) + retro_econ_evals + knowledge_mean_bin + faminc_bin + male + race3 + education | chamber_state_fe,data=x)
summary(full_model_all_2016, cluster = c("chamber_state_fe"))

# 2018

x <- subset(analysis_data_full,analysis_data_full$year == 2018)

full_model_all_2018 <- feols(state_all_vote_inc_party ~ i(dem_president,pres_approve_coded) + i(dem_governor,gov_approve_coded) + i(all_leg_pres_party,state_leg_approve_coded) + i(dem_congress,cong_approval_coded) + pid3 + i(dem_president,absolute_proximity_president) + retro_econ_evals + knowledge_mean_bin + faminc_bin + male + race3 + education | chamber_state_fe,data=x)
summary(full_model_all_2018, cluster = c("chamber_state_fe"))


# 2020

x <- subset(analysis_data_full,analysis_data_full$year == 2020)

full_model_all_2020 <- feols(state_all_vote_inc_party ~ i(dem_president,pres_approve_coded) + i(dem_governor,gov_approve_coded) + i(all_leg_pres_party,state_leg_approve_coded) + i(dem_congress,cong_approval_coded) + pid3 + i(dem_president,absolute_proximity_president) + retro_econ_evals + knowledge_mean_bin + faminc_bin + male + race3 + education | chamber_state_fe,data=x)
summary(full_model_all_2020, cluster = c("chamber_state_fe"))

##### 5) Creation of Manuscript Figures 6 & 7 #####

full_model_lower <- summary(full_model_lower, cluster = c("year","state"))
full_model_lower_effects <- data.frame(full_model_lower$coeftable)
colnames(full_model_lower_effects) <- c("estimate","se","t_value","p_value")
full_model_lower_effects$effect <- rownames(full_model_lower_effects)
rownames(full_model_lower_effects) <- NULL
full_model_lower_effects$nobs <- nobs(full_model_lower)
full_model_lower_effects$model <- "Pooled Baseline"
full_model_lower_effects$chamber <- "Lower"

full_model_upper <- summary(full_model_upper, cluster = c("year","state"))
full_model_upper_effects <- data.frame(full_model_upper$coeftable)
colnames(full_model_upper_effects) <- c("estimate","se","t_value","p_value")
full_model_upper_effects$effect <- rownames(full_model_upper_effects)
rownames(full_model_upper_effects) <- NULL
full_model_upper_effects$nobs <- nobs(full_model_upper)
full_model_upper_effects$model <- "Pooled Baseline"
full_model_upper_effects$chamber <- "Upper"

full_model_all <- summary(full_model_all, cluster = c("year","chamber_state_fe"))
full_model_all_effects <- data.frame(full_model_all$coeftable)
colnames(full_model_all_effects) <- c("estimate","se","t_value","p_value")
full_model_all_effects$effect <- rownames(full_model_all_effects)
rownames(full_model_all_effects) <- NULL
full_model_all_effects$nobs <- nobs(full_model_all)
full_model_all_effects$model <- "Pooled Baseline"
full_model_all_effects$chamber <- "All"

full_model_lower_2007 <- summary(full_model_lower_2007, cluster = c("state"))
full_model_lower_2007_effects <- data.frame(full_model_lower_2007$coeftable)
colnames(full_model_lower_2007_effects) <- c("estimate","se","t_value","p_value")
full_model_lower_2007_effects$effect <- rownames(full_model_lower_2007_effects)
rownames(full_model_lower_2007_effects) <- NULL
full_model_lower_2007_effects$nobs <- nobs(full_model_lower_2007)
full_model_lower_2007_effects$model <- "2007 Model"
full_model_lower_2007_effects$chamber <- "Lower"

full_model_upper_2007 <- summary(full_model_upper_2007, cluster = c("state"))
full_model_upper_2007_effects <- data.frame(full_model_upper_2007$coeftable)
colnames(full_model_upper_2007_effects) <- c("estimate","se","t_value","p_value")
full_model_upper_2007_effects$effect <- rownames(full_model_upper_2007_effects)
rownames(full_model_upper_2007_effects) <- NULL
full_model_upper_2007_effects$nobs <- nobs(full_model_upper_2007)
full_model_upper_2007_effects$model <- "2007 Model"
full_model_upper_2007_effects$chamber <- "Upper"

full_model_all_2007 <- summary(full_model_all_2007, cluster = c("chamber_state_fe"))
full_model_all_2007_effects <- data.frame(full_model_all_2007$coeftable)
colnames(full_model_all_2007_effects) <- c("estimate","se","t_value","p_value")
full_model_all_2007_effects$effect <- rownames(full_model_all_2007_effects)
rownames(full_model_all_2007_effects) <- NULL
full_model_all_2007_effects$nobs <- nobs(full_model_all_2007)
full_model_all_2007_effects$model <- "2007 Model"
full_model_all_2007_effects$chamber <- "All"

full_model_lower_2008 <- summary(full_model_lower_2008, cluster = c("state"))
full_model_lower_2008_effects <- data.frame(full_model_lower_2008$coeftable)
colnames(full_model_lower_2008_effects) <- c("estimate","se","t_value","p_value")
full_model_lower_2008_effects$effect <- rownames(full_model_lower_2008_effects)
rownames(full_model_lower_2008_effects) <- NULL
full_model_lower_2008_effects$nobs <- nobs(full_model_lower_2008)
full_model_lower_2008_effects$model <- "2008 Model"
full_model_lower_2008_effects$chamber <- "Lower"

full_model_upper_2008 <- summary(full_model_upper_2008, cluster = c("state"))
full_model_upper_2008_effects <- data.frame(full_model_upper_2008$coeftable)
colnames(full_model_upper_2008_effects) <- c("estimate","se","t_value","p_value")
full_model_upper_2008_effects$effect <- rownames(full_model_upper_2008_effects)
rownames(full_model_upper_2008_effects) <- NULL
full_model_upper_2008_effects$nobs <- nobs(full_model_upper_2008)
full_model_upper_2008_effects$model <- "2008 Model"
full_model_upper_2008_effects$chamber <- "Upper"

full_model_all_2008 <- summary(full_model_all_2008, cluster = c("chamber_state_fe"))
full_model_all_2008_effects <- data.frame(full_model_all_2008$coeftable)
colnames(full_model_all_2008_effects) <- c("estimate","se","t_value","p_value")
full_model_all_2008_effects$effect <- rownames(full_model_all_2008_effects)
rownames(full_model_all_2008_effects) <- NULL
full_model_all_2008_effects$nobs <- nobs(full_model_all_2008)
full_model_all_2008_effects$model <- "2008 Model"
full_model_all_2008_effects$chamber <- "All"

full_model_lower_2010 <- summary(full_model_lower_2010, cluster = c("state"))
full_model_lower_2010_effects <- data.frame(full_model_lower_2010$coeftable)
colnames(full_model_lower_2010_effects) <- c("estimate","se","t_value","p_value")
full_model_lower_2010_effects$effect <- rownames(full_model_lower_2010_effects)
rownames(full_model_lower_2010_effects) <- NULL
full_model_lower_2010_effects$nobs <- nobs(full_model_lower_2010)
full_model_lower_2010_effects$model <- "2010 Model"
full_model_lower_2010_effects$chamber <- "Lower"

full_model_upper_2010 <- summary(full_model_upper_2010, cluster = c("state"))
full_model_upper_2010_effects <- data.frame(full_model_upper_2010$coeftable)
colnames(full_model_upper_2010_effects) <- c("estimate","se","t_value","p_value")
full_model_upper_2010_effects$effect <- rownames(full_model_upper_2010_effects)
rownames(full_model_upper_2010_effects) <- NULL
full_model_upper_2010_effects$nobs <- nobs(full_model_upper_2010)
full_model_upper_2010_effects$model <- "2010 Model"
full_model_upper_2010_effects$chamber <- "Upper"

full_model_all_2010 <- summary(full_model_all_2010, cluster = c("chamber_state_fe"))
full_model_all_2010_effects <- data.frame(full_model_all_2010$coeftable)
colnames(full_model_all_2010_effects) <- c("estimate","se","t_value","p_value")
full_model_all_2010_effects$effect <- rownames(full_model_all_2010_effects)
rownames(full_model_all_2010_effects) <- NULL
full_model_all_2010_effects$nobs <- nobs(full_model_all_2010)
full_model_all_2010_effects$model <- "2010 Model"
full_model_all_2010_effects$chamber <- "All"

full_model_lower_2012 <- summary(full_model_lower_2012, cluster = c("state"))
full_model_lower_2012_effects <- data.frame(full_model_lower_2012$coeftable)
colnames(full_model_lower_2012_effects) <- c("estimate","se","t_value","p_value")
full_model_lower_2012_effects$effect <- rownames(full_model_lower_2012_effects)
rownames(full_model_lower_2012_effects) <- NULL
full_model_lower_2012_effects$nobs <- nobs(full_model_lower_2012)
full_model_lower_2012_effects$model <- "2012 Model"
full_model_lower_2012_effects$chamber <- "Lower"

full_model_upper_2012 <- summary(full_model_upper_2012, cluster = c("state"))
full_model_upper_2012_effects <- data.frame(full_model_upper_2012$coeftable)
colnames(full_model_upper_2012_effects) <- c("estimate","se","t_value","p_value")
full_model_upper_2012_effects$effect <- rownames(full_model_upper_2012_effects)
rownames(full_model_upper_2012_effects) <- NULL
full_model_upper_2012_effects$nobs <- nobs(full_model_upper_2012)
full_model_upper_2012_effects$model <- "2012 Model"
full_model_upper_2012_effects$chamber <- "Upper"

full_model_all_2012 <- summary(full_model_all_2012, cluster = c("chamber_state_fe"))
full_model_all_2012_effects <- data.frame(full_model_all_2012$coeftable)
colnames(full_model_all_2012_effects) <- c("estimate","se","t_value","p_value")
full_model_all_2012_effects$effect <- rownames(full_model_all_2012_effects)
rownames(full_model_all_2012_effects) <- NULL
full_model_all_2012_effects$nobs <- nobs(full_model_all_2012)
full_model_all_2012_effects$model <- "2012 Model"
full_model_all_2012_effects$chamber <- "All"

full_model_lower_2014 <- summary(full_model_lower_2014, cluster = c("state"))
full_model_lower_2014_effects <- data.frame(full_model_lower_2014$coeftable)
colnames(full_model_lower_2014_effects) <- c("estimate","se","t_value","p_value")
full_model_lower_2014_effects$effect <- rownames(full_model_lower_2014_effects)
rownames(full_model_lower_2014_effects) <- NULL
full_model_lower_2014_effects$nobs <- nobs(full_model_lower_2014)
full_model_lower_2014_effects$model <- "2014 Model"
full_model_lower_2014_effects$chamber <- "Lower"

full_model_upper_2014 <- summary(full_model_upper_2014, cluster = c("state"))
full_model_upper_2014_effects <- data.frame(full_model_upper_2014$coeftable)
colnames(full_model_upper_2014_effects) <- c("estimate","se","t_value","p_value")
full_model_upper_2014_effects$effect <- rownames(full_model_upper_2014_effects)
rownames(full_model_upper_2014_effects) <- NULL
full_model_upper_2014_effects$nobs <- nobs(full_model_upper_2014)
full_model_upper_2014_effects$model <- "2014 Model"
full_model_upper_2014_effects$chamber <- "Upper"

full_model_all_2014 <- summary(full_model_all_2014, cluster = c("chamber_state_fe"))
full_model_all_2014_effects <- data.frame(full_model_all_2014$coeftable)
colnames(full_model_all_2014_effects) <- c("estimate","se","t_value","p_value")
full_model_all_2014_effects$effect <- rownames(full_model_all_2014_effects)
rownames(full_model_all_2014_effects) <- NULL
full_model_all_2014_effects$nobs <- nobs(full_model_all_2014)
full_model_all_2014_effects$model <- "2014 Model"
full_model_all_2014_effects$chamber <- "All"

full_model_lower_2016 <- summary(full_model_lower_2016, cluster = c("state"))
full_model_lower_2016_effects <- data.frame(full_model_lower_2016$coeftable)
colnames(full_model_lower_2016_effects) <- c("estimate","se","t_value","p_value")
full_model_lower_2016_effects$effect <- rownames(full_model_lower_2016_effects)
rownames(full_model_lower_2016_effects) <- NULL
full_model_lower_2016_effects$nobs <- nobs(full_model_lower_2016)
full_model_lower_2016_effects$model <- "2016 Model"
full_model_lower_2016_effects$chamber <- "Lower"

full_model_upper_2016 <- summary(full_model_upper_2016, cluster = c("state"))
full_model_upper_2016_effects <- data.frame(full_model_upper_2016$coeftable)
colnames(full_model_upper_2016_effects) <- c("estimate","se","t_value","p_value")
full_model_upper_2016_effects$effect <- rownames(full_model_upper_2016_effects)
rownames(full_model_upper_2016_effects) <- NULL
full_model_upper_2016_effects$nobs <- nobs(full_model_upper_2016)
full_model_upper_2016_effects$model <- "2016 Model"
full_model_upper_2016_effects$chamber <- "Upper"

full_model_all_2016 <- summary(full_model_all_2016)
full_model_all_2016_effects <- data.frame(full_model_all_2016$coeftable)
colnames(full_model_all_2016_effects) <- c("estimate","se","t_value","p_value")
full_model_all_2016_effects$effect <- rownames(full_model_all_2016_effects)
rownames(full_model_all_2016_effects) <- NULL
full_model_all_2016_effects$nobs <- nobs(full_model_all_2016)
full_model_all_2016_effects$model <- "2016 Model"
full_model_all_2016_effects$chamber <- "All"

full_model_lower_2018 <- summary(full_model_lower_2018, cluster = c("state"))
full_model_lower_2018_effects <- data.frame(full_model_lower_2018$coeftable)
colnames(full_model_lower_2018_effects) <- c("estimate","se","t_value","p_value")
full_model_lower_2018_effects$effect <- rownames(full_model_lower_2018_effects)
rownames(full_model_lower_2018_effects) <- NULL
full_model_lower_2018_effects$nobs <- nobs(full_model_lower_2018)
full_model_lower_2018_effects$model <- "2018 Model"
full_model_lower_2018_effects$chamber <- "Lower"

full_model_upper_2018 <- summary(full_model_upper_2018, cluster = c("state"))
full_model_upper_2018_effects <- data.frame(full_model_upper_2018$coeftable)
colnames(full_model_upper_2018_effects) <- c("estimate","se","t_value","p_value")
full_model_upper_2018_effects$effect <- rownames(full_model_upper_2018_effects)
rownames(full_model_upper_2018_effects) <- NULL
full_model_upper_2018_effects$nobs <- nobs(full_model_upper_2018)
full_model_upper_2018_effects$model <- "2018 Model"
full_model_upper_2018_effects$chamber <- "Upper"

full_model_all_2018 <- summary(full_model_all_2018, cluster = c("chamber_state_fe"))
full_model_all_2018_effects <- data.frame(full_model_all_2018$coeftable)
colnames(full_model_all_2018_effects) <- c("estimate","se","t_value","p_value")
full_model_all_2018_effects$effect <- rownames(full_model_all_2018_effects)
rownames(full_model_all_2018_effects) <- NULL
full_model_all_2018_effects$nobs <- nobs(full_model_all_2018)
full_model_all_2018_effects$model <- "2018 Model"
full_model_all_2018_effects$chamber <- "All"

full_model_lower_2020 <- summary(full_model_lower_2020, cluster = c("state"))
full_model_lower_2020_effects <- data.frame(full_model_lower_2020$coeftable)
colnames(full_model_lower_2020_effects) <- c("estimate","se","t_value","p_value")
full_model_lower_2020_effects$effect <- rownames(full_model_lower_2020_effects)
rownames(full_model_lower_2020_effects) <- NULL
full_model_lower_2020_effects$nobs <- nobs(full_model_lower_2020)
full_model_lower_2020_effects$model <- "2020 Model"
full_model_lower_2020_effects$chamber <- "Lower"

full_model_upper_2020 <- summary(full_model_upper_2020, cluster = c("state"))
full_model_upper_2020_effects <- data.frame(full_model_upper_2020$coeftable)
colnames(full_model_upper_2020_effects) <- c("estimate","se","t_value","p_value")
full_model_upper_2020_effects$effect <- rownames(full_model_upper_2020_effects)
rownames(full_model_upper_2020_effects) <- NULL
full_model_upper_2020_effects$nobs <- nobs(full_model_upper_2020)
full_model_upper_2020_effects$model <- "2020 Model"
full_model_upper_2020_effects$chamber <- "Upper"

full_model_all_2020 <- summary(full_model_all_2020, cluster = c("chamber_state_fe"))
full_model_all_2020_effects <- data.frame(full_model_all_2020$coeftable)
colnames(full_model_all_2020_effects) <- c("estimate","se","t_value","p_value")
full_model_all_2020_effects$effect <- rownames(full_model_all_2020_effects)
rownames(full_model_all_2020_effects) <- NULL
full_model_all_2020_effects$nobs <- nobs(full_model_all_2020)
full_model_all_2020_effects$model <- "2020 Model"
full_model_all_2020_effects$chamber <- "All"

# Combine datasets

models <- rbind(full_model_lower_effects,full_model_upper_effects,full_model_all_effects,full_model_lower_2007_effects,full_model_upper_2007_effects,full_model_all_2007_effects,full_model_lower_2008_effects,full_model_upper_2008_effects,full_model_all_2008_effects,full_model_lower_2010_effects,full_model_upper_2010_effects,full_model_all_2010_effects,full_model_lower_2012_effects,full_model_upper_2012_effects,full_model_all_2012_effects,full_model_lower_2014_effects,full_model_upper_2014_effects,full_model_all_2014_effects,full_model_lower_2016_effects,full_model_upper_2016_effects,full_model_all_2016_effects,full_model_lower_2018_effects,full_model_upper_2018_effects,full_model_all_2018_effects,full_model_lower_2020_effects,full_model_upper_2020_effects,full_model_all_2020_effects)
models2 <- models

models <- subset(models,models$effect %in% c("dem_governor::0:gov_approve_coded","dem_governor::1:gov_approve_coded","dem_president::0:pres_approve_coded","dem_president::1:pres_approve_coded","i(factor_var = dem_president, var = pres_approve_coded, ref_special = TRUE)"))
models$effect <- ifelse(models$effect %in% "i(factor_var = dem_president, var = pres_approve_coded, ref_special = TRUE)" & models$model %in% c("2007 Model","2008 Model","2018 Model","2020 Model"),"dem_president::0:pres_approve_coded",ifelse(models$effect %in% "i(factor_var = dem_president, var = pres_approve_coded, ref_special = TRUE)" & models$model %in% c("2010 Model","2012 Model","2014 Model","2016 Model"),"dem_president::1:pres_approve_coded",models$effect))

models$lower_90 <- models$estimate - qt(0.95,df=models$nobs) * models$se 
models$upper_90 <- models$estimate + qt(0.95,df=models$nobs) * models$se 

models$lower <- models$estimate - qt(0.975,df=models$nobs) * models$se 
models$upper <- models$estimate + qt(0.975,df=models$nobs) * models$se 

models$color_sig <- ifelse(models$p_value < 0.10,"sig","no sig")
models$shape <- ifelse(models$chamber == "All",1,ifelse(models$chamber == "Lower",22,23))

y <- subset(models,models$effect %in% c("dem_president::0:pres_approve_coded","dem_president::1:pres_approve_coded"))
y$model <- gsub("Model","\nModel",y$model)
y$model <- gsub("Baseline","\nBaseline",y$model)

y$model <- factor(y$model,levels=c("Pooled \nBaseline","2007 \nModel","2008 \nModel","2010 \nModel","2012 \nModel","2014 \nModel","2016 \nModel","2018 \nModel","2020 \nModel"))

y$chamber <- ifelse(y$chamber == "Lower","Lower Legislative Chambers",ifelse(y$chamber == "Upper","Upper Legislative Chambers",ifelse(y$chamber == "All","Pooled Legislative Chambers",NA)))

y$chamber <- factor(y$chamber,levels=c("Lower Legislative Chambers","Upper Legislative Chambers","Pooled Legislative Chambers"))

y$label <- round(y$estimate,3)
y$label[y$label == 0.1] <- "0.10"
y$color_sig <- ifelse(y$p_value < 0.10,"sig","no sig")
y$effect <- ifelse(y$effect == "dem_president::0:pres_approve_coded","Republican President",ifelse(y$effect == "dem_president::1:pres_approve_coded","Democratic President",NA))
y$effect <- factor(y$effect,levels=c("Republican President","Democratic President"))

pres <- subset(y,y$model == "Pooled \nBaseline")

plot <- ggplot(y,aes(x=model,y=estimate,group=effect,shape=effect,fill=effect,color=effect,label=label)) + theme_minimal() + facet_wrap(~chamber) + geom_linerange(aes(x= model, ymin = lower_90, ymax = upper_90), lwd  = 1) + geom_pointrange(aes(x= model, ymin = lower, ymax = upper,shape=effect,fill=effect), lwd = 1/2) + theme_bw() + geom_hline(yintercept = 0, colour = "red", lty = 2) + coord_flip() + theme(legend.position = "bottom") + scale_y_continuous("Estimated Marginal Effect on State Legislative Voting (Democratic Party)",breaks=seq(-0.25,0.25,0.05)) + scale_fill_manual("",values=rep("white",30)) + scale_shape_manual("",values=c(22,21)) + scale_x_discrete("") + theme(axis.text.x = element_text(hjust = 0.5),axis.text.y = element_text(hjust = 0.5)) + scale_color_manual("",values=c("red","blue")) + geom_text(vjust=-1,show.legend=F) + labs(caption="Each panel articulates nine model results for each individual cross-sectional year and a baseline model. \nTotal Model N = 27. 90% & 95% Model CIs estimated from year-chamber clustered robust standard errors. \nCross-Sectional Estimates derived from one-way state-chamber fixed effects models with vector of voter-level controls. \nBaseline estimates derived from two-way year-state legislative chamber fixed effects models with vector of voter-level controls.")
ggsave(file="Fig6A_pres_approval_cces_effects_dem_direction.png", plot, width = 9, height = 6, units = "in")  
print(plot)

y <- subset(models,!(models$effect %in% c("dem_president::0:pres_approve_coded","dem_president::1:pres_approve_coded")))
y$model <- gsub("Model","\nModel",y$model)
y$model <- gsub("Baseline","\nBaseline",y$model)

y$model <- factor(y$model,levels=c("Pooled \nBaseline","2007 \nModel","2008 \nModel","2010 \nModel","2012 \nModel","2014 \nModel","2016 \nModel","2018 \nModel","2020 \nModel"))

y$chamber <- ifelse(y$chamber == "Lower","Lower Legislative Chambers",ifelse(y$chamber == "Upper","Upper Legislative Chambers",ifelse(y$chamber == "All","Pooled Legislative Chambers",NA)))

y$chamber <- factor(y$chamber,levels=c("Lower Legislative Chambers","Upper Legislative Chambers","Pooled Legislative Chambers"))

y$effect <- ifelse(y$effect %in% "dem_governor::0:gov_approve_coded","Republican Governor",ifelse(y$effect %in% "dem_governor::1:gov_approve_coded","Democratic Governor",NA))
y$effect <- factor(y$effect,levels=c("Republican Governor","Democratic Governor"))
y$label <- round(y$estimate,3)
y$label <- ifelse(y$color_sig == "no sig",NA,y$label)

govs <- subset(y,y$model == "Pooled \nBaseline")

plot <- ggplot(y,aes(x=model,y=estimate,group=effect,alpha=color_sig,shape=effect,fill=effect,color=effect,label=label)) + theme_minimal() + facet_wrap(~chamber) + geom_linerange(aes(x= model, ymin = lower_90, ymax = upper_90), lwd  = 1) + geom_pointrange(aes(x= model, ymin = lower, ymax = upper,shape=effect,fill=effect), lwd = 1/2) + theme_bw() + geom_hline(yintercept = 0, colour = "red", lty = 2) + coord_flip() + theme(legend.position = "bottom") + scale_y_continuous("Estimated Marginal Effect on State Legislative Voting (Democratic Party)",breaks=seq(-0.05,.05,0.02)) + scale_fill_manual("",values=rep("white",54)) + scale_alpha_manual("",values=c(0.2,1),guide="none") + scale_shape_manual("",values=c(22,21)) + scale_x_discrete("") + theme(axis.text.x = element_text(hjust = 0.5),axis.text.y = element_text(hjust = 0.5)) + labs(caption="Each panel articulates nine model results for each individual cross-sectional year and a baseline model. \nTotal Model N = 27. 90% & 95% Model CIs estimated from year-chamber clustered robust standard errors. \nCross-Sectional Estimates derived from one-way state-chamber fixed effects models with vector of voter-level controls. \nBaseline estimates derived from two-way year-state legislative chamber fixed effects models with vector of voter-level controls.") + scale_color_manual("",values=c("red","blue")) + geom_text(vjust=-1,show.legend=F,size=3)
ggsave(file="Fig6B_gov_approval_cces_effects_dem_direction.png", plot, width = 9, height = 6, units = "in")  
print(plot)

additive_model_results <- models

models2 <- subset(models2,models2$effect %in% c("all_leg_pres_party::Republican Legislature:state_leg_approve_coded","all_leg_pres_party::Split Legislature:state_leg_approve_coded","all_leg_pres_party::Democratic Legislature:state_leg_approve_coded","dem_congress::0:cong_approval_coded","dem_congress::1:cong_approval_coded","dem_state_house::0:state_leg_approve_coded","dem_legislature::Democratic Legislature:state_leg_approve_coded","dem_legislature::Republican Legislature:state_leg_approve_coded","dem_legislature::Split Legislature:state_leg_approve_coded","i(factor_var = dem_congress, var = cong_approval_coded, ref_special = TRUE)","dem_congress::Split:cong_approval_coded","dem_congress::Republican:cong_approval_coded","dem_congress::Democratic:cong_approval_coded"))

models2$effect <- ifelse(models2$effect %in% "i(factor_var = dem_congress, var = cong_approval_coded, ref_special = TRUE)" & models2$model %in% c("2007 Model","2008 Model","2009 Model","2010 Model"),"dem_congress::Democratic:cong_approval_coded",ifelse(models2$effect %in% "i(factor_var = dem_congress, var = cong_approval_coded, ref_special = TRUE)" & models2$model %in% c("2006 Model","2015 Model","2016 Model","2017 Model","2018 Model"),"dem_congress::Republican:cong_approval_coded",ifelse(models2$effect %in% "i(factor_var = dem_congress, var = cong_approval_coded, ref_special = TRUE)" & models2$model %in% c("2011 Model","2012 Model","2013 Model","2014 Model","2019 Model","2020 Model"),"dem_congress::Split:cong_approval_coded",models2$effect)))

models2$effect <- gsub("dem_legislature","dem_chamber",models2$effect)
models2$effect <- gsub("all_leg_pres_party","dem_chamber",models2$effect)

models2$lower_90 <- models2$estimate - qt(0.95,df=models2$nobs) * models2$se 
models2$upper_90 <- models2$estimate + qt(0.95,df=models2$nobs) * models2$se 

models2$lower <- models2$estimate - qt(0.975,df=models2$nobs) * models2$se 
models2$upper <- models2$estimate + qt(0.975,df=models2$nobs) * models2$se 

models2$color_sig <- ifelse(models2$p_value < 0.10,"sig","no sig")
models2$shape <- ifelse(models2$chamber == "All",1,ifelse(models2$chamber == "Lower",22,23))

y <- subset(models2,models2$effect %in% c("dem_congress::Republican:cong_approval_coded","dem_congress::Split:cong_approval_coded","dem_congress::Democratic:cong_approval_coded"))
y$model <- gsub("Model","\nModel",y$model)
y$model <- gsub("Baseline","\nBaseline",y$model)

y$model <- factor(y$model,levels=c("Pooled \nBaseline","2007 \nModel","2008 \nModel","2010 \nModel","2012 \nModel","2014 \nModel","2016 \nModel","2018 \nModel","2020 \nModel"))

y$chamber <- ifelse(y$chamber == "Lower","Lower Legislative Chambers",ifelse(y$chamber == "Upper","Upper Legislative Chambers",ifelse(y$chamber == "All","Pooled Legislative Chambers",NA)))

y$chamber <- factor(y$chamber,levels=c("Lower Legislative Chambers","Upper Legislative Chambers","Pooled Legislative Chambers"))

y$label <- round(y$estimate,3)
y$color_sig <- ifelse(y$p_value < 0.10,"sig","no sig")
y$label <- ifelse(y$color_sig == "no sig",NA,y$label)

y$effect <- ifelse(y$effect == "dem_congress::Republican:cong_approval_coded","Republican Congress",ifelse(y$effect == "dem_congress::Democratic:cong_approval_coded","Democratic Congress","Split Congress"))
y$effect <- factor(y$effect,levels=c("Republican Congress","Split Congress","Democratic Congress"))

pres <- subset(y,y$model == "Pooled \nBaseline")

plot <- ggplot(y,aes(x=model,y=estimate,group=effect,alpha=color_sig,shape=effect,fill=effect,color=effect,label=label)) + theme_minimal() + facet_wrap(~chamber) + geom_linerange(aes(x= model, ymin = lower_90, ymax = upper_90), lwd  = 1) + geom_pointrange(aes(x= model, ymin = lower, ymax = upper,shape=effect,fill=effect), lwd = 1/2) + theme_bw() + geom_hline(yintercept = 0, colour = "red", lty = 2) + coord_flip() + theme(legend.position = "bottom") + scale_y_continuous("Estimated Marginal Effect on State Legislative Voting (Democratic Party)",breaks=seq(-0.05,.05,0.02)) + scale_fill_manual("",values=rep("white",33)) + scale_alpha_manual("",values=c(0.2,1),guide="none") + scale_shape_manual("",values=c(22,23,21)) + scale_x_discrete("") + theme(axis.text.x = element_text(hjust = 0.5),axis.text.y = element_text(hjust = 0.5)) + labs(caption="Each panel articulates nine model results for each individual cross-sectional year and a baseline model. \nTotal Model N = 27. 90% & 95% Model CIs estimated from year-chamber clustered robust standard errors. \nCross-Sectional Estimates derived from one-way state-chamber fixed effects models with vector of voter-level controls. \nBaseline estimates derived from two-way year-state legislative chamber fixed effects models with vector of voter-level controls.") + scale_color_manual("",values=c("red","purple","blue")) + geom_text(vjust=-1,show.legend=F,size=3)
ggsave(file="Fig6C_cong_approval_cces_effects_dem_direction.png", plot, width = 9, height = 6, units = "in")  
print(plot)

y <- subset(models2,models2$effect %in% c("dem_chamber::Democratic Legislature:state_leg_approve_coded","dem_chamber::Republican Legislature:state_leg_approve_coded","dem_chamber::Split Legislature:state_leg_approve_coded"))
y$model <- gsub("Model","\nModel",y$model)
y$model <- gsub("Baseline","\nBaseline",y$model)

y$model <- factor(y$model,levels=c("Pooled \nBaseline","2007 \nModel","2008 \nModel","2010 \nModel","2012 \nModel","2014 \nModel","2016 \nModel","2018 \nModel","2020 \nModel"))

y$chamber <- ifelse(y$chamber == "Lower","Lower Legislative Chambers",ifelse(y$chamber == "Upper","Upper Legislative Chambers",ifelse(y$chamber == "All","Pooled Legislative Chambers",NA)))

y$chamber <- factor(y$chamber,levels=c("Lower Legislative Chambers","Upper Legislative Chambers","Pooled Legislative Chambers"))

y$effect <- ifelse(y$effect %in% "dem_chamber::Republican Legislature:state_leg_approve_coded","Republican Legislature",ifelse(y$effect %in% "dem_chamber::Democratic Legislature:state_leg_approve_coded","Democratic Legislature",ifelse(y$effect %in% "dem_chamber::Split Legislature:state_leg_approve_coded","Split Legislature",NA)))

y$effect <- factor(y$effect,levels=c("Republican Legislature","Split Legislature","Democratic Legislature"))
y$label <- round(y$estimate,3)
y$label <- ifelse(y$color_sig == "no sig",NA,y$label)

govs <- subset(y,y$model == "Pooled \nBaseline")

plot <- ggplot(y,aes(x=model,y=estimate,group=effect,alpha=color_sig,shape=effect,fill=effect,color=effect,label=label)) + theme_minimal() + facet_wrap(~chamber) + geom_linerange(aes(x= model, ymin = lower_90, ymax = upper_90), lwd  = 1) + geom_pointrange(aes(x= model, ymin = lower, ymax = upper,shape=effect,fill=effect), lwd = 1/2) + theme_bw() + geom_hline(yintercept = 0, colour = "red", lty = 2) + coord_flip() + theme(legend.position = "bottom") + scale_y_continuous("Estimated Marginal Effect on State Legislative Voting (Democratic Party)",breaks=seq(-0.05,.05,0.02)) + scale_fill_manual("",values=rep("white",81)) + scale_alpha_manual("",values=c(0.2,1),guide="none") + scale_shape_manual("",values=c(22,23,21)) + scale_x_discrete("") + theme(axis.text.x = element_text(hjust = 0.5),axis.text.y = element_text(hjust = 0.5)) + labs(caption="Each panel articulates nine model results for each individual cross-sectional year and a baseline model. \nTotal Model N = 27. 90% & 95% Model CIs estimated from year-chamber clustered robust standard errors. \nCross-Sectional Estimates derived from one-way state-chamber fixed effects models with vector of voter-level controls. \nBaseline estimates derived from two-way year-state legislative chamber fixed effects models with vector of voter-level controls.") + scale_color_manual("",values=c("red","purple","blue")) + geom_text(vjust=-1,show.legend=F,size=3)
ggsave(file="Fig6D_state_leg_approval_cces_effects_dem_direction.png", plot, width = 9, height = 6, units = "in")  
print(plot)

# Histogram of Effects

x <- subset(additive_model_results,additive_model_results$color_sig == "sig")
#x$model <- "Additive Model"

x$effect <- ifelse(x$effect %in% c("dem_governor::0:gov_approve_coded","dem_governor::1:gov_approve_coded"),"Gubernatorial Approval","Presidential Approval")

hist <- rbind.fill(x)
median(abs(hist$estimate[hist$effect == "Presidential Approval" & hist$model == "Additive Model"]))
median(abs(hist$estimate[hist$effect == "Gubernatorial Approval" & hist$model == "Additive Model"]))

median(abs(hist$estimate[hist$effect == "Presidential Approval" & hist$model == "Interactive Partisan Heterogenity Model"]))
median(abs(hist$estimate[hist$effect == "Gubernatorial Approval" & hist$model == "Interactive Partisan Heterogenity Model"]))

#hist <- subset(hist,hist$model == "Additive Model")

x <- additive_model_results
#x$model <- "Additive Model"

x$effect2 <- ifelse(x$effect %in% c("dem_governor::0:gov_approve_coded","dem_governor::1:gov_approve_coded"),"Gubernatorial Approval","Presidential Approval")

table(x$color_sig[x$effect2 == "Gubernatorial Approval"])
49/54
table(x$color_sig[x$effect2 == "Presidential Approval"])

summary(abs(hist$estimate[hist$effect == "Presidential Approval"]))
summary(abs(hist$estimate[hist$effect == "Gubernatorial Approval"]))

plot <- ggplot() + stat_density(data=subset(hist,hist$effect == "Gubernatorial Approval"),aes(x=abs(estimate),group=effect,fill=effect,linetype=effect,color=effect),alpha=0.2,geom="area") + stat_density(data=subset(hist,hist$effect == "Presidential Approval"),aes(x=abs(estimate),group=effect,fill=effect,linetype=effect,color=effect),alpha=0.2,geom="area") + theme_bw() + theme(legend.position = "bottom") + scale_x_continuous("Absolute Value of Significant Approval Estimate",limits=c(0,0.25)) + scale_fill_manual("",values=c("#F9866D","#00BFC4")) + scale_linetype_manual("",values=c("solid","dashed")) + scale_color_manual("",values=c("#F9866D","#00BFC4")) + scale_y_continuous("Density") + labs(caption="49/54 (91%) gubernatorial effects & 30/30 presidential effects significant. \nAbsolute Effect Sizes: Median Pres. Approval = 0.13 & Median Gov. Approval = 0.02. \nTotal Models = 27 Models, 9 for each legislative context (1 pooled baseline model + 8 cross-sectional models). \nTwo estimated gubernatorial effects per model. One estimated presidential effect per cross-sectional model & two presidential effects per pooled baseline model.")
ggsave(file="Fig7_additive_effects_histogram.png", plot, width = 9, height = 6, units = "in")  
