####################
# Wrangling data & creating variables
# for analysis of WERN survey experiment
####################
# Created By: John Ahlquist 020923, Jake Grumbach 020603




setwd("../data")

data <- read.csv("wern_analysis_v3.0.csv"); gc()




#QIR index from Eric
data$race_create_conflict_num_rev <- 6 - data$race_create_conflict_num
qir_index_vars <- c("alone_help_num", "alone_activity_num", "worker_support_num", "race_create_conflict_num_rev")

for(i in qir_index_vars){
  
  data$temp <- NA
  data$temp <- data[,i]
  data$temp[data$temp==-77] <- NA
  data$temp <- (data$temp-min(data$temp, na.rm=T))/(max(data$temp, na.rm=T)-min(data$temp, na.rm=T))
  
  data[,paste(i, "01", sep="")] <- data$temp
}

data$qir_index <- rowSums(data[,c("alone_help_num01","alone_activity_num01","worker_support_num01","race_create_conflict_num_rev01")], na.rm=T)/4


data$x_wage_theft_index <- data$wage_theft_index/2


outcome_vars <- c("petition", "union", "social_med", "donate", "call_rep", "strike")

for(i in outcome_vars){
  
  i <- paste(i, "_num", sep="")
  
  data$temp <- NA
  data$temp[is.na(data$temp)] <- data[is.na(data$temp),paste("vignette_control_", i, sep="")]
  data$temp[is.na(data$temp)] <- data[is.na(data$temp),paste("vignette_coworkertreat_", i, sep="")]
  data$temp[is.na(data$temp)] <- data[is.na(data$temp),paste("vignette_neighbortreat_", i, sep="")]
  
  data$temp[data$temp==-77] <- NA  
  
  data$temp <- (data$temp-min(data$temp, na.rm=T))/(max(data$temp, na.rm=T)-min(data$temp, na.rm=T))
  
  data[,paste("dv_", i, sep="")] <- data$temp
  
}

data$dv_index <- rowSums(data[,grepl("dv_", names(data))])
data$dv_index <- (data$dv_index-min(data$dv_index, na.rm=T))/(max(data$dv_index, na.rm=T)-min(data$dv_index, na.rm=T))

data$x_age <- 2022 - data$age_num 

data$x_race <- data$race
data$x_race[data$race %in% c("American Indian or Alaska Native", "Other")] <- "Other"
data$x_race[data$race=="Hispanic or Latino/Latina"] <- "Latino"
data$x_race[data$race=="Black or African American"] <- "Black"
data$x_race[data$race=="White"] <- "White"

data$x_white <- NA
data$x_white[data$x_race=="White"] <- 1
data$x_white[data$x_race!="White"] <- 0

data$x_gender <- data$gender
data$x_gender[data$gender=="Non-Binary/Gender Non-conforming"] <- "Other"

data$female <- NA
data$female[data$gender=="Female"] <- 1
data$female[data$gender!="Female"] <- 0



saveRDS(data, "wern_experiment_analysis_data.rds") #outputting data file for analysis

