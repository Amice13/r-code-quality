######################## Code Summary ##################
#Replication for The Impact of Vote-By-Mail Policy on Turnout and Vote Share in the 2020 Election
#Election Law Journal 
#Amlani & Collitt (2021)
#R Version: 4.1.1
#December 8 2021

#This R script creates Table 1: Joint Distribution of Vote-by-Mail Policies in 2016 and 2020  
########################## Prelude #####################

rm(list=ls(all=TRUE))
options(stringsAsFactors = FALSE)
options(scipen =99)
set.seed(1993)

######################## Upload Data ##################

#This neat snippet sets your working directory to wherever the script is located!
setwd(dirname(rstudioapi::getActiveDocumentContext()$path))

#Upload Data
load(file = "Replication Data - Amlani & Collitt 2021.rda"); VBM_Master.1 <- VBM_Master.Pure

##################### Subset Cases Less Than 100 Percent VAP ##################### 
##################### 2020 Only Data##################### 
VBM_Master_2020.1<- subset(VBM_Master.1, year == 2020)

#********************** Fix Levels **************************
VBM_Master_2020.1$Mail_In_Vote_Lagged <- factor(VBM_Master_2020.1$Mail_In_Vote_Lagged, levels = c("Absentee with excuse", "No-excuse absentee", "Mail-in ballots automatically sent"))

VBM_Master_2020.1$Mail_In_Vote <- factor(VBM_Master_2020.1$Mail_In_Vote, levels = c("need an excuse", "request a mail-in ballot", "Mail-in ballot applications automatically sent", "Mail-in ballots automatically sent"))

###################### Table 1 #########################
#Table 1: Joint Distribution of Vote-by-Mail Policies in 2016 and 2020 

#Raw Numbers 
table(VBM_Master_2020.1$Mail_In_Vote_Lagged, VBM_Master_2020.1$Mail_In_Vote)

#Percent of Sample
round(table(VBM_Master_2020.1$Mail_In_Vote_Lagged, VBM_Master_2020.1$Mail_In_Vote)/nrow(VBM_Master_2020.1),2)*100
