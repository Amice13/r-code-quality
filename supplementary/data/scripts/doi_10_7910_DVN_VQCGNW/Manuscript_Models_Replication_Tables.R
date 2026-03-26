library(lmtest)
library(MASS)
library(sandwich)
library(multiwayvcov)
library(pscl)
library(plyr)
library(reshape2)
library(descr)

setwd("/Final Data Replication Files")

set.seed(123)

# This R script contains the code to replicate the analysis and model tables in the Political Behavior Manuscript: "Congressional Approval & Responsible Party Government: The Role of Partisanship & Ideology in Citizen Assessments of the Contemporary U.S. Congress."

###### Table 1 & 2: CCES Roll-Call Support Tables ###### 

load("roll_call_table_data.Rdata")

# Table 1 Panel Data

respondents_panel <- list()
for(i in 6:12){
x <- rollcall2010_panel[,c(4,i)]
respondents <- subset(x,x$group %in% "CCES Respondent")
respondents <- data.frame(freq(respondents[,2]))
respondents$response <- rownames(respondents)
respondents <- subset(respondents,respondents$response != "Total")
respondents$group <- "CCES Respondents"
respondents$vote <- colnames(x)[2]
respondents$Valid.Percent <- NULL
respondents_panel[[i]] <- respondents
}
respondents_panel <- ldply(respondents_panel,data.frame)

senators_panel <- list()
for(i in c(6,8:12)){
  x <- rollcall2010_panel[,c(4,i)]
  respondents <- subset(x,x$group %in% "U.S. Senate")
  respondents <- data.frame(freq(respondents[,2]))
  respondents$response <- rownames(respondents)
  respondents <- subset(respondents,respondents$response != "Total")
  respondents$group <- "U.S. Senate"
  respondents$vote <- colnames(x)[2]
  respondents$Valid.Percent <- NULL
  senators_panel[[i]] <- respondents
}
senators_panel <- ldply(senators_panel,data.frame)

mcs_panel <- list()
for(i in c(6:11)){
  x <- rollcall2010_panel[,c(4,i)]
  respondents <- subset(x,x$group %in% "U.S. House")
  respondents <- data.frame(freq(respondents[,2]))
  respondents$response <- rownames(respondents)
  respondents <- subset(respondents,respondents$response != "Total")
  respondents$group <- "U.S. House"
  respondents$vote <- colnames(x)[2]
  respondents$Valid.Percent <- NULL
  mcs_panel[[i]] <- respondents
}
mcs_panel <- ldply(mcs_panel,data.frame)

table1_cces_panel_roll_call_percents <- rbind(respondents_panel,senators_panel,mcs_panel)
table1_cces_panel_roll_call_percents <- reshape(data = table1_cces_panel_roll_call_percents, idvar= c("group","response"),v.names= c("Frequency","Percent"), timevar= c("vote"), direction = "wide")

# Table 2 by year

# 2008
respondents_panel <- list()
for(i in 6:14){
  x <- rollcall2008[,c(4,i)]
  respondents <- subset(x,x$group %in% "CCES Respondent")
  respondents <- data.frame(freq(respondents[,2]))
  respondents$response <- rownames(respondents)
  respondents <- subset(respondents,respondents$response != "Total")
  respondents$group <- "CCES Respondents"
  respondents$vote <- colnames(x)[2]
  respondents$Valid.Percent <- NULL
  respondents_panel[[i]] <- respondents
}
respondents_panel <- ldply(respondents_panel,data.frame)

senators_panel <- list()
for(i in 6:14){
  x <- rollcall2008[,c(4,i)]
  respondents <- subset(x,x$group %in% "U.S. Senate")
  respondents <- data.frame(freq(respondents[,2]))
  respondents$response <- rownames(respondents)
  respondents <- subset(respondents,respondents$response != "Total")
  respondents$group <- "U.S. Senate"
  respondents$vote <- colnames(x)[2]
  respondents$Valid.Percent <- NULL
  senators_panel[[i]] <- respondents
}
senators_panel <- ldply(senators_panel,data.frame)

mcs_panel <- list()
for(i in 6:14){
  x <- rollcall2008[,c(4,i)]
  respondents <- subset(x,x$group %in% "U.S. House")
  respondents <- data.frame(freq(respondents[,2]))
  respondents$response <- rownames(respondents)
  respondents <- subset(respondents,respondents$response != "Total")
  respondents$group <- "U.S. House"
  respondents$vote <- colnames(x)[2]
  respondents$Valid.Percent <- NULL
  mcs_panel[[i]] <- respondents
}
mcs_panel <- ldply(mcs_panel,data.frame)

table_2008 <- rbind(respondents_panel,senators_panel,mcs_panel)
table_2008$year <- 2008

# 2009
respondents_panel <- list()
for(i in 6:12){
  x <- rollcall2009[,c(4,i)]
  respondents <- subset(x,x$group %in% "CCES Respondent")
  respondents <- data.frame(freq(respondents[,2]))
  respondents$response <- rownames(respondents)
  respondents <- subset(respondents,respondents$response != "Total")
  respondents$group <- "CCES Respondents"
  respondents$vote <- colnames(x)[2]
  respondents$Valid.Percent <- NULL
  respondents_panel[[i]] <- respondents
}
respondents_panel <- ldply(respondents_panel,data.frame)

senators_panel <- list()
for(i in c(6,9:12)){
  x <- rollcall2009[,c(4,i)]
  respondents <- subset(x,x$group %in% "U.S. Senate")
  respondents <- data.frame(freq(respondents[,2]))
  respondents$response <- rownames(respondents)
  respondents <- subset(respondents,respondents$response != "Total")
  respondents$group <- "U.S. Senate"
  respondents$vote <- colnames(x)[2]
  respondents$Valid.Percent <- NULL
  senators_panel[[i]] <- respondents
}
senators_panel <- ldply(senators_panel,data.frame)

mcs_panel <- list()
for(i in 6:11){
  x <- rollcall2009[,c(4,i)]
  respondents <- subset(x,x$group %in% "U.S. House")
  respondents <- data.frame(freq(respondents[,2]))
  respondents$response <- rownames(respondents)
  respondents <- subset(respondents,respondents$response != "Total")
  respondents$group <- "U.S. House"
  respondents$vote <- colnames(x)[2]
  respondents$Valid.Percent <- NULL
  mcs_panel[[i]] <- respondents
}
mcs_panel <- ldply(mcs_panel,data.frame)

table_2009 <- rbind(respondents_panel,senators_panel,mcs_panel)
table_2009$year <- 2009

# 2010
respondents_panel <- list()
for(i in 6:15){
  x <- rollcall2010[,c(4,i)]
  respondents <- subset(x,x$group %in% "CCES Respondent")
  respondents <- data.frame(freq(respondents[,2]))
  respondents$response <- rownames(respondents)
  respondents <- subset(respondents,respondents$response != "Total")
  respondents$group <- "CCES Respondents"
  respondents$vote <- colnames(x)[2]
  respondents$Valid.Percent <- NULL
  respondents_panel[[i]] <- respondents
}
respondents_panel <- ldply(respondents_panel,data.frame)

senators_panel <- list()
for(i in c(6,8:11,15)){
  x <- rollcall2010[,c(4,i)]
  respondents <- subset(x,x$group %in% "U.S. Senate")
  respondents <- data.frame(freq(respondents[,2]))
  respondents$response <- rownames(respondents)
  respondents <- subset(respondents,respondents$response != "Total")
  respondents$group <- "U.S. Senate"
  respondents$vote <- colnames(x)[2]
  respondents$Valid.Percent <- NULL
  senators_panel[[i]] <- respondents
}
senators_panel <- ldply(senators_panel,data.frame)

mcs_panel <- list()
for(i in 6:14){
  x <- rollcall2010[,c(4,i)]
  respondents <- subset(x,x$group %in% "U.S. House")
  respondents <- data.frame(freq(respondents[,2]))
  respondents$response <- rownames(respondents)
  respondents <- subset(respondents,respondents$response != "Total")
  respondents$group <- "U.S. House"
  respondents$vote <- colnames(x)[2]
  respondents$Valid.Percent <- NULL
  mcs_panel[[i]] <- respondents
}
mcs_panel <- ldply(mcs_panel,data.frame)

table_2010 <- rbind(respondents_panel,senators_panel,mcs_panel)
table_2010$year <- 2010

#2011
respondents_panel <- list()
for(i in 6:12){
  x <- rollcall2011[,c(4,i)]
  respondents <- subset(x,x$group %in% "CCES Respondent")
  respondents <- data.frame(freq(respondents[,2]))
  respondents$response <- rownames(respondents)
  respondents <- subset(respondents,respondents$response != "Total")
  respondents$group <- "CCES Respondents"
  respondents$vote <- colnames(x)[2]
  respondents$Valid.Percent <- NULL
  respondents_panel[[i]] <- respondents
}
respondents_panel <- ldply(respondents_panel,data.frame)

senators_panel <- list()
for(i in c(6,8:10)){
  x <- rollcall2011[,c(4,i)]
  respondents <- subset(x,x$group %in% "U.S. Senate")
  respondents <- data.frame(freq(respondents[,2]))
  respondents$response <- rownames(respondents)
  respondents <- subset(respondents,respondents$response != "Total")
  respondents$group <- "U.S. Senate"
  respondents$vote <- colnames(x)[2]
  respondents$Valid.Percent <- NULL
  senators_panel[[i]] <- respondents
}
senators_panel <- ldply(senators_panel,data.frame)

mcs_panel <- list()
for(i in 6:12){
  x <- rollcall2011[,c(4,i)]
  respondents <- subset(x,x$group %in% "U.S. House")
  respondents <- data.frame(freq(respondents[,2]))
  respondents$response <- rownames(respondents)
  respondents <- subset(respondents,respondents$response != "Total")
  respondents$group <- "U.S. House"
  respondents$vote <- colnames(x)[2]
  respondents$Valid.Percent <- NULL
  mcs_panel[[i]] <- respondents
}
mcs_panel <- ldply(mcs_panel,data.frame)

table_2011 <- rbind(respondents_panel,senators_panel,mcs_panel)
table_2011$year <- 2011

#2012
respondents_panel <- list()
for(i in c(5:6,9:17)){
  x <- rollcall2012[,c(3,i)]
  respondents <- subset(x,x$group %in% "CCES Respondent")
  respondents <- data.frame(freq(respondents[,2]))
  respondents$response <- rownames(respondents)
  respondents <- subset(respondents,respondents$response != "Total")
  respondents$group <- "CCES Respondents"
  respondents$vote <- colnames(x)[2]
  respondents$Valid.Percent <- NULL
  respondents_panel[[i]] <- respondents
}
respondents_panel <- ldply(respondents_panel,data.frame)

senators_panel <- list()
for(i in c(6:9,11,13:17)){
  x <- rollcall2012[,c(3,i)]
  respondents <- subset(x,x$group %in% "U.S. Senate")
  respondents <- data.frame(freq(respondents[,2]))
  respondents$response <- rownames(respondents)
  respondents <- subset(respondents,respondents$response != "Total")
  respondents$group <- "U.S. Senate"
  respondents$vote <- colnames(x)[2]
  respondents$Valid.Percent <- NULL
  senators_panel[[i]] <- respondents
}
senators_panel <- ldply(senators_panel,data.frame)

mcs_panel <- list()
for(i in c(5:12,16:17)){
  x <- rollcall2012[,c(3,i)]
  respondents <- subset(x,x$group %in% "U.S. House")
  respondents <- data.frame(freq(respondents[,2]))
  respondents$response <- rownames(respondents)
  respondents <- subset(respondents,respondents$response != "Total")
  respondents$group <- "U.S. House"
  respondents$vote <- colnames(x)[2]
  respondents$Valid.Percent <- NULL
  mcs_panel[[i]] <- respondents
}
mcs_panel <- ldply(mcs_panel,data.frame)

table_2012 <- rbind(respondents_panel,senators_panel,mcs_panel)
table_2012$year <- 2012

#2013
respondents_panel <- list()
for(i in c(5,7:13)){
  x <- rollcall2013[,c(4,i)]
  respondents <- subset(x,x$group %in% "CCES Respondent")
  respondents <- data.frame(freq(respondents[,2]))
  respondents$response <- rownames(respondents)
  respondents <- subset(respondents,respondents$response != "Total")
  respondents$group <- "CCES Respondents"
  respondents$vote <- colnames(x)[2]
  respondents$Valid.Percent <- NULL
  respondents_panel[[i]] <- respondents
}
respondents_panel <- ldply(respondents_panel,data.frame)

senators_panel <- list()
for(i in c(7:10)){
  x <- rollcall2013[,c(4,i)]
  respondents <- subset(x,x$group %in% "U.S. Senate")
  respondents <- data.frame(freq(respondents[,2]))
  respondents$response <- rownames(respondents)
  respondents <- subset(respondents,respondents$response != "Total")
  respondents$group <- "U.S. Senate"
  respondents$vote <- colnames(x)[2]
  respondents$Valid.Percent <- NULL
  senators_panel[[i]] <- respondents
}
senators_panel <- ldply(senators_panel,data.frame)

mcs_panel <- list()
for(i in c(5,8:13)){
  x <- rollcall2013[,c(4,i)]
  respondents <- subset(x,x$group %in% "U.S. House")
  respondents <- data.frame(freq(respondents[,2]))
  respondents$response <- rownames(respondents)
  respondents <- subset(respondents,respondents$response != "Total")
  respondents$group <- "U.S. House"
  respondents$vote <- colnames(x)[2]
  respondents$Valid.Percent <- NULL
  mcs_panel[[i]] <- respondents
}
mcs_panel <- ldply(mcs_panel,data.frame)

table_2013 <- rbind(respondents_panel,senators_panel,mcs_panel)
table_2013$year <- 2013

#2014
respondents_panel <- list()
for(i in c(6:9)){
  x <- rollcall2014[,c(4,i)]
  respondents <- subset(x,x$group %in% "CCES Respondent")
  respondents <- data.frame(freq(respondents[,2]))
  respondents$response <- rownames(respondents)
  respondents <- subset(respondents,respondents$response != "Total")
  respondents$group <- "CCES Respondents"
  respondents$vote <- colnames(x)[2]
  respondents$Valid.Percent <- NULL
  respondents_panel[[i]] <- respondents
}
respondents_panel <- ldply(respondents_panel,data.frame)

senators_panel <- list()
for(i in c(6,9)){
  x <- rollcall2014[,c(4,i)]
  respondents <- subset(x,x$group %in% "U.S. Senate")
  respondents <- data.frame(freq(respondents[,2]))
  respondents$response <- rownames(respondents)
  respondents <- subset(respondents,respondents$response != "Total")
  respondents$group <- "U.S. Senate"
  respondents$vote <- colnames(x)[2]
  respondents$Valid.Percent <- NULL
  senators_panel[[i]] <- respondents
}
senators_panel <- ldply(senators_panel,data.frame)

mcs_panel <- list()
for(i in c(6:9)){
  x <- rollcall2014[,c(4,i)]
  respondents <- subset(x,x$group %in% "U.S. House")
  respondents <- data.frame(freq(respondents[,2]))
  respondents$response <- rownames(respondents)
  respondents <- subset(respondents,respondents$response != "Total")
  respondents$group <- "U.S. House"
  respondents$vote <- colnames(x)[2]
  respondents$Valid.Percent <- NULL
  mcs_panel[[i]] <- respondents
}
mcs_panel <- ldply(mcs_panel,data.frame)

table_2014 <- rbind(respondents_panel,senators_panel,mcs_panel)
table_2014$year <- 2014

#2015
respondents_panel <- list()
for(i in c(6:11)){
  x <- rollcall2015[,c(4,i)]
  respondents <- subset(x,x$group %in% "CCES Respondent")
  respondents <- data.frame(freq(respondents[,2]))
  respondents$response <- rownames(respondents)
  respondents <- subset(respondents,respondents$response != "Total")
  respondents$group <- "CCES Respondents"
  respondents$vote <- colnames(x)[2]
  respondents$Valid.Percent <- NULL
  respondents_panel[[i]] <- respondents
}
respondents_panel <- ldply(respondents_panel,data.frame)

senators_panel <- list()
for(i in c(6,9:11)){
  x <- rollcall2015[,c(4,i)]
  respondents <- subset(x,x$group %in% "U.S. Senate")
  respondents <- data.frame(freq(respondents[,2]))
  respondents$response <- rownames(respondents)
  respondents <- subset(respondents,respondents$response != "Total")
  respondents$group <- "U.S. Senate"
  respondents$vote <- colnames(x)[2]
  respondents$Valid.Percent <- NULL
  senators_panel[[i]] <- respondents
}
senators_panel <- ldply(senators_panel,data.frame)

mcs_panel <- list()
for(i in c(6:11)){
  x <- rollcall2015[,c(4,i)]
  respondents <- subset(x,x$group %in% "U.S. House")
  respondents <- data.frame(freq(respondents[,2]))
  respondents$response <- rownames(respondents)
  respondents <- subset(respondents,respondents$response != "Total")
  respondents$group <- "U.S. House"
  respondents$vote <- colnames(x)[2]
  respondents$Valid.Percent <- NULL
  mcs_panel[[i]] <- respondents
}
mcs_panel <- ldply(mcs_panel,data.frame)

table_2015 <- rbind(respondents_panel,senators_panel,mcs_panel)
table_2015$year <- 2015

#2016
respondents_panel <- list()
for(i in c(6:16)){
  x <- rollcall2016[,c(4,i)]
  respondents <- subset(x,x$group %in% "CCES Respondent")
  respondents <- data.frame(freq(respondents[,2]))
  respondents$response <- rownames(respondents)
  respondents <- subset(respondents,respondents$response != "Total")
  respondents$group <- "CCES Respondents"
  respondents$vote <- colnames(x)[2]
  respondents$Valid.Percent <- NULL
  respondents_panel[[i]] <- respondents
}
respondents_panel <- ldply(respondents_panel,data.frame)

senators_panel <- list()
for(i in c(6,8,10)){
  x <- rollcall2016[,c(4,i)]
  respondents <- subset(x,x$group %in% "U.S. Senate")
  respondents <- data.frame(freq(respondents[,2]))
  respondents$response <- rownames(respondents)
  respondents <- subset(respondents,respondents$response != "Total")
  respondents$group <- "U.S. Senate"
  respondents$vote <- colnames(x)[2]
  respondents$Valid.Percent <- NULL
  senators_panel[[i]] <- respondents
}
senators_panel <- ldply(senators_panel,data.frame)

mcs_panel <- list()
for(i in c(6:10)){
  x <- rollcall2016[,c(4,i)]
  respondents <- subset(x,x$group %in% "U.S. House")
  respondents <- data.frame(freq(respondents[,2]))
  respondents$response <- rownames(respondents)
  respondents <- subset(respondents,respondents$response != "Total")
  respondents$group <- "U.S. House"
  respondents$vote <- colnames(x)[2]
  respondents$Valid.Percent <- NULL
  mcs_panel[[i]] <- respondents
}
mcs_panel <- ldply(mcs_panel,data.frame)

table_2016 <- rbind(respondents_panel,senators_panel,mcs_panel)
table_2016$year <- 2016

table2_cces_pooled_roll_call_percents <- rbind(table_2008,table_2009,table_2010,table_2011,table_2012,table_2013,table_2014,table_2015,table_2016)

table2_cces_pooled_roll_call_percents <- reshape(data = table2_cces_pooled_roll_call_percents, idvar= c("year","group","response"),v.names= c("Frequency","Percent"), timevar= c("vote"), direction = "wide")

rm(table_2008,table_2009,table_2010,table_2011,table_2012,table_2013,table_2014,table_2015,table_2016,mcs_panel,respondents,respondents_panel,rollcall2008,rollcall2009,rollcall2010,rollcall2011,rollcall2012,rollcall2013,rollcall2014,rollcall2015,rollcall2016,senators_panel,x,i,rollcall2010_panel)

###### Table 3: CCES Pooled Models of Congressional Approval ###### 

load("Replication_CCES_ANES_Files.Rdata")

options(scipen=999)

summary(model <- glm(cong_approval_binary ~ pid3*congress_type + dem_proximity_rule_aldmck_cong_pty_placement*congress_type + comprehenesive_knowledge_scale_cut + pres_approval_num + retro_econ_eval_num + cong_delegation_approval + aldmck_proximity_betwen_mc_citizen, data=cces, weights=weight, family = binomial(link = "logit")))

y <- coeftest(model, vcov = cluster.vcov(model, cluster=cces$year_district_id)) # Figure 4A & Figure 5A models
model_df <- data.frame(N =nobs(model), psuedoR2=round(PseudoR2(model),2),AIC=stats::AIC(model)[1], var=rownames(y),coef=round(y[,1],2),se=round(y[,2],2),z=round(y[,3],2),p=round(y[,4],4))
rownames(model_df) <- NULL

table3_perceptual_based_model <- model_df

summary(model <- glm(cong_approval_binary ~ pid3*congress_type + dem_proximity_rule_joint_scaling*congress_type + comprehenesive_knowledge_scale_cut + pres_approval_num + retro_econ_eval_num + cong_delegation_approval + jointscaling_proximity_between_mc_citizen, data=cces, weights=weight, family = binomial(link = "logit"))) # Figure 4B & 5B Models

y <- coeftest(model, vcov = cluster.vcov(model, cluster=cces$year_district_id)) # Figure 5A models

model_df <- data.frame(N =nobs(model), psuedoR2=round(PseudoR2(model),2),AIC=stats::AIC(model)[1], var=rownames(y),coef=round(y[,1],2),se=round(y[,2],2),z=round(y[,3],2),p=round(y[,4],4))
rownames(model_df) <- NULL

table3_roll_call_based_model <- model_df

table3_perceptual_based_model$model <- "Perceptual-Based Model"
table3_roll_call_based_model$model <- "Roll-Call Based Model"

table3_cces_pooled_models <- rbind(table3_perceptual_based_model,table3_roll_call_based_model)
rm(table3_perceptual_based_model,table3_roll_call_based_model,model,model_df,y)

###### Table 3A: CCES Panel Models of Congressional Approval ###### 

summary(model <- glm(cong_approval_binary_10 ~ dem_proximity_rule_joint_scaling + approval_mcs_senators_scale_10 + knowledge_scale_10_cut + obama_approval_binary_10 + pid3_10 + retro_econ_eval_10 + jointscaling_proximity_between_mc_citizen, data=cces_panel, weights=weight, family = binomial(link = "logit"))) # Figure 6 A & B 2010 Models

y <- coeftest(model, vcov = vcovBS(model,R=1000)) 

model_df <- data.frame(N =nobs(model), psuedoR2=round(PseudoR2(model),2),AIC=stats::AIC(model)[1], var=rownames(y),coef=round(y[,1],2),se=round(y[,2],2),z=round(y[,3],2),p=round(y[,4],4))
rownames(model_df) <- NULL

table3A_panel_model_2010 <- model_df

summary(model <- glm(cong_approval_binary_12 ~ dem_proximity_rule_joint_scaling + cong_approval_binary_10 + approval_mcs_senators_scale_12 + knowledge_scale_12_cut + obama_approval_binary_12 + pid3_10 + retro_econ_eval_12 + jointscaling_proximity_between_mc_citizen, data=cces_panel, weights=weight, family = binomial(link = "logit"))) # Figure 6 A & B 2012 Models

y <- coeftest(model, vcov = vcovBS(model,R=1000)) 

model_df <- data.frame(N =nobs(model), psuedoR2=round(PseudoR2(model),2),AIC=stats::AIC(model)[1], var=rownames(y),coef=round(y[,1],2),se=round(y[,2],2),z=round(y[,3],2),p=round(y[,4],4))
rownames(model_df) <- NULL

table3A_panel_model_2012 <- model_df

summary(model <- glm(cong_approval_binary_14 ~ dem_proximity_rule_joint_scaling + cong_approval_binary_12 + approval_mcs_senators_scale_14 + knowledge_scale_14_cut + obama_approval_binary_14 + pid3_10 + retro_econ_eval_14 + jointscaling_proximity_between_mc_citizen, data=cces_panel, weights=weight, family = binomial(link = "logit"))) # Figure 6 A & B 2014 Models

y <- coeftest(model, vcov = vcovBS(model,R=1000)) 

model_df <- data.frame(N =nobs(model), psuedoR2=round(PseudoR2(model),2), AIC=stats::AIC(model)[1], var=rownames(y),coef=round(y[,1],2),se=round(y[,2],2),z=round(y[,3],2),p=round(y[,4],4))
rownames(model_df) <- NULL

table3A_panel_model_2014 <- model_df

table3A_panel_model_2010$model <- "2010 Survey Panel Wave"
table3A_panel_model_2012$model <- "2012 Survey Panel Wave"
table3A_panel_model_2014$model <- "2014 Survey Panel Wave"

table3A_panel_model <- rbind(table3A_panel_model_2010,table3A_panel_model_2012,table3A_panel_model_2014)

rm(table3A_panel_model_2010,table3A_panel_model_2012,table3A_panel_model_2014,model,model_df,y)

###### Table 4A/B/C/D/E/F Isolating Ideological Effects ###### 

table4F_cces_panel_independent_model <- list()
for(i in c("I")){
  x <- subset(cces_panel,cces_panel$pid3_10 == i)
  
  summary(model <- glm(cong_approval_binary_10 ~ dem_proximity_rule_joint_scaling + approval_mcs_senators_scale_10 + knowledge_scale_10_cut + retro_econ_eval_10 + jointscaling_proximity_between_mc_citizen, data=x, weights=weight, family = binomial(link = "logit")))

  y <- coeftest(model, vcov = vcovBS(model,R=1000)) 
  
  model_df1 <- data.frame(N =nobs(model), psuedoR2=round(PseudoR2(model),2),AIC=stats::AIC(model)[1], var=rownames(y),coef=round(y[,1],2),se=round(y[,2],2),z=round(y[,3],2),p=round(y[,4],4))
  rownames(model_df1) <- NULL
  model_df1$model <- "2010 Survey Panel Wave"
  
  summary(model <- glm(cong_approval_binary_12 ~ dem_proximity_rule_joint_scaling + cong_approval_binary_10 + approval_mcs_senators_scale_12 + knowledge_scale_12_cut + retro_econ_eval_12 + jointscaling_proximity_between_mc_citizen, data=x, weights=weight, family = binomial(link = "logit")))
  
  y <- coeftest(model, vcov = vcovBS(model,R=1000)) 
  
  model_df2 <- data.frame(N =nobs(model), psuedoR2=round(PseudoR2(model),2), AIC=stats::AIC(model)[1], var=rownames(y),coef=round(y[,1],2),se=round(y[,2],2),z=round(y[,3],2),p=round(y[,4],4))
  rownames(model_df2) <- NULL
  model_df2$model <- "2012 Survey Panel Wave"
  
  summary(model <- glm(cong_approval_binary_14 ~ dem_proximity_rule_joint_scaling + cong_approval_binary_12 + approval_mcs_senators_scale_14 + knowledge_scale_14_cut + retro_econ_eval_14 + jointscaling_proximity_between_mc_citizen, data=x, weights=weight, family = binomial(link = "logit")))
 
  y <- coeftest(model, vcov = vcovBS(model,R=1000)) 
  
  model_df3 <- data.frame(N =nobs(model), psuedoR2=round(PseudoR2(model),2), AIC=stats::AIC(model)[1], var=rownames(y),coef=round(y[,1],2),se=round(y[,2],2),z=round(y[,3],2),p=round(y[,4],4))
  rownames(model_df3) <- NULL
  model_df3$model <- "2014 Survey Panel Wave"
  
  models <- rbind(model_df1,model_df2,model_df3)
  
  table4F_cces_panel_independent_model[[i]] <- models
}
table4F_cces_panel_independent_model <- ldply(table4F_cces_panel_independent_model,data.frame)

partisan_models <- list()

for(i in c("R","D")){
  x <- subset(cces_panel,cces_panel$pid3_10 == i)
  
  summary(model <- glm(cong_approval_binary_10 ~ dem_proximity_rule_joint_scaling + approval_mcs_senators_scale_10 + knowledge_scale_10_cut + retro_econ_eval_10 + jointscaling_proximity_between_mc_citizen, data=x, weights=weight, family = binomial(link = "logit")))
  
  y <- coeftest(model, vcov = vcovBS(model,R=1000)) 
  
  model_df1 <- data.frame(N =nobs(model), psuedoR2=round(PseudoR2(model),2), AIC=stats::AIC(model)[1], var=rownames(y),coef=round(y[,1],2),se=round(y[,2],2),z=round(y[,3],2),p=round(y[,4],4))
  rownames(model_df1) <- NULL
  model_df1$model <- "2010 Survey Panel Wave"
  
  summary(model <- glm(cong_approval_binary_12 ~ dem_proximity_rule_joint_scaling + cong_approval_binary_10 + approval_mcs_senators_scale_12 + knowledge_scale_12_cut + retro_econ_eval_12 + jointscaling_proximity_between_mc_citizen, data=x, weights=weight, family = binomial(link = "logit")))
  
  y <- coeftest(model, vcov = vcovBS(model,R=1000)) 
  
  model_df2 <- data.frame(N =nobs(model), psuedoR2=round(PseudoR2(model),2), AIC=stats::AIC(model)[1], var=rownames(y),coef=round(y[,1],2),se=round(y[,2],2),z=round(y[,3],2),p=round(y[,4],4))
  rownames(model_df2) <- NULL
  model_df2$model <- "2012 Survey Panel Wave"
  
  summary(model <- glm(cong_approval_binary_14 ~ dem_proximity_rule_joint_scaling + cong_approval_binary_12 + approval_mcs_senators_scale_14 + knowledge_scale_14_cut + retro_econ_eval_14 + jointscaling_proximity_between_mc_citizen, data=x, weights=weight, family = binomial(link = "logit")))
  
  y <- coeftest(model, vcov = vcovBS(model,R=1000)) 
  
  model_df3 <- data.frame(N =nobs(model), psuedoR2=round(PseudoR2(model),2), AIC=stats::AIC(model)[1], var=rownames(y),coef=round(y[,1],2),se=round(y[,2],2),z=round(y[,3],2),p=round(y[,4],4))
  rownames(model_df3) <- NULL
  model_df3$model <- "2014 Survey Panel Wave"
  
  models <- rbind(model_df1,model_df2,model_df3)
  
  partisan_models[[i]] <- models
}
partisan_models <- ldply(partisan_models,data.frame)

table4E_cces_panel_republican_model <- subset(partisan_models,partisan_models$.id %in% "R")
table4D_cces_panel_democratic_model <- subset(partisan_models,partisan_models$.id %in% "D")

partisan_models <- list()
for(i in c("Republican","Democrat")){
  x <- subset(cces,cces$pid3 == i)
  
  summary(model <- glm(cong_approval_binary ~ dem_proximity_rule_aldmck_cong_pty_placement*congress_type + comprehenesive_knowledge_scale_cut + pres_approval_num + retro_econ_eval_num + cong_delegation_approval + aldmck_proximity_betwen_mc_citizen , data=x, weights=weight, family = binomial(link = "logit")))
  
  y <- coeftest(model, vcov = cluster.vcov(model, cluster=x$year_district_id)) 
  
  model_df <- data.frame(N =nobs(model), psuedoR2=round(PseudoR2(model),2), AIC=stats::AIC(model)[1], var=rownames(y),coef=round(y[,1],2),se=round(y[,2],2),z=round(y[,3],2),p=round(y[,4],4))
  rownames(model_df) <- NULL
  model_df$model <- "Perceptual-Based Model"

  partisan_models[[i]] <- model_df
}

for(i in c("Independent")){
  x <- subset(cces,cces$pid3 == i)
  
  summary(model <- glm(cong_approval_binary ~ dem_proximity_rule_aldmck_cong_pty_placement*congress_type + comprehenesive_knowledge_scale_cut + pres_approval_num + retro_econ_eval_num + cong_delegation_approval + aldmck_proximity_betwen_mc_citizen, data=x, weights=weight, family = binomial(link = "logit")))
  y <- coeftest(model, vcov = cluster.vcov(model, cluster=x$year_district_id)) 
  
  model_df <- data.frame(N =nobs(model), psuedoR2=round(PseudoR2(model),2), AIC=stats::AIC(model)[1], var=rownames(y),coef=round(y[,1],2),se=round(y[,2],2),z=round(y[,3],2),p=round(y[,4],4))
  rownames(model_df) <- NULL
  model_df$model <- "Perceptual-Based Model"
  
  partisan_models[[i]] <- model_df
}
partisan_models <- ldply(partisan_models,data.frame)

partisan_models2 <- list()
for(i in c("Republican","Democrat")){
  x <- subset(cces,cces$pid3 == i)
  
  summary(model <- glm(cong_approval_binary ~ dem_proximity_rule_joint_scaling*congress_type + comprehenesive_knowledge_scale_cut + pres_approval_num + retro_econ_eval_num + cong_delegation_approval + jointscaling_proximity_between_mc_citizen , data=x, weights=weight, family = binomial(link = "logit")))
 
  y <- coeftest(model, vcov = cluster.vcov(model, cluster=x$year_district_id)) 
  
  model_df <- data.frame(N =nobs(model), psuedoR2=round(PseudoR2(model),2), AIC=stats::AIC(model)[1], var=rownames(y),coef=round(y[,1],2),se=round(y[,2],2),z=round(y[,3],2),p=round(y[,4],4))
  rownames(model_df) <- NULL
  model_df$model <- "Roll-Call Based Model"
  
  partisan_models2[[i]] <- model_df
}

for(i in c("Independent")){
  x <- subset(cces,cces$pid3 == i)
  
  summary(model <- glm(cong_approval_binary ~ dem_proximity_rule_joint_scaling*congress_type + comprehenesive_knowledge_scale_cut + pres_approval_num + retro_econ_eval_num + cong_delegation_approval + jointscaling_proximity_between_mc_citizen, data=x, weights=weight, family = binomial(link = "logit")))
  
  y <- coeftest(model, vcov = cluster.vcov(model, cluster=x$year_district_id)) 
  
  model_df <- data.frame(N =nobs(model), psuedoR2=round(PseudoR2(model),2), AIC=stats::AIC(model)[1], var=rownames(y),coef=round(y[,1],2),se=round(y[,2],2),z=round(y[,3],2),p=round(y[,4],4))
  rownames(model_df2) <- NULL
  model_df$model <- "Roll-Call Based Model"
  
  partisan_models2[[i]] <- model_df
}
partisan_models2 <- ldply(partisan_models2,data.frame)

partisan_models <- rbind(partisan_models,partisan_models2)

table4A_cces_pooled_democratic_model <- subset(partisan_models,partisan_models$.id %in% "Democrat")
table4B_cces_pooled_republican_model <- subset(partisan_models,partisan_models$.id %in% "Republican")
table4C_cces_pooled_republican_model <- subset(partisan_models,partisan_models$.id %in% "Independent")

rm(model,model_df,model_df1,model_df2,model_df3,models,partisan_models,partisan_models2,x,i,y)

###### Appendix Tables 1/2/3: Yearly CCES Models & ANES Robustness Checks Figures ###### 

table2APPENDIX_cces_pooled_roll_call_models_yearly <- list()
for(i in seq(2008,2016,1)){
  y <- subset(cces,cces$year %in% i)
summary(model <- glm(cong_approval_binary ~ pid3 + dem_proximity_rule_joint_scaling + comprehenesive_knowledge_scale_cut + pres_approval_num + retro_econ_eval_num + cong_delegation_approval + jointscaling_proximity_between_mc_citizen, data=y, weights=weight, family = binomial(link = "logit")))

y <- coeftest(model, vcov = cluster.vcov(model, cluster=y$year_district_id))

model_df <- data.frame(N =nobs(model), psuedoR2=round(PseudoR2(model),2), AIC=stats::AIC(model)[1], var=rownames(y),coef=round(y[,1],2),se=round(y[,2],2),z=round(y[,3],2),p=round(y[,4],4))
rownames(model_df) <- NULL
model_df$model <- "Roll-Call Based Model"
model_df$survey_year <- i

table2APPENDIX_cces_pooled_roll_call_models_yearly[[i]] <- model_df
}

table2APPENDIX_cces_pooled_roll_call_models_yearly <- ldply(table2APPENDIX_cces_pooled_roll_call_models_yearly,data.frame)

table1APPENDIX_cces_pooled_perceptual_models_yearly <- list()
for(i in seq(2008,2016,1)){
  y <- subset(cces,cces$year %in% i)
  summary(model <- glm(cong_approval_binary ~ pid3 + dem_proximity_rule_aldmck_cong_pty_placement + comprehenesive_knowledge_scale_cut + pres_approval_num + retro_econ_eval_num + cong_delegation_approval + aldmck_proximity_betwen_mc_citizen, data=y, weights=weight, family = binomial(link = "logit")))
  
  y <- coeftest(model, vcov = cluster.vcov(model, cluster=y$year_district_id))
  
  model_df <- data.frame(N =nobs(model), psuedoR2=round(PseudoR2(model),2), AIC=stats::AIC(model)[1], var=rownames(y),coef=round(y[,1],2),se=round(y[,2],2),z=round(y[,3],2),p=round(y[,4],4))
  rownames(model_df) <- NULL
  model_df$model <- "Perceptual-Based Model"
  model_df$survey_year <- i
  
  table1APPENDIX_cces_pooled_perceptual_models_yearly[[i]] <- model_df
}

table1APPENDIX_cces_pooled_perceptual_models_yearly <- ldply(table1APPENDIX_cces_pooled_perceptual_models_yearly,data.frame)

summary(model <- glm(cong_approval_dich ~ pid3*congress_type + dem_pty_proximity_bam*congress_type + dem_pres_approve_clean + full_knowledge_scale + inc_approval_clean_factor_dich + retro_econ_eval_clean_scale, data=nes, weights=weight, family = binomial(link = "logit")))

y <- coeftest(model, vcov = cluster.vcov(model, cluster=nes$year))

model_df <- data.frame(N =nobs(model), psuedoR2=round(PseudoR2(model),2), AIC=stats::AIC(model)[1], var=rownames(y),coef=round(y[,1],2),se=round(y[,2],2),z=round(y[,3],2),p=round(y[,4],4))
rownames(model_df) <- NULL
model_df$model <- "Perceptual-Based Model"

table3APPENDIX_anes_perceptual_models <- model_df

rm(model,model_df,y,nes,cces,cces_panel,y)