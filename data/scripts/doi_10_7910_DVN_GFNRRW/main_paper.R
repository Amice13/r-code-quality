####Parts:
## 1 Preamble (Line 27)
## 2 Data Processing (Line 196)
##   2.1 Households Connections (Line 198)
##   2.2 Baseline Survey Welfare Indicators (Line 228)
##   2.3 Estimate alternate welfare measures for robustness (Line 475)
##   2.4 PMT Scores (Line 681)
##   2.5 Follow-up Welfare indicators (Line 819)
##   2.6 Shocks/Covid Benefits (Line 1083)
##   2.7 Additional Variables (Line 1118)
## 3 Analysis: Correlations Estimation/Matching Analysis (Line 1252)
##   3.1 Baseline Survey/Participant Correlations of Same Metric (Line 1253)
##   3.2 Correlations within survey metrics and within participant metrics (Line 1322)
##   3.3 Alternate Metrics Correlations (Line 1405)
##   3.4 Shock and Benefit Report Matching (Line 1505)
##   3.5 PMT/Community Aggregation Correlations (Line 1523)
##   3.6 Correlations between targeting and info ranks (Line 1832)
##   3.7 Correlations Between Preference/Second order belief ranks and Info Ranks (Line 1893)
##   3.8 Percentage of the 2, 3, and 4 poorest identified (Line 1973)
##   3.9 Correlations Between Rounds by metric (Line 2008)
##   3.10 Follow-up Survey/Participant Correlations of Same Metric (Line 2066)
## 4 Analysis: Regression Estimation (Line 2066)
## 5 Main Tables (Line 2220)
## 6 Main Figures (Line 2538)
## 7 Appendix Tables (Line 2648)

#1.PREAMBLE#############################################################
#Clear Environment
rm(list=ls())

#Load packages (install first if not already installed)
library(tidyverse)
library(plotrix)
library(lfe)
library(stargazer)
library(psych)
library(ClusterBootstrap)
library(DescTools)
library(FactoMineR)
library(factoextra)
library(reticulate)
library(ggplot2)
library(ggpubr)
library(cocor)
library(car)
library(simpleboot)
library(boot)

#Set working directory and read in data
setwd("YourDirectory\\replication_package_final")

survey<-read.csv("Raw Data\\survey.csv")
known<-read.csv("Raw Data\\participant_connections.csv")
ranks<-read.csv("Raw Data\\participant_rankings.csv")
shocks<-read.csv("Raw Data\\participant_shock_reports.csv")
covid<-read.csv("Raw Data\\participant_covid_reports.csv")
franks<-read.csv("Raw Data\\participant_rankings_followup.csv")
follow_up<-read.csv("Raw Data\\survey_followup.csv")
comrank<-read.csv("Raw Data\\community_meeting.csv")

#Define Functions 
#Mean with removing missing
mean.fun <- function(dat, idx) mean(dat[idx], na.rm = TRUE)

#This will be used later to cluster bootstrap standard errors
clbootse<- function(var1, df1){
  
  test1<-aggregate(df1[, c(var1)], by=list(df1$enum_area), FUN=mean.fun)
  colnames(test1)<-c("enum_area", "dm")
  
  w1<-c(nrow(split(df1,df1$enum_area)$`1`[complete.cases(split(df1,df1$enum_area)$`1`),]),nrow(split(df1,df1$enum_area)$`2`[complete.cases(split(df1,df1$enum_area)$`2`),]),nrow(split(df1,df1$enum_area)$`3`[complete.cases(split(df1,df1$enum_area)$`3`),]),nrow(split(df1,df1$enum_area)$`4`[complete.cases(split(df1,df1$enum_area)$`4`),]),nrow(split(df1,df1$enum_area)$`5`[complete.cases(split(df1,df1$enum_area)$`5`),]),nrow(split(df1,df1$enum_area)$`6`[complete.cases(split(df1,df1$enum_area)$`6`),]),nrow(split(df1,df1$enum_area)$`7`[complete.cases(split(df1,df1$enum_area)$`7`),]),nrow(split(df1,df1$enum_area)$`8`[complete.cases(split(df1,df1$enum_area)$`8`),]),nrow(split(df1,df1$enum_area)$`9`[complete.cases(split(df1,df1$enum_area)$`9`),]),nrow(split(df1,df1$enum_area)$`10`[complete.cases(split(df1,df1$enum_area)$`10`),]))
  
  sim1<-data.frame(do.call("rbind", replicate(5000, test1$dm, simplify = FALSE)))
  sim1$wavg<-NA
  for (i in 1:nrow(sim1)){
    set.seed(i)
    rows<-as.vector(sample(c(1:10), 10, replace = TRUE))
    sim1[i,]$wavg<-(sim1[i, rows[1]]*w1[rows[1]]+sim1[i, rows[2]]*w1[rows[2]]+sim1[i, rows[3]]*w1[rows[3]]+sim1[i, rows[4]]*w1[rows[4]]+sim1[i, rows[5]]*w1[rows[5]]+sim1[i, rows[6]]*w1[rows[6]]+sim1[i, rows[7]]*w1[rows[7]]+sim1[i, rows[8]]*w1[rows[8]]+sim1[i, rows[9]]*w1[rows[9]]+sim1[i, rows[10]]*w1[rows[10]])/sum(w1[rows[1]]+w1[rows[2]]+w1[rows[3]]+w1[rows[4]]+w1[rows[5]]+w1[rows[6]]+w1[rows[7]]+w1[rows[8]]+w1[rows[9]]+w1[rows[10]])
  }
  
  sd<-sd(sim1$wavg)
  sd
  
}

###Return p value of t-test of difference of means using bootstrapped SEs
bootstrap_simp <- function(var1, var2, df1, df2){
  set.seed(151052)
  a<-mean(df1[, c(var1)], na.rm=T)
  b<-mean(df2[, c(var2)], na.rm=T)
  t0<-a-b
  c<-(a*nrow(df1[complete.cases(df1),])+b*nrow(df2[complete.cases(df2),]))/(nrow(df1[complete.cases(df1),])+nrow(df2[complete.cases(df2),]))
  df1$dm<-df1[, c(var1)]-a+c
  df2$dm<-df2[, c(var2)]-b+c
  
  test1<-aggregate(df1$dm, by=list(df1$enum_area), FUN=mean.fun)
  colnames(test1)<-c("enum_area", "dm")
  
  test2<-aggregate(df2$dm, by=list(df2$enum_area), FUN=mean.fun)
  colnames(test2)<-c("enum_area", "dm")
  
  
  w1<-c(nrow(split(df1,df1$enum_area)$`1`[complete.cases(split(df1,df1$enum_area)$`1`),]),nrow(split(df1,df1$enum_area)$`2`[complete.cases(split(df1,df1$enum_area)$`2`),]),nrow(split(df1,df1$enum_area)$`3`[complete.cases(split(df1,df1$enum_area)$`3`),]),nrow(split(df1,df1$enum_area)$`4`[complete.cases(split(df1,df1$enum_area)$`4`),]),nrow(split(df1,df1$enum_area)$`5`[complete.cases(split(df1,df1$enum_area)$`5`),]),nrow(split(df1,df1$enum_area)$`6`[complete.cases(split(df1,df1$enum_area)$`6`),]),nrow(split(df1,df1$enum_area)$`7`[complete.cases(split(df1,df1$enum_area)$`7`),]),nrow(split(df1,df1$enum_area)$`8`[complete.cases(split(df1,df1$enum_area)$`8`),]),nrow(split(df1,df1$enum_area)$`9`[complete.cases(split(df1,df1$enum_area)$`9`),]),nrow(split(df1,df1$enum_area)$`10`[complete.cases(split(df1,df1$enum_area)$`10`),]))
  
  sim1<-data.frame(do.call("rbind", replicate(5000, test1$dm, simplify = FALSE)))
  sim1$wavg<-NA
  for (i in 1:nrow(sim1)){
    set.seed(i)
    rows<-as.vector(sample(c(1:10), 10, replace = TRUE))
    sim1[i,]$wavg<-(sim1[i, rows[1]]*w1[rows[1]]+sim1[i, rows[2]]*w1[rows[2]]+sim1[i, rows[3]]*w1[rows[3]]+sim1[i, rows[4]]*w1[rows[4]]+sim1[i, rows[5]]*w1[rows[5]]+sim1[i, rows[6]]*w1[rows[6]]+sim1[i, rows[7]]*w1[rows[7]]+sim1[i, rows[8]]*w1[rows[8]]+sim1[i, rows[9]]*w1[rows[9]]+sim1[i, rows[10]]*w1[rows[10]])/sum(w1[rows[1]]+w1[rows[2]]+w1[rows[3]]+w1[rows[4]]+w1[rows[5]]+w1[rows[6]]+w1[rows[7]]+w1[rows[8]]+w1[rows[9]]++w1[rows[10]])
  }
  
  w2<-c(nrow(split(df2,df2$enum_area)$`1`[complete.cases(split(df2,df2$enum_area)$`1`),]),nrow(split(df2,df2$enum_area)$`2`[complete.cases(split(df2,df2$enum_area)$`2`),]),nrow(split(df2,df2$enum_area)$`3`[complete.cases(split(df2,df2$enum_area)$`3`),]),nrow(split(df2,df2$enum_area)$`4`[complete.cases(split(df2,df2$enum_area)$`4`),]),nrow(split(df2,df2$enum_area)$`5`[complete.cases(split(df2,df2$enum_area)$`5`),]),nrow(split(df2,df2$enum_area)$`6`[complete.cases(split(df2,df2$enum_area)$`6`),]),nrow(split(df2,df2$enum_area)$`7`[complete.cases(split(df2,df2$enum_area)$`7`),]),nrow(split(df2,df2$enum_area)$`8`[complete.cases(split(df2,df2$enum_area)$`8`),]),nrow(split(df2,df2$enum_area)$`9`[complete.cases(split(df2,df2$enum_area)$`9`),]),nrow(split(df2,df2$enum_area)$`10`[complete.cases(split(df2,df2$enum_area)$`10`),]))
  
  sim2<-data.frame(do.call("rbind", replicate(5000, test1$dm, simplify = FALSE)))
  sim2$wavg<-NA  
  for (i in 1:nrow(sim2)){
    set.seed(24000+i)
    rows<-as.vector(sample(c(1:10), 10, replace = TRUE))
    sim2[i,]$wavg<-(sim2[i, rows[1]]*w2[rows[1]]+sim2[i, rows[2]]*w2[rows[2]]+sim2[i, rows[3]]*w2[rows[3]]+sim2[i, rows[4]]*w2[rows[4]]+sim2[i, rows[5]]*w2[rows[5]]+sim2[i, rows[6]]*w2[rows[6]]+sim2[i, rows[7]]*w2[rows[7]]+sim2[i, rows[8]]*w2[rows[8]]+sim2[i, rows[9]]*w2[rows[9]]+sim2[i, rows[10]]*w2[rows[10]])/sum(w2[rows[1]]+w2[rows[2]]+w2[rows[3]]+w2[rows[4]]+w2[rows[5]]+w2[rows[6]]+w2[rows[7]]+w2[rows[8]]+w2[rows[9]]++w2[rows[10]])
  }
  
  vec<-data.frame(avg1=sim1$wavg, avg2=sim2$wavg)
  vec$bootdif<-vec$avg1-vec$avg2
  p<-nrow(vec[which(abs(vec$bootdif)>abs(t0)),])/nrow(vec[complete.cases(vec),])
  p
  
}

#Bootstrapped standard errors on follow-up data
clbootse_2<- function(var1, df1){
  
  test1<-aggregate(df1[, c(var1)], by=list(df1$enum_area), FUN=mean.fun)
  colnames(test1)<-c("enum_area", "dm")
  
  w1<-c(nrow(split(df1,df1$enum_area)$`1`[complete.cases(split(df1,df1$enum_area)$`1`),]),nrow(split(df1,df1$enum_area)$`2`[complete.cases(split(df1,df1$enum_area)$`2`),]),nrow(split(df1,df1$enum_area)$`7`[complete.cases(split(df1,df1$enum_area)$`7`),]))
  
  sim1<-data.frame(do.call("rbind", replicate(5000, test1$dm, simplify = FALSE)))
  sim1$wavg<-NA
  for (i in 1:nrow(sim1)){
    set.seed(i)
    rows<-as.vector(sample(c(1:3), 3, replace = TRUE))
    sim1[i,]$wavg<-(sim1[i, rows[1]]*w1[rows[1]]+sim1[i, rows[2]]*w1[rows[2]]+sim1[i, rows[3]]*w1[rows[3]])/sum(w1[rows[1]]+w1[rows[2]]+w1[rows[3]])
  }
  
  sd<-sd(sim1$wavg)
  sd
  
}

##Bootstrapped t-test on the follow up data
bootstrap_simp_2 <- function(var1, var2, df1, df2){
  set.seed(151052)
  a<-mean(df1[, c(var1)], na.rm=T)
  b<-mean(df2[, c(var2)], na.rm=T)
  t0<-a-b
  c<-(a*nrow(df1[complete.cases(df1),])+b*nrow(df2[complete.cases(df2),]))/(nrow(df1[complete.cases(df1),])+nrow(df2[complete.cases(df2),]))
  df1$dm<-df1[, c(var1)]-a+c
  df2$dm<-df2[, c(var2)]-b+c
  
  test1<-aggregate(df1$dm, by=list(df1$enum_area), FUN=mean.fun)
  colnames(test1)<-c("enum_area", "dm")
  
  test2<-aggregate(df2$dm, by=list(df2$enum_area), FUN=mean.fun)
  colnames(test2)<-c("enum_area", "dm")
  
  
  w1<-c(nrow(split(df1,df1$enum_area)$`1`[complete.cases(split(df1,df1$enum_area)$`1`),]),nrow(split(df1,df1$enum_area)$`2`[complete.cases(split(df1,df1$enum_area)$`2`),]),nrow(split(df1,df1$enum_area)$`7`[complete.cases(split(df1,df1$enum_area)$`7`),]))
  
  sim1<-data.frame(do.call("rbind", replicate(5000, test1$dm, simplify = FALSE)))
  sim1$wavg<-NA
  for (i in 1:nrow(sim1)){
    set.seed(i)
    rows<-as.vector(sample(c(1:3), 3, replace = TRUE))
    sim1[i,]$wavg<-(sim1[i, rows[1]]*w1[rows[1]]+sim1[i, rows[2]]*w1[rows[2]]+sim1[i, rows[3]]*w1[rows[3]])/sum(w1[rows[1]]+w1[rows[2]]+w1[rows[3]])
  }
  
  w2<-c(nrow(split(df2,df2$enum_area)$`1`[complete.cases(split(df2,df2$enum_area)$`1`),]),nrow(split(df2,df2$enum_area)$`2`[complete.cases(split(df2,df2$enum_area)$`2`),]),nrow(split(df2,df2$enum_area)$`7`[complete.cases(split(df2,df2$enum_area)$`7`),]))
  
  sim2<-data.frame(do.call("rbind", replicate(5000, test1$dm, simplify = FALSE)))
  sim2$wavg<-NA  
  for (i in 1:nrow(sim2)){
    set.seed(24000+i)
    rows<-as.vector(sample(c(1:3), 3, replace = TRUE))
    sim2[i,]$wavg<-(sim2[i, rows[1]]*w2[rows[1]]+sim2[i, rows[2]]*w2[rows[2]]+sim2[i, rows[3]]*w2[rows[3]])/sum(w2[rows[1]]+w2[rows[2]]+w2[rows[3]])
  }
  
  vec<-data.frame(avg1=sim1$wavg, avg2=sim2$wavg)
  vec$bootdif<-vec$avg1-vec$avg2
  p<-nrow(vec[which(abs(vec$bootdif)>abs(t0)),])/nrow(vec[complete.cases(vec),])
  p
  
}


#2.Data Processing#########################################################

######2.1 Households' Connections

#Create variables for households "known well" and "close family/friends"
known$well<-ifelse(known$know>3, 1, 0)
known$ff<-ifelse(known$know==5, 1, 0)

#Number of individuals known per participant
total_known<-known%>%
  group_by(hhid)%>%
  summarize(n_know = sum(well, na.rm = T))

#Merge participant connection info with ranks
ranks<-merge(ranks, known[, c("hhid", "rankid", "well", "ff")])
ranks<-merge(ranks, total_known)

#Number of individuals who know participant
known_by<-known%>%
  group_by(rankid)%>%
  summarize(times_ranked = n(), n_by=sum(well, na.rm = T))
colnames(known_by)[1]<-"hhid"

#get measures of how known the ranker and the ranked both are
known_by$fam_percent<-known_by$n_by/known_by$times_ranked
known_by$fam_percent_r<-known_by$fam_percent
known_by$times_ranked_r<-known_by$times_ranked

ranks<-merge(ranks, known_by[, c("hhid", "fam_percent", "times_ranked")], all.x=T)
ranks<-merge(ranks, known_by[, c("hhid", "fam_percent_r", "times_ranked_r")], by.x="rankid", by.y = "hhid", all.x=T)
ranks<-merge(ranks, total_known, all.x=T)

######2.2 Baseline Survey Welfare indicators

####Expenditures

#Cleaning
survey[survey$hhid==1025,]$exp92<-1340000

#Generate expenditure aggregates
exp_columns <- survey[,grep("exp", names(survey))]
survey$total_food_exp<-rowSums(dplyr::select_if(exp_columns, is.numeric), na.rm=T)
survey$pc_food_exp<-survey$total_food_exp/survey$numhh    
rm(exp_columns)

df_exp<-survey[, c("hhid", "pc_food_exp", "total_food_exp")]
colnames(df_exp)[1]<-"rankid"

#Merge with participant ranking data
ranks<-merge(ranks, df_exp, all.x=T)

#Convert to USD (for later summary)
ranks$pc_food_exp_dollar<-ranks$pc_food_exp/4700


#Create Survey Rankings
ranks$exp_rank_t<-NA
for (i in unique(ranks$hhid)){
  ranks[ranks$hhid==i,]$exp_rank_t<-rank(ranks[ranks$hhid==i,]$pc_food_exp, na.last = "keep")
}

ranks$exp_rank<-as.numeric(ranks$exp_rank)
ranks$exp_rank_t<-as.numeric(ranks$exp_rank_t)

####MUE 

#Create consumption categories data frame
y<-cbind(survey[, c("hhid", "enum_area")], rep(1, nrow(survey)), survey[,grep("exp", names(survey))])
y<-dplyr::select_if(y, is.numeric)
colnames(y)[c(1:3)]<-c("j","m","t")
y<-y[, c(1, 3, 2, 4:185)]
y$pc_food_exp<-NULL
y$total_food_exp<-NULL
y<-y[, which(!apply(y == 0, 2, all))]
y$rice<-y$exp1+y$exp2
y$starch<-y$exp3+y$exp7+y$exp8+y$exp9+y$exp11+y$exp71+y$exp4+y$exp5+y$exp10
y$fish<-y$exp12+y$exp14+y$exp16+y$exp17+y$exp18+y$exp19+y$exp20+y$exp22+y$exp23+y$exp24+y$exp26+y$exp29+y$exp30
y$meat<-y$exp33+y$exp35+y$exp42+y$exp43+y$exp44+y$exp45+y$exp46+y$exp47+y$exp36+y$exp48+y$exp49+y$exp50+y$exp52
y$veg<-y$exp65+y$exp66+y$exp67+y$exp69+y$exp70+y$exp72+y$exp75+y$exp82+y$exp83+y$exp89+y$exp78+y$exp79+y$exp60+y$exp60x+y$exp61+y$exp62+y$exp84+y$exp85+y$exp86+y$exp63+y$exp64+y$exp76+y$exp77+y$exp95+y$exp96+y$exp99
y$milk<-y$exp53+y$exp54+y$exp55+y$exp56+y$exp57+y$exp58+y$exp59
y$fruit<-y$exp73+y$exp74+y$exp103+y$exp104+y$exp105+y$exp106+y$exp107+y$exp108+y$exp110+y$exp111+y$exp112+y$exp113+y$exp114+y$exp115+y$exp116+y$exp118+y$exp119+y$exp120+y$exp121+y$exp123+y$exp126
y$soyprod<-y$exp100+y$exp101+y$exp102
y$oilfat<-y$exp128+y$exp130+y$exp131+y$exp132
y$spices_sauces<-y$exp134+y$exp135+y$exp142+y$exp143+y$exp144+y$exp145+y$exp146+y$exp147+y$exp155+y$exp156+y$exp148+y$exp149+y$exp150+y$exp151+y$exp152+y$exp153+y$exp154
y$beverage<-y$exp136+y$exp137+y$exp139+y$exp140+y$exp141+y$exp176+y$exp177+y$exp178+y$exp179+y$exp180+y$exp181+y$exp182+y$exp184+y$exp187
y$prepared<-y$exp158+y$exp159+y$exp160+y$exp87+y$exp88+y$exp161+y$exp162+y$exp164+y$exp166+y$exp172+y$exp173+y$exp174+y$exp175+y$exp183+y$exp167+y$exp168+y$exp169+y$exp170+y$exp171
y$tobacco<-y$exp188+y$exp190

y3<-y[, c(1:3,163:175)]
y3[y3==0]<-NA
y3[4:16]<-log(y3[4:16])
y3$m<-1

write.csv(y3, "Processed Data\\y.csv")

#Create household covariates data frame
z_sub<-cbind(survey[, c("hhid", "enum_area")],  survey[,grep("sex", names(survey))],  survey[,grep("age_", names(survey))])
colnames(z_sub)[1]<-"hhid"
z_sub[is.na(z_sub)]<-0
z_sub <- data.frame(sapply(z_sub, as.numeric ))

z_sub$adultmale<-if_else(z_sub$sex_1 == 1, 1, 0)*if_else(z_sub$age_1 > 17, 1, 0)+if_else(z_sub$sex_2 == 1, 1, 0)*if_else(z_sub$age_2 > 17, 1, 0)+if_else(z_sub$sex_3 == 1, 1, 0)*if_else(z_sub$age_3 > 17, 1, 0)+if_else(z_sub$sex_4 == 1, 1, 0)*if_else(z_sub$age_4 > 17, 1, 0)+if_else(z_sub$sex_5 == 1, 1, 0)*if_else(z_sub$age_5 > 17, 1, 0)+if_else(z_sub$sex_6 == 1, 1, 0)*if_else(z_sub$age_6 > 17, 1, 0)+if_else(z_sub$sex_7 == 1, 1, 0)*if_else(z_sub$age_7 > 17, 1, 0)
z_sub$adultfem<-if_else(z_sub$sex_1 == 2, 1, 0)*if_else(z_sub$age_1 > 17, 1, 0)+if_else(z_sub$sex_2 == 2, 1, 0)*if_else(z_sub$age_2 > 17, 1, 0)+if_else(z_sub$sex_3 == 2, 1, 0)*if_else(z_sub$age_3 > 17, 1, 0)+if_else(z_sub$sex_4 == 2, 1, 0)*if_else(z_sub$age_4 > 17, 1, 0)+if_else(z_sub$sex_5 == 2, 1, 0)*if_else(z_sub$age_5 > 17, 1, 0)+if_else(z_sub$sex_6 == 2, 1, 0)*if_else(z_sub$age_6 > 17, 1, 0)+if_else(z_sub$sex_7 == 2, 1, 0)*if_else(z_sub$age_7 > 17, 1, 0)
z_sub$childmale<-if_else(z_sub$sex_1 == 1, 1, 0)*if_else(z_sub$age_1 <= 17, 1, 0)+if_else(z_sub$sex_2 == 1, 1, 0)*if_else(z_sub$age_2 <= 17, 1, 0)+if_else(z_sub$sex_3 == 1, 1, 0)*if_else(z_sub$age_3 <= 17, 1, 0)+if_else(z_sub$sex_4 == 1, 1, 0)*if_else(z_sub$age_4 <= 17, 1, 0)+if_else(z_sub$sex_5 == 1, 1, 0)*if_else(z_sub$age_5 <= 17, 1, 0)+if_else(z_sub$sex_6 == 1, 1, 0)*if_else(z_sub$age_6 <= 17, 1, 0)+if_else(z_sub$sex_7 == 1, 1, 0)*if_else(z_sub$age_7 <= 17, 1, 0)
z_sub$childfem<-if_else(z_sub$sex_1 == 2, 1, 0)*if_else(z_sub$age_1 <= 17, 1, 0)+if_else(z_sub$sex_2 == 2, 1, 0)*if_else(z_sub$age_2 <= 17, 1, 0)+if_else(z_sub$sex_3 == 2, 1, 0)*if_else(z_sub$age_3 <= 17, 1, 0)+if_else(z_sub$sex_4 == 2, 1, 0)*if_else(z_sub$age_4 <= 17, 1, 0)+if_else(z_sub$sex_5 == 2, 1, 0)*if_else(z_sub$age_5 <= 17, 1, 0)+if_else(z_sub$sex_6 == 2, 1, 0)*if_else(z_sub$age_6 <= 17, 1, 0)+if_else(z_sub$sex_7 == 2, 1, 0)*if_else(z_sub$age_7 <= 17, 1, 0)
z_sub$logHHsize<-log(z_sub$adultmale+z_sub$adultfem+z_sub$childmale+z_sub$childfem)
z_sub$HHsize<-z_sub$adultmale+z_sub$adultfem+z_sub$childmale+z_sub$childfem
z<-cbind(z_sub[, c("hhid")], rep(1, nrow(z_sub)), z_sub[,c("enum_area", "adultmale", "adultfem", "childmale", "childfem", "logHHsize")])
colnames(z)[c(1:3)]<-c("j", "t", "m")

z$m<-1
write.csv(z,"Processed Data\\z.csv")
rm(y, z)


###Uncomment the code below to estimate the MUE using python code but through R.
### This code estimates the MUE parameters
### (You don't have to as the output files are also provided in the replication package)
#use_virtualenv("r-reticulate")
#py_install("scipy")
#py_install("pandas")
#py_install("CFEDemands")

#py_run_file("Scripts\\mue.py")


#Read in outputs of estimation above
ll<-read.csv("Processed Data\\loglambda.csv")
ll<-ll[!is.na(ll$loglambdas),]
beta<-read.csv("Processed Data\\beta.csv")
colnames(beta)<-c("Good", "Elasticity")
beta<-beta[order(beta$Elasticity, decreasing = T),]
pred_exp<-read.csv("Processed Data\\pred_exp.csv")


colnames(ll)[1]<-"rankid"

ranks<-merge(ranks, ll[, c("rankid", "loglambdas")], all.x=T)

#Create Survey Rankings
ranks$need_rank_t<-NA
for (i in unique(ranks$hhid)){
  ranks[ranks$hhid==i,]$need_rank_t<-rank(-ranks[ranks$hhid==i,]$loglambdas, na.last = "keep")
}

ranks$need_rank<-as.numeric(ranks$need_rank)
ranks$need_rank_t<-as.numeric(ranks$need_rank_t)

####Assets

#Land data cleaning
land2<-survey[, c("hhid", "numhh", "enum_area", "land01a", "land02","land02m", "land02m_o", "land01b", "land02b","land02bm", "land02bm_o","land01c", "land02c", "land02cm","land02cm_o","land01d", "land02d", "land02dm","land02dm_o","land01e", "land02e", "land02em", "land02em_o")]
land2[which(land2$land02m==99),]$land02m<-6
land2[which(land2$land02bm==99),]$land02bm<-6
land2[which(land2$land02cm_o=="Ubin"),]$land02cm<-6
#Most likely units for other outliers based on units for similar amounts
land2[which(land2$hhid==913),]$land02m<-6
land2[which(land2$hhid==716),]$land02bm<-1
land2[which(land2$hhid==1006),]$land02cm<-6
land2[which(land2$hhid==628),]$land02dm<-1

##Unit conversation by category
land2$area1<-NA
land2[which(land2$land01a==3),]$area1<-0
land2[which(land2$land02m==1),]$area1<-land2[which(land2$land02m==1),]$land02
land2[which(land2$land02m==3),]$area1<-land2[which(land2$land02m==3),]$land02*1.68
land2[which(land2$land02m==5),]$area1<-land2[which(land2$land02m==5),]$land02*840
land2[which(land2$land02m==6),]$area1<-land2[which(land2$land02m==6),]$land02*14
land2[which(land2$land02==999999),]$area1<-median(land2[which(land2$land02!=999999),]$area1, na.rm=T) #impute missing values to median
land2$area2<-NA
land2[which(land2$land01b==3),]$area2<-0
land2[which(land2$land02bm==1),]$area2<-land2[which(land2$land02bm==1),]$land02b
land2[which(land2$land02bm==5),]$area2<-land2[which(land2$land02bm==5),]$land02b*840
land2[which(land2$land02bm==6),]$area2<-land2[which(land2$land02bm==6),]$land02b*14
land2[which(land2$land02b==999999),]$area2<-median(land2[which(land2$land02b!=999999),]$area2, na.rm=T)
land2$area3<-NA
land2[which(land2$land01c==3),]$area3<-0
land2[which(land2$land02cm==1),]$area3<-land2[which(land2$land02cm==1),]$land02c
land2[which(land2$land02cm==3),]$area3<-land2[which(land2$land02cm==3),]$land02c*1.68
land2[which(land2$land02cm==5),]$area3<-land2[which(land2$land02cm==5),]$land02c*840
land2[which(land2$land02cm==6),]$area3<-land2[which(land2$land02cm==6),]$land02c*14
land2[which(land2$land02c==999999),]$area3<-median(land2[which(land2$land02c!=999999),]$area3, na.rm=T)
land2$area4<-NA
land2[which(land2$land01d==3),]$area4<-0
land2[which(land2$land02dm==1),]$area4<-land2[which(land2$land02dm==1),]$land02d
land2[which(land2$land02dm==6),]$area4<-land2[which(land2$land02dm==6),]$land02d*14
land2[which(land2$land02d==999999),]$area4<-median(land2[which(land2$land02d!=999999),]$area4, na.rm=T)
land2$area5<-NA
land2[which(land2$land01e==3),]$area5<-0
land2[which(land2$land02em==6),]$area5<-land2[which(land2$land02em==6),]$land02e*14
land2[which(land2$land02e==999999),]$area5<-median(land2[which(land2$land02e!=999999),]$area5, na.rm=T)

#Aggregate Land
land2$area<-land2$area1+land2$area2+land2$area3+land2$area4+land2$area5
land2$pc_area<-land2$area/land2$numhh


#Prep house characteristics
house<-survey[, c("hhid", "house_4", "house_6", "house_8", "house_9", "house_10", "house_12", "house_14", "house_15", "house_22")]
house[,2:10] <- lapply(house[,2:10], as.factor)


#Select other assets and merge
assetk<-data.frame(cbind(survey[, c("hhid")], survey[grep("asset02", names(survey))]))
assetk[is.na(assetk)]<-0
colnames(assetk)[1]<-"hhid"
assetk<-merge(assetk, survey[, c("hhid", "asset01_27", "house_1", "house_2")], all.x=T)
assetk<-merge(assetk, land2[, c("hhid", "pc_area")], all.x=T)
assetk<-merge(assetk, house, all=T)

colnames(assetk)[1]<-"rankid"

#Additional Cleaning
assetk[assetk$asset01_27==7,]$asset02_27<-NA
assetk[which(assetk$rankid==919),]$asset02_1<-NA #This houshold had missings for indicators of having various assets
assetk[which(assetk$rankid==919), ]$asset02_2<-NA
assetk[which(assetk$rankid==919), ]$asset02_3<-NA
assetk[which(assetk$rankid==919), ]$asset02_4<-NA
assetk[which(assetk$rankid==919), ]$asset02_5<-NA
assetk[which(assetk$rankid==919), ]$asset02_6<-NA
assetk[which(assetk$rankid==919), ]$asset02_9<-NA
assetk[which(assetk$rankid==919), ]$asset02_29<-NA
assetk[which(assetk$rankid==919), ]$asset02_30<-NA
assetk[which(assetk$rankid==1016), ]$asset02_26<-NA
assetk[which(assetk$asset02_39==63), ]$asset02_39<-3
#Removal of rows with no variation
assetk$asset02_8<-NULL
assetk$asset02_20<-NULL
assetk$asset02_22<-NULL
assetk$asset02_26<-NULL
assetk$asset02_38<-NULL
assetk$asset02_27<-NULL
#Additional Cleaning
assetk[which(assetk$asset02_28==999999),]<-NA
assetk[which(assetk$asset02_27==999999),]<-NA
asset2<-assetk[complete.cases(assetk),]
asset2$asset02_29<-NULL
asset2$asset01_27<-as.factor(asset2$asset01_27)

#Estimate FAMD model
res.famd <- FAMD(asset2[,c(2:46)], ncp=5, graph = FALSE)

#Use first component as score
asset2$score<-res.famd$ind$coord[,1]

#Merge in to ranking data
ranks<-merge(ranks, asset2[, c("rankid", "score")], all.x=T)

ranks$asset_rank_t<-NA
for (i in unique(ranks$hhid)){
  ranks[ranks$hhid==i,]$asset_rank_t<-rank(ranks[ranks$hhid==i,]$score, na.last = "keep")
}


ranks$asset_rank<-as.numeric(ranks$asset_rank)
ranks$asset_rank_t<-as.numeric(ranks$asset_rank_t)

#additional asset variables for sum stat table
asset2s<-asset2
asset2s$floor_ceramic<-ifelse(asset2s$house_4==1,1,0)
asset2s$floor_tiles<-ifelse(asset2s$house_4==2,1,0)
asset2s$floor_cement<-ifelse(asset2s$house_4==3,1,0)

asset2s$roof_tiles<-ifelse(asset2s$house_6==2,1,0)
asset2s$roof_palmfiber<-ifelse(asset2s$house_6==6,1,0)
asset2s$wall_plaster<-ifelse(asset2s$house_8==1,1,0)
asset2s$own_house<-ifelse(asset2s$house_9==1,1,0)

asset2s$water_pumpwell<-ifelse(asset2s$house_10==3,1,0)
asset2s$water_protectedwell<-ifelse(asset2s$house_10==6,1,0)
asset2s$water_mineralbottled<-ifelse(asset2s$house_10==1,1,0)
asset2s$water_piped<-ifelse(asset2s$house_10==2,1,0)
asset2s$water_unprotectedwell<-ifelse(asset2s$house_10==7,1,0)
asset2s$water_outside<-ifelse(asset2s$house_12==2,1,0)
asset2s$latrine_own<-ifelse(asset2s$house_14==3,1,0)
asset2s$latrine_gooseneck<-ifelse(asset2s$house_15==1,1,0)

asset2s<-asset2s[, c(2:37, 48:62)]

######2.3 Estimate alternate welfare measures for robustness

####Consumption
#AE Exp
z_sub$adultmale_ae<-if_else(z_sub$sex_1 == 1, 1, 0)*if_else(z_sub$age_1 > 14, 1, 0)+if_else(z_sub$sex_2 == 1, 1, 0)*if_else(z_sub$age_2 > 14, 1, 0)+if_else(z_sub$sex_3 == 1, 1, 0)*if_else(z_sub$age_3 > 14, 1, 0)+if_else(z_sub$sex_4 == 1, 1, 0)*if_else(z_sub$age_4 > 14, 1, 0)+if_else(z_sub$sex_5 == 1, 1, 0)*if_else(z_sub$age_5 > 14, 1, 0)+if_else(z_sub$sex_6 == 1, 1, 0)*if_else(z_sub$age_6 > 14, 1, 0)+if_else(z_sub$sex_7 == 1, 1, 0)*if_else(z_sub$age_7 > 14, 1, 0)
z_sub$adultfem_ae<-if_else(z_sub$sex_1 == 2, 1, 0)*if_else(z_sub$age_1 > 14, 1, 0)+if_else(z_sub$sex_2 == 2, 1, 0)*if_else(z_sub$age_2 > 14, 1, 0)+if_else(z_sub$sex_3 == 2, 1, 0)*if_else(z_sub$age_3 > 14, 1, 0)+if_else(z_sub$sex_4 == 2, 1, 0)*if_else(z_sub$age_4 > 14, 1, 0)+if_else(z_sub$sex_5 == 2, 1, 0)*if_else(z_sub$age_5 > 14, 1, 0)+if_else(z_sub$sex_6 == 2, 1, 0)*if_else(z_sub$age_6 > 14, 1, 0)+if_else(z_sub$sex_7 == 2, 1, 0)*if_else(z_sub$age_7 > 14, 1, 0)
z_sub$childmale_ae<-if_else(z_sub$sex_1 == 1, 1, 0)*if_else(z_sub$age_1 <= 14, 1, 0)+if_else(z_sub$sex_2 == 1, 1, 0)*if_else(z_sub$age_2 <= 14, 1, 0)+if_else(z_sub$sex_3 == 1, 1, 0)*if_else(z_sub$age_3 <= 14, 1, 0)+if_else(z_sub$sex_4 == 1, 1, 0)*if_else(z_sub$age_4 <= 14, 1, 0)+if_else(z_sub$sex_5 == 1, 1, 0)*if_else(z_sub$age_5 <= 14, 1, 0)+if_else(z_sub$sex_6 == 1, 1, 0)*if_else(z_sub$age_6 <= 14, 1, 0)+if_else(z_sub$sex_7 == 1, 1, 0)*if_else(z_sub$age_7 <= 14, 1, 0)
z_sub$childfem_ae<-if_else(z_sub$sex_1 == 2, 1, 0)*if_else(z_sub$age_1 <= 14, 1, 0)+if_else(z_sub$sex_2 == 2, 1, 0)*if_else(z_sub$age_2 <= 14, 1, 0)+if_else(z_sub$sex_3 == 2, 1, 0)*if_else(z_sub$age_3 <= 14, 1, 0)+if_else(z_sub$sex_4 == 2, 1, 0)*if_else(z_sub$age_4 <= 14, 1, 0)+if_else(z_sub$sex_5 == 2, 1, 0)*if_else(z_sub$age_5 <= 14, 1, 0)+if_else(z_sub$sex_6 == 2, 1, 0)*if_else(z_sub$age_6 <= 14, 1, 0)+if_else(z_sub$sex_7 == 2, 1, 0)*if_else(z_sub$age_7 <= 14, 1, 0)
z_sub$ae<-z_sub$adultmale_ae+0.8*z_sub$adultfem_ae+0.5*z_sub$childmale_ae+0.5*z_sub$childfem_ae
ranks<-merge(ranks, z_sub[, c("hhid", "ae")], all.x = T, by.x="rankid", by.y="hhid")
ranks$pc_food_exp_ae<-ranks$total_food_exp/ranks$ae
z_sub<-z_sub[, c(1:23)]

ranks$ae_rank_t<-NA
for (i in unique(ranks$hhid)){
  ranks[ranks$hhid==i,]$ae_rank_t<-rank(ranks[ranks$hhid==i,]$pc_food_exp_ae, na.last = "keep")
}
ranks$ae_rank_t<-as.numeric(ranks$ae_rank_t)

#PC Cons
cons_columns <- data.frame(cbind(survey[,c("hhid")],survey[,grep("value", names(survey))]))
colnames(cons_columns)[1]<-"hhid"
cc<-c(grep("_o", names(cons_columns)))
cons_columns[,cc]<-NULL
#Cleaning
cons_columns[which(cons_columns$hhid==1028),]$value2<-55000
cons_columns[is.na(cons_columns)]<-0
cons_columns$total<-rowSums(cons_columns[, c(2:179)])
cons_columns<-merge(cons_columns, survey[, c("hhid", "numhh")])
cons_columns$pc_cons<-cons_columns$total/cons_columns$numhh
tc<-merge(df_exp, cons_columns[, c("hhid", "pc_cons", "total")],by.x="rankid", by.y="hhid", all=T)
tc$pc_exp_cons<-tc$pc_cons+tc$pc_food_exp
tc$total_cons<-tc$total+tc$total_food_exp
ranks<-merge(ranks, tc[, c("rankid","pc_exp_cons", "total_cons")], all.x=T)


ranks$pc_cons_rank_t<-NA
for (i in unique(ranks$hhid)){
  ranks[ranks$hhid==i,]$pc_cons_rank_t<-rank(ranks[ranks$hhid==i,]$pc_exp_cons, na.last="keep")
}

ranks$pc_cons_rank_t<-as.numeric(ranks$pc_cons_rank_t)


#PC Pred Exp
y_flip<-y3 %>%gather(i, log_exp, rice:tobacco)

pred_exp<-merge(pred_exp,y_flip, all.x=T)
pred_exp[is.na(pred_exp$log_exp),]$pred_exp<-0

est<-aggregate(pred_exp$pred_exp, by=list(pred_exp$j), FUN=sum)
colnames(est)<-c("hhid", "total_pred_exp")
est<-merge(est, survey[, c("hhid", "numhh")])
est$pc_pred_exp<-est$total_pred_exp/est$numhh
est<-merge(est, df_exp, by.x="hhid", by.y="rankid")
ranks<-merge(ranks, est[, c("hhid", "pc_pred_exp", "total_pred_exp")], by.x="rankid", by.y="hhid", all.x=T)
rm(est)

ranks$pc_pred_exp_rank_t<-NA
for (i in unique(ranks$hhid)){
  ranks[ranks$hhid==i,]$pc_pred_exp_rank_t<-rank(ranks[ranks$hhid==i,]$pc_pred_exp, na.last = "keep")
}
ranks$pc_pred_exp_rank_t<-as.numeric(ranks$pc_pred_exp_rank_t)


####MUE

#MUE (Cons data)
#Generate new y frame
yc<-cbind(survey[, c("hhid", "enum_area")], rep(1, nrow(survey)), survey[,grep("exp", names(survey))], survey[,grep("value", names(survey))])
yc<-dplyr::select_if(yc, is.numeric)
colnames(yc)[c(1:3)]<-c("j","m","t")
yc<-yc[, c(1, 3, 2, 4:363)]
yc$pc_food_exp<-NULL
yc$pc_exp_all<-NULL
yc[is.na(yc$value89),]$value89<-0
yc[is.na(yc$value114),]$value114<-0
yc[is.na(yc$value126),]$value126<-0
yc[is.na(yc$value141),]$value141<-0
yc<-yc[, which(!apply(yc == 0, 2, all))]
yc$rice<-yc$exp1+yc$exp2+yc$value1+yc$value2
yc$starch<-yc$exp3+yc$exp7+yc$exp8+yc$exp9+yc$exp11+yc$exp71+yc$exp4+yc$exp5+yc$exp10+yc$value3+yc$value7+yc$value8+yc$value9+yc$value71+yc$value4+yc$value5+yc$value10
yc$fish<-yc$exp12+yc$exp14+yc$exp16+yc$exp17+yc$exp18+yc$exp19+yc$exp20+yc$exp22+yc$exp23+yc$exp24+yc$exp26+yc$exp29+yc$exp30+yc$value12+yc$value17+yc$value18+yc$value19+yc$value24+yc$value26
yc$meat<-yc$exp33+yc$exp35+yc$exp42+yc$exp43+yc$exp44+yc$exp45+yc$exp46+yc$exp47+yc$exp36+yc$exp48+yc$exp49+yc$exp50+yc$exp52+yc$value33+yc$value43+yc$value36+yc$value48+yc$value49+yc$value50+yc$value52
yc$veg<-yc$exp65+yc$exp66+yc$exp67+yc$exp69+yc$exp70+yc$exp72+yc$exp75+yc$exp82+yc$exp83+yc$exp89+yc$exp78+yc$exp79+yc$exp60+yc$exp60x+yc$exp61+yc$exp62+yc$exp84+yc$exp85+yc$exp86+yc$exp63+yc$exp64+yc$exp76+yc$exp77+yc$exp95+yc$exp96+yc$exp99+yc$value65+yc$value66+yc$value67+yc$value69+yc$value70+yc$value75+yc$value82+yc$value89+yc$value78+yc$value79+yc$value60+yc$value60x+yc$value61+yc$value62+yc$value84+yc$value85+yc$value86+yc$value63+yc$value64+yc$value76
yc$milk<-yc$exp53+yc$exp54+yc$exp55+yc$exp56+yc$exp57+yc$exp58+yc$exp59+yc$value55+yc$value56+yc$value59
yc$fruit<-yc$exp73+yc$exp74+yc$exp103+yc$exp104+yc$exp105+yc$exp106+yc$exp107+yc$exp108+yc$exp110+yc$exp111+yc$exp112+yc$exp113+yc$exp114+yc$exp115+yc$exp116+yc$exp118+yc$exp119+yc$exp120+yc$exp121+yc$exp123+yc$exp126+yc$value73+yc$value74+yc$value103+yc$value104+yc$value105+yc$value107+yc$value110+yc$value111+yc$value112+yc$value113+yc$value114+yc$value115+yc$value116+yc$value118+yc$value119+yc$value120+yc$value121+yc$value126
yc$soyprod<-yc$exp100+yc$exp101+yc$exp102+yc$value100+yc$value101
yc$oilfat<-yc$exp128+yc$exp130+yc$exp131+yc$exp132+yc$value128+yc$value131+yc$value132
yc$spices_sauces<-yc$exp134+yc$exp135+yc$exp142+yc$exp143+yc$exp144+yc$exp145+yc$exp146+yc$exp147+yc$exp155+yc$exp156+yc$exp148+yc$exp149+yc$exp150+yc$exp151+yc$exp152+yc$exp153+yc$exp154+yc$value134+yc$value135+yc$value142+yc$value143+yc$value144+yc$value145+yc$value147+yc$value155+yc$value149+yc$value150+yc$value151+yc$value152+yc$value153+yc$value154
yc$beverage<-yc$exp136+yc$exp137+yc$exp139+yc$exp140+yc$exp141+yc$exp176+yc$exp177+yc$exp178+yc$exp179+yc$exp180+yc$exp181+yc$exp182+yc$exp184+yc$exp187+yc$value136+yc$value137+yc$value141+yc$value184
yc$prepared<-yc$exp158+yc$exp159+yc$exp160+yc$exp87+yc$exp88+yc$exp161+yc$exp162+yc$exp164+yc$exp166+yc$exp172+yc$exp173+yc$exp174+yc$exp175+yc$exp183+yc$exp167+yc$exp168+yc$exp169+yc$exp170+yc$exp171+yc$value158+yc$value161+yc$value162+yc$value164+yc$value166+yc$value172+yc$value173+yc$value174+yc$value175+yc$value183+yc$value167+yc$value168+yc$value169+yc$value170+yc$value171
yc$tobacco<-yc$exp188+yc$exp190+yc$value188

y_2<-yc[, c(1:3,278:290)]
y_2[y_2==0]<-NA
y_2[4:16]<-log(y_2[4:16])
y_2$m<-1

write.csv(y3, "Processed Data\\y_2.csv")

###Uncomment the code below to estimate the MUE using python code but through R.
### This code estimates the MUE parameters
### (You don't have to as the output files are also provided in the replication package)
#use_virtualenv("r-reticulate")
#py_install("scipy")
#py_install("pandas")
#py_install("CFEDemands")

#py_run_file("Scripts\\mue_alt.py")


ll2<-read.csv("Processed Data\\loglambda_2.csv")
ll2<-ll2[!is.na(ll2$loglambdas),]

colnames(ll2)[1]<-"rankid"
colnames(ll2)[4]<-"loglambdas2"


ranks<-merge(ranks, ll2[, c("rankid", "loglambdas2")], all.x=T)

ranks$alt_need_rank_t<-NA
for (i in unique(ranks$hhid)){
  ranks[ranks$hhid==i,]$alt_need_rank_t<-rank(-ranks[ranks$hhid==i,]$loglambdas2, na.last = "keep")
}
ranks$alt_need_rank_t<-as.numeric(ranks$alt_need_rank_t)

####Assets

#Land Value
land<-survey[, c("hhid", "numhh","enum_area", "land01a", "land04a", "land01b", "land04b", "land01c", "land04c", "land01d", "land04d", "land01e", "land04e")]
land[which(land$land01a==3),]$land04a<-0
land[which(land$land01b==3),]$land04b<-0
land[which(land$land01c==3),]$land04c<-0
land[which(land$land01d==3),]$land04d<-0
land[which(land$land01e==3),]$land04e<-0
a<-median(land[which(land$land04a!=999999&land$land04a>0),]$land04a, na.rm=T)
b<-median(land[which(land$land04b!=999999&land$land04b!=99999&land$land04b!=9999&land$land04b>0),]$land04b, na.rm=T)
c<-median(land[which(land$land04c!=999999&land$land04c!=99999&land$land04c!=9999&land$land04c>0),]$land04c, na.rm=T)
d<-median(land[which(land$land04d!=999999&land$land04d!=9999999&land$land04d!=9999&land$land04d>0),]$land04d, na.rm=T)
e<-median(land[which(land$land04e!=999999&land$land04e>0),]$land04e, na.rm=T)
land[which(land$land04a==999999),]$land04a<-a
land[which(is.na(land$land04a)&land$land01a==1),]$land04a<-a
land[which(land$land04b==999999|land$land04b==99999|land$land04b==9999),]$land04b<-b
land[which(land$land04c==999999|land$land04c==99999|land$land04c==9999),]$land04c<-c
land[which(land$land04d==999999|land$land04d==9999|land$land04d==9999999),]$land04d<-d
land[which(land$land04e==999999),]$land04e<-e
land[which(is.na(land$land04e)&land$land01e==1),]$land04e<-e
land$landval<-land$land04a+land$land04b+land$land04c+land$land04d+land$land04e
land$pc_landval<-land$landval/land$numhh

ranks<-merge(ranks, land[, c("hhid", "landval", "pc_landval")], by.x="rankid", by.y = "hhid", all.x=T)

ranks$land_rank_t<-NA
for (i in unique(ranks$hhid)){
  ranks[ranks$hhid==i,]$land_rank_t<-rank(ranks[ranks$hhid==i,]$landval, na.last = "keep")
}

ranks[is.na(ranks$landval),]$land_rank_t<-NA

ranks$pc_value_rank_t<-NA
for (i in unique(ranks$hhid)){
  ranks[ranks$hhid==i,]$pc_value_rank_t<-rank(ranks[ranks$hhid==i,]$pc_landval, na.last = "keep")
}

ranks$pc_value_rank_t<-as.numeric(ranks$pc_value_rank_t)

#Land Area
ranks<-merge(ranks, land2[, c("hhid", "area")], by.x= "rankid", by.y="hhid", all.x=T)

ranks$area_rank_t<-NA
for (i in unique(ranks$hhid)){
  ranks[ranks$hhid==i,]$area_rank_t<-rank(ranks[ranks$hhid==i,]$area, na.last = "keep")
}

ranks$area_rank_t<-as.numeric(ranks$area_rank_t)

#Asset Count
assetk$no_assets<-rowSums(assetk[, c(2:35)], na.rm=T)
ranks<-merge(ranks, assetk[, c("rankid", "no_assets")], all.x=T)

ranks$no_assets_rank_t<-NA
for (i in unique(ranks$hhid)){
  ranks[ranks$hhid==i,]$no_assets_rank_t<-rank(ranks[ranks$hhid==i,]$no_assets, na.last = "keep")
}

ranks$no_assets_rank_t<-as.numeric(ranks$no_assets_rank_t)

#House Index
house_ind<-assetk[,c(1,36, 37, 39:47)]
house_ind<-house_ind[complete.cases(house_ind),]

res.famdh <- FAMD(house_ind[,c(2:12)], ncp=5, graph = FALSE)

house_ind$score_h<-res.famdh$ind$coord[,1]


ranks<-merge(ranks, house_ind[, c("rankid", "score_h")], all.x = T)

ranks$score_h_rank_t<-NA
for (i in unique(ranks$hhid)){
  ranks[ranks$hhid==i,]$score_h_rank_t<-rank(-ranks[ranks$hhid==i,]$score_h, na.last = "keep")
}

ranks$score_h_rank_t<-as.numeric(ranks$score_h_rank_t)

######4. PMT Scores

#PPI
#Q1- Region
survey$ppi_1<-1 

#Q2 - HH Size
survey$ppi_2<-NA
survey[which(survey$numhh<4),]$ppi_2<-14
survey[which(survey$numhh==4|survey$numhh==5),]$ppi_2<-8
survey[which(survey$numhh>5),]$ppi_2<-0

#Q3 - Members age 0-5

survey$young_child<-0
survey[which(survey$age_3<6),]$young_child<-survey[which(survey$age_3<6),]$young_child+1
survey[which(survey$age_4<6),]$young_child<-survey[which(survey$age_4<6),]$young_child+1
survey[which(survey$age_5<6),]$young_child<-survey[which(survey$age_5<6),]$young_child+1
survey[which(survey$age_6<6),]$young_child<-survey[which(survey$age_6<6),]$young_child+1
survey[which(survey$age_7<6),]$young_child<-survey[which(survey$age_7<6),]$young_child+1

survey$ppi_3<-if_else(survey$young_child<2, 9,0)

#Q4 - Members age 6-10

survey$med_child<-0
survey[which(survey$age_2>5&survey$age_2<11),]$med_child<-survey[which(survey$age_2>5&survey$age_2<11),]$med_child+1
survey[which(survey$age_3>5&survey$age_3<11),]$med_child<-survey[which(survey$age_3>5&survey$age_3<11),]$med_child+1
survey[which(survey$age_4>5&survey$age_4<11),]$med_child<-survey[which(survey$age_4>5&survey$age_4<11),]$med_child+1
survey[which(survey$age_5>5&survey$age_5<11),]$med_child<-survey[which(survey$age_5>5&survey$age_5<11),]$med_child+1
survey[which(survey$age_6>5&survey$age_6<11),]$med_child<-survey[which(survey$age_6>5&survey$age_6<11),]$med_child+1
survey[which(survey$age_7>5&survey$age_7<11),]$med_child<-survey[which(survey$age_7>5&survey$age_7<11),]$med_child+1

survey$ppi_4<-if_else(survey$med_child<2, 8,0)

#Q5 - Internet Access
survey$ppi_5<-0 #Didn't collect this at baseline

#Q6 - Refrigerator
survey$ppi_6<-ifelse(survey$asset01_9==1, 9,0)

#Q7 - Motorcycle
survey$ppi_7<-ifelse(survey$asset01_23==1, 11,0)

#Q8 - Floor Material
survey$ppi_8<-NA
survey[which(survey$house_4==1),]$ppi_8<-9
survey[which(survey$house_4==2|survey$house_4==3),]$ppi_8<-6
survey[which(survey$house_4==6),]$ppi_8<-0

#Q9 - Toilet Type
survey$ppi_9<-ifelse(survey$house_15==1, 6, 0)

#Q10 - Fuel type used
survey$ppi_10<-ifelse(survey$house_23==1, 0, 7)

#Total 
survey$ppi<-survey$ppi_1+survey$ppi_2+survey$ppi_3+survey$ppi_4+survey$ppi_5+survey$ppi_6+survey$ppi_7+survey$ppi_8+survey$ppi_9+survey$ppi_10

####Alt (poverty scorecard)
#Q1: Location
survey$ps_1<-4

#Q2: Household Members
survey$ps_2<-NA
survey[survey$numhh==1,]$ps_2<-35
survey[survey$numhh==2,]$ps_2<-24
survey[survey$numhh==3,]$ps_2<-17
survey[survey$numhh==4,]$ps_2<-10
survey[survey$numhh==5,]$ps_2<-4
survey[survey$numhh>5,]$ps_2<-0

#Q3: Working HH members

survey$working_members<-0
survey[which(survey$employment_1==1),]$working_members<-survey[which(survey$employment_1==1),]$working_members+1
survey[which(survey$employment_2==1),]$working_members<-survey[which(survey$employment_2==1),]$working_members+1
survey[which(survey$employment_3==1),]$working_members<-survey[which(survey$employment_3==1),]$working_members+1
survey[which(survey$employment_4==1),]$working_members<-survey[which(survey$employment_4==1),]$working_members+1
survey[which(survey$employment_5==1),]$working_members<-survey[which(survey$employment_5==1),]$working_members+1
survey[which(survey$employment_6==1),]$working_members<-survey[which(survey$employment_6==1),]$working_members+1
survey[which(survey$employment_7==1),]$working_members<-survey[which(survey$employment_7==1),]$working_members+1

survey$ps_3<-NA
survey[survey$working_members==0,]$ps_3<-0
survey[survey$working_members==1,]$ps_3<-3
survey[survey$working_members>1,]$ps_3<-7

#Q4: HH Members w/ Permanent work
survey$perm_members<-0
survey[which(survey$employment1_1==1|survey$employment1_1==4|survey$employment1_1==5),]$perm_members<-survey[which(survey$employment1_1==1|survey$employment1_1==4|survey$employment1_1==5),]$perm_members+1
survey[which(survey$employment1_2==1|survey$employment1_2==4|survey$employment1_2==5),]$perm_members<-survey[which(survey$employment1_2==1|survey$employment1_2==4|survey$employment1_2==5),]$perm_members+1
survey[which(survey$employment1_3==1|survey$employment1_3==3|survey$employment1_3==4|survey$employment1_3==5),]$perm_members<-survey[which(survey$employment1_3==1|survey$employment1_3==3|survey$employment1_3==4|survey$employment1_3==5),]$perm_members+1
survey[which(survey$employment1_4==1|survey$employment1_4==4|survey$employment1_4==5),]$perm_members<-survey[which(survey$employment1_4==1|survey$employment1_4==4|survey$employment1_4==5),]$perm_members+1
survey[which(survey$employment1_5==1|survey$employment1_5==5),]$perm_members<-survey[which(survey$employment1_5==1|survey$employment1_5==5),]$perm_members+1
survey[which(survey$employment1_6==5),]$perm_members<-survey[which(survey$employment1_6==5),]$perm_members+1
survey[which(survey$employment1_7==5),]$perm_members<-survey[which(survey$employment1_7==5),]$perm_members+1

survey$ps_4<-NA
survey[survey$perm_members==0,]$ps_4<-0
survey[survey$perm_members==1,]$ps_4<-3
survey[survey$perm_members>1,]$ps_4<-7

#Q5: female head has phone (approximation if >1 phone/household)
survey$ps_5<-ifelse(survey$asset02_19>1,5,0)
survey[which(survey$asset01_19==3),]$ps_5<-0

#Q6: Fuel type
survey$ps_6<-ifelse(survey$house_23==1, 0, 6)

#Q7: Toilet type
survey$ps_7<-ifelse(survey$house_15==1, 3, 0)

#Q8: Refrigerator
survey$ps_8<-ifelse(survey$asset01_9==1, 9,0)

#Q9: Motorcycle
survey$ps_9<-ifelse(survey$asset01_23==1, 10,0)

#Q10: Rastra Recipient
survey$ps_10<-ifelse(survey$program2b==3, 4,0)
survey[survey$program1b==3,]$ps_10<-4

##total
survey$ps<-survey$ps_1+survey$ps_2+survey$ps_3+survey$ps_4+survey$ps_5+survey$ps_6+survey$ps_7+survey$ps_8+survey$ps_9+survey$ps_10
ranks<-merge(ranks, survey[, c("hhid", "ppi", "ps")], by.x="rankid", by.y="hhid", all.x=T)

ranks$ppi_rank<-NA
for (i in unique(ranks$hhid)){
  ranks[ranks$hhid==i,]$ppi_rank<-rank(ranks[ranks$hhid==i,]$ppi, na.last = "keep")
}
ranks$ppi_rank<-as.numeric(as.character(ranks$ppi_rank))

ranks$ps_rank<-NA
for (i in unique(ranks$hhid)){
  ranks[ranks$hhid==i,]$ps_rank<-rank(ranks[ranks$hhid==i,]$ps, na.last = "keep")
}

######2.5 Follow up Welfare indicators

####Updated HH Roster (and z matrix for MUE estimation)

franks$hhid<-as.numeric(franks$hhid)
fz<-z_sub
###13 households with roster changes (some mistakes--confirmed with team)
fz[which(fz$hhid==114),]$adultmale<-1
fz[which(fz$hhid==114),]$HHsize<-3
fz[which(fz$hhid==117),]$adultmale<-1
fz[which(fz$hhid==117),]$adultfem<-0
fz[which(fz$hhid==117),]$HHsize<-1
fz[which(fz$hhid==130),]$adultmale<-2
fz[which(fz$hhid==130),]$HHsize<-5
fz[which(fz$hhid==134),]$adultfem<-2
fz[which(fz$hhid==134),]$HHsize<-3
fz[which(fz$hhid==136),]$adultmale<-1
fz[which(fz$hhid==136),]$HHsize<-2
fz[which(fz$hhid==202),]$adultmale<-1
fz[which(fz$hhid==202),]$HHsize<-3
fz[which(fz$hhid==203),]$adultfem<-2
fz[which(fz$hhid==203),]$HHsize<-5
fz[which(fz$hhid==204),]$adultfem<-2
fz[which(fz$hhid==204),]$HHsize<-3
fz[which(fz$hhid==213),]$adultmale<-3
fz[which(fz$hhid==213),]$HHsize<-6
fz[which(fz$hhid==230),]$adultmale<-1
fz[which(fz$hhid==230),]$adultfem<-1
fz[which(fz$hhid==230),]$HHsize<-3
fz[which(fz$hhid==716),]$adultmale<-0
fz[which(fz$hhid==716),]$HHsize<-1

fz$logHHsize<-log(fz$HHsize)
colnames(fz)[23]<-"numhh_update"

follow_up<-merge(follow_up, fz[, c("hhid", "numhh_update")], all.x=T)

colnames(fz)[c(1)]<-"j"

fz<-fz[which(fz$enum_area==1| fz$enum_area==2|fz$enum_area==7),]
fz<-fz[fz$j!=218,]
fz<-fz[, c(1,18:22)]
fz$t<-2
fz$m<-1

pz<-z_sub[, c(1,18:22)]
pz$t<-1
pz$m<-1
colnames(pz)[1]<-"j"

z_f<-rbind(fz, pz)
z_f<-z_f[, c(1, 7:8, 2:6)]

write.csv(z_f, "Processed Data\\z_m.csv")

####Generate Expenditure ranks (follow up)

exp_columns <- follow_up[,grep("exp", names(follow_up))]
follow_up$total_food_exp<-rowSums(dplyr::select_if(exp_columns, is.numeric), na.rm=T)
follow_up$pc_food_exp<-follow_up$total_food_exp/follow_up$numhh_update    

exp_sub<-dplyr::select_if(exp_columns, is.numeric)
list1<-list()
for (i in c(1:ncol(exp_sub))){
  list1[i]<-as.numeric(max(exp_sub[i], na.rm=T))
}
df_exp<-follow_up[, c("hhid", "pc_food_exp", "total_food_exp")]
colnames(df_exp)[1]<-"rankid"

franks<-merge(franks, df_exp, all.x=T)

franks$exp_rank_t<-NA
for (i in unique(franks$hhid)){
  franks[franks$hhid==i,]$exp_rank_t<-rank(franks[franks$hhid==i,]$pc_food_exp, na.last = "keep")
}

franks$exp_rank<-as.numeric(franks$exp_rank)
franks$exp_rank_t<-as.numeric(franks$exp_rank_t)

franks$pc_food_exp_dollar2<-franks$pc_food_exp/4700
franks$pc_food_exp2<-franks$pc_food_exp
franks$pc_food_exp<-NULL

franks<-merge(franks, ranks[, c("rankid", "hhid", "pc_food_exp")], all.x=T)


####MUE Ranks (follow-up)
yf<-cbind(follow_up[, c("hhid", "enum_area")], rep(2, nrow(follow_up)), follow_up[,grep("exp", names(follow_up))])
yf<-dplyr::select_if(yf, is.numeric)
colnames(yf)[c(1:3)]<-c("j","m","t")
yf<-yf[, c(1, 3, 2, 4:185)]
yf$pc_food_exp<-NULL
yf$total_food_exp<-NULL
yf<-yf[, which(!apply(yf == 0, 2, all))]
yf$rice<-yf$exp1+yf$exp2
yf$starch<-yf$exp7+yf$exp8+yf$exp9+yf$exp71+yf$exp4+yf$exp5+yf$exp10
yf$fish<-yf$exp12+yf$exp14+yf$exp18+yf$exp19+yf$exp20+yf$exp22+yf$exp23+yf$exp24+yf$exp26+yf$exp29+yf$exp30
yf$meat<-yf$exp35+yf$exp46+yf$exp36+yf$exp48+yf$exp49+yf$exp50+yf$exp52
yf$veg<-yf$exp65+yf$exp66+yf$exp67+yf$exp69+yf$exp70+yf$exp72+yf$exp75+yf$exp82+yf$exp83+yf$exp89+yf$exp78+yf$exp79+yf$exp60+yf$exp60x+yf$exp61+yf$exp62+yf$exp84+yf$exp85+yf$exp86+yf$exp63+yf$exp64+yf$exp95+yf$exp96
yf$milk<-yf$exp53+yf$exp54+yf$exp55+yf$exp56+yf$exp59
yf$fruit<-yf$exp73+yf$exp74+yf$exp103+yf$exp104+yf$exp105+yf$exp106+yf$exp110+yf$exp111+yf$exp112+yf$exp113+yf$exp114+yf$exp115+yf$exp116+yf$exp117+yf$exp118+yf$exp120+yf$exp121+yf$exp126
yf$soyprod<-yf$exp100+yf$exp101
yf$oilfat<-yf$exp128+yf$exp131+yf$exp127
yf$spices_sauces<-yf$exp134+yf$exp135+yf$exp142+yf$exp143+yf$exp144+yf$exp145+yf$exp147+yf$exp155+yf$exp149+yf$exp150+yf$exp151+yf$exp152+yf$exp153+yf$exp154
yf$beverage<-yf$exp136+yf$exp137+yf$exp138+yf$exp140+yf$exp141+yf$exp176+yf$exp177+yf$exp178+yf$exp180+yf$exp182+yf$exp184
yf$prepared<-yf$exp158+yf$exp159+yf$exp160+yf$exp87+yf$exp88+yf$exp161+yf$exp162+yf$exp164+yf$exp166+yf$exp172+yf$exp173+yf$exp174+yf$exp183+yf$exp167+yf$exp168+yf$exp169+yf$exp170+yf$exp171
yf$tobacco<-yf$exp188+yf$exp190

y4<-yf[, c(1:3,136:148)]
y4[y4==0]<-NA
y4[4:16]<-log(y4[4:16])
y4$m<-1

y_f<-rbind(y3, y4)
write.csv(y_f, "Processed Data\\y_f.csv")

###Uncomment the code below to estimate the MUE using python code but through R.
### This code estimates the MUE parameters
### (You don't have to as the output files are also provided in the replication package)
#use_virtualenv("r-reticulate")
#py_install("scipy")
#py_install("pandas")
#py_install("CFEDemands")

#py_run_file("Scripts\\muef.py")
llf<-read.csv("Processed Data\\loglambda_f.csv")
llf<-llf[!is.na(llf$loglambdas),]
llf$loglambdas2<-llf$loglambdas
llf$loglambdas<-NULL
colnames(llf)[1]<-"rankid"


llf<-llf[llf$t==2,]

franks<-merge(franks, ll[, c("rankid", "loglambdas")], all.x=T)

franks<-merge(franks, llf[, c("rankid", "loglambdas2")], all.x=T)

franks$need_rank_t<-NA
for (i in unique(franks$hhid)){
  franks[franks$hhid==i,]$need_rank_t<-rank(-franks[franks$hhid==i,]$loglambdas2, na.last = "keep")
}

franks$need_rank<-as.numeric(franks$need_rank)
franks$need_rank_t<-as.numeric(franks$need_rank_t)


####Assets Rank (follow-up)
###Generate Asset ranks (follow up)
##land
fland2<-follow_up[, c("hhid", "numhh_update", "enum_area", "land01a", "land02","land02m", "land02m_o", "land01b", "land02b","land02bm", "land02bm_o","land01c", "land02c", "land02cm","land02cm_o","land01d", "land02d", "land02dm","land02dm_o","land01e", "land02e", "land02em", "land02em_o")]
fland2$area1<-NA
fland2[which(fland2$land01a==3),]$area1<-0
fland2[which(fland2$land02m==1),]$area1<-fland2[which(fland2$land02m==1),]$land02
fland2[which(fland2$land02m==3),]$area1<-fland2[which(fland2$land02m==3),]$land02*1.68
fland2[which(fland2$land02m==5),]$area1<-fland2[which(fland2$land02m==5),]$land02*840
fland2[which(fland2$land02m==6),]$area1<-fland2[which(fland2$land02m==6),]$land02*14
fland2[which(fland2$land02==999999),]$area1<-median(fland2[which(fland2$land02!=999999),]$area1, na.rm=T)
fland2$area2<-NA
fland2[which(fland2$land01b==3),]$area2<-0
fland2[which(fland2$land02bm==3),]$area2<-fland2[which(fland2$land02bm==3),]$land02b*1.68
fland2[which(fland2$land02bm==5),]$area2<-fland2[which(fland2$land02bm==5),]$land02b*840
fland2[which(fland2$land02bm==6),]$area2<-fland2[which(fland2$land02bm==6),]$land02b*14
fland2$area3<-NA
fland2[which(fland2$land01c==3),]$area3<-0
fland2[which(fland2$land02cm==3),]$area3<-fland2[which(fland2$land02cm==3),]$land02c*1.68
fland2[which(fland2$land02cm==6),]$area3<-fland2[which(fland2$land02cm==6),]$land02c*14
fland2$area4<-NA
fland2[which(fland2$land01d==3),]$area4<-0
fland2[which(fland2$land02dm==1),]$area4<-fland2[which(fland2$land02dm==1),]$land02d
fland2$area5<-NA
fland2[which(fland2$land01e==3),]$area5<-0
fland2[which(fland2$land02em==1),]$area4<-fland2[which(fland2$land02em==1),]$land02e
fland2$area<-fland2$area1+fland2$area2+fland2$area3+fland2$area4+fland2$area5
fland2$pc_area<-fland2$area/fland2$numhh
fhouse<-follow_up[, c("hhid", "house_4", "house_6", "house_8", "house_9", "house_10", "house_12", "house_14", "house_15", "house_22")]
fhouse[,2:10] <- lapply(fhouse[,2:10], as.factor)



assetk2<-data.frame(cbind(follow_up[, c("hhid")], follow_up[grep("asset02", names(follow_up))]))
colnames(assetk2)[1]<-"hhid"
assetk2[is.na(assetk2)]<-0
assetk2<-merge(assetk2, follow_up[, c("hhid", "asset01_27", "house_1", "house_2")], all.x=T)
assetk2<-merge(assetk2, fland2[, c("hhid", "pc_area")], all.x=T)
assetk2<-merge(assetk2, fhouse, all=T)

colnames(assetk2)[1]<-"rankid"

assetk2[assetk2$asset01_27==7,]$asset02_27<-NA
assetk2[assetk2$rankid==116,]$house_1<-56
assetk2[assetk2$rankid==116,]$house_2<-2

assetk2$asset02_8<-NULL
assetk2$asset02_20<-NULL
assetk2$asset02_22<-NULL
assetk2$asset02_26<-NULL
assetk2$asset02_38<-NULL
assetk2$asset02_27<-NULL

asset2f<-assetk2[complete.cases(assetk2),]
asset2f$asset02_29<-NULL
asset2f$asset01_27<-as.factor(asset2f$asset01_27)

asset2_comb<-rbind(asset2[1:46], asset2f)
asset2_comb$time<-c(rep(1, 266), rep(2, 79))

res.famd1 <- FAMD(asset2_comb[,c(2:46)], ncp=5, graph = FALSE)

asset2_comb$score2<-res.famd1$ind$coord[,1]


asset2_c<-merge(asset2_comb, asset2[, c("rankid", "score")], all.x=T)

franks<-merge(franks, asset2_c[asset2_c$time==2, c("rankid", "score", "score2")], all.x=T)

franks$asset_rank_t<-NA
for (i in unique(franks$hhid)){
  franks[franks$hhid==i,]$asset_rank_t<-rank(franks[franks$hhid==i,]$score2, na.last = "keep")
}

franks$asset_rank_t<-as.numeric(franks$asset_rank_t)

####Merge in R1 rankings as well
subranks<-ranks[, c("hhid", "rankid", "exp_rank", "need_rank", "asset_rank", "exp_rank_t", "need_rank_t", "asset_rank_t")]
colnames(subranks)<-c("hhid", "rankid", "exp_rank_r1", "need_rank_r1", "asset_rank_r1", "exp_rankf", "need_rankf", "asset_rankf")

franks<-merge(franks, subranks, all.x = T)

####Merge in Community meeting rankings
franks<-merge(franks, comrank[, c("rankid", "comm_rank")], all.x=T)

###Calculate relative community rankings for set ranked by each individual

franks$comm_rank_hh<-NA
for (i in unique(ranks$hhid)){
  franks[franks$hhid==i,]$comm_rank_hh<-rank(franks[franks$hhid==i,]$comm_rank, na.last = "keep")
}
franks$comm_rank_hh<-as.numeric(franks$comm_rank_hh)

###Calculate changes in rankings between rounds
##Participant rank changes
franks$dr_exp<-franks$exp_rank-franks$exp_rank_r1
franks$dr_need<-franks$need_rank-franks$need_rank_r1
franks$asset_rank_r1<-as.numeric(franks$asset_rank_r1)
franks$dr_asset<-franks$asset_rank-franks$asset_rank_r1

##Survey rank changes
franks$drt_exp<-franks$exp_rank_t-franks$exp_rankf
franks$drt_need<-franks$need_rank_t-franks$need_rankf
franks$drt_asset<-franks$asset_rank_t-franks$asset_rankf

##Survey value changes
franks$dexp<-franks$pc_food_exp2-franks$pc_food_exp
franks$dneed<- -(franks$loglambdas2-franks$loglambdas)
franks$dasset<-franks$score2-franks$score

## SD Changes (for graph)
franks$dexp_pc<-franks$dexp/4700
franks$dexp_sd<-franks$dexp_pc/34.26
franks$dneed_sd<-franks$dneed/1
franks$dasset_sd<-franks$dneed/2.78


######2.6 Shocks and Benefits
##Shocks
#Recode no shocks and missings
shocks[which(shocks$shock==3),]$shock<-0
shocks[which(shocks$shock==7|shocks$shock==8),]$shock<-NA
shocks[which(shocks$shock==0),c(4:12)]<-0

#prep survey data for merging
shocks_true<-survey[, c("hhid", "shock1a", "shock1b", "shock1c", "shock1d", "shock1e", "shock1f", "shock1g", "shock1h", "shock1i")]
shocks_true[shocks_true==3]<-0
colnames(shocks_true)<-c("rankid", "t_death_head", "t_death_other", "t_illness_head", "t_illness_other", "t_employment_loss", "t_disaster", "t_harvest_fail", "t_harvest_small", "t_other_shock")

#merge
shocks<-merge(shocks, shocks_true, all.x = T, by.x = "rankid")

#Create variables for having any shock
shocks$t_shock<-0
shocks[which(rowSums(shocks[,c(14:22)])>0),]$t_shock<-1
shocks[is.na(rowSums(shocks[,c(14:22)])),]$t_shock<-NA

#Benefits
#Recode no benefits and missings
covid[which(covid$ben==3),]$ben<-0
covid[which(covid$ben==7|covid$ben==8),]$ben<-NA

#prep survey data for merging
cov_true<-survey[, c("hhid", "program2h")]
cov_true[cov_true==3|is.na(cov_true)]<-0
colnames(cov_true)[1]<-"rankid"

#merge
covid<-merge(covid, cov_true[, c("rankid", "program2h")], all.x=T)



######2.7 Additional Variables (For summary stats)

z_sub$HHsize<-z_sub$adultmale+z_sub$adultfem+z_sub$childmale+z_sub$childfem
z<- z_sub[,c("hhid","enum_area", "adultmale", "adultfem", "childmale", "childfem", "HHsize")]


head1<-survey[which(survey$relationhh_1==1),c(10, 23:35 )]
head2<-survey[which(survey$relationhh_2==1),c(10, 38:50 )]
head3<-survey[which(survey$relationhh_3==1),c(10, 53:65 )]

colnames(head2)<-colnames(head1)
colnames(head3)<-colnames(head1)
heads<-data.frame(rbind(head1, head2, head3))
heads$male<-ifelse(heads$sex_1==1, 1, 0)
heads$disabled<-ifelse(heads$disability_1==2, 1, 0)
heads$educ_any<-ifelse(heads$education_1>1, 1, 0)
heads$hs_above<-ifelse(heads$education_1>7, 1, 0)
heads$married<-ifelse(heads$marital_1==2, 1, 0)
heads$employed<-ifelse(heads$employment_1==1,1, 0)
heads$employed_ag<-ifelse(heads$employment2_1==1, 1, 0)
heads[is.na(heads$employment2_1),]$employed_ag<-0 ###Not employed

z2<-merge(z, heads[, c("hhid", "male", "age_1", "disabled", "educ_any", "hs_above", "married", "employed", "employed_ag")], all.x=T)


#max education and disability status of HH
survey$max_edu<-pmax(survey$education_1, survey$education_2, survey$education_3, survey$education_4, survey$education_5, survey$education_6, survey$education_7, na.rm=T)
survey[survey$hhid==635,]$max_edu<-18 #fix 99
survey[survey$hhid==610,]$max_edu<-3 #fix 99

survey$dm<-pmax(survey$disability_1, survey$disability_2, survey$disability_3, survey$disability_4, survey$disability_5, survey$disability_6, survey$disability_7, na.rm=T)
survey[survey$hhid==635,]$max_edu<-18 #fix 99
survey[survey$hhid==610,]$max_edu<-3 #fix 99

survey$hh_hs<-ifelse(survey$max_edu>7, 1, 0)
survey$edhh<-ifelse(survey$max_edu>1, 1, 0)


survey$disabilityhh<-ifelse(survey$dm>1, 1, 0)
survey$jawa<-ifelse(survey$ethnic==1, 1, 0)
survey$muslim<-ifelse(survey$religion==1, 1, 0)
survey$speakjava<-ifelse(survey$language==2, 1, 0)
survey$bornvill<-ifelse(survey$living==1, 1, 0)


survey$official<-ifelse(survey$hhid%in%unique(survey[which(survey$know_chairman==4|survey$know_hamlet==4|survey$know_cluster==4),]$hhid), 1, 0)
survey$know_official<-ifelse(survey$hhid%in%unique(survey[which(survey$know_chairman==1|survey$know_hamlet==1|survey$know_cluster==1|survey$know_dprd==1),]$hhid), 1, 0)
survey$comm_org<-ifelse(survey$hhid%in%unique(survey[which(survey$part_credit==1|survey$part_govins==1|survey$part_relig==1|survey$part_mass==1|survey$part_workers==1|survey$part_social==1|survey$part_recreational==1|survey$part_other==1),]$hhid), 1, 0)

survey$nf_month<-rowSums(survey[grep("nonfood_month", names(survey))])
survey$nf_year<-rowSums(survey[grep("nonfood_year", names(survey))])

survey$pc_exp_all<-(survey$total_food_exp+survey$nf_month/4+survey$nf_year/52)/survey$numhh

survey$shock<-ifelse(survey$hhid%in%unique(survey[which(survey$shock1a==1|survey$shock1b==1|survey$shock1c==1|survey$shock1d==1|survey$shock1e==1|survey$shock1f==1|survey$shock1g==1|survey$shock1h==1|survey$shock1i==1),]$hhid), 1, 0)

survey$benefit<-ifelse(survey$hhid%in%unique(survey[which(survey$program2a==1|survey$program2b==1|survey$program2c==1|survey$program2d==1|survey$program2e==1|survey$program2f==1|survey$program2g==1|survey$program2h==1|survey$program2i==1|survey$program2j==1|survey$program2k==1|survey$program2l==1|survey$program2m==1|survey$program2n==1|survey$program2o==1),]$hhid), 1, 0)
survey$benefit_covid<-ifelse(survey$hhid%in%unique(survey[which(survey$program2h==1),]$hhid), 1, 0)


zs2<-merge(z2, survey[, c("hhid","hh_hs", "edhh", "disabilityhh", "jawa", "muslim", "speakjava","official", "know_official", "comm_org","bornvill", "hhliveyears","ladder", "pc_food_exp", "pc_exp_all", "shock", "benefit", "benefit_covid")], all.x=T)
adjustyrs = function(x) {
  ifelse(x > 1000, 2021 - x, x)
}

zs2$hhliveyears <- adjustyrs(zs2$hhliveyears)
zs2[zs2$ladder==8,]$ladder<-NA

#merge in Welfare
zs2<-merge(zs2, ll[, c("rankid", "loglambdas")], by.x="hhid", by.y="rankid", all.x=T)
zs2<-merge(zs2, asset2[,c("rankid", "score")], by.x="hhid", by.y="rankid", all.x=T)
zs2<-merge(zs2, land2[,c("hhid", "area")], all.x=T)
zs2<-merge(zs2, land[,c("hhid", "landval")], all.x=T)
zs2<-merge(zs2, survey[,c("hhid", "ppi", "ps")],  all.x=T)
zs2$pc_food_exp<-zs2$pc_food_exp/4700 #dollars
zs2$pc_exp_all<-zs2$pc_exp_all/4700 #dollars
zs2$landval<-zs2$landval/4700 # dollars
zs2$area<-zs2$area/10000 #hectares

###others and others to id respondents

spouse1<-survey[which(survey$relationhh_1==2),c(10, 23:35 )]
spouse2<-survey[which(survey$relationhh_2==2),c(10, 38:50 )]
spouse3<-survey[which(survey$relationhh_4==2),c(10, 68:80 )]

colnames(spouse2)<-colnames(spouse1)
colnames(spouse3)<-colnames(spouse1)
spouses<-data.frame(rbind(spouse1, spouse2, spouse3))
spouses$male<-ifelse(spouses$sex_1==1, 1, 0)
spouses$disabled<-ifelse(spouses$disability_1==2, 1, 0)
spouses$educ_any<-ifelse(spouses$education_1>1, 1, 0)
spouses$hs_above<-ifelse(spouses$education_1>7, 1, 0)
spouses$married<-ifelse(spouses$marital_1==2, 1, 0)
spouses$employed<-ifelse(spouses$employment_1==1,1, 0)
spouses$employed_ag<-ifelse(spouses$employment2_1==1, 1, 0)
spouses[is.na(spouses$employment2_1),]$employed_ag<-0 ###Not employed


#others
other1a<-survey[survey$hhid==920|survey$hhid==928|survey$hhid==629|survey$hhid==510,c(10, 38:50 )]
other2a<-survey[survey$hhid==1017|survey$hhid==209|survey$hhid==211|survey$hhid==114|survey$hhid==1038|survey$hhid==506|survey$hhid==808,c(10, 53:65 )]
other3a<-survey[survey$hhid==720,c(10, 113:125 )]
colnames(other1a)<-colnames(spouse1)
colnames(other2a)<-colnames(spouse1)
colnames(other3a)<-colnames(spouse1)

others<-data.frame(rbind(other1a, other2a, other3a))
others$male<-ifelse(others$sex_1==1, 1, 0)
others$disabled<-ifelse(others$disability_1==2, 1, 0)
others$educ_any<-ifelse(others$education_1>1, 1, 0)
others$hs_above<-ifelse(others$education_1>7, 1, 0)
others$married<-ifelse(others$marital_1==2, 1, 0)
others$employed<-ifelse(others$employment_1==1,1, 0)
others$employed_ag<-ifelse(others$employment2_1==1, 1, 0)
others[is.na(others$employment2_1),]$employed_ag<-0 ###Not employed

resp1<-survey[which(survey$respondent==1),]
resp2<-survey[which(survey$respondent==2),]
resp3<-survey[which(survey$respondent==3),]

resp1a<-heads[heads$hhid%in%unique(resp1$hhid), c("hhid", "male", "age_1")]
resp2a<-spouses[spouses$hhid%in%unique(resp2$hhid), c("hhid", "male", "age_1")]
resp3a<-others[others$hhid%in%unique(resp3$hhid), c("hhid", "male", "age_1")]

respondents<-rbind(resp1a, resp2a, resp3a)
respondents$enum_area<-as.integer(substr(respondents$hhid, 1, 1))
respondents[respondents$hhid>999,]$enum_area<-as.integer(substr(respondents[respondents$hhid>999,]$hhid, 1, 2))

respondents_tsub<-respondents[respondents$enum_area==1|respondents$enum_area==2|respondents$enum_area==7,]
respondents_tsub<-respondents_tsub[respondents_tsub$hhid!=218,]

zs2_tsub<-zs2[zs2$enum_area==1|zs2$enum_area==2|zs2$enum_area==7,]
zs2_tsub<-zs2_tsub[zs2_tsub$hhid!=218,]

#3.Correlations/Matching Estimation########
##3.1 Baseline Survey/Participant Correlations of Same Metric
#Expenditures
df3<-data.frame(hhid=numeric(), corr_exp=numeric())

for (i in unique(ranks$hhid)) {
  ck<-c(i,cor(ranks[ranks$hhid==i,]$exp_rank, ranks[ranks$hhid==i,]$exp_rank_t, method = "spearman", use="pairwise.complete.obs"))
  df3<-rbind(df3,ck)
}
colnames(df3)<-c("hhid", "corr_exp")

#Well-known only
df3a<-data.frame(hhid=numeric(), corr_exp=numeric())
temp<-ranks[ranks$well==1,]
for (i in unique(temp$hhid)) {
  ck<-c(i,cor(temp[temp$hhid==i,]$exp_rank, temp[temp$hhid==i,]$exp_rank_t, method = "spearman", use="pairwise.complete.obs"))
  df3a<-rbind(df3a,ck)
}
colnames(df3a)<-c("hhid", "corr_exp")

#MUE
df4<-data.frame(hhid=numeric(), corr_expn=numeric())

for (i in unique(ranks$hhid)) {
  ck<-c(i,cor(ranks[ranks$hhid==i,]$need_rank, ranks[ranks$hhid==i,]$need_rank_t, method = "spearman", use="pairwise.complete.obs"))
  df4<-rbind(df4,ck)
}
colnames(df4)<-c("hhid", "corr_expn")

#wellknown
df4a<-data.frame(hhid=numeric(), corr_expn=numeric())
for (i in unique(temp$hhid)) {
  ck<-c(i,cor(temp[temp$hhid==i,]$need_rank, temp[temp$hhid==i,]$need_rank_t, method = "spearman", use="pairwise.complete.obs"))
  df4a<-rbind(df4a,ck)
}
colnames(df4a)<-c("hhid", "corr_expn")

#assets
df5<-data.frame(hhid=numeric(), corr_expa=numeric())

for (i in unique(ranks$hhid)) {
  ck<-c(i,cor(ranks[ranks$hhid==i,]$asset_rank, ranks[ranks$hhid==i,]$asset_rank_t, method = "spearman", use="pairwise.complete.obs"))
  df5<-rbind(df5,ck)
}
colnames(df5)<-c("hhid", "corr_expa")

#well-known
df5aa<-data.frame(hhid=numeric(), corr_expa=numeric())

for (i in unique(temp$hhid)) {
  ck<-c(i,cor(temp[temp$hhid==i,]$asset_rank, temp[temp$hhid==i,]$asset_rank_t, method = "spearman", use="pairwise.complete.obs"))
  df5aa<-rbind(df5aa,ck)
}

colnames(df5aa)<-c("hhid", "corr_expa")

#Add enumeration area back in for cluster bootstrapping later
df3<-merge(df3, survey[, c("hhid", "enum_area")], all.x=T)
df4<-merge(df4, survey[, c("hhid", "enum_area")], all.x=T)
df5<-merge(df5, survey[, c("hhid", "enum_area")], all.x=T)

df3a<-merge(df3a, survey[, c("hhid", "enum_area")], all.x=T)
df4a<-merge(df4a, survey[, c("hhid", "enum_area")], all.x=T)
df5aa<-merge(df5aa, survey[, c("hhid", "enum_area")], all.x=T)

#Differences (absolute) survey vs. participant
ranks$d_exp<-abs(ranks$exp_rank-ranks$exp_rank_t)
ranks$d_need<-abs(ranks$need_rank-ranks$need_rank_t)
ranks$d_asset<-abs(ranks$asset_rank-ranks$asset_rank_t)

##3.2 Correlations within survey metrics and within participant metrics
expneed<-data.frame(hhid=numeric(), corr_exp=numeric())

for (i in unique(ranks$hhid)) {
  ck<-c(i,cor(ranks[ranks$hhid==i,]$exp_rank_t, ranks[ranks$hhid==i,]$need_rank_t, method = "spearman", use="pairwise.complete.obs"))
  expneed<-rbind(expneed,ck)
}
colnames(expneed)<-c("hhid", "corr_exp")

expasset<-data.frame(hhid=numeric(), corr_exp=numeric())

for (i in unique(ranks$hhid)) {
  ck<-c(i,cor(ranks[ranks$hhid==i,]$exp_rank_t, ranks[ranks$hhid==i,]$asset_rank_t, method = "spearman", use="pairwise.complete.obs"))
  expasset<-rbind(expasset,ck)
}
colnames(expasset)<-c("hhid", "corr_exp")

needasset<-data.frame(hhid=numeric(), corr_need=numeric())

for (i in unique(ranks$hhid)) {
  ck<-c(i,cor(ranks[ranks$hhid==i,]$asset_rank_t, ranks[ranks$hhid==i,]$need_rank_t, method = "spearman", use="pairwise.complete.obs"))
  needasset<-rbind(needasset,ck)
}
colnames(needasset)<-c("hhid", "corr_exp")


expneeds<-data.frame(hhid=numeric(), corr_exp=numeric())

for (i in unique(ranks$hhid)) {
  ck<-c(i,cor(ranks[ranks$hhid==i,]$exp_rank, ranks[ranks$hhid==i,]$need_rank, method = "spearman", use="pairwise.complete.obs"))
  expneeds<-rbind(expneeds,ck)
}
colnames(expneeds)<-c("hhid", "corr_exp")

expassets<-data.frame(hhid=numeric(), corr_exp=numeric())

for (i in unique(ranks$hhid)) {
  ck<-c(i,cor(ranks[ranks$hhid==i,]$exp_rank, ranks[ranks$hhid==i,]$asset_rank, method = "spearman", use="pairwise.complete.obs"))
  expassets<-rbind(expassets,ck)
}
colnames(expassets)<-c("hhid", "corr_exp")


needassets<-data.frame(hhid=numeric(), corr_need=numeric())

for (i in unique(ranks$hhid)) {
  ck<-c(i,cor(ranks[ranks$hhid==i,]$asset_rank, ranks[ranks$hhid==i,]$need_rank, method = "spearman", use="pairwise.complete.obs"))
  needassets<-rbind(needassets,ck)
}
colnames(needassets)<-c("hhid", "corr_exp")

#Add enumeration area back in for cluster bootstrapping later
expneed<-merge(expneed, survey[, c("hhid", "enum_area")], all.x=T)
expneeds<-merge(expneeds, survey[, c("hhid", "enum_area")], all.x=T)
expasset<-merge(expasset, survey[, c("hhid", "enum_area")], all.x=T)
expassets<-merge(expassets, survey[, c("hhid", "enum_area")], all.x=T)
needasset<-merge(needasset, survey[, c("hhid", "enum_area")], all.x=T)
needassets<-merge(needassets, survey[, c("hhid", "enum_area")],, all.x=T)

##Calculate correlations of participant expend. and MUE rankings with survey measured assets

expassets2<-data.frame(hhid=numeric(), corr_exp=numeric())

for (i in unique(ranks$hhid)) {
  ck<-c(i,cor(ranks[ranks$hhid==i,]$exp_rank, ranks[ranks$hhid==i,]$asset_rank_t, method = "spearman", use="pairwise.complete.obs"))
  expassets2<-rbind(expassets2,ck)
}
colnames(expassets2)<-c("hhid", "corr_exp")

needassets2<-data.frame(hhid=numeric(), corr_need=numeric())

for (i in unique(ranks$hhid)) {
  ck<-c(i,cor(ranks[ranks$hhid==i,]$need_rank, ranks[ranks$hhid==i,]$asset_rank_t, method = "spearman", use="pairwise.complete.obs"))
  needassets2<-rbind(needassets2,ck)
}
colnames(needassets2)<-c("hhid", "corr_need")

#Add enumeration area back in for cluster bootstrapping later
expassets2<-merge(expassets2, survey[, c("hhid", "enum_area")])
needassets2<-merge(needassets2, survey[, c("hhid", "enum_area")])



##3.3 Alternate Metrics Correlations
##Consumption

#AE Exp
ae_df<-data.frame(hhid=numeric(), corr_exp=numeric())

for (i in unique(ranks$hhid)) {
  ck<-c(i,cor(ranks[ranks$hhid==i,]$exp_rank, ranks[ranks$hhid==i,]$ae_rank_t, method = "spearman", use="pairwise.complete.obs"))
  ae_df<-rbind(ae_df,ck)
}
colnames(ae_df)<-c("hhid", "corr_exp")

#PC Cons
pc_cons_df<-data.frame(hhid=numeric(), corr_exp=numeric())

for (i in unique(ranks$hhid)) {
  ck<-c(i,cor(ranks[ranks$hhid==i,]$exp_rank, ranks[ranks$hhid==i,]$pc_cons_rank_t, method = "spearman", use="pairwise.complete.obs"))
  pc_cons_df<-rbind(pc_cons_df,ck)
}
colnames(pc_cons_df)<-c("hhid", "corr_exp")

#Pred Exp
pc_pred_exp_df<-data.frame(hhid=numeric(), corr_exp=numeric())

for (i in unique(ranks$hhid)) {
  ck<-c(i,cor(ranks[ranks$hhid==i,]$exp_rank, ranks[ranks$hhid==i,]$pc_pred_exp_rank_t, method = "spearman", use="pairwise.complete.obs"))
  pc_pred_exp_df<-rbind(pc_pred_exp_df,ck)
}
colnames(pc_pred_exp_df)<-c("hhid", "corr_exp")

ae_df<-merge(ae_df, survey[, c("hhid", "enum_area")])
pc_cons_df<-merge(pc_cons_df,survey[, c("hhid", "enum_area")])
pc_pred_exp_df<-merge(pc_pred_exp_df, survey[, c("hhid", "enum_area")])

#Neediness

alt_need_df<-data.frame(hhid=numeric(), corr_exp=numeric())

for (i in unique(ranks$hhid)) {
  ck<-c(i,cor(ranks[ranks$hhid==i,]$need_rank, ranks[ranks$hhid==i,]$alt_need_rank_t, method = "spearman", use="pairwise.complete.obs"))
  alt_need_df<-rbind(alt_need_df,ck)
}
colnames(alt_need_df)<-c("hhid", "corr_exp")
alt_need_df<-merge(alt_need_df, survey[, c("hhid", "enum_area")])

#Assets
ranks$land_val_dollar<-ranks$landval/4700

#land value
df5a<-data.frame(hhid=numeric(), corr_expa=numeric())

for (i in unique(ranks$hhid)) {
  ck<-c(i,cor(ranks[ranks$hhid==i,]$asset_rank, ranks[ranks$hhid==i,]$land_rank_t, method = "spearman", use="pairwise.complete.obs"))
  df5a<-rbind(df5a,ck)
}
colnames(df5a)<-c("hhid", "corr_expa")

#pc land value
pc_value_df<-data.frame(hhid=numeric(), corr_exp=numeric())

for (i in unique(ranks$hhid)) {
  ck<-c(i,cor(ranks[ranks$hhid==i,]$asset_rank, ranks[ranks$hhid==i,]$pc_value_rank_t, method = "spearman", use="pairwise.complete.obs"))
  pc_value_df<-rbind(pc_value_df,ck)
}
colnames(pc_value_df)<-c("hhid", "corr_exp")

#land area
area_df<-data.frame(hhid=numeric(), corr_exp=numeric())

for (i in unique(ranks$hhid)) {
  ck<-c(i,cor(ranks[ranks$hhid==i,]$asset_rank, ranks[ranks$hhid==i,]$area_rank_t, method = "spearman", use="pairwise.complete.obs"))
  area_df<-rbind(area_df,ck)
}
colnames(area_df)<-c("hhid", "corr_exp")

#asset count
no_assets_df<-data.frame(hhid=numeric(), corr_exp=numeric())

for (i in unique(ranks$hhid)) {
  ck<-c(i,cor(ranks[ranks$hhid==i,]$asset_rank, ranks[ranks$hhid==i,]$no_assets_rank_t, method = "spearman", use="pairwise.complete.obs"))
  no_assets_df<-rbind(no_assets_df,ck)
}
colnames(no_assets_df)<-c("hhid", "corr_exp")

#housing score
score_h_df<-data.frame(hhid=numeric(), corr_exp=numeric())

for (i in unique(ranks$hhid)) {
  ck<-c(i,cor(ranks[ranks$hhid==i,]$asset_rank, ranks[ranks$hhid==i,]$score_h_rank_t, method = "spearman", use="pairwise.complete.obs"))
  score_h_df<-rbind(score_h_df,ck)
}
colnames(score_h_df)<-c("hhid", "corr_exp")


df5a<-merge(df5a, survey[, c("hhid", "enum_area")])
pc_value_df<-merge(pc_value_df,survey[, c("hhid", "enum_area")])
area_df<-merge(area_df, survey[, c("hhid", "enum_area")])
no_assets_df<-merge(no_assets_df,survey[, c("hhid", "enum_area")])
score_h_df<-merge(score_h_df, survey[, c("hhid", "enum_area")])

##3.4 Shock and Benefit Report Matching
##Shocks and Benefits Matching
#Create variables of whether participant and survey shock reports match
shocks$shock_match<-ifelse(shocks$shock==shocks$t_shock, 1, 0)
shocks$death_head_match<-ifelse(shocks$death_head==shocks$t_death_head, 1, 0)
shocks$death_other_match<-ifelse(shocks$death_other==shocks$t_death_other, 1, 0)
shocks$illness_head_match<-ifelse(shocks$illness_head==shocks$t_illness_head, 1, 0)
shocks$illness_other_match<-ifelse(shocks$illness_other==shocks$t_illness_other, 1, 0)
shocks$employment_loss_match<-ifelse(shocks$employment_loss==shocks$t_employment_loss, 1, 0)
shocks$disaster_match<-ifelse(shocks$disaster==shocks$t_disaster, 1, 0)
shocks$harvest_fail_match<-ifelse(shocks$harvest_fail==shocks$t_harvest_fail, 1, 0)
shocks$harvest_small_match<-ifelse(shocks$harvest_small==shocks$t_harvest_small, 1, 0)
shocks$other_shock_match<-ifelse(shocks$other_shock==shocks$t_other_shock, 1, 0)
shocks$t_harvest<-pmax(shocks$t_harvest_fail, shocks$t_harvest_small)
shocks$harvest_match<-ifelse(pmax(shocks$harvest_fail, shocks$harvest_small)==pmax(shocks$t_harvest_fail, shocks$t_harvest_small), 1, 0)

covid$benmatch<-ifelse(covid$ben==covid$program2h, 1, 0)

##3.5 PMT/Community Aggregation Correlations
agg_score<-ranks%>%
  group_by(rankid)%>%
  summarize(enum_area=mean(enum_area), avg_targ = mean(sq_rank, na.rm=T), avg_exp = mean(exp_rank, na.rm=T), avg_need=mean(need_rank, na.rm=T), avg_asset=mean(asset_rank, na.rm=T), exp=mean(pc_food_exp), need=mean(loglambdas), asset=mean(score), ppi=mean(ppi), ps=mean(ps))
agg_score<-agg_score[which(agg_score$rankid%in%unique(survey$hhid)),]

agg_score$ag_targ<-NA
for (i in unique(agg_score$enum_area)){
  agg_score[agg_score$enum_area==i,]$ag_targ<-rank(agg_score[agg_score$enum_area==i,]$avg_targ, na.last = "keep")
}

agg_score$ag_targ<-as.numeric(agg_score$ag_targ)

agg_score$ag_exp<-NA
for (i in unique(agg_score$enum_area)){
  agg_score[agg_score$enum_area==i,]$ag_exp<-rank(agg_score[agg_score$enum_area==i,]$avg_exp, na.last = "keep")
}

agg_score$ag_exp<-as.numeric(agg_score$ag_exp)

agg_score$ag_need<-NA
for (i in unique(agg_score$enum_area)){
  agg_score[agg_score$enum_area==i,]$ag_need<-rank(agg_score[agg_score$enum_area==i,]$avg_need, na.last = "keep")
}

agg_score$ag_need<-as.numeric(agg_score$ag_need)

agg_score$ag_asset<-NA
for (i in unique(agg_score$enum_area)){
  agg_score[agg_score$enum_area==i,]$ag_asset<-rank(agg_score[agg_score$enum_area==i,]$avg_asset, na.last = "keep")
}

agg_score$ag_asset<-as.numeric(agg_score$ag_asset)

###

agg_score$agt_exp<-NA
for (i in unique(agg_score$enum_area)){
  agg_score[agg_score$enum_area==i,]$agt_exp<-rank(agg_score[agg_score$enum_area==i,]$exp, na.last = "keep")
}

agg_score$agt_exp<-as.numeric(agg_score$agt_exp)

agg_score$agt_need<-NA
for (i in unique(agg_score$enum_area)){
  agg_score[agg_score$enum_area==i,]$agt_need<-rank(-agg_score[agg_score$enum_area==i,]$need, na.last = "keep")
}

agg_score$agt_need<-as.numeric(agg_score$agt_need)

agg_score$agt_asset<-NA
for (i in unique(agg_score$enum_area)){
  agg_score[agg_score$enum_area==i,]$agt_asset<-rank(agg_score[agg_score$enum_area==i,]$asset, na.last = "keep")
}

agg_score$agt_asset<-as.numeric(agg_score$agt_asset)

##
agg_score$ag_ppi<-NA
for (i in unique(agg_score$enum_area)){
  agg_score[agg_score$enum_area==i,]$ag_ppi<-rank(agg_score[agg_score$enum_area==i,]$ppi, na.last = "keep")
}

agg_score$ag_ppi<-as.numeric(agg_score$ag_ppi)

agg_score$ag_ps<-NA
for (i in unique(agg_score$enum_area)){
  agg_score[agg_score$enum_area==i,]$ag_ps<-rank(agg_score[agg_score$enum_area==i,]$ps, na.last = "keep")
}


agg_score$ag_ps<-as.numeric(agg_score$ag_ps)

agg_score<-merge(agg_score, comrank, all.x=T)

colnames(agg_score)[21]<-"ag_meet"

agg_score$hybrid<-(agg_score$ag_meet+agg_score$ag_ppi)/2

agg_score$ag_hybrid<-NA
for (i in unique(agg_score$enum_area)){
  agg_score[agg_score$enum_area==i,]$ag_hybrid<-rank(agg_score[agg_score$enum_area==i,]$hybrid, na.last = "keep")
}

agg_score$cominc20_exp<-ifelse(agg_score$ag_exp<=6&agg_score$agt_exp>6, 1, 0)
agg_score$comexc20_exp<-ifelse(agg_score$ag_exp>6&agg_score$agt_exp<=6, 1, 0)
agg_score$comacc20_exp<-1-agg_score$cominc20_exp-agg_score$comexc20_exp

agg_score$cominc30_exp<-ifelse(agg_score$ag_exp<=9&agg_score$agt_exp>9, 1, 0)
agg_score$comexc30_exp<-ifelse(agg_score$ag_exp>9&agg_score$agt_exp<=9, 1, 0)
agg_score$comacc30_exp<-1-agg_score$cominc30_exp-agg_score$comexc30_exp

agg_score$cominc40_exp<-ifelse(agg_score$ag_exp<=12&agg_score$agt_exp>12, 1, 0)
agg_score$comexc40_exp<-ifelse(agg_score$ag_exp>12&agg_score$agt_exp<=12, 1, 0)
agg_score$comacc40_exp<-1-agg_score$cominc40_exp-agg_score$comexc40_exp

agg_score$cominc20_need<-ifelse(agg_score$ag_need<=6&agg_score$agt_need>6, 1, 0)
agg_score$comexc20_need<-ifelse(agg_score$ag_need>6&agg_score$agt_need<=6, 1, 0)
agg_score$comacc20_need<-1-agg_score$cominc20_need-agg_score$comexc20_need

agg_score$cominc30_need<-ifelse(agg_score$ag_need<=9&agg_score$agt_need>9, 1, 0)
agg_score$comexc30_need<-ifelse(agg_score$ag_need>9&agg_score$agt_need<=9, 1, 0)
agg_score$comacc30_need<-1-agg_score$cominc30_need-agg_score$comexc30_need

agg_score$cominc40_need<-ifelse(agg_score$ag_need<=12&agg_score$agt_need>12, 1, 0)
agg_score$comexc40_need<-ifelse(agg_score$ag_need>12&agg_score$agt_need<=12, 1, 0)
agg_score$comacc40_need<-1-agg_score$cominc40_need-agg_score$comexc40_need

agg_score$cominc20_asset<-ifelse(agg_score$ag_asset<=6&agg_score$agt_asset>6, 1, 0)
agg_score$comexc20_asset<-ifelse(agg_score$ag_asset>6&agg_score$agt_asset<=6, 1, 0)
agg_score$comacc20_asset<-1-agg_score$cominc20_asset-agg_score$comexc20_asset

agg_score$cominc30_asset<-ifelse(agg_score$ag_asset<=9&agg_score$agt_asset>9, 1, 0)
agg_score$comexc30_asset<-ifelse(agg_score$ag_asset>9&agg_score$agt_asset<=9, 1, 0)
agg_score$comacc30_asset<-1-agg_score$cominc30_asset-agg_score$comexc30_asset

agg_score$cominc40_asset<-ifelse(agg_score$ag_asset<=12&agg_score$agt_asset>12, 1, 0)
agg_score$comexc40_asset<-ifelse(agg_score$ag_asset>12&agg_score$agt_asset<=12, 1, 0)
agg_score$comacc40_asset<-1-agg_score$cominc40_asset-agg_score$comexc40_asset

#############################

agg_score$ppiinc20_exp<-ifelse(agg_score$ag_ppi<=6&agg_score$agt_exp>6, 1, 0)
agg_score$ppiexc20_exp<-ifelse(agg_score$ag_ppi>6&agg_score$agt_exp<=6, 1, 0)
agg_score$ppiacc20_exp<-1-agg_score$ppiinc20_exp-agg_score$ppiexc20_exp

agg_score$ppiinc30_exp<-ifelse(agg_score$ag_ppi<=9&agg_score$agt_exp>9, 1, 0)
agg_score$ppiexc30_exp<-ifelse(agg_score$ag_ppi>9&agg_score$agt_exp<=9, 1, 0)
agg_score$ppiacc30_exp<-1-agg_score$ppiinc30_exp-agg_score$ppiexc30_exp

agg_score$ppiinc40_exp<-ifelse(agg_score$ag_ppi<=12&agg_score$agt_exp>12, 1, 0)
agg_score$ppiexc40_exp<-ifelse(agg_score$ag_ppi>12&agg_score$agt_exp<=12, 1, 0)
agg_score$ppiacc40_exp<-1-agg_score$ppiinc40_exp-agg_score$ppiexc40_exp

agg_score$ppiinc20_need<-ifelse(agg_score$ag_ppi<=6&agg_score$agt_need>6, 1, 0)
agg_score$ppiexc20_need<-ifelse(agg_score$ag_ppi>6&agg_score$agt_need<=6, 1, 0)
agg_score$ppiacc20_need<-1-agg_score$ppiinc20_need-agg_score$ppiexc20_need

agg_score$ppiinc30_need<-ifelse(agg_score$ag_ppi<=9&agg_score$agt_need>9, 1, 0)
agg_score$ppiexc30_need<-ifelse(agg_score$ag_ppi>9&agg_score$agt_need<=9, 1, 0)
agg_score$ppiacc30_need<-1-agg_score$ppiinc30_need-agg_score$ppiexc30_need

agg_score$ppiinc40_need<-ifelse(agg_score$ag_ppi<=12&agg_score$agt_need>12, 1, 0)
agg_score$ppiexc40_need<-ifelse(agg_score$ag_ppi>12&agg_score$agt_need<=12, 1, 0)
agg_score$ppiacc40_need<-1-agg_score$ppiinc40_need-agg_score$ppiexc40_need

agg_score$ppiinc20_asset<-ifelse(agg_score$ag_ppi<=6&agg_score$agt_asset>6, 1, 0)
agg_score$ppiexc20_asset<-ifelse(agg_score$ag_ppi>6&agg_score$agt_asset<=6, 1, 0)
agg_score$ppiacc20_asset<-1-agg_score$ppiinc20_asset-agg_score$ppiexc20_asset

agg_score$ppiinc30_asset<-ifelse(agg_score$ag_ppi<=9&agg_score$agt_asset>9, 1, 0)
agg_score$ppiexc30_asset<-ifelse(agg_score$ag_ppi>9&agg_score$agt_asset<=9, 1, 0)
agg_score$ppiacc30_asset<-1-agg_score$ppiinc30_asset-agg_score$ppiexc30_asset

agg_score$ppiinc40_asset<-ifelse(agg_score$ag_ppi<=12&agg_score$agt_asset>12, 1, 0)
agg_score$ppiexc40_asset<-ifelse(agg_score$ag_ppi>12&agg_score$agt_asset<=12, 1, 0)
agg_score$ppiacc40_asset<-1-agg_score$ppiinc40_asset-agg_score$ppiexc40_asset

########

agg_score$psinc20_exp<-ifelse(agg_score$ag_ps<=6&agg_score$agt_exp>6, 1, 0)
agg_score$psexc20_exp<-ifelse(agg_score$ag_ps>6&agg_score$agt_exp<=6, 1, 0)
agg_score$psacc20_exp<-1-agg_score$psinc20_exp-agg_score$psexc20_exp

agg_score$psinc30_exp<-ifelse(agg_score$ag_ps<=9&agg_score$agt_exp>9, 1, 0)
agg_score$psexc30_exp<-ifelse(agg_score$ag_ps>9&agg_score$agt_exp<=9, 1, 0)
agg_score$psacc30_exp<-1-agg_score$psinc30_exp-agg_score$psexc30_exp

agg_score$psinc40_exp<-ifelse(agg_score$ag_ps<=12&agg_score$agt_exp>12, 1, 0)
agg_score$psexc40_exp<-ifelse(agg_score$ag_ps>12&agg_score$agt_exp<=12, 1, 0)
agg_score$psacc40_exp<-1-agg_score$psinc40_exp-agg_score$psexc40_exp

agg_score$psinc20_need<-ifelse(agg_score$ag_ps<=6&agg_score$agt_need>6, 1, 0)
agg_score$psexc20_need<-ifelse(agg_score$ag_ps>6&agg_score$agt_need<=6, 1, 0)
agg_score$psacc20_need<-1-agg_score$psinc20_need-agg_score$psexc20_need

agg_score$psinc30_need<-ifelse(agg_score$ag_ps<=9&agg_score$agt_need>9, 1, 0)
agg_score$psexc30_need<-ifelse(agg_score$ag_ps>9&agg_score$agt_need<=9, 1, 0)
agg_score$psacc30_need<-1-agg_score$psinc30_need-agg_score$psexc30_need

agg_score$psinc40_need<-ifelse(agg_score$ag_ps<=12&agg_score$agt_need>12, 1, 0)
agg_score$psexc40_need<-ifelse(agg_score$ag_ps>12&agg_score$agt_need<=12, 1, 0)
agg_score$psacc40_need<-1-agg_score$psinc40_need-agg_score$psexc40_need

agg_score$psinc20_asset<-ifelse(agg_score$ag_ps<=6&agg_score$agt_asset>6, 1, 0)
agg_score$psexc20_asset<-ifelse(agg_score$ag_ps>6&agg_score$agt_asset<=6, 1, 0)
agg_score$psacc20_asset<-1-agg_score$psinc20_asset-agg_score$psexc20_asset

agg_score$psinc30_asset<-ifelse(agg_score$ag_ps<=9&agg_score$agt_asset>9, 1, 0)
agg_score$psexc30_asset<-ifelse(agg_score$ag_ps>9&agg_score$agt_asset<=9, 1, 0)
agg_score$psacc30_asset<-1-agg_score$psinc30_asset-agg_score$psexc30_asset

agg_score$psinc40_asset<-ifelse(agg_score$ag_ps<=12&agg_score$agt_asset>12, 1, 0)
agg_score$psexc40_asset<-ifelse(agg_score$ag_ps>12&agg_score$agt_asset<=12, 1, 0)
agg_score$psacc40_asset<-1-agg_score$psinc40_asset-agg_score$psexc40_asset

########

agg_score$targinc20_exp<-ifelse(agg_score$ag_targ<=6&agg_score$agt_exp>6, 1, 0)
agg_score$targexc20_exp<-ifelse(agg_score$ag_targ>6&agg_score$agt_exp<=6, 1, 0)
agg_score$targacc20_exp<-1-agg_score$targinc20_exp-agg_score$targexc20_exp

agg_score$targinc30_exp<-ifelse(agg_score$ag_targ<=9&agg_score$agt_exp>9, 1, 0)
agg_score$targexc30_exp<-ifelse(agg_score$ag_targ>9&agg_score$agt_exp<=9, 1, 0)
agg_score$targacc30_exp<-1-agg_score$targinc30_exp-agg_score$targexc30_exp

agg_score$targinc40_exp<-ifelse(agg_score$ag_targ<=12&agg_score$agt_exp>12, 1, 0)
agg_score$targexc40_exp<-ifelse(agg_score$ag_targ>12&agg_score$agt_exp<=12, 1, 0)
agg_score$targacc40_exp<-1-agg_score$targinc40_exp-agg_score$targexc40_exp

agg_score$targinc20_need<-ifelse(agg_score$ag_targ<=6&agg_score$agt_need>6, 1, 0)
agg_score$targexc20_need<-ifelse(agg_score$ag_targ>6&agg_score$agt_need<=6, 1, 0)
agg_score$targacc20_need<-1-agg_score$targinc20_need-agg_score$targexc20_need

agg_score$targinc30_need<-ifelse(agg_score$ag_targ<=9&agg_score$agt_need>9, 1, 0)
agg_score$targexc30_need<-ifelse(agg_score$ag_targ>9&agg_score$agt_need<=9, 1, 0)
agg_score$targacc30_need<-1-agg_score$targinc30_need-agg_score$targexc30_need

agg_score$targinc40_need<-ifelse(agg_score$ag_targ<=12&agg_score$agt_need>12, 1, 0)
agg_score$targexc40_need<-ifelse(agg_score$ag_targ>12&agg_score$agt_need<=12, 1, 0)
agg_score$targacc40_need<-1-agg_score$targinc40_need-agg_score$targexc40_need

agg_score$targinc20_asset<-ifelse(agg_score$ag_targ<=6&agg_score$agt_asset>6, 1, 0)
agg_score$targexc20_asset<-ifelse(agg_score$ag_targ>6&agg_score$agt_asset<=6, 1, 0)
agg_score$targacc20_asset<-1-agg_score$targinc20_asset-agg_score$targexc20_asset

agg_score$targinc30_asset<-ifelse(agg_score$ag_targ<=9&agg_score$agt_asset>9, 1, 0)
agg_score$targexc30_asset<-ifelse(agg_score$ag_targ>9&agg_score$agt_asset<=9, 1, 0)
agg_score$targacc30_asset<-1-agg_score$targinc30_asset-agg_score$targexc30_asset

agg_score$targinc40_asset<-ifelse(agg_score$ag_targ<=12&agg_score$agt_asset>12, 1, 0)
agg_score$targexc40_asset<-ifelse(agg_score$ag_targ>12&agg_score$agt_asset<=12, 1, 0)
agg_score$targacc40_asset<-1-agg_score$targinc40_asset-agg_score$targexc40_asset

####

agg_score$meetinc20_exp<-ifelse(agg_score$ag_meet<=6&agg_score$agt_exp>6, 1, 0)
agg_score$meetexc20_exp<-ifelse(agg_score$ag_meet>6&agg_score$agt_exp<=6, 1, 0)
agg_score$meetacc20_exp<-1-agg_score$meetinc20_exp-agg_score$meetexc20_exp

agg_score$meetinc30_exp<-ifelse(agg_score$ag_meet<=9&agg_score$agt_exp>9, 1, 0)
agg_score$meetexc30_exp<-ifelse(agg_score$ag_meet>9&agg_score$agt_exp<=9, 1, 0)
agg_score$meetacc30_exp<-1-agg_score$meetinc30_exp-agg_score$meetexc30_exp

agg_score$meetinc40_exp<-ifelse(agg_score$ag_meet<=12&agg_score$agt_exp>12, 1, 0)
agg_score$meetexc40_exp<-ifelse(agg_score$ag_meet>12&agg_score$agt_exp<=12, 1, 0)
agg_score$meetacc40_exp<-1-agg_score$meetinc40_exp-agg_score$meetexc40_exp

agg_score$meetinc20_need<-ifelse(agg_score$ag_meet<=6&agg_score$agt_need>6, 1, 0)
agg_score$meetexc20_need<-ifelse(agg_score$ag_meet>6&agg_score$agt_need<=6, 1, 0)
agg_score$meetacc20_need<-1-agg_score$meetinc20_need-agg_score$meetexc20_need

agg_score$meetinc30_need<-ifelse(agg_score$ag_meet<=9&agg_score$agt_need>9, 1, 0)
agg_score$meetexc30_need<-ifelse(agg_score$ag_meet>9&agg_score$agt_need<=9, 1, 0)
agg_score$meetacc30_need<-1-agg_score$meetinc30_need-agg_score$meetexc30_need

agg_score$meetinc40_need<-ifelse(agg_score$ag_meet<=12&agg_score$agt_need>12, 1, 0)
agg_score$meetexc40_need<-ifelse(agg_score$ag_meet>12&agg_score$agt_need<=12, 1, 0)
agg_score$meetacc40_need<-1-agg_score$meetinc40_need-agg_score$meetexc40_need

agg_score$meetinc20_asset<-ifelse(agg_score$ag_meet<=6&agg_score$agt_asset>6, 1, 0)
agg_score$meetexc20_asset<-ifelse(agg_score$ag_meet>6&agg_score$agt_asset<=6, 1, 0)
agg_score$meetacc20_asset<-1-agg_score$meetinc20_asset-agg_score$meetexc20_asset

agg_score$meetinc30_asset<-ifelse(agg_score$ag_meet<=9&agg_score$agt_asset>9, 1, 0)
agg_score$meetexc30_asset<-ifelse(agg_score$ag_meet>9&agg_score$agt_asset<=9, 1, 0)
agg_score$meetacc30_asset<-1-agg_score$meetinc30_asset-agg_score$meetexc30_asset

agg_score$meetinc40_asset<-ifelse(agg_score$ag_meet<=12&agg_score$agt_asset>12, 1, 0)
agg_score$meetexc40_asset<-ifelse(agg_score$ag_meet>12&agg_score$agt_asset<=12, 1, 0)
agg_score$meetacc40_asset<-1-agg_score$meetinc40_asset-agg_score$meetexc40_asset


agg_score$hybridinc20_exp<-ifelse(agg_score$ag_hybrid<=6&agg_score$agt_exp>6, 1, 0)
agg_score$hybridexc20_exp<-ifelse(agg_score$ag_hybrid>6&agg_score$agt_exp<=6, 1, 0)
agg_score$hybridacc20_exp<-1-agg_score$hybridinc20_exp-agg_score$hybridexc20_exp

agg_score$hybridinc30_exp<-ifelse(agg_score$ag_hybrid<=9&agg_score$agt_exp>9, 1, 0)
agg_score$hybridexc30_exp<-ifelse(agg_score$ag_hybrid>9&agg_score$agt_exp<=9, 1, 0)
agg_score$hybridacc30_exp<-1-agg_score$hybridinc30_exp-agg_score$hybridexc30_exp

agg_score$hybridinc40_exp<-ifelse(agg_score$ag_hybrid<=12&agg_score$agt_exp>12, 1, 0)
agg_score$hybridexc40_exp<-ifelse(agg_score$ag_hybrid>12&agg_score$agt_exp<=12, 1, 0)
agg_score$hybridacc40_exp<-1-agg_score$hybridinc40_exp-agg_score$hybridexc40_exp

agg_score$hybridinc20_need<-ifelse(agg_score$ag_hybrid<=6&agg_score$agt_need>6, 1, 0)
agg_score$hybridexc20_need<-ifelse(agg_score$ag_hybrid>6&agg_score$agt_need<=6, 1, 0)
agg_score$hybridacc20_need<-1-agg_score$hybridinc20_need-agg_score$hybridexc20_need

agg_score$hybridinc30_need<-ifelse(agg_score$ag_hybrid<=9&agg_score$agt_need>9, 1, 0)
agg_score$hybridexc30_need<-ifelse(agg_score$ag_hybrid>9&agg_score$agt_need<=9, 1, 0)
agg_score$hybridacc30_need<-1-agg_score$hybridinc30_need-agg_score$hybridexc30_need

agg_score$hybridinc40_need<-ifelse(agg_score$ag_hybrid<=12&agg_score$agt_need>12, 1, 0)
agg_score$hybridexc40_need<-ifelse(agg_score$ag_hybrid>12&agg_score$agt_need<=12, 1, 0)
agg_score$hybridacc40_need<-1-agg_score$hybridinc40_need-agg_score$hybridexc40_need

agg_score$hybridinc20_asset<-ifelse(agg_score$ag_hybrid<=6&agg_score$agt_asset>6, 1, 0)
agg_score$hybridexc20_asset<-ifelse(agg_score$ag_hybrid>6&agg_score$agt_asset<=6, 1, 0)
agg_score$hybridacc20_asset<-1-agg_score$hybridinc20_asset-agg_score$hybridexc20_asset

agg_score$hybridinc30_asset<-ifelse(agg_score$ag_hybrid<=9&agg_score$agt_asset>9, 1, 0)
agg_score$hybridexc30_asset<-ifelse(agg_score$ag_hybrid>9&agg_score$agt_asset<=9, 1, 0)
agg_score$hybridacc30_asset<-1-agg_score$hybridinc30_asset-agg_score$hybridexc30_asset

agg_score$hybridinc40_asset<-ifelse(agg_score$ag_hybrid<=12&agg_score$agt_asset>12, 1, 0)
agg_score$hybridexc40_asset<-ifelse(agg_score$ag_hybrid>12&agg_score$agt_asset<=12, 1, 0)
agg_score$hybridacc40_asset<-1-agg_score$hybridinc40_asset-agg_score$hybridexc40_asset

##3.6 Correlations between targeting and info ranks

targexp<-data.frame(hhid=numeric(), corr=numeric())

for (i in unique(ranks$hhid)) {
  ck<-c(i,cor(ranks[ranks$hhid==i,]$exp_rank, ranks[ranks$hhid==i,]$sq_rank, method = "spearman", use="pairwise.complete.obs"))
  targexp<-rbind(targexp,ck)
}
colnames(targexp)<-c("hhid", "corr")


targneed<-data.frame(hhid=numeric(), corr=numeric())

for (i in unique(ranks$hhid)) {
  ck<-c(i,cor(ranks[ranks$hhid==i,]$need_rank, ranks[ranks$hhid==i,]$sq_rank, method = "spearman", use="pairwise.complete.obs"))
  targneed<-rbind(targneed,ck)
}
colnames(targneed)<-c("hhid", "corr")

targasset<-data.frame(hhid=numeric(), corr=numeric())

for (i in unique(ranks$hhid)) {
  ck<-c(i,cor(ranks[ranks$hhid==i,]$asset_rank, ranks[ranks$hhid==i,]$sq_rank, method = "spearman", use="pairwise.complete.obs"))
  targasset<-rbind(targasset,ck)
}
colnames(targasset)<-c("hhid", "corr")

##well-known only
targexp2<-data.frame(hhid=numeric(), corr=numeric())

for (i in unique(temp$hhid)) {
  ck<-c(i,cor(temp[temp$hhid==i,]$exp_rank, temp[temp$hhid==i,]$sq_rank, method = "spearman", use="pairwise.complete.obs"))
  targexp2<-rbind(targexp2,ck)
}
colnames(targexp2)<-c("hhid", "corr")


targneed2<-data.frame(hhid=numeric(), corr=numeric())

for (i in unique(temp$hhid)) {
  ck<-c(i,cor(temp[temp$hhid==i,]$need_rank, temp[temp$hhid==i,]$sq_rank, method = "spearman", use="pairwise.complete.obs"))
  targneed2<-rbind(targneed2,ck)
}
colnames(targneed2)<-c("hhid", "corr")

targasset2<-data.frame(hhid=numeric(), corr=numeric())

for (i in unique(temp$hhid)) {
  ck<-c(i,cor(temp[temp$hhid==i,]$asset_rank, temp[temp$hhid==i,]$sq_rank, method = "spearman", use="pairwise.complete.obs"))
  targasset2<-rbind(targasset2,ck)
}
colnames(targasset2)<-c("hhid", "corr")

##Merge enum area back for bootstap se
targexp<-merge(targexp, survey[, c("hhid", "enum_area")])
targexp2<-merge(targexp2, survey[, c("hhid", "enum_area")])
targneed<-merge(targneed, survey[, c("hhid", "enum_area")])
targneed2<-merge(targneed2, survey[, c("hhid", "enum_area")])
targasset<-merge(targasset, survey[, c("hhid", "enum_area")])
targasset2<-merge(targasset2, survey[, c("hhid", "enum_area")])

#3.7 Correlations Between Preference Ranks/Second order belief ranks and Info Ranks

#Preference Ranks
prefsq<-data.frame(hhid=numeric(), corr=numeric())

for (i in unique(ranks$hhid)) {
  ck<-c(i,cor(ranks[ranks$hhid==i,]$pref_rank, ranks[ranks$hhid==i,]$sq_rank, method = "spearman", use="pairwise.complete.obs"))
  prefsq<-rbind(prefsq,ck)
}
colnames(prefsq)<-c("hhid", "corr")

prefexp<-data.frame(hhid=numeric(), corr=numeric())

for (i in unique(ranks$hhid)) {
  ck<-c(i,cor(ranks[ranks$hhid==i,]$pref_rank, ranks[ranks$hhid==i,]$exp_rank, method = "spearman", use="pairwise.complete.obs"))
  prefexp<-rbind(prefexp,ck)
}
colnames(prefexp)<-c("hhid", "corr")

prefneed<-data.frame(hhid=numeric(), corr=numeric())

for (i in unique(ranks$hhid)) {
  ck<-c(i,cor(ranks[ranks$hhid==i,]$pref_rank, ranks[ranks$hhid==i,]$need_rank, method = "spearman", use="pairwise.complete.obs"))
  prefneed<-rbind(prefneed,ck)
}
colnames(prefneed)<-c("hhid", "corr")

prefasset<-data.frame(hhid=numeric(), corr=numeric())

for (i in unique(ranks$hhid)) {
  ck<-c(i,cor(ranks[ranks$hhid==i,]$pref_rank, ranks[ranks$hhid==i,]$asset_rank, method = "spearman", use="pairwise.complete.obs"))
  prefasset<-rbind(prefasset,ck)
}
colnames(prefasset)<-c("hhid", "corr")


#Second Order Beliefs
sopsq<-data.frame(hhid=numeric(), corr=numeric())

for (i in unique(ranks$hhid)) {
  ck<-c(i,cor(ranks[ranks$hhid==i,]$sop_rank, ranks[ranks$hhid==i,]$sq_rank, method = "spearman", use="pairwise.complete.obs"))
  sopsq<-rbind(sopsq,ck)
}
colnames(sopsq)<-c("hhid", "corr")

sopexp<-data.frame(hhid=numeric(), corr=numeric())

for (i in unique(ranks$hhid)) {
  ck<-c(i,cor(ranks[ranks$hhid==i,]$sop_rank, ranks[ranks$hhid==i,]$exp_rank, method = "spearman", use="pairwise.complete.obs"))
  sopexp<-rbind(sopexp,ck)
}
colnames(sopexp)<-c("hhid", "corr")

sopneed<-data.frame(hhid=numeric(), corr=numeric())

for (i in unique(ranks$hhid)) {
  ck<-c(i,cor(ranks[ranks$hhid==i,]$sop_rank, ranks[ranks$hhid==i,]$need_rank, method = "spearman", use="pairwise.complete.obs"))
  sopneed<-rbind(sopneed,ck)
}
colnames(sopneed)<-c("hhid", "corr")

sopasset<-data.frame(hhid=numeric(), corr=numeric())

for (i in unique(ranks$hhid)) {
  ck<-c(i,cor(ranks[ranks$hhid==i,]$sop_rank, ranks[ranks$hhid==i,]$asset_rank, method = "spearman", use="pairwise.complete.obs"))
  sopasset<-rbind(sopasset,ck)
}
colnames(sopasset)<-c("hhid", "corr")


prefsq<-merge(prefsq, survey[, c("hhid", "enum_area")])
prefexp<-merge(prefexp,survey[, c("hhid", "enum_area")])
prefneed<-merge(prefneed, survey[, c("hhid", "enum_area")])
prefasset<-merge(prefasset, survey[, c("hhid", "enum_area")])

sopsq<-merge(sopsq, survey[, c("hhid", "enum_area")])
sopexp<-merge(sopexp, survey[, c("hhid", "enum_area")])
sopneed<-merge(sopneed, survey[, c("hhid", "enum_area")])
sopasset<-merge(sopasset, survey[, c("hhid", "enum_area")])

#3.8 Percentage of the 2, 3, and 4 poorest identified
###Bottom 2 correctness
ranks_2exp<-ranks[which(ranks$exp_rank_t<3),]
ranks_2exp$correct<-ifelse(ranks_2exp$exp_rank<=2, 1, 0)

ranks_3exp<-ranks[which(ranks$exp_rank_t<4),]
ranks_3exp$correct<-ifelse(ranks_3exp$exp_rank<=3, 1, 0)

ranks_4exp<-ranks[which(ranks$exp_rank_t<5),]
ranks_4exp$correct<-ifelse(ranks_4exp$exp_rank<=4, 1, 0)

#
ranks_2need<-ranks[which(ranks$need_rank_t<3),]
ranks_2need$correct<-ifelse(ranks_2need$need_rank<=2, 1, 0)


ranks_3need<-ranks[which(ranks$need_rank_t<4),]
ranks_3need$correct<-ifelse(ranks_3need$need_rank<=3, 1, 0)


ranks_4need<-ranks[which(ranks$need_rank_t<5),]
ranks_4need$correct<-ifelse(ranks_4need$need_rank<=4, 1, 0)

#
ranks_2asset<-ranks[which(ranks$asset_rank_t<3),]
ranks_2asset$correct<-ifelse(ranks_2asset$asset_rank<=2, 1, 0)


ranks_3asset<-ranks[which(ranks$asset_rank_t<4),]
ranks_3asset$correct<-ifelse(ranks_3asset$asset_rank<=3, 1, 0)


ranks_4asset<-ranks[which(ranks$asset_rank_t<5),]
ranks_4asset$correct<-ifelse(ranks_4asset$asset_rank<=4, 1, 0)

##3.9 Correlation Between baseline and followup rounds
dfcheck1<-data.frame(hhid=numeric(), corr_expa=numeric())
for (i in unique(franks$hhid)) {
  ck<-c(i,cor(franks[franks$hhid==i,]$exp_rank, franks[franks$hhid==i,]$exp_rank_r1, method = "spearman", use="pairwise.complete.obs"))
  dfcheck1<-rbind(dfcheck1,ck)
}
colnames(dfcheck1)<-c("hhid", "corr_expa")

dfcheck2<-data.frame(hhid=numeric(), corr_expa=numeric())
for (i in unique(franks$hhid)) {
  ck<-c(i,cor(franks[franks$hhid==i,]$need_rank, franks[franks$hhid==i,]$need_rank_r1, method = "spearman", use="pairwise.complete.obs"))
  dfcheck2<-rbind(dfcheck2,ck)
}
colnames(dfcheck2)<-c("hhid", "corr_expa")


dfcheck3<-data.frame(hhid=numeric(), corr_expa=numeric())
for (i in unique(franks$hhid)) {
  ck<-c(i,cor(franks[franks$hhid==i,]$asset_rank, franks[franks$hhid==i,]$asset_rank_r1, method = "spearman", use="pairwise.complete.obs"))
  dfcheck3<-rbind(dfcheck3,ck)
}
colnames(dfcheck3)<-c("hhid", "corr_expa")



df3f<-data.frame(hhid=numeric(), corr_expa=numeric())

for (i in unique(franks$hhid)) {
  ck<-c(i,cor(franks[franks$hhid==i,]$exp_rank_t, franks[franks$hhid==i,]$exp_rankf, method = "spearman", use="pairwise.complete.obs"))
  df3f<-rbind(df3f,ck)
}
colnames(df3f)<-c("hhid", "corr_expa")

df4f<-data.frame(hhid=numeric(), corr_expa=numeric())

for (i in unique(franks$hhid)) {
  ck<-c(i,cor(franks[franks$hhid==i,]$need_rank_t, franks[franks$hhid==i,]$need_rankf, method = "spearman", use="pairwise.complete.obs"))
  df4f<-rbind(df4f,ck)
}
colnames(df4f)<-c("hhid", "corr_expa")

df5f<-data.frame(hhid=numeric(), corr_expa=numeric())

for (i in unique(franks$hhid)) {
  ck<-c(i,cor(franks[franks$hhid==i,]$asset_rank_t, franks[franks$hhid==i,]$asset_rankf, method = "spearman", use="pairwise.complete.obs"))
  df5f<-rbind(df5f,ck)
}
colnames(df5f)<-c("hhid", "corr_expa")

#merge enum area back in for SEs
dfcheck1<-merge(dfcheck1, survey[, c("hhid", "enum_area")])
dfcheck2<-merge(dfcheck2, survey[, c("hhid", "enum_area")])
dfcheck3<-merge(dfcheck3, survey[, c("hhid", "enum_area")])

df3f<-merge(df3f, survey[, c("hhid", "enum_area")])
df4f<-merge(df4f, survey[, c("hhid", "enum_area")])
df5f<-merge(df5f, survey[, c("hhid", "enum_area")])

#3.10 Follow-up Survey/Participant Correlations of Same Metric

##Baseline survey/participant correlations of communities surveyed at follow up

ranks_f<-ranks[ranks$enum_area%in%c(1,2,7),]
ranks_f<-ranks_f[which(ranks_f$hhid!=218),]
df_1<-data.frame(hhid=numeric(), corr_expa=numeric())
for (i in unique(ranks_f$hhid)) {
  ck<-c(i,cor(ranks_f[ranks_f$hhid==i,]$exp_rank, ranks_f[ranks_f$hhid==i,]$exp_rank_t, method = "spearman", use="pairwise.complete.obs"))
  df_1<-rbind(df_1,ck)
}
colnames(df_1)<-c("hhid", "corr_expa")

df_2<-data.frame(hhid=numeric(), corr_expa=numeric())
for (i in unique(ranks_f$hhid)) {
  ck<-c(i,cor(ranks_f[ranks_f$hhid==i,]$need_rank, ranks_f[ranks_f$hhid==i,]$need_rank_t, method = "spearman", use="pairwise.complete.obs"))
  df_2<-rbind(df_2,ck)
}
colnames(df_2)<-c("hhid", "corr_expa")


df_3<-data.frame(hhid=numeric(), corr_expa=numeric())
for (i in unique(ranks_f$hhid)) {
  ck<-c(i,cor(ranks_f[ranks_f$hhid==i,]$asset_rank, ranks_f[ranks_f$hhid==i,]$asset_rank_t, method = "spearman", use="pairwise.complete.obs"))
  df_3<-rbind(df_3,ck)
}
colnames(df_3)<-c("hhid", "corr_expa")

##Follow up

df3f<-data.frame(hhid=numeric(), corr_expa=numeric())
for (i in unique(franks$hhid)) {
  ck<-c(i,cor(franks[franks$hhid==i,]$exp_rank, franks[franks$hhid==i,]$exp_rank_t, method = "spearman", use="pairwise.complete.obs"))
  df3f<-rbind(df3f,ck)
}
colnames(df3f)<-c("hhid", "corr_expa")


df4f<-data.frame(hhid=numeric(), corr_expa=numeric())
for (i in unique(franks$hhid)) {
  ck<-c(i,cor(franks[franks$hhid==i,]$need_rank, franks[franks$hhid==i,]$need_rank_t, method = "spearman", use="pairwise.complete.obs"))
  df4f<-rbind(df4f,ck)
}
colnames(df4f)<-c("hhid", "corr_expa")

df5f<-data.frame(hhid=numeric(), corr_expa=numeric())
for (i in unique(franks$hhid)) {
  ck<-c(i,cor(franks[franks$hhid==i,]$asset_rank, franks[franks$hhid==i,]$asset_rank_t, method = "spearman", use="pairwise.complete.obs"))
  df5f<-rbind(df5f,ck)
}
colnames(df5f)<-c("hhid", "corr_expa")

df_1<-merge(df_1, survey[, c("hhid", "enum_area")])
df_2<-merge(df_2, survey[, c("hhid", "enum_area")])
df_3<-merge(df_3, survey[, c("hhid", "enum_area")])

df3f<-merge(df3f, survey[, c("hhid", "enum_area")])
df4f<-merge(df4f, survey[, c("hhid", "enum_area")])
df5f<-merge(df5f, survey[, c("hhid", "enum_area")])

#4.Regression Estimation#############
set.seed(102936)

#Change in participant rank vs. change in survey ranks 
ax<-felm(dr_exp~drt_exp|0|0|hhid, data=franks)
bx<-felm(dr_need~drt_need|0|0|hhid, data=franks)
cx<-felm(dr_asset~drt_asset|0|0|hhid, data=franks)
##Cluster Bootstrapped SEs
ax_cl<-clusbootglm(dr_exp~drt_exp,franks, hhid)
bx_cl<-clusbootglm(dr_need~drt_need,franks, hhid)
cx_cl<-clusbootglm(dr_asset~drt_asset,franks, hhid)


#Change in participant ranks vs. change in survey values
ax1<-felm(dr_exp~dexp|0|0|hhid, data=franks)
bx1<-felm(dr_need~dneed|0|0|hhid, data=franks)
cx1<-felm(dr_asset~dasset|0|0|hhid, data=franks)
##Cluster Bootstrapped SEs
ax_cl1<-clusbootglm(dr_exp~dexp,franks, hhid)
bx_cl1<-clusbootglm(dr_need~dneed,franks, hhid)
cx_cl1<-clusbootglm(dr_asset~dasset,franks, hhid)

##Targeting Rank regressed on participant info ranks
ranks_reg<-ranks[!is.na(ranks$asset_rank),] ##need to remove manually to match residuals to observations
reg_1<-felm(sq_rank~exp_rank+need_rank+asset_rank|0|0|hhid, data=ranks_reg)
reg_2a<-felm(sq_rank~asset_rank|0|0|hhid, data=ranks_reg)
ranks_reg$resid<-reg_2a$residuals
reg_2<-felm(resid~exp_rank+need_rank|0|0|hhid, data=ranks_reg)

reg1se<-clusbootglm(sq_rank~exp_rank+need_rank+asset_rank,ranks, hhid)

reg2se<-clusbootglm(resid~exp_rank+need_rank,ranks_reg, hhid)

#Merged in shocks data
stest<-merge(ranks_reg, shocks)
st2<-stest[!is.na(stest$asset_rank)&!is.na(stest$shock),]
reg_3<-lm(sq_rank~asset_rank+shock, data=st2)
reg_4<-lm(resid~shock, data=st2)

reg3se<-Boot(reg_3, f=coef, R=5000)
reg4se<-Boot(reg_4, f=coef, R=5000)
reg3se_vec<-c(sd(reg3se$t[,1]), sd(reg3se$t[,2]), sd(reg3se$t[,3]))
reg4se_vec<-c(sd(reg4se$t[,1]), sd(reg4se$t[,2]))

#Ranking accuracy vs. familiarity with other participants
ta<-felm(d_exp~fam_percent+fam_percent_r+times_ranked+times_ranked_r|0|0|hhid, data=ranks)
tb<-felm(d_need~fam_percent+fam_percent_r+times_ranked+times_ranked_r|0|0|hhid, data=ranks)
tc<-felm(d_asset~fam_percent+fam_percent_r+times_ranked+times_ranked_r|0|0|hhid, data=ranks)

regta<-clusbootglm(d_exp~fam_percent+fam_percent_r+times_ranked+times_ranked_r,ranks, hhid)
regtb<-clusbootglm(d_need~fam_percent+fam_percent_r+times_ranked+times_ranked_r,ranks, hhid)
regtc<-clusbootglm(d_asset~fam_percent+fam_percent_r+times_ranked+times_ranked_r,ranks, hhid)

ta1<-felm(d_exp~n_know|0|0|hhid, data=ranks)
tb1<-felm(d_need~n_know|0|0|hhid, data=ranks)
tc1<-felm(d_asset~n_know|0|0|hhid, data=ranks)


regta1<-clusbootglm(d_exp~n_know, ranks, hhid)
regtb1<-clusbootglm(d_need~n_know, ranks, hhid)
regtc1<-clusbootglm(d_asset~n_know, ranks, hhid)

#Explaining changes in rankings between round
newreg1<-felm(exp_rank~exp_rank_r1+drt_exp+comm_rank_hh|0|0|hhid, data=franks)
newreg2<-felm(need_rank~need_rank_r1+drt_need+comm_rank_hh|0|0|hhid, data=franks)
newreg3<-felm(asset_rank~asset_rank_r1+drt_asset+comm_rank_hh|0|0|hhid, data=franks)
newreg4<-felm(exp_rank~exp_rank_r1+exp_rank_t+comm_rank_hh|0|0|hhid, data=franks)
newreg5<-felm(need_rank~need_rank_r1+need_rank_t+comm_rank_hh|0|0|hhid, data=franks)
newreg6<-felm(asset_rank~asset_rank_r1+asset_rank_t+comm_rank_hh|0|0|hhid, data=franks)
##Cluster Bootstrapped SEs
se1<-clusbootglm(exp_rank~exp_rank_r1+drt_exp+comm_rank_hh,franks, hhid)
se2<-clusbootglm(need_rank~need_rank_r1+drt_need+comm_rank_hh,franks, hhid)
se3<-clusbootglm(asset_rank~asset_rank_r1+drt_asset+comm_rank_hh,franks, hhid)
se4<-clusbootglm(exp_rank~exp_rank_r1+exp_rank_t+comm_rank_hh,franks, hhid)
se5<-clusbootglm(need_rank~need_rank_r1+need_rank_t+comm_rank_hh,franks, hhid)
se6<-clusbootglm(asset_rank~asset_rank_r1+asset_rank_t+comm_rank_hh,franks, hhid)

#Looking at systematic diffs in ranks for family/close friends, those known well
ranks_reg<-ranks[!is.na(ranks$need_rank)&!is.na(ranks$asset_rank),]
ranks_reg2<-ranks[!is.na(ranks$need_rank_t)&!is.na(ranks$asset_rank_t)&!is.na(ranks$exp_rank_t),]
reg_a<-felm(sq_rank~ff|0|0|hhid, data=ranks)
reg_b<-felm(sq_rank~ff+exp_rank+need_rank+asset_rank|0|0|hhid, data=ranks_reg)
reg_c<-felm(sq_rank~ff+exp_rank_t+need_rank_t+asset_rank_t|0|0|hhid, data=ranks_reg2)
reg_d<-felm(sq_rank~well|0|0|hhid, data=ranks)
reg_e<-felm(sq_rank~well+exp_rank+need_rank+asset_rank|0|0|hhid, data=ranks_reg)
reg_f<-felm(sq_rank~well+exp_rank_t+need_rank_t+asset_rank_t|0|0|hhid, data=ranks_reg2)
##Cluster SE
reg_a1<-clusbootglm(sq_rank~ff,ranks, hhid)
reg_b1<-clusbootglm(sq_rank~ff+exp_rank+need_rank+asset_rank, ranks_reg, hhid)
reg_c1<-clusbootglm(sq_rank~ff+exp_rank_t+need_rank_t+asset_rank_t, ranks_reg2, hhid)
reg_d1<-clusbootglm(sq_rank~well,ranks, hhid)
reg_e1<-clusbootglm(sq_rank~well+exp_rank+need_rank+asset_rank, ranks_reg, hhid)
reg_f1<-clusbootglm(sq_rank~well+exp_rank_t+need_rank_t+asset_rank_t, ranks_reg2, hhid)

#5.Main Tables###################

##Table 1
stargazer(zs2[, c(7:38)],summary.stat = c( "mean", "sd", "median", "min", "max", "n"), title="Sample Summary Statistics")

##Table 2
#Note: bootstrap SEs will take a moment to run
r1<-c(mean(df3$corr_exp, na.rm=T), mean(df4$corr_expn, na.rm=T), mean(df5$corr_expa, na.rm=T))
r2<-c(clbootse("corr_exp", df3), clbootse("corr_expn", df4), clbootse("corr_expa", df5))
r3<-c(nrow(df3[!is.na(df3$corr_exp),]), nrow(df4[!is.na(df4$corr_expn),]), nrow(df5[!is.na(df5$corr_expa),]))

r4<-c(mean(df3a$corr_exp, na.rm=T), mean(df4a$corr_expn, na.rm=T), mean(df5aa$corr_expa, na.rm=T))
r5<-c(clbootse("corr_exp", df3a), clbootse("corr_expn", df4a), clbootse("corr_expa", df5aa))
r6<-c(nrow(df3a[!is.na(df3a$corr_exp),]), nrow(df4a[!is.na(df4a$corr_expn),]), nrow(df5aa[!is.na(df5aa$corr_expa),]))

rows<-data.frame(rbind(r1, r2, r3, r4, r5, r6))
colnames(rows)<-c("Exp", "IMUE", "Asset")
rows[c(1, 4),]$Exp<-round(as.numeric(rows[c(1, 4),]$Exp), digits = 3)
rows[c(1, 4),]$IMUE<-round(as.numeric(rows[c(1, 4),]$IMUE), digits = 3)
rows[c(1, 4),]$Asset<-round(as.numeric(rows[c(1, 4),]$Asset), digits = 3)


stargazer(rows, summary=F)

#Tests for differences of means
bootstrap_simp("corr_exp", "corr_expn", df3, df4)
bootstrap_simp("corr_exp", "corr_expa", df3, df5)
bootstrap_simp("corr_expn", "corr_expa", df4, df5)

bootstrap_simp("corr_exp", "corr_expn", df3a, df4a)
bootstrap_simp("corr_exp", "corr_expa", df3a, df5aa)
bootstrap_simp("corr_expn", "corr_expa", df4a, df5aa)


bootstrap_simp("corr_exp", "corr_exp", df3, df3a)
bootstrap_simp("corr_expn", "corr_expn", df4, df4a)
bootstrap_simp("corr_expa", "corr_expa", df5, df5aa)

##Table 3
r1<-c(1, mean(expneed$corr_exp, na.rm=T), mean(expasset$corr_exp, na.rm=T), 1, mean(expneeds$corr_exp, na.rm=T), mean(expassets$corr_exp, na.rm=T))
r2<-c("", 1, mean(needasset$corr_exp, na.rm=T), "", 1, mean(needassets$corr_exp, na.rm=T))
r3<-c("","" , 1, "", "", 1)

rows<-data.frame(rbind(r1, r2, r3))
colnames(rows)<-c( "Exp", "IMUE", "Asset",  "Exp", "IMUE", "Asset")
stargazer(rows, summary = F)

bootstrap_simp("corr_exp", "corr_exp", expneed, expneeds)
bootstrap_simp("corr_exp", "corr_exp", expasset, expassets)
bootstrap_simp("corr_exp", "corr_exp", needasset, needassets)

##Table 4

r1<-c(mean(df3$corr_exp, na.rm=T), mean(expassets2$corr_exp, na.rm=T))
r2<-c(clbootse("corr_exp", df3), clbootse("corr_exp", expassets2))
r3<-c(mean(df4$corr_expn, na.rm=T), mean(needassets2$corr_need, na.rm=T))
r4<-c(clbootse("corr_expn", df4), clbootse("corr_need", needassets2))

rows<-data.frame(rbind(r1, r2, r3, r4))
colnames(rows)<-c("Survey Benchmark", "Survey Asset Index")
stargazer(rows, summary = F)

bootstrap_simp("corr_exp", "corr_exp", df3, expassets2)
bootstrap_simp("corr_expn", "corr_need", df4, needassets2)

##Table 5
r1<-c("Any Shock",mean(shocks$t_shock, na.rm=T), mean(shocks$shock, na.rm = T) ,mean(shocks[which(shocks$t_shock==1),]$shock_match, na.rm = T), mean(shocks$shock_match, na.rm = T))
r2<-c("HH Head Death",mean(shocks$t_death_head, na.rm=T), mean(shocks$death_head, na.rm = T), "NA", mean(shocks$death_head_match, na.rm = T))
r3<-c("Other HH Member Death",mean(shocks$t_death_other, na.rm=T), mean(shocks$death_other, na.rm = T), mean(shocks[which(shocks$t_death_other==1),]$death_other_match, na.rm = T), mean(shocks$death_other_match, na.rm = T))
r4<-c("HH Head Grave Illness",mean(shocks$t_illness_head, na.rm=T), mean(shocks$illness_head, na.rm = T), mean(shocks[which(shocks$t_illness_head==1),]$illness_head_match, na.rm = T), mean(shocks$illness_head_match, na.rm = T))
r5<-c("Other HH Member Grave Illness",mean(shocks$t_illness_other, na.rm=T), mean(shocks$illness_other, na.rm = T), mean(shocks[which(shocks$t_illness_other==1),]$illness_other_match, na.rm = T), mean(shocks$illness_other_match, na.rm = T))
r6<-c("Employment Loss",mean(shocks$t_employment_loss, na.rm=T), mean(shocks$employment_loss, na.rm = T), mean(shocks[which(shocks$t_employment_loss==1),]$employment_loss_match, na.rm = T), mean(shocks$employment_loss_match, na.rm = T))
r7<-c("Fire/Earthquake",mean(shocks$t_disaster, na.rm=T), mean(shocks$disaster, na.rm = T), "NA", mean(shocks$disaster_match, na.rm = T))
r8<-c("Harvest Failure",mean(shocks$t_harvest_fail, na.rm=T), mean(shocks$harvest_fail, na.rm = T), mean(shocks[which(shocks$t_harvest_fail==1),]$harvest_fail_match, na.rm = T), mean(shocks$harvest_fail_match, na.rm = T))
r9<-c("Small Harvest",mean(shocks$t_harvest_small, na.rm=T), mean(shocks$harvest_small, na.rm = T), mean(shocks[which(shocks$t_harvest_small==1),]$harvest_small_match, na.rm = T), mean(shocks$harvest_small_match, na.rm = T))
r10<-c("Other Shock",mean(shocks$t_other_shock, na.rm=T), mean(shocks$other_shock, na.rm = T), mean(shocks[which(shocks$t_other_shock==1),]$other_shock_match, na.rm = T), mean(shocks$other_shock_match, na.rm = T))
r11<-c("COVID Benefits",mean(covid$program2h, na.rm=T), mean(covid$ben, na.rm = T), mean(covid[which(covid$program2h==1),]$benmatch, na.rm=T), mean(covid$benmatch, na.rm = T))


rows<-data.frame(rbind(r1, r2, r3, r4, r5, r6, r7, r8, r9, r10, r11))
colnames(rows)<-c("Variable", "Self", "Neighbor", "%Match (Shock)", "%Match (All)")
rows$Self<-round(100*as.numeric(rows$Self), digits = 1)
rows$Neighbor<-round(100*as.numeric(rows$Neighbor), digits = 1)
rows$`%Match (All)`<-round(100*as.numeric(rows$`%Match (All)`), digits = 1)
rows$`%Match (Shock)`<-round(100*as.numeric(rows$`%Match (Shock)`), digits = 1)

stargazer(rows, summary = F)

##Table 6
stargazer(ax, bx, cx, ax1, bx1, cx1, se=list(ax_cl$boot.sds, bx_cl$boot.sds, cx_cl$boot.sds, ax_cl1$boot.sds, bx_cl1$boot.sds, cx_cl1$boot.sds))


##Table 7

r1<-c(mean(targexp$corr, na.rm=T), mean(targneed$corr, na.rm=T), mean(targasset$corr, na.rm=T))
r2<-c(clbootse("corr", targexp), clbootse("corr", targneed), clbootse("corr", targasset))
r3<-c(nrow(targexp[!is.na(targexp$corr),]), nrow(targneed[!is.na(targneed$corr),]), nrow(targasset[!is.na(targasset$corr),]))
r4<-c(mean(targexp2$corr, na.rm=T), mean(targneed2$corr, na.rm=T), mean(targasset2$corr, na.rm=T))
r5<-c(clbootse("corr", targexp2), clbootse("corr", targneed2), clbootse("corr", targasset2))
r6<-c(nrow(targexp2[!is.na(targexp2$corr),]), nrow(targneed2[!is.na(targneed2$corr),]), nrow(targasset2[!is.na(targasset2$corr),]))

rows<-data.frame(rbind(r1, r2, r3, r4, r5, r6))
colnames(rows)<-c("Exp", "IMUE", "Asset")
rows$Exp<-round(as.numeric(rows$Exp), digits = 3)
rows$IMUE<-round(as.numeric(rows$IMUE), digits = 3)
rows$Asset<-round(as.numeric(rows$Asset), digits = 3)

stargazer(rows, summary=F)

##Statistical testing
bootstrap_simp("corr", "corr", targexp, targneed)
bootstrap_simp("corr", "corr", targexp, targasset)
bootstrap_simp("corr", "corr", targneed, targasset)
bootstrap_simp("corr", "corr", targexp2, targneed2)
bootstrap_simp("corr", "corr", targexp2, targasset2)
bootstrap_simp("corr", "corr", targneed2, targasset2)

bootstrap_simp("corr", "corr", targexp, targexp2)
bootstrap_simp("corr", "corr", targneed, targneed2)
bootstrap_simp("corr", "corr", targasset, targasset2)

#Table 8
stargazer(reg_1, reg_2, reg_3, reg_4, se=list(reg1se$boot.sds, reg2se$boot.sds, reg3se_vec,reg4se_vec))

#Table 9
r1<-c(mean(prefsq$corr, na.rm=T), mean(prefexp$corr, na.rm=T), mean(prefneed$corr, na.rm=T), mean(prefasset$corr, na.rm=T))
r2<-c(clbootse("corr", prefsq), clbootse("corr", prefexp), clbootse("corr", prefneed), clbootse("corr", prefasset))
r3<-c(nrow(prefsq[!is.na(prefsq$corr),]), nrow(prefexp[!is.na(prefexp$corr),]), nrow(prefneed[!is.na(prefneed$corr),]),nrow(prefasset[!is.na(prefasset$corr),]))
r4<-c(mean(sopsq$corr, na.rm=T), mean(sopexp$corr, na.rm=T), mean(sopneed$corr, na.rm=T), mean(sopasset$corr, na.rm=T))
r5<-c(clbootse("corr", sopsq), clbootse("corr", sopexp), clbootse("corr", sopneed), clbootse("corr", sopasset))
r6<-c(nrow(sopsq[!is.na(sopsq$corr),]), nrow(sopexp[!is.na(sopexp$corr),]), nrow(sopneed[!is.na(sopneed$corr),]),nrow(sopasset[!is.na(sopasset$corr),]))


rows<-data.frame(rbind(r1, r2, r3, r4, r5, r6))
colnames(rows)<-c("Targeting", "Exp", "Need", "Assets")

stargazer(rows, summary = F)

##Table 10
r1<-c("Expenditure", "20p", mean(agg_score[which(agg_score$agt_exp<=6),]$meetacc20_exp, na.rm=T),  mean(agg_score[which(agg_score$agt_exp<=6),]$targacc20_exp, na.rm=T), mean(agg_score[which(agg_score$agt_exp<=6),]$comacc20_exp, na.rm=T), mean(agg_score[which(agg_score$agt_exp<=6),]$ppiacc20_exp, na.rm=T), mean(agg_score[which(agg_score$agt_exp<=6),]$psacc20_exp, na.rm=T) , mean(agg_score[which(agg_score$agt_exp<=6),]$hybridacc20_exp, na.rm=T))
r2<-c("", "",  clbootse("meetacc20_exp",agg_score[which(agg_score$agt_exp<=6),]), clbootse("targacc20_exp",agg_score[which(agg_score$agt_exp<=6),]), clbootse("comacc20_exp",agg_score[which(agg_score$agt_exp<=6),]), clbootse("ppiacc20_exp",agg_score[which(agg_score$agt_exp<=6),]), clbootse("psacc20_exp",agg_score[which(agg_score$agt_exp<=6),]), clbootse("hybridacc20_exp",agg_score[which(agg_score$agt_exp<=6),]))
r3<-c("", "30p",mean(agg_score[which(agg_score$agt_exp<=9),]$meetacc30_exp, na.rm=T),  mean(agg_score[which(agg_score$agt_exp<=9),]$targacc30_exp, na.rm=T), mean(agg_score[which(agg_score$agt_exp<=9),]$comacc30_exp, na.rm=T), mean(agg_score[which(agg_score$agt_exp<=9),]$ppiacc30_exp, na.rm=T), mean(agg_score[which(agg_score$agt_exp<=9),]$psacc30_exp, na.rm=T), mean(agg_score[which(agg_score$agt_exp<=9),]$hybridacc30_exp, na.rm=T))
r4<-c("", "", clbootse("meetacc30_exp",agg_score[which(agg_score$agt_exp<=9),]), clbootse("targacc30_exp",agg_score[which(agg_score$agt_exp<=9),]), clbootse("comacc30_exp",agg_score[which(agg_score$agt_exp<=9),]), clbootse("ppiacc30_exp",agg_score[which(agg_score$agt_exp<=9),]), clbootse("psacc30_exp",agg_score[which(agg_score$agt_exp<=9),]), clbootse("hybridacc30_exp",agg_score[which(agg_score$agt_exp<=9),]))
r5<-c("", "40p", mean(agg_score[which(agg_score$agt_exp<=12),]$meetacc40_exp, na.rm=T), mean(agg_score[which(agg_score$agt_exp<=12),]$targacc40_exp, na.rm=T), mean(agg_score[which(agg_score$agt_exp<=12),]$comacc40_exp, na.rm=T), mean(agg_score[which(agg_score$agt_exp<=12),]$ppiacc40_exp, na.rm=T), mean(agg_score[which(agg_score$agt_exp<=12),]$psacc40_exp, na.rm=T), mean(agg_score[which(agg_score$agt_exp<=12),]$hybridacc40_exp, na.rm=T))
r6<-c("", "", clbootse("meetacc40_exp",agg_score[which(agg_score$agt_exp<=12),]), clbootse("targacc40_exp",agg_score[which(agg_score$agt_exp<=12),]), clbootse("comacc40_exp",agg_score[which(agg_score$agt_exp<=12),]), clbootse("ppiacc40_exp",agg_score[which(agg_score$agt_exp<=12),]), clbootse("psacc40_exp",agg_score[which(agg_score$agt_exp<=12),]), clbootse("hybridacc40_exp",agg_score[which(agg_score$agt_exp<=12),]))
r7<-c("Need", "20p", mean(agg_score[which(agg_score$agt_need<=6),]$meetacc20_need, na.rm=T), mean(agg_score[which(agg_score$agt_need<=6),]$targacc20_need, na.rm=T), mean(agg_score[which(agg_score$agt_need<=6),]$comacc20_need, na.rm=T), mean(agg_score[which(agg_score$agt_need<=6),]$ppiacc20_need, na.rm=T), mean(agg_score[which(agg_score$agt_need<=6),]$psacc20_need, na.rm=T), mean(agg_score[which(agg_score$agt_need<=6),]$hybridacc20_need, na.rm=T))
r8<-c("", "", clbootse("meetacc20_need",agg_score[which(agg_score$agt_need<=6),]), clbootse("targacc20_need",agg_score[which(agg_score$agt_need<=6),]), clbootse("comacc20_need",agg_score[which(agg_score$agt_need<=6),]), clbootse("ppiacc20_need",agg_score[which(agg_score$agt_need<=6),]), clbootse("psacc20_need",agg_score[which(agg_score$agt_need<=6),]), clbootse("hybridacc20_need",agg_score[which(agg_score$agt_need<=6),]))
r9<-c("", "30p", mean(agg_score[which(agg_score$agt_need<=9),]$meetacc30_need, na.rm=T), mean(agg_score[which(agg_score$agt_need<=9),]$targacc30_need, na.rm=T), mean(agg_score[which(agg_score$agt_need<=9),]$comacc30_need, na.rm=T), mean(agg_score[which(agg_score$agt_need<=9),]$ppiacc30_need, na.rm=T), mean(agg_score[which(agg_score$agt_need<=9),]$psacc30_need, na.rm=T), mean(agg_score[which(agg_score$agt_need<=9),]$hybridacc30_need, na.rm=T))
r10<-c("", "", clbootse("meetacc30_need",agg_score[which(agg_score$agt_need<=9),]), clbootse("targacc30_need",agg_score[which(agg_score$agt_need<=9),]), clbootse("comacc30_need",agg_score[which(agg_score$agt_need<=9),]), clbootse("ppiacc30_need",agg_score[which(agg_score$agt_need<=9),]), clbootse("psacc30_need",agg_score[which(agg_score$agt_need<=9),]), clbootse("hybridacc30_need",agg_score[which(agg_score$agt_need<=9),]))
r11<-c("", "40p", mean(agg_score[which(agg_score$agt_need<=12),]$meetacc40_need, na.rm=T), mean(agg_score[which(agg_score$agt_need<=12),]$targacc40_need, na.rm=T), mean(agg_score[which(agg_score$agt_need<=12),]$comacc40_need, na.rm=T), mean(agg_score[which(agg_score$agt_need<=12),]$ppiacc40_need, na.rm=T), mean(agg_score[which(agg_score$agt_need<=12),]$psacc40_need, na.rm=T), mean(agg_score[which(agg_score$agt_need<=12),]$hybridacc40_need, na.rm=T))
r12<-c("", "",  clbootse("meetacc40_need",agg_score[which(agg_score$agt_need<=12),]), clbootse("targacc40_need",agg_score[which(agg_score$agt_need<=12),]), clbootse("comacc40_need",agg_score[which(agg_score$agt_need<=12),]), clbootse("ppiacc40_need",agg_score[which(agg_score$agt_need<=12),]), clbootse("psacc40_need",agg_score[which(agg_score$agt_need<=12),]), clbootse("hybridacc40_need",agg_score[which(agg_score$agt_need<=12),]))
r13<-c("Assets", "20p", mean(agg_score[which(agg_score$agt_asset<=6),]$meetacc20_asset, na.rm=T), mean(agg_score[which(agg_score$agt_asset<=6),]$targacc20_asset, na.rm=T), mean(agg_score[which(agg_score$agt_asset<=6),]$comacc20_asset, na.rm=T), mean(agg_score[which(agg_score$agt_asset<=6),]$ppiacc20_asset, na.rm=T), mean(agg_score[which(agg_score$agt_asset<=6),]$psacc20_asset, na.rm=T), mean(agg_score[which(agg_score$agt_asset<=6),]$hybridacc20_asset, na.rm=T))
r14<-c("", "", clbootse("meetacc20_asset",agg_score[which(agg_score$agt_asset<=6),]), clbootse("targacc20_asset",agg_score[which(agg_score$agt_asset<=6),]), clbootse("comacc20_asset",agg_score[which(agg_score$agt_asset<=6),]), clbootse("ppiacc20_asset",agg_score[which(agg_score$agt_asset<=6),]), clbootse("psacc20_asset",agg_score[which(agg_score$agt_asset<=6),]), clbootse("hybridacc20_asset",agg_score[which(agg_score$agt_asset<=6),]))
r15<-c("", "30p", mean(agg_score[which(agg_score$agt_asset<=9),]$meetacc30_asset, na.rm=T), mean(agg_score[which(agg_score$agt_asset<=9),]$targacc30_asset, na.rm=T), mean(agg_score[which(agg_score$agt_asset<=9),]$comacc30_asset, na.rm=T), mean(agg_score[which(agg_score$agt_asset<=9),]$ppiacc30_asset, na.rm=T), mean(agg_score[which(agg_score$agt_asset<=9),]$psacc30_asset, na.rm=T), mean(agg_score[which(agg_score$agt_asset<=9),]$hybridacc30_asset, na.rm=T))
r16<-c("", "", clbootse("meetacc30_asset",agg_score[which(agg_score$agt_asset<=9),]), clbootse("targacc30_asset",agg_score[which(agg_score$agt_asset<=9),]), clbootse("comacc30_asset",agg_score[which(agg_score$agt_asset<=9),]), clbootse("ppiacc30_asset",agg_score[which(agg_score$agt_asset<=9),]), clbootse("psacc30_asset",agg_score[which(agg_score$agt_asset<=9),]), clbootse("hybridacc30_asset",agg_score[which(agg_score$agt_asset<=9),]))
r17<-c("", "40p", mean(agg_score[which(agg_score$agt_asset<=12),]$meetacc40_asset, na.rm=T), mean(agg_score[which(agg_score$agt_asset<=12),]$targacc40_asset, na.rm=T), mean(agg_score[which(agg_score$agt_asset<=12),]$comacc40_asset, na.rm=T), mean(agg_score[which(agg_score$agt_asset<=12),]$ppiacc40_asset, na.rm=T), mean(agg_score[which(agg_score$agt_asset<=12),]$psacc40_asset, na.rm=T), mean(agg_score[which(agg_score$agt_asset<=12),]$hybridacc40_asset, na.rm=T))
r18<-c("", "",  clbootse("meetacc40_asset",agg_score[which(agg_score$agt_asset<=12),]),  clbootse("targacc40_asset",agg_score[which(agg_score$agt_asset<=12),]), clbootse("comacc40_asset",agg_score[which(agg_score$agt_asset<=12),]), clbootse("ppiacc40_asset",agg_score[which(agg_score$agt_asset<=12),]), clbootse("psacc40_asset",agg_score[which(agg_score$agt_asset<=12),]), clbootse("hybridacc40_asset",agg_score[which(agg_score$agt_asset<=12),]))

rows<-data.frame(rbind(r1, r2, r3, r4, r5, r6, r7, r8, r9, r10, r11, r12, r13, r14, r15, r16, r17, r18))
colnames(rows)<-c("Metric", "Poverty Line", "Meeting Targeting Rank", "Individual Targeting Rank", "Metric-Specific Rank", "PPI", "PS", "Hybrid")
rows$`Meeting Targeting Rank`<-round(as.numeric(rows$`Meeting Targeting Rank`), digits=2)
rows$`Individual Targeting Rank`<-round(as.numeric(rows$`Individual Targeting Rank`), digits=2)
rows$`Metric-Specific Rank`<-round(as.numeric(rows$`Metric-Specific Rank`), digits=2)
rows$PPI<-round(as.numeric(rows$PPI), digits=2)
rows$PS<-round(as.numeric(rows$PS), digits=2)
rows$Hybrid<-round(as.numeric(rows$Hybrid), digits=2)
rownames(rows)<-NULL
stargazer(rows, summary=F)


#Statistical Tests
bootstrap_simp("meetacc20_exp", "targacc20_exp", agg_score[which(agg_score$agt_exp<=6),], agg_score[which(agg_score$agt_exp<=6),])
bootstrap_simp("meetacc20_exp", "comacc20_exp", agg_score[which(agg_score$agt_exp<=6),], agg_score[which(agg_score$agt_exp<=6),])
bootstrap_simp("meetacc20_exp", "ppiacc20_exp", agg_score[which(agg_score$agt_exp<=6),], agg_score[which(agg_score$agt_exp<=6),])
bootstrap_simp("meetacc20_exp", "psacc20_exp", agg_score[which(agg_score$agt_exp<=6),], agg_score[which(agg_score$agt_exp<=6),])
bootstrap_simp("meetacc20_exp", "hybridacc20_exp", agg_score[which(agg_score$agt_exp<=6),], agg_score[which(agg_score$agt_exp<=6),])
bootstrap_simp("targacc20_exp", "comacc20_exp", agg_score[which(agg_score$agt_exp<=6),], agg_score[which(agg_score$agt_exp<=6),])
bootstrap_simp("targacc20_exp", "ppiacc20_exp", agg_score[which(agg_score$agt_exp<=6),], agg_score[which(agg_score$agt_exp<=6),])
bootstrap_simp("targacc20_exp", "psacc20_exp", agg_score[which(agg_score$agt_exp<=6),], agg_score[which(agg_score$agt_exp<=6),])
bootstrap_simp("targacc20_exp", "hybridacc20_exp", agg_score[which(agg_score$agt_exp<=6),], agg_score[which(agg_score$agt_exp<=6),])
bootstrap_simp("comacc20_exp", "ppiacc20_exp", agg_score[which(agg_score$agt_exp<=6),], agg_score[which(agg_score$agt_exp<=6),])
bootstrap_simp("comacc20_exp", "psacc20_exp", agg_score[which(agg_score$agt_exp<=6),], agg_score[which(agg_score$agt_exp<=6),])
bootstrap_simp("comacc20_exp", "hybridacc20_exp", agg_score[which(agg_score$agt_exp<=6),], agg_score[which(agg_score$agt_exp<=6),])
bootstrap_simp("ppiacc20_exp", "psacc20_exp", agg_score[which(agg_score$agt_exp<=6),], agg_score[which(agg_score$agt_exp<=6),])
bootstrap_simp("ppiacc20_exp", "hybridacc20_exp", agg_score[which(agg_score$agt_exp<=6),], agg_score[which(agg_score$agt_exp<=6),])
bootstrap_simp("psacc20_exp", "hybridacc20_exp", agg_score[which(agg_score$agt_exp<=6),], agg_score[which(agg_score$agt_exp<=6),])

bootstrap_simp("meetacc30_exp", "targacc30_exp", agg_score[which(agg_score$agt_exp<=9),], agg_score[which(agg_score$agt_exp<=9),])
bootstrap_simp("meetacc30_exp", "comacc30_exp", agg_score[which(agg_score$agt_exp<=9),], agg_score[which(agg_score$agt_exp<=9),])
bootstrap_simp("meetacc30_exp", "ppiacc30_exp", agg_score[which(agg_score$agt_exp<=9),], agg_score[which(agg_score$agt_exp<=9),])
bootstrap_simp("meetacc30_exp", "psacc30_exp", agg_score[which(agg_score$agt_exp<=9),], agg_score[which(agg_score$agt_exp<=9),])
bootstrap_simp("meetacc30_exp", "hybridacc30_exp", agg_score[which(agg_score$agt_exp<=9),], agg_score[which(agg_score$agt_exp<=9),])
bootstrap_simp("targacc30_exp", "comacc30_exp", agg_score[which(agg_score$agt_exp<=9),], agg_score[which(agg_score$agt_exp<=9),])
bootstrap_simp("targacc30_exp", "ppiacc30_exp", agg_score[which(agg_score$agt_exp<=9),], agg_score[which(agg_score$agt_exp<=9),])
bootstrap_simp("targacc30_exp", "psacc30_exp", agg_score[which(agg_score$agt_exp<=9),], agg_score[which(agg_score$agt_exp<=9),])
bootstrap_simp("targacc30_exp", "hybridacc30_exp", agg_score[which(agg_score$agt_exp<=9),], agg_score[which(agg_score$agt_exp<=9),])
bootstrap_simp("comacc30_exp", "ppiacc30_exp", agg_score[which(agg_score$agt_exp<=9),], agg_score[which(agg_score$agt_exp<=9),])
bootstrap_simp("comacc30_exp", "psacc30_exp", agg_score[which(agg_score$agt_exp<=9),], agg_score[which(agg_score$agt_exp<=9),])
bootstrap_simp("comacc30_exp", "hybridacc30_exp", agg_score[which(agg_score$agt_exp<=9),], agg_score[which(agg_score$agt_exp<=9),])
bootstrap_simp("ppiacc30_exp", "psacc30_exp", agg_score[which(agg_score$agt_exp<=9),], agg_score[which(agg_score$agt_exp<=9),])
bootstrap_simp("ppiacc30_exp", "hybridacc30_exp", agg_score[which(agg_score$agt_exp<=9),], agg_score[which(agg_score$agt_exp<=9),])
bootstrap_simp("psacc30_exp", "hybridacc30_exp", agg_score[which(agg_score$agt_exp<=9),], agg_score[which(agg_score$agt_exp<=9),])

bootstrap_simp("meetacc40_exp", "targacc40_exp", agg_score[which(agg_score$agt_exp<=12),], agg_score[which(agg_score$agt_exp<=12),])
bootstrap_simp("meetacc40_exp", "comacc40_exp", agg_score[which(agg_score$agt_exp<=12),], agg_score[which(agg_score$agt_exp<=12),])
bootstrap_simp("meetacc40_exp", "ppiacc40_exp", agg_score[which(agg_score$agt_exp<=12),], agg_score[which(agg_score$agt_exp<=12),])
bootstrap_simp("meetacc40_exp", "psacc40_exp", agg_score[which(agg_score$agt_exp<=12),], agg_score[which(agg_score$agt_exp<=12),])
bootstrap_simp("meetacc40_exp", "hybridacc40_exp", agg_score[which(agg_score$agt_exp<=12),], agg_score[which(agg_score$agt_exp<=12),])
bootstrap_simp("targacc40_exp", "comacc40_exp", agg_score[which(agg_score$agt_exp<=12),], agg_score[which(agg_score$agt_exp<=12),])
bootstrap_simp("targacc40_exp", "ppiacc40_exp", agg_score[which(agg_score$agt_exp<=12),], agg_score[which(agg_score$agt_exp<=12),])
bootstrap_simp("targacc40_exp", "psacc40_exp", agg_score[which(agg_score$agt_exp<=12),], agg_score[which(agg_score$agt_exp<=12),])
bootstrap_simp("targacc40_exp", "hybridacc40_exp", agg_score[which(agg_score$agt_exp<=12),], agg_score[which(agg_score$agt_exp<=12),])
bootstrap_simp("comacc40_exp", "ppiacc40_exp", agg_score[which(agg_score$agt_exp<=12),], agg_score[which(agg_score$agt_exp<=12),])
bootstrap_simp("comacc40_exp", "psacc40_exp", agg_score[which(agg_score$agt_exp<=12),], agg_score[which(agg_score$agt_exp<=12),])
bootstrap_simp("comacc40_exp", "hybridacc40_exp", agg_score[which(agg_score$agt_exp<=12),], agg_score[which(agg_score$agt_exp<=12),])
bootstrap_simp("ppiacc40_exp", "psacc40_exp", agg_score[which(agg_score$agt_exp<=12),], agg_score[which(agg_score$agt_exp<=12),])
bootstrap_simp("ppiacc40_exp", "hybridacc40_exp", agg_score[which(agg_score$agt_exp<=12),], agg_score[which(agg_score$agt_exp<=12),])
bootstrap_simp("psacc40_exp", "hybridacc40_exp", agg_score[which(agg_score$agt_exp<=12),], agg_score[which(agg_score$agt_exp<=12),])


bootstrap_simp("meetacc20_need", "targacc20_need", agg_score[which(agg_score$agt_need<=6),], agg_score[which(agg_score$agt_need<=6),])
bootstrap_simp("meetacc20_need", "comacc20_need", agg_score[which(agg_score$agt_need<=6),], agg_score[which(agg_score$agt_need<=6),])
bootstrap_simp("meetacc20_need", "ppiacc20_need", agg_score[which(agg_score$agt_need<=6),], agg_score[which(agg_score$agt_need<=6),])
bootstrap_simp("meetacc20_need", "psacc20_need", agg_score[which(agg_score$agt_need<=6),], agg_score[which(agg_score$agt_need<=6),])
bootstrap_simp("meetacc20_need", "hybridacc20_need", agg_score[which(agg_score$agt_need<=6),], agg_score[which(agg_score$agt_need<=6),])
bootstrap_simp("targacc20_need", "comacc20_need", agg_score[which(agg_score$agt_need<=6),], agg_score[which(agg_score$agt_need<=6),])
bootstrap_simp("targacc20_need", "ppiacc20_need", agg_score[which(agg_score$agt_need<=6),], agg_score[which(agg_score$agt_need<=6),])
bootstrap_simp("targacc20_need", "psacc20_need", agg_score[which(agg_score$agt_need<=6),], agg_score[which(agg_score$agt_need<=6),])
bootstrap_simp("targacc20_need", "hybridacc20_need", agg_score[which(agg_score$agt_need<=6),], agg_score[which(agg_score$agt_need<=6),])
bootstrap_simp("comacc20_need", "ppiacc20_need", agg_score[which(agg_score$agt_need<=6),], agg_score[which(agg_score$agt_need<=6),])
bootstrap_simp("comacc20_need", "psacc20_need", agg_score[which(agg_score$agt_need<=6),], agg_score[which(agg_score$agt_need<=6),])
bootstrap_simp("comacc20_need", "hybridacc20_need", agg_score[which(agg_score$agt_need<=6),], agg_score[which(agg_score$agt_need<=6),])
bootstrap_simp("ppiacc20_need", "psacc20_need", agg_score[which(agg_score$agt_need<=6),], agg_score[which(agg_score$agt_need<=6),])
bootstrap_simp("ppiacc20_need", "hybridacc20_need", agg_score[which(agg_score$agt_need<=6),], agg_score[which(agg_score$agt_need<=6),])
bootstrap_simp("psacc20_need", "hybridacc20_need", agg_score[which(agg_score$agt_need<=6),], agg_score[which(agg_score$agt_need<=6),])

bootstrap_simp("meetacc30_need", "targacc30_need", agg_score[which(agg_score$agt_need<=9),], agg_score[which(agg_score$agt_need<=9),])
bootstrap_simp("meetacc30_need", "comacc30_need", agg_score[which(agg_score$agt_need<=9),], agg_score[which(agg_score$agt_need<=9),])
bootstrap_simp("meetacc30_need", "ppiacc30_need", agg_score[which(agg_score$agt_need<=9),], agg_score[which(agg_score$agt_need<=9),])
bootstrap_simp("meetacc30_need", "psacc30_need", agg_score[which(agg_score$agt_need<=9),], agg_score[which(agg_score$agt_need<=9),])
bootstrap_simp("meetacc30_need", "hybridacc30_need", agg_score[which(agg_score$agt_need<=9),], agg_score[which(agg_score$agt_need<=9),])
bootstrap_simp("targacc30_need", "comacc30_need", agg_score[which(agg_score$agt_need<=9),], agg_score[which(agg_score$agt_need<=9),])
bootstrap_simp("targacc30_need", "ppiacc30_need", agg_score[which(agg_score$agt_need<=9),], agg_score[which(agg_score$agt_need<=9),])
bootstrap_simp("targacc30_need", "psacc30_need", agg_score[which(agg_score$agt_need<=9),], agg_score[which(agg_score$agt_need<=9),])
bootstrap_simp("targacc30_need", "hybridacc30_need", agg_score[which(agg_score$agt_need<=9),], agg_score[which(agg_score$agt_need<=9),])
bootstrap_simp("comacc30_need", "ppiacc30_need", agg_score[which(agg_score$agt_need<=9),], agg_score[which(agg_score$agt_need<=9),])
bootstrap_simp("comacc30_need", "psacc30_need", agg_score[which(agg_score$agt_need<=9),], agg_score[which(agg_score$agt_need<=9),])
bootstrap_simp("comacc30_need", "hybridacc30_need", agg_score[which(agg_score$agt_need<=9),], agg_score[which(agg_score$agt_need<=9),])
bootstrap_simp("ppiacc30_need", "psacc30_need", agg_score[which(agg_score$agt_need<=9),], agg_score[which(agg_score$agt_need<=9),])
bootstrap_simp("ppiacc30_need", "hybridacc30_need", agg_score[which(agg_score$agt_need<=9),], agg_score[which(agg_score$agt_need<=9),])
bootstrap_simp("psacc30_need", "hybridacc30_need", agg_score[which(agg_score$agt_need<=9),], agg_score[which(agg_score$agt_need<=9),])

bootstrap_simp("meetacc40_need", "targacc40_need", agg_score[which(agg_score$agt_need<=12),], agg_score[which(agg_score$agt_need<=12),])
bootstrap_simp("meetacc40_need", "comacc40_need", agg_score[which(agg_score$agt_need<=12),], agg_score[which(agg_score$agt_need<=12),])
bootstrap_simp("meetacc40_need", "ppiacc40_need", agg_score[which(agg_score$agt_need<=12),], agg_score[which(agg_score$agt_need<=12),])
bootstrap_simp("meetacc40_need", "psacc40_need", agg_score[which(agg_score$agt_need<=12),], agg_score[which(agg_score$agt_need<=12),])
bootstrap_simp("meetacc40_need", "hybridacc40_need", agg_score[which(agg_score$agt_need<=12),], agg_score[which(agg_score$agt_need<=12),])
bootstrap_simp("targacc40_need", "comacc40_need", agg_score[which(agg_score$agt_need<=12),], agg_score[which(agg_score$agt_need<=12),])
bootstrap_simp("targacc40_need", "ppiacc40_need", agg_score[which(agg_score$agt_need<=12),], agg_score[which(agg_score$agt_need<=12),])
bootstrap_simp("targacc40_need", "psacc40_need", agg_score[which(agg_score$agt_need<=12),], agg_score[which(agg_score$agt_need<=12),])
bootstrap_simp("targacc40_need", "hybridacc40_need", agg_score[which(agg_score$agt_need<=12),], agg_score[which(agg_score$agt_need<=12),])
bootstrap_simp("comacc40_need", "ppiacc40_need", agg_score[which(agg_score$agt_need<=12),], agg_score[which(agg_score$agt_need<=12),])
bootstrap_simp("comacc40_need", "psacc40_need", agg_score[which(agg_score$agt_need<=12),], agg_score[which(agg_score$agt_need<=12),])
bootstrap_simp("comacc40_need", "hybridacc40_need", agg_score[which(agg_score$agt_need<=12),], agg_score[which(agg_score$agt_need<=12),])
bootstrap_simp("ppiacc40_need", "psacc40_need", agg_score[which(agg_score$agt_need<=12),], agg_score[which(agg_score$agt_need<=12),])
bootstrap_simp("ppiacc40_need", "hybridacc40_need", agg_score[which(agg_score$agt_need<=12),], agg_score[which(agg_score$agt_need<=12),])
bootstrap_simp("psacc40_need", "hybridacc40_need", agg_score[which(agg_score$agt_need<=12),], agg_score[which(agg_score$agt_need<=12),])


bootstrap_simp("meetacc20_asset", "targacc20_asset", agg_score[which(agg_score$agt_asset<=6),], agg_score[which(agg_score$agt_asset<=6),])
bootstrap_simp("meetacc20_asset", "comacc20_asset", agg_score[which(agg_score$agt_asset<=6),], agg_score[which(agg_score$agt_asset<=6),])
bootstrap_simp("meetacc20_asset", "ppiacc20_asset", agg_score[which(agg_score$agt_asset<=6),], agg_score[which(agg_score$agt_asset<=6),])
bootstrap_simp("meetacc20_asset", "psacc20_asset", agg_score[which(agg_score$agt_asset<=6),], agg_score[which(agg_score$agt_asset<=6),])
bootstrap_simp("meetacc20_asset", "hybridacc20_asset", agg_score[which(agg_score$agt_asset<=6),], agg_score[which(agg_score$agt_asset<=6),])
bootstrap_simp("targacc20_asset", "comacc20_asset", agg_score[which(agg_score$agt_asset<=6),], agg_score[which(agg_score$agt_asset<=6),])
bootstrap_simp("targacc20_asset", "ppiacc20_asset", agg_score[which(agg_score$agt_asset<=6),], agg_score[which(agg_score$agt_asset<=6),])
bootstrap_simp("targacc20_asset", "psacc20_asset", agg_score[which(agg_score$agt_asset<=6),], agg_score[which(agg_score$agt_asset<=6),])
bootstrap_simp("targacc20_asset", "hybridacc20_asset", agg_score[which(agg_score$agt_asset<=6),], agg_score[which(agg_score$agt_asset<=6),])
bootstrap_simp("comacc20_asset", "ppiacc20_asset", agg_score[which(agg_score$agt_asset<=6),], agg_score[which(agg_score$agt_asset<=6),])
bootstrap_simp("comacc20_asset", "psacc20_asset", agg_score[which(agg_score$agt_asset<=6),], agg_score[which(agg_score$agt_asset<=6),])
bootstrap_simp("comacc20_asset", "hybridacc20_asset", agg_score[which(agg_score$agt_asset<=6),], agg_score[which(agg_score$agt_asset<=6),])
bootstrap_simp("ppiacc20_asset", "psacc20_asset", agg_score[which(agg_score$agt_asset<=6),], agg_score[which(agg_score$agt_asset<=6),])
bootstrap_simp("ppiacc20_asset", "hybridacc20_asset", agg_score[which(agg_score$agt_asset<=6),], agg_score[which(agg_score$agt_asset<=6),])
bootstrap_simp("psacc20_asset", "hybridacc20_asset", agg_score[which(agg_score$agt_asset<=6),], agg_score[which(agg_score$agt_asset<=6),])

bootstrap_simp("meetacc30_asset", "targacc30_asset", agg_score[which(agg_score$agt_asset<=9),], agg_score[which(agg_score$agt_asset<=9),])
bootstrap_simp("meetacc30_asset", "comacc30_asset", agg_score[which(agg_score$agt_asset<=9),], agg_score[which(agg_score$agt_asset<=9),])
bootstrap_simp("meetacc30_asset", "ppiacc30_asset", agg_score[which(agg_score$agt_asset<=9),], agg_score[which(agg_score$agt_asset<=9),])
bootstrap_simp("meetacc30_asset", "psacc30_asset", agg_score[which(agg_score$agt_asset<=9),], agg_score[which(agg_score$agt_asset<=9),])
bootstrap_simp("meetacc30_asset", "hybridacc30_asset", agg_score[which(agg_score$agt_asset<=9),], agg_score[which(agg_score$agt_asset<=9),])
bootstrap_simp("targacc30_asset", "comacc30_asset", agg_score[which(agg_score$agt_asset<=9),], agg_score[which(agg_score$agt_asset<=9),])
bootstrap_simp("targacc30_asset", "ppiacc30_asset", agg_score[which(agg_score$agt_asset<=9),], agg_score[which(agg_score$agt_asset<=9),])
bootstrap_simp("targacc30_asset", "psacc30_asset", agg_score[which(agg_score$agt_asset<=9),], agg_score[which(agg_score$agt_asset<=9),])
bootstrap_simp("targacc30_asset", "hybridacc30_asset", agg_score[which(agg_score$agt_asset<=9),], agg_score[which(agg_score$agt_asset<=9),])
bootstrap_simp("comacc30_asset", "ppiacc30_asset", agg_score[which(agg_score$agt_asset<=9),], agg_score[which(agg_score$agt_asset<=9),])
bootstrap_simp("comacc30_asset", "psacc30_asset", agg_score[which(agg_score$agt_asset<=9),], agg_score[which(agg_score$agt_asset<=9),])
bootstrap_simp("comacc30_asset", "hybridacc30_asset", agg_score[which(agg_score$agt_asset<=9),], agg_score[which(agg_score$agt_asset<=9),])
bootstrap_simp("ppiacc30_asset", "psacc30_asset", agg_score[which(agg_score$agt_asset<=9),], agg_score[which(agg_score$agt_asset<=9),])
bootstrap_simp("ppiacc30_asset", "hybridacc30_asset", agg_score[which(agg_score$agt_asset<=9),], agg_score[which(agg_score$agt_asset<=9),])
bootstrap_simp("psacc30_asset", "hybridacc30_asset", agg_score[which(agg_score$agt_asset<=9),], agg_score[which(agg_score$agt_asset<=9),])

bootstrap_simp("meetacc40_asset", "targacc40_asset", agg_score[which(agg_score$agt_asset<=12),], agg_score[which(agg_score$agt_asset<=12),])
bootstrap_simp("meetacc40_asset", "comacc40_asset", agg_score[which(agg_score$agt_asset<=12),], agg_score[which(agg_score$agt_asset<=12),])
bootstrap_simp("meetacc40_asset", "ppiacc40_asset", agg_score[which(agg_score$agt_asset<=12),], agg_score[which(agg_score$agt_asset<=12),])
bootstrap_simp("meetacc40_asset", "psacc40_asset", agg_score[which(agg_score$agt_asset<=12),], agg_score[which(agg_score$agt_asset<=12),])
bootstrap_simp("meetacc40_asset", "hybridacc40_asset", agg_score[which(agg_score$agt_asset<=12),], agg_score[which(agg_score$agt_asset<=12),])
bootstrap_simp("targacc40_asset", "comacc40_asset", agg_score[which(agg_score$agt_asset<=12),], agg_score[which(agg_score$agt_asset<=12),])
bootstrap_simp("targacc40_asset", "ppiacc40_asset", agg_score[which(agg_score$agt_asset<=12),], agg_score[which(agg_score$agt_asset<=12),])
bootstrap_simp("targacc40_asset", "psacc40_asset", agg_score[which(agg_score$agt_asset<=12),], agg_score[which(agg_score$agt_asset<=12),])
bootstrap_simp("targacc40_asset", "hybridacc40_asset", agg_score[which(agg_score$agt_asset<=12),], agg_score[which(agg_score$agt_asset<=12),])
bootstrap_simp("comacc40_asset", "ppiacc40_asset", agg_score[which(agg_score$agt_asset<=12),], agg_score[which(agg_score$agt_asset<=12),])
bootstrap_simp("comacc40_asset", "psacc40_asset", agg_score[which(agg_score$agt_asset<=12),], agg_score[which(agg_score$agt_asset<=12),])
bootstrap_simp("comacc40_asset", "hybridacc40_asset", agg_score[which(agg_score$agt_asset<=12),], agg_score[which(agg_score$agt_asset<=12),])
bootstrap_simp("ppiacc40_asset", "psacc40_asset", agg_score[which(agg_score$agt_asset<=12),], agg_score[which(agg_score$agt_asset<=12),])
bootstrap_simp("ppiacc40_asset", "hybridacc40_asset", agg_score[which(agg_score$agt_asset<=12),], agg_score[which(agg_score$agt_asset<=12),])
bootstrap_simp("psacc40_asset", "hybridacc40_asset", agg_score[which(agg_score$agt_asset<=12),], agg_score[which(agg_score$agt_asset<=12),])


#6.Main Figures###################
##Figure 1
ranks$logexp<-log(ranks$pc_food_exp_dollar)
q<-ggplot(ranks, aes(x=logexp)) + 
  geom_histogram(color="black", fill="gray", bins = 20) +
  labs(x=" Log PC Food Expenditures ($)", y = "Freq") +
  ylim(0, 800)+
  theme(axis.title.y = element_text(size = rel(1.0), angle = 90))+
  theme(axis.title.x = element_text(size = rel(1.0), angle = 00))

r<-ggplot(ranks, aes(x=loglambdas)) + 
  geom_histogram(color="black", fill="gray", bins = 20) +
  labs(x="(-) MUE", y = "Freq") +
  ylim(0, 800)+
  theme(axis.title.y = element_text(size = rel(1.0), angle = 90))+
  theme(axis.title.x = element_text(size = rel(1.0), angle = 00))

s<-ggplot(ranks, aes(x=score)) + 
  geom_histogram(color="black", fill="gray", bins = 20) +
  labs(x="Asset Index Score", y = "Freq") +
  ylim(0, 800)+
  theme(axis.title.y = element_text(size = rel(1.0), angle = 90))+
  theme(axis.title.x = element_text(size = rel(1.0), angle = 00))

figure_1 <- ggarrange(q, r, s, ncol = 2, nrow = 2)


##Figure 2
figure_2<-ggplot(total_known, aes(x=n_know)) + 
  geom_histogram(aes(y=..density..),color="black", fill="gray", bins = 20) +
  labs(x="Number of Neighbors Known", y = "Freq") +
  theme(axis.title.y = element_text(size = rel(1.0), angle = 90))+
  theme(axis.title.x = element_text(size = rel(1.0), angle = 00))

##Figure 3
a<-ggplot(df3, aes(x=corr_exp)) + 
  geom_histogram(color="black", fill="gray", bins = 20) +
  labs(x="Cor(Survey PC Exp., Participant PC Exp.)", y = "Freq") +
  ylim(0,40)+
  theme(axis.title.y = element_text(size = rel(1.0), angle = 90))+
  theme(axis.title.x = element_text(size = rel(0.6), angle = 00))

b<-ggplot(df4, aes(x=corr_expn)) + 
  geom_histogram(color="black", fill="gray", bins = 20) +
  labs(x="Cor(Survey MUE., Participant Neediness)", y = "Freq") +
  ylim(0,40)+
  theme(axis.title.y = element_text(size = rel(1.0), angle = 90))+
  theme(axis.title.x = element_text(size = rel(0.6), angle = 00))

c<-ggplot(df5, aes(x=corr_expa)) + 
  geom_histogram(color="black", fill="gray", bins = 20) +
  labs(x="Cor(Survey Asset Index, Participant Asset Value)", y = "Freq") +
  ylim(0,40)+
  theme(axis.title.y = element_text(size = rel(1.0), angle = 90))+
  theme(axis.title.x = element_text(size = rel(0.6), angle = 00))

figure_3 <- ggarrange(a, b, c, ncol = 2, nrow = 2)

## Figure 4
q<-ggplot(franks, aes(x=dexp_sd)) + 
  geom_histogram(color="black", fill="gray", bins=20) +
  labs(x=" Change PC Food Exp. (SD)", y = "Freq") +
  ylim(0, 250)+
  theme(axis.title.y = element_text(size = rel(1.0), angle = 90))+
  theme(axis.title.x = element_text(size = rel(1.0), angle = 00))

r<-ggplot(franks, aes(x=dneed_sd)) + 
  geom_histogram(color="black", fill="gray", bins = 20) +
  labs(x="(-) Change in MUE (SD)", y = "Freq") +
  ylim(0, 250)+
  theme(axis.title.y = element_text(size = rel(1.0), angle = 90))+
  theme(axis.title.x = element_text(size = rel(1.0), angle = 00))

s<-ggplot(franks, aes(x=dasset_sd)) + 
  geom_histogram(color="black", fill="gray", bins = 20) +
  labs(x="Change in Asset Index (SD)", y = "Freq") +
  ylim(0, 250)+
  theme(axis.title.y = element_text(size = rel(1.0), angle = 90))+
  theme(axis.title.x = element_text(size = rel(1.0), angle = 00))


figure_4 <- ggarrange(q, r, s,ncol = 2, nrow = 2)

##Figure 5

e<-ggplot(targexp, aes(x=corr)) + 
  geom_histogram(color="black", fill="gray", bins=15) +
  labs(x="Cor(Targeting Task, Participant PC Exp.)", y = "Freq") +
  ylim(0,250)+
  theme(axis.title.y = element_text(size = rel(1.0), angle = 90))+
  theme(axis.title.x = element_text(size = rel(0.6), angle = 00))

f<-ggplot(targneed, aes(x=corr),) + 
  geom_histogram(color="black", fill="gray", bins=15) +
  labs(x="Cor(Targeting Task, Participant Neediness)", y = "Freq") +
  ylim(0,250)+
  theme(axis.title.y = element_text(size = rel(1.0), angle = 90))+
  theme(axis.title.x = element_text(size = rel(0.6), angle = 00))

g<-ggplot(targasset, aes(x=corr)) + 
  geom_histogram(color="black", fill="gray", bins=15) +
  labs(x="Cor(Targeting Task, Participant Asset Value)", y = "Freq") +
  ylim(0,250)+
  theme(axis.title.y = element_text(size = rel(1.0), angle = 90))+
  theme(axis.title.x = element_text(size = rel(0.6), angle = 00))



figure_5 <- ggarrange(e, f, g, ncol = 2, nrow = 2)

#7. Appendix Tables##################

##Table A1
r1<-c("Respondent/Participant Gender", mean(respondents$male, na.rm=T), mean(respondents_tsub$male, na.rm=T), t.test(respondents$male,respondents_tsub$male)$p.value)
r2<-c("", std.error(respondents$male, na.rm=T), std.error(respondents_tsub$male, na.rm=T), "")
r3<-c("Respondent/Participant Age", mean(respondents$age_1, na.rm=T), mean(respondents_tsub$age_1, na.rm=T), t.test(respondents$age_1,respondents_tsub$age_1)$p.value)
r4<-c("", std.error(respondents$age_1, na.rm=T), std.error(respondents_tsub$age_1, na.rm=T), "")
r5<-c("Family Size", mean(zs2$HHsize, na.rm=T), mean(zs2_tsub$HHsize, na.rm=T), t.test(zs2$HHsize,zs2_tsub$HHsize)$p.value)
r6<-c("", std.error(zs2$HHsize, na.rm=T), std.error(zs2_tsub$HHsize, na.rm=T), "")
r7<-c("Family Head: Male", mean(zs2$male, na.rm=T), mean(zs2_tsub$male, na.rm=T), t.test(zs2$male,zs2_tsub$male)$p.value)
r8<-c("", std.error(zs2$male, na.rm=T), std.error(zs2_tsub$male, na.rm=T), "")
r9<-c("Family Head: Age", mean(zs2$age_1, na.rm=T), mean(zs2_tsub$age_1, na.rm=T), t.test(zs2$age_1,zs2_tsub$age_1)$p.value)
r10<-c("", std.error(zs2$age_1, na.rm=T), std.error(zs2_tsub$age_1, na.rm=T), "")
r11<-c("Family Head: Disabled", mean(zs2$disabled, na.rm=T), mean(zs2_tsub$disabled, na.rm=T), t.test(zs2$disabled,zs2_tsub$disabled)$p.value)
r12<-c("", std.error(zs2$disabled, na.rm=T), std.error(zs2_tsub$disabled, na.rm=T),"")
r13<-c("Family Head: Any Education", mean(zs2$educ_any, na.rm=T), mean(zs2_tsub$educ_any, na.rm=T), t.test(zs2$educ_any,zs2_tsub$educ_any)$p.value)
r14<-c("", std.error(zs2$educ_any, na.rm=T), std.error(zs2_tsub$educ_any, na.rm=T),"")
r15<-c("Family Head: Sr. High School", mean(zs2$hs_above, na.rm=T), mean(zs2_tsub$hs_above, na.rm=T),t.test(zs2$hs_above,zs2_tsub$hs_above)$p.value)
r16<-c("", std.error(zs2$hs_above, na.rm=T), std.error(zs2_tsub$hs_above, na.rm=T),"")
r17<-c("Family Head: Married", mean(zs2$married, na.rm=T), mean(zs2_tsub$married, na.rm=T), t.test(zs2$married,zs2_tsub$married)$p.value)
r18<-c("", std.error(zs2$married, na.rm=T), std.error(zs2_tsub$married, na.rm=T),"")
r19<-c("Family Head: Employed", mean(zs2$employed, na.rm=T), mean(zs2_tsub$employed, na.rm=T), t.test(zs2$employed,zs2_tsub$employed)$p.value)
r20<-c("", std.error(zs2$employed, na.rm=T), std.error(zs2_tsub$employed, na.rm=T),"")
r21<-c("Family Head: Employed (Ag.)", mean(zs2$employed_ag, na.rm=T), mean(zs2_tsub$employed_ag, na.rm=T), t.test(zs2$employed_ag,zs2_tsub$employed_ag)$p.value)
r22<-c("", std.error(zs2$employed_ag, na.rm=T), std.error(zs2_tsub$employed_ag, na.rm=T),"")
r23<-c("Family Head: Born in Village", mean(zs2$bornvill, na.rm=T), mean(zs2_tsub$bornvill, na.rm=T), t.test(zs2$bornvill,zs2_tsub$bornvill)$p.value)
r24<-c("", std.error(zs2$bornvill, na.rm=T), std.error(zs2_tsub$bornvill, na.rm=T),"")
r25<-c("Family Head: Years in Community", mean(zs2$hhliveyears, na.rm=T), mean(zs2_tsub$hhliveyears, na.rm=T),  t.test(zs2$hhliveyears,zs2_tsub$hhliveyears)$p.value)
r26<-c("", std.error(zs2$hhliveyears, na.rm=T), std.error(zs2_tsub$hhliveyears, na.rm=T),"")
r27<-c("Family Member: Any Education", mean(zs2$edhh, na.rm=T), mean(zs2_tsub$edhh, na.rm=T), t.test(zs2$edhh,zs2_tsub$edhh)$p.value)
r28<-c("", std.error(zs2$edhh, na.rm=T), std.error(zs2_tsub$edhh, na.rm=T),"")
r29<-c("Family Member: Sr. High School", mean(zs2$hh_hs, na.rm=T), mean(zs2_tsub$hh_hs, na.rm=T), t.test(zs2$hh_hs,zs2_tsub$hh_hs)$p.value)
r30<-c("", std.error(zs2$hh_hs, na.rm=T), std.error(zs2_tsub$hh_hs, na.rm=T),"")
r31<-c("Family Member: Disabled", mean(zs2$disabilityhh, na.rm=T), mean(zs2_tsub$disabilityhh, na.rm=T), t.test(zs2$disabilityhh,zs2_tsub$disabilityhh)$p.value)
r32<-c("", std.error(zs2$disabilityhh, na.rm=T), std.error(zs2_tsub$disabilityhh, na.rm=T),"")
r33<-c("Javanese (Ethinicity)", mean(zs2$jawa, na.rm=T), mean(zs2_tsub$jawa, na.rm=T), t.test(zs2$jawa,zs2_tsub$jawa)$p.value)
r34<-c("", std.error(zs2$jawa, na.rm=T), std.error(zs2_tsub$jawa, na.rm=T),"")
r35<-c("Muslim", mean(zs2$muslim, na.rm=T), mean(zs2_tsub$muslim, na.rm=T), t.test(zs2$muslim,zs2_tsub$muslim)$p.value)
r36<-c("", std.error(zs2$muslim, na.rm=T), std.error(zs2_tsub$muslim, na.rm=T),"")
r37<-c("Speak Java", mean(zs2$speakjava, na.rm=T), mean(zs2_tsub$speakjava, na.rm=T), t.test(zs2$speakjava,zs2_tsub$speakjava)$p.value)
r38<-c("", std.error(zs2$speakjava, na.rm=T), std.error(zs2_tsub$speakjava, na.rm=T),"")
r39<-c("Local Official", mean(zs2$official, na.rm=T), mean(zs2_tsub$official, na.rm=T),  t.test(zs2$official,zs2_tsub$official)$p.value)
r40<-c("", std.error(zs2$official, na.rm=T), std.error(zs2_tsub$official, na.rm=T),"")
r41<-c("Know Local Official", mean(zs2$know_official, na.rm=T), mean(zs2_tsub$know_official, na.rm=T), t.test(zs2$know_official,zs2_tsub$know_official)$p.value)
r42<-c("", std.error(zs2$know_official, na.rm=T), std.error(zs2_tsub$know_official, na.rm=T),"")
r43<-c("Participate in Community Org.", mean(zs2$comm_org, na.rm=T), mean(zs2_tsub$comm_org, na.rm=T), t.test(zs2$comm_org,zs2_tsub$comm_org)$p.value)
r44<-c("", std.error(zs2$comm_org, na.rm=T), std.error(zs2_tsub$comm_org, na.rm=T),"")
r45<-c("Weekly PC Food Expenditures ($PPP)", mean(zs2$pc_food_exp, na.rm=T), mean(zs2_tsub$pc_food_exp, na.rm=T), t.test(zs2$pc_food_exp,zs2_tsub$pc_food_exp)$p.value)
r46<-c("", std.error(zs2$pc_food_exp, na.rm=T), std.error(zs2_tsub$pc_food_exp, na.rm=T),"")
r47<-c("Weekly PC Expenditures ($PPP)", mean(zs2$pc_exp_all, na.rm=T), mean(zs2_tsub$pc_exp_all, na.rm=T), t.test(zs2$pc_exp_all,zs2_tsub$pc_exp_all)$p.value)
r48<-c("", std.error(zs2$pc_exp_all, na.rm=T), std.error(zs2_tsub$pc_exp_all, na.rm=T),"")
r49<-c("Asset Index Score", mean(zs2$score, na.rm=T), mean(zs2_tsub$score, na.rm=T), t.test(zs2$score,zs2_tsub$score)$p.value)
r50<-c("", std.error(zs2$score, na.rm=T), std.error(zs2_tsub$score, na.rm=T),"")
r51<-c("Land Area Owned (hectares)", mean(zs2$area, na.rm=T), mean(zs2_tsub$area, na.rm=T), t.test(zs2$area,zs2_tsub$area)$p.value)
r52<-c("", std.error(zs2$area, na.rm=T), std.error(zs2_tsub$area, na.rm=T),"")
r53<-c("MUE", mean(zs2$loglambdas, na.rm=T), mean(zs2_tsub$loglambdas, na.rm=T),  t.test(zs2$loglambdas,zs2_tsub$loglambdas)$p.value)
r54<-c("", std.error(zs2$loglambdas, na.rm=T), std.error(zs2_tsub$loglambdas, na.rm=T),"")
r55<-c("Ladder Step", mean(zs2$ladder, na.rm=T), mean(zs2_tsub$ladder, na.rm=T), t.test(zs2$ladder,zs2_tsub$ladder)$p.value)
r56<-c("", std.error(zs2$ladder, na.rm=T), std.error(zs2_tsub$ladder, na.rm=T),"")
r57<-c("Had Shock", mean(zs2$shock, na.rm=T), mean(zs2_tsub$shock, na.rm=T), t.test(zs2$shock,zs2_tsub$shock)$p.value)
r58<-c("", std.error(zs2$shock, na.rm=T), std.error(zs2_tsub$shock, na.rm=T),"")
r59<-c("Receives Govt. Benefits", mean(zs2$benefit, na.rm=T), mean(zs2_tsub$benefit, na.rm=T), t.test(zs2$benefit,zs2_tsub$benefit)$p.value)
r60<-c("", std.error(zs2$benefit, na.rm=T), std.error(zs2_tsub$benefit, na.rm=T),"")
r61<-c("Receives COVID Benefits", mean(zs2$benefit_covid, na.rm=T), mean(zs2_tsub$benefit_covid, na.rm=T),  t.test(zs2$benefit_covid,zs2_tsub$benefit_covid)$p.value)
r62<-c("", std.error(zs2$benefit_covid, na.rm=T), std.error(zs2_tsub$benefit_covid, na.rm=T),"")

rows<-data.frame(rbind(r1, r2, r3, r4, r5, r6, r7, r8, r9, r10, r11, r12, r13, r14, r15, r16, r17, r18, r19, r20, r21, r22, r23, r24, r25, r26, r27, r28, r29, r30, r31, r32, r33, r34, r35, r36, r37, r38, r39, r40, r41, r42, r43, r44, r45, r46, r47, r48, r49, r50, r51, r52, r53, r54, r55, r56, r57, r58, r59, r60, r61, r62))
colnames(rows)<-c("Variable", "Survey", "Follow-Up", "P-Value")
rownames(rows)<-NULL
stargazer(rows, summary=F)

##Table A2
r1<-c("Not familiar at all", nrow(known[which(known$know==1),])/6000*100)
r2<-c("Know of/ can recognize someone in the family but have never interacted", nrow(known[which(known$know==2),])/6000*100)
r3<-c("Have talked to a member of the family before infrequently", nrow(known[which(known$know==3),])/6000*100)
r4<-c("Have talked to a member of the family before frequently", nrow(known[which(known$know==4),])/6000*100)
r5<-c("Close friend or family member", nrow(known[which(known$know==5),])/6000*100)
r6<-c("Close colleage at job/position", nrow(known[which(known$know==6),])/6000*100)
rows<-data.frame(rbind(r1, r2, r3, r4, r5, r6))
colnames(rows)<-c("Response", "Frequency")
rownames(rows)<-NULL
stargazer(rows, summary=F)

##Table A3
###Text only

##Table A4
colnames(beta)<-c("Good", "Expenditure Elasticity")
rownames(beta)<-NULL
stargazer(beta, summary=F)

##Table A5
colnames(asset2s)<-c("rankid", "No. Cupboards", "No. Tables", "No. Chairs", "No. Sofas", "No. Beds", "No. Mattresses", "No. Gas Stoves", "No. Refrigerators", "No. Rice Cookers", "No. Mixers/Blenders", "No. Fans", "No. AC Units", "No. Radios/Tape Recorders", "No. TVs", "No. DVD/VCD Players", "No. Parabolic Antenna", "No. Laptop/PC", "No. Cell Phones", "No. Bicycles", "No. Motorcycles", "No. Outboard Motors", "No. Cars/Trucks", "No. Chickens/Ducks", "No. Goats", "No. Cows/Buffaloes", "No. Horses", "No. Husker Machines", "No. Sewing Machines", "No. Electric Water Pumps", "No. Water Dispensers", "No. Ovens", "No. Washing Machines", "Area of House", "No. Rooms", "Per Capita Land Area", "Floor Material: Ceramic/Marble", "Floor Material: Tiles/Terazzo","Floor Material: Cement/Red Brick", "Roof Material: Tiles", "Roof Material: Palm Fiber", "Wall Material: Plaster", "Own House", "Main Water Source: Pumped Well", "Main Water Source: Protected Well", "Main Water Source: Mineral/Bottled", "Main Water Source: Piped", "Main Water Source: Unprotected Well", "Main Water Source Outside Home", "Household has Own Latrine", "Latrine Type: Gooseneck")
stargazer(asset2s, summary.stat = c( "mean", "sd", "median", "min", "max", "n"), title="Asset Index Components Summary Statistics")

##Table A6

r1<-c(mean(df3$corr_exp, na.rm=T), mean(ae_df$corr_exp, na.rm=T), mean(pc_cons_df$corr_exp, na.rm=T), mean(pc_pred_exp_df$corr_exp, na.rm=T))
r2<-c(clbootse("corr_exp", df3), clbootse("corr_exp", ae_df), clbootse("corr_exp", pc_cons_df), clbootse("corr_exp", pc_pred_exp_df))

rows<-data.frame(rbind(r1, r2))
colnames(rows)<-c("PC Exp.", "AE Exp.", "PC Cons.", "PC Predicted Exp")
rownames(rows)<-NULL
stargazer(rows, summary=F)

#Statistical Tests
bootstrap_simp("corr_exp", "corr_exp", df3, ae_df)
bootstrap_simp("corr_exp", "corr_exp", df3, pc_cons_df)
bootstrap_simp("corr_exp", "corr_exp", df3, pc_pred_exp_df)

##Table A7

r1<-c(mean(df4$corr_expn, na.rm=T), mean(alt_need_df$corr_exp, na.rm=T))
r2<-c(clbootse("corr_expn", df4), clbootse("corr_exp", alt_need_df))

rows<-data.frame(rbind(r1, r2))
colnames(rows)<-c("MUE (Exp. Only)", "MUE (All Cons.)")
rownames(rows)<-NULL
stargazer(rows, summary=F)

#Statistical Tests
bootstrap_simp("corr_expn", "corr_exp", df4, alt_need_df)

##Table A8

r1<-c(mean(df5$corr_expa, na.rm=T), mean(df5a$corr_expa, na.rm=T), mean(pc_value_df$corr_exp, na.rm=T), mean(area_df$corr_exp, na.rm=T),mean(no_assets_df$corr_exp, na.rm=T), mean(score_h_df$corr_exp, na.rm=T))
r2<-c(clbootse("corr_expa", df5), clbootse("corr_expa", df5a), clbootse("corr_exp", pc_value_df), clbootse("corr_exp", area_df), clbootse("corr_exp", no_assets_df), clbootse("corr_exp", score_h_df))

rows<-data.frame(rbind(r1, r2))
colnames(rows)<-c("Asset Index", "Land Value", "PC Land Value", "Land Area", "Asset Count", "House Index")
rownames(rows)<-NULL
stargazer(rows, summary=F)

bootstrap_simp("corr_expa", "corr_expa", df5, df5a)
bootstrap_simp("corr_expa", "corr_exp", df5, pc_value_df)
bootstrap_simp("corr_expa", "corr_exp", df5, area_df)
bootstrap_simp("corr_expa", "corr_exp", df5, no_assets_df)
bootstrap_simp("corr_expa", "corr_exp", df5, score_h_df)

##Table A9 
stargazer(ta, tb, tc, ta1, tb1, tc1,  se=list(regta$boot.sds, regtb$boot.sds, regtc$boot.sds, regta1$boot.sds, regtb1$boot.sds, regtc1$boot.sds))

##Table A10

r1<-c("% identified of 2 poorest", 100*mean(ranks_2exp$correct, na.rm=T), 100*mean(ranks_2need$correct, na.rm=T), 100*mean(ranks_2asset$correct, na.rm=T))
r2<-c("% identified of 2 poorest", 100*mean(ranks_3exp$correct, na.rm=T), 100*mean(ranks_3need$correct, na.rm=T), 100*mean(ranks_3asset$correct, na.rm=T))
r3<-c("% identified of 2 poorest", 100*mean(ranks_4exp$correct, na.rm=T), 100*mean(ranks_4need$correct, na.rm=T), 100*mean(ranks_4asset$correct, na.rm=T))

rows<-data.frame(rbind(r1, r2, r3))
colnames(rows)<-c("", "Exp", "MUE/Neediness", "Assets")
rownames(rows)<-NULL
stargazer(rows, summary=F)

##Table A11 
##See other script (simulation_error.R)

#Table A12
r1<-c("A) Participant",mean(dfcheck1$corr_expa, na.rm=T), mean(dfcheck2$corr_expa, na.rm=T), mean(dfcheck3$corr_expa, na.rm=T))
r2<-c("",clbootse_2("corr_expa", dfcheck1), clbootse_2("corr_expa", dfcheck2), clbootse_2("corr_expa", dfcheck3))
r3<-c("N",nrow(dfcheck1[!is.na(dfcheck1$corr_expa),]), nrow(dfcheck2[!is.na(dfcheck2$corr_expa),]), nrow(dfcheck3[!is.na(dfcheck3$corr_expa),]))
r4<-c("B) Survey", mean(df3f$corr_expa, na.rm=T), mean(df4f$corr_expa, na.rm=T), mean(df5f$corr_expa, na.rm=T))
r5<-c("",clbootse_2("corr_expa", df3f), clbootse_2("corr_expa", df4f),clbootse_2("corr_expa", df5f))
r6<-c("N", nrow(df3f[!is.na(df3f$corr_expa),]), nrow(df4f[!is.na(df4f$corr_expa),]), nrow(df5f[!is.na(df5f$corr_expa),]))

rows<-data.frame(rbind(r1, r2, r3, r4, r5, r6))
colnames(rows)<-c("", "Exp (1)", "MUE/Neediness (2)", "Asset (3)")
rownames(rows)<-NULL
stargazer(rows, summary=F)

#statistical testing

bootstrap_simp_2("corr_expa", "corr_expa", dfcheck1, dfcheck2)
bootstrap_simp_2("corr_expa", "corr_expa", dfcheck1, dfcheck3)
bootstrap_simp_2("corr_expa", "corr_expa", dfcheck2, dfcheck3)

bootstrap_simp_2("corr_expa", "corr_expa", df3f, df4f)
bootstrap_simp_2("corr_expa", "corr_expa", df3f, df5f)
bootstrap_simp_2("corr_expa", "corr_expa", df4f, df5f)

bootstrap_simp_2("corr_expa", "corr_expa", dfcheck1, df3f)
bootstrap_simp_2("corr_expa", "corr_expa", dfcheck2, df4f)
bootstrap_simp_2("corr_expa", "corr_expa", dfcheck3, df5f)

##Table A13
r1<-c("A) Baseline (3 Communities)",mean(df_1$corr_expa, na.rm=T), mean(df_2$corr_expa, na.rm=T), mean(df_3$corr_expa, na.rm=T))
r2<-c("",clbootse_2("corr_expa", df_1), clbootse_2("corr_expa", df_2), clbootse_2("corr_expa", df_3))
r3<-c("N",nrow(df_1[!is.na(df_1$corr_expa),]), nrow(df_2[!is.na(df_2$corr_expa),]), nrow(df_3[!is.na(df_3$corr_expa),]))
r4<-c("B) Follow-up", mean(df3f$corr_expa, na.rm=T), mean(df4f$corr_expa, na.rm=T), mean(df5f$corr_expa, na.rm=T))
r5<-c("",clbootse_2("corr_expa", df3f), clbootse_2("corr_expa", df4f),clbootse_2("corr_expa", df5f))
r6<-c("N", nrow(df3f[!is.na(df3f$corr_expa),]), nrow(df4f[!is.na(df4f$corr_expa),]), nrow(df5f[!is.na(df5f$corr_expa),]))

rows<-data.frame(rbind(r1, r2, r3, r4, r5, r6))
colnames(rows)<-c("", "Exp (1)", "MUE/Neediness (2)", "Asset (3)")
rownames(rows)<-NULL
stargazer(rows, summary=F)

bootstrap_simp_2("corr_expa", "corr_expa", df_1, df_2)
bootstrap_simp_2("corr_expa", "corr_expa", df_1, df_3)
bootstrap_simp_2("corr_expa", "corr_expa", df_2, df_3)

bootstrap_simp_2("corr_expa", "corr_expa", df3f, df4f)
bootstrap_simp_2("corr_expa", "corr_expa", df3f, df5f)
bootstrap_simp_2("corr_expa", "corr_expa", df4f, df5f)

bootstrap_simp_2("corr_expa", "corr_expa", df_1, df3f)
bootstrap_simp_2("corr_expa", "corr_expa", df_2, df4f)
bootstrap_simp_2("corr_expa", "corr_expa", df_3, df5f)

##Table A14
stargazer(newreg1, newreg2, newreg3, newreg4, newreg5, newreg6, se=list(se1$boot.sds, se2$boot.sds, se3$boot.sds, se4$boot.sds, se5$boot.sds,se6$boot.sds))

##Table A15
#See Additional Script (text_analysis.R)

##Table A16
stargazer(reg_a, reg_b, reg_c, reg_d, reg_e, reg_f, se=list(reg_a1$boot.sds, reg_b1$boot.sds, reg_c1$boot.sds, reg_d1$boot.sds, reg_e1$boot.sds,reg_f1$boot.sds))

