rm(list = ls())
load("correlation.RData")

save(list = ls(all = TRUE), file= "correlation.RData")

install.packages("")

library(foreign)
library(readstata13)
library(manifestoR)
library(xlsx)
library(dplyr)
library(Hmisc)
library(reshape)





###############################
###### Load the Datasets ######
###############################

# my dynamic analysis
load("data/correlations/dynamic_model.RData")
data <- data[,c("party","year","50%")]
colnames(data) <- c("party","year","mean")
data$party[data$party=="KDU-CSL2"] <- "KDU-CSL"
data$party[data$party=="KDU-CSL3"] <- "KDU-CSL"


# Chapel Hill Expert Survey (derived from http://chesdata.eu)
ches <- read.dta13("data/correlations/ches/1999-2014_CHES_dataset_means.dta") 
ches <- ches[ches$country==21,] # selecting only the Czech Republic
ches$party[ches$party=="US-DEU"] <- "US"


# Comparative Manifesto Project (derived from https://manifestoproject.wzb.eu)
cmp <- read.dta13("data/correlations/cmp/MPDataset_MPDS2016b_stata14.dta") 
cmp <- cmp[cmp$countryname=="Czech Republic",]
cmp$party <- cmp$partyabbrev
cmp$year <- as.integer(substr(cmp$date,1,4))
cmp$party[cmp$party=="KDU-ČSL"] <- "KDU-CSL"
cmp$party[cmp$party=="ČSSD"] <- "CSSD"
cmp$party[cmp$party=="KSČM"] <- "KSCM"
cmp$party[cmp$party=="ANO"] <- "ANO2011"
cmp$party[cmp$party=="Úsvit"] <- "USVIT"
cmp$party[cmp$party=="SPR-RSČ"] <- "SPR-RSC"
cmp$party[cmp$party=="KDU-ČSL-US-DEU"] <- "K"


# IVVM/CVVM (derived from http://nesstar.soc.cas.cz/webview/)
cvvm1701 <- read.spss("data/correlations/ivvm_cvvm/V1701_F1.sav", to.data.frame=TRUE, reencode='windows-1250') # 2017
cvvm1701 <- cvvm1701[cvvm1701$IDE_2>=18,]
cvvm1701$party <- cvvm1701$PV_4
cvvm1701$lepr <- cvvm1701$PO_2
cvvm1701$lepr[cvvm1701$lepr==99] <- NA
cvvm1701$lepr[cvvm1701$lepr==0] <- NA
cvvm1701$lepr[cvvm1701$lepr==97] <- NA
plot(density(cvvm1701$lepr[!is.na(cvvm1701$lepr)]))
cvvm <- cvvm1701 %>%
  select(party,lepr) %>%
  group_by(party) %>%
  summarise(
    lepr = mean(lepr, na.rm = TRUE))
cvvm$year <- 2017
cvvm1412 <- read.spss("data/correlations/ivvm_cvvm/V1412_F1.sav", to.data.frame=TRUE, reencode='windows-1250') # 2014
cvvm1412 <- cvvm1412[cvvm1412$IDE_2>=18,]
cvvm1412$party <- cvvm1412$PV_4
cvvm1412$lepr <- cvvm1412$PO_2
cvvm1412$lepr[cvvm1412$lepr==99] <- NA
plot(density(cvvm1412$lepr[!is.na(cvvm1412$lepr)]))
cvvm1 <- cvvm1412 %>%
  select(party,lepr) %>%
  group_by(party) %>%
  summarise(
    lepr = mean(lepr, na.rm = TRUE))
cvvm1$year <- 2014
cvvm <- rbind(cvvm,cvvm1)
rm(cvvm1)
cvvm1312 <- read.spss("data/correlations/ivvm_cvvm/V1312_F1.sav", to.data.frame=TRUE, reencode='windows-1250') # 2013
cvvm1312 <- cvvm1312[cvvm1312$IDE_2>=18,]
cvvm1312$party <- cvvm1312$PV_4
cvvm1312$lepr <- cvvm1312$PO_2
cvvm1312$lepr[cvvm1312$lepr==99] <- NA
plot(density(cvvm1312$lepr[!is.na(cvvm1312$lepr)]))
cvvm1 <- cvvm1312 %>%
  select(party,lepr) %>%
  group_by(party) %>%
  summarise(
    lepr = mean(lepr, na.rm = TRUE))
cvvm1$year <- 2013
cvvm <- rbind(cvvm,cvvm1)
rm(cvvm1)
cvvm1012 <- read.spss("data/correlations/ivvm_cvvm/V1012_F1.sav", to.data.frame=TRUE, reencode='windows-1250') # 2010
cvvm1012 <- cvvm1012[cvvm1012$IDE_2>=18,]
cvvm1012$party <- cvvm1012$PV_4
cvvm1012$lepr <- cvvm1012$PO_2
cvvm1012$lepr[cvvm1012$lepr==99] <- NA
plot(density(cvvm1012$lepr[!is.na(cvvm1012$lepr)]))
cvvm1 <- cvvm1012 %>%
  select(party,lepr) %>%
  group_by(party) %>%
  summarise(
    lepr = mean(lepr, na.rm = TRUE))
cvvm1$year <- 2010
cvvm <- rbind(cvvm,cvvm1)
rm(cvvm1)
cvvm0611 <- read.spss("data/correlations/ivvm_cvvm/V0611_F1.sav", to.data.frame=TRUE, reencode='windows-1250') # 2006
cvvm0611 <- cvvm0611[cvvm0611$IDE_2>=18,]
cvvm0611$party <- cvvm0611$PV_4
cvvm0611$lepr <- cvvm0611$PO_2
cvvm0611$lepr[cvvm0611$lepr==99] <- NA
cvvm0611$lepr[cvvm0611$lepr==12] <- NA
plot(density(cvvm0611$lepr[!is.na(cvvm0611$lepr)]))
cvvm1 <- cvvm0611 %>%
  select(party,lepr) %>%
  group_by(party) %>%
  summarise(
    lepr = mean(lepr, na.rm = TRUE))
cvvm1$year <- 2006
cvvm <- rbind(cvvm,cvvm1)
rm(cvvm1)
cvvm0211 <- read.csv("data/correlations/ivvm_cvvm/V0211_F1.csv") # 2002
cvvm0211 <- cvvm0211[cvvm0211$ide2>=18,]
cvvm0211$party <- cvvm0211$pv4
cvvm0211$party[cvvm0211$party==4] <- "CSSD"
cvvm0211$party[cvvm0211$party==5] <- "ODS"
cvvm0211$party[cvvm0211$party==6] <- "KSCM"
cvvm0211$party[cvvm0211$party==7] <- "KDU-CSL"
cvvm0211$party[cvvm0211$party==8] <- "US"
cvvm0211$party[cvvm0211$party==12] <- "K"
cvvm0211$lepr <- cvvm0211$po2
cvvm0211$lepr[cvvm0211$lepr==99] <- NA
cvvm0211$lepr[cvvm0211$lepr==0] <- NA
plot(density(cvvm0211$lepr[!is.na(cvvm0211$lepr)]))
cvvm1 <- cvvm0211 %>%
  select(party,lepr) %>%
  group_by(party) %>%
  summarise(
    lepr = mean(lepr, na.rm = TRUE))
cvvm1$year <- 2002
cvvm <- rbind(cvvm,cvvm1)
rm(cvvm1)
cvvm9811 <- read.spss("data/correlations/ivvm_cvvm/V9811_F1.sav", to.data.frame=TRUE, reencode='windows-1250') # 1998
cvvm9811 <- cvvm9811[cvvm9811$vek>=18,]
cvvm9811$party <- cvvm9811$p4
cvvm9811$lepr <- as.integer(cvvm9811$p26)
cvvm9811$lepr[cvvm9811$lepr==99] <- NA
cvvm9811$lepr[cvvm9811$lepr==8] <- NA
plot(density(cvvm9811$lepr[!is.na(cvvm9811$lepr)]))
cvvm1 <- cvvm9811 %>%
  select(party,lepr) %>%
  group_by(party) %>%
  summarise(
    lepr = mean(lepr, na.rm = TRUE))
cvvm1$year <- 1998
cvvm <- rbind(cvvm,cvvm1)
rm(cvvm1)
cvvm9612 <- read.spss("data/correlations/ivvm_cvvm/V9612_F1.sav", to.data.frame=TRUE, reencode='windows-1250') # 1996
cvvm9612 <- cvvm9612[cvvm9612$vek>=18,]
cvvm9612$party <- cvvm9612$p31
cvvm9612$lepr <- as.integer(cvvm9612$p32)
cvvm9612$lepr[cvvm9612$lepr==99] <- NA
cvvm9612$lepr[cvvm9612$lepr==8] <- NA
plot(density(cvvm9612$lepr[!is.na(cvvm9612$lepr)]))
cvvm1 <- cvvm9612 %>%
  select(party,lepr) %>%
  group_by(party) %>%
  summarise(
    lepr = mean(lepr, na.rm = TRUE))
cvvm1$year <- 1996
cvvm <- rbind(cvvm,cvvm1)
rm(cvvm1)
cvvm9311 <- read.spss("data/correlations/ivvm_cvvm/V9311_F1.sav", to.data.frame=TRUE, reencode='windows-1250') # 1993
cvvm9311 <- cvvm9311[cvvm9311$vek>=18,]
cvvm9311$party <- cvvm9311$p3
cvvm9311$lepr <- as.integer(cvvm9311$p35)
cvvm9311$lepr[cvvm9311$lepr==99] <- NA
cvvm9311$lepr[cvvm9311$lepr==8] <- NA
plot(density(cvvm9311$lepr[!is.na(cvvm9311$lepr)]))
cvvm1 <- cvvm9311 %>%
  select(party,lepr) %>%
  group_by(party) %>%
  summarise(
    lepr = mean(lepr, na.rm = TRUE))
cvvm1$year <- 1993
cvvm <- rbind(cvvm,cvvm1)
rm(cvvm1)
cvvm$party <- as.character(cvvm$party)
cvvm$party[cvvm$party=="KDU-ČSL"] <- "KDU-CSL"
cvvm$party[cvvm$party=="ČSSD"] <- "CSSD"
cvvm$party[cvvm$party=="KSČM"] <- "KSCM"
cvvm$party[cvvm$party=="Strana zelených"] <- "SZ"
cvvm$party[cvvm$party=="Strana Zelených"] <- "SZ"
cvvm$party[cvvm$party=="Věci veřejné"] <- "VV"
cvvm$party[cvvm$party=="Úsvit přímé demokracie"] <- "USVIT"
cvvm$party[cvvm$party=="TOP 09"] <- "TOP09"
cvvm$party[cvvm$party=="ANO"] <- "ANO2011"
cvvm$party[cvvm$party=="HSDMS"] <- "HSD-SMS"
cvvm$party[cvvm$party=="MNS-HSMS"] <- "HSD-SMS"
cvvm$party[cvvm$party=="Úsvit - Národní koalice"] <- "USVIT"





#############################
####### Correlations ########
#############################

test <- function(year1,data,var1,var2){ # create a function for correlations
  cor <- cor(data[data$year==year1,c(var1,var2)],use="complete.obs", method="pearson")
  return(cor[2])
}


final <- as.data.frame(1993:2017) # create dataset for results
colnames(final) <- "year"


# dynamic model versus Chapel Hill Expert Survey
data_ches <- merge(data, ches[,c("party","year","lrgen","lrecon")],
                   by = c("party","year")) # select the varibles to be correlated
for(i in 1:length(unique(data_ches$year))){
  final$ches_lrgen[final$year==unique(data_ches$year)[i]] <- test(unique(data_ches$year)[i],data_ches,"mean","lrgen")[1]
}
for(i in 1:length(unique(data_ches$year))){
  final$ches_lrecon[final$year==unique(data_ches$year)[i]] <- test(unique(data_ches$year)[i],data_ches,"mean","lrecon")[1]
}


# dynamic model versus Comparative Manifesto Project
data_cmp <- merge(data, cmp[,c("party","year","rile","markeco","welfare","intpeace")],
                  by = c("party","year")) # select the varibles to be correlated
data_cmp <- data_cmp[data_cmp$year>=1993,]
#data_cmp <- data_cmp[data_cmp$party!="SPR-RSC",] # potentially exclude the right-wing SPR-RSC
for(i in 1:length(unique(data_cmp$year))){
  final$cmp_rile[final$year==unique(data_cmp$year)[i]] <- test(unique(data_cmp$year)[i],data_cmp,"mean","rile")[1]
}
for(i in 1:length(unique(data_cmp$year))){
  final$cmp_markeco[final$year==unique(data_cmp$year)[i]] <- test(unique(data_cmp$year)[i],data_cmp,"mean","markeco")[1]
}
for(i in 1:length(unique(data_cmp$year))){
  final$cmp_welfare[final$year==unique(data_cmp$year)[i]] <- test(unique(data_cmp$year)[i],data_cmp,"mean","welfare")[1]
}
for(i in 1:length(unique(data_cmp$year))){
  final$cmp_intpeace[final$year==unique(data_cmp$year)[i]] <- test(unique(data_cmp$year)[i],data_cmp,"mean","intpeace")[1]
}


# dynamic model versus IVVM/CVVM
data_cvvm <- merge(data, cvvm[,c("party","year","lepr")],
                   by = c("party","year")) # select the varibles to be correlated
#data_cvvm <- data_cvvm[data_cvvm$party!="SPR-RSC",] # potentially exclude the right-wing SPR-RSC
for(i in 1:length(unique(data_cvvm$year))){
  final$cvvm_lepr[final$year==unique(data_cvvm$year)[i]] <- test(unique(data_cvvm$year)[i],data_cvvm,"mean","lepr")[1]
}





####################################
########## OUTPUT ##################
####################################

rownames(final) <- as.character(final[,1])
final <- final[,2:ncol(final)]
final <- round(final, 3)
final <- final[!(rowSums(is.na(final[,2:ncol(final)]))==NCOL(final[,2:ncol(final)])),] 
final <- t(final)
final <- format(final, digits = 3)
final[final=="    NA"] <- ""
write.xlsx(final, "correlations.xlsx", row.names = T, col.names = T, sheetName = "correl")





####################################
########## Autocorrelation #########
####################################

final1 <- as.data.frame(matrix(0,length(unique(data$year)),length(unique(data$year))))
colnames(final1) <- sort(unique(data$year))
rownames(final1) <- sort(unique(data$year))
data1 <- as.data.frame(cast(data, party ~ year))
data1 <- data1[,c(2,5,7,11,15,19,22,26)]
final1 <- final1[,c(1,4,6,10,14,18,21,25)]
final1 <- final1[c(1,4,6,10,14,18,21,25),]
for(k in 1:8){
  for(i in 1:8){
    if (unname(tail(table(as.numeric(!is.na(data1[,k])) + as.numeric(!is.na(data1[,i]))), n=1)>=3))
      final1[k,i] <- rcorr(data1[,k], data1[,i], type = "pearson")$r[2]
    else
      final1[k,i] <- 0
  }
}
final1 <- format(round(final1, 3), nsmall = 3)
write.xlsx(final1, "correlations.xlsx", row.names = T, col.names = T, sheetName = "auto",append=TRUE) # create the output




