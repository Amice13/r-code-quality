###############################################
#model used for daily accounted crimes
###############################################
#clear all variables in workspace
rm(list=ls()) 
#load libraries
#library(readxl) 
#library(forecast)
#library(fpp2)
#library(tidyverse)
#library(dplyr)
#library(ggpubr)
#library(car)
#library(ggsci)
#library(ggplot2)
#library(ggstatsplot)


###############################################
#read DB in a dataset named 'bd'

#csv from Macbook
bd<- read.csv(header = TRUE, sep = ",", "/Users/fred/Desktop/BH/2-Entry.csv")

#csv from Windows
#bd<- read.csv("C:/Users/Frederico/Desktop/BH/2-Entry.csv")


###############################################
#Descriptive stats
###############################################
summary(bd)


###############################################
#Creat Descriptive report
library(pastecs)
options(scipen = 100, digits = 4) #to prevent use of exponential notation (e.g. e+10) 

descriptive<-stat.desc(bd[,2:10], basic=TRUE,desc=TRUE,norm=TRUE)
num <-format(round(descriptive, 0), nsmall = 0)  # round numbers to no digits
rownames(num) [1] <- "n"
rownames(num) [2] <- "null"
rownames(num) [3] <- "missing"

#Shapiro-Wilk (n<50)
norms <- signif(descriptive[20,], digits = 4)# maintain digits in SW p values
rownames(norms) [1] <- "Shapiro-Wilk"



#kolmogorov-Smirnov (n>50)
normk<-data.frame("R"="0.95","T"="0.95","H"="0.95","RW"="0.95","RV"="0.95","RH"="0.95","TW"="0.95","TV"="0.95","TH"="0.95")
normk[2,1]<- signif(ks.test(bd[,2],"pnorm",mean(bd[,2]),sd(bd[,2]))$p.value, digits = 4)
normk[2,2]<- signif(ks.test(bd[,3],"pnorm",mean(bd[,3]),sd(bd[,3]))$p.value, digits = 4)
normk[2,3]<- signif(ks.test(bd[,4],"pnorm",mean(bd[,4]),sd(bd[,4]))$p.value, digits = 4)
normk[2,4]<- signif(ks.test(bd[,5],"pnorm",mean(bd[,5]),sd(bd[,5]))$p.value, digits = 4)
normk[2,5]<- signif(ks.test(bd[,6],"pnorm",mean(bd[,6]),sd(bd[,6]))$p.value, digits = 4)
normk[2,6]<- signif(ks.test(bd[,7],"pnorm",mean(bd[,7]),sd(bd[,7]))$p.value, digits = 4)
normk[2,7]<- signif(ks.test(bd[,8],"pnorm",mean(bd[,8]),sd(bd[,8]))$p.value, digits = 4)
normk[2,8]<- signif(ks.test(bd[,9],"pnorm",mean(bd[,9]),sd(bd[,9]))$p.value, digits = 4)
normk[2,9]<- signif(ks.test(bd[,10],"pnorm",mean(bd[,10]),sd(bd[,10]))$p.value, digits = 4)

rownames(normk) [1] <- "Norm Test CI"
rownames(normk) [2] <- "Kolmogorov"


#Anderson-Darling (secondary normality test)
library(nortest)
norma<-data.frame("R"="0.95","T"="0.95","H"="0.95","RW"="0.95","RV"="0.95","RH"="0.95","TW"="0.95","TV"="0.95","TH"="0.95")
norma[2,1]<-signif(as.data.frame(ad.test(bd[,2])$p.value),digits = 4)
norma[2,2]<-signif(as.data.frame(ad.test(bd[,3])$p.value),digits = 4)
norma[2,3]<-signif(as.data.frame(ad.test(bd[,4])$p.value),digits = 4)
norma[2,4]<-signif(as.data.frame(ad.test(bd[,5])$p.value),digits = 4)
norma[2,5]<-signif(as.data.frame(ad.test(bd[,6])$p.value),digits = 4)
norma[2,6]<-signif(as.data.frame(ad.test(bd[,7])$p.value),digits = 4)
norma[2,7]<-signif(as.data.frame(ad.test(bd[,8])$p.value),digits = 4)
norma[2,8]<-signif(as.data.frame(ad.test(bd[,9])$p.value),digits = 4)
norma[2,9]<-signif(as.data.frame(ad.test(bd[,10])$p.value),digits = 4)

rownames(norma) [1] <- "Norm Test CI"
rownames(norma) [2] <- "Anderson-Darling"


#Combine all descriptive in one single report

library(dplyr)
descriptive1 <-num %>% slice(1:7)
descriptive2 <-descriptive %>% slice(8:15)
descriptive3 <-descriptive %>% slice(17:17)
norma <-norma %>% slice(2:2)

descriptive <- rbind(descriptive1, descriptive2)
descriptive <- rbind(descriptive, descriptive3)
descriptive <- rbind(descriptive, normk)
descriptive <- rbind(descriptive, norms)
descriptive <- rbind(descriptive, norma)

rm(descriptive1,descriptive2,descriptive3,num,norma,normk,norms)

#Creating Normality dummy
descriptive[21,1]<-if(max(descriptive[18:20,1])>0.05) {"FALSE"} else {"TRUE"}
descriptive[21,2]<-if(max(descriptive[18:20,2])>0.05) {"FALSE"} else {"TRUE"}
descriptive[21,3]<-if(max(descriptive[18:20,3])>0.05) {"FALSE"} else {"TRUE"}
descriptive[21,4]<-if(max(descriptive[18:20,4])>0.05) {"FALSE"} else {"TRUE"}
descriptive[21,5]<-if(max(descriptive[18:20,5])>0.05) {"FALSE"} else {"TRUE"}
descriptive[21,6]<-if(max(descriptive[18:20,6])>0.05) {"FALSE"} else {"TRUE"}
descriptive[21,7]<-if(max(descriptive[18:20,7])>0.05) {"FALSE"} else {"TRUE"}
descriptive[21,8]<-if(max(descriptive[18:20,8])>0.05) {"FALSE"} else {"TRUE"}
descriptive[21,9]<-if(max(descriptive[18:20,9])>0.05) {"FALSE"} else {"TRUE"}

rownames(descriptive) [21] <- "Data Normality"

###############################################
#Outlier detection
###############################################

#Grubbs test  to detect whether the highest or lowest value in a dataset is an outlier
#p-value <0.05, the lowest/highest value is an outlier X p-value >=0.5 lowest/highest value is not an outlier.
library(outliers)
outliers<-data.frame("R"="0.95","T"="0.95","H"="0.95","RW"="0.95","RV"="0.95","RH"="0.95","TW"="0.95","TV"="0.95","TH"="0.95")
outliers[2,1]<-signif(max(grubbs.test(bd[,2])$p.value), digits = 4)
outliers[2,2]<-signif(max(grubbs.test(bd[,3])$p.value), digits = 4)
outliers[2,3]<-signif(max(grubbs.test(bd[,4])$p.value), digits = 4)
outliers[2,4]<-signif(max(grubbs.test(bd[,5])$p.value), digits = 4)
outliers[2,5]<-signif(max(grubbs.test(bd[,6])$p.value), digits = 4)
outliers[2,6]<-signif(max(grubbs.test(bd[,7])$p.value), digits = 4)
outliers[2,7]<-signif(max(grubbs.test(bd[,8], type = 11)$p.value), digits = 4)
outliers[2,8]<-signif(max(grubbs.test(bd[,9])$p.value), digits = 4)
outliers[2,9]<-signif(max(grubbs.test(bd[,10])$p.value), digits = 4)
outliers[3,1]<-signif(max(grubbs.test(bd[,2], opposite = TRUE)$p.value), digits = 4)
outliers[3,2]<-signif(max(grubbs.test(bd[,3], opposite = TRUE)$p.value), digits = 4)
outliers[3,3]<-signif(max(grubbs.test(bd[,4], opposite = TRUE)$p.value), digits = 4)
outliers[3,4]<-signif(max(grubbs.test(bd[,5], opposite = TRUE)$p.value), digits = 4)
outliers[3,5]<-signif(max(grubbs.test(bd[,6], opposite = TRUE)$p.value), digits = 4)
outliers[3,6]<-signif(max(grubbs.test(bd[,7], opposite = TRUE)$p.value), digits = 4)
outliers[3,7]<-signif(max(grubbs.test(bd[,8], opposite = TRUE)$p.value), digits = 4)
outliers[3,8]<-signif(max(grubbs.test(bd[,9], opposite = TRUE)$p.value), digits = 4)
outliers[3,9]<-signif(max(grubbs.test(bd[,10], opposite = TRUE)$p.value), digits = 4)
outliers[4,1]<-if(min(outliers[2:3,1])<0.05) {"TRUE"} else {"FALSE"}
outliers[4,2]<-if(min(outliers[2:3,2])<0.05) {"TRUE"} else {"FALSE"}
outliers[4,3]<-if(min(outliers[2:3,3])<0.05) {"TRUE"} else {"FALSE"}
outliers[4,4]<-if(min(outliers[2:3,4])<0.05) {"TRUE"} else {"FALSE"}
outliers[4,5]<-if(min(outliers[2:3,5])<0.05) {"TRUE"} else {"FALSE"}
outliers[4,6]<-if(min(outliers[2:3,6])<0.05) {"TRUE"} else {"FALSE"}
outliers[4,7]<-if(min(outliers[2:3,7])<0.05) {"TRUE"} else {"FALSE"}
outliers[4,8]<-if(min(outliers[2:3,8])<0.05) {"TRUE"} else {"FALSE"}
outliers[4,9]<-if(min(outliers[2:3,9])<0.05) {"TRUE"} else {"FALSE"}

rownames(outliers) [1] <- "Grubbs CI"
rownames(outliers) [2] <- "Grubbs p H"
rownames(outliers) [3] <- "Grubbs p L"
rownames(outliers) [4] <- "Outliers"

grubbs.test(bd[,8], type = 11)
descriptive <- rbind(descriptive, outliers)
rm(outliers)

library(robustX)
library(dplyr)
substrRight <- function(x, n){substr(x, nchar(x)-n+1, nchar(x))} #creates function "substrRight" to get last char

outliers<-data.frame("R"="0.95","T"="0.95","H"="0.95","RW"="0.95","RV"="0.95","RH"="0.95","TW"="0.95","TV"="0.95","TH"="0.95")
t<-as.character(capture.output(BACON(bd[,2],  init.sel = "V2", alpha = 0.05, verbose = TRUE)))
out<-as.data.frame(capture.output(BACON(bd[,2],  init.sel = "V2", alpha = 0.05, verbose = TRUE)))
outliers[2,1]<-substrRight(out %>% slice(max(grep('^M', t)):max(grep('^M', t))), 8)
outliers[3,1]<-grepl((substrRight(out %>% slice(max(grep('^M', t)):max(grep('^M', t))), 8))," (100 %)", fixed = TRUE)

t<-as.character(capture.output(BACON(bd[,3],  init.sel = "V2", alpha = 0.05, verbose = TRUE)))
out<-as.data.frame(capture.output(BACON(bd[,3],  init.sel = "V2", alpha = 0.05, verbose = TRUE)))
outliers[2,2]<-substrRight(out %>% slice(max(grep('^M', t)):max(grep('^M', t))), 8)
outliers[3,2]<-grepl((substrRight(out %>% slice(max(grep('^M', t)):max(grep('^M', t))), 8))," (100 %)", fixed = TRUE)

t<-as.character(capture.output(BACON(bd[,4],  init.sel = "V2", alpha = 0.05, verbose = TRUE)))
out<-as.data.frame(capture.output(BACON(bd[,4],  init.sel = "V2", alpha = 0.05, verbose = TRUE)))
outliers[2,3]<-substrRight(out %>% slice(max(grep('^M', t)):max(grep('^M', t))), 8)
outliers[3,3]<-grepl((substrRight(out %>% slice(max(grep('^M', t)):max(grep('^M', t))), 8))," (100 %)", fixed = TRUE)

t<-as.character(capture.output(BACON(bd[,5],  init.sel = "V2", alpha = 0.05, verbose = TRUE)))
out<-as.data.frame(capture.output(BACON(bd[,5],  init.sel = "V2", alpha = 0.05, verbose = TRUE)))
outliers[2,4]<-substrRight(out %>% slice(max(grep('^M', t)):max(grep('^M', t))), 8)
outliers[3,4]<-grepl((substrRight(out %>% slice(max(grep('^M', t)):max(grep('^M', t))), 8))," (100 %)", fixed = TRUE)

t<-as.character(capture.output(BACON(bd[,6],  init.sel = "V2", alpha = 0.05, verbose = TRUE)))
out<-as.data.frame(capture.output(BACON(bd[,6],  init.sel = "V2", alpha = 0.05, verbose = TRUE)))
outliers[2,5]<-substrRight(out %>% slice(max(grep('^M', t)):max(grep('^M', t))), 8)
outliers[3,5]<-grepl((substrRight(out %>% slice(max(grep('^M', t)):max(grep('^M', t))), 8))," (100 %)", fixed = TRUE)

t<-as.character(capture.output(BACON(bd[,7],  init.sel = "V2", alpha = 0.05, verbose = TRUE)))
out<-as.data.frame(capture.output(BACON(bd[,7],  init.sel = "V2", alpha = 0.05, verbose = TRUE)))
outliers[2,6]<-substrRight(out %>% slice(max(grep('^M', t)):max(grep('^M', t))), 8)
outliers[3,6]<-grepl((substrRight(out %>% slice(max(grep('^M', t)):max(grep('^M', t))), 8))," (100 %)", fixed = TRUE)

t<-as.character(capture.output(BACON(bd[,8],  init.sel = "V2", alpha = 0.05, verbose = TRUE)))
out<-as.data.frame(capture.output(BACON(bd[,8],  init.sel = "V2", alpha = 0.05, verbose = TRUE)))
outliers[2,7]<-substrRight(out %>% slice(max(grep('^M', t)):max(grep('^M', t))), 8)
outliers[3,7]<-grepl((substrRight(out %>% slice(max(grep('^M', t)):max(grep('^M', t))), 8))," (100 %)", fixed = TRUE)

t<-as.character(capture.output(BACON(bd[,9],  init.sel = "V2", alpha = 0.05, verbose = TRUE)))
out<-as.data.frame(capture.output(BACON(bd[,9],  init.sel = "V2", alpha = 0.05, verbose = TRUE)))
outliers[2,8]<-substrRight(out %>% slice(max(grep('^M', t)):max(grep('^M', t))), 8)
outliers[3,8]<-grepl((substrRight(out %>% slice(max(grep('^M', t)):max(grep('^M', t))), 8))," (100 %)", fixed = TRUE)

t<-as.character(capture.output(BACON(bd[,10],  init.sel = "V2", alpha = 0.05, verbose = TRUE)))
out<-as.data.frame(capture.output(BACON(bd[,10],  init.sel = "V2", alpha = 0.05, verbose = TRUE)))
outliers[2,9]<-substrRight(out %>% slice(max(grep('^M', t)):max(grep('^M', t))), 8)
outliers[3,9]<-grepl((substrRight(out %>% slice(max(grep('^M', t)):max(grep('^M', t))), 8))," (100 %)", fixed = TRUE)

rownames(outliers) [1] <- "BACON CI"
rownames(outliers) [2] <- "Values under limit"
rownames(outliers) [3] <- "Outliers"


outliers<-as.data.frame(t(as.data.frame(t(outliers)) %>%
  mutate(
    Outliers = case_when(
      Outliers == "TRUE"        ~ "FALSE",
      TRUE                      ~ "TRUE"))))

descriptive <- rbind(descriptive, outliers)

rownames(descriptive) [28] <- "Outliers Robust"


rm(outliers, out, t, substrRight)            
#export results
write.csv(descriptive, file = "/Users/fred/Desktop/BH/4.1-descriptive.csv")
#write.csv(outliers, file = "C:/Users/Frederico/Desktop/BH/4.1-descriptive.csv")

