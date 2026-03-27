### JCR US CES survey replication

## Data loading 
rm(list = ls(all = TRUE))

library(readxl)
library(tidyverse)
library(dplyr)
library(stargazer)
library(foreign)
library(MASS)

## load the data
d167<-read_xlsx("/Users/victorxu/Desktop/WWD/Journal submission/JCR/Accepted/Replication dataset/Harvard dataset/JCR_US_CES.xlsx")

## Replication of Figure A11


## Limiting trade 
robust1.1<- polr(as.factor(trade)~tr1+age+male+educ+income, data=d167,Hess = TRUE)## pride
summary(robust1.1)
robust1.2<- polr(as.factor(trade)~tr2+age+male+educ+income, data=d167,Hess = TRUE)## Humiliation
summary(robust1.2)
robust1.3<- polr(as.factor(trade)~tr3+age+male+educ+income, data=d167,Hess = TRUE)## defamatory
summary(robust1.3)

## limiting China growth 
robust2.1<- polr(as.factor(growth)~tr1+age+male+educ+income, data=d167,Hess = TRUE)## pride
summary(robust2.1)
robust2.2<- polr(as.factor(growth)~tr2+age+male+educ+income, data=d167,Hess = TRUE)## Humiliation
summary(robust2.2)
robust2.3<- polr(as.factor(growth)~tr3+age+male+educ+income, data=d167,Hess = TRUE)## defamatory
summary(robust2.3)


## Promoting Dialouge 
robust3.1<- polr(as.factor(diplomacy)~tr1+age+male+educ+income, data=d167,Hess = TRUE)## pride
summary(robust3.1)
robust3.2<- polr(as.factor(diplomacy)~tr2+age+male+educ+income, data=d167,Hess = TRUE)## Humiliation
summary(robust3.2)
robust3.3<- polr(as.factor(diplomacy)~tr3+age+male+educ+income, data=d167,Hess = TRUE)## defamatory
summary(robust3.3)

library(stargazer)

## Replication of Table A11
stargazer(robust1.1, robust1.2,robust1.3,
          type = "html", 
          title = "",
          covariate.labels = c("Pride rhetoric",
                               "Humiliation rhetoric",
                               "Defamatory rhetoric",
                               "Age",
                               "Male",
                               "Education",
                               "Income"),
          dep.var.caption  = "Support for Reducing Trade between the U.S. and China (Seven Point Scale)",
          dep.var.labels   = "Support for Reducing Trade between the U.S. and China (Seven Point Scale)",
          column.separate = c(4, 1),
          omit.stat=c("LL","aic"), no.space=TRUE,
          align = TRUE,
          style = "qje",
          notes = ("Two-tail significance levels."),
          out="/Users/victorxu/Desktop/WWD/Journal submission/JCR/Accepted/Replication dataset/CES/table_trade.html")

## Replication of Table A12
stargazer(robust2.1, robust2.2,robust2.3,
          type = "html", 
          title = "",
          covariate.labels = c("Pride rhetoric",
                               "Humiliation rhetoric",
                               "Defamatory rhetoric",
                               "Age",
                               "Male",
                               "Education",
                               "Income"),
          dep.var.labels   = "Support for Limiting the Growth of Chinese Power (Seven Point Scale)",
          column.separate = c(4, 1),
          omit.stat=c("LL","aic"), no.space=TRUE,
          align = TRUE,
          style = "qje",
          notes = ("Two-tail significance levels."),
          out="/Users/victorxu/Desktop/WWD/Journal submission/JCR/Accepted/Replication dataset/CES/table_growth.html")

## Replication of Table A13
stargazer(robust3.1, robust3.2,robust3.3,
          type = "html", 
          title = "",
          covariate.labels = c("Pride rhetoric",
                               "Humiliation rhetoric",
                               "Defamatory rhetoric",
                               "Age",
                               "Male",
                               "Education",
                               "Income"),
          dep.var.labels   = "Support for Engaging in Diplomacy with China (Seven Point Scale)",
          column.separate = c(4, 1),
          omit.stat=c("LL","aic"), no.space=TRUE,
          align = TRUE,
          style = "qje",
          notes = ("Two-tail significance levels."),
          out="/Users/victorxu/Desktop/WWD/Journal submission/JCR/Accepted/Replication dataset/CES/diplomacy.html")
