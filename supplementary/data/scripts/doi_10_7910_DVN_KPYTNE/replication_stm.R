# Appendix 4 #

rm(list = ls())
require(quanteda)
require(quanteda.corpora)
require(quanteda.textmodels)
library(readtext)
library(tm)
require(topicmodels)
require(stm)
require(lubridate)
library(dplyr)
library(corrplot)
library(proxyC)
library(topicmodels)
library(stringr)
library(igraph)



stm_cab_20 <- stm(dfmat, K=20, data=metadata, seed=123)
summary(stm_cab_20)

