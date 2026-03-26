#!/usr/bin/Rscript
##################################################################################################
# Social Media and Political Agenda Setting (Classified Tweets Merging)
##################################################################################################
# Content
##################################################################################################
# 1) Dependencies
##################################################################################################
rm(list=ls())
library(rjson)
library(jsonlite)
library(readr)
library(data.table)
library(dplyr)
library(parallel)
library(doParallel)
library(iterators)
library(stringr)
library(elastic)
##################################################################################################
# 2) Read Data filter for languages other than german and french
##################################################################################################
# Directories:
args = commandArgs()

scriptName = args[substr(args,1,7) == '--file=']

if (length(scriptName) == 0) {
  scriptName <- rstudioapi::getSourceEditorContext()$path
} else {
  scriptName <- substr(scriptName, 8, nchar(scriptName))
}

pathName = substr(
  scriptName, 
  1, 
  nchar(scriptName) - nchar(strsplit(scriptName, '.*[/|\\]')[[1]][2])
)

setwd(pathName)
parent_path <- getwd()

setwd("../data")
df <- readRDS("Tweets_2018_2019_curated_sentiment.rds")
df <- df %>% filter(!la %in% c("fr", "de", "it")) %>% rename("text" = "Text")
##################################################################################################
# 3) Add Col selects class and makre them as unclassified!
##################################################################################################
df$selectsclass <- "Not_Classified"
##################################################################################################
# 4) Wirte Out data:
write_rds(df, "Twitter_CLASSIFIED_OTHER.RDS")
##################################################################################################
# 5) Combine the three parts with each other
##################################################################################################
df <- readRDS("Twitter_CLASSIFIED_OTHER.RDS")
df1 <- df
df <- readRDS("Twitter_CLASSIFIED_FR.RDS")
df2<- df
df <- readRDS("Twitter_CLASSIFIED_DE.RDS")
df3 <- df
rm(df)

df <- rbind(df1,df2,df3)
df <- df %>% rename("Text" = "text")

#Min Date / Max Date
minday <- min(df$Datum)
maxday <- max(df$Datum)

countdf <- df %>% group_by(selectsclass) %>% summarise(n = n())

df <- df %>% dplyr::select(c("Status_id","User_id","Datum","Akteur.Typ","Party","la","First_Name",
                             "Last_Name","Canton","Gender","Is_quote","Is_retweet","Verified",
                             "sentiment_value","selectsclass"))

filename <- paste0("Tweets_2018_2019_curated_sentiment_class.RDS")
write_rds(df, filename)
##################################################################################################

                