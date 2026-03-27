#!/usr/bin/Rscript
#################################################################
# Social Media and Political Agenda Setting (Classified Press Releases Merging)
#################################################################
# Content
#################################################################
# 1) Dependencies
#################################################################
rm(list=ls())
#Libraries
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
#################################################################
# 2) Read Data filter for languages other than german and french
#################################################################
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

df <- readRDS("../data/pressreleases_to_classify.RDS")
df <- df %>% filter(!la %in% c("fr", "de")) %>% rename("text" = "Text")
df <- df %>% dplyr::select(-c(X_type,X_index,X_score,X_id))
#################################################################
# 3) Add Col selects class and makre them as unclassified!
#################################################################
df$selectsclass <- "Not Classified"
#################################################################
# 4) Wirte Out data:
write_rds(df, "../data/PRESS_CLASSIFIED_OTHER.RDS")
#################################################################
# 5) Combine the three parts with each other
#################################################################
df <- readRDS("../data/PRESS_CLASSIFIED_OTHER.RDS")
df1 <- df
df <- readRDS("../data/PRESS_CLASSIFIED_FR.RDS")
df2<- df
df <- readRDS("../data//PRESS_CLASSIFIED_DE.RDS")
df3 <- df
rm(df)

df <- rbind(df1,df2,df3)
df <- df %>% rename("tx" = "text") %>% rename("pubDateTime" = "Datum") %>% rename("url" = "Link")

#Min Date / Max Date
minday <- min(df$pubDateTime)
maxday <- max(df$pubDateTime)

filename <- paste0("pressreleases_2018-2019.RDS")

df <- df %>% mutate(id = row_number()) %>% select(c("id","pubDateTime", "Akteur", "Kürzel", "la", "selectsclass"))

finaldir <- "../data"
setwd(paste0(finaldir))
write_rds(df, filename)




                