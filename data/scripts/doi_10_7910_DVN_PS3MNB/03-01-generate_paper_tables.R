#!/usr/bin/Rscript
##########################################################################################
# Social Media and Political Agenda Setting
##########################################################################################
# Description:
# This Script contains all the code necessary to produce the tables presented in the paper
# except the classifier performance tables.
##########################################################################################
# Content
##########################################################################################
# 1) Dependencies
# 2) Load Data
# 3) Transform Data
# 4) Make Tables
##########################################################################################
# 1) Dependencies
##########################################################################################
rm(list = ls())
library(dplyr)
library(tidyverse)
library(data.table)
library(readr)
library(lubridate)
library(purrr)
library(magrittr)
library(vars)
library(boot)
library(rio)
library(stargazer)

# - set dir
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

# - define a global seed (used in all scripts)
set.seed(2019)

# - remove retweets (TRUE)
retweet_out <- TRUE
##########################################################################################
# 2) Load Data
##########################################################################################
# - load raw data while removing some topics we do not use in the end 
# - load press releases
press <- read_rds("../data/pressreleases_2018-2019.RDS")
# - load media articles
smd <- read_rds("../data/smd_minified_2018-2019.RDS")
# - load tweets
tweets <- read_rds("../data/Tweets_2018_2019_curated_sentiment_class.RDS") 
##########################################################################################
# 3) Transform Data
##########################################################################################
## 3.1) Some minor transformations
##########################################################################################
# rename paper names (so_txt)
smd <- smd %>% 
  mutate(so_txt = case_when(
    so_txt %in% c("20 minuten online", "20 minutes","20 minuti") ~ "20 minuten", 
    so_txt %in% c("Newsnet / 24 heures") ~ "24 heures",
    so_txt %in% c("Newsnet / Basler Zeitung") ~ "Basler Zeitung",
    so_txt %in% c("Newsnet / Berner Zeitung") ~ "Berner Zeitung",
    so_txt %in% c("Newsnet / Der Bund") ~ "Der Bund",
    so_txt %in% c("Newsnet / Le Matin") ~ "Le Matin",
    so_txt %in% c("Newsnet / Tribune de Genève", "Tribune de Genève") ~ "Tribune de Genève",
    so_txt %in% c("Newsnet / Tages-Anzeiger") ~ "Tages-Anzeiger",
    so_txt %in% c("Handelszeitung online") ~ "Handelszeitung",
    so_txt %in% c("rts.ch", "RTS.ch") ~ "srf.ch",
    so_txt %in% c("SWI swissinfo.ch") ~ "swissinfo.ch",
    so_txt %in% c("Finanz und Wirtschaft Online") ~ "Finanz und Wirtschaft",
    so_txt %in% c("Anzeigen von Uster", "Anzegier von Uster") ~ "Anzeiger von Uster",
    so_txt %in% c("L'Agefi") ~ "Agefi",
    so_txt %in% c("Aargauer Zeitung", "Aargauer Zeitung / MLZ") ~ "Aargauer Zeitung",
    so_txt %in% c("Migros-Magazin", "Migros Magazine") ~ "Migros-Magazin",
    so_txt %in% c("Cooperazione", "Coopzeitung", "Coopération") ~ "Coopzeitung",
    so_txt %in% c("L'Express / L'Impartial", "Arcinfo") ~ "Arcinfo",
    TRUE ~ so_txt
  ))

# - remove all retweets from tweets
if(retweet_out == T){
  tweets_full <- tweets
  tweets <- tweets %>% filter(Is_retweet != T)
}

# - select parties of interest and all other accounts from tweets
tweets <- tweets %>% 
  filter(Party %in% c("grÜne (basels starke alternative)", "grÜnliberale partei", 
                      "sozialdemokratische partei der schweiz", "alternative - die grÜnen zug",
                      "schweizerische volkspartei", "fdp.die liberalen", 
                      "christlichsoziale volkspartei oberwallis",
                      "grÜne partei der schweiz", "christlich-soziale partei",
                      "christdemokratische volkspartei der schweiz",
                      "christlichdemokratische volkspartei der schweiz",
                      "bÜrgerlich-demokratische partei schweiz", "NA", NA)) %>% 
  mutate(Party = case_when(Party %in% c("grÜne (basels starke alternative)", 
                                        "grÜne partei der schweiz", 
                                        "alternative - die grÜnen zug") ~ "Grüne",
                           Party %in% c("sozialdemokratische partei der schweiz") ~ "SP",
                           Party %in% c("schweizerische volkspartei") ~ "SVP",
                           Party %in% c("fdp.die liberalen") ~ "FDP",
                           Party %in% c("christdemokratische volkspartei der schweiz",
                                        "christlichdemokratische volkspartei der schweiz",
                                        "christlich-soziale partei", 
                                        "christlichsoziale volkspartei oberwallis") ~ "CVP",
                           Party %in% c("grÜnliberale partei") ~ "GLP",
                           Party %in% c("bÜrgerlich-demokratische partei schweiz") ~"BDP",
                           TRUE ~ Party))
##########################################################################################
# Twitter Tables 
##########################################################################################
tweets2 <- tweets %>% group_by(Akteur.Typ, Party, User_id) %>% 
  slice(1) %>% 
  summarise(n =n()) %>% 
  group_by(Akteur.Typ, Party) %>% 
  summarise(n = n())

tweets3 <- tweets %>% group_by(Akteur.Typ, Party) %>% summarise(n = n())

tweets4 <- tweets3 %>% group_by(Akteur.Typ) %>% mutate(nn = sum(n))

# Table 2:
tweets_stat <- tweets %>% filter(Akteur.Typ %in% c("Party", "Person")) %>% filter(Party != "NA") %>%
  group_by(Akteur.Typ, Party, User_id) %>% summarise(n = n()) %>% 
  ungroup %>% group_by(Akteur.Typ, Party) %>% 
  summarise(N = n_distinct(User_id), AVG = round(mean(n), digits = 2), min = min(n), max = max(n),  Tweets = sum(n))

stargazer::stargazer(tweets_stat, summary = F, type = "latex", out = "../tables/table_01.tex", header = F)
stargazer::stargazer(tweets_stat, summary = F, type = "html", out = "../tables/table_01.html", header = F)

# Table 2:
tweets_stat <- tweets %>% filter(Akteur.Typ != "Institute") %>% 
  mutate(Akteur.Typ = recode(Akteur.Typ, Departement = "Government", Administration = "Government")) %>% 
  group_by(Akteur.Typ, User_id) %>% summarise(n = n()) %>% 
  summarise(N = n_distinct(User_id), AVG = round(mean(n),digits = 2), min = min(n), max = max(n),  Tweets = sum(n))

stargazer::stargazer(tweets_stat, summary = F, type = "latex", out = "../tables/table_02.tex", header = F)
stargazer::stargazer(tweets_stat, summary = F, type = "html", out = "../tables/table_02.html", header = F)
##########################################################################################
# SMD Table
##########################################################################################
# Table 3
smd_stat <- smd %>% group_by(so_txt, pubDateTime) %>% 
  summarise(n = n()) %>% 
  group_by(so_txt) %>% 
  summarise(Articles = sum(n), AVG = mean(n), min = min(n), max = max(n))

smd_stat$so_txt <- ifelse(smd_stat$so_txt == "Werdenberger & Obertoggenburger", "Werdenberger / Obertoggenburger", smd_stat$so_txt)

smd_stat <- smd_stat %>% mutate_if(is.numeric, round, 2)

stargazer::stargazer(smd_stat, summary = F, type = "latex", out = "../tables/table_03.tex", header = F, digits = 2)
stargazer::stargazer(smd_stat, summary = F, type = "html", out = "../tables/table_03.html", header = F, digits = 2)

tweets <- tweets %>% dplyr::select(-c(X_index, X_type, X_id, X_score, X_ignored)) %>% 
  dplyr::filter(!selectsclass %in% c("Not_Classified", "Elections", "Poll")) %>%
  dplyr::mutate(selectsclass = ifelse(selectsclass == "EU_Europe", "EU_Europa", selectsclass))

smd <- smd %>% filter(!selectsclass %in% c("Not Classified", "NotPolitical", "Other_Problems", "Other_unclassified_Political_Texts"))
##########################################################################################
