#!/usr/bin/Rscript
##########################################################################################
# Social Media and Political Agenda Setting Data Preparation for VAR
##########################################################################################
# Description:
# This Script contains teh code to transform the data into the format used by barbera et al. 
# for their plots and their model
##########################################################################################
# Content
##########################################################################################
# 1) Dependencies
# 2) Load Data
# 3) Transform Data
## 3.1) Re-code Paper Names in so_txt since there are errors in the smd database
## 3.2) Rename Topic Political science and one of the other Topics to just the other topic
## 3.3) Get Data into Shape
# 4) Save Data 
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
smd <- smd %>% filter(!selectsclass %in% c("Not Classified", "NotPolitical", "Other_Problems", "Other_unclassified_Political_Texts"))
# - load tweets 
tweets <- read_rds("../data/Tweets_2018_2019_curated_sentiment_class.RDS")
tweets <- tweets %>% filter(!selectsclass %in% c("Not_Classified", "Elections", "Poll")) %>%
                     mutate(selectsclass = ifelse(selectsclass == "EU_Europe", "EU_Europa", selectsclass))
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
## 3.2) Rename Topic Political science and one of the other Topics to just the other topic
##########################################################################################
## Double Classifiactions of Political System and something else are recoded to the other 
## highly likly topic, since it is of greater interest to know on what subject the article 
## is on rather than knowing only that it has to do with the political system 
## (eg. election / poll / party)

smd$selectsclass <- gsub(".*,", "", smd$selectsclass)
press$selectsclass <- gsub(".*,", "", press$selectsclass)
tweets$selectsclass <- gsub(".*,", "", tweets$selectsclass)

sort(unique(smd$so_txt))
##########################################################################################
## 3.3) Get Data into Shape
##########################################################################################
# Transform SMD Data: 
smd_ana <- smd %>% group_by(pubDateTime, selectsclass) %>% 
                   summarise(n = n()) %>%
                   mutate(freq = n / sum(n))

# Transform Press Data:
.parties <- c("SVP", "GPS", "CVP","SPS", "FDP", "GLP", "BDP")
press_ana <- press %>% mutate(Akteur_Art = ifelse(Kürzel %in% .parties, "Party", 
                                                  ifelse(Kürzel == "admin.ch", "Gov", "Org"))) %>%
                       dplyr::group_by(pubDateTime, selectsclass, Akteur_Art) %>% 
                       summarise(n = n()) %>% ungroup() %>% dplyr::group_by(Akteur_Art, pubDateTime) %>% 
                       mutate(freq = n / sum(n))

# Tranform Twitter Data 
tweet_ana <- tweets %>% as.data.frame() %>%
             #filter(Akteur.Typ != "Media") %>%
             mutate(Akteur_Art = ifelse(Akteur.Typ == "Party", "Party",
                    ifelse(Akteur.Typ == "Media", "Media", 
                           ifelse(Akteur.Typ == "Person", "Politican", 
                                  ifelse(Akteur.Typ == "Administration", "Gov",
                                         ifelse(Akteur.Typ == "Organisation", "Org", "Other")))))) %>%
             filter(Akteur_Art != "Other") %>%
             dplyr::group_by(Datum, selectsclass, Akteur_Art) %>%
             summarise(n = n()) %>% ungroup() %>% dplyr::group_by(Akteur_Art, Datum) %>% 
             mutate(freq = n / sum(n))

# - add missing dates with NA not 0 
# - will be changed later to 0.01 before loglink transformation to avoid error
smd_ana %<>% ungroup()%>% mutate(pubDateTime = as.Date(pubDateTime)) %>%
             complete(pubDateTime = seq.Date(min(pubDateTime), max(pubDateTime), by = "day"), selectsclass)

press_ana %<>% ungroup()%>% mutate(pubDateTime = as.Date(pubDateTime)) %>%
             complete(pubDateTime = seq.Date(min(pubDateTime), max(pubDateTime), by = "day"), selectsclass, Akteur_Art)

tweet_ana %<>% ungroup() %>% 
               rename(pubDateTime = Datum) %>%
               mutate(pubDateTime = as.Date(pubDateTime)) %>%
               complete(pubDateTime = seq.Date(min(pubDateTime), max(pubDateTime), by = "day"), selectsclass, Akteur_Art)

# - spread data frame with more than one grouping variable besides selectsclass
press_ana_wide <- press_ana %>% dplyr::select(-c("n")) %>% 
 tidyr::spread(Akteur_Art, freq, fill = NA)

tweet_ana_wide <- tweet_ana  %>% dplyr::select(-c("n")) %>% 
  tidyr::spread(Akteur_Art, freq, fill = NA)

smd_ana_wide <- smd_ana %>% dplyr::select(-c("n")) %>% rename(Media_SMD = freq)

# - rename columns for mergeing process
colnames(tweet_ana_wide) <- c("pubDateTime", "selectsclass", "Gov_TW", "Media_TW",
                              "Org_TW", "Party_TW", "Politician_TW")

colnames(press_ana_wide) <- c("pubDateTime", "selectsclass","Gov_PR", "Org_PR", "Party_PR")

# - merge the three data frames:
ana_data_wide <- left_join(smd_ana_wide, press_ana_wide, by = c("pubDateTime", "selectsclass"))
ana_data_wide <- left_join(ana_data_wide, tweet_ana_wide, by = c("pubDateTime", "selectsclass"))
##########################################################################################
# 4) Save Data 
##########################################################################################
saveRDS(ana_data_wide, "../data/main_data_for_paper_all_topics_18_19.RDS")