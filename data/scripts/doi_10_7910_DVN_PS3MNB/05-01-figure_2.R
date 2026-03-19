#!/usr/bin/Rscript
##########################################################################################
# Social Media and Political Agenda Setting
##########################################################################################
# Description:
# Figure 2
##########################################################################################
# Content
##########################################################################################
# 1) Dependencies
# 2) Data Import
# 3) Arrange Data
# 4) Plot
##########################################################################################
# 1) Dependencies
##########################################################################################
rm(list = ls())
library(dplyr)
library(tidyverse)
library(data.table)
library(readr)
library(lubridate)
library(ggplot2)
library(graphlayouts)
library(scales)
library(purrr)
library(magrittr)
library(sysfonts)
library(cowplot)
##########################################################################################
# 2) Data Import
##########################################################################################
rm(list = ls())
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

# - load fonts used in plots
font_add_google("Montserrat", "Montserrat")
font_add_google("Roboto", "Roboto")

# - define a global seed (used in all scripts)
set.seed(2019)

# - load raw data 
# - load press releases
press <- read_rds("../data/pressreleases_2018-2019.RDS")
# - load media articles
smd <- read_rds("../data/smd_minified_2018-2019.RDS")
# - load tweets
tweets <- read_rds("../data/Tweets_2018_2019_curated_sentiment_class.RDS")
tweets <- tweets %>% dplyr::mutate(selectsclass = ifelse(selectsclass == "EU_Europe", "EU_Europa", selectsclass))

# - remove retweets (TRUE)
retweet_out <- TRUE

# - load ddl theme
suppressMessages(suppressWarnings(source('../ggplot_theme_ddl.R')))
##########################################################################################
# 3) Arrange Data
##########################################################################################
##########################################################################################
## 3.1) Re-code Paper Names in so_txt since there are errors in the smd database
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
  tweets <- tweets %>% dplyr::filter(Is_retweet != T)
}

# - select parties of interest and all other accounts from tweets
tweets <- tweets %>% 
  dplyr::filter(Party %in% c("grÜne (basels starke alternative)", "grÜnliberale partei", 
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
## 3.3) Transform Data
##########################################################################################
# Transform SMD Data: 
smd_ana <- smd %>% mutate(pubDateTime = as.Date(pubDateTime)) %>% 
                   group_by(week = cut.Date(pubDateTime, "week"), selectsclass) %>% 
                   summarise(n = n()) %>%
                   mutate(freq = n / sum(n))  %>% mutate(source = "Newspapers")

# Transform Press Data:
.parties <- c("SVP", "GPS", "CVP","SPS", "FDP", "GLP", "BDP")
press_ana <- press %>% mutate(Akteur_Art = ifelse(Kürzel %in% .parties, "Party", 
                                                  ifelse(Kürzel == "admin.ch", "Gov", "Org")),
                              pubDateTime = as.Date(pubDateTime)) %>%
  dplyr::group_by(week = cut.Date(pubDateTime, "week"), selectsclass) %>% 
  summarise(n = n()) %>%
  mutate(freq = n / sum(n))  %>% mutate(source = "Press Releases")

# Tranform Twitter Data 
tweet_ana <- tweets %>% as.data.frame() %>%
  dplyr::mutate(Akteur_Art = ifelse(Akteur.Typ == "Party", "Party",
                                    ifelse(Akteur.Typ == "Media", "Media", 
                                           ifelse(Akteur.Typ == "Person", "Politicians", 
                                                  ifelse(Akteur.Typ == "Administration", "Gov",
                                                         ifelse(Akteur.Typ == "Organisation", "Org", "Other"))))),
         Datum = as.Date(Datum)) %>%
  dplyr::filter(Akteur_Art != "Other") %>%
  dplyr::group_by(week = cut.Date(Datum, "week"), Akteur_Art, selectsclass) %>%
  dplyr::summarise(n = n()) %>% 
  dplyr::mutate(freq = n / sum(n)) %>% mutate(source = ifelse(Akteur_Art == "Party", "Tweets by Parties", 
                                                              ifelse(Akteur_Art == "Politicians", "Tweets by Politicians",
                                                                     ifelse(Akteur_Art == "Gov", "Tweets by Governments",
                                                                            ifelse(Akteur_Art == "Media", "Tweets by Newspapers", "Tweets by Organisations"))))) #%>% rename(pubDateTime = Datum)


# - build data for timeline plot 
df_time <- rbind(smd_ana,press_ana,tweet_ana) %>%
           dplyr::filter(selectsclass %in% c("EU_Europa", "GenderIssues_Discrimination", "Immigration_Asylum", "Environment_Energy"))

# - relabel the topics so they fit/look better in the plot
df_time$topic <- recode(df_time$selectsclass,
                               `EU_Europa` = "Europe",
                               `GenderIssues_Discrimination` = "Gender",
                               `Immigration_Asylum` = "Immigration",
                               `Environment_Energy` = "Environment")

df_time <-   df_time %>% dplyr::select(-selectsclass) %>% 
                         dplyr::mutate(topic = as.factor(topic)) %>% 
                         dplyr::mutate(topic = fct_relevel(topic, "Environment", "Gender", "Europe", "Immigration")) %>%
                         dplyr::ungroup() %>% 
                         dplyr::mutate(week = as.Date(week)) %>% 
                         tidyr::complete(week = seq.Date(min(week), max(week), by = "7 days"), topic, source, fill = list(n = 0, freq = 0)) %>%
                         dplyr::mutate(topic_rev = fct_relevel(topic, "Immigration", "Europe", "Gender", "Environment")) %>% 
                         dplyr::filter(week > as.Date("2017-12-31")) %>% 
                         dplyr::filter(week < as.Date("2019-12-24"))
##########################################################################################
# 4) Plot
##########################################################################################
annotate1 <- data.frame(x = ymd(as.Date("2019-06-10")),
                        max = c(df_time %>% dplyr::filter(source %in% c("Newspapers", "Tweets by Politicians", "Tweets by Parties")) %>% 
                                group_by(source) %>% summarise(max = max(freq))),
                        lab = "Women's Strike")
colnames(annotate1) <- c("x", "source", "max", "lab")

annotate2 <- data.frame(x = ymd(as.Date(rep(c("2018-12-17","2019-09-23"),each = 3))),
                        max = c(df_time %>% dplyr::filter(source %in% c("Newspapers", "Tweets by Politicians", "Tweets by Parties")) %>% 
                                group_by(source) %>% summarise(max = max(freq))),
                        lab = "Climate Strikes")
colnames(annotate2) <- c("x", "source", "max", "lab")

annotate <- bind_rows(annotate1, annotate2)

topics_filter <- c("Environment", "Gender", "Europe", "Immigration")
values <- c("#009E73","#DD2461","#0072B2","#999999")
plots <- list()

for(i in 1:length(topics_filter)){
  df_time_tt <- df_time %>% filter(topic == topics_filter[i])
  
  plots[[i]] <- ggplot(data = dplyr::filter(df_time_tt, source %in% c("Newspapers", "Tweets by Politicians", "Tweets by Parties"))) +
    geom_path(aes(x = week, y = freq,  group = topic_rev, linetype = topic, color = topic), 
              position = position_dodge(width = 0.55),  alpha = .75, size = .7) +
    scale_y_continuous(labels = scales::percent_format(scale = 100, accuracy = 0.5), limits = c(0,NA), expand = c(0,0)) +
    scale_x_date(breaks = "3 months", labels = date_format("%b %Y"), 
                 limits = c(as.Date("2018-01-01"), as.Date("2020-01-01")), expand = c(0, 0)) +
    geom_vline(xintercept=as.numeric(as.Date("2019-01-01")), colour="grey60", linetype = "dashed") +
    facet_wrap(~source, ncol = 1, scales = "free_y") +
    labs(subtitle = "", title = paste0(topics_filter[i]), y = "Share of Documents per Week", x = "") +
    #scale_linetype_manual(labels = c("Environment","Gender","Europe","Immigration"),
     #                     values = c("solid","longdash", "dotted", "dotdash")) +
    scale_color_manual(labels = topics_filter[i], 
                       values = values[i]) +
    scale_fill_manual(labels = topics_filter[i], 
                      values = values[i]) +
    ddl_theme(panel.grid.major = element_blank(),
              panel.grid.minor = element_blank(),
              panel.grid.major.y = element_line(size=.3, color = "lightgrey")) +
    theme(legend.position = "none", legend.title = element_blank(), 
          strip.background = element_blank(), strip.text = element_text(color = "black"),
          axis.text.x = element_text(angle = 0, hjust = 0.5, vjust = 0.0, size = 18, color = "black"),  
          axis.text.y = element_text(hjust=.5, size = 18, color = "black"),
          strip.text.x = element_text(size = 20, color = "black"),
          axis.title = element_text(size = 20, color = "black"),
          plot.title = element_blank(),
          legend.text = element_text(size = 18, color = "black"),
          legend.key = element_blank(),
          plot.margin = unit(c(.5,1.5,.1,.4,.5), "cm"),
          legend.key.size = unit(1,"line"),
          axis.line.x = element_line(color="black", size = .5),
          axis.line.y = element_line(color="black", size = .5),
          legend.key.width = unit(1.5, "cm"))
  
  if(i == 1){plots[[i]] <-  plots[[i]] + geom_vline(xintercept = as.numeric(c(as.Date("2018-12-17"),as.Date("2019-01-14"),as.Date("2019-01-28"),
                                                                              as.Date("2019-03-11"),as.Date("2019-04-01"),as.Date("2019-05-20"),
                                                                              as.Date("2019-07-22"),as.Date("2019-09-23"))), 
                                                    linetype = "solid", color = "#009E73") + 
    geom_text(aes(x = x-6, y = max, label = lab), 
              data = annotate2,
              hjust = "right", color = "black", angle = 90)}
  
  if(i == 2){plots[[i]] <-  plots[[i]] +  geom_text(aes(x = x-6, y = max, label = lab), 
                                                    data = annotate1,
                                                    hjust = "right", color = "black", angle = 90) +
                                          geom_vline(xintercept = as.numeric(c(as.Date("2019-06-10"))), 
                                                     linetype = "solid", color = "#DD2461")  }
}

cowplot::plot_grid(plotlist = plots, align = "hv", nrow = 4, ncol = 1, label_x = c(.411,.458,.458,.419),
                   labels = topics_filter, label_size = 20, label_fontface = "bold")


ggsave("../images/figure_2.pdf", width = 16, height = 22.6, device = cairo_pdf)
##########################################################################################


