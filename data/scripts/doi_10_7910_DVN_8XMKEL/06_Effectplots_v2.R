# load packages and data --------------------------------------------------
library(haven)
library(tidyverse)
library(dplyr)
library(stargazer)
library(stringr)
library(xlsx)
library(ggplot2)
library(ggpubr)

root <- "C:/Users/Wieczorek_W_Station/Dropbox/Projekte/Effektive_Schulsteuerung_PISA/Daten/PISA/"
path <-  paste0(root,"Output_BIFIE/")


setwd(path)
files <- as.tibble(list.files())

#countries <- c("SGP","KOR")
countries <- c("FIN","SWE", "DEU", "GBR")


df <- tibble(parameter = as.character(),
                full_model = as.double(),
                year = as.double(),
                country = as.character()
                )

for(country in countries){
  print(paste("Currently at country:", country))
  
  ## load data for PISA-waves 2000, 2009, 2015
  df2000 <- read.xlsx(paste0("2000_",country,".xlsx"), sheetIndex = 1)
  df2009 <- read.xlsx(paste0("2009_",country,".xlsx"), sheetIndex = 1)
  df2015 <- read.xlsx(paste0("2015_",country,".xlsx"), sheetIndex = 1)
  
  ## fetch parameter names
  parm_names <- df2000 %>%
    pull(parameter)
  
  ## mutate variables to floating point values
  df2000 <- df2000 %>%
    mutate_all(.,~gsub("\\(|\\)|\\*","",.)) %>%
    mutate_all(.,~as.double(.)) %>%
    mutate(parameter   = parm_names) %>%
    mutate(year = 2000,
           country = country) 
  
  df2009 <- df2009 %>%
    mutate_all(.,~gsub("\\(|\\)|\\*","",.)) %>%
    mutate_all(.,~as.double(.)) %>%
    mutate(parameter   = parm_names) %>%
    mutate(year = 2009,
           country = country)
  
  df2015 <- df2015 %>%
    mutate_all(.,~gsub("\\(|\\)|\\*","",.)) %>%
    mutate_all(.,~as.double(.)) %>%
    mutate(parameter   = parm_names) %>%
    mutate(year = 2015,
           country = country)
  


# Keep full models only, merge and melt dataframes ------------------------
  dfs <-  df2000 %>%
    add_row(df2009) %>%
    add_row(df2015) %>%
    select(parameter, 
           Model_9, year,
           country) 
  
  
  dfs <- dfs %>%
    rename(full_model = Model_9)
  
  rm(df2000,df2009,df2015)
  
  
  
  df <- df %>%
    add_row(dfs)
  
  
  rm(dfs)
  
}

df %>%
  pull(parameter) %>%
  unique()


df <- df %>%
  mutate(full_model = case_when(parameter == "R2_Total" ~ full_model * 100,
                                TRUE ~ full_model))


test <- df %>%
  #filter(str_detect(parameter, "Z_ESCS|beta_Z_MEAN_ESCS$|DISCLIMA|beta_Z_AUTONOMY|beta_Z_ACCOUNTABILITY|R2_Total")) %>%
  filter(!str_detect(parameter, "_SD|:|ICC|R2_Lev|Students|Schools|Intercept"))


# Align Variables to those within the monograph -------------------------------
exchange <- test %>%
  select(parameter) %>%
  unique()

varnames <- c("Schülerebene: ESCS",
              "Schulebene: Mittlerer ESCS",
              "Schülerebene: Migrations-\nhintergrund (2. Generation)",
              "Schulebene: % Migrations-\nhintergrund (2. Generation)",
              "Testsprache wird im \nElternhaushalt gesprochen",
              "Schülerebene: Disziplinäres Klima",
              "Schulebene: Duchschnittliches\ndisziplinäres Klima",
              "Governance: Schulautonomie",
              "Governance: Schulführung",
              "Governance: Accountability",
              "R²: Gesamt")

exchange <- exchange %>%
  mutate(varnames = varnames)

test <- test %>%
  mutate(parameter = case_when(parameter == exchange$parameter[1] ~ exchange$varnames[1],
                               parameter == exchange$parameter[2] ~ exchange$varnames[2],
                               parameter == exchange$parameter[3] ~ exchange$varnames[3],
                               parameter == exchange$parameter[4] ~ exchange$varnames[4],
                               parameter == exchange$parameter[5] ~ exchange$varnames[5],
                               parameter == exchange$parameter[6] ~ exchange$varnames[6],
                               parameter == exchange$parameter[7] ~ exchange$varnames[7],
                               parameter == exchange$parameter[8] ~ exchange$varnames[8],
                               parameter == exchange$parameter[9] ~ exchange$varnames[9],
                               parameter == exchange$parameter[10] ~ exchange$varnames[10],
                               parameter == exchange$parameter[11] ~ exchange$varnames[11]),
         country = case_when(country == "FIN" ~ "FIN",
                             country == "SWE" ~ "SWE",
                             country == "DEU" ~ "DEU",
                             country == "GBR" ~ "UK")
         )

test <- test %>%
  rename(Land = country)

# Create the plot ---------------------------------------------------------
g <- ggplot(data = test, aes(x = year, 
                        y = full_model, 
                        hue = Land)) + 
  geom_point(aes(shape = Land), size = 2) + 
  geom_line(aes(linetype = Land)) + 
  facet_wrap(~factor(parameter, unique(parameter)),
             scales = "free_y",
             ncol = 3) +
  theme_bw() +
  theme(axis.text.x = element_text(size = 8,
                                   angle = 90,
                                   vjust = 0.5),
        axis.text.y = element_text(size = 8),
        strip.text = element_text(size=8, hjust = 0.5),
        legend.position = "right") +
  scale_x_continuous(limits = c(2000,2015),
                     breaks = c(2000,2009,2015),
                     labels = c(2000,2009,2015)) +
  ylab("Vollmodell") +
  xlab("PISA-Welle")


ggsave(plot = g, 
       filename = "Effectplot.png",
       height = 14,
       width = 17,
       unit = "cm", 
       dpi = 1200)









