# load packages and data --------------------------------------------------
library(haven)
library(tidyverse)
library(dplyr)
library(stargazer)
library(stringr)
library(xlsx)

library("ggplot2")
library("tidyr")
library("dotwhisker")
library("effects")

## get version number of used packages
packageVersion("xlsx")
packageVersion("dotwhisker")
packageVersion("effects")


root <- "C:/Users/Wieczorek_W_Station/Dropbox/Projekte/Effektive_Schulsteuerung_PISA/Daten/PISA/"
path <-  paste0(root,"Output_BIFIE/")


setwd(path)
files <- as.tibble(list.files())

#countries <- c("SGP","KOR")
countries <- c("FIN","SWE")

#country <- "SGP"
for(country in countries){
  ## get file names for each country
  #files <- files %>%
  #  filter(grepl(paste0(country,".xlsx"), value))
  
  ## load data for 2009 & 2015
  df2009 <- read.xlsx(paste0("2009_",country,".xlsx"), sheetIndex = 1)
  df2015 <- read.xlsx(paste0("2015_",country,".xlsx"), sheetIndex = 1)
  
  df2009 <- df2009 %>%
    mutate(Model_0 = gsub("\\(|\\)|\\*","",Model_0),
           Model_1 = gsub("\\(|\\)|\\*","",Model_1),
           Model_2 = gsub("\\(|\\)|\\*","",Model_2),
           Model_3 = gsub("\\(|\\)|\\*","",Model_3),
           Model_4 = gsub("\\(|\\)|\\*","",Model_4),
           Model_5 = gsub("\\(|\\)|\\*","",Model_5),
           Model_6 = gsub("\\(|\\)|\\*","",Model_6),
           Model_7 = gsub("\\(|\\)|\\*","",Model_7),
           Model_8 = gsub("\\(|\\)|\\*","",Model_8),
           Model_9 = gsub("\\(|\\)|\\*","",Model_9))
  
  df2009 <- df2009 %>%
    mutate(Model_0 = as.double(Model_0),
           Model_1 = as.double(Model_1),
           Model_2 = as.double(Model_2),
           Model_3 = as.double(Model_3),
           Model_4 = as.double(Model_4),
           Model_5 = as.double(Model_5),
           Model_6 = as.double(Model_6),
           Model_7 = as.double(Model_7),
           Model_8 = as.double(Model_8),
           Model_9 = as.double(Model_9))
  
  

# Generate first tidy-dataframe for dotwhisker-plot -----------------------


# Parameters --------------------------------------------------------------
## first appearences of parameters in 2009
  #model 1
  beta_Z_ESCS <- df2009 %>%
    filter(parameter == "beta_Z_ESCS")
  beta_Z_ESCS <- beta_Z_ESCS$Model_1
  
  ## model 2
  beta_Z_MEAN_ESCS <- df2009 %>%
    filter(parameter == "beta_Z_MEAN_ESCS")
  beta_Z_MEAN_ESCS <- beta_Z_MEAN_ESCS$Model_2
  
  # Model 3
  beta_Z_ESCS_Z_MEAN_ESCS <- df2009 %>%
    filter(parameter == "beta_Z_ESCS:Z_MEAN_ESCS")
  beta_Z_ESCS_Z_MEAN_ESCS <- beta_Z_ESCS_Z_MEAN_ESCS$Model_3
  
  # model 4
  beta_MIG_2ND <- df2009 %>%
    filter(parameter == "beta_MIG_2ND")
  beta_MIG_2ND <- beta_MIG_2ND$Model_4
  
  beta_Z_MEAN_MIG <- df2009 %>%
    filter(parameter == "beta_Z_MEAN_MIG")
  beta_Z_MEAN_MIG <- beta_Z_MEAN_MIG$Model_4
  
  beta_LANGN <- df2009 %>%
    filter(parameter == "beta_LANGN")
  beta_LANGN <- beta_LANGN$Model_4

  # model 5
  beta_Z_DISCLIMA <- df2009 %>%
    filter(parameter == "beta_Z_DISCLIMA")
  beta_Z_DISCLIMA <- beta_Z_DISCLIMA$Model_5
  
  beta_Z_MEAN_DISCLIMA <- df2009 %>%
    filter(parameter == "beta_Z_MEAN_DISCLIMA")
  beta_Z_MEAN_DISCLIMA <- beta_Z_MEAN_DISCLIMA$Model_5
  
  # model 6
  beta_Z_AUTONOMY <- df2009 %>%
    filter(parameter == "beta_Z_AUTONOMY")
  beta_Z_AUTONOMY <- beta_Z_AUTONOMY$Model_6 
  
  beta_Z_LEADERSHIP <- df2009 %>%
    filter(parameter == "beta_Z_LEADERSHIP")
  beta_Z_LEADERSHIP <- beta_Z_LEADERSHIP$Model_6
  
  beta_Z_ACCOUNTABILITY <- df2009 %>%
    filter(parameter == "beta_Z_ACCOUNTABILITY")
  beta_Z_ACCOUNTABILITY <- beta_Z_ACCOUNTABILITY$Model_6
  

# Standard Deviations -----------------------------------------------------
## first appearences of standard_deviations in 2009
  #model 1
  beta_Z_ESCS_SD <- df2009 %>%
    filter(parameter == "beta_Z_ESCS_SD")
  beta_Z_ESCS_SD <- beta_Z_ESCS_SD$Model_1
  
  #model 2
  beta_Z_MEAN_ESCS_SD <- df2009 %>%
    filter(parameter == "beta_Z_MEAN_ESCS_SD")
  beta_Z_MEAN_ESCS_SD <- beta_Z_MEAN_ESCS_SD$Model_2 
  
  # Model 3
  beta_Z_ESCS_Z_MEAN_ESCS_SD <- df2009 %>%
    filter(parameter == "beta_Z_ESCS:Z_MEAN_ESCS_SD")
  beta_Z_ESCS_Z_MEAN_ESCS_SD <- beta_Z_ESCS_Z_MEAN_ESCS_SD$Model_3
  
  # model 4
  beta_MIG_2ND_SD <- df2009 %>%
    filter(parameter == "beta_MIG_2ND_SD")
  beta_MIG_2ND_SD <- beta_MIG_2ND_SD$Model_4
  
  beta_Z_MEAN_MIG_SD <- df2009 %>%
    filter(parameter == "beta_Z_MEAN_MIG_SD")
  beta_Z_MEAN_MIG_SD <- beta_Z_MEAN_MIG_SD$Model_4
  
  beta_LANGN_SD <- df2009 %>%
    filter(parameter == "beta_LANGN_SD")
  beta_LANGN_SD <- beta_LANGN_SD$Model_4
  
  # model 5
  beta_Z_DISCLIMA_SD <- df2009 %>%
    filter(parameter == "beta_Z_DISCLIMA_SD")
  beta_Z_DISCLIMA_SD <- beta_Z_DISCLIMA_SD$Model_5
  
  beta_Z_MEAN_DISCLIMA_SD <- df2009 %>%
    filter(parameter == "beta_Z_MEAN_DISCLIMA_SD")
  beta_Z_MEAN_DISCLIMA_SD <- beta_Z_MEAN_DISCLIMA_SD$Model_5
  
  
  # model 6
  beta_Z_AUTONOMY_SD <- df2009 %>%
    filter(parameter == "beta_Z_AUTONOMY_SD")
  beta_Z_AUTONOMY_SD <- beta_Z_AUTONOMY_SD$Model_6
  
  beta_Z_LEADERSHIP_SD <- df2009 %>%
    filter(parameter == "beta_Z_LEADERSHIP_SD")
  beta_Z_LEADERSHIP_SD <- beta_Z_LEADERSHIP_SD$Model_6
  
  beta_Z_ACCOUNTABILITY_SD <- df2009 %>%
    filter(parameter == "beta_Z_ACCOUNTABILITY_SD")
  beta_Z_ACCOUNTABILITY_SD <- beta_Z_ACCOUNTABILITY_SD$Model_6
  
  
  

# vectorize the values and remove from memory -----------------------------

  variables <- c("beta_Z_ESCS",
                 "beta_Z_MEAN_ESCS", 
                 "beta_Z_ESCS_Z_MEAN_ESCS",
                 "beta_MIG_2ND", "beta_Z_MEAN_MIG", "beta_LANGN",
                 "beta_Z_DISCLIMA", "beta_Z_MEAN_DISCLIMA",
                 "beta_Z_AUTONOMY", "beta_Z_LEADERSHIP", "beta_Z_ACCOUNTABILITY")
  
  estimations <- c(beta_Z_ESCS,
                   beta_Z_ESCS_SD, 
                   beta_Z_ESCS_Z_MEAN_ESCS,
                   beta_MIG_2ND, beta_Z_MEAN_MIG, beta_LANGN,
                   beta_Z_DISCLIMA, beta_Z_MEAN_DISCLIMA,
                   beta_Z_AUTONOMY, beta_Z_LEADERSHIP, beta_Z_ACCOUNTABILITY)
  
  standard_errors <- c(beta_Z_ESCS_SD, 
                       beta_Z_MEAN_ESCS_SD,
                       beta_Z_ESCS_Z_MEAN_ESCS_SD,
                       beta_MIG_2ND_SD, beta_Z_MEAN_MIG_SD, beta_LANGN_SD,
                       beta_Z_DISCLIMA_SD, beta_Z_MEAN_DISCLIMA_SD,
                       beta_Z_AUTONOMY_SD, beta_Z_LEADERSHIP_SD, beta_Z_ACCOUNTABILITY_SD)

# Generate Tidy-Dataframe -------------------------------------------------

## term = parameter
## estimate = beta_z_XXXX
## std.error = beta_z_XXXX

first_appearence <- tibble(term = variables,
       estimate = estimations,
       std.error = standard_errors)

selvars <- c()
for(v in variables){
  
  test <- str_replace(v,"beta_Z_ESCS_Z_MEAN_ESCS","beta_Z_ESCS:Z_MEAN_ESCS")
  #print(paste(v, "               ", test))
  selvars <- append(selvars, test)
}  

estimations <- df2009 %>%
  filter(parameter %in% selvars)
estimations <- estimations$Model_9 


sd_vars <- c()
for(v in selvars){ 
  print(paste0(v,"_SD"))
  sd_vars <- append(sd_vars,paste0(v,"_SD"))
}

standard_errors <- df2009 %>%
  filter(parameter %in% sd_vars)
standard_errors <- standard_errors$Model_9

full_model <- tibble(term = variables,
                     estimate = estimations,
                     std.error = standard_errors)

first_appearence <- first_appearence%>% mutate(model = "first_models")
full_model <- full_model %>% mutate(model = "full_model")
two_models <- rbind(first_appearence, full_model)

dwplot(two_models)

ggplot(data = full_model, aes(y = estimate, x = std.error)) +
  geom_point()

  }



