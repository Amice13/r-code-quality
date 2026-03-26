#############################################
# Project:    Emergency Debate contribution
# Task:       Build common corpus
# Author:     Christian Rauh (09.04.2021)
#############################################

# R version 4.0.3 (2020-10-10) -- "Bunny-Wunnies Freak Out"

# Packages ####
library(tidyverse) # 1.3.0


# Commission speeches ####

# Corpus is built from an original scraper targeting all speeches in the official Press Archive of the European Commission
# https://ec.europa.eu/commission/presscorner/advancedsearch/en?keywords=&dotyp=4&parea=0&datepickerbefore=&datebefore=&commissioner=0&datepickerafter=&dateafter=&pagenumber=1
# Scraping was executed September 10 2020
# Scraping and text cleaning scripts are available from @ChRauh

comm <- read_rds("./Corpora/ComSpeechesRaw.Rds") %>% 
  mutate(institution = "EU Commission", country = NA) %>%
  filter(language == "ENGLISH") %>% 
  rename(speaker = speaker.name) %>% 
  select(date, institution, country, speaker, length, text) %>% 
  filter(text != "")



# ECB speeches ####

# Derived from the official ECB source - please cite this appropriately!
# https://www.ecb.europa.eu/press/key/html/downloads.en.html (09.04.2021)
# See article main text for the reference

ecb <- read_rds("./Corpora/ECBSpeeches.Rds") %>% 
  mutate(institution = "European Central Bank", country = NA) %>%
  filter(language == "ENGLISH") %>% 
  select(date, institution, country, speaker, length, text) %>% 
  filter(text != "")


# Nat leaders during Eurocrisis ####

# Derived from EUSpeech V1 - please cite the original source when using this!
# https://doi.org/10.7910/DVN/XPCVEI
# The version used here as a re-construction of the raw text vectors from the EuSpeech csvs
# as used in Rauh, BEs, Schoonvelde (2020, EJPR): https://doi.org/10.1111/1475-6765.12350

load("./Corpora/EuSpeechEdit.Rdata")
nat <- corpus
rm(corpus)
nat <- nat %>% 
  filter(str_detect(institution, "Nat. leader|Council President")) %>% 
  mutate(length = str_count(text, " ") + 1) %>% 
  select(date, institution, country, speaker, length, text)

nat$date <- as.character(nat$date)


# Combine and export ####
corp <- rbind(comm, ecb, nat)
write_rds(corp, "./corpora/EmergencyCorp.rds")

str(corp)
