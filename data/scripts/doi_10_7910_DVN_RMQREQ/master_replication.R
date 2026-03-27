
rm(list=ls())

here::i_am("POPPA_Replication.Rproj")

getwd()

#packages used.
library(here)
library(tidyverse)
library(naniar)
library(dataverse)
library(lavaan)
library(conflicted)

#Individuals scripts ####

source("rcode/cleaning_rawdata.R")

source("rcode/expert_dataset.R")

source("rcode/party_dataset.R")

source("rcode/combine_waves.R")

