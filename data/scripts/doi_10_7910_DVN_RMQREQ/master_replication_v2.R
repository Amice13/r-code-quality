
rm(list=ls())

here::i_am("POPPA_Replication_v2.Rproj")

getwd()

#packages used.
library(here)
library(tidyverse)
library(naniar)
library(dataverse)
library(lavaan)
library(conflicted)

# Individuals scripts ####

source("rcode_v2/cleaning_rawdata_v2.R")

source("rcode_v2/expert_dataset_v2.R")

source("rcode_v2/party_dataset_v2.R")

source("rcode_v2/combine_waves_v2.R")

