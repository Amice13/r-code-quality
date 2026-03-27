###########################################################
# Project:  Supranational emergency politics?
# Task:     Baseline for NE scale on sample of HoC speeches
# Author:   Christian Rauh (08.09.2020)
###########################################################


# Packages ####
library(tidyverse)
library(quanteda)
library(Hmisc)
`%notin%` <- Negate(`%in%`)

# Project crash - set wd manually
setwd("~/Desktop/SSD Drive/rauh/WordVectors")

# HoC Corpus ###
corp <- read_rds("./Corpora/SV2_Corp_HouseOfCommons_V2.rds") %>% # ParlSpech V2 (save version 2)
  filter(chair == F) %>% # Exclude organisational speeches from the chair
  select(date, party, text)

# Sample
sample <- corp %>% sample_n(100000)
rm(corp)

# NE dictionary ####
nc <- read.csv2("./CrisisDictionaries/EmergencyScale250.csv")

# Named vector of nc weigths
cnw <- nc$diff
names(cnw) <- nc$term

# DFM
corp <- corpus(sample$text)
m <- dfm(corp, tolower = T, select = names(cnw)) # Only the words on the nc scale, otherwise too big
rm(corp)

# Weigth DFM with NC scale
m2 <- dfm_weight(m, weights = cnw)
head(m2)
sparsity(m2)

m3 <- convert(m2, to = "matrix")
m3[m3==0] <- NA # Need to set zeros to NAs, otherwise I draw the rowmean to zero

# Write average NC scale to corpus 
sample$ne.scale <- rowMeans(m3, na.rm = T)
hist(sample$ne.scale)

hoc.ref.ne <- mean_cl_boot(sample$ne.scale)



