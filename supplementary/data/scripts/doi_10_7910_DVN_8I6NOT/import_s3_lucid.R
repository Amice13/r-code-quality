############################################### #
# The Psychology of Online Political Hostility  #
#                                               #
# This code imports and cleans the data file    #
#       for Study 3                             #
#       from 2019 Oct Lucid survey              #
#                                               #
# 2019.10.10.                                   #
# code by Alexander Bor                         #
############################################### #

library(tidyverse)
library(rio)
library(here)
source("C.code/functions.R")


raw.luc <- rio::import(here("A.original.data/lucid_full.csv"))

names(raw.luc) <- gsub(pattern = "host_on_", replacement = "hostile_online", 
                       names(raw.luc))
names(raw.luc) <- gsub(pattern = "host_off_", replacement = "hostile_offline", 
                       names(raw.luc))


witness_on <- paste0("witness_on_", 1:3)
witness_off <- paste0("witness_off_", 1:3)
perc_on <- paste0("perc_on_", 1:8)
perc_off <- paste0('perc_off_', 1:8)
host_on <- paste0("hostile_online", 1:6)
host_off <- paste0("hostile_offline", 1:4)

pid.lucid <- function(xp){
        require(dplyr)
        x <- case_when(
                xp <= 3  ~ "Democrat", 
                xp == 6  ~ "Democrat", 
                xp == 5  ~ "Republican", 
                xp >= 8  ~ "Republican", 
                TRUE ~ "Independent"
        )
        x <- as.factor(x)
}



luc <- raw.luc %>% 
        mutate_at(c(perc_on, perc_off, host_on, host_off,witness_on, witness_off), 
                  zero.one) %>% 
        mutate(perc_on_6 = flip(perc_on_6),
               perc_on_7 = flip(perc_on_7),
               perc_on_8 = flip(perc_on_8),
        
                perc_off_6 = flip(perc_off_6),
                perc_off_7 = flip(perc_off_7),
                perc_off_8 = flip(perc_off_8), 
                
                pid = pid.lucid(political_party),
               highered = ifelse(education >= 6, 1,
                         ifelse(education == -3105, NA, 0)),
               female = gender-1,
               white  = ifelse(ethnicity == 1, 1, 0), 
               id = 1:nrow(.)) %>% 
        filter(!is.na(hostile_offline4) & !is.na(hostile_online4) )


luc$perc_on <- rowMeans(luc[, perc_on])
luc$perc_off <- rowMeans(luc[, perc_off])

luc$perc_on <- rowMeans(luc[, perc_on])
luc$perc_off <- rowMeans(luc[, perc_off])


luc$hostile_online <- rowMeans(luc[, host_on])
luc$hostile_offline <- rowMeans(luc[, host_off])

depvars <- c(perc_on, perc_off, host_on, host_off,witness_on, witness_off, 
	"perc_on", "perc_off", "hostile_online", "hostile_offline"
	)
	
controlvars <- c("pid", "highered", "female", "white", "age", "id", "online_first")

df.us2 <- luc %>%
		dplyr::select(depvars, controlvars)

saveRDS(df.us2, file = "B.analysis.data/s3_us_2019.rds")
write.csv(df.us2, file = "B.analysis.data/s3_us_2019.csv")
