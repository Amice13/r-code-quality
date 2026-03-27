library(tidyverse)
library(readr)

path_LM = "/Users/louis-maeljean/Dropbox (MIT)/West Bengal Information Campaign/AER_I/for_submission"

path = path_LM
path_FB = paste0(path, "/data/FacebookMobility")

files <- list.files(path = paste0(path_FB,"/437586182221167_2020-03-21_2020-04-19_csv/"), pattern="*.csv", full.names=TRUE, recursive=FALSE)
files2 <- list.files(path = paste0(path_FB,"/437586182221167_2020-04-19_2020-05-18_csv/"), pattern="*.csv", full.names=TRUE, recursive=FALSE)
files3 <- list.files(path= paste0(path_FB, "/437586182221167_2020-05-19_2020-06-16_csv/"), pattern="*.csv", full.names=TRUE, recursive=FALSE)

#Minimum counts: If either the Baseline or Users during crisis time count is less than a threshold value of 10 users, then both are dropped.
output = tibble()
for (fn in c(files,files2,files3)) {
  output = output %>% bind_rows(read_csv(fn) %>% filter(country == "IN") %>% select(quadkey,clipped_z_score, n_baseline, n_crisis, date_time))
}

output %>% filter(!is.na(n_baseline)) %>% write_csv(paste0(path_FB,"/facebook_pop.csv"))

