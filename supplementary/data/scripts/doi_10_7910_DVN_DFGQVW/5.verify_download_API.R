rm(list=ls())
library(tidyverse)
library(readxl)

meeting_id <- read_excel('meeting_overview.xlsx')

paths <- dir('raw_diet/')
paths <- as.numeric(gsub(paths, pattern='[^0-9]', replacement = ''))
print(setdiff(1:203, paths))

for (ses in paths){
  print(ses)
  data_session <- read_excel(paste0('raw_diet//session', ses, '.xlsx'))
  fmt_id <- stringr::str_replace(data_session$speechID, pattern='_[0-9]+$', replacement = '')
  stopifnot(all.equal(fmt_id, data_session$attempted_url))
  
  m1 <- setdiff(fmt_id, meeting_id %>% filter(session == ses) %>% pull(link))
  m2 <- setdiff(meeting_id %>% filter(session == ses) %>% pull(link), fmt_id)
  if ( (length(m1) == 0 & length(m2) == 0) == TRUE){
    
  }else{
    stop()
    print(c(m1, m2))
  }
  
}
