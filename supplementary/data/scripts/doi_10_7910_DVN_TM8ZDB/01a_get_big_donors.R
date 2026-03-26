setwd(dirname(rstudioapi::getActiveDocumentContext()$path))
setwd("..")

library(tidyverse)
library(cowplot)

load("data_raw/dime/dime_contributors_1979_2020.rdata")

contribs2 = contribs %>%
  filter(contributor.type == "I") %>%
  mutate(cumul2012 = rowSums(across(amount_1980:amount_2010)),
         lastcycles = (amount_2008 + amount_2010) != 0) %>%
  select(bonica.cid, most.recent.contributor.name,
         most.recent.contributor.latitude,
         most.recent.contributor.longitude,
         contributor.cfscore, num.distinct,
         most.recent.contributor.state,
         lastcycles,
         cumul2012) %>%
  filter(cumul2012 > 10000,
         most.recent.contributor.latitude < 52,
         most.recent.contributor.latitude > 23)

save(contribs2, file="data_clean/donors_clean.RData")

contribs2 %>% filter(cumul2012> 200000 & lastcycles==TRUE) %>% nrow()

contribs2 = contribs2 %>% mutate(ideo_bin = as.numeric(cut(contributor.cfscore, breaks = c(-Inf,-0.5,.5, Inf))))

donor100 = contribs2 %>% filter(cumul2012 > 100000) %>% mutate(type = "donors100k")
donor250 = contribs2 %>% filter(cumul2012 > 250000) %>% mutate(type = "donors250k")
donor500 = contribs2 %>% filter(cumul2012 > 500000) %>% mutate(type = "donors500k")
donor200 = contribs2 %>% filter(cumul2012 > 200000 & lastcycles==T) %>% mutate(type = "donors200k")
donorleft = contribs2 %>% 
  filter(cumul2012 > 200000 & lastcycles==T & ideo_bin == 1) %>%
  mutate(type = "donorsleft")
donorright = contribs2 %>%
  filter(cumul2012 > 200000 & lastcycles==T & ideo_bin == 3) %>%
  mutate(type = "donorsright")
donorswing = contribs2 %>%
  filter(cumul2012 > 200000 & lastcycles==T & ideo_bin == 2) %>%
  mutate(type = "donorsswing")

donor = rbind(donor100, donor250, donor500, donor200,
              donorleft,donorright,donorswing) %>%
  select(lon = most.recent.contributor.longitude,
         lat = most.recent.contributor.latitude,
         obj = most.recent.contributor.name,
         state = most.recent.contributor.state,
         ideo = contributor.cfscore,
         dollars = cumul2012,
         type)

table(donor$type)
table(donor$type, donor$state)

save(donor, file = "data_clean/all_donors.RData")
