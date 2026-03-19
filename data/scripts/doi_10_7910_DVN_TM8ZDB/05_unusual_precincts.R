setwd(dirname(rstudioapi::getActiveDocumentContext()$path))
setwd("../output/unusualness")
library(tidyverse)

#### Stack all the unusualness data ####
f = list.files()


d1 = read.csv(f[1]) %>% mutate(state = "AZ") %>%
  select(NAME, prop.d, firms, pacfirms, hospitals, airports, walmarts,
         donors200k,donorsleft,donorsright,donorsswing,topincome,medincome,urban,
         pop = TOTPOP, DVOTE = VOT18D, RVOTE = VOT18R, party,state,
         white = NH_WHITE, black = NH_BLACK, hispanic = HISP)

d2 = read.csv(f[2]) %>% mutate(state = "GA") %>%
  select(NAME, prop.d, firms, pacfirms, hospitals, airports, walmarts,
         donors200k,donorsleft,donorsright,donorsswing,topincome,medincome,urban,
         pop = TOTPOP, DVOTE = VOT16D, RVOTE = VOT16R, party,state,
         white = NH_WHITE, black = NH_BLACK, hispanic = HISP)

d3 = read.csv(f[3]) %>% mutate(state = "IA") %>%
  select(NAME, prop.d, firms, pacfirms, hospitals, airports, walmarts,
         donors200k,donorsleft,donorsright,donorsswing,topincome,medincome,urban,
         pop = TOTPOP, DVOTE = VOTE12D, RVOTE = VOTE12R, party,state,
         white = NH_WHITE, black = NH_BLACK, hispanic = HISP)

d4 = read.csv(f[4]) %>% mutate(state = "MA") %>%
  select(NAME, prop.d, firms, pacfirms, hospitals, airports, walmarts,
         donors200k,donorsleft,donorsright,donorsswing,topincome,medincome,urban,
         pop = TOTPOP, DVOTE = VOT12D, RVOTE = VOT12R, party,state,
         white = NH_WHITE, black = NH_BLACK, hispanic = HISP)

d5 = read.csv(f[5]) %>% mutate(state = "MD") %>%
  select(NAME, prop.d, firms, pacfirms, hospitals, airports, walmarts,
         donors200k,donorsleft,donorsright,donorsswing,topincome,medincome,urban,
         pop = TOTPOP, DVOTE = VOT12D, RVOTE = VOT12R, party,state,
         white = NH_WHITE, black = NH_BLACK, hispanic = HISP)

d6 = read.csv(f[6]) %>% mutate(state = "MN") %>%
  select(NAME, prop.d, firms, pacfirms, hospitals, airports, walmarts,
         donors200k,donorsleft,donorsright,donorsswing,topincome,medincome,urban,
         pop = TOTPOP, DVOTE = VOT12D, RVOTE = VOT12R, party,state,
         white = NH_WHITE, black = NH_BLACK, hispanic = HISP)

d7 = read.csv(f[7]) %>% mutate(state = "NC") %>%
  select(NAME, prop.d, firms, pacfirms, hospitals, airports, walmarts,
         donors200k,donorsleft,donorsright,donorsswing,topincome,medincome,urban,
         pop = TOTPOP, DVOTE = EL12G_VT_D, RVOTE = EL12G_VT_R, party,state,
         white = NH_WHITE, black = NH_BLACK, hispanic = HISP)

d8 = read.csv(f[8]) %>% mutate(state = "NM") %>%
  select(NAME, prop.d, firms, pacfirms, hospitals, airports, walmarts,
         donors200k,donorsleft,donorsright,donorsswing,topincome,medincome,urban,
         pop = TOTPOP, DVOTE = VOT18D, RVOTE = VOT18R, party,state,
         white = NH_WHITE, black = NH_BLACK, hispanic = HISP) %>%
  mutate(pacfirms = 0)

d9 = read.csv(f[9]) %>% mutate(state = "OH") %>%
  select(NAME, prop.d, firms, pacfirms, hospitals, airports, walmarts,
         donors200k,donorsleft,donorsright,donorsswing,topincome,medincome,urban,
         pop = TOTPOP, DVOTE = VOT12D, RVOTE = VOT12R, party,state,
         white = NH_WHITE, black = NH_BLACK, hispanic = HISP)

d10 = read.csv(f[10]) %>% mutate(state = "OR") %>%
  select(NAME, prop.d, firms, pacfirms, hospitals, airports, walmarts,
         donors200k,donorsleft,donorsright,donorsswing,topincome,medincome,urban,
         pop = TOTPOP, DVOTE = VOT12D, RVOTE = VOT12R, party,state,
         white = NH_WHITE, black = NH_BLACK, hispanic = HISP)

d11 = read.csv(f[11]) %>% mutate(state = "PA") %>%
  select(NAME, prop.d, firms, pacfirms, hospitals, airports, walmarts,
         donors200k,donorsleft,donorsright,donorsswing,topincome,medincome,urban,
         pop = TOTPOP, DVOTE = VOT12D, RVOTE = VOT12R, party,state,
         white = NH_WHITE, black = NH_BLACK, hispanic = HISP)

d12 = read.csv(f[12]) %>% mutate(state = "TX") %>%
  select(NAME, prop.d, firms, pacfirms, hospitals, airports, walmarts,
         donors200k,donorsleft,donorsright,donorsswing,topincome,medincome,urban,
         pop = TOTPOP, DVOTE = VOT12D, RVOTE = VOT12R, party,state,
         white = WHITE, black = BLACK, hispanic = HISPANIC ) %>%
  filter(!is.na(firms))

d13 = read.csv(f[13]) %>% mutate(state = "VA") %>%
  select(NAME, prop.d, firms, pacfirms, hospitals, airports, walmarts,
         donors200k,donorsleft,donorsright,donorsswing,topincome,medincome,urban,
         pop = TOTPOP, DVOTE = G16DVOT, RVOTE = G16RVOT, party,state,
         white = NH_WHITE, black = NH_BLACK, hispanic = HISP)

d14 = read.csv(f[14]) %>% mutate(state = "WI") %>%
  select(NAME, prop.d, firms, pacfirms, hospitals, airports, walmarts,
         donors200k,donorsleft,donorsright,donorsswing,topincome,medincome,urban,
         pop = PERSONS, DVOTE = VOTDEM12, RVOTE = VOTREP12, party,state,
         white = WHITE, black = BLACK, hispanic = HISPANIC)

dat = rbind(d1,d2,d3,d4,d5,d6,d7,d8,d9,d10,d11,d12,d13,d14)

write.csv(dat, file="../allstates_unusualness.csv")


## How co-located are firms and big donors?
dat = read.csv("../allstates_unusualness.csv")

table(dat$donors200k!=0, dat$firms!=0)

dat %>% mutate(anydonors = donors200k > 0,
               anyfirms = firms > 0) %>%
  group_by(state) %>%
  summarize(table(anydonors, anyfirms))

### Table A3
dat %>%
  group_by(state) %>%
  summarize(cor(topincome, firms),
            cor(urban, firms),
            cor(white, firms))



sum(dat$donors200k[dat$firms==0])
sum(dat$donors200k[dat$firms!=0])

table(dat$donors200k)

dat[dat$donors200k == 74,]
dat[dat$donors200k == 46,] 
# For IA I have County name instead of FIPS but that's fine

table(dat$firms)

dat[dat$firms == 180,]
dat[dat$firms == 117,]

dat[dat$donors200k == 74,]
dat[dat$donors200k == 46,]