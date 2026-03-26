
## !!! Needs results from 04_Figure1.R !!! ##
# source("code/04_Figure1.R")

#### 1. Discussion of difference between July 2020 and March 2021 ####
## Table
mdat %>% filter(wave %in% c("July 2020", "March 2021"))
## Test
svyttest(agree1 ~ WAVE, design = subset(des, WAVE %in% c("July 2020", "March 2021")))

#### 2. Discussion of difference between July 2020 and July 2022 ####
## Table
mdat %>% filter(wave %in% c("July 2020", "July 2022"))
## Test
svyttest(agree1 ~ WAVE, design = subset(des, WAVE %in% c("July 2020", "July 2022")))

