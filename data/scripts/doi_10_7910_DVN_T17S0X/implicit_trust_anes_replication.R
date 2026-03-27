# 2008 anes replication file

# load library
library(foreign)

# load 2008 anes data, fyi: change file location
datanes <- read.dta("/anes2008.dta")

table(datanes$V085147A)
table(datanes$V085148) 
table(datanes$V085149) 
table(datanes$V085150) 

# create new column with recoded trust to 0-100 scale
datanes$trust[datanes$V085147A=="4. Never {VOL}"] <- 0
datanes$trust[datanes$V085147A=="3. Only some of the time"] <- 33
datanes$trust[datanes$V085147A=="2. Most of the time"] <- 67
datanes$trust[datanes$V085147A=="1. Just about always"] <- 100

# create new column with recoded interests to 0-100 scale
datanes$interests[datanes$V085148=="5. Gov't run for the benefit of all"] <- 100
datanes$interests[datanes$V085148=="1. Gov't run by a few big interests"] <- 0

# create new column with recoded waste to 0-100 scale
datanes$waste[datanes$V085149=="5. Don't waste very much"] <- 100
datanes$waste[datanes$V085149=="3. Waste some"] <- 50
datanes$waste[datanes$V085149=="1. Waste a lot"] <- 0

# create new column with recoded crooked to 0-100 scale
datanes$crooked[datanes$V085150=="5. Hardly any are crooked"] <- 100
datanes$crooked[datanes$V085150=="3. Not very many are crooked"] <- 50
datanes$crooked[datanes$V085150=="1. Quite a few are crooked"] <- 0

# add trust items together
datanes$trustadd <- datanes$trust + datanes$interests + datanes$waste + datanes$crooked

# create trust index
datanes$trustindex <- datanes$trustadd/4

# mean trust index
mean(datanes$trustindex, na.rm=TRUE)

table(datanes$V085307) # show
table(datanes$V085308) # actor
table(datanes$V085309) # attention

# higher numbers means high self monitor

# recode variables
datanes$sm1[datanes$V085307=="5. Never"] <- 1
datanes$sm1[datanes$V085307=="4. Once in a while"] <- 2
datanes$sm1[datanes$V085307=="3. About half the time"] <- 3
datanes$sm1[datanes$V085307=="2. Most of the time"] <- 4
datanes$sm1[datanes$V085307=="1. Always"] <- 5

datanes$sm2[datanes$V085308=="5. Very poor"] <- 1
datanes$sm2[datanes$V085308=="4. Poor"] <- 2
datanes$sm2[datanes$V085308=="3. Fair"] <- 3
datanes$sm2[datanes$V085308=="2. Good"] <- 4
datanes$sm2[datanes$V085308=="1. Excellent"] <- 5

datanes$sm3[datanes$V085309=="5. Never"] <- 1
datanes$sm3[datanes$V085309=="4. Once in a while"] <- 2
datanes$sm3[datanes$V085309=="3. About half the time"] <- 3
datanes$sm3[datanes$V085309=="2. Most of the time"] <- 4
datanes$sm3[datanes$V085309=="1. Always"] <- 5

# create self monitoring index
datanes$sm <- (datanes$sm1 + datanes$sm2 + datanes$sm3)/3

# median self monitoring index
median(datanes$sm, na.rm=TRUE)

# mean among those answering self-monitoring questions
mean(datanes$trustindex[datanes$sm>0], na.rm=TRUE)

# t-test between high and low self-monitors
t.test(datanes$trustindex[datanes$sm<2], datanes$trustindex[datanes$sm>1.99])






