## Author: Kabir Khanna
## Updated: December 20, 2015
## Note: First run L2.Demo.R

library(wru)
source("classify.R")

## Load Replication Dataset with Pr(Precinct, Age, Gender | Race) from L2
load("df.l2.RData")

eth <- c("whi", "bla", "his", "asi", "oth")

## Name, Precinct, and Demographics, i.e., Pr(Race | Surname, Precinct, Age, Sex)
for (k in 1:length(eth)) {
  df.l2[paste("r", eth[k], sep = "_")] <- df.l2[paste("l2_prc_age_sex", eth[k], sep = "_")]
}
df.l2.pred6 <- race.pred(voters = df.l2, name.clean = FALSE)
df.l2.pred6 <- classify(df.l2.pred6)
save(df.l2.pred6, file="df.l2.pred6.RData")

## Name, Precinct, Demographics, and Party, i.e., Pr(Race | Surname, Precinct, Age, Gender, Party)
for (k in 1:length(eth)) {
  df.l2[paste("r", eth[k], sep = "_")] <- df.l2[paste("l2_prc_age_sex", eth[k], sep = "_")] * df.l2[paste("r_pid", eth[k], sep = "_")]
}
df.l2.pred8 <- race.pred(voters = df.l2, name.clean = FALSE)
df.l2.pred8 <- classify(df.l2.pred8)
save(df.l2.pred8, file="df.l2.pred8.RData")
