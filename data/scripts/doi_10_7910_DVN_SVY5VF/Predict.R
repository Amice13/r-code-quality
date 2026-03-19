## Author: Kabir Khanna
## Updated: December 19, 2015

library(wru)
source("classify.R")

## Load Replication Dataset
load("FL.Anon.RData")

## Subset Voters with Non-Missing Self-Reported Race
df <- fl.anon[!is.na(fl.anon$SR.WHI) & !is.na(fl.anon$SR.BLA) & !is.na(fl.anon$SR.HIS) & !is.na(fl.anon$SR.ASI) & !is.na(fl.anon$SR.OTH), ]
save(df, file = "df.RData")

eth <- c("whi", "bla", "his", "asi", "oth")
SR <- c("SR.WHI", "SR.BLA", "SR.HIS", "SR.ASI", "SR.OTH")
keep <- c("VoterID", "District", "Precinct", "vote08", SR, paste("pred", eth, sep = "."))

## Name-Only Prediction, i.e., Pr(Race | Surname)
df.pred1 <- race.pred(voters = df, name.clean = FALSE, surname.only = TRUE)
df.pred1 <- classify(df.pred1[keep])
save(df.pred1, file="df.pred1.RData")

## Name and Precinct, i.e., Pr(Race | Surname, Precinct)
for (k in 1:length(eth)) {
  df[paste("r", eth[k], sep = "_")] <- df[paste("r_prc", eth[k], sep = "_")]
}
df.pred2 <- race.pred(voters = df, name.clean = FALSE)
df.pred2 <- classify(df.pred2[keep])
save(df.pred2, file="df.pred2.RData")

## Name and Block, i.e., Pr(Race | Surname, Block)
for (k in 1:length(eth)) {
  df[paste("r", eth[k], sep = "_")] <- df[paste("r_blk", eth[k], sep = "_")]
}
df.pred3 <- race.pred(voters = df, name.clean = FALSE)
df.pred3 <- classify(df.pred3[keep])
save(df.pred3, file="df.pred3.RData")

## Name, Precinct, and Party, i.e., Pr(Race | Surname, Precinct, Party)
for (k in 1:length(eth)) {
  df[paste("r", eth[k], sep = "_")] <- df[paste("r_prc", eth[k], sep = "_")]
}
df.pred4 <- race.pred(voters = df, name.clean = FALSE, party = "PID")
df.pred4 <- classify(df.pred4[keep])
save(df.pred4, file="df.pred4.RData")

## Name, Block, and Party, i.e., Pr(Race | Surname, Block, Party)
for (k in 1:length(eth)) {
  df[paste("r", eth[k], sep = "_")] <- df[paste("r_blk", eth[k], sep = "_")]
}
df.pred5 <- race.pred(voters = df, name.clean = FALSE, party = "PID")
df.pred5 <- classify(df.pred5[keep])
save(df.pred5, file="df.pred5.RData")


### Appendix Table 4 Only ###

## Name, Precinct, and Demographics, i.e., Pr(Race | Surname, Precinct, Age, Gender)
for (k in 1:length(eth)) {
  df[paste("r", eth[k], sep = "_")] <- df[paste("r_prc_age_sex", eth[k], sep = "_")]
}
df.pred6 <- race.pred(voters = df, name.clean = FALSE)
df.pred6 <- classify(df.pred6[keep])
save(df.pred6, file="df.pred6.RData")

## Name, Block, and Demographics, i.e., Pr(Race | Surname, Block, Age, Gender)
for (k in 1:length(eth)) {
  df[paste("r", eth[k], sep = "_")] <- df[paste("r_blk_age_sex", eth[k], sep = "_")]
}
df.pred7 <- race.pred(voters = df, name.clean = FALSE)
df.pred7 <- classify(df.pred7[keep])
save(df.pred7, file="df.pred7.RData")

## Name, Precinct, Demographics, and Party, i.e., Pr(Race | Surname, Precinct, Age, Gender, Party)
for (k in 1:length(eth)) {
  df[paste("r", eth[k], sep = "_")] <- df[paste("r_prc_age_sex", eth[k], sep = "_")] * df[paste("r_pid", eth[k], sep = "_")]
}
df.pred8 <- race.pred(voters = df, name.clean = FALSE)
df.pred8 <- classify(df.pred8[keep])
save(df.pred8, file="df.pred8.RData")

## Name, Block, Demographics, and Party, i.e., Pr(Race | Surname, Block, Age, Gender, Party)
for (k in 1:length(eth)) {
  df[paste("r", eth[k], sep = "_")] <- df[paste("r_blk_age_sex", eth[k], sep = "_")] * df[paste("r_pid", eth[k], sep = "_")]
}
df.pred9 <- race.pred(voters = df, name.clean = FALSE)
df.pred9 <- classify(df.pred9[keep])
save(df.pred9, file="df.pred9.RData")
