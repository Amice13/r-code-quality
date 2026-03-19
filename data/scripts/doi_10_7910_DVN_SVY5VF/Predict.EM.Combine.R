## Author: Kabir Khanna
## Updated: December 18, 2015
## First run Predict.EM1.Precinct.R, Predict.EM1.Block.R, Predict.EM2.Precinct.R, and Predict.EM2.Block.R

source("classify.R")

eth <- c("whi", "bla", "his", "asi", "oth")
SR <- c("SR.WHI", "SR.BLA", "SR.HIS", "SR.ASI", "SR.OTH")
keep <- c("VoterID", "District", "Precinct", "vote08", SR, paste("pred", eth, sep = "."))

df.pred10 <- NULL
for (i in 1:25) {
  load(paste("FL", i, ".EM1.Precinct.RData", sep = ""))
  df.pred10 <- rbind(df.pred10, df.em)
  print(i)
}
df.pred10 <- classify(df.pred10[keep])
save(df.pred10, file = "df.pred10.RData")

df.pred11 <- NULL
for (i in 1:25) {
  load(paste("FL", i, ".EM1.Block.RData", sep = ""))
  df.pred11 <- rbind(df.pred11, df.em)
  print(i)
}
df.pred11 <- classify(df.pred11[keep])
save(df.pred11, file = "df.pred11.RData")

df.pred12 <- NULL
for (i in 1:25) {
  load(paste("FL", i, ".EM2.Precinct.RData", sep = ""))
  df.pred12 <- rbind(df.pred12, df.em)
  print(i)
}
df.pred12 <- classify(df.pred12[keep])
save(df.pred12, file = "df.pred12.RData")

df.pred13 <- NULL
for (i in 1:25) {
  load(paste("FL", i, ".EM2.Block.RData", sep = ""))
  df.pred13 <- rbind(df.pred13, df.em)
  print(i)
}
df.pred13 <- classify(df.pred13[keep])
save(df.pred13, file = "df.pred13.RData")
