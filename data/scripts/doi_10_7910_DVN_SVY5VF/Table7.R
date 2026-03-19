## Author: Kabir Khanna
## Updated: December 17, 2015
## Note: First run Predict.R

library(xtable)

eth <- c("whi", "bla", "his", "asi", "oth")
SR <- paste("SR", toupper(eth), sep = ".")

load("df.pred1.RData")
load("df.pred4.RData")

df.pred1.fn <- df.pred1[df.pred1$SR.WHI == 1 & df.pred1$pred.whi.class == 0, ]
df.pred1.fp <- df.pred1[df.pred1$SR.WHI == 0 & df.pred1$pred.whi.class == 1, ]

vote.pred1.fnfp <- vote.pred4.fnfp <- matrix(NA, nrow = 3, ncol = 5)
for (k in 1:5) {
  print(paste(eth[k], "begin"))
  
  df.pred1.fn <- df.pred1[df.pred1[SR[k]] == 1 & df.pred1[paste("pred", eth[k], "class", sep = ".")] == 0, ]
  df.pred1.fp <- df.pred1[df.pred1[SR[k]] == 0 & df.pred1[paste("pred", eth[k], "class", sep = ".")] == 1, ]
  vote.pred1.fnfp[1, k] <- table(df.pred1.fn$vote08)["1"] / sum(table(df.pred1.fn$vote08))
  vote.pred1.fnfp[2, k] <- table(df.pred1.fp$vote08)["1"] / sum(table(df.pred1.fp$vote08))

  df.pred4.fn <- df.pred4[df.pred4[SR[k]] == 1 & df.pred4[paste("pred", eth[k], "class", sep = ".")] == 0, ]
  df.pred4.fp <- df.pred4[df.pred4[SR[k]] == 0 & df.pred4[paste("pred", eth[k], "class", sep = ".")] == 1, ]
  vote.pred4.fnfp[1, k] <- table(df.pred4.fn$vote08)["1"] / sum(table(df.pred4.fn$vote08))
  vote.pred4.fnfp[2, k] <- table(df.pred4.fp$vote08)["1"] / sum(table(df.pred4.fp$vote08))
  
  print(paste(eth[k], "complete"))
}

vote.pred1.fnfp[3, ] <- vote.pred1.fnfp[1, ] - vote.pred1.fnfp[2, ]
vote.pred4.fnfp[3, ] <- vote.pred4.fnfp[1, ] - vote.pred4.fnfp[2, ]

## Table 7
table7 <- rbind(vote.pred1.fnfp, vote.pred4.fnfp)
write.csv(table7, "table7.csv")
xtable(table7, digits = 3)
