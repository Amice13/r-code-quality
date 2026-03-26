## Author: Kabir Khanna
## Updated: December 17, 2015
## Note: First run Goodman.R, KingEI.R, and Turnout.R

library(xtable)

eth <- c("whi", "bla", "his", "asi", "oth")

## Precinct-Level Turnout

## Observed Turnout and Number of Voters
load("nsr.prc.RData")
load("vote.prc.RData")

nsr.prc <- nsr.prc[order(nsr.prc$Precinct), ] #Sort by Precinct
vote.prc <- vote.prc[order(vote.prc$Precinct), ] #Sort by Precinct

## Goodman's Regression (Univariate)
load("good.uv.prc.RData")

good.uv.prc <- good.uv.prc[order(good.uv.prc$Precinct), c(1, 8:12)] #Sort by Precinct and subset columns

good.uv.prc.bias <- good.uv.prc.rmse <- rep(NA, 5)
for (k in 1:5) {
  good.uv.prc.bias[k] <- sum(nsr.prc[, k + 1] * (good.uv.prc[, k + 1] - vote.prc[, k + 1]), na.rm = T) / sum(nsr.prc[, k + 1])
  good.uv.prc.rmse[k] <- sqrt(sum(nsr.prc[, k + 1] * (good.uv.prc[, k + 1] - vote.prc[, k + 1]) ^ 2, na.rm = T) / sum(nsr.prc[, k + 1], na.rm = T))
}

## King et al.'s EI
load("ei.prc.RData")

ei.prc <- ei.prc[order(ei.prc$Precinct), c(2, 5:9)] #Sort by Precinct and subset columns

ei.prc.bias <- ei.prc.rmse <- rep(NA, 5)
for (k in 1:5) {
  ei.prc.bias[k] <- sum(nsr.prc[, k + 1] * (ei.prc[, k + 1] - vote.prc[, k + 1]), na.rm = T) / sum(nsr.prc[, k + 1])
  ei.prc.rmse[k] <- sqrt(sum(nsr.prc[, k + 1] * (ei.prc[, k + 1] - vote.prc[, k + 1]) ^ 2, na.rm = T) / sum(nsr.prc[, k + 1], na.rm = T))
}

## Surname-Only Predictions
load("vote.pred1.RData")
load("vote.pred1.n.RData")

vote.pred1 <- vote.pred1[order(vote.pred1$Precinct), ] #Sort by Precinct
vote.pred1.n <- vote.pred1.n[order(vote.pred1.n$Precinct), ] #Sort by Precinct
vote.pred1[, -1] <- vote.pred1[, -1] / vote.pred1.n[, -1] #Predicted probability of turning out

pred1.prc.bias <- pred1.prc.rmse <- rep(NA, 5)
for (k in 1:5) {
  pred1.prc.bias[k] <- sum(nsr.prc[, k + 1] * (vote.pred1[, k + 1] - vote.prc[, k + 1]), na.rm = T) / sum(nsr.prc[, k + 1])
  pred1.prc.rmse[k] <- sqrt(sum(nsr.prc[, k + 1] * (vote.pred1[, k + 1] - vote.prc[, k + 1]) ^ 2, na.rm = T) / sum(nsr.prc[, k + 1], na.rm = T))
}

## Surname, Precinct, Party
load("vote.pred4.RData")
load("vote.pred4.n.RData")

vote.pred4 <- vote.pred4[order(vote.pred4$Precinct), ] #Sort by Precinct
vote.pred4.n <- vote.pred4.n[order(vote.pred4.n$Precinct), ] #Sort by Precinct
vote.pred4[, -1] <- vote.pred4[, -1] / vote.pred4.n[, -1] #Predicted probability of turning out

pred4.prc.bias <- pred4.prc.rmse <- rep(NA, 5)
for (k in 1:5) {
  pred4.prc.bias[k] <- sum(nsr.prc[, k + 1] * (vote.pred4[, k + 1] - vote.prc[, k + 1]), na.rm = T) / sum(nsr.prc[, k + 1])
  pred4.prc.rmse[k] <- sqrt(sum(nsr.prc[, k + 1] * (vote.pred4[, k + 1] - vote.prc[, k + 1]) ^ 2, na.rm = T) / sum(nsr.prc[, k + 1], na.rm = T))
}



## District-Level Turnout

## Observed Turnout and Number of Voters
load("nsr.cd.RData")
load("vote.cd.RData")

## Goodman's Regression (Univariate)
load("good.uv.cd.RData")

good.uv.cd.bias <- good.uv.cd.rmse <- rep(NA, 5)
for (k in 1:5) {
  good.uv.cd.bias[k] <- mean(good.uv.cd[, k ] - vote.cd[, k + 1], na.rm = T)
  good.uv.cd.rmse[k] <- sqrt(mean((good.uv.cd[, k ] - vote.cd[, k + 1]) ^ 2, na.rm = T))
}

## King et al.'s EI
load("ei.cd.RData")

ei.cd.bias <- ei.cd.rmse <- rep(NA, 5)
for (k in 1:5) {
  ei.cd.bias[k] <- mean(ei.cd[, k ] - vote.cd[, k + 1], na.rm = T)
  ei.cd.rmse[k] <- sqrt(mean((ei.cd[, k] - vote.cd[, k + 1]) ^ 2, na.rm = T))
}

## Surname-Only Predictions
load("vote.cd.pred1.RData")
load("vote.cd.pred1.n.RData")

vote.cd.pred1[, -1] <- vote.cd.pred1[, -1] / vote.cd.pred1.n[, -1] #Predicted probability of turning out

pred1.cd.bias <- pred1.cd.rmse <- rep(NA, 5)
for (k in 1:5) {
  pred1.cd.bias[k] <- mean((vote.cd.pred1[, k + 1] - vote.cd[, k + 1]), na.rm = T)
  pred1.cd.rmse[k] <- sqrt(mean((vote.cd.pred1[, k + 1] - vote.cd[, k + 1]) ^ 2, na.rm = T))
}

## Surname, Precinct, Party
load("vote.cd.pred4.RData")
load("vote.cd.pred4.n.RData")

vote.cd.pred4[, -1] <- vote.cd.pred4[, -1] / vote.cd.pred4.n[, -1] #Predicted probability of turning out

pred4.cd.bias <- pred4.cd.rmse <- rep(NA, 5)
for (k in 1:5) {
  pred4.cd.bias[k] <- mean((vote.cd.pred4[, k + 1] - vote.cd[, k + 1]), na.rm = T)
  pred4.cd.rmse[k] <- sqrt(mean((vote.cd.pred4[, k + 1] - vote.cd[, k + 1]) ^ 2, na.rm = T))
}


## Table 2
table2.prc <- cbind(good.uv.prc.bias, good.uv.prc.rmse, ei.prc.bias, ei.prc.rmse, 
                    pred1.prc.bias, pred1.prc.rmse, pred4.prc.bias, pred4.prc.rmse)
table2.cd <- cbind(good.uv.cd.bias, good.uv.cd.rmse, ei.cd.bias, ei.cd.rmse, 
                   pred1.cd.bias, pred1.cd.rmse, pred4.cd.bias, pred4.cd.rmse)
table2 <- rbind(table2.prc, table2.cd)
write.csv(table2, "table2.csv")
xtable(table2, digits = 3)
