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

## Goodman's Regression (Multivariate)
load("good.mv.prc.RData")

good.mv.prc <- good.mv.prc[order(good.mv.prc$Precinct), c(1, 8:12)] #Sort by Precinct and subset columns

good.mv.prc.bias <- good.mv.prc.rmse <- rep(NA, 5)
for (k in 1:5) {
  good.mv.prc.bias[k] <- sum(nsr.prc[, k + 1] * (good.mv.prc[, k + 1] - vote.prc[, k + 1]), na.rm = T) / sum(nsr.prc[, k + 1])
  good.mv.prc.rmse[k] <- sqrt(sum(nsr.prc[, k + 1] * (good.mv.prc[, k + 1] - vote.prc[, k + 1]) ^ 2, na.rm = T) / sum(nsr.prc[, k + 1], na.rm = T))
}

## Surname-Only Classifications
load("vote.pred1.class.RData")
load("vote.pred1.class.n.RData")

vote.pred1.class <- vote.pred1.class[order(vote.pred1.class$Precinct), ] #Sort by Precinct
vote.pred1.class.n <- vote.pred1.class.n[order(vote.pred1.class.n$Precinct), ] #Sort by Precinct
vote.pred1.class[, -1] <- vote.pred1.class[, -1] / vote.pred1.class.n[, -1] #Predicted probability of turning out

pred1.class.prc.bias <- pred1.class.prc.rmse <- rep(NA, 5)
for (k in 1:5) {
  pred1.class.prc.bias[k] <- sum(nsr.prc[, k + 1] * (vote.pred1.class[, k + 1] - vote.prc[, k + 1]), na.rm = T) / sum(nsr.prc[, k + 1])
  pred1.class.prc.rmse[k] <- sqrt(sum(nsr.prc[, k + 1] * (vote.pred1.class[, k + 1] - vote.prc[, k + 1]) ^ 2, na.rm = T) / sum(nsr.prc[, k + 1], na.rm = T))
}

## Surname, Precinct, Party Classifications
load("vote.pred4.class.RData")
load("vote.pred4.class.n.RData")

vote.pred4.class <- vote.pred4.class[order(vote.pred4.class$Precinct), ] #Sort by Precinct
vote.pred4.class.n <- vote.pred4.class.n[order(vote.pred4.class.n$Precinct), ] #Sort by Precinct
vote.pred4.class[, -1] <- vote.pred4.class[, -1] / vote.pred4.class.n[, -1] #Predicted probability of turning out

pred4.class.prc.bias <- pred4.class.prc.rmse <- rep(NA, 5)
for (k in 1:5) {
  pred4.class.prc.bias[k] <- sum(nsr.prc[, k + 1] * (vote.pred4.class[, k + 1] - vote.prc[, k + 1]), na.rm = T) / sum(nsr.prc[, k + 1])
  pred4.class.prc.rmse[k] <- sqrt(sum(nsr.prc[, k + 1] * (vote.pred4.class[, k + 1] - vote.prc[, k + 1]) ^ 2, na.rm = T) / sum(nsr.prc[, k + 1], na.rm = T))
}


## District-Level Turnout

## Observed Turnout and Number of Voters
load("nsr.cd.RData")
load("vote.cd.RData")

## Goodman's Regression (Multivariate)
load("good.mv.cd.RData")

good.mv.cd.bias <- good.mv.cd.rmse <- rep(NA, 5)
for (k in 1:5) {
  good.mv.cd.bias[k] <- mean(good.mv.cd[, k ] - vote.cd[, k + 1], na.rm = T)
  good.mv.cd.rmse[k] <- sqrt(mean((good.mv.cd[, k ] - vote.cd[, k + 1]) ^ 2, na.rm = T))
}

## Surname-Only Classifications
load("vote.cd.pred1.class.RData")
load("vote.cd.pred1.class.n.RData")

vote.cd.pred1.class[, -1] <- vote.cd.pred1.class[, -1] / vote.cd.pred1.class.n[, -1] #Predicted probability of turning out

pred1.class.cd.bias <- pred1.class.cd.rmse <- rep(NA, 5)
for (k in 1:5) {
  pred1.class.cd.bias[k] <- mean((vote.cd.pred1.class[, k + 1] - vote.cd[, k + 1]), na.rm = T)
  pred1.class.cd.rmse[k] <- sqrt(mean((vote.cd.pred1.class[, k + 1] - vote.cd[, k + 1]) ^ 2, na.rm = T))
}

## Surname, Precinct, Party Classifications
load("vote.cd.pred4.class.RData")
load("vote.cd.pred4.class.n.RData")

vote.cd.pred4.class[, -1] <- vote.cd.pred4.class[, -1] / vote.cd.pred4.class.n[, -1] #Predicted probability of turning out

pred4.class.cd.bias <- pred4.class.cd.rmse <- rep(NA, 5)
for (k in 1:5) {
  pred4.class.cd.bias[k] <- mean((vote.cd.pred4.class[, k + 1] - vote.cd[, k + 1]), na.rm = T)
  pred4.class.cd.rmse[k] <- sqrt(mean((vote.cd.pred4.class[, k + 1] - vote.cd[, k + 1]) ^ 2, na.rm = T))
}

## Table 5
table5.prc <- cbind(good.mv.prc.bias, good.mv.prc.rmse, 
                    pred1.class.prc.bias, pred1.class.prc.rmse, 
                    pred4.class.prc.bias, pred4.class.prc.rmse)
table5.cd <- cbind(good.mv.cd.bias, good.mv.cd.rmse, 
                   pred1.class.cd.bias, pred1.class.cd.rmse, 
                   pred4.class.cd.bias, pred4.class.cd.rmse)
table5 <- rbind(table5.prc, table5.cd)
write.csv(table5, "table5.csv")
xtable(table5, digits = 3)
