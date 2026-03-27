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

## Subset Racially Homogenous Precincts
nsr.prc.eth <- nsr.prc
nsr.prc.eth[, -1] <- nsr.prc[, -1] / apply(nsr.prc[, -1], 1, sum)

prc.hom <- nsr.prc.eth[(!is.na(nsr.prc.eth$whi) & nsr.prc.eth$whi >= .9) | 
                         (!is.na(nsr.prc.eth$bla) & nsr.prc.eth$bla >= .9) | 
                         (!is.na(nsr.prc.eth$his) & nsr.prc.eth$his >= .9) | 
                         (!is.na(nsr.prc.eth$asi) & nsr.prc.eth$asi >= .9), "Precinct"]
length(prc.hom) #2,567 homogenous precincts
nrow(nsr.prc.eth[nsr.prc.eth$whi >= .9, ]) / length(prc.hom) #92 percent homogenously white

nsr.prc <- nsr.prc[nsr.prc$Precinct %in% prc.hom, ]
vote.prc <- vote.prc[vote.prc$Precinct %in% prc.hom, ]


## Goodman's Regression (Univariate)
load("good.uv.prc.RData")

good.uv.prc <- good.uv.prc[order(good.uv.prc$Precinct), c(1, 8:12)] #Sort by Precinct and subset columns
good.uv.prc <- good.uv.prc[good.uv.prc$Precinct %in% prc.hom, ]

good.uv.prc.bias <- good.uv.prc.rmse <- rep(NA, 5)
for (k in 1:5) {
  good.uv.prc.bias[k] <- sum(nsr.prc[, k + 1] * (good.uv.prc[, k + 1] - vote.prc[, k + 1]), na.rm = T) / sum(nsr.prc[, k + 1])
  good.uv.prc.rmse[k] <- sqrt(sum(nsr.prc[, k + 1] * (good.uv.prc[, k + 1] - vote.prc[, k + 1]) ^ 2, na.rm = T) / sum(nsr.prc[, k + 1], na.rm = T))
}

## King et al.'s EI
load("ei.prc.RData")

ei.prc <- ei.prc[order(ei.prc$Precinct), c(2, 5:9)] #Sort by Precinct and subset columns
ei.prc <- ei.prc[ei.prc$Precinct %in% prc.hom, ]

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

vote.pred1 <- vote.pred1[vote.pred1$Precinct %in% prc.hom, ]
vote.pred1.n <- vote.pred1.n[vote.pred1.n$Precinct %in% prc.hom, ]

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

vote.pred4 <- vote.pred4[vote.pred4$Precinct %in% prc.hom, ]
vote.pred4.n <- vote.pred4.n[vote.pred4.n$Precinct %in% prc.hom, ]

vote.pred4[, -1] <- vote.pred4[, -1] / vote.pred4.n[, -1] #Predicted probability of turning out

pred4.prc.bias <- pred4.prc.rmse <- rep(NA, 5)
for (k in 1:5) {
  pred4.prc.bias[k] <- sum(nsr.prc[, k + 1] * (vote.pred4[, k + 1] - vote.prc[, k + 1]), na.rm = T) / sum(nsr.prc[, k + 1])
  pred4.prc.rmse[k] <- sqrt(sum(nsr.prc[, k + 1] * (vote.pred4[, k + 1] - vote.prc[, k + 1]) ^ 2, na.rm = T) / sum(nsr.prc[, k + 1], na.rm = T))
}


## Table 6
table6 <- cbind(good.uv.prc.bias, good.uv.prc.rmse, ei.prc.bias, ei.prc.rmse, 
                pred1.prc.bias, pred1.prc.rmse, pred4.prc.bias, pred4.prc.rmse)
write.csv(table6, "table6.csv")
xtable(table6, digits = 3)
