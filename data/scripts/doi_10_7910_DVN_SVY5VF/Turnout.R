## Author: Kabir Khanna
## Updated: December 17, 2015
## Note: First run Predict.R

load("df.RData")
load("df.pred1.RData")
load("df.pred4.RData")

eth <- c("whi", "bla", "his", "asi", "oth")

## Observed Turnout and Number of Voters by Precinct
precincts <- unique(df$Precinct)
nsr.prc <- vote.prc <- as.data.frame(matrix(NA, nrow = length(precincts), ncol = 6))
for (p in 1:length(precincts)) {
  df.prc <- df[df$Precinct == precincts[p], c("vote08", SR)]
  print(paste("Precinct ", precincts[p], " (", p, " of ", length(precincts), "): ", nrow(df.prc), " voters", sep = ""))
  nsr.prc[p, 1] <- vote.prc[p, 1] <- as.character(precincts[p])
  nsr.prc[p, 2:6] <- c(sum(df.prc$SR.WHI, na.rm = T), 
                       sum(df.prc$SR.BLA, na.rm = T), 
                       sum(df.prc$SR.HIS, na.rm = T), 
                       sum(df.prc$SR.ASI, na.rm = T), 
                       sum(df.prc$SR.OTH, na.rm = T))
  vote.prc[p, 2:6] <- c(sum(df.prc$SR.WHI * df.prc$vote08, na.rm = T) / sum(df.prc$SR.WHI, na.rm = T), 
                        sum(df.prc$SR.BLA * df.prc$vote08, na.rm = T) / sum(df.prc$SR.BLA, na.rm = T), 
                        sum(df.prc$SR.HIS * df.prc$vote08, na.rm = T) / sum(df.prc$SR.HIS, na.rm = T), 
                        sum(df.prc$SR.ASI * df.prc$vote08, na.rm = T) / sum(df.prc$SR.ASI, na.rm = T), 
                        sum(df.prc$SR.OTH * df.prc$vote08, na.rm = T) / sum(df.prc$SR.OTH, na.rm = T))
}
names(nsr.prc) <- names(vote.prc) <- c("Precinct", eth)
save(nsr.prc, file="nsr.prc.RData")
save(vote.prc, file="vote.prc.RData")

## Observed Turnout and Number of Voters by District
nsr.cd <- vote.cd <- as.data.frame(matrix(NA, nrow = 25, ncol = 6))
for (d in 1:25) {
  df.cd <- df[df$CD == d, c("vote08", SR)]
  print(paste("District ", d, " of 25: ", nrow(df.cd), " voters", sep = ""))
  nsr.cd[d, 1] <- vote.cd[d, 1] <- d
  nsr.cd[d, 2:6] <- c(sum(df.cd$SR.WHI, na.rm = T), 
                      sum(df.cd$SR.BLA, na.rm = T), 
                      sum(df.cd$SR.HIS, na.rm = T), 
                      sum(df.cd$SR.ASI, na.rm = T), 
                      sum(df.cd$SR.OTH, na.rm = T))
  vote.cd[d, 2:6] <- c(sum(df.cd$SR.WHI * df.cd$vote08, na.rm = T) / sum(df.cd$SR.WHI, na.rm = T), 
                      sum(df.cd$SR.BLA * df.cd$vote08, na.rm = T) / sum(df.cd$SR.BLA, na.rm = T), 
                      sum(df.cd$SR.HIS * df.cd$vote08, na.rm = T) / sum(df.cd$SR.HIS, na.rm = T), 
                      sum(df.cd$SR.ASI * df.cd$vote08, na.rm = T) / sum(df.cd$SR.ASI, na.rm = T), 
                      sum(df.cd$SR.OTH * df.cd$vote08, na.rm = T) / sum(df.cd$SR.OTH, na.rm = T))
}
names(nsr.cd) <- names(vote.cd) <- c("CD", eth)
save(nsr.cd, file="nsr.cd.RData")
save(vote.cd, file="vote.cd.RData")


## Estimate Precinct-Level Turnout by Race

## Name-Only Prediction
precincts <- unique(df.pred1$Precinct)
vote.pred1 <- vote.pred1.n <- as.data.frame(matrix(NA, nrow = length(precincts), ncol = 6))
for (p in 1:length(precincts)) {
  df.prc <- df.pred1[df.pred1$Precinct == precincts[p], ]
  print(paste("Precinct ", precincts[p], " (", p, " of ", length(precincts), "): ", nrow(df.prc), " voters", sep = ""))
  vote.pred1[p, 1] <- vote.pred1.n[p, 1] <- as.character(precincts[p])
  vote.pred1[p, 2:6] <- c(sum(df.prc$pred.whi * df.prc$vote08, na.rm = T), 
                          sum(df.prc$pred.bla * df.prc$vote08, na.rm = T), 
                          sum(df.prc$pred.his * df.prc$vote08, na.rm = T), 
                          sum(df.prc$pred.asi * df.prc$vote08, na.rm = T), 
                          sum(df.prc$pred.oth * df.prc$vote08, na.rm = T))
  vote.pred1.n[p, 2:6] <- c(sum(df.prc$pred.whi, na.rm = T), 
                            sum(df.prc$pred.bla, na.rm = T), 
                            sum(df.prc$pred.his, na.rm = T), 
                            sum(df.prc$pred.asi, na.rm = T), 
                            sum(df.prc$pred.oth, na.rm = T))
}
names(vote.pred1) <- names(vote.pred1.n) <- c("Precinct", eth)
save(vote.pred1, file="vote.pred1.RData")
save(vote.pred1.n, file="vote.pred1.n.RData")

## Name-Only Classification
precincts <- unique(df.pred1$Precinct)
vote.pred1.class <- vote.pred1.class.n <- as.data.frame(matrix(NA, nrow = length(precincts), ncol = 6))
for (p in 1:length(precincts)) {
  df.prc <- df.pred1[df.pred1$Precinct == precincts[p], ]
  print(paste("Precinct ", precincts[p], " (", p, " of ", length(precincts), "): ", nrow(df.prc), " voters", sep = ""))
  vote.pred1.class[p, 1] <- vote.pred1.class.n[p, 1] <- as.character(precincts[p])
  vote.pred1.class[p, 2:6] <- c(sum(df.prc$pred.whi.class * df.prc$vote08, na.rm = T), 
                                sum(df.prc$pred.bla.class * df.prc$vote08, na.rm = T), 
                                sum(df.prc$pred.his.class * df.prc$vote08, na.rm = T), 
                                sum(df.prc$pred.asi.class * df.prc$vote08, na.rm = T), 
                                sum(df.prc$pred.oth.class * df.prc$vote08, na.rm = T))
  vote.pred1.class.n[p, 2:6] <- c(sum(df.prc$pred.whi.class, na.rm = T), 
                                  sum(df.prc$pred.bla.class, na.rm = T), 
                                  sum(df.prc$pred.his.class, na.rm = T), 
                                  sum(df.prc$pred.asi.class, na.rm = T), 
                                  sum(df.prc$pred.oth.class, na.rm = T))
}
names(vote.pred1.class) <- names(vote.pred1.class.n) <- c("Precinct", eth)
save(vote.pred1.class, file="vote.pred1.class.RData")
save(vote.pred1.class.n, file="vote.pred1.class.n.RData")

## Bayesian Prediction
precincts <- unique(df.pred4$Precinct)
vote.pred4 <- vote.pred4.n <- as.data.frame(matrix(NA, nrow = length(precincts), ncol = 6))
for (p in 1:length(precincts)) {
  df.prc <- df.pred4[df.pred4$Precinct == precincts[p], ]
  print(paste("Precinct ", precincts[p], " (", p, " of ", length(precincts), "): ", nrow(df.prc), " voters", sep = ""))
  vote.pred4[p, 1] <- vote.pred4.n[p, 1] <- as.character(precincts[p])
  vote.pred4[p, 2:6] <- c(sum(df.prc$pred.whi * df.prc$vote08, na.rm = T), 
                          sum(df.prc$pred.bla * df.prc$vote08, na.rm = T), 
                          sum(df.prc$pred.his * df.prc$vote08, na.rm = T), 
                          sum(df.prc$pred.asi * df.prc$vote08, na.rm = T), 
                          sum(df.prc$pred.oth * df.prc$vote08, na.rm = T))
  vote.pred4.n[p, 2:6] <- c(sum(df.prc$pred.whi, na.rm = T), 
                            sum(df.prc$pred.bla, na.rm = T), 
                            sum(df.prc$pred.his, na.rm = T), 
                            sum(df.prc$pred.asi, na.rm = T), 
                            sum(df.prc$pred.oth, na.rm = T))
}
names(vote.pred4) <- names(vote.pred4.n) <- c("Precinct", eth)
save(vote.pred4, file="vote.pred4.RData")
save(vote.pred4.n, file="vote.pred4.n.RData")

## Bayesian Classification
precincts <- unique(df.pred4$Precinct)
vote.pred4.class <- vote.pred4.class.n <- as.data.frame(matrix(NA, nrow = length(precincts), ncol = 6))
for (p in 1:length(precincts)) {
  df.prc <- df.pred4[df.pred4$Precinct == precincts[p], ]
  print(paste("Precinct ", precincts[p], " (", p, " of ", length(precincts), "): ", nrow(df.prc), " voters", sep = ""))
  vote.pred4.class[p, 1] <- vote.pred4.class.n[p, 1] <- as.character(precincts[p])
  vote.pred4.class[p, 2:6] <- c(sum(df.prc$pred.whi.class * df.prc$vote08, na.rm = T), 
                                sum(df.prc$pred.bla.class * df.prc$vote08, na.rm = T), 
                                sum(df.prc$pred.his.class * df.prc$vote08, na.rm = T), 
                                sum(df.prc$pred.asi.class * df.prc$vote08, na.rm = T), 
                                sum(df.prc$pred.oth.class * df.prc$vote08, na.rm = T))
  vote.pred4.class.n[p, 2:6] <- c(sum(df.prc$pred.whi.class, na.rm = T), 
                                  sum(df.prc$pred.bla.class, na.rm = T), 
                                  sum(df.prc$pred.his.class, na.rm = T), 
                                  sum(df.prc$pred.asi.class, na.rm = T), 
                                  sum(df.prc$pred.oth.class, na.rm = T))
}
names(vote.pred4.class) <- names(vote.pred4.class.n) <- c("Precinct", eth)
save(vote.pred4.class, file="vote.pred4.class.RData")
save(vote.pred4.class.n, file="vote.pred4.class.n.RData")


## Estimate District-Level Turnout by Race

## Name-Only Prediction
vote.cd.pred1 <- vote.cd.pred1.n <- as.data.frame(matrix(NA, nrow = 25, ncol = 6))
for (d in 1:25) {
  df.cd <- df.pred1[df.pred1$CD == d, ]
  print(paste("District ", d, " of 25: ", nrow(df.cd), " voters", sep = ""))
  vote.cd.pred1[d, 1] <- vote.cd.pred1.n[d, 1] <- d
  vote.cd.pred1[d, 2:6] <- c(sum(df.cd$pred.whi * df.cd$vote08, na.rm = T), 
                             sum(df.cd$pred.bla * df.cd$vote08, na.rm = T), 
                             sum(df.cd$pred.his * df.cd$vote08, na.rm = T), 
                             sum(df.cd$pred.asi * df.cd$vote08, na.rm = T), 
                             sum(df.cd$pred.oth * df.cd$vote08, na.rm = T))
  vote.cd.pred1.n[d, 2:6] <- c(sum(df.cd$pred.whi, na.rm = T), 
                               sum(df.cd$pred.bla, na.rm = T), 
                               sum(df.cd$pred.his, na.rm = T), 
                               sum(df.cd$pred.asi, na.rm = T), 
                               sum(df.cd$pred.oth, na.rm = T))
}
names(vote.cd.pred1) <- names(vote.cd.pred1.n) <- c("CD", eth)
save(vote.cd.pred1, file="vote.cd.pred1.RData")
save(vote.cd.pred1.n, file="vote.cd.pred1.n.RData")

## Name-Only Classification
vote.cd.pred1.class <- vote.cd.pred1.class.n <- as.data.frame(matrix(NA, nrow = 25, ncol = 6))
for (d in 1:25) {
  df.cd <- df.pred1[df.pred1$CD == d, ]
  print(paste("District ", d, " of 25: ", nrow(df.cd), " voters", sep = ""))
  vote.cd.pred1.class[d, 1] <- vote.cd.pred1.class.n[d, 1] <- d
  vote.cd.pred1.class[d, 2:6] <- c(sum(df.cd$pred.whi.class * df.cd$vote08, na.rm = T), 
                                   sum(df.cd$pred.bla.class * df.cd$vote08, na.rm = T), 
                                   sum(df.cd$pred.his.class * df.cd$vote08, na.rm = T), 
                                   sum(df.cd$pred.asi.class * df.cd$vote08, na.rm = T), 
                                   sum(df.cd$pred.oth.class * df.cd$vote08, na.rm = T))
  vote.cd.pred1.class.n[d, 2:6] <- c(sum(df.cd$pred.whi.class, na.rm = T), 
                                     sum(df.cd$pred.bla.class, na.rm = T), 
                                     sum(df.cd$pred.his.class, na.rm = T), 
                                     sum(df.cd$pred.asi.class, na.rm = T), 
                                     sum(df.cd$pred.oth.class, na.rm = T))
}
names(vote.cd.pred1.class) <- names(vote.cd.pred1.class.n) <- c("CD", eth)
save(vote.cd.pred1.class, file="vote.cd.pred1.class.RData")
save(vote.cd.pred1.class.n, file="vote.cd.pred1.class.n.RData")

## Bayesian Prediction
vote.cd.pred4 <- vote.cd.pred4.n <- as.data.frame(matrix(NA, nrow = 25, ncol = 6))
for (d in 1:25) {
  df.cd <- df.pred4[df.pred4$CD == d, ]
  print(paste("District ", d, " of 25: ", nrow(df.cd), " voters", sep = ""))
  vote.cd.pred4[d, 1] <- vote.cd.pred4.n[d, 1] <- d
  vote.cd.pred4[d, 2:6] <- c(sum(df.cd$pred.whi * df.cd$vote08, na.rm = T), 
                             sum(df.cd$pred.bla * df.cd$vote08, na.rm = T), 
                             sum(df.cd$pred.his * df.cd$vote08, na.rm = T), 
                             sum(df.cd$pred.asi * df.cd$vote08, na.rm = T), 
                             sum(df.cd$pred.oth * df.cd$vote08, na.rm = T))
  vote.cd.pred4.n[d, 2:6] <- c(sum(df.cd$pred.whi, na.rm = T), 
                               sum(df.cd$pred.bla, na.rm = T), 
                               sum(df.cd$pred.his, na.rm = T), 
                               sum(df.cd$pred.asi, na.rm = T), 
                               sum(df.cd$pred.oth, na.rm = T))
}
names(vote.cd.pred4) <- names(vote.cd.pred4.n) <- c("CD", eth)
save(vote.cd.pred4, file="vote.cd.pred4.RData")
save(vote.cd.pred4.n, file="vote.cd.pred4.n.RData")

## Bayesian Classification
vote.cd.pred4.class <- vote.cd.pred4.class.n <- as.data.frame(matrix(NA, nrow = 25, ncol = 6))
for (d in 1:25) {
  df.cd <- df.pred4[df.pred4$CD == d, ]
  print(paste("District ", d, " of 25: ", nrow(df.cd), " voters", sep = ""))
  vote.cd.pred4.class[d, 1] <- vote.cd.pred4.class.n[d, 1] <- d
  vote.cd.pred4.class[d, 2:6] <- c(sum(df.cd$pred.whi.class * df.cd$vote08, na.rm = T), 
                                   sum(df.cd$pred.bla.class * df.cd$vote08, na.rm = T), 
                                   sum(df.cd$pred.his.class * df.cd$vote08, na.rm = T), 
                                   sum(df.cd$pred.asi.class * df.cd$vote08, na.rm = T), 
                                   sum(df.cd$pred.oth.class * df.cd$vote08, na.rm = T))
  vote.cd.pred4.class.n[d, 2:6] <- c(sum(df.cd$pred.whi.class, na.rm = T), 
                                     sum(df.cd$pred.bla.class, na.rm = T), 
                                     sum(df.cd$pred.his.class, na.rm = T), 
                                     sum(df.cd$pred.asi.class, na.rm = T), 
                                     sum(df.cd$pred.oth.class, na.rm = T))
}
names(vote.cd.pred4.class) <- names(vote.cd.pred4.class.n) <- c("CD", eth)
save(vote.cd.pred4.class, file="vote.cd.pred4.class.RData")
save(vote.cd.pred4.class.n, file="vote.cd.pred4.class.n.RData")
