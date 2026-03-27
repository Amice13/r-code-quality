## Author: Kabir Khanna
## Updated: December 15, 2015
## Note: First run Predict.R

library(xtable)
source("error.R")

load("df.pred1.RData")
load("df.pred2.RData")
load("df.pred3.RData")
load("df.pred4.RData")
load("df.pred5.RData")

class.error <- matrix(NA, nrow = 5, ncol = 22)
class.error[1, ] <- as.matrix(error(df.pred1))
class.error[2, ] <- as.matrix(error(df.pred2))
class.error[3, ] <- as.matrix(error(df.pred3))
class.error[4, ] <- as.matrix(error(df.pred4))
class.error[5, ] <- as.matrix(error(df.pred5))

## Table 1
table1 <- rbind(round(class.error[, 2] / class.error[, 1], 3), 
                round(class.error[, 3] / class.error[, 4], 3), round(class.error[, 5] / class.error[, 6], 3), 
                round(class.error[, 7] / class.error[, 8], 3), round(class.error[, 9] / class.error[, 10], 3), 
                round(class.error[, 11] / class.error[, 12], 3), round(class.error[, 13] / class.error[, 14], 3), 
                round(class.error[, 15] / class.error[, 16], 3), round(class.error[, 17] / class.error[, 18], 3), 
                round(class.error[, 19] / class.error[, 20], 3), round(class.error[, 21] / class.error[, 22], 3)
                )
write.csv(table1, "table1.csv")
xtable(table1, digts = 3)
