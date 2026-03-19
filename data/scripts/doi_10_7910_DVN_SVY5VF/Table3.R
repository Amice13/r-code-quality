## Author: Kabir Khanna
## Updated: December 20, 2015
## Note: First run Predict.R and Predict.L2.R

library(xtable)
source("error.R")

load("df.pred6.RData")
load("df.l2.pred6.RData")
load("df.pred8.RData")
load("df.l2.pred8.RData")

class.error <- matrix(NA, nrow = 4, ncol = 22)
class.error[1, ] <- as.matrix(error(df.pred6))
class.error[2, ] <- as.matrix(error(df.l2.pred6))
class.error[3, ] <- as.matrix(error(df.pred8))
class.error[4, ] <- as.matrix(error(df.l2.pred8))

## Table 3
table3 <- rbind(round(class.error[, 2] / class.error[, 1], 3), 
                round(class.error[, 3] / class.error[, 4], 3), 
                round(class.error[, 5] / class.error[, 6], 3), 
                round(class.error[, 7] / class.error[, 8], 3), 
                round(class.error[, 9] / class.error[, 10], 3), 
                round(class.error[, 11] / class.error[, 12], 3), 
                round(class.error[, 13] / class.error[, 14], 3), 
                round(class.error[, 15] / class.error[, 16], 3), 
                round(class.error[, 17] / class.error[, 18], 3))
write.csv(table3, "table3.csv")
xtable(table3, digits = 3)
