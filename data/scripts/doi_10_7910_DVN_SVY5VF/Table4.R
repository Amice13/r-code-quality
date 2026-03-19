## Author: Kabir Khanna
## Updated: December 17, 2015
## Note: First run Predict.R and Predict.EM.Combine.R

library(xtable)
source("error.R")

for (j in 1:13) {
  load(paste("df.pred", j, ".RData", sep = ""))
  class.error <- as.matrix(error(get(paste("df.pred", j, sep = ""))))
  table4.j <- cbind(round(class.error[, 2] / class.error[, 1], 3), 
                    round(class.error[, 3] / class.error[, 4], 3), round(class.error[, 5] / class.error[, 6], 3), 
                    round(class.error[, 7] / class.error[, 8], 3), round(class.error[, 9] / class.error[, 10], 3), 
                    round(class.error[, 11] / class.error[, 12], 3), round(class.error[, 13] / class.error[, 14], 3), 
                    round(class.error[, 15] / class.error[, 16], 3), round(class.error[, 17] / class.error[, 18], 3)
                    )
  write.csv(table4.j, paste("table4.pred", j, ".csv", sep = ""))
  print(j)
}

## Table 4
table4 <- matrix(NA, nrow = 13, ncol = 9)
pred.order <- c(1, 2, 6, 4, 8, 10, 12, 3, 7, 5, 9, 11, 13) #Order predictions appear by row in Table 4
for (j in 1:13) {
  table4[j, ] <- as.matrix(read.csv(paste("table4.pred", pred.order[j], ".csv", sep = ""))[, -c(1, 11:12)])
}
write.csv(table4, "table4.csv")
xtable(table4, digits = 3)
