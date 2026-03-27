library(haven)
library(randomForest)

# d <- as.data.frame(data[, c(18,89:148)])
# dctrl <- as.data.frame(data[,c(18, 33:72, 74:84, 89:148 )])

rf1 <- randomForest(adopted~. , data=d, importance=TRUE, trees=500)
rf1

varImpPlot(rf1, type=2)
sort(importance(rf1, type=2))

rf2 <- randomForest(adopted~. , data=dctrl, importance=TRUE, trees=500)
rf2

varImpPlot(rf2, type=2)
importance(rf2, type=2)
