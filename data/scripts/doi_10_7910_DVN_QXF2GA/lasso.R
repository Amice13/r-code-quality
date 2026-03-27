
library(caret)
library(glmnet)
library(gridExtra)


data_path <- "data.csv"


data <- read.csv(data_path)


data$Group <- as.factor(data$Group)


colnames(data)


selected_features <- setdiff(colnames(data), "Group")


set.seed(123)
trainIndex <- createDataPartition(data$Group, p = .7, 
                                  list = FALSE, 
                                  times = 1)
dataTrain <- data[trainIndex,]
dataTest <- data[-trainIndex,]


x_train <- model.matrix(Group ~ ., dataTrain)[,-1]
y_train <- dataTrain$Group


set.seed(123)
lasso_model <- cv.glmnet(x_train, y_train, alpha = 1, family = "binomial")


best_lambda <- lasso_model$lambda.min
print(paste("Best lambda: ", best_lambda))


file1 <- "Lasso_Path.png"
file2 <- "Lasso_Coefficient_Paths.png"
file3 <- "Lasso_Coefficient_Table.png"


png(file1, width = 8, height = 8, units = "in", res = 600)
plot(lasso_model, main = "", col.main = "blue")
title(main = "Lasso Path", adj = 0.5, line = 2.5)
dev.off()   


png(file2, width = 8, height = 8, units = "in", res = 600)
plot(lasso_model$glmnet.fit, xvar = "lambda", label = TRUE)
title(main = "Lasso Coefficient Paths", adj = 0.5, line = 2.5)
dev.off()


png(file3, width = 8, height = 8, units = "in", res = 600)
grid.table(as.matrix(coef(lasso_model, s = "lambda.min")))
dev.off()

cat("PNG output saved to:", file1, file2, file3, "\n")

