library(readr)
library(caret)
library(rms)
library(pROC)
library(ROSE)
library(dcurves)

data <- read_csv("data.csv")


names(data)


names(data) <- make.names(names(data))



set.seed(123)  
trainIndex <- createDataPartition(data$Group, p = 0.7, list = FALSE, times = 1)
trainData <- data[trainIndex, ]
testData <- data[-trainIndex, ]


table(trainData$Group)
table(testData$Group)


trainData_balanced <- ROSE(Group ~ ., data = trainData, seed = 123)$data


table(trainData_balanced$Group)


fit <- glm(Group ~ LVEF + CKMB + hscTNI + Csub + WBC + ApoBA + NLR, data = trainData_balanced, family = binomial(), x = TRUE, y = TRUE)

summary_fit <- summary(fit)


coefficients <- summary_fit$coefficients
odds_ratios <- exp(coefficients[, "Estimate"])
conf_int <- exp(confint(fit))


results <- data.frame(
  Beta = round(coefficients[, "Estimate"], 3),
  SE = round(coefficients[, "Std. Error"], 3),
  Wald = round(coefficients[, "z value"], 3),
  P = round(coefficients[, "Pr(>|z|)"], 3),
  OR = round(odds_ratios, 3),
  `95% CL Lower` = round(conf_int[, 1], 3),
  `95% CL Upper` = round(conf_int[, 2], 3)
)

print(results)


dd <- datadist(trainData_balanced)
options(datadist = 'dd')
fit_lrm <- lrm(Group ~ LVEF + CKMB + Csub + WBC + ApoBA + NLR, data = trainData_balanced, x = TRUE, y = TRUE)


nomogram <- nomogram(fit_lrm, fun = plogis, fun.at = c(0.1, 0.5, 0.9), lp = FALSE,
                     LVEF = seq(20, 100, by = 5),
                     CKMB = seq(0, 600, by = 50),
                     Csub = seq(0, 110, by = 10),
                     WBC = seq(0, 20, by = 2),
                     ApoBA = seq(0, 2, by = 0.2),
                     NLR = seq(0, 11, by = 1))

testData$predicted <- predict(fit, newdata = testData, type = "response")


roc_curve <- roc(testData$Group, testData$predicted)
ci <- ci.auc(roc_curve) 


testData$predicted_CKMB <- testData$CKMB
testData$predicted_Csub <- testData$Csub


roc_curve_CKMB <- roc(testData$Group, testData$predicted_CKMB, ci=TRUE)
roc_curve_Csub <- roc(testData$Group, testData$predicted_Csub, ci=TRUE)


roc_curve <- roc(testData$Group, testData$predicted, ci=TRUE)


ci_model <- ci.auc(roc_curve)
ci_CKMB <- ci.auc(roc_curve_CKMB)
ci_Csub <- ci.auc(roc_curve_Csub)


par(pty = "s")  
plot(roc_curve, col="lightcoral", main="Combined ROC Curves", lwd=2, legacy.axes = TRUE)

plot(roc_curve_CKMB, col="deepskyblue", add=TRUE, lwd=2)
plot(roc_curve_Csub, col="mediumseagreen", add=TRUE, lwd=2)


auc_model <- round(auc(roc_curve), 3)
auc_CKMB <- round(auc(roc_curve_CKMB), 3)
auc_Csub <- round(auc(roc_curve_Csub), 3)

ci_model_str <- paste0(round(ci_model[1], 3), " - ", round(ci_model[3], 3))
ci_CKMB_str <- paste0(round(ci_CKMB[1], 3), " - ", round(ci_CKMB[3], 3))
ci_Csub_str <- paste0(round(ci_Csub[1], 3), " - ", round(ci_Csub[3], 3))


library(grid)


png(filename = "Combined_ROC_Curves.png", width = 10, height = 10, units = "in", res = 600)
par(pty = "s")  
plot(roc_curve, col="lightcoral", main="Combined ROC Curves", lwd=2, legacy.axes = TRUE)
plot(roc_curve_CKMB, col="deepskyblue", add=TRUE, lwd=2)
plot(roc_curve_Csub, col="mediumseagreen", add=TRUE, lwd=2)

# 手动绘制图例的线条和文本
grid.lines(x = unit(c(0.52, 0.54), "npc"), y = unit(c(0.18, 0.18), "npc"), gp = gpar(col = "lightcoral", lwd = 2))
grid.lines(x = unit(c(0.52, 0.54), "npc"), y = unit(c(0.15, 0.15), "npc"), gp = gpar(col = "deepskyblue", lwd = 2))
grid.lines(x = unit(c(0.52, 0.54), "npc"), y = unit(c(0.12, 0.12), "npc"), gp = gpar(col = "mediumseagreen", lwd = 2))

grid.text(sprintf("All:   AUC = %.3f (95%% CI: %s)", auc_model, ci_model_str),
          x = unit(0.55, "npc"), y = unit(0.18, "npc"), just = "left", gp = gpar(col = "lightcoral", fontsize = 14))
grid.text(sprintf("CK-MB:  AUC = %.3f (95%% CI: %s)", auc_CKMB, ci_CKMB_str),
          x = unit(0.55, "npc"), y = unit(0.15, "npc"), just = "left", gp = gpar(col = "deepskyblue", fontsize = 14))
grid.text(sprintf("Csub:  AUC = %.3f (95%% CI: %s)", auc_Csub, ci_Csub_str),
          x = unit(0.55, "npc"), y = unit(0.12, "npc"), just = "left", gp = gpar(col = "mediumseagreen", fontsize = 14))

dev.off()  


