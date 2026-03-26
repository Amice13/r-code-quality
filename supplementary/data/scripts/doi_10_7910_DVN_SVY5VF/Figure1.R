## Author: Kabir Khanna
## Updated: December 10, 2015
## Note: First run Predict.R

library(ROCR)

load("df.pred1.RData")
load("df.pred2.RData")
load("df.pred4.RData")

eth <- c("whi", "bla", "his", "asi", "oth")
SR <- c("SR.WHI", "SR.BLA", "SR.HIS", "SR.ASI", "SR.OTH")

AUC <- as.data.frame(matrix(NA, nrow = 3, ncol = 3))
names(AUC) <- eth[1:3]

roc.temp <- as.data.frame(matrix(NA, nrow = 11, ncol = 2))
names(roc.temp) <- c("fpr", "tpr")
cut <- seq(0, 1, length = 11)

pred <- c(1, 2, 4)
for (j in 1:3) {
  print(paste("pred", pred[j], sep = ""))
  for (k in 1:3) {
    print(eth[k])
    df.temp <- get(paste("df.pred", pred[j], sep = ""))
    df.temp <- df.temp[!is.na(df.temp[paste("pred", eth[k], sep = ".")]), ]
    preds <- df.temp[, paste("pred", eth[k], sep = ".")]
    truth <- df.temp[, SR[k]]
    
    ## Calculate False Positive Rate (fpr) and True Positive Rate (tpr) for ROC curve
    for (l in 1:10) {
      print(paste("Cutpoint ", l, ": ", cut[l], sep = ""))
      table.temp <- table(truth, preds >= cut[l])
      roc.temp$fpr[l] <- table.temp[rownames(table.temp) == "0", colnames(table.temp) == "TRUE"] / table(truth)["0"]
      roc.temp$tpr[l] <- table.temp[rownames(table.temp) == "1", colnames(table.temp) == "TRUE"] / table(truth)["1"]
      roc.temp$tpr <- ifelse(is.na(roc.temp$tpr), 1, roc.temp$tpr)
    }
    print("Cutpoint 11: 1")
    table.temp <- table(truth, preds >= 1)
    roc.temp$fpr[11] <- 1 - table.temp[rownames(table.temp) == "0", colnames(table.temp) == "FALSE"] / table(truth)["0"]
    roc.temp$tpr[11] <- 1 - table.temp[rownames(table.temp) == "1", colnames(table.temp) == "FALSE"] / table(truth)["1"]
    assign(paste("roc.", eth[k], ".pred", pred[j], sep = ""), roc.temp)
    
    ## Calculate Area under Curve using ROCR package functions
    AUC[j, k] <- attributes(performance(prediction(preds, truth), "auc"))$y.values[[1]]
  }
}

## Format AUC for Plot Area
AUC.form <- AUC
for (i in 1:3) {
  for (j in 1:3) {
    AUC.form[i, j] <- gsub("0.", ".", sprintf("%.2f", AUC[i, j]))
  }
}

## Figure 1
pdf(file = "figure1.pdf", width = 11, height = 4) ## Export to PDF (12" by 4")

par(mfrow=c(1, 3), mar=c(5.1, 4.1, 3.1, .6))
pred.leg <- c("Name, Precinct, Party", "Name & Precinct", "Name Only")

## White Voters
plot(NA, NA, xlim = c(0, 1), ylim = c(0, 1), cex.axis = 1.1, pch = 20,
     xlab = "False Positive Rate", ylab = "True Positive Rate", cex.lab = 1.1,
     main = "White Voters", cex.main = 1.1)
axis(1, at = seq(0, 1, .1), lab = NA)
axis(2, at = seq(0, 1, .1), lab = NA)
legend(.25, .5, legend = paste(pred.leg, " (", AUC.form$whi[4 - order(AUC.form$whi)], ")", sep = ""), lty = c(1, 4, 3), cex = 1.1, 
       y.intersp = 1.5, text.width = .5, bty = "n")
lines(roc.whi.pred4$fpr, roc.whi.pred4$tpr, lty = 1, col = "black")
lines(roc.whi.pred2$fpr, roc.whi.pred2$tpr, lty = 4, col = "black")
lines(roc.whi.pred1$fpr, roc.whi.pred1$tpr, lty = 3, col = "black")

## Black Voters
plot(NA, NA, xlim = c(0, 1), ylim = c(0, 1), cex.axis = 1.1, pch = 20,
     xlab = "False Positive Rate", ylab = "", cex.lab = 1.1,
     main = "Black Voters", cex.main = 1.1)
axis(1, at = seq(0, 1, .1), lab = NA)
axis(2, at = seq(0, 1, .1), lab = NA)
legend(.25, .5, legend = paste(pred.leg, " (", AUC.form$bla[4 - order(AUC.form$bla)], ")", sep = ""), lty = c(1, 4, 3), cex = 1.1, 
       y.intersp = 1.5, text.width = .5, bty = "n")
lines(roc.bla.pred4$fpr, roc.bla.pred4$tpr, lty = 1, col = "black")
lines(roc.bla.pred2$fpr, roc.bla.pred2$tpr, lty = 4, col = "black")
lines(roc.bla.pred1$fpr, roc.bla.pred1$tpr, lty = 3, col = "black")

## Latino Voters
plot(NA, NA, xlim = c(0, 1), ylim = c(0, 1), cex.axis = 1.1, pch = 20,
     xlab = "False Positive Rate", ylab = "", cex.lab = 1.1,
     main = "Latino Voters", cex.main = 1.1)
axis(1, at = seq(0, 1, .1), lab = NA)
axis(2, at = seq(0, 1, .1), lab = NA)
legend(.25, .5, legend = paste(pred.leg, " (", AUC.form$his[4 - order(AUC.form$his)], ")", sep = ""), lty = c(1, 4, 3), cex = 1.1, 
       y.intersp = 1.5, text.width = .5, bty = "n")
lines(roc.his.pred4$fpr, roc.his.pred4$tpr, lty = 1, col = "black")
lines(roc.his.pred2$fpr, roc.his.pred2$tpr, lty = 4, col = "black")
lines(roc.his.pred1$fpr, roc.his.pred1$tpr, lty = 3, col = "black")

dev.off()
