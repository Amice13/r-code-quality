## 01_generate_figures.R - Generates Figures 1-4 for the manuscript.
##

# 1. Setup and Data Loading
# ----------------------------------------------------
# Load necessary libraries
library(jsonlite)
library(data.table)
library(ROCR)
library(zoo)

# Read in the data from the correct relative path
d <- read.csv("data/raw/raw_gtd_multilabel_data.csv") 


# 2. Data Processing 
# ----------------------------------------------------

##### GTD Baseline ####
tmp <- as.factor(d$attacktype1_txt)
contrasts(tmp) = contr.treatment(9)
GTD <- model.matrix(~ tmp - 1)
types <- gsub("tmp", "", colnames(GTD))
types <- gsub(" ", ".", types)
types <- gsub("[()]", "", types)
types <- gsub("[/]", ".", types)
colnames(GTD) <- types

##### ConfliBERT ####
CB <- d[,138]
CB1 <- lapply(CB, fromJSON)
CB2 <- lapply(CB1, unlist, recursive=FALSE)
CB3 <- lapply(1:length(CB2), function(i) {
  if(length(CB2[[i]]) > 0) data.frame(t(unlist(CB2[i]))) else data.frame()
})
CB4 <- as.data.frame(rbindlist(CB3, fill=TRUE, use.names=TRUE))
colnames(CB4) <- sub("[.]$", "",
                      gsub("\\.\\.", ".", colnames(CB4)))

##### Llama ####
L <- d$llama3.1.latest_predictions
L1 <- lapply(L, fromJSON)
L2 <- lapply(L1, unlist, recursive=FALSE)
tmp_impute <- L2[36159]
L2[unlist(lapply(L2, is.null))==TRUE] <- tmp_impute
L3 <- lapply(1:length(L2), function(i) {
  if(length(L2[[i]]) > 0) data.frame(t(unlist(L2[i]))) else data.frame()
})
L4 <- as.data.frame(rbindlist(L3, fill=TRUE, use.names=TRUE))
Ltypes <- colnames(L4)
Ltypes <- sub("[.]$", "", Ltypes)
Ltypes <- gsub("\\.\\.", ".", Ltypes)
colnames(L4) <- Ltypes

##### ConflLlamaQ4 ####
CF4_col <- d$hf.co.shreyasmeher.ConflLlama.Q4_K_M_predictions
CF4a <- strsplit(as.character(CF4_col), "[|]")
CF4c <- lapply(CF4a, function(x) trimws(x[1]))
pred_factor_cf4 <- factor(unlist(CF4c))
if (nlevels(pred_factor_cf4) > 1) {
  CF4 <- model.matrix(~ pred_factor_cf4 - 1)
  types <- gsub("pred_factor_cf4", "", colnames(CF4))
  types <- gsub(" ", ".", types); types <- gsub("[()]", "", types); types <- gsub("[/]", ".", types)
  colnames(CF4) <- types
} else {
  CF4 <- data.frame(matrix(1, nrow = length(pred_factor_cf4), ncol = 1))
  colnames(CF4) <- gsub("[()/]", ".", gsub("[()]", "", gsub("\\s", ".", levels(pred_factor_cf4))))
}
CF4 <- as.data.frame(CF4)


##### ConflLlamaQ8 ####
CF8_col <- d$hf.co.shreyasmeher.ConflLlama.Q8_0_predictions
CF8a <- strsplit(as.character(CF8_col), "[|]")
CF8c <- lapply(CF8a, function(x) trimws(x[1]))
pred_factor_cf8 <- factor(unlist(CF8c))
if (nlevels(pred_factor_cf8) > 1) {
  CF8 <- model.matrix(~ pred_factor_cf8 - 1)
  types <- gsub("pred_factor_cf8", "", colnames(CF8))
  types <- gsub(" ", ".", types); types <- gsub("[()]", "", types); types <- gsub("[/]", ".", types)
  colnames(CF8) <- types
} else {
  CF8 <- data.frame(matrix(1, nrow = length(pred_factor_cf8), ncol = 1))
  colnames(CF8) <- gsub("[()/]", ".", gsub("[()]", "", gsub("\\s", ".", levels(pred_factor_cf8))))
}
CF8 <- as.data.frame(CF8)


##### Gemma ####
G <- d$gemma2.latest_predictions
G1 <- lapply(G, fromJSON)
G2 <- lapply(G1, unlist, recursive=FALSE)
G3 <- lapply(1:length(G2), function(i) {
  if(length(G2[[i]]) > 0) data.frame(t(unlist(G2[i]))) else data.frame()
})
G4 <- as.data.frame(rbindlist(G3, fill=TRUE, use.names=TRUE))
Gtypes <- colnames(G4)
Gtypes <- sub("[.]$", "", Gtypes)
Gtypes <- gsub("\\.\\.", ".", Gtypes)
colnames(G4) <- Gtypes

##### QWEN ####
Q <- d$qwen2.5.14b_predictions
Q1 <- lapply(Q, fromJSON)
Q2 <- lapply(Q1, unlist, recursive=FALSE)
Q2[unlist(lapply(Q2, is.null))==TRUE] <- tmp_impute
Q3 <- lapply(1:length(Q2), function(i) {
  if(length(Q2[[i]]) > 0) data.frame(t(unlist(Q2[i]))) else data.frame()
})
Q4 <- as.data.frame(rbindlist(Q3, fill=TRUE, use.names=TRUE))
Qtypes <- colnames(Q4)
Qtypes <- sub("[.]$", "", Qtypes)
Qtypes <- gsub("\\.\\.", ".", Qtypes)
colnames(Q4) <- Qtypes

##### Align and Clean Data Frames ####
GTD_df <- data.frame(id = d$eventid, GTD)

align_df <- function(pred_df, gtd_cols) {
  aligned <- as.data.frame(matrix(0, nrow=nrow(GTD_df), ncol=length(gtd_cols)))
  colnames(aligned) <- gtd_cols
  common_cols <- intersect(gtd_cols, colnames(pred_df))
  if(length(common_cols) > 0) {
    aligned[, common_cols] <- pred_df[, common_cols]
  }
  aligned[is.na(aligned)] <- 0
  aligned[aligned > 1] <- 1
  aligned[aligned < 0] <- 0
  return(aligned)
}
gtd_cols_only <- colnames(GTD_df)[-1]
CB.sorted <- align_df(CB4, gtd_cols_only)
L.sorted <- align_df(L4, gtd_cols_only)
G.sorted <- align_df(G4, gtd_cols_only)
Q.sorted <- align_df(Q4, gtd_cols_only)
CF4.sorted <- align_df(CF4, gtd_cols_only)
CF8.sorted <- align_df(CF8, gtd_cols_only)


# 3. Performance Object Creation 
# ----------------------------------------------------

##### ROCR Prediction & Performance Objects ####
CB.p <- prediction(predictions = CB.sorted, labels = GTD_df[,-1])
G.p <- prediction(predictions = G.sorted, labels = GTD_df[,-1])
L.p <- prediction(predictions = L.sorted, labels = GTD_df[,-1])
Q.p <- prediction(predictions = Q.sorted, labels = GTD_df[,-1])
CF4.p <- prediction(predictions = CF4.sorted, labels=GTD_df[,-1])
CF8.p <- prediction(predictions = CF8.sorted, labels=GTD_df[,-1])

# For Figure 1
CB.perf <- performance(CB.p, "tpr", "fpr")
G.perf <- performance(G.p, "tpr", "fpr")
L.perf <- performance(L.p, "tpr", "fpr")
Q.perf <- performance(Q.p, "tpr", "fpr")
CF4.perf <- performance(CF4.p, "tpr", "fpr")
CF8.perf <- performance(CF8.p, "tpr", "fpr")

# For Figure 2
CB.pr <- performance(CB.p, "prec", "rec")
G.pr <- performance(G.p, "prec", "rec")
L.pr <- performance(L.p, "prec", "rec")
Q.pr <- performance(Q.p,  "prec", "rec")
CF4.pr <- performance(CF4.p, "prec", "rec")
CF8.pr <- performance(CF8.p,  "prec", "rec")

# For Figure 3
for(i in colnames(GTD_df[,-1]))
{
  tmp <- prediction(predictions = cbind(CB.sorted[,i],
                                         CF4.sorted[,i],
                                         CF8.sorted[,i],
                                         G.sorted[,i],
                                         L.sorted[,i],
                                         Q.sorted[,i]),
                      labels = matrix(rep(GTD_df[,i],6), ncol=6))
  assign(paste(i, ".p", sep=""), tmp)
}
for (i in colnames(GTD_df[,-1]))
{
  tmp <- performance(get(paste(i, ".p", sep="")), "f")
  assign(paste(i, ".f", sep=""), tmp)
}

# AUC calculations for Figure 1 legends
CB.auc <- performance(CB.p, "auc"); G.auc <- performance(G.p, "auc"); L.auc <- performance(L.p, "auc");
Q.auc <- performance(Q.p, "auc"); CF4.auc <- performance(CF4.p, "auc"); CF8.auc <- performance(CF8.p, "auc");

aucs <- cbind(unlist(CB.auc@y.values), unlist(CF4.auc@y.values),
              unlist(CF8.auc@y.values), unlist(G.auc@y.values),
              unlist(L.auc@y.values), unlist(Q.auc@y.values))
colnames(aucs) <- c("ConfliBERT", "ConflLlama4", "ConflLlama8", "Gemma", "Llama", "Qwen")


# 4. Figure Generation
# ----------------------------------------------------

##### Common plotting resources from original script ####
v1 <- c("Armed Assault", "Assassination", "Bombing/Explosion", "Facility/Infrastructure Attack",
        "Hijacking", "Hostage Taking (Barricade Incident)", "Hostage Taking (Kidnapping)",
        "Unarmed Assault", "Unknown")

# MODIFIED plotting function for better legend spacing
plotf <- function(x, main = "", vnames, add45 = TRUE)
{
  m <- length(x@x.values)
  plot(c(0,1), c(0,1), type="n", main=main, las=1,
       xlab = x@x.name, ylab=x@y.name, xlim=c(0,1), ylim=c(0,1),
       asp=1.0)
  if(add45) segments(0,0,1,1, lty=3, col="grey")
  for(i in 1:m)
  { lines(x@x.values[[i]], x@y.values[[i]], col=i, lwd=2)}
  if(is.null(vnames)==FALSE) legend(
    "bottomright",
    vnames,
    col=1:m,
    lwd=2,
    cex = 0.6,      # Slightly reduced font size
    y.intersp = 0.8, # Reduced vertical spacing
    bty = "n"       # Removed the box
  )
}


add_legend <- function(...) {
  opar <- par(fig=c(0, 1, 0, 1), oma=c(0, 0, 0, 0),
              mar=c(0, 0, 0, 0), new=TRUE)
  on.exit(par(opar))
  plot(0, 0, type='n', bty='n', xaxt='n', yaxt='n')
  legend(...)
}

##### Figure 1 ####
cat("Generating figures/Figure_1.pdf\n")
# CHANGED: Added "figures/" to the path
pdf(file = "figures/Figure_1.pdf", onefile = TRUE, width = 7, height=7)
par(mfcol=c(3,2), pty="s", mgp=c(2,1,0), mar=c(3.2,3,1,0))
plotf(CB.perf, main="ConfliBERT", vnames = paste(v1, ", AUC = ", round(as.numeric(aucs[,1]),2), sep=""))
plotf(CF4.perf, main="ConflLlama4", vnames = paste(v1, ", AUC = ", round(as.numeric(aucs[,2]),2), sep=""))
plotf(CF8.perf, main="ConflLlama8", vnames = paste(v1, ", AUC = ", round(as.numeric(aucs[,3]),2), sep=""))
plotf(G.perf, main="Gemma", vnames = paste(v1, ", AUC = ", round(as.numeric(aucs[,4]),2), sep=""))
plotf(L.perf, main="Llama", vnames = paste(v1, ", AUC = ", round(as.numeric(aucs[,5]),2), sep=""))
plotf(Q.perf, main="Qwen", vnames = paste(v1, ", AUC = ", round(as.numeric(aucs[,6]),2), sep=""))
dev.off()

##### Figure 2 ####
cat("Generating figures/Figure_2.pdf\n")
# CHANGED: Added "figures/" to the path
pdf(file = "figures/Figure_2.pdf", onefile = TRUE, width = 7, height=8)
par(mfcol=c(3,3), pty="s", mgp=c(2.1,1,0), mar=c(2,3,1,0))
plotf(CB.pr, main = "ConfliBERT", vnames=NULL)
plotf(CF4.pr, main = "ConflLlama4", vnames=NULL)
plotf(CF8.pr, main = "ConflLlama8", vnames=NULL)
plotf(G.pr, main="Gemma", vnames=NULL)
plotf(L.pr, main="Llama", vnames=NULL)
plotf(Q.pr, main="Qwen", vnames=NULL)
plot.new()
plot.new()
legend("center", v1, col=1:length(v1), lwd=2)
dev.off()

##### Figure 3 ####
cat("Generating figures/Figure_3.pdf\n")
# CHANGED: Added "figures/" to the path
pdf(file = "figures/Figure_3.pdf", onefile = TRUE, width = 7, height=7)
par(mfrow = c(3,3), oma=c(4,0,2,0), xpd=TRUE, pty="s",
    mgp=c(2.1,1,0), mar=c(3,4,1,1))
for(i in 1:9)
{
  plotf(get(paste(colnames(GTD_df[,-1])[i], ".f", sep="")),
        main = v1[i], vnames=NULL)
}
add_legend("bottom", legend=colnames(aucs), col=1:6, horiz = TRUE,
           cex=0.95, lwd=2)
dev.off()

##### Figure 4 ####
cat("Generating figures/Figure_4.pdf\n")
dt <- ISOdate(d$iyear, d$imonth, d$iday)
start <- min(as.Date(dt)); end <- max(as.Date(dt))
times <- seq(start, end, by="day")
full.sample <- zoo(, times)

GTD.ts <- cbind(dt, GTD_df[,-1])
tmp <- aggregate(GTD.ts[,-1], by=list(as.factor(GTD.ts[,1])), sum)
GTD.ts <- merge(full.sample, zoo(tmp[,-1], as.Date(tmp[,1])), fill=0)
GTD.cts <- zoo(apply(coredata(GTD.ts), 2, cumsum), index(GTD.ts))

CB.ts <- as.data.frame(cbind(dt, CB.sorted))
tmp <- aggregate(CB.ts[,-1], by=list(as.factor(CB.ts[,1])), sum)
CB.ts <- merge(full.sample, zoo(tmp[,-1], as.Date(tmp[,1])), fill=0)
CB.cts <- zoo(apply(coredata(CB.ts), 2, cumsum), index(CB.ts))

G.ts <- as.data.frame(cbind(dt, G.sorted))
tmp <- aggregate(G.ts[,-1], by=list(as.factor(G.ts[,1])), sum)
G.ts <- merge(full.sample, zoo(tmp[,-1], as.Date(tmp[,1])), fill=0)
G.cts <- zoo(apply(coredata(G.ts), 2, cumsum), index(G.ts))

L.ts <- as.data.frame(cbind(dt, L.sorted))
tmp <- aggregate(L.ts[,-1], by=list(as.factor(L.ts[,1])), sum)
L.ts <- merge(full.sample, zoo(tmp[,-1], as.Date(tmp[,1])), fill=0)
L.cts <- zoo(apply(coredata(L.ts), 2, cumsum), index(L.ts))

Q.ts <- as.data.frame(cbind(dt,Q.sorted))
tmp <- aggregate(Q.ts[,-1], by=list(as.factor(Q.ts[,1])), sum)
Q.ts <- merge(full.sample, zoo(tmp[,-1], as.Date(tmp[,1])), fill=0)
Q.cts <- zoo(apply(coredata(Q.ts), 2, cumsum), index(Q.ts))

CF4.ts <- as.data.frame(cbind(dt,CF4.sorted))
tmp <- aggregate(CF4.ts[,-1], by=list(as.factor(CF4.ts[,1])), sum)
CF4.ts <- merge(full.sample, zoo(tmp[,-1], as.Date(tmp[,1])), fill=0)
CF4.cts <- zoo(apply(coredata(CF4.ts), 2, cumsum), index(CF4.ts))

CF8.ts <- as.data.frame(cbind(dt,CF8.sorted))
tmp <- aggregate(CF8.ts[,-1], by=list(as.factor(CF8.ts[,1])), sum)
CF8.ts <- merge(full.sample, zoo(tmp[,-1], as.Date(tmp[,1])), fill=0)
CF8.cts <- zoo(apply(coredata(CF8.ts), 2, cumsum), index(CF8.ts))

pdf(file = "figures/Figure_4.pdf", onefile = TRUE, width = 6, height=6)
par(mfrow = c(3,3), oma=c(4,0,2,0), xpd=TRUE, pty="s",
    mgp=c(2.1,1,0), mar=c(3,4,1,1))
for(i in 1:9)
{
  tmp <- cbind(GTD.cts[,i],CB.cts[,i],CF4.cts[,i],CF8.cts[,i],
               G.cts[,i],L.cts[,i],Q.cts[,i] )
  plot(tmp, plot.type="s", main = v1[i], col=c(1,1:6),
       lty=c(2,rep(1,6)), lwd=2, ylab="", xlab="")
}
add_legend("bottom", legend=c("GTD", colnames(aucs)),
           col=c(1,1:6), lty=c(2,rep(1,6)),
           horiz = TRUE,
           cex=0.9, lwd=2)
dev.off()

cat("\nAll figures generated successfully.\n")