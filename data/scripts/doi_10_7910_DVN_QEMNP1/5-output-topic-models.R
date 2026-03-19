#########################################################
# Policy Diffusion: The Issue-Definition Stage
# Fabrizio Gilardi, Charles R. Shipan & Bruno Wueest
# Generate output of topic models for tables and figures
# 2019-11-18
#########################################################

#basic settings
rm(list = ls())
library(stm)
library(matrixStats)
options(stringsAsFactors = FALSE)
suppressMessages(library(stm))
set.seed(0213)

lang <- "english"

setwd("/Users/fgilardi/Downloads/AJPS-final/")


# sessionInfo()
# R version 3.6.1 (2019-07-05)
# Platform: x86_64-apple-darwin15.6.0 (64-bit)
# Running under: macOS Catalina 10.15.1

# Matrix products: default
# BLAS:   /Library/Frameworks/R.framework/Versions/3.6/Resources/lib/libRblas.0.dylib
# LAPACK: /Library/Frameworks/R.framework/Versions/3.6/Resources/lib/libRlapack.dylib

# locale:
# [1] en_US.UTF-8/en_US.UTF-8/en_US.UTF-8/C/en_US.UTF-8/en_US.UTF-8

# attached base packages:
# [1] stats     graphics  grDevices utils     datasets  methods   base     

# other attached packages:
# [1] matrixStats_0.55.0 stm_1.3.4         

# loaded via a namespace (and not attached):
# [1] compiler_3.6.1    Matrix_1.2-17     Rcpp_1.0.3        grid_3.6.1        data.table_1.12.6
# [6] lattice_0.20-38


# STM functions

# extract information from estimates
js.estimate <- function(prob, ct) {
  if(ct <= 1) {
  	# if we only observe a count of 1, the variance goes to infinity and we get the uniform distribution.
    return(rep(1/length(prob), length(prob)))
  }
  
  # MLE of probability  estimate
  mlvar <- prob*(1-prob)/(ct-1)
  unif <- rep(1/length(prob), length(prob)) 
  
  # Deviation from uniform distribution
  deviation <- sum((prob-unif)^2)
  
  # Take care of special case,if no difference it doesn't matter
  if(deviation == 0) return(prob)
  
  lambda <- sum(mlvar)/deviation
  # if despite  our best efforts we end up with an NaN number, just return the uniform distribution.
  if(is.nan(lambda)) return(unif)
  
  # truncate
  if(lambda>1) lambda <- 1
  if(lambda<0) lambda <- 0
  
  # Construct shrinkage estimator as convex combination of the two
  lambda*unif + (1 - lambda)*prob
}

# from the stm source code
col.lse <- function(mat) {
  colLogSumExps(mat)
}

# from the stm source code
safelog <- function(x) {
  out <- log(x)
  out[which(out< -1000)] <- -1000
  out
}

k <- 12

# -----------------------------------------------------------------------------
# Spatial Lag: Network
# -----------------------------------------------------------------------------

# Load model and corpus

load(file = "./output/STM_model_network.RData")
load(file = "./corpus/corpus_txt.RData") # Load corpus

# Print 5 most relevant documents per topic

thoughts <- findThoughts(STM, texts = corpus$meta$text, n = 5, topics = 1:k)
thoughtsDocs <- data.frame(matrix(unlist(thoughts$docs), nrow=k, byrow=T))
thoughtsIndices <- data.frame(matrix(unlist(thoughts$index), nrow=k, byrow=T))
thoughtsMeta <- thoughtsIndices
for (v in 1:k){
  thoughtsMeta[v,] <- corpus$meta$path[as.numeric(thoughtsIndices[v,])]
}
thoughts <- cbind(thoughtsDocs, thoughtsIndices, thoughtsMeta)
write.csv(thoughts, paste0("./output/thoughts_network.csv"), fileEncoding = "utf-8")

# Estimate effects

prep <- estimateEffect(1:k ~ s(month), STM, meta = corpus$meta)
x <- plot(prep, covariate = "month")
dfs <- data.frame()
for (i in 1:length(x$means)){
  df <- data.frame(x$means[[i]], x$cis[[i]][1,], x$cis[[i]][2,])
  df <- cbind(df, i, x$uvals)
  dfs <- rbind(dfs, df)
}
colnames(dfs) <- c("pointEstimate", "lb", "ub", "topic", "month")
outname <- "./output/month_network.txt"
write.table(dfs, outname, col.names = TRUE, row.names = FALSE, sep = "\t", quote = FALSE)

prep <- estimateEffect(1:k ~ smokers, STM, meta = corpus$meta)
x <- plot(prep, covariate = "smokers")
dfs <- data.frame()
for (i in 1:length(x$means)){
  df <- data.frame(x$means[[i]], x$cis[[i]][1,], x$cis[[i]][2,])
  df <- cbind(df, i, x$uvals)
  dfs <- rbind(dfs, df)
}
colnames(dfs) <- c("pointEstimate", "lb", "ub", "topic", "smokers")
outname <- "./output/smokers_network.txt"
write.table(dfs, outname, col.names = TRUE, row.names = FALSE, sep = "\t", quote = FALSE)

prep <- estimateEffect(1:k ~ unifiedDemocrats, STM, meta = corpus$meta)
x <- plot(prep, covariate = "unifiedDemocrats")
dfs <- data.frame()
for (i in 1:length(x$means)){
  df <- data.frame(x$means[[i]], x$cis[[i]][1,], x$cis[[i]][2,])
  df <- cbind(df, i, x$uvals)
  dfs <- rbind(dfs, df)
}
colnames(dfs) <- c("pointEstimate", "lb", "ub", "topic", "unifiedDemocrats")
outname <- paste0("./output/unifiedDemocrats_network.txt")
write.table(dfs, outname, col.names = TRUE, row.names = FALSE, sep = "\t", quote = FALSE)

prep <- estimateEffect(1:k ~ unifiedRepublicans, STM, meta = corpus$meta)
x <- plot(prep, covariate = "unifiedRepublicans")
dfs <- data.frame()
for (i in 1:length(x$means)){
  df <- data.frame(x$means[[i]], x$cis[[i]][1,], x$cis[[i]][2,])
  df <- cbind(df, i, x$uvals)
  dfs <- rbind(dfs, df)
}
colnames(dfs) <- c("pointEstimate", "lb", "ub", "topic", "unifiedRepublicans")
outname <- paste0("./output/unifiedRepublicans_network.txt")
write.table(dfs, outname, col.names = TRUE, row.names = FALSE, sep = "\t", quote = FALSE)

prep <- estimateEffect(1:k ~ shareDemLow, STM, meta = corpus$meta)
x <- plot(prep, covariate = "shareDemLow")
dfs <- data.frame()
for (i in 1:length(x$means)){
  df <- data.frame(x$means[[i]], x$cis[[i]][1,], x$cis[[i]][2,])
  df <- cbind(df, i, x$uvals)
  dfs <- rbind(dfs, df)
}
colnames(dfs) <- c("pointEstimate", "lb", "ub", "topic", "shareDemLow")
outname <- paste0("./output/shareDemLow_network.txt")
write.table(dfs, outname, col.names = TRUE, row.names = FALSE, sep = "\t", quote = FALSE)

prep <- estimateEffect(1:k ~ s(Policy_Months_Before_After_Enacted), STM, meta = corpus$meta)
x <- plot(prep, covariate = "Policy_Months_Before_After_Enacted")
dfs <- data.frame()
for (i in 1:length(x$means)){
  df <- data.frame(x$means[[i]], x$cis[[i]][1,], x$cis[[i]][2,])
  df <- cbind(df, i, x$uvals)
  dfs <- rbind(dfs, df)
}
colnames(dfs) <- c("pointEstimate", "lb", "ub", "topic", "Policy_Months_Before_After_Enacted")
outname <- paste0("./output/policyMonthsBeforeAfterEnacted_network.txt")
write.table(dfs, outname, col.names = TRUE, row.names = FALSE, sep = "\t", quote = FALSE)

prep <- estimateEffect(1:k ~ Policy_Spatial_Lag_Network_Pcent, STM, meta = corpus$meta)
x <- plot(prep, covariate = "Policy_Spatial_Lag_Network_Pcent")
dfs <- data.frame()
for (i in 1:length(x$means)){
  df <- data.frame(x$means[[i]], x$cis[[i]][1,], x$cis[[i]][2,])
  df <- cbind(df, i, x$uvals)
  dfs <- rbind(dfs, df)
}
colnames(dfs) <- c("pointEstimate", "lb", "ub", "topic", "Policy_Spatial_Lag_Network_Pcent")
outname <- paste0("./output/policySpatialLag_network.txt")
write.table(dfs, outname, col.names = TRUE, row.names = FALSE, sep = "\t", quote = FALSE)

prep <- estimateEffect(1:k ~ Policy_Enacted_This_Month, STM, meta = corpus$meta)
x <- plot(prep, covariate = "Policy_Enacted_This_Month", method = "difference", cov.value1=1, cov.value2=0)
dfs <- data.frame()
for (i in 1:length(x$means)){
  df <- data.frame(x$means[[i]], x$cis[[i]][1], x$cis[[i]][2])
  df <- cbind(df, i, x$labels[[i]])
  dfs <- rbind(dfs, df)
}
colnames(dfs) <- c("diff01", "lb", "ub", "topic", "description")
outname <- paste0("./output/Policy_Enacted_This_Month_difference_network.txt")
write.table(dfs, outname, col.names = TRUE, row.names = FALSE, sep = "\t", quote = FALSE)

prep <- estimateEffect(1:k ~ sentiment, STM, meta = corpus$meta)
x <- plot(prep, covariate = "sentiment", method = "difference", cov.value1=1, cov.value2=0)
dfs <- data.frame()
for (i in 1:length(x$means)){
  df <- data.frame(x$means[[i]], x$cis[[i]][1], x$cis[[i]][2])
  df <- cbind(df, i, x$labels[[i]])
  dfs <- rbind(dfs, df)
}
colnames(dfs) <- c("diff01", "lb", "ub", "topic", "description")
outname <- paste0("./output/sentiment_difference_network.txt")
write.table(dfs, outname, col.names = TRUE, row.names = FALSE, sep = "\t", quote = FALSE)

words <- labelTopics(STM, n = 100)
words <- as.data.frame(t(words[[1]]))
outname <- paste0("./output/words_network.txt")
colnames(words) <- c(1:k)
write.table(words, outname, col.names = TRUE, row.names = FALSE, sep = "\t", quote = FALSE)

# -----------------------------------------------------------------------------
# Write out word probabilities, exclusivities and frequencies
# -----------------------------------------------------------------------------

logbeta <- STM$beta$logbeta[[1]]
topics <- 1:nrow(logbeta) #A vector of numbers indicating the topics to include
K <- STM$settings$dim$K

wordcounts <- STM$settings$dim$wcounts$x
excl <- t(t(logbeta) - col.lse(logbeta))
excl <- safelog(sapply(1:ncol(excl), function(x) js.estimate(exp(excl[,x]), wordcounts[x])))

freqscore <- apply(logbeta,1,rank)/ncol(logbeta)
freqscore <- as.data.frame(freqscore)
colnames(freqscore) <- paste("frequency_", 1:K, sep = "")

exclscore <- apply(excl,1,rank)/ncol(logbeta)
exclscore <- as.data.frame(exclscore)
colnames(exclscore) <- paste("exclusivity_", 1:K, sep = "")

beta <- lapply(STM$beta$logbeta, exp)
probscore <- as.data.frame(t(beta[[1]]))
colnames(probscore) <- paste("probability_", 1:K, sep = "")

scores <- cbind(probscore, exclscore, freqscore)
scores$vocab <- STM$vocab

outname <- c("./output/scores_network.txt")
write.table(scores, outname, col.names = T, row.names = F, sep = "\t", quote = F)

# write out topic proportions
out <- cbind(STM$theta, corpus$meta$Media_Source, corpus$meta$trend)
thetaOut <- c("./output/theta_network.txt")
write.table(out, thetaOut, quote = F, row.names = F, sep = "\t")


# -----------------------------------------------------------------------------
# Estimate effects (Spatial Lag: Neighbors)
# -----------------------------------------------------------------------------

# Load model and corpus

load(file = "./output/STM_model_neighbors.RData")
load(file = "./corpus/corpus_txt.RData") # Load corpus


# Estimate effects

prep <- estimateEffect(1:k ~ Policy_Spatial_Lag_Neighbors, STM, meta = corpus$meta)
x <- plot(prep, covariate = "Policy_Spatial_Lag_Neighbors")
dfs <- data.frame()
for (i in 1:length(x$means)){
  df <- data.frame(x$means[[i]], x$cis[[i]][1,], x$cis[[i]][2,])
  df <- cbind(df, i, x$uvals)
  dfs <- rbind(dfs, df)
}
colnames(dfs) <- c("pointEstimate", "lb", "ub", "topic", "Policy_Spatial_Lag_Neighbors")
outname <- paste0("./output/policySpatialLag_all.txtatialLag_neighbors.txt")
write.table(dfs, outname, col.names = TRUE, row.names = FALSE, sep = "\t", quote = FALSE)



# -----------------------------------------------------------------------------
# Estimate effects (Spatial Lag: All)
# -----------------------------------------------------------------------------

# Load model and corpus

load(file = "./output/STM_model_all.RData")
load(file = "./corpus/corpus_txt.RData") # Load corpus

# Estimate effects

prep <- estimateEffect(1:k ~ Policy_Spatial_Lag_All, STM, meta = corpus$meta)
x <- plot(prep, covariate = "Policy_Spatial_Lag_All")
dfs <- data.frame()
for (i in 1:length(x$means)){
  df <- data.frame(x$means[[i]], x$cis[[i]][1,], x$cis[[i]][2,])
  df <- cbind(df, i, x$uvals)
  dfs <- rbind(dfs, df)
}
colnames(dfs) <- c("pointEstimate", "lb", "ub", "topic", "Policy_Spatial_Lag_All")
outname <- paste0("./output/policySpatialLag_all.txt")
write.table(dfs, outname, col.names = TRUE, row.names = FALSE, sep = "\t", quote = FALSE)


