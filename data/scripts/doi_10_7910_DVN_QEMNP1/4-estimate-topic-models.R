######################################################
# Policy Diffusion: The Issue-Definition Stage
# Fabrizio Gilardi, Charles R. Shipan & Bruno Wueest
# Estimate topic models
# 2019-11-18
######################################################

rm(list = ls())
library(tm)
library(stm)
library(parallel)
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
# [1] parallel  stats     graphics  grDevices utils     datasets  methods   base     

# other attached packages:
# [1] stm_1.3.4 tm_0.7-6  NLP_0.2-0

# loaded via a namespace (and not attached):
# [1] compiler_3.6.1    Matrix_1.2-17     Rcpp_1.0.3        slam_0.1-46       xml2_1.2.2       
# [6] grid_3.6.1        data.table_1.12.6 lattice_0.20-38  


# Load corpus
load(file = "./corpus/corpus_txt.RData") 

n_words <- 100 # number of words
k <- 12 # number of topics

# -----------------------------------------------------------------------------
# Estimate topic models (Spatial Lag: Network)
# -----------------------------------------------------------------------------

# define formula for model estimation
formula <- c("prevalence =~ s(month) + as.factor(paper) + sentiment + smokers + unifiedDemocrats + unifiedRepublicans + shareDemLow + producer + slant + Policy_Enacted_This_Month + Policy_Months_Before_After_Enacted + Policy_Spatial_Lag_Network_Pcent")

# estimate model
STM <- stm(corpus$documents, corpus$vocab, K = k, as.formula(formula), data = corpus$meta, init.type = "Spectral", verbose = TRUE, control = list(rp.s = 0.05, rp.p = 2000, rp.d.group.size = 2000))

#save model
save(STM, file = "./output/STM_model_network.RData")

# -----------------------------------------------------------------------------
# Estimate topic models (Spatial Lag: Neighbors)
# -----------------------------------------------------------------------------

# define formula for model estimation
formula <- c("prevalence =~ s(month) + as.factor(paper) + sentiment + smokers + unifiedDemocrats + unifiedRepublicans + shareDemLow + producer + slant + Policy_Enacted_This_Month + Policy_Months_Before_After_Enacted + Policy_Spatial_Lag_Neighbors")

# estimate model
STM <- stm(corpus$documents, corpus$vocab, K = k, as.formula(formula), data = corpus$meta, init.type = "Spectral", verbose = TRUE, control = list(rp.s = 0.05, rp.p = 2000, rp.d.group.size = 2000))

#save model
save(STM, file = "./output/STM_model_neighbors.RData")

# -----------------------------------------------------------------------------
# Estimate topic models (Spatial Lag: All)
# -----------------------------------------------------------------------------

# define formula for model estimation
formula <- c("prevalence =~ s(month) + as.factor(paper) + sentiment + smokers + unifiedDemocrats + unifiedRepublicans + shareDemLow + producer + slant + Policy_Enacted_This_Month + Policy_Months_Before_After_Enacted + Policy_Spatial_Lag_All")

# estimate model
STM <- stm(corpus$documents, corpus$vocab, K = k, as.formula(formula), data = corpus$meta,
           init.type = "Spectral", verbose = TRUE, control = list(rp.s = 0.05, rp.p = 2000,
           rp.d.group.size = 2000))

#save model
save(STM, file = "./output/STM_model_all.RData")

