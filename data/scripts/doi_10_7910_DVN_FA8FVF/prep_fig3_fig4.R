########################################################################################################################
# This Code Generates the Data for Figures 3 and 4 in the Main Paper
########################################################################################################################

print("Running prep_fig3_fig4.R...")

########################################################################################################################
# Packages and Setup
########################################################################################################################
# Packages
  library(ggplot2)
  library(ggalt)
  library(gridExtra)

# Set seed
  set.seed(02138)


########################################################################################################################
# Load Raw Data from MTurk
########################################################################################################################

  resp1 = read.csv("../data/rankings_resp1.csv")[,-1]
  colnames(resp1) = c("district", "rank_1")
  resp1b = read.csv("../data/rankings_resp1b.csv")[,-1]
  colnames(resp1b) = c( "district", "rank_2")

  resp2 = read.csv("../data/rankings_resp2.csv")
  colnames(resp2) = c("rank_1", "district")
  resp2b = read.csv("../data/rankings_resp2b.csv")
  colnames(resp2b) = c( "rank_2", "district")

  resp3 = read.csv("../data/rankings_resp3.csv")[,-1]
  colnames(resp3) = c("district","rank_1")
  resp3b = read.csv("../data/rankings_resp3b.csv")[,-1]
  colnames(resp3b) = c("district", "rank_2")

  resp4 = read.csv("../data/rankings_resp4.csv")[,-1]
  colnames(resp4) = c("district","rank_1")
  resp4b = read.csv("../data/rankings_resp4b.csv")[,-1]
  colnames(resp4b) = c("district", "rank_2")

  resp5 = read.csv("../data/rankings_resp5.csv", sep="\t")[,-1]
  colnames(resp5) = c("district","rank_1")
  resp5b = read.csv("../data/rankings_resp5b.csv", header=F, sep="\t")
  colnames(resp5b) = c("rank_2", "district")

  resp6 = read.csv("../data/rankings_resp6.csv")[,-1]
  colnames(resp6) = c("district","rank_1")
  resp6b = read.csv("../data/rankings_resp6b.csv")[,-1]
  colnames(resp6b) = c( "district", "rank_2")
  
  resp7 = read.csv("../data/rankings_resp7.csv")[,-1]
  colnames(resp7) = c("district","rank_1")
  resp7b = read.csv("../data/rankings_resp7b.csv")[,-1]
  colnames(resp7b) = c("district", "rank_2")
  
  resp8 = read.csv("../data/rankings_resp8.csv")[,-1]
  colnames(resp8) = c("district","rank_1")
  resp8b = read.csv("../data/rankings_resp8b.csv")[,-1]
  colnames(resp8b) = c( "district", "rank_2")

########################################################################################################################
# Calculate Intracoder Reliability
########################################################################################################################

# Store correlations for each MTurker's responses over time
  intracoder <- c()
  temp = merge(resp1, resp1b)
  intracoder[1] <- cor(temp$rank_1, temp$rank_2)
  temp = merge(resp2, resp2b)
  intracoder[2] <- cor(temp$rank_1, temp$rank_2)
  temp = merge(resp3, resp3b)
  intracoder[3] <- cor(temp$rank_1, temp$rank_2)
  temp = merge(resp4, resp4b)
  intracoder[4] <- cor(temp$rank_1, temp$rank_2)
  temp = merge(resp5, resp5b)
  intracoder[5] <- cor(temp$rank_1, temp$rank_2)
  temp = merge(resp6, resp6b)
  intracoder[6] <- cor(temp$rank_1, temp$rank_2)
  temp = merge(resp7, resp7b)
  intracoder[7] <- cor(temp$rank_1, temp$rank_2)
  temp = merge(resp8, resp8b)
  intracoder[8] <- cor(temp$rank_1, temp$rank_2)

# Export Data
  save(intracoder, file = "../results/fig4_data.RData")
  
########################################################################################################################
# Calculate Intercoder Reliability
########################################################################################################################
  
  resp5 = resp5[order(resp5$district),] 
  time1 = cbind(resp1, resp2, resp3, resp4, resp6, resp7, resp8, resp5)
  time1 = time1[,c(2,3,6,8,10,12,14)]
  mat = cor(time1)
  mat2 = mat[upper.tri(mat)]
  
  resp5b = resp5b[order(resp5b$district),] 
  time2 = cbind(resp1b, resp2b, resp3b, resp4b, resp6b, resp7b, resp8b, resp5b)
  time2 = time2[,c(2,6,8,10,12,14)]
  mat3 = cor(time2)
  mat3 = mat3[upper.tri(mat3)]
  
  mat2 = c(mat2, mat3)
  rand = replicate(1000, cor(sample(1:100), sample(1:100)))
  df = data.frame(val = c(unlist(mat2), rand), source = c(rep("Truth", length(unlist(mat2))), rep("Random", length(unlist(rand)))))
  
  save(df, time1, file = "../results/fig3_data.RData")
  