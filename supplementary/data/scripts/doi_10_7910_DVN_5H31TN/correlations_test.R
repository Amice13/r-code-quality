library(cocor)

#get correlations with pvalues
index_cor_full <- comparison_df %>%
  select(c('gdb','odb','ODIN_score', 'SCI', 'SPI.INDEX' ),devindex) %>%
  rename(
    SPI=SPI.INDEX,
    SCI=SCI, 
    ODIN=ODIN_score, 
    ODB=odb, 
    GDB=gdb
  ) %>%
  as.matrix() %>%
  rcorr()

#get just pearon correlation
index_cor <- index_cor_full$r
index_cor <- index_cor[6:11,1:5]

#get correlation between indices
index_between_cor <- index_cor_full$r
index_between_cor <- index_between_cor[1:5,1:5]

#get Ns
index_n <- index_cor_full$n
index_n <- index_n[6:11,1:5]


#round
index_cor <- round(index_cor,2)

#color
index_col <- index_cor
index_col[abs(index_cor)>=0.5] <- "#fcca46"
index_col[abs(index_cor)>=0.65] <- "#a1c181"
index_col[abs(index_cor)>=0.8] <- "#619b8a"
index_col[abs(index_cor)<0.5] <- "white"

#get statistical significance
index_sig <- index_cor_full$P
index_sig <- index_sig[6:11,1:5]

rownames(index_cor) <- devindex_names
rownames(index_sig) <- devindex_names 



# Create a correlation matrix
cor_matrix <- index_cor

# Function to calculate p-value from z-score
p_value_from_z <- function(z) {
  2 * (1 - pnorm(abs(z)))
}

# Iterate through the correlation matrix
n <- nrow(cor_matrix)
c <- ncol(cor_matrix)
p_values <- matrix(NA, n, c*c)  # Store p-values for each pairwise comparison
p_values_pearson <- matrix(NA, n, c*c)  # Store p-values for each pairwise comparison
p_values_steiger <- matrix(NA, n, c*c)  # Store p-values for each pairwise comparison

for (i in 1:n) {
  for (j in 1:c) {
    for (k in 1:c) {
      
      
      row1 <- i
      col1 <- j
      correlation_1 <- cor_matrix[row1,col1]
      
      row2 <- i
      col2 <- k
      correlation_2 <- cor_matrix[row2, col2]
      
      #between correlation
      correlation_between <- index_between_cor[col1,col2]
      
      # Step 3: Calculate standard errors of z-transformed correlations
      n_1 <- index_n[row1,col1]  # Sample size for correlation_1
      n_2 <- index_n[row2,col2]  # Sample size for correlation_2
      n_min <- min(n_1,n_2)
      
      
      # Step 4: Perform z-test for the difference between z-scores. pearson1898: Pearson and Filon’s [7] z
      kfactor = correlation_between*(1-correlation_1^2-correlation_2^2)-0.5*(correlation_1*correlation_2)*(1-correlation_1^2-correlation_2^2-correlation_between^2)
      z_diff <- sqrt(n_min)*(correlation_1-correlation_2)/sqrt((1-correlation_1^2)^2+(1-correlation_2^2)^2-2*kfactor)
      
      
      if (is.na(z_diff)) {
        z_diff=0
      }
      # Calculate p-value
      pos <- (j-1)*5+k
      pval <- p_value_from_z(z_diff)
      
      if (k==1 & pval > 0.05)  {
        txt <- "GDB"
      } else if (k==2 & pval > 0.05) {
        txt <- "ODB"
      } else if (k==3  & pval > 0.05) {
        txt <- "ODIN"
      } else if (k==4  & pval > 0.05) {
        txt <- "SCI"
      } else if (k==5  & pval > 0.05) {
        txt <- "SPI"
      } else {
        txt <- ""
      }
      p_values[i, pos] <- pval
      
      #compare to cocor
      cocor_results <- cocor::cocor.dep.groups.overlap(r.jk=correlation_1, r.jh=correlation_2, r.kh=correlation_between, n=n_min)
      p_values_pearson[i, pos] <- cocor_results@pearson1898$p.value
      p_values_steiger[i, pos] <- cocor_results@steiger1980$p.value
      
    }
  }
}

#format as table
pval_df <- as.data.frame(p_values) %>%
  cbind(`Dev Index`=devindex_names) %>%
  select(-V1, -V7, -V13, -V19, -V25) %>%
  select(`Dev Index`, everything()) 

#format as table
pval_pearson_df <- as.data.frame(p_values_pearson) %>%
  cbind(`Dev Index`=devindex_names) %>%
  select(-V1, -V7, -V13, -V19, -V25) %>%
  select(`Dev Index`, everything()) 

pval_steiger_df <- as.data.frame(p_values_steiger) %>%
  cbind(`Dev Index`=devindex_names) %>%
  select(-V1, -V7, -V13, -V19, -V25) %>%
  select(`Dev Index`, everything()) 

#results are identical for pearson and nearly identical for steiger
dataCompareR::rCompare(pval_df, pval_pearson_df) %>% summary()
dataCompareR::rCompare(pval_df, pval_steiger_df) %>% summary()

