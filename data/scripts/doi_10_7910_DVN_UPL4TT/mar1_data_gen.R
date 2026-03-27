######################################################################
                                                                  
##      PROJECT TITLE:    MIDAS                                        
##      CODE AUTHOR:      THOMAS S. ROBINSON                        
##      EMAIL:            THOMAS.ROBINSON@POLITICS.OX.AC.UK         
                                                                  
##      DESCRIPTION:      MAR-1 data generation and R results                                  
                                                                  
######################################################################

## Packages
library(tidyverse)
library(MASS)
library(Amelia)

set.seed(89)

if (!file.exists("mar1/data_tmp")) {
  dir.create("mar1/data_tmp")
} else {
  unlink("mar1/data_tmp", recursive = TRUE)
  dir.create("mar1/data_tmp")
}

## Fixed covariance matrix
sigma_mat <- matrix(c(1, -.12, -.1, .5, .1,
                      -.12, 1, .1, -.6, .1,
                      -.1, .1, 1, -.5, .1,
                      .5, -.6, -.5, 1, .1,
                      .1, .1, .1, .1, 1), 
                    ncol = 5) 

## Results table
B <- 100 # no. of simulations
S <- 3 # no. of R-based strategies
Ms <- 10 # no. of imputed datasets
results <- data.frame(boot = rep(1:B, each = S),
                      type = NA,
                      b0 = NA,
                      b1 = NA,
                      b2 = NA,
                      rmse = NA)

# Bootstrap procedure

complete_cases <- c()

for (b in 1:B) {
  
  ## Note: will save perturbed dataset in data folder, which can then be 
  ## parsed through MIDAS python script
  
  ## MAR-1 simulation based on Honnaker and King 2001, p.61, 
  ## with altered levels of missingness
  
  # Draw from multivariate normal
  D <- mvrnorm(n = 500, mu= c(0,0,0,0,0), Sigma = sigma_mat) %>% 
    as.data.frame(.)
  
  colnames(D) <- c("Y","X1","X2","X3","X4")
  
  # Generate empty missingness matrix
  M <- matrix(ncol = 5, nrow = 500)
  
  # Y and X4 are MCAR
  U1 <- runif(500,0,1)
  M[,1] <- ifelse(U1 < 0.85, 1, 0)
  M[,5] <- ifelse(U1 < 0.85, 1, 0)
  
  # U5 <- runif(500,0,1)
  # M[,5] <- ifelse(U5 < 0.6, 1, 0)
  
  # X3 is completely observed
  M[,4] <- 1
  
  # X1 and X2 are MAR:
  U2 <- runif(500,0,1)
  M[,2] <- ifelse((D[,"X3"] < -1) & (U2 < 0.9), 0, 1)
  
  U3 <- runif(500,0,1)
  M[,3] <- ifelse((D[,"X3"] < -1) & (U3 < 0.9), 0, 1)
  
  # Copy D to create missing data matrix
  miss <- D
  
  # Loop over columns applying missingness
  for (i in 1:5) {
    miss[,i] <- ifelse(M[,i] == 0, NA, D[,i])
  }
  
  # Just to keep track of how many regression rows are fully observed per simulation
  complete_cases[b] <- mean(complete.cases(miss[,1:3]))
  
  # Save file for MIDAS test
  write_csv(miss, path = paste0("mar1/data_tmp/mar1_draw_",b,".csv"))
  
  ## Complete data
  
  full_lm <- lm(Y ~ X1 + X2, data = D)
  full_coefs <- full_lm$coefficients
  full_rmse <- sqrt(mean((full_lm$residuals)^2))
  
  results[(3*b)-2,2] <- "Complete Data"
  results[(3*b)-2,3:6] <- c(full_coefs[1],
                            full_coefs[2],
                            full_coefs[3],
                            full_rmse)
  
  ## Listwise analysis
  lw_data <- miss[,c("Y","X1","X2")] %>% 
    filter(complete.cases(.))
  
  lw_lm <- lm(Y ~ X1 + X2, data = lw_data)
  lw_coefs <- lw_lm$coefficients
  lw_rmse <- sqrt(mean((lw_lm$residuals)^2))
  
  results[(3*b)-1,2] <- "Listwise"
  results[(3*b)-1,3:6] <- c(lw_coefs,lw_rmse)
  
  ## Amelia
  
  # Generate imputed datasets
  am_imputes <- amelia(miss, m=Ms)$imputations
  
  # Set counters for coefficients
  am_b0 <- 0
  am_b1 <- 0
  am_b2 <- 0
  am_rmse <- 0
  
  
  # Loop over imputed datasets and recover coefficient & rmse estimates
  for (m in 1:Ms) {
    am_lm <- lm(Y ~ X1 + X2, data = am_imputes[[m]])
  
    am_b0 <- am_b0 + am_lm$coefficients[1]
    am_b1 <- am_b1 + am_lm$coefficients[2]
    am_b2 <- am_b2 + am_lm$coefficients[3]
    am_rmse <- am_rmse + sqrt(mean((am_lm$residuals)^2))
    
  }
  
  # Take averages
  amelia_b0 <- am_b0/Ms
  amelia_b1 <- am_b1/Ms
  amelia_b2 <- am_b2/Ms
  amelia_rmse <- am_rmse/Ms
  
  results[(3*b),2] <- "Amelia"
  results[(3*b),3:6] <- c(amelia_b0,
                       amelia_b1,
                       amelia_b2,
                       amelia_rmse)
  
}

# Save
write_csv(results, "mar1/mar1_r_results.csv")
