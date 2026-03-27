######################################################################

##      PROJECT TITLE:    MIDAS                                        
##      CODE AUTHOR:      THOMAS S. ROBINSON                        
##      EMAIL:            THOMAS.ROBINSON@POLITICS.OX.AC.UK         

##      DESCRIPTION:      MAR-1 MIDAS import                                  

######################################################################

library(tidyverse)
library(MASS)
library(Amelia)

B <- 100
Ms <- 10 # no. of imputed datasets
######################################################################
####  *** Python midas code must have been run before proceeding ***  
######################################################################
#### Import MIDAS imputed datasets and run analysis ####

## MIDAS analysis

midas_results <- data.frame(boot = (1:B),
                            type = NA,
                            b0 = NA,
                            b1 = NA,
                            b2 = NA,
                            rmse = NA)
for (b in 1:B) {
  
  # Set counters for coefficients
  mid_b0 <- 0
  mid_b1 <- 0
  mid_b2 <- 0
  mid_rmse <- 0
  
  
  # Loop over imputed datasets and recover coefficient & rmse estimates
  for (m in 1:Ms) {
    mid_data <- paste0("mar1/data_tmp/midas_",b,"_",m,".csv")
    mid_lm <- lm(Y ~ X1 + X2, data = read_csv(mid_data))
    
    mid_b0 <- mid_b0 + mid_lm$coefficients[1]
    mid_b1 <- mid_b1 + mid_lm$coefficients[2]
    mid_b2 <- mid_b2 + mid_lm$coefficients[3]
    mid_rmse <- mid_rmse + sqrt(mean((mid_lm$residuals)^2))
    
  }
  
  # Take averages
  midas_b0 <- mid_b0/Ms
  midas_b1 <- mid_b1/Ms
  midas_b2 <- mid_b2/Ms
  midas_rmse <- mid_rmse/Ms
  
  midas_results[b,2] <- "MIDAS"
  midas_results[b,3:6] <- c(midas_b0,
                            midas_b1,
                            midas_b2,
                            midas_rmse)
}


# Save
write_csv(midas_results, "mar1/mar1_midas_results.csv")