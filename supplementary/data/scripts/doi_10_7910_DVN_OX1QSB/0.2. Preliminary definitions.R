# Adjust input here and only here.

# Choose prepared setup (1-5) or set it manually (0)
    # 0: Manually choose below
    # 1: Quarterly data, Goyal Welch predictors. sample start 1947, sample end 2022, horizon = 1, other params as in paper)
    # 2: Monthly data, Goyal Welch predictors, sample start 1947, sample end 2022, horizon = 1, other params as in paper)
    # 3: Monthly data, Goyal Welch predictors, sample start 1927, sample end 2022, horizon = 1, other params as in paper)
    # 4: Monthly data, Li Tsiakas predictors, sample start 1947, sample end 2022, horizon = 1, other params as in paper)
    # 5: Monthly data, Rapach Zhou predictors, sample start 1947, sample end 2022, horizon = 1, other params as in paper)

setup = 3


#-----------------------------------------------------------------------------------------------
# Start of manual settings if setup==0
if (setup == 0) {
  
  # General definitions ---------------------------
  start.data <- 19261 # first period of data set to prepare (yyyyq for quarterly, yyyymm for monthly data)
  first.oos <- 19651 # first out-of-sample period (yyyyq or yyyymm)
  last.oos <- 20224 # last out-of-sample period
  start.year.is <- 1947 # first year used for in-sample estimation
  start.year.oos <- 1965 # first year used for out-of-sample estimation
  
  
  # Select predictors; options are:
  predictors <- "welch.goyal.quarterly" # As in RSZ for quarterly data
  # predictors <- "welch.goyal.monthly"
  # predictors <- "li.tsiakas" # as in Li & Tsiakas
  # predictors <- "rapach.zhou" # as in Rapach & Zhou
  
  
  
  # Define frequency (4 = quarterly, 12 = monthly)
  frequency <- 12
  # Define length of prediction horizon (1 = 1 period, and so on. Results for h>1 are not used in paper, and not validated. Implementation for h>1differs from the Stata 
  # implementation used for the paper)
  horizon <- 1
  
  
  # Definitions for E-Net, LASSO, Ridge -------------------
  vec <- c(0, 0.5, 1)
  method <- c("Ridge", "Elasticnet", "LASSO")
  
  # Definitions for utility gains -----------------------------
  
  variance.years <- 10
  gamma.allocation <- 3
  transaction.cost <- 20   # basis points
}

#----------------------------------------------------------------------------------------------------
#End of manual settings. Below are only pre-defined setups






if (setup == 1) {

  # General definitions ---------------------------
  start.data <- 19261 # first period of data set to prepare (yyyyq for quarterly, yyyymm for monthly data)
  first.oos <- 19651 # first out-of-sample period (yyyyq or yyyymm)
  last.oos <- 20224 # last out-of-sample period
  start.year.is <- 1947 # first year used for in-sample estimation
  start.year.oos <- 1965 # first year used for out-of-sample estimation

  predictors <- "welch.goyal.quarterly" # As in RSZ for quarterly data
  
  # Define frequency (4 = quarterly, 12 = monthly)
  frequency <- 4
  # Define length of prediction horizon (1 = 1 period, and so on. Results for h>1 are not used in paper, and not validated. Implementation for h>1differs from the Stata 
  # implementation used for the paper)
  horizon <- 1
  
  # Definitions for E-Net, LASSO, Ridge -------------------
  vec <- c(0, 0.5, 1)
  method <- c("Ridge", "Elasticnet", "LASSO")
  
  # Definitions for utility gains -----------------------------
  
  variance.years <- 10
  gamma.allocation <- 3
  transaction.cost <- 20   # basis points
}






if (setup == 2) {
  
  # General definitions ---------------------------
  start.data <- 192601 # first period of data set to prepare (yyyyq for quarterly, yyyymm for monthly data)
  first.oos <- 196501 # first out-of-sample period (yyyyq or yyyymm)
  last.oos <- 202212 # last out-of-sample period
  start.year.is <- 1947 # first year used for in-sample estimation
  start.year.oos <- 1965 # first year used for out-of-sample estimation
  
  predictors <- "welch.goyal.monthly"
  
  # Define frequency (4 = quarterly, 12 = monthly)
  frequency <- 12
  # Define length of prediction horizon (1 = 1 period, and so on. Results for h>1 are not used in paper, and not validated. Implementation for h>1differs from the Stata 
  # implementation used for the paper)
  horizon <- 1
  
  # Definitions for E-Net, LASSO, Ridge -------------------
  vec <- c(0, 0.5, 1)
  method <- c("Ridge", "Elasticnet", "LASSO")
  
  # Definitions for utility gains -----------------------------
  
  variance.years <- 10
  gamma.allocation <- 3
  transaction.cost <- 20   # basis points
}



if (setup == 3) {
  
  # General definitions ---------------------------
  start.data <- 192601 # first period of data set to prepare (yyyyq for quarterly, yyyymm for monthly data)
  first.oos <- 196501 # first out-of-sample period (yyyyq or yyyymm)
  last.oos <- 202212 # last out-of-sample period
  start.year.is <- 1927 # first year used for in-sample estimation
  start.year.oos <- 1965 # first year used for out-of-sample estimation
  
  predictors <- "welch.goyal.monthly"
  
  # Define frequency (4 = quarterly, 12 = monthly)
  frequency <- 12
  # Define length of prediction horizon (1 = 1 period, and so on. Results for h>1 are not used in paper, and not validated. Implementation for h>1differs from the Stata 
  # implementation used for the paper)
  horizon <- 1
  
  # Definitions for E-Net, LASSO, Ridge -------------------
  vec <- c(0, 0.5, 1)
  method <- c("Ridge", "Elasticnet", "LASSO")
  
  # Definitions for utility gains -----------------------------
  
  variance.years <- 10
  gamma.allocation <- 3
  transaction.cost <- 20   # basis points
}




if (setup == 4) {
  
  # General definitions ---------------------------
  start.data <- 192601 # first period of data set to prepare (yyyyq for quarterly, yyyymm for monthly data)
  first.oos <- 196501 # first out-of-sample period (yyyyq or yyyymm)
  last.oos <- 202212 # last out-of-sample period
  start.year.is <- 1947 # first year used for in-sample estimation
  start.year.oos <- 1965 # first year used for out-of-sample estimation
  
  predictors <- "li.tsiakas" # as in Li & Tsiakas
  
  # Define frequency (4 = quarterly, 12 = monthly)
  frequency <- 12
  # Define length of prediction horizon (1 = 1 period, and so on. Results for h>1 are not used in paper, and not validated. Implementation for h>1differs from the Stata 
  # implementation used for the paper)
  horizon <- 1
  
  # Definitions for E-Net, LASSO, Ridge -------------------
  vec <- c(0, 0.5, 1)
  method <- c("Ridge", "Elasticnet", "LASSO")
  
  # Definitions for utility gains -----------------------------
  
  variance.years <- 10
  gamma.allocation <- 3
  transaction.cost <- 20   # basis points
}



if (setup == 5) {
  
  # General definitions ---------------------------
  start.data <- 192601 # first period of data set to prepare (yyyyq for quarterly, yyyymm for monthly data)
  first.oos <- 196501 # first out-of-sample period (yyyyq or yyyymm)
  last.oos <- 202212 # last out-of-sample period
  start.year.is <- 1947 # first year used for in-sample estimation
  start.year.oos <- 1965 # first year used for out-of-sample estimation
  
  predictors <- "rapach.zhou" # as in Rapach & Zhou
  
  # Define frequency (4 = quarterly, 12 = monthly)
  frequency <- 12
  # Define length of prediction horizon (1 = 1 period, and so on. Results for h>1 are not used in paper, and not validated. Implementation for h>1differs from the Stata 
  # implementation used for the paper)
  horizon <- 1
  
  # Definitions for E-Net, LASSO, Ridge -------------------
  vec <- c(0, 0.5, 1)
  method <- c("Ridge", "Elasticnet", "LASSO")
  
  # Definitions for utility gains -----------------------------
  
  variance.years <- 10
  gamma.allocation <- 3
  transaction.cost <- 20   # basis points
}




