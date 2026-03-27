#########################################################################
##  This is an illustrative R script to create the following datasets: ##    
##   1. SIMDAT30 data (SIMDAT30.csv) for Section 3.1 in the paper      ##
##   2. SIMDAT60 data (SIMDAT60.csv) and                               ##
##      the item metadata (ITEMMETA_EX2.csv) for Section 3.2           ##  
#########################################################################

######################################################
# 1. Create the SIMDAT30 data for Section 3.1
######################################################
# Load required packages
library(irtQ)
library(tidyverse)

# Set the path of the Data folder where the data will be saved
path.data <- ".../Data"  

# Set conditions to be simulated
tsize <- 30
mu.a <- 0.5
sigma.a <- 0.3
mu.b <- 0.0
sigma.b <- 1.0

# Set the random seed number
set.seed(11)

# Generate the item parameters for 30 items
# (1) a-parameters (with a mean of about 1.72 and SD of about 0.53)
repeat {
  a.par <- rlnorm(tsize, mu.a, sigma.a)  
  range.a <- range(a.par)
  if (range.a[1] >= 0.5 & range.a[2] <= 3.5) {
    break
  } else {
    next
  }
}

# (2) b-parameters (with a mean of 0 and SD of 1)
repeat {
  b.par <- rnorm(tsize, mu.b, sigma.b)  
  range.b <- range(b.par)
  if (range.b[1] >= -3.5 & range.b[2] <= 3.5) {
    break
  } else {
    next
  }
}

# (3) g-parameters
repeat {
  g.par <- runif(tsize, min = 0, max = 0.3)  
  range.g <- range(g.par)
  if (range.g[2] <= 0.3) {
    break
  } else {
    next
  }
}

# Create the item metadata 
x <- irtQ::shape_df(par.drm = list(a = a.par, b = b.par, g = g.par), 
                    cats = 2, model = "3PLM")

# Generate 2,000 true thetas
theta <- rnorm(2000, mean = 0, sd = 1)

# Generate the item response data for all 30 items
resp <- irtQ::simdat(x = x, theta = theta, D = 1.702)

# Save the data
write.table(x = resp, 
            file = ".../SIMDAT30.csv", 
            sep = ",", row.names = FALSE, col.names = FALSE)

######################################################
# 2. Generate the SIMDAT60 data and 
#    the item metadata for Section 3.2
######################################################
# Load required packages
library(irtQ)
library(tidyverse)

# Set the path of the Data folder where the data will be saved
path.data <- ".../Data"  

# Set conditions to be simulated
tsize <- 60
mu.a <- 0.5
sigma.a <- 0.3
mu.b <- 0.0
sigma.b <- 1.0

# Set the random seed number
set.seed(10)

# Generate the item parameters for 60 items
# (1) a-parameters (with a mean of about 1.72 and SD of about 0.53)
repeat {
  a.par <- rlnorm(tsize, mu.a, sigma.a)  
  range.a <- range(a.par)
  if (range.a[1] >= 0.5 & range.a[2] <= 3.5) {
    break
  } else {
    next
  }
}

# (2) b-parameters (with a mean of 0 and SD of 1)
repeat {
  b.par <- rnorm(tsize, mu.b, sigma.b)  
  range.b <- range(b.par)
  if (range.b[1] >= -3.5 & range.b[2] <= 3.5) {
    break
  } else {
    next
  }
}

# (3) g-parameters
repeat {
  g.par <- runif(tsize, min = 0, max = 0.3)  
  range.g <- range(g.par)
  if (range.g[2] <= 0.3) {
    break
  } else {
    next
  }
}

# Create the item metadata 
x <- irtQ::shape_df(par.drm = list(a = a.par, b = b.par, g = g.par), 
                    cats = 2, model = "3PLM") %>% 
  dplyr::mutate_at(.vars = 4:6, ~ {round(x = .x, digits = 2)})

# Assign item IDs for the 50 operational and 10 pretest items 
x$id <- c(paste0("OP", 1:50), paste0("PT", 1:10))

# Generate 2,000 true thetas
theta <- rnorm(2000, mean = 0, sd = 1)

# Generate the item response data for all 60 items
resp <- irtQ::simdat(x = x, theta = theta, D = 1.702)

# Divide the response data into two groups: operational and pretest items
resp_op <- resp[, 1:50]
resp_pt <- resp[, 51:60] 

# Add missing values (NAs) randomly to the pretest item response data
# Each examinee will respond to only 5 pretest items
for (i in 1:length(theta)) {
  ran <- sample(x = 1:10, size = 5, replace = FALSE)
  resp_pt[i, ran] <- NA
}

# Recreate the full response data by combining the 50 operational items and 
# the 10 pretest items, including NAs
resp_new <- cbind(resp_op, resp_pt)

# Save the data
write.table(x = x, 
            file = here::here(path.data, "ITEMMETA_EX2.csv"), 
            sep = ",", row.names = FALSE, col.names = TRUE)
write.table(x = resp_new, 
            file = here::here(path.data, "SIMDAT60.csv"), 
            sep = ",", row.names = FALSE, col.names = FALSE)
