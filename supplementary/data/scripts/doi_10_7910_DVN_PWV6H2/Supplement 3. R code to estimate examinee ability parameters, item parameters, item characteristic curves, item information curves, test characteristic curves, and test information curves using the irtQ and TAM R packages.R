# Supplement 3. Ability parameter, difficulty parameter, TCC, IIC, ICC, ITC
# This R code is for the item analysis of the nursing exam based on the Rasch model. 
# Difficulty parameter estimate, ability parameter estimate,  total score, test characteristics curve, test information curve, item characteristics curve, and item information curve were obtained.
# R package irtQ (version 0.2.1 Published on 2024-08-25 ) was used. 
# R version 4.5.1, 
# R Studio version RStudio 2025.05.0+496 "Mariposa Orchid" Release (f0b76cc00df96fe7f0ee687d4bed0423bc3de1f8, 2025-05-04) for windows Mozilla/5.0 (Windows NT 10.0; Win64; x64) AppleWebKit/537.36 (KHTML, like Gecko) RStudio/2025.05.0+496 Chrome/132.0.6834.210 Electron/34.5.1 Safari/537.36, Quarto 1.6.42 

# Install and load required packages
install.packages("irtQ")
library(irtQ)

# Check if shape_df is included in irtQ package
ls("package:irtQ")

#if shape_df is not found add: source("https://raw.githubusercontent.com/cran/irtQ/master/R/shape_df.R")

# Load data
datRaw <- read.csv("Dataset_1.csv", header = TRUE)
itemOnly <- datRaw[, -1]      # Remove the first column (ID)
examinee_id <- datRaw[, 1]    # Save examinee IDs

# Calculate total scores for examinees
total_scores <- rowSums(itemOnly)

# Rasch model (1PL) setup 
par.drm <- list(a = NULL, b = NULL, g = NULL)
itemModel_rasch <- shape_df(par.drm = par.drm, itemOnly,item.id = names(itemOnly), cats = 2, model = "1PLM", default.par = TRUE)

# Parameter estimation
raschParameter <- est_irt(x = itemModel_rasch, data = itemOnly, fix.a.1pl = TRUE, a.val.1pl = 1)

# Estimate examinee ability (theta)
raschTheta <- est_score(x = raschParameter, data = itemOnly, method = "ML", range = c(-5, 5))

# Store examinee IDs with theta estimates
scores <- data.frame(
  id = examinee_id,
  theta = as.numeric(raschTheta[[1]])
  )

# Save results (optional)
write.table(scores, "rasch_theta.csv", sep = ",", row.names = FALSE)

# Save item difficulties
item_difficulty <- data.frame(
  item.id    = names(itemOnly),
  difficulty = raschParameter$par
  )
write.table(item_difficulty, "item_difficulty.csv", sep = ",", row.names = FALSE)

#  <--From here, plotting for test characteristric curve, test information curve, \
# item characteristic curve, and item information curve  -->


# Define theta range for plotting
theta_range <- seq(-4, 4, 0.1)

# <-- Plot TCC  (Test Characteristic Curve) -->
# Calculate the expected scores for each item at each theta
ICC_list <- traceline(raschParameter, theta_range)
plot(ICC_list$theta, ICC_list$tcc, type = "l", lwd = 2,
     xlab = "Ability (theta)",
     ylab = "Expected Total Score",
     main = "Test Characteristic Curve (TCC)")
grid()


# <-- Plot TIF (Test Information Function) -->
TIF_list <- info(raschParameter, theta_range)
plot(TIF_list$theta, TIF_list$tif, type = "l", lwd = 2,
     xlab = "Ability (theta)",
     ylab = "Test Information",
     main = "Test Information Function (TIF)")
grid()

# <-- Plot ICC (Item Characteristic Curve) for item 16-->
ICC_list <- traceline(raschParameter, theta_range)
num_items <- ncol(itemOnly)
num_theta <- length(theta_range)

# Reshape to a matrix: rows = theta, columns = items
ICC_matrix <- matrix(ICC_list$icc, nrow = num_theta, byrow = FALSE)

# For item 16 (column 16)
icc_item16 <- ICC_matrix[, 16]

# Plot
plot(theta_range, icc_item16, type = "l", lwd = 2,
     xlab = "Ability (theta)",
     ylab = "Probability of Correct Response",
     main = "Item Characteristic Curve (ICC) - Item 16")
grid()

# <-- Plot IIC (Item Information Curve) for item 16  -->
# Extract the IIC matrix (items × theta)
IIC_list <- info(raschParameter, theta_range)
IIC_matrix <- IIC_list$iif   # already has dimensions [items, theta]

# Extract the theta vector
theta_vals <- IIC_list$theta

# Plot IIC for item 16
plot(theta_vals, IIC_matrix[16, ], type = "l", lwd = 2,
     xlab = "Ability (theta)",
     ylab = "Item Information",
     main = "Item Information Curve (IIC) - Item 16")
grid()


