###################################################################
# Title: Post-processing and visualization of DaPars results
# Purpose: Identify and visualize significant APA events
# Method: DaPars PDUI-based filtering and scatter plot
# Written by Parmit Kumar Singh, PhD (Dana-Farber Cancer Institute)
# Criteria for significance:
#   - Absolute PDUI difference ≥ 0.2
#   - Absolute log2 fold change ≥ 1
#   - Directional change in PDUI between conditions
#
# Output:
#   - Scatter plot of mean PDUI values (Group B vs Group A)
#   - Color-coded genes:
#       Blue: 3′UTR lengthening (higher PDUI in Group A)
#       Red:  3′UTR shortening (lower PDUI in Group A)
#       Gray: Not significant
#
# This script was used in the DaPars analysis deposited
# in Harvard Dataverse.
##################################################################

# --------------------------------------------------
# Input: DaPars prediction output of DaPars_main.py 
# --------------------------------------------------
# Replace the path below with the location of your
# DaPars *_All_Prediction_Results.txt file

file1 <- "DaPars_All_Prediction_Results.txt"

# Read DaPars output
data <- read.table(file1, header = TRUE, sep = "\t")

# Remove rows with missing values
data1 <- data[complete.cases(data), ]

# Check dimensions of filtered data
dim(data1)

# -------------------------------
# Load required libraries
# -------------------------------
library(ggplot2)

# -------------------------------
# Classify genes based on PDUI
# -------------------------------
# Blue  : Significant 3′UTR lengthening (Group A > Group B)
# Red   : Significant 3′UTR shortening (Group A < Group B)
# Gray  : Not significant

data1[abs(data1$PDUI_Group_diff) >= 0.2 &
        abs(log2(data1$Group_A_Mean_PDUI / data1$Group_B_Mean_PDUI)) >= 1 &
        data1$Group_A_Mean_PDUI > data1$Group_B_Mean_PDUI,
      "quadrantx"] <- "blue"

data1[abs(data1$PDUI_Group_diff) >= 0.2 &
        abs(log2(data1$Group_A_Mean_PDUI / data1$Group_B_Mean_PDUI)) >= 1 &
        data1$Group_A_Mean_PDUI < data1$Group_B_Mean_PDUI,
      "quadrantx"] <- "red"

# Assign gray color to non-significant genes
data1$quadrantx[is.na(data1$quadrantx)] <- "gray"

# -------------------------------
# Visualization
# -------------------------------
# Scatter plot of mean PDUI values

gp <- ggplot(
  data1,
  aes(x = Group_B_Mean_PDUI,
      y = Group_A_Mean_PDUI,
      color = quadrantx)
) +
  geom_point() +
  labs(
    x = "Group B Mean PDUI",
    y = "Group A Mean PDUI",
    title = "DaPars PDUI Comparison Between Experimental Conditions"
  ) +
  scale_color_identity(guide = "legend") +
  theme_bw() +
  theme(
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank(),
    text = element_text(size = 16, face = "bold"),
    legend.title = element_blank()
  )

# Display plot
print(gp)
