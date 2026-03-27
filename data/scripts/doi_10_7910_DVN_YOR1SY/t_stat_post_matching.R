######################################################################################################################################
###                                                                                                                                ###
###     Are sustainability-linked loans designed to effectively incentivize corporate sustainability? A framework for review       ###
###                                                                                                                                ###  
###                                                             SEP 2023                                                           ###
###                                                                                                                                ###
###                                                                                                                                ###
######################################################################################################################################



# Load required libraries
library(readxl)
library(tibble)

# Set the path to the Dropbox folder
dropbox_path <- "Path"

# Specify the file path within the Dropbox folder
file_path <- file.path(dropbox_path, "file_for_matching_post.csv")

# Read the Excel file into R
data <- read_csv(file_path)

# Subset the data to include only the relevant columns for t-tests
selected_columns <- c("issued_SLL", "issued_conventional", "btm", "leverage",
                      "total_assets", "prof", "msci_score")

data_subset <- data[selected_columns]

# Split the data into two groups: treated group (issued_SLL = 1) and control group (issued_conventional_loan = 1)
treated_group <- data_subset[data_subset$issued_SLL == 1, ]
control_group <- data_subset[data_subset$issued_conventional == 1, ]

# Function to calculate means, t-statistic, and p-value for a given covariate
get_summary_stats <- function(covariate) {
  treated_mean <- mean(treated_group[[covariate]], na.rm = TRUE)
  control_mean <- mean(control_group[[covariate]], na.rm = TRUE)
  t_result <- t.test(treated_group[[covariate]], control_group[[covariate]], var.equal = FALSE)
  t_stat <- format(t_result$statistic, digits = 3)
  p_value <- t_result$p.value
  
  tibble(
    Covariate = covariate,
    Treated_Mean = treated_mean,
    Control_Mean = control_mean,
    T_Statistic = t_stat,
    P_Value = p_value
  )
}

# Calculate summary statistics for each covariate
covariates <- c("btm", "leverage", "total_assets", "prof", "msci_score")
summary_stats <- lapply(covariates, get_summary_stats)

# Create a summary table with results for all covariates
summary_table <- do.call(rbind, summary_stats)

# Print the summary table
print(summary_table)

# Export the summary table to an Excel file
library(writexl)
output_file <- file.path(dropbox_path, "summary_table_post.csv")
write_xlsx(summary_table, output_file)

# short cut
t.test(leverage ~ issued_SLL, data = post_matching, var.equal = FALSE)
t.test(btm ~ issued_SLL, data = post_matching, var.equal = FALSE)
t.test(total_assets ~ issued_SLL, data = post_matching, var.equal = FALSE)
t.test(prof ~ issued_SLL, data = post_matching, var.equal = FALSE)
t.test(msci_score ~ issued_SLL, data = post_matching, var.equal = FALSE)
