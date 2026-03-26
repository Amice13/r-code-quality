# Title: Statistical Syntax for Replication: Reading Proficiency of Grade 7 Students
# Repository: Submitted to Harvard Dataverse / openICPSR
# Purpose: This R script loads the provided CSV data and reproduces the key statistical findings
# (Weighted Mean and Descriptive Level) presented in the research manuscript (Chapter 4).

# --- 1. SETUP AND LOAD DATA ---
# This script uses the tidyverse package for efficient data handling.
# If 'tidyverse' is not installed, uncomment the line below:
# install.packages('tidyverse')
library(tidyverse)

# Define the file path to the CSV data file (Assumes the CSV is uploaded in the same repository folder)
data_file_path <- "reading_proficiency_data.csv"
data_raw <- read_csv(data_file_path)

# --- 2. DEFINE WEIGHTS AND LIKERT SCALE BOUNDARIES ---

# Assign weights to the proficiency levels as defined in the manuscript (Chapter III, Page 21):
# Frustration = 1, Instructional = 2, Independent = 3
WEIGHTS <- c(1, 2, 3)

# Define the numerical thresholds for the Descriptive Level (based on WM ranges):
# 1.00-1.66 (Frustration); 1.67-2.33 (Instructional); 2.34-3.00 (Independent)

# --- 3. REPRODUCE CALCULATIONS ---

results_reproduced <- data_raw %>%
# STEP A: Calculate the Product Sum (Numerator for Weighted Mean)
  mutate(
    Product_Sum = (Frustration_f * WEIGHTS[1]) +
                  (Instructional_f * WEIGHTS[2]) +
                  (Independent_f * WEIGHTS[3])
  ) %>%
# STEP B: Calculate the Weighted Mean (WM)
  mutate(
    Weighted_Mean = round(Product_Sum / N_Total, 4)
  ) %>%
# STEP C: Assign the Descriptive Level based on the predefined thresholds
  mutate(
    Descriptive_Level = case_when(
      Weighted_Mean <= 1.66 ~ "Frustration",
      Weighted_Mean <= 2.33 ~ "Instructional",
      TRUE ~ "Independent"
    )
  ) %>%
# STEP D: Select and display the final columns
  select(
    Section, Test, N_Total,
    Weighted_Mean, Descriptive_Level
  )

# --- 4. DISPLAY FINAL REPRODUCED RESULTS ---
# This output verifies that the script successfully replicates the manuscript's key findings.
print(results_reproduced)
