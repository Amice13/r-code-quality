# =================================================================================================
# SCRIPT: 01_Data_Preprocessing.R
#
# PROJECT: Pandemic Digital Media and Adolescents' Parenthood Desires
# AUTHOR: Seunghwan (Shawn) Yoo
# DATE: September 12, 2025
#
# PURPOSE:
# This script prepares the analytical dataset for the study. It performs the following steps:
# 1. Loads raw, yearly Monitoring the Future (MTF) data files from 2018-2023.
# 2. Combines the yearly data into a single panel dataset.
# 3. Renames variables from MTF codes to intuitive names.
# 4. Performs sample selection and reports exclusion counts for transparency.
# 5. Imputes missing data on select control variables using K-Nearest Neighbors (KNN).
# 6. Exports the final, cleaned dataset as a .dta file for analysis in Stata.
#
# INSTRUCTIONS:
# 1. Place this R script in your main project folder.
# 2. Create a subfolder named "Data" within your project folder.
# 3. Place all the raw MTF yearly data folders (e.g., "2023 MTF (ICPSR 38882)") inside the "Data" subfolder.
# 4. The script will automatically find the data and process it.
# =================================================================================================

# SECTION 1: SETUP - LOAD PACKAGES AND SETTINGS
# -------------------------------------------------------------------------------------------------
# This section loads all required packages. It will automatically install them if they are missing.

# List of required packages
packages <- c("dplyr", "tidyr", "haven", "VIM", "here")

# Check, install, and load packages
for (pkg in packages) {
  if (!require(pkg, character.only = TRUE)) {
    install.packages(pkg)
    library(pkg, character.only = TRUE)
  }
}

# SECTION 2: DEFINE FILE PATHS AND GLOBAL VARIABLES
# -------------------------------------------------------------------------------------------------
# Define the path to the raw data. The `here` package helps make paths relative and portable.
# This assumes your data is in a subfolder named "Data/Data 2013-2023/2024 Paper".
# Please adjust the path if your folder structure is different.
data_path <- here::here("Data", "Data 2013-2023", "2024 Paper")
folder_list <- list.dirs(data_path, recursive = FALSE)

# Define the list of variables to be extracted from the raw MTF files.
variables_to_keep <- c(
  'V3252', # Desired_Children
  'V3624', # Social_Media_Hours
  'V3628', # Weekday_Video_Hours
  'V3629', # Weekend_Video_Hours
  'V3150', # Gender
  'V3151', # Race
  'V3153', # Parental_Marital_Status
  'V3170', # Religion_Importance
  'V3152', # City_Size
  'V3167', # Political_Beliefs (Poli_Beli)
  'V3156', # Lives_with_Mother
  'V3155', # Lives_with_Father
  'V49',   # Siblings
  'V3208', # Happiness_Level
  'V3164', # Mother_Education
  'V3163', # Father_Education
  'V3621', # Gaming_Hours
  'V3195', # Dating_Frequency (Date_Wk)
  'ARCHIVE_WT' # Survey Weight
)

# SECTION 3: DATA LOADING AND PREPROCESSING FUNCTION
# -------------------------------------------------------------------------------------------------
# This function loads and performs initial cleaning for one year of MTF data.

load_and_preprocess_data <- function(folder_path, variables) {
  # Extract ICPSR number from the folder path to construct the file name
  folder_num <- gsub(".*\\(([^)]+)\\).*", "\\1", folder_path)
  folder_temp <- paste(unlist(strsplit(folder_num, " ")), collapse = "_")
  
  # Generate the full path to the .rda data file
  data_file <- file.path(folder_path, folder_temp, "DS0004", paste0(strsplit(folder_num, " ")[[1]][2], "-0004-Data.rda"))
  
  # Load the data object
  load(data_file)
  data <- get(paste0("da", strsplit(folder_num, " ")[[1]][2], ".0004"))
  
  # Add a 'Survey_Year' column
  year <- as.numeric(gsub(".*(20\\d{2}).*", "\\1", folder_path))
  data <- data %>% mutate(Survey_Year = year)
  
  # Select only the required variables
  data <- data %>% select(all_of(variables), Survey_Year)
  
  return(data)
}

# SECTION 4: COMBINE DATA AND RENAME VARIABLES
# -------------------------------------------------------------------------------------------------
# Apply the function to all yearly folders and combine them into one dataset.
data_list <- lapply(folder_list, load_and_preprocess_data, variables = variables_to_keep)
combined_data <- bind_rows(data_list)

# Rename cryptic MTF variable codes to intuitive names for clarity.
combined_data <- combined_data %>%
  rename(
    Desired_Children      = V3252,
    Social_Media_Hours    = V3624,
    Weekday_Video_Hours   = V3628,
    Weekend_Video_Hours   = V3629,
    Gender                = V3150,
    Race                  = V3151,
    Parental_Marital_Status = V3153,
    Religion_Importance   = V3170,
    City_Size             = V3152,
    Poli_Beli             = V3167,
    Lives_with_Mother     = V3156,
    Lives_with_Father     = V3155,
    Siblings              = V49,
    Happiness_Level       = V3208,
    Mother_Education      = V3164,
    Father_Education      = V3163,
    Gaming_Hours          = V3621,
    Date_Wk               = V3195,
    ARCHIVE_WT            = ARCHIVE_WT
  )

# Convert labelled Gender variable to a simple numeric type for reliable filtering.
combined_data <- combined_data %>% mutate(Gender = as.numeric(Gender))


# SECTION 5: SAMPLE CONSTRUCTION & EXCLUSION COUNTS
# -------------------------------------------------------------------------------------------------
# This section documents the step-by-step process of arriving at the final analytical sample.
# These numbers are used in the Data and Methods section of the manuscript.

print("--- Sample Construction ---")

# Step 1: Report the total number of cases after combining all survey years.
total_initial_sample <- nrow(combined_data)
print(paste("1. Initial total sample size (2018-2023):", total_initial_sample))

# Step 2: Calculate and exclude respondents not identifying as man or woman.
# MTF codes Gender as 1=Male, 2=Female. Other values are excluded.
excluded_gender <- combined_data %>%
  filter(!(Gender %in% c(1, 2))) %>%
  nrow()
print(paste("2. Excluded for not identifying as man or woman:", excluded_gender))

# Apply the gender filter.
data_gender_filtered <- combined_data %>% filter(Gender %in% c(1, 2))
print(paste("   -> Sample size after gender filter:", nrow(data_gender_filtered)))

# Step 3: Calculate and exclude respondents with missing data on the dependent variable.
excluded_missing_dv <- data_gender_filtered %>%
  filter(is.na(Desired_Children)) %>%
  nrow()
print(paste("3. Excluded for missing data on Parenthood Desire:", excluded_missing_dv))

# Step 4: Report the final analytical sample size.
final_sample <- data_gender_filtered %>%
  filter(!is.na(Desired_Children))
final_sample_count <- nrow(final_sample)
print(paste("4. Final analytical sample size:", final_sample_count))
print("---------------------------")


# SECTION 6: MISSING DATA IMPUTATION
# -------------------------------------------------------------------------------------------------
# Use KNN imputation to handle missingness on control variables.

# Note: For accuracy, imputation should ideally be run on the final, filtered sample.
# The following code applies imputation to the `final_sample` data frame.
imputation_vars <- c(
  'Weekday_Video_Hours', 'Weekend_Video_Hours', 'Siblings', 
  'Happiness_Level', 'Mother_Education', 'Father_Education', 
  'Gaming_Hours', 'Religion_Importance', 'Poli_Beli', 'Date_Wk'
)

# Perform KNN Imputation with k=5
imputed_data <- kNN(final_sample, variable = imputation_vars, k = 5)

# The kNN function returns a data frame with imputed values and flags.
# We select only the original and imputed columns, removing the flag columns.
final_sample_imputed <- imputed_data %>% select(names(final_sample))


# SECTION 7: FINAL DATA EXPORT
# -------------------------------------------------------------------------------------------------
# Export the cleaned, imputed, and finalized dataset to a Stata .dta file.
# This file will be used as the input for the Stata analysis script (02_Analysis.do).

# Define the output path
output_path <- here::here("Data", "Social_Media_Fertility_Weight_2018-2023_Processed.dta")

# Write the file
write_dta(final_sample_imputed, output_path)

print(paste("Final dataset successfully saved to:", output_path))

# --- END OF SCRIPT ---