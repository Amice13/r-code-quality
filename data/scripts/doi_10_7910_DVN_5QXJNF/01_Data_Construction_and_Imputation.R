# =================================================================================================
# SCRIPT: 01_Data_Preprocessing.R
#
# PROJECT: Digital Video Use, Emotional Well-Being, and Fertility Intentions
# AUTHOR: [Your Name]
# DATE: December 2025
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
# INSTRUCTIONS FOR REPLICATION:
# 1. Create a folder named "Raw_Data" inside your project directory.
# 2. Place the unzipped yearly MTF data folders inside "Raw_Data".
#    IMPORTANT: Do NOT rename the original folders (e.g., "2023 MTF (ICPSR 38882)").
#    The script relies on the folder naming convention to identify years and file paths.
# 3. Run this script. It will generate "MTF_Processed_2018_2023.dta" in the project root.
# =================================================================================================

# SECTION 1: SETUP - LOAD PACKAGES AND SETTINGS
# -------------------------------------------------------------------------------------------------
# Clear environment
rm(list = ls())

# List of required packages
packages <- c("dplyr", "tidyr", "haven", "VIM", "here")

# Check, install, and load packages automatically
for (pkg in packages) {
  if (!require(pkg, character.only = TRUE)) {
    install.packages(pkg)
    library(pkg, character.only = TRUE)
  }
}

# [CRITICAL] Set seed for reproducibility of KNN imputation
set.seed(12345)


# SECTION 2: DEFINE FILE PATHS AND GLOBAL VARIABLES
# -------------------------------------------------------------------------------------------------
# Define path to raw data. 
# NOTE: Assumes raw data folders are in a subfolder named "Raw_Data".
data_path <- here::here("Raw_Data") 

# Check if directory exists
if (!dir.exists(data_path)) {
  stop("Directory 'Raw_Data' not found. Please create it and add MTF folders.")
}

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
  # Logic: Expects format like "...(ICPSR XXXXX)..."
  folder_num <- gsub(".*\\(([^)]+)\\).*", "\\1", folder_path)
  folder_temp <- paste(unlist(strsplit(folder_num, " ")), collapse = "_")
  
  # Generate the full path to the .rda data file
  # Note: Adjusts for standard ICPSR folder structure (DS0004 usually contains Form 4 data)
  icpsr_code <- strsplit(folder_num, " ")[[1]][2]
  data_file <- file.path(folder_path, folder_temp, "DS0004", paste0(icpsr_code, "-0004-Data.rda"))
  
  if (!file.exists(data_file)) {
    warning(paste("File not found:", data_file, "- Skipping this folder."))
    return(NULL)
  }
  
  # Load the data object
  load(data_file)
  data_obj_name <- paste0("da", icpsr_code, ".0004")
  
  if (exists(data_obj_name)) {
    data <- get(data_obj_name)
  } else {
    warning(paste("Data object", data_obj_name, "not found in", data_file))
    return(NULL)
  }
  
  # Add a 'Survey_Year' column based on folder name
  year <- as.numeric(gsub(".*(20\\d{2}).*", "\\1", folder_path))
  data <- data %>% mutate(Survey_Year = year)
  
  # Select only the required variables
  # Use any_of() instead of all_of() to avoid errors if a variable is missing in one year
  data <- data %>% select(any_of(c(variables, "Survey_Year")))
  
  return(data)
}

# SECTION 4: COMBINE DATA AND RENAME VARIABLES
# -------------------------------------------------------------------------------------------------
# Apply the function to all yearly folders and combine them into one dataset.
data_list <- lapply(folder_list, load_and_preprocess_data, variables = variables_to_keep)

# Remove NULL entries if any files failed to load
data_list <- data_list[!sapply(data_list, is.null)]
combined_data <- bind_rows(data_list)

# Rename cryptic MTF variable codes to intuitive names.
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
    Date_Wk               = V3195
    # ARCHIVE_WT is already named correctly
  )

# Convert labelled Gender variable to numeric for reliable filtering
combined_data <- combined_data %>% mutate(Gender = as.numeric(Gender))


# SECTION 5: SAMPLE CONSTRUCTION & EXCLUSION COUNTS
# -------------------------------------------------------------------------------------------------
# This section documents the step-by-step process of arriving at the final analytical sample.

print("--- Sample Construction ---")

# Step 1: Initial Sample
total_initial_sample <- nrow(combined_data)
print(paste("1. Initial total sample size (2018-2023):", total_initial_sample))

# Step 2: Gender Exclusions
# MTF codes: 1=Male, 2=Female
excluded_gender <- combined_data %>%
  filter(!(Gender %in% c(1, 2))) %>%
  nrow()
print(paste("2. Excluded for not identifying as man or woman:", excluded_gender))

data_gender_filtered <- combined_data %>% filter(Gender %in% c(1, 2))

# Step 3: Missing DV Exclusions
excluded_missing_dv <- data_gender_filtered %>%
  filter(is.na(Desired_Children)) %>%
  nrow()
print(paste("3. Excluded for missing data on Parenthood Desire:", excluded_missing_dv))

# Step 4: Final Sample for Imputation
final_sample <- data_gender_filtered %>%
  filter(!is.na(Desired_Children))
final_sample_count <- nrow(final_sample)
print(paste("4. Final analytical sample size:", final_sample_count))
print("---------------------------")


# SECTION 6: MISSING DATA IMPUTATION (KNN)
# -------------------------------------------------------------------------------------------------
# Use KNN imputation to handle missingness on control variables.
# Note: Imputation is performed ONLY on the final analytical sample to ensure accuracy.

imputation_vars <- c(
  'Weekday_Video_Hours', 'Weekend_Video_Hours', 'Siblings', 
  'Happiness_Level', 'Mother_Education', 'Father_Education', 
  'Gaming_Hours', 'Religion_Importance', 'Poli_Beli', 'Date_Wk'
)

print("Starting KNN Imputation... (This may take a moment)")

# Perform KNN Imputation with k=5
# Note: set.seed() at the top ensures this is reproducible.
imputed_data <- kNN(final_sample, variable = imputation_vars, k = 5)

# Select only original and imputed columns (remove VIM flag columns)
final_sample_imputed <- imputed_data %>% select(names(final_sample))


# SECTION 7: FINAL DATA EXPORT
# -------------------------------------------------------------------------------------------------
# Export to Stata .dta format
# Output filename is generalized for reproduction.

output_filename <- "Social_Media_Fertility_Weight_2018-2023.dta"
output_path <- here::here(output_filename)

write_dta(final_sample_imputed, output_path)

print(paste("Processing Complete. File saved as:", output_filename))

# --- END OF SCRIPT ---