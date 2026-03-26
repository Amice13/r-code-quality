library(dplyr)
library(tidyr)
library(broom)
library(stargazer)
library(glmnet)
library(ggplot2)
library(haven)
library(VIM)

# ==============================================================================
# SECTION 1: WORKING DIRECTORY AND DATA PATH
# ==============================================================================
# Set path based on the folder containing the R script
# (Path for original user)
setwd("C:/Users/ysh96/OneDrive - Louisiana State University/Working Paper/Social Media & Fertility")
# (For new users): Set this path to your local project folder
# setwd("YOUR/PATH/TO/PROJECT/FOLDER")

# (Path for original user)
folder_list <- list.dirs("Data 2013-2023/2024 Paper", recursive = FALSE)
# (For new users): Set this path to your data sub-folder
# folder_list <- list.dirs("YOUR/PATH/TO/DATA/FOLDER", recursive = FALSE)


# ==============================================================================
# SECTION 2: DATA LOADING FUNCTION
# ==============================================================================
# Define data loading and preprocessing function
load_and_preprocess_data <- function(folder_path, variables) {
  # Extract ICPSR number from the folder path
  folder_num <- gsub(".*\\(([^)]+)\\).*", "\\1", folder_path)
  folder_temp <- paste(unlist(strsplit(folder_num, " ")), collapse = "_")
  
  # Generate the data file path
  data_file <- paste0(folder_path, "/", folder_temp, "/DS0004/", strsplit(folder_num, " ")[[1]][2], "-0004-Data.rda")
  
  # Load the data
  load(data_file)
  data <- get(paste0("da", strsplit(folder_num, " ")[[1]][2], ".0004"))
  
  # Add survey year information
  year <- as.numeric(gsub(".*(20\\d{2}).*", "\\1", folder_path))
  data <- data %>% mutate(Survey_Year = year)
  
  # Select only the required variables
  data <- data %>% select(all_of(variables), Survey_Year)
  
  return(data)
}

# ==============================================================================
# SECTION 3: LOAD AND COMBINE DATA
# ==============================================================================
# List of variables to keep
variables <- c('V3252', 'V3624', 'V3628', 'V3629', 
               'V3150', 'V3151', 'V3153', 'V3170', 'V3152', 
               'V3166',  # Political preference -> Poli_Pre
               'V3167',  # Political belief -> Poli_Beli
               'V3156', 'V3155', 'V49', 'V3208', 'V3617', 
               'V3164', 'V3163', 'V3621', 'V3195', 'ARCHIVE_WT')

# Combine data from all years
data_list <- lapply(folder_list, load_and_preprocess_data, variables = variables)
combined_data <- bind_rows(data_list)

# ==============================================================================
# SECTION 4: RENAME VARIABLES
# ==============================================================================
# Rename variables to be more descriptive
combined_data <- combined_data %>%
  rename(
    Desired_Children        = V3252,
    Social_Media_Hours      = V3624,
    Weekday_Video_Hours     = V3628,
    Weekend_Video_Hours     = V3629,
    Gender                  = V3150,
    Race                    = V3151,
    Parental_Marital_Status = V3153,
    Religion_Importance     = V3170,
    City_Size               = V3152,
    Poli_Pre                = V3166,  # Political preference
    Poli_Beli               = V3167,  # Political belief
    Lives_with_Mother       = V3156,
    Lives_with_Father       = V3155,
    Siblings                = V49,
    Happiness_Level         = V3208,
    Feel_Bullied_Online     = V3617,
    Mother_Education        = V3164,
    Father_Education        = V3163,
    Gaming_Hours            = V3621,
    Date_Wk                 = V3195,
    ARCHIVE_WT              = ARCHIVE_WT
  )

# Convert Gender to numeric (to prevent filtering errors)
combined_data <- combined_data %>% mutate(Gender = as.numeric(Gender))

# ==============================================================================
# SECTION 5: SAMPLE CONSTRUCTION & *** PRE-PANDEMIC FILTERING ***
# ==============================================================================
print("--- Sample Construction (2018-2023 Full Data) ---")
# 1. Check total sample size immediately after data combination
total_initial_sample <- nrow(combined_data)
print(paste("1. Total respondents after initial data combination (2018-2023):", total_initial_sample))

# 2. Check sample size excluded based on gender (non-male/female)
excluded_gender <- combined_data %>%
  filter(!(Gender %in% c(1, 2))) %>%
  nrow()
print(paste("2. Number of respondents excluded (non-male/female) (2018-2023):", excluded_gender))

# First filtering based on gender
data_gender_filtered <- combined_data %>% filter(Gender %in% c(1, 2))

# 3. Check sample size excluded due to missing DV (Parenthood Desire)
excluded_missing_dv <- data_gender_filtered %>%
  filter(is.na(Desired_Children)) %>%
  nrow()
print(paste("3. Number excluded due to missing DV (Parenthood Desire) (2018-2023):", excluded_missing_dv))

# 4. Sample size after DV filtering (2018-2023)
final_sample_all_years <- data_gender_filtered %>%
  filter(!is.na(Desired_Children))
print(paste("4. Sample size after DV filtering (2018-2023):", nrow(final_sample_all_years)))

# --- !!! CRITICAL REVISION: FILTER *BEFORE* IMPUTATION !!! ---
# 5. Filter for the final analytical sample (2018-2020, Pre-Pandemic)
final_sample_pre_pandemic <- final_sample_all_years %>%
  filter(Survey_Year <= 2020)
print("---------------------------------------------------------")
print(paste("5. *** FINAL ANALYTICAL SAMPLE (2018-2020) ***:", nrow(final_sample_pre_pandemic)))
print("---------------------------------------------------------")


# ==============================================================================
# SECTION 6: IMPUTATION (ON 2018-2020 DATA ONLY)
# ==============================================================================
# Define list of variables for imputation (control variables)
# (Excluding DV, weights, Survey_Year, Gender, etc.)
imputation_vars <- c(
  'Social_Media_Hours', 
  'Weekday_Video_Hours', 
  'Weekend_Video_Hours', 
  'Siblings', 
  'Happiness_Level', 
  'Mother_Education', 
  'Father_Education', 
  'Gaming_Hours', 
  'Religion_Importance', 
  'Poli_Pre',
  'Poli_Beli',
  'Date_Wk', 
  'Feel_Bullied_Online',
  'Race',
  'Parental_Marital_Status',
  'City_Size',
  'Lives_with_Mother',
  'Lives_with_Father'
)

# Perform k-Nearest Neighbors (kNN) Imputation (k=5)
# **Note**: Run only on the 2018-2020 data (final_sample_pre_pandemic)
imputed_data_knn <- kNN(final_sample_pre_pandemic, variable = imputation_vars, k = 5)

# The kNN function adds '_imp' flag columns; select only original column names for the final dataset
final_sample_imputed <- imputed_data_knn %>% 
  select(names(final_sample_pre_pandemic))


# ==============================================================================
# SECTION 7: EXPORT FINAL DATA (2018-2020 PROCESSED)
# ==============================================================================
# Export final data to Stata (.dta) format (2018-2020, weighted, imputed)
# **Note**: File name changed
write_dta(final_sample_imputed, "Social_Media_Fertility_Weight_2018-2020_Processed.dta")

print("--- SCRIPT COMPLETE ---")
print("File saved as: Social_Media_Fertility_Weight_2018-2020_Processed.dta")