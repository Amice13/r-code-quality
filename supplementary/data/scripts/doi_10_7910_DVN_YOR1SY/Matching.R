######################################################################################################################################
###                                                                                                                                ###
###     Are sustainability-linked loans designed to effectively incentivize corporate sustainability? A framework for review       ###
###                                                                                                                                ###  
###                                                             SEP 2023                                                           ###
###                                                                                                                                ###
###                                                                                                                                ###
######################################################################################################################################


# Load required libraries
library(MatchIt)
library(readxl)
library(writexl)
library(DescTools)
library(tableone)
library(tibble)

# Set the path to the Dropbox folder
dropbox_path <- "Path"

# Specify the file path within the Dropbox folder
file_path <- file.path(dropbox_path, "file_for_2018_matching.csv")

# Read the Excel file into R
data <- read_csv(file_path)

# Filter out rows with missing or non-finite values in relevant variables
data <- na.omit(data[c("issued_SLL_in_2018", "issued_conventional_loan_in_2018",
                       "prof_2017", "btm_2017", "ln_total_asset_2017","total_debt_total_assets_2017", "msci_score_2017",
                       "industry", "Region", "ID")])

# Winsorize variables
data$prof_2017 <- Winsorize(data$prof_2017, p = c(0.05, 0.95))
data$btm_2017 <- Winsorize(data$btm_2017, p = c(0.05, 0.95))
data$ln_total_asset_2017 <- Winsorize(data$ln_total_asset_2017, p = c(0.05, 0.95))
data$total_debt_total_assets_2017 <- Winsorize(data$total_debt_total_assets_2017, p = c(0.05, 0.95))

# Create a data frame with the relevant variables
data <- data.frame(
  issued_SLL_in_2018 = as.factor(data$issued_SLL_in_2018),
  issued_conventional_loan_in_2018 = as.factor(data$issued_conventional_loan_in_2018),
  btm_2017 = data$btm_2017,
  leverage_2017 = data$total_debt_total_assets_2017,
  prof_2017 = data$prof_2017,
  total_assets_2017 = data$ln_total_asset_2017,
  msci_score_2017 = data$msci_score_2017,
  industry = as.factor(data$industry),
  region = as.factor(data$Region),
  ID = as.character(data$ID)
)

# Perform propensity score matching
match_object <- matchit(
  issued_SLL_in_2018 ~ btm_2017 + leverage_2017 + prof_2017 + total_assets_2017 + industry + region + msci_score_2017,
  data = data,
  method = "nearest"
)

# Extract matched treated and control firms
matched_data_2018 <- match.data(match_object)

controls_2018 <- subset(matched_data_2018, issued_SLL_in_2018 ==0)
treated_2018 <- subset(matched_data_2018, issued_SLL_in_2018 ==1)


# Specify the file path within the Dropbox folder
file_path <- file.path(dropbox_path, "file_for_2019_matching.csv")

# Read the Excel file into R
data <- read_csv(file_path)

# Filter out rows with missing or non-finite values in relevant variables
data <- na.omit(data[c("issued_SLL_in_2019", "issued_conventional_loan_in_2019",
                       "prof_2018", "btm_2018", "ln_total_asset_2018","total_debt_total_assets_2018", "msci_score_2018",
                       "industry", "Region", "ID")])

# Winsorize variables
data$prof_2018 <- Winsorize(data$prof_2018, p = c(0.05, 0.95))
data$btm_2018 <- Winsorize(data$btm_2018, p = c(0.05, 0.95))
data$ln_total_asset_2018 <- Winsorize(data$ln_total_asset_2018, p = c(0.05, 0.95))
data$total_debt_total_assets_2018 <- Winsorize(data$total_debt_total_assets_2018, p = c(0.05, 0.95))

# Create a data frame with the relevant variables
data <- data.frame(
  issued_SLL_in_2019 = as.factor(data$issued_SLL_in_2019),
  issued_conventional_loan_in_2019 = as.factor(data$issued_conventional_loan_in_2019),
  btm_2018 = data$btm_2018,
  leverage_2018 = data$total_debt_total_assets_2018,
  prof_2018 = data$prof_2018,
  total_assets_2018 = data$ln_total_asset_2018,
  msci_score_2018 = data$msci_score_2018,
  industry = as.factor(data$industry),
  region = as.factor(data$Region),
  ID = as.character(data$ID)
)

data<-data[!(data$ID %in% controls_2018$ID),]



# Perform propensity score matching
match_object <- matchit(
  issued_SLL_in_2019 ~ btm_2018 + leverage_2018 + prof_2018 + total_assets_2018 + industry + region + msci_score_2018,
  data = data,
  method = "nearest"
)

# Extract matched treated and control firms
matched_data_2019 <- match.data(match_object)

controls_2019 <- subset(matched_data_2019, issued_SLL_in_2019 ==0)
treated_2019 <- subset(matched_data_2019, issued_SLL_in_2019 ==1)



# Specify the file path within the Dropbox folder
file_path <- file.path(dropbox_path, "file_for_2020_matching.csv")

# Read the Excel file into R
data <- read_csv(file_path)

# Filter out rows with missing or non-finite values in relevant variables
data <- na.omit(data[c("issued_SLL_in_2020", "issued_conventional_loan_in_2020",
                       "prof_2019", "btm_2019", "ln_total_asset_2019","total_debt_total_assets_2019", "msci_score_2019",
                       "industry", "Region", "ID")])

# Winsorize variables
data$prof_2019 <- Winsorize(data$prof_2019, p = c(0.05, 0.95))
data$btm_2019 <- Winsorize(data$btm_2019, p = c(0.05, 0.95))
data$ln_total_asset_2019 <- Winsorize(data$ln_total_asset_2019, p = c(0.05, 0.95))
data$total_debt_total_assets_2019 <- Winsorize(data$total_debt_total_assets_2019, p = c(0.05, 0.95))

# Create a data frame with the relevant variables
data <- data.frame(
  issued_SLL_in_2020 = as.factor(data$issued_SLL_in_2020),
  issued_conventional_loan_in_2020 = as.factor(data$issued_conventional_loan_in_2020),
  btm_2019 = data$btm_2019,
  leverage_2019 = data$total_debt_total_assets_2019,
  prof_2019 = data$prof_2019,
  total_assets_2019 = data$ln_total_asset_2019,
  msci_score_2019 = data$msci_score_2019,
  industry = as.factor(data$industry),
  region = as.factor(data$Region),
  ID = as.character(data$ID)
)

data<-data[!(data$ID %in% controls_2018$ID),]
data<-data[!(data$ID %in% controls_2019$ID),]



# Perform propensity score matching
match_object <- matchit(
  issued_SLL_in_2020 ~ btm_2019 + leverage_2019 + prof_2019 + total_assets_2019 + industry + region + msci_score_2019,
  data = data,
  method = "nearest"
)

# Extract matched treated and control firms
matched_data_2020 <- match.data(match_object)

controls_2020 <- subset(matched_data_2020, issued_SLL_in_2020 ==0)
treated_2020 <- subset(matched_data_2020, issued_SLL_in_2020 ==1)




# Specify the file path within the Dropbox folder
file_path <- file.path(dropbox_path, "file_for_2021_matching.csv")

# Read the Excel file into R
data <- read_csv(file_path)

# Filter out rows with missing or non-finite values in relevant variables
data <- na.omit(data[c("issued_SLL_in_2021", "issued_conventional_loan_in_2021",
                       "prof_2020", "btm_2020", "ln_total_asset_2020","total_debt_total_assets_2020", "msci_score_2020",
                       "industry", "Region", "ID")])

# Winsorize variables
data$prof_2020 <- Winsorize(data$prof_2020, p = c(0.05, 0.95))
data$btm_2020 <- Winsorize(data$btm_2020, p = c(0.05, 0.95))
data$ln_total_asset_2020 <- Winsorize(data$ln_total_asset_2020, p = c(0.05, 0.95))
data$total_debt_total_assets_2020 <- Winsorize(data$total_debt_total_assets_2020, p = c(0.05, 0.95))

# Create a data frame with the relevant variables
data <- data.frame(
  issued_SLL_in_2021 = as.factor(data$issued_SLL_in_2021),
  issued_conventional_loan_in_2021 = as.factor(data$issued_conventional_loan_in_2021),
  btm_2020 = data$btm_2020,
  leverage_2020 = data$total_debt_total_assets_2020,
  prof_2020 = data$prof_2020,
  total_assets_2020 = data$ln_total_asset_2020,
  msci_score_2020 = data$msci_score_2020,
  industry = as.factor(data$industry),
  region = as.factor(data$Region),
  Identifier = as.character(data$ID)
)

data<-data[!(data$ID %in% controls_2018$ID),]
data<-data[!(data$ID %in% controls_2019$ID),]
data<-data[!(data$ID %in% controls_2020$ID),]



# Perform propensity score matching
match_object <- matchit(
  issued_SLL_in_2021 ~ btm_2020 + leverage_2020 + prof_2020 + total_assets_2020 + industry + region + msci_score_2020,
  data = data,
  method = "nearest"
)

# Extract matched treated and control firms
matched_data_2021 <- match.data(match_object)

controls_2021 <- subset(matched_data_2021, issued_SLL_in_2021==0)
treated_2021 <- subset(matched_data_2021, issued_SLL_in_2021 ==1)


# Specify the file path within the Dropbox folder
file_path <- file.path(dropbox_path, "file_for_2022_matching.csv")

# Read the Excel file into R
data <- read_csv(file_path)

# Filter out rows with missing or non-finite values in relevant variables
data <- na.omit(data[c("issued_SLL_in_2022", "issued_conventional_loan_in_2022",
                       "prof_2021", "btm_2021", "ln_total_asset_2021","total_debt_total_assets_2021", "msci_score_2021",
                       "industry", "Region", "ID")])

# Winsorize variables
data$prof_2021 <- Winsorize(data$prof_2021, p = c(0.05, 0.95))
data$btm_2021 <- Winsorize(data$btm_2021, p = c(0.05, 0.95))
data$ln_total_asset_2021 <- Winsorize(data$ln_total_asset_2021, p = c(0.05, 0.95))
data$total_debt_total_assets_2021 <- Winsorize(data$total_debt_total_assets_2021, p = c(0.05, 0.95))

# Create a data frame with the relevant variables
data <- data.frame(
  issued_SLL_in_2022 = as.factor(data$issued_SLL_in_2022),
  issued_conventional_loan_in_2022 = as.factor(data$issued_conventional_loan_in_2022),
  btm_2021 = data$btm_2021,
  leverage_2021 = data$total_debt_total_assets_2021,
  prof_2021 = data$prof_2021,
  total_assets_2021 = data$ln_total_asset_2021,
  msci_score_2021 = data$msci_score_2021,
  industry = as.factor(data$industry),
  region = as.factor(data$Region),
  ID = as.character(data$ID)
)

data<-data[!(data$ID %in% controls_2018$ID),]
data<-data[!(data$ID %in% controls_2019$ID),]
data<-data[!(data$ID %in% controls_2020$ID),]
data<-data[!(data$ID %in% controls_2021$ID),]



# Perform propensity score matching
match_object <- matchit(
  issued_SLL_in_2022 ~ btm_2021 + leverage_2021 + prof_2021 + total_assets_2021 + industry + region + msci_score_2021,
  data = data,
  method = "nearest"
)

# Extract matched treated and control firms
matched_data_2022 <- match.data(match_object)

controls_2022 <- subset(matched_data_2022, issued_SLL_in_2022==0)
treated_2022<- subset(matched_data_2022, issued_SLL_in_2022 ==1)

# create files with matched pairs

controls_2018$match_year<-2018
controls_2019$match_year<-2019
controls_2020$match_year<-2020
controls_2021$match_year<-2021
controls_2022$match_year<-2022

treated_2018$match_year<-2018
treated_2019$match_year<-2019
treated_2020$match_year<-2020
treated_2021$match_year<-2021
treated_2022$match_year<-2022

matched_pairs_2018<-rbind(controls_2018, treated_2018)
matched_pairs_2019<-rbind(controls_2019, treated_2019)
matched_pairs_2020<-rbind(controls_2020, treated_2020)
matched_pairs_2021<-rbind(controls_2021, treated_2021)
matched_pairs_2022<-rbind(controls_2022, treated_2022)

write.csv(matched_pairs_2018, "C:/..path.../matched_pair_2018.csv")
write.csv(matched_pairs_2019, "C:/..path.../matched_pair_2019.csv")
write.csv(matched_pairs_2020, "C:/..path.../matched_pair_2020.csv")
write.csv(matched_pairs_2021, "C:/..path.../matched_pair_2021.csv")
write.csv(matched_pairs_2022, "C:/..path.../matched_pair_2022.csv")

