############################## REPLICATION DATA AND FILES #############################################
### KEEPING TABS THROUGH COLLABORATION? SHARING MINISTERIAL RESPONSIBILITY IN COALITION GOVERNMENTS ###
################################# K. JONATHAN KLÜSER ##################################################
#################################### 2022-04-24 #######################################################

renv::restore(prompt = FALSE)

# The correct version of all required packages are installed via RENV from the provided lockfile.
# ATTENTION: The analysis has been carried out with the following versions
#[1] forcats_0.5.1      stringr_1.4.0      dplyr_1.0.8        purrr_0.3.4        readr_2.1.1        tidyr_1.2.0       
#[7] tibble_3.1.6       ggplot2_3.3.5      tidyverse_1.3.1    metR_0.5.0         xtable_1.8-4       vtable_1.3.3      
#[13] kableExtra_1.3.4   interactions_1.1.5 betareg_3.1-4      stargazer_5.2.2    lmtest_0.9-39      zoo_1.8-9         
#[19] sandwich_3.0-1     haven_2.4.3        stringdist_0.9.8

# List of packages
list.of.packages <- c("stringdist","haven","sandwich","lmtest", "stargazer",
                      "betareg","interactions","vtable",
                      "xtable","metR","tidyverse")

# Load packages
ifelse(all(unlist(lapply(list.of.packages, require, character.only = TRUE))),
       paste("All required packages loaded"),
       ("Not all required packages loaded"))

# Default warn level
defaultW <- getOption("warn")

# Some helper functions
str_similarity <- function(org, cab){
  ord <- str_to_lower(org, locale = "dk")
  cab <- str_to_lower(cab, locale = "dk")
  
  cab <- str_replace_all(cab, "(?<=\\w)minister|minister(?=\\s)", "ministeriet")
  
  return(stringdist(org, cab, method = "lv"))
}

inflate <- function(y, n){
  return(n**(-1) * (y * (n - 1) + 0.5))
}

summary_cl <- function(..., type="text", out=NULL, cluster = ~ country,
                       order=NULL, dep.var.labels=NULL, covariate.labels=NULL){
  
  ses <- lapply(list(...), function(x) {
    if (is.null(cluster)) {
      cluster <- x$model$country
    }
    sqrt(diag(vcovCL(x, cluster = cluster)))
  })
  stargazer(..., type=type, out=out, se = ses,
            order = order, dep.var.labels = dep.var.labels, covariate.labels = covariate.labels)
}

get_cabinet_data <- function(data) data %>% group_by(cabinet) %>% distinct(party)

create_complete_data <- function(country, cabinet_data, cap_codes){
  data <- list()
  for (cabinet in unique(cabinet_data$cabinet)) {
    parties <- pull(cabinet_data[cabinet_data$cabinet == cabinet, "party"])
    party_combinations <- t(combn(parties, 2))
    party_combinations <- tibble(country = country,
                                 cabinet = cabinet,
                                 party_x = party_combinations[, 1],
                                 party_y = party_combinations[, 2])
    data <- c(data, list(party_combinations))
  }
  data %>% reduce(bind_rows) %>% expand_grid(national_minor = cap_codes)
}

sum_na <- function(x, y){
  x <- ifelse(is.na(x), 0, x)
  y <- ifelse(is.na(y), 0, y)
  x + y
}

if(.Platform$OS.type == "unix") {
  # DATA PREPARATION
  source("01_data_preparation.R")
  
  # REGRESSION ANALYSIS
  source("02_regression_analysis.R")
  
  # FIGURES
  source("03_figures.R")
  
  # TABLES
  source("04_tables.R")
} else {
  print("You are running this script on a Windows.")
  print("Please use RStudio to execute the following scripts manually. Select the script, highlight everything, and run.")
  print("01_data_preparation.R")
  print("02_regression_analysis.R")
  print("03_figures.R")
  print("04_tables.R")
}
