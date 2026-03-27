# Code to clean up the demographics survey file (.csv Drupal survey database dump)
# for further analysis. 

library(stringr)
library(plyr)

raw_data_file <- "data/Demographics_Data_All.csv"
cached_data_file <- "data/Demographics_Data_Clean"

# Let's go and clean our raw data!
if(!file.exists(paste0(cached_data_file,".Rda")) | TRUE){
  
  # Read raw data file
  data <- read.csv(raw_data_file)
  
  # First remove the duplicate context values 
  keep <- c("Survey","Where.do.you.currently.reside.", "What.s.your.gender.", "What.s.your.age.", "What.is.the.highest.level.of.education.you.have.received.", "UUID")
  data <- data[keep]
  rm(keep)
  
  names(data)[names(data)=="ï..Survey"] <- "Experiment"
  
  # Save cleaned data as cached_data_file
  save(data,file = paste0(cached_data_file,".Rda"))
  
  # Save a CSV for potential import in other applications
  write.csv(data, file = paste0(cached_data_file,".csv"))
  
  rm(cached_data_file,raw_data_file)
  
} else {
  
  # Load a cached data frame
  load(paste0(cached_data_file,".Rda"))
  rm(cached_data_file,raw_data_file)
  
}
