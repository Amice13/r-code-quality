# Code to clean up the data file (.csv mongo database dump)
# for further analysis. 


library(stringr)
library(plyr)

raw_data_file <- "data/Lift_Data_All.csv"
cached_data_file <- "data/Lift_Data_Clean"

# Let's go and clean our raw data!
if(!file.exists(paste0(cached_data_file,".Rda")) | TRUE){
  
  # Read raw data file
  data <- read.csv(raw_data_file)
  
  # Now let's clean the raw data, and save it to a new file
  
  # First remove the duplicate context values 
  keep <- c("q", "x", "t", "A", "dvalue","y","x0","type","UUID")
  data <- data[keep]
  rm(keep)
  
  # Remove all "getadvice" values - superfluous for our analysis
  data <- data[data$type == "setreward" ,] 
  
  # Now we can remove "type" column, not needed anymore
  data$type <- NULL
  
  # Derive decoy scenario and experiment version from q
  data["experiment"] <- lapply(data["q"], str_sub, 1,3)
  
  # Remove all irrelevant experiments
  data <- data[(data$experiment %in% c("120", "130", "900", "160")),] 
  
  # Convert experiment to type factor
  data["experiment"] <- lapply(data["experiment"] , factor)
  
  # More readable experiment labels
  data$experiment <- mapvalues(data$experiment, from=c("900","160","120","130"),
                        to = c("LiFI", "LiFII", "Control", "Random"))
  
  
  # derive decoy scenario from q
  data["scenario"] <- lapply(data["q"], str_sub, start= -1)
  
  # Remove LiF2 coffee scenario
  data <- data[data$scenario!="9",] 
  
  # Convert experiment to type factor
  data["scenario"] <- lapply(data["scenario"] , factor)
  
  # More readable scenario labels
  data$scenario <- mapvalues(data$scenario, from=c("1","2","3","4","5","6","7","8"),
  								      to = c("Laptop", "Wineshop", "Beer", "Pizza", "Hotel", "Economist", "Soda can", "Juice"))
  
  # Now we can remove "q" column, not needed anymore
  data$q <- NULL
  
  # rename cryptic dvalue to "choice" - where D stands for decoy, T for target, and C for competitor
  names(data)[names(data)=="dvalue"] <- "choice"
  data$choice = factor(data$choice)
  
  # Remove all data where is not either 1 or 0 (safeguard - nor really needed here)
  data <- data[data$y==1|data$y==0,]
  
  # Remove all data where no y value (safeguard - not really needed here)
  data <- data[is.finite(data$y) & is.finite(data$y), ]
  
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
