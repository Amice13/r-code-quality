###########################################################
########## Human Coded Data Analysis ################

# Data In: "human_coded_data.csv"

# Data Out: 
            # Figure A1

#################################################


# Load Packages
library(tidyverse)
library(lubridate)
library(xtable)

# Set options for plotting 
options(scipen=999999)

# Set working directory to replication folder (EGExile_Replication)

# Open code file from "EGExile_Replication/code/human_validation_analysis.R"

# Obtain the full path of the current script in RStudio
script_path <- rstudioapi::getActiveDocumentContext()$path

# If the script path is non-empty, proceed to set the working directory
if (!is.null(script_path)) {
  # Calculate the parent directory of the script's directory
  parent_directory <- dirname(dirname(script_path))
  
  # Set the working directory to the parent directory
  setwd(parent_directory)
} else {
  cat("Script path is not set. Ensure your script is saved and you are running RStudio.")
}
# Check Working Directory
getwd()

# Read in data 
data <- read_csv("data/human_coded_data.csv")


#############
# Figure A1 #
#############
data$count<-1
prop_table<-data%>%
  group_by(classification)%>%
  summarise(proportion=sum(count)/1000)
  

ggplot(prop_table, aes(reorder(x=classification,-proportion), y=proportion))+ 
  geom_bar(position="dodge", stat="identity")+scale_fill_grey()+ylim(0,1)+
  theme_minimal(base_size=18)+
  labs(x = "Human Coding of Tweets",
       y = "Proportion of Tweets")
ggsave("plots/FigureA1.pdf", width = 11, height = 7)



