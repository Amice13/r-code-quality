###########################################################
########## Offline Protest Data Analysis ################

# Data In: "protest_data.csv"

# Data Out: # Table A2 
            # Figure A6

#################################################


# Load Packages
library(tidyverse)
library(lubridate)
library(xtable)

# Set options for plotting 
options(scipen=999999)

# Set working directory to replication folder (EGExile_Replication)

# Open code file from "EGExile_Replication/code/protest_data_analysis.R"

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
data <- read_csv("data/protest_data.csv")

############
# Table A2 #
############

#Identify events in September-November 2019 period 
data$protest_2019<-ifelse(data$date>="2019-09-01" & data$date<="2019-11-01",1,0)
data$count<-1

#Count total protests by governorate in September-November 2019 period 
protest_counts <- data %>%
  group_by(governorate, protest_2019) %>%
  summarise(protest_count = sum(count, na.rm = TRUE), .groups = 'drop') %>%
  complete(governorate, protest_2019, fill = list(protest_count = 0)) %>%
  filter(protest_2019==1)
protest_counts<-protest_counts[order(-protest_counts$protest_count),]
protest_counts<-protest_counts[c("governorate", "protest_count")]

#Export Table
file_path <- "tables/TableA2.tex"
tableA2 <- xtable(protest_counts)
print.xtable(tableA2, type = "latex", file = file_path, include.rownames = FALSE)

#############
# Figure A6 #
#############
protest_counts$protest_2019_governorate<-ifelse(protest_counts$protest_count>0,1,0)
protest_2019_governorate<-subset(protest_counts, protest_2019_governorate==1)
data$protest_2019_governorate<-ifelse(data$governorate %in% protest_2019_governorate$governorate," Protest Governorate","No Protest Governorate")

data %>% 
  group_by(protest_2019_governorate, date) %>% 
  summarize(protest_count = n()) %>%
  ggplot(mapping = aes(x = date, y = protest_count)) +
  geom_bar(stat="identity", width=10)+
  geom_vline(xintercept = mdy("9-1-19")) +
  geom_vline(xintercept = mdy("11-1-19")) +
  annotate("text", x = ymd("2019-09-10"), y = 50, label = "2019 Protest Period", size = 5)+
  facet_wrap(~protest_2019_governorate, ncol=1) +
  labs(x = "Year",
       y = "Political Protests (Count)") +
  theme_minimal(base_size=30)+
  theme(legend.position = "none")
ggsave("plots/FigureA6.pdf", width = 11, height = 7)



