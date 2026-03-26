#######################################################
########## Google Trends Data Analysis ################

# Data In: "gtrends_panel.csv"
         # "gtrends_egypt_map_city.csv"
         
            
# Data Out: Figure 8
          # Table A1
      

#################################################


# Load Packages
library(tidyverse)
library(lubridate)
library(xtable)

# Set options for plotting 
options(scipen=999999)

# Set working directory to replication folder (EGExile_Replication)

# Open code file from "EGExile_Replication/code/gtrends_analysis.R"

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
panel_data <- read_csv("data/gtrends_panel.csv")
egypt_map_city <-read_csv("data/gtrends_egypt_map_city.csv")

###############
#  Figure 8  #
###############

# Plot relative search volume (inside Egypt)

panel_data %>% 
  ggplot() + 
  geom_line(aes(x = date, y = egypt_search_interest)) + 
  labs(y = "Daily Relative Search Interest", x = "Date")+
  geom_vline(xintercept = ymd("2019-09-10"), linetype=2, color="grey", linewidth=.6) + 
  annotate("text", x = ymd("2019-09-10"), y = 100, label = "Twitter \n Created", size = 5)+
  geom_vline(xintercept = ymd("2019-09-15"), linetype=2, color="grey", size=.6) + 
  annotate("text", x = ymd("2019-09-15"), y = 80, label = "Nat'l Youth \n Conference", size = 5)+
  geom_vline(xintercept = ymd("2019-09-20"), linetype=2, color="grey", size=.6) + 
  annotate("text", x = ymd("2019-09-20"), y = 100, label = "Protests \n Begin", size = 5)+
  geom_vline(xintercept = ymd("2019-09-27"), linetype=2, color="grey", size=.6) + 
  annotate("text", x = ymd("2019-09-27"), y = 80, label = "Salvation \n Friday", size = 5)+
  theme_minimal(base_size=30)
ggsave("plots/Figure8.pdf", width = 11, height = 7)

##############
# Table A1 ##
##############
file_path <- "tables/TableA1.tex"
table <- xtable(egypt_map_city)
print.xtable(table, type = "latex", file = file_path, include.rownames=FALSE)





