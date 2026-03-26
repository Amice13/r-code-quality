#################################################
########## Facebook Data Analysis ################

# Data In: "facebook_data.csv"
# Data Out: 
          # Figure 7a
          # Figure 7b
          # Figure A5

#################################################


# Load Packages
library(tidyverse)
library(lubridate)

# Set options for plotting 
options(scipen=999999)

# Set working directory to replication folder (EGExile_Replication)

# Open code file from "EGExile_Replication/code/facebook_analysis.R"

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
data<-read_csv("data/facebook_data.csv")

#############
# Figure A5 #
#############

# Plot mobilization posts over time (Inside vs Outside Egypt)

data %>% 
  ggplot() + 
  aes(x = date, y = posts_per_day, linetype=in_egypt, group=in_egypt) + 
  geom_line(color="black") + 
  labs(y = "Daily Volume of Facebook Posts", x = "Date")+
  geom_vline(xintercept = ymd("2019-09-09"), linetype=2, color="grey", size=.6) + 
  annotate("text", x = ymd("2019-09-09"), y = 800, label = "Twitter \nCreated", size = 5)+
  geom_vline(xintercept = ymd("2019-09-03"), linetype=2, color="grey", size=.6) + 
  annotate("text", x = ymd("2019-09-03"), y = 1000, label = "First\n Youtube \n Video", size = 5)+
  geom_vline(xintercept = ymd("2019-09-15"), linetype=2, color="grey", size=.6) + 
  annotate("text", x = ymd("2019-09-15"), y = 1100, label = "Nat'l Youth \n Conference", size = 5)+
  geom_vline(xintercept = ymd("2019-09-20"), linetype=2, color="grey", size=.6) + 
  annotate("text", x = ymd("2019-09-20"), y = 800, label = "Protests \n Begin", size = 5)+
  geom_vline(xintercept = ymd("2019-09-27"), linetype=2, color="grey", size=.6) + 
  annotate("text", x = ymd("2019-09-27"), y = 1200, label = "Salvation Friday", size = 5)+
  theme_minimal(base_size=26)+
  theme(legend.title = element_blank())
ggsave("plots/FigureA5.pdf", width = 13, height = 7)

#############
# Figure 7a #
#############

#Plot daily post engagement (Inside vs Outside Egypt)

data %>% 
  ggplot() + 
  aes(x = date, y = engagement_per_day, group=in_egypt, linetype=in_egypt) + 
  geom_line(color="black") + 
  labs(y = "Daily Volume of Engagement \n with Facebook Posts", x = "Date")+
  geom_vline(xintercept = ymd("2019-09-09"), linetype=2, color="grey", size=.6) + 
  annotate("text", x = ymd("2019-09-09"), y = 1100000, label = "Twitter \nCreated", size = 5)+
  geom_vline(xintercept = ymd("2019-09-03"), linetype=2, color="grey", size=.6) + 
  annotate("text", x = ymd("2019-09-03"), y = 1000000, label = "First\n Youtube \n Video", size = 5)+
  geom_vline(xintercept = ymd("2019-09-15"), linetype=2, color="grey", size=.6) + 
  annotate("text", x = ymd("2019-09-15"), y = 1200000, label = "Nat'l Youth \n Conference", size = 5)+
  geom_vline(xintercept = ymd("2019-09-20"), linetype=2, color="grey", size=.6) + 
  annotate("text", x = ymd("2019-09-20"), y = 1000000, label = "Protests \n Begin", size = 5)+
  geom_vline(xintercept = ymd("2019-09-27"), linetype=2, color="grey", size=.6) + 
  annotate("text", x = ymd("2019-09-27"), y = 1200000, label = "Salvation Friday", size = 5)+
  theme_minimal(base_size=26)+
  theme(legend.title = element_blank()) 
ggsave("plots/Figure7a.pdf", width = 13, height = 7)


#############
# Figure 7b #
#############

#Plot video views (Inside vs Outside Egypt)

data %>% 
  ggplot() + 
  aes(x = date, y = views_per_day, group=in_egypt, linetype=in_egypt) + 
  geom_line(color="black") + 
  labs(y = "Daily Facebook Video Views ", x = "Date")+
  geom_vline(xintercept = ymd("2019-09-09"), linetype=2, color="grey", size=.6) + 
  annotate("text", x = ymd("2019-09-09"), y = 200000000, label = "Twitter \nCreated", size = 5)+
  geom_vline(xintercept = ymd("2019-09-03"), linetype=2, color="grey", size=.6) + 
  annotate("text", x = ymd("2019-09-03"), y = 180000000, label = "First\n Youtube \n Video", size = 5)+
  geom_vline(xintercept = ymd("2019-09-15"), linetype=2, color="grey", size=.6) + 
  annotate("text", x = ymd("2019-09-15"), y = 160000000, label = "Nat'l Youth \n Conference", size = 5)+
  geom_vline(xintercept = ymd("2019-09-20"), linetype=2, color="grey", size=.6) + 
  annotate("text", x = ymd("2019-09-20"), y = 220000000, label = "Protests \n Begin", size = 5)+
  geom_vline(xintercept = ymd("2019-09-27"), linetype=2, color="grey", size=.6) + 
  annotate("text", x = ymd("2019-09-27"), y = 200000000, label = "Salvation Friday", size = 5)+
  theme_minimal(base_size=26)+
  theme(legend.title = element_blank()) 
ggsave("plots/Figure7b.pdf", width = 13, height = 7)


