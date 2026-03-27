#################################################
########## Youtube Data Analysis ################

# Data In: "youtube.csv"
# Data Out: Figure 1a, Figure 2, Figure 6a, 6b, 6c

#################################################


# Load Packages
library(tidyverse)
library(lubridate)

# Set options for plotting 
options(scipen=999999)

# Set working directory to replication folder (EGExile_Replication)

# Open code file from "EGExile_Replication/code/youtube_analysis.R"

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
data<-read_csv("data/youtube_data.csv")

# Add Count Var
data$count<-1

#############
# Figure 1a #
#############

# Plot videos over time 

vpd = data %>% 
  group_by(date) %>%
  summarise(vpd = sum(count))%>%
  filter(date<="2019-11-01")


vpd %>% 
  ggplot() + 
  aes(x = date, y = vpd) + 
  #geom_smooth() + 
  #geom_point()+
  geom_line()+
  labs(y = "Daily Volume of Videos", x = "Date")+
  geom_vline(xintercept = ymd("2019-09-10"), linetype=2, color="grey", linewidth=.6) + 
  annotate("text", x = ymd("2019-09-10"), y = 8, label = "Twitter \n Created", size = 5)+
  geom_vline(xintercept = ymd("2019-09-15"), linetype=2, color="grey", size=.6) + 
  annotate("text", x = ymd("2019-09-15"), y = 7, label = "Nat'l Youth \n Conference", size = 5)+
  geom_vline(xintercept = ymd("2019-09-20"), linetype=2, color="grey", size=.6) + 
  annotate("text", x = ymd("2019-09-20"), y = 6, label = "Protests \n Begin", size = 5)+
  geom_vline(xintercept = ymd("2019-09-27"), linetype=2, color="grey", size=.6) + 
  annotate("text", x = ymd("2019-09-27"), y = 7, label = "Salvation \n Friday", size = 5)+
  theme_minimal(base_size=30)+
  ylim(0,9)
ggsave("plots/Figure1a.pdf", width = 11, height = 7)

############
# Figure 2 #
############

#Plot daily counts by type (coordination vs. opposition) 

vpd_type = data %>% 
  group_by(date, type) %>%
  summarise(vpd = sum(count))%>%
  filter(date<="2019-11-01")


ggplot(vpd_type, aes(y=vpd, x=date, color=type, fill=type)) + 
  geom_bar(stat="identity", position="stack")+
  labs(y = "Daily Volume of Videos", x = "Date")+
  geom_vline(xintercept = ymd("2019-09-10"), linetype=2, color="darkred", size=.6) + 
  annotate("text", x = ymd("2019-09-10"), y = 8, label = "Twitter \n Created", size = 5)+
  geom_vline(xintercept = ymd("2019-09-15"), linetype=2, color="darkred", size=.6) + 
  annotate("text", x = ymd("2019-09-15"), y = 7, label = "Nat'l Youth \n Conference", size = 5)+
  geom_vline(xintercept = ymd("2019-09-20"), linetype=2, color="darkred", size=.6) + 
  annotate("text", x = ymd("2019-09-20"), y = 6, label = "Protests \n Begin", size = 5)+
  geom_vline(xintercept = ymd("2019-09-27"), linetype=2, color="darkred", size=.6) + 
  annotate("text", x = ymd("2019-09-27"), y = 7, label = "Salvation \n Friday", size = 5)+
  theme_minimal(base_size=30)+scale_fill_grey()+scale_color_grey()+
  ylim(0,9)
ggsave("plots/Figure2.pdf", width = 11, height = 7)


############
# Figure 6 #
############

#Plot views over time (Figure 6a)

vpd_views = data %>% 
  group_by(date, type) %>%
  summarise(vpd = sum(as.numeric(viewCount)))%>%
  filter(date<="2019-11-01")

vpd_views %>% 
  ggplot() + 
  aes(x = date, y = vpd) + 
  geom_line()+
  geom_vline(xintercept = ymd("2019-09-10"), linetype=2, color="grey", size=.6) + 
  annotate("text", x = ymd("2019-09-10"), y = 500000, label = "Twitter \n Created", size = 5)+
  geom_vline(xintercept = ymd("2019-09-15"), linetype=2, color="grey", size=.6) + 
  annotate("text", x = ymd("2019-09-15"), y = 450000, label = "Nat'l Youth \n Conference", size = 5)+
  geom_vline(xintercept = ymd("2019-09-20"), linetype=2, color="grey", size=.6) + 
  annotate("text", x = ymd("2019-09-20"), y = 500000, label = "Protests \n Begin", size = 5)+
  geom_vline(xintercept = ymd("2019-09-27"), linetype=2, color="grey", size=.6) + 
  annotate("text", x = ymd("2019-09-27"), y = 450000, label = "Salvation \n Friday", size = 5)+
  theme_minimal(base_size=30)+
  labs(y = "Daily Volume of Video Views", x = "Date")+
  theme_minimal(base_size=30)
ggsave("plots/Figure6a.pdf", width = 11, height = 7)


# Plot likes over time (Figure 6b)

vpd_likes = data %>% 
  group_by(date) %>%
  summarise(vpd = sum(as.numeric(likeCount)))%>%
  filter(date<="2019-11-01")

vpd_likes %>% 
  ggplot() + 
  aes(x = date, y = vpd) + 
  geom_line()+
  geom_vline(xintercept = ymd("2019-09-10"), linetype=2, color="grey", size=.6) + 
  annotate("text", x = ymd("2019-09-10"), y = 12000, label = "Twitter \n Created", size = 5)+
  geom_vline(xintercept = ymd("2019-09-15"), linetype=2, color="grey", size=.6) + 
  annotate("text", x = ymd("2019-09-15"), y = 11000, label = "Nat'l Youth \n Conference", size = 5)+
  geom_vline(xintercept = ymd("2019-09-20"), linetype=2, color="grey", size=.6) + 
  annotate("text", x = ymd("2019-09-20"), y = 12000, label = "Protests \n Begin", size = 5)+
  geom_vline(xintercept = ymd("2019-09-27"), linetype=2, color="grey", size=.6) + 
  annotate("text", x = ymd("2019-09-27"), y = 11000, label = "Salvation \n Friday", size = 5)+
  labs(y = "Daily Volume of Video Likes", x = "Date")+
  theme_minimal(base_size=30)
ggsave("plots/Figure6b.pdf", width = 11, height = 7)

# Plot dislikes over time (Figure 6c)

vpd_dislikes = data %>% 
  group_by(date) %>%
  summarise(vpd = sum(as.numeric(dislikeCount)))%>%
  filter(date<="2019-11-01")

vpd_dislikes %>% 
  ggplot() + 
  aes(x = date, y = vpd) + 
  geom_line()+
  geom_vline(xintercept = ymd("2019-09-10"), linetype=2, color="grey", size=.6) + 
  annotate("text", x = ymd("2019-09-10"), y = 1000, label = "Twitter \n Created", size = 5)+
  geom_vline(xintercept = ymd("2019-09-15"), linetype=2, color="grey", size=.6) + 
  annotate("text", x = ymd("2019-09-15"), y = 900, label = "Nat'l Youth \n Conference", size = 5)+
  geom_vline(xintercept = ymd("2019-09-20"), linetype=2, color="grey", size=.6) + 
  annotate("text", x = ymd("2019-09-20"), y = 1000, label = "Protests \n Begin", size = 5)+
  geom_vline(xintercept = ymd("2019-09-27"), linetype=2, color="grey", size=.6) + 
  annotate("text", x = ymd("2019-09-27"), y = 900, label = "Salvation \n Friday", size = 5)+
  theme_minimal(base_size=30)+
  labs(y = "Daily Volume of Video Disikes", x = "Date")+
  theme_minimal(base_size=30)
ggsave("plots/Figure6c.pdf", width = 11, height = 7)



