###########################################################
########## Twitter Network Analysis ################

# Data In: "twitter_network_data.csv"
            
# Data Out: Figure 4
          # Figure A3
          # Figure A4
      
#################################################

# Load Packages
library(tidyverse)
library(lubridate)
library(xtable)

# Set options for plotting 
options(scipen=999999)

# Set working directory to replication folder (EGExile_Replication)

# Open code file from "EGExile_Replication/code/twitter_network_analysis.R"

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
data <- read_csv("data/twitter_network_data.csv")

# Prepare data for plots 

#Split up data into core and periphery 
core<-subset(data, k_core==18)
periphery<-subset(data, k_core==1)

#Split up data into inside and outside Egypt 
in_egypt<-subset(data, in_egypt=="Inside Egypt")
outside_egypt<-subset(data, in_egypt=="Outside Egypt")

#Calculate proportion of users inside Egypt in the core and in the periphery 
in_egypt_core<-as.data.frame(table(in_egypt$k_core))
in_egypt_core$location<-"Inside Egypt"
in_egypt_core$prop<-in_egypt_core$Freq/sum(in_egypt_core$Freq)

#Calculate the proportion of users outside Egypt in the core and the periphery 
outside_egypt_core<-as.data.frame(table(outside_egypt$k_core))
outside_egypt_core$location<-"Outside Egypt"
outside_egypt_core$prop<-outside_egypt_core$Freq/sum(outside_egypt_core$Freq)

#Combine inside and outside Egypt data to plot 
core_location<-rbind(in_egypt_core, outside_egypt_core)
core_location$k_core<-core_location$Var1



############
#Figure 4a #
############

core_location_sub<-subset(core_location, k_core==18)

ggplot(core_location_sub, aes(fill=location, y=prop, x=k_core)) + 
  geom_bar(position="dodge", stat="identity")+theme_minimal(base_size=30)+
  ylab("Proportion of Users in Each Core")+
  xlab("Core")+scale_color_grey()+scale_fill_grey()
ggsave("plots/Figure4a.pdf", width = 14, height = 7)


############
#Figure 4b #
############

periphery_location_sub<-subset(core_location, k_core==1)

ggplot(periphery_location_sub, aes(fill=location, y=prop, x=k_core)) + 
  geom_bar(position="dodge", stat="identity")+theme_minimal(base_size=30)+
  ylab("Proportion of Users in Each Core")+
  xlab("Periphery")+scale_color_grey()+scale_fill_grey()
ggsave("plots/Figure4b.pdf", width = 14, height = 7)

############
#Figure 4c #
############

#Total Reach of Inside vs Outside Egypt Accounts in Core vs Periphery 
core<-subset(data, k_core==18)
core_followers<-core%>%
  group_by(in_egypt)%>%
  summarize(total_followers=sum(actor.followersCount))
core_followers$k_core<-"Core"

periphery<-subset(data, k_core==1)
periphery_followers<-periphery%>%
  group_by(in_egypt)%>%
  summarize(total_followers=sum(actor.followersCount))
periphery_followers$k_core<-"Periphery"

total_followers<-rbind(core_followers, periphery_followers)
total_followers<-na.omit(total_followers)
total_followers$location<-total_followers$in_egypt

ggplot(total_followers, aes(y=total_followers, x=k_core, fill=location)) + 
  geom_bar(position="dodge", stat="identity")+theme_minimal(base_size=30)+
  ylab("Total Followers")+
  xlab("K Core")+scale_color_grey()+scale_fill_grey()
ggsave("plots/Figure4c.pdf", width = 14, height = 7)



###############
#  Figure A3 #
###############

ggplot(core_location, aes(fill=location, y=prop, x=k_core)) + 
  geom_bar(position="dodge", stat="identity")+theme_minimal(base_size=26)+
  ylab("Proportion of Users in Each Core")+
  xlab("K Core")+scale_color_grey()+scale_fill_grey()
ggsave("plots/FigureA3.pdf", width = 14, height = 7)




###############
#  Figure A4  #
###############

#Number of Users in Core
core_only<-subset(core_location, k_core==18)
#Number of Users in Periphery 
periphery_only<-subset(core_location, k_core==1)

periphery_only$type<-"Periphery"
core_only$type<-"Core"
core_periphery_count<-rbind(core_only, periphery_only)

ggplot(core_periphery_count, aes(y=Freq, x=location, fill=location)) + 
  geom_bar(position="dodge", stat="identity") +
  theme_minimal(base_size=30) +
  ylab("Number of Users") +
  xlab("K Core") +
  facet_wrap(~ type, scales = "free_y")+
  theme(legend.position="none")+scale_color_grey()+scale_fill_grey()
ggsave("plots/FigureA4.pdf", width = 14, height = 7)






