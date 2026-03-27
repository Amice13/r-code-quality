###########################################################
########## Real-Time Twitter Data Analysis ################

# Data In: "realtime_twitter_data.csv"
            
# Data Out: Figure 1b, 1c 
          # Figure 5
          # Figure 9 
          # Table A3
      
#################################################


# Load Packages
library(tidyverse)
library(lubridate)
library(xtable)

# Set options for plotting 
options(scipen=999999)

# Set working directory to replication folder (EGExile_Replication)

# Open code file from "EGExile_Replication/code/realtime_twitter_analysis.R"

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
data <- read_csv("data/realtime_twitter_data.csv")


###############
#  Figure 1b  #
###############

#Subset tweets to those produced by Mo Ali's Twitter Account
mo_ali<-subset(data, mo_ali==1)

#Plot status count over time

mo_ali %>% 
  ggplot() + 
  aes(x = date, y = user.statuses_count) + 
  geom_line() + 
  labs(y = "Status Count Over Time", x = "Date")+
  theme_minimal(base_size=26) +
  geom_vline(xintercept = ymd("2019-09-10"), linetype=2, color="grey", size=.6) + 
  annotate("text", x = ymd("2019-09-10"), y = 5000, label = "Twitter \n Created", size = 5)+
  geom_vline(xintercept = ymd("2019-09-15"), linetype=2, color="grey", size=.6) + 
  annotate("text", x = ymd("2019-09-15"), y = 4000, label = "Nat'l Youth \n Conference", size = 5)+
  geom_vline(xintercept = ymd("2019-09-20"), linetype=2, color="grey", size=.6) + 
  annotate("text", x = ymd("2019-09-20"), y = 5000, label = "Protests \n Begin", size = 5)+
  geom_vline(xintercept = ymd("2019-09-27"), linetype=2, color="grey", size=.6) + 
  annotate("text", x = ymd("2019-09-27"), y = 4000, label = "Salvation \n Friday", size = 5)+
  theme_minimal(base_size=30)
ggsave("plots/Figure1b.pdf", width = 11, height = 7)

###############
#  Figure 1c  #
###############

# Plot follower count over time 

mo_ali %>% 
  ggplot() + 
  aes(x = date, y = user.followers_count) + 
  geom_line() + 
  labs(y = "Follower Count Over Time", x = "Date")+
  geom_vline(xintercept = ymd("2019-09-10"), linetype=2, color="grey", size=.6) + 
  annotate("text", x = ymd("2019-09-10"), y = 200000, label = "Twitter \n Created", size = 5)+
  geom_vline(xintercept = ymd("2019-09-15"), linetype=2, color="grey", size=.6) + 
  annotate("text", x = ymd("2019-09-15"), y = 300000, label = "Nat'l Youth \n Conference", size = 5)+
  geom_vline(xintercept = ymd("2019-09-20"), linetype=2, color="grey", size=.6) + 
  annotate("text", x = ymd("2019-09-20"), y = 400000, label = "Protests \n Begin", size = 5)+
  geom_vline(xintercept = ymd("2019-09-27"), linetype=2, color="grey", size=.6) + 
  annotate("text", x = ymd("2019-09-27"), y = 300000, label = "Salvation \n Friday", size = 5)+
  theme_minimal(base_size=30)
ggsave("plots/Figure1c.pdf", width = 11, height = 7)


##############
#  Figure 5  #
##############

# Aggregate tweets by date and location and calculate daily proportion of mobilization tweets

mobilization_loc = data %>% 
  group_by(date, in_egypt) %>%
  summarise(tpd = sum(mobilization)/n())

# Remove NAs from location variable (no self-reported location metadata)
mobilization_loc<-na.omit(mobilization_loc)

# Plot over time
mobilization_loc %>% 
  ggplot() + 
  aes(x = date, y = tpd, group=in_egypt, linetype=in_egypt) + 
  geom_line() + 
  labs(y = "Daily Prop. of Tweets", x = "Date")+
  geom_vline(xintercept = ymd("2019-09-09"), linetype=2, color="grey", size=.6) + 
  annotate("text", x = ymd("2019-09-09"), y = .2, label = "Mo Ali\n Twitter \n Account \nCreated", size = 5)+
  geom_vline(xintercept = ymd("2019-09-03"), linetype=2, color="grey", size=.6) + 
  annotate("text", x = ymd("2019-09-03"), y = .2, label = "First\n Youtube \n Video", size = 5)+
  geom_vline(xintercept = ymd("2019-09-15"), linetype=2, color="grey", size=.6) + 
  annotate("text", x = ymd("2019-09-15"), y = .25, label = "Nat'l Youth \n Conference", size = 5)+
  geom_vline(xintercept = ymd("2019-09-20"), linetype=2, color="grey", size=.6) + 
  annotate("text", x = ymd("2019-09-20"), y = .37, label = "Protests \n Begin", size = 5)+
  geom_vline(xintercept = ymd("2019-09-27"), linetype=2, color="grey", size=.6) + 
  annotate("text", x = ymd("2019-09-27"), y = .32, label = "Largest \n Protests", size = 5)+
  theme_minimal(base_size=26)+
  theme(legend.title = element_blank())
ggsave("plots/Figure5.pdf", width = 11, height = 7)

############
# Figure 9 #
############


#Calculate local references to mobilization by governorate
data$count<-1
mobilization_gov = data %>% 
  group_by(date, local_mobilization_governorate) %>%
  summarise(tpd = sum(count))
mobilization_gov<-na.omit(mobilization_gov)

#Select most frequently mentioned governorates for easier data viz 
top_govs<-as.data.frame(table(data$local_mobilization_governorate))
top_govs<-top_govs[order(-top_govs$Freq),]
top_10_govs<-top_govs[1:10,]
top_govs_df<-subset(mobilization_gov, local_mobilization_governorate %in% top_10_govs$Var1 )

top_govs_df %>% 
  ggplot() + 
  aes(x = date, y = tpd, group=local_mobilization_governorate) + 
  geom_line() + 
  labs(y = "Daily Volume of Tweets \n with Mobilization Hashtags", x = "Date")+
  geom_vline(xintercept = ymd("2019-09-20"), linetype=2, color="darkred", size=.6) + 
  geom_vline(xintercept = ymd("2019-09-27"), linetype=2, color="darkred", size=.6) + 
  theme_minimal(base_size=30)+
  theme(legend.position = "none") +
  facet_wrap(~local_mobilization_governorate, scales="free", dir="v")
ggsave("plots/Figure9.pdf", width = 30, height = 20)


############
# Table A3 #
############

top_govs$local_mobilization_governorate<-top_govs$Var1
top_govs$num_mobilization_tweets<-top_govs$Freq
top_govs<-top_govs[c("local_mobilization_governorate", "num_mobilization_tweets")]
top_govs<-top_govs[order(-top_govs$num_mobilization_tweets),]
file_path <- "tables/TableA3.tex"
tableA3 <- xtable(top_govs)
print.xtable(tableA3, type = "latex", file = file_path, include.rownames = FALSE)




