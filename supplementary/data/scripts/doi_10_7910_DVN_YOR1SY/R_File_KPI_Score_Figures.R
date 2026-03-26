######################################################################################################################################
###                                                                                                                                ###
###     Are sustainability-linked loans designed to effectively incentivize corporate sustainability? A framework for review       ###
###                                                                                                                                ###  
###                                                             SEP 2023                                                           ###
###                                                                                                                                ###
###                                                                                                                                ###
######################################################################################################################################
  

# This R file contains code to replicate Figures 1, 2 and 3. Tables 1-7 can be directly replicated using 
# the excel file SLL_Data_raw_scores_ALL_anonymous.xlsx. Hence, there is no R script for these tables. 


# The following contains code for the score results and distribution, which refers to Figures 1-3 in the paper.
# Load libraries
library(readxl)
library(tidyr)
library(dplyr)
library(DescTools)
library(plm)
library(stargazer)
library(sandwich)
library(lmtest)
library(haven)
library(foreign)
library(writexl)
library(AER)
library(ggplot2)
library(lubridate)
library(gridExtra)


#### Assessment of KPI score per dimension ####

# Read the Excel file into R
dropbox_path <- "Path" # Insert your path here
file_path <- file.path(dropbox_path, "SLL_Data_raw_scores_ALL_anonymous.csv")
data <- read_csv(file_path)

# Filter SLLs with KPI data
SLL_Data_KPIs <- subset(data, data$`Information on KPIs?` == "Yes" )


# Calculate the distribution for each of the six score dimensions and then plot it.

score_1_dist <- SLL_Data_KPIs %>%  count(SLL_Data_KPIs$`Dimension 1 Score - Strategic Relevance`)
score_2_dist <- SLL_Data_KPIs %>%  count(SLL_Data_KPIs$`Dimension 2 Score - Materiality`)
score_3_dist <- SLL_Data_KPIs %>%  count(SLL_Data_KPIs$`Dimension 3 - Measurability`)
score_4_dist <- SLL_Data_KPIs %>%  count(SLL_Data_KPIs$`Dimension 4 - Benchmarking ability`)
score_5_dist <- SLL_Data_KPIs %>%  count(SLL_Data_KPIs$`Dimension 5 - Pricing Mechanism`)
score_6_dist <- SLL_Data_KPIs %>%  count(SLL_Data_KPIs$`Dimension 6 - External Review`)

colnames(score_1_dist)[1] <- "dim_1_score"
colnames(score_2_dist)[1] <- "dim_2_score"
colnames(score_3_dist)[1] <- "dim_3_score"
colnames(score_4_dist)[1] <- "dim_4_score"
colnames(score_5_dist)[1] <- "dim_5_score"
colnames(score_6_dist)[1] <- "dim_6_score"

plot1<-ggplot(data=score_1_dist, aes(x=factor(dim_1_score), y=n, group=1)) +
  geom_bar(stat="identity", fill="cyan", colour="#006000")+
  scale_x_discrete(labels=c('0.0', '0.5', '1.0'))+
  xlab("KPI Score")+
  ylab("Number of SLLs")+
  ggtitle("Panel A: Dimension 1: Strategic Relevance")+
  theme(plot.title = element_text(size = 10, face = "bold"))

plot2<-ggplot(data=score_2_dist, aes(x=factor(dim_2_score), y=n, group=1)) +
  geom_bar(stat="identity", fill="cyan", colour="#006000")+
  scale_x_discrete(labels=c('0.0', '0.5', '1.0'))+
  xlab("KPI Score")+
  ylab("Number of SLLs")+
  ggtitle("Panel B: Dimension 2: Materiality")+
  theme(plot.title = element_text(size = 10, face = "bold"))

plot3<-ggplot(data=score_3_dist, aes(x=factor(dim_3_score), y=n, group=1)) +
  geom_bar(stat="identity", fill="cyan", colour="#006000")+
  scale_x_discrete(labels=c('0.5', '1.0'))+
  xlab("KPI Score")+
  ylab("Number of SLLs")+
  ggtitle("Panel C: Dimension 3: Measurability")+
  theme(plot.title = element_text(size = 10, face = "bold"))


plot4<-ggplot(data=score_4_dist, aes(x=factor(dim_4_score), y=n, group=1)) +
  geom_bar(stat="identity", fill="cyan", colour="#006000")+
  scale_x_discrete(labels=c('0.0', '0.5', '1.0'))+
  xlab("KPI Score")+
  ylab("Number of SLLs")+
  ggtitle("Panel D: Dimension 4: Benchmarking")+
  theme(plot.title = element_text(size = 10, face = "bold"))


plot5<-ggplot(data=score_5_dist, aes(x=factor(dim_5_score), y=n, group=1)) +
  geom_bar(stat="identity", fill="cyan", colour="#006000")+
  scale_x_discrete(labels=c('0.0', '0.5', '1.0'))+
  xlab("KPI Score")+
  ylab("Number of SLLs")+
  ggtitle("Panel E: Dimension 5: Pricing Mechanism")+
  theme(plot.title = element_text(size = 10, face = "bold"))


plot6<-ggplot(data=score_6_dist, aes(x=factor(dim_6_score), y=n, group=1)) +
  geom_bar(stat="identity", fill="cyan", colour="#006000")+
  scale_x_discrete(labels=c('0.0', '0.5', '1.0'))+
  xlab("KPI Score")+
  ylab("Number of SLLs")+
  ggtitle("Panel F: Dimension 6: External Review")+
  theme(plot.title = element_text(size = 10, face = "bold"))

# Plot all dimensions
grid.arrange(plot1, plot2,plot3,plot4,plot5,plot6, ncol=2)


#### Number of SLLs by total KPI score ####

# Plot the distribution of SLLs by total KPI score. 

score_overall_dist <- SLL_Data_KPIs %>%  count(SLL_Data_KPIs$`Total Score`)
colnames(score_overall_dist)[1] <- "overall_score"


plot7 <- ggplot(data=score_overall_dist, aes(x=factor(overall_score), y=n, group=1)) +
  geom_bar(stat="identity", fill="cyan", colour="#006000")+
  scale_x_discrete(labels=c('1.0', '1.5', '2.0', '2.5', '3.0', '3.5', '4.0', '4.5', '5.0','5.5','6.0'))+
  xlab("KPI Score")+
  ylab("Number of SLLs")

show(plot7)

# Plot the average KPI score per year and the average KPI score by number of KPIs. 

SLL_Data_KPIs$`Total Score`<-as.numeric(SLL_Data_KPIs$`Total Score`)
SLL_Data_KPIs$year <- year(SLL_Data_KPIs$`Date Announced`)
Graph_KPI_overtime <- aggregate(SLL_Data_KPIs[,65:65], list(SLL_Data_KPIs$year), mean)
colnames(Graph_KPI_overtime)[1] <- "year"
colnames(Graph_KPI_overtime)[2] <- "total_score"


Graph_KPI_overtime$year<-as.character(Graph_KPI_overtime$year)

plot9e<-ggplot(data=Graph_KPI_overtime, aes(x=year, y=total_score, group=1)) +
  geom_bar(stat="identity", fill="cyan", colour="#006000")+
  xlab("Year")+
  ylab("Average KPI Score")+
  ggtitle("Panel A: Average KPI Score per Year")+
  theme(plot.title = element_text(size = 10, face = "bold"))


Graph_KPI_number_of_KPIS <- aggregate(SLL_Data_KPIs[,65:65], list(SLL_Data_KPIs$`Total Number of KPIs`), mean)
colnames(Graph_KPI_number_of_KPIS)[1] <- "number_of_KPIs"
colnames(Graph_KPI_number_of_KPIS)[2] <- "total_score"


plot9f<-ggplot(data=Graph_KPI_number_of_KPIS, aes(x=number_of_KPIs, y=total_score, group=1)) +
  geom_bar(stat="identity", fill="cyan", colour="#006000")+
  xlab("Number of KPIs")+
  ylab("Average KPI Score")+
  ggtitle("Panel B: Average KPI Score per Number of KPIs")+
  theme(plot.title = element_text(size = 10, face = "bold"))


# Plot both average KPI score per year and number of KPIs.
grid.arrange(plot9e, plot9f, ncol=2)

