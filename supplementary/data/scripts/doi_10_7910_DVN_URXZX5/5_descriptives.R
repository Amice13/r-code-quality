# Descriptives

# Import libraries
library(xtable)
library(reporttools)
library(Hmisc)
library(data.table)

# Load our clean data - makes sure each entry has a UUID in both tables

load("data/Lift_And_Demo_Data_All.Rda")
lift_data <- joined_data
rm(joined_data)

# Limit columns to relevant ones, 
# that is "choice" (C,T,D for each scenario) 
# and "survey type" (R1,D2,D3,D3_2)

lift_data <- lift_data[,c(2,31:37)]

# Rename the columns of the "choice" vars

cols <- c("Survey","Laptop","Wine","Beer","Pizza","Hotel","Economist","Soda")
colnames(lift_data) <- cols
rm(cols)

# Describe choices within surveys

print (describe(lift_data[which(lift_data$Survey=="R1"),]))
print (describe(lift_data[which(lift_data$Survey=="D2"),]))
print (describe(lift_data[which(lift_data$Survey=="D3"),]))
print (describe(lift_data[which(lift_data$Survey=="D3_2"),]))

# Create a nice looking graph

tableNominal(vars = lift_data, cap = "Descriptives", lab = "tab: nominal", group=lift_data[, "Survey"], cumsum=FALSE)

