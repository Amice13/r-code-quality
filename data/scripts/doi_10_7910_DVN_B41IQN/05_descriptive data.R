### ------------------------------------------------------------
# Thesis Survey Experiment
# 05 Descriptive data
# Friedrich Püttmann
# October 2023
### ------------------------------------------------------------

## load packages -----------------------------------------------

library(tidyverse)
library(rstatix)

## load dataset ------------------------------------------------

data_exp <- read_rds("02_data/03_data_clean.rds")

## descriptive data on dependent variables ---------------------

# Staying away from Syrians

table(data_exp$try_stayaway) 
table_try_stayaway <- table(data_exp$try_stayaway)        
percentage_table <- prop.table(table_try_stayaway) * 100     
percentage_table

summary(data_exp$try_stayaway)
total_count <- sum(!is.na(data_exp$try_stayaway))
print(total_count)

# Talking to Syrians

table(data_exp$syr_talking) 
table_syr_talking <- table(data_exp$syr_talking)        
percentage_table <- prop.table(table_syr_talking) * 100     
percentage_table

summary(data_exp$syr_talking)
total_count <- sum(!is.na(data_exp$syr_talking))
print(total_count)

# Syrian friend

table(data_exp$syr_friend) 
table_syr_friend <- table(data_exp$syr_friend)        
percentage_table <- prop.table(table_syr_friend) * 100     
percentage_table

summary(data_exp$syr_friend)
total_count <- sum(!is.na(data_exp$syr_friend))
print(total_count)

# Socialise with Syrians

table(data_exp$syr_socialising) 
table_syr_socialising <- table(data_exp$syr_socialising)        
percentage_table <- prop.table(table_syr_socialising) * 100     
percentage_table

summary(data_exp$syr_socialising)
total_count <- sum(!is.na(data_exp$syr_socialising))
print(total_count)

# Accepted in the same country

table(data_exp$Soru901) 
table_Soru901 <- table(data_exp$Soru901)
percentage_table <- prop.table(table_Soru901) * 100      
print(percentage_table)  

summary(data_exp$Soru901) 
total_count <- sum(!is.na(data_exp$Soru901))
print(total_count)

# Accepted in the same city

table(data_exp$Soru902) 
table_Soru902 <- table(data_exp$Soru902)
percentage_table <- prop.table(table_Soru902) * 100      
print(percentage_table)  

summary(data_exp$Soru902) 
total_count <- sum(!is.na(data_exp$Soru902))
print(total_count)


# Accepted in the same neighbourhood, workplace or school

table(data_exp$Soru903) 
table_Soru903 <- table(data_exp$Soru903)
percentage_table <- prop.table(table_Soru903) * 100      
print(percentage_table)  

summary(data_exp$Soru903) 
total_count <- sum(!is.na(data_exp$Soru903))
print(total_count)


# Accepted in the same building, group of friends or as a neighbour

table(data_exp$Soru904) 
table_Soru904 <- table(data_exp$Soru904)
percentage_table <- prop.table(table_Soru904) * 100      
print(percentage_table)  

summary(data_exp$Soru904) 
total_count <- sum(!is.na(data_exp$Soru904))
print(total_count)


# Accepted in the same home or family

table(data_exp$Soru905) 
table_Soru905 <- table(data_exp$Soru905)
percentage_table <- prop.table(table_Soru905) * 100      
print(percentage_table)  

summary(data_exp$Soru905) 
total_count <- sum(!is.na(data_exp$Soru905))
print(total_count)




