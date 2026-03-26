library(tidyverse) 
library(gmodels) #for CrossTable
library(describedata) #for proc_means

df_data = read.csv("newdata_july8.csv",header=TRUE,sep= ",")

attach(df_data)


proc_means(df_data, vars = c("seller_rating", "seller_sales","seller_admirers", "seller_number_of_reviews"), n = T,
           mean = TRUE, max = TRUE, min = TRUE, median = TRUE, sd = TRUE, 
           q1 = FALSE, q3 = FALSE, iqr = FALSE, nmiss = FALSE,
           nobs = FALSE, p = FALSE, p_round = 4, display_round = 3)