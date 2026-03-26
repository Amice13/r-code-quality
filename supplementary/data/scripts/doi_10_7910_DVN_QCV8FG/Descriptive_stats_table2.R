library(tidyverse) 
library(gmodels) #for CrossTable
library(describedata) #for proc_means
library(ggplot2) #for heatmap
library(reshape2)

df_data = read.csv("EtsyCleanMainData.csv",header=TRUE,sep= ",")

df_data$acrylic = as.integer(as.logical(df_data$acrylicMaterial_New))
df_data$oil = as.integer(as.logical(df_data$oilMaterial_New))
df_data$mixedmed = as.integer(as.logical(df_data$mixedmediaMaterial_New))
df_data$canvas = as.integer(as.logical(df_data$canvasMaterial_New))
df_data$framed = as.integer(as.logical(df_data$isFramed_New))
df_data$handmade = as.integer(as.logical(df_data$isHandmade_New))
df_data$northamerica = ifelse(df_data$location_clean=="North America", 1,0)
df_data$europe = ifelse(df_data$location_clean=="Europe", 1,0)

#artist shop metrics summary
#need to dedupe for multiple paintings to get unique number of seller-artists
mydata <- df_data[, c(3,5,21,22,48,49)]
head(mydata)
#extracting unique number of sellers
unique_sellers = unique(mydata)

proc_means(unique_sellers, vars = c("seller_rating", "sales","admirers", "seller_number_of_reviews",
                                    "northamerica", "europe"), n = T,
           mean = TRUE, max = TRUE, min = TRUE, median = TRUE, sd = TRUE, 
           q1 = FALSE, q3 = FALSE, iqr = FALSE, nmiss = FALSE,
           nobs = FALSE, p = FALSE, p_round = 4, display_round = 3)

#painting features summary

mydata2 <- df_data[, c(9,11,13, 39:47)]

head(mydata2)

proc_means(mydata2, vars = c("num_images", "num_words_in_title","painting_description_num_words", "width_clean",
                                    "height_clean", "price_clean", "acrylic", "oil", "mixedmed", "canvas",
                             "framed", "handmade"), n = T,
           mean = TRUE, max = TRUE, min = TRUE, median = TRUE, sd = TRUE, 
           q1 = FALSE, q3 = FALSE, iqr = FALSE, nmiss = FALSE,
           nobs = FALSE, p = FALSE, p_round = 4, display_round = 6)






