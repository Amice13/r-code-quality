
#Security Studies Manuscript
#Title: Full-Spectrum Propaganda in the Social Media Era
#Renee DiResta and Josh A. Goldstein

#Clear environment

rm(list = ls())

#Load required libraries

library(ggplot2)
library(tidyverse)
library(stringr)
library(lubridate)

#Load China Twitter Dataset

  data<-read.csv() #Add File Path to China_Twitter_Hashed.csv

#1 - What are the most common hashtags in the dataset? (Appendix Table 1)

    #Create function to extract hashtags 
    extract_hashtags <- function(hashtag_str) {
     
      #Skip tweet rows that don't have any hashtags
      if (hashtag_str == "[]") {
        return(character(0))
      }
      
      #Remove the brackets and extract content between single quotes 
      clean_str <- str_remove_all(hashtag_str, "^\\[|\\]$")
      tags <- str_extract_all(clean_str, "'[^']*'")[[1]]
      
      #Remove the single quotes 
      tags <- str_remove_all(tags, "^'|'$")
      
      return(tags)
      }
    
    #Apply function 
    all_hashtags <- unlist(lapply(data$hashtags, extract_hashtags))

    #Count frequency of each hashtag
    hashtag_counts <- table(all_hashtags)
    
    #Convert to a dataframe, rename columns 
    hashtag_df <- as.data.frame(hashtag_counts)
    colnames(hashtag_df) <- c("Hashtag", "Count")
    
    #Sort by count in descending order
    top_hashtags <- hashtag_df %>%
      arrange(desc(Count))
    
    #View the top 10 (Appendix Table 1)
    print(head(top_hashtags, 10))

#2. How many hashtags in the dataset are "#Xinjiang" (uppercase, lowercase, or in Chinese). 
    
    #Convert all hashtags to lowercase
    all_hashtags_lower <- tolower(all_hashtags)
    
    #List the variations of 'Xinjiang' to count
    xinjiang_variants <- c("xinjiang", "新疆")
    
    #Filter and count occurrences
    xinjiang_count <- sum(all_hashtags_lower %in% xinjiang_variants)
    
    #View the count
    print(paste("Total count for 'Xinjiang' related hashtags:", xinjiang_count))

#3 Plot tweets over time that include the hashtag Xinjiang (uppercase, lowercase, or in Chinese).
    
    #Format tweet time
    data$tweet_time <- mdy_hm(data$tweet_time)
    
    #Convert all to lowercase
    data$hashtags_lower <- lapply(data$hashtags, function(x) tolower(extract_hashtags(x)))
    
    #List variations of 'Xinjiang' of interest
    xinjiang_variants <- c("xinjiang", "新疆")
    
    #Create dataset of only rows with one of the hashtags of interest
    data_xinjiang <- data %>%
      filter(sapply(hashtags_lower, function(tags) any(tags %in% xinjiang_variants)))
    
    #Aggregate tweet counts by date
    data_xinjiang_daily <- data_xinjiang %>%
      mutate(date = as.Date(tweet_time)) %>%
      group_by(date) %>%
      summarise(count = n())

    #Create figure of tweets over time that use xinjiang hashtag (uppercase, lowercase, or in Chinese)
    ggplot(data_xinjiang_daily, aes(x = date, y = count)) +
      geom_col() +
      scale_x_date(date_breaks = "2 months", date_labels = "%b %Y") +  
      labs(title = "Tweets Over Time with Xinjiang Hashtag",
           x = "Date",
           y = "Number of Tweets") +
      theme_minimal() +
      scale_y_continuous(limits = c(0, 1025)) +
      annotate("text", 
               x = as.Date("2021-01-19"), 
               y = 960, 
               label = "Pompeo \n Statement", 
               vjust = -.5, 
               size = 3)
    
#4 Accounts removed by Twitter retweeted the title of the article:
#"Xinjiang's counter-terrorism measure protect human rights" over 1,000 times between January 19 and January 22, 2021.

    CT_post_tweets <- data %>%
      filter(str_detect(tweet_text, fixed("counter-terrorism measures protect human rights", ignore_case = TRUE))) %>%
      filter(tweet_time >= mdy("01-19-2021") & tweet_time <= mdy("01-22-2021"))  
    
    #Count the number of matching tweets
    CT_post_tweets_count <- nrow(CT_post_tweets) 

    #View the count
    print(paste("Total count for 'counter-terrorism measures protect human rights':", CT_post_tweets_count))
    
