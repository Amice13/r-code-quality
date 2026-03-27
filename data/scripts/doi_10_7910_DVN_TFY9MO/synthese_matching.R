library(stringr)
library(dplyr)

setwd("C:/Users/mdick/Desktop/synthese_journals")

journals = read.csv("journals_filtered.csv")


journ_match = str_detect(journals[ , 2], 
                        regex("(?:proves\\W+(?:\\w+\\W+){0,10}?probably|probably\\W+(?:\\w+\\W+){0,10}?proves)"))



# Convert to character --> str_replace with digits --> 
# Coerce to numeric vector: 

journ_match_convert = as.character(journ_match) %>% 
  str_replace("TRUE", "1") %>% 
  str_replace("FALSE", "0") %>%
  as.numeric()


# Bind the new vector to the dataframe: 

journ_match_df = cbind(journals, journ_match_convert)


# Isolate the positive values (that is, the documents that match the pattern): 

journ_match_filtered = journ_match_df %>%
  filter(journ_match_convert == 1)


nrow(journ_match_filtered) # get the number of documents. 
# Record nrow in google drive spreadsheet.

journ_match_filtered$journ_match_convert = NULL # Column no longer needed

glimpse(journ_match_filtered)
