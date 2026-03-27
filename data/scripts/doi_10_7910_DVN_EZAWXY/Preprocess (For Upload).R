#############################################################
## Preprocessing Steps for Fox/MSNBC News Text Data Analysis
## (English Version)
## Language: R
#############################################################

library(data.table)
library(plyr)
library(quanteda)
library(quanteda.textstats)
library(quanteda.textplots)
library(ggplot2)
library(stm)
library(arrow)
library(stringr)
library(purrr)
library(doParallel)
library(foreach)
library(doSNOW)

# Load preprocessed Python feather data
result <- as.data.frame(arrow::read_feather("result_Stanza_ReadyToR.feather"))

# Remove duplicated contents
result <- result[!duplicated(result$Content), ]

# Text trimming and normalization function
trim_text <- function(text){
  text_temp <- str_remove_all(text, "\\b(the_|this_|these_|that_|which_|a_)")
  text_temp <- gsub("(u\\.s\\.a\\.\\.|u\\.s\\.a\\.|u\\.s\\.a|unite_state_of_america|united_states|united_state|unite_states|unite_state)", "u.s.", text_temp)
  text_temp <- gsub("('|#)", "", text_temp)
  text_temp <- gsub("-", "_", text_temp)
  return(text_temp)
}

# Apply trimming function
result$Content_clean <- sapply(result$Content_clean, trim_text)

# Add new variables
result$Year <- as.numeric(result$Year)
result$Year_Press <- paste0(result$Year, "_", result$Press)

# Subset documents by keyword frequency if needed (set threshold)
thresholds <- 5
result_key <- result

# Save intermediate data
save(result_key, thresholds, file="abortion_preprocessed_data.RData")

#############################
## Synonym replacement, name disambiguation, and stopword removal
#############################

# Load synonym dictionary
Sameword_list <- readLines("sameword_foxmsnbc.txt", encoding="UTF-8")
Sameword_list <- Sameword_list[nchar(Sameword_list) > 0]

syns <- lapply(Sameword_list, function(line){
  terms <- unlist(strsplit(line, " "))
  list(term = paste(terms[-1], collapse="|"), syns = terms[1])
})
regex_batch_sameword <- setNames(sapply(syns, `[[`, "term"), paste0("\\b", sapply(syns, `[[`, "syns"), "\\b"))

Sameword_fun <- function(input){
  word <- gsub("\\([^,]+,[0-9]+\\)", "", input)
  info <- str_extract(input, "\\([^,]+,[0-9]+\\)")
  replaced <- str_replace_all(word, regex_batch_sameword)
  paste0(replaced, info)
}

# Parallel apply synonym replacement
numCores <- detectCores() - 2
cl <- makeCluster(numCores)
registerDoParallel(cl)

result_key$Content_clean <- foreach(i = 1:nrow(result_key), .combine=c, .packages=c("stringr")) %dopar% {
  words <- strsplit(result_key$Content_clean[i], " ")[[1]]
  paste(sapply(words, Sameword_fun), collapse=" ")
}
stopCluster(cl)

# Load name disambiguation list and apply similarly
Samename_list <- readLines("samename_foxmsnbc.txt", encoding="UTF-8")
Samename_list <- Samename_list[nchar(Samename_list) > 0]

syns_name <- lapply(Samename_list, function(line){
  terms <- unlist(strsplit(line, " "))
  list(term = paste(terms[-1], collapse="|"), syns = terms[1])
})
regex_batch_samename <- setNames(sapply(syns_name, `[[`, "term"), paste0("\\b", sapply(syns_name, `[[`, "syns"), "\\b"))

Samename_fun <- function(input){
  if (!grepl("\\(PERSON,", input)) return(input)
  word <- gsub("\\([^,]+,[0-9]+\\)", "", input)
  info <- str_extract(input, "\\([^,]+,[0-9]+\\)")
  replaced <- str_replace_all(word, regex_batch_samename)
  paste0(replaced, info)
}

# Apply parallel name disambiguation (setup cluster again)
cl <- makeCluster(numCores)
registerDoParallel(cl)

result_key$Content_clean <- foreach(i = 1:nrow(result_key), .combine=c, .packages=c("stringr")) %dopar% {
  words <- strsplit(result_key$Content_clean[i], " ")[[1]]
  paste(sapply(words, Samename_fun), collapse=" ")
}
stopCluster(cl)

# Load stopword lists and names to save
Savename_list <- readLines("savename_foxmsnbc.txt", encoding="UTF-8")
stopwords_f <- c(readLines("stopwords_foxmsnbc.txt"), readLines("stopwords_2.txt"))

StopwordNPerson_fun <- function(input){
  word <- gsub("\\([^,]+,[0-9]+\\)", "", input)
  if (grepl("\\(PERSON,", input)){
    if (word %in% Savename_list) return(input) else return("")
  } else {
    if (word %in% stopwords_f) return("") else return(input)
  }
}

# Parallel apply stopword removal
cl <- makeCluster(numCores)
registerDoParallel(cl)

result_key$Content_clean <- foreach(i = 1:nrow(result_key), .combine=c, .packages=c("stringr")) %dopar% {
  words <- strsplit(result_key$Content_clean[i], " ")[[1]]
  cleaned <- sapply(words, StopwordNPerson_fun)
  paste(cleaned[cleaned != ""], collapse=" ")
}
stopCluster(cl)

# Save cleaned data
save(result_key, thresholds, file="Abortion_Corpus_Preprocessed.RData")