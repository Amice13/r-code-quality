##CREDITING INVISIBLE WORK: CONGRESS AND THE LAWMAKING PRODUCTIVITY METRIC (LAWPROM)- EATOUGH & PREECE
  #TO BE USED IN CONJUNCTION WITH: LawProM.do

#CONTENTS:
#1. Processing Legislative Action Data
#2. Processing Legislative Text Data
#3. Processing Amendment Data
#4. Tokenization of Legislative Text Data & Text Corpus Creation
#5. Alignment Measurement Part One - Jaccard Similarity
#6. Alignment Measurement Part Two - Smith-Waterman


#1. Processing Legislative Action Data
  ##Import Bill Status for All Bills from JSON & Export to CSV
  library(jsonlite)
  library(data.table)
  library(rowr)
  billstatus_json_files<-list.files(path ="filepath",pattern="*.json",full.names = TRUE, recursive = TRUE)
  json_data_final <- data.frame(matrix(, nrow=26, ncol=1))
  for (i in billstatus_json_files)
  {
    json_data <- fromJSON(i)
    json_df <- as.data.frame(as.matrix(json_data)) 
    json_df$V1 <- sapply(json_df$V1, paste, collapse=",")
    json_data_final <- Map(cbind, json_data_final, json_df, MoreArgs = list(fill=NA))
  }
  json_data_finaldata <- as.data.frame(json_data_final)
  transposed <- t(json_data_finaldata)
  json_data_finaldata<- as.data.frame(transposed)
  fwrite (json_data_finaldata, "filepath.csv", row.names=TRUE) 

#2. Processing Legislative Text Data
  ##Import Bill Text from HTML & Save In Directory as CSV
  library(htm2txt)
  library(data.table)
  bill_text_html_files<-list.files(path ="filepath",pattern="*.html",full.names = TRUE, recursive = TRUE)
  html_data_final <- data.frame(matrix(, ncol=1))
  for (i in bill_text_html_files) 
  {
    rawHTML <- paste(readLines(i), collapse="\n")
    html_data <- htm2txt(rawHTML)
    html_data_final <- rbind(html_data_final, html_data)
  }
  html_data_final <- subset(html_data_final, html_data_final$matrix...ncol...1.!="NA")
  html_data_final$file <- bill_text_html_files
  colnames(html_data_final) <- c("bill_text", "file")
  fwrite (html_data_final, "filepath.csv", row.names=TRUE) 
  
#3. Processing Amendment Data
  ##Import Bill Status for All Amendments from JSON & Export to CSV
  library(jsonlite)
  library(data.table)
  library(rowr)
  amendment_json_files<-list.files(path ="filepath",pattern="*.json",full.names = TRUE, recursive = TRUE)
  json_data_final <- data.frame(matrix(, nrow=17, ncol=1))
  for (i in amendment_json_files)
  {
    json_data <- fromJSON(i)
    json_df <- as.data.frame(as.matrix(json_data)) 
    json_df$V1 <- sapply(json_df$V1, paste, collapse=",")
    json_data_final <- Map(cbind, json_data_final, json_df, MoreArgs = list(fill=NA))
  }
  json_data_finaldata <- as.data.frame(json_data_final)
  transposed <- t(json_data_finaldata)
  json_data_finaldata<- as.data.frame(transposed)
  fwrite (json_data_finaldata, "filepath.csv", row.names=TRUE)
  
  
# 4. Tokenization of Legislative Text Data & Text Corpus Creation
  library(readr)
  library(tidyverse)
  library(tokenizers)
  library(tidyr)
  library(data.table)
  ##TOKENIZATION
  #Import Bill Text File
  bill_text <- read.csv("filepath/Bill Text.csv")
  #This is a File Created in Stata Which Reformatted the Output of the Previous
  #Clean Data 
  bill_text$Text <- NULL
  bill_text$Text_CR <- NULL
  bill_text$Text_JR <- NULL
  bill_text$Text_R <- NULL
  #Tokenize By Bill Section
  bill_text$bill_text <- as.character(bill_text$bill_text)
  bill_text$bill_textsections <- tokenize_regex(bill_text$bill_text, pattern =c("SEC. "), simplify = FALSE)
  bill_text$sections <- lengths(bill_text$bill_textsections)
  bill_text<- unnest(bill_text, bill_textsections)
  bill_text$bill_text <- NULL
  #Remove Short Title & Table of Contents In Multi-Section Bills Only
  singlesection <- subset(bill_text, sections==1)
  multisection <- subset(bill_text, sections>1)
  multisection <- multisection[!grepl("SHORT TITLE", multisection$bill_textsections),]
  multisection <- multisection[!grepl("TABLE OF CONTENTS", multisection$bill_textsections),]
  bill_text <- rbind(singlesection,multisection)
  #Number Bill sections
  #Temporary Label Creation
  bill_text$label <- paste(bill_text$congress, bill_text$bill_type, bill_text$bill_version, bill_text$number, sep="_")
  bill_text$section_number <- ave(bill_text$bill_textsections, bill_text$label, FUN = seq_along)
  #Temporary Label Removal
  bill_text$label <- NULL
  #Add Measures of Section Size
  bill_text$numberofwords <- count_words(bill_text$bill_textsections)
  bill_text$numberofsentences <- count_sentences(bill_text$bill_textsections)
  #Drop sections too Small For Comparison
  bill_text <-  subset(bill_text, numberofwords>=30)
  #Add Label & Clean Text
  #Label
  bill_text$label <- paste(bill_text$congress, bill_text$bill_type, bill_text$bill_version, bill_text$number, bill_text$section_number, sep="_")
  #Clean Text
  library(tm)
  bill_text$bill_textsections <- tolower(bill_text$bill_textsections)
  
  bill_text$bill_textsections <- removePunctuation(bill_text$bill_textsections, 
                                                   preserve_intra_word_contractions = TRUE,
                                                   preserve_intra_word_dashes = TRUE)
  bill_text$bill_textsections <- stripWhitespace(bill_text$bill_textsections)
  #Remove Incorrect Quotation Characters
  bill_text$bill_textsections <- gsub("???????~a", "", bill_text$bill_textsections)
  bill_text$bill_textsections <- gsub("???????T", "", bill_text$bill_textsections)
  #Simplify & Save .CSV
  rm(multisection)
  rm(singlesection)
  fwrite (bill_text, "filepath/bill_text_sections.csv", row.names=FALSE) 
  #Save Separate .CSV by Bill Type
  bill_text_hconres <- subset(bill_text, bill_text$bill_type=="hconres")
  fwrite (bill_text_hconres, "filepath/bill_text_hconres.csv", row.names=FALSE)
  bill_text_hjres <- subset(bill_text, bill_text$bill_type=="hjres")
  fwrite (bill_text_hjres, "filepath/bill_text_hjres.csv", row.names=FALSE)
  bill_text_hr <- subset(bill_text, bill_text$bill_type=="hr")
  fwrite (bill_text_hr, "filepath/bill_text_hr.csv", row.names=FALSE)
  bill_text_hres <- subset(bill_text, bill_text$bill_type=="hres")
  fwrite (bill_text_hres, "filepath/bill_text_hres.csv", row.names=FALSE)
  bill_text_s <- subset(bill_text, bill_text$bill_type=="s")
  fwrite (bill_text_s, "filepath/bill_text_s.csv", row.names=FALSE)
  bill_text_sconres <- subset(bill_text, bill_text$bill_type=="sconres")
  fwrite (bill_text_sconres, "filepath/bill_text_sconres.csv", row.names=FALSE)
  bill_text_sjres <- subset(bill_text, bill_text$bill_type=="sjres")
  fwrite (bill_text_sjres, "filepath/bill_text_sjres.csv", row.names=FALSE)
  bill_text_sres <- subset(bill_text, bill_text$bill_type=="sres")
  fwrite (bill_text_sres, "filepath/bill_text_sres.csv", row.names=FALSE)
  
  ##TEXT CORPUS CREATION
  #Done By Bill Type & Congress for Processing Purposes - Could Be Simplified
  ####HCONRES####
  bill_text <- read.csv("filepath/bill_text_hconres.csv")
  #Create Congressional Text Corpus
  #Remove Other Variables
  bill_text$bill_type <- NULL
  bill_text$number <- NULL
  bill_text$bill_version <- NULL
  bill_text$section_number <- NULL
  bill_text$numberofwords <- NULL
  bill_text$numberofsentences<- NULL
  bill_text$sections <- NULL
  bill_text$bill_id <-NULL
  rownames(bill_text) <- NULL
  #Separate By Congress
  bill_text_101 <- subset(bill_text, bill_text$congress==101)
  bill_text_102 <- subset(bill_text, bill_text$congress==102)
  bill_text_103 <- subset(bill_text, bill_text$congress==103)
  bill_text_104 <- subset(bill_text, bill_text$congress==104)
  bill_text_105 <- subset(bill_text, bill_text$congress==105)
  bill_text_106 <- subset(bill_text, bill_text$congress==106)
  bill_text_107 <- subset(bill_text, bill_text$congress==107)
  bill_text_108 <- subset(bill_text, bill_text$congress==108)
  bill_text_109 <- subset(bill_text, bill_text$congress==109)
  bill_text_110 <- subset(bill_text, bill_text$congress==110)
  bill_text_111 <- subset(bill_text, bill_text$congress==111)
  bill_text_112 <- subset(bill_text, bill_text$congress==112)
  bill_text_113 <- subset(bill_text, bill_text$congress==113)
  bill_text_114 <- subset(bill_text, bill_text$congress==114)
  #Remove Congress Variable
  bill_text_101$congress <- NULL
  bill_text_102$congress <- NULL
  bill_text_103$congress <- NULL
  bill_text_104$congress <- NULL 
  bill_text_105$congress <- NULL
  bill_text_106$congress <- NULL
  bill_text_107$congress <- NULL
  bill_text_108$congress <- NULL
  bill_text_109$congress <- NULL
  bill_text_110$congress <- NULL
  bill_text_111$congress <- NULL
  bill_text_112$congress <- NULL
  bill_text_113$congress <- NULL
  bill_text_114$congress <- NULL
  #Remove Large File
  rm(bill_text)
  #Create Corpuses
  #Set Working Directory
  setwd("filepath/Bill Type/HCONRES/101")
  #Create .txt Files
  lapply(1:nrow(bill_text_101), function(i) write.table(bill_text_101[i,1], 
                                                        file = paste0(bill_text_101[i,2], ".txt"),
                                                        row.names = FALSE, col.names = FALSE,
                                                        quote = FALSE))
  #Repeat for Each Congress
  setwd("filepath/Bill Type/HCONRES/102")
  lapply(1:nrow(bill_text_102), function(i) write.table(bill_text_102[i,1], 
                                                        file = paste0(bill_text_102[i,2], ".txt"),
                                                        row.names = FALSE, col.names = FALSE,
                                                        quote = FALSE))
  setwd("filepath/Bill Type/HCONRES/103")
  lapply(1:nrow(bill_text_103), function(i) write.table(bill_text_103[i,1], 
                                                        file = paste0(bill_text_103[i,2], ".txt"),
                                                        row.names = FALSE, col.names = FALSE,
                                                        quote = FALSE))
  setwd("filepath/Bill Type/HCONRES/104")
  lapply(1:nrow(bill_text_104), function(i) write.table(bill_text_104[i,1], 
                                                        file = paste0(bill_text_104[i,2], ".txt"),
                                                        row.names = FALSE, col.names = FALSE,
                                                        quote = FALSE))
  setwd("filepath/Bill Type/HCONRES/105")
  lapply(1:nrow(bill_text_105), function(i) write.table(bill_text_105[i,1], 
                                                        file = paste0(bill_text_105[i,2], ".txt"),
                                                        row.names = FALSE, col.names = FALSE,
                                                        quote = FALSE))
  setwd("filepath/Bill Type/HCONRES/106")
  lapply(1:nrow(bill_text_106), function(i) write.table(bill_text_106[i,1], 
                                                        file = paste0(bill_text_106[i,2], ".txt"),
                                                        row.names = FALSE, col.names = FALSE,
                                                        quote = FALSE))
  setwd("filepath/Bill Type/HCONRES/107")
  lapply(1:nrow(bill_text_107), function(i) write.table(bill_text_107[i,1], 
                                                        file = paste0(bill_text_107[i,2], ".txt"),
                                                        row.names = FALSE, col.names = FALSE,
                                                        quote = FALSE))
  setwd("filepath/Bill Type/HCONRES/108")
  lapply(1:nrow(bill_text_108), function(i) write.table(bill_text_108[i,1], 
                                                        file = paste0(bill_text_108[i,2], ".txt"),
                                                        row.names = FALSE, col.names = FALSE,
                                                        quote = FALSE))
  setwd("filepath/Bill Type/HCONRES/109")
  lapply(1:nrow(bill_text_109), function(i) write.table(bill_text_109[i,1], 
                                                        file = paste0(bill_text_109[i,2], ".txt"),
                                                        row.names = FALSE, col.names = FALSE,
                                                        quote = FALSE))
  setwd("filepath/Bill Type/HCONRES/110")
  lapply(1:nrow(bill_text_110), function(i) write.table(bill_text_110[i,1], 
                                                        file = paste0(bill_text_110[i,2], ".txt"),
                                                        row.names = FALSE, col.names = FALSE,
                                                        quote = FALSE))
  setwd("filepath/Bill Type/HCONRES/111")
  lapply(1:nrow(bill_text_111), function(i) write.table(bill_text_111[i,1], 
                                                        file = paste0(bill_text_111[i,2], ".txt"),
                                                        row.names = FALSE, col.names = FALSE,
                                                        quote = FALSE))
  setwd("filepath/Bill Type/HCONRES/112")
  lapply(1:nrow(bill_text_112), function(i) write.table(bill_text_112[i,1], 
                                                        file = paste0(bill_text_112[i,2], ".txt"),
                                                        row.names = FALSE, col.names = FALSE,
                                                        quote = FALSE))
  setwd("filepath/Bill Type/HCONRES/113")
  lapply(1:nrow(bill_text_113), function(i) write.table(bill_text_113[i,1], 
                                                        file = paste0(bill_text_113[i,2], ".txt"),
                                                        row.names = FALSE, col.names = FALSE,
                                                        quote = FALSE))
  setwd("filepath/Bill Type/HCONRES/114")
  lapply(1:nrow(bill_text_114), function(i) write.table(bill_text_114[i,1], 
                                                        file = paste0(bill_text_114[i,2], ".txt"),
                                                        row.names = FALSE, col.names = FALSE,
                                                        quote = FALSE))
  
  ####HJRES####
  bill_text <- read.csv("filepath/bill_text_hjres.csv")
  #Create Congressional Text Corpus
  #Remove Other Variables
  bill_text$bill_type <- NULL
  bill_text$number <- NULL
  bill_text$bill_version <- NULL
  bill_text$section_number <- NULL
  bill_text$numberofwords <- NULL
  bill_text$numberofsentences<- NULL
  bill_text$sections <- NULL
  bill_text$bill_id <-NULL
  rownames(bill_text) <- NULL
  #Separate By Congress
  bill_text_101 <- subset(bill_text, bill_text$congress==101)
  bill_text_102 <- subset(bill_text, bill_text$congress==102)
  bill_text_103 <- subset(bill_text, bill_text$congress==103)
  bill_text_104 <- subset(bill_text, bill_text$congress==104)
  bill_text_105 <- subset(bill_text, bill_text$congress==105)
  bill_text_106 <- subset(bill_text, bill_text$congress==106)
  bill_text_107 <- subset(bill_text, bill_text$congress==107)
  bill_text_108 <- subset(bill_text, bill_text$congress==108)
  bill_text_109 <- subset(bill_text, bill_text$congress==109)
  bill_text_110 <- subset(bill_text, bill_text$congress==110)
  bill_text_111 <- subset(bill_text, bill_text$congress==111)
  bill_text_112 <- subset(bill_text, bill_text$congress==112)
  bill_text_113 <- subset(bill_text, bill_text$congress==113)
  bill_text_114 <- subset(bill_text, bill_text$congress==114)
  #Remove Congress Variable
  bill_text_101$congress <- NULL
  bill_text_102$congress <- NULL
  bill_text_103$congress <- NULL
  bill_text_104$congress <- NULL 
  bill_text_105$congress <- NULL
  bill_text_106$congress <- NULL
  bill_text_107$congress <- NULL
  bill_text_108$congress <- NULL
  bill_text_109$congress <- NULL
  bill_text_110$congress <- NULL
  bill_text_111$congress <- NULL
  bill_text_112$congress <- NULL
  bill_text_113$congress <- NULL
  bill_text_114$congress <- NULL
  #Remove Large File
  rm(bill_text)
  #Create Corpuses
  #Set Working Directory
  setwd("filepath/Bill Type/HJRES/101")
  #Create .txt Files
  lapply(1:nrow(bill_text_101), function(i) write.table(bill_text_101[i,1], 
                                                        file = paste0(bill_text_101[i,2], ".txt"),
                                                        row.names = FALSE, col.names = FALSE,
                                                        quote = FALSE))
  #Repeat for Each Congress
  setwd("filepath/Bill Type/HJRES/102")
  lapply(1:nrow(bill_text_102), function(i) write.table(bill_text_102[i,1], 
                                                        file = paste0(bill_text_102[i,2], ".txt"),
                                                        row.names = FALSE, col.names = FALSE,
                                                        quote = FALSE))
  setwd("filepath/Bill Type/HJRES/103")
  lapply(1:nrow(bill_text_103), function(i) write.table(bill_text_103[i,1], 
                                                        file = paste0(bill_text_103[i,2], ".txt"),
                                                        row.names = FALSE, col.names = FALSE,
                                                        quote = FALSE))
  setwd("filepath/Bill Type/HJRES/104")
  lapply(1:nrow(bill_text_104), function(i) write.table(bill_text_104[i,1], 
                                                        file = paste0(bill_text_104[i,2], ".txt"),
                                                        row.names = FALSE, col.names = FALSE,
                                                        quote = FALSE))
  setwd("filepath/Bill Type/HJRES/105")
  lapply(1:nrow(bill_text_105), function(i) write.table(bill_text_105[i,1], 
                                                        file = paste0(bill_text_105[i,2], ".txt"),
                                                        row.names = FALSE, col.names = FALSE,
                                                        quote = FALSE))
  setwd("filepath/Bill Type/HJRES/106")
  lapply(1:nrow(bill_text_106), function(i) write.table(bill_text_106[i,1], 
                                                        file = paste0(bill_text_106[i,2], ".txt"),
                                                        row.names = FALSE, col.names = FALSE,
                                                        quote = FALSE))
  setwd("filepath/Bill Type/HJRES/107")
  lapply(1:nrow(bill_text_107), function(i) write.table(bill_text_107[i,1], 
                                                        file = paste0(bill_text_107[i,2], ".txt"),
                                                        row.names = FALSE, col.names = FALSE,
                                                        quote = FALSE))
  setwd("filepath/Bill Type/HJRES/108")
  lapply(1:nrow(bill_text_108), function(i) write.table(bill_text_108[i,1], 
                                                        file = paste0(bill_text_108[i,2], ".txt"),
                                                        row.names = FALSE, col.names = FALSE,
                                                        quote = FALSE))
  setwd("filepath/Bill Type/HJRES/109")
  lapply(1:nrow(bill_text_109), function(i) write.table(bill_text_109[i,1], 
                                                        file = paste0(bill_text_109[i,2], ".txt"),
                                                        row.names = FALSE, col.names = FALSE,
                                                        quote = FALSE))
  setwd("filepath/Bill Type/HJRES/110")
  lapply(1:nrow(bill_text_110), function(i) write.table(bill_text_110[i,1], 
                                                        file = paste0(bill_text_110[i,2], ".txt"),
                                                        row.names = FALSE, col.names = FALSE,
                                                        quote = FALSE))
  setwd("filepath/Bill Type/HJRES/111")
  lapply(1:nrow(bill_text_111), function(i) write.table(bill_text_111[i,1], 
                                                        file = paste0(bill_text_111[i,2], ".txt"),
                                                        row.names = FALSE, col.names = FALSE,
                                                        quote = FALSE))
  setwd("filepath/Bill Type/HJRES/112")
  lapply(1:nrow(bill_text_112), function(i) write.table(bill_text_112[i,1], 
                                                        file = paste0(bill_text_112[i,2], ".txt"),
                                                        row.names = FALSE, col.names = FALSE,
                                                        quote = FALSE))
  setwd("filepath/Bill Type/HJRES/113")
  lapply(1:nrow(bill_text_113), function(i) write.table(bill_text_113[i,1], 
                                                        file = paste0(bill_text_113[i,2], ".txt"),
                                                        row.names = FALSE, col.names = FALSE,
                                                        quote = FALSE))
  setwd("filepath/Bill Type/HJRES/114")
  lapply(1:nrow(bill_text_114), function(i) write.table(bill_text_114[i,1], 
                                                        file = paste0(bill_text_114[i,2], ".txt"),
                                                        row.names = FALSE, col.names = FALSE,
                                                        quote = FALSE))     
  
  ####HR####
  bill_text <- read.csv("filepath/bill_text_hr.csv")
  #Create Congressional Text Corpus
  #Remove Other Variables
  bill_text$bill_type <- NULL
  bill_text$number <- NULL
  bill_text$bill_version <- NULL
  bill_text$section_number <- NULL
  bill_text$numberofwords <- NULL
  bill_text$numberofsentences<- NULL
  bill_text$sections <- NULL
  bill_text$bill_id <-NULL
  rownames(bill_text) <- NULL
  #Separate By Congress
  bill_text_101 <- subset(bill_text, bill_text$congress==101)
  bill_text_102 <- subset(bill_text, bill_text$congress==102)
  bill_text_103 <- subset(bill_text, bill_text$congress==103)
  bill_text_104 <- subset(bill_text, bill_text$congress==104)
  bill_text_105 <- subset(bill_text, bill_text$congress==105)
  bill_text_106 <- subset(bill_text, bill_text$congress==106)
  bill_text_107 <- subset(bill_text, bill_text$congress==107)
  bill_text_108 <- subset(bill_text, bill_text$congress==108)
  bill_text_109 <- subset(bill_text, bill_text$congress==109)
  bill_text_110 <- subset(bill_text, bill_text$congress==110)
  bill_text_111 <- subset(bill_text, bill_text$congress==111)
  bill_text_112 <- subset(bill_text, bill_text$congress==112)
  bill_text_113 <- subset(bill_text, bill_text$congress==113)
  bill_text_114 <- subset(bill_text, bill_text$congress==114)
  #Remove Congress Variable
  bill_text_101$congress <- NULL
  bill_text_102$congress <- NULL
  bill_text_103$congress <- NULL
  bill_text_104$congress <- NULL 
  bill_text_105$congress <- NULL
  bill_text_106$congress <- NULL
  bill_text_107$congress <- NULL
  bill_text_108$congress <- NULL
  bill_text_109$congress <- NULL
  bill_text_110$congress <- NULL
  bill_text_111$congress <- NULL
  bill_text_112$congress <- NULL
  bill_text_113$congress <- NULL
  bill_text_114$congress <- NULL
  #Remove Large File
  rm(bill_text)
  #Create Corpuses
  #Set Working Directory
  setwd("filepath/Bill Type/HR/101")
  #Create .txt Files
  lapply(1:nrow(bill_text_101), function(i) write.table(bill_text_101[i,1], 
                                                        file = paste0(bill_text_101[i,2], ".txt"),
                                                        row.names = FALSE, col.names = FALSE,
                                                        quote = FALSE))
  #Repeat for Each Congress
  setwd("filepath/Bill Type/HR/102")
  lapply(1:nrow(bill_text_102), function(i) write.table(bill_text_102[i,1], 
                                                        file = paste0(bill_text_102[i,2], ".txt"),
                                                        row.names = FALSE, col.names = FALSE,
                                                        quote = FALSE))
  setwd("filepath/Bill Type/HR/103")
  lapply(1:nrow(bill_text_103), function(i) write.table(bill_text_103[i,1], 
                                                        file = paste0(bill_text_103[i,2], ".txt"),
                                                        row.names = FALSE, col.names = FALSE,
                                                        quote = FALSE))
  setwd("filepath/Bill Type/HR/104")
  lapply(1:nrow(bill_text_104), function(i) write.table(bill_text_104[i,1], 
                                                        file = paste0(bill_text_104[i,2], ".txt"),
                                                        row.names = FALSE, col.names = FALSE,
                                                        quote = FALSE))
  setwd("filepath/Bill Type/HR/105")
  lapply(1:nrow(bill_text_105), function(i) write.table(bill_text_105[i,1], 
                                                        file = paste0(bill_text_105[i,2], ".txt"),
                                                        row.names = FALSE, col.names = FALSE,
                                                        quote = FALSE))
  setwd("filepath/Bill Type/HR/106")
  lapply(1:nrow(bill_text_106), function(i) write.table(bill_text_106[i,1], 
                                                        file = paste0(bill_text_106[i,2], ".txt"),
                                                        row.names = FALSE, col.names = FALSE,
                                                        quote = FALSE))
  setwd("filepath/Bill Type/HR/107")
  lapply(1:nrow(bill_text_107), function(i) write.table(bill_text_107[i,1], 
                                                        file = paste0(bill_text_107[i,2], ".txt"),
                                                        row.names = FALSE, col.names = FALSE,
                                                        quote = FALSE))
  setwd("filepath/Bill Type/HR/108")
  lapply(1:nrow(bill_text_108), function(i) write.table(bill_text_108[i,1], 
                                                        file = paste0(bill_text_108[i,2], ".txt"),
                                                        row.names = FALSE, col.names = FALSE,
                                                        quote = FALSE))
  setwd("filepath/Bill Type/HR/109")
  lapply(1:nrow(bill_text_109), function(i) write.table(bill_text_109[i,1], 
                                                        file = paste0(bill_text_109[i,2], ".txt"),
                                                        row.names = FALSE, col.names = FALSE,
                                                        quote = FALSE))
  setwd("filepath/Bill Type/HR/110")
  lapply(1:nrow(bill_text_110), function(i) write.table(bill_text_110[i,1], 
                                                        file = paste0(bill_text_110[i,2], ".txt"),
                                                        row.names = FALSE, col.names = FALSE,
                                                        quote = FALSE))
  setwd("filepath/Bill Type/HR/111")
  lapply(1:nrow(bill_text_111), function(i) write.table(bill_text_111[i,1], 
                                                        file = paste0(bill_text_111[i,2], ".txt"),
                                                        row.names = FALSE, col.names = FALSE,
                                                        quote = FALSE))
  setwd("filepath/Bill Type/HR/112")
  lapply(1:nrow(bill_text_112), function(i) write.table(bill_text_112[i,1], 
                                                        file = paste0(bill_text_112[i,2], ".txt"),
                                                        row.names = FALSE, col.names = FALSE,
                                                        quote = FALSE))
  setwd("filepath/Bill Type/HR/113")
  lapply(1:nrow(bill_text_113), function(i) write.table(bill_text_113[i,1], 
                                                        file = paste0(bill_text_113[i,2], ".txt"),
                                                        row.names = FALSE, col.names = FALSE,
                                                        quote = FALSE))
  setwd("filepath/Bill Type/HR/114")
  lapply(1:nrow(bill_text_114), function(i) write.table(bill_text_114[i,1], 
                                                        file = paste0(bill_text_114[i,2], ".txt"),
                                                        row.names = FALSE, col.names = FALSE,
                                                        quote = FALSE))
  ####HRES####
  bill_text <- read.csv("filepath/bill_text_hres.csv")
  #Create Congressional Text Corpus
  #Remove Other Variables
  bill_text$bill_type <- NULL
  bill_text$number <- NULL
  bill_text$bill_version <- NULL
  bill_text$section_number <- NULL
  bill_text$numberofwords <- NULL
  bill_text$numberofsentences<- NULL
  bill_text$sections <- NULL
  bill_text$bill_id <-NULL
  rownames(bill_text) <- NULL
  #Separate By Congress
  bill_text_101 <- subset(bill_text, bill_text$congress==101)
  bill_text_102 <- subset(bill_text, bill_text$congress==102)
  bill_text_103 <- subset(bill_text, bill_text$congress==103)
  bill_text_104 <- subset(bill_text, bill_text$congress==104)
  bill_text_105 <- subset(bill_text, bill_text$congress==105)
  bill_text_106 <- subset(bill_text, bill_text$congress==106)
  bill_text_107 <- subset(bill_text, bill_text$congress==107)
  bill_text_108 <- subset(bill_text, bill_text$congress==108)
  bill_text_109 <- subset(bill_text, bill_text$congress==109)
  bill_text_110 <- subset(bill_text, bill_text$congress==110)
  bill_text_111 <- subset(bill_text, bill_text$congress==111)
  bill_text_112 <- subset(bill_text, bill_text$congress==112)
  bill_text_113 <- subset(bill_text, bill_text$congress==113)
  bill_text_114 <- subset(bill_text, bill_text$congress==114)
  #Remove Congress Variable
  bill_text_101$congress <- NULL
  bill_text_102$congress <- NULL
  bill_text_103$congress <- NULL
  bill_text_104$congress <- NULL 
  bill_text_105$congress <- NULL
  bill_text_106$congress <- NULL
  bill_text_107$congress <- NULL
  bill_text_108$congress <- NULL
  bill_text_109$congress <- NULL
  bill_text_110$congress <- NULL
  bill_text_111$congress <- NULL
  bill_text_112$congress <- NULL
  bill_text_113$congress <- NULL
  bill_text_114$congress <- NULL
  #Remove Large File
  rm(bill_text)
  #Create Corpuses
  #Set Working Directory
  setwd("filepath/Bill Type/HRES/101")
  #Create .txt Files
  lapply(1:nrow(bill_text_101), function(i) write.table(bill_text_101[i,1], 
                                                        file = paste0(bill_text_101[i,2], ".txt"),
                                                        row.names = FALSE, col.names = FALSE,
                                                        quote = FALSE))
  #Repeat for Each Congress
  setwd("filepath/Bill Type/HRES/102")
  lapply(1:nrow(bill_text_102), function(i) write.table(bill_text_102[i,1], 
                                                        file = paste0(bill_text_102[i,2], ".txt"),
                                                        row.names = FALSE, col.names = FALSE,
                                                        quote = FALSE))
  setwd("filepath/Bill Type/HRES/103")
  lapply(1:nrow(bill_text_103), function(i) write.table(bill_text_103[i,1], 
                                                        file = paste0(bill_text_103[i,2], ".txt"),
                                                        row.names = FALSE, col.names = FALSE,
                                                        quote = FALSE))
  setwd("filepath/Bill Type/HRES/104")
  lapply(1:nrow(bill_text_104), function(i) write.table(bill_text_104[i,1], 
                                                        file = paste0(bill_text_104[i,2], ".txt"),
                                                        row.names = FALSE, col.names = FALSE,
                                                        quote = FALSE))
  setwd("filepath/Bill Type/HRES/105")
  lapply(1:nrow(bill_text_105), function(i) write.table(bill_text_105[i,1], 
                                                        file = paste0(bill_text_105[i,2], ".txt"),
                                                        row.names = FALSE, col.names = FALSE,
                                                        quote = FALSE))
  setwd("filepath/Bill Type/HRES/106")
  lapply(1:nrow(bill_text_106), function(i) write.table(bill_text_106[i,1], 
                                                        file = paste0(bill_text_106[i,2], ".txt"),
                                                        row.names = FALSE, col.names = FALSE,
                                                        quote = FALSE))
  setwd("filepath/Bill Type/HRES/107")
  lapply(1:nrow(bill_text_107), function(i) write.table(bill_text_107[i,1], 
                                                        file = paste0(bill_text_107[i,2], ".txt"),
                                                        row.names = FALSE, col.names = FALSE,
                                                        quote = FALSE))
  setwd("filepath/Bill Type/HRES/108")
  lapply(1:nrow(bill_text_108), function(i) write.table(bill_text_108[i,1], 
                                                        file = paste0(bill_text_108[i,2], ".txt"),
                                                        row.names = FALSE, col.names = FALSE,
                                                        quote = FALSE))
  setwd("filepath/Bill Type/HRES/109")
  lapply(1:nrow(bill_text_109), function(i) write.table(bill_text_109[i,1], 
                                                        file = paste0(bill_text_109[i,2], ".txt"),
                                                        row.names = FALSE, col.names = FALSE,
                                                        quote = FALSE))
  setwd("filepath/Bill Type/HRES/110")
  lapply(1:nrow(bill_text_110), function(i) write.table(bill_text_110[i,1], 
                                                        file = paste0(bill_text_110[i,2], ".txt"),
                                                        row.names = FALSE, col.names = FALSE,
                                                        quote = FALSE))
  setwd("filepath/Bill Type/HRES/111")
  lapply(1:nrow(bill_text_111), function(i) write.table(bill_text_111[i,1], 
                                                        file = paste0(bill_text_111[i,2], ".txt"),
                                                        row.names = FALSE, col.names = FALSE,
                                                        quote = FALSE))
  setwd("filepath/Bill Type/HRES/112")
  lapply(1:nrow(bill_text_112), function(i) write.table(bill_text_112[i,1], 
                                                        file = paste0(bill_text_112[i,2], ".txt"),
                                                        row.names = FALSE, col.names = FALSE,
                                                        quote = FALSE))
  setwd("filepath/Bill Type/HRES/113")
  lapply(1:nrow(bill_text_113), function(i) write.table(bill_text_113[i,1], 
                                                        file = paste0(bill_text_113[i,2], ".txt"),
                                                        row.names = FALSE, col.names = FALSE,
                                                        quote = FALSE))
  setwd("filepath/Bill Type/HRES/114")
  lapply(1:nrow(bill_text_114), function(i) write.table(bill_text_114[i,1], 
                                                        file = paste0(bill_text_114[i,2], ".txt"),
                                                        row.names = FALSE, col.names = FALSE,
                                                        quote = FALSE))
  ####S####
  bill_text <- read.csv("filepath/bill_text_s.csv")
  #Create Congressional Text Corpus
  #Remove Other Variables
  bill_text$bill_type <- NULL
  bill_text$number <- NULL
  bill_text$bill_version <- NULL
  bill_text$section_number <- NULL
  bill_text$numberofwords <- NULL
  bill_text$numberofsentences<- NULL
  bill_text$sections <- NULL
  bill_text$bill_id <-NULL
  rownames(bill_text) <- NULL
  #Separate By Congress
  bill_text_101 <- subset(bill_text, bill_text$congress==101)
  bill_text_102 <- subset(bill_text, bill_text$congress==102)
  bill_text_103 <- subset(bill_text, bill_text$congress==103)
  bill_text_104 <- subset(bill_text, bill_text$congress==104)
  bill_text_105 <- subset(bill_text, bill_text$congress==105)
  bill_text_106 <- subset(bill_text, bill_text$congress==106)
  bill_text_107 <- subset(bill_text, bill_text$congress==107)
  bill_text_108 <- subset(bill_text, bill_text$congress==108)
  bill_text_109 <- subset(bill_text, bill_text$congress==109)
  bill_text_110 <- subset(bill_text, bill_text$congress==110)
  bill_text_111 <- subset(bill_text, bill_text$congress==111)
  bill_text_112 <- subset(bill_text, bill_text$congress==112)
  bill_text_113 <- subset(bill_text, bill_text$congress==113)
  bill_text_114 <- subset(bill_text, bill_text$congress==114)
  #Remove Congress Variable
  bill_text_101$congress <- NULL
  bill_text_102$congress <- NULL
  bill_text_103$congress <- NULL
  bill_text_104$congress <- NULL 
  bill_text_105$congress <- NULL
  bill_text_106$congress <- NULL
  bill_text_107$congress <- NULL
  bill_text_108$congress <- NULL
  bill_text_109$congress <- NULL
  bill_text_110$congress <- NULL
  bill_text_111$congress <- NULL
  bill_text_112$congress <- NULL
  bill_text_113$congress <- NULL
  bill_text_114$congress <- NULL
  #Remove Large File
  rm(bill_text)
  #Create Corpuses
  #Set Working Directory
  setwd("filepath/Bill Type/S/101")
  #Create .txt Files
  lapply(1:nrow(bill_text_101), function(i) write.table(bill_text_101[i,1], 
                                                        file = paste0(bill_text_101[i,2], ".txt"),
                                                        row.names = FALSE, col.names = FALSE,
                                                        quote = FALSE))
  #Repeat for Each Congress
  setwd("filepath/Bill Type/S/102")
  lapply(1:nrow(bill_text_102), function(i) write.table(bill_text_102[i,1], 
                                                        file = paste0(bill_text_102[i,2], ".txt"),
                                                        row.names = FALSE, col.names = FALSE,
                                                        quote = FALSE))
  setwd("filepath/Bill Type/S/103")
  lapply(1:nrow(bill_text_103), function(i) write.table(bill_text_103[i,1], 
                                                        file = paste0(bill_text_103[i,2], ".txt"),
                                                        row.names = FALSE, col.names = FALSE,
                                                        quote = FALSE))
  setwd("filepath/Bill Type/S/104")
  lapply(1:nrow(bill_text_104), function(i) write.table(bill_text_104[i,1], 
                                                        file = paste0(bill_text_104[i,2], ".txt"),
                                                        row.names = FALSE, col.names = FALSE,
                                                        quote = FALSE))
  setwd("filepath/Bill Type/S/105")
  lapply(1:nrow(bill_text_105), function(i) write.table(bill_text_105[i,1], 
                                                        file = paste0(bill_text_105[i,2], ".txt"),
                                                        row.names = FALSE, col.names = FALSE,
                                                        quote = FALSE))
  setwd("filepath/Bill Type/S/106")
  lapply(1:nrow(bill_text_106), function(i) write.table(bill_text_106[i,1], 
                                                        file = paste0(bill_text_106[i,2], ".txt"),
                                                        row.names = FALSE, col.names = FALSE,
                                                        quote = FALSE))
  setwd("filepath/Bill Type/S/107")
  lapply(1:nrow(bill_text_107), function(i) write.table(bill_text_107[i,1], 
                                                        file = paste0(bill_text_107[i,2], ".txt"),
                                                        row.names = FALSE, col.names = FALSE,
                                                        quote = FALSE))
  setwd("filepath/Bill Type/S/108")
  lapply(1:nrow(bill_text_108), function(i) write.table(bill_text_108[i,1], 
                                                        file = paste0(bill_text_108[i,2], ".txt"),
                                                        row.names = FALSE, col.names = FALSE,
                                                        quote = FALSE))
  setwd("filepath/Bill Type/S/109")
  lapply(1:nrow(bill_text_109), function(i) write.table(bill_text_109[i,1], 
                                                        file = paste0(bill_text_109[i,2], ".txt"),
                                                        row.names = FALSE, col.names = FALSE,
                                                        quote = FALSE))
  setwd("filepath/Bill Type/S/110")
  lapply(1:nrow(bill_text_110), function(i) write.table(bill_text_110[i,1], 
                                                        file = paste0(bill_text_110[i,2], ".txt"),
                                                        row.names = FALSE, col.names = FALSE,
                                                        quote = FALSE))
  setwd("filepath/Bill Type/S/111")
  lapply(1:nrow(bill_text_111), function(i) write.table(bill_text_111[i,1], 
                                                        file = paste0(bill_text_111[i,2], ".txt"),
                                                        row.names = FALSE, col.names = FALSE,
                                                        quote = FALSE))
  setwd("filepath/Bill Type/S/112")
  lapply(1:nrow(bill_text_112), function(i) write.table(bill_text_112[i,1], 
                                                        file = paste0(bill_text_112[i,2], ".txt"),
                                                        row.names = FALSE, col.names = FALSE,
                                                        quote = FALSE))
  setwd("filepath/Bill Type/S/113")
  lapply(1:nrow(bill_text_113), function(i) write.table(bill_text_113[i,1], 
                                                        file = paste0(bill_text_113[i,2], ".txt"),
                                                        row.names = FALSE, col.names = FALSE,
                                                        quote = FALSE))
  setwd("filepath/Bill Type/S/114")
  lapply(1:nrow(bill_text_114), function(i) write.table(bill_text_114[i,1], 
                                                        file = paste0(bill_text_114[i,2], ".txt"),
                                                        row.names = FALSE, col.names = FALSE,
                                                        quote = FALSE))
  ####SCONRES####
  bill_text <- read.csv("filepath/bill_text_sconres.csv")
  #Create Congressional Text Corpus
  #Remove Other Variables
  bill_text$bill_type <- NULL
  bill_text$number <- NULL
  bill_text$bill_version <- NULL
  bill_text$section_number <- NULL
  bill_text$numberofwords <- NULL
  bill_text$numberofsentences<- NULL
  bill_text$sections <- NULL
  bill_text$bill_id <-NULL
  rownames(bill_text) <- NULL
  #Separate By Congress
  bill_text_101 <- subset(bill_text, bill_text$congress==101)
  bill_text_102 <- subset(bill_text, bill_text$congress==102)
  bill_text_103 <- subset(bill_text, bill_text$congress==103)
  bill_text_104 <- subset(bill_text, bill_text$congress==104)
  bill_text_105 <- subset(bill_text, bill_text$congress==105)
  bill_text_106 <- subset(bill_text, bill_text$congress==106)
  bill_text_107 <- subset(bill_text, bill_text$congress==107)
  bill_text_108 <- subset(bill_text, bill_text$congress==108)
  bill_text_109 <- subset(bill_text, bill_text$congress==109)
  bill_text_110 <- subset(bill_text, bill_text$congress==110)
  bill_text_111 <- subset(bill_text, bill_text$congress==111)
  bill_text_112 <- subset(bill_text, bill_text$congress==112)
  bill_text_113 <- subset(bill_text, bill_text$congress==113)
  bill_text_114 <- subset(bill_text, bill_text$congress==114)
  #Remove Congress Variable
  bill_text_101$congress <- NULL
  bill_text_102$congress <- NULL
  bill_text_103$congress <- NULL
  bill_text_104$congress <- NULL 
  bill_text_105$congress <- NULL
  bill_text_106$congress <- NULL
  bill_text_107$congress <- NULL
  bill_text_108$congress <- NULL
  bill_text_109$congress <- NULL
  bill_text_110$congress <- NULL
  bill_text_111$congress <- NULL
  bill_text_112$congress <- NULL
  bill_text_113$congress <- NULL
  bill_text_114$congress <- NULL
  #Remove Large File
  rm(bill_text)
  #Create Corpuses
  #Set Working Directory
  setwd("filepath/Bill Type/SCONRES/101")
  #Create .txt Files
  lapply(1:nrow(bill_text_101), function(i) write.table(bill_text_101[i,1], 
                                                        file = paste0(bill_text_101[i,2], ".txt"),
                                                        row.names = FALSE, col.names = FALSE,
                                                        quote = FALSE))
  #Repeat for Each Congress
  setwd("filepath/Bill Type/SCONRES/102")
  lapply(1:nrow(bill_text_102), function(i) write.table(bill_text_102[i,1], 
                                                        file = paste0(bill_text_102[i,2], ".txt"),
                                                        row.names = FALSE, col.names = FALSE,
                                                        quote = FALSE))
  setwd("filepath/Bill Type/SCONRES/103")
  lapply(1:nrow(bill_text_103), function(i) write.table(bill_text_103[i,1], 
                                                        file = paste0(bill_text_103[i,2], ".txt"),
                                                        row.names = FALSE, col.names = FALSE,
                                                        quote = FALSE))
  setwd("filepath/Bill Type/SCONRES/104")
  lapply(1:nrow(bill_text_104), function(i) write.table(bill_text_104[i,1], 
                                                        file = paste0(bill_text_104[i,2], ".txt"),
                                                        row.names = FALSE, col.names = FALSE,
                                                        quote = FALSE))
  setwd("filepath/Bill Type/SCONRES/105")
  lapply(1:nrow(bill_text_105), function(i) write.table(bill_text_105[i,1], 
                                                        file = paste0(bill_text_105[i,2], ".txt"),
                                                        row.names = FALSE, col.names = FALSE,
                                                        quote = FALSE))
  setwd("filepath/Bill Type/SCONRES/106")
  lapply(1:nrow(bill_text_106), function(i) write.table(bill_text_106[i,1], 
                                                        file = paste0(bill_text_106[i,2], ".txt"),
                                                        row.names = FALSE, col.names = FALSE,
                                                        quote = FALSE))
  setwd("filepath/Bill Type/SCONRES/107")
  lapply(1:nrow(bill_text_107), function(i) write.table(bill_text_107[i,1], 
                                                        file = paste0(bill_text_107[i,2], ".txt"),
                                                        row.names = FALSE, col.names = FALSE,
                                                        quote = FALSE))
  setwd("filepath/Bill Type/SCONRES/108")
  lapply(1:nrow(bill_text_108), function(i) write.table(bill_text_108[i,1], 
                                                        file = paste0(bill_text_108[i,2], ".txt"),
                                                        row.names = FALSE, col.names = FALSE,
                                                        quote = FALSE))
  setwd("filepath/Bill Type/SCONRES/109")
  lapply(1:nrow(bill_text_109), function(i) write.table(bill_text_109[i,1], 
                                                        file = paste0(bill_text_109[i,2], ".txt"),
                                                        row.names = FALSE, col.names = FALSE,
                                                        quote = FALSE))
  setwd("filepath/Bill Type/SCONRES/110")
  lapply(1:nrow(bill_text_110), function(i) write.table(bill_text_110[i,1], 
                                                        file = paste0(bill_text_110[i,2], ".txt"),
                                                        row.names = FALSE, col.names = FALSE,
                                                        quote = FALSE))
  setwd("filepath/Bill Type/SCONRES/111")
  lapply(1:nrow(bill_text_111), function(i) write.table(bill_text_111[i,1], 
                                                        file = paste0(bill_text_111[i,2], ".txt"),
                                                        row.names = FALSE, col.names = FALSE,
                                                        quote = FALSE))
  setwd("filepath/Bill Type/SCONRES/112")
  lapply(1:nrow(bill_text_112), function(i) write.table(bill_text_112[i,1], 
                                                        file = paste0(bill_text_112[i,2], ".txt"),
                                                        row.names = FALSE, col.names = FALSE,
                                                        quote = FALSE))
  setwd("filepath/Bill Type/SCONRES/113")
  lapply(1:nrow(bill_text_113), function(i) write.table(bill_text_113[i,1], 
                                                        file = paste0(bill_text_113[i,2], ".txt"),
                                                        row.names = FALSE, col.names = FALSE,
                                                        quote = FALSE))
  setwd("filepath/Bill Type/SCONRES/114")
  lapply(1:nrow(bill_text_114), function(i) write.table(bill_text_114[i,1], 
                                                        file = paste0(bill_text_114[i,2], ".txt"),
                                                        row.names = FALSE, col.names = FALSE,
                                                        quote = FALSE))
  ####SJRES####
  bill_text <- read.csv("filepath/bill_text_sjres.csv")
  #Create Congressional Text Corpus
  #Remove Other Variables
  bill_text$bill_type <- NULL
  bill_text$number <- NULL
  bill_text$bill_version <- NULL
  bill_text$section_number <- NULL
  bill_text$numberofwords <- NULL
  bill_text$numberofsentences<- NULL
  bill_text$sections <- NULL
  bill_text$bill_id <-NULL
  rownames(bill_text) <- NULL
  #Separate By Congress
  bill_text_101 <- subset(bill_text, bill_text$congress==101)
  bill_text_102 <- subset(bill_text, bill_text$congress==102)
  bill_text_103 <- subset(bill_text, bill_text$congress==103)
  bill_text_104 <- subset(bill_text, bill_text$congress==104)
  bill_text_105 <- subset(bill_text, bill_text$congress==105)
  bill_text_106 <- subset(bill_text, bill_text$congress==106)
  bill_text_107 <- subset(bill_text, bill_text$congress==107)
  bill_text_108 <- subset(bill_text, bill_text$congress==108)
  bill_text_109 <- subset(bill_text, bill_text$congress==109)
  bill_text_110 <- subset(bill_text, bill_text$congress==110)
  bill_text_111 <- subset(bill_text, bill_text$congress==111)
  bill_text_112 <- subset(bill_text, bill_text$congress==112)
  bill_text_113 <- subset(bill_text, bill_text$congress==113)
  bill_text_114 <- subset(bill_text, bill_text$congress==114)
  #Remove Congress Variable
  bill_text_101$congress <- NULL
  bill_text_102$congress <- NULL
  bill_text_103$congress <- NULL
  bill_text_104$congress <- NULL 
  bill_text_105$congress <- NULL
  bill_text_106$congress <- NULL
  bill_text_107$congress <- NULL
  bill_text_108$congress <- NULL
  bill_text_109$congress <- NULL
  bill_text_110$congress <- NULL
  bill_text_111$congress <- NULL
  bill_text_112$congress <- NULL
  bill_text_113$congress <- NULL
  bill_text_114$congress <- NULL
  #Remove Large File
  rm(bill_text)
  #Create Corpuses
  #Set Working Directory
  setwd("filepath/Bill Type/SJRES/101")
  #Create .txt Files
  lapply(1:nrow(bill_text_101), function(i) write.table(bill_text_101[i,1], 
                                                        file = paste0(bill_text_101[i,2], ".txt"),
                                                        row.names = FALSE, col.names = FALSE,
                                                        quote = FALSE))
  #Repeat for Each Congress
  setwd("filepath/Bill Type/SJRES/102")
  lapply(1:nrow(bill_text_102), function(i) write.table(bill_text_102[i,1], 
                                                        file = paste0(bill_text_102[i,2], ".txt"),
                                                        row.names = FALSE, col.names = FALSE,
                                                        quote = FALSE))
  setwd("filepath/Bill Type/SJRES/103")
  lapply(1:nrow(bill_text_103), function(i) write.table(bill_text_103[i,1], 
                                                        file = paste0(bill_text_103[i,2], ".txt"),
                                                        row.names = FALSE, col.names = FALSE,
                                                        quote = FALSE))
  setwd("filepath/Bill Type/SJRES/104")
  lapply(1:nrow(bill_text_104), function(i) write.table(bill_text_104[i,1], 
                                                        file = paste0(bill_text_104[i,2], ".txt"),
                                                        row.names = FALSE, col.names = FALSE,
                                                        quote = FALSE))
  setwd("filepath/Bill Type/SJRES/105")
  lapply(1:nrow(bill_text_105), function(i) write.table(bill_text_105[i,1], 
                                                        file = paste0(bill_text_105[i,2], ".txt"),
                                                        row.names = FALSE, col.names = FALSE,
                                                        quote = FALSE))
  setwd("filepath/Bill Type/SJRES/106")
  lapply(1:nrow(bill_text_106), function(i) write.table(bill_text_106[i,1], 
                                                        file = paste0(bill_text_106[i,2], ".txt"),
                                                        row.names = FALSE, col.names = FALSE,
                                                        quote = FALSE))
  setwd("filepath/Bill Type/SJRES/107")
  lapply(1:nrow(bill_text_107), function(i) write.table(bill_text_107[i,1], 
                                                        file = paste0(bill_text_107[i,2], ".txt"),
                                                        row.names = FALSE, col.names = FALSE,
                                                        quote = FALSE))
  setwd("filepath/Bill Type/SJRES/108")
  lapply(1:nrow(bill_text_108), function(i) write.table(bill_text_108[i,1], 
                                                        file = paste0(bill_text_108[i,2], ".txt"),
                                                        row.names = FALSE, col.names = FALSE,
                                                        quote = FALSE))
  setwd("filepath/Bill Type/SJRES/109")
  lapply(1:nrow(bill_text_109), function(i) write.table(bill_text_109[i,1], 
                                                        file = paste0(bill_text_109[i,2], ".txt"),
                                                        row.names = FALSE, col.names = FALSE,
                                                        quote = FALSE))
  setwd("filepath/Bill Type/SJRES/110")
  lapply(1:nrow(bill_text_110), function(i) write.table(bill_text_110[i,1], 
                                                        file = paste0(bill_text_110[i,2], ".txt"),
                                                        row.names = FALSE, col.names = FALSE,
                                                        quote = FALSE))
  setwd("filepath/Bill Type/SJRES/111")
  lapply(1:nrow(bill_text_111), function(i) write.table(bill_text_111[i,1], 
                                                        file = paste0(bill_text_111[i,2], ".txt"),
                                                        row.names = FALSE, col.names = FALSE,
                                                        quote = FALSE))
  setwd("filepath/Bill Type/SJRES/112")
  lapply(1:nrow(bill_text_112), function(i) write.table(bill_text_112[i,1], 
                                                        file = paste0(bill_text_112[i,2], ".txt"),
                                                        row.names = FALSE, col.names = FALSE,
                                                        quote = FALSE))
  setwd("filepath/Bill Type/SJRES/113")
  lapply(1:nrow(bill_text_113), function(i) write.table(bill_text_113[i,1], 
                                                        file = paste0(bill_text_113[i,2], ".txt"),
                                                        row.names = FALSE, col.names = FALSE,
                                                        quote = FALSE))
  setwd("filepath/Bill Type/SJRES/114")
  lapply(1:nrow(bill_text_114), function(i) write.table(bill_text_114[i,1], 
                                                        file = paste0(bill_text_114[i,2], ".txt"),
                                                        row.names = FALSE, col.names = FALSE,
                                                        quote = FALSE))  
  ####SRES####
  bill_text <- read.csv("filepath/bill_text_sres.csv")
  #Create Congressional Text Corpus
  #Remove Other Variables
  bill_text$bill_type <- NULL
  bill_text$number <- NULL
  bill_text$bill_version <- NULL
  bill_text$section_number <- NULL
  bill_text$numberofwords <- NULL
  bill_text$numberofsentences<- NULL
  bill_text$sections <- NULL
  bill_text$bill_id <-NULL
  rownames(bill_text) <- NULL
  #Separate By Congress
  bill_text_101 <- subset(bill_text, bill_text$congress==101)
  bill_text_102 <- subset(bill_text, bill_text$congress==102)
  bill_text_103 <- subset(bill_text, bill_text$congress==103)
  bill_text_104 <- subset(bill_text, bill_text$congress==104)
  bill_text_105 <- subset(bill_text, bill_text$congress==105)
  bill_text_106 <- subset(bill_text, bill_text$congress==106)
  bill_text_107 <- subset(bill_text, bill_text$congress==107)
  bill_text_108 <- subset(bill_text, bill_text$congress==108)
  bill_text_109 <- subset(bill_text, bill_text$congress==109)
  bill_text_110 <- subset(bill_text, bill_text$congress==110)
  bill_text_111 <- subset(bill_text, bill_text$congress==111)
  bill_text_112 <- subset(bill_text, bill_text$congress==112)
  bill_text_113 <- subset(bill_text, bill_text$congress==113)
  bill_text_114 <- subset(bill_text, bill_text$congress==114)
  #Remove Congress Variable
  bill_text_101$congress <- NULL
  bill_text_102$congress <- NULL
  bill_text_103$congress <- NULL
  bill_text_104$congress <- NULL 
  bill_text_105$congress <- NULL
  bill_text_106$congress <- NULL
  bill_text_107$congress <- NULL
  bill_text_108$congress <- NULL
  bill_text_109$congress <- NULL
  bill_text_110$congress <- NULL
  bill_text_111$congress <- NULL
  bill_text_112$congress <- NULL
  bill_text_113$congress <- NULL
  bill_text_114$congress <- NULL
  #Remove Large File
  rm(bill_text)
  #Create Corpuses
  #Set Working Directory
  setwd("filepath/Bill Type/SRES/101")
  #Create .txt Files
  lapply(1:nrow(bill_text_101), function(i) write.table(bill_text_101[i,1], 
                                                        file = paste0(bill_text_101[i,2], ".txt"),
                                                        row.names = FALSE, col.names = FALSE,
                                                        quote = FALSE))
  #Repeat for Each Congress
  setwd("filepath/Bill Type/SRES/102")
  lapply(1:nrow(bill_text_102), function(i) write.table(bill_text_102[i,1], 
                                                        file = paste0(bill_text_102[i,2], ".txt"),
                                                        row.names = FALSE, col.names = FALSE,
                                                        quote = FALSE))
  setwd("filepath/Bill Type/SRES/103")
  lapply(1:nrow(bill_text_103), function(i) write.table(bill_text_103[i,1], 
                                                        file = paste0(bill_text_103[i,2], ".txt"),
                                                        row.names = FALSE, col.names = FALSE,
                                                        quote = FALSE))
  setwd("filepath/Bill Type/SRES/104")
  lapply(1:nrow(bill_text_104), function(i) write.table(bill_text_104[i,1], 
                                                        file = paste0(bill_text_104[i,2], ".txt"),
                                                        row.names = FALSE, col.names = FALSE,
                                                        quote = FALSE))
  setwd("filepath/Bill Type/SRES/105")
  lapply(1:nrow(bill_text_105), function(i) write.table(bill_text_105[i,1], 
                                                        file = paste0(bill_text_105[i,2], ".txt"),
                                                        row.names = FALSE, col.names = FALSE,
                                                        quote = FALSE))
  setwd("filepath/Bill Type/SRES/106")
  lapply(1:nrow(bill_text_106), function(i) write.table(bill_text_106[i,1], 
                                                        file = paste0(bill_text_106[i,2], ".txt"),
                                                        row.names = FALSE, col.names = FALSE,
                                                        quote = FALSE))
  setwd("filepath/Bill Type/SRES/107")
  lapply(1:nrow(bill_text_107), function(i) write.table(bill_text_107[i,1], 
                                                        file = paste0(bill_text_107[i,2], ".txt"),
                                                        row.names = FALSE, col.names = FALSE,
                                                        quote = FALSE))
  setwd("filepath/Bill Type/SRES/108")
  lapply(1:nrow(bill_text_108), function(i) write.table(bill_text_108[i,1], 
                                                        file = paste0(bill_text_108[i,2], ".txt"),
                                                        row.names = FALSE, col.names = FALSE,
                                                        quote = FALSE))
  setwd("filepath/Bill Type/SRES/109")
  lapply(1:nrow(bill_text_109), function(i) write.table(bill_text_109[i,1], 
                                                        file = paste0(bill_text_109[i,2], ".txt"),
                                                        row.names = FALSE, col.names = FALSE,
                                                        quote = FALSE))
  setwd("filepath/Bill Type/SRES/110")
  lapply(1:nrow(bill_text_110), function(i) write.table(bill_text_110[i,1], 
                                                        file = paste0(bill_text_110[i,2], ".txt"),
                                                        row.names = FALSE, col.names = FALSE,
                                                        quote = FALSE))
  setwd("filepath/Bill Type/SRES/111")
  lapply(1:nrow(bill_text_111), function(i) write.table(bill_text_111[i,1], 
                                                        file = paste0(bill_text_111[i,2], ".txt"),
                                                        row.names = FALSE, col.names = FALSE,
                                                        quote = FALSE))
  setwd("filepath/Bill Type/SRES/112")
  lapply(1:nrow(bill_text_112), function(i) write.table(bill_text_112[i,1], 
                                                        file = paste0(bill_text_112[i,2], ".txt"),
                                                        row.names = FALSE, col.names = FALSE,
                                                        quote = FALSE))
  setwd("filepath/Bill Type/SRES/113")
  lapply(1:nrow(bill_text_113), function(i) write.table(bill_text_113[i,1], 
                                                        file = paste0(bill_text_113[i,2], ".txt"),
                                                        row.names = FALSE, col.names = FALSE,
                                                        quote = FALSE))
  setwd("filepath/Bill Type/SRES/114")
  lapply(1:nrow(bill_text_114), function(i) write.table(bill_text_114[i,1], 
                                                        file = paste0(bill_text_114[i,2], ".txt"),
                                                        row.names = FALSE, col.names = FALSE,
                                                        quote = FALSE))
  
  
  
  #### 5. Alignment Measurement Part One - Jaccard Similarity ####
  #Note: Due to Processing Limitations This is Being Done in Multiple Stages (CongressxCongress). Could be run in a single stage if processing power allowed.
  library(textreuse)
  library(data.table)
  #### HR - Within House Comparisons####
  #By Congress
  corpus_HR_101<-list.files(path ="filepath/Bill Text Corpus/Bill Type/HR/101/",pattern="*.txt",full.names = TRUE)
  corpus_HR_102<-list.files(path ="filepath/Bill Text Corpus/Bill Type/HR/102",pattern="*.txt",full.names = TRUE)
  corpus_HR_103<-list.files(path ="filepath/Bill Text Corpus/Bill Type/HR/103/",pattern="*.txt",full.names = TRUE)
  corpus_HR_104<-list.files(path ="filepath/Bill Text Corpus/Bill Type/HR/104/",pattern="*.txt",full.names = TRUE)
  corpus_HR_105<-list.files(path ="filepath/Bill Text Corpus/Bill Type/HR/105/",pattern="*.txt",full.names = TRUE)
  corpus_HR_106<-list.files(path ="filepath/Bill Text Corpus/Bill Type/HR/106/",pattern="*.txt",full.names = TRUE)
  corpus_HR_107<-list.files(path ="filepath/Bill Text Corpus/Bill Type/HR/107/",pattern="*.txt",full.names = TRUE)
  corpus_HR_108<-list.files(path ="filepath/Bill Text Corpus/Bill Type/HR/108/",pattern="*.txt",full.names = TRUE)
  corpus_HR_109<-list.files(path ="filepath/Bill Text Corpus/Bill Type/HR/109/",pattern="*.txt",full.names = TRUE)
  corpus_HR_110<-list.files(path ="filepath/Bill Text Corpus/Bill Type/HR/110/",pattern="*.txt",full.names = TRUE)
  corpus_HR_111<-list.files(path ="filepath/Bill Text Corpus/Bill Type/HR/111/",pattern="*.txt",full.names = TRUE)
  corpus_HR_112<-list.files(path ="filepath/Bill Text Corpus/Bill Type/HR/112/",pattern="*.txt",full.names = TRUE)
  corpus_HR_113<-list.files(path ="filepath/Bill Text Corpus/Bill Type/HR/113/",pattern="*.txt",full.names = TRUE)
  corpus_HR_114<-list.files(path ="filepath/Bill Text Corpus/Bill Type/HR/114/",pattern="*.txt",full.names = TRUE)
  #Create Minhash Function for Locality Sensitive Hashing (LSH) - SAME FOR ALL COMPARISONS
  minhash <- minhash_generator(n= 240, seed = 3352)
  
  #101 Corpus & Similarity Measurements
  #Similarity Measurement (Detailed) 
  #Create Corpus Paths List & Generate TextReuseCorpus  
  corpus_HR_paths <- c(corpus_HR_101, corpus_HR_102)
  corpus <- TextReuseCorpus(paths=corpus_HR_paths, 
                            tokenizer = tokenize_ngrams, n = 5, 
                            minhash_func = minhash, 
                            keep_tokens = TRUE, 
                            progress = TRUE)
  #LSH using Created Minhash
  buckets <- lsh(corpus, bands = 60, progress = TRUE)
  #NOTE ON BAND CHOICE OPTIONS:
  #lsh_threshold(h=240, b=30) #Measures Above .65 Likely Matches
  #lsh_threshold(h=240, b=40) #Measures Above .54 Likely Matches
  #lsh_threshold(h=240, b=48) #Measures Above .46 Likely Matches
  #lsh_threshold(h=240, b=60) #Measures Above .35 Likely Matches
  #lsh_threshold(h=240, b=80) #Measures Above .23 Likely Matches
  #lsh_threshold(h=240, b=120) #Measures Above .09 Likely Matches
  #Identify Potential Matches
  candidates <- lsh_candidates(buckets) 
  #Jaccard Similarity Measurement for Potential Matches
  jaccardsimilarity<- lsh_compare (candidates, corpus, jaccard_similarity, progress=TRUE)
  #Save .CSV  
  fwrite (jaccardsimilarity, "filepath/Bill Text Data/jaccardsimilarity101HR-102HR.csv", row.names=FALSE)
  corpus_HR_paths <- c(corpus_HR_101, corpus_HR_103)
  corpus <- TextReuseCorpus(paths=corpus_HR_paths, 
                            tokenizer = tokenize_ngrams, n = 5, 
                            minhash_func = minhash, 
                            keep_tokens = TRUE, 
                            progress = TRUE)
  buckets <- lsh(corpus, bands = 60, progress = TRUE)
  candidates <- lsh_candidates(buckets) 
  jaccardsimilarity<- lsh_compare (candidates, corpus, jaccard_similarity, progress=TRUE)
  fwrite (jaccardsimilarity, "filepath/Bill Text Data/jaccardsimilarity101HR-103HR.csv", row.names=FALSE)
  corpus_HR_paths <- c(corpus_HR_101, corpus_HR_104)
  corpus <- TextReuseCorpus(paths=corpus_HR_paths, 
                            tokenizer = tokenize_ngrams, n = 5, 
                            minhash_func = minhash, 
                            keep_tokens = TRUE, 
                            progress = TRUE)
  buckets <- lsh(corpus, bands = 60, progress = TRUE)
  candidates <- lsh_candidates(buckets) 
  jaccardsimilarity<- lsh_compare (candidates, corpus, jaccard_similarity, progress=TRUE)
  fwrite (jaccardsimilarity, "filepath/Bill Text Data/jaccardsimilarity101HR-104HR.csv", row.names=FALSE)
  corpus_HR_paths <- c(corpus_HR_101, corpus_HR_105)
  corpus <- TextReuseCorpus(paths=corpus_HR_paths, 
                            tokenizer = tokenize_ngrams, n = 5, 
                            minhash_func = minhash, 
                            keep_tokens = TRUE, 
                            progress = TRUE)
  buckets <- lsh(corpus, bands = 60, progress = TRUE)
  candidates <- lsh_candidates(buckets) 
  jaccardsimilarity<- lsh_compare (candidates, corpus, jaccard_similarity, progress=TRUE)
  fwrite (jaccardsimilarity, "filepath/Bill Text Data/jaccardsimilarity101HR-105HR.csv", row.names=FALSE)
  corpus_HR_paths <- c(corpus_HR_101, corpus_HR_106)
  corpus <- TextReuseCorpus(paths=corpus_HR_paths, 
                            tokenizer = tokenize_ngrams, n = 5, 
                            minhash_func = minhash, 
                            keep_tokens = TRUE, 
                            progress = TRUE)
  buckets <- lsh(corpus, bands = 60, progress = TRUE)
  candidates <- lsh_candidates(buckets) 
  jaccardsimilarity<- lsh_compare (candidates, corpus, jaccard_similarity, progress=TRUE)
  fwrite (jaccardsimilarity, "filepath/Bill Text Data/jaccardsimilarity101HR-106HR.csv", row.names=FALSE)
  corpus_HR_paths <- c(corpus_HR_101, corpus_HR_107)
  corpus <- TextReuseCorpus(paths=corpus_HR_paths, 
                            tokenizer = tokenize_ngrams, n = 5, 
                            minhash_func = minhash, 
                            keep_tokens = TRUE, 
                            progress = TRUE)
  buckets <- lsh(corpus, bands = 60, progress = TRUE)
  candidates <- lsh_candidates(buckets) 
  jaccardsimilarity<- lsh_compare (candidates, corpus, jaccard_similarity, progress=TRUE)
  fwrite (jaccardsimilarity, "filepath/Bill Text Data/jaccardsimilarity101HR-107HR.csv", row.names=FALSE)
  corpus_HR_paths <- c(corpus_HR_101, corpus_HR_108)
  corpus <- TextReuseCorpus(paths=corpus_HR_paths, 
                            tokenizer = tokenize_ngrams, n = 5, 
                            minhash_func = minhash, 
                            keep_tokens = TRUE, 
                            progress = TRUE)
  buckets <- lsh(corpus, bands = 60, progress = TRUE)
  candidates <- lsh_candidates(buckets) 
  jaccardsimilarity<- lsh_compare (candidates, corpus, jaccard_similarity, progress=TRUE)
  fwrite (jaccardsimilarity, "filepath/Bill Text Data/jaccardsimilarity101HR-108HR.csv", row.names=FALSE)
  corpus_HR_paths <- c(corpus_HR_101, corpus_HR_109)
  corpus <- TextReuseCorpus(paths=corpus_HR_paths, 
                            tokenizer = tokenize_ngrams, n = 5, 
                            minhash_func = minhash, 
                            keep_tokens = TRUE, 
                            progress = TRUE)
  buckets <- lsh(corpus, bands = 60, progress = TRUE)
  candidates <- lsh_candidates(buckets) 
  jaccardsimilarity<- lsh_compare (candidates, corpus, jaccard_similarity, progress=TRUE)
  fwrite (jaccardsimilarity, "filepath/Bill Text Data/jaccardsimilarity101HR-109HR.csv", row.names=FALSE)
  corpus_HR_paths <- c(corpus_HR_101, corpus_HR_110)
  corpus <- TextReuseCorpus(paths=corpus_HR_paths, 
                            tokenizer = tokenize_ngrams, n = 5, 
                            minhash_func = minhash, 
                            keep_tokens = TRUE, 
                            progress = TRUE)
  buckets <- lsh(corpus, bands = 60, progress = TRUE)
  candidates <- lsh_candidates(buckets) 
  jaccardsimilarity<- lsh_compare (candidates, corpus, jaccard_similarity, progress=TRUE)
  fwrite (jaccardsimilarity, "filepath/Bill Text Data/jaccardsimilarity101HR-110HR.csv", row.names=FALSE)
  corpus_HR_paths <- c(corpus_HR_101, corpus_HR_111)
  corpus <- TextReuseCorpus(paths=corpus_HR_paths, 
                            tokenizer = tokenize_ngrams, n = 5, 
                            minhash_func = minhash, 
                            keep_tokens = TRUE, 
                            progress = TRUE)
  buckets <- lsh(corpus, bands = 60, progress = TRUE)
  candidates <- lsh_candidates(buckets) 
  jaccardsimilarity<- lsh_compare (candidates, corpus, jaccard_similarity, progress=TRUE)
  fwrite (jaccardsimilarity, "filepath/Bill Text Data/jaccardsimilarity101HR-111HR.csv", row.names=FALSE)
  corpus_HR_paths <- c(corpus_HR_101, corpus_HR_112)
  corpus <- TextReuseCorpus(paths=corpus_HR_paths, 
                            tokenizer = tokenize_ngrams, n = 5, 
                            minhash_func = minhash, 
                            keep_tokens = TRUE, 
                            progress = TRUE)
  buckets <- lsh(corpus, bands = 60, progress = TRUE)
  candidates <- lsh_candidates(buckets) 
  jaccardsimilarity<- lsh_compare (candidates, corpus, jaccard_similarity, progress=TRUE)
  fwrite (jaccardsimilarity, "filepath/Bill Text Data/jaccardsimilarity101HR-112HR.csv", row.names=FALSE)
  corpus_HR_paths <- c(corpus_HR_101, corpus_HR_113)
  corpus <- TextReuseCorpus(paths=corpus_HR_paths, 
                            tokenizer = tokenize_ngrams, n = 5, 
                            minhash_func = minhash, 
                            keep_tokens = TRUE, 
                            progress = TRUE)
  buckets <- lsh(corpus, bands = 60, progress = TRUE)
  candidates <- lsh_candidates(buckets) 
  jaccardsimilarity<- lsh_compare (candidates, corpus, jaccard_similarity, progress=TRUE)
  fwrite (jaccardsimilarity, "filepath/Bill Text Data/jaccardsimilarity101HR-113HR.csv", row.names=FALSE)
  corpus_HR_paths <- c(corpus_HR_101, corpus_HR_114)
  corpus <- TextReuseCorpus(paths=corpus_HR_paths, 
                            tokenizer = tokenize_ngrams, n = 5, 
                            minhash_func = minhash, 
                            keep_tokens = TRUE, 
                            progress = TRUE)
  buckets <- lsh(corpus, bands = 60, progress = TRUE)
  candidates <- lsh_candidates(buckets) 
  jaccardsimilarity<- lsh_compare (candidates, corpus, jaccard_similarity, progress=TRUE)
  fwrite (jaccardsimilarity, "filepath/Bill Text Data/jaccardsimilarity101HR-114HR.csv", row.names=FALSE)
  
  #102 Corpus & Similarity Measurements
  corpus_HR_paths <- c(corpus_HR_102, corpus_HR_103)
  corpus <- TextReuseCorpus(paths=corpus_HR_paths, 
                            tokenizer = tokenize_ngrams, n = 5, 
                            minhash_func = minhash, 
                            keep_tokens = TRUE, 
                            progress = TRUE)
  buckets <- lsh(corpus, bands = 60, progress = TRUE)
  candidates <- lsh_candidates(buckets) 
  jaccardsimilarity<- lsh_compare (candidates, corpus, jaccard_similarity, progress=TRUE)
  fwrite (jaccardsimilarity, "filepath/Bill Text Data/jaccardsimilarity102HR-103HR.csv", row.names=FALSE)
  corpus_HR_paths <- c(corpus_HR_102, corpus_HR_104)
  corpus <- TextReuseCorpus(paths=corpus_HR_paths, 
                            tokenizer = tokenize_ngrams, n = 5, 
                            minhash_func = minhash, 
                            keep_tokens = TRUE, 
                            progress = TRUE)
  buckets <- lsh(corpus, bands = 60, progress = TRUE)
  candidates <- lsh_candidates(buckets) 
  jaccardsimilarity<- lsh_compare (candidates, corpus, jaccard_similarity, progress=TRUE)
  fwrite (jaccardsimilarity, "filepath/Bill Text Data/jaccardsimilarity102HR-104HR.csv", row.names=FALSE)
  corpus_HR_paths <- c(corpus_HR_102, corpus_HR_105)
  corpus <- TextReuseCorpus(paths=corpus_HR_paths, 
                            tokenizer = tokenize_ngrams, n = 5, 
                            minhash_func = minhash, 
                            keep_tokens = TRUE, 
                            progress = TRUE)
  buckets <- lsh(corpus, bands = 60, progress = TRUE)
  candidates <- lsh_candidates(buckets) 
  jaccardsimilarity<- lsh_compare (candidates, corpus, jaccard_similarity, progress=TRUE)
  fwrite (jaccardsimilarity, "filepath/Bill Text Data/jaccardsimilarity102HR-105HR.csv", row.names=FALSE)
  corpus_HR_paths <- c(corpus_HR_102, corpus_HR_106)
  corpus <- TextReuseCorpus(paths=corpus_HR_paths, 
                            tokenizer = tokenize_ngrams, n = 5, 
                            minhash_func = minhash, 
                            keep_tokens = TRUE, 
                            progress = TRUE)
  buckets <- lsh(corpus, bands = 60, progress = TRUE)
  candidates <- lsh_candidates(buckets) 
  jaccardsimilarity<- lsh_compare (candidates, corpus, jaccard_similarity, progress=TRUE)
  fwrite (jaccardsimilarity, "filepath/Bill Text Data/jaccardsimilarity102HR-106HR.csv", row.names=FALSE)
  corpus_HR_paths <- c(corpus_HR_102, corpus_HR_107)
  corpus <- TextReuseCorpus(paths=corpus_HR_paths, 
                            tokenizer = tokenize_ngrams, n = 5, 
                            minhash_func = minhash, 
                            keep_tokens = TRUE, 
                            progress = TRUE)
  buckets <- lsh(corpus, bands = 60, progress = TRUE)
  candidates <- lsh_candidates(buckets) 
  jaccardsimilarity<- lsh_compare (candidates, corpus, jaccard_similarity, progress=TRUE)
  fwrite (jaccardsimilarity, "filepath/Bill Text Data/jaccardsimilarity102HR-107HR.csv", row.names=FALSE)
  corpus_HR_paths <- c(corpus_HR_102, corpus_HR_108)
  corpus <- TextReuseCorpus(paths=corpus_HR_paths, 
                            tokenizer = tokenize_ngrams, n = 5, 
                            minhash_func = minhash, 
                            keep_tokens = TRUE, 
                            progress = TRUE)
  buckets <- lsh(corpus, bands = 60, progress = TRUE)
  candidates <- lsh_candidates(buckets) 
  jaccardsimilarity<- lsh_compare (candidates, corpus, jaccard_similarity, progress=TRUE)
  fwrite (jaccardsimilarity, "filepath/Bill Text Data/jaccardsimilarity102HR-108HR.csv", row.names=FALSE)
  corpus_HR_paths <- c(corpus_HR_102, corpus_HR_109)
  corpus <- TextReuseCorpus(paths=corpus_HR_paths, 
                            tokenizer = tokenize_ngrams, n = 5, 
                            minhash_func = minhash, 
                            keep_tokens = TRUE, 
                            progress = TRUE)
  buckets <- lsh(corpus, bands = 60, progress = TRUE)
  candidates <- lsh_candidates(buckets) 
  jaccardsimilarity<- lsh_compare (candidates, corpus, jaccard_similarity, progress=TRUE)
  fwrite (jaccardsimilarity, "filepath/Bill Text Data/jaccardsimilarity102HR-109HR.csv", row.names=FALSE)
  corpus_HR_paths <- c(corpus_HR_102, corpus_HR_110)
  corpus <- TextReuseCorpus(paths=corpus_HR_paths, 
                            tokenizer = tokenize_ngrams, n = 5, 
                            minhash_func = minhash, 
                            keep_tokens = TRUE, 
                            progress = TRUE)
  buckets <- lsh(corpus, bands = 60, progress = TRUE)
  candidates <- lsh_candidates(buckets) 
  jaccardsimilarity<- lsh_compare (candidates, corpus, jaccard_similarity, progress=TRUE)
  fwrite (jaccardsimilarity, "filepath/Bill Text Data/jaccardsimilarity102HR-110HR.csv", row.names=FALSE)
  corpus_HR_paths <- c(corpus_HR_102, corpus_HR_111)
  corpus <- TextReuseCorpus(paths=corpus_HR_paths, 
                            tokenizer = tokenize_ngrams, n = 5, 
                            minhash_func = minhash, 
                            keep_tokens = TRUE, 
                            progress = TRUE)
  buckets <- lsh(corpus, bands = 60, progress = TRUE)
  candidates <- lsh_candidates(buckets) 
  jaccardsimilarity<- lsh_compare (candidates, corpus, jaccard_similarity, progress=TRUE)
  fwrite (jaccardsimilarity, "filepath/Bill Text Data/jaccardsimilarity102HR-111HR.csv", row.names=FALSE)
  corpus_HR_paths <- c(corpus_HR_102, corpus_HR_112)
  corpus <- TextReuseCorpus(paths=corpus_HR_paths, 
                            tokenizer = tokenize_ngrams, n = 5, 
                            minhash_func = minhash, 
                            keep_tokens = TRUE, 
                            progress = TRUE)
  buckets <- lsh(corpus, bands = 60, progress = TRUE)
  candidates <- lsh_candidates(buckets) 
  jaccardsimilarity<- lsh_compare (candidates, corpus, jaccard_similarity, progress=TRUE)
  fwrite (jaccardsimilarity, "filepath/Bill Text Data/jaccardsimilarity102HR-112HR.csv", row.names=FALSE)
  corpus_HR_paths <- c(corpus_HR_102, corpus_HR_113)
  corpus <- TextReuseCorpus(paths=corpus_HR_paths, 
                            tokenizer = tokenize_ngrams, n = 5, 
                            minhash_func = minhash, 
                            keep_tokens = TRUE, 
                            progress = TRUE)
  buckets <- lsh(corpus, bands = 60, progress = TRUE)
  candidates <- lsh_candidates(buckets) 
  jaccardsimilarity<- lsh_compare (candidates, corpus, jaccard_similarity, progress=TRUE)
  fwrite (jaccardsimilarity, "filepath/Bill Text Data/jaccardsimilarity102HR-113HR.csv", row.names=FALSE)
  corpus_HR_paths <- c(corpus_HR_102, corpus_HR_114)
  corpus <- TextReuseCorpus(paths=corpus_HR_paths, 
                            tokenizer = tokenize_ngrams, n = 5, 
                            minhash_func = minhash, 
                            keep_tokens = TRUE, 
                            progress = TRUE)
  buckets <- lsh(corpus, bands = 60, progress = TRUE)
  candidates <- lsh_candidates(buckets) 
  jaccardsimilarity<- lsh_compare (candidates, corpus, jaccard_similarity, progress=TRUE)
  fwrite (jaccardsimilarity, "filepath/Bill Text Data/jaccardsimilarity102HR-114HR.csv", row.names=FALSE)
  
  #103 Corpus & Similarity Measurements
  corpus_HR_paths <- c(corpus_HR_103, corpus_HR_104)
  corpus <- TextReuseCorpus(paths=corpus_HR_paths, 
                            tokenizer = tokenize_ngrams, n = 5, 
                            minhash_func = minhash, 
                            keep_tokens = TRUE, 
                            progress = TRUE)
  buckets <- lsh(corpus, bands = 60, progress = TRUE)
  candidates <- lsh_candidates(buckets) 
  jaccardsimilarity<- lsh_compare (candidates, corpus, jaccard_similarity, progress=TRUE)
  fwrite (jaccardsimilarity, "filepath/Bill Text Data/jaccardsimilarity103HR-104HR.csv", row.names=FALSE)
  corpus_HR_paths <- c(corpus_HR_103, corpus_HR_105)
  corpus <- TextReuseCorpus(paths=corpus_HR_paths, 
                            tokenizer = tokenize_ngrams, n = 5, 
                            minhash_func = minhash, 
                            keep_tokens = TRUE, 
                            progress = TRUE)
  buckets <- lsh(corpus, bands = 60, progress = TRUE)
  candidates <- lsh_candidates(buckets) 
  jaccardsimilarity<- lsh_compare (candidates, corpus, jaccard_similarity, progress=TRUE)
  fwrite (jaccardsimilarity, "filepath/Bill Text Data/jaccardsimilarity103HR-105HR.csv", row.names=FALSE)
  corpus_HR_paths <- c(corpus_HR_103, corpus_HR_106)
  corpus <- TextReuseCorpus(paths=corpus_HR_paths, 
                            tokenizer = tokenize_ngrams, n = 5, 
                            minhash_func = minhash, 
                            keep_tokens = TRUE, 
                            progress = TRUE)
  buckets <- lsh(corpus, bands = 60, progress = TRUE)
  candidates <- lsh_candidates(buckets) 
  jaccardsimilarity<- lsh_compare (candidates, corpus, jaccard_similarity, progress=TRUE)
  fwrite (jaccardsimilarity, "filepath/Bill Text Data/jaccardsimilarity103HR-106HR.csv", row.names=FALSE)
  corpus_HR_paths <- c(corpus_HR_103, corpus_HR_107)
  corpus <- TextReuseCorpus(paths=corpus_HR_paths, 
                            tokenizer = tokenize_ngrams, n = 5, 
                            minhash_func = minhash, 
                            keep_tokens = TRUE, 
                            progress = TRUE)
  buckets <- lsh(corpus, bands = 60, progress = TRUE)
  candidates <- lsh_candidates(buckets) 
  jaccardsimilarity<- lsh_compare (candidates, corpus, jaccard_similarity, progress=TRUE)
  fwrite (jaccardsimilarity, "filepath/Bill Text Data/jaccardsimilarity103HR-107HR.csv", row.names=FALSE)
  corpus_HR_paths <- c(corpus_HR_103, corpus_HR_108)
  corpus <- TextReuseCorpus(paths=corpus_HR_paths, 
                            tokenizer = tokenize_ngrams, n = 5, 
                            minhash_func = minhash, 
                            keep_tokens = TRUE, 
                            progress = TRUE)
  buckets <- lsh(corpus, bands = 60, progress = TRUE)
  candidates <- lsh_candidates(buckets) 
  jaccardsimilarity<- lsh_compare (candidates, corpus, jaccard_similarity, progress=TRUE)
  fwrite (jaccardsimilarity, "filepath/Bill Text Data/jaccardsimilarity103HR-108HR.csv", row.names=FALSE)
  corpus_HR_paths <- c(corpus_HR_103, corpus_HR_109)
  corpus <- TextReuseCorpus(paths=corpus_HR_paths, 
                            tokenizer = tokenize_ngrams, n = 5, 
                            minhash_func = minhash, 
                            keep_tokens = TRUE, 
                            progress = TRUE)
  buckets <- lsh(corpus, bands = 60, progress = TRUE)
  candidates <- lsh_candidates(buckets) 
  jaccardsimilarity<- lsh_compare (candidates, corpus, jaccard_similarity, progress=TRUE)
  fwrite (jaccardsimilarity, "filepath/Bill Text Data/jaccardsimilarity103HR-109HR.csv", row.names=FALSE)
  corpus_HR_paths <- c(corpus_HR_103, corpus_HR_110)
  corpus <- TextReuseCorpus(paths=corpus_HR_paths, 
                            tokenizer = tokenize_ngrams, n = 5, 
                            minhash_func = minhash, 
                            keep_tokens = TRUE, 
                            progress = TRUE)
  buckets <- lsh(corpus, bands = 60, progress = TRUE)
  candidates <- lsh_candidates(buckets) 
  jaccardsimilarity<- lsh_compare (candidates, corpus, jaccard_similarity, progress=TRUE)
  fwrite (jaccardsimilarity, "filepath/Bill Text Data/jaccardsimilarity103HR-110HR.csv", row.names=FALSE)
  corpus_HR_paths <- c(corpus_HR_103, corpus_HR_111)
  corpus <- TextReuseCorpus(paths=corpus_HR_paths, 
                            tokenizer = tokenize_ngrams, n = 5, 
                            minhash_func = minhash, 
                            keep_tokens = TRUE, 
                            progress = TRUE)
  buckets <- lsh(corpus, bands = 60, progress = TRUE)
  candidates <- lsh_candidates(buckets) 
  jaccardsimilarity<- lsh_compare (candidates, corpus, jaccard_similarity, progress=TRUE)
  fwrite (jaccardsimilarity, "filepath/Bill Text Data/jaccardsimilarity103HR-111HR.csv", row.names=FALSE)
  corpus_HR_paths <- c(corpus_HR_103, corpus_HR_112)
  corpus <- TextReuseCorpus(paths=corpus_HR_paths, 
                            tokenizer = tokenize_ngrams, n = 5, 
                            minhash_func = minhash, 
                            keep_tokens = TRUE, 
                            progress = TRUE)
  buckets <- lsh(corpus, bands = 60, progress = TRUE)
  candidates <- lsh_candidates(buckets) 
  jaccardsimilarity<- lsh_compare (candidates, corpus, jaccard_similarity, progress=TRUE)
  fwrite (jaccardsimilarity, "filepath/Bill Text Data/jaccardsimilarity103HR-112HR.csv", row.names=FALSE)
  corpus_HR_paths <- c(corpus_HR_103, corpus_HR_113)
  corpus <- TextReuseCorpus(paths=corpus_HR_paths, 
                            tokenizer = tokenize_ngrams, n = 5, 
                            minhash_func = minhash, 
                            keep_tokens = TRUE, 
                            progress = TRUE)
  buckets <- lsh(corpus, bands = 60, progress = TRUE)
  candidates <- lsh_candidates(buckets) 
  jaccardsimilarity<- lsh_compare (candidates, corpus, jaccard_similarity, progress=TRUE)
  fwrite (jaccardsimilarity, "filepath/Bill Text Data/jaccardsimilarity103HR-113HR.csv", row.names=FALSE)
  corpus_HR_paths <- c(corpus_HR_103, corpus_HR_114)
  corpus <- TextReuseCorpus(paths=corpus_HR_paths, 
                            tokenizer = tokenize_ngrams, n = 5, 
                            minhash_func = minhash, 
                            keep_tokens = TRUE, 
                            progress = TRUE)
  buckets <- lsh(corpus, bands = 60, progress = TRUE)
  candidates <- lsh_candidates(buckets) 
  jaccardsimilarity<- lsh_compare (candidates, corpus, jaccard_similarity, progress=TRUE)
  fwrite (jaccardsimilarity, "filepath/Bill Text Data/jaccardsimilarity103HR-114HR.csv", row.names=FALSE)
  
  #104 Corpus & Similarity Measurements
  corpus_HR_paths <- c(corpus_HR_104, corpus_HR_105)
  corpus <- TextReuseCorpus(paths=corpus_HR_paths, 
                            tokenizer = tokenize_ngrams, n = 5, 
                            minhash_func = minhash, 
                            keep_tokens = TRUE, 
                            progress = TRUE)
  buckets <- lsh(corpus, bands = 60, progress = TRUE)
  candidates <- lsh_candidates(buckets) 
  jaccardsimilarity<- lsh_compare (candidates, corpus, jaccard_similarity, progress=TRUE)
  fwrite (jaccardsimilarity, "filepath/Bill Text Data/jaccardsimilarity104HR-105HR.csv", row.names=FALSE)
  corpus_HR_paths <- c(corpus_HR_104, corpus_HR_106)
  corpus <- TextReuseCorpus(paths=corpus_HR_paths, 
                            tokenizer = tokenize_ngrams, n = 5, 
                            minhash_func = minhash, 
                            keep_tokens = TRUE, 
                            progress = TRUE)
  buckets <- lsh(corpus, bands = 60, progress = TRUE)
  candidates <- lsh_candidates(buckets) 
  jaccardsimilarity<- lsh_compare (candidates, corpus, jaccard_similarity, progress=TRUE)
  fwrite (jaccardsimilarity, "filepath/Bill Text Data/jaccardsimilarity104HR-106HR.csv", row.names=FALSE)
  corpus_HR_paths <- c(corpus_HR_104, corpus_HR_107)
  corpus <- TextReuseCorpus(paths=corpus_HR_paths, 
                            tokenizer = tokenize_ngrams, n = 5, 
                            minhash_func = minhash, 
                            keep_tokens = TRUE, 
                            progress = TRUE)
  buckets <- lsh(corpus, bands = 60, progress = TRUE)
  candidates <- lsh_candidates(buckets) 
  jaccardsimilarity<- lsh_compare (candidates, corpus, jaccard_similarity, progress=TRUE)
  fwrite (jaccardsimilarity, "filepath/Bill Text Data/jaccardsimilarity104HR-107HR.csv", row.names=FALSE)
  corpus_HR_paths <- c(corpus_HR_104, corpus_HR_108)
  corpus <- TextReuseCorpus(paths=corpus_HR_paths, 
                            tokenizer = tokenize_ngrams, n = 5, 
                            minhash_func = minhash, 
                            keep_tokens = TRUE, 
                            progress = TRUE)
  buckets <- lsh(corpus, bands = 60, progress = TRUE)
  candidates <- lsh_candidates(buckets) 
  jaccardsimilarity<- lsh_compare (candidates, corpus, jaccard_similarity, progress=TRUE)
  fwrite (jaccardsimilarity, "filepath/Bill Text Data/jaccardsimilarity104HR-108HR.csv", row.names=FALSE)
  corpus_HR_paths <- c(corpus_HR_104, corpus_HR_109)
  corpus <- TextReuseCorpus(paths=corpus_HR_paths, 
                            tokenizer = tokenize_ngrams, n = 5, 
                            minhash_func = minhash, 
                            keep_tokens = TRUE, 
                            progress = TRUE)
  buckets <- lsh(corpus, bands = 60, progress = TRUE)
  candidates <- lsh_candidates(buckets) 
  jaccardsimilarity<- lsh_compare (candidates, corpus, jaccard_similarity, progress=TRUE)
  fwrite (jaccardsimilarity, "filepath/Bill Text Data/jaccardsimilarity104HR-109HR.csv", row.names=FALSE)
  corpus_HR_paths <- c(corpus_HR_104, corpus_HR_110)
  corpus <- TextReuseCorpus(paths=corpus_HR_paths, 
                            tokenizer = tokenize_ngrams, n = 5, 
                            minhash_func = minhash, 
                            keep_tokens = TRUE, 
                            progress = TRUE)
  buckets <- lsh(corpus, bands = 60, progress = TRUE)
  candidates <- lsh_candidates(buckets) 
  jaccardsimilarity<- lsh_compare (candidates, corpus, jaccard_similarity, progress=TRUE)
  fwrite (jaccardsimilarity, "filepath/Bill Text Data/jaccardsimilarity104HR-110HR.csv", row.names=FALSE)
  corpus_HR_paths <- c(corpus_HR_104, corpus_HR_111)
  corpus <- TextReuseCorpus(paths=corpus_HR_paths, 
                            tokenizer = tokenize_ngrams, n = 5, 
                            minhash_func = minhash, 
                            keep_tokens = TRUE, 
                            progress = TRUE)
  buckets <- lsh(corpus, bands = 60, progress = TRUE)
  candidates <- lsh_candidates(buckets) 
  jaccardsimilarity<- lsh_compare (candidates, corpus, jaccard_similarity, progress=TRUE)
  fwrite (jaccardsimilarity, "filepath/Bill Text Data/jaccardsimilarity104HR-111HR.csv", row.names=FALSE)
  corpus_HR_paths <- c(corpus_HR_104, corpus_HR_112)
  corpus <- TextReuseCorpus(paths=corpus_HR_paths, 
                            tokenizer = tokenize_ngrams, n = 5, 
                            minhash_func = minhash, 
                            keep_tokens = TRUE, 
                            progress = TRUE)
  buckets <- lsh(corpus, bands = 60, progress = TRUE)
  candidates <- lsh_candidates(buckets) 
  jaccardsimilarity<- lsh_compare (candidates, corpus, jaccard_similarity, progress=TRUE)
  fwrite (jaccardsimilarity, "filepath/Bill Text Data/jaccardsimilarity104HR-112HR.csv", row.names=FALSE)
  corpus_HR_paths <- c(corpus_HR_104, corpus_HR_113)
  corpus <- TextReuseCorpus(paths=corpus_HR_paths, 
                            tokenizer = tokenize_ngrams, n = 5, 
                            minhash_func = minhash, 
                            keep_tokens = TRUE, 
                            progress = TRUE)
  buckets <- lsh(corpus, bands = 60, progress = TRUE)
  candidates <- lsh_candidates(buckets) 
  jaccardsimilarity<- lsh_compare (candidates, corpus, jaccard_similarity, progress=TRUE)
  fwrite (jaccardsimilarity, "filepath/Bill Text Data/jaccardsimilarity104HR-113HR.csv", row.names=FALSE)
  corpus_HR_paths <- c(corpus_HR_104, corpus_HR_114)
  corpus <- TextReuseCorpus(paths=corpus_HR_paths, 
                            tokenizer = tokenize_ngrams, n = 5, 
                            minhash_func = minhash, 
                            keep_tokens = TRUE, 
                            progress = TRUE)
  buckets <- lsh(corpus, bands = 60, progress = TRUE)
  candidates <- lsh_candidates(buckets) 
  jaccardsimilarity<- lsh_compare (candidates, corpus, jaccard_similarity, progress=TRUE)
  fwrite (jaccardsimilarity, "filepath/Bill Text Data/jaccardsimilarity104HR-114HR.csv", row.names=FALSE)
  
  #105 Corpus & Similarity Measurements
  corpus_HR_paths <- c(corpus_HR_105, corpus_HR_106)
  corpus <- TextReuseCorpus(paths=corpus_HR_paths, 
                            tokenizer = tokenize_ngrams, n = 5, 
                            minhash_func = minhash, 
                            keep_tokens = TRUE, 
                            progress = TRUE)
  buckets <- lsh(corpus, bands = 60, progress = TRUE)
  candidates <- lsh_candidates(buckets) 
  jaccardsimilarity<- lsh_compare (candidates, corpus, jaccard_similarity, progress=TRUE)
  fwrite (jaccardsimilarity, "filepath/Bill Text Data/jaccardsimilarity105HR-106HR.csv", row.names=FALSE)
  corpus_HR_paths <- c(corpus_HR_105, corpus_HR_107)
  corpus <- TextReuseCorpus(paths=corpus_HR_paths, 
                            tokenizer = tokenize_ngrams, n = 5, 
                            minhash_func = minhash, 
                            keep_tokens = TRUE, 
                            progress = TRUE)
  buckets <- lsh(corpus, bands = 60, progress = TRUE)
  candidates <- lsh_candidates(buckets) 
  jaccardsimilarity<- lsh_compare (candidates, corpus, jaccard_similarity, progress=TRUE)
  fwrite (jaccardsimilarity, "filepath/Bill Text Data/jaccardsimilarity105HR-107HR.csv", row.names=FALSE)
  corpus_HR_paths <- c(corpus_HR_105, corpus_HR_108)
  corpus <- TextReuseCorpus(paths=corpus_HR_paths, 
                            tokenizer = tokenize_ngrams, n = 5, 
                            minhash_func = minhash, 
                            keep_tokens = TRUE, 
                            progress = TRUE)
  buckets <- lsh(corpus, bands = 60, progress = TRUE)
  candidates <- lsh_candidates(buckets) 
  jaccardsimilarity<- lsh_compare (candidates, corpus, jaccard_similarity, progress=TRUE)
  fwrite (jaccardsimilarity, "filepath/Bill Text Data/jaccardsimilarity105HR-108HR.csv", row.names=FALSE)
  corpus_HR_paths <- c(corpus_HR_105, corpus_HR_109)
  corpus <- TextReuseCorpus(paths=corpus_HR_paths, 
                            tokenizer = tokenize_ngrams, n = 5, 
                            minhash_func = minhash, 
                            keep_tokens = TRUE, 
                            progress = TRUE)
  buckets <- lsh(corpus, bands = 60, progress = TRUE)
  candidates <- lsh_candidates(buckets) 
  jaccardsimilarity<- lsh_compare (candidates, corpus, jaccard_similarity, progress=TRUE)
  fwrite (jaccardsimilarity, "filepath/Bill Text Data/jaccardsimilarity105HR-109HR.csv", row.names=FALSE)
  corpus_HR_paths <- c(corpus_HR_105, corpus_HR_110)
  corpus <- TextReuseCorpus(paths=corpus_HR_paths, 
                            tokenizer = tokenize_ngrams, n = 5, 
                            minhash_func = minhash, 
                            keep_tokens = TRUE, 
                            progress = TRUE)
  buckets <- lsh(corpus, bands = 60, progress = TRUE)
  candidates <- lsh_candidates(buckets) 
  jaccardsimilarity<- lsh_compare (candidates, corpus, jaccard_similarity, progress=TRUE)
  fwrite (jaccardsimilarity, "filepath/Bill Text Data/jaccardsimilarity105HR-110HR.csv", row.names=FALSE)
  corpus_HR_paths <- c(corpus_HR_105, corpus_HR_111)
  corpus <- TextReuseCorpus(paths=corpus_HR_paths, 
                            tokenizer = tokenize_ngrams, n = 5, 
                            minhash_func = minhash, 
                            keep_tokens = TRUE, 
                            progress = TRUE)
  buckets <- lsh(corpus, bands = 60, progress = TRUE)
  candidates <- lsh_candidates(buckets) 
  jaccardsimilarity<- lsh_compare (candidates, corpus, jaccard_similarity, progress=TRUE)
  fwrite (jaccardsimilarity, "filepath/Bill Text Data/jaccardsimilarity105HR-111HR.csv", row.names=FALSE)
  corpus_HR_paths <- c(corpus_HR_105, corpus_HR_112)
  corpus <- TextReuseCorpus(paths=corpus_HR_paths, 
                            tokenizer = tokenize_ngrams, n = 5, 
                            minhash_func = minhash, 
                            keep_tokens = TRUE, 
                            progress = TRUE)
  buckets <- lsh(corpus, bands = 60, progress = TRUE)
  candidates <- lsh_candidates(buckets) 
  jaccardsimilarity<- lsh_compare (candidates, corpus, jaccard_similarity, progress=TRUE)
  fwrite (jaccardsimilarity, "filepath/Bill Text Data/jaccardsimilarity105HR-112HR.csv", row.names=FALSE)
  corpus_HR_paths <- c(corpus_HR_105, corpus_HR_113)
  corpus <- TextReuseCorpus(paths=corpus_HR_paths, 
                            tokenizer = tokenize_ngrams, n = 5, 
                            minhash_func = minhash, 
                            keep_tokens = TRUE, 
                            progress = TRUE)
  buckets <- lsh(corpus, bands = 60, progress = TRUE)
  candidates <- lsh_candidates(buckets) 
  jaccardsimilarity<- lsh_compare (candidates, corpus, jaccard_similarity, progress=TRUE)
  fwrite (jaccardsimilarity, "filepath/Bill Text Data/jaccardsimilarity105HR-113HR.csv", row.names=FALSE)
  corpus_HR_paths <- c(corpus_HR_105, corpus_HR_114)
  corpus <- TextReuseCorpus(paths=corpus_HR_paths, 
                            tokenizer = tokenize_ngrams, n = 5, 
                            minhash_func = minhash, 
                            keep_tokens = TRUE, 
                            progress = TRUE)
  buckets <- lsh(corpus, bands = 60, progress = TRUE)
  candidates <- lsh_candidates(buckets) 
  jaccardsimilarity<- lsh_compare (candidates, corpus, jaccard_similarity, progress=TRUE)
  fwrite (jaccardsimilarity, "filepath/Bill Text Data/jaccardsimilarity105HR-114HR.csv", row.names=FALSE)
  
  #106 Corpus & Similarity Measurements
  corpus_HR_paths <- c(corpus_HR_106, corpus_HR_107)
  corpus <- TextReuseCorpus(paths=corpus_HR_paths, 
                            tokenizer = tokenize_ngrams, n = 5, 
                            minhash_func = minhash, 
                            keep_tokens = TRUE, 
                            progress = TRUE)
  buckets <- lsh(corpus, bands = 60, progress = TRUE)
  candidates <- lsh_candidates(buckets) 
  jaccardsimilarity<- lsh_compare (candidates, corpus, jaccard_similarity, progress=TRUE)
  fwrite (jaccardsimilarity, "filepath/Bill Text Data/jaccardsimilarity106HR-107HR.csv", row.names=FALSE)
  corpus_HR_paths <- c(corpus_HR_106, corpus_HR_108)
  corpus <- TextReuseCorpus(paths=corpus_HR_paths, 
                            tokenizer = tokenize_ngrams, n = 5, 
                            minhash_func = minhash, 
                            keep_tokens = TRUE, 
                            progress = TRUE)
  buckets <- lsh(corpus, bands = 60, progress = TRUE)
  candidates <- lsh_candidates(buckets) 
  jaccardsimilarity<- lsh_compare (candidates, corpus, jaccard_similarity, progress=TRUE)
  fwrite (jaccardsimilarity, "filepath/Bill Text Data/jaccardsimilarity106HR-108HR.csv", row.names=FALSE)
  corpus_HR_paths <- c(corpus_HR_106, corpus_HR_109)
  corpus <- TextReuseCorpus(paths=corpus_HR_paths, 
                            tokenizer = tokenize_ngrams, n = 5, 
                            minhash_func = minhash, 
                            keep_tokens = TRUE, 
                            progress = TRUE)
  buckets <- lsh(corpus, bands = 60, progress = TRUE)
  candidates <- lsh_candidates(buckets) 
  jaccardsimilarity<- lsh_compare (candidates, corpus, jaccard_similarity, progress=TRUE)
  fwrite (jaccardsimilarity, "filepath/Bill Text Data/jaccardsimilarity106HR-109HR.csv", row.names=FALSE)
  corpus_HR_paths <- c(corpus_HR_106, corpus_HR_110)
  corpus <- TextReuseCorpus(paths=corpus_HR_paths, 
                            tokenizer = tokenize_ngrams, n = 5, 
                            minhash_func = minhash, 
                            keep_tokens = TRUE, 
                            progress = TRUE)
  buckets <- lsh(corpus, bands = 60, progress = TRUE)
  candidates <- lsh_candidates(buckets) 
  jaccardsimilarity<- lsh_compare (candidates, corpus, jaccard_similarity, progress=TRUE)
  fwrite (jaccardsimilarity, "filepath/Bill Text Data/jaccardsimilarity106HR-110HR.csv", row.names=FALSE)
  corpus_HR_paths <- c(corpus_HR_106, corpus_HR_111)
  corpus <- TextReuseCorpus(paths=corpus_HR_paths, 
                            tokenizer = tokenize_ngrams, n = 5, 
                            minhash_func = minhash, 
                            keep_tokens = TRUE, 
                            progress = TRUE)
  buckets <- lsh(corpus, bands = 60, progress = TRUE)
  candidates <- lsh_candidates(buckets) 
  jaccardsimilarity<- lsh_compare (candidates, corpus, jaccard_similarity, progress=TRUE)
  fwrite (jaccardsimilarity, "filepath/Bill Text Data/jaccardsimilarity106HR-111HR.csv", row.names=FALSE)
  corpus_HR_paths <- c(corpus_HR_106, corpus_HR_112)
  corpus <- TextReuseCorpus(paths=corpus_HR_paths, 
                            tokenizer = tokenize_ngrams, n = 5, 
                            minhash_func = minhash, 
                            keep_tokens = TRUE, 
                            progress = TRUE)
  buckets <- lsh(corpus, bands = 60, progress = TRUE)
  candidates <- lsh_candidates(buckets) 
  jaccardsimilarity<- lsh_compare (candidates, corpus, jaccard_similarity, progress=TRUE)
  fwrite (jaccardsimilarity, "filepath/Bill Text Data/jaccardsimilarity106HR-112HR.csv", row.names=FALSE)
  corpus_HR_paths <- c(corpus_HR_106, corpus_HR_113)
  corpus <- TextReuseCorpus(paths=corpus_HR_paths, 
                            tokenizer = tokenize_ngrams, n = 5, 
                            minhash_func = minhash, 
                            keep_tokens = TRUE, 
                            progress = TRUE)
  buckets <- lsh(corpus, bands = 60, progress = TRUE)
  candidates <- lsh_candidates(buckets) 
  jaccardsimilarity<- lsh_compare (candidates, corpus, jaccard_similarity, progress=TRUE)
  fwrite (jaccardsimilarity, "filepath/Bill Text Data/jaccardsimilarity106HR-113HR.csv", row.names=FALSE)
  corpus_HR_paths <- c(corpus_HR_106, corpus_HR_114)
  corpus <- TextReuseCorpus(paths=corpus_HR_paths, 
                            tokenizer = tokenize_ngrams, n = 5, 
                            minhash_func = minhash, 
                            keep_tokens = TRUE, 
                            progress = TRUE)
  buckets <- lsh(corpus, bands = 60, progress = TRUE)
  candidates <- lsh_candidates(buckets) 
  jaccardsimilarity<- lsh_compare (candidates, corpus, jaccard_similarity, progress=TRUE)
  fwrite (jaccardsimilarity, "filepath/Bill Text Data/jaccardsimilarity106HR-114HR.csv", row.names=FALSE)
  
  #107 Corpus & Similarity Measurements
  corpus_HR_paths <- c(corpus_HR_107, corpus_HR_108)
  corpus <- TextReuseCorpus(paths=corpus_HR_paths, 
                            tokenizer = tokenize_ngrams, n = 5, 
                            minhash_func = minhash, 
                            keep_tokens = TRUE, 
                            progress = TRUE)
  buckets <- lsh(corpus, bands = 60, progress = TRUE)
  candidates <- lsh_candidates(buckets) 
  jaccardsimilarity<- lsh_compare (candidates, corpus, jaccard_similarity, progress=TRUE)
  fwrite (jaccardsimilarity, "filepath/Bill Text Data/jaccardsimilarity107HR-108HR.csv", row.names=FALSE)
  corpus_HR_paths <- c(corpus_HR_107, corpus_HR_109)
  corpus <- TextReuseCorpus(paths=corpus_HR_paths, 
                            tokenizer = tokenize_ngrams, n = 5, 
                            minhash_func = minhash, 
                            keep_tokens = TRUE, 
                            progress = TRUE)
  buckets <- lsh(corpus, bands = 60, progress = TRUE)
  candidates <- lsh_candidates(buckets) 
  jaccardsimilarity<- lsh_compare (candidates, corpus, jaccard_similarity, progress=TRUE)
  fwrite (jaccardsimilarity, "filepath/Bill Text Data/jaccardsimilarity107HR-109HR.csv", row.names=FALSE)
  corpus_HR_paths <- c(corpus_HR_107, corpus_HR_110)
  corpus <- TextReuseCorpus(paths=corpus_HR_paths, 
                            tokenizer = tokenize_ngrams, n = 5, 
                            minhash_func = minhash, 
                            keep_tokens = TRUE, 
                            progress = TRUE)
  buckets <- lsh(corpus, bands = 60, progress = TRUE)
  candidates <- lsh_candidates(buckets) 
  jaccardsimilarity<- lsh_compare (candidates, corpus, jaccard_similarity, progress=TRUE)
  fwrite (jaccardsimilarity, "filepath/Bill Text Data/jaccardsimilarity107HR-110HR.csv", row.names=FALSE)
  corpus_HR_paths <- c(corpus_HR_107, corpus_HR_111)
  corpus <- TextReuseCorpus(paths=corpus_HR_paths, 
                            tokenizer = tokenize_ngrams, n = 5, 
                            minhash_func = minhash, 
                            keep_tokens = TRUE, 
                            progress = TRUE)
  buckets <- lsh(corpus, bands = 60, progress = TRUE)
  candidates <- lsh_candidates(buckets) 
  jaccardsimilarity<- lsh_compare (candidates, corpus, jaccard_similarity, progress=TRUE)
  fwrite (jaccardsimilarity, "filepath/Bill Text Data/jaccardsimilarity107HR-111HR.csv", row.names=FALSE)
  corpus_HR_paths <- c(corpus_HR_107, corpus_HR_112)
  corpus <- TextReuseCorpus(paths=corpus_HR_paths, 
                            tokenizer = tokenize_ngrams, n = 5, 
                            minhash_func = minhash, 
                            keep_tokens = TRUE, 
                            progress = TRUE)
  buckets <- lsh(corpus, bands = 60, progress = TRUE)
  candidates <- lsh_candidates(buckets) 
  jaccardsimilarity<- lsh_compare (candidates, corpus, jaccard_similarity, progress=TRUE)
  fwrite (jaccardsimilarity, "filepath/Bill Text Data/jaccardsimilarity107HR-112HR.csv", row.names=FALSE)
  corpus_HR_paths <- c(corpus_HR_107, corpus_HR_113)
  corpus <- TextReuseCorpus(paths=corpus_HR_paths, 
                            tokenizer = tokenize_ngrams, n = 5, 
                            minhash_func = minhash, 
                            keep_tokens = TRUE, 
                            progress = TRUE)
  buckets <- lsh(corpus, bands = 60, progress = TRUE)
  candidates <- lsh_candidates(buckets) 
  jaccardsimilarity<- lsh_compare (candidates, corpus, jaccard_similarity, progress=TRUE)
  fwrite (jaccardsimilarity, "filepath/Bill Text Data/jaccardsimilarity107HR-113HR.csv", row.names=FALSE)
  corpus_HR_paths <- c(corpus_HR_107, corpus_HR_114)
  corpus <- TextReuseCorpus(paths=corpus_HR_paths, 
                            tokenizer = tokenize_ngrams, n = 5, 
                            minhash_func = minhash, 
                            keep_tokens = TRUE, 
                            progress = TRUE)
  buckets <- lsh(corpus, bands = 60, progress = TRUE)
  candidates <- lsh_candidates(buckets) 
  jaccardsimilarity<- lsh_compare (candidates, corpus, jaccard_similarity, progress=TRUE)
  fwrite (jaccardsimilarity, "filepath/Bill Text Data/jaccardsimilarity107HR-114HR.csv", row.names=FALSE)
  
  #108 Corpus & Similarity Measurements
  corpus_HR_paths <- c(corpus_HR_108, corpus_HR_109)
  corpus <- TextReuseCorpus(paths=corpus_HR_paths, 
                            tokenizer = tokenize_ngrams, n = 5, 
                            minhash_func = minhash, 
                            keep_tokens = TRUE, 
                            progress = TRUE)
  buckets <- lsh(corpus, bands = 60, progress = TRUE)
  candidates <- lsh_candidates(buckets) 
  jaccardsimilarity<- lsh_compare (candidates, corpus, jaccard_similarity, progress=TRUE)
  fwrite (jaccardsimilarity, "filepath/Bill Text Data/jaccardsimilarity108HR-109HR.csv", row.names=FALSE)
  corpus_HR_paths <- c(corpus_HR_108, corpus_HR_110)
  corpus <- TextReuseCorpus(paths=corpus_HR_paths, 
                            tokenizer = tokenize_ngrams, n = 5, 
                            minhash_func = minhash, 
                            keep_tokens = TRUE, 
                            progress = TRUE)
  buckets <- lsh(corpus, bands = 60, progress = TRUE)
  candidates <- lsh_candidates(buckets) 
  jaccardsimilarity<- lsh_compare (candidates, corpus, jaccard_similarity, progress=TRUE)
  fwrite (jaccardsimilarity, "filepath/Bill Text Data/jaccardsimilarity108HR-110HR.csv", row.names=FALSE)
  corpus_HR_paths <- c(corpus_HR_108, corpus_HR_111)
  corpus <- TextReuseCorpus(paths=corpus_HR_paths, 
                            tokenizer = tokenize_ngrams, n = 5, 
                            minhash_func = minhash, 
                            keep_tokens = TRUE, 
                            progress = TRUE)
  buckets <- lsh(corpus, bands = 60, progress = TRUE)
  candidates <- lsh_candidates(buckets) 
  jaccardsimilarity<- lsh_compare (candidates, corpus, jaccard_similarity, progress=TRUE)
  fwrite (jaccardsimilarity, "filepath/Bill Text Data/jaccardsimilarity108HR-111HR.csv", row.names=FALSE)
  corpus_HR_paths <- c(corpus_HR_108, corpus_HR_112)
  corpus <- TextReuseCorpus(paths=corpus_HR_paths, 
                            tokenizer = tokenize_ngrams, n = 5, 
                            minhash_func = minhash, 
                            keep_tokens = TRUE, 
                            progress = TRUE)
  buckets <- lsh(corpus, bands = 60, progress = TRUE)
  candidates <- lsh_candidates(buckets) 
  jaccardsimilarity<- lsh_compare (candidates, corpus, jaccard_similarity, progress=TRUE)
  fwrite (jaccardsimilarity, "filepath/Bill Text Data/jaccardsimilarity108HR-112HR.csv", row.names=FALSE)
  corpus_HR_paths <- c(corpus_HR_108, corpus_HR_113)
  corpus <- TextReuseCorpus(paths=corpus_HR_paths, 
                            tokenizer = tokenize_ngrams, n = 5, 
                            minhash_func = minhash, 
                            keep_tokens = TRUE, 
                            progress = TRUE)
  buckets <- lsh(corpus, bands = 60, progress = TRUE)
  candidates <- lsh_candidates(buckets) 
  jaccardsimilarity<- lsh_compare (candidates, corpus, jaccard_similarity, progress=TRUE)
  fwrite (jaccardsimilarity, "filepath/Bill Text Data/jaccardsimilarity108HR-113HR.csv", row.names=FALSE)
  corpus_HR_paths <- c(corpus_HR_108, corpus_HR_114)
  corpus <- TextReuseCorpus(paths=corpus_HR_paths, 
                            tokenizer = tokenize_ngrams, n = 5, 
                            minhash_func = minhash, 
                            keep_tokens = TRUE, 
                            progress = TRUE)
  buckets <- lsh(corpus, bands = 60, progress = TRUE)
  candidates <- lsh_candidates(buckets) 
  jaccardsimilarity<- lsh_compare (candidates, corpus, jaccard_similarity, progress=TRUE)
  fwrite (jaccardsimilarity, "filepath/Bill Text Data/jaccardsimilarity108HR-114HR.csv", row.names=FALSE)
  
  #109 Corpus & Similarity Measurements
  corpus_HR_paths <- c(corpus_HR_109, corpus_HR_110)
  corpus <- TextReuseCorpus(paths=corpus_HR_paths, 
                            tokenizer = tokenize_ngrams, n = 5, 
                            minhash_func = minhash, 
                            keep_tokens = TRUE, 
                            progress = TRUE)
  buckets <- lsh(corpus, bands = 60, progress = TRUE)
  candidates <- lsh_candidates(buckets) 
  jaccardsimilarity<- lsh_compare (candidates, corpus, jaccard_similarity, progress=TRUE)
  fwrite (jaccardsimilarity, "filepath/Bill Text Data/jaccardsimilarity109HR-110HR.csv", row.names=FALSE)
  corpus_HR_paths <- c(corpus_HR_109, corpus_HR_111)
  corpus <- TextReuseCorpus(paths=corpus_HR_paths, 
                            tokenizer = tokenize_ngrams, n = 5, 
                            minhash_func = minhash, 
                            keep_tokens = TRUE, 
                            progress = TRUE)
  buckets <- lsh(corpus, bands = 60, progress = TRUE)
  candidates <- lsh_candidates(buckets) 
  jaccardsimilarity<- lsh_compare (candidates, corpus, jaccard_similarity, progress=TRUE)
  fwrite (jaccardsimilarity, "filepath/Bill Text Data/jaccardsimilarity109HR-111HR.csv", row.names=FALSE)
  corpus_HR_paths <- c(corpus_HR_109, corpus_HR_112)
  corpus <- TextReuseCorpus(paths=corpus_HR_paths, 
                            tokenizer = tokenize_ngrams, n = 5, 
                            minhash_func = minhash, 
                            keep_tokens = TRUE, 
                            progress = TRUE)
  buckets <- lsh(corpus, bands = 60, progress = TRUE)
  candidates <- lsh_candidates(buckets) 
  jaccardsimilarity<- lsh_compare (candidates, corpus, jaccard_similarity, progress=TRUE)
  fwrite (jaccardsimilarity, "filepath/Bill Text Data/jaccardsimilarity109HR-112HR.csv", row.names=FALSE)
  corpus_HR_paths <- c(corpus_HR_109, corpus_HR_113)
  corpus <- TextReuseCorpus(paths=corpus_HR_paths, 
                            tokenizer = tokenize_ngrams, n = 5, 
                            minhash_func = minhash, 
                            keep_tokens = TRUE, 
                            progress = TRUE)
  buckets <- lsh(corpus, bands = 60, progress = TRUE)
  candidates <- lsh_candidates(buckets) 
  jaccardsimilarity<- lsh_compare (candidates, corpus, jaccard_similarity, progress=TRUE)
  fwrite (jaccardsimilarity, "filepath/Bill Text Data/jaccardsimilarity109HR-113HR.csv", row.names=FALSE)
  corpus_HR_paths <- c(corpus_HR_109, corpus_HR_114)
  corpus <- TextReuseCorpus(paths=corpus_HR_paths, 
                            tokenizer = tokenize_ngrams, n = 5, 
                            minhash_func = minhash, 
                            keep_tokens = TRUE, 
                            progress = TRUE)
  buckets <- lsh(corpus, bands = 60, progress = TRUE)
  candidates <- lsh_candidates(buckets) 
  jaccardsimilarity<- lsh_compare (candidates, corpus, jaccard_similarity, progress=TRUE)
  fwrite (jaccardsimilarity, "filepath/Bill Text Data/jaccardsimilarity109HR-114HR.csv", row.names=FALSE)
  
  #110 Corpus & Similarity Measurements
  corpus_HR_paths <- c(corpus_HR_110, corpus_HR_111)
  corpus <- TextReuseCorpus(paths=corpus_HR_paths, 
                            tokenizer = tokenize_ngrams, n = 5, 
                            minhash_func = minhash, 
                            keep_tokens = TRUE, 
                            progress = TRUE)
  buckets <- lsh(corpus, bands = 60, progress = TRUE)
  candidates <- lsh_candidates(buckets) 
  jaccardsimilarity<- lsh_compare (candidates, corpus, jaccard_similarity, progress=TRUE)
  fwrite (jaccardsimilarity, "filepath/Bill Text Data/jaccardsimilarity110HR-111HR.csv", row.names=FALSE)
  corpus_HR_paths <- c(corpus_HR_110, corpus_HR_112)
  corpus <- TextReuseCorpus(paths=corpus_HR_paths, 
                            tokenizer = tokenize_ngrams, n = 5, 
                            minhash_func = minhash, 
                            keep_tokens = TRUE, 
                            progress = TRUE)
  buckets <- lsh(corpus, bands = 60, progress = TRUE)
  candidates <- lsh_candidates(buckets) 
  jaccardsimilarity<- lsh_compare (candidates, corpus, jaccard_similarity, progress=TRUE)
  fwrite (jaccardsimilarity, "filepath/Bill Text Data/jaccardsimilarity110HR-112HR.csv", row.names=FALSE)
  corpus_HR_paths <- c(corpus_HR_110, corpus_HR_113)
  corpus <- TextReuseCorpus(paths=corpus_HR_paths, 
                            tokenizer = tokenize_ngrams, n = 5, 
                            minhash_func = minhash, 
                            keep_tokens = TRUE, 
                            progress = TRUE)
  buckets <- lsh(corpus, bands = 60, progress = TRUE)
  candidates <- lsh_candidates(buckets) 
  jaccardsimilarity<- lsh_compare (candidates, corpus, jaccard_similarity, progress=TRUE)
  fwrite (jaccardsimilarity, "filepath/Bill Text Data/jaccardsimilarity110HR-113HR.csv", row.names=FALSE)
  corpus_HR_paths <- c(corpus_HR_110, corpus_HR_114)
  corpus <- TextReuseCorpus(paths=corpus_HR_paths, 
                            tokenizer = tokenize_ngrams, n = 5, 
                            minhash_func = minhash, 
                            keep_tokens = TRUE, 
                            progress = TRUE)
  buckets <- lsh(corpus, bands = 60, progress = TRUE)
  candidates <- lsh_candidates(buckets) 
  jaccardsimilarity<- lsh_compare (candidates, corpus, jaccard_similarity, progress=TRUE)
  fwrite (jaccardsimilarity, "filepath/Bill Text Data/jaccardsimilarity110HR-114HR.csv", row.names=FALSE)
  
  #111 Corpus & Similarity Measurements
  corpus_HR_paths <- c(corpus_HR_111, corpus_HR_112)
  corpus <- TextReuseCorpus(paths=corpus_HR_paths, 
                            tokenizer = tokenize_ngrams, n = 5, 
                            minhash_func = minhash, 
                            keep_tokens = TRUE, 
                            progress = TRUE)
  buckets <- lsh(corpus, bands = 60, progress = TRUE)
  candidates <- lsh_candidates(buckets) 
  jaccardsimilarity<- lsh_compare (candidates, corpus, jaccard_similarity, progress=TRUE)
  fwrite (jaccardsimilarity, "filepath/Bill Text Data/jaccardsimilarity111HR-112HR.csv", row.names=FALSE)
  corpus_HR_paths <- c(corpus_HR_111, corpus_HR_113)
  corpus <- TextReuseCorpus(paths=corpus_HR_paths, 
                            tokenizer = tokenize_ngrams, n = 5, 
                            minhash_func = minhash, 
                            keep_tokens = TRUE, 
                            progress = TRUE)
  buckets <- lsh(corpus, bands = 60, progress = TRUE)
  candidates <- lsh_candidates(buckets) 
  jaccardsimilarity<- lsh_compare (candidates, corpus, jaccard_similarity, progress=TRUE)
  fwrite (jaccardsimilarity, "filepath/Bill Text Data/jaccardsimilarity111HR-113HR.csv", row.names=FALSE)
  corpus_HR_paths <- c(corpus_HR_111, corpus_HR_114)
  corpus <- TextReuseCorpus(paths=corpus_HR_paths, 
                            tokenizer = tokenize_ngrams, n = 5, 
                            minhash_func = minhash, 
                            keep_tokens = TRUE, 
                            progress = TRUE)
  buckets <- lsh(corpus, bands = 60, progress = TRUE)
  candidates <- lsh_candidates(buckets) 
  jaccardsimilarity<- lsh_compare (candidates, corpus, jaccard_similarity, progress=TRUE)
  fwrite (jaccardsimilarity, "filepath/Bill Text Data/jaccardsimilarity111HR-114HR.csv", row.names=FALSE)
  
  #112 Corpus & Similarity Measurements
  corpus_HR_paths <- c(corpus_HR_112, corpus_HR_113)
  corpus <- TextReuseCorpus(paths=corpus_HR_paths, 
                            tokenizer = tokenize_ngrams, n = 5, 
                            minhash_func = minhash, 
                            keep_tokens = TRUE, 
                            progress = TRUE)
  buckets <- lsh(corpus, bands = 60, progress = TRUE)
  candidates <- lsh_candidates(buckets) 
  jaccardsimilarity<- lsh_compare (candidates, corpus, jaccard_similarity, progress=TRUE)
  fwrite (jaccardsimilarity, "filepath/Bill Text Data/jaccardsimilarity112HR-113HR.csv", row.names=FALSE)
  corpus_HR_paths <- c(corpus_HR_112, corpus_HR_114)
  corpus <- TextReuseCorpus(paths=corpus_HR_paths, 
                            tokenizer = tokenize_ngrams, n = 5, 
                            minhash_func = minhash, 
                            keep_tokens = TRUE, 
                            progress = TRUE)
  buckets <- lsh(corpus, bands = 60, progress = TRUE)
  candidates <- lsh_candidates(buckets) 
  jaccardsimilarity<- lsh_compare (candidates, corpus, jaccard_similarity, progress=TRUE)
  fwrite (jaccardsimilarity, "filepath/Bill Text Data/jaccardsimilarity112HR-114HR.csv", row.names=FALSE)
  
  #113 Corpus & Similarity Measurements
  corpus_HR_paths <- c(corpus_HR_113, corpus_HR_114)
  corpus <- TextReuseCorpus(paths=corpus_HR_paths, 
                            tokenizer = tokenize_ngrams, n = 5, 
                            minhash_func = minhash, 
                            keep_tokens = TRUE, 
                            progress = TRUE)
  buckets <- lsh(corpus, bands = 60, progress = TRUE)
  candidates <- lsh_candidates(buckets) 
  jaccardsimilarity<- lsh_compare (candidates, corpus, jaccard_similarity, progress=TRUE)
  fwrite (jaccardsimilarity, "filepath/Bill Text Data/jaccardsimilarity113HR-114HR.csv", row.names=FALSE)
  
  
  #### S - Within Senate Comparisons####
  #By Congress
  corpus_S_101<-list.files(path ="filepath/Bill Text Corpus/Bill Type/S/101/",pattern="*.txt",full.names = TRUE)
  corpus_S_102<-list.files(path ="filepath/Bill Text Corpus/Bill Type/S/102",pattern="*.txt",full.names = TRUE)
  corpus_S_103<-list.files(path ="filepath/Bill Text Corpus/Bill Type/S/103/",pattern="*.txt",full.names = TRUE)
  corpus_S_104<-list.files(path ="filepath/Bill Text Corpus/Bill Type/S/104/",pattern="*.txt",full.names = TRUE)
  corpus_S_105<-list.files(path ="filepath/Bill Text Corpus/Bill Type/S/105/",pattern="*.txt",full.names = TRUE)
  corpus_S_106<-list.files(path ="filepath/Bill Text Corpus/Bill Type/S/106/",pattern="*.txt",full.names = TRUE)
  corpus_S_107<-list.files(path ="filepath/Bill Text Corpus/Bill Type/S/107/",pattern="*.txt",full.names = TRUE)
  corpus_S_108<-list.files(path ="filepath/Bill Text Corpus/Bill Type/S/108/",pattern="*.txt",full.names = TRUE)
  corpus_S_109<-list.files(path ="filepath/Bill Text Corpus/Bill Type/S/109/",pattern="*.txt",full.names = TRUE)
  corpus_S_110<-list.files(path ="filepath/Bill Text Corpus/Bill Type/S/110/",pattern="*.txt",full.names = TRUE)
  corpus_S_111<-list.files(path ="filepath/Bill Text Corpus/Bill Type/S/111/",pattern="*.txt",full.names = TRUE)
  corpus_S_112<-list.files(path ="filepath/Bill Text Corpus/Bill Type/S/112/",pattern="*.txt",full.names = TRUE)
  corpus_S_113<-list.files(path ="filepath/Bill Text Corpus/Bill Type/S/113/",pattern="*.txt",full.names = TRUE)
  corpus_S_114<-list.files(path ="filepath/Bill Text Corpus/Bill Type/S/114/",pattern="*.txt",full.names = TRUE)
  #Create Minhash Function for Locality Sensitive Hashing (LSH) - SAME FOR ALL COMPARISONS
  minhash <- minhash_generator(n= 240, seed = 3352)
  
  #101 Corpus & Similarity Measurements
  corpus_S_paths <- c(corpus_S_101, corpus_S_102)
  corpus <- TextReuseCorpus(paths=corpus_S_paths, 
                            tokenizer = tokenize_ngrams, n = 5, 
                            minhash_func = minhash, 
                            keep_tokens = TRUE, 
                            progress = TRUE)
  candidates <- lsh_candidates(buckets) 
  jaccardsimilarity<- lsh_compare (candidates, corpus, jaccard_similarity, progress=TRUE)
  fwrite (jaccardsimilarity, "filepath/Bill Text Data/jaccardsimilarity101S-102S.csv", row.names=FALSE)
  corpus_S_paths <- c(corpus_S_101, corpus_S_103)
  corpus <- TextReuseCorpus(paths=corpus_S_paths, 
                            tokenizer = tokenize_ngrams, n = 5, 
                            minhash_func = minhash, 
                            keep_tokens = TRUE, 
                            progress = TRUE)
  buckets <- lsh(corpus, bands = 60, progress = TRUE)
  candidates <- lsh_candidates(buckets) 
  jaccardsimilarity<- lsh_compare (candidates, corpus, jaccard_similarity, progress=TRUE)
  fwrite (jaccardsimilarity, "filepath/Bill Text Data/jaccardsimilarity101S-103S.csv", row.names=FALSE)
  corpus_S_paths <- c(corpus_S_101, corpus_S_104)
  corpus <- TextReuseCorpus(paths=corpus_S_paths, 
                            tokenizer = tokenize_ngrams, n = 5, 
                            minhash_func = minhash, 
                            keep_tokens = TRUE, 
                            progress = TRUE)
  buckets <- lsh(corpus, bands = 60, progress = TRUE)
  candidates <- lsh_candidates(buckets) 
  jaccardsimilarity<- lsh_compare (candidates, corpus, jaccard_similarity, progress=TRUE)
  fwrite (jaccardsimilarity, "filepath/Bill Text Data/jaccardsimilarity101S-104S.csv", row.names=FALSE)
  corpus_S_paths <- c(corpus_S_101, corpus_S_105)
  corpus <- TextReuseCorpus(paths=corpus_S_paths, 
                            tokenizer = tokenize_ngrams, n = 5, 
                            minhash_func = minhash, 
                            keep_tokens = TRUE, 
                            progress = TRUE)
  buckets <- lsh(corpus, bands = 60, progress = TRUE)
  candidates <- lsh_candidates(buckets) 
  jaccardsimilarity<- lsh_compare (candidates, corpus, jaccard_similarity, progress=TRUE)
  fwrite (jaccardsimilarity, "filepath/Bill Text Data/jaccardsimilarity101S-105S.csv", row.names=FALSE)
  corpus_S_paths <- c(corpus_S_101, corpus_S_106)
  corpus <- TextReuseCorpus(paths=corpus_S_paths, 
                            tokenizer = tokenize_ngrams, n = 5, 
                            minhash_func = minhash, 
                            keep_tokens = TRUE, 
                            progress = TRUE)
  buckets <- lsh(corpus, bands = 60, progress = TRUE)
  candidates <- lsh_candidates(buckets) 
  jaccardsimilarity<- lsh_compare (candidates, corpus, jaccard_similarity, progress=TRUE)
  fwrite (jaccardsimilarity, "filepath/Bill Text Data/jaccardsimilarity101S-106S.csv", row.names=FALSE)
  corpus_S_paths <- c(corpus_S_101, corpus_S_107)
  corpus <- TextReuseCorpus(paths=corpus_S_paths, 
                            tokenizer = tokenize_ngrams, n = 5, 
                            minhash_func = minhash, 
                            keep_tokens = TRUE, 
                            progress = TRUE)
  buckets <- lsh(corpus, bands = 60, progress = TRUE)
  candidates <- lsh_candidates(buckets) 
  jaccardsimilarity<- lsh_compare (candidates, corpus, jaccard_similarity, progress=TRUE)
  fwrite (jaccardsimilarity, "filepath/Bill Text Data/jaccardsimilarity101S-107S.csv", row.names=FALSE)
  corpus_S_paths <- c(corpus_S_101, corpus_S_108)
  corpus <- TextReuseCorpus(paths=corpus_S_paths, 
                            tokenizer = tokenize_ngrams, n = 5, 
                            minhash_func = minhash, 
                            keep_tokens = TRUE, 
                            progress = TRUE)
  buckets <- lsh(corpus, bands = 60, progress = TRUE)
  candidates <- lsh_candidates(buckets) 
  jaccardsimilarity<- lsh_compare (candidates, corpus, jaccard_similarity, progress=TRUE)
  fwrite (jaccardsimilarity, "filepath/Bill Text Data/jaccardsimilarity101S-108S.csv", row.names=FALSE)
  corpus_S_paths <- c(corpus_S_101, corpus_S_109)
  corpus <- TextReuseCorpus(paths=corpus_S_paths, 
                            tokenizer = tokenize_ngrams, n = 5, 
                            minhash_func = minhash, 
                            keep_tokens = TRUE, 
                            progress = TRUE)
  buckets <- lsh(corpus, bands = 60, progress = TRUE)
  candidates <- lsh_candidates(buckets) 
  jaccardsimilarity<- lsh_compare (candidates, corpus, jaccard_similarity, progress=TRUE)
  fwrite (jaccardsimilarity, "filepath/Bill Text Data/jaccardsimilarity101S-109S.csv", row.names=FALSE)
  corpus_S_paths <- c(corpus_S_101, corpus_S_110)
  corpus <- TextReuseCorpus(paths=corpus_S_paths, 
                            tokenizer = tokenize_ngrams, n = 5, 
                            minhash_func = minhash, 
                            keep_tokens = TRUE, 
                            progress = TRUE)
  buckets <- lsh(corpus, bands = 60, progress = TRUE)
  candidates <- lsh_candidates(buckets) 
  jaccardsimilarity<- lsh_compare (candidates, corpus, jaccard_similarity, progress=TRUE)
  fwrite (jaccardsimilarity, "filepath/Bill Text Data/jaccardsimilarity101S-110S.csv", row.names=FALSE)
  corpus_S_paths <- c(corpus_S_101, corpus_S_111)
  corpus <- TextReuseCorpus(paths=corpus_S_paths, 
                            tokenizer = tokenize_ngrams, n = 5, 
                            minhash_func = minhash, 
                            keep_tokens = TRUE, 
                            progress = TRUE)
  buckets <- lsh(corpus, bands = 60, progress = TRUE)
  candidates <- lsh_candidates(buckets) 
  jaccardsimilarity<- lsh_compare (candidates, corpus, jaccard_similarity, progress=TRUE)
  fwrite (jaccardsimilarity, "filepath/Bill Text Data/jaccardsimilarity101S-111S.csv", row.names=FALSE)
  corpus_S_paths <- c(corpus_S_101, corpus_S_112)
  corpus <- TextReuseCorpus(paths=corpus_S_paths, 
                            tokenizer = tokenize_ngrams, n = 5, 
                            minhash_func = minhash, 
                            keep_tokens = TRUE, 
                            progress = TRUE)
  buckets <- lsh(corpus, bands = 60, progress = TRUE)
  candidates <- lsh_candidates(buckets) 
  jaccardsimilarity<- lsh_compare (candidates, corpus, jaccard_similarity, progress=TRUE)
  fwrite (jaccardsimilarity, "filepath/Bill Text Data/jaccardsimilarity101S-112S.csv", row.names=FALSE)
  corpus_S_paths <- c(corpus_S_101, corpus_S_113)
  corpus <- TextReuseCorpus(paths=corpus_S_paths, 
                            tokenizer = tokenize_ngrams, n = 5, 
                            minhash_func = minhash, 
                            keep_tokens = TRUE, 
                            progress = TRUE)
  buckets <- lsh(corpus, bands = 60, progress = TRUE)
  candidates <- lsh_candidates(buckets) 
  jaccardsimilarity<- lsh_compare (candidates, corpus, jaccard_similarity, progress=TRUE)
  fwrite (jaccardsimilarity, "filepath/Bill Text Data/jaccardsimilarity101S-113S.csv", row.names=FALSE)
  corpus_S_paths <- c(corpus_S_101, corpus_S_114)
  corpus <- TextReuseCorpus(paths=corpus_S_paths, 
                            tokenizer = tokenize_ngrams, n = 5, 
                            minhash_func = minhash, 
                            keep_tokens = TRUE, 
                            progress = TRUE)
  buckets <- lsh(corpus, bands = 60, progress = TRUE)
  candidates <- lsh_candidates(buckets) 
  jaccardsimilarity<- lsh_compare (candidates, corpus, jaccard_similarity, progress=TRUE)
  fwrite (jaccardsimilarity, "filepath/Bill Text Data/jaccardsimilarity101S-114S.csv", row.names=FALSE)
  
  #102 Corpus & Similarity Measurements
  corpus_S_paths <- c(corpus_S_102, corpus_S_103)
  corpus <- TextReuseCorpus(paths=corpus_S_paths, 
                            tokenizer = tokenize_ngrams, n = 5, 
                            minhash_func = minhash, 
                            keep_tokens = TRUE, 
                            progress = TRUE)
  buckets <- lsh(corpus, bands = 60, progress = TRUE)
  candidates <- lsh_candidates(buckets) 
  jaccardsimilarity<- lsh_compare (candidates, corpus, jaccard_similarity, progress=TRUE)
  fwrite (jaccardsimilarity, "filepath/Bill Text Data/jaccardsimilarity102S-103S.csv", row.names=FALSE)
  corpus_S_paths <- c(corpus_S_102, corpus_S_104)
  corpus <- TextReuseCorpus(paths=corpus_S_paths, 
                            tokenizer = tokenize_ngrams, n = 5, 
                            minhash_func = minhash, 
                            keep_tokens = TRUE, 
                            progress = TRUE)
  buckets <- lsh(corpus, bands = 60, progress = TRUE)
  candidates <- lsh_candidates(buckets) 
  jaccardsimilarity<- lsh_compare (candidates, corpus, jaccard_similarity, progress=TRUE)
  fwrite (jaccardsimilarity, "filepath/Bill Text Data/jaccardsimilarity102S-104S.csv", row.names=FALSE)
  corpus_S_paths <- c(corpus_S_102, corpus_S_105)
  corpus <- TextReuseCorpus(paths=corpus_S_paths, 
                            tokenizer = tokenize_ngrams, n = 5, 
                            minhash_func = minhash, 
                            keep_tokens = TRUE, 
                            progress = TRUE)
  buckets <- lsh(corpus, bands = 60, progress = TRUE)
  candidates <- lsh_candidates(buckets) 
  jaccardsimilarity<- lsh_compare (candidates, corpus, jaccard_similarity, progress=TRUE)
  fwrite (jaccardsimilarity, "filepath/Bill Text Data/jaccardsimilarity102S-105S.csv", row.names=FALSE)
  corpus_S_paths <- c(corpus_S_102, corpus_S_106)
  corpus <- TextReuseCorpus(paths=corpus_S_paths, 
                            tokenizer = tokenize_ngrams, n = 5, 
                            minhash_func = minhash, 
                            keep_tokens = TRUE, 
                            progress = TRUE)
  buckets <- lsh(corpus, bands = 60, progress = TRUE)
  candidates <- lsh_candidates(buckets) 
  jaccardsimilarity<- lsh_compare (candidates, corpus, jaccard_similarity, progress=TRUE)
  fwrite (jaccardsimilarity, "filepath/Bill Text Data/jaccardsimilarity102S-106S.csv", row.names=FALSE)
  corpus_S_paths <- c(corpus_S_102, corpus_S_107)
  corpus <- TextReuseCorpus(paths=corpus_S_paths, 
                            tokenizer = tokenize_ngrams, n = 5, 
                            minhash_func = minhash, 
                            keep_tokens = TRUE, 
                            progress = TRUE)
  buckets <- lsh(corpus, bands = 60, progress = TRUE)
  candidates <- lsh_candidates(buckets) 
  jaccardsimilarity<- lsh_compare (candidates, corpus, jaccard_similarity, progress=TRUE)
  fwrite (jaccardsimilarity, "filepath/Bill Text Data/jaccardsimilarity102S-107S.csv", row.names=FALSE)
  corpus_S_paths <- c(corpus_S_102, corpus_S_108)
  corpus <- TextReuseCorpus(paths=corpus_S_paths, 
                            tokenizer = tokenize_ngrams, n = 5, 
                            minhash_func = minhash, 
                            keep_tokens = TRUE, 
                            progress = TRUE)
  buckets <- lsh(corpus, bands = 60, progress = TRUE)
  candidates <- lsh_candidates(buckets) 
  jaccardsimilarity<- lsh_compare (candidates, corpus, jaccard_similarity, progress=TRUE)
  fwrite (jaccardsimilarity, "filepath/Bill Text Data/jaccardsimilarity102S-108S.csv", row.names=FALSE)
  corpus_S_paths <- c(corpus_S_102, corpus_S_109)
  corpus <- TextReuseCorpus(paths=corpus_S_paths, 
                            tokenizer = tokenize_ngrams, n = 5, 
                            minhash_func = minhash, 
                            keep_tokens = TRUE, 
                            progress = TRUE)
  buckets <- lsh(corpus, bands = 60, progress = TRUE)
  candidates <- lsh_candidates(buckets) 
  jaccardsimilarity<- lsh_compare (candidates, corpus, jaccard_similarity, progress=TRUE)
  fwrite (jaccardsimilarity, "filepath/Bill Text Data/jaccardsimilarity102S-109S.csv", row.names=FALSE)
  corpus_S_paths <- c(corpus_S_102, corpus_S_110)
  corpus <- TextReuseCorpus(paths=corpus_S_paths, 
                            tokenizer = tokenize_ngrams, n = 5, 
                            minhash_func = minhash, 
                            keep_tokens = TRUE, 
                            progress = TRUE)
  buckets <- lsh(corpus, bands = 60, progress = TRUE)
  candidates <- lsh_candidates(buckets) 
  jaccardsimilarity<- lsh_compare (candidates, corpus, jaccard_similarity, progress=TRUE)
  fwrite (jaccardsimilarity, "filepath/Bill Text Data/jaccardsimilarity102S-110S.csv", row.names=FALSE)
  corpus_S_paths <- c(corpus_S_102, corpus_S_111)
  corpus <- TextReuseCorpus(paths=corpus_S_paths, 
                            tokenizer = tokenize_ngrams, n = 5, 
                            minhash_func = minhash, 
                            keep_tokens = TRUE, 
                            progress = TRUE)
  buckets <- lsh(corpus, bands = 60, progress = TRUE)
  candidates <- lsh_candidates(buckets) 
  jaccardsimilarity<- lsh_compare (candidates, corpus, jaccard_similarity, progress=TRUE)
  fwrite (jaccardsimilarity, "filepath/Bill Text Data/jaccardsimilarity102S-111S.csv", row.names=FALSE)
  corpus_S_paths <- c(corpus_S_102, corpus_S_112)
  corpus <- TextReuseCorpus(paths=corpus_S_paths, 
                            tokenizer = tokenize_ngrams, n = 5, 
                            minhash_func = minhash, 
                            keep_tokens = TRUE, 
                            progress = TRUE)
  buckets <- lsh(corpus, bands = 60, progress = TRUE)
  candidates <- lsh_candidates(buckets) 
  jaccardsimilarity<- lsh_compare (candidates, corpus, jaccard_similarity, progress=TRUE)
  fwrite (jaccardsimilarity, "filepath/Bill Text Data/jaccardsimilarity102S-112S.csv", row.names=FALSE)
  corpus_S_paths <- c(corpus_S_102, corpus_S_113)
  corpus <- TextReuseCorpus(paths=corpus_S_paths, 
                            tokenizer = tokenize_ngrams, n = 5, 
                            minhash_func = minhash, 
                            keep_tokens = TRUE, 
                            progress = TRUE)
  buckets <- lsh(corpus, bands = 60, progress = TRUE)
  candidates <- lsh_candidates(buckets) 
  jaccardsimilarity<- lsh_compare (candidates, corpus, jaccard_similarity, progress=TRUE)
  fwrite (jaccardsimilarity, "filepath/Bill Text Data/jaccardsimilarity102S-113S.csv", row.names=FALSE)
  corpus_S_paths <- c(corpus_S_102, corpus_S_114)
  corpus <- TextReuseCorpus(paths=corpus_S_paths, 
                            tokenizer = tokenize_ngrams, n = 5, 
                            minhash_func = minhash, 
                            keep_tokens = TRUE, 
                            progress = TRUE)
  buckets <- lsh(corpus, bands = 60, progress = TRUE)
  candidates <- lsh_candidates(buckets) 
  jaccardsimilarity<- lsh_compare (candidates, corpus, jaccard_similarity, progress=TRUE)
  fwrite (jaccardsimilarity, "filepath/Bill Text Data/jaccardsimilarity102S-114S.csv", row.names=FALSE)
  
  #103 Corpus & Similarity Measurements
  corpus_S_paths <- c(corpus_S_103, corpus_S_104)
  corpus <- TextReuseCorpus(paths=corpus_S_paths, 
                            tokenizer = tokenize_ngrams, n = 5, 
                            minhash_func = minhash, 
                            keep_tokens = TRUE, 
                            progress = TRUE)
  buckets <- lsh(corpus, bands = 60, progress = TRUE)
  candidates <- lsh_candidates(buckets) 
  jaccardsimilarity<- lsh_compare (candidates, corpus, jaccard_similarity, progress=TRUE)
  fwrite (jaccardsimilarity, "filepath/Bill Text Data/jaccardsimilarity103S-104S.csv", row.names=FALSE)
  corpus_S_paths <- c(corpus_S_103, corpus_S_105)
  corpus <- TextReuseCorpus(paths=corpus_S_paths, 
                            tokenizer = tokenize_ngrams, n = 5, 
                            minhash_func = minhash, 
                            keep_tokens = TRUE, 
                            progress = TRUE)
  buckets <- lsh(corpus, bands = 60, progress = TRUE)
  candidates <- lsh_candidates(buckets) 
  jaccardsimilarity<- lsh_compare (candidates, corpus, jaccard_similarity, progress=TRUE)
  fwrite (jaccardsimilarity, "filepath/Bill Text Data/jaccardsimilarity103S-105S.csv", row.names=FALSE)
  corpus_S_paths <- c(corpus_S_103, corpus_S_106)
  corpus <- TextReuseCorpus(paths=corpus_S_paths, 
                            tokenizer = tokenize_ngrams, n = 5, 
                            minhash_func = minhash, 
                            keep_tokens = TRUE, 
                            progress = TRUE)
  buckets <- lsh(corpus, bands = 60, progress = TRUE)
  candidates <- lsh_candidates(buckets) 
  jaccardsimilarity<- lsh_compare (candidates, corpus, jaccard_similarity, progress=TRUE)
  fwrite (jaccardsimilarity, "filepath/Bill Text Data/jaccardsimilarity103S-106S.csv", row.names=FALSE)
  corpus_S_paths <- c(corpus_S_103, corpus_S_107)
  corpus <- TextReuseCorpus(paths=corpus_S_paths, 
                            tokenizer = tokenize_ngrams, n = 5, 
                            minhash_func = minhash, 
                            keep_tokens = TRUE, 
                            progress = TRUE)
  buckets <- lsh(corpus, bands = 60, progress = TRUE)
  candidates <- lsh_candidates(buckets) 
  jaccardsimilarity<- lsh_compare (candidates, corpus, jaccard_similarity, progress=TRUE)
  fwrite (jaccardsimilarity, "filepath/Bill Text Data/jaccardsimilarity103S-107S.csv", row.names=FALSE)
  corpus_S_paths <- c(corpus_S_103, corpus_S_108)
  corpus <- TextReuseCorpus(paths=corpus_S_paths, 
                            tokenizer = tokenize_ngrams, n = 5, 
                            minhash_func = minhash, 
                            keep_tokens = TRUE, 
                            progress = TRUE)
  buckets <- lsh(corpus, bands = 60, progress = TRUE)
  candidates <- lsh_candidates(buckets) 
  jaccardsimilarity<- lsh_compare (candidates, corpus, jaccard_similarity, progress=TRUE)
  fwrite (jaccardsimilarity, "filepath/Bill Text Data/jaccardsimilarity103S-108S.csv", row.names=FALSE)
  corpus_S_paths <- c(corpus_S_103, corpus_S_109)
  corpus <- TextReuseCorpus(paths=corpus_S_paths, 
                            tokenizer = tokenize_ngrams, n = 5, 
                            minhash_func = minhash, 
                            keep_tokens = TRUE, 
                            progress = TRUE)
  buckets <- lsh(corpus, bands = 60, progress = TRUE)
  candidates <- lsh_candidates(buckets) 
  jaccardsimilarity<- lsh_compare (candidates, corpus, jaccard_similarity, progress=TRUE)
  fwrite (jaccardsimilarity, "filepath/Bill Text Data/jaccardsimilarity103S-109S.csv", row.names=FALSE)
  corpus_S_paths <- c(corpus_S_103, corpus_S_110)
  corpus <- TextReuseCorpus(paths=corpus_S_paths, 
                            tokenizer = tokenize_ngrams, n = 5, 
                            minhash_func = minhash, 
                            keep_tokens = TRUE, 
                            progress = TRUE)
  buckets <- lsh(corpus, bands = 60, progress = TRUE)
  candidates <- lsh_candidates(buckets) 
  jaccardsimilarity<- lsh_compare (candidates, corpus, jaccard_similarity, progress=TRUE)
  fwrite (jaccardsimilarity, "filepath/Bill Text Data/jaccardsimilarity103S-110S.csv", row.names=FALSE)
  corpus_S_paths <- c(corpus_S_103, corpus_S_111)
  corpus <- TextReuseCorpus(paths=corpus_S_paths, 
                            tokenizer = tokenize_ngrams, n = 5, 
                            minhash_func = minhash, 
                            keep_tokens = TRUE, 
                            progress = TRUE)
  buckets <- lsh(corpus, bands = 60, progress = TRUE)
  candidates <- lsh_candidates(buckets) 
  jaccardsimilarity<- lsh_compare (candidates, corpus, jaccard_similarity, progress=TRUE)
  fwrite (jaccardsimilarity, "filepath/Bill Text Data/jaccardsimilarity103S-111S.csv", row.names=FALSE)
  corpus_S_paths <- c(corpus_S_103, corpus_S_112)
  corpus <- TextReuseCorpus(paths=corpus_S_paths, 
                            tokenizer = tokenize_ngrams, n = 5, 
                            minhash_func = minhash, 
                            keep_tokens = TRUE, 
                            progress = TRUE)
  buckets <- lsh(corpus, bands = 60, progress = TRUE)
  candidates <- lsh_candidates(buckets) 
  jaccardsimilarity<- lsh_compare (candidates, corpus, jaccard_similarity, progress=TRUE)
  fwrite (jaccardsimilarity, "filepath/Bill Text Data/jaccardsimilarity103S-112S.csv", row.names=FALSE)
  corpus_S_paths <- c(corpus_S_103, corpus_S_113)
  corpus <- TextReuseCorpus(paths=corpus_S_paths, 
                            tokenizer = tokenize_ngrams, n = 5, 
                            minhash_func = minhash, 
                            keep_tokens = TRUE, 
                            progress = TRUE)
  buckets <- lsh(corpus, bands = 60, progress = TRUE)
  candidates <- lsh_candidates(buckets) 
  jaccardsimilarity<- lsh_compare (candidates, corpus, jaccard_similarity, progress=TRUE)
  fwrite (jaccardsimilarity, "filepath/Bill Text Data/jaccardsimilarity103S-113S.csv", row.names=FALSE)
  corpus_S_paths <- c(corpus_S_103, corpus_S_114)
  corpus <- TextReuseCorpus(paths=corpus_S_paths, 
                            tokenizer = tokenize_ngrams, n = 5, 
                            minhash_func = minhash, 
                            keep_tokens = TRUE, 
                            progress = TRUE)
  buckets <- lsh(corpus, bands = 60, progress = TRUE)
  candidates <- lsh_candidates(buckets) 
  jaccardsimilarity<- lsh_compare (candidates, corpus, jaccard_similarity, progress=TRUE)
  fwrite (jaccardsimilarity, "filepath/Bill Text Data/jaccardsimilarity103S-114S.csv", row.names=FALSE)
  
  #104 Corpus & Similarity Measurements
  corpus_S_paths <- c(corpus_S_104, corpus_S_105)
  corpus <- TextReuseCorpus(paths=corpus_S_paths, 
                            tokenizer = tokenize_ngrams, n = 5, 
                            minhash_func = minhash, 
                            keep_tokens = TRUE, 
                            progress = TRUE)
  buckets <- lsh(corpus, bands = 60, progress = TRUE)
  candidates <- lsh_candidates(buckets) 
  jaccardsimilarity<- lsh_compare (candidates, corpus, jaccard_similarity, progress=TRUE)
  fwrite (jaccardsimilarity, "filepath/Bill Text Data/jaccardsimilarity104S-105S.csv", row.names=FALSE)
  corpus_S_paths <- c(corpus_S_104, corpus_S_106)
  corpus <- TextReuseCorpus(paths=corpus_S_paths, 
                            tokenizer = tokenize_ngrams, n = 5, 
                            minhash_func = minhash, 
                            keep_tokens = TRUE, 
                            progress = TRUE)
  buckets <- lsh(corpus, bands = 60, progress = TRUE)
  candidates <- lsh_candidates(buckets) 
  jaccardsimilarity<- lsh_compare (candidates, corpus, jaccard_similarity, progress=TRUE)
  fwrite (jaccardsimilarity, "filepath/Bill Text Data/jaccardsimilarity104S-106S.csv", row.names=FALSE)
  corpus_S_paths <- c(corpus_S_104, corpus_S_107)
  corpus <- TextReuseCorpus(paths=corpus_S_paths, 
                            tokenizer = tokenize_ngrams, n = 5, 
                            minhash_func = minhash, 
                            keep_tokens = TRUE, 
                            progress = TRUE)
  buckets <- lsh(corpus, bands = 60, progress = TRUE)
  candidates <- lsh_candidates(buckets) 
  jaccardsimilarity<- lsh_compare (candidates, corpus, jaccard_similarity, progress=TRUE)
  fwrite (jaccardsimilarity, "filepath/Bill Text Data/jaccardsimilarity104S-107S.csv", row.names=FALSE)
  corpus_S_paths <- c(corpus_S_104, corpus_S_108)
  corpus <- TextReuseCorpus(paths=corpus_S_paths, 
                            tokenizer = tokenize_ngrams, n = 5, 
                            minhash_func = minhash, 
                            keep_tokens = TRUE, 
                            progress = TRUE)
  buckets <- lsh(corpus, bands = 60, progress = TRUE)
  candidates <- lsh_candidates(buckets) 
  jaccardsimilarity<- lsh_compare (candidates, corpus, jaccard_similarity, progress=TRUE)
  fwrite (jaccardsimilarity, "filepath/Bill Text Data/jaccardsimilarity104S-108S.csv", row.names=FALSE)
  corpus_S_paths <- c(corpus_S_104, corpus_S_109)
  corpus <- TextReuseCorpus(paths=corpus_S_paths, 
                            tokenizer = tokenize_ngrams, n = 5, 
                            minhash_func = minhash, 
                            keep_tokens = TRUE, 
                            progress = TRUE)
  buckets <- lsh(corpus, bands = 60, progress = TRUE)
  candidates <- lsh_candidates(buckets) 
  jaccardsimilarity<- lsh_compare (candidates, corpus, jaccard_similarity, progress=TRUE)
  fwrite (jaccardsimilarity, "filepath/Bill Text Data/jaccardsimilarity104S-109S.csv", row.names=FALSE)
  corpus_S_paths <- c(corpus_S_104, corpus_S_110)
  corpus <- TextReuseCorpus(paths=corpus_S_paths, 
                            tokenizer = tokenize_ngrams, n = 5, 
                            minhash_func = minhash, 
                            keep_tokens = TRUE, 
                            progress = TRUE)
  buckets <- lsh(corpus, bands = 60, progress = TRUE)
  candidates <- lsh_candidates(buckets) 
  jaccardsimilarity<- lsh_compare (candidates, corpus, jaccard_similarity, progress=TRUE)
  fwrite (jaccardsimilarity, "filepath/Bill Text Data/jaccardsimilarity104S-110S.csv", row.names=FALSE)
  corpus_S_paths <- c(corpus_S_104, corpus_S_111)
  corpus <- TextReuseCorpus(paths=corpus_S_paths, 
                            tokenizer = tokenize_ngrams, n = 5, 
                            minhash_func = minhash, 
                            keep_tokens = TRUE, 
                            progress = TRUE)
  buckets <- lsh(corpus, bands = 60, progress = TRUE)
  candidates <- lsh_candidates(buckets) 
  jaccardsimilarity<- lsh_compare (candidates, corpus, jaccard_similarity, progress=TRUE)
  fwrite (jaccardsimilarity, "filepath/Bill Text Data/jaccardsimilarity104S-111S.csv", row.names=FALSE)
  corpus_S_paths <- c(corpus_S_104, corpus_S_112)
  corpus <- TextReuseCorpus(paths=corpus_S_paths, 
                            tokenizer = tokenize_ngrams, n = 5, 
                            minhash_func = minhash, 
                            keep_tokens = TRUE, 
                            progress = TRUE)
  buckets <- lsh(corpus, bands = 60, progress = TRUE)
  candidates <- lsh_candidates(buckets) 
  jaccardsimilarity<- lsh_compare (candidates, corpus, jaccard_similarity, progress=TRUE)
  fwrite (jaccardsimilarity, "filepath/Bill Text Data/jaccardsimilarity104S-112S.csv", row.names=FALSE)
  corpus_S_paths <- c(corpus_S_104, corpus_S_113)
  corpus <- TextReuseCorpus(paths=corpus_S_paths, 
                            tokenizer = tokenize_ngrams, n = 5, 
                            minhash_func = minhash, 
                            keep_tokens = TRUE, 
                            progress = TRUE)
  buckets <- lsh(corpus, bands = 60, progress = TRUE)
  candidates <- lsh_candidates(buckets) 
  jaccardsimilarity<- lsh_compare (candidates, corpus, jaccard_similarity, progress=TRUE)
  fwrite (jaccardsimilarity, "filepath/Bill Text Data/jaccardsimilarity104S-113S.csv", row.names=FALSE)
  corpus_S_paths <- c(corpus_S_104, corpus_S_114)
  corpus <- TextReuseCorpus(paths=corpus_S_paths, 
                            tokenizer = tokenize_ngrams, n = 5, 
                            minhash_func = minhash, 
                            keep_tokens = TRUE, 
                            progress = TRUE)
  buckets <- lsh(corpus, bands = 60, progress = TRUE)
  candidates <- lsh_candidates(buckets) 
  jaccardsimilarity<- lsh_compare (candidates, corpus, jaccard_similarity, progress=TRUE)
  fwrite (jaccardsimilarity, "filepath/Bill Text Data/jaccardsimilarity104S-114S.csv", row.names=FALSE)
  
  #105 Corpus & Similarity Measurements
  corpus_S_paths <- c(corpus_S_105, corpus_S_106)
  corpus <- TextReuseCorpus(paths=corpus_S_paths, 
                            tokenizer = tokenize_ngrams, n = 5, 
                            minhash_func = minhash, 
                            keep_tokens = TRUE, 
                            progress = TRUE)
  buckets <- lsh(corpus, bands = 60, progress = TRUE)
  candidates <- lsh_candidates(buckets) 
  jaccardsimilarity<- lsh_compare (candidates, corpus, jaccard_similarity, progress=TRUE)
  fwrite (jaccardsimilarity, "filepath/Bill Text Data/jaccardsimilarity105S-106S.csv", row.names=FALSE)
  corpus_S_paths <- c(corpus_S_105, corpus_S_107)
  corpus <- TextReuseCorpus(paths=corpus_S_paths, 
                            tokenizer = tokenize_ngrams, n = 5, 
                            minhash_func = minhash, 
                            keep_tokens = TRUE, 
                            progress = TRUE)
  buckets <- lsh(corpus, bands = 60, progress = TRUE)
  candidates <- lsh_candidates(buckets) 
  jaccardsimilarity<- lsh_compare (candidates, corpus, jaccard_similarity, progress=TRUE)
  fwrite (jaccardsimilarity, "filepath/Bill Text Data/jaccardsimilarity105S-107S.csv", row.names=FALSE)
  corpus_S_paths <- c(corpus_S_105, corpus_S_108)
  corpus <- TextReuseCorpus(paths=corpus_S_paths, 
                            tokenizer = tokenize_ngrams, n = 5, 
                            minhash_func = minhash, 
                            keep_tokens = TRUE, 
                            progress = TRUE)
  buckets <- lsh(corpus, bands = 60, progress = TRUE)
  candidates <- lsh_candidates(buckets) 
  jaccardsimilarity<- lsh_compare (candidates, corpus, jaccard_similarity, progress=TRUE)
  fwrite (jaccardsimilarity, "filepath/Bill Text Data/jaccardsimilarity105S-108S.csv", row.names=FALSE)
  corpus_S_paths <- c(corpus_S_105, corpus_S_109)
  corpus <- TextReuseCorpus(paths=corpus_S_paths, 
                            tokenizer = tokenize_ngrams, n = 5, 
                            minhash_func = minhash, 
                            keep_tokens = TRUE, 
                            progress = TRUE)
  buckets <- lsh(corpus, bands = 60, progress = TRUE)
  candidates <- lsh_candidates(buckets) 
  jaccardsimilarity<- lsh_compare (candidates, corpus, jaccard_similarity, progress=TRUE)
  fwrite (jaccardsimilarity, "filepath/Bill Text Data/jaccardsimilarity105S-109S.csv", row.names=FALSE)
  corpus_S_paths <- c(corpus_S_105, corpus_S_110)
  corpus <- TextReuseCorpus(paths=corpus_S_paths, 
                            tokenizer = tokenize_ngrams, n = 5, 
                            minhash_func = minhash, 
                            keep_tokens = TRUE, 
                            progress = TRUE)
  buckets <- lsh(corpus, bands = 60, progress = TRUE)
  candidates <- lsh_candidates(buckets) 
  jaccardsimilarity<- lsh_compare (candidates, corpus, jaccard_similarity, progress=TRUE)
  fwrite (jaccardsimilarity, "filepath/Bill Text Data/jaccardsimilarity105S-110S.csv", row.names=FALSE)
  corpus_S_paths <- c(corpus_S_105, corpus_S_111)
  corpus <- TextReuseCorpus(paths=corpus_S_paths, 
                            tokenizer = tokenize_ngrams, n = 5, 
                            minhash_func = minhash, 
                            keep_tokens = TRUE, 
                            progress = TRUE)
  buckets <- lsh(corpus, bands = 60, progress = TRUE)
  candidates <- lsh_candidates(buckets) 
  jaccardsimilarity<- lsh_compare (candidates, corpus, jaccard_similarity, progress=TRUE)
  fwrite (jaccardsimilarity, "filepath/Bill Text Data/jaccardsimilarity105S-111S.csv", row.names=FALSE)
  corpus_S_paths <- c(corpus_S_105, corpus_S_112)
  corpus <- TextReuseCorpus(paths=corpus_S_paths, 
                            tokenizer = tokenize_ngrams, n = 5, 
                            minhash_func = minhash, 
                            keep_tokens = TRUE, 
                            progress = TRUE)
  buckets <- lsh(corpus, bands = 60, progress = TRUE)
  candidates <- lsh_candidates(buckets) 
  jaccardsimilarity<- lsh_compare (candidates, corpus, jaccard_similarity, progress=TRUE)
  fwrite (jaccardsimilarity, "filepath/Bill Text Data/jaccardsimilarity105S-112S.csv", row.names=FALSE)
  corpus_S_paths <- c(corpus_S_105, corpus_S_113)
  corpus <- TextReuseCorpus(paths=corpus_S_paths, 
                            tokenizer = tokenize_ngrams, n = 5, 
                            minhash_func = minhash, 
                            keep_tokens = TRUE, 
                            progress = TRUE)
  buckets <- lsh(corpus, bands = 60, progress = TRUE)
  candidates <- lsh_candidates(buckets) 
  jaccardsimilarity<- lsh_compare (candidates, corpus, jaccard_similarity, progress=TRUE)
  fwrite (jaccardsimilarity, "filepath/Bill Text Data/jaccardsimilarity105S-113S.csv", row.names=FALSE)
  corpus_S_paths <- c(corpus_S_105, corpus_S_114)
  corpus <- TextReuseCorpus(paths=corpus_S_paths, 
                            tokenizer = tokenize_ngrams, n = 5, 
                            minhash_func = minhash, 
                            keep_tokens = TRUE, 
                            progress = TRUE)
  buckets <- lsh(corpus, bands = 60, progress = TRUE)
  candidates <- lsh_candidates(buckets) 
  jaccardsimilarity<- lsh_compare (candidates, corpus, jaccard_similarity, progress=TRUE)
  fwrite (jaccardsimilarity, "filepath/Bill Text Data/jaccardsimilarity105S-114S.csv", row.names=FALSE)
  
  #106 Corpus & Similarity Measurements
  corpus_S_paths <- c(corpus_S_106, corpus_S_107)
  corpus <- TextReuseCorpus(paths=corpus_S_paths, 
                            tokenizer = tokenize_ngrams, n = 5, 
                            minhash_func = minhash, 
                            keep_tokens = TRUE, 
                            progress = TRUE)
  buckets <- lsh(corpus, bands = 60, progress = TRUE)
  candidates <- lsh_candidates(buckets) 
  jaccardsimilarity<- lsh_compare (candidates, corpus, jaccard_similarity, progress=TRUE)
  fwrite (jaccardsimilarity, "filepath/Bill Text Data/jaccardsimilarity106S-107S.csv", row.names=FALSE)
  corpus_S_paths <- c(corpus_S_106, corpus_S_108)
  corpus <- TextReuseCorpus(paths=corpus_S_paths, 
                            tokenizer = tokenize_ngrams, n = 5, 
                            minhash_func = minhash, 
                            keep_tokens = TRUE, 
                            progress = TRUE)
  buckets <- lsh(corpus, bands = 60, progress = TRUE)
  candidates <- lsh_candidates(buckets) 
  jaccardsimilarity<- lsh_compare (candidates, corpus, jaccard_similarity, progress=TRUE)
  fwrite (jaccardsimilarity, "filepath/Bill Text Data/jaccardsimilarity106S-108S.csv", row.names=FALSE)
  corpus_S_paths <- c(corpus_S_106, corpus_S_109)
  corpus <- TextReuseCorpus(paths=corpus_S_paths, 
                            tokenizer = tokenize_ngrams, n = 5, 
                            minhash_func = minhash, 
                            keep_tokens = TRUE, 
                            progress = TRUE)
  buckets <- lsh(corpus, bands = 60, progress = TRUE)
  candidates <- lsh_candidates(buckets) 
  jaccardsimilarity<- lsh_compare (candidates, corpus, jaccard_similarity, progress=TRUE)
  fwrite (jaccardsimilarity, "filepath/Bill Text Data/jaccardsimilarity106S-109S.csv", row.names=FALSE)
  corpus_S_paths <- c(corpus_S_106, corpus_S_110)
  corpus <- TextReuseCorpus(paths=corpus_S_paths, 
                            tokenizer = tokenize_ngrams, n = 5, 
                            minhash_func = minhash, 
                            keep_tokens = TRUE, 
                            progress = TRUE)
  buckets <- lsh(corpus, bands = 60, progress = TRUE)
  candidates <- lsh_candidates(buckets) 
  jaccardsimilarity<- lsh_compare (candidates, corpus, jaccard_similarity, progress=TRUE)
  fwrite (jaccardsimilarity, "filepath/Bill Text Data/jaccardsimilarity106S-110S.csv", row.names=FALSE)
  corpus_S_paths <- c(corpus_S_106, corpus_S_111)
  corpus <- TextReuseCorpus(paths=corpus_S_paths, 
                            tokenizer = tokenize_ngrams, n = 5, 
                            minhash_func = minhash, 
                            keep_tokens = TRUE, 
                            progress = TRUE)
  buckets <- lsh(corpus, bands = 60, progress = TRUE)
  candidates <- lsh_candidates(buckets) 
  jaccardsimilarity<- lsh_compare (candidates, corpus, jaccard_similarity, progress=TRUE)
  fwrite (jaccardsimilarity, "filepath/Bill Text Data/jaccardsimilarity106S-111S.csv", row.names=FALSE)
  corpus_S_paths <- c(corpus_S_106, corpus_S_112)
  corpus <- TextReuseCorpus(paths=corpus_S_paths, 
                            tokenizer = tokenize_ngrams, n = 5, 
                            minhash_func = minhash, 
                            keep_tokens = TRUE, 
                            progress = TRUE)
  buckets <- lsh(corpus, bands = 60, progress = TRUE)
  candidates <- lsh_candidates(buckets) 
  jaccardsimilarity<- lsh_compare (candidates, corpus, jaccard_similarity, progress=TRUE)
  fwrite (jaccardsimilarity, "filepath/Bill Text Data/jaccardsimilarity106S-112S.csv", row.names=FALSE)
  corpus_S_paths <- c(corpus_S_106, corpus_S_113)
  corpus <- TextReuseCorpus(paths=corpus_S_paths, 
                            tokenizer = tokenize_ngrams, n = 5, 
                            minhash_func = minhash, 
                            keep_tokens = TRUE, 
                            progress = TRUE)
  buckets <- lsh(corpus, bands = 60, progress = TRUE)
  candidates <- lsh_candidates(buckets) 
  jaccardsimilarity<- lsh_compare (candidates, corpus, jaccard_similarity, progress=TRUE)
  fwrite (jaccardsimilarity, "filepath/Bill Text Data/jaccardsimilarity106S-113S.csv", row.names=FALSE)
  corpus_S_paths <- c(corpus_S_106, corpus_S_114)
  corpus <- TextReuseCorpus(paths=corpus_S_paths, 
                            tokenizer = tokenize_ngrams, n = 5, 
                            minhash_func = minhash, 
                            keep_tokens = TRUE, 
                            progress = TRUE)
  buckets <- lsh(corpus, bands = 60, progress = TRUE)
  candidates <- lsh_candidates(buckets) 
  jaccardsimilarity<- lsh_compare (candidates, corpus, jaccard_similarity, progress=TRUE)
  fwrite (jaccardsimilarity, "filepath/Bill Text Data/jaccardsimilarity106S-114S.csv", row.names=FALSE)
  
  #107 Corpus & Similarity Measurements
  corpus_S_paths <- c(corpus_S_107, corpus_S_108)
  corpus <- TextReuseCorpus(paths=corpus_S_paths, 
                            tokenizer = tokenize_ngrams, n = 5, 
                            minhash_func = minhash, 
                            keep_tokens = TRUE, 
                            progress = TRUE)
  buckets <- lsh(corpus, bands = 60, progress = TRUE)
  candidates <- lsh_candidates(buckets) 
  jaccardsimilarity<- lsh_compare (candidates, corpus, jaccard_similarity, progress=TRUE)
  fwrite (jaccardsimilarity, "filepath/Bill Text Data/jaccardsimilarity107S-108S.csv", row.names=FALSE)
  corpus_S_paths <- c(corpus_S_107, corpus_S_109)
  corpus <- TextReuseCorpus(paths=corpus_S_paths, 
                            tokenizer = tokenize_ngrams, n = 5, 
                            minhash_func = minhash, 
                            keep_tokens = TRUE, 
                            progress = TRUE)
  buckets <- lsh(corpus, bands = 60, progress = TRUE)
  candidates <- lsh_candidates(buckets) 
  jaccardsimilarity<- lsh_compare (candidates, corpus, jaccard_similarity, progress=TRUE)
  fwrite (jaccardsimilarity, "filepath/Bill Text Data/jaccardsimilarity107S-109S.csv", row.names=FALSE)
  corpus_S_paths <- c(corpus_S_107, corpus_S_110)
  corpus <- TextReuseCorpus(paths=corpus_S_paths, 
                            tokenizer = tokenize_ngrams, n = 5, 
                            minhash_func = minhash, 
                            keep_tokens = TRUE, 
                            progress = TRUE)
  buckets <- lsh(corpus, bands = 60, progress = TRUE)
  candidates <- lsh_candidates(buckets) 
  jaccardsimilarity<- lsh_compare (candidates, corpus, jaccard_similarity, progress=TRUE)
  fwrite (jaccardsimilarity, "filepath/Bill Text Data/jaccardsimilarity107S-110S.csv", row.names=FALSE)
  corpus_S_paths <- c(corpus_S_107, corpus_S_111)
  corpus <- TextReuseCorpus(paths=corpus_S_paths, 
                            tokenizer = tokenize_ngrams, n = 5, 
                            minhash_func = minhash, 
                            keep_tokens = TRUE, 
                            progress = TRUE)
  buckets <- lsh(corpus, bands = 60, progress = TRUE)
  candidates <- lsh_candidates(buckets) 
  jaccardsimilarity<- lsh_compare (candidates, corpus, jaccard_similarity, progress=TRUE)
  fwrite (jaccardsimilarity, "filepath/Bill Text Data/jaccardsimilarity107S-111S.csv", row.names=FALSE)
  corpus_S_paths <- c(corpus_S_107, corpus_S_112)
  corpus <- TextReuseCorpus(paths=corpus_S_paths, 
                            tokenizer = tokenize_ngrams, n = 5, 
                            minhash_func = minhash, 
                            keep_tokens = TRUE, 
                            progress = TRUE)
  buckets <- lsh(corpus, bands = 60, progress = TRUE)
  candidates <- lsh_candidates(buckets) 
  jaccardsimilarity<- lsh_compare (candidates, corpus, jaccard_similarity, progress=TRUE)
  fwrite (jaccardsimilarity, "filepath/Bill Text Data/jaccardsimilarity107S-112S.csv", row.names=FALSE)
  corpus_S_paths <- c(corpus_S_107, corpus_S_113)
  corpus <- TextReuseCorpus(paths=corpus_S_paths, 
                            tokenizer = tokenize_ngrams, n = 5, 
                            minhash_func = minhash, 
                            keep_tokens = TRUE, 
                            progress = TRUE)
  buckets <- lsh(corpus, bands = 60, progress = TRUE)
  candidates <- lsh_candidates(buckets) 
  jaccardsimilarity<- lsh_compare (candidates, corpus, jaccard_similarity, progress=TRUE)
  fwrite (jaccardsimilarity, "filepath/Bill Text Data/jaccardsimilarity107S-113S.csv", row.names=FALSE)
  corpus_S_paths <- c(corpus_S_107, corpus_S_114)
  corpus <- TextReuseCorpus(paths=corpus_S_paths, 
                            tokenizer = tokenize_ngrams, n = 5, 
                            minhash_func = minhash, 
                            keep_tokens = TRUE, 
                            progress = TRUE)
  buckets <- lsh(corpus, bands = 60, progress = TRUE)
  candidates <- lsh_candidates(buckets) 
  jaccardsimilarity<- lsh_compare (candidates, corpus, jaccard_similarity, progress=TRUE)
  fwrite (jaccardsimilarity, "filepath/Bill Text Data/jaccardsimilarity107S-114S.csv", row.names=FALSE)
  
  #108 Corpus & Similarity Measurements
  corpus_S_paths <- c(corpus_S_108, corpus_S_109)
  corpus <- TextReuseCorpus(paths=corpus_S_paths, 
                            tokenizer = tokenize_ngrams, n = 5, 
                            minhash_func = minhash, 
                            keep_tokens = TRUE, 
                            progress = TRUE)
  buckets <- lsh(corpus, bands = 60, progress = TRUE)
  candidates <- lsh_candidates(buckets) 
  jaccardsimilarity<- lsh_compare (candidates, corpus, jaccard_similarity, progress=TRUE)
  fwrite (jaccardsimilarity, "filepath/Bill Text Data/jaccardsimilarity108S-109S.csv", row.names=FALSE)
  corpus_S_paths <- c(corpus_S_108, corpus_S_110)
  corpus <- TextReuseCorpus(paths=corpus_S_paths, 
                            tokenizer = tokenize_ngrams, n = 5, 
                            minhash_func = minhash, 
                            keep_tokens = TRUE, 
                            progress = TRUE)
  buckets <- lsh(corpus, bands = 60, progress = TRUE)
  candidates <- lsh_candidates(buckets) 
  jaccardsimilarity<- lsh_compare (candidates, corpus, jaccard_similarity, progress=TRUE)
  fwrite (jaccardsimilarity, "filepath/Bill Text Data/jaccardsimilarity108S-110S.csv", row.names=FALSE)
  corpus_S_paths <- c(corpus_S_108, corpus_S_111)
  corpus <- TextReuseCorpus(paths=corpus_S_paths, 
                            tokenizer = tokenize_ngrams, n = 5, 
                            minhash_func = minhash, 
                            keep_tokens = TRUE, 
                            progress = TRUE)
  buckets <- lsh(corpus, bands = 60, progress = TRUE)
  candidates <- lsh_candidates(buckets) 
  jaccardsimilarity<- lsh_compare (candidates, corpus, jaccard_similarity, progress=TRUE)
  fwrite (jaccardsimilarity, "filepath/Bill Text Data/jaccardsimilarity108S-111S.csv", row.names=FALSE)
  corpus_S_paths <- c(corpus_S_108, corpus_S_112)
  corpus <- TextReuseCorpus(paths=corpus_S_paths, 
                            tokenizer = tokenize_ngrams, n = 5, 
                            minhash_func = minhash, 
                            keep_tokens = TRUE, 
                            progress = TRUE)
  buckets <- lsh(corpus, bands = 60, progress = TRUE)
  candidates <- lsh_candidates(buckets) 
  jaccardsimilarity<- lsh_compare (candidates, corpus, jaccard_similarity, progress=TRUE)
  fwrite (jaccardsimilarity, "filepath/Bill Text Data/jaccardsimilarity108S-112S.csv", row.names=FALSE)
  corpus_S_paths <- c(corpus_S_108, corpus_S_113)
  corpus <- TextReuseCorpus(paths=corpus_S_paths, 
                            tokenizer = tokenize_ngrams, n = 5, 
                            minhash_func = minhash, 
                            keep_tokens = TRUE, 
                            progress = TRUE)
  buckets <- lsh(corpus, bands = 60, progress = TRUE)
  candidates <- lsh_candidates(buckets) 
  jaccardsimilarity<- lsh_compare (candidates, corpus, jaccard_similarity, progress=TRUE)
  fwrite (jaccardsimilarity, "filepath/Bill Text Data/jaccardsimilarity108S-113S.csv", row.names=FALSE)
  corpus_S_paths <- c(corpus_S_108, corpus_S_114)
  corpus <- TextReuseCorpus(paths=corpus_S_paths, 
                            tokenizer = tokenize_ngrams, n = 5, 
                            minhash_func = minhash, 
                            keep_tokens = TRUE, 
                            progress = TRUE)
  buckets <- lsh(corpus, bands = 60, progress = TRUE)
  candidates <- lsh_candidates(buckets) 
  jaccardsimilarity<- lsh_compare (candidates, corpus, jaccard_similarity, progress=TRUE)
  fwrite (jaccardsimilarity, "filepath/Bill Text Data/jaccardsimilarity108S-114S.csv", row.names=FALSE)
  
  #109 Corpus & Similarity Measurements
  corpus_S_paths <- c(corpus_S_109, corpus_S_110)
  corpus <- TextReuseCorpus(paths=corpus_S_paths, 
                            tokenizer = tokenize_ngrams, n = 5, 
                            minhash_func = minhash, 
                            keep_tokens = TRUE, 
                            progress = TRUE)
  buckets <- lsh(corpus, bands = 60, progress = TRUE)
  candidates <- lsh_candidates(buckets) 
  jaccardsimilarity<- lsh_compare (candidates, corpus, jaccard_similarity, progress=TRUE)
  fwrite (jaccardsimilarity, "filepath/Bill Text Data/jaccardsimilarity109S-110S.csv", row.names=FALSE)
  corpus_S_paths <- c(corpus_S_109, corpus_S_111)
  corpus <- TextReuseCorpus(paths=corpus_S_paths, 
                            tokenizer = tokenize_ngrams, n = 5, 
                            minhash_func = minhash, 
                            keep_tokens = TRUE, 
                            progress = TRUE)
  buckets <- lsh(corpus, bands = 60, progress = TRUE)
  candidates <- lsh_candidates(buckets) 
  jaccardsimilarity<- lsh_compare (candidates, corpus, jaccard_similarity, progress=TRUE)
  fwrite (jaccardsimilarity, "filepath/Bill Text Data/jaccardsimilarity109S-111S.csv", row.names=FALSE)
  corpus_S_paths <- c(corpus_S_109, corpus_S_112)
  corpus <- TextReuseCorpus(paths=corpus_S_paths, 
                            tokenizer = tokenize_ngrams, n = 5, 
                            minhash_func = minhash, 
                            keep_tokens = TRUE, 
                            progress = TRUE)
  buckets <- lsh(corpus, bands = 60, progress = TRUE)
  candidates <- lsh_candidates(buckets) 
  jaccardsimilarity<- lsh_compare (candidates, corpus, jaccard_similarity, progress=TRUE)
  fwrite (jaccardsimilarity, "filepath/Bill Text Data/jaccardsimilarity109S-112S.csv", row.names=FALSE)
  corpus_S_paths <- c(corpus_S_109, corpus_S_113)
  corpus <- TextReuseCorpus(paths=corpus_S_paths, 
                            tokenizer = tokenize_ngrams, n = 5, 
                            minhash_func = minhash, 
                            keep_tokens = TRUE, 
                            progress = TRUE)
  buckets <- lsh(corpus, bands = 60, progress = TRUE)
  candidates <- lsh_candidates(buckets) 
  jaccardsimilarity<- lsh_compare (candidates, corpus, jaccard_similarity, progress=TRUE)
  fwrite (jaccardsimilarity, "filepath/Bill Text Data/jaccardsimilarity109S-113S.csv", row.names=FALSE)
  corpus_S_paths <- c(corpus_S_109, corpus_S_114)
  corpus <- TextReuseCorpus(paths=corpus_S_paths, 
                            tokenizer = tokenize_ngrams, n = 5, 
                            minhash_func = minhash, 
                            keep_tokens = TRUE, 
                            progress = TRUE)
  buckets <- lsh(corpus, bands = 60, progress = TRUE)
  candidates <- lsh_candidates(buckets) 
  jaccardsimilarity<- lsh_compare (candidates, corpus, jaccard_similarity, progress=TRUE)
  fwrite (jaccardsimilarity, "filepath/Bill Text Data/jaccardsimilarity109S-114S.csv", row.names=FALSE)
  
  #110 Corpus & Similarity Measurements
  corpus_S_paths <- c(corpus_S_110, corpus_S_111)
  corpus <- TextReuseCorpus(paths=corpus_S_paths, 
                            tokenizer = tokenize_ngrams, n = 5, 
                            minhash_func = minhash, 
                            keep_tokens = TRUE, 
                            progress = TRUE)
  buckets <- lsh(corpus, bands = 60, progress = TRUE)
  candidates <- lsh_candidates(buckets) 
  jaccardsimilarity<- lsh_compare (candidates, corpus, jaccard_similarity, progress=TRUE)
  fwrite (jaccardsimilarity, "filepath/Bill Text Data/jaccardsimilarity110S-111S.csv", row.names=FALSE)
  corpus_S_paths <- c(corpus_S_110, corpus_S_112)
  corpus <- TextReuseCorpus(paths=corpus_S_paths, 
                            tokenizer = tokenize_ngrams, n = 5, 
                            minhash_func = minhash, 
                            keep_tokens = TRUE, 
                            progress = TRUE)
  buckets <- lsh(corpus, bands = 60, progress = TRUE)
  candidates <- lsh_candidates(buckets) 
  jaccardsimilarity<- lsh_compare (candidates, corpus, jaccard_similarity, progress=TRUE)
  fwrite (jaccardsimilarity, "filepath/Bill Text Data/jaccardsimilarity110S-112S.csv", row.names=FALSE)
  corpus_S_paths <- c(corpus_S_110, corpus_S_113)
  corpus <- TextReuseCorpus(paths=corpus_S_paths, 
                            tokenizer = tokenize_ngrams, n = 5, 
                            minhash_func = minhash, 
                            keep_tokens = TRUE, 
                            progress = TRUE)
  buckets <- lsh(corpus, bands = 60, progress = TRUE)
  candidates <- lsh_candidates(buckets) 
  jaccardsimilarity<- lsh_compare (candidates, corpus, jaccard_similarity, progress=TRUE)
  fwrite (jaccardsimilarity, "filepath/Bill Text Data/jaccardsimilarity110S-113S.csv", row.names=FALSE)
  corpus_S_paths <- c(corpus_S_110, corpus_S_114)
  corpus <- TextReuseCorpus(paths=corpus_S_paths, 
                            tokenizer = tokenize_ngrams, n = 5, 
                            minhash_func = minhash, 
                            keep_tokens = TRUE, 
                            progress = TRUE)
  buckets <- lsh(corpus, bands = 60, progress = TRUE)
  candidates <- lsh_candidates(buckets) 
  jaccardsimilarity<- lsh_compare (candidates, corpus, jaccard_similarity, progress=TRUE)
  fwrite (jaccardsimilarity, "filepath/Bill Text Data/jaccardsimilarity110S-114S.csv", row.names=FALSE)
  
  #111 Corpus & Similarity Measurements
  corpus_S_paths <- c(corpus_S_111, corpus_S_112)
  corpus <- TextReuseCorpus(paths=corpus_S_paths, 
                            tokenizer = tokenize_ngrams, n = 5, 
                            minhash_func = minhash, 
                            keep_tokens = TRUE, 
                            progress = TRUE)
  buckets <- lsh(corpus, bands = 60, progress = TRUE)
  candidates <- lsh_candidates(buckets) 
  jaccardsimilarity<- lsh_compare (candidates, corpus, jaccard_similarity, progress=TRUE)
  fwrite (jaccardsimilarity, "filepath/Bill Text Data/jaccardsimilarity111S-112S.csv", row.names=FALSE)
  corpus_S_paths <- c(corpus_S_111, corpus_S_113)
  corpus <- TextReuseCorpus(paths=corpus_S_paths, 
                            tokenizer = tokenize_ngrams, n = 5, 
                            minhash_func = minhash, 
                            keep_tokens = TRUE, 
                            progress = TRUE)
  buckets <- lsh(corpus, bands = 60, progress = TRUE)
  candidates <- lsh_candidates(buckets) 
  jaccardsimilarity<- lsh_compare (candidates, corpus, jaccard_similarity, progress=TRUE)
  fwrite (jaccardsimilarity, "filepath/Bill Text Data/jaccardsimilarity111S-113S.csv", row.names=FALSE)
  corpus_S_paths <- c(corpus_S_111, corpus_S_114)
  corpus <- TextReuseCorpus(paths=corpus_S_paths, 
                            tokenizer = tokenize_ngrams, n = 5, 
                            minhash_func = minhash, 
                            keep_tokens = TRUE, 
                            progress = TRUE)
  buckets <- lsh(corpus, bands = 60, progress = TRUE)
  candidates <- lsh_candidates(buckets) 
  jaccardsimilarity<- lsh_compare (candidates, corpus, jaccard_similarity, progress=TRUE)
  fwrite (jaccardsimilarity, "filepath/Bill Text Data/jaccardsimilarity111S-114S.csv", row.names=FALSE)
  
  #112 Corpus & Similarity Measurements
  corpus_S_paths <- c(corpus_S_112, corpus_S_113)
  corpus <- TextReuseCorpus(paths=corpus_S_paths, 
                            tokenizer = tokenize_ngrams, n = 5, 
                            minhash_func = minhash, 
                            keep_tokens = TRUE, 
                            progress = TRUE)
  buckets <- lsh(corpus, bands = 60, progress = TRUE)
  candidates <- lsh_candidates(buckets) 
  jaccardsimilarity<- lsh_compare (candidates, corpus, jaccard_similarity, progress=TRUE)
  fwrite (jaccardsimilarity, "filepath/Bill Text Data/jaccardsimilarity112S-113S.csv", row.names=FALSE)
  corpus_S_paths <- c(corpus_S_112, corpus_S_114)
  corpus <- TextReuseCorpus(paths=corpus_S_paths, 
                            tokenizer = tokenize_ngrams, n = 5, 
                            minhash_func = minhash, 
                            keep_tokens = TRUE, 
                            progress = TRUE)
  buckets <- lsh(corpus, bands = 60, progress = TRUE)
  candidates <- lsh_candidates(buckets) 
  jaccardsimilarity<- lsh_compare (candidates, corpus, jaccard_similarity, progress=TRUE)
  fwrite (jaccardsimilarity, "filepath/Bill Text Data/jaccardsimilarity112S-114S.csv", row.names=FALSE)
  
  #113 Corpus & Similarity Measurements
  corpus_S_paths <- c(corpus_S_113, corpus_S_114)
  corpus <- TextReuseCorpus(paths=corpus_S_paths, 
                            tokenizer = tokenize_ngrams, n = 5, 
                            minhash_func = minhash, 
                            keep_tokens = TRUE, 
                            progress = TRUE)
  buckets <- lsh(corpus, bands = 60, progress = TRUE)
  candidates <- lsh_candidates(buckets) 
  jaccardsimilarity<- lsh_compare (candidates, corpus, jaccard_similarity, progress=TRUE)
  fwrite (jaccardsimilarity, "filepath/Bill Text Data/jaccardsimilarity113S-114S.csv", row.names=FALSE)
  
  
  
  #### HR & S - Cross-Chamber Comparisons####
  #By Bill Type & Congress
  
  corpus_101<-list.files(c("filepath/Bill Text Corpus/Bill Type/HR/101/","filepath/Bill Text Corpus/Bill Type/S/101/"),pattern="*.txt", full.names = TRUE)
  corpus_102<-list.files(c("filepath/Bill Text Corpus/Bill Type/HR/102/","filepath/Bill Text Corpus/Bill Type/S/102/"),pattern="*.txt", full.names = TRUE)
  corpus_103<-list.files(c("filepath/Bill Text Corpus/Bill Type/HR/103/","filepath/Bill Text Corpus/Bill Type/S/103/"),pattern="*.txt", full.names = TRUE)
  corpus_104<-list.files(c("filepath/Bill Text Corpus/Bill Type/HR/104/","filepath/Bill Text Corpus/Bill Type/S/104/"),pattern="*.txt", full.names = TRUE)
  corpus_105<-list.files(c("filepath/Bill Text Corpus/Bill Type/HR/105/","filepath/Bill Text Corpus/Bill Type/S/105/"),pattern="*.txt", full.names = TRUE)
  corpus_106<-list.files(c("filepath/Bill Text Corpus/Bill Type/HR/106/","filepath/Bill Text Corpus/Bill Type/S/106/"),pattern="*.txt", full.names = TRUE)
  corpus_107<-list.files(c("filepath/Bill Text Corpus/Bill Type/HR/107/","filepath/Bill Text Corpus/Bill Type/S/107/"),pattern="*.txt", full.names = TRUE)
  corpus_108<-list.files(c("filepath/Bill Text Corpus/Bill Type/HR/108/","filepath/Bill Text Corpus/Bill Type/S/108/"),pattern="*.txt", full.names = TRUE)
  corpus_109<-list.files(c("filepath/Bill Text Corpus/Bill Type/HR/109/","filepath/Bill Text Corpus/Bill Type/S/109/"),pattern="*.txt", full.names = TRUE)
  corpus_110<-list.files(c("filepath/Bill Text Corpus/Bill Type/HR/110/","filepath/Bill Text Corpus/Bill Type/S/110/"),pattern="*.txt", full.names = TRUE)
  corpus_111<-list.files(c("filepath/Bill Text Corpus/Bill Type/HR/111/","filepath/Bill Text Corpus/Bill Type/S/111/"),pattern="*.txt", full.names = TRUE)
  corpus_112<-list.files(c("filepath/Bill Text Corpus/Bill Type/HR/112/","filepath/Bill Text Corpus/Bill Type/S/112/"),pattern="*.txt", full.names = TRUE)
  corpus_113<-list.files(c("filepath/Bill Text Corpus/Bill Type/HR/113/","filepath/Bill Text Corpus/Bill Type/S/113/"),pattern="*.txt", full.names = TRUE)
  corpus_114<-list.files(c("filepath/Bill Text Corpus/Bill Type/HR/114/","filepath/Bill Text Corpus/Bill Type/S/114/"),pattern="*.txt", full.names = TRUE)
  
  #Create Minhash Function for Locality Sensitive Hashing (LSH) - SAME FOR ALL COMPARISONS
  minhash <- minhash_generator(n= 240, seed = 3352)
  
  #101 Corpus & Similarity Measurements
  corpus_paths <- c(corpus_101, corpus_102)
  corpus <- TextReuseCorpus(paths=corpus_paths, 
                            tokenizer = tokenize_ngrams, n = 5, 
                            minhash_func = minhash, 
                            keep_tokens = TRUE, 
                            progress = TRUE)
  buckets <- lsh(corpus, bands = 60, progress = TRUE)
  candidates <- lsh_candidates(buckets) 
  jaccardsimilarity<- lsh_compare (candidates, corpus, jaccard_similarity, progress=TRUE)
  fwrite (jaccardsimilarity, "filepath/Bill Text Data/jaccardsimilarity101-102.csv", row.names=FALSE)
  corpus_paths <- c(corpus_101, corpus_103)
  corpus <- TextReuseCorpus(paths=corpus_paths, 
                            tokenizer = tokenize_ngrams, n = 5, 
                            minhash_func = minhash, 
                            keep_tokens = TRUE, 
                            progress = TRUE)
  buckets <- lsh(corpus, bands = 60, progress = TRUE)
  candidates <- lsh_candidates(buckets) 
  jaccardsimilarity<- lsh_compare (candidates, corpus, jaccard_similarity, progress=TRUE)
  fwrite (jaccardsimilarity, "filepath/Bill Text Data/jaccardsimilarity101-103.csv", row.names=FALSE)
  corpus_paths <- c(corpus_101, corpus_104)
  corpus <- TextReuseCorpus(paths=corpus_paths, 
                            tokenizer = tokenize_ngrams, n = 5, 
                            minhash_func = minhash, 
                            keep_tokens = TRUE, 
                            progress = TRUE)
  buckets <- lsh(corpus, bands = 60, progress = TRUE)
  candidates <- lsh_candidates(buckets) 
  jaccardsimilarity<- lsh_compare (candidates, corpus, jaccard_similarity, progress=TRUE)
  fwrite (jaccardsimilarity, "filepath/Bill Text Data/jaccardsimilarity101-104.csv", row.names=FALSE)
  corpus_paths <- c(corpus_101, corpus_105)
  corpus <- TextReuseCorpus(paths=corpus_paths, 
                            tokenizer = tokenize_ngrams, n = 5, 
                            minhash_func = minhash, 
                            keep_tokens = TRUE, 
                            progress = TRUE)
  buckets <- lsh(corpus, bands = 60, progress = TRUE)
  candidates <- lsh_candidates(buckets) 
  jaccardsimilarity<- lsh_compare (candidates, corpus, jaccard_similarity, progress=TRUE)
  fwrite (jaccardsimilarity, "filepath/Bill Text Data/jaccardsimilarity101-105.csv", row.names=FALSE)
  corpus_paths <- c(corpus_101, corpus_106)
  corpus <- TextReuseCorpus(paths=corpus_paths, 
                            tokenizer = tokenize_ngrams, n = 5, 
                            minhash_func = minhash, 
                            keep_tokens = TRUE, 
                            progress = TRUE)
  buckets <- lsh(corpus, bands = 60, progress = TRUE)
  candidates <- lsh_candidates(buckets) 
  jaccardsimilarity<- lsh_compare (candidates, corpus, jaccard_similarity, progress=TRUE)
  fwrite (jaccardsimilarity, "filepath/Bill Text Data/jaccardsimilarity101-106.csv", row.names=FALSE)
  corpus_paths <- c(corpus_101, corpus_107)
  corpus <- TextReuseCorpus(paths=corpus_paths, 
                            tokenizer = tokenize_ngrams, n = 5, 
                            minhash_func = minhash, 
                            keep_tokens = TRUE, 
                            progress = TRUE)
  buckets <- lsh(corpus, bands = 60, progress = TRUE)
  candidates <- lsh_candidates(buckets) 
  jaccardsimilarity<- lsh_compare (candidates, corpus, jaccard_similarity, progress=TRUE)
  fwrite (jaccardsimilarity, "filepath/Bill Text Data/jaccardsimilarity101-107.csv", row.names=FALSE)
  corpus_paths <- c(corpus_101, corpus_108)
  corpus <- TextReuseCorpus(paths=corpus_paths, 
                            tokenizer = tokenize_ngrams, n = 5, 
                            minhash_func = minhash, 
                            keep_tokens = TRUE, 
                            progress = TRUE)
  buckets <- lsh(corpus, bands = 60, progress = TRUE)
  candidates <- lsh_candidates(buckets) 
  jaccardsimilarity<- lsh_compare (candidates, corpus, jaccard_similarity, progress=TRUE)
  fwrite (jaccardsimilarity, "filepath/Bill Text Data/jaccardsimilarity101-108.csv", row.names=FALSE)
  corpus_paths <- c(corpus_101, corpus_109)
  corpus <- TextReuseCorpus(paths=corpus_paths, 
                            tokenizer = tokenize_ngrams, n = 5, 
                            minhash_func = minhash, 
                            keep_tokens = TRUE, 
                            progress = TRUE)
  buckets <- lsh(corpus, bands = 60, progress = TRUE)
  candidates <- lsh_candidates(buckets) 
  jaccardsimilarity<- lsh_compare (candidates, corpus, jaccard_similarity, progress=TRUE)
  fwrite (jaccardsimilarity, "filepath/Bill Text Data/jaccardsimilarity101-109.csv", row.names=FALSE)
  corpus_paths <- c(corpus_101, corpus_110)
  corpus <- TextReuseCorpus(paths=corpus_paths, 
                            tokenizer = tokenize_ngrams, n = 5, 
                            minhash_func = minhash, 
                            keep_tokens = TRUE, 
                            progress = TRUE)
  buckets <- lsh(corpus, bands = 60, progress = TRUE)
  candidates <- lsh_candidates(buckets) 
  jaccardsimilarity<- lsh_compare (candidates, corpus, jaccard_similarity, progress=TRUE)
  fwrite (jaccardsimilarity, "filepath/Bill Text Data/jaccardsimilarity101-110.csv", row.names=FALSE)
  corpus_paths <- c(corpus_101, corpus_111)
  corpus <- TextReuseCorpus(paths=corpus_paths, 
                            tokenizer = tokenize_ngrams, n = 5, 
                            minhash_func = minhash, 
                            keep_tokens = TRUE, 
                            progress = TRUE)
  buckets <- lsh(corpus, bands = 60, progress = TRUE)
  candidates <- lsh_candidates(buckets) 
  jaccardsimilarity<- lsh_compare (candidates, corpus, jaccard_similarity, progress=TRUE)
  fwrite (jaccardsimilarity, "filepath/Bill Text Data/jaccardsimilarity101-111.csv", row.names=FALSE)
  corpus_paths <- c(corpus_101, corpus_112)
  corpus <- TextReuseCorpus(paths=corpus_paths, 
                            tokenizer = tokenize_ngrams, n = 5, 
                            minhash_func = minhash, 
                            keep_tokens = TRUE, 
                            progress = TRUE)
  buckets <- lsh(corpus, bands = 60, progress = TRUE)
  candidates <- lsh_candidates(buckets) 
  jaccardsimilarity<- lsh_compare (candidates, corpus, jaccard_similarity, progress=TRUE)
  fwrite (jaccardsimilarity, "filepath/Bill Text Data/jaccardsimilarity101-112.csv", row.names=FALSE)
  corpus_paths <- c(corpus_101, corpus_113)
  corpus <- TextReuseCorpus(paths=corpus_paths, 
                            tokenizer = tokenize_ngrams, n = 5, 
                            minhash_func = minhash, 
                            keep_tokens = TRUE, 
                            progress = TRUE)
  buckets <- lsh(corpus, bands = 60, progress = TRUE)
  candidates <- lsh_candidates(buckets) 
  jaccardsimilarity<- lsh_compare (candidates, corpus, jaccard_similarity, progress=TRUE)
  fwrite (jaccardsimilarity, "filepath/Bill Text Data/jaccardsimilarity101-113.csv", row.names=FALSE)
  corpus_paths <- c(corpus_101, corpus_114)
  corpus <- TextReuseCorpus(paths=corpus_paths, 
                            tokenizer = tokenize_ngrams, n = 5, 
                            minhash_func = minhash, 
                            keep_tokens = TRUE, 
                            progress = TRUE)
  buckets <- lsh(corpus, bands = 60, progress = TRUE)
  candidates <- lsh_candidates(buckets) 
  jaccardsimilarity<- lsh_compare (candidates, corpus, jaccard_similarity, progress=TRUE)
  fwrite (jaccardsimilarity, "filepath/Bill Text Data/jaccardsimilarity101-114.csv", row.names=FALSE)
  
  #102 Corpus & Similarity Measurements
  corpus_paths <- c(corpus_102, corpus_103)
  corpus <- TextReuseCorpus(paths=corpus_paths, 
                            tokenizer = tokenize_ngrams, n = 5, 
                            minhash_func = minhash, 
                            keep_tokens = TRUE, 
                            progress = TRUE)
  buckets <- lsh(corpus, bands = 60, progress = TRUE)
  candidates <- lsh_candidates(buckets) 
  jaccardsimilarity<- lsh_compare (candidates, corpus, jaccard_similarity, progress=TRUE)
  fwrite (jaccardsimilarity, "filepath/Bill Text Data/jaccardsimilarity102-103.csv", row.names=FALSE)
  corpus_paths <- c(corpus_102, corpus_104)
  corpus <- TextReuseCorpus(paths=corpus_paths, 
                            tokenizer = tokenize_ngrams, n = 5, 
                            minhash_func = minhash, 
                            keep_tokens = TRUE, 
                            progress = TRUE)
  buckets <- lsh(corpus, bands = 60, progress = TRUE)
  candidates <- lsh_candidates(buckets) 
  jaccardsimilarity<- lsh_compare (candidates, corpus, jaccard_similarity, progress=TRUE)
  fwrite (jaccardsimilarity, "filepath/Bill Text Data/jaccardsimilarity102-104.csv", row.names=FALSE)
  corpus_paths <- c(corpus_102, corpus_105)
  corpus <- TextReuseCorpus(paths=corpus_paths, 
                            tokenizer = tokenize_ngrams, n = 5, 
                            minhash_func = minhash, 
                            keep_tokens = TRUE, 
                            progress = TRUE)
  buckets <- lsh(corpus, bands = 60, progress = TRUE)
  candidates <- lsh_candidates(buckets) 
  jaccardsimilarity<- lsh_compare (candidates, corpus, jaccard_similarity, progress=TRUE)
  fwrite (jaccardsimilarity, "filepath/Bill Text Data/jaccardsimilarity102-105.csv", row.names=FALSE)
  corpus_paths <- c(corpus_102, corpus_106)
  corpus <- TextReuseCorpus(paths=corpus_paths, 
                            tokenizer = tokenize_ngrams, n = 5, 
                            minhash_func = minhash, 
                            keep_tokens = TRUE, 
                            progress = TRUE)
  buckets <- lsh(corpus, bands = 60, progress = TRUE)
  candidates <- lsh_candidates(buckets) 
  jaccardsimilarity<- lsh_compare (candidates, corpus, jaccard_similarity, progress=TRUE)
  fwrite (jaccardsimilarity, "filepath/Bill Text Data/jaccardsimilarity102-106.csv", row.names=FALSE)
  corpus_paths <- c(corpus_102, corpus_107)
  corpus <- TextReuseCorpus(paths=corpus_paths, 
                            tokenizer = tokenize_ngrams, n = 5, 
                            minhash_func = minhash, 
                            keep_tokens = TRUE, 
                            progress = TRUE)
  buckets <- lsh(corpus, bands = 60, progress = TRUE)
  candidates <- lsh_candidates(buckets) 
  jaccardsimilarity<- lsh_compare (candidates, corpus, jaccard_similarity, progress=TRUE)
  fwrite (jaccardsimilarity, "filepath/Bill Text Data/jaccardsimilarity102-107.csv", row.names=FALSE)
  corpus_paths <- c(corpus_102, corpus_108)
  corpus <- TextReuseCorpus(paths=corpus_paths, 
                            tokenizer = tokenize_ngrams, n = 5, 
                            minhash_func = minhash, 
                            keep_tokens = TRUE, 
                            progress = TRUE)
  buckets <- lsh(corpus, bands = 60, progress = TRUE)
  candidates <- lsh_candidates(buckets) 
  jaccardsimilarity<- lsh_compare (candidates, corpus, jaccard_similarity, progress=TRUE)
  fwrite (jaccardsimilarity, "filepath/Bill Text Data/jaccardsimilarity102-108.csv", row.names=FALSE)
  corpus_paths <- c(corpus_102, corpus_109)
  corpus <- TextReuseCorpus(paths=corpus_paths, 
                            tokenizer = tokenize_ngrams, n = 5, 
                            minhash_func = minhash, 
                            keep_tokens = TRUE, 
                            progress = TRUE)
  buckets <- lsh(corpus, bands = 60, progress = TRUE)
  candidates <- lsh_candidates(buckets) 
  jaccardsimilarity<- lsh_compare (candidates, corpus, jaccard_similarity, progress=TRUE)
  fwrite (jaccardsimilarity, "filepath/Bill Text Data/jaccardsimilarity102-109.csv", row.names=FALSE)
  corpus_paths <- c(corpus_102, corpus_110)
  corpus <- TextReuseCorpus(paths=corpus_paths, 
                            tokenizer = tokenize_ngrams, n = 5, 
                            minhash_func = minhash, 
                            keep_tokens = TRUE, 
                            progress = TRUE)
  buckets <- lsh(corpus, bands = 60, progress = TRUE)
  candidates <- lsh_candidates(buckets) 
  jaccardsimilarity<- lsh_compare (candidates, corpus, jaccard_similarity, progress=TRUE)
  fwrite (jaccardsimilarity, "filepath/Bill Text Data/jaccardsimilarity102-110.csv", row.names=FALSE)
  corpus_paths <- c(corpus_102, corpus_111)
  corpus <- TextReuseCorpus(paths=corpus_paths, 
                            tokenizer = tokenize_ngrams, n = 5, 
                            minhash_func = minhash, 
                            keep_tokens = TRUE, 
                            progress = TRUE)
  buckets <- lsh(corpus, bands = 60, progress = TRUE)
  candidates <- lsh_candidates(buckets) 
  jaccardsimilarity<- lsh_compare (candidates, corpus, jaccard_similarity, progress=TRUE)
  fwrite (jaccardsimilarity, "filepath/Bill Text Data/jaccardsimilarity102-111.csv", row.names=FALSE)
  corpus_paths <- c(corpus_102, corpus_112)
  corpus <- TextReuseCorpus(paths=corpus_paths, 
                            tokenizer = tokenize_ngrams, n = 5, 
                            minhash_func = minhash, 
                            keep_tokens = TRUE, 
                            progress = TRUE)
  buckets <- lsh(corpus, bands = 60, progress = TRUE)
  candidates <- lsh_candidates(buckets) 
  jaccardsimilarity<- lsh_compare (candidates, corpus, jaccard_similarity, progress=TRUE)
  fwrite (jaccardsimilarity, "filepath/Bill Text Data/jaccardsimilarity102-112.csv", row.names=FALSE)
  corpus_paths <- c(corpus_102, corpus_113)
  corpus <- TextReuseCorpus(paths=corpus_paths, 
                            tokenizer = tokenize_ngrams, n = 5, 
                            minhash_func = minhash, 
                            keep_tokens = TRUE, 
                            progress = TRUE)
  buckets <- lsh(corpus, bands = 60, progress = TRUE)
  candidates <- lsh_candidates(buckets) 
  jaccardsimilarity<- lsh_compare (candidates, corpus, jaccard_similarity, progress=TRUE)
  fwrite (jaccardsimilarity, "filepath/Bill Text Data/jaccardsimilarity102-113.csv", row.names=FALSE)
  corpus_paths <- c(corpus_102, corpus_114)
  corpus <- TextReuseCorpus(paths=corpus_paths, 
                            tokenizer = tokenize_ngrams, n = 5, 
                            minhash_func = minhash, 
                            keep_tokens = TRUE, 
                            progress = TRUE)
  buckets <- lsh(corpus, bands = 60, progress = TRUE)
  candidates <- lsh_candidates(buckets) 
  jaccardsimilarity<- lsh_compare (candidates, corpus, jaccard_similarity, progress=TRUE)
  fwrite (jaccardsimilarity, "filepath/Bill Text Data/jaccardsimilarity102-114.csv", row.names=FALSE)
  
  #103 Corpus & Similarity Measurements
  corpus_paths <- c(corpus_103, corpus_104)
  corpus <- TextReuseCorpus(paths=corpus_paths, 
                            tokenizer = tokenize_ngrams, n = 5, 
                            minhash_func = minhash, 
                            keep_tokens = TRUE, 
                            progress = TRUE)
  buckets <- lsh(corpus, bands = 60, progress = TRUE)
  candidates <- lsh_candidates(buckets) 
  jaccardsimilarity<- lsh_compare (candidates, corpus, jaccard_similarity, progress=TRUE)
  fwrite (jaccardsimilarity, "filepath/Bill Text Data/jaccardsimilarity103-104.csv", row.names=FALSE)
  corpus_paths <- c(corpus_103, corpus_105)
  corpus <- TextReuseCorpus(paths=corpus_paths, 
                            tokenizer = tokenize_ngrams, n = 5, 
                            minhash_func = minhash, 
                            keep_tokens = TRUE, 
                            progress = TRUE)
  buckets <- lsh(corpus, bands = 60, progress = TRUE)
  candidates <- lsh_candidates(buckets) 
  jaccardsimilarity<- lsh_compare (candidates, corpus, jaccard_similarity, progress=TRUE)
  fwrite (jaccardsimilarity, "filepath/Bill Text Data/jaccardsimilarity103-105.csv", row.names=FALSE)
  corpus_paths <- c(corpus_103, corpus_106)
  corpus <- TextReuseCorpus(paths=corpus_paths, 
                            tokenizer = tokenize_ngrams, n = 5, 
                            minhash_func = minhash, 
                            keep_tokens = TRUE, 
                            progress = TRUE)
  buckets <- lsh(corpus, bands = 60, progress = TRUE)
  candidates <- lsh_candidates(buckets) 
  jaccardsimilarity<- lsh_compare (candidates, corpus, jaccard_similarity, progress=TRUE)
  fwrite (jaccardsimilarity, "filepath/Bill Text Data/jaccardsimilarity103-106.csv", row.names=FALSE)
  corpus_paths <- c(corpus_103, corpus_107)
  corpus <- TextReuseCorpus(paths=corpus_paths, 
                            tokenizer = tokenize_ngrams, n = 5, 
                            minhash_func = minhash, 
                            keep_tokens = TRUE, 
                            progress = TRUE)
  buckets <- lsh(corpus, bands = 60, progress = TRUE)
  candidates <- lsh_candidates(buckets) 
  jaccardsimilarity<- lsh_compare (candidates, corpus, jaccard_similarity, progress=TRUE)
  fwrite (jaccardsimilarity, "filepath/Bill Text Data/jaccardsimilarity103-107.csv", row.names=FALSE)
  corpus_paths <- c(corpus_103, corpus_108)
  corpus <- TextReuseCorpus(paths=corpus_paths, 
                            tokenizer = tokenize_ngrams, n = 5, 
                            minhash_func = minhash, 
                            keep_tokens = TRUE, 
                            progress = TRUE)
  buckets <- lsh(corpus, bands = 60, progress = TRUE)
  candidates <- lsh_candidates(buckets) 
  jaccardsimilarity<- lsh_compare (candidates, corpus, jaccard_similarity, progress=TRUE)
  fwrite (jaccardsimilarity, "filepath/Bill Text Data/jaccardsimilarity103-108.csv", row.names=FALSE)
  corpus_paths <- c(corpus_103, corpus_109)
  corpus <- TextReuseCorpus(paths=corpus_paths, 
                            tokenizer = tokenize_ngrams, n = 5, 
                            minhash_func = minhash, 
                            keep_tokens = TRUE, 
                            progress = TRUE)
  buckets <- lsh(corpus, bands = 60, progress = TRUE)
  candidates <- lsh_candidates(buckets) 
  jaccardsimilarity<- lsh_compare (candidates, corpus, jaccard_similarity, progress=TRUE)
  fwrite (jaccardsimilarity, "filepath/Bill Text Data/jaccardsimilarity103-109.csv", row.names=FALSE)
  corpus_paths <- c(corpus_103, corpus_110)
  corpus <- TextReuseCorpus(paths=corpus_paths, 
                            tokenizer = tokenize_ngrams, n = 5, 
                            minhash_func = minhash, 
                            keep_tokens = TRUE, 
                            progress = TRUE)
  buckets <- lsh(corpus, bands = 60, progress = TRUE)
  candidates <- lsh_candidates(buckets) 
  jaccardsimilarity<- lsh_compare (candidates, corpus, jaccard_similarity, progress=TRUE)
  fwrite (jaccardsimilarity, "filepath/Bill Text Data/jaccardsimilarity103-110.csv", row.names=FALSE)
  corpus_paths <- c(corpus_103, corpus_111)
  corpus <- TextReuseCorpus(paths=corpus_paths, 
                            tokenizer = tokenize_ngrams, n = 5, 
                            minhash_func = minhash, 
                            keep_tokens = TRUE, 
                            progress = TRUE)
  buckets <- lsh(corpus, bands = 60, progress = TRUE)
  candidates <- lsh_candidates(buckets) 
  jaccardsimilarity<- lsh_compare (candidates, corpus, jaccard_similarity, progress=TRUE)
  fwrite (jaccardsimilarity, "filepath/Bill Text Data/jaccardsimilarity103-111.csv", row.names=FALSE)
  corpus_paths <- c(corpus_103, corpus_112)
  corpus <- TextReuseCorpus(paths=corpus_paths, 
                            tokenizer = tokenize_ngrams, n = 5, 
                            minhash_func = minhash, 
                            keep_tokens = TRUE, 
                            progress = TRUE)
  buckets <- lsh(corpus, bands = 60, progress = TRUE)
  candidates <- lsh_candidates(buckets) 
  jaccardsimilarity<- lsh_compare (candidates, corpus, jaccard_similarity, progress=TRUE)
  fwrite (jaccardsimilarity, "filepath/Bill Text Data/jaccardsimilarity103-112.csv", row.names=FALSE)
  corpus_paths <- c(corpus_103, corpus_113)
  corpus <- TextReuseCorpus(paths=corpus_paths, 
                            tokenizer = tokenize_ngrams, n = 5, 
                            minhash_func = minhash, 
                            keep_tokens = TRUE, 
                            progress = TRUE)
  buckets <- lsh(corpus, bands = 60, progress = TRUE)
  candidates <- lsh_candidates(buckets) 
  jaccardsimilarity<- lsh_compare (candidates, corpus, jaccard_similarity, progress=TRUE)
  fwrite (jaccardsimilarity, "filepath/Bill Text Data/jaccardsimilarity103-113.csv", row.names=FALSE)
  corpus_paths <- c(corpus_103, corpus_114)
  corpus <- TextReuseCorpus(paths=corpus_paths, 
                            tokenizer = tokenize_ngrams, n = 5, 
                            minhash_func = minhash, 
                            keep_tokens = TRUE, 
                            progress = TRUE)
  buckets <- lsh(corpus, bands = 60, progress = TRUE)
  candidates <- lsh_candidates(buckets) 
  jaccardsimilarity<- lsh_compare (candidates, corpus, jaccard_similarity, progress=TRUE)
  fwrite (jaccardsimilarity, "filepath/Bill Text Data/jaccardsimilarity103-114.csv", row.names=FALSE)
  
  #104 Corpus & Similarity Measurements
  corpus_paths <- c(corpus_104, corpus_105)
  corpus <- TextReuseCorpus(paths=corpus_paths, 
                            tokenizer = tokenize_ngrams, n = 5, 
                            minhash_func = minhash, 
                            keep_tokens = TRUE, 
                            progress = TRUE)
  buckets <- lsh(corpus, bands = 60, progress = TRUE)
  candidates <- lsh_candidates(buckets) 
  jaccardsimilarity<- lsh_compare (candidates, corpus, jaccard_similarity, progress=TRUE)
  fwrite (jaccardsimilarity, "filepath/Bill Text Data/jaccardsimilarity104-105.csv", row.names=FALSE)
  corpus_paths <- c(corpus_104, corpus_106)
  corpus <- TextReuseCorpus(paths=corpus_paths, 
                            tokenizer = tokenize_ngrams, n = 5, 
                            minhash_func = minhash, 
                            keep_tokens = TRUE, 
                            progress = TRUE)
  buckets <- lsh(corpus, bands = 60, progress = TRUE)
  candidates <- lsh_candidates(buckets) 
  jaccardsimilarity<- lsh_compare (candidates, corpus, jaccard_similarity, progress=TRUE)
  fwrite (jaccardsimilarity, "filepath/Bill Text Data/jaccardsimilarity104-106.csv", row.names=FALSE)
  corpus_paths <- c(corpus_104, corpus_107)
  corpus <- TextReuseCorpus(paths=corpus_paths, 
                            tokenizer = tokenize_ngrams, n = 5, 
                            minhash_func = minhash, 
                            keep_tokens = TRUE, 
                            progress = TRUE)
  buckets <- lsh(corpus, bands = 60, progress = TRUE)
  candidates <- lsh_candidates(buckets) 
  jaccardsimilarity<- lsh_compare (candidates, corpus, jaccard_similarity, progress=TRUE)
  fwrite (jaccardsimilarity, "filepath/Bill Text Data/jaccardsimilarity104-107.csv", row.names=FALSE)
  corpus_paths <- c(corpus_104, corpus_108)
  corpus <- TextReuseCorpus(paths=corpus_paths, 
                            tokenizer = tokenize_ngrams, n = 5, 
                            minhash_func = minhash, 
                            keep_tokens = TRUE, 
                            progress = TRUE)
  buckets <- lsh(corpus, bands = 60, progress = TRUE)
  candidates <- lsh_candidates(buckets) 
  jaccardsimilarity<- lsh_compare (candidates, corpus, jaccard_similarity, progress=TRUE)
  fwrite (jaccardsimilarity, "filepath/Bill Text Data/jaccardsimilarity104-108.csv", row.names=FALSE)
  corpus_paths <- c(corpus_104, corpus_109)
  corpus <- TextReuseCorpus(paths=corpus_paths, 
                            tokenizer = tokenize_ngrams, n = 5, 
                            minhash_func = minhash, 
                            keep_tokens = TRUE, 
                            progress = TRUE)
  buckets <- lsh(corpus, bands = 60, progress = TRUE)
  candidates <- lsh_candidates(buckets) 
  jaccardsimilarity<- lsh_compare (candidates, corpus, jaccard_similarity, progress=TRUE)
  fwrite (jaccardsimilarity, "filepath/Bill Text Data/jaccardsimilarity104-109.csv", row.names=FALSE)
  corpus_paths <- c(corpus_104, corpus_110)
  corpus <- TextReuseCorpus(paths=corpus_paths, 
                            tokenizer = tokenize_ngrams, n = 5, 
                            minhash_func = minhash, 
                            keep_tokens = TRUE, 
                            progress = TRUE)
  buckets <- lsh(corpus, bands = 60, progress = TRUE)
  candidates <- lsh_candidates(buckets) 
  jaccardsimilarity<- lsh_compare (candidates, corpus, jaccard_similarity, progress=TRUE)
  fwrite (jaccardsimilarity, "filepath/Bill Text Data/jaccardsimilarity104-110.csv", row.names=FALSE)
  corpus_paths <- c(corpus_104, corpus_111)
  corpus <- TextReuseCorpus(paths=corpus_paths, 
                            tokenizer = tokenize_ngrams, n = 5, 
                            minhash_func = minhash, 
                            keep_tokens = TRUE, 
                            progress = TRUE)
  buckets <- lsh(corpus, bands = 60, progress = TRUE)
  candidates <- lsh_candidates(buckets) 
  jaccardsimilarity<- lsh_compare (candidates, corpus, jaccard_similarity, progress=TRUE)
  fwrite (jaccardsimilarity, "filepath/Bill Text Data/jaccardsimilarity104-111.csv", row.names=FALSE)
  corpus_paths <- c(corpus_104, corpus_112)
  corpus <- TextReuseCorpus(paths=corpus_paths, 
                            tokenizer = tokenize_ngrams, n = 5, 
                            minhash_func = minhash, 
                            keep_tokens = TRUE, 
                            progress = TRUE)
  buckets <- lsh(corpus, bands = 60, progress = TRUE)
  candidates <- lsh_candidates(buckets) 
  jaccardsimilarity<- lsh_compare (candidates, corpus, jaccard_similarity, progress=TRUE)
  fwrite (jaccardsimilarity, "filepath/Bill Text Data/jaccardsimilarity104-112.csv", row.names=FALSE)
  corpus_paths <- c(corpus_104, corpus_113)
  corpus <- TextReuseCorpus(paths=corpus_paths, 
                            tokenizer = tokenize_ngrams, n = 5, 
                            minhash_func = minhash, 
                            keep_tokens = TRUE, 
                            progress = TRUE)
  buckets <- lsh(corpus, bands = 60, progress = TRUE)
  candidates <- lsh_candidates(buckets) 
  jaccardsimilarity<- lsh_compare (candidates, corpus, jaccard_similarity, progress=TRUE)
  fwrite (jaccardsimilarity, "filepath/Bill Text Data/jaccardsimilarity104-113.csv", row.names=FALSE)
  corpus_paths <- c(corpus_104, corpus_114)
  corpus <- TextReuseCorpus(paths=corpus_paths, 
                            tokenizer = tokenize_ngrams, n = 5, 
                            minhash_func = minhash, 
                            keep_tokens = TRUE, 
                            progress = TRUE)
  buckets <- lsh(corpus, bands = 60, progress = TRUE)
  candidates <- lsh_candidates(buckets) 
  jaccardsimilarity<- lsh_compare (candidates, corpus, jaccard_similarity, progress=TRUE)
  fwrite (jaccardsimilarity, "filepath/Bill Text Data/jaccardsimilarity104-114.csv", row.names=FALSE)
  
  #105 Corpus & Similarity Measurements
  corpus_paths <- c(corpus_105, corpus_106)
  corpus <- TextReuseCorpus(paths=corpus_paths, 
                            tokenizer = tokenize_ngrams, n = 5, 
                            minhash_func = minhash, 
                            keep_tokens = TRUE, 
                            progress = TRUE)
  buckets <- lsh(corpus, bands = 60, progress = TRUE)
  candidates <- lsh_candidates(buckets) 
  jaccardsimilarity<- lsh_compare (candidates, corpus, jaccard_similarity, progress=TRUE)
  fwrite (jaccardsimilarity, "filepath/Bill Text Data/jaccardsimilarity105-106.csv", row.names=FALSE)
  corpus_paths <- c(corpus_105, corpus_107)
  corpus <- TextReuseCorpus(paths=corpus_paths, 
                            tokenizer = tokenize_ngrams, n = 5, 
                            minhash_func = minhash, 
                            keep_tokens = TRUE, 
                            progress = TRUE)
  buckets <- lsh(corpus, bands = 60, progress = TRUE)
  candidates <- lsh_candidates(buckets) 
  jaccardsimilarity<- lsh_compare (candidates, corpus, jaccard_similarity, progress=TRUE)
  fwrite (jaccardsimilarity, "filepath/Bill Text Data/jaccardsimilarity105-107.csv", row.names=FALSE)
  corpus_paths <- c(corpus_105, corpus_108)
  corpus <- TextReuseCorpus(paths=corpus_paths, 
                            tokenizer = tokenize_ngrams, n = 5, 
                            minhash_func = minhash, 
                            keep_tokens = TRUE, 
                            progress = TRUE)
  buckets <- lsh(corpus, bands = 60, progress = TRUE)
  candidates <- lsh_candidates(buckets) 
  jaccardsimilarity<- lsh_compare (candidates, corpus, jaccard_similarity, progress=TRUE)
  fwrite (jaccardsimilarity, "filepath/Bill Text Data/jaccardsimilarity105-108.csv", row.names=FALSE)
  corpus_paths <- c(corpus_105, corpus_109)
  corpus <- TextReuseCorpus(paths=corpus_paths, 
                            tokenizer = tokenize_ngrams, n = 5, 
                            minhash_func = minhash, 
                            keep_tokens = TRUE, 
                            progress = TRUE)
  buckets <- lsh(corpus, bands = 60, progress = TRUE)
  candidates <- lsh_candidates(buckets) 
  jaccardsimilarity<- lsh_compare (candidates, corpus, jaccard_similarity, progress=TRUE)
  fwrite (jaccardsimilarity, "filepath/Bill Text Data/jaccardsimilarity105-109.csv", row.names=FALSE)
  corpus_paths <- c(corpus_105, corpus_110)
  corpus <- TextReuseCorpus(paths=corpus_paths, 
                            tokenizer = tokenize_ngrams, n = 5, 
                            minhash_func = minhash, 
                            keep_tokens = TRUE, 
                            progress = TRUE)
  buckets <- lsh(corpus, bands = 60, progress = TRUE)
  candidates <- lsh_candidates(buckets) 
  jaccardsimilarity<- lsh_compare (candidates, corpus, jaccard_similarity, progress=TRUE)
  fwrite (jaccardsimilarity, "filepath/Bill Text Data/jaccardsimilarity105-110.csv", row.names=FALSE)
  corpus_paths <- c(corpus_105, corpus_111)
  corpus <- TextReuseCorpus(paths=corpus_paths, 
                            tokenizer = tokenize_ngrams, n = 5, 
                            minhash_func = minhash, 
                            keep_tokens = TRUE, 
                            progress = TRUE)
  buckets <- lsh(corpus, bands = 60, progress = TRUE)
  candidates <- lsh_candidates(buckets) 
  jaccardsimilarity<- lsh_compare (candidates, corpus, jaccard_similarity, progress=TRUE)
  fwrite (jaccardsimilarity, "filepath/Bill Text Data/jaccardsimilarity105-111.csv", row.names=FALSE)
  corpus_paths <- c(corpus_105, corpus_112)
  corpus <- TextReuseCorpus(paths=corpus_paths, 
                            tokenizer = tokenize_ngrams, n = 5, 
                            minhash_func = minhash, 
                            keep_tokens = TRUE, 
                            progress = TRUE)
  buckets <- lsh(corpus, bands = 60, progress = TRUE)
  candidates <- lsh_candidates(buckets) 
  jaccardsimilarity<- lsh_compare (candidates, corpus, jaccard_similarity, progress=TRUE)
  fwrite (jaccardsimilarity, "filepath/Bill Text Data/jaccardsimilarity105-112.csv", row.names=FALSE)
  corpus_paths <- c(corpus_105, corpus_113)
  corpus <- TextReuseCorpus(paths=corpus_paths, 
                            tokenizer = tokenize_ngrams, n = 5, 
                            minhash_func = minhash, 
                            keep_tokens = TRUE, 
                            progress = TRUE)
  buckets <- lsh(corpus, bands = 60, progress = TRUE)
  candidates <- lsh_candidates(buckets) 
  jaccardsimilarity<- lsh_compare (candidates, corpus, jaccard_similarity, progress=TRUE)
  fwrite (jaccardsimilarity, "filepath/Bill Text Data/jaccardsimilarity105-113.csv", row.names=FALSE)
  corpus_paths <- c(corpus_105, corpus_114)
  corpus <- TextReuseCorpus(paths=corpus_paths, 
                            tokenizer = tokenize_ngrams, n = 5, 
                            minhash_func = minhash, 
                            keep_tokens = TRUE, 
                            progress = TRUE)
  buckets <- lsh(corpus, bands = 60, progress = TRUE)
  candidates <- lsh_candidates(buckets) 
  jaccardsimilarity<- lsh_compare (candidates, corpus, jaccard_similarity, progress=TRUE)
  fwrite (jaccardsimilarity, "filepath/Bill Text Data/jaccardsimilarity105-114.csv", row.names=FALSE)
  
  #106 Corpus & Similarity Measurements
  corpus_paths <- c(corpus_106, corpus_107)
  corpus <- TextReuseCorpus(paths=corpus_paths, 
                            tokenizer = tokenize_ngrams, n = 5, 
                            minhash_func = minhash, 
                            keep_tokens = TRUE, 
                            progress = TRUE)
  buckets <- lsh(corpus, bands = 60, progress = TRUE)
  candidates <- lsh_candidates(buckets) 
  jaccardsimilarity<- lsh_compare (candidates, corpus, jaccard_similarity, progress=TRUE)
  fwrite (jaccardsimilarity, "filepath/Bill Text Data/jaccardsimilarity106-107.csv", row.names=FALSE)
  corpus_paths <- c(corpus_106, corpus_108)
  corpus <- TextReuseCorpus(paths=corpus_paths, 
                            tokenizer = tokenize_ngrams, n = 5, 
                            minhash_func = minhash, 
                            keep_tokens = TRUE, 
                            progress = TRUE)
  buckets <- lsh(corpus, bands = 60, progress = TRUE)
  candidates <- lsh_candidates(buckets) 
  jaccardsimilarity<- lsh_compare (candidates, corpus, jaccard_similarity, progress=TRUE)
  fwrite (jaccardsimilarity, "filepath/Bill Text Data/jaccardsimilarity106-108.csv", row.names=FALSE)
  corpus_paths <- c(corpus_106, corpus_109)
  corpus <- TextReuseCorpus(paths=corpus_paths, 
                            tokenizer = tokenize_ngrams, n = 5, 
                            minhash_func = minhash, 
                            keep_tokens = TRUE, 
                            progress = TRUE)
  buckets <- lsh(corpus, bands = 60, progress = TRUE)
  candidates <- lsh_candidates(buckets) 
  jaccardsimilarity<- lsh_compare (candidates, corpus, jaccard_similarity, progress=TRUE)
  fwrite (jaccardsimilarity, "filepath/Bill Text Data/jaccardsimilarity106-109.csv", row.names=FALSE)
  corpus_paths <- c(corpus_106, corpus_110)
  corpus <- TextReuseCorpus(paths=corpus_paths, 
                            tokenizer = tokenize_ngrams, n = 5, 
                            minhash_func = minhash, 
                            keep_tokens = TRUE, 
                            progress = TRUE)
  buckets <- lsh(corpus, bands = 60, progress = TRUE)
  candidates <- lsh_candidates(buckets) 
  jaccardsimilarity<- lsh_compare (candidates, corpus, jaccard_similarity, progress=TRUE)
  fwrite (jaccardsimilarity, "filepath/Bill Text Data/jaccardsimilarity106-110.csv", row.names=FALSE)
  corpus_paths <- c(corpus_106, corpus_111)
  corpus <- TextReuseCorpus(paths=corpus_paths, 
                            tokenizer = tokenize_ngrams, n = 5, 
                            minhash_func = minhash, 
                            keep_tokens = TRUE, 
                            progress = TRUE)
  buckets <- lsh(corpus, bands = 60, progress = TRUE)
  candidates <- lsh_candidates(buckets) 
  jaccardsimilarity<- lsh_compare (candidates, corpus, jaccard_similarity, progress=TRUE)
  fwrite (jaccardsimilarity, "filepath/Bill Text Data/jaccardsimilarity106-111.csv", row.names=FALSE)
  corpus_paths <- c(corpus_106, corpus_112)
  corpus <- TextReuseCorpus(paths=corpus_paths, 
                            tokenizer = tokenize_ngrams, n = 5, 
                            minhash_func = minhash, 
                            keep_tokens = TRUE, 
                            progress = TRUE)
  buckets <- lsh(corpus, bands = 60, progress = TRUE)
  candidates <- lsh_candidates(buckets) 
  jaccardsimilarity<- lsh_compare (candidates, corpus, jaccard_similarity, progress=TRUE)
  fwrite (jaccardsimilarity, "filepath/Bill Text Data/jaccardsimilarity106-112.csv", row.names=FALSE)
  corpus_paths <- c(corpus_106, corpus_113)
  corpus <- TextReuseCorpus(paths=corpus_paths, 
                            tokenizer = tokenize_ngrams, n = 5, 
                            minhash_func = minhash, 
                            keep_tokens = TRUE, 
                            progress = TRUE)
  buckets <- lsh(corpus, bands = 60, progress = TRUE)
  candidates <- lsh_candidates(buckets) 
  jaccardsimilarity<- lsh_compare (candidates, corpus, jaccard_similarity, progress=TRUE)
  fwrite (jaccardsimilarity, "filepath/Bill Text Data/jaccardsimilarity106-113.csv", row.names=FALSE)
  corpus_paths <- c(corpus_106, corpus_114)
  corpus <- TextReuseCorpus(paths=corpus_paths, 
                            tokenizer = tokenize_ngrams, n = 5, 
                            minhash_func = minhash, 
                            keep_tokens = TRUE, 
                            progress = TRUE)
  buckets <- lsh(corpus, bands = 60, progress = TRUE)
  candidates <- lsh_candidates(buckets) 
  jaccardsimilarity<- lsh_compare (candidates, corpus, jaccard_similarity, progress=TRUE)
  fwrite (jaccardsimilarity, "filepath/Bill Text Data/jaccardsimilarity106-114.csv", row.names=FALSE)
  
  #107 Corpus & Similarity Measurements
  corpus_paths <- c(corpus_107, corpus_108)
  corpus <- TextReuseCorpus(paths=corpus_paths, 
                            tokenizer = tokenize_ngrams, n = 5, 
                            minhash_func = minhash, 
                            keep_tokens = TRUE, 
                            progress = TRUE)
  buckets <- lsh(corpus, bands = 60, progress = TRUE)
  candidates <- lsh_candidates(buckets) 
  jaccardsimilarity<- lsh_compare (candidates, corpus, jaccard_similarity, progress=TRUE)
  fwrite (jaccardsimilarity, "filepath/Bill Text Data/jaccardsimilarity107-108.csv", row.names=FALSE)
  corpus_paths <- c(corpus_107, corpus_109)
  corpus <- TextReuseCorpus(paths=corpus_paths, 
                            tokenizer = tokenize_ngrams, n = 5, 
                            minhash_func = minhash, 
                            keep_tokens = TRUE, 
                            progress = TRUE)
  buckets <- lsh(corpus, bands = 60, progress = TRUE)
  candidates <- lsh_candidates(buckets) 
  jaccardsimilarity<- lsh_compare (candidates, corpus, jaccard_similarity, progress=TRUE)
  fwrite (jaccardsimilarity, "filepath/Bill Text Data/jaccardsimilarity107-109.csv", row.names=FALSE)
  corpus_paths <- c(corpus_107, corpus_110)
  corpus <- TextReuseCorpus(paths=corpus_paths, 
                            tokenizer = tokenize_ngrams, n = 5, 
                            minhash_func = minhash, 
                            keep_tokens = TRUE, 
                            progress = TRUE)
  buckets <- lsh(corpus, bands = 60, progress = TRUE)
  candidates <- lsh_candidates(buckets) 
  jaccardsimilarity<- lsh_compare (candidates, corpus, jaccard_similarity, progress=TRUE)
  fwrite (jaccardsimilarity, "filepath/Bill Text Data/jaccardsimilarity107-110.csv", row.names=FALSE)
  corpus_paths <- c(corpus_107, corpus_111)
  corpus <- TextReuseCorpus(paths=corpus_paths, 
                            tokenizer = tokenize_ngrams, n = 5, 
                            minhash_func = minhash, 
                            keep_tokens = TRUE, 
                            progress = TRUE)
  buckets <- lsh(corpus, bands = 60, progress = TRUE)
  candidates <- lsh_candidates(buckets) 
  jaccardsimilarity<- lsh_compare (candidates, corpus, jaccard_similarity, progress=TRUE)
  fwrite (jaccardsimilarity, "filepath/Bill Text Data/jaccardsimilarity107-111.csv", row.names=FALSE)
  corpus_paths <- c(corpus_107, corpus_112)
  corpus <- TextReuseCorpus(paths=corpus_paths, 
                            tokenizer = tokenize_ngrams, n = 5, 
                            minhash_func = minhash, 
                            keep_tokens = TRUE, 
                            progress = TRUE)
  buckets <- lsh(corpus, bands = 60, progress = TRUE)
  candidates <- lsh_candidates(buckets) 
  jaccardsimilarity<- lsh_compare (candidates, corpus, jaccard_similarity, progress=TRUE)
  fwrite (jaccardsimilarity, "filepath/Bill Text Data/jaccardsimilarity107-112.csv", row.names=FALSE)
  corpus_paths <- c(corpus_107, corpus_113)
  corpus <- TextReuseCorpus(paths=corpus_paths, 
                            tokenizer = tokenize_ngrams, n = 5, 
                            minhash_func = minhash, 
                            keep_tokens = TRUE, 
                            progress = TRUE)
  buckets <- lsh(corpus, bands = 60, progress = TRUE)
  candidates <- lsh_candidates(buckets) 
  jaccardsimilarity<- lsh_compare (candidates, corpus, jaccard_similarity, progress=TRUE)
  fwrite (jaccardsimilarity, "filepath/Bill Text Data/jaccardsimilarity107-113.csv", row.names=FALSE)
  corpus_paths <- c(corpus_107, corpus_114)
  corpus <- TextReuseCorpus(paths=corpus_paths, 
                            tokenizer = tokenize_ngrams, n = 5, 
                            minhash_func = minhash, 
                            keep_tokens = TRUE, 
                            progress = TRUE)
  buckets <- lsh(corpus, bands = 60, progress = TRUE)
  candidates <- lsh_candidates(buckets) 
  jaccardsimilarity<- lsh_compare (candidates, corpus, jaccard_similarity, progress=TRUE)
  fwrite (jaccardsimilarity, "filepath/Bill Text Data/jaccardsimilarity107-114.csv", row.names=FALSE)
  
  #108 Corpus & Similarity Measurements
  corpus_paths <- c(corpus_108, corpus_109)
  corpus <- TextReuseCorpus(paths=corpus_paths, 
                            tokenizer = tokenize_ngrams, n = 5, 
                            minhash_func = minhash, 
                            keep_tokens = TRUE, 
                            progress = TRUE)
  buckets <- lsh(corpus, bands = 60, progress = TRUE)
  candidates <- lsh_candidates(buckets) 
  jaccardsimilarity<- lsh_compare (candidates, corpus, jaccard_similarity, progress=TRUE)
  fwrite (jaccardsimilarity, "filepath/Bill Text Data/jaccardsimilarity108-109.csv", row.names=FALSE)
  corpus_paths <- c(corpus_108, corpus_110)
  corpus <- TextReuseCorpus(paths=corpus_paths, 
                            tokenizer = tokenize_ngrams, n = 5, 
                            minhash_func = minhash, 
                            keep_tokens = TRUE, 
                            progress = TRUE)
  buckets <- lsh(corpus, bands = 60, progress = TRUE)
  candidates <- lsh_candidates(buckets) 
  jaccardsimilarity<- lsh_compare (candidates, corpus, jaccard_similarity, progress=TRUE)
  fwrite (jaccardsimilarity, "filepath/Bill Text Data/jaccardsimilarity108-110.csv", row.names=FALSE)
  corpus_paths <- c(corpus_108, corpus_111)
  corpus <- TextReuseCorpus(paths=corpus_paths, 
                            tokenizer = tokenize_ngrams, n = 5, 
                            minhash_func = minhash, 
                            keep_tokens = TRUE, 
                            progress = TRUE)
  buckets <- lsh(corpus, bands = 60, progress = TRUE)
  candidates <- lsh_candidates(buckets) 
  jaccardsimilarity<- lsh_compare (candidates, corpus, jaccard_similarity, progress=TRUE)
  fwrite (jaccardsimilarity, "filepath/Bill Text Data/jaccardsimilarity108-111.csv", row.names=FALSE)
  corpus_paths <- c(corpus_108, corpus_112)
  corpus <- TextReuseCorpus(paths=corpus_paths, 
                            tokenizer = tokenize_ngrams, n = 5, 
                            minhash_func = minhash, 
                            keep_tokens = TRUE, 
                            progress = TRUE)
  buckets <- lsh(corpus, bands = 60, progress = TRUE)
  candidates <- lsh_candidates(buckets) 
  jaccardsimilarity<- lsh_compare (candidates, corpus, jaccard_similarity, progress=TRUE)
  fwrite (jaccardsimilarity, "filepath/Bill Text Data/jaccardsimilarity108-112.csv", row.names=FALSE)
  corpus_paths <- c(corpus_108, corpus_113)
  corpus <- TextReuseCorpus(paths=corpus_paths, 
                            tokenizer = tokenize_ngrams, n = 5, 
                            minhash_func = minhash, 
                            keep_tokens = TRUE, 
                            progress = TRUE)
  buckets <- lsh(corpus, bands = 60, progress = TRUE)
  candidates <- lsh_candidates(buckets) 
  jaccardsimilarity<- lsh_compare (candidates, corpus, jaccard_similarity, progress=TRUE)
  fwrite (jaccardsimilarity, "filepath/Bill Text Data/jaccardsimilarity108-113.csv", row.names=FALSE)
  corpus_paths <- c(corpus_108, corpus_114)
  corpus <- TextReuseCorpus(paths=corpus_paths, 
                            tokenizer = tokenize_ngrams, n = 5, 
                            minhash_func = minhash, 
                            keep_tokens = TRUE, 
                            progress = TRUE)
  buckets <- lsh(corpus, bands = 60, progress = TRUE)
  candidates <- lsh_candidates(buckets) 
  jaccardsimilarity<- lsh_compare (candidates, corpus, jaccard_similarity, progress=TRUE)
  fwrite (jaccardsimilarity, "filepath/Bill Text Data/jaccardsimilarity108-114.csv", row.names=FALSE)
  
  #109 Corpus & Similarity Measurements
  corpus_paths <- c(corpus_109, corpus_110)
  corpus <- TextReuseCorpus(paths=corpus_paths, 
                            tokenizer = tokenize_ngrams, n = 5, 
                            minhash_func = minhash, 
                            keep_tokens = TRUE, 
                            progress = TRUE)
  buckets <- lsh(corpus, bands = 60, progress = TRUE)
  candidates <- lsh_candidates(buckets) 
  jaccardsimilarity<- lsh_compare (candidates, corpus, jaccard_similarity, progress=TRUE)
  fwrite (jaccardsimilarity, "filepath/Bill Text Data/jaccardsimilarity109-110.csv", row.names=FALSE)
  corpus_paths <- c(corpus_109, corpus_111)
  corpus <- TextReuseCorpus(paths=corpus_paths, 
                            tokenizer = tokenize_ngrams, n = 5, 
                            minhash_func = minhash, 
                            keep_tokens = TRUE, 
                            progress = TRUE)
  buckets <- lsh(corpus, bands = 60, progress = TRUE)
  candidates <- lsh_candidates(buckets) 
  jaccardsimilarity<- lsh_compare (candidates, corpus, jaccard_similarity, progress=TRUE)
  fwrite (jaccardsimilarity, "filepath/Bill Text Data/jaccardsimilarity109-111.csv", row.names=FALSE)
  corpus_paths <- c(corpus_109, corpus_112)
  corpus <- TextReuseCorpus(paths=corpus_paths, 
                            tokenizer = tokenize_ngrams, n = 5, 
                            minhash_func = minhash, 
                            keep_tokens = TRUE, 
                            progress = TRUE)
  buckets <- lsh(corpus, bands = 60, progress = TRUE)
  candidates <- lsh_candidates(buckets) 
  jaccardsimilarity<- lsh_compare (candidates, corpus, jaccard_similarity, progress=TRUE)
  fwrite (jaccardsimilarity, "filepath/Bill Text Data/jaccardsimilarity109-112.csv", row.names=FALSE)
  corpus_paths <- c(corpus_109, corpus_113)
  corpus <- TextReuseCorpus(paths=corpus_paths, 
                            tokenizer = tokenize_ngrams, n = 5, 
                            minhash_func = minhash, 
                            keep_tokens = TRUE, 
                            progress = TRUE)
  buckets <- lsh(corpus, bands = 60, progress = TRUE)
  candidates <- lsh_candidates(buckets) 
  jaccardsimilarity<- lsh_compare (candidates, corpus, jaccard_similarity, progress=TRUE)
  fwrite (jaccardsimilarity, "filepath/Bill Text Data/jaccardsimilarity109-113.csv", row.names=FALSE)
  corpus_paths <- c(corpus_109, corpus_114)
  corpus <- TextReuseCorpus(paths=corpus_paths, 
                            tokenizer = tokenize_ngrams, n = 5, 
                            minhash_func = minhash, 
                            keep_tokens = TRUE, 
                            progress = TRUE)
  buckets <- lsh(corpus, bands = 60, progress = TRUE)
  candidates <- lsh_candidates(buckets) 
  jaccardsimilarity<- lsh_compare (candidates, corpus, jaccard_similarity, progress=TRUE)
  fwrite (jaccardsimilarity, "filepath/Bill Text Data/jaccardsimilarity109-114.csv", row.names=FALSE)
  
  #110 Corpus & Similarity Measurements
  corpus_paths <- c(corpus_110, corpus_111)
  corpus <- TextReuseCorpus(paths=corpus_paths, 
                            tokenizer = tokenize_ngrams, n = 5, 
                            minhash_func = minhash, 
                            keep_tokens = TRUE, 
                            progress = TRUE)
  buckets <- lsh(corpus, bands = 60, progress = TRUE)
  candidates <- lsh_candidates(buckets) 
  jaccardsimilarity<- lsh_compare (candidates, corpus, jaccard_similarity, progress=TRUE)
  fwrite (jaccardsimilarity, "filepath/Bill Text Data/jaccardsimilarity110-111.csv", row.names=FALSE)
  corpus_paths <- c(corpus_110, corpus_112)
  corpus <- TextReuseCorpus(paths=corpus_paths, 
                            tokenizer = tokenize_ngrams, n = 5, 
                            minhash_func = minhash, 
                            keep_tokens = TRUE, 
                            progress = TRUE)
  buckets <- lsh(corpus, bands = 60, progress = TRUE)
  candidates <- lsh_candidates(buckets) 
  jaccardsimilarity<- lsh_compare (candidates, corpus, jaccard_similarity, progress=TRUE)
  fwrite (jaccardsimilarity, "filepath/Bill Text Data/jaccardsimilarity110-112.csv", row.names=FALSE)
  corpus_paths <- c(corpus_110, corpus_113)
  corpus <- TextReuseCorpus(paths=corpus_paths, 
                            tokenizer = tokenize_ngrams, n = 5, 
                            minhash_func = minhash, 
                            keep_tokens = TRUE, 
                            progress = TRUE)
  buckets <- lsh(corpus, bands = 60, progress = TRUE)
  candidates <- lsh_candidates(buckets) 
  jaccardsimilarity<- lsh_compare (candidates, corpus, jaccard_similarity, progress=TRUE)
  fwrite (jaccardsimilarity, "filepath/Bill Text Data/jaccardsimilarity110-113.csv", row.names=FALSE)
  corpus_paths <- c(corpus_110, corpus_114)
  corpus <- TextReuseCorpus(paths=corpus_paths, 
                            tokenizer = tokenize_ngrams, n = 5, 
                            minhash_func = minhash, 
                            keep_tokens = TRUE, 
                            progress = TRUE)
  buckets <- lsh(corpus, bands = 60, progress = TRUE)
  candidates <- lsh_candidates(buckets) 
  jaccardsimilarity<- lsh_compare (candidates, corpus, jaccard_similarity, progress=TRUE)
  fwrite (jaccardsimilarity, "filepath/Bill Text Data/jaccardsimilarity110-114.csv", row.names=FALSE)
  
  #111 Corpus & Similarity Measurements
  corpus_paths <- c(corpus_111, corpus_112)
  corpus <- TextReuseCorpus(paths=corpus_paths, 
                            tokenizer = tokenize_ngrams, n = 5, 
                            minhash_func = minhash, 
                            keep_tokens = TRUE, 
                            progress = TRUE)
  buckets <- lsh(corpus, bands = 60, progress = TRUE)
  candidates <- lsh_candidates(buckets) 
  jaccardsimilarity<- lsh_compare (candidates, corpus, jaccard_similarity, progress=TRUE)
  fwrite (jaccardsimilarity, "filepath/Bill Text Data/jaccardsimilarity111-112.csv", row.names=FALSE)
  corpus_paths <- c(corpus_111, corpus_113)
  corpus <- TextReuseCorpus(paths=corpus_paths, 
                            tokenizer = tokenize_ngrams, n = 5, 
                            minhash_func = minhash, 
                            keep_tokens = TRUE, 
                            progress = TRUE)
  buckets <- lsh(corpus, bands = 60, progress = TRUE)
  candidates <- lsh_candidates(buckets) 
  jaccardsimilarity<- lsh_compare (candidates, corpus, jaccard_similarity, progress=TRUE)
  fwrite (jaccardsimilarity, "filepath/Bill Text Data/jaccardsimilarity111-113.csv", row.names=FALSE)
  corpus_paths <- c(corpus_111, corpus_114)
  corpus <- TextReuseCorpus(paths=corpus_paths, 
                            tokenizer = tokenize_ngrams, n = 5, 
                            minhash_func = minhash, 
                            keep_tokens = TRUE, 
                            progress = TRUE)
  buckets <- lsh(corpus, bands = 60, progress = TRUE)
  candidates <- lsh_candidates(buckets) 
  jaccardsimilarity<- lsh_compare (candidates, corpus, jaccard_similarity, progress=TRUE)
  fwrite (jaccardsimilarity, "filepath/Bill Text Data/jaccardsimilarity111-114.csv", row.names=FALSE)
  
  #112 Corpus & Similarity Measurements
  corpus_paths <- c(corpus_112, corpus_113)
  corpus <- TextReuseCorpus(paths=corpus_paths, 
                            tokenizer = tokenize_ngrams, n = 5, 
                            minhash_func = minhash, 
                            keep_tokens = TRUE, 
                            progress = TRUE)
  buckets <- lsh(corpus, bands = 60, progress = TRUE)
  candidates <- lsh_candidates(buckets) 
  jaccardsimilarity<- lsh_compare (candidates, corpus, jaccard_similarity, progress=TRUE)
  fwrite (jaccardsimilarity, "filepath/Bill Text Data/jaccardsimilarity112-113.csv", row.names=FALSE)
  corpus_paths <- c(corpus_112, corpus_114)
  corpus <- TextReuseCorpus(paths=corpus_paths, 
                            tokenizer = tokenize_ngrams, n = 5, 
                            minhash_func = minhash, 
                            keep_tokens = TRUE, 
                            progress = TRUE)
  buckets <- lsh(corpus, bands = 60, progress = TRUE)
  candidates <- lsh_candidates(buckets) 
  jaccardsimilarity<- lsh_compare (candidates, corpus, jaccard_similarity, progress=TRUE)
  fwrite (jaccardsimilarity, "filepath/Bill Text Data/jaccardsimilarity112-114.csv", row.names=FALSE)
  
  #113 Corpus & Similarity Measurements
  corpus_paths <- c(corpus_113, corpus_114)
  corpus <- TextReuseCorpus(paths=corpus_paths, 
                            tokenizer = tokenize_ngrams, n = 5, 
                            minhash_func = minhash, 
                            keep_tokens = TRUE, 
                            progress = TRUE)
  buckets <- lsh(corpus, bands = 60, progress = TRUE)
  candidates <- lsh_candidates(buckets) 
  jaccardsimilarity<- lsh_compare (candidates, corpus, jaccard_similarity, progress=TRUE)
  fwrite (jaccardsimilarity, "filepath/Bill Text Data/jaccardsimilarity113-114.csv", row.names=FALSE)
  
  
#6. Alignment Measurement Part Two - Smith-Waterman 
  #Import Data
  #NOTE: Using Processed Jaccard Similarity Output Combined with Bill & Section Data from STATA (See LawProM.do - Section 6)
  dataFrame <- read.csv("filepath/Data/Bill Status with Jaccard.csv")
  library(textreuse)
  library(data.table)
  #Create Column for Smith-Waterman Score
  dataFrame$smithwaterman_score <- ""
  for(i in 1:nrow(dataFrame)) 
  {
    section_a <- as.vector(dataFrame[i,]$bill_textsections_a)
    section_b <- as.vector(dataFrame[i,]$bill_textsections_b)
    alignment_smithwaterman <- align_local(section_a, 
                                           section_b, match = 2L, mismatch = -1L, 
                                           gap = -1L, edit_mark = "#", progress = interactive())
    dataFrame[i,]$smithwaterman_score <- alignment_smithwaterman$score
  }
  fwrite (dataFrame, "filepath/Data/Smith-Waterman.csv", row.names=TRUE) 
  
  