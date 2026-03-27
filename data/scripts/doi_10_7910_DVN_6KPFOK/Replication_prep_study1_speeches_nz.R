# Libraries
install.packages(c("plyr","readtext"))
library(readtext)
library(plyr)

# To install and load 0.99.12 version of quanteda:
if (!require("pacman")) install.packages("pacman") 
pacman::p_load(data.table, SnowballC, wordcloud, Rcpp, RcppParallel, RSpectra, stringi, fastmatch, ggplot2, XML, yaml, lubridate, magrittr, plyr, readtext, devtools, RcppArmadillo) 
install.packages("https://cran.rstudio.com//src/contrib/Archive/quanteda/quanteda_0.99.12.tar.gz", repos=NULL, type="source")
library(quanteda)

# Read in the two files.
total <- vector("list",2)
total[[1]] <- readtext("National_2011.txt")
total[[2]] <- readtext("NZF_2011.txt")

#################################
# Run the mfd_left dictinoary and calculate proportion of moral sentences
#################################

# Run the dictionary on each speech. For loop the number of times that is equal to the number of speeches.
liwcdict <- dictionary(file = "mfd_left_wo_general.dic", format = "LIWC")
prop_mfd_left_sents <- NULL
for(i in 1:length(total)){
  txt1 <- corpus(total[[i]])
  txt1 <- corpus_reshape(txt1, to="sentences")
  
  # Run dictionary to identify whether each sentence has at least one word from the dictionary
  texts <- txt1$documents[,"texts"]
  texts_split <- removeFeatures(tokens(texts, what="word", remove_punct=T, remove_symbols=T, remove_separators=T, remove_numbers=T, remove_twitter=T, remove_url=T, remove_hyphens=F), stopwords("english"))
  temp <- dfm(texts_split, tolower=F)
  temp_dfm <- dfm_lookup(temp, liwcdict, case_insensitive=FALSE)
  counts <- ntoken(temp_dfm)
  
  for(j in 1:length(texts_split)){
    
    if(any(texts_split[[j]]%in%"care")){
      prev_word <- texts_split[[j]][which(texts_split[[j]] %in% "care")-1]
      if(length(prev_word)!=0){
        for(k in 1:length(prev_word)){
          if(prev_word[k] == "child" | prev_word[k] == "health" | prev_word[k] == "dental"  | prev_word[k] == "cancer"  | prev_word[k] == "day" | prev_word[k] == "respite" | prev_word[k] == "primary"){counts[j] <- counts[j]-1}		
        }			
      }
    }	
    
    if(any(texts_split[[j]]%in%"right")){	
      post_word <- texts_split[[j]][which(texts_split[[j]] %in% "right")+1]
      if(length(post_word)!=0){	
        for(k in 1:length(post_word)){
          if(!is.na(post_word[k]) & post_word[k] == "across" | !is.na(post_word[k]) & post_word[k] == "now"){	counts[j] <- counts[j]-1}
        }
      }
    }
    
    if(any(texts_split[[j]]%in%"equity")){	
      post_word <- texts_split[[j]][which(texts_split[[j]] %in% "equity")+1]
      if(length(post_word)!=0){	
        for(k in 1:length(post_word)){
          if(!is.na(post_word[k]) & post_word[k] == "capital"){counts[j] <- counts[j]-1}
        }
      }
    }
    
    if(any(texts_split[[j]]%in%"balance")){	
      prev_word <- texts_split[[j]][which(texts_split[[j]] %in% "balance")-1]
      if(length(prev_word)!=0){
        for(k in 1:length(prev_word)){	
          if(prev_word[k] == "government"){counts[j] <- counts[j]-1}
        }
      }
    }
    
    if(any(texts_split[[j]]%in%"foreign")){	
      post_word <- texts_split[[j]][which(texts_split[[j]] %in% "foreign")+1]
      if(length(post_word)!=0){
        for(k in 1:length(post_word)){	
          if(!is.na(post_word[k]) & post_word[k] == "reserve" | !is.na(post_word[k]) & post_word[k] == "reserves" | !is.na(post_word[k]) & post_word[k] == "investment" | !is.na(post_word[k]) & post_word[k] == "investments" | !is.na(post_word[k]) & post_word[k] == "policy" | !is.na(post_word[k]) & post_word[k] == "policies" | !is.na(post_word[k]) & post_word[k] == "affair" | !is.na(post_word[k]) & post_word[k] == "affairs" | !is.na(post_word[k]) & post_word[k] == "language" | !is.na(post_word[k]) & post_word[k] == "languages" | !is.na(post_word[k]) & post_word[k] == "aid" | !is.na(post_word[k]) & post_word[k] == "assistance" | !is.na(post_word[k]) & post_word[k] == "direct" | !is.na(post_word[k]) & post_word[k] == "earnings"){counts[j] <- counts[j]-1}
        }
      }
    }
    
    if(any(texts_split[[j]]%in%"foreign")){	
      post_word1 <- texts_split[[j]][which(texts_split[[j]] %in% "foreign")+1]
      post_word3 <- texts_split[[j]][which(texts_split[[j]] %in% "foreign")+3]
      if(length(post_word1)!=0){
        for(k in 1:length(post_word1)){	
          if(!is.na(post_word1[k]) & post_word1[k] == "and" & !is.na(post_word3[k]) & post_word3[k] == "policy"){counts[j] <- counts[j]-1}
        }
      }
    }
    
    if(any(texts_split[[j]]%in%"order")){	
      prev_word <- texts_split[[j]][which(texts_split[[j]] %in% "order")-1]
      post_word <- texts_split[[j]][which(texts_split[[j]] %in% "order")+1]
      if(length(prev_word)!=0){
        for(k in 1:length(prev_word)){		
          if(prev_word[k] == "in" & !is.na(post_word[k]) & post_word[k]=="to"){counts[j] <- counts[j]-1}		}
      }
    }
    
    if(any(texts_split[[j]]%in%"authority")){	
      prev_word <- texts_split[[j]][which(texts_split[[j]] %in% "authority")-1]
      if(length(prev_word)!=0){
        for(k in 1:length(prev_word)){	
          if(prev_word[k] == "local"){counts[j] <- counts[j]-1}	
        }
      }
    }
    
    if(any(texts_split[[j]]%in%"authorities")){	
      prev_word <- texts_split[[j]][which(texts_split[[j]] %in% "authorities")-1]
      if(length(prev_word)!=0){
        for(k in 1:length(prev_word)){	
          if(prev_word[k] == "local"){counts[j] <- counts[j]-1}	
        }
      }
    }
    
    if(any(texts_split[[j]]%in%"refuse")){	
      prev_word <- texts_split[[j]][which(texts_split[[j]] %in% "refuse")-1]
      if(length(prev_word)!=0){
        for(k in 1:length(prev_word)){	
          if(prev_word[k] == "domestic"){counts[j] <- counts[j]-1}	
        }
      }
    }
    
    if(any(texts_split[[j]]%in%"refuse")){	
      post_word <- texts_split[[j]][which(texts_split[[j]] %in% "refuse")+1]
      if(length(post_word)!=0){
        for(k in 1:length(post_word)){	
          if(!is.na(post_word[k]) & post_word[k] == "service"|!is.na(post_word[k]) & post_word[k] == "services"|!is.na(post_word[k]) & post_word[k] == "charge"|!is.na(post_word[k]) & post_word[k] == "charges"){counts[j] <- counts[j]-1}	
        }
      }
    }
    
    if(any(texts_split[[j]]%in%"gross")){	
      post_word <- texts_split[[j]][which(texts_split[[j]] %in% "gross")+1]
      if(length(post_word)!=0){
        for(k in 1:length(post_word)){	
          if(!is.na(post_word[k]) & post_word[k]== "current" | !is.na(post_word[k]) & post_word[k]=="spending"){counts[j] <- counts[j]-1}
        }
      }
    }
    
    if(any(texts_split[[j]]%in%"gross")){	
      post_word <- texts_split[[j]][which(texts_split[[j]] %in% "gross")+1]
      if(length(post_word)!=0){
        for(k in 1:length(post_word)){	
          if(!is.na(post_word[k]) & post_word[k]== "income" | !is.na(post_word[k]) & post_word[k]=="pay" | !is.na(post_word[k]) & post_word[k]=="national"){counts[j] <- counts[j]-1}
        }
      }
    }
    
    if(any(texts_split[[j]]%in%"security")){	
      prev_word <- texts_split[[j]][which(texts_split[[j]] %in% "security")-1]
      if(length(prev_word)!=0){
        for(k in 1:length(prev_word)){	
          if(prev_word[k] == "social"){counts[j] <- counts[j]-1}	
        }
      }
    }
    
    if(any(texts_split[[j]]%in%"community")){	
      post_word <- texts_split[[j]][which(texts_split[[j]] %in% "community")+1]
      if(length(post_word)!=0){
        for(k in 1:length(post_word)){	
          if(!is.na(post_word[k]) & post_word[k] == "college" | !is.na(post_word[k]) & post_word[k] == "colleges"){counts[j] <- counts[j]-1}
        }
      }
    }
    
    if(any(texts_split[[j]]%in%"value")){	
      prev_word <- texts_split[[j]][which(texts_split[[j]] %in% "value")-1]
      if(length(prev_word)!=0){
        for(k in 1:length(prev_word)){	
          if(prev_word[k] == "rental"|prev_word[k] == "maximum"|prev_word[k] == "better"){counts[j] <- counts[j]-1}	
        }
      }
    }
    
    if(any(texts_split[[j]]%in%"family")){	
      post_word <- texts_split[[j]][which(texts_split[[j]] %in% "family")+1]
      if(length(post_word)!=0){
        for(k in 1:length(post_word)){	
          if(!is.na(post_word[k]) & post_word[k] == "policy" | !is.na(post_word[k]) & post_word[k] == "policies"){counts[j] <- counts[j]-1}
        }
      }
    }
    
    if(any(texts_split[[j]]%in%"safety")){	
      prev_word <- texts_split[[j]][which(texts_split[[j]] %in% "safety")-1]
      if(length(prev_word)!=0){
        for(k in 1:length(prev_word)){	
          if(prev_word[k] == "food"|prev_word[k] == "rail"|prev_word[k] == "road"){counts[j] <- counts[j]-1}	
        }
      }
    }
    
  }
  
  txt1$documents$counts <- counts
  
  prop_mfd_left_sents <- c(prop_mfd_left_sents, sum(txt1$documents$counts!=0)/length(txt1$documents$counts))
}

dat <- data.frame(party=c("Nat","NZF"), prop_mfd_left_sents = prop_mfd_left_sents)

#################################
# Run the mfd_right dictinoary and calculate proportion of moral sentences
#################################

# Run the dictionary on each speech. For loop the number of times that is equal to the number of speeches.
liwcdict <- dictionary(file = "mfd_right_wo_general.dic", format = "LIWC")
prop_mfd_right_sents <- NULL
for(i in 1:length(total)){
  txt1 <- corpus(total[[i]])
  txt1 <- corpus_reshape(txt1, to="sentences")
  
  # Run dictionary to identify whether each sentence has at least one word from the dictionary
  texts <- txt1$documents[,"texts"]
  texts_split <- removeFeatures(tokens(texts, what="word", remove_punct=T, remove_symbols=T, remove_separators=T, remove_numbers=T, remove_twitter=T, remove_url=T, remove_hyphens=F), stopwords("english"))
  temp <- dfm(texts_split, tolower=F)
  temp_dfm <- dfm_lookup(temp, liwcdict, case_insensitive=FALSE)
  counts <- ntoken(temp_dfm)
  
  for(j in 1:length(texts_split)){
    
    if(any(texts_split[[j]]%in%"care")){
      prev_word <- texts_split[[j]][which(texts_split[[j]] %in% "care")-1]
      if(length(prev_word)!=0){
        for(k in 1:length(prev_word)){
          if(prev_word[k] == "child" | prev_word[k] == "health" | prev_word[k] == "dental"  | prev_word[k] == "cancer"  | prev_word[k] == "day" | prev_word[k] == "respite" | prev_word[k] == "primary"){counts[j] <- counts[j]-1}		
        }			
      }
    }	

    if(any(texts_split[[j]]%in%"right")){	
      post_word <- texts_split[[j]][which(texts_split[[j]] %in% "right")+1]
      if(length(post_word)!=0){	
        for(k in 1:length(post_word)){
          if(!is.na(post_word[k]) & post_word[k] == "across" | !is.na(post_word[k]) & post_word[k] == "now"){	counts[j] <- counts[j]-1}
        }
      }
    }
    
    if(any(texts_split[[j]]%in%"equity")){	
      post_word <- texts_split[[j]][which(texts_split[[j]] %in% "equity")+1]
      if(length(post_word)!=0){	
        for(k in 1:length(post_word)){
          if(!is.na(post_word[k]) & post_word[k] == "capital"){counts[j] <- counts[j]-1}
        }
      }
    }
    
    if(any(texts_split[[j]]%in%"balance")){	
      prev_word <- texts_split[[j]][which(texts_split[[j]] %in% "balance")-1]
      if(length(prev_word)!=0){
        for(k in 1:length(prev_word)){	
          if(prev_word[k] == "government"){counts[j] <- counts[j]-1}
        }
      }
    }
    
    if(any(texts_split[[j]]%in%"foreign")){	
      post_word <- texts_split[[j]][which(texts_split[[j]] %in% "foreign")+1]
      if(length(post_word)!=0){
        for(k in 1:length(post_word)){	
          if(!is.na(post_word[k]) & post_word[k] == "reserve" | !is.na(post_word[k]) & post_word[k] == "reserves" | !is.na(post_word[k]) & post_word[k] == "investment" | !is.na(post_word[k]) & post_word[k] == "investments" | !is.na(post_word[k]) & post_word[k] == "policy" | !is.na(post_word[k]) & post_word[k] == "policies" | !is.na(post_word[k]) & post_word[k] == "affair" | !is.na(post_word[k]) & post_word[k] == "affairs" | !is.na(post_word[k]) & post_word[k] == "language" | !is.na(post_word[k]) & post_word[k] == "languages" | !is.na(post_word[k]) & post_word[k] == "aid" | !is.na(post_word[k]) & post_word[k] == "assistance" | !is.na(post_word[k]) & post_word[k] == "direct" | !is.na(post_word[k]) & post_word[k] == "earnings"){counts[j] <- counts[j]-1}
        }
      }
    }
    
    if(any(texts_split[[j]]%in%"foreign")){	
      post_word1 <- texts_split[[j]][which(texts_split[[j]] %in% "foreign")+1]
      post_word3 <- texts_split[[j]][which(texts_split[[j]] %in% "foreign")+3]
      if(length(post_word1)!=0){
        for(k in 1:length(post_word1)){	
          if(!is.na(post_word1[k]) & post_word1[k] == "and" & !is.na(post_word3[k]) & post_word3[k] == "policy"){counts[j] <- counts[j]-1}
        }
      }
    }
    
    if(any(texts_split[[j]]%in%"order")){	
      prev_word <- texts_split[[j]][which(texts_split[[j]] %in% "order")-1]
      post_word <- texts_split[[j]][which(texts_split[[j]] %in% "order")+1]
      if(length(prev_word)!=0){
        for(k in 1:length(prev_word)){		
          if(prev_word[k] == "in" & !is.na(post_word[k]) & post_word[k]=="to"){counts[j] <- counts[j]-1}		}
      }
    }
    
    if(any(texts_split[[j]]%in%"authority")){	
      prev_word <- texts_split[[j]][which(texts_split[[j]] %in% "authority")-1]
      if(length(prev_word)!=0){
        for(k in 1:length(prev_word)){	
          if(prev_word[k] == "local"){counts[j] <- counts[j]-1}	
        }
      }
    }
    
    if(any(texts_split[[j]]%in%"authorities")){	
      prev_word <- texts_split[[j]][which(texts_split[[j]] %in% "authorities")-1]
      if(length(prev_word)!=0){
        for(k in 1:length(prev_word)){	
          if(prev_word[k] == "local"){counts[j] <- counts[j]-1}	
        }
      }
    }
    
    if(any(texts_split[[j]]%in%"refuse")){	
      prev_word <- texts_split[[j]][which(texts_split[[j]] %in% "refuse")-1]
      if(length(prev_word)!=0){
        for(k in 1:length(prev_word)){	
          if(prev_word[k] == "domestic"){counts[j] <- counts[j]-1}	
        }
      }
    }
    
    if(any(texts_split[[j]]%in%"refuse")){	
      post_word <- texts_split[[j]][which(texts_split[[j]] %in% "refuse")+1]
      if(length(post_word)!=0){
        for(k in 1:length(post_word)){	
          if(!is.na(post_word[k]) & post_word[k] == "service"|!is.na(post_word[k]) & post_word[k] == "services"|!is.na(post_word[k]) & post_word[k] == "charge"|!is.na(post_word[k]) & post_word[k] == "charges"){counts[j] <- counts[j]-1}	
        }
      }
    }
    
    if(any(texts_split[[j]]%in%"gross")){	
      post_word <- texts_split[[j]][which(texts_split[[j]] %in% "gross")+1]
      if(length(post_word)!=0){
        for(k in 1:length(post_word)){	
          if(!is.na(post_word[k]) & post_word[k]== "current" | !is.na(post_word[k]) & post_word[k]=="spending"){counts[j] <- counts[j]-1}
        }
      }
    }
    
    if(any(texts_split[[j]]%in%"gross")){	
      post_word <- texts_split[[j]][which(texts_split[[j]] %in% "gross")+1]
      if(length(post_word)!=0){
        for(k in 1:length(post_word)){	
          if(!is.na(post_word[k]) & post_word[k]== "income" | !is.na(post_word[k]) & post_word[k]=="pay" | !is.na(post_word[k]) & post_word[k]=="national"){counts[j] <- counts[j]-1}
        }
      }
    }
    
    if(any(texts_split[[j]]%in%"security")){	
      prev_word <- texts_split[[j]][which(texts_split[[j]] %in% "security")-1]
      if(length(prev_word)!=0){
        for(k in 1:length(prev_word)){	
          if(prev_word[k] == "social"){counts[j] <- counts[j]-1}	
        }
      }
    }
    
    if(any(texts_split[[j]]%in%"community")){	
      post_word <- texts_split[[j]][which(texts_split[[j]] %in% "community")+1]
      if(length(post_word)!=0){
        for(k in 1:length(post_word)){	
          if(!is.na(post_word[k]) & post_word[k] == "college" | !is.na(post_word[k]) & post_word[k] == "colleges"){counts[j] <- counts[j]-1}
        }
      }
    }
    
    if(any(texts_split[[j]]%in%"value")){	
      prev_word <- texts_split[[j]][which(texts_split[[j]] %in% "value")-1]
      if(length(prev_word)!=0){
        for(k in 1:length(prev_word)){	
          if(prev_word[k] == "rental"|prev_word[k] == "maximum"|prev_word[k] == "better"){counts[j] <- counts[j]-1}	
        }
      }
    }
    
    if(any(texts_split[[j]]%in%"family")){	
      post_word <- texts_split[[j]][which(texts_split[[j]] %in% "family")+1]
      if(length(post_word)!=0){
        for(k in 1:length(post_word)){	
          if(!is.na(post_word[k]) & post_word[k] == "policy" | !is.na(post_word[k]) & post_word[k] == "policies"){counts[j] <- counts[j]-1}
        }
      }
    }
    
    if(any(texts_split[[j]]%in%"safety")){	
      prev_word <- texts_split[[j]][which(texts_split[[j]] %in% "safety")-1]
      if(length(prev_word)!=0){
        for(k in 1:length(prev_word)){	
          if(prev_word[k] == "food"|prev_word[k] == "rail"|prev_word[k] == "road"){counts[j] <- counts[j]-1}	
        }
      }
    }
    
  }
  
  txt1$documents$counts <- counts
  
  prop_mfd_right_sents <- c(prop_mfd_right_sents, sum(txt1$documents$counts!=0)/length(txt1$documents$counts))
}

dat <- cbind(dat, prop_mfd_right_sents = prop_mfd_right_sents)

#################################
# Run the mfd dictionary and calculate proportion of moral sentences
#################################

liwcdict <- dictionary(file = "mfd.dic", format = "LIWC")

# Run the dictionary on each speech. For loop the number of times that is equal to the number of speeches.
prop_mfd_sents <- NULL
for(i in 1:length(total)){
  txt1 <- corpus(total[[i]])
  txt1 <- corpus_reshape(txt1, to="sentences")
  
  # Run dictionary to identify whether each sentence has at least one word from mfd
  texts <- txt1$documents[,"texts"]
  texts_split <- removeFeatures(tokens(texts, what="word", remove_punct=T, remove_symbols=T, remove_separators=T, remove_numbers=T, remove_twitter=T, remove_url=T, remove_hyphens=F), stopwords("english"))
  temp <- dfm(texts_split, tolower=F)
  temp_dfm <- dfm_lookup(temp, liwcdict, case_insensitive=FALSE)
  counts <- ntoken(temp_dfm)
  
  for(j in 1:length(texts_split)){
    
    if(any(texts_split[[j]]%in%"care")){
      prev_word <- texts_split[[j]][which(texts_split[[j]] %in% "care")-1]
      if(length(prev_word)!=0){
        for(k in 1:length(prev_word)){
          if(prev_word[k] == "child" | prev_word[k] == "health" | prev_word[k] == "dental"  | prev_word[k] == "cancer"  | prev_word[k] == "day" | prev_word[k] == "respite" | prev_word[k] == "primary"){counts[j] <- counts[j]-1}		
        }			
      }
    }	
    
    if(any(texts_split[[j]]%in%"right")){	
      post_word <- texts_split[[j]][which(texts_split[[j]] %in% "right")+1]
      if(length(post_word)!=0){	
        for(k in 1:length(post_word)){
          if(!is.na(post_word[k]) & post_word[k] == "across" | !is.na(post_word[k]) & post_word[k] == "now"){	counts[j] <- counts[j]-1}
        }
      }
    }
    
    if(any(texts_split[[j]]%in%"equity")){	
      post_word <- texts_split[[j]][which(texts_split[[j]] %in% "equity")+1]
      if(length(post_word)!=0){	
        for(k in 1:length(post_word)){
          if(!is.na(post_word[k]) & post_word[k] == "capital"){counts[j] <- counts[j]-1}
        }
      }
    }
    
    if(any(texts_split[[j]]%in%"balance")){	
      prev_word <- texts_split[[j]][which(texts_split[[j]] %in% "balance")-1]
      if(length(prev_word)!=0){
        for(k in 1:length(prev_word)){	
          if(prev_word[k] == "government"){counts[j] <- counts[j]-1}
        }
      }
    }
    
    if(any(texts_split[[j]]%in%"foreign")){	
      post_word <- texts_split[[j]][which(texts_split[[j]] %in% "foreign")+1]
      if(length(post_word)!=0){
        for(k in 1:length(post_word)){	
          if(!is.na(post_word[k]) & post_word[k] == "reserve" | !is.na(post_word[k]) & post_word[k] == "reserves" | !is.na(post_word[k]) & post_word[k] == "investment" | !is.na(post_word[k]) & post_word[k] == "investments" | !is.na(post_word[k]) & post_word[k] == "policy" | !is.na(post_word[k]) & post_word[k] == "policies" | !is.na(post_word[k]) & post_word[k] == "affair" | !is.na(post_word[k]) & post_word[k] == "affairs" | !is.na(post_word[k]) & post_word[k] == "language" | !is.na(post_word[k]) & post_word[k] == "languages" | !is.na(post_word[k]) & post_word[k] == "aid" | !is.na(post_word[k]) & post_word[k] == "assistance" | !is.na(post_word[k]) & post_word[k] == "direct" | !is.na(post_word[k]) & post_word[k] == "earnings"){counts[j] <- counts[j]-1}
        }
      }
    }
    
    if(any(texts_split[[j]]%in%"foreign")){	
      post_word1 <- texts_split[[j]][which(texts_split[[j]] %in% "foreign")+1]
      post_word3 <- texts_split[[j]][which(texts_split[[j]] %in% "foreign")+3]
      if(length(post_word1)!=0){
        for(k in 1:length(post_word1)){	
          if(!is.na(post_word1[k]) & post_word1[k] == "and" & !is.na(post_word3[k]) & post_word3[k] == "policy"){counts[j] <- counts[j]-1}
        }
      }
    }
    
    if(any(texts_split[[j]]%in%"order")){	
      prev_word <- texts_split[[j]][which(texts_split[[j]] %in% "order")-1]
      post_word <- texts_split[[j]][which(texts_split[[j]] %in% "order")+1]
      if(length(prev_word)!=0){
        for(k in 1:length(prev_word)){		
          if(prev_word[k] == "in" & !is.na(post_word[k]) & post_word[k]=="to"){counts[j] <- counts[j]-1}		}
      }
    }
    
    if(any(texts_split[[j]]%in%"authority")){	
      prev_word <- texts_split[[j]][which(texts_split[[j]] %in% "authority")-1]
      if(length(prev_word)!=0){
        for(k in 1:length(prev_word)){	
          if(prev_word[k] == "local"){counts[j] <- counts[j]-1}	
        }
      }
    }
    
    if(any(texts_split[[j]]%in%"authorities")){	
      prev_word <- texts_split[[j]][which(texts_split[[j]] %in% "authorities")-1]
      if(length(prev_word)!=0){
        for(k in 1:length(prev_word)){	
          if(prev_word[k] == "local"){counts[j] <- counts[j]-1}	
        }
      }
    }
    
    if(any(texts_split[[j]]%in%"refuse")){	
      prev_word <- texts_split[[j]][which(texts_split[[j]] %in% "refuse")-1]
      if(length(prev_word)!=0){
        for(k in 1:length(prev_word)){	
          if(prev_word[k] == "domestic"){counts[j] <- counts[j]-1}	
        }
      }
    }
    
    if(any(texts_split[[j]]%in%"refuse")){	
      post_word <- texts_split[[j]][which(texts_split[[j]] %in% "refuse")+1]
      if(length(post_word)!=0){
        for(k in 1:length(post_word)){	
          if(!is.na(post_word[k]) & post_word[k] == "service"|!is.na(post_word[k]) & post_word[k] == "services"|!is.na(post_word[k]) & post_word[k] == "charge"|!is.na(post_word[k]) & post_word[k] == "charges"){counts[j] <- counts[j]-1}	
        }
      }
    }
    
    if(any(texts_split[[j]]%in%"gross")){	
      post_word <- texts_split[[j]][which(texts_split[[j]] %in% "gross")+1]
      if(length(post_word)!=0){
        for(k in 1:length(post_word)){	
          if(!is.na(post_word[k]) & post_word[k]== "current" | !is.na(post_word[k]) & post_word[k]=="spending"){counts[j] <- counts[j]-1}
        }
      }
    }
    
    if(any(texts_split[[j]]%in%"gross")){	
      post_word <- texts_split[[j]][which(texts_split[[j]] %in% "gross")+1]
      if(length(post_word)!=0){
        for(k in 1:length(post_word)){	
          if(!is.na(post_word[k]) & post_word[k]== "income" | !is.na(post_word[k]) & post_word[k]=="pay" | !is.na(post_word[k]) & post_word[k]=="national"){counts[j] <- counts[j]-1}
        }
      }
    }
    
    if(any(texts_split[[j]]%in%"security")){	
      prev_word <- texts_split[[j]][which(texts_split[[j]] %in% "security")-1]
      if(length(prev_word)!=0){
        for(k in 1:length(prev_word)){	
          if(prev_word[k] == "social"){counts[j] <- counts[j]-1}	
        }
      }
    }
    
    if(any(texts_split[[j]]%in%"community")){	
      post_word <- texts_split[[j]][which(texts_split[[j]] %in% "community")+1]
      if(length(post_word)!=0){
        for(k in 1:length(post_word)){	
          if(!is.na(post_word[k]) & post_word[k] == "college" | !is.na(post_word[k]) & post_word[k] == "colleges"){counts[j] <- counts[j]-1}
        }
      }
    }
    
    if(any(texts_split[[j]]%in%"value")){	
      prev_word <- texts_split[[j]][which(texts_split[[j]] %in% "value")-1]
      if(length(prev_word)!=0){
        for(k in 1:length(prev_word)){	
          if(prev_word[k] == "rental"|prev_word[k] == "maximum"|prev_word[k] == "better"){counts[j] <- counts[j]-1}	
        }
      }
    }
    
    if(any(texts_split[[j]]%in%"family")){	
      post_word <- texts_split[[j]][which(texts_split[[j]] %in% "family")+1]
      if(length(post_word)!=0){
        for(k in 1:length(post_word)){	
          if(!is.na(post_word[k]) & post_word[k] == "policy" | !is.na(post_word[k]) & post_word[k] == "policies"){counts[j] <- counts[j]-1}
        }
      }
    }
    
    if(any(texts_split[[j]]%in%"safety")){	
      prev_word <- texts_split[[j]][which(texts_split[[j]] %in% "safety")-1]
      if(length(prev_word)!=0){
        for(k in 1:length(prev_word)){	
          if(prev_word[k] == "food"|prev_word[k] == "rail"|prev_word[k] == "road"){counts[j] <- counts[j]-1}	
        }
      }
    }
    
  }
  
  txt1$documents$counts <- counts
  
  prop_mfd_sents <- c(prop_mfd_sents, sum(txt1$documents$counts!=0)/length(txt1$documents$counts))
}

dat <- cbind(dat, prop_mfd_sents = prop_mfd_sents)

#################################
# Add party info
#################################

dat <- cbind(dat, party_name=c("Nat","NZF"))
dat$election_party <- paste(2011, dat$party_name, sep="_")
dat <- arrange(dat, election_party)

#################################
# Read in and add information in Data_mr_ideo.csv.
#################################

mr_ideo <- read.csv("Data_mr_ideo.csv")
mr_ideo <- subset(mr_ideo, country_name=="New Zealand")
# Nat is 64620. NZF is 64621.
mr_ideo <- subset(mr_ideo, party_number==64620 | party_number==64621)
mr_ideo <- subset(mr_ideo, election_date=="201111")
mr_ideo$party <- mapvalues(mr_ideo$party_number, c(64620, 64621),c("Nat","NZF"))
mr_ideo$election_party <- paste(2011, mr_ideo$party, sep="_")
mr_ideo <- arrange(mr_ideo, election_party)

temp <- cbind(mr_ideo, dat)

#################################
# Read in and add information in Data_mr.csv.
#################################

mr <- read.csv("Data_mr.csv")
mr <- subset(mr, country_name=="New Zealand")
# Nat is 64620. NZF is 64621.
mr <- subset(mr, party_number==64620 | party_number==64621)
mr <- subset(mr, election_date=="201111")
mr$party <- mapvalues(mr$party_number, c(64620, 64621),c("Nat","NZF"))
mr$election_party <- paste(2011, mr$party, sep="_")
mr <- arrange(mr, election_party)

temp <- cbind(temp,prop_moral_qs=mr[,c("prop_moral_qs")])

#################################
# Save the data
#################################

write.csv(temp[,c("election_date","party","prop_moral_qs","prop_moral_qs_left","prop_moral_qs_right","prop_mfd_sents","prop_mfd_left_sents","prop_mfd_right_sents")], file="NZ_mr_in_manifesto&speech.csv",row.names = F)
