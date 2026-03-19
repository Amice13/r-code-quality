# Libraries
install.packages(c("plyr","manifestoR","quanteda"))
library(plyr)
library(manifestoR)
library(quanteda)

# Set up manifestoR. Type in the text file for your API key in the mp_setapikey function.
mp_load_cache(file="cache.RData")
mp_setapikey("manifesto_apikey.txt")

# Set up quanteda
liwcdict <- dictionary(file = "mfd.dic", format = "LIWC")

# Create empty object for data collection
data_collection <- NULL

# Start the for loop for each country:

for(c  in c("Australia", "Canada", "Ireland", "New Zealand", "United Kingdom", "United States")){

# Download the entire corpus
corp <- mp_corpus(countryname == c)

#####################
##### Reduce to annotated manifestos
#####################

corp_cleaned <- corp

# Delete quasi-sentences not part of the manifesto.
for(i in 1:length(corp_cleaned)){
  if(any(content(corp_cleaned[[i]]) %in% "<No title information>")){
    corp_cleaned[[i]]$content <- corp_cleaned[[i]]$content[-c(1:which(content(corp_cleaned[[i]]) %in% "<No title information>")),]		
  }
}

# Identify manifestos not annotated at all
take_out <- NULL
for(i in 1:length(corp_cleaned)){
  take_out <- c(take_out, corp_cleaned[[i]]$meta$annotations) # If FALSE, not annotated
}

# If all manifestos are annotated, the below chunk of code is skipped. If at least one manifesto is not annotated, the below code chunk takes out the unannotated manifesto.
if(length(which(!take_out))!=0){
  corp_cleaned <- corp_cleaned[-which(!take_out)]
}

#####################
##### Reduce to English manifestos
#####################

# Check if all manifestos are in English.
english <- NULL
for(i in 1:length(corp_cleaned)){
  english <- c(english, corp_cleaned[[i]]$meta$language=="english")
}

# For Canada, check that the manifestos that aren't marked as English are really not English.
if(c=="Canada"){
english[3] <- TRUE
}

# If all manifestos are in English, the below chunk of code is skipped. If at least one manifesto is not english, the below code chunk takes out the non-English manifesto.
if(length(which(!english))!=0){
  corp_cleaned <- corp_cleaned[-which(!english)]
}

#####################
##### Reduce to quasi-sentences
#####################

# Reduce each manifesto to quasi-sentences 
for(i in 1:length(corp_cleaned)){
  corp_cleaned[[i]]$content <- subset(corp_cleaned[[i]]$content, !is.na(cmp_code))	
}

#####################
##### Run dictionary
#####################

# Run the dictionary on each manifesto, with each document being a quasi-sentence. For loop the number of times that is equal to the number of manifestos. Calculate proportion of quasi-sentences that have a non-zero value
prop_moral_qs <- NULL
for(i in 1:length(corp_cleaned)){
  texts_split <- corpus(corp_cleaned[[i]]$content$text)
  texts_split <- tokens(texts_split, remove_punct=T, remove_symbols=T, remove_url=T)
  temp <- dfm(texts_split, verbose = FALSE, tolower=FALSE)
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
  
  prop_moral_qs <- c(prop_moral_qs, sum(counts!=0)/length(counts))
}

# Combine the proportions with corresponding party
temp <- data.frame(party_election=names(corp_cleaned), prop_moral_qs = prop_moral_qs)

# Create a variable for party number, excluding election date
temp$party_number <- gsub(pattern="_.*$", replacement="", x=temp$party_election)

# Create a variable for election date, excluding party number
temp$election_date <- gsub(".*_","",x=temp$party_election)

# Add country name variable
temp <- data.frame(country_name=c,temp)

# Append
data_collection <- rbind(data_collection, temp)

}

# Reorder in increasing order of country_name, election_date, and party_number.
data_collection <- arrange(data_collection, country_name, election_date, party_number)

################
##### Save the data
##################

write.csv(data_collection, file="Data_mr.csv",row.names=F)
