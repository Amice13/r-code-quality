# Libraries
install.packages(c("plyr","manifestoR","quanteda","rms","lme4","texreg","R2jags","arm"))
library(plyr)
library(manifestoR)
library(quanteda)
library(rms)
library(lme4)
library(texreg)
library(R2jags)
library(arm)

##########################
###### SI1: Intercoder Reliability
##########################

# Read in codings for words I dropped. If both left and right coders give 1 to half or less of the quasi-sentences, then the choice to drop the word is valid.

benefit <- read.csv("QS_benefit.csv",stringsAsFactors=F)
colnames(benefit)
nrow(benefit)
length(which(benefit$Care_left==1))
length(which(benefit$Care_right==1))

class <- read.csv("QS_class.csv",stringsAsFactors=F)
colnames(class)
nrow(class)
length(which(class$Authority_left==1))
length(which(class$Authority_right==1))

group <- read.csv("QS_group.csv",stringsAsFactors=F)
colnames(group)
nrow(group)
length(which(group$Loyalty_left==1))
length(which(group$Loyalty_right==1))

immigra <- read.csv("QS_immigra.csv",stringsAsFactors=F)
colnames(immigra)
nrow(immigra)
length(which(immigra$Betrayal_left==1))
length(which(immigra$Betrayal_right==1))

individual <- read.csv("QS_individual.csv",stringsAsFactors=F)
colnames(individual)
nrow(individual)
length(which(individual$Betrayal_left==1))
length(which(individual$Betrayal_right==1))

law <- read.csv("QS_law.csv",stringsAsFactors=F)
colnames(law)
nrow(law)
length(which(law$Authority_left==1))
length(which(law$Authority_right==1))

legal <- read.csv("QS_legal.csv",stringsAsFactors=F)
colnames(legal)
nrow(legal)
length(which(legal$Authority_left==1))
length(which(legal$Authority_right==1))

member <- read.csv("QS_member.csv",stringsAsFactors=F)
colnames(member)
nrow(member)
length(which(member$Loyalty_left==1))
length(which(member$Loyalty_right==1))

nation <- read.csv("QS_nation.csv",stringsAsFactors=F)
colnames(nation)
nrow(nation)
length(which(nation$Loyalty_left==1))
length(which(nation$Loyalty_right==1))

oppose <- read.csv("QS_oppose.csv",stringsAsFactors=F)
colnames(oppose)
nrow(oppose)
length(which(oppose$Subversion_left==1))
length(which(oppose$Subversion_right==1))

permission <- read.csv("QS_permission.csv",stringsAsFactors=F)
colnames(permission)
nrow(permission)
length(which(permission$Authority_left==1))
length(which(permission$Authority_right==1))

permit <- read.csv("QS_permit.csv",stringsAsFactors=F)
colnames(permit)
nrow(permit)
length(which(permit$Authority_left==1))
length(which(permit$Authority_right==1))

position <- read.csv("QS_position.csv",stringsAsFactors=F)
colnames(position)
nrow(position)
length(which(position$Authority_left==1))
length(which(position$Authority_right==1))

preference <- read.csv("QS_preference.csv",stringsAsFactors=F)
colnames(preference)
nrow(preference)
length(which(preference$Cheating_left==1))
length(which(preference$Cheating_right==1))

rank <- read.csv("QS_rank.csv",stringsAsFactors=F)
colnames(rank)
nrow(rank)
length(which(rank$Authority_left==1))
length(which(rank$Authority_right==1))

status <- read.csv("QS_status.csv",stringsAsFactors=F)
colnames(status)
nrow(status)
length(which(status$Authority_left==1))
length(which(status$Authority_right==1))

terroris <- read.csv("QS_terroris.csv",stringsAsFactors=F)
colnames(terroris)
nrow(terroris)
length(which(terroris$Betrayal_left==1))
length(which(terroris$Betrayal_right==1))

worth <- read.csv("QS_worth.csv",stringsAsFactors=F)
colnames(worth)
nrow(worth)
length(which(worth$General_left==1))
length(which(worth$General_right==1))

# Read in codings for words I added. If both left and right coders give 1 to more than half of the quasi-sentences, then the choice to add the word is valid.

dignity <- read.csv("QS_dignity.csv",stringsAsFactors=F)
colnames(dignity)
nrow(dignity)
length(which(dignity$Sanctity_left==1))
length(which(dignity$Sanctity_right==1))

sanctity <- read.csv("QS_sanctity.csv",stringsAsFactors=F)
colnames(sanctity)
nrow(sanctity)
length(which(sanctity$Sanctity_left==1))
length(which(sanctity$Sanctity_right==1))

##########################
###### SI2: Randomly chosen section from manifestos
##########################

# Write function to read in the assembled quasi-sentences
readBack <- function(file.name){
  
  # Read translated file
  txt1 <- readLines(file.name)
  # Separate each line by empty space
  results <- strsplit(txt1," ")
  # Take out cmp_code
  cmp <- laply(results, function(x){x[(length(x)-2)]})
  cmp <- cmp[-1]
  # Take out only text part
  text <- llply(results, function(x){x[2:(length(x)-3)]})
  # Combine text part by row
  text <- laply(text, function(x){paste(x,collapse=" ")})
  text <- text[-1]
  # Take out manifesto_id part
  manifesto_id <- laply(results, function(x){x[1]})
  manifesto_id <- manifesto_id[-1]
  # Take out moral part
  moral <- laply(results, function(x){x[length(x)-1]})
  moral <- moral[-1]
  # Take out id part
  id <- laply(results, function(x){x[length(x)]})
  id <- id[-1]  
  # Combine into dataframe format
  txt1 <- data.frame("manifesto_id"=manifesto_id,"text"=text,"cmp_code"=cmp,"moral"=moral, "id"=id,stringsAsFactors=F)
  
  return(txt1)
  
}

# Read in all
qs <- readBack("Data_quasi_sentences.txt")

# Subset to data used in analysis
qs <- qs[-which(qs$manifesto_id %in% c("51951_201505","51903_201505", "51902_201505", "51620_201505", "51421_201505", "51340_201505","51210_201505", "51110_201505" ,"51902_200106", "51901_201505","51621_201505", "51320_201505")),]

# Randomly choose a row and look at section covering that row
set.seed(612)
samp <- sample(qs$id, 1)
qs[which(qs$id %in% samp),]
qs[33015:33030,]

##########################
###### SI3: Validity: Random sample of quasi-sentences
##########################

set.seed(6123228)

# Moral quasi-sentences
qs_moral <- subset(qs, moral==1)
set_moral <- sample(qs_moral$id, 10)
qs_moral[which(qs_moral$id %in% set_moral),]
cbind(qs_moral[which(qs_moral$id %in% set_moral),c("text","manifesto_id")],1:length(set_moral))
qs_moral[which(qs_moral$id %in% set_moral),c("text")]

# Non-moral quasi-sentences
qs_nmoral <- subset(qs, moral==0)
head(qs_nmoral)
set_nmoral <- sample(qs_nmoral$id, 10)
qs_nmoral[which(qs_nmoral$id %in% set_nmoral),]
cbind(qs_nmoral[which(qs_nmoral$id %in% set_nmoral),c("text","manifesto_id")],1:length(set_nmoral))
qs_nmoral[which(qs_nmoral$id %in% set_nmoral),c("text")]

##########################
###### SI3: Validity: Left versus right
##########################

# Read in measures of individualizing and binding moral appeals
ideo_measures <- read.csv("Data_mr_ideo.csv",stringsAsFactors=F)

# Read in supplementary information
supp <- read.csv("Data_supplementary.csv")

# Assign rile to ideo_measures using party_election
ideo_measures$rile <- as.numeric(mapvalues(ideo_measures$party_election, supp$party_election, supp$rile))

# Check mean of rile
mean(ideo_measures$rile) #-0.06016867

# T-test of individualizing vs. binding appeal for left-wing parties.
t.test(subset(ideo_measures, rile<=-0.06017)$prop_moral_qs_left, subset(ideo_measures, rile<=-0.06017)$prop_moral_qs_right, paired=T, alternative="greater")

# T-test of individualizing vs. binding appeal for right-wing parties.
t.test(subset(ideo_measures, rile>-0.06017)$prop_moral_qs_left, subset(ideo_measures, rile>-0.06017)$prop_moral_qs_right, paired=T)

pdf(file="plot_validity_left_bar.pdf")
indiv <- subset(ideo_measures, rile<=-0.06017)$prop_moral_qs_left
bind <- subset(ideo_measures, rile<=-0.06017)$prop_moral_qs_right
m        = c(mean(indiv), mean(bind))
names(m) = c("Individualizing","Binding")
se       = c(sd(indiv)/sqrt(length(indiv)), 
             sd(bind)/sqrt(length(bind)))
bp = barplot(m, ylim=c(0,0.2), xpd=FALSE, ylab="Moral rhetoric (left-wing parties)",cex.names=1.5,cex.axis=1.5,cex.lab=1.5)
arrows(x0=bp, y0=m-se, y1=m+se, code=3, angle=90)
dev.off()

pdf(file="plot_validity_right_bar.pdf")
indiv <- subset(ideo_measures, rile>-0.06017)$prop_moral_qs_left
bind <- subset(ideo_measures, rile>-0.06017)$prop_moral_qs_right
m        = c(mean(indiv), mean(bind))
names(m) = c("Individualizing","Binding")
se       = c(sd(indiv)/sqrt(length(indiv)), 
             sd(bind)/sqrt(length(bind)))
bp = barplot(m, ylim=c(0,0.2), xpd=FALSE, ylab="Moral rhetoric (right-wing parties)",cex.names=1.5,cex.axis=1.5,cex.lab=1.5)
arrows(x0=bp, y0=m-se, y1=m+se, code=3, angle=90)
dev.off()

pdf(file="plot_validity_indiv_bar.pdf")
left <- subset(ideo_measures, rile<=-0.06017)$prop_moral_qs_left
right <- subset(ideo_measures, rile>-0.06017)$prop_moral_qs_left
m        = c(mean(left), mean(right))
names(m) = c("Left-wing","Right-wing")
se       = c(sd(left)/sqrt(length(left)), 
             sd(right)/sqrt(length(right)))
bp = barplot(m, ylim=c(0,0.2), xpd=FALSE, ylab="Moral rhetoric (individualizing foundations)",cex.names=1.5,cex.axis=1.5,,cex.lab=1.5)
arrows(x0=bp, y0=m-se, y1=m+se, code=3, angle=90)
dev.off()

pdf(file="plot_validity_bind_bar.pdf")
left <- subset(ideo_measures, rile<=-0.06017)$prop_moral_qs_right
right <- subset(ideo_measures, rile>-0.06017)$prop_moral_qs_right
m        = c(mean(left), mean(right))
names(m) = c("Left-wing","Right-wing")
se       = c(sd(left)/sqrt(length(left)), 
             sd(right)/sqrt(length(right)))
bp = barplot(m, ylim=c(0,0.2), xpd=FALSE, ylab="Moral rhetoric (binding foundations)",cex.names=1.5,cex.axis=1.5,cex.lab=1.5)
arrows(x0=bp, y0=m-se, y1=m+se, code=3, angle=90)
dev.off()

# T-test of individualizing vs. binding appeal for left-wing parties. Using 0 to separate left and right parties.
t.test(subset(ideo_measures, rile<=0)$prop_moral_qs_left, subset(ideo_measures, rile<=0)$prop_moral_qs_right, paired=T, alternative="greater")

# T-test of individualizing vs. binding appeal for right-wing parties. Using 0 to separate left and right parties.
t.test(subset(ideo_measures, rile>0)$prop_moral_qs_right, subset(ideo_measures, rile>0)$prop_moral_qs_left, paired=T)

# T-test of individualizing appeal for left-wing parties.
mean(subset(ideo_measures, rile<=-0.06017)$prop_moral_qs_left)
mean(subset(ideo_measures, rile>-0.06017)$prop_moral_qs_left)
t.test(subset(ideo_measures, rile<=-0.06017)$prop_moral_qs_left, subset(ideo_measures, rile>-0.06017)$prop_moral_qs_left,alternative="greater")

# T-test of individualizing appeal for right-wing parties.
mean(subset(ideo_measures, rile<=-0.06017)$prop_moral_qs_right)
mean(subset(ideo_measures, rile>-0.06017)$prop_moral_qs_right)
t.test(subset(ideo_measures, rile<=-0.06017)$prop_moral_qs_right, subset(ideo_measures, rile>-0.06017)$prop_moral_qs_right,alternative="less")

##########################
###### SI3: Validity: correlation with sociocultural issue emphasis
##########################

# Categories of sociocultural issues, according to Tavits and Potter 2014, is 601 to 608, 705-706

# Set up manifestoR. Type in the text file for your API key in the mp_setapikey function.
mp_load_cache(file="cache.RData")
mp_setapikey("manifesto_apikey.txt")

# Get column indices for the categories
head(supp,3)
mpd <- mp_maindataset()
index <- which(colnames(as.data.frame(subset(mpd, party==supp$party_number[1] & date==supp$election_date[1]))) %in% paste("per",c(601:608,705,706),sep=""))

# Calculate proportion of sociocultural issue emphasis
socio_pct <- NULL
for(i in 1:length(supp$party_election)){
  socio_pct <- c(socio_pct, sum(as.data.frame(subset(mpd, party==supp$party_number[i] & date==supp$election_date[i]))[,index], na.rm=T))
}
supp$socio_pct <- socio_pct

# Add mr measure to supp
mr <- read.csv("Data_mr.csv")
supp$mr <- as.numeric(as.character(mapvalues(supp$party_election, mr$party_election, mr$prop_moral_qs)))

# Correlation
cor(supp$socio_pct, supp$mr)
cor.test(supp$socio_pct, supp$mr, alternative="greater")

##########################
###### SI3: Validity: sociocultural dimension versus economic dimension
##########################

# Prepare dictionary
liwcdict <- dictionary(file = "mfd.dic", format = "LIWC")

# Create empty object for data collection
data_collection_econ <- NULL
data_collection_socio <- NULL

# Start the for loop for each country:

for(c  in c("Australia", "Canada", "Ireland", "New Zealand", "United Kingdom", "United States")){
  
# Download the entire corpus
corp <- mp_corpus(countryname == c)

# Reduce to annotated manifestos

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

# Reduce each manifesto to quasi-sentences that fall in the economic dimension
corp_cleaned_econ <- corp_cleaned
for(i in 1:length(corp_cleaned_econ)){
  corp_cleaned_econ[[i]]$content <- subset(corp_cleaned_econ[[i]]$content, cmp_code == 401 | cmp_code == 402 | cmp_code == 403 | cmp_code == 404 | cmp_code == 406 | cmp_code == 407 | cmp_code == 409 | cmp_code == 412 | cmp_code == 413 | cmp_code == 503 | cmp_code == 504 | cmp_code == 505 | cmp_code ==701 | cmp_code == 702)	
}

# Check if all manifestos are in English.
english <- NULL
for(i in 1:length(corp_cleaned_econ)){
  english <- c(english, corp_cleaned_econ[[i]]$meta$language=="english")
}

# For Canada, check that the manifestos that aren't marked as English are really not English.
if(c=="Canada"){
english[3] <- TRUE
}

# If all manifestos are in English, the below chunk of code is skipped. If at least one manifesto is not english, the below code chunk takes out the non-English manifesto.
if(length(which(!english))!=0){
  corp_cleaned_econ <- corp_cleaned_econ[-which(!english)]
}

# Run the dictionary on each manifesto, with each document being a quasi-sentence. For loop the number of times that is equal to the number of manifestos. Calculate proportion of quasi-sentences that have a non-zero value
prop_moral_qs <- NULL
for(i in 1:length(corp_cleaned_econ)){
  texts_split <- corpus(corp_cleaned_econ[[i]]$content$text)
  texts_split <- tokens(texts_split)
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
  
  prop_moral_qs <- c(prop_moral_qs, sum(counts!=0)/length(counts))
}

# Combine the proportions with corresponding party
temp <- data.frame(party_election=names(corp_cleaned_econ), prop_moral_qs_econ = prop_moral_qs)

# Create a variable for party number, excluding election date
temp$party_number <- gsub(pattern="_.*$", replacement="", x=temp$party_election)

# Create a variable for election date, excluding party number
temp$election_date <- gsub(".*_","",x=temp$party_election)

# Add country name
temp <- data.frame(country_name=c,temp)

# Append
data_collection_econ <- rbind(data_collection_econ, temp)

# Reduce each manifesto to quasi-sentences that fall in the sociocultural dimension
corp_cleaned_socio <- corp_cleaned
for(i in 1:length(corp_cleaned_socio)){
  corp_cleaned_socio[[i]]$content <- subset(corp_cleaned_socio[[i]]$content, cmp_code == 601 | cmp_code == 602 | cmp_code == 603 | cmp_code == 604 | cmp_code==605 | cmp_code == 606 | cmp_code == 607 | cmp_code == 608 | cmp_code == 705 | cmp_code == 706)	
}

# Check if all manifestos are in English.
english <- NULL
for(i in 1:length(corp_cleaned_socio)){
  english <- c(english, corp_cleaned_socio[[i]]$meta$language=="english")
}

# For Canada, check that the manifestos that aren't marked as English are really not English.
if(c=="Canada"){
 english[3] <- TRUE
}

# If all manifestos are in English, the below chunk of code is skipped. If at least one manifesto is not english, the below code chunk takes out the non-English manifesto.
if(length(which(!english))!=0){
  corp_cleaned_socio <- corp_cleaned_socio[-which(!english)]
}

# Run the dictionary
prop_moral_qs <- NULL
for(i in 1:length(corp_cleaned_socio)){
  
  if(length(unique(corp_cleaned_socio[[i]]$content$cmp_code))!=0){ # To deal with parties with no sociocultural issue talk.
    
    texts_split <- corpus(corp_cleaned_socio[[i]]$content$text)
    texts_split <- tokens(texts_split)
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
    
    prop_moral_qs <- c(prop_moral_qs, sum(counts!=0)/length(counts))
    
  }else{
    prop_moral_qs <- c(prop_moral_qs, NA)
  }
}

# Combine the proportions with corresponding party
temp <- data.frame(party_election=names(corp_cleaned_socio), prop_moral_qs_socio = prop_moral_qs)

# Create a variable for party number, excluding election date
temp$party_number <- gsub(pattern="_.*$", replacement="", x=temp$party_election)

# Create a variable for election date, excluding party number
temp$election_date <- gsub(".*_","",x=temp$party_election)

# Add country name
temp <- data.frame(country_name=c,temp)

# Append
data_collection_socio <- rbind(data_collection_socio, temp)

}

# T-test
econ_socio_measures <- merge(data_collection_econ, data_collection_socio)
t.test(econ_socio_measures$prop_moral_qs_econ, econ_socio_measures$prop_moral_qs_socio)
t.test(econ_socio_measures$prop_moral_qs_econ, econ_socio_measures$prop_moral_qs_socio, paired=T, alternative="less")

##########################
###### SI3: Validity: correlation with ideology
##########################

# Plot mr with rile
pdf(file="plot_mr&rile.pdf")
plot(supp$rile, supp$mr,ylim=c(0,0.6), pch=16, xlab="Party ideology", ylab="Moral rhetoric", cex.lab=1.3) 
dev.off()

# Correlation between mr and rile
cor(supp$rile, supp$mr)
cor.test(supp$rile, supp$mr) 

# T-test for mr and niche
t.test(supp$mr ~ supp$niche)

##########################
###### SI3: Validity: left-wing versus Christinan Democratic parties
##########################

# Mean for CD parties
mean(supp[which(supp$parfam_content=="CHR"),"mr"])

# Mean for left-wing parties
mean(supp[which(supp$rile<=0),"mr"])

##########################
###### SI3: Validity: correlation with campaign speech data
##########################

# Read in the speech measures datasets
au <- read.csv("AU_mr_in_manifesto&speech.csv")
nz <- read.csv("NZ_mr_in_manifesto&speech.csv")

# Combine the datsets
tot <- rbind(au, nz)

# Correlation between mr and speech-based mr
cor.test(tot$prop_moral_qs, tot$prop_mfd_sents)

# Correlation by foundation type
cor.test(tot$prop_moral_qs_left, tot$prop_mfd_left_sents)
cor.test(tot$prop_moral_qs_right, tot$prop_mfd_right_sents)

# Correlation combining both types
cor.test(c(tot$prop_moral_qs_left,tot$prop_moral_qs_right), c(tot$prop_mfd_left_sents,tot$prop_mfd_right_sents))

##########################
###### SI4: Positive versus negative moral rhetoric
##########################

# Read in the moral rhetoric measures using only negative and positive words.
pos <- read.csv("Data_mr_pos.csv",stringsAsFactors=F)
neg <- read.csv("Data_mr_neg.csv",stringsAsFactors=F)

# Do a paired t-test. Is the difference between proportion of negative quasi-sentences and the proportion of positive quasi-sentences significant?
t.test(pos$prop_moral_qs, neg$prop_moral_qs, paired=T)
mean(pos$prop_moral_qs)
mean(neg$prop_moral_qs)

######################
## Table SI5.1
#####################

load("Study1_including_leaners.RData")

t <- study1_incl_leaners[,c("mr", "voted","which_id_party_election","educ_scaled","age_decades","male","income_scaled","niche","country_name","prev_enp_nat")]
t <- t[complete.cases(t),]
w_leaners_mod1 <- lrm(voted ~ mr*educ_scaled + age_decades + male + income_scaled  + niche +prev_enp_nat+ country_name, data=t, x=T, y=T)
w_leaners_mod1  <- robcov(w_leaners_mod1, t$which_id_party_election, method="huber")

w_leaners_mod2 <- glmer(voted ~ mr*educ_scaled + age_decades + male + income_scaled + (1| country_name) + (1|country_election) + (1|which_id_party), data=study1_incl_leaners, family="binomial")

load("Study1.RData")

t <- subset(study1, country_name!="Australia")
t <- t[,c("mr", "voted","which_id_party_election","educ_scaled","age_decades","male","income_scaled","niche","country_name","prev_enp_nat")]
t <- t[complete.cases(t),]
wo_au_mod1 <- lrm(voted ~ mr*educ_scaled + age_decades + male + income_scaled + niche +prev_enp_nat+ country_name, data=t, x=T, y=T)
wo_au_mod1  <- robcov(wo_au_mod1, t$which_id_party_election, method="huber")

wo_au_mod2 <- glmer(voted ~ mr*educ_scaled + age_decades + male + income_scaled + (1| country_name) + (1|country_election) + (1|which_id_party), data=subset(study1, country_name!="Australia"), family="binomial")

t <- study1[,c("mr", "voted","which_id_party_election","educ_scaled","age_decades","male","income_scaled","niche","prev_enp_nat")]
t <- t[complete.cases(t),]
no_cfe_mod1 <- lrm(voted ~ mr*educ_scaled + age_decades + male + income_scaled+ niche +prev_enp_nat, data=t, x=T, y=T)
no_cfe_mod1  <- robcov(no_cfe_mod1, t$which_id_party_election, method="huber")

glmer_w_party_election_vars <- glmer(voted ~ mr*educ_scaled + age_decades + male + income_scaled + niche + prev_enp_nat + (1| country_name) + (1|country_election) + (1|which_id_party), data=study1, family="binomial")

texreg(list(w_leaners_mod1, w_leaners_mod2, wo_au_mod1, wo_au_mod2, no_cfe_mod1, glmer_w_party_election_vars), stars=c(0.05))

######################
## Table SI5.2
#####################

t <- study1[,c("mr", "voted","which_id_party_election","educ_scaled","age_decades","male","income_scaled","niche","country_name","prev_enp_nat")]
t <- subset(t, educ_scaled >= 0.5)
t <- t[complete.cases(t),]
high_educ_mod1 <- lrm(voted ~ mr + educ_scaled + age_decades+ male + income_scaled+niche +prev_enp_nat+ country_name, data=t, x=T, y=T)
high_educ_mod1  <- robcov(high_educ_mod1, t$which_id_party_election, method="huber")

high_educ_mod2 <- glmer(voted ~ mr + educ_scaled + age_decades + male + income_scaled  + (1| country_name) + (1|country_election) + (1|which_id_party), data=subset(study1, educ_scaled >= 0.5), family="binomial")

t <- study1[,c("mr", "voted","which_id_party_election","educ_scaled","age_decades","male","income_scaled","niche","country_name","prev_enp_nat")]
t <- subset(t, educ_scaled < 0.5)
t <- t[complete.cases(t),]
low_educ_mod1 <- lrm(voted ~ mr + educ_scaled +age_decades+ male + income_scaled + niche +prev_enp_nat+ country_name, data=t, x=T, y=T)
low_educ_mod1  <- robcov(low_educ_mod1, t$which_id_party_election, method="huber")

low_educ_mod2 <- glmer(voted ~ mr + educ_scaled + age_decades + male + income_scaled  + (1| country_name) + (1|country_election) + (1|which_id_party), data=subset(study1, educ_scaled < 0.5), family="binomial")

t <- study1[,c("mr", "voted","which_id_party_election","educ_scaled","age_decades","male","income_scaled","niche","country_name","prev_enp_nat")]
t <- t[complete.cases(t),]
direct_mod1 <- lrm(voted ~ mr + educ_scaled + age_decades + male + income_scaled + niche +prev_enp_nat+ country_name, data=t, x=T, y=T)
direct_mod1  <- robcov(direct_mod1, t$which_id_party_election, method="huber")

direct_mod2 <- glmer(voted ~ mr + educ_scaled + age_decades + male + income_scaled  +(1| country_name) + (1|country_election) + (1|which_id_party), data=study1, family="binomial")

# Read in the moral rhetoric measure including liberty foundation
mr_incl_liberty <- read.csv("Data_mr_with_liberty.csv",stringsAsFactors=F)

# Assign the measure.
study1$mr_incl_liberty <- as.numeric(mapvalues(study1$which_id_party_election, mr_incl_liberty$party_election,mr_incl_liberty$prop_moral_qs))
study1$mr_incl_liberty[!(grepl("_",study1$which_id_party_election))] <- NA

t <- study1[,c("mr_incl_liberty", "voted","which_id_party_election","educ_scaled","age_decades","male","income_scaled","niche","country_name","prev_enp_nat")]
t <- t[complete.cases(t),]
liberty_mod1 <- lrm(voted ~ mr_incl_liberty*educ_scaled + age_decades + male + income_scaled + niche +prev_enp_nat+ country_name, data=t, x=T, y=T)
liberty_mod1  <- robcov(liberty_mod1, t$which_id_party_election, method="huber")

liberty_mod2 <- glmer(voted ~ mr_incl_liberty*educ_scaled + age_decades + male + income_scaled + (1| country_name) + (1|country_election) + (1|which_id_party), data=study1, family="binomial")

texreg(list(high_educ_mod1, high_educ_mod2, low_educ_mod1, low_educ_mod2, direct_mod1, direct_mod2, liberty_mod1, liberty_mod2), stars=c(0.05))

######################
## Table 5.3
#####################

dat <- study1[,c("mr", "voted","educ_scaled","age_decades","male","income_scaled","country_name","country_election","which_id_party")]
dat <- dat[complete.cases(dat),]
dat$mr_educ_scaled <- dat$mr*dat$educ_scaled

# Repeat NA to the length of the rows
cntry <- rep(NA, dim(dat)[[1]])
cntry
# Assign numeric to each country, in increasing order
j <- 1
for (i in unique(dat$country_name)){
  cntry[dat$country_name==i] <- j
  j <- j+1
}
cntry
# Define total number of country levels
n.cntry <- length(unique(dat$country_name))

# Repeat NA to the length of the rows
election <- rep(NA, dim(dat)[[1]])
# Assign numeric to each election, in increasing order
l <- 1
for (i in unique(dat$country_election)){
  election[dat$country_election==i] <- l
  l <- l+1
}
# Define total number of election levels
n.election <- length(unique(dat$country_election))

# Repeat NA to the length of the rows
party <- rep(NA, dim(dat)[[1]])
# Assign numeric to each party, in increasing order
m <- 1
for (i in unique(dat$which_id_party)){
  party[dat$which_id_party==i] <- m
  m <- m+1
}
# Define total number of party levels
n.party <- length(unique(dat$which_id_party))

# Define number of observations
N <- dim(dat)[[1]]

# Create other variables
y <- dat$voted
mr <- dat$mr
educ <- dat$educ_scaled
age <- dat$age_decades
male <- dat$male
income <- dat$income_scaled

# Define model
model <- "model{
for(i in 1:N){
y[i] ~ dbin(p.bound[i], 1)
p.bound[i] <- max(0, min(1, p[i]))
logit(p[i]) <- Xbeta[i]
Xbeta[i] <- b.0 + b.mr*mr[i] + b.educ*educ[i] + b.mr.educ*mr[i]*educ[i] + b.age*age[i] + b.male*male[i] + b.income *income[i] + b.cntry[cntry[i]] + b.election[election[i]] + b.party[party[i]]
}
b.0 ~ dnorm(0, .0001)
b.mr ~ dnorm(0, .0001)
b.educ ~ dnorm(0, .0001)
b.mr.educ ~ dnorm(0, .0001)
b.age ~ dnorm(0, .0001)
b.male ~ dnorm(0, .0001)
b.income ~ dnorm(0, .0001)

for(j in 1:n.party){b.party[j] ~ dnorm(0, tau.party)}
for(j in 1:n.election){b.election[j] ~ dnorm(0, tau.election)}
for(j in 1:n.cntry){b.cntry[j] ~ dnorm(0, tau.cntry)}

tau.party <- pow(sigma.party, -2)
tau.election <- pow(sigma.election, -2)
tau.cntry <- pow(sigma.cntry, -2)

sigma.party ~ dunif(0, 100)
sigma.election ~ dunif(0, 100)
sigma.cntry ~ dunif(0, 100)
}"

# Create a list of the DV, intercept, independent variables, country, num of obs, num of countries, and number of columns
data <- list("y", "mr","educ","age","male","income","cntry","election","party", "N","n.cntry","n.election","n.party")
inits <- function(){
  list(b.0=rnorm(1),b.mr=rnorm(1),b.educ=rnorm(1),b.mr.educ=rnorm(1),b.age=rnorm(1),b.male=rnorm(1),b.income=rnorm(1),b.cntry=rnorm(n.cntry),b.election=rnorm(n.election),b.party=rnorm(n.party),sigma.party=runif(1),sigma.election=runif(1),sigma.cntry=runif(1))
}
parameters <- c("b.0","b.mr","b.educ","b.mr.educ","b.age","b.male","b.income","sigma.party","sigma.cntry","sigma.election")
fit <- jags(data, inits, parameters, textConnection(model),n.chains=3)
fit

######################
## Table 5.4 
#####################

# Assign individualizing and binding mr measures.
study1$mr_left <- as.numeric(mapvalues(study1$which_id_party_election, ideo_measures$party_election,ideo_measures$prop_moral_qs_left))
study1$mr_left[!(grepl("_",study1$which_id_party_election))] <- NA
study1$mr_right <- as.numeric(mapvalues(study1$which_id_party_election, ideo_measures$party_election,ideo_measures$prop_moral_qs_right))
study1$mr_right[!(grepl("_",study1$which_id_party_election))] <- NA

# Divide the data to left and right
dat_left <- subset(study1, rile<=0)
dat_right <- subset(study1, rile>0)

t <- dat_left[,c("mr_left", "voted","which_id_party_election","educ_scaled","age_decades","male","income_scaled","niche","country_name","prev_enp_nat")]
t <- t[complete.cases(t),]
mod1_only_left <- lrm(voted ~ mr_left*educ_scaled + age_decades + male + income_scaled+ niche +prev_enp_nat+ country_name, data=t, x=T, y=T)
mod1_only_left  <- robcov(mod1_only_left, t$which_id_party_election, method="huber")

mod2_only_left <- glmer(voted ~ mr_left*educ_scaled + age_decades + male + income_scaled+ (1| country_name) + (1|country_election) + (1|which_id_party), data=dat_left, family="binomial")

t <- dat_left[,c("mr_left", "voted","which_id_party_election","educ_scaled","age_decades","male","income_scaled","niche","country_name","prev_enp_nat","mr_right")]
t <- t[complete.cases(t),]
mod1_left <- lrm(voted ~ mr_left*educ_scaled + mr_right*educ_scaled + age_decades + male + income_scaled + niche +prev_enp_nat+ country_name, data=t, x=T, y=T)
mod1_left  <- robcov(mod1_left, t$which_id_party_election, method="huber")

mod2_left <- glmer(voted ~ mr_left*educ_scaled +mr_right*educ_scaled+ age_decades + male + income_scaled + (1| country_name) + (1|country_election) + (1|which_id_party), data=dat_left, family="binomial")

t <- dat_right[,c("mr_left", "voted","which_id_party_election","educ_scaled","age_decades","male","income_scaled","niche","country_name","prev_enp_nat","mr_right")]
t <- t[complete.cases(t),]
mod1_right <- lrm(voted ~ mr_left*educ_scaled + mr_right*educ_scaled + age_decades + male + income_scaled + niche +prev_enp_nat+ country_name, data=t, x=T, y=T)
mod1_right  <- robcov(mod1_right, t$which_id_party_election, method="huber")

mod2_right <- glmer(voted ~ mr_left*educ_scaled +mr_right*educ_scaled+ age_decades + male + income_scaled + (1| country_name) + (1|country_election) + (1|which_id_party), data=dat_right, family="binomial")

# Table
texreg(list(mod1_only_left, mod2_only_left, mod1_left, mod2_left, mod1_right, mod2_right), stars=c(0.05))

######################
## Figure SI5.1
#####################

table1_mod2 <- glmer(voted ~ mr*educ_scaled + age_decades + male + income_scaled + (1| country_name) + (1|country_election) + (1|which_id_party), data=study1, family="binomial")

pframe0_low <- expand.grid(mr =seq(0.06, 0.51,by=0.005), educ_scaled =round(summary(table1_mod2@frame[,"educ_scaled"])[2],2), age_decades =round(mean(table1_mod2@frame[,"age_decades"]),2),male=0, income_scaled =round(mean(table1_mod2@frame[,"income_scaled"]),2))
pframe0_low$"mr:educ_scaled" <- unname(pframe0_low$mr*pframe0_high$educ_scaled)
mm <- model.matrix(~ mr  + educ_scaled + age_decades + male + income_scaled + mr:educ_scaled, data=pframe0_low)
pframe1_low <- data.frame(pframe0_low, eta=mm %*% fixef(table1_mod2)) 
pframe1_low <- with(pframe1_low, data.frame(pframe1_low, CaseMarking=invlogit(eta)))
pframe1_low$pse <- diag(mm %*% tcrossprod(vcov(table1_mod2),mm))
pframe1_low$hi <- with(pframe1_low, eta+1.96*pse) # upper bound in log odds
pframe1_low$low <- with(pframe1_low, eta-1.96*pse) # lower bound in log odds
pframe1_low$hi.prob <- invlogit(pframe1_low$hi) # Change to probability
pframe1_low$low.prob <- invlogit(pframe1_low$low) # Change to probability

pdf(file="plot_mlm_pred_low_educ.pdf")
plot(seq(0.06, 0.51,by=0.005), pframe1_low$CaseMarking, type="l",ylim=c(0.85,1),xlab="Moral rhetoric",ylab="Probability of turnout",main="",cex.lab=1.3,lwd=2)
polygon.x <- c(seq(0.06, 0.51,by=0.005), rev(seq(0.06, 0.51,by=0.005)))
polygon.y <- c(pframe1_low$low.prob, rev(pframe1_low$hi.prob))
polygon(polygon.x, polygon.y, col=adjustcolor("black", alpha.f=0.1),border=NA)
par(new=T)
hist(unique(table1_mod2@frame[,"mr"]), axes=F,xlab="",ylab="",border="gray",main="")
dev.off()

############################
### FD calculations for MR value one sd below and above mean
#############################

set.seed(314)
sims <- sim(table1_mod2,n.sims=100000)
fe.sims <- fixef(sims)

# choose values for the individual-level variables
f.educ <- round(summary(table1_mod2@frame[,"educ_scaled"])[2],2)
f.age <- mean(table1_mod2@frame[,"age_decades"])
f.male <- 0
f.income <- mean(table1_mod2@frame[,"income_scaled"])

# function to create X.pred for fixed coefficients
create.X.pred <- function() {
  X <- cbind(1,                                    
             f.mr,                                         
             f.educ,                                 
             f.age,                                   
             f.male,                                            
             f.income,                                       
             f.mr*f.educ)
}

# set values of variable of interest
f.mr <- c(0.20, 0.39)

# create prediction matrix
X.pred <- create.X.pred()

# compute the simulated probabilities
y.star <- X.pred%*%t(fe.sims)
p.lo <- plogis(y.star[1, ] )
p.hi <- plogis(y.star[2,])

# compute the qis
fd.stat.gen.high.educ <- p.hi - p.lo

fd <- quantile(fd.stat.gen.high.educ, 0.5)
lwr <- quantile(fd.stat.gen.high.educ, 0.025)
upr <- quantile(fd.stat.gen.high.educ, 0.975)
c(lwr, fd, upr)

############################
### FD calculations for MR value min and max
#############################

# choose values for the individual-level variables
f.educ <- round(summary(table1_mod2@frame[,"educ_scaled"])[2],2)
f.age <- mean(table1_mod2@frame[,"age_decades"])
f.male <- 0
f.income <- mean(table1_mod2@frame[,"income_scaled"])

# set values of variable of interest
f.mr <- c(0.06, 0.51)

# create prediction matrix
X.pred <- create.X.pred()

# compute the simulated probabilities
y.star <- X.pred%*%t(fe.sims)
p.lo <- plogis(y.star[1, ] )
p.hi <- plogis(y.star[2,])

# compute the qis
fd.stat.gen.high.educ <- p.hi - p.lo

fd <- quantile(fd.stat.gen.high.educ, 0.5)
lwr <- quantile(fd.stat.gen.high.educ, 0.025)
upr <- quantile(fd.stat.gen.high.educ, 0.975)
c(lwr, fd, upr)
