# Where conspiracy theories flourish: A study of YouTube comments and Bill Gates conspiracy theories ######
### Load pakacages ####
library(quanteda)
library(ggplot2)
library(stringr)
library(vader)
library(lubridate)
library(stm)
library(dplyr)

### Load the dataset ####
data_1 <-read.csv('VOX_CGTN_Fox.csv', 
                  stringsAsFactors = F,encoding='utf-8')
#Remove columns
data_1 <- data_1[ ,!names(data_1) %in% "vosonTxt_comment"]
#Unescaping unicode 
data_1$Comment <- stringi::stri_unescape_unicode(data_1$Comment)


#### Preprocessing data #######
# Filter non-English characters
#install.packages("cld3")
library(cld3)
data_prepro <- subset(data_1, detect_language(data_1$Comment) == 'en')

# Remove spams in comment
data_prepro_2 <- data_prepro[!duplicated(data_prepro['Comment']),]
dim(data_prepro_2[duplicated(data_prepro_2$Comment),])
View(data_prepro[duplicated(data_prepro$Comment),])

# Reformat time
data_prepro_2$PublishedAt <- gsub("[a-zA-Z ]", " ",data_prepro_2$PublishedAt)
str_trim(data_prepro_2$PublishedAt, side =("right")) #Strim white space
data_prepro_2$PublishedAt <- as.POSIXct(data_prepro_2$PublishedAt,tz ="UTC")

# Create a new time column
## Create a new publish time column
data_prepro_2 <- data_prepro_2 %>% mutate(Published_date = as.Date(data_prepro_2$PublishedAt, format = "%m/%d/%y"))
data_prepro_2$Published_date

# Change into numeric date
data_prepro_2$Published_day_of_year <- yday(data_prepro_2$Published_date)
data_prepro_2$Published_day_of_year[1:5]
class(data_prepro_2$Published_day_of_year)
data_prepro_2%>%count(Published_date)

#### Text preprocessing #######
# Create a new column for text processing
data_prepro_2 <- data_prepro_2 %>% mutate(process_txt=data_prepro_2$Comment)
View(data_prepro_2)

# Filter rows based on length of string
class(data_prepro_2$process_txt)
data_prepro_2 <- data_prepro_2[nchar(as.character(data_prepro_2$process_txt))>20,]

# Check if contain keywords ######
data_prepro_2 %>% filter(str_detect(process_txt, "(?i)bill|gates|bill gates|bill")) #?i ignore case
data_prepro_2 %>% filter(str_detect(process_txt, "(?i)q\\s?anon"))
data_prepro_2 %>% filter(str_detect(process_txt, "(?i)event\\s?201|id\\s?2020"))
data_prepro_2 %>% filter(str_detect(process_txt, "(?i)agenda\\s?201"))
data_prepro_2 %>% filter(str_detect(process_txt, "(?i)doctor|scientist|virologist"))
data_prepro_2 %>% filter(str_detect(process_txt, "(?i)plandemic"))
data_prepro_2 %>% filter(str_detect(process_txt, "(?i)communist|china|chinese"))
data_prepro_2 %>% filter(str_detect(process_txt, "(?i)bio\\s?weapon|bio-weapon"))
data_prepro_2 %>% filter(str_detect(process_txt, "(?i)melinda"))
data_prepro_2 %>% filter(str_detect(process_txt, "(?i)dr\\.+\\s?fauci|anthony+\\s?fauci|fauci"))
data_prepro_2 %>% filter(str_detect(process_txt, "(?i)dr\\.+\\s?faucci|anthony+\\s?faucci|faucci"))
data_prepro_2 %>% filter(str_detect(process_txt, "(?i)president\\s?trump|donald\\s?trump(s)?"))
data_prepro_2 %>% filter(str_detect(process_txt, "(?i)billy\\s?boy|billy|billy\\s?gates"))
data_prepro_2 %>% filter(str_detect(process_txt, "(?i)\\bQ\\b"))
data_prepro_2 %>% filter(str_detect(Comment, "(?i)\\bbflyers95\\b|\\bbroadstreet\\s?flyers95"))

data_prepro_2 %>% filter(str_detect(process_txt, "(?i)\\bgod\\s?bless\\b|\\bgod\\s?sakes\\b|\\bjesus\\s?christ\\b"))
data_prepro_2 %>% filter(str_detect(process_txt, "(?i)\\bhey\\b"))

data_prepro_2 %>% filter(str_detect(process_txt, "(?i)ha\\s?ha|lol"))
data_prepro_2 %>% filter(str_detect(process_txt, "(?i)blah\\s?blah{3}"))


### Edit all variations of topics mentioned ####
data_prepro_2$process_txt <- data_prepro_2$process_txt%>%str_replace_all("(?i)event\\s?201","event201")
data_prepro_2$process_txt  <- data_prepro_2$process_txt%>%str_replace_all("(?i)id\\s?2020","id2020")
data_prepro_2$process_txt  <- data_prepro_2$process_txt%>%str_replace_all(c("(?i)q\\s?anon" = 'qanon', 
                                                                            "(?i)Qanondotpub" = "qanon"))
data_prepro_2$process_txt  <- data_prepro_2$process_txt%>%str_replace_all("(?i)bill\\s?gates", "billgates")
data_prepro_2$process_txt  <- gsub(pattern = "agenda 201",replace='event201',x=data_prepro_2$process_txt)

data_prepro_2$process_txt  <- data_prepro_2$process_txt%>%str_replace_all(c("(?i)dr\\.+\\s?faucc?i" = 'anthonyfauci', 
                                                                            "(?i)anthony+\\s?faucc?i" = "anthonyfauci",
                                                                            "(?i)faucc?i" = 'anthonyfauci'))

data_prepro_2$process_txt  <- data_prepro_2$process_txt%>%str_replace_all(c("(?i)president\\s?trump(s)?" = 'donaldtrump', 
                                                                            "(?i)donald\\s?trump(s)?" = "donaldtrump"))

data_prepro_2$process_txt  <- data_prepro_2$process_txt%>%str_replace_all(c("(?i)billy\\s?boy" = 'billgates', 
                                                                            "(?i)billy" = "billgates",
                                                                            "(?i)billy\\s?gates" = 'billgates'))

### Remove laugh
data_prepro_2$process_txt  <- data_prepro_2$process_txt %>%str_replace_all("(?i)lolwut|Bahahaha|muhuhahaha|Hahahahahhahahahahaha|Ahhaha|Ahhahahha", "")
data_prepro_2$process_txt<- data_prepro_2$process_txt %>%str_remove_all("(?i)\\b(a*ha+h[ha]*|o?l+o+l+[ol]*)\\b")

### Remove blah blah 
data_prepro_2$process_txt<- data_prepro_2$process_txt %>%str_remove_all("(?i)\\bblah\\b")
data_prepro_2$process_txt<- data_prepro_2$process_txt %>%str_remove_all("(?i)\\bhey\\b")


### Remove emoticon 
data_prepro_2$process_txt <- iconv(data_prepro_2$process_txt,"latin1", "ASCII", sub="")

### Remove mention @user from new column
data_prepro_2$process_txt<- gsub(pattern='@\\w+', replace='',x=data_prepro_2$process_txt)
url_pattern <- "http[s]?://(?:[a-zA-Z]|[0-9]|[$-_@.&+]|[!*\\(\\),]|(?:%[0-9a-fA-F][0-9a-fA-F]))+"
data_prepro_2$process_txt <- data_prepro_2$process_txt %>%str_remove_all(url_pattern)

### Remove whitespace
data_prepro_2$process_txt <- gsub(pattern='^[[:space:]]*|[[:space:]]*$', replace='',x=data_prepro_2$process_txt)
data_prepro_2$process_txt[27]
data_prepro_2$Comment[27]

### Check duplicate comment (Some spams use different mentioned username)
dim(data_prepro_2[duplicated(data_prepro_2$process_txt),])
data_prepro_2 <- data_prepro_2 %>% distinct(process_txt, .keep_all =TRUE)

### Extract URLs
url_pattern <- "http[s]?://(?:[a-zA-Z]|[0-9]|[$-_@.&+]|[!*\\(\\),]|(?:%[0-9a-fA-F][0-9a-fA-F]))+"
data_prepro_2 <- data_prepro_2 %>% mutate(URL=str_extract_all(data_prepro_2$Comment,url_pattern))
data_prepro_2$URL <- lapply(data_prepro_2$URL, function(x) if(identical(x,character(0))) NA_character_ else x)

### Mention username
mention_pattern_space <- paste(c("@([A-Za-z]+[A-Za-z0-9_]+)(?![A-Za-z0-9_]*\\.)(\\s+?[A-Z-0_9_])([a-z0-9_]+)+", "@(\\w+)"),collapse="|")
data_prepro_2 <- data_prepro_2 %>% mutate(mentioned_username_2=str_extract_all(data_prepro_2$Comment,mention_pattern_space))
data_prepro_2$mentioned_username_2 <- lapply(data_prepro_2$mentioned_username_2, function(x) if(identical(x,character(0))) NA_character_ else x)
View(data_prepro_2)

### Save the processed data
#data_prepro_3 <- apply(data_prepro_2,2,as.character)
#write.csv(data_prepro_3, file = "processed_VOX_CGTN_FOX_merged_April_28.csv", row.names = FALSE)

## Topic modelling using stm ######
# Transforming into factors
data_prepro_2$video_channel <- factor(data_prepro_2$video_channel, levels = c("CGTN","Vox","Fox"))
levels(data_prepro_2$video_channel)

# Create a new column from rowID
data_prepro_2 <- tibble::rowid_to_column(data_prepro_2,"ID")

# Create a corpus and document feature matrix
corp_1_ <- corpus(data_prepro_2$process_txt, 
                  docvars=data_prepro_2[,c("Published_day_of_year","Published_date","video_channel","Comment","LikeCount","ReplyCount")])

# Tokenise the corpus (BIGRAM)
#library("udpipe")
#install.packages("lexicon")
library("lexicon")
library("tidytext")
data("stop_words")
data("profanity_zac_anger")

video_dfm_1 <- corp_1_ %>% quanteda::tokens(remove_punct = TRUE,
                                            remove_numbers=TRUE,
                                            remove_symbols=TRUE,
                                            remove_url=TRUE) %>% 
  quanteda::tokens_remove(stop_words$word,padding=TRUE) %>%
  quanteda::tokens_remove(lexicon::profanity_zac_anger,padding=TRUE) %>%
  quanteda::tokens_replace(pattern = lexicon::hash_lemmas$token, replacement = lexicon::hash_lemmas$lemma) %>%
  quanteda::tokens_ngrams(n=2) %>% 
  dfm() %>%
  dfm_trim(min_termfreq = 5)

# Create a document variable (docvar to keep track of the original comment)
#docvars(video_dfm_1, 'text_ORIGINAL') <-data_prepro_2$Comment

###### Convert to stm object ###############
## Convert the quanteda object into an input for stm
out <- convert(video_dfm_1,to = 'stm') #For stminsights

## Search K ######
##Search K 
k_search_output <- searchK(out$documents,out$vocab,K=c(8,10,12,14,16,18,20,22,24), prevalence =~ video_channel + s(Published_day_of_year),
                           data=out$meta,verbose=FALSE,heldout.seed = 123)


# Fit a topic model ##########
# Fit a topic model, specifying the fixed number of topics; New model using dataset CGTN, Fox, and Vox
# K = 22
st_model_ <-stm(documents=out$documents,vocab = out$vocab,
                  K=22,prevalence =~ video_channel + s(Published_day_of_year),
                  data=out$meta,verbose=FALSE, init.type = "Spectral",seed=42)

# Estimate 
st_model_esti_22 <- estimateEffect(1:22 ~video_channel + s(Published_day_of_year), st_model_18,
                                   meta=out$meta)

# Method difference
plot.estimateEffect(st_model_esti_22, "video_channel",method='difference',
                    cov.value1 = 'Fox', cov.value2 = 'Vox',
                    labeltype = "frex", n=2, verbose.labels = F,
                    model=st_model_)

# Plot the convergence of model, to assess goodness of fit
plot(st_model_18$convergence$bound, type='l',ylab ='Approximate Objective', main="Convergence")

# Visualise topic modelling (top 10 words)
plot(st_model_, type="summary", text.cex=0.5, n=8)

# Wordcloud
cloud(st_model_, topic=2, scale=c(2,.25))

# Examine top 5 words for all topics
labelTopics(st_model_,c(1:8),n=20)

# Examine top 10 words for topic 1 
labelTopics(st_model_,1,n=10)

