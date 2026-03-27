library(tidyverse)
library(csvread)
library(quanteda)


#CHOOSE CORPUS
mydata <- corpus1 #or corpus2
mydata.dep <- corpus1.dep #or corpus2.dep


#SENTIMENT and AUTHENTICITY ANALYSIS
library(sentimentr)
sentences <- get_sentences(mydata$text)

##CREATE AND UPDATE KEYS
###AUTHENTICITY SUBTYPE KEYS
craft <- read_csv("craft_authent.csv")
idio <- read_csv("idio_authent.csv")
moral <- read_csv("moral_authent.csv")
type <- read_csv("type_authent.csv")

####Rescale scores to -1, 1 scoring system
craft$score <- craft$score %>% 
    scale(center = T, scale = F) %>% 
    general_rescale(lower = -1, upper = 1)
 
idio$score <- idio$score %>% 
   scale(center = T, scale = F) %>% 
   general_rescale(lower = -1, upper = 1)
  
moral$score <- moral$score %>% 
   scale(center = T, scale = F) %>% 
   general_rescale(lower = -1, upper = 1)

type$score <- type$score %>% 
   scale(center = T, scale = F) %>% 
   general_rescale(lower = -1, upper = 1)

####Create Keys
craft_key <- as_key(craft, comparison = lexicon::hash_valence_shifters, sentiment = T)
idio_key <- as_key(idio, comparison = lexicon::hash_valence_shifters, sentiment = T)
moral_key <- as_key(moral, comparison = lexicon::hash_valence_shifters, sentiment = T)
type_key <- as_key(type, comparison = lexicon::hash_valence_shifters, sentiment = T)

remove(craft, type, idio, moral)


###SENTIMENT Keys with authenticity removed
jr_no_auth_key <- lexicon::hash_sentiment_jockers_rinker %>% 
  update_key(comparison = craft_key) %>% 
  update_key(comparison = idio_key) %>% 
  update_key(comparison = moral_key) %>% 
  update_key(comparison = type_key)


#ANALYSIS
##Overall AUTHENTICITY Score Uses average of the 4 subtypes
sentiment_scores <- sentiment(sentences, polarity_dt = jr_no_auth_key) %>% 
  sentiment_by()

craft_scores <- sentiment(sentences, polarity_dt = craft_key) %>% 
  sentiment_by()

idio_scores <- sentiment(sentences, polarity_dt = idio_key) %>% 
  sentiment_by()

moral_scores <- sentiment(sentences, polarity_dt = moral_key) %>% 
  sentiment_by()

type_scores <- sentiment(sentences, polarity_dt = type_key) %>% 
  sentiment_by()


###Combine back together
sentiment_scores <- rename(sentiment_scores, doc_id = element_id)

craft_scores <- craft_scores %>% 
  rename(doc_id = element_id) %>% 
  rename(craft_avg = ave_sentiment)

idio_scores <- idio_scores %>% 
  rename(doc_id = element_id) %>% 
  rename(idio_avg = ave_sentiment)

moral_scores <- moral_scores %>% 
  rename(doc_id = element_id) %>% 
  rename(moral_avg = ave_sentiment)

type_scores <- type_scores %>% 
  rename(doc_id = element_id) %>% 
  rename(type_avg = ave_sentiment)

craft_scores[craft_scores == 0 ] <- NA
idio_scores[idio_scores == 0] <- NA
moral_scores[moral_scores == 0] <- NA
type_scores[type_scores == 0] <- NA

mydata.m1 <- mydata %>% 
  left_join(select(sentiment_scores, -sd), by = "doc_id") %>% 
  left_join(select(craft_scores, -word_count, -sd), by = "doc_id") %>% 
  left_join(select(idio_scores, -word_count, -sd), by = "doc_id") %>% 
  left_join(select(moral_scores, -word_count, -sd), by = "doc_id") %>% 
  left_join(select(type_scores, -word_count, -sd), by = "doc_id")

###Create combined average  
mydata.m1$combined_avg <- rowMeans(mydata.m1[ ,13:16], na.rm=T) #for corpus 1
#or
mydata.m1$combined_avg <- rowMeans(mydata.m1[ ,14:17], na.rm=T) #for corpus 2

###Center and scale data 
mydata.m1$ave_sentiment <- general_rescale(mydata.m1$ave_sentiment, lower = -1, upper = 1, keep.zero = T)
mydata.m1$craft_avg <- general_rescale(mydata.m1$craft_avg, lower = -1, upper = 1, keep.zero = T)
mydata.m1$idio_avg <- general_rescale(mydata.m1$idio_avg, lower = -1, upper = 1, keep.zero = T)
mydata.m1$moral_avg <- general_rescale(mydata.m1$moral_avg, lower = -1, upper = 1, keep.zero = T)
mydata.m1$type_avg <- general_rescale(mydata.m1$type_avg, lower = -1, upper = 1, keep.zero = T)
mydata.m1$combined_avg <- general_rescale(mydata.m1$combined_avg, lower = -1, upper = 1, keep.zero = T)


###Number of Authenticity Words per Review
authent_words <- read_csv("authent_2.csv")
authent_words <- rename(authent_words, "lemma" = "word")

hits_by_review <- mydata.dep %>%                           
  inner_join(authent_words, by = "lemma") %>% 
  group_by(doc_id) %>% 
  summarise(total_hits = n_distinct(sentence_id, token_id)) #prevents one hit from being counted twice

mydata.m1 < mydata.m1 %>% 
  inner_join(hits_by_review, by = "doc_id")

corpus1.m1 <- mydata.m1
#or
corpus2.m1 <- mydata.m1

###clean Up
remove(craft_scores, type_scores, idio_scores, moral_scores, craft_key, type_key, idio_key, moral_key, sentiment, sentences, sentiment_scores, jr_no_auth_key)
remove(mydata, mydata.m1, authent_words, hits_by_review)

  
#DATA VISUALISATION and STATS
corpus1_five.m1 <- filter(corpus1.m1, total_hits >= 5)

##DATA VISUALISATION
###fig. 1
ggplot(data = corpus1.m1, mapping = aes(x = combined_avg, y = ave_sentiment)) +
  geom_point(size = .1) +
  geom_jitter(size = .1) +
  ylim(c(-1,1)) +
  xlim(c(-1,1)) +
  geom_smooth() +
  labs(x = "Authenticity Rating", y = "Sentiment Rating", title = "Authenticity Compared to Sentiment:", subtitle = "averaged per review for all data")

###fig. 2
ggplot(data = corpus1_five.m1, mapping = aes(x = combined_avg, y = ave_sentiment)) +
  geom_point(size = .1) +
  geom_jitter(size = .1) +
  ylim(c(-1,1)) +
  xlim(c(-1,1)) +
  geom_smooth() +
  labs(x = "Authenticity Rating", y = "Sentiment Rating", title = "Authenticity Compared to Sentiment:", subtitle = "for reviews where authenticity keywords n >= 5")

###fig. 3
ggplot(data = corpus2.m1, mapping = aes(x = combined_avg, y = ave_sentiment)) +
  geom_point(size = .1) +
  geom_jitter(size = .1) +
  ylim(c(-1,1)) +
  xlim(c(-1,1)) +
  geom_smooth() +
  labs(x = "Authenticity Rating", y = "Sentiment Rating", title = "Authenticity Compared to Sentiment:", subtitle = "for reviews where authenticity keywords n >= 5")


##STATISTICS
###table 1
auth_sent_c1 <- lm(ave_sentiment ~ combined_avg, data = corpus1.m1)
summary(auth_sent_c1)

auth_sent_c1_5 <- lm(ave_sentiment ~ combined_avg, data = corpus1_five.m1) ###!!!!
summary(auth_sent_c1_5)

auth_sent_c2 <- lm(ave_sentiment ~ combined_avg, data = corpus2.m1)
summary(auth_sent_c2)

###table 2
fiveplus_sentiment <- lm(ave_sentiment ~ moral_avg+type_avg+idio_avg+craft_avg, data = fiveplus_mydata.m1)
summary(fiveplus_sentiment)

