library(tidyverse)
library(tidytext)
library(cld2)


# read yelp master files in
yelp_business <- read_csv("yelp_business.csv")
yelp_reviews  <- read_csv("yelp_review.csv")
yelp_business_attributes <- read_csv("yelp_business_attributes.csv")

# split the categories up using tokenization
split_list <- yelp_business %>% 
  unnest_tokens(category_split, categories)

# filter for only instances of "restaurants" category
restaurant_list <- filter(split_list, category_split == "restaurants" )

#generate list of only restaurants with business id to use to sort reviews
restaurant_list_id_only <- select(restaurant_list, business_id) 

#filter out for only reviews of restaurants using inner_join
restaurant_reviews <- inner_join(yelp_reviews, restaurant_list_id_only, by = "business_id")

#Select only English language reviews in the USA and remove unneded variables
mydata <- select(restaurant_list, -category_split, -is_open) %>% 
  inner_join(restaurant_reviews, by = "business_id") %>% 
  rename(rest_stars = stars.x, review_stars = stars.y) %>% 
  select(-latitude, -longitude, -neighborhood, -city, -useful, -funny, -cool, -name, -address, -review_count, -postal_code)

mydata <- mutate(mydata, lang = detect_language(mydata$text))

mydata <- mydata %>% 
  filter(lang == "en") %>% 
  filter(state == "AK" | state == "AZ" | state == "CA" | state == "CO" 
                      | state == "IL" |state == "IN" | state == "NC" | state == "NV" | state == "NY" | state == "OH" 
                      | state == "PA" | state == "SC" |state == "VA" | state == "WI" )

#Create sample for Corpus 1 and filter for reviews with Authenticity words
samp <- sample_n(mydata, 500000)

#Split sample to chunks to not overwhelm string_detect and choose reviews with at least one authenticity term. This prevents parsing "unused" reviews later in spacy.
temp_samp <- samp %>% 
  slice_sample(n = 100000)

samp <- samp %>% 
  anti_join(temp_samp, by = "review_id")

temp_samp <- temp_samp %>% 
  filter(str_detect(text, "authent.*?|ethical.*?|genuin.*?|real.*?|skill.*?|
                                 faithful.*?|legitimate.*?|original.*?|tradition.*?|pure.*?|historic.*?|
                                 sincere.*?|craftsmanship|honest.*?|integrity|quintessential.*?|expert.*?|
                                 iconic.*?|inspiring.*?|unique.*?|wholesom.*?|professional.*?|skillful.*?|
                                 truthful.*?|unmistakabl.*?|artisan.*?|unpretentious.*?|heartful.*?|delicious.*?|
                                 virtuous.*?|normal.*?|creativ.*?|interesting.*?|orthodox|artful.*?|.*?special.*?|
                                 .*?righteous.*?|substantial.*?|authoritativ.*?|typical.*?|awesome.*?|moral.*?|
                                 eccentric.*?|fresh.*?|old-fashioned|usual.*?|decent.*?|unusual.*?|caring.*?|
                                 ambitious.*?|replica|offbeat|atypical.*?|unassuming.*?|invented|new.*?|
                                 unconventional.*?|peculiar.*?|outlandish.*?|assumed|idiosyncratic.*?|
                                 quirky|extroverted|modern.*?|unorthodox|pretentious.*?|untraditional.*?|
                                 artificial.*?|bogus|forgery|fake.*?|hoax|cheat.*?|dishonest.*?|feigned|
                                 ersatz|faked.*?|imitat.*?|quack.*?|unreal|humbug|imposter|sham|unauthentic.*?|
                                 deceptiv.*?|inauthentic.*?|false.*?|phony.*?|phonily|scam") == T)

corpus1 <- temp_samp #create tibble, first time

corpus1 <- corpus1 %>% #interations 2-5
    bind_rows(temp_samp) %>% 
    distinct()
  
  
##FILTER for 5 or more AUTHENTICITY WORDS 
temp <- mydata

###iterate this until all reviews done
####choose with 5+ matches
temp_samp <- temp %>% 
  slice_sample(n = 200000)

temp <- temp %>% 
  anti_join(temp_samp, by = "review_id")

temp_samp <- temp_samp %>% 
  mutate(auth_count = str_count(text, "authent.*?|ethical.*?|genuin.*?|real.*?|skill.*?|faithful.*?|legitimate.*?|original.*?|
                                tradition.*?|pure.*?|historic.*?|sincere.*?|craftsmanship|honest.*?|integrity|quintessential.*?|
                                expert.*?|iconic.*?|inspiring.*?|unique.*?|wholesom.*?|professional.*?|skillful.*?|truthful.*?|
                                unmistakabl.*?|artisan.*?|unpretentious.*?|heartful.*?|delicious.*?|virtuous.*?|normal.*?|
                                creativ.*?|interesting.*?|orthodox|artful.*?|.*?special.*?|.*?righteous.*?|substantial.*?|
                                authoritativ.*?|typical.*?|awesome.*?|moral.*?|eccentric.*?|fresh.*?|old-fashioned|usual.*?|decent.*?|
                                unusual.*?|caring.*?|ambitious.*?|replica|offbeat|atypical.*?|unassuming.*?|invented|new.*?|
                                unconventional.*?|peculiar.*?|outlandish.*?|assumed|idiosyncratic.*?|quirky|extroverted|modern.*?|
                                unorthodox|pretentious.*?|untraditional.*?|artificial.*?|bogus|forgery|fake.*?|hoax|cheat.*?|
                                dishonest.*?|feigned|ersatz|faked.*?|imitat.*?|quack.*?|unreal|humbug|imposter|sham|unauthentic.*?|
                                deceptiv.*?|inauthentic.*?|false.*?|phony.*?|phonily|scam")) %>% 
  filter(auth_count >= 5)

corpus2 <- temp_samp #first time only to make tibble

corpus2 <- corpus2 %>% #all other times
  bind_rows(temp_samp) %>% 
  distinct()

##Add doc_id for each corpus
corpus1 <- corpus1 %>% 
  rownames_to_column(var = "doc_id") %>% 
  relocate(doc_id)

corpus2 <- corpus2 %>% 
  rownames_to_column(var = "doc_id") %>% 
  relocate(doc_id)

corpus1$doc_id <- as.numeric(corpus1$doc_id)
corpus2$doc_id <- as.numeric(corpus2$doc_id)
 

#time to parse
parse_me <- corpus1 #or corpus2
parse_me$text <- as.character(parse_me$text)

library(spacyr)
spacy_initialize()

parse_me.dep <- spacy_parse(parse_me$text, pos = T, tag = T, lemma = T, multithread = T)

# Remove the string"text" from doc_id column (eg, text1 -> 1)
parse_me.dep <- select(parse_me.dep, -X1)
parse_me.dep$doc_id <- str_replace(parse_me.dep$doc_id, "text", "")

# Change the data type of the doc_id column to numeric for later use
parse_me.dep$doc_id <- as.numeric(parse_me.dep$doc_id)

#save
corpus1.dep <- parse_me.dep #or corpus2

spacy_finalize()

