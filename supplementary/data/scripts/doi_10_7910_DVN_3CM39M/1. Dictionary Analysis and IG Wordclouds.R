# Load packages
if (!require("pacman")) install.packages("pacman")
p_load(vars, tseries, tsm, TSstudio, forecast, mFilter, tidyverse, readxl, writexl, cowplot, quanteda,
       janitor, readtext, irr, bit64, igraph, ggraph, ggplot2, tscount,
       beepr, pbapply, rtweet, lmtest, vrtest, pracma, lubridate, quanteda.textplots, tscount, bruceR)

# disable scientific notation of numbers
options(scipen=999)

# In this script I execute the dictionary analysis to identify the frames and compute the issue salience of SOPA, PIPA, COICA 
# and the EU copyright directive (EUCD). To protect copyright from Twitter, Newspapers, and Interest Groups websites, 
# the shared datasets do not contain text. Therefore, this script is only replicable if the datasets are scraped using the scripts
# indicated in brackets behind the datasets. I have three different datasets:
# 1. Leading Newspapers from the US and EU countries (EUCD Parse Factiva and Nexis & SOPA_Parse Factiva)
# 2. Twitter Data (all, interest groups, parliamentarians) (Twitter Academic API)
# 3. Interest Group Websites (EUCD_1. Identify IGs and scrape IG data & EUCD_2. Scrape Interest Group Websites &
# 1. SOPA_Scrape_OpenSecrets & 2. SOPA_Scrape Interest Group Websites)

######################### 1. Build all the functions ############################
#stopword removal from dictionary
# create a function that removes stopwords from dictionary and creates regular expressions from plain text
rm_stopwords_dict_sub = function(data, var, var_str, lang, stopw = T){
  
  if(stopw == T){
    stopwords = stopwords(lang)
  } else {
    stopwords = stopwords_polish
  }
  
  stopwords = paste0("\\b(", paste0(stopwords, collapse = "|"), ")\\b")
  #convert to lower
  var = tolower(na.omit(data[[var_str]]))
  #remove punctuation
  var = gsub(",", " ", var)
  
  var = gsub(stopwords, "", var, ignore.case = T)
  # remove leading/trailing white space
  var = trimws(var)
  # remove white space between words and replace with single white space
  var = gsub("\\s+"," ",var)
  
  var = paste0("(?=.*\\b(", var, ")).*")
  var = gsub(" ", "))(?=.*?\\b(", var, fixed = T)
  
  return(var)
}

# create a function that applies the `rm_stopwords_dict_sub` function to all frames included in the dictionary
rm_stopwords_dict = function(data, lang, stopw = T){
  
  censor = rm_stopwords_dict_sub(data = data, var = censor, var_str = "censor", lang = lang, stopw = stopw)
  overly_broad = rm_stopwords_dict_sub(data = data, var = overly_broad, var_str = "overly_broad", lang = lang, stopw = stopw)
  break_internet = rm_stopwords_dict_sub(data = data, var = break_internet, var_str = "break_internet", lang = lang, stopw = stopw)
  uploadfilter = rm_stopwords_dict_sub(data = data, var = uploadfilter, var_str = "uploadfilter", lang = lang, stopw = stopw)
  economy = rm_stopwords_dict_sub(data = data, var = economy, var_str = "economy", lang = lang, stopw = stopw)
  inno = rm_stopwords_dict_sub(data = data, var = innovation, var_str = "innovation", lang = lang, stopw = stopw)
  lobby = rm_stopwords_dict_sub(data = data, var = lobbying, var_str = "lobbying", lang = lang, stopw = stopw)
  astroturf = rm_stopwords_dict_sub(data = data, var = astroturf, var_str = "astroturf", lang = lang, stopw = stopw)
  diversity_freepress = rm_stopwords_dict_sub(data = data, var = diversity_freepress, var_str = "diversity_freepress", lang = lang, stopw = stopw)
  privacy = rm_stopwords_dict_sub(data = data, var = privacy, var_str = "privacy", lang = lang, stopw = stopw)
  consumers_users = rm_stopwords_dict_sub(data = data, var = consumers_users, var_str = "consumers_users", lang = lang, stopw = stopw)
  public_safety = rm_stopwords_dict_sub(data = data, var = public_safety, var_str = "public_safety", lang = lang, stopw = stopw)
  criminal = rm_stopwords_dict_sub(data = data, var = criminal, var_str = "criminal", lang = lang, stopw = stopw)
  fair_pay = rm_stopwords_dict_sub(data = data, var = fair_pay, var_str = "fair_pay", lang = lang, stopw = stopw)
  culture_creator = rm_stopwords_dict_sub(data = data, var = culture_creator, var_str = "culture_creator", lang = lang, stopw = stopw)
  bad_platforms = rm_stopwords_dict_sub(data = data, var = bad_platforms, var_str = "bad_platforms", lang = lang, stopw = stopw)
  market_power = rm_stopwords_dict_sub(data = data, var = market_power, var_str = "market_power", lang = lang, stopw = stopw)
  misinfo = rm_stopwords_dict_sub(data = data, var = misinfo, var_str = "misinfo", lang = lang, stopw = stopw)
  reach = rm_stopwords_dict_sub(data = data, var = reach, var_str = "reach", lang = lang, stopw = stopw)
  
  list = list(censor,
              overly_broad,
              break_internet,
              uploadfilter, 
              economy,            
              inno,
              lobby,
              astroturf,
              diversity_freepress, 
              privacy,
              consumers_users, 
              public_safety,
              criminal,
              fair_pay,
              culture_creator,
              bad_platforms,
              market_power,
              misinfo,
              reach)
  
  return(list)
}

# Create a function that combines two dictionaries of different languages
combine_dicts = function(dict1, dict2){
  
  list_dict = vector(mode = "list")
  
  list_dict[[1]] = unique(c(dict1[[1]], dict2[[1]]))
  list_dict[[2]] = unique(c(dict1[[2]], dict2[[2]]))
  list_dict[[3]] = unique(c(dict1[[3]], dict2[[3]]))
  list_dict[[4]] = unique(c(dict1[[4]], dict2[[4]]))
  list_dict[[5]] = unique(c(dict1[[5]], dict2[[5]]))
  list_dict[[6]] = unique(c(dict1[[6]], dict2[[6]]))
  list_dict[[7]] = unique(c(dict1[[7]], dict2[[7]]))
  list_dict[[8]] = unique(c(dict1[[8]], dict2[[8]]))
  list_dict[[9]] = unique(c(dict1[[9]], dict2[[9]]))
  list_dict[[10]] = unique(c(dict1[[10]], dict2[[10]]))
  list_dict[[11]] = unique(c(dict1[[11]], dict2[[11]]))
  list_dict[[12]] = unique(c(dict1[[12]], dict2[[12]]))
  list_dict[[13]] = unique(c(dict1[[13]], dict2[[13]]))
  list_dict[[14]] = unique(c(dict1[[14]], dict2[[14]]))
  list_dict[[15]] = unique(c(dict1[[15]], dict2[[15]]))
  list_dict[[16]] = unique(c(dict1[[16]], dict2[[16]]))
  list_dict[[17]] = unique(c(dict1[[17]], dict2[[17]]))
  list_dict[[18]] = unique(c(dict1[[18]], dict2[[18]]))
  list_dict[[19]] = unique(c(dict1[[19]], dict2[[19]]))
  
  return(list_dict)
}

# create a function that applies the dictionary to a corpus of tweets/newspaper articles
apply_dict = function(data, cnt_tit, list_dict, newsortw, lang = "english", folder = NULL, stopw = T, text_field = "paragraph", format = "xlsx"){
  
  #convert date variable to date type
  data$datetime = as.Date(data$datetime,
                          format = "%Y-%m-%d")
  
  #create a text corpus
  corp = corpus(data, text_field = text_field, docid_field = "doc_id")
  
  # create stopword list
  if(stopw == T){
    stopwords = stopwords(lang)
  } else {
    stopwords = stopwords_polish
  }
  
  #tokenize & 4grams
  toks = tokens(corp, remove_punct = TRUE, remove_numbers = TRUE, remove_symbols = TRUE)
  toks = tokens_remove(toks, stopwords)
  toks_ngram = tokens_ngrams(toks, n = 1:5, concatenator = " ")
  
  #construct dfm
  dfm = dfm(toks_ngram, tolower=TRUE, remove_padding = TRUE)
  
  #create dictionary
  dict = dictionary(list(censor = list_dict[[1]],
                         overly_broad = list_dict[[2]],
                         break_internet = list_dict[[3]],
                         uploadfilter = list_dict[[4]],
                         economy = list_dict[[5]],
                         inno = list_dict[[6]],
                         lobby = list_dict[[7]],
                         astroturf = list_dict[[8]],
                         diversity_freepress = list_dict[[9]],
                         privacy = list_dict[[10]],
                         consumers_users = list_dict[[11]],
                         public_safety = list_dict[[12]],
                         criminal = list_dict[[13]],
                         fair_pay =  list_dict[[14]],
                         culture_creator = list_dict[[15]],
                         bad_platforms = list_dict[[16]],
                         market_power = list_dict[[17]],
                         misinfo = list_dict[[18]],
                         reach = list_dict[[19]]
  ))
  
  #create a sparse dfm including only the two categories of our dictionary as features 
  dfm.dict = dfm_lookup(dfm, #give our document feature matrix as input
                        verbose=T, #the command returns messages
                        dictionary = dict, #applies our dictionary to the initial dfm
                        case_insensitive = TRUE,
                        valuetype = "regex")
  
  dfm.dict = data.frame(dfm.dict, date = docvars(corp,"datetime")) #create a dataframe including information on date
  
  #convert date variable to date type
  dfm.dict$datetime <- as.Date(dfm.dict$date,
                               format = "%Y-%m-%d")
  
  #set paragraph to one when the frame is used in the paragraph
  # Create a variable indicating whether a tweet uses a frame
  dfm.dict$censor = ifelse(dfm.dict$censor > 0, 1, 0)
  dfm.dict$overly_broad = ifelse(dfm.dict$overly_broad > 0, 1, 0)
  dfm.dict$break_internet = ifelse(dfm.dict$break_internet > 0, 1, 0)
  dfm.dict$uploadfilter = ifelse(dfm.dict$uploadfilter > 0, 1, 0)
  dfm.dict$economy = ifelse(dfm.dict$economy > 0, 1, 0)
  dfm.dict$inno = ifelse(dfm.dict$inno > 0, 1, 0)
  dfm.dict$lobby = ifelse(dfm.dict$lobby > 0, 1, 0)
  dfm.dict$astroturf = ifelse(dfm.dict$astroturf > 0, 1, 0)
  dfm.dict$diversity_freepress = ifelse(dfm.dict$diversity_freepress > 0, 1, 0)
  dfm.dict$privacy = ifelse(dfm.dict$privacy > 0, 1, 0)
  dfm.dict$consumers_users = ifelse(dfm.dict$consumers_users > 0, 1, 0)
  dfm.dict$public_safety = ifelse(dfm.dict$public_safety > 0, 1, 0)
  dfm.dict$criminal = ifelse(dfm.dict$criminal > 0, 1, 0)
  dfm.dict$fair_pay =  ifelse(dfm.dict$fair_pay > 0, 1, 0)
  dfm.dict$culture_creator = ifelse(dfm.dict$culture_creator > 0, 1, 0)
  dfm.dict$bad_platforms = ifelse(dfm.dict$bad_platforms > 0, 1, 0)
  dfm.dict$market_power = ifelse(dfm.dict$market_power > 0, 1, 0)
  dfm.dict$misinfo = ifelse(dfm.dict$misinfo > 0, 1, 0)
  dfm.dict$reach = ifelse(dfm.dict$reach > 0, 1, 0)
  
  # write dfm.dict
  # merge dfm.dict and original data
  data$datetime = as.Date(data$datetime)
  dfm.dict$doc_id = as.integer(dfm.dict$doc_id)
  dta = full_join(data, dfm.dict, by = c("datetime", "doc_id"))
  
  if(format == "xlsx"){
    write_xlsx(dta, paste0(folder, "/Frames_data_", cnt_tit, newsortw, ".xlsx"))
  } else if(format == "csv"){
    write.csv(dta, paste0(folder, "/Frames_data_", cnt_tit, newsortw, ".csv"))
  }
  
  return(dta)
}

# Create function that loops and applies dictionary to chunks of 100 instead of full data, since it takes incredibly long and 
# takes up a lot of memory to run on full dataset. This way I can easily stop and restart the function whenever I want. Also
# the dfm.lookup function performs faster on smaller datasets.

apply_dict_loop = function(data, dict, cnt_tit, newsortw = "news", folder = "EUCD/Datasets_Dictionary Applied_Chunks", start_it = 1, end_it, text_field = "paragraph", format = "xlsx"){
  
  data$lang_index = 1:nrow(data)
  
  sub_in_data = vector(mode = "list")
  sub_out_data = vector(mode = "list")
  
  for (i in start_it:end_it) {
    
    if (i == 1){
      sub_in_data[[i]] = subset(data, lang_index <= i*100)
    } else {
      sub_in_data[[i]] = subset(data, lang_index > (i-1)*100 & lang_index <= i*100)
    }
    
    sub_out_data[[i]] = apply_dict(data = sub_in_data[[i]], list_dict = dict, cnt_tit = paste0(cnt_tit, i), newsortw = newsortw, folder = folder, text_field = text_field, format = format)
    
    print(i)
  }
}

# Create a function to read-in chunks and combine to a single data frame
read_frames_dta = function(folder = "EUCD/Datasets_Dictionary Applied_Chunks_NP/", filename, newsortw = "news", filetype = ".xlsx", start_it = 1, end_it){
  
  list_news_dict = vector(mode = "list", length = end_it)
  
  for(i in start_it:end_it){
    list_news_dict[[i]] = read_xlsx(paste0(folder, filename, i, newsortw, filetype))
  }
  
  data = bind_rows(list_news_dict)
  
  return(data)
}

######################### 2. Dictionary Analysis ############################
# Note: Because of copyright restrictions text from tweets, newspapers, and IG websites cannot be shared. The application
# of the dictionary is thus not reproduceable. Code from here until line cannot be executed. 
############# a) Newspaper ############# 
###### i) SOPA ######
# load data
sopa_news_dta = read_xlsx("SOPA/SOPA_news_RAW_para.xlsx")
sopa_news_dta$index = 1:nrow(sopa_news_dta)
sopa_news_dta$doc_id = sopa_news_dta$paragraph_id

# load frames
frames_en = read_xlsx("Full_Dict_all_lang.xlsx", sheet = "English")

# clean and create dictionary
list_dict_en = rm_stopwords_dict(data = frames_en, lang = "english")

# apply the dictionary
#convert date variable to date type
sopa_news_dict = apply_dict(data = sopa_news_dta, list_dict = list_dict_en, cnt_tit = "SOPA", newsortw = "news", folder = "SOPA")

write.csv(sopa_news_dict, "SOPA/Frames_data_SOPAnews_allLang.csv")

###### ii) EUCD ######
# load data
eucd_news_dta = read_xlsx("EUCD/EU_Newspapers_RAW_paragraph.xlsx")
eucd_news_dta$index = 1:nrow(eucd_news_dta)
eucd_news_dta$doc_id = eucd_news_dta$paragraph_id

# load frames
frames_en = read_xlsx("Full_Dict_all_lang.xlsx", sheet = "English")
frames_de = read_xlsx("Full_Dict_all_lang.xlsx", sheet = "German")
frames_nl = read_xlsx("Full_Dict_all_lang.xlsx", sheet = "Dutch")
frames_pt = read_xlsx("Full_Dict_all_lang.xlsx", sheet = "Portuguese")
frames_es = read_xlsx("Full_Dict_all_lang.xlsx", sheet = "Spanish")
frames_pl = read_xlsx("Full_Dict_all_lang.xlsx", sheet = "Polish")
frames_fr = read_xlsx("Full_Dict_all_lang.xlsx", sheet = "French")
frames_it = read_xlsx("Full_Dict_all_lang.xlsx", sheet = "Italian")

# there is no default stopword list for polish, I translated the german and english stopword list to create a custom list
stopwords_polish = unique(read_xlsx("stopwords_polish.xlsx"))
stopwords_polish = as.character(stopwords_polish$stopwords)

# clean and create dictionary
list_dict_en = rm_stopwords_dict(data = frames_en, lang = "en")
list_dict_de = rm_stopwords_dict(data = frames_de, lang = "de")
list_dict_nl = rm_stopwords_dict(data = frames_nl, lang = "nl")
list_dict_pt = rm_stopwords_dict(data = frames_pt, lang = "pt")
list_dict_es = rm_stopwords_dict(data = frames_es, lang = "es")
list_dict_pl = rm_stopwords_dict(data = frames_pl, lang = "pl", stopw = F)
list_dict_fr = rm_stopwords_dict(data = frames_fr, lang = "fr")
list_dict_it = rm_stopwords_dict(data = frames_it, lang = "it")

# combine all language dictionaries into one
list_dict_al = combine_dicts(list_dict_en, list_dict_de)
list_dict_al = combine_dicts(list_dict_al, list_dict_nl)
list_dict_al = combine_dicts(list_dict_al, list_dict_pt)
list_dict_al = combine_dicts(list_dict_al, list_dict_es)
list_dict_al = combine_dicts(list_dict_al, list_dict_pl)
list_dict_al = combine_dicts(list_dict_al, list_dict_fr)
list_dict_al = combine_dicts(list_dict_al, list_dict_it)

# combine english and non-english dictionaries
list_dict_de = combine_dicts(list_dict_de, list_dict_en)
list_dict_nl = combine_dicts(list_dict_nl, list_dict_en)
list_dict_pt = combine_dicts(list_dict_pt, list_dict_en)
list_dict_es = combine_dicts(list_dict_es, list_dict_en)
list_dict_pl = combine_dicts(list_dict_pl, list_dict_en)
list_dict_fr = combine_dicts(list_dict_fr, list_dict_en)
list_dict_it = combine_dicts(list_dict_it, list_dict_en)

# create subsets of data for each language
en_news_dta = subset(eucd_news_dta, language == "Englisch")
de_news_dta = subset(eucd_news_dta, language == "Deutsch")
fr_news_dta = subset(eucd_news_dta, language == "Französisch")
es_news_dta = subset(eucd_news_dta, language == "Spanisch")
it_news_dta = subset(eucd_news_dta, language == "Italienisch")
pl_news_dta = subset(eucd_news_dta, language == "Polnisch")
pt_news_dta = subset(eucd_news_dta, language == "Portugiesisch")
nl_news_dta = subset(eucd_news_dta, language == "Niederländisch")

# Plot the frames per languages
# apply the dictionary on chunks of 100
apply_dict_loop(data = en_news_dta, dict = list_dict_en, cnt_tit = "English", folder = "EUCD/Datasets_Dictionary Applied_Chunks_NP", start_it = 1, end_it = 14)
apply_dict_loop(data = de_news_dta, dict = list_dict_de, cnt_tit = "German", folder = "EUCD/Datasets_Dictionary Applied_Chunks_NP", start_it = 1, end_it = 32)
apply_dict_loop(data = fr_news_dta, dict = list_dict_fr, cnt_tit = "French", folder = "EUCD/Datasets_Dictionary Applied_Chunks_NP", start_it = 5, end_it = 21)
apply_dict_loop(data = es_news_dta, dict = list_dict_es, cnt_tit = "Spain", folder = "EUCD/Datasets_Dictionary Applied_Chunks_NP", start_it = 1, end_it = 5)
apply_dict_loop(data = it_news_dta, dict = list_dict_it, cnt_tit = "Italian", folder = "EUCD/Datasets_Dictionary Applied_Chunks_NP", start_it = 1, end_it = 14)
apply_dict_loop(data = pl_news_dta, dict = list_dict_pl, cnt_tit = "Polish", folder = "EUCD/Datasets_Dictionary Applied_Chunks_NP", start_it = 1, end_it = 23)
apply_dict_loop(data = pt_news_dta, dict = list_dict_pt, cnt_tit = "Portuguese", folder = "EUCD/Datasets_Dictionary Applied_Chunks_NP", start_it = 1, end_it = 6)
apply_dict_loop(data = nl_news_dta, dict = list_dict_nl, cnt_tit = "Dutch", folder = "EUCD/Datasets_Dictionary Applied_Chunks_NP", start_it = 1, end_it = 5)

# read data from chunks
en_news_dict = read_frames_dta(filename = "Frames_data_English", end_it = 14)
de_news_dict = read_frames_dta(filename = "Frames_data_German", end_it = 32)
fr_news_dict = read_frames_dta(filename = "Frames_data_French", end_it = 21)
es_news_dict = read_frames_dta(filename = "Frames_data_Spain", end_it = 5)
it_news_dict = read_frames_dta(filename = "Frames_data_Italian", end_it = 14)
pl_news_dict = read_frames_dta(filename = "Frames_data_Polish", end_it = 23)
pt_news_dict = read_frames_dta(filename = "Frames_data_Portuguese", end_it = 6)
nl_news_dict = read_frames_dta(filename = "Frames_data_Dutch", end_it = 5)

# combine data from languages to EU data
eu_news_dict = rbind(en_news_dict, de_news_dict, fr_news_dict, es_news_dict, it_news_dict, pl_news_dict, pt_news_dict, nl_news_dict)

# export single dataset
write.csv(eu_news_dict, "EUCD/Frames_data_EUCDnews_allLang.csv")

############# b) All Tweets #############
##### i) SOPA #####
# load data
sopa_tweets_dta = read.csv("SOPA/SOPA_tweets_final.csv")

# only keep english language tweets and tweets were the language cannot be determined
sopa_tweets_dta = subset(sopa_tweets_dta, language == "en" | language == "und")

# add id variables
sopa_tweets_dta$index = 1:nrow(sopa_tweets_dta) # unique identifier for every tweet
# identifier for every original tweet (dropping exact copies of tweets; excluding them from dictionary analysis saves time, coding later transferred from original tweet to all duplicates. Including all duplicates gives better overview of salience)
sopa_tweets_dta_unique = distinct(sopa_tweets_dta, text, .keep_all = TRUE)
sopa_tweets_dta_unique$doc_id = 1:nrow(sopa_tweets_dta_unique)

# add matching ids for duplicates
sopa_tweets_dta$duplicates = duplicated(sopa_tweets_dta$text)

sopa_tweets_dta = sopa_tweets_dta %>% 
  group_by(text) %>%
  mutate(match_id = ifelse(duplicates, first(index), NA_character_))

sopa_tweets_dta_dup = subset(sopa_tweets_dta, duplicates == T)

# apply the dictionary on chunks of 100s
apply_dict_loop(data = sopa_tweets_dta_unique, dict = list_dict_en, cnt_tit = "SOPA",
                start_it = 1, end_it = 9911,
                newsortw = "tweets", folder = "SOPA/Datasets_Dictionary Applied_Chunks_TW", 
                text_field = "text")

# read data from chunks
setwd("SOPA/Datasets_Dictionary Applied_Chunks_TW/")

sopa_tweets_filenames = list.files(pattern = "*.xlsx")
sopa_tweets_filenames1 = sopa_tweets_filenames[1:500]
sopa_tweets_filenames2 = sopa_tweets_filenames[501:1000]
sopa_tweets_filenames3 = sopa_tweets_filenames[1001:1500]
sopa_tweets_filenames4 = sopa_tweets_filenames[1501:2000]
sopa_tweets_filenames5 = sopa_tweets_filenames[2001:2500]
sopa_tweets_filenames6 = sopa_tweets_filenames[2501:3000]
sopa_tweets_filenames7 = sopa_tweets_filenames[3001:3500]
sopa_tweets_filenames8 = sopa_tweets_filenames[3501:4000]
sopa_tweets_filenames9 = sopa_tweets_filenames[4001:4500]
sopa_tweets_filenames10 = sopa_tweets_filenames[4501:5000]
sopa_tweets_filenames11 = sopa_tweets_filenames[5001:5500]
sopa_tweets_filenames12 = sopa_tweets_filenames[5501:6000]
sopa_tweets_filenames13 = sopa_tweets_filenames[6001:6500]
sopa_tweets_filenames14 = sopa_tweets_filenames[6501:7000]
sopa_tweets_filenames15 = sopa_tweets_filenames[7001:7500]
sopa_tweets_filenames16 = sopa_tweets_filenames[7501:8000]
sopa_tweets_filenames17 = sopa_tweets_filenames[8001:8500]
sopa_tweets_filenames18 = sopa_tweets_filenames[8501:9000]
sopa_tweets_filenames19 = sopa_tweets_filenames[9001:9500]
sopa_tweets_filenames20 = sopa_tweets_filenames[9501:9911]

sopa_tweets_dict1 = pblapply(sopa_tweets_filenames1, read_xlsx)
sopa_tweets_dict2 = pblapply(sopa_tweets_filenames2, read_xlsx)
sopa_tweets_dict3 = pblapply(sopa_tweets_filenames3, read_xlsx)
sopa_tweets_dict4 = pblapply(sopa_tweets_filenames4, read_xlsx)
sopa_tweets_dict5 = pblapply(sopa_tweets_filenames5, read_xlsx)
sopa_tweets_dict6 = pblapply(sopa_tweets_filenames6, read_xlsx)
sopa_tweets_dict7 = pblapply(sopa_tweets_filenames7, read_xlsx)
sopa_tweets_dict8 = pblapply(sopa_tweets_filenames8, read_xlsx)
sopa_tweets_dict9 = pblapply(sopa_tweets_filenames9, read_xlsx)
sopa_tweets_dict10 = pblapply(sopa_tweets_filenames10, read_xlsx)
sopa_tweets_dict11 = pblapply(sopa_tweets_filenames11, read_xlsx)
sopa_tweets_dict12 = pblapply(sopa_tweets_filenames12, read_xlsx)
sopa_tweets_dict13 = pblapply(sopa_tweets_filenames13, read_xlsx)
sopa_tweets_dict14 = pblapply(sopa_tweets_filenames14, read_xlsx)
sopa_tweets_dict15 = pblapply(sopa_tweets_filenames15, read_xlsx)
sopa_tweets_dict16 = pblapply(sopa_tweets_filenames16, read_xlsx)
sopa_tweets_dict17 = pblapply(sopa_tweets_filenames17, read_xlsx)
sopa_tweets_dict18 = pblapply(sopa_tweets_filenames18, read_xlsx)
sopa_tweets_dict19 = pblapply(sopa_tweets_filenames19, read_xlsx)
sopa_tweets_dict20 = pblapply(sopa_tweets_filenames20, read_xlsx)

sopa_tweets_dict1 = bind_rows(sopa_tweets_dict1)
sopa_tweets_dict2 = bind_rows(sopa_tweets_dict2)
sopa_tweets_dict3 = bind_rows(sopa_tweets_dict3)
sopa_tweets_dict4 = bind_rows(sopa_tweets_dict4)
sopa_tweets_dict5 = bind_rows(sopa_tweets_dict5)
sopa_tweets_dict6 = bind_rows(sopa_tweets_dict6)
sopa_tweets_dict7 = bind_rows(sopa_tweets_dict7)
sopa_tweets_dict8 = bind_rows(sopa_tweets_dict8)
sopa_tweets_dict9 = bind_rows(sopa_tweets_dict9)
sopa_tweets_dict10 = bind_rows(sopa_tweets_dict10)
sopa_tweets_dict11 = bind_rows(sopa_tweets_dict11)
sopa_tweets_dict12 = bind_rows(sopa_tweets_dict12)
sopa_tweets_dict13 = bind_rows(sopa_tweets_dict13)
sopa_tweets_dict14 = bind_rows(sopa_tweets_dict14)
sopa_tweets_dict15 = bind_rows(sopa_tweets_dict15)
sopa_tweets_dict16 = bind_rows(sopa_tweets_dict16)
sopa_tweets_dict17 = bind_rows(sopa_tweets_dict17)
sopa_tweets_dict18 = bind_rows(sopa_tweets_dict18)
sopa_tweets_dict19 = bind_rows(sopa_tweets_dict19)
sopa_tweets_dict20 = bind_rows(sopa_tweets_dict20)

sopa_tweets_dict = rbind(sopa_tweets_dict1, sopa_tweets_dict2, sopa_tweets_dict3, sopa_tweets_dict4, sopa_tweets_dict5,
                         sopa_tweets_dict6, sopa_tweets_dict7, sopa_tweets_dict8, sopa_tweets_dict9, sopa_tweets_dict10,
                         sopa_tweets_dict11, sopa_tweets_dict12, sopa_tweets_dict13, sopa_tweets_dict14, sopa_tweets_dict15,
                         sopa_tweets_dict16, sopa_tweets_dict17, sopa_tweets_dict18, sopa_tweets_dict19, sopa_tweets_dict20)

setwd("Z:/Mitarbeiter/Michael Kemmerling/Dissertation/Papers/2021_10_21_Sicherung_Copyright Paper/Final Data/")

# Merge the frames data with the duplicate tweets that I did exclude to save computing power
# drop a bunch of variables and then add it to the data
sopa_tweets_dict_dup = select(sopa_tweets_dict, c(index, censor, overly_broad, break_internet, uploadfilter, 
                                                  economy, inno, lobby, astroturf, diversity_freepress, privacy,
                                                  consumers_users, public_safety, criminal, fair_pay, 
                                                  culture_creator, bad_platforms, market_power,	misinfo, reach))

sopa_tweets_dta_dup$match_id = as.numeric(sopa_tweets_dta_dup$match_id)

sopa_tweets_dta_dup = inner_join(sopa_tweets_dta_dup, sopa_tweets_dict_dup, by = c("match_id" = "index"))

sopa_tweets_dict$duplicates = NA
sopa_tweets_dict$match_id = NA
sopa_tweets_dta_dup$X.1 = NA
sopa_tweets_dta_dup$paragraph_id = NA
sopa_tweets_dta_dup$doc_id = NA
sopa_tweets_dta_dup$lang_index = NA
sopa_tweets_dta_dup$uniquetext_id = NA
sopa_tweets_dta_dup$date = sopa_tweets_dta_dup$datetime

sopa_tweets_dict = rbind(sopa_tweets_dict, sopa_tweets_dta_dup)

# write df
write.csv(sopa_tweets_dict, "SOPA/AA_SOPA_Tweets_all_frames_Incl_duplicates.csv")

##### ii) EUCD #####
# load data
eucd_tweets_dta = read.csv("EUCD/EU_tweets_final.csv")
eucd_tweets_dta$index = 1:nrow(eucd_tweets_dta)

# identifier for every original tweet (dropping exact copies of tweets; excluding them from dictionary analysis saves time, coding later transferred from original tweet to all duplicates. Including all duplicates gives better overview of salience)
eucd_tweets_dta_unique = distinct(eucd_tweets_dta, text, .keep_all = TRUE)
eucd_tweets_dta_unique$doc_id = 1:nrow(eucd_tweets_dta_unique)

# add matching ids for duplicates
eucd_tweets_dta$duplicates = duplicated(eucd_tweets_dta$text)

eucd_tweets_dta = eucd_tweets_dta %>% 
  group_by(text) %>%
  mutate(match_id = ifelse(duplicates, first(index), NA_character_))

eucd_tweets_dta_dup = subset(eucd_tweets_dta, duplicates == T)

# create subsets of data for each language
en_tweets_dta = subset(eucd_tweets_dta_unique, language == "en")
de_tweets_dta = subset(eucd_tweets_dta_unique, language == "de")
fr_tweets_dta = subset(eucd_tweets_dta_unique, language == "fr")
es_tweets_dta = subset(eucd_tweets_dta_unique, language == "es")
it_tweets_dta = subset(eucd_tweets_dta_unique, language == "it")
pl_tweets_dta = subset(eucd_tweets_dta_unique, language == "pl")
pt_tweets_dta = subset(eucd_tweets_dta_unique, language == "pt")
nl_tweets_dta = subset(eucd_tweets_dta_unique, language == "nl")
un_tweets_dta = subset(eucd_tweets_dta_unique, language == "und")

# Plot the frames per languages
# apply the dictionary on chunks of 100
apply_dict_loop(data = en_tweets_dta, dict = list_dict_en, cnt_tit = "EUCD_English",
                start_it = 1, end_it = 4288,
                newsortw = "tweets", folder = "EUCD/Datasets_Dictionary Applied_Chunks_TW",
                text_field = "text", format = "csv")

apply_dict_loop(data = de_tweets_dta, dict = list_dict_de, cnt_tit = "EUCD_German",
                start_it = 3817, end_it = 3839,
                newsortw = "tweets", folder = "EUCD/Datasets_Dictionary Applied_Chunks_TW",
                text_field = "text", format = "csv")

apply_dict_loop(data = fr_tweets_dta, dict = list_dict_fr, cnt_tit = "EUCD_French",
                start_it = 1, end_it = 470,
                newsortw = "tweets", folder = "EUCD/Datasets_Dictionary Applied_Chunks_TW",
                text_field = "text", format = "csv")

apply_dict_loop(data = es_tweets_dta, dict = list_dict_es, cnt_tit = "EUCD_Spain",
                start_it = 150, end_it = 699,
                newsortw = "tweets", folder = "EUCD/Datasets_Dictionary Applied_Chunks_TW",
                text_field = "text", format = "csv")

apply_dict_loop(data = it_tweets_dta, dict = list_dict_it, cnt_tit = "EUCD_Italian",
                start_it = 1, end_it = 326,
                newsortw = "tweets", folder = "EUCD/Datasets_Dictionary Applied_Chunks_TW",
                text_field = "text", format = "csv")

apply_dict_loop(data = pl_tweets_dta, dict = list_dict_pl, cnt_tit = "EUCD_Polish",
                start_it = 109, end_it = 236,
                newsortw = "tweets", folder = "EUCD/Datasets_Dictionary Applied_Chunks_TW",
                text_field = "text", format = "csv")

apply_dict_loop(data = pt_tweets_dta, dict = list_dict_pt, cnt_tit = "EUCD_Portuguese",
                start_it = 1, end_it = 113,
                newsortw = "tweets", folder = "EUCD/Datasets_Dictionary Applied_Chunks_TW",
                text_field = "text", format = "csv")

apply_dict_loop(data = nl_tweets_dta, dict = list_dict_nl, cnt_tit = "EUCD_Dutch",
                start_it = 65, end_it = 87,
                newsortw = "tweets", folder = "EUCD/Datasets_Dictionary Applied_Chunks_TW",
                text_field = "text", format = "csv")

apply_dict_loop(data = un_tweets_dta, dict = list_dict_al, cnt_tit = "EUCD_Undetermined",
                start_it = 237, end_it = 627,
                newsortw = "tweets", folder = "EUCD/Datasets_Dictionary Applied_Chunks_TW",
                text_field = "text", format = "csv")

# read data from chunks
setwd("EUCD/Datasets_Dictionary Applied_Chunks_TW/")

# get list of filenames
eucd_tweets_filenames = list.files(pattern = "*.csv")

# feed list of filenames to read.csv function and put dfs from chunks into a list
eucd_tweets_dict = pblapply(eucd_tweets_filenames, read.csv)

# change format from a few variables to match between dfs
for (i in 1:10188){
  eucd_tweets_dict[i][[1]]$referenced_author_id = as.character(eucd_tweets_dict[i][[1]]$referenced_author_id)
  eucd_tweets_dict[i][[1]]$referenced_likes = as.character(eucd_tweets_dict[i][[1]]$referenced_likes)
  eucd_tweets_dict[i][[1]]$referenced_retweets = as.character(eucd_tweets_dict[i][[1]]$referenced_retweets)
  eucd_tweets_dict[i][[1]]$referenced_quotes = as.character(eucd_tweets_dict[i][[1]]$referenced_quotes)
  eucd_tweets_dict[i][[1]]$referenced_replies = as.character(eucd_tweets_dict[i][[1]]$referenced_replies)
  eucd_tweets_dict[i][[1]]$qt_username1 = as.character(eucd_tweets_dict[i][[1]]$qt_username1)
  eucd_tweets_dict[i][[1]]$rp_username13 = as.character(eucd_tweets_dict[i][[1]]$rp_username13)
  eucd_tweets_dict[i][[1]]$rp_username5 = as.character(eucd_tweets_dict[i][[1]]$rp_username5)
  eucd_tweets_dict[i][[1]]$rp_username2 = as.character(eucd_tweets_dict[i][[1]]$rp_username2)
  eucd_tweets_dict[i][[1]]$X = as.integer(eucd_tweets_dict[i][[1]]$X)
}

# convert list of dfs into a single df
eucd_tweets_dict = bind_rows(eucd_tweets_dict)

setwd("Z:/Mitarbeiter/Michael Kemmerling/Dissertation/Papers/2021_10_21_Sicherung_Copyright Paper/Final Data/")

# Merge the frames data with the duplicate tweets that I did exclude to save computing power
# drop a bunch of variables and then add it to the data
eucd_tweets_dict_dup = select(eucd_tweets_dict, c(index, censor, overly_broad, break_internet, uploadfilter, 
                                                  economy, inno, lobby, astroturf, diversity_freepress, privacy,
                                                  consumers_users, public_safety, criminal, fair_pay, 
                                                  culture_creator, bad_platforms, market_power,	misinfo, reach))

eucd_tweets_dta_dup$match_id = as.numeric(eucd_tweets_dta_dup$match_id)

eucd_tweets_dta_dup = inner_join(eucd_tweets_dta_dup, eucd_tweets_dict_dup, by = c("match_id" = "index"))

eucd_tweets_dict$duplicates = NA
eucd_tweets_dict$match_id = NA
eucd_tweets_dta_dup$X.1 = NA
eucd_tweets_dta_dup$doc_id = NA
eucd_tweets_dta_dup$lang_index = NA
eucd_tweets_dta_dup$date = eucd_tweets_dta_dup$datetime

eucd_tweets_dict = rbind(eucd_tweets_dict, eucd_tweets_dta_dup)

# Draw the plots
# create and convert variables
eucd_tweets_dict$article_id = eucd_tweets_dict$index1
eucd_tweets_dict$datetime = as.Date(eucd_tweets_dict$datetime)
eucd_tweets_dict$paragraph_id = eucd_tweets_dict$index1

# write df
write.csv(eucd_tweets_dict, "EUCD/AA_EUCD_Tweets_all_frames_Incl_duplicates.csv")

############# c) Interest Groups #############
######## i) SOPA #######
##### I) Twitter #####
# Get the user ids of interest groups
# In the `identify IGs and get IG info from lobbyfacts` script I collected information on Interest Groups involved in EU copyright policy,
# including their twitter handles. Using the twitter api I search their user ids based on these handles.

# authenticate with twitter API (details have been left blank to protect security of my API)
twitter_token = create_token(app = "",
                             consumer_key = '',
                             consumer_secret = '',
                             access_token = '',
                             access_secret = ''
)

# import data on interest groups
SOPA_IG_info = read_xlsx("SOPA/OpenSecrets_IGs_lobbying_on_Copyright.xlsx")

# select only usernames variable
SOPA_IG_usernames = c(na.omit(SOPA_IG_info$twitter_username1), na.omit(SOPA_IG_info$twitter_username2), na.omit(SOPA_IG_info$twitter_username3), na.omit(SOPA_IG_info$twitter_username4))

# delete unneccessary whitespace
SOPA_IG_usernames = gsub(" ", "", SOPA_IG_usernames, fixed = TRUE)

# use the rtweet's lookup_users function to get the user ids and screennames
SOPA_IG_users = lookup_users(SOPA_IG_usernames)
SOPA_IG_users = select(SOPA_IG_users, c(user_id, screen_name))

# export IG twitter IDs
write.csv(SOPA_IG_users, "Interest Group Twitter IDs.csv")
# import IG twitter IDs
SOPA_IG_users = read.csv("SOPA/Interest Groups/Interest Group Twitter IDs.csv") 

# Merge twitter ids and splitted usernames to ig_info dataframe
merge_tw_username = function(num, IG_info, IG_users) {
  tw_username = paste0("tw_username", num)
  tw_id = paste0("tw_id", num)
  
  IG_info[[tw_id]] = NA
  
  for (i in 1:nrow(IG_info)) {
    for (j in 1:nrow(IG_users)) {
      if (IG_info[[tw_username]][[i]] %in% IG_users$screen_name[[j]]) {
        IG_info[[tw_id]][[i]] = IG_users$user_id[[j]]
      }
    }
  }
  return(IG_info)
}

SOPA_IG_info = merge_tw_username(1, IG_info = SOPA_IG_info, IG_users = SOPA_IG_users)
SOPA_IG_info = merge_tw_username(2, IG_info = SOPA_IG_info, IG_users = SOPA_IG_users)
SOPA_IG_info = merge_tw_username(3, IG_info = SOPA_IG_info, IG_users = SOPA_IG_users)
SOPA_IG_info = merge_tw_username(4, IG_info = SOPA_IG_info, IG_users = SOPA_IG_users)

write_xlsx(SOPA_IG_info, "SOPA/Interest Groups/OpenSecrets_IGs_lobbying_on_Copyright.xlsx")
#SOPA_IG_info = read_xlsx("SOPA/Interest Groups/OpenSecrets_IGs_lobbying_on_Copyright.xlsx")
# keep only those tweets by interest group accounts
SOPA_IG_tweets = subset(sopa_tweets_dict, ((author_id %in% SOPA_IG_users$user_id) | (author_username %in% SOPA_IG_users$screen_name)))
write.csv(SOPA_IG_tweets, "SOPA/Interest Groups/SOPA_Interest Groups_Tweets_frames.csv")

##### II) Websites #####
sopa_ig_web = read.csv("SOPA/Interest Groups/Interest Groups_Websites and Memo_Paragraphs.csv")
# change to date format, create an index variable
sopa_ig_web$datetime = as.Date(sopa_ig_web$date)
sopa_ig_web$document_id = sopa_ig_web$doc_id
sopa_ig_web$doc_id = 1:nrow(sopa_ig_web)

# remove leading/trailing white space
sopa_ig_web$para_text_web = trimws(sopa_ig_web$para_text_web)
sopa_ig_web$text_web = trimws(sopa_ig_web$text_web)
# remove white space between words and replace with single white space
sopa_ig_web$para_text_web = gsub("\\s+"," ", sopa_ig_web$para_text_web)
sopa_ig_web$text_web = gsub("\\s+"," ", sopa_ig_web$text_web)

sopa_ig_web$para_text_web = gsub('[[:punct:] ]+',' ',sopa_ig_web$para_text_web)
sopa_ig_web$text_web = gsub('[[:punct:] ]+',' ',sopa_ig_web$text_web)
sopa_ig_web$snippet = gsub('[[:punct:] ]+',' ',sopa_ig_web$snippet)

# apply the dictionary on chunks of 100s
apply_dict_loop(data = sopa_ig_web, dict = list_dict_en, cnt_tit = "SOPA",
                start_it = 1, end_it = 105, newsortw = "IG_web", 
                folder = "SOPA/Interest Groups/Datasets_Dictionary Applied_Chunks_IG_TW", 
                text_field = "para_text_web", format = "csv")

## read data from chunks
setwd("SOPA/Interest Groups/Datasets_Dictionary Applied_Chunks_IG_TW/")

# get list of filenames
sopa_IG_web_filenames = list.files(pattern = "*.csv")

# feed list of filenames to read.csv function and put dfs from chunks into a list
sopa_IG_web_dict = pblapply(sopa_IG_web_filenames, read.csv)

for (i in 1:105){
  sopa_IG_web_dict[i][[1]]$date.x = as.character(sopa_IG_web_dict[i][[1]]$date.x)
}

# convert list of dfs into a single df
sopa_IG_web_dict = bind_rows(sopa_IG_web_dict)

setwd("Z:/Mitarbeiter/Michael Kemmerling/Dissertation/Papers/2021_10_21_Sicherung_Copyright Paper/Final Data/")

# merge Frames and Interest Group Info data
sopa_IG_web_dict$Name = tolower(sopa_IG_web_dict$Name)
SOPA_IG_info$Name = tolower(SOPA_IG_info$Name)

sopa_IG_web_dict = full_join(sopa_IG_web_dict, SOPA_IG_info, by = "Name")

# export
write.csv(sopa_IG_web_dict, "SOPA/Interest Groups/SOPA_Interest Groups_Websites and Memo_Frames.csv")
sopa_IG_web_dict = read.csv("SOPA/Interest Groups/SOPA_Interest Groups_Websites and Memo_Frames.csv")

######## ii) EUCD ########
##### I) Twitter #####
# Get the user ids of interest groups
# In the `identify IGs and get IG info from lobbyfacts` script I collected information on Interest Groups involved in EU copyright policy,
# including their twitter handles. Using the twitter api I search their user ids based on these handles.

# import data on interest groups
EUCD_IG_info = read_xlsx("EUCD/Interest Groups/Interest Group Scraping/EU Copyright_IGs_Klassifizierung.xlsx")

# select only usernames variable and cases that are not NA
EUCD_IG_usernames = select(EUCD_IG_info, twitter_short)
EUCD_IG_usernames = EUCD_IG_usernames[complete.cases(EUCD_IG_usernames), ]

# split unsernames variable into separate rows for each username
EUCD_IG_usernames = separate_rows(EUCD_IG_usernames, twitter_short, sep=",")

# define as a vector
EUCD_IG_usernames = as.vector(EUCD_IG_usernames$twitter_short)

# delete unneccessary whitespace
EUCD_IG_usernames = gsub(" ", "", EUCD_IG_usernames, fixed = TRUE)

# use the rtweet's lookup_users function to get the user ids and screennames
EUCD_IG_users = lookup_users(EUCD_IG_usernames)
EUCD_IG_users = select(EUCD_IG_users, c(user_id, screen_name))

# export IG twitter IDs
write.csv(EUCD_IG_users, "Interest Group Twitter IDs.csv")

# Merge twitter ids and splitted usernames to ig_info dataframe
EUCD_IG_info$twitter_short = gsub(" ", "", EUCD_IG_info$twitter_short, fixed = TRUE)

EUCD_IG_info = separate(EUCD_IG_info, 
                        twitter_short,
                        sep = ",",
                        into = c("tw_username1", "tw_username2", "tw_username3", "tw_username4", "tw_username5", "tw_username6", "tw_username7", "tw_username8", "tw_username9",
                                 "tw_username10", "tw_username11", "tw_username12", "tw_username13"),
                        fill = "warn",
                        extra = "warn")

EUCD_IG_info = merge_tw_username(1, IG_info = EUCD_IG_info, IG_users = EUCD_IG_users)
EUCD_IG_info = merge_tw_username(2, IG_info = EUCD_IG_info, IG_users = EUCD_IG_users)
EUCD_IG_info = merge_tw_username(3, IG_info = EUCD_IG_info, IG_users = EUCD_IG_users)
EUCD_IG_info = merge_tw_username(4, IG_info = EUCD_IG_info, IG_users = EUCD_IG_users)
EUCD_IG_info = merge_tw_username(5, IG_info = EUCD_IG_info, IG_users = EUCD_IG_users)
EUCD_IG_info = merge_tw_username(6, IG_info = EUCD_IG_info, IG_users = EUCD_IG_users)
EUCD_IG_info = merge_tw_username(7, IG_info = EUCD_IG_info, IG_users = EUCD_IG_users)
EUCD_IG_info = merge_tw_username(8, IG_info = EUCD_IG_info, IG_users = EUCD_IG_users)
EUCD_IG_info = merge_tw_username(9, IG_info = EUCD_IG_info, IG_users = EUCD_IG_users)
EUCD_IG_info = merge_tw_username(10, IG_info = EUCD_IG_info, IG_users = EUCD_IG_users)
EUCD_IG_info = merge_tw_username(11, IG_info = EUCD_IG_info, IG_users = EUCD_IG_users)
EUCD_IG_info = merge_tw_username(12, IG_info = EUCD_IG_info, IG_users = EUCD_IG_users)
EUCD_IG_info = merge_tw_username(13, IG_info = EUCD_IG_info, IG_users = EUCD_IG_users)

# export import
write_xlsx(EUCD_IG_info, "EUCD/Interest Groups/EU Copyright_IGs_Klassifizierung.xlsx")
#EUCD_IG_info = read_xlsx("EUCD/Interest Groups/EU Copyright_IGs_Klassifizierung.xlsx")

# load the list of Interest Groups Twitter ids
EUCD_IG_ids = read.csv("EUCD/Interest Groups/Interest Group Twitter IDs.csv")

# load EUCD tweets (after dictionary analysis)
#eucd_tweets_dict = read.csv("EUCD/AA_EUCD_Tweets_all_frames_Incl_duplicates.csv")

# keep only those tweets by interest group accounts
EUCD_IG_tweets = subset(eucd_tweets_dict, author_id %in% EUCD_IG_ids$user_id)

# Draw the plots
EUCD_IG_tweets$article_id = EUCD_IG_tweets$index1
EUCD_IG_tweets$datetime = as.Date(EUCD_IG_tweets$datetime)

write.csv(EUCD_IG_tweets, "EUCD/EUCD_Interest Group Twitter_frames.csv")

##### II) Websites #####
eucd_ig_web = read.csv("EUCD/Interest Groups/EUCD_IG Websites from Google Search and Manual Search.csv")

eucd_ig_web_1 = subset(eucd_ig_web, !is.na(eucd_ig_web$keyword))
eucd_ig_web_2 = subset(eucd_ig_web, is.na(eucd_ig_web$keyword))

eucd_ig_web_2$doc_id = eucd_ig_web_2$doc_id + max(eucd_ig_web_1$doc_id)

# change to date format, create an index variable
eucd_ig_web$datetime = as.Date(eucd_ig_web$date)
eucd_ig_web$document_id = eucd_ig_web$doc_id
eucd_ig_web$doc_id = 1:nrow(eucd_ig_web)

# remove leading/trailing white space
eucd_ig_web$para_text_web = trimws(eucd_ig_web$para_text_web)
eucd_ig_web$text_web = trimws(eucd_ig_web$text_web)
# remove white space between words and replace with single white space
eucd_ig_web$para_text_web = gsub("\\s+"," ", eucd_ig_web$para_text_web)
eucd_ig_web$text_web = gsub("\\s+"," ", eucd_ig_web$text_web)
# remove punctuation
eucd_ig_web$para_text_web = gsub('[[:punct:] ]+',' ',eucd_ig_web$para_text_web)
eucd_ig_web$text_web = gsub('[[:punct:] ]+',' ',eucd_ig_web$text_web)
eucd_ig_web$snippet = gsub('[[:punct:] ]+',' ',eucd_ig_web$snippet)

# create subdatasets by language
# I forgot to add the language to the interest group website dataset. It can however 
# be inferred from the search term
eucd_ig_web$search_term = "NA"

for (i in 1:nrow(eucd_ig_web)){
  eucd_ig_web$search_term[i] = strsplit(eucd_ig_web$keyword, "site")[[i]][1]
  print(i)
}

english_search = eucd_ig_web$search_term[1]
german_search = eucd_ig_web$search_term[114]
swedish_search = eucd_ig_web$search_term[1499]
dutch_search = eucd_ig_web$search_term[7933]

eucd_ig_web$language = ifelse(eucd_ig_web$search_term == english_search, "en", "NA")
eucd_ig_web$language = ifelse(eucd_ig_web$search_term == german_search, "de", eucd_ig_web$language)
eucd_ig_web$language = ifelse(eucd_ig_web$search_term == swedish_search, "sw", eucd_ig_web$language)
eucd_ig_web$language = ifelse(eucd_ig_web$search_term == dutch_search, "nl", eucd_ig_web$language)

# for the manual search the keyword is NA, thus language has to be determined manually
eucd_ig_web$language = ifelse(is.na(eucd_ig_web$keyword), "en", eucd_ig_web$language)
eucd_ig_web$language = ifelse(eucd_ig_web$doc_id == 8398 | eucd_ig_web$doc_id == 8421 | eucd_ig_web$doc_id == 8422 
                              | eucd_ig_web$doc_id == 8423 | eucd_ig_web$doc_id == 8424 | eucd_ig_web$doc_id == 8518,
                              "de", eucd_ig_web$language)

eucd_ig_web = subset(eucd_ig_web, is.na(keyword))

# create subsets (swedish is not in the tweet or newspaper sample and will therefore be excluded)
en_ig_web = subset(eucd_ig_web, language == "en")
de_ig_web = subset(eucd_ig_web, language == "de")
nl_ig_web = subset(eucd_ig_web, language == "nl")

# apply the dictionary on chunks of 100s
apply_dict_loop(data = nl_ig_web, dict = list_dict_nl, cnt_tit = "NL",
                start_it = 1, end_it = 1, newsortw = "IG_web", 
                folder = "EUCD/Interest Groups/Datasets_Dictionary Applied_Chunks_IG_TW", 
                text_field = "para_text_web", format = "csv")

apply_dict_loop(data = en_ig_web, dict = list_dict_en, cnt_tit = "EN",
                start_it = 1, end_it = 85, newsortw = "IG_web", 
                folder = "EUCD/Interest Groups/Zusatz", 
                text_field = "para_text_web", format = "csv")

apply_dict_loop(data = de_ig_web, dict = list_dict_de, cnt_tit = "DE",
                start_it = 1, end_it = 1, newsortw = "IG_web", 
                folder = "EUCD/Interest Groups/Zusatz", 
                text_field = "para_text_web", format = "csv")

## read data from chunks
setwd("EUCD/Interest Groups/Zusatz/")

# get list of filenames
eucd_IG_web_filenames = list.files(pattern = "*.csv")

# feed list of filenames to read.csv function and put dfs from chunks into a list
eucd_IG_web_dict = pblapply(eucd_IG_web_filenames, read.csv)

# convert list of dfs into a single df
eucd_IG_web_dict = bind_rows(eucd_IG_web_dict)

setwd("Z:/Mitarbeiter/Michael Kemmerling/Dissertation/Papers/2021_10_21_Sicherung_Copyright Paper/Final Data/")

# merge Frames and Interest Group Info data
eucd_IG_web_dict = full_join(eucd_IG_web_dict, EUCD_IG_info, by = "regid")

# export
write.csv(eucd_IG_web_dict, "EUCD/Interest Groups/EUCD_Interest Groups_Google & Manual_Frames.csv")
eucd_IG_web_dict = read.csv("EUCD/Interest Groups/EUCD_Interest Groups_Google & Manual_Frames.csv")

############# d) Parliamentarians #############
###### i) MEPs ######
##### I) Twitter #####
# read excel file with MEPs twitter ids
meps_ids = read_xlsx("EUCD/Twitter_Party_MEPs.xlsx", sheet = "Tabelle1")

meps_ids$user_id = as.numeric(meps_ids$user_id)

# subset eucd_tweets to only tweets by MEPs
eucd_tweets_meps = subset(eucd_tweets_dict, author_id %in% meps_ids$user_id)

# merge meps_tweets and meps_ids
eucd_tweets_meps = left_join(eucd_tweets_meps, meps_ids, by = c("author_id" = "user_id"))

write.csv(eucd_tweets_meps, "EUCD/EUCD_MEPs Tweets Frames.csv")

###### ii) US Congresspeople ######
## Twitter
# members of congress official twitter handles have to start with rep or sen followed by their name

## reps
sopa_tweets_dict$author_username_3 = tolower(substr(sopa_tweets_dict$author_username, 1, 3))

reps_twitter = subset(sopa_tweets_dict, author_username_3 == "rep")

#
exclude = c("rep_09", "REP_PR", "Rep23", "Repair_Labs", "RepairEm", "repairmanxjack", "repairmenow", "RepairParts",
            "repalec", "repampanos", "ReparoLaptops", "repat123", "repcax", "RepChiTown2", "RepDefender", "repeace",
            "RePeAteD", "repeated_poser", "repeatingbeats", "Repede91", "repejota", "repellr", "Repent2335",
            "repercussionist", "repetenz", "rePetePro", "repgawd", "Rephiscorth", "rephXking", "repi", "Repidadk",
            "Repie89", "repinemon", "replacebswithds", "ReplanningVe", "replarc", "Repli_cated", "replicamask",
            "replicantN7MAA", "replikhante", "Reploiid", "replore", "replykushagra", "ReporReportCO", "REPORT__",
            "ReplyMag", "RepMyCityX", "RepofAmerica", "RepOfTexTV", "Repojay", "repomom1", "Reporaf", "RepoReaper",
            "reportbywilson", "Reporte_Qro", "REPORTEDEESO", "ReportedTeacup", "Reporter_Barbie", "ReporterAaron",
            "ReporterAmber", "reporterarm", "ReporterBartonD", "ReporterClaudia", "ReporterGarriss", "reporterjulie",
            "ReporterMathew", "ReporterNatasha", "reporteroscol", "ReporterPhoenix", "ReporterRicky", "ReporterWorld",
            "ReportesAnonVE", "Reporti", "reportingsjr", "ReportingTexas", "ReportrTeam", "reportster", "reporttohmrccam",
            "reposed", "reposemassage", "reposeme", "RepowerRenew", "reppep", "reppesgaard", "reprapr", "repreport",
            "reppewelander", "REPPINjt", "ReppinMyBird", "Repptar75", "Reppundit", "reprahekim", "RepressedNews",
            "ReprintWeg", "reprisevintage", "reprizal", "reprocesszor", "reprojustice", "repstos", "repsupsean", 
            "Reptakon", "REPTAR_PORN", "ReptarAzar", "reptardeathl0ck", "ReptarRoe_RAWR", "Reptile_House", "Reptile_Xav", 
            "reptileKoopr", "reptilescorpio", "Reptilianbard", "reptiliangad", "Reptililia", "repub_voter", "repub9989",
            "Republic1st", "republica_fit", "RepublicaHavas", "RepublicanCarol", "RepublicanDalek", "republicanes",
            "Republicans_HQ", "RepublicansHQ", "RepublicanWatch", "RepublicanWiki", "RepubliClowns", "RepubliGAL", 
            "Republiken", "republikmgmt", "repubofcat", "RepubsRScum", "RepucationCo", "repugnantpilot", "RepulicofIndia",
            "RepulsiveCoder", "repustate", "Reputation_Com", "ReputationBuff", "ReputationHELP", "RepuTrack", "repvik", 
            "repvoice", "RepYourCity")

reps_twitter = subset(reps_twitter, !(author_username %in% exclude))

## senators
sopa_tweets_dict$author_username_3 = tolower(substr(sopa_tweets_dict$author_username, 1, 3))
sen_twitter = subset(sopa_tweets_dict, author_username_3 == "sen")

table(sen_twitter$author_username)

##
include = c("SenateCommerce", "SenateGOP", "SenateJudiciary", "SenatorAkaka", "SenatorBennet",
            "SenatorCardin", "SenatorCollins", "SenatorKirk", "SenatorTimScott", "SenatorTomUdall",
            "SenatorLankford", "SenatorLeahy", "SenatorMenendez", "SenatorReid", "SenatorShaheen",
            "SenBlumenthal", "SenGillibrand", "SenJeffMerkley", "SenLeePressSec", "SenLeeResearch",
            "SenMcCarthy", "SenMikeLee", "SenNewell", "SenSanders", "SenSchumer", "SenScottBrown",
            "SenToomey")

sen_twitter = subset(sen_twitter, (author_username %in% include))

congress_twitter = rbind(sen_twitter, reps_twitter)

congress_twitter$article_id = 1:nrow(congress_twitter)
congress_twitter$paragraph_id = 1:nrow(congress_twitter)

congress_twitter$datetime = as.Date(congress_twitter$datetime, format = "%Y-%m-%d")

write.csv(congress_twitter, "SOPA/SOPA_Congress_Twitter.csv")

######################### 3. Interest Groups Descriptive Analysis ######################### 
wordcloud = function(data, text_field, lang, stopw = T, add_stopw, subtype_list, max_words = 150){
  
  data = subset(data, Subtype %in% subtype_list)
  
  data$doc_id_intern = 1:nrow(data)
  
  #create a text corpus
  corp = corpus(data, text_field = text_field, docid_field = "doc_id_intern")
  
  # create stopword list
  if(stopw == T){
    stopwords = stopwords(lang)
  } else {
    stopwords = stopwords_polish
  }
  
  stopwords = c(stopwords, add_stopw)
  
  #tokenize & 4grams
  toks = tokens(corp, remove_punct = TRUE, remove_numbers = TRUE, remove_symbols = TRUE)
  toks = tokens_remove(toks, stopwords)
  toks_ngram = tokens_ngrams(toks, n = 1:2, concatenator = "_")
  
  #construct dfm
  dfm = dfm(toks_ngram, tolower=TRUE, remove_padding = TRUE)
  
  cloud_plot = textplot_wordcloud(dfm, max_words = max_words, color = "black",)
  
  return(cloud_plot)
}

############# a) SOPA ############# 
subtypes_rightsholder = c("Telecom", "Telecom & Media", "Education", "Manufacturer", 
                          "Manufacturers", "ICT & Consulting", "Electronics & ICT", 
                          "Manufacturing", "Rightsholder")

subtypes_plat = c("Platform", "Platforms and User Rights")

subtypes_civ_soc = c("User/Consumer Rights", "Library", 
                     "Open Source", "Open Software", 
                     "Cybersecurity", "Human Rights")

subtypes_plat_rh_mix = c("Domains", "Platform & ICT")

subtypes_creatives = c("Creatives")

subtypes_lobbyists = c("Law Firm", "Lobbying Agency")

subtypes_others = c("Other", "Seniors Advocacy", "Trade", "Trade Union", "Transport",
                    "Political Party", "Tax", "Security", "Real Estate", "Government",
                    "Political Action Committee", "Plumbing", "Police", "Firefighter", "Energy",
                    "Gambling", "Environment", "Finance",
                    "Hospitality", "Individual", "Individual Rights", "Science", "Marketing", "Retail Sales",
                    "Crypto", "Construction", "Chamber of Commerce")

##### i) Wordcloud #####
## tweets
sopa_tweets_wc = SOPA_IG_info %>%
  gather(key="name", value = "author_id", tw_id1:tw_id4)

sopa_tweets_wc = subset(sopa_tweets_wc, !is.na(author_id))

sopa_tweets_wc = full_join(sopa_tweets_wc, sopa_IG_tweets, by = "author_id")

sopa_tweets_wc$link = sopa_tweets_wc$link_to_tweet
sopa_tweets_wc$source = "tweet"

sopa_tweets_wc = select(sopa_tweets_wc, c(Name, text, link, source, Subtype, regid, Support, Oppose, censor:reach, datetime))

## website
sopa_web_wc = select(SOPA_IG_info, c(Name, Subtype))

sopa_web_wc$Name = tolower(sopa_web_wc$Name)

sopa_web_wc = full_join(sopa_web_wc, sopa_IG_web_dict, by = "Name") 

sopa_web_wc$text = sopa_web_wc$para_text_web

sopa_web_wc$link = sopa_web_wc$source
sopa_web_wc$source = "website"

sopa_web_wc = select(sopa_web_wc, c(Name, text, link, source, Subtype, regid, Support, Oppose, censor:reach, datetime))

## bind IG web and tweets data together
sopa_wc = rbind(sopa_web_wc, sopa_tweets_wc)

sopa_wc

# save subtype datasets
sopa_IG_rh_cr = subset(sopa_wc, Subtype %in% subtypes_rightsholder | Subtype %in% subtypes_creatives)

sopa_IG_plat = subset(sopa_wc, Subtype %in% subtypes_plat | Subtype %in% subtypes_civ_soc)

sopa_IG_oth = subset(sopa_wc, !(Subtype %in% subtypes_plat | 
                                  Subtype %in% subtypes_rightsholder | 
                                  Subtype %in% subtypes_creatives |
                                  Subtype %in% subtypes_civ_soc))

write.csv(sopa_IG_rh_cr, "SOPA/Interest Groups/SOPA_IG_Rightsholder&creatives_web&tweets.csv")
write.csv(sopa_IG_plat, "SOPA/Interest Groups/SOPA_IG_Platforms&Userrights_web&tweets.csv")
write.csv(sopa_IG_oth, "SOPA/Interest Groups/SOPA_IG_others_web&tweets.csv")

## Plot wordclouds
add_stopw = c("sopa", "pipa", "rt", "protectip", "stop", "online", "act", "protect", "intellectual",
              "property", "copyright", "patent", "rights", "trademark", "patenty", "copyrights", "s",
              "u", "u.s", "c2", "a9", "m", "us", "r", "t", "rt", "c-11", "o-32", "see", "may", "can",
              "ip", "new", "one", "many", "without", "inc", "even", "via", "bsa", "make", "year",
              "high", "case", "cases", "also", "pc", "ipr", "well", "jquery", "must", "fb01", "made",
              "another", "including", "take", "two", "today", "third", "now", "top", "used", "mr", 
              "john", "floyd", "abrams", "yes", "dga", "ibt", "afm", "among", "include", "regarding",
              "vice", "tion", "cam-", "ing", "ment", "june", "sag", "ever", "8th", "york", "j", "w", 
              "a0", "fl", "e", "u002f", "href", "sm", "u003c", "c", "e", "n", "https", "return",
              "aspectratio", "every", "u003e", "ny", "width", "ca", "type", "u003ca", "value_type",
              "type_value", "get", "thanks", "years", "much", "md", "always", "since", "http", "just",
              "like", "p", "o", "away", "around", "want", "function", "value", "stand", "still", "var",
              "lg", "others", "need", "available", "allow", "know", "keep", "really", "say", "read", 
              "give", "continue", "going", "please", "next", "think", "let", "v", "says", "back",
              "using", "wa", "tx", "u002fa", "fffd", "cth", "f0b7", "due", "pls", "however", "might",
              "increasingly", "a7")

wordcloud(sopa_wc, subtype_list = subtypes_rightsholder,
          text_field = "text", lang = "en", add_stopw = add_stopw)
wordcloud(sopa_wc, subtype_list = subtypes_creatives,
          text_field = "text", lang = "en", add_stopw = add_stopw)
wordcloud(sopa_wc, subtype_list = subtypes_plat,
          text_field = "text", lang = "en", add_stopw = add_stopw)
wordcloud(sopa_wc, subtype_list = subtypes_civ_soc,
          text_field = "text", lang = "en", add_stopw = add_stopw)

# to small sample for: #subtypes_plat_rh_mix, subtypes_lobbyists

############# a) EUCD ############# 
subtypes_rightsholder = c("Media Retail", "Data Analytics", "Furniture", "Television",
                          "Press Stock", "Electronics", "ICT", "Copyright Management", 
                          "Industry", "Education", "Rightsholder", "Telecom & Media",
                          "ICT, Media, Telecom", "Telecom & ICT", "Telecom")

subtypes_plat = c("Platform", "Platforms", "Platforms and User Rights", "Platform-dependent entrepreneur", 
                  "Tech & Platforms")

subtypes_civ_soc = c("Consumer Rights, User Rights", "Archives, Libraries, Museum, Theatre",
                     "Open Knowledge", "Open Source", "Open Access", "Open Source Software",
                     "Human Rights", "Blind Rights", "ICT Refurbishing")

subtypes_plat_rh_mix = c("Platform & ICT", "Media & ICT & Platforms", "ICT & Platform", "Platform & Media",
                         "Tech & Start ups")

subtypes_creatives = c("Artists", "Creative Sector", "Theater", "Translators")

subtypes_lobbyists = c("Law Firm", "Lobbying Agency", "Consultancy", "Consultant")

subtypes_others = c("Anti-Corruption", "Blockchain", "Development", "Health Policy", "Language",
                    "Political Party", "Start ups", "Transpareny", "Anti-HateSpeech", "Betting",
                    "Energy", "Geography", "Hospitality", "Individual", "Market Research",
                    "Regional Organization", "Tech & Digital Research", "Uploadfilter",
                    "Electoral Support", "Finance", "Health",  "Marketing", "Retail",
                    "Standardization", "Trade Union", "White collar workers", "Think Tank", 
                    "Science", "Computer Science", "Private-sponsored Science")

##### i) Wordcloud #####
## tweets
eucd_tweets_wc = EUCD_IG_info %>%
  gather(key="tw_id", value = "author_id", tw_id1:tw_id13)

eucd_tweets_wc = subset(eucd_tweets_wc, !is.na(author_id))

eucd_tweets_wc$author_id = as.numeric(eucd_tweets_wc$author_id)

eucd_tweets_wc = full_join(eucd_tweets_wc, EUCD_IG_tweets, by = "author_id")

eucd_tweets_wc$link = eucd_tweets_wc$link_to_tweet
eucd_tweets_wc$source = "tweet"

eucd_tweets_wc = select(eucd_tweets_wc, c(name, text, link, source, Subtype, regid, language, censor:reach, datetime))

## website
eucd_IG_web_dict$text = eucd_IG_web_dict$para_text_web

eucd_IG_web_dict$link = eucd_IG_web_dict$source
eucd_IG_web_dict$source = "website"

eucd_web_wc = select(eucd_IG_web_dict, c(name, text, link, source, Subtype, regid, language, censor:reach, datetime))

## bind IG web and tweets data together
eucd_wc = rbind(eucd_web_wc, eucd_tweets_wc)


write.csv(eucd_wc, "EUCD/EUCD_Interest Groups_Web&Twitter.csv")

# save subtype datasets
eucd_IG_rh_cr = subset(eucd_wc, Subtype %in% subtypes_rightsholder | Subtype %in% subtypes_creatives)

eucd_IG_plat = subset(eucd_wc, Subtype %in% subtypes_plat | Subtype %in% subtypes_civ_soc)

eucd_IG_oth = subset(eucd_wc, !(Subtype %in% subtypes_plat | 
                                  Subtype %in% subtypes_rightsholder | 
                                  Subtype %in% subtypes_creatives | 
                                  Subtype %in% subtypes_civ_soc))

write.csv(eucd_IG_oth, "EUCD/Interest Groups/EUCD_IG_others_web&tweets.csv")
write.csv(eucd_IG_rh_cr, "EUCD/Interest Groups/EUCD_IG_Rightsholder&creatives_web&tweets.csv")
write.csv(eucd_IG_plat, "EUCD/Interest Groups/EUCD_IG_Platforms&Userrights_web&tweets.csv")

## Plot wordclouds
add_stopw = c("directive", "european", "copyright", "directive", "union", "intellectual", 
              "property", "oecd", "office", "eu", "reform", "policy", "recommendation", 
              stopwords("de"), stopwords("fr"), stopwords("es"), stopwords("pt"), stopwords("nl"), 
              stopwords("it"), stopwords_polish, "urheberrecht", "f", "fc", "u", 
              "e9", "s", "r", "e4", "https", "rt", "u003c", "europe", "commission", "v3", "nc",
              "x", "gram", "a9", "get", "want", "e", "just", "like", "now", "see", "keep",
              "read", "make", "today", "can", "next", "needs", "please", "copyrightdirective",
              "week", "time", "yes", "say", "without", "us", "available", "including", "dear", 
              "senficon", "making", "ensure", "find", "nothing", "urheberrechts", "https", 
              "httpstcohmjrhtlays", "know", "f6", "years", "amp", "may", "atau", "take", "dauteur",
              "droit", "hp", "via", "give", "a0", "o", "js", "w", "co", "na", "css", "ue", "xx",
              "fbcdn", "static", "php", "rsrc", "dass", "need", "u003e", "must", "even", "net",
              "current", "new", "heute", "tdm", "ij3wp8lg5kz", "direttiva", "src", "still", "tell",
              "p", "uk", "fundamental", "stop", "urheberrechtsreform", "prawoautorskie", "prawo",
              "autorskie", "v", "day", "glynmoody", "join", "important", "type", "march", "better",
              "class", "http", "across", "check", "many", "true", "around", "adopted", "div", "title",
              "frank", "allow", "april", "sebastian", "org", "k", "g", "within", "anne", "related",
              "paulkeller", "september", "different", "october", "february", "certain", "possible",
              "auteursrecht", "well", "u0022", "thank", "d'auteur", "httpstcosjdyeqglmg", "u0022eu", 
              "u003cdiv", "helgatruepel", "jeanmariecavada", "adopt", "ask", "favour", "tomorrow")


wordcloud(eucd_wc, subtype_list = subtypes_rightsholder,
          text_field = "text", lang = "en", add_stopw = add_stopw)
wordcloud(eucd_wc, subtype_list = subtypes_creatives,
          text_field = "text", lang = "en", add_stopw = add_stopw)
wordcloud(eucd_wc, subtype_list = subtypes_plat,
          text_field = "text", lang = "en", add_stopw = add_stopw)
wordcloud(eucd_wc, subtype_list = subtypes_civ_soc,
          text_field = "text", lang = "en", add_stopw = add_stopw)

# to small sample for: #subtypes_plat_rh_mix, subtypes_lobbyists