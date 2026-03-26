####################################################################
####################################################################
## Replication Material
##
## Shaun Bowler, Gail McElroy, Stefan Müller:
## Voter Expectations of Government Formation in Coalition Systems: 
## The Importance of the Information Context.
## European Journal of Political Research

## File: 04_text_analysis_newspaper_articles.R

## See 000_description_replication_material.pdf for a detailed 
## overview of the required data and the outputs of this file.

####################################################################
####################################################################


## Note: load newspaper articles potentially relating to German federal or state coalitions, 
## classify geographical focus (federal level, subnational level (which state?)),
## reshape corpus to level of sentences, apply dictionaries of parties, coalitions, and cooperation,
## aggregate results for a time window of 2 years prior the start of each survey in each state
## and store this data frame for subsequent analyses. Each election-coalition option with at least one
## mention is included in the dataset.

## For copyright reasons, the raw articles cannot be uploaded publicly. 
## Please get in touch with the authors to discuss possibilities of accessing 
## the raw data
## The dataset data_news_coalitions.rds contains the election-government specified
## estimates of coalition signals


## load required packages
library(quanteda) # Quantitative Analysis of Textual Data, CRAN v2.1.2
library(dplyr) # A Grammar of Data Manipulation, CRAN v1.0.2
library(newsmap) # Semi-Supervised Model for Geographical Document Classification, CRAN v0.7.2
library(tidyr) # Tidy Messy Data, CRAN v1.1.2

## load all newspaper articles that were retrieved from NexisLexis
load("data_dontshare/corpus_ger_elections.Rdata")

n_newspapers_total <- ndoc(data_corpus)
n_newspapers_total

## remove duplicated articles, filtered based on date and title (head)

title_df <- data.frame(
  date = docvars(data_corpus, "date"),
  head = docvars(data_corpus, "head"),
  tokens = ntoken(data_corpus)
)

## select the first article for each article group
title_df_select <- title_df %>% 
  group_by(head, date) %>% 
  mutate(first_article_in_corpus = row_number())  

data_corpus$first_article_in_corpus <- title_df_select$first_article_in_corpus

newspapers <- data_corpus$pub

data_corpus_unique <- data_corpus %>% 
  corpus_subset(first_article_in_corpus == 1)


table(newspapers)


ndoc(data_corpus)

ndoc(data_corpus_unique)


## classify geographic focus of article using newsmap package

data_dictionary_newsmap_ger_pol <- dictionary(file = "dictionary_german_politics_terms.yml")

## tokenize corpus
toks_de_newsmap <- tokens(data_corpus_unique)

## apply dictionary to tokens object
label_toks_de_newsmap <- tokens_lookup(toks_de_newsmap, 
                                       dictionary = data_dictionary_newsmap_ger_pol,
                                       levels = 2)

## create document feature matrix
label_dfm_de_newsmap <- dfm(label_toks_de_newsmap)

topfeatures(label_dfm_de_newsmap, n = 100)

feat_dfm_de_newsmap <- dfm(toks_de_newsmap, tolower = FALSE)

topfeatures(feat_dfm_de_newsmap, n = 100)

## run newsmap model
model_de_newsmap <- newsmap::textmodel_newsmap(feat_dfm_de_newsmap, label_dfm_de_newsmap)

## get most important terms
terms_important_newsmap <- coef(model_de_newsmap, n = 10)

dat_terms <- do.call(rbind, lapply(terms_important_newsmap, as.data.frame))

dat_terms$region_term <- rownames(dat_terms)

dat_term$newsmap_score <- dat_term$`X[[i]]`
dat_term <- separate(dat_terms, region_term, into = c("region", "term"),
                     sep = "\\.")


## get predictions
predictions_newsmap <- predict(model_de_newsmap)

## add column to corpus that shows prediction
data_corpus_unique$newsmap_predict <- predictions_newsmap


## now check that only articles are considered that actually mention an election or the institutions
dfmat_unique <- dfm(data_corpus_unique, 
                    remove = stopwords("german"), 
                    tolower = TRUE,
                    remove_punct = TRUE, remove_numbers = TRUE)

dict_filter_relevant <- dictionary(list(land_election = c("landtag*", "bürgerschaft*"),
                                        federal_election = c("bundestag*"),
                                        coalition = c("koalieren", "koalition*", "bündnis*", "zusammenarbeit*")))

data_dfm_lookup <- dfm_lookup(dfmat_unique, 
                              dictionary = dict_filter_relevant)

## add values of land_election and federal_election
data_dfm_lookup_df <- convert(data_dfm_lookup, to = "data.frame") %>% 
  mutate(election_merged = land_election + federal_election)

## get old docvars
docvars_old <- docvars(data_corpus_unique)

## bind new docvars and old docvars
docvars_new <- bind_cols(data_dfm_lookup_df, docvars_old)

docvars(data_corpus_unique) <- docvars_new

ndoc(data_corpus_unique)
ndoc(data_corpus_filter)

## keep only articles that mention one of the terms for coalitions and the election/institution
data_corpus_filter <- data_corpus_unique %>% 
  corpus_subset(coalition > 0 & election_merged > 0)


## number of relevant articles
ndoc(data_corpus_filter)

## reshape corpus to the level of sentences
corp_sentence <- data_corpus_filter %>% 
  corpus_reshape(to = "sentences")

n_sentences_relevant <- ndoc(corp_sentence)

## get descriptive statistics
n_newspapers_total
n_newspaper_relevant
n_sentences_relevant


## parties dictionaries 
cdu <- c("cdu", 
         "christlich-demokratische union", 
         "christdemokraten", 
         "union")

csu <- c("csu", "christlich-soziale union", 
         "christdemokraten")

spd <- c("spd", 
         "sozialdemokratische partei deutschlands", 
         "sozialdemokraten")

fdp <- c("fdp", 
         "freie demokratische partei", 
         "liberalen", 
         "freie* demokraten")

greens <- c("grünen", "grüne", "bündnis 90")

left <- c("die linke", "linkspartei", "WASG")

afd <- c("afd", "alternative für deutschland")

## coalitions

cdu_spd <- c("große* koalition", "schwarz-rot*", "rot-schwarz*")

spd_greens <- c("rot-grün*", "grün-rot*")

cdu_fdp <- c("schwarz-gelb*")

cdu_greens <- c("schwarz-grün*")

cdu_fdp_greens <- c("jamaika*", "jamaica*", "schwarz-gelb-grün*", "schwarz-grün-gelb*")

spd_left_greens <- c("rot-rot-grün*")

spd_left <- c("rot-rot", "rot-rote")

spd_fdp_greens <- c("ampel*", "rot-gelb-grün*", "rot-grün-gelb*")

spd_fdp <- c("sozialliberale koalition*")


## create dictionary for coalition types

dict_parties <- dictionary(list(
  coalition_terms = c("koali*", "eingeh*",
                      "regier*", "koooper*", "arbeit*",
                      "bündnis*"),
  parties = list(spd = spd,
                 cdu = cdu, 
                 csu = csu,
                 fdp = fdp,
                 greens = greens, 
                 left = left, 
                 afd = afd),
  coalitions = list(cdu_spd = cdu_spd,
                    cdu_fdp = cdu_fdp,
                    cdu_greens = cdu_greens,
                    cdu_fdp_greens = cdu_fdp_greens,
                    spd_greens = spd_greens,
                    spd_left = spd_left,
                    spd_left_greens = spd_left_greens,
                    spd_fdp = spd_fdp,
                    spd_fdp_greens = spd_fdp_greens)))


## tokenize sentence-level corpus and look up dictionary
toks_ger <- corp_sentence %>% 
  tokens() %>%
  tokens_lookup(dictionary = dict_parties)

## create document-feature matrix
dfmat_ger <- dfm(toks_ger)

topfeatures(dfmat_ger)

dfmat_ger_lookup <- dfmat_ger %>% 
  dfm_weight(scheme = "boolean") ## make entries binary (1/0) (mentioned/not mentioned)

## convert dfm to data frame
dat_lookup_df <- convert(dfmat_ger_lookup,
                         to = "data.frame") 

## bind docvars from sentence-level corpus with data about coalitions and mentions of cooperation
data_full <- bind_cols(docvars(corp_sentence),
                       dat_lookup_df)

## add texts to data frame
data_full$text <- texts(corp_sentence)

## create indicators that check whether coalition or parties are mentioned
## and then only filter sentences that have at least one match for 
## a coalition and a term indicating cooperation

data_relevant <- data_full %>% 
  mutate(parties_coalitions_sum = parties.spd +  parties.cdu + 
           parties.fdp + parties.greens + parties.left +
           parties.afd + coalitions.cdu_spd + coalitions.cdu_fdp + 
           coalitions.cdu_greens + coalitions.cdu_fdp_greens + coalitions.spd_greens +
           coalitions.spd_fdp_greens + coalitions.spd_left_greens + 
           coalitions.spd_left) %>% 
  mutate(cdu_spd = parties.cdu + parties.spd,
         cdu_fdp = parties.cdu + parties.fdp,
         cdu_greens = parties.cdu + parties.greens,
         cdu_fdp_greens = parties.cdu + parties.greens + parties.fdp,
         spd_greens = parties.spd + parties.greens,
         spd_left = parties.spd + parties.left,
         spd_left_greens = parties.spd + parties.greens + parties.left,
         spd_fdp = parties.spd + parties.fdp,
         spd_fdp_greens = parties.spd + parties.greens + parties.fdp) %>% 
  filter(parties_coalitions_sum > 0) %>% ## filter only sentences that mention coalition
  filter(coalition_terms > 0) ## filter only sentences that mention cooperation

head(data_relevant$text)
sum(data_relevant$coalitions.cdu_fdp_greens)


## recode two- and three-party coalitions
data_relevant <- data_relevant %>% 
  mutate(cdu_spd = parties.cdu + parties.spd,
         cdu_fdp = ifelse(cdu_fdp_greens >= 3, 0, cdu_fdp),
         cdu_greens = ifelse(cdu_fdp_greens >= 3, 0, cdu_greens),
         spd_greens = ifelse(spd_left_greens >= 3, 0, spd_greens), 
         spd_left = ifelse(spd_left_greens >= 3,0,
                           ifelse(spd_fdp_greens >= 3, 0, spd_left)))


## important: a two-party coalition needs to have at least a value of 2, 
## a three party coalition has to have a value of at least 3 - therefore recoding is required
data_relevant <- data_relevant %>% 
  mutate(coalition_type = case_when(
    coalitions.spd_fdp_greens == 1 | spd_fdp_greens >= 3 ~ "spd_fdp_greens",
    coalitions.spd_left_greens == 1 | spd_left_greens >= 3 ~ "spd_left_greens",
    coalitions.cdu_fdp_greens == 1 | cdu_fdp_greens >= 3 ~ "cdu_fdp_greens",
    coalitions.cdu_fdp == 1 | cdu_fdp >= 2 ~ "cdu_fdp",
    coalitions.cdu_greens == 1 | cdu_greens >= 2 ~ "cdu_greens",
    coalitions.cdu_spd == 1 |  cdu_spd >= 2 ~ "cdu_spd",
    coalitions.spd_left == 1 | spd_left >= 2 ~ "spd_left",
    coalitions.spd_fdp == 1 | spd_fdp >= 2 ~ "spd_fdp",
    coalitions.spd_greens == 1 | spd_greens >= 2 ~ "spd_greens"))


table(data_relevant$coalition_type)

## dummy that checks whether coalition is on the federal or subnational level
data_relevant <- data_relevant %>%
  mutate(federal_dummy = if_else(land_election > 1, "Land level",
                                 ifelse(federal_election > 1, "Federal level", NA)))

table(data_relevant$federal_dummy)


## keep only relevant articles that mention a coalition 
## and can be classified as federal or land
data_relevant <- data_relevant %>% 
  filter(!is.na(coalition_type)) %>% 
  filter(!is.na(federal_dummy)) %>% 
  filter(date != "")


## number of relevant articles that mention a coalition and cooperation
nrow(data_relevant)

## get most frequent terms after accounting for MWEs

## identify collocations
collocs <- data_relevant %>%
  corpus(text_field = "text") %>% 
  tokens(remove_punct = TRUE, 
         remove_numbers = TRUE, 
         padding = TRUE) %>%
  tokens_remove(stopwords("de"), padding = TRUE) %>%
  textstat_collocations()

## create dfm
dfmat_collocs <- data_relevant %>%
  corpus(text_field = "text") %>% 
  tokens() %>%
  tokens_compound(collocs, concatenator = " ") %>% #compound MWEs
  tokens(remove_punct = TRUE, remove_numbers = TRUE) %>% 
  dfm() %>% 
  dfm_remove(pattern = c(stopwords("de"), "dass", "|"))

## get 50 most frequent word, save as csv and manually
## translate terms to English for a plot
tstat_freq <- textstat_frequency(dfmat_collocs, n = 50)

## write_csv(tstat_freq, "data_raw/data_tstat_freq.csv")


## add election dates to assign news articles to elections

## subset land elections
dta_metadata_land <- readRDS(file = "metadata_elections_land.rds")

dta_metadata_federal <- readRDS(file = "metadata_elections_federal.rds")

dta_metadata_federal$election_year <- dta_metadata_federal$year

## function to subset only articles that mention a certain Bundesland
## and fall within the specified window of days
subset_articles <- function(election_year, land_select) {
  
  if (land_select == "federal election") {
    dta_survey_metadata_select <- dta_metadata_federal
  }
  
  else {
    dta_survey_metadata_select <- dta_metadata_land
  }
  
  dta_survey_metadata_select <- dta_survey_metadata_select %>%
    select(bundesland, year, date_survey_start) %>% 
    unique() %>% ## get survey start dates for Land elections
    mutate(year = as.numeric(year),
           date_survey_start = as.Date(date_survey_start))
  
  dta_date_election <- dta_survey_metadata_select %>% 
    mutate(year = as.numeric(year)) %>% 
    filter(year == as.numeric(election_year) &
             bundesland == as.character(land_select))
  
  if (land_select == "federal election") {
    data_relevant_subset <- data_relevant %>% 
      filter(federal_dummy == "Federal level")
  }
  
  else {
    data_relevant_subset <- data_relevant %>% 
      filter(federal_dummy == "Land level") %>% 
      filter(newsmap_predict == as.character(land_select))
  }
  
  data_relevant_subset
  
  date_survey_start <- dta_date_election$date_survey_start
  
  date_articles_start <- date_survey_start - 730
  
  dta_articles_relevant_subset <- data_relevant_subset %>% 
    mutate(date = as.Date(date)) %>% 
    filter(date > date_articles_start) %>% 
    filter(date < date_survey_start) 
  
  dta_articles_relevant_subset_final <- dta_articles_relevant_subset %>% 
    mutate(number_articles_election = n()) %>% 
    group_by(coalition_type) %>%
    mutate(number_mentions_coalition = n()) %>% 
    select(federal_dummy, coalition_type, #negation,
           number_articles_election, number_mentions_coalition) %>% 
    unique() %>% 
    mutate(year = as.character(election_year)) %>% 
    mutate(bundesland = land_select) %>% 
    ungroup() #%>% 
  ## mutate(negation = car::recode(negation, "'0'='ref_positive'; '1'='ref_negative'")) %>% 
  ## spread(negation, number_mentions_coalition) 
  
  dta_articles_relevant_subset_final <- dta_articles_relevant_subset_final %>% 
    mutate(date_min = min(dta_articles_relevant_subset$date),
           date_max = max(dta_articles_relevant_subset$date))
}


## federal elections
news_2009_federal <- subset_articles(land_select = "federal election", 
                                     election_year = 2009)

news_2013_federal <- subset_articles(land_select = "federal election", 
                                     election_year = 2013)

news_2017_federal <- subset_articles(land_select = "federal election",
                                     election_year = 2017)


## land elections
news_2010_nrw <- subset_articles(election_year = 2010, 
                                 land_select = "nordrhein-westfalen")

news_2011_sachsen_anhalt <- subset_articles(election_year = 2011, 
                                            land_select = "sachsen-anhalt")

news_2011_rheinland_pfalz <- subset_articles(election_year = 2011, 
                                             land_select = "rheinland-pfalz")


news_2011_baden_wuerttemberg <- subset_articles(election_year = 2011, 
                                                land_select = "baden-wuerttemberg")

news_2011_berlin <- subset_articles(election_year = 2011, 
                                    land_select = "berlin")

news_2011_mecklenburg_vorpommern <- subset_articles(election_year = 2011,
                                                    land_select = "mecklenburg-vorpommern")


news_2012_nrw <- subset_articles(election_year = 2012, 
                                 land_select = "nordrhein-westfalen")


news_2012_schleswig_holstein <- subset_articles(election_year = 2012, 
                                                land_select = "schleswig-holstein")

news_2013_niedersachsen <- subset_articles(election_year = 2013, 
                                           land_select = "niedersachsen")

news_2013_hessen <- subset_articles(election_year = 2013, 
                                    land_select = "hessen")

news_2013_bayern <- subset_articles(election_year = 2013, 
                                    land_select = "bayern")

news_2014_sachsen <- subset_articles(election_year = 2014,
                                     land_select = "sachsen")

news_2014_brandenburg <- subset_articles(election_year = 2014, 
                                         land_select = "brandenburg")

news_2014_thueringen<- subset_articles(election_year = 2014, 
                                       land_select = "thueringen")

news_2016_baden_wuerttemberg <- subset_articles(election_year = 2016, 
                                                land_select = "baden-wuerttemberg")

news_2016_sachsen_anhalt <- subset_articles(election_year = 2016, 
                                            land_select = "sachsen-anhalt")

news_2016_rheinland_pfalz <- subset_articles(election_year = 2016, 
                                             land_select = "rheinland-pfalz")

news_2016_mecklenburg_vorpommern <- subset_articles(election_year = 2016, 
                                                    land_select = "mecklenburg-vorpommern")

news_2017_schleswig_holstein <- subset_articles(election_year = 2017, 
                                                land_select = "schleswig-holstein")

news_2017_nrw <- subset_articles(election_year = 2017, 
                                 land_select = "nordrhein-westfalen")


## bind all relevant articles into a single data frame
news_combined <- bind_rows(news_2010_nrw,
                           news_2011_baden_wuerttemberg,
                           news_2011_berlin,
                           news_2011_mecklenburg_vorpommern,
                           news_2011_rheinland_pfalz,
                           news_2011_sachsen_anhalt,
                           news_2012_nrw,
                           news_2012_schleswig_holstein,
                           news_2013_niedersachsen,
                           news_2013_bayern,
                           news_2013_hessen,
                           news_2014_brandenburg,
                           news_2014_sachsen,
                           news_2014_thueringen,
                           news_2016_mecklenburg_vorpommern,
                           news_2016_baden_wuerttemberg,
                           news_2016_sachsen_anhalt,
                           news_2016_rheinland_pfalz,
                           news_2017_nrw,
                           news_2017_schleswig_holstein,
                           news_2009_federal,
                           news_2013_federal,
                           news_2017_federal)

## change NA to 0
news_combined[is.na(news_combined)] <- 0

## group data frame by election and sum the references of positive and negative metnions
news_combined <- news_combined %>% 
  group_by(bundesland, year) %>% 
  rename(ref_total = number_mentions_coalition) %>% 
  mutate(ref_perc_of_all_coalitions = 100 * (ref_total / sum(ref_total, na.rm = TRUE)))

## save data
saveRDS(news_combined, file = "data_news_coalitions.rds")

