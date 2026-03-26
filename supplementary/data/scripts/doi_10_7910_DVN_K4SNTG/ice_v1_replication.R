######################################################
## Copyright 2024 (CC BY-NC-ND 4.0) ##################
## Proper attribution required. Not for commercial use.
## Request permission for modifications/derivatives ##
######################################################

rm(list=ls())
set.seed(888)
library(data.table)
library(dplyr)
library(tidyr)
library(tidyverse)

# set to wherever your data file is located
# setwd("~/yourdirectory/")
# load compressed DAT file (63.1 MB)
df <- read.table(gzfile("ice_v1.dat.gz"), row.names=1)
# or load CSV, if preferred (481.7 MB)
# df <- as.data.frame(data.table::fread("ice_v1.csv", strip.white=TRUE, stringsAsFactors = F, na.strings=c("","NA"))) 


####################################################
########## OPTIONAL: RECODE ABSTENTIONS ############
####################################################

# identify set of UN docs where abstentions might have occurred
# disregard this section unless abstentions are of theoretical interest

# un <- subset(df, Forum=="UN" & 
#               Subscribers_n<max(Subscribers_n) & 
#               Subscribers_n>1)
# 
# # find unique links matching criteria: 140
# # note: abstentions will be found in only a small subset of these,
# # but we have to inspect all of them to identify which.
# unique(un$Link)
# 
# # extract list of unique links
# links <- unique(un$Link)
# # some incl > 1 link; whenever that's the case, we go with first one 
# links <- unique(gsub("(.*);.*", "\\1", links))
# # add http:// if not included already
# links <- ifelse(grepl("https://", links), links, paste0("https://", links, sep=""))
# 
# # this opens in browser tabs when executed!
# # it should use system's primary browser
# # may freeze R; if code doesn't work, we can always copy/paste manually.
# lapply(links,function(x) browseURL(as.character(x)))
# 
# # upon inspecting, any links broken since data first compiled? 
# # if so, use URL content/metadata to find titles + search manually
# 
# # coding abstentions using raw documents:
# df$Abstain <- 0
# # how to use the following code:
# # 1. paste full URL in open quotes. recommend a new line for each document.
# # 2. for each document, list all abstaining countries x,y,z,... in c(x,y,z,...) 
# df$Abstain <- ifelse(grepl("_paste_url_here_", df$Link) & 
#                        df$Country %in% c("x","y","z"), 
#                      1, df$Abstain) # abstain=1 if conditions met; else do nothing.
# # ... copy line & repeat for all relevant documents.
# # when done, run the following to treat abstentions as non-endorsement: 
# df <- subset(df, Abstain==0)


####################################################
################ DESCRIPTIVE STATS #################
####################################################

#  num expressions made by each country
length(unique(df$Country))
table(df[!is.na(df$Title), "Country"]) 
# average wordcount - slow
mean(stringr::str_count(df$Text,'\\w+'), na.rm=T) + 1
# total wordcount - slow
sum(stringr::str_count(df$Text,'\\w+'), na.rm=T) + 1
# survivor sources
table(df$DF) 
length(unique(df$Country))
nrow(df)
# missingness - total
sum(is.na(df$Text)) / nrow(df)
# missingness - inclusion criteria met
sum(is.na(subset(df, !is.na(IdealPoint_f))$Text)) / nrow(subset(df, !is.na(IdealPoint_f)))




####################################################
############ OPTIONAL: REPLICATING DF ##############
####################################################

# remove NLP + idealpoint columns for replication
df <- df[,-c(93:123)] 




# ideal points (Voeten, Erik; Strezhnev, Anton; Bailey, Michael, 2009) 
# "United Nations General Assembly Voting Data", 
# https://doi.org/10.7910/DVN/LEJUQZ 

df$IdealPoint_f <- df$IdealPoint

# *** note: about to make a big assumption *** 
# to avoid dropping texts after 2021...
# ...assume no change from previous year
df <- df %>%
  group_by(Country) %>%
  fill(IdealPoint_f, .direction = "downup") %>%
  fill(IdealPoint_f, .direction = "updown")


# creating a "faction" label for plotting -------
# assume "factions" refer to diplomatic alignment
library(Hmisc)
# split into equal-sized tertiles
df$Faction <- as.factor(as.numeric(cut2(df$IdealPoint_f, g=3)))

# apply a faction label to each country
# < 33% = "redspace" / < 66% = "bluespace"
levels(df$Faction) <- c("Redspace", "Grayspace", "Bluespace")  
table(df$Faction)

# bluespace vs rest
df$Faction_W <- ifelse(df$Faction=="Bluespace", 1, NA)
df$Faction_W <- ifelse(df$Faction=="Grayspace" | df$Faction=="Redspace", 0, df$Faction_W)

# creating "mentions" vars  -----
df$M_Norms <- ifelse(grepl("responsible behavior|responsible behaviour|responsible state| norm|norm |norm,| norm\\.|norm-|norms", df$Text, ignore.case=TRUE), 1, 0)
df$M_Law <- ifelse(grepl("international law|international legal | charter|article 2|article 51|already appl*|right to self*|obligation to |armed attack|ihl|international humanitarian law", df$Text, ignore.case=TRUE), 1, 0)
df$M_Sovereignty <- ifelse(grepl("sovereign|sovereignty|non-interference|noninterference|territorial integrity|equal footing|equitable cyberspace|just cyberspace", df$Text, ignore.case=TRUE), 1, 0)

# countries who've invoked "LMC" shibboleths vs. rest
LMC <- unique(subset(df, !is.na(Country) & grepl("likeminded |like minded |like-minded ", Text, ignore.case=TRUE))$Country)
df$Faction_LMC <- ifelse(df$Country %in% LMC, 1, 0)

# create wordcount var -----------
df$Wordcount <- stringr::str_count(df$Text, "\\W+") + 1 # counts word separators

# regions
df$Continent <- countrycode::countrycode(df$Country, "country.name", "continent")
df$Region <- countrycode::countrycode(df$Country, "country.name", "un.regionsub.name") 


# convert empty strings to NA ----------
df <- df %>% mutate_all(na_if,"")

# ensure columns in correct format ----
df$Date <- as.Date(as.character(df$Date))
df$Year <- as.numeric(df$Year)
df$Text <- as.character(df$Text)

# refresh index (important!) ---
df <- df[order(df$Country, df$Date, df$Title, df$Text, df$Link, df$Summary, df$DF, df$Forum),]
df$index <- 1:nrow(df)
df.backup <- df

# remove de jure "nonstate" entities
nrow(df)
df <- subset(df, !is.na(Country) & Country!="NON-STATE" & !is.na(IdealPoint_f)); nrow(df) 

# subset on nonmissing for NLP encoding
df <- subset(df, !is.na(Text)); nrow(df) 

# create a separate ID for left-in documents 
df <- df[order(df$Country, df$Date, df$Title),]
df$doc_id <- 1:nrow(df)
df$doc_id <- paste("text", df$doc_id)
df$doc_id <- gsub(" ", "", df$doc_id)


##########################################################
# CAUTION: the following introduces errors (see Readme)
# (1) abbreviations replace full words the text.
# (2) all numerals in the text are deleted.
# -> changes were mistakenly merged back into the dataset.
# -> most applications should be unaffected, but be aware.
# -> stay tuned for corrections that restore 1 + 2.
##########################################################

# remove some superfluous special chars from text
df$Text <- gsub('[[:digit:]]+', ' ', df$Text) # digits
# collapsing common trigrams
df$Text <- gsub('open-ended working group|open ended working group', 'OEWG', ignore.case = T, df$Text) # digits
df$Text <- gsub('group of governmental experts|gge|gges', 'GGE', ignore.case = T, df$Text) # digits
df$Text <- gsub('information communication technology|information communication technologies', 'ICT', ignore.case = T, df$Text) # digits

##########################################################

# readability scores -------------
library(RcppParallel)
library(quanteda)
setThreadOptions(numThreads = 4)
quanteda_options(threads=4, verbose=T)
quanteda_options("pattern_hashtag" = NULL) # del twitter keys
library(quanteda.textstats)
library(quanteda.dictionaries)
library(stm)

# flesch-kincaid (or choose another measure)
set.seed(888)
df$Text <- as.character(df$Text)
df$FK_Score <- textstat_readability(df$Text, measure = "Flesch.Kincaid")[[2]]


# NLP-derived measures -----------------------
# optional: uncomment in studio with ctrl+c

# check preprocessing sensitivity (optional) 
# # http://www.mjdenny.com/getting_started_with_preText.html
# # devtools::install_github("matthewjdenny/preText")
# library(preText)
# 
# corp <- corpus(df$Text, docvars = df)
# corp <- corpus_subset(corp, subset=!is.na(IdealPoint_f) & !is.na(Text), drop_docid = F)
# 
# # # randomly sample 10% of the corpus
# # set.seed(888)
# # corp.samp <- corpus_sample(corp, size = 3000, replace = F)  # change to desired sample size
# # 
# # preprocessed_documents <- factorial_preprocessing(corp.samp, use_ngrams = TRUE, infrequent_term_threshold = 0.1, verbose = T)
# # 
# # preText_results <- preText(preprocessed_documents, dataset_name = "Corpus Sample", distance_method = "cosine", num_comparisons = 20, verbose = T)
# # 
# # head(preprocessed_documents$choices)
# # preText_score_plot(preText_results) # lower score = less risky
# # regression_coefficient_plot(preText_results, remove_intercept = TRUE)
# #
# # # guidelines from denny & spirling (2018):
# # # negative coefficient => perform that step
# # # positive coefficient => riskier to perform it
# #
# #
# # # finding the optimal number of topics (k) --- very slow!
# # # illustration with 2-grams; alternatively, set to 1:2 or 1:3
# # # to save memory, we'll take a random sample (caution: results sensitive)
# #
# set.seed(888)
# df$randomizer <- sample.int(nrow(df), nrow(df), replace=F)
# #
# library(caret)
# set.seed(888)
# split <- createDataPartition(y = df$"randomizer", p=.67, list=FALSE) #.3
# train <- df[split,]
# # # # test <- df[-split,]
# # #
# corp <- corpus(train$Text, docvars = train)
# corp <- corpus_subset(corp, subset=!is.na(IdealPoint_f) & !is.na(Text), drop_docid = F)
# # # #
# # # # # preview
# # summary(corpus_subset(corp, Year > 1998))
# # # #
# quanteda_options("pattern_hashtag" = NULL) # del twitter keys
# toks <- corp %>% tokens(what="word1", include_docvars=T,
#                         remove_numbers = T, # remove_twitter = T,
#                         split_hyphens=T, remove_url=T, remove_symbols=T,
#                         remove_separators=T, verbose=T, remove_punct = T) %>%
#   tokens_tolower() %>% tokens_remove(stopwords("english")) %>%
#   tokens_lookup(data_dictionary_uk2us, exclusive = FALSE, capkeys=F) %>%
#   tokens_wordstem() %>%
#   tokens_ngrams(n = 2, skip = 0)
# 
# # # convert to stm
# dfm <- dfm(toks)
# dfm.stm <- convert(dfm, to = "stm", docvars = docvars(corp))


# now, ready to find k ---------
# save as much memory as possible
# rm(list=setdiff(ls(), c("dfm.stm", "df", "df.backup", "corp")))
# 
# # start with Lee and Minmo (2014) algorithm for range: k=0
# # auto solves for vertices of the convex hull of word co-occurances
# # in practice this seems to give the upper bound (max k)
# # devtools::install_github("davidcsterratt/geometry", subdir="pkg")
# 
# set.seed(888)
# system.time(
#   stm_object <- stm(documents = dfm.stm$documents,
#                     vocab = dfm.stm$vocab,
#                     data = dfm.stm$meta,
#                     prevalence = ~IdealPoint_f*s(Year),
#                     K = 0, max.em.its = 300, #runs = 20,
#                     init.type="Spectral", seed = 888)
# )
# 
# 
# # we could simple use stm_object with whatever k=0 recommends
# # however this gives us too many (nonexclusive) topics to work with
# 
# # another suggestion (optional): refine using range from k=0
# # 1. run searchk again, treating k* as max k
# # 2. run fine-grained searchk in optimal range
# # 3. select K=k from last run.
# 
# # run first
# k <- c(18:26) # VERY slow - consider tranches!
# #
# #
# system.time(
#   kResult_predictors <- searchK(dfm.stm$documents, prevalence=~IdealPoint_f * s(Year), dfm.stm$vocab,
#                      K=k, data=dfm.stm$meta, heldout.seed=888, cores=3)
# )
# #
# plot(kResult_predictors) #
# 
# 
# # first run plot(kResult_predictors) -- 
# # then identify narrower range...
# k <- c(15:25)
# # and run lines again for new k*


# recall that we randomly sampled the corpus ---------
# so run tokenizer again, this time with full document set
corp <- corpus(df$Text, docvars = df)
corp <- corpus_subset(corp, subset=!is.na(IdealPoint_f) & !is.na(Text), drop_docid = F)

# tokenize full set using preText recommendations
quanteda_options("pattern_hashtag" = NULL) # omit twitter keys
toks <- corp %>% tokens(what="word1", include_docvars=T, 
                        remove_numbers = T, remove_twitter = T,
                        split_hyphens=T, remove_url=T, remove_symbols=T,
                        remove_separators=T, verbose=T, remove_punct = T) %>%
  tokens_tolower() %>% tokens_remove(stopwords("english")) %>%
  tokens_lookup(data_dictionary_uk2us, exclusive = FALSE, capkeys=F) %>%
  tokens_wordstem() %>%
  tokens_ngrams(n = 2, skip = 0) # increase/decrease depending on computational power

dfm <- dfm(toks)

# freq plot for trimming frequent terms
features_dfm <- textstat_frequency(dfm, n = 100)

# Sort by reverse frequency order
features_dfm$feature <- with(features_dfm, reorder(feature, -frequency))
ggplot(features_dfm, aes(x = feature, y = frequency)) +
  geom_point() + 
  theme(axis.text.x = element_text(angle = 90, hjust = 1))

# trim at inflection point 
dfm <- dfm_trim(dfm, max_termfreq = 100000) 

# convert to STM
dfm.stm <- quanteda::convert(dfm, to = "stm", docvars = docvars(corp))

# build STM model
k <- 21 # set k*

set.seed(888)
system.time(
  stm_object <- stm(documents = dfm.stm$documents,
                    vocab = dfm.stm$vocab,
                    data = dfm.stm$meta,
                    prevalence = ~IdealPoint_f*s(Year),
                    K = k, max.em.its = 300, #runs = 20,
                    init.type="Spectral", seed = 888)
)

# choosing topic labels
labelTopics(stm_object) 
findThoughts(stm_object, topics=9, n=c(1), texts = corp)


# encoding topic proportions 
options(scipen = 999)
dt <- make.dt(stm_object, meta=corp)
dt <- sapply(dt, as.numeric)
colMeans(dt) * 100 # topic percentages
sum(na.omit(colMeans(dt)) * 100) - 1627350 # check they sum to 100%
options(scipen = 0)
topicCorr(stm_object) # no strong correlations between topics

# extract theta to code documents by their dominant topic:
# ---> returns topic proportion for each row --->
stm_object$theta

# create dataframe of thetas 
dx <- as.data.frame(stm_object$theta)
dx$names <- rownames(dx) # assumes same order
dx$names <- paste("text",dx$names, sep="")

# label each with topic name for convenience
names(dx) <- c("t_Resources", "t_Military", 
               "t_Inclusion", "t_CBMs",
               "t_Threats", "t_Development", 
               "t_Admin", "t_Implement", 
               "t_Misc", "t_Investment", 
               "t_Access", "t_Standards", 
               "t_Crime","t_Dialogue",
               "t_Behavior", "t_Espionage",
               "t_Disinfo", "t_Personalities",
               "t_Defense", "t_EconGrowth",
               "t_Regional",
               "doc_id") 

# treat maximum theta as the "dominant" theme of a given document
dt <- as.data.frame(colnames(as.data.frame(stm_object$theta))[apply(stm_object$theta,1,which.max)])

# see distribution of "dominant" topics 
dtplot <- dt %>%
  group_by(dt[,1]) %>%
  summarise(counts = n())

ggplot(dtplot, aes(x = dtplot$`dt[, 1]`, y = counts)) +
  geom_bar(fill = "#0073C2FF", stat = "identity") +
  geom_text(aes(label = counts), vjust = -0.3)

# rename columns for convenience
names(dt) <- "topic_num"
dt$topic_num <- gsub("V", "", dt$topic_num)
dt$doc_id <- rownames(dt) # assumes same order
dt$doc_id <- paste("text",dt$doc_id, sep="")
dt$topic_label <- dt$topic_num

# manually naming topic_labels
dt$topic_label[dt$topic_label==1] <- "Resource Planning"
dt$topic_label[dt$topic_label==2] <- "Militarization"
dt$topic_label[dt$topic_label==3] <- "Inclusion"
dt$topic_label[dt$topic_label==4] <- "Confidence-Building"
dt$topic_label[dt$topic_label==5] <- "Threat Landscape"
dt$topic_label[dt$topic_label==6] <- "Development" 
dt$topic_label[dt$topic_label==7] <- "Administrative" 
dt$topic_label[dt$topic_label==8] <- "Implementation"
dt$topic_label[dt$topic_label==9] <- "Miscellaneous"
dt$topic_label[dt$topic_label==10] <- "Investment & Aid"
dt$topic_label[dt$topic_label==11] <- "Access" 
dt$topic_label[dt$topic_label==12] <- "Standards"
dt$topic_label[dt$topic_label==13] <- "Criminal Misuse" 
dt$topic_label[dt$topic_label==14] <- "Dialogue" 
dt$topic_label[dt$topic_label==15] <- "State Behavior"
dt$topic_label[dt$topic_label==16] <- "Espionage/Privacy"
dt$topic_label[dt$topic_label==17] <- "Disinformation"
dt$topic_label[dt$topic_label==18] <- "Personalities"
dt$topic_label[dt$topic_label==19] <- "Defense & Resilience"
dt$topic_label[dt$topic_label==20] <- "Economic Growth"
dt$topic_label[dt$topic_label==21] <- "Regional Approaches"


# merge dominant topics w/ labels
df <- merge(df, dt, by="doc_id", all.x=T)

# merge all theta values 
df <- merge(df, dx, by="doc_id", all.x=T)


# encoding sentiment ----------------------
# using LSD (https://quanteda.io/reference/data_dictionary_LSD2015.html)
# (optional: choose a more advanced technique if desired)
toks_lsd <- tokens_lookup(toks, dictionary = data_dictionary_LSD2015[1:4]) # keep only pos/neg

# create a document-feature matrix 
dfmat_lsd <- dfm(toks_lsd, dictionary = data_dictionary_LSD2015[1:4])
dfmat_lsd <- quanteda::convert(dfmat_lsd, to = "data.frame")

# calculate sum sentiment scores 
# (no double negatives / negated positives found)
dfmat_lsd$Sentiment <- dfmat_lsd$positive - dfmat_lsd$negative
dfmat_lsd <- dfmat_lsd[,c(1,6)]

# merge with rest of data
df <- merge(df, dfmat_lsd, all.x=T, by = "doc_id")
names(df)


# reconstructing dataset with newly replicated NLP information -----

# first, obtain the set of held-out expressions
df.backup <- subset(df.backup, is.na(Country) | Country=="NON-STATE" | is.na(IdealPoint_f) |  is.na(Text)); nrow(df.backup)

# reattach for full df
df <- plyr::rbind.fill(df, df.backup)

# reset order
df <- df[order(df$Country, df$Date, df$Title, df$Text, df$Link, df$Summary, df$DF, df$Forum),]
df$index <- 1:nrow(df)


# enjoy your reconstructed dataset!




####################################################
############### REPRODUCING FIGURES ################
####################################################


##### Codebook Section VI
forum_list <- df %>% group_by(Forum) %>% summarise(n = n())
stargazer::stargazer(forum_list, type = "latex", summary=F)


##### Codebook Section XI
library(stargazer)
set.seed(888)
df <- df[sample(1:nrow(df)), ] # randomly shuffle
# rm "action" placeholder + cown + index + approx_date
stargazer(df, rownames=T, type="latex", column.sep.width = "1pt")

# printed in codebook using modelsummary package
library(modelsummary)
library(kableExtra)

graphics.off() 
datasummary_skim(df, type = "numeric", output = 'img/img_dxstats.png')


##### Main Text Figure 2 & Online Appendix
topics_idealpt <- c(1:k) 
topic_labels_idealpt <- c("Resources", 
                          "Militarization", 
                          "Inclusivity", 
                          "CBMs", 
                          "Threats",
                          "Development",
                          "Admin",
                          "Implementation",
                          "Miscellaneous",
                          "Investment",
                          "Access",
                          "Standards",
                          "Criminal Misuse",
                          "Dialogue",
                          "Behavior",
                          "Espionage",
                          "Disinformation",
                          "Personalities",
                          "Defense",
                          "Econ Growth",
                          "Regional")

stm_fx_idealpt <- estimateEffect(topics_idealpt ~ IdealPoint_f*s(Year),
                                 stmobj = stm_object,
                                 meta = dfm.stm$meta,
                                 uncertainty = "Global") 
summary(stm_fx_idealpt, topic=topics_idealpt) # shows which covars signif influence on content in each topic!

resultsList <- list()

for (i in 1:k) {
  stm_fx <- estimateEffect(i ~ Faction*s(Year),
                           stmobj = stm_object,
                           meta = dfm.stm$meta,
                           uncertainty = "Global")
  resultsList[[i]] <- stm_fx
}

levels(df$Faction)
f <- c("Redspace","Grayspace","Bluespace") # factions factor
m <- c("gray80","gray50","black")


# topic prevalence by faction over time --------
graphics.off()
png(filename="img/img_prevalence_tall_21_bw.png", width=1200, height=1400, res=200)
par(mar=c(2,2,2,1))
par(mfrow=c(7,3))

for (i in 1:21) {  # alt: i in 1:k
  for (j in 3:1) {  # faction levels
    x <- resultsList[[i]]
    plot(x, covariate = "Year", model = stm_object, method = "continuous",
         xlab = "Year", moderator = "Faction", 
         moderator.value = unique(f[[j]]), linecol = m[[j]], 
         ci.level = 0, # CIs removed at Reviewer 2's request
         ylim = c(-.05, .25),
         printlegend = F,  xaxt='n', main = paste(topic_labels_idealpt[[i]]))
    axis(side =1, seq(1998, 2022, 6), labels = T)
    par(new=TRUE)
  } 
par(new=FALSE)
} 

dev.off()


##### Online Appendix
library(ggplot2)
library(RColorBrewer)
df$Date <- as.Date(df$Date)


# observing skewness
plot(df$Date, df$Sentiment)
plot(df$Date, df$Wordcount)
plot(df$Sentiment) # skewed - +/-, cube root transform
plot(df$Wordcount) # skewed - strictly > 0, log transform

# transformations
df$Wordcount_ln <- log(df$Wordcount+1)
df$Sentiment_cr <- sign(df$Sentiment) * (abs(df$Sentiment))^(1/3)

# valence * activism
df$Satisfaction <- df$Sentiment_cr * df$Wordcount_ln # higher = happier
df$Discontent <- df$Satisfaction * (-1) # higher = unhappier

# sorting to match prevalence figure
df$topic_label <- as.character(df$topic_label)
df$topic_label <- ifelse(is.na(df$topic_label), "Not Categorized", df$topic_label)
df$topic_label <- as.factor(as.character(df$topic_label))
df$topic_label <- factor(df$topic_label, 
                          levels = c("Resource Planning", "Militarization", "Inclusion", 
                                     "Confidence-Building", "Threat Landscape", "Development",
                                     "Administrative", "Implementation", "Miscellaneous", 
                                     "Investment & Aid", "Access", "Standards",
                                     "Criminal Misuse", "Dialogue", "State Behavior",
                                     "Espionage/Privacy", "Disinformation", "Personalities",
                                     "Defense & Resilience", "Economic Growth", "Regional Approaches",
                                     "Not Categorized"))

# sentiment change over time ----------
graphics.off() 
ggplot(subset(df, topic_label!="Not Categorized"), aes(x=Date, y=Discontent, colour = Faction)) + 
  geom_point(size=1, alpha=.2, position = "jitter",color="white") +
  geom_point(shape=1, size=1, alpha=.1, position = "jitter") + 
  geom_smooth(se = T, fill = NA, method="loess", span=1.2) + 
  facet_wrap(~topic_label, ncol=3) + 
  scale_x_date(breaks = "5 year", date_labels = "%Y") +
  coord_cartesian(ylim=c(-30, 30)) + 
  theme_bw() + # theme(legend.position = c(0.85,0.05)) +
  theme(legend.position = "none",
        legend.title=element_blank(),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        panel.background = element_blank(),
        axis.text.x = element_text(size = 6)) + 
  scale_colour_manual(values = c("red", "black", "blue")) +
  labs(x = "", y = "Dissatisfaction with Status Quo") +
  geom_hline(yintercept=0, linetype="dotted", alpha=0.5) + 
  geom_vline(xintercept = df$date[df$date=="2008-01-31"], linetype="dashed", color = "black", size=.25) +
  geom_vline(xintercept = df$date[df$date=="2014-01-01"], linetype="dashed", color = "black", size=.25) +
  geom_vline(xintercept = df$date[df$date=="2018-01-01"], linetype="dashed", color = "black", size=.25) 


# readibility change over time ----------
graphics.off() 
ggplot(subset(df, topic_label!="Not Categorized"), aes(x=Date, y=log(FK_Score+1), colour = Faction)) + 
  geom_point(size=1, alpha=.2, position = "jitter",color="white") +
  geom_point(shape=1, size=1, alpha=.1, position = "jitter") + 
  geom_smooth(se = T, fill = NA, method="loess", span=1.2) + 
  facet_wrap(~topic_label, ncol=3) +
  scale_x_date(breaks = "5 year", date_labels = "%Y") + 
  coord_cartesian(ylim=c(2.5, 4)) + 
  theme_bw() + # theme(legend.position = c(0.85,0.05)) +
  theme(legend.title=element_blank(),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        panel.background = element_blank(),
        axis.text.x = element_text(size = 6)) + 
  scale_colour_manual(values = c("red", "black", "blue")) +
  labs(x = "", y = "Ln Complexity") +
  geom_vline(xintercept = df$date[df$date=="2008-01-31"], linetype="dashed", color = "black", size=.25) +
  geom_vline(xintercept = df$date[df$date=="2014-01-01"], linetype="dashed", color = "black", size=.25) +
  geom_vline(xintercept = df$date[df$date=="2018-01-01"], linetype="dashed", color = "black", size=.25)


### Main Text Figure 1
library(maps)
library(mapdata)
library(rworldmap)
library(rworldxtra)
library(RColorBrewer)

df$iso3c <- as.character(countrycode::countrycode(df$Country, origin="country.name", destination="iso3c"))
df$iso3c <- ifelse(df$Country=="Sudan", "SDN", df$iso3c)
df$iso3c <- ifelse(df$Country=="South Sudan", "SDS", df$iso3c)

counts_all <- aggregate(Wordcount ~ iso3c, data = subset(df, df$DF!="FOCUS_pull"), FUN= "sum" )

# getting all cols between these two poles
colfunc <- colorRampPalette(c("white", "black")) # or white?
col <- gsub(" ", "", paste(as.character(colfunc(201)), ""))


# weighting by proportion per year
colall <- head(col, n=sum(length(unique(df$iso3c))))
colblank <- head(col, n=1)

# map of expression volume ----------------------
options(scipen=10000)
spdf <- joinCountryData2Map(counts_all, joinCode="ISO3", nameJoinColumn="iso3c", mapResolution = "high", verbose=T)
spdf <- subset(spdf, continent != "Antarctica")
mapParams <- mapCountryData(spdf, nameColumnToPlot="action", numCats=length(table(counts_all$iso3c)), catMethod="pretty", colourPalette=colall, mapTitle='', lwd=.5, addLegend=FALSE, borderCol = "black")
do.call( addMapLegend, c(mapParams, legendWidth=.7, legendMar=3.5, legendShrink=.2,labelFontSize=.5, horizontal=F)) # 






