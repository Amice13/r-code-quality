library("readtext")
library("quanteda")
library("text2vec")
library("stopwords")
library("quanteda.textmodels")
library("devtools")
library("readxl")
library("igraph")
library("stopwords")
library("ggplot2")
library("quanteda.textplots")
packageVersion("quanteda")
data_britain <- read.csv("G:\\My Drive\\university\\phd\\women image\\britain.csv", stringsAsFactors = FALSE, encoding="UTF-8")
data_britain$PartyTerm <- paste(data_britain$Party, data_britain$Term)
data_britain$id <- seq(1, nrow(data_britain), 1)
corp1 <- corpus(data_britain$Text)
docvars(corp1, "text_id") <-data_britain$text_id
docvars(corp1, "Speaker") <-data_britain$Speaker
docvars(corp1, "Party") <-data_britain$Party
docvars(corp1, "European.Party") <-data_britain$European.Party
docvars(corp1, "State") <-data_britain$State
docvars(corp1, "Gender") <-data_britain$Gender
docvars(corp1, "Term") <-data_britain$Term
docvars(corp1, "Date") <-data_britain$Date
docvars(corp1, "Title") <-data_britain$Title
docvars(corp1, "Age") <-data_britain$Age
docvars(corp1, "Word") <-data_britain$Word
docvars(corp1, "Link") <-data_britain$LINK
docvars(corp1, "PartyTerm") <- data_britain$PartyTerm

toks <- tokens(corp1, remove_punct = TRUE)
toks <- tokens_tolower(toks)
toks <- tokens_remove(toks, stopwords("english"))
toks <- tokens_wordstem(toks, language = "english")

cdfm <- dfm(toks)

cdfm <- dfm_group(cdfm, groups = docvars(corp1, "Party"))

cdfm <- dfm_group(cdfm)
wf <- textmodel_wordfish(cdfm)
#textplot_scale1d(wf, groups = c(docvars(cdfm, "Party")), doclabels = docvars(cdfm, "PartyTerm"), sort = FALSE)

textplot_scale1d(wf, groups = c(docvars(cdfm, "Party")), doclabels = docvars(cdfm, "Party"), sort = FALSE)
textplot_scale1d(wf, groups = c(docvars(cdfm, "Party")), doclabels = docvars(cdfm, "Party"), sort = TRUE, highlighted_color = "black") 

textplot_scale1d(wf, margin = "features", highlighted = c("equal", "propaganda", "stereotyp", "gap", "taxpay", "innocence", "interfere", "reproduction", "key", "interfer", "women", "quota", "work", "need" ))
textplot_scale1d(wf, margin = "features" )

head(wf$features[order(wf$beta, decreasing = T)])
head(wf$features[order(wf$beta, decreasing = F)])


