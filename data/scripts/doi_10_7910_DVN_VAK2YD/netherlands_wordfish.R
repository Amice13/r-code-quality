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

mydf_n <- read.csv("G:\\My Drive\\university\\phd\\women image\\netherlands.csv", stringsAsFactors = FALSE, encoding="UTF-8")
mydf_n$PartyTerm <- paste(mydf_n$Party, mydf_n$Term)
mydf_n$id <- seq(1, nrow(mydf_n), 1)
corp3 <- corpus(mydf_n$Text)
docvars(corp3, "text_id") <-mydf_n$text_id
docvars(corp3, "Speaker") <-mydf_n$Speaker
docvars(corp3, "Party") <-mydf_n$Party
docvars(corp3, "European.Party") <-mydf_n$European.Party
docvars(corp3, "State") <-mydf_n$State
docvars(corp3, "Gender") <-mydf_n$Gender
docvars(corp3, "Term") <-mydf_n$Term
docvars(corp3, "Date") <-mydf_n$Date
docvars(corp3, "Title") <-mydf_n$Title
docvars(corp3, "Age") <-mydf_n$Age
docvars(corp3, "Word") <-mydf_n$Word
docvars(corp3, "Link") <-mydf_n$LINK
#docvars(corp3, "PartyTerm") <- mydf_n$PartyTerm
toks_n <- tokens(corp3, remove_punct = TRUE)
toks_n <- tokens_tolower(toks_n)
toks_n <- tokens_remove(toks_n, stopwords("dutch"))
toks_n <- tokens_wordstem(toks_n, language = "dutch")

cdfm_n <- dfm(toks_n)

cdfm_n <- dfm_group(cdfm_n, groups = docvars(corp3, "Party"))

wf_n <- textmodel_wordfish(cdfm_n)
textplot_scale1d(wf_n, groups = c(docvars(cdfm_n, "Party")), doclabels = docvars(cdfm_n, "Party"), sort = FALSE)
textplot_scale1d(wf_n, groups = c(docvars(cdfm_n, "Party")), doclabels = docvars(cdfm_n, "Party"), sort = TRUE, highlighted_color = "black") 

textplot_scale1d(wf_n, margin = "features", highlighted = c("geweld", "islam", "positie", "nodig", "goed", "partij", "werk", "vrouw"))
head(wf_n$features[order(wf_n$beta, decreasing = T)])
head(wf_n$features[order(wf_n$beta, decreasing = F)])


