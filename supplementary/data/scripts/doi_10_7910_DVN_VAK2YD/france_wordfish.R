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

mydf_f <- read.csv("G:\\My Drive\\university\\phd\\women image\\france.csv", stringsAsFactors = FALSE, encoding="UTF-8")
mydf_f$PartyTerm <- paste(mydf_f$Party, mydf_f$Term)
mydf_f$id <- seq(1, nrow(mydf_f), 1)
corp2 <- corpus(mydf_f, text_field = "Text")
docvars(corp2, "text_id") <-mydf_f$text_id
docvars(corp2, "Speaker") <-mydf_f$Speaker
docvars(corp2, "Party") <-mydf_f$Party
docvars(corp2, "European.Party") <-mydf_f$European.Party
docvars(corp2, "State") <-mydf_f$State
docvars(corp2, "Gender") <-mydf_f$Gender
docvars(corp2, "Term") <-mydf_f$Term
docvars(corp2, "Date") <-mydf_f$Date
docvars(corp2, "Title") <-mydf_f$Title
docvars(corp2, "Age") <-mydf_f$Age
docvars(corp2, "Word") <-mydf_f$Word
docvars(corp2, "Link") <-mydf_f$LINK
#docvars(corp2, "PartyTerm") <- mydf_f$PartyTerm

toks_f <- tokens(corp2, remove_punct = TRUE)
toks_f <- tokens_tolower(toks_f)
toks_f <- tokens_remove(toks_f, stopwords("french"))
toks_f <- tokens_wordstem(toks_f, language = "french")

cdfm_f <- dfm(toks_f)

cdfm_f <- dfm_group(cdfm_f, groups = docvars(corp2, "Party"))

wf_f <- textmodel_wordfish(cdfm_f)
#textplot_scale1d(wf_f, groups = c(docvars(cdfm_f, "Party")), doclabels = docvars(cdfm_f, "PartyTerm"), sort = TRUE, highlighted_color = "black") 
textplot_scale1d(wf_f, groups = c(docvars(cdfm_f, "Party")), doclabels = docvars(cdfm_f, "Party"), sort = TRUE, highlighted_color = "black") 

#textplot_scale1d(wf_f, margin = "features")

textplot_scale1d(wf_f, margin = "features", highlighted = c("violenc", "progress", "migr", "androphob", "effort", "voix", "femm", "civil", "minor"))
head(wf_f$features[order(wf_f$beta, decreasing = T)])
head(wf_f$features[order(wf_f$beta, decreasing = F)])
