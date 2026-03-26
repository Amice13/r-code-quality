# Arthur Spirling
# 10/9/2024

rm(list=ls())
set.seed(1649)

# PURPOSE: provides a simple summary of the data (i.e. pamphlets) wrt their
# length

require(quanteda)
require(readtext)

#grab the tar files and unpack
untar("useful_objects.tar")
untar("leveller_texts.tar")

# get the doc variables for the corpus
overview <-  read.csv("useful_objects/overview_files_data.csv")


# get the Leveller pamphlet collection
setwd("leveller_texts")
rtL <- readtext("*.txt")


# set the variables up
m <-  match(rtL$doc_id, overview$DocCode)
variables <- overview[m,]

rtL_corp <- corpus(rtL)
docvars(rtL_corp) <- variables
# this is a bit roundabout -- we won't use this corpus as is, it's just here to
# make the docvars assignment easier later...

# drop the repeated documents
repeats <- which(docvars(rtL_corp)$Comments=="repeated doc")
rtL_corp <- rtL_corp[-c(repeats)]
docvars(rtL_corp)$Comments 

# this is the table in SI on summary data
summary(ntoken(rtL_corp))
